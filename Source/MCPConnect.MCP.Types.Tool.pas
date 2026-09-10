unit MCPConnect.MCP.Types.Tool;

interface

{$I 'MCPConnect.inc' }

uses
  System.SysUtils,
  System.Rtti,
  System.JSON,
  System.Types,
  System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL,
  Neon.Core.Utils,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Attributes;


const
  /// <summary>
  ///   Tool names SHOULD be 1 to 128 characters long, case-sensitive, made only
  ///   of ASCII letters, digits, underscore, hyphen and dot. 2026-07-28 added
  ///   the dot and raised the length from the 64 of earlier revisions.
  /// </summary>
  MCP_TOOL_NAME_MIN_LENGTH = 1;
  MCP_TOOL_NAME_MAX_LENGTH = 128;

  /// <summary>
  ///   The [McpParam] tag (and the WithParam one) that asks for a parameter to
  ///   be mirrored into a Mcp-Param-&lt;value&gt; header: its value is written
  ///   into the generated schema as the "x-mcp-header" annotation.
  /// </summary>
  MCP_TOOL_PARAM_HEADER_TAG = 'header';

type
  /// <summary>
  /// Optional properties describing tool behavior
  /// </summary>
  TToolAnnotation = class

    /// <summary>
    /// Human-readable title for the tool
    /// </summary>
    Title: Nullable<string>;

    /// <summary>
    /// If true, the tool does not modify its environment
    /// </summary>
    ReadOnlyHint: Nullable<Boolean>;

    /// <summary>
    /// If true, the tool may perform destructive updates
    /// </summary>
    DestructiveHint: Nullable<Boolean>;

    /// <summary>
    /// If true, repeated calls with same args have no additional effect
    /// </summary>
    IdempotentHint: Nullable<Boolean>;

    /// <summary>
    /// If true, tool interacts with external entities
    /// </summary>
    OpenWorldHint: Nullable<Boolean>;
  end;

  TMCPUIApp = class
  private
    FMeta: TJSONObject;
    function GetResourceUri: NullString;
    procedure SetResourceUri(const Value: NullString);
  public
    constructor Create(AMeta: TJSONObject);

    /// <summary>
    ///   URI of the UI resource to display for this tool, if any. This is converted to
    ///   _meta.ui.resourceUri
    /// </summary>
    property ResourceUri: NullString read GetResourceUri write SetResourceUri;
  end;

  TMCPToolParam = class(TMetaClass)
  public
    Param: TRttiParameter;
    ParamName: string;
    Name: string;
    Description: string;
  end;

  /// <summary>
  /// Tool represents the definition for a tool the client can call.
  /// </summary>
  TMCPTool = class(TMetaClass)
  private type
    /// <summary>
    ///   Model: visible to and callable by the agent
    ///   App: callable by the app from this server only
    /// </summary>
    ToolVisibility = (Model, App);
  public
    [NeonIgnore] ToolClass: TClass;
    [NeonIgnore] Method: TRttiMethod;
    [NeonIgnore] MethodName: string;
    [NeonIgnore] MethodParams: TObjectList<TMCPToolParam>;
  public
    [NeonIgnore] Category: string;
    [NeonIgnore] Disabled: Boolean;
    [NeonIgnore] UI: TMCPUIApp;
    [NeonIgnore] Visibility: ToolVisibility;
  public
    /// <summary>
    /// The name of the tool
    /// </summary>
    Name: string;

    /// <summary>
    ///   Intended for UI and end-user contexts � optimized to be
    ///   human-readable and easily understood, even by those unfamiliar with
    ///   domain-specific terminology.
    /// </summary>
    Title: NullString;

    /// <summary>
    /// A human-readable description of the tool
    /// </summary>
    Description: Nullable<string>;

    /// <summary>
    /// A JSON Schema object defining the expected parameters for the tool
    /// </summary>
    InputSchema: TJSONObject;

    /// <summary>
    /// A JSON Schema object defining the expected parameters for the tool
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] OutputSchema: TJSONObject;

    /// <summary>
    /// Optional properties describing tool behavior
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TToolAnnotation;

    /// <summary>
    ///   Optional set of sized icons that the client can display in a user interface
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TMCPIconList;

  public
    constructor Create;
    destructor Destroy; override;

    function FindMCPParam(const AName: string): TMCPToolParam;
    function FindRttiParam(const AName: string): TRttiParameter;

    procedure ExchangeInputSchema(ASchema: TJSONObject);
    procedure ExchangeOutputSchema(ASchema: TJSONObject);
    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

/// <summary>
///   Whether AName satisfies the tool-name rule of the 2026-07-28 spec: 1 to
///   128 characters drawn from A-Z, a-z, 0-9, underscore, hyphen and dot.
/// </summary>
/// <remarks>
///   Scoped names are validated whole, separator included, since the scope
///   prefix is part of the name the client sees.
/// </remarks>
function IsValidToolName(const AName: string): Boolean;

/// <summary>
///   Whether AName may be the value of an "x-mcp-header" annotation: a
///   non-empty RFC 9110 field-name token (1*tchar). The rule excludes the
///   control characters CR and LF by construction, which is what makes an
///   annotation safe to hand to a client to put on the wire verbatim.
/// </summary>
/// <remarks>
///   The value names the Mcp-Param-&lt;name&gt; header, so the
///   MCP_HEADER_PARAM_PREFIX is not part of what is checked here.
/// </remarks>
function IsValidHeaderParamName(const AName: string): Boolean;

/// <summary>
///   The JSON type ASchema declares for itself, looking through the
///   ["X","null"] union a Nullable&lt;T&gt; generates, or '' when the schema
///   declares no scalar "type" at all.
/// </summary>
function SchemaJSONType(ASchema: TJSONObject): string;

implementation

function IsValidToolName(const AName: string): Boolean;
var
  LChar: Char;
begin
  if (Length(AName) < MCP_TOOL_NAME_MIN_LENGTH) or
     (Length(AName) > MCP_TOOL_NAME_MAX_LENGTH) then
    Exit(False);

  for LChar in AName do
    case LChar of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.': ;
    else
      // Anything else, non-ASCII letters included, is out
      Exit(False);
    end;

  Result := True;
end;

function IsValidHeaderParamName(const AName: string): Boolean;
var
  LChar: Char;
begin
  if AName.IsEmpty then
    Exit(False);

  for LChar in AName do
    case LChar of
      // tchar, RFC 9110 5.6.2: the unreserved punctuation plus DIGIT and ALPHA.
      // Everything a field name may not carry - the space, the separators, the
      // control characters, and CR and LF with them - falls through to the else
      'A'..'Z', 'a'..'z', '0'..'9',
      '!', '#', '$', '%', '&', '''', '*', '+', '-', '.',
      '^', '_', '`', '|', '~': ;
    else
      Exit(False);
    end;

  Result := True;
end;

function SchemaJSONType(ASchema: TJSONObject): string;
var
  LType: TJSONValue;
  LEntry: TJSONValue;
begin
  Result := '';
  if not Assigned(ASchema) then
    Exit;

  LType := ASchema.GetValue('type');

  if LType is TJSONString then
    Exit(TJSONString(LType).Value);

  // A Nullable<T> is rendered as a union with "null": the T is the type the
  // schema is about, and the null is the absence the caller already handles
  if LType is TJSONArray then
    for LEntry in TJSONArray(LType) do
      if (LEntry is TJSONString) and (TJSONString(LEntry).Value <> 'null') then
        Exit(TJSONString(LEntry).Value);
end;

{ TMCPUIApp }

constructor TMCPUIApp.Create(AMeta: TJSONObject);
begin
  FMeta := AMeta;
end;

function TMCPUIApp.GetResourceUri: NullString;
var
  LUiValue: TJSONValue;
begin
  LUiValue := FMeta.GetValue('ui');

  if Assigned(LUiValue) then
    Result := LUiValue.GetValue<string>('resourceUri');
end;

procedure TMCPUIApp.SetResourceUri(const Value: NullString);
var
  LUiValue: TJSONValue;
begin
  LUiValue := FMeta.GetValue('ui');

  if not Assigned(LUiValue) then
  begin
    LUiValue := TJSONObject.Create;
    FMeta.AddPair('ui', LUiValue);
  end;

  (LUiValue as TJSONObject).AddPair('resourceUri', Value);
end;

constructor TMCPTool.Create;
begin
  inherited;
  UI := TMCPUIApp.Create(Meta);
  MethodParams := TObjectList<TMCPToolParam>.Create(True);
  InputSchema := TJSONObject.Create;
  Annotations := TToolAnnotation.Create;
  OutputSchema := TJSONObject.Create;
end;

destructor TMCPTool.Destroy;
begin
  InputSchema.Free;
  Annotations.Free;
  OutputSchema.Free;
  MethodParams.Free;
  UI.Free;
  inherited;
end;

procedure TMCPTool.ExchangeInputSchema(ASchema: TJSONObject);
begin
  if ASchema = nil then
    Exit;

  InputSchema.Free;
  InputSchema := ASchema;
end;

procedure TMCPTool.ExchangeOutputSchema(ASchema: TJSONObject);
begin
  if ASchema = nil then
    Exit;

  OutputSchema.Free;
  OutputSchema := ASchema;
end;

function TMCPTool.FindMCPParam(const AName: string): TMCPToolParam;
begin
  Result := nil;
  for var par in MethodParams do
    if SameText(AName, par.ParamName) then
      Exit(par);
end;

function TMCPTool.FindRttiParam(const AName: string): TRttiParameter;
begin
  Result := nil;
  for var par in Method.GetParameters do
    if SameText(AName, par.Name) then
      Exit(par);
end;

function TMCPTool.ToJSON(APrettyPrint: Boolean): string;
begin
  Result := TNeon.ObjectToJSONString(Self, MCPNeonConfig.SetPrettyPrint(APrettyPrint));
end;

end.
