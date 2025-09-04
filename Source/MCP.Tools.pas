unit MCP.Tools;

interface

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
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Serializers.RTL,
  Neon.Core.Utils,

  MCP.Types,
  MCP.Attributes;

type
  /// <summary>
  /// Parameters for CallToolRequest
  /// </summary>
  TCallToolParams = class
    [NeonProperty('name')] Name: string;
    [NeonProperty('arguments'), NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONMap;
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Optional properties describing tool behavior
  /// </summary>
  TToolAnnotation = class
    /// <summary>
    /// Human-readable title for the tool
    /// </summary>
    [NeonProperty('title')] Title: Nullable<string>;

    /// <summary>
    /// If true, the tool does not modify its environment
    /// </summary>
    [NeonProperty('readOnlyHint')] ReadOnlyHint: Nullable<Boolean>;

    /// <summary>
    /// If true, the tool may perform destructive updates
    /// </summary>
    [NeonProperty('destructiveHint')] DestructiveHint: Nullable<Boolean>;

    /// <summary>
    /// If true, repeated calls with same args have no additional effect
    /// </summary>
    [NeonProperty('idempotentHint')] IdempotentHint: Nullable<Boolean>;

    /// <summary>
    /// If true, tool interacts with external entities
    /// </summary>
    [NeonProperty('openWorldHint')] OpenWorldHint: Nullable<Boolean>;
  end;

  /// <summary>
  /// Tool represents the definition for a tool the client can call.
  /// </summary>
  TTool = class
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;

    /// <summary>
    /// The name of the tool
    /// </summary>
    [NeonProperty('name')] Name: string;

    /// <summary>
    /// A human-readable description of the tool
    /// </summary>
    [NeonProperty('description')] Description: Nullable<string>;

    /// <summary>
    /// A JSON Schema object defining the expected parameters for the tool
    /// </summary>
    [NeonProperty('inputSchema')] InputSchema: TJSONObject;

    /// <summary>
    /// A JSON Schema object defining the expected parameters for the tool
    /// </summary>
    [NeonProperty('outputSchema'), NeonInclude(IncludeIf.NotEmpty)] OutputSchema: TJSONObject;

    /// <summary>
    /// Optional properties describing tool behavior
    /// </summary>
    [NeonProperty('annotations'), NeonInclude(IncludeIf.NotEmpty)] Annotations: TToolAnnotation;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExchangeInputSchema(ASchema: TJSONObject);
    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  TTools = class(TObjectList<TTool>)
  end;

  TListToolsResult = class
  public
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;

    [NeonProperty('tools')] Tools: TTools;

    /// <summary>
    /// An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    [NeonProperty('nextCursor')] NextCursor: Nullable<string>;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  TCallToolResult = class
    /// <summary>
    /// Meta is a metadata object that is reserved by MCP for storing additional information
    /// </summary>
    [NeonProperty('_meta'), NeonInclude(IncludeIf.NotEmpty)] Meta: TMeta;


	  [NeonProperty('content')] Content: TJSONObject; // Can be TextContent, ImageContent, AudioContent, or EmbeddedResource

    /// <summary>
    /// Structured content returned as a JSON object in the structuredContent field of a result.
    /// For backwards compatibility, a tool that returns structured content SHOULD also return
    /// functionally equivalent unstructured content.
    /// </summary>
    StructuredContent: TJSONObject;

    /// <summary>
    /// Whether the tool call ended in an error.
    /// If not set, this is assumed to be false (the call was successful).
    /// </summary>
    IsError: Nullable<Boolean>;
  end;

type
  /// <summary>
  ///   MCP Tools Schema Generator
  /// </summary>
  TMCPSchemaGenerator = class
  protected
    /// <summary>
    ///   Writer for a method's params
    /// </summary>
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);

    /// <summary>
    ///   Writer for Integer types
    /// </summary>
    function WriteMethodOld(AMethod: TRttiMethod): TJSONObject;
    function WriteMethod(AMethod: TRttiMethod): TTool;

    function WriteMethodsOld(AType: TRttiType): TJSONArray;
    procedure WriteMethods(AType: TRttiType; AList: TTools);
  public
    /// <summary>
    ///   Serialize a Delphi method as a MCP tool
    ///   The Delphi method must be marked with the MCP attributes
    /// </summary>
    class function MethodToTool(AMethod: TRttiMethod): TTool;

    /// <summary>
    ///   Loops through the methods of a class/record and populate a structure
    ///   in response to the tools/list from a LLM client
    /// </summary>
    class function ListTools(AType: TRttiType): TListToolsResult; overload;
    class function ListTools(AClass: TClass): TListToolsResult; overload;
  end;




implementation

{ TTool }

constructor TTool.Create;
begin
  Meta := TMeta.Create;
  InputSchema := TJSONObject.Create;
  Annotations := TToolAnnotation.Create;
  OutputSchema := TJSONObject.Create;
end;

destructor TTool.Destroy;
begin
  Meta.Free;
  InputSchema.Free;
  Annotations.Free;
  OutputSchema.Free;

  inherited;
end;

procedure TTool.ExchangeInputSchema(ASchema: TJSONObject);
begin
  if Aschema = nil then
    Exit;

  InputSchema.Free;
  InputSchema := ASchema;
end;

function TTool.ToJSON(APrettyPrint: Boolean): string;
begin
  Result := TNeon.ObjectToJSONString(Self, GetNeonConfig.SetPrettyPrint(APrettyPrint));
end;

{ TListToolsResult }

constructor TListToolsResult.Create;
begin
  Meta := TMeta.Create;
  Tools := TTools.Create(True);
end;

destructor TListToolsResult.Destroy;
begin
  Tools.Free;
  Meta.Free;

  inherited;
end;

function TListToolsResult.ToJSON(APrettyPrint: Boolean = False): string;
begin
  Result := TNeon.ObjectToJSONString(Self, GetNeonConfig.SetPrettyPrint(APrettyPrint));
end;

class function TMCPSchemaGenerator.ListTools(AType: TRttiType): TListToolsResult;
var
  LGenerator: TMCPSchemaGenerator;
begin
  Result := TListToolsResult.Create;
  try
    LGenerator := TMCPSchemaGenerator.Create();
    try
      LGenerator.WriteMethods(AType, Result.Tools);
    finally
      LGenerator.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TMCPSchemaGenerator.ListTools(AClass: TClass): TListToolsResult;
begin
  Result := ListTools(TRttiUtils.Context.GetType(AClass));
end;

class function TMCPSchemaGenerator.MethodToTool(AMethod: TRttiMethod): TTool;
var
  LGenerator: TMCPSchemaGenerator;
begin
  LGenerator := TMCPSchemaGenerator.Create();
  try
    Result := LGenerator.WriteMethod(AMethod);
  finally
    LGenerator.Free;
  end;
end;

function TMCPSchemaGenerator.WriteMethodOld(AMethod: TRttiMethod): TJSONObject;
var
  LToolName, LToolDesc: string;
  LProps, LSchema: TJSONObject;
  LAttr: MCPToolAttribute;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(AMethod, LProps, LRequired);
  except
    LProps.Free;
    LRequired.Free;
    raise;
  end;

  LSchema := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProps);
    //.AddPair('additionalProperties', False);
    //.AddPair('$schema', 'http://json-schema.org/draft-07/schema#');

  LToolName := AMethod.Name;
  LToolDesc := AMethod.Name;

  LAttr := AMethod.GetAttribute<MCPToolAttribute>;
  if Assigned(LAttr) then
  begin
    if LAttr.Name <> '' then
      LToolName := LAttr.Name;

    if LAttr.Description <> '' then
      LToolDesc := LAttr.Description;
  end;

  Result := TJSONObject.Create
    .AddPair('name', LToolName)
    .AddPair('description', LToolDesc)
    .AddPair('inputSchema', LSchema);

  if LRequired.Count > 0 then
    Result.AddPair('required', LRequired)
  else
    LRequired.Free;
end;

function TMCPSchemaGenerator.WriteMethod(AMethod: TRttiMethod): TTool;
var
  LToolName, LToolDesc: string;
  LProps, LInputSchema: TJSONObject;
  LAttr: MCPToolAttribute;
  LRequired: TJSONArray;

begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(AMethod, LProps, LRequired);
  except
    LProps.Free;
    LRequired.Free;
    raise;
  end;

  LInputSchema := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProps);
    //.AddPair('additionalProperties', False);
    //.AddPair('$schema', 'http://json-schema.org/draft-07/schema#');

  if LRequired.Count > 0 then
    LInputSchema.AddPair('required', LRequired)
  else
    LRequired.Free;

  LToolName := AMethod.Name;
  LToolDesc := AMethod.Name;

  LAttr := AMethod.GetAttribute<MCPToolAttribute>;
  if Assigned(LAttr) then
  begin
    if LAttr.Name <> '' then
      LToolName := LAttr.Name;

    if LAttr.Description <> '' then
      LToolDesc := LAttr.Description;
  end;

  Result := TTool.Create;
  try
    Result.Name := LToolName;
    Result.Description := LToolDesc;
    Result.ExchangeInputSchema(LInputSchema);

    //LAttr.Tags.ApplyToFields(Result.Annotations);

    if LAttr.Tags.Exists('readonly') then
      Result.Annotations.ReadOnlyHint := True;

    if LAttr.Tags.Exists('destructive') then
      Result.Annotations.DestructiveHint := True;

    if LAttr.Tags.Exists('idempotent') then
      Result.Annotations.IdempotentHint := True;

    if LAttr.Tags.Exists('openworld') then
      Result.Annotations.OpenWorldHint := True;
  except
    Result.Free;
    raise;
  end;
end;

function TMCPSchemaGenerator.WriteMethodsOld(AType: TRttiType): TJSONArray;
var
  LMethod: TRttiMethod;
  LMethods: TArray<TRttiMethod>;
  LTool: TJSONObject;
begin
  Result := TJSONArray.Create;
  try
    LMethods := AType.GetMethods;
    for LMethod in LMethods do
      if Assigned(LMethod.GetAttribute(MCPToolAttribute)) then
      begin
        LTool := WriteMethodOld(LMethod);
        Result.AddElement(LTool);
      end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPSchemaGenerator.WriteMethods(AType: TRttiType; AList: TTools);
var
  LMethod: TRttiMethod;
  LMethods: TArray<TRttiMethod>;
  LTool: TTool;
begin
  try
    LMethods := AType.GetMethods;
    for LMethod in LMethods do
      if Assigned(LMethod.GetAttribute(MCPToolAttribute)) then
      begin
        LTool := WriteMethod(LMethod);
        AList.Add(LTool);
      end;
  except
    AList.Clear;
    raise;
  end;
end;

procedure TMCPSchemaGenerator.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
  LAttr: MCPParamAttribute;
begin
  for LParam in AMethod.GetParameters do
  begin
    LAttr := LParam.GetAttribute<MCPParamAttribute>;
      if not Assigned(LAttr) then
        raise Exception.Create('Non-annotated params are not permitted');

    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, GetNeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(LAttr.Description));
    AProps.AddPair(LAttr.Name, LJSONObj);
    ARequired.Add(LAttr.Name);
  end;
end;

{ TCallToolParams }

constructor TCallToolParams.Create;
begin
  Arguments := TJSONMap.Create;
  Meta := TMeta.Create;
end;

destructor TCallToolParams.Destroy;
begin
  Meta.Free;
  Arguments.Free;

  inherited;
end;

end.
