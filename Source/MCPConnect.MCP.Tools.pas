{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.MCP.Tools;

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

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  /// <summary>
  /// Parameters for CallToolRequest
  /// </summary>
  TCallToolParams = class(TMetaClass)

    /// <summary>
    ///   Name for the params
    /// </summary>
    Name: string;

    /// <summary>
    ///   Arguments for the tool
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;
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

  /// <summary>
  /// Tool represents the definition for a tool the client can call.
  /// </summary>
  TMCPTool = class(TMetaClass)
  private type
    /// <summary>
    ///   Model: visible to and callable by the agent <br />
    ///   App: callable by the app from this server only
    /// </summary>
    ToolVisibility = (Model, App);
  public
    [NeonIgnore] Classe: TClass;
    [NeonIgnore] Method: TRttiMethod;
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
    [NeonInclude(IncludeIf.NotEmpty)] Icons: TIconList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ExchangeInputSchema(ASchema: TJSONObject);
    function ToJSON(APrettyPrint: Boolean = False): string;
    procedure AddIcon(const ASrc: string);
  end;

  TMCPTools = class(TObjectList<TMCPTool>);
  TMCPToolRegistry = class(TObjectDictionary<string, TMCPTool>);
  TMCPToolConfigurator = reference to procedure(ATool: TMCPTool);
  TMCPToolFilterFunc = reference to function (ATool: TMCPTool): Boolean;

  TListToolsResult = class(TMetaClass)
  public

    /// <summary>
    ///   Tool list
    /// </summary>
    Tools: TMCPTools;

    /// <summary>
    ///   An opaque token representing the pagination position after the last returned result. If present, there may be more results available
    /// </summary>
    NextCursor: NullString;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON(APrettyPrint: Boolean = False): string;
  end;

  /// <summary>
  ///   Can be TextContent, ImageContent, AudioContent, ResourceLink, or
  ///   EmbeddedResource
  /// </summary>
  TCallToolResult = class(TMetaClass)
  public

   /// <summary>
   ///   Can be TextContent, ImageContent, AudioContent, ResourceLink, or
   ///   EmbeddedResource
   /// </summary>
	  Content: TContentList;

    /// <summary>
    ///   Structured content returned as a JSON object in the structuredContent
    ///   field of a result. For backwards compatibility, a tool that returns
    ///   structured content SHOULD also return functionally equivalent
    ///   unstructured content.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] StructuredContent: TJSONObject;

    /// <summary>
    ///   Whether the tool call ended in an error. If not set, this is assumed
    ///   to be false (the call was successful).
    /// </summary>
    [NeonIgnore] IsError: Nullable<Boolean>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContent(AContent: TToolContent);
  end;

implementation

uses
  MCPConnect.JRPC.Core;

procedure TMCPTool.AddIcon(const ASrc: string);
var
  LIcon: TMCPIcon;
begin
  if not ASrc.IsEmpty then
  begin
    LIcon.Src := ASrc;
    Icons := Icons + [LIcon];
  end;
end;

constructor TMCPTool.Create;
begin
  inherited;
  UI := TMCPUIApp.Create(Meta);

  InputSchema := TJSONObject.Create;
  Annotations := TToolAnnotation.Create;
  OutputSchema := TJSONObject.Create;
end;

destructor TMCPTool.Destroy;
begin
  InputSchema.Free;
  Annotations.Free;
  OutputSchema.Free;
  UI.Free;
  inherited;
end;

procedure TMCPTool.ExchangeInputSchema(ASchema: TJSONObject);
begin
  if Aschema = nil then
    Exit;

  InputSchema.Free;
  InputSchema := ASchema;
end;

function TMCPTool.ToJSON(APrettyPrint: Boolean): string;
begin
  Result := TNeon.ObjectToJSONString(Self, MCPNeonConfig.SetPrettyPrint(APrettyPrint));
end;

{ TListToolsResult }

constructor TListToolsResult.Create;
begin
  inherited;
  Tools := TMCPTools.Create(False);
end;

destructor TListToolsResult.Destroy;
begin
  Tools.Free;
  inherited;
end;

function TListToolsResult.ToJSON(APrettyPrint: Boolean = False): string;
begin
  Result := TNeon.ObjectToJSONString(Self, MCPNeonConfig.SetPrettyPrint(APrettyPrint));
end;

constructor TCallToolParams.Create;
begin
  inherited;
  Arguments := TJSONObject.Create;
end;

destructor TCallToolParams.Destroy;
begin
  Arguments.Free;
  inherited;
end;

{ TCallToolResult }

constructor TCallToolResult.Create;
begin
  inherited;
  Content := TContentList.Create;
  StructuredContent := TJSONObject.Create;
end;

destructor TCallToolResult.Destroy;
begin
  Content.Free;
  StructuredContent.Free;
  inherited;
end;

procedure TCallToolResult.AddContent(AContent: TToolContent);
begin
  Content.Add(AContent);
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

end.
