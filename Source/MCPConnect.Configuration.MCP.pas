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
unit MCPConnect.Configuration.MCP;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Rtti,

  Neon.Core.Nullables,

  MCPConnect.Core.Utils,
  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources,

  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core, System.JSON;

type
  /// <summary>
  ///   Represents a tools/resources/prompts class registration with its namespace.
  /// </summary>
  TMCPClassInfo = record
    Scope: string;
    MCPClass: TClass;
  end;

  TMCPBaseConfig = class;
  TMCPListConfig = class;
  TMCPToolsConfig = class;
  TMCPServerConfig = class;
  TMCPResourcesConfig = class;

  /// <summary>
  ///   Primary configuration interface for Model Context Protocol (MCP) servers.
  ///   Configures server metadata, tool classes, and custom content writers for
  ///   handling complex return types. This is the main entry point for setting
  ///   up an MCP server using the MCPConnect framework.
  /// </summary>
  /// <remarks>
  ///   This configuration is required for all MCP servers. At minimum, you must:
  ///   1. Set a tool class using SetToolClass (contains methods marked with [McpTool])
  ///   2. Optionally set server name/version for client identification
  ///   3. Register custom writers if returning complex types (images, streams, etc.)
  ///
  ///   The tool class contains all methods exposed as MCP tools via the [McpTool]
  ///   attribute. Methods are automatically discovered and registered via RTTI.
  ///
  ///   Custom writers handle conversion of complex Delphi types to MCP content
  ///   formats. Built-in writers exist for common types (TStream, TStringList),
  ///   and VCL-specific writers handle TPicture, TBitmap, TImage, etc.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Basic MCP server configuration:
  ///   FJRPCServer.Plugin.Configure&lt;IMCPConfig&gt;
  ///     .SetServerName('my-mcp-server')
  ///     .SetServerVersion('1.0.0')
  ///     .SetToolClass(TMyMCPTools)
  ///     .ApplyConfig;
  ///
  ///   // With custom writers for VCL image types:
  ///   FJRPCServer.Plugin.Configure&lt;IMCPConfig&gt;
  ///     .SetServerName('image-processor')
  ///     .SetServerVersion('2.1.0')
  ///     .SetToolClass(TImageTools)
  ///     .RegisterWriter(TMCPPictureWriter)
  ///     .RegisterWriter(TMCPBitmapWriter)
  ///     .RegisterWriter(TMCPImageWriter)
  ///     .ApplyConfig;
  ///
  ///   // Now tool methods can return TPicture/TBitmap directly:
  ///   [McpTool('get_image', 'Returns an image')]
  ///   function GetImage: TPicture;  // Automatically converted to base64 PNG
  ///   </code>
  /// </example>
  IMCPConfig = interface(IJRPCConfiguration)
  ['{B8BBD257-2FE1-479A-8D63-5331164CF5E5}']

    /// <summary>
    ///   Server configuration
    /// </summary>
    function Server: TMCPServerConfig;

    /// <summary>
    ///   Tools configuration
    /// </summary>
    function Tools: TMCPToolsConfig;

    /// <summary>
    ///   Resources configuration
    /// </summary>
    function Resources: TMCPResourcesConfig;

    /// <summary>
    ///   Prompts configuration
    /// </summary>
    function Prompts: TMCPListConfig;
  end;

  TMCPBaseConfig = class
  protected
    FConfig: IMCPConfig;
  public
    constructor Create(AConfig: IMCPConfig);
    function SetIcon(const ASrc: string; var AIcon: TMCPIcon): Boolean;

    function BackToMCP: IMCPConfig;
  end;

  TMCPCapability = (Tools, Resources, Prompts, Tasks, Logging, Completions);
  TMCPCapabilities = set of TMCPCapability;

  TMCPServerConfig = class(TMCPBaseConfig)
  public
    IconFolder: string;
    ScopeSeparator: string;
    Name: string;
    Description: string;
    Version: string;
    Capabilities: TMCPCapabilities;
    WriterRegistry: TMCPWriterRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;
  public

    function SetIconFolder(const AFolder: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the separator character/string used between scope and tool name.
    ///   Default is '_' (underscore), resulting in names like "auth_login".
    /// </summary>
    /// <param name="ASeparator">
    ///   Separator string. Must be MCP-compliant (only a-zA-Z0-9_-).
    ///   Common values: '_' (default), '-'
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   IMPORTANT: MCP requires tool names to match ^[a-zA-Z0-9_-]{1,64}$
    ///   Do NOT use '.', ':', or other special characters.
    /// </remarks>
    function SetScopeSeparator(const ASeparator: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server name returned in the MCP initialize response.
    ///   Identifies the server to MCP clients (Claude Desktop, etc.).
    /// </summary>
    /// <param name="AName">Human-readable server name (default: 'MCPServer')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetName(const AName: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server description returned in the MCP initialize response.
    /// </summary>
    /// <param name="ADescription">Description for the server (default: '')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetDescription(const ADescription: string): TMCPServerConfig;

    /// <summary>
    ///   Sets the server version returned in the MCP initialize response.
    ///   Helps clients identify server capabilities and compatibility.
    /// </summary>
    /// <param name="AVersion">Semantic version string (default: '1.0')</param>
    /// <returns>Self for fluent chaining</returns>
    function SetVersion(const AVersion: string): TMCPServerConfig;

    function SetCapabilities(ACapabilities: TMCPCapabilities): TMCPServerConfig;

    /// <summary>
    ///   Registers a custom content writer for handling complex return types.
    ///   Writers convert Delphi types (TPicture, TStream, etc.) to MCP content
    ///   formats (base64 images, text, embedded resources).
    /// </summary>
    /// <param name="AClass">
    ///   Content writer class (must inherit from TMCPCustomWriter). Examples:
    ///   - TMCPPictureWriter (VCL TPicture -> base64 PNG)
    ///   - TMCPStreamWriter (TStream -> base64 or text)
    ///   - TMCPStringListWriter (TStringList -> text)
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   Writers are checked in registration order. First matching writer handles
    ///   the conversion. Built-in writers for basic types are always available.
    /// </remarks>
    function RegisterWriter(AClass: TCustomWriterClass): TMCPServerConfig;
  end;

  TMCPListConfig = class(TMCPBaseConfig)
  private
    FClasses: TObjectDictionary<string, TClass>;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPListConfig; overload;
    function GetClasses: TArray<TMCPClassInfo>;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const ANamespace: string): TObject;
  end;


  TMCPToolsConfig = class(TMCPBaseConfig)
  private
    procedure WriteInputSchema(ATool: TMCPTool);
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
    procedure WriteTool(ATool: TMCPTool; AToolAttr: MCPToolAttribute; AAppAttr: MCPAppAttribute);
  public
    ToolRegistry: TMCPToolRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPToolsConfig;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const ATool: string): TObject;

    function ListComplete: TListToolsResult;
    function ListEnabled: TListToolsResult;

    procedure FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
  end;

  TMCPStaticResource = class
  public
    class procedure GetResource(AConfig: IMCPConfig; AResource: TMCPResource; AResult: TReadResourceResult);
  end;

  TMCPResourcesConfig = class(TMCPBaseConfig)
  private type
    TypeKindSet = set of TTypeKind;
  private const
    URI_REGEX = '[^{\}]+(?=})';
  public
    Registry: TMCPResourceRegistry;
    TemplateRegistry: TMCPTemplateRegistry;
    MimeTypes: TMCPMimeTypes;
    Schemes: TDictionary<string, string>;
    BasePath: string;
  private
    function ParamIsType(AParam: TRttiParameter; ATypes: TypeKindSet): Boolean;
    function ValidUriResource(const AUri: string): Boolean;
    function GetUriParams(const AUri: string): TArray<string>;

    procedure RegisterUIMethod(AClass: TClass; AMethod: TRttiMethod; AAttr: MCPAppUIAttribute);
    procedure RegisterResMethod(AClass: TClass; AMethod: TRttiMethod; AAttr: MCPResourceAttribute);
    procedure RegisterTplMethod(AClass: TClass; AMethod: TRttiMethod; AAttr: MCPTemplateAttribute);
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function AddMimeType(AEncoding: TMimeEncoding; const AMime: string; const AExt: string = ''): TMCPResourcesConfig;
    function SetBasePath(const APath: string): TMCPResourcesConfig;
    function RegisterScheme(const AScheme, APath: string): TMCPResourcesConfig;

    function RegisterClass(AClass: TClass): TMCPResourcesConfig;
    function RegisterFile(const AFileName, ADescription: string; const AMime: string = ''): TMCPResourcesConfig;
    function RegisterResource(AClass: TClass; const AMethod, AUri: string; AConfig: TMCPResourceConfigurator): TMCPResourcesConfig;
    function RegisterTemplate(AClass: TClass; const AMethod, AUriTemplate: string; AConfig: TMCPTemplateConfigurator): TMCPResourcesConfig;
    function RegisterUI(AClass: TClass; const AMethod, AUri: string; AConfig: TMCPUIResourceConfigurator): TMCPResourcesConfig;

    function GetResource(const AUri: string): TMCPResource;
    function GetTemplate(const AUri: string): TMCPResourceTemplate;


    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const AUri: string): TObject;

    procedure ResourceList(AList: TListResourcesResult);
    procedure TemplateList(AList: TListResourceTemplatesResult);
  end;

  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FServer: TMCPServerConfig;
    FTools: TMCPToolsConfig;
    FResources: TMCPResourcesConfig;

    FPrompts: TMCPListConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;

    { IMCPConfig }
    function Server: TMCPServerConfig;
    function Tools: TMCPToolsConfig;
    function Resources: TMCPResourcesConfig;

    function Prompts: TMCPListConfig;
  end;


implementation

uses
  System.TypInfo,
  System.IOUtils,
  System.RegularExpressions,
  Neon.Core.Utils,
  Neon.Core.Persistence.JSON.Schema;

constructor TMCPConfig.Create(AApp: IJRPCApplication);
begin
  inherited;

  FServer := TMCPServerConfig.Create(Self);
  FTools := TMCPToolsConfig.Create(Self);
  FResources := TMCPResourcesConfig.Create(Self);
  FPrompts := TMCPListConfig.Create(Self);
end;

destructor TMCPConfig.Destroy;
begin
  FPrompts.Free;
  FResources.Free;
  FTools.Free;
  FServer.Free;

  inherited;
end;

function TMCPConfig.Prompts: TMCPListConfig;
begin
  Result := FPrompts;
end;

function TMCPConfig.Resources: TMCPResourcesConfig;
begin
  Result := FResources;
end;

function TMCPConfig.Tools: TMCPToolsConfig;
begin
  Result := FTools;
end;

function TMCPConfig.Server: TMCPServerConfig;
begin
  Result := FServer;
end;

{ TMCPListConfig }

constructor TMCPListConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  FClasses := TObjectDictionary<string, TClass>.Create;
end;

destructor TMCPListConfig.Destroy;
begin
  FClasses.Free;
  inherited;
end;

function TMCPListConfig.GetClasses: TArray<TMCPClassInfo>;
var
  LPair: TPair<string, TClass>;
  LIndex: Integer;
begin
  SetLength(Result, FClasses.Count);
  LIndex := 0;
  for LPair in FClasses do
  begin
    Result[LIndex].Scope := LPair.Key;
    Result[LIndex].MCPClass := LPair.Value;
    Inc(LIndex);
  end;
end;

function TMCPListConfig.RegisterClass(AClass: TClass): TMCPListConfig;
begin
  FClasses.AddOrSetValue(AClass.ClassName, AClass);
  Result := Self;
end;

function TMCPListConfig.CreateInstance(const ANamespace: string): TObject;
var
  LClass: TClass;
begin
  if not FClasses.TryGetValue('', LClass) then
    raise EJRPCException.CreateFmt('Tool class not found for namespace "%s"', [ANamespace]);

  Result := TRttiUtils.CreateInstance(LClass);
end;

constructor TMCPToolsConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  ToolRegistry := TMCPToolRegistry.Create([doOwnsValues]);
end;

function TMCPToolsConfig.CreateInstance(const ATool: string): TObject;
var
  LTool: TMCPTool;
begin
  if not ToolRegistry.TryGetValue(ATool, LTool) then
    raise EMCPException.CreateFmt('Tool [%s] not found', [ATool]);

  Result := TRttiUtils.CreateInstance(LTool.Classe);
end;

destructor TMCPToolsConfig.Destroy;
begin
  ToolRegistry.Free;

  inherited;
end;

function TMCPToolsConfig.ListEnabled: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    if not pair.Value.Disabled then
      Result.Tools.Add(pair.Value);
end;

function TMCPToolsConfig.RegisterClass(AClass: TClass): TMCPToolsConfig;
var
  LScope: string;
  LClassType: TRttiType;
  LAppAttr: MCPAppAttribute;
  LToolAttr: MCPToolAttribute;
  LScopeAttr: MCPScopeAttribute;
  LTool: TMCPTool;
begin
  LScope := '';
  LClassType := TRttiUtils.Context.GetType(AClass);
  LScopeAttr := TRttiUtils.FindAttribute<MCPScopeAttribute>(LClassType);
  if Assigned(LScopeAttr) then
    LScope := LScopeAttr.Name + FConfig.Server.ScopeSeparator;

  // Registers all the tools found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LToolAttr := TRttiUtils.FindAttribute<MCPToolAttribute>(LMethod);
    if not Assigned(LToolAttr) then
      Continue;

    LAppAttr := TRttiUtils.FindAttribute<MCPAppAttribute>(LMethod);

    LTool := TMCPTool.Create;
    try
      LTool.Name := LScope + LToolAttr.Name;
      LTool.Description := LToolAttr.Description;
      LTool.Classe := AClass;
      LTool.Method := LMethod;

      WriteTool(LTool, LToolAttr, LAppAttr);

      ToolRegistry.Add(LTool.Name, LTool);
    except
      LTool.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPToolsConfig.ListComplete: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    Result.Tools.Add(pair.Value);
end;

procedure TMCPToolsConfig.FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
begin
  for var pair in ToolRegistry do
    if AFilter(pair.Value) then
      AList.Tools.Add(pair.Value);
end;

procedure TMCPToolsConfig.WriteTool(ATool: TMCPTool; AToolAttr: MCPToolAttribute; AAppAttr: MCPAppAttribute);
var
  LIcon: TMCPIcon;
begin
  if SetIcon(AToolAttr.Tags.GetValueAs<string>('icon'), LIcon) then
    ATool.Icons := ATool.Icons + [LIcon];

  ATool.Category := AToolAttr.Tags.GetValueAs<string>('category');
  ATool.Disabled := AToolAttr.Tags.GetBoolValue('disabled');

  if Assigned(AAppAttr) then
    ATool.UI.ResourceUri := AAppAttr.UI
  else if AToolAttr.Tags.Exists('app') then
    ATool.UI.ResourceUri := AToolAttr.Tags.GetValueAs<string>('app');

  if AToolAttr.Tags.Exists('readonly') then
    ATool.Annotations.ReadOnlyHint := AToolAttr.Tags.GetBoolValue('readonly');
  if AToolAttr.Tags.Exists('destructive') then
    ATool.Annotations.DestructiveHint := AToolAttr.Tags.GetBoolValue('destructive');
  if AToolAttr.Tags.Exists('idempotent') then
    ATool.Annotations.IdempotentHint := AToolAttr.Tags.GetBoolValue('idempotent');
  if AToolAttr.Tags.Exists('openworld') then
    ATool.Annotations.OpenWorldHint := AToolAttr.Tags.GetBoolValue('openworld');

  WriteInputSchema(ATool);
end;

procedure TMCPToolsConfig.WriteInputSchema(ATool: TMCPTool);
var
  LProps, LInputSchema: TJSONObject;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(ATool.Method, LProps, LRequired);
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

  ATool.ExchangeInputSchema(LInputSchema);
end;

procedure TMCPToolsConfig.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
  LAttr: MCPParamAttribute;
begin
  for LParam in AMethod.GetParameters do
  begin
    LAttr := LParam.GetAttribute<MCPParamAttribute>;
      if not Assigned(LAttr) then
        raise EJRPCException.Create('Non-annotated params are not permitted');

    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, MCPNeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(LAttr.Description));
    AProps.AddPair(LAttr.Name, LJSONObj);
    ARequired.Add(LAttr.Name);
  end;
end;

constructor TMCPServerConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  WriterRegistry := TMCPWriterRegistry.Create;

  IconFolder := TPath.GetAppPath;
  ScopeSeparator := '_';  // Default separator (MCP requires ^[a-zA-Z0-9_-]{1,64}$)
  Capabilities := [TMCPCapability.Tools, TMCPCapability.Resources, TMCPCapability.Prompts];
end;

destructor TMCPServerConfig.Destroy;
begin
  WriterRegistry.Free;
  inherited;
end;

function TMCPServerConfig.RegisterWriter(AClass: TCustomWriterClass): TMCPServerConfig;
begin
  WriterRegistry.RegisterWriter(AClass);
  Result := Self;
end;

function TMCPServerConfig.SetCapabilities(ACapabilities: TMCPCapabilities): TMCPServerConfig;
begin
  Capabilities := ACapabilities;
  Result := Self;
end;

function TMCPServerConfig.SetDescription(const ADescription: string): TMCPServerConfig;
begin
  Description := ADescription;
  Result := Self;
end;

function TMCPServerConfig.SetIconFolder(const AFolder: string): TMCPServerConfig;
begin
  IconFolder := AFolder;
  Result := Self;
end;

function TMCPServerConfig.SetName(const AName: string): TMCPServerConfig;
begin
  Name := AName;
  Result := Self;
end;

function TMCPServerConfig.SetScopeSeparator(const ASeparator: string): TMCPServerConfig;
begin
  ScopeSeparator := ASeparator;
  Result := Self;
end;

function TMCPServerConfig.SetVersion(const AVersion: string): TMCPServerConfig;
begin
  Version := AVersion;
  Result := Self;
end;

{ TMCPBaseConfig }

function TMCPBaseConfig.SetIcon(const ASrc: string; var AIcon: TMCPIcon): Boolean;
begin
  if ASrc.IsEmpty then
    Exit(False);

  if ASrc.Contains('://') then
  begin
    AIcon.Src := ASrc;
    Exit(True);
  end;

  AIcon.FromFile(TPath.Combine(FConfig.Server.IconFolder, ASrc));
  Exit(True);
end;

function TMCPBaseConfig.BackToMCP: IMCPConfig;
begin
  Result := FConfig;
end;

constructor TMCPBaseConfig.Create(AConfig: IMCPConfig);
begin
  FConfig := AConfig;
end;

{ TMCPResourcesConfig }

function TMCPResourcesConfig.AddMimeType(AEncoding: TMimeEncoding; const AMime: string; const AExt: string): TMCPResourcesConfig;
begin
  MimeTypes.AddMime(AEncoding, AMime, AExt);
  Result := Self;
end;

procedure TMCPResourcesConfig.ResourceList(AList: TListResourcesResult);
begin
  for var pair in Registry do
    if not pair.Value.Disabled then
      AList.Resources.Add(pair.Value);
end;

procedure TMCPResourcesConfig.TemplateList(AList: TListResourceTemplatesResult);
begin
  for var pair in TemplateRegistry do
    if not pair.Value.Disabled then
      AList.ResourceTemplates.Add(pair.Value);
end;

constructor TMCPResourcesConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  BasePath := GetCurrentDir;
  ForceDirectories(BasePath);
  MimeTypes := TMCPMimeTypes.Create;
  Registry := TMCPResourceRegistry.Create([doOwnsValues]);
  TemplateRegistry := TMCPTemplateRegistry.Create([doOwnsValues]);
end;

destructor TMCPResourcesConfig.Destroy;
begin
  MimeTypes.Free;
  Registry.Free;
  TemplateRegistry.Free;
  inherited;
end;

function TMCPResourcesConfig.CreateInstance(const AUri: string): TObject;
var
  LResource: TMCPResource;
begin
  if not Registry.TryGetValue(AUri, LResource) then
    raise EMCPException.CreateFmt('Resource [%s] not found', [AUri]);

  Result := TRttiUtils.CreateInstance(LResource.Classe);
end;

function TMCPResourcesConfig.GetResource(const AUri: string): TMCPResource;
begin
  if not Registry.TryGetValue(AUri, Result) then
    Exit(nil);
end;

function TMCPResourcesConfig.GetTemplate(const AUri: string): TMCPResourceTemplate;
begin
  Result := nil;
  var router := TRouteMatcher.Create;
  try
    for var pair in TemplateRegistry do
      if router.Match(pair.Key, AUri) then
        Exit(pair.Value);
  finally
    router.Free;
  end;
end;

function TMCPResourcesConfig.GetUriParams(const AUri: string): TArray<string>;
begin
  Result := [];
  var matches := TRegEx.Matches(AUri, URI_REGEX);

  for var match in matches do
    Result := Result + [match.Value];
end;

function TMCPResourcesConfig.ParamIsType(AParam: TRttiParameter; ATypes: TypeKindSet): Boolean;
begin
  Result := AParam.ParamType.TypeKind in ATypes;
end;

procedure TMCPResourcesConfig.RegisterUIMethod(AClass: TClass; AMethod:
    TRttiMethod; AAttr: MCPAppUIAttribute);
var
  LRes: TMCPResource;
begin
  if Length(AMethod.GetParameters) > 0 then
    raise EMCPException.CreateFmt('Standard method for resource [%s] cannot have parameters', [AAttr.Name]);

  if not AAttr.Uri.StartsWith('ui://') then
    raise EMCPException.Create('Apps UI uri must use the "ui://" scheme');

  if not ValidUriResource(AAttr.Uri) then
    raise EMCPException.Create('Resource uri cannot have template parameters');

  LRes := TMCPResource.Create;
  try
    LRes.Name := AAttr.Name;
    LRes.Uri := AAttr.Uri;
    LRes.MimeType := 'text/html;profile=mcp-app';
    LRes.Description := AAttr.Description;
    LRes.Classe := AClass;
    LRes.Method := AMethod;

    Registry.Add(LRes.Uri, LRes);
  except
    LRes.Free;
    raise;
  end;
end;

procedure TMCPResourcesConfig.RegisterResMethod(AClass: TClass; AMethod: TRttiMethod; AAttr: MCPResourceAttribute);
var
  LRes: TMCPResource;
begin
  if Length(AMethod.GetParameters) > 0 then
    raise EMCPException.CreateFmt('Standard method for resource [%s] cannot have parameters', [AAttr.Name]);

  if not ValidUriResource(AAttr.Uri) then
    raise EMCPException.Create('Resource uri cannot have template parameters');

  LRes := TMCPResource.Create;
  try
    LRes.Name := AAttr.Name;
    LRes.Uri := AAttr.Uri;
    LRes.MimeType := AAttr.MimeType;
    LRes.Description := AAttr.Description;
    LRes.Classe := AClass;
    LRes.Method := AMethod;
    Registry.Add(LRes.Uri, LRes);
  except
    LRes.Free;
    raise;
  end;

end;

function TMCPResourcesConfig.RegisterClass(AClass: TClass): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LAppAttr: MCPAppUIAttribute;
  LResAttr: MCPResourceAttribute;
  LTplAttr: MCPTemplateAttribute;
begin
  Result := Self;
  LClassType := TRttiUtils.Context.GetType(AClass);

  // Registers all the Resources and UIResources found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LResAttr := TRttiUtils.FindAttribute<MCPResourceAttribute>(LMethod);
    if Assigned(LResAttr) then
    begin
      RegisterResMethod(AClass, LMethod, LResAttr);
      Continue;
    end;

    LTplAttr := TRttiUtils.FindAttribute<MCPTemplateAttribute>(LMethod);
    if Assigned(LTplAttr) then
    begin
      RegisterTplMethod(AClass, LMethod, LTplAttr);
      Continue;
    end;

    LAppAttr := TRttiUtils.FindAttribute<MCPAppUIAttribute>(LMethod);
    if Assigned(LAppAttr) then
    begin
      RegisterUIMethod(AClass, LMethod, LAppAttr);
      Continue;
    end;

  end;
end;

function TMCPResourcesConfig.RegisterResource(AClass: TClass; const AMethod, AUri: string;
  AConfig: TMCPResourceConfigurator): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethod);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create('Resource''s method cannot have parameters');

  LRes := TMCPResource.Create;
  try
    Registry.Add(AUri, LRes);

    AConfig(LRes);
  except
    LRes.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourcesConfig.RegisterScheme(const AScheme, APath: string): TMCPResourcesConfig;
begin
  Schemes.Add(AScheme, APath);
  Result := Self;
end;

function TMCPResourcesConfig.RegisterTemplate(AClass: TClass; const AMethod, AUriTemplate: string;
  AConfig: TMCPTemplateConfigurator): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LRes: TMCPResourceTemplate;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethod);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create('Resource''s method cannot have parameters');

  LRes := TMCPResourceTemplate.Create;
  try
    TemplateRegistry.Add(AUriTemplate, LRes);

    AConfig(LRes);
  except
    LRes.Free;
    raise;
  end;

  Result := Self;
end;

procedure TMCPResourcesConfig.RegisterTplMethod(AClass: TClass; AMethod: TRttiMethod; AAttr: MCPTemplateAttribute);
var
  LTpl: TMCPResourceTemplate;
begin
  var uriParams := GetUriParams(AAttr.UriTemplate);

  if Length(uriParams) = 0 then
    raise EMCPException.Create('Template uri must have parameters: {}');

  if Length(AMethod.GetParameters) <> Length(uriParams) then
    raise EMCPException.CreateFmt('Parameters for template method [%s] must match uri parameters', [AMethod.Name]);

  for var par in AMethod.GetParameters do
  begin
    if not par.HasAttribute<MCPParamAttribute> then
      raise EMCPException.Create('Template method parameters must have the [MCPParam] attribute');

    if not ParamIsType(par, [tkChar, tkWChar, tkString, tkLString, tkWString, tkUString]) then
      raise EMCPException.Create('Parameter type is not supported');
  end;

  LTpl := TMCPResourceTemplate.Create;
  try
    LTpl.Name := AAttr.Name;
    LTpl.UriTemplate := AAttr.UriTemplate;
    LTpl.MimeType := AAttr.MimeType;
    LTpl.Description := AAttr.Description;
    LTpl.Classe := AClass;
    LTpl.Method := AMethod;
    TemplateRegistry.Add(LTpl.UriTemplate, LTpl);
  except
    LTpl.Free;
    raise;
  end;
end;

function TMCPResourcesConfig.RegisterUI(AClass: TClass; const AMethod, AUri:
    string; AConfig: TMCPUIResourceConfigurator): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
  LApp: TMCPResource;
  LAppUI: TUIResourceUI;
  LJSON: TJSONObject;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethod);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create('App''s method cannot have parameters');

  LApp := TMCPResource.Create;
  try
    Registry.Add(AUri, LApp);

    LAppUI := TUIResourceUI.Create;
    try
      AConfig(LApp, LAppUI);
      LJSON := LAppUI.ToJSON;
      if LJSON.Count > 0 then
        LApp.Meta.AddPair('ui', LJSON);
    finally
      LAppUI.Free;
    end;
  except
    LApp.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourcesConfig.RegisterFile(const AFileName, ADescription:
    string; const AMime: string = ''): TMCPResourcesConfig;
const
  RES_CLASS: TClass = TMCPStaticResource;
  RES_METHOD = 'GetResource';
var
  LMime, LExt: string;
  LClassType: TRttiType;
  LRes: TMCPResource;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(RES_CLASS);
  LMethod := LClassType.GetMethod(RES_METHOD);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [RES_METHOD, RES_CLASS.ClassName]);

  LMime := AMime;
  LExt := ExtractFileExt(AFileName);

  if LMime = '' then
    LMime := MimeTypes.MediaByExtension(LExt);
  if LMime = '' then
    raise EMCPException.CreateFmt('No MIME type found for [%s] extension, please specify a MIME type', [LExt]);

  LRes := TMCPResource.Create;
  try
    LRes.FileName := AFileName;
    LRes.Name :=   ExtractFileName(AFileName);

    { TODO -opaolo -c : Customize the URI (URI Schemes?) 16/02/2026 13:01:25 }
    LRes.Uri := 'res://' + StringReplace(LRes.FileName, '\', '/', [rfReplaceAll]);
    LRes.MimeType := LMime;
    LRes.Description := ADescription;
    LRes.Classe := RES_CLASS;
    LRes.Method := LMethod;

    Registry.Add(LRes.Uri, LRes);
  except
    LRes.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourcesConfig.SetBasePath(const APath: string): TMCPResourcesConfig;
begin
  BasePath := APath;
  Result := Self;
end;

function TMCPResourcesConfig.ValidUriResource(const AUri: string): Boolean;
begin
  var matches := TRegEx.Matches(AUri, URI_REGEX);
  Result := matches.Count = 0;
end;

{ TMCPStaticResource }

class procedure TMCPStaticResource.GetResource(AConfig: IMCPConfig; AResource:
    TMCPResource; AResult: TReadResourceResult);
var
  LFileName: string;
  LEncoding: TMimeEncoding;
begin
  if AResource.FileName.IsEmpty then
    raise EMCPException.CreateFmt('No filename specified for static resource [%s]', [AResource.Name]);

  LFileName := TPath.Combine(AConfig.Resources.BasePath, AResource.FileName);

  if not FileExists(LFileName) then
    raise EMCPException.CreateFmt('File [%s] not found for resource [%s]', [LFileName, AResource.Name]);

  { TODO -opaolo -c : check the mime type and serve accordingly 16/02/2026 12:44:47 }
  LEncoding := AConfig.Resources.MimeTypes.EncodingByMedia(AResource.MimeType);

  if LEncoding = TMimeEncoding.Plain then
    AResult.AddTextContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName))
  else
    AResult.AddBlobContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName));

end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
