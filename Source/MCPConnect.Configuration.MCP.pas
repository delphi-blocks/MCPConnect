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
  System.Classes, System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Rtti,

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
  TMCPToolConfig = class;
  TMCPServerConfig = class;
  TMCPResourceConfig = class;
  
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
    function Tools: TMCPToolConfig;

    /// <summary>
    ///   Resources configuration
    /// </summary>
    function Resources: TMCPResourceConfig;

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

    function BackToMCP: IMCPConfig;
  end;

  TMCPCapability = (Tools, Resources, Prompts, Tasks, Logging, Completions);
  TMCPCapabilities = set of TMCPCapability;

  TMCPServerConfig = class(TMCPBaseConfig)
  public
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


  TMCPToolConfig = class(TMCPBaseConfig)
  private
    procedure WriteInputSchema(ATool: TMCPTool);
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
    procedure WriteTool(ATool: TMCPTool; AAttr: MCPToolAttribute);
  public
    ToolRegistry: TMCPToolRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPToolConfig;

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


  TMCPResourceConfig = class(TMCPBaseConfig)
  public
    Registry: TMCPResourceRegistry;
    Schemes: TDictionary<string, string>;
    BasePath: string;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function SetBasePath(const APath: string): TMCPResourceConfig;
    function RegisterScheme(const AScheme, APath: string): TMCPResourceConfig;

    function RegisterClass(AClass: TClass): TMCPResourceConfig;
    function RegisterStatic(const AFileName, AMime, ADescription: string): TMCPResourceConfig;
    //function RegisterStatic(const AScheme, APath, AMime, ADescription: string): TMCPResourceConfig;

    function RegisterResource(AClass: TClass; const AMethod, AUri: string; AConfig: TMCPResourceConfigurator): TMCPResourceConfig;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const AUri: string): TObject;

    function ListEnabled: TListResourcesResult;
  end;


  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FServer: TMCPServerConfig;
    FTools: TMCPToolConfig;
    FResources: TMCPResourceConfig;
    FTemplates: TMCPListConfig;
    FPrompts: TMCPListConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;

    { IMCPConfig }
    function Server: TMCPServerConfig;
    function Tools: TMCPToolConfig;
    function Resources: TMCPResourceConfig;
    function Templates: TMCPListConfig;
    function Prompts: TMCPListConfig;
  end;


implementation

uses
  System.IOUtils,
  Neon.Core.Utils,
  Neon.Core.Persistence.JSON.Schema;

constructor TMCPConfig.Create(AApp: IJRPCApplication);
begin
  inherited;

  FServer := TMCPServerConfig.Create(Self);
  FTools := TMCPToolConfig.Create(Self);
  FResources := TMCPResourceConfig.Create(Self);
  FTemplates := TMCPListConfig.Create(Self);
  FPrompts := TMCPListConfig.Create(Self);
end;

destructor TMCPConfig.Destroy;
begin
  FPrompts.Free;
  FTemplates.Free;
  FResources.Free;
  FTools.Free;
  FServer.Free;

  inherited;
end;

function TMCPConfig.Prompts: TMCPListConfig;
begin
  Result := FPrompts;
end;

function TMCPConfig.Resources: TMCPResourceConfig;
begin
  Result := FResources;
end;

function TMCPConfig.Templates: TMCPListConfig;
begin
  Result := FTemplates;
end;

function TMCPConfig.Tools: TMCPToolConfig;
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

constructor TMCPToolConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  ToolRegistry := TMCPToolRegistry.Create([doOwnsValues]);
end;

function TMCPToolConfig.CreateInstance(const ATool: string): TObject;
var
  LTool: TMCPTool;
begin
  if not ToolRegistry.TryGetValue(ATool, LTool) then
    raise EMCPException.CreateFmt('Tool [%s] not found', [ATool]);

  Result := TRttiUtils.CreateInstance(LTool.Classe);
end;

destructor TMCPToolConfig.Destroy;
begin
  ToolRegistry.Free;

  inherited;
end;

function TMCPToolConfig.ListEnabled: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    if not pair.Value.Disabled then
      Result.Tools.Add(pair.Value);
end;

function TMCPToolConfig.RegisterClass(AClass: TClass): TMCPToolConfig;
var
  LScope: string;
  LClassType: TRttiType;
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

    LTool := TMCPTool.Create;
    try
      LTool.Name := LScope + LToolAttr.Name;
      LTool.Description := LToolAttr.Description;
      LTool.Classe := AClass;
      LTool.Method := LMethod;

      WriteTool(LTool, LToolAttr);


      ToolRegistry.Add(LTool.Name, LTool);
    except
      LTool.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPToolConfig.ListComplete: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in ToolRegistry do
    Result.Tools.Add(pair.Value);
end;

procedure TMCPToolConfig.FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
begin
  for var pair in ToolRegistry do
    if AFilter(pair.Value) then
      AList.Tools.Add(pair.Value);
end;

procedure TMCPToolConfig.WriteTool(ATool: TMCPTool; AAttr: MCPToolAttribute);
begin
  ATool.AddIcon(AAttr.Tags.GetValueAs<string>('icon'));
  ATool.Category := AAttr.Tags.GetValueAs<string>('category');
  ATool.Disabled := AAttr.Tags.GetBoolValue('disabled');

  if AAttr.Tags.Exists('meta.ui') then
    ATool.UI.ResourceUri := AAttr.Tags.GetValueAs<string>('meta.ui');

  if AAttr.Tags.Exists('readonly') then
    ATool.Annotations.ReadOnlyHint := AAttr.Tags.GetBoolValue('readonly');
  if AAttr.Tags.Exists('destructive') then
    ATool.Annotations.DestructiveHint := AAttr.Tags.GetBoolValue('destructive');
  if AAttr.Tags.Exists('idempotent') then
    ATool.Annotations.IdempotentHint := AAttr.Tags.GetBoolValue('idempotent');
  if AAttr.Tags.Exists('openworld') then
    ATool.Annotations.OpenWorldHint := AAttr.Tags.GetBoolValue('openworld');

  WriteInputSchema(ATool);
end;

procedure TMCPToolConfig.WriteInputSchema(ATool: TMCPTool);
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

procedure TMCPToolConfig.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
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

function TMCPBaseConfig.BackToMCP: IMCPConfig;
begin
  Result := FConfig;
end;

constructor TMCPBaseConfig.Create(AConfig: IMCPConfig);
begin
  FConfig := AConfig;
end;

{ TMCPResourceConfig }

constructor TMCPResourceConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  BasePath := GetCurrentDir + '\data';
  ForceDirectories(BasePath);
  Registry := TMCPResourceRegistry.Create([doOwnsValues]);
end;

function TMCPResourceConfig.CreateInstance(const AUri: string): TObject;
var
  LResource: TMCPResource;
begin
  if not Registry.TryGetValue(AUri, LResource) then
    raise EMCPException.CreateFmt('Resource [%s] not found', [AUri]);

  Result := TRttiUtils.CreateInstance(LResource.Classe);
end;

destructor TMCPResourceConfig.Destroy;
begin
  Registry.Free;
  inherited;
end;

function TMCPResourceConfig.ListEnabled: TListResourcesResult;
begin
  Result := TListResourcesResult.Create;
  for var pair in Registry do
    if not pair.Value.Disabled then
      Result.Resources.Add(pair.Value);
end;

function TMCPResourceConfig.RegisterClass(AClass: TClass): TMCPResourceConfig;
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LResAttr: MCPResourceAttribute;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);

  // Registers all the resources found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LResAttr := TRttiUtils.FindAttribute<MCPResourceAttribute>(LMethod);
    if not Assigned(LResAttr) then
      Continue;

    if Length(LMethod.GetParameters) > 0 then
      raise EMCPException.CreateFmt('Standard method for resource [%s] cannot have parameters', [LResAttr.Name]);

    LRes := TMCPResource.Create;
    try
      LRes.Name := LResAttr.Name;
      LRes.Uri := LResAttr.Uri;
      LRes.MimeType := LResAttr.MimeType;
      LRes.Description := LResAttr.Description;
      LRes.Classe := AClass;
      LRes.Method := LMethod;

      Registry.Add(LRes.Uri, LRes);
    except
      LRes.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPResourceConfig.RegisterResource(AClass: TClass; const AMethod, AUri: string;
  AConfig: TMCPResourceConfigurator): TMCPResourceConfig;
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

function TMCPResourceConfig.RegisterScheme(const AScheme, APath: string): TMCPResourceConfig;
begin
  Schemes.Add(AScheme, APath);
end;

function TMCPResourceConfig.RegisterStatic(const AFileName, AMime, ADescription: string): TMCPResourceConfig;
const
  RES_CLASS: TClass = TMCPStaticResource;
  RES_METHOD = 'GetResource';
var
  LClassType: TRttiType;
  LRes: TMCPResource;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(RES_CLASS);
  LMethod := LClassType.GetMethod(RES_METHOD);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt('Method [%s] not found in class [%s]', [RES_METHOD, RES_CLASS.ClassName]);

  LRes := TMCPResource.Create;
  try
    LRes.FileName := AFileName;
    LRes.Name :=   ExtractFileName(AFileName);

    { TODO -opaolo -c : Customize the URI (URI Schemes?) 16/02/2026 13:01:25 }
    LRes.Uri := 'res://' + LRes.Name;
    LRes.MimeType := AMime;
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

function TMCPResourceConfig.SetBasePath(const APath: string): TMCPResourceConfig;
begin
  BasePath := APath;
  Result := Self;
end;

{ TMCPStaticResource }

class procedure TMCPStaticResource.GetResource(AConfig: IMCPConfig; AResource:
    TMCPResource; AResult: TReadResourceResult);
var
  LFileName: string;
begin
  if AResource.FileName.IsEmpty then
    raise EMCPException.CreateFmt('No filename specified for static resource [%s]', [AResource.Name]);

  LFileName := TPath.Combine(AConfig.Resources.BasePath, AResource.FileName);

  if not FileExists(LFileName) then
    raise EMCPException.CreateFmt('File [%s] not found for resource [%s]', [LFileName, AResource.Name]);

  { TODO -opaolo -c : check the mime type and serve accordingly 16/02/2026 12:44:47 }

  AResult.AddTextContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName));
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
