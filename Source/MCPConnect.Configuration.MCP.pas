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
  System.JSON,

  Neon.Core.Tags,
  Neon.Core.Nullables,
  Neon.Core.Persistence,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources,

  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core;

resourcestring
  SToolNotFoundFmt = 'Tool [%s] not found';
  SMethodInClassNotFoundFmt = 'Method [%s] in class [%s] not found';
  SToolMustBeFunction = 'Tool must be a function';
  SOutputSchemaMustBeObjectFmt = 'outputSchema can only be a JSON object. [%s]';
  SNonConfiguredParamsNotPermitted = 'Non-configured params are not permitted';
  SParamHasNoConfigurationFmt = 'The [%s] parameter has no configuration';
  SNonAnnotatedParamsNotPermitted = 'Non-annotated params are not permitted';
  SConfigResourceNotFoundFmt = 'Resource [%s] not found';
  SStandardMethodNoParamsFmt = 'Standard method for resource [%s] cannot have parameters';
  SAppsUIUriScheme = 'Apps UI uri must use the "ui://" scheme';
  SResourceUriNoTemplateParams = 'Resource uri cannot have template parameters';
  SMethodNotFoundInClassFmt = 'Method [%s] not found in class [%s]';
  SResourceMethodNoParams = 'Resource''s method cannot have parameters';
  STemplateUriMustHaveParams = 'Template uri must have parameters: {}';
  STemplateMethodParamsMismatchFmt = 'Parameters for template method [%s] must match uri parameters';
  STemplateMethodParamsNeedAttribute = 'Template method parameters must have the [MCPParam] attribute';
  SParamTypeNotSupported = 'Parameter type is not supported';
  SAppMethodNoParams = 'App''s method cannot have parameters';
  SMimeTypeNotFoundFmt = 'No MIME type found for [%s] extension, please specify a MIME type';
  SNoFilenameForResourceFmt = 'No filename specified for static resource [%s]';
  SFileNotFoundForResourceFmt = 'File [%s] not found for resource [%s]';
  SPromptNotFoundFmt = 'Prompt [%s] not found';

type
  /// <summary>
  ///   Represents a tools/resources/prompts class registration with its namespace.
  /// </summary>
  TMCPClassInfo = record
    Scope: string;
    MCPClass: TClass;
  end;

  TMCPBaseConfig = class;
  TMCPToolsConfig = class;
  TMCPPromptsConfig = class;
  TMCPResourcesConfig = class;
  TMCPServerConfig = class;
  TMCPSecurityConfig = class;
  TMCPMessageHandlingConfig = class;

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
    ///   Handlers for inbound JSON-RPC messages from the client
    ///   (notifications such as cancelled/initialized, and requests such as logging/setLevel).
    /// </summary>
    function MessageHandling: TMCPMessageHandlingConfig;

    /// <summary>
    ///   Resolves the constructor proxy for an inbound JSON-RPC method name.
    ///   Looks first in the per-server registry populated via
    ///   MessageHandling.RegisterApi, then falls back to the global
    ///   TJRPCRegistry.Instance. Returns False if neither registry has a match.
    /// </summary>
    function GetConstructorProxy(const AName: string; out AProxy: TJRPCConstructorProxy): Boolean;

    /// <summary>
    ///   Security configuration
    /// </summary>
    function Security: TMCPSecurityConfig;

    /// <summary>
    ///   Tools configuration
    /// </summary>
    function Tools: TMCPToolsConfig;

    /// <summary>
    ///   Prompts configuration
    /// </summary>
    function Prompts: TMCPPromptsConfig;

    /// <summary>
    ///   Resources configuration
    /// </summary>
    function Resources: TMCPResourcesConfig;
  end;

  /// <summary>
  ///   Base class for MCP configuration sections.
  /// </summary>
  TMCPBaseConfig = class
  protected
    FConfig: IMCPConfig;
  public
    constructor Create(AConfig: IMCPConfig);
    function SetIcon(const ASrc: string; var AIcon: TMCPIcon): Boolean;

    function BackToMCP: IMCPConfig; virtual;
  end;

  TMCPCapability = (Tools, Resources, Prompts, Tasks, Logging, Completions);
  TMCPCapabilities = set of TMCPCapability;

  /// <summary>
  ///   Configuration for the MCP server details (name, version, etc.).
  /// </summary>
  TMCPServerConfig = class(TMCPBaseConfig)
  public
    IconFolder: string;
    ScopeSeparator: string;
    Name: string;
    Description: string;
    Version: string;
    Capabilities: TServerCapabilities;
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

    /// <summary>
    ///   Sets the server capabilities explicitly. If never called, MCPConnect infers them
    ///   from the registered tools, resources and prompts.
    /// </summary>
    /// <remarks>
    ///   Takes ownership of the passed instance; it will be freed by the config.
    /// </remarks>
    /// <returns>Self for fluent chaining</returns>
    function SetCapabilities(ACapabilities: TServerCapabilities): TMCPServerConfig; overload;

    /// <summary>
    ///   Convenience overload: builds a TServerCapabilities from a simple set of flags.
    /// </summary>
    /// <returns>Self for fluent chaining</returns>
    function SetCapabilities(ACapabilities: TMCPCapabilities): TMCPServerConfig; overload;

    /// <summary>
    ///   Convenience overload: creates a TServerCapabilities and lets the caller configure
    ///   it inline via an anonymous procedure.
    /// </summary>
    /// <returns>Self for fluent chaining</returns>
    function SetCapabilities(AProc: TProc<TServerCapabilities>): TMCPServerConfig; overload;

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

  /// <summary>
  ///   Configuration for handlers invoked when the server receives inbound
  ///   JSON-RPC messages from the client. Covers both notifications
  ///   (fire-and-forget, e.g. notifications/cancelled, notifications/initialized)
  ///   and requests that need a response (e.g. logging/setLevel).
  /// </summary>
  TMCPMessageHandlingConfig = class(TMCPBaseConfig)
  private
    FRegistry: TJRPCRegistry;
    FCancelledProc: TProc<TJRPCContext, TCancelledNotificationParams>;
    FInitializedProc: TProc<TJRPCContext>;
    FSetLogLevelProc: TProc<TJRPCContext, TLogSetLevel>;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    /// <summary>
    ///   Registers a class as an alternative implementation for one or more
    ///   inbound JSON-RPC handlers. The class is inspected via [JRPC]
    ///   attributes the same way the global registry does, but lookups for
    ///   the matching method names will prefer this registration over the
    ///   built-in classes defined in MCPConnect.MCP.Server.Api.
    /// </summary>
    /// <param name="AClass">
    ///   Class decorated with [JRPC('namespace')] and [JRPC('method')] attributes.
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function RegisterApi(AClass: TClass): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Per-server registry used to override the global TJRPCRegistry on a
    ///   method-by-method basis. Exposed read-only; populate it via RegisterApi.
    /// </summary>
    property Registry: TJRPCRegistry read FRegistry;

    /// <summary>
    ///   Registers a handler for the "notifications/cancelled" notification,
    ///   sent by the client when it wants to cancel an in-flight request.
    /// </summary>
    /// <param name="AProc">
    ///   Callback receiving the cancellation parameters (request id and optional reason).
    ///   Pass nil to unregister.
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function OnCancelled(AProc: TProc<TJRPCContext, TCancelledNotificationParams>): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Registers a handler for the "notifications/initialized" notification,
    ///   sent by the client once the initialization handshake is complete.
    /// </summary>
    /// <param name="AProc">Callback invoked after initialization. Pass nil to unregister.</param>
    /// <returns>Self for fluent chaining</returns>
    function OnInitialized(AProc: TProc<TJRPCContext>): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Registers a handler for the "logging/setLevel" request, sent by the
    ///   client to adjust the minimum severity the server should emit.
    /// </summary>
    /// <param name="AProc">
    ///   Callback receiving the requested log level (RFC-5424 severities).
    ///   The server is expected to apply the level synchronously; the response
    ///   is sent automatically with an empty result. Pass nil to unregister.
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function OnSetLogLevel(AProc: TProc<TJRPCContext, TLogSetLevel>): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Read-only access to the registered "notifications/cancelled" handler.
    ///   Used by the framework to dispatch incoming cancellation notifications.
    /// </summary>
    property CancelledProc: TProc<TJRPCContext, TCancelledNotificationParams> read FCancelledProc;

    /// <summary>
    ///   Read-only access to the registered "notifications/initialized" handler.
    ///   Used by the framework to dispatch the post-handshake initialized notification.
    /// </summary>
    property InitializedProc: TProc<TJRPCContext> read FInitializedProc;

    /// <summary>
    ///   Read-only access to the registered "logging/setLevel" handler.
    ///   Used by the framework to apply log level changes requested by the client.
    /// </summary>
    property SetLogLevelProc: TProc<TJRPCContext, TLogSetLevel> read FSetLogLevelProc;
  end;

  /// <summary>
  ///   Configuration for security settings like CORS.
  /// </summary>
  TMCPSecurityConfig = class(TMCPBaseConfig)
  public
    CORS: Boolean;
    AllowedMethods: TArray<string>;
    AllowedOrigins: TArray<string>;

    /// <summary>
    ///   Whether session/auth cookies are sent with the "Secure" attribute (HTTPS only).
    ///   Default: True. Set to False only for plain-HTTP local/dev deployments,
    ///   where browsers would otherwise silently drop the cookie.
    /// </summary>
    CookieSecure: Boolean;
  public
    constructor Create(AConfig: IMCPConfig);

    function SetCORS(AEnable: Boolean): TMCPSecurityConfig;
    function SetAllowedMethods(const AMethods: TArray<string>): TMCPSecurityConfig;
    function SetAllowedOrigins(const AOrigins: TArray<string>): TMCPSecurityConfig;
    function SetCookieSecure(AEnable: Boolean): TMCPSecurityConfig;
  end;

  TMCPParamConfig = class
    ParamName: string;
    Name: string;
    Description: string;
    TagStr: string;
    Tags: TAttributeTags;

    constructor Create;
    destructor Destroy; override;
  end;

  TMCPToolConfig = class
  private
    Parent: TMCPToolsConfig;
  public
    ToolClass: TClass;
    MethodName: string;
    Method: TRttiMethod;
    Name: string;
    Description: string;
    Tags: TAttributeTags;
    Params: TObjectList<TMCPParamConfig>;

    constructor Create(AParent: TMCPToolsConfig);
    destructor Destroy; override;
    function FindParam(const AName: string): TMCPParamConfig;
    function WithParam(const AParamName, AName, ADescription: string; const ATags: string = ''): TMCPToolConfig;
    function EndTool: TMCPToolsConfig;
  end;

  {
  TMCPToolClassConfig = class(TMCPToolBaseConfig)
    ToolClass: TClass;
    Prefix: string;
    function AddTool(const AMethodName, AName, ADescription: string; const ATags: string = ''): TMCPToolConfig;
  end;
  }

  /// <summary>
  ///   Configuration for MCP tools registration and discovery.
  /// </summary>
  TMCPToolsConfig = class(TMCPBaseConfig)
  private
    Configs: TObjectList<TMCPToolConfig>;
    procedure WriteInputSchema(ATool: TMCPTool; AConfig: TMCPToolConfig);
    procedure WriteOutputSchema(ATool: TMCPTool);
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray); overload;
    procedure WriteParams(AConfig: TMCPToolConfig; AProps: TJSONObject; ARequired: TJSONArray); overload;

    procedure WriteTool(ATool: TMCPTool; ATags: TAttributeTags);

    procedure EndTool(AConfig: TMCPToolConfig);
  public
    Registry: TMCPToolRegistry;
    NeonConfig: INeonConfiguration;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPToolsConfig;
    function RegisterTool(AClass: TClass; const AMethodName, AName, ADescription: string; const ATags: string = ''): TMCPToolConfig;
    function SetSchemaNeonConfig(ANeonConfig: INeonConfiguration): TMCPToolsConfig;

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
    
    function BackToMCP: IMCPConfig; override;
  end;

  TMCPPromptsConfig = class(TMCPBaseConfig)
  private
    procedure WritePrompt(APrompt: TMCPPrompt; APromptAttr: MCPPromptAttribute);
  public
    Registry: TMCPPromptRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPPromptsConfig;

    /// <summary>
    ///   Creates an instance of a class by namespace.
    ///   Used internally by the framework to instantiate tools.
    /// </summary>
    /// <param name="ANamespace">Namespace of the tool class to instantiate</param>
    /// <returns>New instance of the tool class</returns>
    /// <exception cref="EJRPCException">Raised if namespace not found</exception>
    function CreateInstance(const APrompt: string): TObject;

    function ListComplete: TListPromptsResult;
  end;

  /// <summary>
  ///   Helper class for serving static resources.
  /// </summary>
  TMCPStaticResource = class
  public
    class procedure GetResource(AConfig: IMCPConfig; AResource: TMCPResource; AResult: TReadResourceResult);
  end;

  /// <summary>
  ///   Configuration for MCP resources, templates, and UI.
  /// </summary>
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

  /// <summary>
  ///   Main implementation of IMCPConfig, aggregating all configuration sections.
  /// </summary>
  [Implements(IMCPConfig)]
  TMCPConfig = class(TJRPCConfiguration, IMCPConfig)
  private
    FServer: TMCPServerConfig;
    FSecurity: TMCPSecurityConfig;
    FMessageHandling: TMCPMessageHandlingConfig;

    FTools: TMCPToolsConfig;
    FPrompts: TMCPPromptsConfig;
    FResources: TMCPResourcesConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;

    { IMCPConfig }
    function Server: TMCPServerConfig;
    function Security: TMCPSecurityConfig;
    function Tools: TMCPToolsConfig;
    function Prompts: TMCPPromptsConfig;
    function Resources: TMCPResourcesConfig;
    function MessageHandling: TMCPMessageHandlingConfig;
    function GetConstructorProxy(const AName: string; out AProxy: TJRPCConstructorProxy): Boolean;
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
  FSecurity := TMCPSecurityConfig.Create(Self);
  FMessageHandling := TMCPMessageHandlingConfig.Create(Self);

  FTools := TMCPToolsConfig.Create(Self);
  FPrompts := TMCPPromptsConfig.Create(Self);
  FResources := TMCPResourcesConfig.Create(Self);
end;

destructor TMCPConfig.Destroy;
begin
  FPrompts.Free;
  FResources.Free;
  FTools.Free;

  FMessageHandling.Free;
  FSecurity.Free;
  FServer.Free;
  inherited;
end;

function TMCPConfig.GetConstructorProxy(const AName: string; out AProxy: TJRPCConstructorProxy): Boolean;
begin
  Result := FMessageHandling.Registry.GetConstructorProxy(AName, AProxy);
  if not Result then
    Result := TJRPCRegistry.Instance.GetConstructorProxy(AName, AProxy);
end;

function TMCPConfig.MessageHandling: TMCPMessageHandlingConfig;
begin
  Result := FMessageHandling;
end;

function TMCPConfig.Prompts: TMCPPromptsConfig;
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

function TMCPConfig.Security: TMCPSecurityConfig;
begin
  Result := FSecurity;
end;

function TMCPConfig.Server: TMCPServerConfig;
begin
  Result := FServer;
end;

function TMCPToolsConfig.BackToMCP: IMCPConfig;
begin
  Configs.Clear;
  inherited;  
end;

constructor TMCPToolsConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  Registry := TMCPToolRegistry.Create([doOwnsValues]);
  Configs := TObjectList<TMCPToolConfig>.Create(True);
  NeonConfig := TNeonConfiguration.Camel;
end;

function TMCPToolsConfig.CreateInstance(const ATool: string): TObject;
var
  LTool: TMCPTool;
begin
  if not Registry.TryGetValue(ATool, LTool) then
    raise EMCPException.CreateFmt(SToolNotFoundFmt, [ATool]);

  Result := TRttiUtils.CreateInstance(LTool.Classe);
end;

destructor TMCPToolsConfig.Destroy;
begin
  Registry.Free;
  Configs.Free;
  inherited;
end;

procedure TMCPToolsConfig.EndTool(AConfig: TMCPToolConfig);
var
  LTool: TMCPTool;
begin
  LTool := TMCPTool.Create;
  try
    LTool.Name := AConfig.Name;
    LTool.Description := AConfig.Description;
    LTool.Classe := AConfig.ToolClass;
    LTool.Method := AConfig.Method;

    WriteTool(LTool, AConfig.Tags);

    WriteInputSchema(LTool, AConfig);

    if AConfig.Tags.Exists('structured') then
      WriteOutputSchema(LTool);

    Registry.Add(LTool.Name, LTool);
  except
    LTool.Free;
    raise;
  end;
end;

function TMCPToolsConfig.ListEnabled: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in Registry do
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

    LTool := TMCPTool.Create;
    try
      LTool.Name := LScope + LToolAttr.Name;
      LTool.Description := LToolAttr.Description;
      LTool.Classe := AClass;
      LTool.Method := LMethod;

      LAppAttr := TRttiUtils.FindAttribute<MCPAppAttribute>(LMethod);
      if Assigned(LAppAttr) then
        LToolAttr.Tags.TagMap.AddOrSetValue('app', LAppAttr.UI);

      WriteTool(LTool, LToolAttr.Tags);

      WriteInputSchema(LTool, nil);

      if LToolAttr.Tags.Exists('structured') then
        WriteOutputSchema(LTool);

      Registry.Add(LTool.Name, LTool);
    except
      LTool.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPToolsConfig.RegisterTool(AClass: TClass;
  const AMethodName, AName, ADescription, ATags: string): TMCPToolConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethodName);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt(SMethodInClassNotFoundFmt, [AMethodName, AClass.ClassName]);

  Result := TMCPToolConfig.Create(Self);
  Configs.Add(Result);

  Result.ToolClass := AClass;
  Result.MethodName := AMethodName;
  Result.Method := LMethod;

  Result.Name := AName;
  Result.Description := ADescription;
  Result.Tags.TagMap.Clear;
  Result.Tags.Parse(ATags);
end;

function TMCPToolsConfig.ListComplete: TListToolsResult;
begin
  Result := TListToolsResult.Create;
  for var pair in Registry do
    Result.Tools.Add(pair.Value);
end;

procedure TMCPToolsConfig.FilterList(AList: TListToolsResult; AFilter: TMCPToolFilterFunc);
begin
  for var pair in Registry do
    if AFilter(pair.Value) then
      AList.Tools.Add(pair.Value);
end;

function TMCPToolsConfig.SetSchemaNeonConfig(ANeonConfig: INeonConfiguration): TMCPToolsConfig;
begin
  NeonConfig := ANeonConfig;
  Result := Self;
end;

procedure TMCPToolsConfig.WriteTool(ATool: TMCPTool; ATags: TAttributeTags);
var
  LIcon: TMCPIcon;
begin
  if SetIcon(ATags.GetValueAs<string>('icon'), LIcon) then
    ATool.Icons := ATool.Icons + [LIcon];

  ATool.Category := ATags.GetValueAs<string>('category');
  ATool.Disabled := ATags.GetBoolValue('disabled');

  if ATags.Exists('app') then
    ATool.UI.ResourceUri := ATags.GetValueAs<string>('app');

  if ATags.Exists('readonly') then
    ATool.Annotations.ReadOnlyHint := ATags.GetBoolValue('readonly');
  if ATags.Exists('destructive') then
    ATool.Annotations.DestructiveHint := ATags.GetBoolValue('destructive');
  if ATags.Exists('idempotent') then
    ATool.Annotations.IdempotentHint := ATags.GetBoolValue('idempotent');
  if ATags.Exists('openworld') then
    ATool.Annotations.OpenWorldHint := ATags.GetBoolValue('openworld');
end;

procedure TMCPToolsConfig.WriteInputSchema(ATool: TMCPTool; AConfig: TMCPToolConfig);
var
  LProps, LInputSchema: TJSONObject;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    if not Assigned(AConfig) then
      WriteParams(ATool.Method, LProps, LRequired)
    else
      WriteParams(AConfig, LProps, LRequired);

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

procedure TMCPToolsConfig.WriteOutputSchema(ATool: TMCPTool);
var
  LSchemaType: TJSONPair;
  LJSONObj: TJSONObject;
  LType: TRttiType;
begin
  LType := ATool.Method.ReturnType;
  if not Assigned(LType) then
    raise EMCPException.Create(SToolMustBeFunction);

  LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LType, NeonConfig);

  // outputSchema and structuredContent are (for now) limited to a JSON Object
  // See: https://github.com/modelcontextprotocol/php-sdk/issues/357
  LSchemaType := LJSONObj.Get('type');
  if not (LSchemaType.JsonValue.Value = 'object') then
  begin
    LJSONObj.Free;
    raise EMCPException.CreateFmt(SOutputSchemaMustBeObjectFmt, [ATool.Name]);
  end;

  ATool.ExchangeOutputSchema(LJSONObj);
end;

procedure TMCPToolsConfig.WriteParams(AConfig: TMCPToolConfig;
  AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
begin
  if AConfig.Params.Count <> Length(AConfig.Method.GetParameters) then
    raise EJRPCException.Create(SNonConfiguredParamsNotPermitted);
  
  for LParam in AConfig.Method.GetParameters do
  begin
    var par := AConfig.FindParam(LParam.Name);
    if not Assigned(par) then
      raise EJRPCException.CreateFmt(SParamHasNoConfigurationFmt, [LParam.Name]);
      
    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, NeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(par.Description));
    AProps.AddPair(par.Name, LJSONObj);
    ARequired.Add(par.Name);
  end;

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
        raise EJRPCException.Create(SNonAnnotatedParamsNotPermitted);

    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, NeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(LAttr.Description));
    AProps.AddPair(LAttr.Name, LJSONObj);
    ARequired.Add(LAttr.Name);
  end;
end;

constructor TMCPServerConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  WriterRegistry := TMCPWriterRegistry.Create;

  IconFolder := '';
  ScopeSeparator := '_';  // Default separator (MCP requires ^[a-zA-Z0-9_-]{1,64}$)
  Capabilities := nil;
end;

destructor TMCPServerConfig.Destroy;
begin
  Capabilities.Free;
  WriterRegistry.Free;
  inherited;
end;

function TMCPServerConfig.RegisterWriter(AClass: TCustomWriterClass): TMCPServerConfig;
begin
  WriterRegistry.RegisterWriter(AClass);
  Result := Self;
end;

function TMCPServerConfig.SetCapabilities(ACapabilities: TServerCapabilities): TMCPServerConfig;
begin
  if Assigned(Capabilities) then
    Capabilities.Free;
  Capabilities := ACapabilities;
  Result := Self;
end;

function TMCPServerConfig.SetCapabilities(
  ACapabilities: TMCPCapabilities): TMCPServerConfig;
begin
  if Assigned(Capabilities) then
    Capabilities.Free;
  Capabilities := TServerCapabilities.Create;

  if TMCPCapability.Tools in ACapabilities then
    Capabilities.Tools.ListChanged := False;
  if TMCPCapability.Resources in ACapabilities then
  begin
    Capabilities.Resources.ListChanged := False;
    Capabilities.Resources.Subscribe := False;
  end;
  if TMCPCapability.Prompts in ACapabilities then
    Capabilities.Prompts.ListChanged := False;

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

  if FConfig.Server.IconFolder.IsEmpty then
    Exit(False);

  AIcon.FromFile(TPath.Combine(FConfig.Server.IconFolder, ASrc));
  Exit(True);
end;

function TMCPBaseConfig.BackToMCP: IMCPConfig;
begin
  Result := FConfig;
end;

constructor TMCPBaseConfig.Create(AConfig: IMCPConfig);
begin
  inherited Create;
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
    raise EMCPException.CreateFmt(SConfigResourceNotFoundFmt, [AUri]);

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
    raise EMCPException.CreateFmt(SStandardMethodNoParamsFmt, [AAttr.Name]);

  if not AAttr.Uri.StartsWith('ui://') then
    raise EMCPException.Create(SAppsUIUriScheme);

  if not ValidUriResource(AAttr.Uri) then
    raise EMCPException.Create(SResourceUriNoTemplateParams);

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
    raise EMCPException.CreateFmt(SStandardMethodNoParamsFmt, [AAttr.Name]);

  if not ValidUriResource(AAttr.Uri) then
    raise EMCPException.Create(SResourceUriNoTemplateParams);

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
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create(SResourceMethodNoParams);

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
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create(SResourceMethodNoParams);

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
    raise EMCPException.Create(STemplateUriMustHaveParams);

  if Length(AMethod.GetParameters) <> Length(uriParams) then
    raise EMCPException.CreateFmt(STemplateMethodParamsMismatchFmt, [AMethod.Name]);

  for var par in AMethod.GetParameters do
  begin
    if not par.HasAttribute<MCPParamAttribute> then
      raise EMCPException.Create(STemplateMethodParamsNeedAttribute);

    if not ParamIsType(par, [tkChar, tkWChar, tkString, tkLString, tkWString, tkUString]) then
      raise EMCPException.Create(SParamTypeNotSupported);
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
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethod, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create(SAppMethodNoParams);

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
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [RES_METHOD, RES_CLASS.ClassName]);

  LMime := AMime;
  LExt := ExtractFileExt(AFileName);

  if LMime = '' then
    LMime := MimeTypes.MediaByExtension(LExt);
  if LMime = '' then
    raise EMCPException.CreateFmt(SMimeTypeNotFoundFmt, [LExt]);

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
    raise EMCPException.CreateFmt(SNoFilenameForResourceFmt, [AResource.Name]);

  LFileName := TPath.Combine(AConfig.Resources.BasePath, AResource.FileName);

  if not FileExists(LFileName) then
    raise EMCPException.CreateFmt(SFileNotFoundForResourceFmt, [LFileName, AResource.Name]);

  { TODO -opaolo -c : check the mime type and serve accordingly 16/02/2026 12:44:47 }
  LEncoding := AConfig.Resources.MimeTypes.EncodingByMedia(AResource.MimeType);

  if LEncoding = TMimeEncoding.Plain then
    AResult.AddTextContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName))
  else
    AResult.AddBlobContent(AResource.Uri, AResource.MimeType, TFile.ReadAllText(LFileName));

end;

{ TMCPPromptsConfig }

constructor TMCPPromptsConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  Registry := TMCPPromptRegistry.Create([doOwnsValues]);
end;

function TMCPPromptsConfig.CreateInstance(const APrompt: string): TObject;
var
  LPrompt: TMCPPrompt;
begin
  if not Registry.TryGetValue(APrompt, LPrompt) then
    raise EMCPException.CreateFmt(SPromptNotFoundFmt, [APrompt]);

  Result := TRttiUtils.CreateInstance(LPrompt.Classe);
end;

destructor TMCPPromptsConfig.Destroy;
begin
  Registry.Free;
  inherited;
end;

function TMCPPromptsConfig.ListComplete: TListPromptsResult;
begin
  Result := TListPromptsResult.Create;
  for var pair in Registry do
    Result.Prompts.Add(pair.Value);
end;

function TMCPPromptsConfig.RegisterClass(AClass: TClass): TMCPPromptsConfig;
var
  LScope: string;
  LClassType: TRttiType;
  LPromptAttr: MCPPromptAttribute;
  LScopeAttr: MCPScopeAttribute;
  LPrompt: TMCPPrompt;
begin
  LScope := '';
  LClassType := TRttiUtils.Context.GetType(AClass);
  LScopeAttr := TRttiUtils.FindAttribute<MCPScopeAttribute>(LClassType);
  if Assigned(LScopeAttr) then
    LScope := LScopeAttr.Name + FConfig.Server.ScopeSeparator;

  // Registers all the prompts found in AClass
  for var LMethod in LClassType.GetMethods do
  begin
    LPromptAttr := TRttiUtils.FindAttribute<MCPPromptAttribute>(LMethod);
    if not Assigned(LPromptAttr) then
      Continue;

    LPrompt := TMCPPrompt.Create;
    try
      LPrompt.Name := LScope + LPromptAttr.Name;
      LPrompt.Title := LPromptAttr.Title;
      LPrompt.Description := LPromptAttr.Description;
      LPrompt.Classe := AClass;
      LPrompt.Method := LMethod;

      WritePrompt(LPrompt, LPromptAttr);

      for var LParam in LMethod.GetParameters do
      begin
        var LAttr := LParam.GetAttribute<MCPArgumentAttribute>;
          if not Assigned(LAttr) then
            raise EJRPCException.Create(SNonAnnotatedParamsNotPermitted);

        var LArg := TPromptArgument.New(LAttr.Name, LAttr.Description);
        if LAttr.Tags.Exists('required') then
          LArg.Required := LAttr.Tags.GetBoolValue('required');

        LPrompt.Arguments := LPrompt.Arguments + [LArg];
      end;

      Registry.Add(LPrompt.Name, LPrompt);
    except
      LPrompt.Free;
      raise;
    end;
  end;
  Result := Self;
end;

procedure TMCPPromptsConfig.WritePrompt(APrompt: TMCPPrompt; APromptAttr: MCPPromptAttribute);
var
  LIcon: TMCPIcon;
begin
  if SetIcon(APromptAttr.Tags.GetValueAs<string>('icon'), LIcon) then
    APrompt.Icons := APrompt.Icons + [LIcon];

  APrompt.Category := APromptAttr.Tags.GetValueAs<string>('category');
  APrompt.Disabled := APromptAttr.Tags.GetBoolValue('disabled');
end;


{ TMCPSecurityConfig }

constructor TMCPSecurityConfig.Create(AConfig: IMCPConfig);
begin
  inherited Create(AConfig);
  AllowedMethods := ['POST'];
  CookieSecure := True;
end;

function TMCPSecurityConfig.SetCookieSecure(AEnable: Boolean): TMCPSecurityConfig;
begin
  CookieSecure := AEnable;
  Result := Self;
end;

function TMCPSecurityConfig.SetAllowedMethods(const AMethods: TArray<string>): TMCPSecurityConfig;
begin
  AllowedMethods := AMethods;
  Result := Self;
end;

function TMCPSecurityConfig.SetAllowedOrigins(const AOrigins: TArray<string>): TMCPSecurityConfig;
begin
  AllowedOrigins := AOrigins;
  Result := Self;
end;

function TMCPSecurityConfig.SetCORS(AEnable: Boolean): TMCPSecurityConfig;
begin
  CORS := AEnable;
  Result := Self;
end;

{ TMCPMessageHandlingConfig }

constructor TMCPMessageHandlingConfig.Create(AConfig: IMCPConfig);
begin
  inherited Create(AConfig);
  FRegistry := TJRPCRegistry.Create;
end;

destructor TMCPMessageHandlingConfig.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TMCPMessageHandlingConfig.RegisterApi(AClass: TClass): TMCPMessageHandlingConfig;
begin
  FRegistry.RegisterClass(AClass, MCPNeonConfig);
  Result := Self;
end;

function TMCPMessageHandlingConfig.OnCancelled(AProc: TProc<TJRPCContext, TCancelledNotificationParams>): TMCPMessageHandlingConfig;
begin
  FCancelledProc := AProc;
  Result := Self;
end;

function TMCPMessageHandlingConfig.OnInitialized(AProc: TProc<TJRPCContext>): TMCPMessageHandlingConfig;
begin
  FInitializedProc := AProc;
  Result := Self;
end;

function TMCPMessageHandlingConfig.OnSetLogLevel(AProc: TProc<TJRPCContext, TLogSetLevel>): TMCPMessageHandlingConfig;
begin
  FSetLogLevelProc := AProc;
  Result := Self;
end;

function TMCPServerConfig.SetCapabilities(
  AProc: TProc<TServerCapabilities>): TMCPServerConfig;
begin
  if Assigned(Capabilities) then
    Capabilities.Free;
  Capabilities := TServerCapabilities.Create;

  AProc(Capabilities);
  Result := Self;
end;

{ TMCPToolConfig }

constructor TMCPToolConfig.Create(AParent: TMCPToolsConfig);
begin
  Parent := AParent;
  Params := TObjectList<TMCPParamConfig>.Create(True);
  Tags := TAttributeTags.Create();
end;

destructor TMCPToolConfig.Destroy;
begin
  Params.Free;
  Tags.Free;
  inherited;
end;

function TMCPToolConfig.EndTool: TMCPToolsConfig;
begin
  Parent.EndTool(Self);
  Result := Parent;
end;

function TMCPToolConfig.FindParam(const AName: string): TMCPParamConfig;
begin
  Result := nil;
  for var par in Params do
    if SameText(par.ParamName, AName) then
      Exit(par);
end;

function TMCPToolConfig.WithParam(const AParamName, AName, ADescription: string;
  const ATags: string): TMCPToolConfig;
begin
  var p := TMCPParamConfig.Create();
  p.ParamName := AParamName;
  p.Name := AName;
  p.Description := ADescription;
  p.Tags.Parse(ATags);
  Params.Add(p);

  Result := Self;
end;

{ TMCPParamConfig }

constructor TMCPParamConfig.Create;
begin
  Tags := TAttributeTags.Create();
end;

destructor TMCPParamConfig.Destroy;
begin
  Tags.Free;
  inherited;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
