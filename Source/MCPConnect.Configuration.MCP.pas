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
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Completion,

  MCPConnect.MCP.Attributes,
  MCPConnect.Content.Writers,
  MCPConnect.Configuration.Core;

resourcestring
  SToolNameInvalidFmt = 'Tool name [%s] is not valid: it must be 1 to %d characters of A-Z, a-z, 0-9, underscore, hyphen or dot';
  SToolNotFoundFmt = 'Tool [%s] not found';
  SToolParamNotFoundFmt = 'Param [%s] for Tool [%s] not found';
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
  SNonConfiguredTemplateParamsNotPermitted = 'Non-configured template params are not permitted';
  STemplateParamNameNotInUriFmt = 'Param name [%s] does not match any placeholder in the template uri';
  SPromptParamNotFoundFmt = 'Param [%s] for Prompt [%s] not found';
  SNonConfiguredPromptParamsNotPermitted = 'Non-configured prompt params are not permitted';
  STemplateNotFoundFmt = 'Template [%s] not found';
  SCompletionProviderNotFoundFmt = 'Completion provider for argument [%s] of [%s] not found';
  SCompletionProviderDuplicateFmt = 'A completion provider for argument [%s] of [%s] is already registered';
  SCompletionRefTargetEmpty = 'Completion reference must name a prompt or a uri';
  SCompletionMethodSignatureFmt = 'Completion method [%s] must take no parameter, a string, or a string and a TMCPCompletionContext';
  SCompletionResultTypeFmt = 'Completion method [%s] must return a TArray<string>, a TStrings or a TCompleteResult';

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
  TMCPCompletionsConfig = class;

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

    /// <summary>
    ///   Argument-completion configuration (the "completion/complete" request)
    /// </summary>
    function Completions: TMCPCompletionsConfig;
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
    ///   Separator string. It becomes part of the tool name, so it may only use
    ///   the characters a name may: a-zA-Z0-9, underscore, hyphen and dot.
    ///   Common values: '_' (default), '-', '.'
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    /// <remarks>
    ///   Tool names must match ^[a-zA-Z0-9_.-]{1,128}$ and are checked when a
    ///   tool is registered, scope prefix included, so a separator such as ':'
    ///   raises there rather than being rejected later by the client.
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
    //FCancelledProc: TProc<TJRPCContext, TCancelledNotificationParams>;
    FInitializedProc: TProc<TJRPCContext>;
    FSetLogLevelProc: TProc<TJRPCContext, TMCPLogLevel>;
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
    ///   Registers a handler for the "notifications/initialized" notification,
    ///   sent by the client once the initialization handshake is complete.
    /// </summary>
    /// <param name="AProc">Callback invoked after initialization. Pass nil to unregister.</param>
    /// <returns>Self for fluent chaining</returns>
    function OnInitialized(AProc: TProc<TJRPCContext>): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Registers a handler for the log level a client asks the server to emit
    ///   at.
    /// </summary>
    /// <remarks>
    ///   2026-07-28 removed the "logging/setLevel" request: the level now
    ///   arrives per request, in "_meta.io.modelcontextprotocol/logLevel", and
    ///   a server that is sent no level MUST NOT emit log notifications for
    ///   that request.
    /// </remarks>
    /// <param name="AProc">
    ///   Callback receiving the requested log level (RFC-5424 severities).
    ///   Pass nil to unregister.
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function OnSetLogLevel(AProc: TProc<TJRPCContext, TMCPLogLevel>): TMCPMessageHandlingConfig;

    /// <summary>
    ///   Read-only access to the registered "notifications/initialized" handler.
    ///   Used by the framework to dispatch the post-handshake initialized notification.
    /// </summary>
    property InitializedProc: TProc<TJRPCContext> read FInitializedProc;

    /// <summary>
    ///   Read-only access to the registered "logging/setLevel" handler.
    ///   Used by the framework to apply log level changes requested by the client.
    /// </summary>
    property SetLogLevelProc: TProc<TJRPCContext, TMCPLogLevel> read FSetLogLevelProc;
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
    ExposeHeaders: TArray<string>;
    RequireOrigin: Boolean;
  public
    constructor Create(AConfig: IMCPConfig);

    function SetCORS(AEnable: Boolean): TMCPSecurityConfig;
    function SetAllowedMethods(const AMethods: TArray<string>): TMCPSecurityConfig;
    function SetAllowedOrigins(const AOrigins: TArray<string>): TMCPSecurityConfig;
    function SetCookieSecure(AEnable: Boolean): TMCPSecurityConfig;
    function SetExposeHeaders(const AHeaders: TArray<string>): TMCPSecurityConfig;
    function SetRequireOrigin(AEnable: Boolean): TMCPSecurityConfig;
  end;

  TMCPToolConfig = class(TMCPTool)
  private
    Parent: TMCPToolsConfig;
  public
    constructor Create(AParent: TMCPToolsConfig);
    destructor Destroy; override;

    function WithParam(const AParamName, AName, ADescription: string; const ATags: string = ''): TMCPToolConfig;
    function EndTool: TMCPToolsConfig;
  end;

  /// <summary>
  ///   Configuration for MCP tools registration and discovery.
  /// </summary>
  TMCPToolsConfig = class(TMCPBaseConfig)
  private
    Configs: TObjectList<TMCPToolConfig>;
    procedure WriteInputSchema(ATool: TMCPTool);
    procedure WriteOutputSchema(ATool: TMCPTool);
    procedure WriteParams(AConfig: TMCPTool; AProps: TJSONObject; ARequired: TJSONArray); overload;

    procedure WriteTool(ATool: TMCPTool);

    procedure EndTool(AConfig: TMCPToolConfig);
  public
    Registry: TMCPToolRegistry;
    NeonConfig: INeonConfiguration;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPToolsConfig;
    function RegisterTool(AClass: TClass; const AMethodName, AName, ADescription: string; const ATags: string = ''): TMCPToolConfig;

    /// <summary>
    ///   Unregisters a single tool by its MCP-facing name. Raises EMCPException if no
    ///   tool is registered under that name.
    /// </summary>
    function UnregisterTool(const AName: string): TMCPToolsConfig;

    /// <summary>
    ///   Unregisters every tool backed by AClass, regardless of whether they were
    ///   registered via [McpTool] (RegisterClass) or programmatically (RegisterTool).
    ///   A no-op if no tool is registered against AClass.
    /// </summary>
    function UnregisterClass(AClass: TClass): TMCPToolsConfig;

    /// <summary>
    ///   Unregisters every tool, clearing the registry entirely. A no-op if
    ///   nothing is registered.
    /// </summary>
    function ClearAll: TMCPToolsConfig;

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

  /// <summary>
  ///   A single argument mapping for a manually-registered prompt (registered without
  ///   [McpPrompt]/[McpArgument] attributes): maps a Delphi RTTI parameter to its
  ///   MCP-facing argument name, description, and required flag.
  /// </summary>
  TMCPPromptArgConfig = record
    ParamName: string;
    Name: string;
    Description: string;
    Required: Boolean;

    class function New(const AParamName, AName: string; const ADescription: string = '';
      ARequired: Boolean = False): TMCPPromptArgConfig; static;
  end;

  TMCPPromptsConfig = class(TMCPBaseConfig)
  private
    procedure WritePrompt(APrompt: TMCPPrompt);
  public
    Registry: TMCPPromptRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    function RegisterClass(AClass: TClass): TMCPPromptsConfig;

    /// <summary>
    ///   Registers a single prompt-serving method directly, without needing [McpPrompt]/
    ///   [McpArgument] attributes. AArguments maps each RTTI parameter (by Delphi name) to
    ///   its MCP-facing argument name/description/required flag.
    /// </summary>
    function RegisterPrompt(AClass: TClass; const AMethodName, AName: string;
      const AArguments: TArray<TMCPPromptArgConfig>; const ATitle: string = '';
      const ADescription: string = ''; const ATags: string = ''): TMCPPromptsConfig;

    /// <summary>
    ///   Unregisters a single prompt by its MCP-facing name. Raises EMCPException if no
    ///   prompt is registered under that name.
    /// </summary>
    function UnregisterPrompt(const AName: string): TMCPPromptsConfig;

    /// <summary>
    ///   Unregisters every prompt backed by AClass, regardless of whether they were
    ///   registered via [McpPrompt] (RegisterClass) or programmatically (RegisterPrompt).
    ///   A no-op if no prompt is registered against AClass.
    /// </summary>
    function UnregisterClass(AClass: TClass): TMCPPromptsConfig;

    /// <summary>
    ///   Unregisters every prompt, clearing the registry entirely. A no-op if
    ///   nothing is registered.
    /// </summary>
    function ClearAll: TMCPPromptsConfig;

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
    function FileNameToUri(const AFileName: string): string;

    /// <summary>
    ///   The size in bytes of the file behind a static resource, or 0 when it
    ///   cannot be read.
    /// </summary>
    function StaticResourceSize(AResource: TMCPResource): Int64;

    /// <summary>
    ///   Parses ATags onto AResource and applies the ones it understands:
    ///   "title=", "category=" and "disabled".
    /// </summary>
    procedure ApplyResourceTags(AResource: TMCPResourceBase; const ATags: string);

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

    /// <summary>
    ///   Registers a single resource-serving method directly, without needing an [McpResource] attribute.
    /// </summary>
    function RegisterResource(AClass: TClass; const AMethodName, AName, AUri: string;
      const AMime: string = ''; const ADescription: string = ''; const ATags: string = ''): TMCPResourcesConfig;

    /// <summary>
    ///   Registers a single resource-template method directly, without needing [McpTemplate]/[McpParam]
    ///   attributes. AParamNames maps the method's RTTI parameters, in declaration order, to the
    ///   uri template's {placeholder} names.
    /// </summary>
    function RegisterTemplate(AClass: TClass; const AMethodName, AName, AUriTemplate: string;
      const AParamNames: TArray<string>; const AMime: string = ''; const ADescription: string = '';
      const ATags: string = ''): TMCPResourcesConfig;

    /// <summary>
    ///   Registers a single MCP App UI method directly, without needing an [McpAppUI] attribute.
    ///   AUIConfig is an optional callback for CSP/permissions/domain configuration.
    /// </summary>
    function RegisterUI(AClass: TClass; const AMethodName, AName, AUri: string;
      const ADescription: string = ''; const ATags: string = '';
      AUIConfig: TMCPUIResourceConfigurator = nil): TMCPResourcesConfig;

    /// <summary>
    ///   Unregisters a single resource or App UI resource by its uri. Raises EMCPException
    ///   if no resource is registered under that uri.
    /// </summary>
    function UnregisterResource(const AUri: string): TMCPResourcesConfig;

    /// <summary>
    ///   Unregisters a single static file resource registered via RegisterFile, by the
    ///   same AFileName that was passed to RegisterFile (the uri is derived from it
    ///   internally). Raises EMCPException if no resource is registered for that file.
    /// </summary>
    function UnregisterFile(const AFileName: string): TMCPResourcesConfig;

    /// <summary>
    ///   Unregisters a single resource template by its uri template. Raises EMCPException
    ///   if no template is registered under that uri template.
    /// </summary>
    function UnregisterTemplate(const AUriTemplate: string): TMCPResourcesConfig;

    /// <summary>
    ///   Unregisters every resource, App UI resource, and resource template backed by
    ///   AClass, regardless of whether they were registered via attributes (RegisterClass)
    ///   or programmatically (RegisterResource/RegisterTemplate/RegisterUI). A no-op if
    ///   nothing is registered against AClass.
    /// </summary>
    function UnregisterClass(AClass: TClass): TMCPResourcesConfig;

    /// <summary>
    ///   Unregisters every resource, App UI resource, and resource template,
    ///   clearing both registries entirely. A no-op if nothing is registered.
    /// </summary>
    function ClearAll: TMCPResourcesConfig;

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
  ///   Configuration for argument completion: the providers backing the
  ///   "completion/complete" request.
  /// </summary>
  /// <remarks>
  ///   A provider answers for one argument of one prompt, or one placeholder of
  ///   one resource template. Its method may take no parameter, the value typed
  ///   so far, or that value plus the TMCPCompletionContext holding the
  ///   arguments the client has already resolved; it returns the candidates as
  ///   a TArray&lt;string&gt;, a TStrings, or a ready-made TCompleteResult.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   [McpScope('demo')]
  ///   TMyPrompts = class
  ///     [McpPrompt('code_review', 'Code review', 'Reviews code')]
  ///     [McpArgument('language', 'Programming language')]
  ///     function CodeReview(const language: string): TMCPGetPromptResult;
  ///
  ///     [McpComplete('code_review', 'language')]
  ///     function CompleteLanguage(const AValue: string): TArray&lt;string&gt;;
  ///   end;
  ///
  ///   // .Completions.RegisterClass(TMyPrompts)
  ///   </code>
  /// </example>
  TMCPCompletionsConfig = class(TMCPBaseConfig)
  private
    function AddProvider(AClass: TClass; AMethod: TRttiMethod;
      ARefKind: TMCPCompletionRefKind; const ATarget, AArgument: string): TMCPCompletionProvider;
    function FindMethod(AClass: TClass; const AMethodName: string): TRttiMethod;
  public
    Registry: TMCPCompletionRegistry;
  public
    constructor Create(AConfig: IMCPConfig);
    destructor Destroy; override;

    /// <summary>
    ///   Registers every [McpComplete] / [McpCompleteTemplate] method of AClass.
    ///   [McpComplete] prompt names are scoped with [McpScope] exactly as the
    ///   prompts themselves are; [McpCompleteTemplate] uris never are.
    /// </summary>
    function RegisterClass(AClass: TClass): TMCPCompletionsConfig;

    /// <summary>
    ///   Registers a single prompt-argument completion method directly, without
    ///   needing an [McpComplete] attribute. APromptName is the MCP-facing name,
    ///   scope included.
    /// </summary>
    function RegisterCompletion(AClass: TClass; const AMethodName, APromptName,
      AArgument: string): TMCPCompletionsConfig;

    /// <summary>
    ///   Registers a single resource-template completion method directly,
    ///   without needing an [McpCompleteTemplate] attribute.
    /// </summary>
    function RegisterTemplateCompletion(AClass: TClass; const AMethodName,
      AUriTemplate, AArgument: string): TMCPCompletionsConfig;

    /// <summary>
    ///   Unregisters the provider for one argument of one prompt. Raises
    ///   EMCPException if nothing is registered for that pair.
    /// </summary>
    function UnregisterCompletion(const APromptName, AArgument: string): TMCPCompletionsConfig;

    /// <summary>
    ///   Unregisters the provider for one placeholder of one resource template.
    ///   Raises EMCPException if nothing is registered for that pair.
    /// </summary>
    function UnregisterTemplateCompletion(const AUriTemplate, AArgument: string): TMCPCompletionsConfig;

    /// <summary>
    ///   Unregisters every provider backed by AClass, however it was registered.
    ///   A no-op if none is.
    /// </summary>
    function UnregisterClass(AClass: TClass): TMCPCompletionsConfig;

    /// <summary>
    ///   Unregisters every provider, clearing the registry entirely.
    /// </summary>
    function ClearAll: TMCPCompletionsConfig;

    /// <summary>
    ///   The provider for a (reference, argument) pair, or nil when none is
    ///   registered or the one registered is disabled.
    /// </summary>
    function Find(ARefKind: TMCPCompletionRefKind; const ATarget,
      AArgument: string): TMCPCompletionProvider;

    /// <summary>
    ///   Whether any provider is registered, which is what makes the server
    ///   advertise the "completions" capability.
    /// </summary>
    function HasProviders: Boolean;
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
    FCompletions: TMCPCompletionsConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;

    { IMCPConfig }
    function Server: TMCPServerConfig;
    function Security: TMCPSecurityConfig;
    function Tools: TMCPToolsConfig;
    function Prompts: TMCPPromptsConfig;
    function Resources: TMCPResourcesConfig;
    function Completions: TMCPCompletionsConfig;
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
  FCompletions := TMCPCompletionsConfig.Create(Self);
end;

destructor TMCPConfig.Destroy;
begin
  FCompletions.Free;
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

function TMCPConfig.Completions: TMCPCompletionsConfig;
begin
  Result := FCompletions;
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
  for var cfg in Configs do
    cfg.Free;
  Configs.Clear;
  Result := inherited;
end;

constructor TMCPToolsConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  Registry := TMCPToolRegistry.Create([doOwnsValues]);
  Configs := TObjectList<TMCPToolConfig>.Create(False);
  NeonConfig := TNeonConfiguration.Camel;
end;

destructor TMCPToolsConfig.Destroy;
begin
  Registry.Free;
  for var cfg in Configs do
    cfg.Free;
  Configs.Free;
  inherited;
end;

function TMCPToolsConfig.CreateInstance(const ATool: string): TObject;
var
  LTool: TMCPTool;
begin
  if not Registry.TryGetValue(ATool, LTool) then
    raise EMCPException.CreateFmt(SToolNotFoundFmt, [ATool]);

  Result := TRttiUtils.CreateInstance(LTool.ToolClass);
end;

procedure TMCPToolsConfig.EndTool(AConfig: TMCPToolConfig);
begin
  WriteTool(AConfig);
  WriteInputSchema(AConfig);

  if AConfig.Tags.Exists('structured') then
    WriteOutputSchema(AConfig);

  Registry.Add(AConfig.Name, AConfig);
  Configs.Remove(AConfig);
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
      LTool.ToolClass := AClass;
      LTool.Method := LMethod;

      LAppAttr := TRttiUtils.FindAttribute<MCPAppAttribute>(LMethod);
      if Assigned(LAppAttr) then
        LToolAttr.Tags.TagMap.AddOrSetValue('app', LAppAttr.UI);

      for var tag in LToolAttr.Tags.TagMap do
        LTool.Tags.TagMap.Add(tag.Key, tag.Value);

      // Fill the tool's parameters
      for var par in LMethod.GetParameters do
      begin
        var attr := par.GetAttribute<MCPParamAttribute>;
          if not Assigned(attr) then
            raise EJRPCException.Create(SNonAnnotatedParamsNotPermitted);

        var toolPar := TMCPToolParam.Create;
        LTool.MethodParams.Add(toolPar);
        toolPar.Param := par;
        toolPar.ParamName := par.Name;
        toolPar.Name := attr.Name;
        toolPar.Description := attr.Description;
        for var tag in attr.Tags.TagMap do
          toolPar.Tags.TagMap.Add(tag.Key, tag.Value);
      end;

      WriteTool(LTool);
      WriteInputSchema(LTool);

      if LTool.Tags.Exists('structured') then
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
  // Add the tool in the Configs list: to be moved on the EndTool call or collected at the end
  Configs.Add(Result);

  Result.ToolClass := AClass;
  Result.MethodName := AMethodName;
  Result.Method := LMethod;

  Result.Name := AName;
  Result.Description := ADescription;
  Result.Tags.TagMap.Clear;
  Result.Tags.Parse(ATags);
end;

function TMCPToolsConfig.UnregisterTool(const AName: string): TMCPToolsConfig;
begin
  if not Registry.ContainsKey(AName) then
    raise EMCPException.CreateFmt(SToolNotFoundFmt, [AName]);

  Registry.Remove(AName);
  Result := Self;
end;

function TMCPToolsConfig.UnregisterClass(AClass: TClass): TMCPToolsConfig;
var
  LNames: TArray<string>;
begin
  LNames := [];
  for var pair in Registry do
    if pair.Value.ToolClass = AClass then
      LNames := LNames + [pair.Key];

  for var name in LNames do
    Registry.Remove(name);

  Result := Self;
end;

function TMCPToolsConfig.ClearAll: TMCPToolsConfig;
begin
  Registry.Clear;
  Result := Self;
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

procedure TMCPToolsConfig.WriteTool(ATool: TMCPTool);
var
  LIcon: TMCPIcon;
begin
  // Caught here, at registration, rather than left for the client to reject:
  // both the attribute and the programmatic path pass through WriteTool, and
  // the name checked is the scoped one the client actually sees
  if not IsValidToolName(ATool.Name) then
    raise EMCPException.CreateFmt(SToolNameInvalidFmt,
      [ATool.Name, MCP_TOOL_NAME_MAX_LENGTH]);

  if SetIcon(ATool.Tags.GetValueAs<string>('icon'), LIcon) then
    ATool.Icons := ATool.Icons + [LIcon];

  ATool.Category := ATool.Tags.GetValueAs<string>('category');
  ATool.Disabled := ATool.Tags.GetBoolValue('disabled');

  if ATool.Tags.Exists('app') then
    ATool.UI.ResourceUri := ATool.Tags.GetValueAs<string>('app');

  if ATool.Tags.Exists('readonly') then
    ATool.Annotations.ReadOnlyHint := ATool.Tags.GetBoolValue('readonly');
  if ATool.Tags.Exists('destructive') then
    ATool.Annotations.DestructiveHint := ATool.Tags.GetBoolValue('destructive');
  if ATool.Tags.Exists('idempotent') then
    ATool.Annotations.IdempotentHint := ATool.Tags.GetBoolValue('idempotent');
  if ATool.Tags.Exists('openworld') then
    ATool.Annotations.OpenWorldHint := ATool.Tags.GetBoolValue('openworld');
end;

procedure TMCPToolsConfig.WriteInputSchema(ATool: TMCPTool);
var
  LProps, LInputSchema: TJSONObject;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(ATool, LProps, LRequired);
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

procedure TMCPToolsConfig.WriteParams(AConfig: TMCPTool;
  AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
begin
  if AConfig.MethodParams.Count <> Length(AConfig.Method.GetParameters) then
    raise EJRPCException.Create(SNonConfiguredParamsNotPermitted);
  
  for LParam in AConfig.Method.GetParameters do
  begin
    var par := AConfig.FindMCPParam(LParam.Name);
    if not Assigned(par) then
      raise EJRPCException.CreateFmt(SParamHasNoConfigurationFmt, [LParam.Name]);
      
    LJSONObj := TNeonSchemaGenerator.TypeToJSONSchema(LParam.ParamType, NeonConfig);

    LJSONObj.AddPair('description', TJSONString.Create(par.Description));
    AProps.AddPair(par.Name, LJSONObj);
    ARequired.Add(par.Name);
  end;

end;

constructor TMCPServerConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  WriterRegistry := TMCPWriterRegistry.Create;

  IconFolder := '';
  ScopeSeparator := '_';  // Default separator
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
  if TMCPCapability.Completions in ACapabilities then
    Capabilities.EnableCompletions;

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

procedure TMCPResourcesConfig.ApplyResourceTags(AResource: TMCPResourceBase;
  const ATags: string);
begin
  AResource.Tags.Parse(ATags);

  // Assigned only when present: a NullString takes an empty string as a value,
  // which would emit "title": "" rather than leaving the member out
  if AResource.Tags.Exists('title') then
    AResource.Title := AResource.Tags.GetValueAs<string>('title');

  AResource.Category := AResource.Tags.GetValueAs<string>('category');
  AResource.Disabled := AResource.Tags.GetBoolValue('disabled');
end;

procedure TMCPResourcesConfig.ResourceList(AList: TListResourcesResult);
begin
  for var pair in Registry do
    if not pair.Value.Disabled then
    begin
      // A static file's size is known, and hosts use it to show file sizes and
      // estimate context usage. It is filled here rather than in RegisterFile
      // because BasePath may still be set after the file is registered.
      if pair.Value.FileName <> '' then
        pair.Value.Size := StaticResourceSize(pair.Value);

      AList.Resources.Add(pair.Value);
    end;
end;

function TMCPResourcesConfig.StaticResourceSize(AResource: TMCPResource): Int64;
var
  LFileName: string;
  LFile: TSearchRec;
begin
  Result := 0;

  LFileName := TPath.Combine(BasePath, AResource.FileName);
  if FindFirst(LFileName, faAnyFile, LFile) = 0 then
  try
    Result := LFile.Size;
  finally
    FindClose(LFile);
  end;
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

function TMCPResourcesConfig.FileNameToUri(const AFileName: string): string;
begin
  { TODO -opaolo -c : Customize the URI (URI Schemes?) 16/02/2026 13:01:25 }
  Result := 'res://' + StringReplace(AFileName, '\', '/', [rfReplaceAll]);
end;

function TMCPResourcesConfig.CreateInstance(const AUri: string): TObject;
var
  LResource: TMCPResource;
begin
  if not Registry.TryGetValue(AUri, LResource) then
    raise EMCPException.CreateFmt(SConfigResourceNotFoundFmt, [AUri]);

  Result := TRttiUtils.CreateInstance(LResource.ResourceClass);
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
    LRes.ResourceClass := AClass;
    LRes.Method := AMethod;
    ApplyResourceTags(LRes, AAttr.AdditionalTags);

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
    LRes.ResourceClass := AClass;
    LRes.Method := AMethod;
    ApplyResourceTags(LRes, AAttr.AdditionalTags);

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

function TMCPResourcesConfig.RegisterResource(AClass: TClass; const AMethodName, AName, AUri: string;
  const AMime: string; const ADescription: string; const ATags: string): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
  LRes: TMCPResource;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethodName);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethodName, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create(SResourceMethodNoParams);

  if not ValidUriResource(AUri) then
    raise EMCPException.Create(SResourceUriNoTemplateParams);

  LRes := TMCPResource.Create;
  try
    LRes.ResourceClass := AClass;
    LRes.Method := LMethod;
    LRes.Name := AName;
    LRes.Uri := AUri;
    LRes.MimeType := AMime;
    LRes.Description := ADescription;
    ApplyResourceTags(LRes, ATags);

    Registry.Add(AUri, LRes);
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

function TMCPResourcesConfig.RegisterTemplate(AClass: TClass; const AMethodName, AName, AUriTemplate: string;
  const AParamNames: TArray<string>; const AMime: string; const ADescription: string;
  const ATags: string): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
  LUriParams: TArray<string>;
  LParams: TArray<TRttiParameter>;
  LTpl: TMCPResourceTemplate;
  I: Integer;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethodName);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethodName, AClass.ClassName]);

  LUriParams := GetUriParams(AUriTemplate);
  if Length(LUriParams) = 0 then
    raise EMCPException.Create(STemplateUriMustHaveParams);

  LParams := LMethod.GetParameters;
  if Length(LParams) <> Length(LUriParams) then
    raise EMCPException.CreateFmt(STemplateMethodParamsMismatchFmt, [LMethod.Name]);

  if Length(AParamNames) <> Length(LParams) then
    raise EMCPException.Create(SNonConfiguredTemplateParamsNotPermitted);

  for var par in LParams do
    if not ParamIsType(par, [tkChar, tkWChar, tkString, tkLString, tkWString, tkUString]) then
      raise EMCPException.Create(SParamTypeNotSupported);

  for var uriName in AParamNames do
  begin
    var found := False;
    for var uriParam in LUriParams do
      if SameText(uriParam, uriName) then
      begin
        found := True;
        Break;
      end;
    if not found then
      raise EMCPException.CreateFmt(STemplateParamNameNotInUriFmt, [uriName]);
  end;

  LTpl := TMCPResourceTemplate.Create;
  try
    LTpl.ResourceClass := AClass;
    LTpl.Method := LMethod;
    LTpl.Name := AName;
    LTpl.UriTemplate := AUriTemplate;
    LTpl.MimeType := AMime;
    LTpl.Description := ADescription;
    ApplyResourceTags(LTpl, ATags);

    for I := 0 to High(LParams) do
    begin
      var tplPar := TMCPResTemplateParam.Create;
      LTpl.MethodParams.Add(tplPar);
      tplPar.Param := LParams[I];
      tplPar.ParamName := LParams[I].Name;
      tplPar.Name := AParamNames[I];
    end;

    TemplateRegistry.Add(AUriTemplate, LTpl);
  except
    LTpl.Free;
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
    LTpl.ResourceClass := AClass;
    LTpl.Method := AMethod;
    ApplyResourceTags(LTpl, AAttr.AdditionalTags);

    for var par in AMethod.GetParameters do
    begin
      var attr := par.GetAttribute<MCPParamAttribute>;

      var tplPar := TMCPResTemplateParam.Create;
      LTpl.MethodParams.Add(tplPar);
      tplPar.Param := par;
      tplPar.ParamName := par.Name;
      tplPar.Name := attr.Name;
      tplPar.Description := attr.Description;
    end;

    TemplateRegistry.Add(LTpl.UriTemplate, LTpl);
  except
    LTpl.Free;
    raise;
  end;
end;

function TMCPResourcesConfig.RegisterUI(AClass: TClass; const AMethodName, AName, AUri: string;
  const ADescription: string; const ATags: string; AUIConfig: TMCPUIResourceConfigurator): TMCPResourcesConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
  LApp: TMCPResource;
  LUI: TUIResourceUI;
  LJSON: TJSONObject;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethodName);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethodName, AClass.ClassName]);

  if Length(LMethod.GetParameters) > 0 then
    raise EMCPException.Create(SAppMethodNoParams);

  if not AUri.StartsWith('ui://') then
    raise EMCPException.Create(SAppsUIUriScheme);

  if not ValidUriResource(AUri) then
    raise EMCPException.Create(SResourceUriNoTemplateParams);

  LApp := TMCPResource.Create;
  try
    LApp.ResourceClass := AClass;
    LApp.Method := LMethod;
    LApp.Name := AName;
    LApp.Uri := AUri;
    LApp.MimeType := 'text/html;profile=mcp-app';
    LApp.Description := ADescription;
    ApplyResourceTags(LApp, ATags);

    if Assigned(AUIConfig) then
    begin
      LUI := TUIResourceUI.Create;
      try
        AUIConfig(LApp, LUI);
        LJSON := LUI.ToJSON;
        if LJSON.Count > 0 then
          LApp.Meta.AddPair('ui', LJSON)
        else
          LJSON.Free;
      finally
        LUI.Free;
      end;
    end;

    Registry.Add(AUri, LApp);
  except
    LApp.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPResourcesConfig.UnregisterResource(const AUri: string): TMCPResourcesConfig;
begin
  if not Registry.ContainsKey(AUri) then
    raise EMCPException.CreateFmt(SConfigResourceNotFoundFmt, [AUri]);

  Registry.Remove(AUri);
  Result := Self;
end;

function TMCPResourcesConfig.UnregisterFile(const AFileName: string): TMCPResourcesConfig;
begin
  Result := UnregisterResource(FileNameToUri(AFileName));
end;

function TMCPResourcesConfig.UnregisterTemplate(const AUriTemplate: string): TMCPResourcesConfig;
begin
  if not TemplateRegistry.ContainsKey(AUriTemplate) then
    raise EMCPException.CreateFmt(STemplateNotFoundFmt, [AUriTemplate]);

  TemplateRegistry.Remove(AUriTemplate);
  Result := Self;
end;

function TMCPResourcesConfig.UnregisterClass(AClass: TClass): TMCPResourcesConfig;
var
  LUris: TArray<string>;
begin
  LUris := [];
  for var pair in Registry do
    if pair.Value.ResourceClass = AClass then
      LUris := LUris + [pair.Key];

  for var uri in LUris do
    Registry.Remove(uri);

  LUris := [];
  for var pair in TemplateRegistry do
    if pair.Value.ResourceClass = AClass then
      LUris := LUris + [pair.Key];

  for var uri in LUris do
    TemplateRegistry.Remove(uri);

  Result := Self;
end;

function TMCPResourcesConfig.ClearAll: TMCPResourcesConfig;
begin
  Registry.Clear;
  TemplateRegistry.Clear;
  Result := Self;
end;

function TMCPResourcesConfig.RegisterFile(const AFileName, ADescription: string;
  const AMime: string = ''): TMCPResourcesConfig;
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
    LRes.Uri := FileNameToUri(AFileName);
    LRes.MimeType := LMime;
    LRes.Description := ADescription;
    LRes.ResourceClass := RES_CLASS;
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

  Result := TRttiUtils.CreateInstance(LPrompt.PromptClass);
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
      LPrompt.PromptClass := AClass;
      LPrompt.Method := LMethod;

      for var tag in LPromptAttr.Tags.TagMap do
        LPrompt.Tags.TagMap.Add(tag.Key, tag.Value);

      WritePrompt(LPrompt);

      for var LParam in LMethod.GetParameters do
      begin
        var LAttr := LParam.GetAttribute<MCPArgumentAttribute>;
          if not Assigned(LAttr) then
            raise EJRPCException.Create(SNonAnnotatedParamsNotPermitted);

        var LArg := TPromptArgument.New(LAttr.Name, LAttr.Description);
        if LAttr.Tags.Exists('required') then
          LArg.Required := LAttr.Tags.GetBoolValue('required');

        LPrompt.Arguments := LPrompt.Arguments + [LArg];

        var LPromptPar := TMCPPromptParam.Create;
        LPrompt.MethodParams.Add(LPromptPar);
        LPromptPar.Param := LParam;
        LPromptPar.ParamName := LParam.Name;
        LPromptPar.Name := LAttr.Name;
      end;

      Registry.Add(LPrompt.Name, LPrompt);
    except
      LPrompt.Free;
      raise;
    end;
  end;
  Result := Self;
end;

function TMCPPromptsConfig.RegisterPrompt(AClass: TClass; const AMethodName, AName: string;
  const AArguments: TArray<TMCPPromptArgConfig>; const ATitle: string; const ADescription: string;
  const ATags: string): TMCPPromptsConfig;
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
  LPrompt: TMCPPrompt;
begin
  LClassType := TRttiUtils.Context.GetType(AClass);
  LMethod := LClassType.GetMethod(AMethodName);
  if not Assigned(LMethod) then
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethodName, AClass.ClassName]);

  if Length(AArguments) <> Length(LMethod.GetParameters) then
    raise EMCPException.Create(SNonConfiguredPromptParamsNotPermitted);

  LPrompt := TMCPPrompt.Create;
  try
    LPrompt.PromptClass := AClass;
    LPrompt.Method := LMethod;
    LPrompt.Name := AName;
    LPrompt.Title := ATitle;
    LPrompt.Description := ADescription;
    LPrompt.Tags.Parse(ATags);

    WritePrompt(LPrompt);

    for var arg in AArguments do
    begin
      var par := LPrompt.FindRttiParam(arg.ParamName);
      if not Assigned(par) then
        raise EMCPException.CreateFmt(SPromptParamNotFoundFmt, [arg.ParamName, AName]);

      var LPromptPar := TMCPPromptParam.Create;
      LPrompt.MethodParams.Add(LPromptPar);
      LPromptPar.Param := par;
      LPromptPar.ParamName := arg.ParamName;
      LPromptPar.Name := arg.Name;

      var LArg := TPromptArgument.New(arg.Name, arg.Description);
      LArg.Required := arg.Required;
      LPrompt.Arguments := LPrompt.Arguments + [LArg];
    end;

    Registry.Add(LPrompt.Name, LPrompt);
  except
    LPrompt.Free;
    raise;
  end;

  Result := Self;
end;

function TMCPPromptsConfig.UnregisterPrompt(const AName: string): TMCPPromptsConfig;
begin
  if not Registry.ContainsKey(AName) then
    raise EMCPException.CreateFmt(SPromptNotFoundFmt, [AName]);

  Registry.Remove(AName);
  Result := Self;
end;

function TMCPPromptsConfig.UnregisterClass(AClass: TClass): TMCPPromptsConfig;
var
  LNames: TArray<string>;
begin
  LNames := [];
  for var pair in Registry do
    if pair.Value.PromptClass = AClass then
      LNames := LNames + [pair.Key];

  for var name in LNames do
    Registry.Remove(name);

  Result := Self;
end;

function TMCPPromptsConfig.ClearAll: TMCPPromptsConfig;
begin
  Registry.Clear;
  Result := Self;
end;

procedure TMCPPromptsConfig.WritePrompt(APrompt: TMCPPrompt);
var
  LIcon: TMCPIcon;
begin
  if SetIcon(APrompt.Tags.GetValueAs<string>('icon'), LIcon) then
    APrompt.Icons := APrompt.Icons + [LIcon];

  APrompt.Category := APrompt.Tags.GetValueAs<string>('category');
  APrompt.Disabled := APrompt.Tags.GetBoolValue('disabled');
end;

{ TMCPPromptArgConfig }

class function TMCPPromptArgConfig.New(const AParamName, AName: string; const ADescription: string;
  ARequired: Boolean): TMCPPromptArgConfig;
begin
  Result.ParamName := AParamName;
  Result.Name := AName;
  Result.Description := ADescription;
  Result.Required := ARequired;
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

function TMCPSecurityConfig.SetExposeHeaders(const AHeaders: TArray<string>): TMCPSecurityConfig;
begin
  ExposeHeaders := AHeaders;
  Result := Self;
end;

function TMCPSecurityConfig.SetRequireOrigin(AEnable: Boolean): TMCPSecurityConfig;
begin
  RequireOrigin := AEnable;
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

function TMCPMessageHandlingConfig.OnInitialized(AProc: TProc<TJRPCContext>): TMCPMessageHandlingConfig;
begin
  FInitializedProc := AProc;
  Result := Self;
end;

function TMCPMessageHandlingConfig.OnSetLogLevel(AProc: TProc<TJRPCContext, TMCPLogLevel>): TMCPMessageHandlingConfig;
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
  inherited Create;
  Parent := AParent;
end;

destructor TMCPToolConfig.Destroy;
begin
  inherited;
end;

function TMCPToolConfig.EndTool: TMCPToolsConfig;
begin
  Parent.EndTool(Self);
  Result := Parent;
end;

function TMCPToolConfig.WithParam(const AParamName, AName, ADescription: string;
  const ATags: string): TMCPToolConfig;
begin
  var par := FindRttiParam(AParamName);
  if not Assigned(par) then
    raise EMCPException.CreateFmt(SToolParamNotFoundFmt, [AParamName, Name]);

  var toolPar := TMCPToolParam.Create();
  toolPar.ParamName := AParamName;
  toolPar.Param := par;

  toolPar.Name := AName;
  toolPar.Description := ADescription;
  toolPar.Tags.Parse(ATags);
  MethodParams.Add(toolPar);

  Result := Self;
end;

{ TMCPCompletionsConfig }

constructor TMCPCompletionsConfig.Create(AConfig: IMCPConfig);
begin
  inherited;
  Registry := TMCPCompletionRegistry.Create([doOwnsValues]);
end;

destructor TMCPCompletionsConfig.Destroy;
begin
  Registry.Free;
  inherited;
end;

function TMCPCompletionsConfig.FindMethod(AClass: TClass; const AMethodName: string): TRttiMethod;
begin
  Result := TRttiUtils.Context.GetType(AClass).GetMethod(AMethodName);
  if not Assigned(Result) then
    raise EMCPException.CreateFmt(SMethodNotFoundInClassFmt, [AMethodName, AClass.ClassName]);
end;

function TMCPCompletionsConfig.AddProvider(AClass: TClass; AMethod: TRttiMethod;
  ARefKind: TMCPCompletionRefKind; const ATarget, AArgument: string): TMCPCompletionProvider;
var
  LKey: string;
begin
  if ATarget.IsEmpty then
    raise EMCPException.Create(SCompletionRefTargetEmpty);

  LKey := TMCPCompletionProvider.KeyFor(ARefKind, ATarget, AArgument);
  if Registry.ContainsKey(LKey) then
    raise EMCPException.CreateFmt(SCompletionProviderDuplicateFmt, [AArgument, ATarget]);

  Result := TMCPCompletionProvider.Create;
  try
    Result.ProviderClass := AClass;
    Result.Method := AMethod;
    Result.RefKind := ARefKind;
    Result.RefTarget := ATarget;
    Result.Argument := AArgument;

    Registry.Add(LKey, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPCompletionsConfig.RegisterClass(AClass: TClass): TMCPCompletionsConfig;
var
  LScope: string;
  LClassType: TRttiType;
  LScopeAttr: MCPScopeAttribute;
  LPromptAttr: MCPCompleteAttribute;
  LTemplateAttr: MCPCompleteTemplateAttribute;
begin
  LScope := '';
  LClassType := TRttiUtils.Context.GetType(AClass);
  LScopeAttr := TRttiUtils.FindAttribute<MCPScopeAttribute>(LClassType);
  if Assigned(LScopeAttr) then
    LScope := LScopeAttr.Name + FConfig.Server.ScopeSeparator;

  for var LMethod in LClassType.GetMethods do
  begin
    // A prompt reference names a prompt, so it carries the same scope prefix
    // the prompt itself was registered under
    LPromptAttr := TRttiUtils.FindAttribute<MCPCompleteAttribute>(LMethod);
    if Assigned(LPromptAttr) then
      AddProvider(AClass, LMethod, TMCPCompletionRefKind.Prompt,
        LScope + LPromptAttr.Name, LPromptAttr.Argument);

    // A resource reference is a uri, which is never scoped
    LTemplateAttr := TRttiUtils.FindAttribute<MCPCompleteTemplateAttribute>(LMethod);
    if Assigned(LTemplateAttr) then
      AddProvider(AClass, LMethod, TMCPCompletionRefKind.ResourceTemplate,
        LTemplateAttr.UriTemplate, LTemplateAttr.Argument);
  end;

  Result := Self;
end;

function TMCPCompletionsConfig.RegisterCompletion(AClass: TClass;
  const AMethodName, APromptName, AArgument: string): TMCPCompletionsConfig;
begin
  AddProvider(AClass, FindMethod(AClass, AMethodName),
    TMCPCompletionRefKind.Prompt, APromptName, AArgument);

  Result := Self;
end;

function TMCPCompletionsConfig.RegisterTemplateCompletion(AClass: TClass;
  const AMethodName, AUriTemplate, AArgument: string): TMCPCompletionsConfig;
begin
  AddProvider(AClass, FindMethod(AClass, AMethodName),
    TMCPCompletionRefKind.ResourceTemplate, AUriTemplate, AArgument);

  Result := Self;
end;

function TMCPCompletionsConfig.UnregisterCompletion(const APromptName,
  AArgument: string): TMCPCompletionsConfig;
var
  LKey: string;
begin
  LKey := TMCPCompletionProvider.KeyFor(TMCPCompletionRefKind.Prompt, APromptName, AArgument);
  if not Registry.ContainsKey(LKey) then
    raise EMCPException.CreateFmt(SCompletionProviderNotFoundFmt, [AArgument, APromptName]);

  Registry.Remove(LKey);
  Result := Self;
end;

function TMCPCompletionsConfig.UnregisterTemplateCompletion(const AUriTemplate,
  AArgument: string): TMCPCompletionsConfig;
var
  LKey: string;
begin
  LKey := TMCPCompletionProvider.KeyFor(TMCPCompletionRefKind.ResourceTemplate, AUriTemplate, AArgument);
  if not Registry.ContainsKey(LKey) then
    raise EMCPException.CreateFmt(SCompletionProviderNotFoundFmt, [AArgument, AUriTemplate]);

  Registry.Remove(LKey);
  Result := Self;
end;

function TMCPCompletionsConfig.UnregisterClass(AClass: TClass): TMCPCompletionsConfig;
var
  LKeys: TArray<string>;
begin
  LKeys := [];
  for var LPair in Registry do
    if LPair.Value.ProviderClass = AClass then
      LKeys := LKeys + [LPair.Key];

  // Collected first: removing while enumerating invalidates the enumerator
  for var LKey in LKeys do
    Registry.Remove(LKey);

  Result := Self;
end;

function TMCPCompletionsConfig.ClearAll: TMCPCompletionsConfig;
begin
  Registry.Clear;
  Result := Self;
end;

function TMCPCompletionsConfig.Find(ARefKind: TMCPCompletionRefKind;
  const ATarget, AArgument: string): TMCPCompletionProvider;
begin
  if not Registry.TryGetValue(TMCPCompletionProvider.KeyFor(ARefKind, ATarget, AArgument), Result) then
    Exit(nil);

  if Result.Disabled then
    Result := nil;
end;

function TMCPCompletionsConfig.HasProviders: Boolean;
begin
  for var LPair in Registry do
    if not LPair.Value.Disabled then
      Exit(True);

  Result := False;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPConfig);

end.
