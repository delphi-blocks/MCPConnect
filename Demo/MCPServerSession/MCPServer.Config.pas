unit MCPServer.Config;

{
  ==============================================================================
   MCPConnect demo - the server definition
  ==============================================================================

  This unit is the heart of the demo: it declares *what* the MCP server is and
  what it exposes. It is deliberately transport-independent - the very same
  TServerConfigurator.ConfigureServer call is used by all four hosts:

      Demo\MCPServer\Indy            (TJRPCIndyServer  - HTTP + SSE)
      Demo\MCPServer\WebBroker       (TJRPCDispatcher  - standalone/ISAPI/CGI)
      Demo\MCPServer\Stdio           (TJRPCStdioServer - launched by the client)
      Demo\MCPServer\WindowsService  (Indy, hosted in a Windows service)

  The scenario is the ticket desk of the "Delphi Day" conference in Italy:
  tools to browse tickets and manage a per-session shopping cart, resources
  with event information, a couple of prompts, and an MCP App (an HTML UI the
  client renders next to the conversation).

  ------------------------------------------------------------------------------
  How the configuration API works
  ------------------------------------------------------------------------------
  TJRPCServer owns a chain of *plugins*, each one a configuration interface:

      .Plugin.Configure<IMCPConfig>       -> the MCP layer itself
      .Plugin.Configure<ISessionConfig>   -> session id transport and lifetime
      .Plugin.Configure<IAuthTokenConfig> -> simple shared-token auth
      .Plugin.Configure<IOAuthConfig>     -> OAuth 2.1 resource server
      .Plugin.Configure<IJRPCNeonConfig>  -> global JSON serialization rules

  Every Configure<T> call returns a fluent builder. Two different "return to
  the caller" verbs are used, and mixing them up is the most common mistake:

      .BackToMCP    closes a *section* of IMCPConfig (.Server, .Tools, ...)
                    and returns to IMCPConfig itself;
      .ApplyConfig  closes a *whole plugin* and returns to TJRPCServer, so
                    another .Plugin.Configure can be chained after it.

  Nothing is executed at "declaration time" except registration: RTTI is
  scanned once, JSON Schemas are generated once, and the result is cached in
  the configuration objects owned by the server.
}

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Server,

  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types,

  // The configuration plugins used below. Each one is optional: a server with
  // no session, auth or Neon configuration is perfectly valid.
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  // Content writers turn "unusual" Delphi return types into MCP content
  // blocks. The RTL unit handles TStream / TBytes / TStrings; the VCL unit
  // adds TPicture / TBitmap / TImage and is only linked in a VCL host.
  MCPConnect.Content.Writers.RTL,
  {$IFDEF FRAMEWORK_VCL}
  MCPConnect.Content.Writers.VCL,
  {$ENDIF}

  MCPConnect.Session.Core,
  MCPServer.Notifications;

type
  /// <summary>
  ///   Static holder for the two lifecycle calls the host form/service makes:
  ///   one at startup and one at shutdown.
  /// </summary>
  TServerConfigurator = class
    /// <summary>
    ///   Declares the whole server. Call once, right after the transport has
    ///   created its TJRPCServer and before the transport is activated.
    /// </summary>
    class procedure ConfigureServer(AServer: TJRPCServer);

    /// <summary>
    ///   Demonstrates the runtime unregistration API. Not needed for a clean
    ///   shutdown (the server frees its own configuration), it is here to show
    ///   how a live server can gain or lose features.
    /// </summary>
    class procedure UnregisterFeatures(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Logify,

  // The units holding the decorated classes. A class is only visible to the
  // protocol once it is *registered* below - simply linking the unit does
  // nothing, which is what lets tool classes live anywhere in your codebase.
  MCPServer.Resources,
  MCPServer.Apps,
  MCPServer.Tools,
  MCPServer.Prompts;


{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  // Everything the demo serves from disk (icons, the MCP App HTML, the PDF,
  // index.md) lives in the "data" folder next to the executable. Remember to
  // copy Demo\data next to the .exe (or run from the project folder).
  LDataPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');

  AServer

  // ===========================================================================
  //  Authentication (disabled in this demo)
  // ===========================================================================
  //  IAuthTokenConfig is the "one shared secret" scheme: the client must send
  //  Authorization: Bearer my-secret-token on every HTTP request. The token is
  //  case sensitive and compared in constant time.
  //  .SetTokenLocation(...) moves it to a cookie or a custom header.
  //  For real, user-level authorization use IOAuthConfig instead - see the
  //  dedicated Demo\MCPOAuthServer project and Docs\oauth.md.
  //  STDIO needs neither: the client launches the process itself.

//    .Plugin.Configure<IAuthTokenConfig>
//      .SetToken('my-secret-token')
//    .ApplyConfig

  // ===========================================================================
  //  Sessions
  // ===========================================================================
  //  A session gives every connected client its own server-side state - here
  //  the shopping cart. Tool classes receive it through a [Context] field
  //  (see TDelphiDayTool.FSession) and never have to look it up.

    .Plugin.Configure<ISessionConfig>
      // Where the session id travels. Header is the MCP-recommended choice and
      // the only one an SSE reconnect can carry reliably; Cookie is handy when
      // the client is a browser-based host.
      .SetLocation(TSessionIdLocation.Header)

      // 'Mcp-Session-Id' is the name the MCP specification prescribes. The
      // server returns it on the initialize response; the client echoes it back.
      .SetHeaderName('Mcp-Session-Id')

      // Idle timeout in minutes: after this the session object is destroyed
      // and the cart is gone.
      .SetTimeout(30)  // 30 minutes timeout

      // Use a *typed* session class instead of the generic TMCPSessionData, so
      // tools get FSession.Cart with compile-time checking. The class must
      // descend from TMCPSessionBase and have a parameterless constructor.
      .SetSessionClass(TShoppingSession)  // Use custom typed session

      // .SetReplayBufferSize(100)  // SSE events kept per session so that a
      //                            // client reconnecting with Last-Event-ID
      //                            // can catch up on missed notifications.
    .ApplyConfig

  // ===========================================================================
  //  The MCP layer
  // ===========================================================================

    .Plugin.Configure<IMCPConfig>

      // ---------------------------------------------------------------------
      //  .Server - identity, capabilities and global helpers
      // ---------------------------------------------------------------------
      .Server
        // Reported to the client in the initialize handshake, and usually
        // shown in the client UI.
        .SetName('delphi-mcp-server')
        .SetVersion('2.0.0')

        // If not set, the server checks the registered tools, resources, etc. and automatically fills the capabilities.
        //.SetCapabilities([Tools, Resources])
        //.SetCapabilities(LCapabilities)
        //.SetCapabilities(
        //  procedure (ACapabilities: TServerCapabilities)
        //  begin
        //    ACapabilities.Tools.ListChanged := True;
        //  end
        //)

        // Where the "icon=<file>" tool tags are resolved from. A tag may also
        // carry a full scheme://host/path URL, in which case no folder lookup
        // happens.
        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))

        // Content writers extend the set of Delphi types a tool may return.
        // Without a writer, a return type Neon cannot serialize raises an
        // error; with one, the value is turned into the right MCP content
        // block (image, audio, blob, ...). Writing your own means implementing
        // IMCPContentWriter and registering it here.
        {$IFDEF FRAMEWORK_VCL}
        .RegisterWriter(TMCPImageWriter)    // TImage   -> image content (PNG)
        .RegisterWriter(TMCPPictureWriter)  // TPicture -> image content (PNG)
        {$ENDIF}
        .RegisterWriter(TMCPStreamWriter)      // TStream     -> base64 blob
        .RegisterWriter(TMCPStringListWriter)  // TStringList -> JSON array

      .BackToMCP

      // ---------------------------------------------------------------------
      //  .MessageHandling - inbound notifications and requests from the client
      // ---------------------------------------------------------------------
      .MessageHandling

        // Uncomment to register a class that overrides the standard MCP API
        // (useful especially for notifications).
        // A registered API class takes over its whole JRPC namespace and wins
        // over both the built-in implementation and the typed callbacks below.
        // See MCPServer.Notifications.pas.
        // .RegisterApi(TNotificationHandler)

        // Fires when the client cancels an in-flight request (notifications/cancelled).
        // AParams.RequestId identifies the call to abandon; a long-running
        // tool should watch a shared flag and bail out early.
        .OnCancelled(
          procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
          begin
            Logger.LogDebug('Cancelled');
          end
        )

        // Fires once the client completes the initialize handshake
        // (notifications/initialized). This is the first moment at which the
        // server may legitimately push notifications to that client.
        .OnInitialized(
          procedure (AContext: TJRPCContext)
          begin
            Logger.LogDebug('Notification: Initialized');
          end
        )

        // Fires when the client adjusts the minimum log severity
        // (logging/setLevel). Store ALevel and use it to filter the
        // notifications you push back to that client.
        .OnSetLogLevel(
          procedure (AContext: TJRPCContext; ALevel: TLogSetLevel)
          begin
            Logger.LogDebug('Log level set to %s',
              [GetEnumName(TypeInfo(TLogSetLevel), Ord(ALevel))]);
          end
        )

      .BackToMCP

      // ---------------------------------------------------------------------
      //  .Security - HTTP-level hardening (ignored by the STDIO transport)
      // ---------------------------------------------------------------------
      .Security
        // Emit CORS headers, required when the MCP host runs in a browser
        // (MCPJam Inspector, web-based clients).
        .SetCORS(True)

        // Only POST is needed for JSON-RPC; add 'GET' if you want browser
        // clients to open the SSE stream. OPTIONS preflights are always
        // answered.
        .SetAllowedMethods(['GET', 'POST'])

        .SetAllowedOrigins(['*'])

        // .SetCookieSecure(False)  // only for plain-HTTP development: session
        //                          // cookies are HttpOnly + SameSite=Strict +
        //                          // Secure by default.
      .BackToMCP

      // ---------------------------------------------------------------------
      //  .Resources - read-only content addressed by URI
      // ---------------------------------------------------------------------
      //  Resources are what the *client* decides to pull into the context;
      //  tools are what the *model* decides to call. Anything static or
      //  reference-like (documentation, catalogues, configuration) belongs here.
      .Resources
        // Root for RegisterFile() relative paths.
        .SetBasePath(LDataPath)

        // Attribute-free registration: same result as decorating the method
        // with [McpResource], but usable on classes that cannot carry Delphi
        // attributes - notably classes compiled with C++Builder.
        // Signature: (class, method, resource name, uri, mime, description)
        .RegisterResource(TCppResource, 'GetGlobalInfo',
          'info-resource', 'text://info', 'text/plain', 'Shows the Info')

        // Attribute-driven registration: scans the class for [McpResource],
        // [McpTemplate] and [McpAppUI] methods.
        .RegisterClass(TWeatherResource)   // a fixed resource + a URI template
        .RegisterClass(TDelphiDayAppUI)    // the ui:// MCP App, see MCPServer.Apps.pas

        // Static files served straight from disk, relative to SetBasePath.
        // The MIME type is detected from the extension; binary files are
        // base64-encoded automatically.
        .RegisterFile('index.md', 'Indice Documentazione')
        .RegisterFile('documentation\mcp\mcpconnect.pdf', 'MCPConnect Introduction')
      .BackToMCP

      // ---------------------------------------------------------------------
      //  .Prompts - reusable, parameterised message templates
      // ---------------------------------------------------------------------
      //  Prompts are user-initiated: the client usually surfaces them as slash
      //  commands or a menu, so they are the right place for canned workflows
      //  a human triggers explicitly.
      .Prompts
        .RegisterClass(TSamplePrompts)
      .BackToMCP

      // ---------------------------------------------------------------------
      //  .Tools - the functions the model may call
      // ---------------------------------------------------------------------
      //  RegisterClass walks the class with RTTI and registers every method
      //  carrying [McpTool]. Undecorated methods stay invisible, so helper
      //  code can live in the same class. Input schemas are generated from the
      //  parameter types by Neon; output schemas from the return type, for
      //  tools tagged 'structured'.
      .Tools

        // TTestTool exercises the serialization corner cases (structured
        // output, multi-content results, images). Enable it when you want to
        // see those; it is off by default to keep the tool list readable.
        //.RegisterClass(TTestTool)

        .RegisterClass(TDelphiDayTool)

      .BackToMCP
  ;
end;

class procedure TServerConfigurator.UnregisterFeatures(AServer: TJRPCServer);
begin
  // Configure<IMCPConfig> returns the *same* configuration object created in
  // ConfigureServer - plugins are created once per server - so the calls below
  // operate on the live registry.
  //
  // Unregistering an unknown name/class is a no-op, which is why this method
  // can safely be called at shutdown regardless of what was registered.
  AServer.Plugin.Configure<IMCPConfig>

  .Tools
    // Removes every tool that came from the class. A single tool can be
    // dropped by name with .UnregisterTool('delphiday_cart-add'), and
    // .ClearAll wipes the whole section.
    .UnregisterClass(TDelphiDayTool)
  .BackToMCP

  .Resources

    .UnregisterClass(TCppResource)
    .UnregisterClass(TWeatherResource)
    .UnregisterClass(TDelphiDayAppUI)

    // Files are keyed by the same relative path passed to RegisterFile.
    .UnregisterFile('index.md')
    .UnregisterFile('documentation\mcp\mcpconnect.pdf')
  .BackToMCP

  .Prompts
    .ClearAll
  .BackToMCP

  ;
end;

end.
