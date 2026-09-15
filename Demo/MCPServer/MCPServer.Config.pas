unit MCPServer.Config;

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.MCP.Server,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types.Base,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Legacy,

  MCPConnect.Content.Writers.RTL
  {$IFDEF FRAMEWORK_VCL}
  , MCPConnect.Content.Writers.VCL
  {$ENDIF}
  ;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TMCPServer);
    class procedure UnregisterFeatures(AServer: TMCPServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Logify,
  Logify.Adapter.Debug,

  MCPServer.Resources,
  MCPServer.Tools,
  MCPServer.Prompts,
  MCPServer.Middleware;

{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TMCPServer);
begin
  AServer

    .Middleware
      .Add(TMessageMiddleWare)
      .Add(TCallToolMiddleware)
      .Add(TDiscoverMiddleware)
      .Add(TDiagnosticMiddleware)
      .Add(TMCPMiddleware)
    .BackToApp

    .Plugin.Configure<IMCPConfig>

      .Server
        .SetName('todo-mcp-server')
        .SetVersion('1.0.0')
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)
      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['GET', 'POST'])
        .SetAllowedOrigins(['*'])
        //.SetRequireOrigin(True)
      .BackToMCP

      .Resources
        .RegisterClass(TTodoResource)
      .BackToMCP

      .Prompts
        .RegisterClass(TTodoPrompts)
      .BackToMCP

      .Tools
        .RegisterClass(TTodoTool)
      .BackToMCP

    .BackToApp

    // .Plugin.Configure<IAuthTokenConfig>
    //   .SetToken('my-secret-token')

    // Answer clients written against MCP 2025-06-18 too: serves the initialize
    // handshake and relaxes the header/_meta validation to Lenient. Partial
    // support - the results stay shaped as 2026-07-28 - which is what the
    // warning it logs says.
//     .Plugin.Configure<IMCPLegacyConfig>
//       .SetEnabled(True)
//     .ApplyConfig
  ;
end;

class procedure TServerConfigurator.UnregisterFeatures(AServer: TMCPServer);
begin
  AServer.Plugin.Configure<IMCPConfig>

  .Tools
    .UnregisterClass(TTodoTool)
  .BackToMCP

  .Resources
    .UnregisterClass(TTodoResource)
  .BackToMCP

  .Prompts
    .ClearAll()
  .BackToMCP

  ;
end;

initialization
  // Route MCPConnect's internal logging to the IDE's Event Log (OutputDebugString).
  // Swap TLogifyAdapterDebugFactory for a file/console adapter to keep a trace
  // outside the debugger. Lowering the level from Debug to Info silences the
  // per-request [PERF] timings.
  TLoggerAdapterRegistry.Instance.RegisterFactory(
    TLogifyAdapterDebugFactory.CreateAdapterFactory('Debug log', TLogLevel.Debug));

end.
