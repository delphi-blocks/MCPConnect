unit MCPServer.Config;

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,

  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,

  Logify;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  MCPConnect.JRPC.Classes,
  MCPServer.Tools,
  MCPServer.Resources,
  MCPServer.Apps;


{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  LDataPath := TPath.Combine(TPath.GetAppPath, 'data');

  AServer

//    .Plugin.Configure<IAuthTokenConfig>
//      .SetToken('my-secret-token')
//    .ApplyConfig

    .Plugin.Configure<ISessionConfig>
      .SetLocation(TSessionIdLocation.Header)
      .SetHeaderName('Mcp-Session-Id')
      .SetTimeout(30)  // 30 minutes timeout
      .SetSessionClass(TShoppingSession)  // Use custom typed session
    .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .Server
        .SetName('delphi-mcp-server')
        .SetVersion('2.0.0')
        .SetCapabilities([Tools, Resources])
        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))

        .RegisterWriter(TMCPImageWriter)
        .RegisterWriter(TMCPPictureWriter)
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)

      .BackToMCP

      .MessageHandling

        .OnCancelled(
          procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
          begin
            Logger.LogDebug('Cancelled');
          end
        )

        .OnInitialized(
          procedure (AContext: TJRPCContext)
          begin
            Logger.LogDebug('Notification: Initialized');
          end
        )

        .OnSetLogLevel(
          procedure (AContext: TJRPCContext; ALevel: TLogSetLevel)
          begin
            Logger.LogDebug('Log level set to %s',
              [GetEnumName(TypeInfo(TLogSetLevel), Ord(ALevel))]);
          end
        )

      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['POST'])
        .SetAllowedOrigins(['http://localhost'])
      .BackToMCP

      .Resources
        .SetBasePath(LDataPath)

        .RegisterClass(TWeatherResource)
        .RegisterClass(TDeplphiDayAppUI)
        .RegisterFile('index.md', 'Indice Documentazione')
        .RegisterFile('documentation\mcp\mcpconnect.pdf', 'MCPConnect Introduction')
      .BackToMCP

      .Tools
        .RegisterClass(TTestTool)
        .RegisterClass(TDelphiDayTool)
        .RegisterClass(TShoppingCartTool)  // Session-based shopping cart
      .BackToMCP
  ;
end;

end.
