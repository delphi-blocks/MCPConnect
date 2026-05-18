unit MCPServer.Config;

interface

uses
  System.SysUtils, System.Classes,
  IdGlobal, IdContext, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Indy,

  MCPConnect.MCP.Server.Api,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
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
