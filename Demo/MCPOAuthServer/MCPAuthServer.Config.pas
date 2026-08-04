unit MCPAuthServer.Config;

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

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,

  MCPConnect.Session.Core;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Logify,
  MCPAuthServer.Tools;


{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  LDataPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');

  AServer

    .Plugin.Configure<IOAuthConfig>
      .SetResource(GetEnvironmentVariable('OIDC_MCP_SERVER'))
      .EnableMetadataProxy(GetEnvironmentVariable('OIDC_AUTH_SERVER'))
      .AddScopesSupported('openid')
      .AddScopesSupported('email')
      .AddScopesSupported('profile')
      .AddScopesSupported(GetEnvironmentVariable('OIDC_MCP_SERVER') + '/access_as_user')
    .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .Server
        .SetName('delphi-oauth-server')
        .SetVersion('2.0.1')

        // If not set, the server checks the registered tools, resources, etc. and automatically fills the capabilities.
        //.SetCapabilities([Tools, Resources])
        //.SetCapabilities(LCapabilities)
        //.SetCapabilities(
        //  procedure (ACapabilities: TServerCapabilities)
        //  begin
        //    ACapabilities.Tools.ListChanged := True;
        //  end
        //)

        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))

        .RegisterWriter(TMCPImageWriter)
        .RegisterWriter(TMCPPictureWriter)
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)

      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['GET','POST', 'OPTIONS'])
        .SetAllowedOrigins(['http://localhost', 'http://127.0.0.1'])
      .BackToMCP

      .Tools
        .RegisterClass(TTestTool)
      .BackToMCP
  ;
end;

end.
