unit MCPAuthServer.Config;

interface

{$I 'MCPConnect.inc' }

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
  MCPConnect.Configuration.Neon,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,

  MCPConnect.Security.Token,
  MCPConnect.Session.Core;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  Neon.Core.Persistence,
  Neon.Core.Serializers.RTL,

  {$IFDEF DELPHI_JOSE_JWT}
  MCPConnect.Security.Token.JOSE,
  {$ENDIF}

  Logify,
  MCPAuthServer.Tools;


{ TServerConfigurator }

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  var NeonConfig :=
   TNeonConfiguration.Camel
     .RegisterSerializer(TJSONValueSerializer);

  LDataPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');

  AServer

    .Plugin.Configure<IOAuthConfig>
      .SetResource(GetEnvironmentVariable('OIDC_MCP_SERVER'))
      .EnableMetadataProxy(GetEnvironmentVariable('OIDC_AUTH_SERVER'))
      .AddTrustedIssuer(GetEnvironmentVariable('OIDC_TOKEN_ISSUER'))
      {$IFDEF DELPHI_JOSE_JWT}
      .SetTokenValidatorClass(TJoseTokenValidator)
      {$ELSE}
      .SetTokenValidatorClass(TClaimsTokenValidator)
      {$ENDIF}
      .AddScopesSupported('openid')
      .AddScopesSupported('email')
      .AddScopesSupported('profile')
      .AddScopesSupported(GetEnvironmentVariable('OIDC_MCP_SERVER') + '/access_as_user')
    .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .Server
        .SetName('delphi-oauth-server')
        .SetVersion('2.0.1')

        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))

        .RegisterWriter(TMCPImageWriter)
        .RegisterWriter(TMCPPictureWriter)
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)

      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['GET','POST', 'OPTIONS'])
        //.SetAllowedOrigins(['http://localhost', 'http://127.0.0.1'])
      .BackToMCP

      .Tools
        .RegisterClass(TTestTool)
        .SetSchemaNeonConfig(NeonConfig)
      .BackToMCP
  ;
end;

end.
