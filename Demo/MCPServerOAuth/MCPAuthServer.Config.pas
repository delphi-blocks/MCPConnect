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

  TEnvironment = class
  strict private
    class var FEnv: TStrings;
  public
    class function Get(const AName: string): string;

    class constructor Create;
    class destructor Destroy;
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

  // Resource identifier for this MCP server (e.g. "https://mcp.example.com")
  if TEnvironment.Get('OIDC_MCP_SERVER') = '' then
    raise Exception.Create('Environment variable "OIDC_MCP_SERVER" is not set. ' +
      'Please define it in the .env file or as a system environment variable.');

  // Base URL of the OIDC authorization server (e.g. "https://login.example.com")
  if TEnvironment.Get('OIDC_AUTH_SERVER') = '' then
    raise Exception.Create('Environment variable "OIDC_AUTH_SERVER" is not set. ' +
      'Please define it in the .env file or as a system environment variable.');

  // Expected "iss" claim in access tokens (e.g. "https://login.example.com/v2.0")
  if TEnvironment.Get('OIDC_TOKEN_ISSUER') = '' then
    raise Exception.Create('Environment variable "OIDC_TOKEN_ISSUER" is not set. ' +
      'Please define it in the .env file or as a system environment variable.');

  AServer
    .Plugin.Configure<IOAuthConfig>
      .SetResource(TEnvironment.Get('OIDC_MCP_SERVER'))
      .AddAuthorizationServer(TEnvironment.Get('OIDC_AUTH_SERVER'))
      //.EnableMetadataProxy(TEnvironment.Get('OIDC_AUTH_SERVER'))
      .AddTrustedIssuer(TEnvironment.Get('OIDC_TOKEN_ISSUER'))
      {$IFDEF DELPHI_JOSE_JWT}
      .SetTokenValidatorClass(TJoseTokenValidator)
      {$ELSE}
      .SetTokenValidatorClass(TClaimsTokenValidator)
      {$ENDIF}
      .AddScopesSupported('openid')
      .AddScopesSupported('email')
      .AddScopesSupported('profile')
      .AddScopesSupported(TEnvironment.Get('OIDC_MCP_SERVER') + '/access_as_user')
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
        .SetAllowedMethods(['GET', 'POST'])
        .SetAllowedOrigins(['*'])
      .BackToMCP

      .Tools
        .RegisterClass(TTestTool)
        .SetSchemaNeonConfig(NeonConfig)
      .BackToMCP
  ;
end;

{ TEnvironment }

class constructor TEnvironment.Create;
const
  EnvFileName = '.env';
begin
  FEnv := TStringList.Create;
  if FileExists(EnvFileName) then
    FEnv.LoadFromFile(EnvFileName);
end;

class destructor TEnvironment.Destroy;
begin
  FEnv.Free;
end;

class function TEnvironment.Get(const AName: string): string;
begin
  if FEnv.IndexOfName(AName) >= 0 then
    Result := FEnv.Values[AName]
  else
    Result := GetEnvironmentVariable(AName);
end;

end.
