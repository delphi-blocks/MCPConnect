unit JRPC.Configuration.Authentication;

interface

uses
  System.Classes, System.SysUtils,
  JRPC.Configuration.Core;

{$SCOPEDENUMS ON}

type
  TAuthTokenLocation = (Bearer, Cookie, Header);

  IJRCPAuthTokenConfig = interface(IJRPCConfiguration)
    ['{5E98537E-1A8F-44D9-8100-CB3CCC7C0BFC}']
    function SetToken(const AToken: string): IJRCPAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IJRCPAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IJRCPAuthTokenConfig;
  end;

  [Implements(IJRCPAuthTokenConfig)]
  TJRCPAuthTokenConfig = class(TJRPCConfiguration, IJRCPAuthTokenConfig)
  private
    FToken: string;
    FLocation: TAuthTokenLocation;
    FCustomHeader: string;
  public
    function SetToken(const AToken: string): IJRCPAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IJRCPAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IJRCPAuthTokenConfig;

    property Token: string read FToken write FToken;
    property Location: TAuthTokenLocation read FLocation write FLocation;
    property CustomHeader: string read FCustomHeader write FCustomHeader;

  end;

implementation

{ TJRCPAuthTokenConfig }

function TJRCPAuthTokenConfig.SetToken(
  const AToken: string): IJRCPAuthTokenConfig;
begin
  FToken := AToken;
  Result := Self;
end;

function TJRCPAuthTokenConfig.SetTokenCustomHeader(
  const ACustomHeader: string): IJRCPAuthTokenConfig;
begin
  FCustomHeader := ACustomHeader;
  Result := Self;
end;

function TJRCPAuthTokenConfig.SetTokenLocation(
  ALocation: TAuthTokenLocation): IJRCPAuthTokenConfig;
begin
  FLocation := ALocation;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TJRCPAuthTokenConfig);

end.
