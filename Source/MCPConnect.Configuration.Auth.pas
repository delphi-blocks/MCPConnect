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
unit MCPConnect.Configuration.Auth;

interface

uses
  System.Classes, System.SysUtils,
  MCPConnect.Configuration.Core;

{$SCOPEDENUMS ON}

type
  TAuthTokenLocation = (Bearer, Cookie, Header);

  IAuthTokenConfig = interface(IJRPCConfiguration)
  ['{5E98537E-1A8F-44D9-8100-CB3CCC7C0BFC}']
    function SetToken(const AToken: string): IAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;
  end;

  [Implements(IAuthTokenConfig)]
  TAuthTokenConfig = class(TJRPCConfiguration, IAuthTokenConfig)
  private
    FToken: string;
    FLocation: TAuthTokenLocation;
    FCustomHeader: string;
  public
    function SetToken(const AToken: string): IAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;

    property Token: string read FToken write FToken;
    property Location: TAuthTokenLocation read FLocation write FLocation;
    property CustomHeader: string read FCustomHeader write FCustomHeader;

  end;

implementation

{ TAuthTokenConfig }

function TAuthTokenConfig.SetToken(
  const AToken: string): IAuthTokenConfig;
begin
  FToken := AToken;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenCustomHeader(
  const ACustomHeader: string): IAuthTokenConfig;
begin
  FCustomHeader := ACustomHeader;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenLocation(
  ALocation: TAuthTokenLocation): IAuthTokenConfig;
begin
  FLocation := ALocation;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TAuthTokenConfig);

end.
