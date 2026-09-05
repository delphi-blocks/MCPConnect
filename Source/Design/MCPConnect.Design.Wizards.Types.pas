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
unit MCPConnect.Design.Wizards.Types;

interface

{$SCOPEDENUMS ON}

type
  /// <summary>
  ///   Kind of executable hosting the generated MCP server
  /// </summary>
  TMCPAppKind = (Console, VCL, Service);

  /// <summary>
  ///   Transport exposing the generated MCP server
  /// </summary>
  TMCPTransportKind = (Stdio, Indy, WebBroker);

  /// <summary>
  ///   Authentication scheme protecting the server. The static token and OAuth
  ///   are alternatives, they are never configured together.
  /// </summary>
  TMCPAuthKind = (None, StaticToken, OAuth);

  /// <summary>
  ///   Where the static token is read from, mirrors TAuthTokenLocation of
  ///   MCPConnect.Configuration.Auth
  /// </summary>
  TMCPTokenLocation = (Bearer, Cookie, Header);

  /// <summary>
  ///   Options collected by the wizard and consumed by the creators.
  ///   The record is passed by value along the whole creator chain.
  /// </summary>
  TMCPProjectConfig = record
  public const
    DefaultServerName = 'delphi-mcp-server';
    DefaultServerVersion = '1.0.0';
    DefaultServerPort = 8080;
    DefaultMcpPath = '/mcp';
    DefaultServiceName = 'MCPServerService';
    DefaultCORSMethods = 'GET, POST, OPTIONS';
    DefaultTokenCustomHeader = 'X-API-Key';
  public
    AppKind: TMCPAppKind;
    Transport: TMCPTransportKind;

    ServerName: string;
    ServerVersion: string;
    ServerPort: Integer;
    McpPath: string;

    /// <summary>
    ///   Name the service registers itself with, used only when the host is a
    ///   Windows service
    /// </summary>
    ServiceName: string;

    UseCORS: Boolean;
    CORSAllowedMethods: string;
    CORSAllowedOrigins: string;

    AuthKind: TMCPAuthKind;

    TokenValue: string;
    TokenLocation: TMCPTokenLocation;
    TokenCustomHeader: string;

    OAuthResource: string;
    OAuthAuthServer: string;
    OAuthTrustedIssuer: string;
    OAuthScopes: string;

    CreateSampleUnit: Boolean;

    class function CreateDefault: TMCPProjectConfig; static;

    /// <summary>
    ///   True when the selected transport speaks HTTP, hence supports CORS,
    ///   authentication
    /// </summary>
    function IsHttpTransport: Boolean;

    /// <summary>
    ///   STDIO needs the standard input/output streams, so it cannot be hosted
    ///   by a VCL application
    /// </summary>
    function IsValidCombination: Boolean;

    /// <summary>
    ///   True when the token location needs an explicit header or cookie name
    /// </summary>
    function TokenNeedsCustomHeader: Boolean;
  end;

implementation

{ TMCPProjectConfig }

class function TMCPProjectConfig.CreateDefault: TMCPProjectConfig;
begin
  Result := Default(TMCPProjectConfig);

  Result.AppKind := TMCPAppKind.VCL;
  Result.Transport := TMCPTransportKind.Indy;

  Result.ServerName := DefaultServerName;
  Result.ServerVersion := DefaultServerVersion;
  Result.ServerPort := DefaultServerPort;
  Result.McpPath := DefaultMcpPath;
  Result.ServiceName := DefaultServiceName;

  Result.UseCORS := True;
  Result.CORSAllowedMethods := DefaultCORSMethods;
  Result.CORSAllowedOrigins := '';

  Result.AuthKind := TMCPAuthKind.None;
  Result.TokenLocation := TMCPTokenLocation.Bearer;
  Result.TokenCustomHeader := DefaultTokenCustomHeader;

  Result.CreateSampleUnit := True;
end;

function TMCPProjectConfig.IsHttpTransport: Boolean;
begin
  Result := Transport <> TMCPTransportKind.Stdio;
end;

function TMCPProjectConfig.IsValidCombination: Boolean;
begin
  Result := (Transport <> TMCPTransportKind.Stdio) or (AppKind = TMCPAppKind.Console);
end;

function TMCPProjectConfig.TokenNeedsCustomHeader: Boolean;
begin
  Result := TokenLocation <> TMCPTokenLocation.Bearer;
end;

end.
