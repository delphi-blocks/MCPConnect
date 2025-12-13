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
unit MCPConnect.Configuration.Session;

interface

uses
  System.Classes, System.SysUtils,
  MCPConnect.Configuration.Core,
  MCPConnect.Session.Core;

{$SCOPEDENUMS ON}

type
  /// <summary>
  ///   Location where the session ID is transmitted in HTTP requests
  /// </summary>
  TSessionIdLocation = (
    /// <summary>
    ///   Session ID in a custom header (default for MCP)
    /// </summary>
    Header,

    /// <summary>
    ///   Session ID in a cookie
    /// </summary>
    Cookie
  );

  /// <summary>
  ///   Configuration interface for session management.
  ///   Sessions are automatically created when this configuration is active.
  /// </summary>
  ISessionConfig = interface(IJRPCConfiguration)
    ['{8B5E9F7A-1D3C-4E5B-9A2F-6C8D4E5F7A8B}']

    /// <summary>
    ///   Set where the session ID should be looked for in the request
    /// </summary>
    function SetLocation(ALocation: TSessionIdLocation): ISessionConfig;

    /// <summary>
    ///   Set the name of the header or cookie containing the session ID.
    ///   Default: "Mcp-Session-Id" for MCP protocol
    /// </summary>
    function SetHeaderName(const AName: string): ISessionConfig;

    /// <summary>
    ///   Set the class to use for creating session data instances.
    ///   Use this to provide custom session data classes with typed properties.
    /// </summary>
    function SetSessionClass(AClass: TSessionDataClass): ISessionConfig;

    /// <summary>
    ///   Set session timeout in minutes. Default: 30
    /// </summary>
    function SetTimeout(AMinutes: Integer): ISessionConfig;

    /// <summary>
    ///   Get the session manager instance
    /// </summary>
    function GetManager: TSessionManager;

    /// <summary>
    ///   Get the configured location for session ID
    /// </summary>
    function GetLocation: TSessionIdLocation;

    /// <summary>
    ///   Get the configured header/cookie name
    /// </summary>
    function GetHeaderName: string;
  end;

  /// <summary>
  ///   Session configuration implementation.
  ///   Uses TSessionManager.Instance singleton for session storage.
  ///   Sessions are automatically created when session ID is not provided.
  /// </summary>
  [Implements(ISessionConfig)]
  TSessionConfig = class(TJRPCConfiguration, ISessionConfig)
  private
    FLocation: TSessionIdLocation;
    FHeaderName: string;
  public
    { ISessionConfig }
    function SetLocation(ALocation: TSessionIdLocation): ISessionConfig;
    function SetHeaderName(const AName: string): ISessionConfig;
    function SetSessionClass(AClass: TSessionDataClass): ISessionConfig;
    function SetTimeout(AMinutes: Integer): ISessionConfig;
    function GetManager: TSessionManager;
    function GetLocation: TSessionIdLocation;
    function GetHeaderName: string;

    constructor Create(AApp: IJRPCApplication); override;
    destructor Destroy; override;
  end;

implementation

{ TSessionConfig }

constructor TSessionConfig.Create(AApp: IJRPCApplication);
begin
  inherited;

  // MCP-friendly defaults
  FLocation := TSessionIdLocation.Header;
  FHeaderName := 'Mcp-Session-Id';
end;

destructor TSessionConfig.Destroy;
begin
  // Session manager is a singleton, don't free it
  inherited;
end;

function TSessionConfig.GetManager: TSessionManager;
begin
  Result := TSessionManager.Instance;
end;

function TSessionConfig.GetHeaderName: string;
begin
  Result := FHeaderName;
end;

function TSessionConfig.GetLocation: TSessionIdLocation;
begin
  Result := FLocation;
end;

function TSessionConfig.SetHeaderName(const AName: string): ISessionConfig;
begin
  FHeaderName := AName;
  Result := Self;
end;

function TSessionConfig.SetLocation(ALocation: TSessionIdLocation): ISessionConfig;
begin
  FLocation := ALocation;
  Result := Self;
end;

function TSessionConfig.SetSessionClass(AClass: TSessionDataClass): ISessionConfig;
begin
  TSessionManager.Instance.SessionClass := AClass;
  Result := Self;
end;

function TSessionConfig.SetTimeout(AMinutes: Integer): ISessionConfig;
begin
  TSessionManager.Instance.TimeoutMinutes := AMinutes;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TSessionConfig);

end.
