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
unit MCPConnect.JRPC.Server;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  MCPConnect.Configuration.Core;

type
  TJRPCServer = class(TComponent, IJRPCApplication)
  private
    FAppConfigurator: TAppConfigurator;
    FConfigRegistry: TJRPCConfigRegistry;
    FSessionManager: TObject;
  public
    { IJRPCApplication }
    function GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
    function GetAppConfigurator: TAppConfigurator;
    function GetConfigurations: TEnumerable<TJRPCConfiguration>;

    function GetConfiguration<T: TJRPCConfiguration>: T;
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
    procedure ApplyConfig(AConfig: IJRPCConfiguration);

    property Plugin: TAppConfigurator read GetAppConfigurator;
    property SessionManager: TObject read FSessionManager;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TAppConfiguratorImpl = class(TAppConfigurator)
  private
    FServer: TJRPCServer;
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; override;
  public
    property Server: TJRPCServer read FServer;
    constructor Create(AServer: TJRPCServer);
  end;


implementation

{ TJRPCServer }

uses
  MCPConnect.Session.Core,
  MCPConnect.Configuration.Session,
  MCPConnect.JRPC.Core;

//function TJRPCServer.Configure<T>: T;
//begin
//
//end;

procedure TJRPCServer.ApplyConfig(AConfig: IJRPCConfiguration);
begin
  if Supports(AConfig, ISessionConfig) then
  begin
    var LSessionManager := FSessionManager as TMCPSessionManager;
    var LSessionConfig := AConfig as TSessionConfig;
    if LSessionConfig.TimeoutMinutes > 0 then
      LSessionManager.TimeoutMinutes := LSessionConfig.TimeoutMinutes;
    if Assigned(LSessionConfig.SessionClass) then
      LSessionManager.SessionClass := LSessionConfig.SessionClass;
  end;
end;

constructor TJRPCServer.Create(AOwner: TComponent);
begin
  inherited;
  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
  FConfigRegistry := TJRPCConfigRegistry.Create([doOwnsValues]);
  FSessionManager := TMCPSessionManager.Create;
end;

destructor TJRPCServer.Destroy;
begin
  FSessionManager.Free;
  FAppConfigurator.Free;
  FConfigRegistry.Free;
  inherited;
end;

function TJRPCServer.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

function TJRPCServer.GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
begin
  Result := FConfigRegistry.GetApplicationConfig(AClass, Self);
end;

function TJRPCServer.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
var
  LConfig: TJRPCConfiguration;
  LConfigClass: TJRPCConfigurationClass;
begin
  LConfigClass := TJRPCConfigClassRegistry.Instance.GetImplementationOf(AInterfaceRef);
  LConfig := GetConfigByClassRef(LConfigClass);

  if not Supports(LConfig, AInterfaceRef, Result) then
    raise EJRPCException.Create('Invalid config');
end;

function TJRPCServer.GetConfiguration<T>: T;
begin
  Result := GetConfigByClassRef(TJRPCConfigurationClass(T)) as T;
end;

function TJRPCServer.GetConfigurations: TEnumerable<TJRPCConfiguration>;
begin
  Result := FConfigRegistry.Values;
end;

{ TAppConfiguratorImpl }

constructor TAppConfiguratorImpl.Create(AServer: TJRPCServer);
begin
  inherited Create;
  FServer := AServer;
end;

function TAppConfiguratorImpl.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
begin
  Result := FServer.GetConfigByInterfaceRef(AInterfaceRef);
end;

end.
