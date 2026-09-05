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
unit MCPConnect.MCP.Server;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Contnrs,
  System.Generics.Collections,

  JRPC.Core,
  JRPC.Classes,
  JRPC.Invoker,
  MCPConnect.Configuration.Core;

resourcestring
  SMCPInvalidConfig = 'Invalid config';

type
  TMCPServer = class(TComponent, IJRPCApplication)
  private
    FAppConfigurator: TAppConfigurator;
    FConfigRegistry: TJRPCConfigRegistry;
    FSessionManager: TObject;
  public
    { IJRPCApplication }
    function GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
    function GetAppConfigurator: TAppConfigurator;
    function GetConfigurations: TArray<TJRPCConfiguration>;

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
    FServer: TMCPServer;
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; override;
  public
    property Server: TMCPServer read FServer;
    constructor Create(AServer: TMCPServer);
  end;


implementation

uses
  System.Diagnostics,

  Neon.Core.Persistence,
  Neon.Core.Utils,
  Logify;


procedure TMCPServer.ApplyConfig(AConfig: IJRPCConfiguration);
begin

end;

constructor TMCPServer.Create(AOwner: TComponent);
begin
  inherited;
  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
  FConfigRegistry := TJRPCConfigRegistry.Create([doOwnsValues]);
end;

destructor TMCPServer.Destroy;
begin
  FAppConfigurator.Free;
  FConfigRegistry.Free;
  inherited;
end;

function TMCPServer.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

function TMCPServer.GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
begin
  Result := FConfigRegistry.GetApplicationConfig(AClass, Self);
end;

function TMCPServer.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
var
  LConfig: TJRPCConfiguration;
  LConfigClass: TJRPCConfigurationClass;
begin
  LConfigClass := TJRPCConfigClassRegistry.Instance.GetImplementationOf(AInterfaceRef);
  LConfig := GetConfigByClassRef(LConfigClass);

  if not Supports(LConfig, AInterfaceRef, Result) then
    raise EJRPCException.Create(SMCPInvalidConfig);
end;

function TMCPServer.GetConfiguration<T>: T;
begin
  Result := GetConfigByClassRef(TJRPCConfigurationClass(T)) as T;
end;

function TMCPServer.GetConfigurations: TArray<TJRPCConfiguration>;
begin
  Result := FConfigRegistry.ValuesSnapshot;
end;

{ TAppConfiguratorImpl }

constructor TAppConfiguratorImpl.Create(AServer: TMCPServer);
begin
  inherited Create;
  FServer := AServer;
end;

function TAppConfiguratorImpl.GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;
begin
  Result := FServer.GetConfigByInterfaceRef(AInterfaceRef);
end;

end.
