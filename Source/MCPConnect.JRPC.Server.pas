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
  public
    { IJRPCApplication }
    function GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
    function GetAppConfigurator: TAppConfigurator;
    function GetConfigurations: TEnumerable<TJRPCConfiguration>;

    function GetConfiguration<T: TJRPCConfiguration>: T;
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface;

    property Plugin: TAppConfigurator read GetAppConfigurator;

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
  MCPConnect.JRPC.Core;

//function TJRPCServer.Configure<T>: T;
//begin
//
//end;

constructor TJRPCServer.Create(AOwner: TComponent);
begin
  inherited;
  FAppConfigurator := TAppConfiguratorImpl.Create(Self);
  FConfigRegistry := TJRPCConfigRegistry.Create([doOwnsValues]);
end;

destructor TJRPCServer.Destroy;
begin
  FAppConfigurator.Free;
  FConfigRegistry.Free;
  inherited;
end;

function TJRPCServer.GetAppConfigurator: TAppConfigurator;
begin
  Result := FAppConfigurator;
end;

function TJRPCServer.GetConfigByClassRef(
  AClass: TJRPCConfigurationClass): TJRPCConfiguration;
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
    raise EJSONRPCException.Create('Invalid config');
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
