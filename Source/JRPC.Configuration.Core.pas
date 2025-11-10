unit JRPC.Configuration.Core;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Rtti, System.Generics.Collections,
  System.Generics.Defaults, System.JSON,

  JSON.RPC;

type
  TAppConfigurator = class(TInterfacedObject)
  protected
    function GetConfigByInterfaceRef(AInterfaceRef: TGUID): IInterface; virtual; abstract;
  public
    function Configure<T: IInterface>: T;
  end;

  IJRPCApplication = interface;

  {$M+}
  TJRPCConfiguration = class;
  {$M-}

  TJRPCConfigurationClass = class of TJRPCConfiguration;

  IJRPCApplication = interface
    ['{1A6AE035-77BF-4191-9D40-EBF538F8BF6D}']

    function GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;
    function GetAppConfigurator: TAppConfigurator;
    function GetConfigurations: TEnumerable<TJRPCConfiguration>;

    property Plugin: TAppConfigurator read GetAppConfigurator;
  end;

  IJRPCConfiguration = interface
    ['{BA740194-E1E6-4FE8-8B02-1A8DC947352E}']
    function BackToApp: IJRPCApplication;
    function ApplyConfig: IJRPCApplication;
  end;

  /// <summary>
  ///   A non-reference counted IInterface implementation
  /// </summary>
  TJRPCConfiguration = class(TNoRefCountObject, IJRPCConfiguration)
  protected
    FApplication: IJRPCApplication;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoAfterCreate; virtual;

    property Application: IJRPCApplication read FApplication write FApplication;

    function BackToApp: IJRPCApplication;
    function ApplyConfig: IJRPCApplication; virtual;
  end;
  {$M-}

  ImplementsAttribute = class(TCustomAttribute)
  private
    FInterfaceRef: TGUID;
  public
    property InterfaceRef: TGUID read FInterfaceRef;
    constructor Create(AInterfaceRef: TGUID);
  end;

  TJRPCConfigRegistry = class(TObjectDictionary<TJRPCConfigurationClass, TJRPCConfiguration>)
  public
    procedure Add(AConfiguration: TJRPCConfiguration); overload;
    function GetApplicationConfig(AClass: TJRPCConfigurationClass; AApp: IJRPCApplication): TJRPCConfiguration;
  end;

  TJRPCConfigClassRegistry = class(TDictionary<TGUID, TJRPCConfigurationClass>)
  private
    class var FInstance: TJRPCConfigClassRegistry;
  protected
    class function GetInstance: TJRPCConfigClassRegistry; static; inline;
  public
    constructor Create; virtual;
    function GetImplementationOf(AInterfaceRef: TGUID): TJRPCConfigurationClass;
    procedure RegisterConfigClass(AConfigurationClass: TJRPCConfigurationClass);

  public
    class property Instance: TJRPCConfigClassRegistry read GetInstance;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  Neon.Core.Utils;

{ TJRPCConfiguration }

function TJRPCConfiguration.BackToApp: IJRPCApplication;
begin
  Result := FApplication;
end;

constructor TJRPCConfiguration.Create;
begin
end;

destructor TJRPCConfiguration.Destroy;
begin
  inherited;
end;

procedure TJRPCConfiguration.DoAfterCreate;
begin
  // Do nothing, allow subclasses to operate on FApplication
end;

function TJRPCConfiguration.ApplyConfig: IJRPCApplication;
begin
  Result := FApplication;
end;

{ ImplementsAttribute }

constructor ImplementsAttribute.Create(AInterfaceRef: TGUID);
begin
  FInterfaceRef := AInterfaceRef;
end;

{ TJRPCConfigClassRegistry }

class constructor TJRPCConfigClassRegistry.Create;
begin
  FInstance := nil;
end;

class destructor TJRPCConfigClassRegistry.Destroy;
begin
  FInstance.Free;
end;

constructor TJRPCConfigClassRegistry.Create;
begin
  inherited Create();
end;

function TJRPCConfigClassRegistry.GetImplementationOf(AInterfaceRef: TGUID): TJRPCConfigurationClass;
begin
  if not TryGetValue(AInterfaceRef, Result) then
    raise EJSONRPCException.Create('Implementation class not found');
end;

class function TJRPCConfigClassRegistry.GetInstance: TJRPCConfigClassRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TJRPCConfigClassRegistry.Create;
  Result := FInstance;
end;

procedure TJRPCConfigClassRegistry.RegisterConfigClass(AConfigurationClass: TJRPCConfigurationClass);
var
  LImplementsAttribute: ImplementsAttribute;
begin
  LImplementsAttribute := TRttiUtils.FindAttribute<ImplementsAttribute>(TRttiUtils.Context.GetType(AConfigurationClass));
  if not Assigned(LImplementsAttribute) then
    raise EJSONRPCException.CreateFmt('Attribute [Implements] not found for [%s] class', [AConfigurationClass.ClassName]);
  Add(LImplementsAttribute.InterfaceRef, AConfigurationClass);
end;

{ TAppConfigurator }

function TAppConfigurator.Configure<T>: T;
var
  LInterfaceRef: TGUID;
  LConfig: IInterface;
begin
  try
    LInterfaceRef := GetTypeData(TypeInfo(T))^.GUID;

    LConfig := GetConfigByInterfaceRef(LInterfaceRef);
    if not Supports(LConfig, LInterfaceRef, Result) then
      raise EJSONRPCException.Create('Invalid config');
  except
    on E: Exception do
    begin
      raise EJSONRPCException.CreateFmt('%s (%s)', [E.Message, GetTypeName(TypeInfo(T))]);
    end;
  end;
end;

{ TJRPCConfigRegistry }

procedure TJRPCConfigRegistry.Add(AConfiguration: TJRPCConfiguration);
begin
  Add(TJRPCConfigurationClass(AConfiguration.ClassType), AConfiguration);
end;

function TJRPCConfigRegistry.GetApplicationConfig(
  AClass: TJRPCConfigurationClass; AApp: IJRPCApplication): TJRPCConfiguration;
begin
  if not TryGetValue(AClass, Result) then
  begin
    Result := TRttiUtils.CreateInstance(AClass) as TJRPCConfiguration;
    try
      Result.Application := AApp;
      Result.DoAfterCreate;
      Add(Result);
    except
      Result.Free;
      raise;
    end;
  end;

end;

end.
