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
unit MCPConnect.Configuration.Core;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Rtti, System.Generics.Collections,
  System.Generics.Defaults, System.JSON,

  MCPConnect.JRPC.Core;

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

  /// <summary>
  ///   Core interface representing a JSON-RPC application that supports
  ///   plugin-based configuration. Provides access to registered configurations
  ///   and a fluent API for configuring the application via the Plugin property.
  /// </summary>
  IJRPCApplication = interface
    ['{1A6AE035-77BF-4191-9D40-EBF538F8BF6D}']

    /// <summary>
    ///   Retrieves a configuration instance by its class reference.
    /// </summary>
    /// <param name="AClass">Configuration class to retrieve</param>
    /// <returns>Configuration instance (creates if not exists)</returns>
    function GetConfigByClassRef(AClass: TJRPCConfigurationClass): TJRPCConfiguration;

    /// <summary>
    ///   Returns the application configurator used for the fluent configuration API.
    /// </summary>
    /// <returns>Configurator instance that provides access to all configuration interfaces</returns>
    function GetAppConfigurator: TAppConfigurator;

    /// <summary>
    ///   Returns all registered configuration instances.
    /// </summary>
    /// <returns>Enumerable collection of all active configurations</returns>
    function GetConfigurations: TEnumerable<TJRPCConfiguration>;

    /// <summary>
    ///   Provides fluent access to configuration interfaces. Use Configure&lt;T&gt;
    ///   to access specific configuration types (IMCPConfig, IAuthTokenConfig, etc.).
    /// </summary>
    property Plugin: TAppConfigurator read GetAppConfigurator;
  end;

  /// <summary>
  ///   Base interface for all configuration plugins in the JSON-RPC framework.
  ///   Provides fluent API support through BackToApp and ApplyConfig methods
  ///   that allow method chaining and navigation between configurations.
  /// </summary>
  IJRPCConfiguration = interface
    ['{BA740194-E1E6-4FE8-8B02-1A8DC947352E}']
    /// <summary>
    ///   Returns to the application interface without applying configuration changes.
    ///   Useful for navigating between configurations without committing changes.
    /// </summary>
    /// <returns>The parent IJRPCApplication instance for further configuration</returns>
    function BackToApp: IJRPCApplication;

    /// <summary>
    ///   Applies the current configuration changes and returns to the application
    ///   interface. This commits all configuration changes made through the
    ///   fluent API methods.
    /// </summary>
    /// <returns>The parent IJRPCApplication instance for further configuration</returns>
    function ApplyConfig: IJRPCApplication;

    /// <summary>
    ///   See **IsApplied* property. 
    /// </summary>
    function GetIsApplied: Boolean;

    /// <summary>
    ///   Indicates whether the configuration has been applied. This can be used
    ///   to track whether changes have been committed.
    /// </summary>
    property IsApplied: Boolean read GetIsApplied;

  end;

  /// <summary>
  ///   A non-reference counted IInterface implementation
  /// </summary>
  TJRPCConfiguration = class(TNoRefCountObject, IJRPCConfiguration)
  protected
    FApplication: IJRPCApplication;
    FIsApplied: Boolean;
  public
    constructor Create(AApp: IJRPCApplication); virtual;
    destructor Destroy; override;

    function BackToApp: IJRPCApplication;
    function ApplyConfig: IJRPCApplication; virtual;
    function GetIsApplied: Boolean; virtual;

    property Application: IJRPCApplication read FApplication;
    property IsApplied: Boolean read GetIsApplied;

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
  Result := ApplyConfig;
end;

constructor TJRPCConfiguration.Create(AApp: IJRPCApplication);
begin
  inherited Create;
  FApplication := AApp;
  FIsApplied := False;
end;

destructor TJRPCConfiguration.Destroy;
begin
  inherited;
end;

function TJRPCConfiguration.GetIsApplied: Boolean;
begin
  Result := FIsApplied;
end;

function TJRPCConfiguration.ApplyConfig: IJRPCApplication;
begin
  FIsApplied := True;
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
    raise EJRPCException.Create('Implementation class not found');
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
    raise EJRPCException.CreateFmt('Attribute [Implements] not found for [%s] class', [AConfigurationClass.ClassName]);
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
      raise EJRPCException.Create('Invalid config');
  except
    on E: Exception do
    begin
      raise EJRPCException.CreateFmt('%s (%s)', [E.Message, GetTypeName(TypeInfo(T))]);
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
var
  LArgs: TArray<TValue>;
begin
  SetLength(LArgs, 1);
  LArgs[0] := TValue.From<IJRPCApplication>(AApp);
  if not TryGetValue(AClass, Result) then
  begin
    Result := TRttiUtils.CreateInstance(AClass, LArgs) as TJRPCConfiguration;
    try
      Add(Result);
    except
      Result.Free;
      raise;
    end;
  end;

end;

end.
