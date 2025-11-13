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
unit MCPConnect.JRPC.Core;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Tags,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL;

const
  // Standard JSON-RPC error codes
	JRPC_PARSE_ERROR      = -32700; // Invalid JSON was received by the server.
	JRPC_INVALID_REQUEST  = -32600; // The JSON sent is not a valid Request object.
	JRPC_METHOD_NOT_FOUND = -32601; // The method does not exist / is not available.
	JRPC_INVALID_PARAMS   = -32602; // Invalid method parameter(s).
	JRPC_INTERNAL_ERROR   = -32603; // Internal error.	Internal JSON-RPC error.

type
  EJSONRPCException = class(Exception);

  TJRPCContext = class;

  TJRPCParamsType = (ByPos, ByName, Null);

  JRPCNotificationAttribute = class(TCustomAttribute)
  end;

  ContextAttribute = class(TCustomAttribute)
  end;

  JRPCAttribute = class(TCustomAttribute)
  private
    FName: string;
    FAdditionalTags: string;
    FAdditional: string;
    FTags: TAttributeTags;
    function GetTags: TAttributeTags;
  public
    property Name: string read FName;
    property AdditionalTags: string read FAdditional write FAdditional;

    property Tags: TAttributeTags read GetTags write FTags;

    constructor Create(const AName: string; const AAdditionalTags: string = '');
    destructor Destroy; override;
  end;

  JRPCPathAttribute = class(JRPCAttribute);
  JRPCMethodAttribute = class(JRPCAttribute);
  JRPCParamAttribute = class(JRPCAttribute);
  JRPCParamsAttribute = class(TCustomAttribute);

  TJRPCID = record
  private
    [NeonInclude] id: TValue;
  public
    class operator Implicit(ASource: Integer): TJRPCID;
    class operator Implicit(ASource: string): TJRPCID;

    class operator Implicit(const ASource: TJRPCID): Integer;
    class operator Implicit(const ASource: TJRPCID): string;
  end;

  TJRPCEnvelope = class
  public const
	  JSONRPC_VERSION = '2.0';
  protected
    FId: TJRPCID;
    FJsonRpc: string;
    function GetNeonConfig: INeonConfiguration;
  public
    constructor Create;

    function ToJson: string; virtual;
    procedure FromJson(const AJSON: string); virtual;

    [NeonProperty('jsonrpc')]
    property JsonRpc: string read FJsonRpc write FJsonRpc;

    [NeonProperty('id'), NeonUnwrapped]
    property Id: TJRPCID read FId write FId;
  end;

  TJRPCError = class
  private
    FCode: NullInteger;
    FData: TValue;
    FMessage: NullString;
  public
    [NeonProperty('code')]
    property Code: NullInteger read FCode write FCode;

    [NeonProperty('message')]
    property Message: NullString read FMessage write FMessage;

    [NeonProperty('data'), NeonInclude(IncludeIf.NotNull)]
    property Data: TValue read FData write FData;
  end;

  TJRPCRequest = class(TJRPCEnvelope)
  protected
    FMethod: string;
    FParams: TJSONValue;

    function GetPositionParams: TJSONArray;
    function GetNamedParams: TJSONObject;
    procedure SetParams(AValue: TJSONValue);
  public
    constructor Create;
    destructor Destroy; override;

    function ParamsType: TJRPCParamsType;
    function ParamsCount: Integer;

    procedure AddPositionParam(const AValue: TValue);
    procedure AddNamedParam(const AName: string; const AValue: TValue);

    [NeonProperty('method')]
    property Method: string read FMethod write FMethod;

    [NeonProperty('params')]
    property Params: TJSONValue read FParams write SetParams;

  public
    class function CreateFromJson(const AJSON: string): TJRPCRequest;
  end;

  // Class representing a JSON-RPC response
  TJRPCResponse = class(TJRPCEnvelope)
  protected
    FError: TJRPCError;
    FResult: TJSONValue;
    procedure SetResult(AValue: TJSONValue);
  public
    constructor Create;
    destructor Destroy; override;

    function IsError: Boolean;
    function IsNotification: Boolean;

    [NeonProperty('error')]
    property Error: TJRPCError read FError write FError;

    [NeonProperty('result')]
    property Result: TJSONValue read FResult write SetResult;
  public
    class function CreateFromJson(const AJSON: string): TJRPCResponse;
  end;

  /// <summary>
  ///   Custom serializer for the TJRPCRequest class.
  /// </summary>
  TJRequestSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  /// <summary>
  ///   Custom serializer for the TJRPCResponse class.
  /// </summary>
  TJResponseSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  /// <summary>
  ///   Custom serializer for the TValue record.
  /// </summary>
  TJValueSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TJRPCConstructorProxy = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
    FNeonConfig: INeonConfiguration;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration);

    property TypeTClass: TClass read FTypeTClass;
    property NeonConfig: INeonConfiguration read FNeonConfig;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TJRPCConstructorProxy;
  end;

  TJRPCRegistry = class(TObjectDictionary<string, TJRPCConstructorProxy>)
  private
    class var FInstance: TJRPCRegistry;
  protected
    class function GetInstance: TJRPCRegistry; static; inline;
  private
    FSeparator: string;
    function GetClassName(AClass: TClass): string;
    procedure AddClass(AClass: TClass; AProxy: TJRPCConstructorProxy);
    procedure RemoveClass(AClass: TClass);
  public
    constructor Create; virtual;
    function RegisterClass(AClass: TClass): TJRPCConstructorProxy; overload;
    function RegisterClass(AClass: TClass; ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;
    function RegisterClass<T: class>: TJRPCConstructorProxy; overload;
    function RegisterClass<T: class>(ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;
    function RegisterClass<T: class>(const AConstructorFunc: TFunc<TObject>): TJRPCConstructorProxy; overload;
    function RegisterClass<T: class>(const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration): TJRPCConstructorProxy; overload;

    function ClassExists(AClass: TClass): Boolean; overload;
    function ClassExists<T: class>: Boolean; overload;

    procedure UnregisterClass(AClass: TClass);

    function GetClassInstance(const AName: string; out Value: TObject): Boolean;
    function GetConstructorProxy(const AName: string; out Value: TJRPCConstructorProxy): Boolean;

    // Class/Method separator character
    property Separator: string read FSeparator write FSeparator;

    class property Instance: TJRPCRegistry read GetInstance;
    class constructor Create;
    class destructor Destroy;
  end;

  TJRPCContextData = class(TDictionary<TClass, TObject>);

  TJRPCContext = class(TObject)
  private
    FRequest: TJRPCRequest;
    FResponse: TJRPCResponse;
    FContextData: TJRPCContextData;
  protected
    function GetRequest: TJRPCRequest;
    function GetResponse: TJRPCResponse;
    procedure AddConfigurations(AObject: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContent(AObject: TObject);

    function GetContextDataAs<T: class>: T; overload;
    function GetContextDataAs(AClass: TClass): TObject; overload;
    function FindContextDataAs<T: class>: T; overload;
    function FindContextDataAs(AClass: TClass): TObject; overload;

    procedure Inject(AObject: TObject); overload;
    procedure Inject(AInterface: IInterface); overload;

    property Request: TJRPCRequest read GetRequest;
    property Response: TJRPCResponse read GetResponse;
  end;


function JRPCNeonConfig: INeonConfiguration;

implementation

uses
  MCPConnect.Configuration.Core;

function JRPCNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel
    .SetPrettyPrint(True)
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJRequestSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;


{ TJRPCEnvelope }

constructor TJRPCEnvelope.Create;
begin
  FJsonRpc := JSONRPC_VERSION;
end;

procedure TJRPCEnvelope.FromJson(const AJSON: string);
begin
  TNeon.JSONToObject(Self, AJSON, JRPCNeonConfig);
end;

function TJRPCEnvelope.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TJSONValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJValueSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;

function TJRPCEnvelope.ToJson: string;
begin
  Result := TNeon.ObjectToJSONString(Self, JRPCNeonConfig);
end;

constructor TJRPCRequest.Create;
begin
  inherited;
  FParams := TJSONNull.Create;
end;

class function TJRPCRequest.CreateFromJson(const AJSON: string): TJRPCRequest;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
  end;
end;

destructor TJRPCRequest.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TJRPCRequest.GetNamedParams: TJSONObject;
begin
  if FParams is TJSONArray then
    raise EJSONRPCException.Create('Only named params are allowed');

  if FParams is TJSONObject then
    Exit(FParams as TJSONObject);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONObject.Create;
    FParams := Result;
  end;
  // This should never happen
  raise EJSONRPCException.Create('Not a valid type for named allowed');
end;

function TJRPCRequest.GetPositionParams: TJSONArray;
begin
  if FParams is TJSONObject then
    raise EJSONRPCException.Create('Only position params are allowed');

  if FParams is TJSONArray then
    Exit(FParams as TJSONArray);

  if FParams is TJSONNull then
  begin
    FParams.Free;
    Result := TJSONArray.Create;
    FParams := Result;
  end;
  // This should never happen
  raise EJSONRPCException.Create('Not a valid type for position allowed');
end;

function TJRPCRequest.ParamsCount: Integer;
begin
  Result := 0;

  if FParams is TJSONNull then
    Exit(0);

  if FParams is TJSONArray then
    Exit((FParams as TJSONArray).Count);

  if FParams is TJSONObject then
    Exit((FParams as TJSONObject).Count);
end;

function TJRPCRequest.ParamsType: TJRPCParamsType;
begin
  Result := TJRPCParamsType.Null;

  if FParams is TJSONNull then
    Exit(TJRPCParamsType.Null);

  if FParams is TJSONArray then
    Exit(TJRPCParamsType.ByPos);

  if FParams is TJSONObject then
    Exit(TJRPCParamsType.ByName);
end;

procedure TJRPCRequest.AddNamedParam(const AName: string; const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, GetNeonConfig);
  if Assigned(LParam) then
    GetNamedParams.AddPair(AName, LParam);
end;

procedure TJRPCRequest.AddPositionParam(const AValue: TValue);
var
  LParam: TJSONValue;
begin
  LParam := TNeon.ValueToJSON(AValue, JRPCNeonConfig);
  if Assigned(LParam) then
    GetPositionParams.AddElement(LParam);
end;

procedure TJRPCRequest.SetParams(AValue: TJSONValue);
begin
  if Assigned(FParams) and (FParams <> AValue) then
  begin
    FParams.Free;
    FParams := AValue;
  end;
end;

{ TJRPCID }

class operator TJRPCID.Implicit(ASource: Integer): TJRPCID;
begin
  Result.id := ASource;
end;

class operator TJRPCID.Implicit(ASource: string): TJRPCID;
begin
  Result.id := ASource;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): string;
begin
  if ASource.Id.IsType<Integer> then
    raise Exception.Create('The Id is an integer');

  Result := ASource.Id.AsString;
end;

class operator TJRPCID.Implicit(const ASource: TJRPCID): Integer;
begin
  if ASource.Id.IsType<string> then
    raise Exception.Create('The Id is a string');

  Result := ASource.Id.AsInteger;
end;

{ TJResponseSerializer }

class function TJResponseSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TJResponseSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCResponse.ClassInfo;
end;

function TJResponseSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LRes: TJRPCResponse;
begin
  LRes := AData.AsType<TJRPCResponse>;

  var j := AValue.FindValue('result');
  if Assigned(j) then
    LRes.Result := j;

  var id := AValue.FindValue('id');
  if id is TJSONNumber then
    LRes.Id := id.GetValue<Integer>
  else
    LRes.Id := id.GetValue<string>;

  LRes.JsonRpc := AValue.FindValue('jsonrpc').Value;

  var err := AValue.FindValue('error');
  if Assigned(err) then
  begin
    var typ := TRttiUtils.Context.GetType(LRes.Error.ClassType);
    AContext.ReadDataMember(err, typ, LRes.Error, True);
  end;

  Result := TValue.From<TJRPCResponse>(LRes);
end;

function TJResponseSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LRes: TJRPCResponse;
begin
  LRes := AValue.AsObject as TJRPCResponse;

  if LRes.IsError then
    AContext.GetConfiguration.Rules.ForClass<TJRPCResponse>
      .AddIgnoreMembers(['Result'])
  else
    AContext.GetConfiguration.Rules.ForClass<TJRPCResponse>
      .AddIgnoreMembers(['Error']);

  Result := AContext.WriteDataMember(AValue, False);
end;


{ TJRPCResponse }

constructor TJRPCResponse.Create;
begin
  inherited;
  FResult := TJSONNull.Create;
  FError := TJRPCError.Create;
end;

destructor TJRPCResponse.Destroy;
begin
  FError.Free;
  FResult.Free;
  inherited;
end;

class function TJRPCResponse.CreateFromJson(const AJSON: string): TJRPCResponse;
begin
  Result := Self.Create;
  try
    Result.FromJson(AJSON);
  except
    Result.Free;
  end;
end;

{ TJRPCResponse }

function TJRPCResponse.IsError: Boolean;
begin
  Result := True;

  if FError.Code.IsNull or FError.Message.IsNull then
    Result := False;
end;

function TJRPCResponse.IsNotification: Boolean;
begin
  Result := (not IsError) and not Assigned(FResult);
end;

procedure TJRPCResponse.SetResult(AValue: TJSONValue);
begin
  if Assigned(FResult) and (FResult <> AValue) then
  begin
    FResult.Free;
    FResult := AValue;
  end;
end;

{ TJValueSerializer }

class function TJValueSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = GetTargetInfo;
end;

function TJValueSerializer.Deserialize(AValue: TJSONValue; const AData: TValue;
  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LType: TRttiType;
  LValue: TValue;
begin
  LValue := TValue.Empty;

  if AValue is TJSONNumber then
  begin
    if (AValue as TJSONNumber).Value.Contains('.') then
      LType := TRttiUtils.Context.GetType(TypeInfo(Integer))
    else
      LType := TRttiUtils.Context.GetType(TypeInfo(Double));

    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end
  else if AValue is TJSONString then
  begin
    LType := TRttiUtils.Context.GetType(TypeInfo(string));
    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end
  else if TJSONUtils.IsBool(AValue) then
  begin
    LType := TRttiUtils.Context.GetType(TypeInfo(Boolean));
    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
  end;

  Result := LValue;
end;

class function TJValueSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TValue);
end;

function TJValueSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
begin
  Result := nil;

  if AValue.TypeInfo = TypeInfo(TValue) then
  begin
    var vv := AValue.AsType<TValue>;
    if not Assigned(TRttiUtils.Context.GetType(vv.TypeInfo)) then
      Exit(nil);

    Result := AContext.WriteDataMember(AValue.AsType<TValue>, False)
  end;
end;

{ TJRequestSerializer }

class function TJRequestSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);
end;

function TJRequestSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LIdValue: TJSONValue;
  LReq: TJRPCRequest;
  LParams: TJSONValue;
begin
  LReq := AData.AsType<TJRPCRequest>;

  LReq.Method := AValue.GetValue<string>('method');
  LReq.JsonRpc := AValue.GetValue<string>('jsonrpc');
  LIdValue := AValue.GetValue<TJSONValue>('id', nil);
  if not Assigned(LIdValue) then
    LReq.Id := ''
  else if LIdValue is TJSONNumber then
    LReq.Id := LIdValue.AsType<Integer>
  else
    LReq.Id := LIdValue.Value;

  // "params" is optional
  if AValue.TryGetValue<TJSONValue>('params', LParams) then
  begin
    // Position parameters
    if LParams is TJSONArray then
      LReq.Params := LParams.Clone as TJSONArray;

    // Named parameters
    if LParams is TJSONObject then
      LReq.Params := LParams.Clone as TJSONObject;
  end;

  Result := TValue.From<TJRPCRequest>(LReq);
end;

class function TJRequestSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TJRPCRequest.ClassInfo;
end;

function TJRequestSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
begin
  Result := AContext.WriteDataMember(AValue, False);
end;

{ JRPCAttribute }

constructor JRPCAttribute.Create(const AName, AAdditionalTags: string);
begin
  FName := AName;
  FAdditionalTags := AAdditionalTags;

  FTags := TAttributeTags.Create();
end;

destructor JRPCAttribute.Destroy;
begin
  FTags.Free;
  inherited;
end;

function JRPCAttribute.GetTags: TAttributeTags;
begin
  if (FTags.Count = 0) and not FAdditionalTags.IsEmpty then
    FTags.Parse(FAdditionalTags);

  Result := FTags;
end;

{ TJRPCRegistry }

procedure TJRPCRegistry.AddClass(AClass: TClass; AProxy: TJRPCConstructorProxy);
begin
  Self.Add(GetClassName(AClass), AProxy);
end;

function TJRPCRegistry.ClassExists(AClass: TClass): Boolean;
var
  LItem: TJRPCConstructorProxy;
begin
  Result := TryGetValue(GetClassName(AClass), LItem);
end;

function TJRPCRegistry.ClassExists<T>: Boolean;
begin
  Result := ClassExists(TClass(T));
end;

class constructor TJRPCRegistry.Create;
begin
  FInstance := nil;
end;

constructor TJRPCRegistry.Create;
begin
  inherited Create([doOwnsValues]);
  FSeparator := '/';
end;

function TJRPCRegistry.GetClassInstance(const AName: string;
  out Value: TObject): Boolean;
var
  LConstructorProxy: TJRPCConstructorProxy;
begin
  Result := False;
  if GetConstructorProxy(AName, LConstructorProxy) then
    Value := LConstructorProxy.ConstructorFunc();
end;

function TJRPCRegistry.GetClassName(AClass: TClass): string;
var
  LJRPCAttribute: JRPCAttribute;
begin
  LJRPCAttribute := TRttiUtils.FindAttribute<JRPCAttribute>(TRttiUtils.Context.GetType(AClass));
  if Assigned(LJRPCAttribute) then
  begin
    Result := LJRPCAttribute.Name;
    if LJRPCAttribute.GetTags.Exists('separator') then
      FSeparator := LJRPCAttribute.GetTags.GetValueAs<string>('separator');
  end
  else
    Result := AClass.QualifiedClassName;
end;

function TJRPCRegistry.GetConstructorProxy(const AName: string; out Value: TJRPCConstructorProxy): Boolean;
var
  LClassName: string;
  LSeparatorIndex: Integer;
begin
  LSeparatorIndex := Pos(FSeparator, AName);
  if LSeparatorIndex > 0 then
    LClassName := Copy(AName, 0, LSeparatorIndex - 1)
  else
    LClassName := AName;

  Value := nil;
  Result := Self.TryGetValue(LClassName, Value);
end;

class function TJRPCRegistry.GetInstance: TJRPCRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TJRPCRegistry.Create;
  Result := FInstance;
end;

function TJRPCRegistry.RegisterClass(AClass: TClass): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(AClass, nil, nil);
  Self.AddClass(AClass, Result);
end;

function TJRPCRegistry.RegisterClass<T>: TJRPCConstructorProxy;
begin
  Result := RegisterClass<T>(nil, nil);
end;

function TJRPCRegistry.RegisterClass(AClass: TClass;
  ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(AClass, nil, ANeonConfig);
  Self.AddClass(AClass, Result);
end;

function TJRPCRegistry.RegisterClass<T>(ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := RegisterClass<T>(nil, ANeonConfig);
end;

function TJRPCRegistry.RegisterClass<T>(const AConstructorFunc: TFunc<TObject>;
  ANeonConfig: INeonConfiguration): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(TClass(T), AConstructorFunc, ANeonConfig);
  AddClass(TClass(T), Result);
end;

function TJRPCRegistry.RegisterClass<T>(const AConstructorFunc: TFunc<TObject>): TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(TClass(T), AConstructorFunc, nil);
  AddClass(TClass(T), Result);
end;

procedure TJRPCRegistry.RemoveClass(AClass: TClass);
begin
  Self.Remove(GetClassName(AClass));
end;

procedure TJRPCRegistry.UnregisterClass(AClass: TClass);
begin
  Self.RemoveClass(AClass);
end;

class destructor TJRPCRegistry.Destroy;
begin
  FInstance.Free;
end;

{ TJRPCConstructorProxy }

function TJRPCConstructorProxy.Clone: TJRPCConstructorProxy;
begin
  Result := TJRPCConstructorProxy.Create(FTypeTClass, FConstructorFunc, FNeonConfig);
end;

constructor TJRPCConstructorProxy.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>; ANeonConfig: INeonConfiguration);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;
  FNeonConfig := ANeonConfig;

  // Default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      begin
        Result := TRttiUtils.CreateInstance(FTypeTClass);
      end;
end;

{ TJRPCContext }

procedure TJRPCContext.AddConfigurations(AObject: TObject);
var
  LApplication: IJRPCApplication;
  LConfig: TJRPCConfiguration;
begin
  if Supports(AObject, IJRPCApplication, LApplication) then
  begin
    for LConfig in LApplication.GetConfigurations do
    begin
      AddContent(LConfig);
    end;
  end;
end;

procedure TJRPCContext.AddContent(AObject: TObject);
begin
  if AObject is TJRPCRequest then
    FRequest := TJRPCRequest(AObject)
  else if AObject is TJRPCResponse then
    FResponse := TJRPCResponse(AObject)
  else
  begin
    FContextData.Add(AObject.ClassType, AObject);
    AddConfigurations(AObject);
  end;
end;

constructor TJRPCContext.Create;
begin
  inherited Create;
  FContextData := TJRPCContextData.Create;
  AddContent(Self);
end;

destructor TJRPCContext.Destroy;
begin
  FContextData.Free;
  inherited;
end;

function TJRPCContext.FindContextDataAs(AClass: TClass): TObject;
begin
  if not FContextData.TryGetValue(AClass, Result) then
    Result := nil;
end;

function TJRPCContext.FindContextDataAs<T>: T;
begin
  Result := FindContextDataAs(TClass(T)) as T;
end;

function TJRPCContext.GetContextDataAs(AClass: TClass): TObject;
begin
  Result := FindContextDataAs(AClass);
  if not Assigned(Result) then
    raise EJSONRPCException.CreateFmt('Context: object "%s" not found', [AClass.ClassName]);
end;

function TJRPCContext.GetContextDataAs<T>: T;
begin
  Result := GetContextDataAs(TClass(T)) as T;
end;

function TJRPCContext.GetRequest: TJRPCRequest;
begin
  if not Assigned(FRequest) then
    raise EJSONRPCException.Create('Request not found');

  Result := FRequest;
end;

function TJRPCContext.GetResponse: TJRPCResponse;
begin
  if not Assigned(FResponse) then
    raise EJSONRPCException.Create('Response not found');

  Result := FResponse;
end;

procedure TJRPCContext.Inject(AInterface: IInterface);
begin
  Inject(AInterface as TObject);
end;

procedure TJRPCContext.Inject(AObject: TObject);
var
  LRttiType: TRttiType;
begin
  LRttiType := TRttiUtils.Context.GetType(AObject.ClassType);
  TRttiUtils.ForEachFieldWithAttribute<ContextAttribute>(LRttiType,
    function (AField: TRttiField; AAttr: ContextAttribute): Boolean
    var
      LValue: TValue;
    begin
      if not (AField.FieldType is TRttiInstanceType) then
        raise EJSONRPCException.Create('Context variables should be an object');

      LValue := GetContextDataAs(TRttiInstanceType(AField.FieldType).MetaclassType);
      AField.SetValue(AObject, LValue);
      Result := True;
    end);
end;

end.

