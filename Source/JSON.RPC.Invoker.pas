unit JSON.RPC.Invoker;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JSON.RPC;

type
  /// <summary>
  ///   This exception is raised when an error occurs during the invocation of a JRPC method.
  ///   It provide the standard information required by the JSON-RPC specification.
  /// </summary>
  EJRPCInvokerError = class(Exception)
  private
    FCode: Integer;
    FData: string;
  public
    property Code: Integer read FCode;
    property Data: string read FData;

    constructor Create(ACode: Integer; const AMessage: string; const AData: string = '');
  end;

  /// <summary>
  ///   This interface define a standard method to handle JRPC call
  /// </summary>
  IJRPCInvokable = interface
  ['{246F6538-B87C-4164-B81C-74F0ABDD2FCD}']
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);
    function GetNeonConfig: INeonConfiguration;
    procedure SetNeonConfig(AConfig: INeonConfiguration);

    property NeonConfig: INeonConfiguration read GetNeonConfig write SetNeonConfig;
  end;

  /// <summary>
  ///   This class is responsible for invoking a method on a given instance.
  ///   The user has to provide the instance and the method to be invoked.
  /// </summary>
  TJRPCMethodInvoker = class(TInterfacedObject, IJRPCInvokable)
  private
    FInstance: TObject;
    FMethod: TRttiMethod;
    FNeonConfig: INeonConfiguration;
  protected
    function GetParamName(LParam: TRttiParameter): string;
    function RequestToRttiParams(ARequest: TJRPCRequest): TArray<TValue>;
    function RttiResultToResponse(AResult: TValue): TJSONValue;
  public
    { IJRPCInvokable }
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);
    function GetNeonConfig: INeonConfiguration;
    procedure SetNeonConfig(AConfig: INeonConfiguration);

    constructor Create(AInstance: TObject; AMethod: TRttiMethod);
  end;

  /// <summary>
  ///   This class invoke a specific method on a given instance.
  ///   The method to be invoked is reached through RTTI
  ///   using the JRPC specific attributes.
  /// </summary>
  TJRPCObjectInvoker = class(TInterfacedObject, IJRPCInvokable)
  private
    FInstance: TObject;
    FRttiType: TRttiType;
    FNeonConfig: INeonConfiguration;
    FSeparator: string;
    function FindMethod(ARequest: TJRPCRequest): TRttiMethod;
    function GetRequestMethodName(ARequest: TJRPCRequest): string;
  public
    { IJRPCInvokable }
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);
    function GetNeonConfig: INeonConfiguration;
    procedure SetNeonConfig(AConfig: INeonConfiguration);

    constructor Create(AInstance: TObject);
  end;

  // With this anonymous method, you can define custom disposal actions
  TDisposeAction = reference to procedure;

  /// <summary> 
  ///   IJRPCGarbageCollector defines an interface for managing and disposing
  ///   of objects or values that require explicit cleanup. It allows adding
  ///   values with optional custom disposal actions, and provides a method
  ///   to collect and dispose all tracked garbage.
  /// </summary>
  IJRPCGarbageCollector = interface
    ['{407C168F-A81C-4E41-96B2-BFEA94C58B0D}']
    procedure Add(const AValue: TValue); overload;
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;
    procedure Add(const AValues: TArray<TValue>); overload;
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;
    procedure CollectGarbage();
  end;

  /// <summary> 
  /// TJRPCGarbageCollector implements IJRPCGarbageCollector and provides a
  /// mechanism to track and dispose of objects or values. It supports
  /// custom disposal actions and recursive cleanup of arrays and class
  /// instances. Garbage is collected either on demand or automatically
  /// on destruction.
  /// NOTICE: When the garbage collector is destroyed, it will automatically
  /// collect and dispose all tracked garbage. If you get the instance through
  /// the *CreateInstance* method, then it will be destroyed automatically
  /// when it goes out of scope.
  /// </summary>
  TJRPCGarbageCollector = class(TInterfacedObject, IJRPCGarbageCollector)
  private
    FGarbage: TDictionary<TValue, TDisposeAction>;
    procedure CollectGarbageValue(const AValue: TValue);
    procedure CollectSingleGarbage(AGarbage: TPair<TValue, TDisposeAction>);
  public
    class function CreateInstance: IJRPCGarbageCollector; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Add(const AValue: TValue); overload;
    procedure Add(const AValue: TValue; AAction: TDisposeAction); overload;
    procedure Add(const AValues: TArray<TValue>); overload;
    procedure Add(const AValues: TArray<TValue>; AAction: TDisposeAction); overload;
    procedure CollectGarbage();
  end;

implementation

{ TJRPCMethodInvoker }

// Check the compatibility of the JSONValue with the function parameters
procedure CheckCompatibility(AParam: TRttiParameter; AValue: TJSONValue);
begin
  if AValue is TJSONNumber then
  begin
    if not (AParam.ParamType.TypeKind in [tkInteger, tkFloat, tkInt64]) then
      raise EJRPCInvokerError.Create(JRPC_INVALID_PARAMS, Format('Invalid parameter for number [%s]', [AParam.Name]));
  end
  else if AValue is TJSONString then
  begin
    if not (AParam.ParamType.TypeKind in [tkString, tkWChar, tkLString, tkWString, tkUString]) then
      raise EJRPCInvokerError.Create(JRPC_INVALID_PARAMS, Format('Invalid parameter for string [%s]', [AParam.Name]));
  end
  else if AValue is TJSONObject then
  begin
    if not (AParam.ParamType.TypeKind in [tkClass, tkRecord, tkInterface]) then
      raise EJRPCInvokerError.Create(JRPC_INVALID_PARAMS, Format('Invalid parameter for object [%s]', [AParam.Name]));
  end
  else if AValue is TJSONArray then
  begin
    if not (AParam.ParamType.TypeKind in [tkArray, tkDynArray]) then
      raise EJRPCInvokerError.Create(JRPC_INVALID_PARAMS, Format('Invalid parameter for array [%s]', [AParam.Name]));
  end
  else
    raise EJRPCInvokerError.Create(JRPC_INVALID_PARAMS, Format('Invalid parameter [%s]', [AParam.Name]));
end;

function CreateNewValue(AType: TRttiType): TValue;
var
  LAllocatedMem: Pointer;
begin
  case AType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64:   Result := TValue.From<Int64>(0);
    tkChar:    Result := TValue.From<UTF8Char>(#0);
    tkWChar:   Result := TValue.From<Char>(#0);
    tkFloat:   Result := TValue.From<Double>(0);
    tkString:  Result := TValue.From<UTF8String>('');
    tkWString: Result := TValue.From<string>('');
    tkLString: Result := TValue.From<UTF8String>('');
    tkUString: Result := TValue.From<string>('');
    tkClass:   Result := nil;
    tkRecord:
    begin
      LAllocatedMem := AllocMem(AType.TypeSize);
      try
        TValue.Make(LAllocatedMem, AType.Handle, Result);
      finally
        FreeMem(LAllocatedMem);
      end;
    end;
  else
    raise EJRPCInvokerError.CreateFmt('Error creating type: %s', [AType.Name]);
  end;
end;



function TJRPCMethodInvoker.RequestToRttiParams(ARequest: TJRPCRequest): TArray<TValue>;

  function CastJSONValue(AParam: TRttiParameter; AValue: TJSONValue): TValue;
  begin
    if not Assigned(AValue) then
    begin
      Result := CreateNewValue(AParam.ParamType);
      Exit;
    end;

    CheckCompatibility(AParam, AValue);
    if AParam.ParamType.IsInstance then
      Result := TNeon.JSONToObject(AParam.ParamType, AValue, GetNeonConfig)
    else
      Result := TNeon.JSONToValue(AParam.ParamType, AValue, GetNeonConfig);
  end;

  function CastParamValue(AParam: TRttiParameter; AValue: TValue): TValue;
  begin
    if AValue.IsObject and (AValue.AsObject is TJSONValue) then
      Result := CastJSONValue(AParam, TJSONValue(AValue.AsObject))
    else
      Result := AValue.Cast(AParam.ParamType.Handle);
  end;

var
  LParam: TRttiParameter;
  LParamIndex: Integer;
  //LParamValue: TValue;
  LParamJSON: TJSONValue;
begin
  Result := [];

  LParamIndex := 0;
  for LParam in FMethod.GetParameters do
  begin
    case ARequest.ParamsType of
      TJRPCParamsType.ByPos:
      begin
        LParamJSON := (ARequest.Params as TJSONArray).Items[LParamIndex];
        //LParamValue := ARequest.Params.ByPos[LParamIndex];
      end;

      TJRPCParamsType.ByName:
      begin
        LParamJSON := (ARequest.Params as TJSONObject).GetValue(GetParamName(LParam));
        //LParamValue := ARequest.Params.ByName[GetParamName(LParam)];
      end;
    else
      raise EJRPCInvokerError.Create(JRPC_INTERNAL_ERROR, 'Unknown params type');
    end;

    Result := Result + [CastJSONValue(LParam, LParamJSON)];
    Inc(LParamIndex);
  end;
end;

function TJRPCMethodInvoker.RttiResultToResponse(AResult: TValue): TJSONValue;
begin
  Result := TNeon.ValueToJSON(AResult, FNeonConfig);
end;

procedure TJRPCMethodInvoker.SetNeonConfig(AConfig: INeonConfiguration);
begin
  FNeonConfig := AConfig;
end;

constructor TJRPCMethodInvoker.Create(AInstance: TObject; AMethod: TRttiMethod);
begin
  inherited Create;
  FInstance := AInstance;
  FMethod := AMethod;
  FNeonConfig := TNeonConfiguration.Default;
end;

function TJRPCMethodInvoker.GetNeonConfig: INeonConfiguration;
begin
  Result := FNeonConfig;
end;

function TJRPCMethodInvoker.GetParamName(LParam: TRttiParameter): string;
var
  LParamAttrib: JRPCAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<JRPCAttribute>(LParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := LParam.Name;
end;

procedure TJRPCMethodInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse);
var
  LArgs: TArray<TValue>;
  LResult: TValue;
  LGarbageCollector: IJRPCGarbageCollector;
begin
  LGarbageCollector := TJRPCGarbageCollector.CreateInstance;
  LArgs := RequestToRttiParams(ARequest);
  try
    LGarbageCollector.Add(LArgs);
    LResult := FMethod.Invoke(FInstance, LArgs);
    try
      AResponse.Id := ARequest.Id;
      AResponse.Result := RttiResultToResponse(LResult);
    finally
      if LResult.IsObject then
        LResult.AsObject.Free;
    end;
  except
    on E: EJRPCInvokerError do
    begin
      AResponse.Error.Code := E.Code;
      AResponse.Error.Message := E.Message;
      if E.Data <> '' then
        AResponse.Error.Data := E.Data;
    end;
    on E: Exception do
    begin
      AResponse.Error.Code := JRPC_INTERNAL_ERROR;
      AResponse.Error.Message := E.Message;
    end;
  end;
end;

{ TJRPCObjectInvoker }

constructor TJRPCObjectInvoker.Create(AInstance: TObject);
begin
  inherited Create;
  FSeparator := '/';
  FInstance := AInstance;
  FRttiType := TRttiUtils.GetType(AInstance);
  FNeonConfig := TNeonConfiguration.Default;

  TRttiUtils.HasAttribute<JRPCAttribute>(FRttiType,
    procedure (LAttrib: JRPCAttribute)
    begin
      if LAttrib.Tags.Exists('separator') then
        FSeparator := LAttrib.Tags.GetValueAs<string>('separator');
    end);
end;

function TJRPCObjectInvoker.FindMethod(ARequest: TJRPCRequest): TRttiMethod;
var
  LMethod: TRttiMethod;
  LJRPCAttrib: JRPCAttribute;
  LMethodName: string;
  LRequestMethodName: string;
begin
  Result := nil;
  LRequestMethodName := GetRequestMethodName(ARequest);
  for LMethod in FRttiType.GetMethods do
  begin
    LJRPCAttrib := TRttiUtils.FindAttribute<JRPCAttribute>(LMethod);
    if Assigned(LJRPCAttrib) then
      LMethodName := LJRPCAttrib.Name
    else
      LMethodName := LMethod.Name;

    if LRequestMethodName = LMethodName then
      Exit(LMethod);
  end;
end;

function TJRPCObjectInvoker.GetNeonConfig: INeonConfiguration;
begin
  Result := FNeonConfig;
end;

procedure TJRPCObjectInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse);
var
  LMethod: TRttiMethod;
  LMethodInvoker: IJRPCInvokable;
begin
  LMethod := FindMethod(ARequest);
  if not Assigned(LMethod) then
  begin
    AResponse.Error.Code := JRPC_METHOD_NOT_FOUND;
    AResponse.Error.Message := Format('Method [%s] non found', [ARequest.Method]);
    Exit;
  end;

  LMethodInvoker := TJRPCMethodInvoker.Create(FInstance, LMethod);
  LMethodInvoker.NeonConfig := FNeonConfig;
  LMethodInvoker.Invoke(ARequest, AResponse);
end;

function TJRPCObjectInvoker.GetRequestMethodName(ARequest: TJRPCRequest): string;
var
  LSeparatorIndex: Integer;
begin
  LSeparatorIndex := Pos(FSeparator, ARequest.Method);
  if LSeparatorIndex > 0 then
    Result := Copy(ARequest.Method, LSeparatorIndex + 1, Length(ARequest.Method))
  else
    Result := ARequest.Method;
end;

procedure TJRPCObjectInvoker.SetNeonConfig(AConfig: INeonConfiguration);
begin
  FNeonConfig := AConfig;
end;

{ EJRPCInvokerError }

constructor EJRPCInvokerError.Create(ACode: Integer; const AMessage,
  AData: string);
begin
  inherited Create(AMessage);
  FCode := ACode;
  FData := AData;
end;

{ TJRPCGarbageCollector }

procedure TJRPCGarbageCollector.Add(const AValue: TValue);
begin
  Add(AValue, nil);
end;

procedure TJRPCGarbageCollector.Add(const AValue: TValue;
  AAction: TDisposeAction);
begin
  if FGarbage.ContainsKey(AValue) then
    Exit;
  FGarbage.Add(AValue, AAction);
end;

procedure TJRPCGarbageCollector.Add(const AValues: TArray<TValue>);
begin
  Add(AValues, nil);
end;

procedure TJRPCGarbageCollector.Add(const AValues: TArray<TValue>;
  AAction: TDisposeAction);
var
  LValue: TValue;
begin
  for LValue in AValues do
    Add(LValue, AAction);
end;

procedure TJRPCGarbageCollector.CollectGarbage;
var
  LGarbage: TPair<TValue, TDisposeAction>;
begin
  for LGarbage in FGarbage do
    CollectSingleGarbage(LGarbage);
  FGarbage.Clear;
end;

procedure TJRPCGarbageCollector.CollectGarbageValue(const AValue: TValue);
var
  LIndex: Integer;
begin
  case AValue.Kind of
    tkClass:
    begin
      if (AValue.AsObject <> nil) then
        //if not TRttiUtils.HasAttribute<SingletonAttribute>(AValue.AsObject.ClassType) then
        AValue.AsObject.Free;
    end;

    tkArray,
    tkDynArray:
    begin
      for LIndex := 0 to AValue.GetArrayLength - 1 do
        CollectGarbageValue(AValue.GetArrayElement(LIndex));
    end;
  end;
end;

procedure TJRPCGarbageCollector.CollectSingleGarbage(
  AGarbage: TPair<TValue, TDisposeAction>);
begin
  if Assigned(AGarbage.Value) then
    AGarbage.Value()
  else
    CollectGarbageValue(AGarbage.Key);
end;

constructor TJRPCGarbageCollector.Create;
begin
  inherited;
  FGarbage := TDictionary<TValue, TDisposeAction>.Create;
end;

class function TJRPCGarbageCollector.CreateInstance: IJRPCGarbageCollector;
begin
  Result := TJRPCGarbageCollector.Create;
end;

destructor TJRPCGarbageCollector.Destroy;
begin
  CollectGarbage;
  FGarbage.Free;
  inherited;
end;

end.
