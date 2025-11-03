unit JSON.RPC.Invoker;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Types,
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
    function Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse): Boolean;
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
    function Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse): Boolean;
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
    function Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse): Boolean;
    function GetNeonConfig: INeonConfiguration;
    procedure SetNeonConfig(AConfig: INeonConfiguration);

    constructor Create(AInstance: TObject);
  end;

implementation

{ TJRPCMethodInvoker }

uses
  MCP.Utils;

// Checks the compatibility of the JSONValue with the function parameters
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
  LRttiParams: TArray<TRttiParameter>;
begin
  Result := [];

  LParamIndex := 0;
  LRttiParams := FMethod.GetParameters;

  if (Length(LRttiParams) = 1) and (TRttiUtils.HasAttribute<JRPCParamsAttribute>(LRttiParams[0])) then
  begin
    //Result := [TNeon.JSONToObject(LRttiParams[0].ParamType, ARequest.Params, TNeonConfiguration.Camel.SetMembers([TNeonMembers.Fields])) ];
    Result := [TNeon.JSONToObject(LRttiParams[0].ParamType, ARequest.Params, GetNeonConfig) ];
  end
  else
  begin
    for LParam in LRttiParams do
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
end;

function TJRPCMethodInvoker.RttiResultToResponse(AResult: TValue): TJSONValue;
begin
  if TRttiUtils.HasAttribute<JRPCNotificationAttribute>(FMethod) then
    Result := nil
  else
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

function TJRPCMethodInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse): Boolean;
var
  LArgs: TArray<TValue>;
  LResult: TValue;
  LGarbageCollector: IGarbageCollector;
begin
  Result := True;
  LGarbageCollector := TGarbageCollector.CreateInstance;
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

function TJRPCObjectInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse): Boolean;
var
  LMethod: TRttiMethod;
  LMethodInvoker: IJRPCInvokable;
begin
  LMethod := FindMethod(ARequest);
  if not Assigned(LMethod) then
  begin
    AResponse.Error.Code := JRPC_METHOD_NOT_FOUND;
    AResponse.Error.Message := Format('Method [%s] non found', [ARequest.Method]);
    Exit(False);
  end;

  LMethodInvoker := TJRPCMethodInvoker.Create(FInstance, LMethod);
  LMethodInvoker.NeonConfig := FNeonConfig;
  Result := LMethodInvoker.Invoke(ARequest, AResponse);
end;

function TJRPCObjectInvoker.GetRequestMethodName(ARequest: TJRPCRequest): string;
var
  LSeparatorIndex: Integer;
begin
  LSeparatorIndex := Pos(FSeparator, ARequest.Method);
  if LSeparatorIndex > 0 then
    Result := Copy(ARequest.Method, LSeparatorIndex + 1, Length(ARequest.Method))
  else
    Result := '';
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

end.
