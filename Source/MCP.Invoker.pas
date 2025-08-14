unit MCP.Invoker;

interface

uses
  System.SysUtils, System.Rtti, System.Classes,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JRPC.Classes;

type
  /// <summary>
  ///   This exception is raised when an error occurs during the invocation of a JRPC method.
  ///   It provide the standard information required by the JSON-RPC specification.
  /// </summary>
  EMCPInvokerError = class(Exception)
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
  IMCPInvokable = interface
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
  TMCPMethodInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    FInstance: TObject;
    FMethod: TRttiMethod;
    FNeonConfig: INeonConfiguration;
  protected
    function GetParamName(LParam: TRttiParameter): string;
    function RequestToRttiParams(ARequest: TJRPCRequest): TArray<TValue>;
    function RttiResultToResponse(AResult: TValue): TValue;
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
  ///   using the MCP specific attributes.
  /// </summary>
  TMCPObjectInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    FInstance: TObject;
    FRttiType: TRttiType;
    FNeonConfig: INeonConfiguration;
    function FindMethod(ARequest: TJRPCRequest): TRttiMethod;
  public
    { IJRPCInvokable }
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);
    function GetNeonConfig: INeonConfiguration;
    procedure SetNeonConfig(AConfig: INeonConfiguration);

    constructor Create(AInstance: TObject);
  end;

const
  JRPC_ERROR_PARSE_ERROR =      -32700; // Invalid JSON was received by the server.
  JRPC_ERROR_INVALID_REQUEST =  -32600; // The JSON sent is not a valid Request object.
  JRPC_ERROR_METHOD_NOT_FOUND = -32601; // The method does not exist / is not available.
  JRPC_ERROR_INVALID_PARAMS =   -32602; // Invalid method parameter(s).
  JRPC_ERROR_INTERNAL_ERROR =   -32603; // Internal error.	Internal JSON-RPC error.

implementation

uses
  MCP.Attributes;

{ TMCPMethodInvoker }

// Check the compatibility of the JSONValue with the function parameters
procedure CheckCompatibility(AParam: TRttiParameter; AValue: TJSONValue);
begin
  if AValue is TJSONNumber then
  begin
    if not (AParam.ParamType.TypeKind in [tkInteger, tkFloat, tkInt64]) then
      raise EMCPInvokerError.Create(JRPC_ERROR_INVALID_PARAMS, Format('Invalid parameter for number [%s]', [AParam.Name]));
  end
  else if AValue is TJSONString then
  begin
    if not (AParam.ParamType.TypeKind in [tkString, tkWChar, tkLString, tkWString, tkUString]) then
      raise EMCPInvokerError.Create(JRPC_ERROR_INVALID_PARAMS, Format('Invalid parameter for string [%s]', [AParam.Name]));
  end
  else if AValue is TJSONObject then
  begin
    if not (AParam.ParamType.TypeKind in [tkClass, tkRecord, tkInterface]) then
      raise EMCPInvokerError.Create(JRPC_ERROR_INVALID_PARAMS, Format('Invalid parameter for object [%s]', [AParam.Name]));
  end
  else if AValue is TJSONArray then
  begin
    if not (AParam.ParamType.TypeKind in [tkArray, tkDynArray]) then
      raise EMCPInvokerError.Create(JRPC_ERROR_INVALID_PARAMS, Format('Invalid parameter for array [%s]', [AParam.Name]));
  end
  else
    raise EMCPInvokerError.Create(JRPC_ERROR_INVALID_PARAMS, Format('Invalid parameter [%s]', [AParam.Name]));
end;


function TMCPMethodInvoker.RequestToRttiParams(
  ARequest: TJRPCRequest): TArray<TValue>;

  function CastJSONValue(AParam: TRttiParameter; AValue: TJSONValue): TValue;
  begin
    CheckCompatibility(AParam, AValue);
    if AParam.ParamType.IsInstance then
      Result := TNeon.JSONToObject(AParam.ParamType, AValue, GetNeonConfig)
    else
      Result := TNeon.JSONToValue(AParam.ParamType, AValue, GetNeonConfig);
  end;

  function CastParamValue(AParam: TRttiParameter; AValue: TValue): TValue;
  begin
    if AValue.IsObject and (AValue.AsObject is TJSONValue) then
    begin
      Result := CastJSONValue(AParam, TJSONValue(AValue.AsObject));
    end
    else
      Result := AValue.Cast(AParam.ParamType.Handle);
  end;

var
  LParam: TRttiParameter;
  LParamIndex: Integer;
  LParamValue: TValue;
begin
  SetLength(Result, ARequest.Params.Count);

  LParamIndex := 0;
  for LParam in FMethod.GetParameters do
  begin
    case ARequest.Params.ParamsType of
      TJRPCParamsType.ByPos: LParamValue := ARequest.Params.ByPos[LParamIndex];
      TJRPCParamsType.ByName: LParamValue := ARequest.Params.ByName[GetParamName(LParam)];
      else
        raise EMCPInvokerError.Create(JRPC_ERROR_INTERNAL_ERROR, 'Unknown params type');
    end;

    Result[LParamIndex] := CastParamValue(LParam, LParamValue);

    Inc(LParamIndex);
  end;
end;

function TMCPMethodInvoker.RttiResultToResponse(AResult: TValue): TValue;
begin
  // TODO: check result type
  Result := AResult;
end;

procedure TMCPMethodInvoker.SetNeonConfig(AConfig: INeonConfiguration);
begin
  FNeonConfig := AConfig;
end;

constructor TMCPMethodInvoker.Create(AInstance: TObject; AMethod: TRttiMethod);
begin
  inherited Create;
  FInstance := AInstance;
  FMethod := AMethod;
  FNeonConfig := TNeonConfiguration.Default;
end;

function TMCPMethodInvoker.GetNeonConfig: INeonConfiguration;
begin
  Result := FNeonConfig;
end;

function TMCPMethodInvoker.GetParamName(LParam: TRttiParameter): string;
var
  LParamAttrib: MCPParamAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<MCPParamAttribute>(LParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := LParam.Name;
end;

procedure TMCPMethodInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse);
var
  LArgs: TArray<TValue>;
  LResult: TValue;
begin
  LArgs := RequestToRttiParams(ARequest);
  try
    LResult := FMethod.Invoke(FInstance, LArgs);
    AResponse.Id := ARequest.Id;
    AResponse.Result := RttiResultToResponse(LResult);
  except
    on E: EMCPInvokerError do
    begin
      AResponse.Error.Code := E.Code;
      AResponse.Error.Message := E.Message;
      if E.Data <> '' then
        AResponse.Error.Data := E.Data;
    end;
    on E: Exception do
    begin
      AResponse.Error.Code := JRPC_ERROR_INTERNAL_ERROR;
      AResponse.Error.Message := E.Message;
    end;
  end;
end;

{ TMCPObjectInvoker }

constructor TMCPObjectInvoker.Create(AInstance: TObject);
begin
  inherited Create;
  FInstance := AInstance;
  FRttiType := TRttiUtils.GetType(AInstance);
  FNeonConfig := TNeonConfiguration.Default;
end;

function TMCPObjectInvoker.FindMethod(ARequest: TJRPCRequest): TRttiMethod;
var
  LMethod: TRttiMethod;
  LToolAttrib: MCPToolAttribute;
  LMethodName: string;
begin
  Result := nil;
  for LMethod in FRttiType.GetMethods do
  begin
    LToolAttrib := TRttiUtils.FindAttribute<MCPToolAttribute>(LMethod);
    if Assigned(LToolAttrib) then
      LMethodName := LToolAttrib.Name
    else
      LMethodName := LMethod.Name;

    if ARequest.Method = LMethodName then
      Exit(LMethod);
  end;
end;

function TMCPObjectInvoker.GetNeonConfig: INeonConfiguration;
begin
  Result := FNeonConfig;
end;

procedure TMCPObjectInvoker.Invoke(ARequest: TJRPCRequest;
  AResponse: TJRPCResponse);
var
  LMethod: TRttiMethod;
  LMethodInvoker: IMCPInvokable;
begin
  LMethod := FindMethod(ARequest);
  if not Assigned(LMethod) then
  begin
    AResponse.Error.Code := JRPC_ERROR_METHOD_NOT_FOUND;
    AResponse.Error.Message := Format('Method [%s] non found', [ARequest.Method]);
    Exit;
  end;

  LMethodInvoker := TMCPMethodInvoker.Create(FInstance, LMethod);
  LMethodInvoker.NeonConfig := FNeonConfig;
  LMethodInvoker.Invoke(ARequest, AResponse);
end;

procedure TMCPObjectInvoker.SetNeonConfig(AConfig: INeonConfiguration);
begin
  FNeonConfig := AConfig;
end;

{ EMCPInvokerError }

constructor EMCPInvokerError.Create(ACode: Integer; const AMessage,
  AData: string);
begin
  inherited Create(AMessage);
  FCode := ACode;
  FData := AData;
end;

end.
