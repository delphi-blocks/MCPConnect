unit MCP.Invoker;

interface

uses
  System.SysUtils, System.Rtti, System.Classes,
  System.TypInfo, System.JSON,

  Neon.Core.Utils,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,

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
  end;

  /// <summary>
  ///   This class is responsible for invoking a method on a given instance.
  ///   The user has to provide the instance and the method to be invoked.
  /// </summary>
  TMCPMethodInvoker = class(TInterfacedObject, IMCPInvokable)
  private
    FInstance: TObject;
    FMethod: TRttiMethod;
  protected
    function GetParamName(LParam: TRttiParameter): string;
    function RequestToRttiParams(ARequest: TJRPCRequest): TArray<TValue>;
    function RttiResultToResponse(AResult: TValue): TValue;
  public
    { IJRPCInvokable }
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);

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
    function FindMethod(ARequest: TJRPCRequest): TRttiMethod;
  public
    { IJRPCInvokable }
    procedure Invoke(ARequest: TJRPCRequest; AResponse: TJRPCResponse);

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

function TMCPMethodInvoker.RequestToRttiParams(
  ARequest: TJRPCRequest): TArray<TValue>;
var
  LParamIndex: Integer;
begin
  // TODO: check parameters number and type
  SetLength(Result, ARequest.Params.Count);
  case ARequest.Params.ParamsType of
    TJRPCParamsType.ByPos:
    begin
      for LParamIndex := 0 to ARequest.Params.Count - 1 do
        Result[LParamIndex] := ARequest.Params.ByPos[LParamIndex];
    end;
    TJRPCParamsType.ByName:
    begin
      LParamIndex := 0;
      for var LParam in FMethod.GetParameters do
      begin
        Result[LParamIndex] := ARequest.Params.ByName[GetParamName(LParam)];
        Inc(LParamIndex);
      end;
    end;
  end;
end;

function TMCPMethodInvoker.RttiResultToResponse(AResult: TValue): TValue;
begin
  // TODO: check result type
  Result := AResult;
end;

constructor TMCPMethodInvoker.Create(AInstance: TObject; AMethod: TRttiMethod);
begin
  inherited Create;
  FInstance := AInstance;
  FMethod := AMethod;
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
  LMethodInvoker.Invoke(ARequest, AResponse);
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
