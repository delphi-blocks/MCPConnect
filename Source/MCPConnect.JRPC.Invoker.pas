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
unit MCPConnect.JRPC.Invoker;

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

  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core;


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

  TJRPCInvokerContext = record
    Garbage: IGarbageCollector;
    Request: TJRPCRequest;
    Responses: TMCPMessageQueue;

    ApiInstance: TObject;
    NeonConfig: INeonConfiguration;

    procedure SelectConfig(AApiConfig: INeonConfiguration; AJRPCConfig: TJRPCNeonConfig);
  end;

  /// <summary>
  ///   This class InternalInvoke a specific method on a given instance.
  ///   The method to be invoked is reached through RTTI
  ///   using the JRPC specific attributes.
  /// </summary>
  TJRPCInvoker = class
  private
    FRttiType: TRttiType;
    FContext: TJRPCInvokerContext;
    FNeonConfig: INeonConfiguration;
    FSeparator: string;

    function FindMethod(ARequest: TJRPCRequest): TRttiMethod;
    function GetRequestMethodName(ARequest: TJRPCRequest): string;
    function RetrieveNeonConfig(ANeonConfig: INeonConfiguration): INeonConfiguration;
    function GetParamName(LParam: TRttiParameter): string;
    function RequestToRttiParams(AMethod: TRttiMethod): TArray<TValue>;
    procedure InternalInvoke;
  public

    constructor Create(AContext: TJRPCInvokerContext);
  public
    class function HandleError(E: Exception; AId: TJRPCID): TJRPCError; static;
    class procedure Invoke(AContext: TJRPCInvokerContext);
  end;

implementation

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

constructor TJRPCInvoker.Create(AContext: TJRPCInvokerContext);
begin
  inherited Create;
  FSeparator := '/';
  FContext := AContext;
  FRttiType := TRttiUtils.GetType(AContext.ApiInstance);

  FNeonConfig := RetrieveNeonConfig(AContext.NeonConfig);

  TRttiUtils.HasAttribute<JRPCAttribute>(FRttiType,
    procedure (LAttrib: JRPCAttribute)
    begin
      if LAttrib.Tags.Exists('separator') then
        FSeparator := LAttrib.Tags.GetValueAs<string>('separator');
    end);

end;

function TJRPCInvoker.FindMethod(ARequest: TJRPCRequest): TRttiMethod;
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

procedure TJRPCInvoker.InternalInvoke;
var
  LMethod: TRttiMethod;
  LResponse: TJRPCResponse;
  LArgs: TArray<TValue>;
  LResult: TValue;
begin
  LMethod := FindMethod(FContext.Request);
  if not Assigned(LMethod) then
    raise EJRPCMethodNotFoundError.CreateFmt('Method [%s] non found', [FContext.Request.Method]);

  try
    LArgs := RequestToRttiParams(LMethod);
    FContext.Garbage.Add(LArgs);
  except
    raise EJRPCInvalidParamsError.Create('Invalid method parameters.');
  end;

  try
    LResult := LMethod.Invoke(FContext.ApiInstance, LArgs);
    FContext.Garbage.Add(LResult);
    LResponse := TJRPCResponse.Create;
  except
    on E: EJRPCException do
      raise;
    on E: Exception do
      raise EJRPCException.CreateFmt('Error calling Api method [%s.%s]',
        [FContext.ApiInstance.ClassName, FContext.Request.Method]);
  end;

  LResponse.Id := FContext.Request.Id;

  { TODO -opaolo -c :  31/03/2026 10:46:08 }
  if TRttiUtils.HasAttribute<JRPCNotificationAttribute>(LMethod) then
    LResponse.Result := nil
  else
    LResponse.Result := TNeon.ValueToJSON(LResult, FNeonConfig);

  FContext.Responses.Enqueue(LResponse);
end;

function TJRPCInvoker.GetParamName(LParam: TRttiParameter): string;
var
  LParamAttrib: JRPCAttribute;
begin
  LParamAttrib := TRttiUtils.FindAttribute<JRPCAttribute>(LParam);
  if Assigned(LParamAttrib) then
    Result := LParamAttrib.Name
  else
    Result := LParam.Name;
end;

function TJRPCInvoker.GetRequestMethodName(ARequest: TJRPCRequest): string;
var
  LSeparatorIndex: Integer;
begin
  LSeparatorIndex := Pos(FSeparator, ARequest.Method);
  if LSeparatorIndex > 0 then
    Result := Copy(ARequest.Method, LSeparatorIndex + 1, Length(ARequest.Method))
  else
    Result := '';
end;

class function TJRPCInvoker.HandleError(E: Exception; AId: TJRPCID): TJRPCError;
begin
  Result := TJRPCError.Create;
  if E is EJRPCException then
  begin
    Result.Id := AId;
    Result.Error.Code := EJRPCException(E).Code;
    Result.Error.Message := E.Message;
  end
  else if E is EJSONParseException then
  begin
    Result.Id := AId;
    Result.Error.Code := JRPC_PARSE_ERROR;
    Result.Error.Message := E.Message;
  end
  else
  begin
    Result.Id := AId;
    Result.Error.Code := JRPC_INVALID_REQUEST;
    Result.Error.Message := E.Message;
    Result.Error.Data := E.ClassName;
  end;
end;

class procedure TJRPCInvoker.Invoke(AContext: TJRPCInvokerContext);
var
  LInvoker: TJRPCInvoker;
begin
  LInvoker := TJRPCInvoker.Create(AContext);
  try
    LInvoker.InternalInvoke();
  finally
    LInvoker.Free;
  end;
end;

function TJRPCInvoker.RequestToRttiParams(AMethod: TRttiMethod): TArray<TValue>;

  function CastJSONValue(AParam: TRttiParameter; AValue: TJSONValue): TValue;
  begin
    if not Assigned(AValue) then
    begin
      Result := CreateNewValue(AParam.ParamType);
      Exit;
    end;

    CheckCompatibility(AParam, AValue);
    if AParam.ParamType.IsInstance then
      Result := TNeon.JSONToObject(AParam.ParamType, AValue, FNeonConfig)
    else
      Result := TNeon.JSONToValue(AParam.ParamType, AValue, FNeonConfig);
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
  LRttiParams := AMethod.GetParameters;

  if (Length(LRttiParams) = 1) and (TRttiUtils.HasAttribute<JRPCParamsAttribute>(LRttiParams[0])) then
  begin
    //Result := [TNeon.JSONToObject(LRttiParams[0].ParamType, ARequest.Params, TNeonConfiguration.Camel.SetMembers([TNeonMembers.Fields])) ];
    Result := [TNeon.JSONToObject(LRttiParams[0].ParamType, FContext.Request.Params, FNeonConfig) ];
  end
  else
  begin
    for LParam in LRttiParams do
    begin
      case FContext.Request.ParamsType of
        TJRPCParamsType.ByPos:
        begin
          if LParamIndex >= (FContext.Request.Params as TJSONArray).Count then
            raise EJRPCInvokerError.CreateFmt('Parameter with index "%d" not found (only %d parameters available)', [LParamIndex, (FContext.Request.Params as TJSONArray).Count]);

          LParamJSON := (FContext.Request.Params as TJSONArray).Items[LParamIndex];
        end;

        TJRPCParamsType.ByName:
        begin
          if not (FContext.Request.Params as TJSONObject).TryGetValue(GetParamName(LParam), LParamJSON) then
            raise EJRPCInvokerError.CreateFmt('Parameter "%s" not found', [GetParamName(LParam)]);
        end;
      else
        raise EJRPCInvokerError.Create(JRPC_INTERNAL_ERROR, 'Unknown params type');
      end;

      Result := Result + [CastJSONValue(LParam, LParamJSON)];
      Inc(LParamIndex);
    end;
  end;
end;

function TJRPCInvoker.RetrieveNeonConfig(ANeonConfig: INeonConfiguration): INeonConfiguration;
begin
  Result := ANeonConfig;
  if not Assigned(Result) then
    Result := TNeonConfiguration.Default;
end;

{ EJRPCInvokerError }

constructor EJRPCInvokerError.Create(ACode: Integer; const AMessage, AData: string);
begin
  inherited Create(AMessage);
  FCode := ACode;
  FData := AData;
end;

{ TJRPCInvokerContext }

procedure TJRPCInvokerContext.SelectConfig(AApiConfig: INeonConfiguration; AJRPCConfig: TJRPCNeonConfig);
begin
  NeonConfig := AApiConfig;

  if not Assigned(NeonConfig) then
    if Assigned(AJRPCConfig) then
      NeonConfig := AJRPCConfig.NeonConfig;

  if not Assigned(NeonConfig) then
    NeonConfig := TNeonConfiguration.Default;
end;

end.
