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
  TJRPCInvokerContext = record
    Garbage: IGarbageCollector;
    Request: TJRPCMethod;
    Responses: TMCPMessageQueue;
    Separator: string;

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

    function FindMethod(ARequest: TJRPCMethod): TRttiMethod;
    function GetRequestMethodName(ARequest: TJRPCMethod): string;
    function RetrieveNeonConfig(ANeonConfig: INeonConfiguration): INeonConfiguration;
    function GetParamName(LParam: TRttiParameter): string;
    function RequestToRttiParams(AMethod: TRttiMethod): TArray<TValue>;
    procedure InternalInvoke;
  public

    constructor Create(AContext: TJRPCInvokerContext);
  public
    class function GetExceptionDetail(E: Exception): string; static;
    class function HandleError(E: Exception; AId: TJRPCID): TJRPCError; static;
    class procedure Invoke(AContext: TJRPCInvokerContext); static;
  end;

implementation

uses
  System.Diagnostics,
  Logify;

// Checks the compatibility of the JSONValue with the function parameters
procedure CheckCompatibility(AParam: TRttiParameter; AValue: TJSONValue);
begin
  if AValue is TJSONNumber then
  begin
    if not (AParam.ParamType.TypeKind in [tkInteger, tkFloat, tkInt64]) then
      raise EJRPCInvalidParamsError.Create(AParam.Name, 'number');
  end
  else if AValue is TJSONString then
  begin
    if not (AParam.ParamType.TypeKind in [tkString, tkWChar, tkLString, tkWString, tkUString]) then
      raise EJRPCInvalidParamsError.Create(AParam.Name, 'string');
  end
  else if AValue is TJSONObject then
  begin
    if not (AParam.ParamType.TypeKind in [tkClass, tkRecord, tkInterface]) then
      raise EJRPCInvalidParamsError.Create(AParam.Name, 'object');
  end
  else if AValue is TJSONArray then
  begin
    if not (AParam.ParamType.TypeKind in [tkArray, tkDynArray]) then
      raise EJRPCInvalidParamsError.Create(AParam.Name, 'array');
  end
  else
    raise EJRPCInvalidParamsError.CreateFmt('Invalid parameter "%s"', [AParam.Name]);
end;

constructor TJRPCInvoker.Create(AContext: TJRPCInvokerContext);
begin
  inherited Create;
  if AContext.Separator <> '' then
    FSeparator := AContext.Separator
  else
    FSeparator := '/';
  FContext := AContext;
  FRttiType := TRttiUtils.GetType(AContext.ApiInstance);

  FNeonConfig := RetrieveNeonConfig(AContext.NeonConfig);
end;

function TJRPCInvoker.FindMethod(ARequest: TJRPCMethod): TRttiMethod;
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
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LMethod := FindMethod(FContext.Request);
  if not Assigned(LMethod) then
    raise EJRPCMethodNotFoundError.CreateFmt(SJRPCMethodNonFound, [FContext.Request.Method]);
  Logger.LogDebug('[PERF] JRPC [%s] FindMethod: %d ms', [FContext.Request.Method, LStopwatch.ElapsedMilliseconds]);

  LStopwatch := TStopwatch.StartNew;
  try
    LArgs := RequestToRttiParams(LMethod);
    FContext.Garbage.Add(LArgs);
  except
    Exception.RaiseOuterException(EJRPCInvalidParamsError.Create(SJRPCInvalidMethodParameters));
  end;
  Logger.LogDebug('[PERF] JRPC [%s] RequestToRttiParams: %d ms', [FContext.Request.Method, LStopwatch.ElapsedMilliseconds]);

  LStopwatch := TStopwatch.StartNew;
  try
    LResult := LMethod.Invoke(FContext.ApiInstance, LArgs);
    FContext.Garbage.Add(LResult);
  except
    on E: EJRPCException do
      raise;
    on E: Exception do
      Exception.RaiseOuterException(
        EJRPCException.CreateFmt(SJRPCErrorCallingApiMethod,
        [FContext.ApiInstance.ClassName, FContext.Request.Method]));
  end;
  Logger.LogDebug('[PERF] JRPC [%s] Method.Invoke: %d ms', [FContext.Request.Method, LStopwatch.ElapsedMilliseconds]);

  LResponse := TJRPCResponse.Create;
  try
    if FContext.Request is TJRPCRequest then
      LResponse.Id := TJRPCRequest(FContext.Request).Id;

    LStopwatch := TStopwatch.StartNew;
    if TRttiUtils.HasAttribute<JRPCNotificationAttribute>(LMethod) then
      LResponse.Result := nil
    else
      LResponse.Result := TNeon.ValueToJSON(LResult, FNeonConfig);
    Logger.LogDebug('[PERF] JRPC [%s] ValueToJSON: %d ms', [FContext.Request.Method, LStopwatch.ElapsedMilliseconds]);
    FContext.Responses.Enqueue(LResponse);
  except
    LResponse.Free;
    raise;
  end;
end;

class function TJRPCInvoker.GetExceptionDetail(E: Exception): string;
const
  MAX_DEPTH = 10;
var
  LCurrent: Exception;
  LDepth: Integer;
begin
  Result := '';
  LCurrent := E;
  LDepth := 0;
  while Assigned(LCurrent) and (LDepth < MAX_DEPTH) do
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + LCurrent.ClassName + ': ' + LCurrent.Message;
    LCurrent := LCurrent.InnerException;
    Inc(LDepth);
  end;
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

function TJRPCInvoker.GetRequestMethodName(ARequest: TJRPCMethod): string;
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
    Result.Error.Code := JRPC_INTERNAL_ERROR;
    Result.Error.Message := E.Message;
  end;
  Result.Error.Data := GetExceptionDetail(E);
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
            raise EJRPCInvalidParamsError.CreateFmt(SJRPCParamIndexNotFound, [LParamIndex, (FContext.Request.Params as TJSONArray).Count]);

          LParamJSON := (FContext.Request.Params as TJSONArray).Items[LParamIndex];
        end;

        TJRPCParamsType.ByName:
        begin
          if not (FContext.Request.Params as TJSONObject).TryGetValue(GetParamName(LParam), LParamJSON) then
            raise EJRPCInvalidParamsError.CreateFmt(SJRPCParamNotFound, [GetParamName(LParam)]);
        end;
      else
        raise EJRPCInvalidParamsError.Create(SJRPCUnknownParamsType);
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
