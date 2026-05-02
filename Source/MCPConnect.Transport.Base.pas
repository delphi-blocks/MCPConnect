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
unit MCPConnect.Transport.Base;

interface

uses
  System.Classes, System.SysUtils,
  IdCustomHTTPServer, IdContext, IdGlobal,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

const
  HTTP_CODE_OK = 200;
  HTTP_CODE_ACCEPTED = 202;
  HTTP_CODE_NOCONTENT = 204;
  HTTP_CODE_UNAUTHORIZED = 401;
  HTTP_CODE_FORBIDDEN = 403;
  HTTP_CODE_NOTFOUND = 404;
  HTTP_CODE_NOTALLOWED = 405;
  HTTP_CODE_NOTACCEPTABLE = 406;

type
  IMCPSSEResponseWriter = interface
    ['{68598454-50C5-4892-B8E0-81687CC2F4DE}']
    procedure Write(const AValue: string); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;
    function Connected: Boolean;
  end;

  TMCPSSEResponseWriter = class(TInterfacedObject, IMCPSSEResponseWriter)
  protected
    function SplitString(const AValue: string): TArray<string>;
    function InternalConnected: Boolean; virtual; abstract;
    procedure InternalWriteLine(const AValue: string); virtual; abstract;
  public
    { IMCPSSEResponseWriter }
    procedure Write(const AValue: string); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;
    function Connected: Boolean;
  end;

  TMCPSSEResponseWriterProc = reference to procedure (AWriter: IMCPSSEResponseWriter);



  TMCPTransportHeaders = class
  private type
    THeaders = class(TDictionary<string, string>)

    end;
  public
    Headers: TDictionary<string, string>;

    constructor Create;
    destructor Destroy; override;

    function GetHeader(const AName: string): string;
  end;

  TMCPTransportRequest = class(TMCPTransportHeaders)
  public
    Url: string;
    Command: string;
    Content: string;

    function GetCookie(const AName: string): string;
  end;

  TMCPTransportRequestConverter = reference to procedure (ARequest: TMCPTransportRequest);


  TMCPTransportResponse = class(TMCPTransportHeaders)
  private
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
  public
    WriterProc: TMCPSSEResponseWriterProc;
    Content: string;
    Code: Integer;

    procedure SSEContent(AWriterProc: TMCPSSEResponseWriterProc);
    procedure SetCookie(const AName, AValue: string);
    property ContentType: string read GetContentType write SetContentType;
  end;

  TMCPTransportResponseConverter = reference to procedure (AResponse: TMCPTransportResponse);

  IMCPTransportHandler = interface
  ['{B2966C2A-7594-4B30-95D9-D702AE20633E}']
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
  end;

  TMCPTransportHandler = class(TInterfacedObject, IMCPTransportHandler)
  private
    FRequest: TMCPTransportRequest;
    FResponse: TMCPTransportResponse;

    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;
    FSession: TMCPSessionBase;

    FMCPConfig: TMCPConfig;
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    FSessionConfig: TSessionConfig;
  private
    procedure InjectCORS;
    function CheckOrigin: Boolean;
    function CheckAuthorization: Boolean;
    function ExtractSessionId: string;
    function HandleSession: TMCPSessionBase;
    procedure HandleMessage(AMessage: TJRPCMessage; AResponseList: TJRPCMessages);

    procedure HandleGET;
    procedure HandlePOST;
    procedure HandleOPTIONS;
  public
    constructor Create(AServer: TJRPCServer);
    destructor Destroy; override;

    { IMCPHttpHandler }
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
  end;

implementation

uses
  System.IOUtils,
  Logify,
  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Invoker;


{ TMCPTransportHandler }

constructor TMCPTransportHandler.Create(AServer: TJRPCServer);
begin
  FRequest := TMCPTransportRequest.Create;
  FResponse := TMCPTransportResponse.Create;

  FServer := AServer;
  FMCPConfig := FServer.GetConfiguration<TMCPConfig>;
  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
  FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
end;

destructor TMCPTransportHandler.Destroy;
begin
  FRequest.Free;
  FResponse.Free;

  Logger.LogDebug('MCPTransportHandler destroyed');
  inherited;
end;

function TMCPTransportHandler.CheckAuthorization: Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if FRequest.GetHeader('Authorization') <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if FRequest.GetCookie(FAuthTokenConfig.CustomHeader) <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if FRequest.GetHeader(FAuthTokenConfig.CustomHeader) <> FAuthTokenConfig.Token then
          Exit(False);
      end;

    else
      raise EJRPCException.Create('Invalid token location');
    end;
  end;
end;

function TMCPTransportHandler.CheckOrigin: Boolean;
var
  LOrigin, LHeader: string;
begin
  if not Assigned(FMCPConfig) then
    raise EJRPCException.Create('Error retrieving MCP configuration');

  Result := True;
  if Length(FMCPConfig.Security.AllowedOrigins) = 0 then
    Exit;

  LHeader := FRequest.GetHeader('Origin');
  if LHeader.IsEmpty then
    Exit;

  for LOrigin in FMCPConfig.Security.AllowedOrigins do
    if LHeader.StartsWith(LOrigin) then
      Exit;

  Result := False;
end;

procedure TMCPTransportHandler.ProcessRequest(ARequestConverter:
    TMCPTransportRequestConverter; AResponseConverter:
    TMCPTransportResponseConverter);
begin
  ARequestConverter(FRequest);

  try
    if not CheckOrigin then
    begin
      FResponse.Code := HTTP_CODE_FORBIDDEN;
      FResponse.Content := '';
      Exit;
    end;

    if not CheckAuthorization then
    begin
      FResponse.Code := HTTP_CODE_FORBIDDEN;
      FResponse.Content := '';
      Exit;
    end;

    FGarbage := TGarbageCollector.CreateInstance;
    FContext := TJRPCContext.Create;

    FGarbage.Add(FContext);
    FContext.AddContent(FGarbage);
    FContext.AddContent(FServer);

    // Handle session (get existing or create new)
    FSession := HandleSession;

    // Add session to context if available
    if Assigned(FSession) then
      FContext.AddContent(FSession);

    if FRequest.Command = 'GET' then
      HandleGET
    else if FRequest.Command = 'POST' then
      HandlePOST
    else if FRequest.Command = 'OPTIONS' then
      HandleOPTIONS
    else
      FResponse.Code := HTTP_CODE_NOTALLOWED;

    InjectCORS;
  except
    on E: EJRPCException do
    begin
      FResponse.Code := 500;
      FResponse.ContentType := 'application/json';
      FResponse.Content := E.ToJSON;
    end;

    on E: Exception do
    begin
      FResponse.Code := 500;
      FResponse.ContentType := 'application/json';
      FResponse.Content := Format('{"message": "%s"}', [E.Message]);
    end;
  end;

  AResponseConverter(FResponse);
end;

function TMCPTransportHandler.ExtractSessionId: string;
begin
  Result := '';

  if not Assigned(FSessionConfig) then
    Exit;

  case FSessionConfig.GetLocation of
    TSessionIdLocation.Header:
      Result := FRequest.GetHeader(FSessionConfig.GetHeaderName);

    TSessionIdLocation.Cookie:
      Result := FRequest.GetCookie(FSessionConfig.GetHeaderName);
  end;

  Result := Result.Trim;
end;

procedure TMCPTransportHandler.HandleGET;
begin
  //AResponse.Headers.AddOrSet('Allow', 'POST');
  //AResponse.Code := HTTP_CODE_NOTALLOWED;

  FResponse.SSEContent(
    procedure (AWriter: IMCPSSEResponseWriter)
    begin
      while AWriter.Connected  do
      begin
        // TODO:
        // 1. Get the active session (exit if not found)
        // 2. Tell the session that there's a SSE channel
        // 3. Read messages from notification manager of the current session
        // 4. Serialize the notifications and send to the Writer
      end;
    end
  );
end;

procedure TMCPTransportHandler.HandleMessage(AMessage: TJRPCMessage; AResponseList: TJRPCMessages);
var
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokerCtx: TJRPCInvokerContext;
begin
  if (AMessage is TJRPCNotification) then
  begin
    if Assigned(FSession) then
    begin
      var LNotification := AMessage as TJRPCNotification;
      Logger.LogDebug('Enqueing notification [%s]', [LNotification.Method]);
      FSession.Inbound.Notifications.Enqueue(LNotification.Clone);
    end;
    Exit;
  end;

  if AMessage is TJRPCResponse then
  begin
    if Assigned(FSession) then
    begin
      var LRes := AMessage as TJRPCResponse;
      Logger.LogDebug('Enqueing response id [%s]', [LRes.Id.AsString]);
      FSession.Inbound.Responses.Enqueue(LRes.Clone);
    end;
    Exit;
  end;

  if AMessage is TJRPCError then
  begin
    var LErr := AMessage as TJRPCError;

    // If the error is in the JRPC request messages then process internally the error.
    if LErr.Request then
    begin
      if Assigned(FSession) then
      begin
        Logger.LogDebug('Enqueing error [%s]', [LErr.Error.Message.Value]);
        FSession.Inbound.Errors.Enqueue(LErr.Clone);
      end;
    end
    else
    begin
      // If the error was generated processing the request, clone the error object
      Logger.LogDebug('Error detected [%s]', [LErr.Error.Message.Value]);
      AResponseList.AddMessage(LErr.Clone);
    end;

    Exit;
  end;

  var LRequest := AMessage as TJRPCRequest;
  try
    Logger.LogDebug('Processing request [%s: %s]', [LRequest.Id.AsString, LRequest.Method]);

    FContext.AddContent(LRequest);

    if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LRequest.Method]);

    LInstance := LConstructorProxy.ConstructorFunc();
    FGarbage.Add(LInstance);

    // Injects the context inside the instance
    FContext.Inject(LInstance);

    LInvokerCtx.Garbage := FGarbage;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := AResponseList;
    LInvokerCtx.ApiInstance := LInstance;
    LInvokerCtx.SelectConfig(LConstructorProxy.NeonConfig, FContext.FindContextDataAs<TJRPCNeonConfig>);

    TJRPCInvoker.Invoke(LInvokerCtx);

    if (LRequest.Method = 'initialize') and Assigned(FSession) then
    begin
      if FSessionConfig.GetLocation = TSessionIdLocation.Header then
        FResponse.Headers.AddOrSetValue(FSessionConfig.GetHeaderName, FSession.SessionId)
      else if FSessionConfig.GetLocation = TSessionIdLocation.Cookie then
        FResponse.SetCookie(FSessionConfig.GetHeaderName, FSession.SessionId);
    end;

  except
    on E: Exception do
    begin
      var err := TJRPCInvoker.HandleError(E, LRequest.Id);
      FContext.Responses.AddMessage(err);
    end;
  end;

end;

procedure TMCPTransportHandler.HandleOPTIONS;
begin
  FResponse.Code := HTTP_CODE_NOCONTENT;
  FResponse.Content := '';
end;

procedure TMCPTransportHandler.HandlePOST;
var
  LMessage: TJRPCMessage;
  LRequestList, LResponseList: TJRPCMessages;
begin
  LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  FGarbage.Add(LRequestList);

  LResponseList := TJRPCMessages.Create(True);
  FGarbage.Add(LResponseList);

  for LMessage in LRequestList.List do
    HandleMessage(LMessage, LResponseList);

  { TODO -opaolo -c : To change based on SSE vs JSON reqs 28/03/2026 17:45:47 }
  FResponse.ContentType := 'application/json';
  FResponse.Content := LResponseList.ToJson;

  if LResponseList.Count = 0 then
    FResponse.Code := HTTP_CODE_ACCEPTED
  else
    FResponse.Code := HTTP_CODE_OK;
end;

function TMCPTransportHandler.HandleSession: TMCPSessionBase;
var
  LSessionId: string;
begin
  Result := nil;

  if not Assigned(FSessionConfig) or (not FSessionConfig.IsApplied) then
    Exit;

  LSessionId := ExtractSessionId;

  // If session ID is provided, try to get existing session
  if not LSessionId.IsEmpty then
  begin
    // GetSession will raise exception if expired or not found
    Result := TMCPSessionManager.Instance.GetSession(LSessionId);
  end
  else
  begin
    // No session ID provided - auto-create new session
    Result := TMCPSessionManager.Instance.CreateSession;
  end;
end;

procedure TMCPTransportHandler.InjectCORS;
var
  LHValue: string;
begin
  if not FMCPConfig.Security.CORS then
    Exit;

  // Set the allowed origins (from security configuration)
  LHValue := FRequest.GetHeader('Origin');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Origin', LHValue);

  // Set the allowed methods supported by the server (from security configuration)
  LHValue := FRequest.GetHeader('Access-Control-Request-Method');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Methods', string.Join(',', FMCPConfig.Security.AllowedMethods));

  // Set the allowed headers as requested
  LHValue := FRequest.GetHeader('Access-Control-Request-Headers');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Headers', LHValue);

end;

{ TMCPTransportResponse }

function TMCPTransportResponse.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

procedure TMCPTransportResponse.SetContentType(const AValue: string);
begin
  Headers.AddOrSetValue('Content-Type', AValue);
end;

procedure TMCPTransportResponse.SetCookie(const AName, AValue: string);
begin
  { TODO -opaolo -c : Finire 29/04/2026 23:34:50 }
end;

procedure TMCPTransportResponse.SSEContent(AWriterProc: TMCPSSEResponseWriterProc);
begin
  WriterProc := AWriterProc;
end;

{ TMCPTransportRequest }

function TMCPTransportRequest.GetCookie(const AName: string): string;
begin
  var LCookies := GetHeader('Cookie');
  if LCookies.IsEmpty then
    Exit('');

  var LCookieList := TStringList.Create;
  try
    LCookieList.NameValueSeparator := '=';
    LCookieList.LineBreak := ';';
    LCookieList.Text := LCookies;

    Result := LCookieList.Values[AName];
  finally
    LCookieList.Free;
  end;

end;

{ TMCPSSEResponseWriter }

function TMCPSSEResponseWriter.Connected: Boolean;
begin
  Result := InternalConnected;
end;

function TMCPSSEResponseWriter.SplitString(
  const AValue: string): TArray<string>;
var
  LLines: TArray<string>;
begin
  LLines := AValue.Split([sLineBreak]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#13]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#10]);
  if Length(LLines) = 0 then
    LLines := [AValue];

  Result := LLines;
end;

procedure TMCPSSEResponseWriter.Write(const AEvent, AValue: string;
  ARetry: Integer);
begin
  Write('', AEvent, AValue, ARetry);
end;

procedure TMCPSSEResponseWriter.Write(const AEvent, AValue: string);
begin
  Write(AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.Write(const AValue: string);
begin
  Write('', AValue);
end;

procedure TMCPSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string);
begin
  Write(AId.ToString, AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string; ARetry: Integer);
begin
  Write(AId.ToString, AEvent, AValue, ARetry);
end;

procedure TMCPSSEResponseWriter.Write(const AId, AEvent, AValue: string;
  ARetry: Integer);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';
  if AId <> '' then
    LMessage := LMessage + 'id: ' + AId + #13#10;
  if AEvent <> '' then
    LMessage := LMessage + 'event: ' + AEvent + #13#10;
  if ARetry > 0 then
    LMessage := LMessage + 'retry: ' + IntToStr(ARetry) + #13#10;

  for LLine in LLines do
    LMessage := LMessage + 'data: ' + LLine + #13#10;

  InternalWriteLine(LMessage);
end;

procedure TMCPSSEResponseWriter.Write(const AId, AEvent, AValue: string);
begin
  Write(AId, AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.WriteComment(const AValue: string);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';

  for LLine in LLines do
    LMessage := LMessage + ': ' + LLine + #13#10;

  InternalWriteLine(LMessage);
end;

constructor TMCPTransportHeaders.Create;
begin
  Headers := TDictionary<string, string>.Create;
end;

destructor TMCPTransportHeaders.Destroy;
begin
  Headers.Free;
  inherited;
end;

function TMCPTransportHeaders.GetHeader(const AName: string): string;
begin
  if not Headers.TryGetValue(AName, Result) then
    Result := '';
end;

end.
