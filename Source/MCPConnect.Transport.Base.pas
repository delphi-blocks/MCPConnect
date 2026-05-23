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
  System.Generics.Defaults,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Transport.AcceptParser,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.MCP.Types,
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
  /// <summary>
  ///   Exception class for all transport related errors
  /// </summary>
  EMCPTransportException = class(EMCPException)
  private
    FCode: Integer;
  public
    constructor Create(ACode: Integer; const AMsg: string);
    function ToJSON: string;
    property Code: Integer read FCode write FCode;
  end;

  IMCPTransportWriter = interface
    ['{68598454-50C5-4892-B8E0-81687CC2F4DE}']
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  TMCPTransportHeaders = class
  private type
    THeaders = class(TDictionary<string, string>)
    end;
  public
    Headers: THeaders;

    constructor Create;
    destructor Destroy; override;

    function GetHeader(const AName: string): string;
    procedure AddOrSetHeader(const AName, AValue: string);
  end;

  TMCPTransportRequest = class(TMCPTransportHeaders)
  private
    FAcceptItems: TAcceptItemList<TAcceptItem>;
    function GetAccept: string;
    procedure SetAccept(const AValue: string);
    function GetAcceptItems: TAcceptItemList<TAcceptItem>;
    function GetAcceptsEventStream: Boolean;
  public
    Url: string;
    Command: string;
    Content: string;

    function GetCookie(const AName: string): string;
    property Accept: string read GetAccept write SetAccept;
    property AcceptItems: TAcceptItemList<TAcceptItem> read GetAcceptItems;
    property AcceptsEventStream: Boolean read GetAcceptsEventStream;

    constructor Create;
    destructor Destroy; override;
  end;

  TMCPTransportRequestConverter = reference to procedure (ARequest: TMCPTransportRequest);

  TMCPTransportResponse = class(TMCPTransportHeaders)
  private
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
  public
    Content: string;
    Code: Integer;
    Outbund: TQueue<string>;

    procedure SetCookie(const AName, AValue: string);
    property ContentType: string read GetContentType write SetContentType;
  end;

  TMCPTransportResponseConverter = reference to procedure (AResponse: TMCPTransportResponse);

  IMCPTransportHandler = interface
  ['{B2966C2A-7594-4B30-95D9-D702AE20633E}']
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
    function GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    procedure SetSendResponseHeadersProc(const Value: TProc<TMCPTransportResponse>);
    property SendResponseHeadersProc: TProc<TMCPTransportResponse> read GetSendResponseHeadersProc write SetSendResponseHeadersProc;
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
    FResponseWriter: IMCPTransportWriter;
    FSendResponseHeadersProc: TProc<TMCPTransportResponse>;
  private
    procedure InjectCORS;
    function CheckOrigin: Boolean;
    function CheckAuthorization: Boolean;
    function ExtractSessionId: string;
    function HandleSession: TMCPSessionBase;
    procedure HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
    procedure SendResponseHeaders(AResponse: TMCPTransportResponse);

    procedure HandleGET;
    procedure HandlePOST;
    procedure HandleOPTIONS;
    function CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
  public
    constructor Create(AServer: TJRPCServer; AResponseWriter: IMCPTransportWriter);
    destructor Destroy; override;

    { IMCPHttpHandler }
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
    function GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    procedure SetSendResponseHeadersProc(const Value: TProc<TMCPTransportResponse>);
  end;

implementation

uses
  System.IOUtils, System.JSON, Logify,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Invoker;


{ TMCPTransportHandler }

constructor TMCPTransportHandler.Create(AServer: TJRPCServer; AResponseWriter: IMCPTransportWriter);
begin
  FRequest := TMCPTransportRequest.Create;
  FResponse := TMCPTransportResponse.Create;

  FServer := AServer;
  FResponseWriter := AResponseWriter;
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
    raise EMCPException.Create('Error retrieving MCP configuration');

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

procedure TMCPTransportHandler.ProcessRequest(
  ARequestConverter: TMCPTransportRequestConverter;
  AResponseConverter: TMCPTransportResponseConverter);
begin
  ARequestConverter(FRequest);

  try try
    if not CheckOrigin then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, 'Cross-Origin Request Blocked: Same Origin Policy');

    if not CheckAuthorization then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, 'Authorization check failed');

    FGarbage := TGarbageCollector.CreateInstance;
    FContext := TJRPCContext.Create;

    FGarbage.Add(FContext);
    FContext.AddContent(FGarbage);
    FContext.AddContent(FServer);

    // Handle session (get existing or create new)
    FSession := HandleSession;

    if Assigned(FSession) then
    begin
      // Add session to context if available
      FContext.AddContent(FSession);
      // Add session header
      case FSessionConfig.GetLocation of
        TSessionIdLocation.Header:
          FResponse.AddOrSetHeader(FSessionConfig.GetHeaderName, FSession.SessionId);
        TSessionIdLocation.Cookie:
          FResponse.SetCookie(FSessionConfig.GetHeaderName, FSession.SessionId);
        else
          raise EMCPTransportException.Create(500, 'SessionId Location not found');
      end;

    end;

    InjectCORS;

    if FRequest.Command = 'GET' then
      HandleGET
    else if FRequest.Command = 'POST' then
      HandlePOST
    else if FRequest.Command = 'OPTIONS' then
      HandleOPTIONS
    else
      raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, 'Http method not allowed');

  except
    on E: EMCPTransportException do
    begin
      FResponse.Code := E.Code;
      FResponse.ContentType := 'application/json';
      FResponse.Content := E.ToJSON;
    end;

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
  finally
    AResponseConverter(FResponse);
  end;
end;

procedure TMCPTransportHandler.SendResponseHeaders(AResponse: TMCPTransportResponse);
begin
  if Assigned(FSendResponseHeadersProc) then
    FSendResponseHeadersProc(AResponse);
end;

procedure TMCPTransportHandler.SetSendResponseHeadersProc(
  const Value: TProc<TMCPTransportResponse>);
begin
  FSendResponseHeadersProc := Value;
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

function TMCPTransportHandler.GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
begin
  Result := FSendResponseHeadersProc;
end;

function TMCPTransportHandler.CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
begin
  var LAsyncExecute := TThread.CreateAnonymousThread(
    procedure
    begin
      for var LMessage in ARequestList.List do
        HandleMessage(LMessage, AResponseQueue);
    end
  );
  LAsyncExecute.FreeOnTerminate := False;
  LAsyncExecute.Start;
  Result := LAsyncExecute;
end;

procedure TMCPTransportHandler.HandleGET;
const
  QueueReadTimeout = 500;

  procedure ProcessQueue(AQueue: TMCPMessageQueue);
  begin
    AQueue.Process(
      procedure (AMessage: TJRPCMessage; var ADispose: Boolean)
      begin
        FResponseWriter.Write(AMessage.ToJson);
      end,
      QueueReadTimeout
    );
  end;


begin
  if not FResponseWriter.SupportsStreaming then
    raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, 'SSE not supported');

  if not FRequest.AcceptsEventStream then
    raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, 'Only Event Stream response is supported for GET requests');

  // TODO: handle global messages
  if not Assigned(FSession) then
    raise EMCPTransportException.Create(HTTP_CODE_NOTACCEPTABLE, 'Session not found');

  FResponse.Code := HTTP_CODE_OK;
  FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;
  SendResponseHeaders(FResponse);
  while FResponseWriter.Connected do
  begin
    ProcessQueue(FSession.Outbound);
  end;
end;

procedure TMCPTransportHandler.HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
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
      FSession.Inbound.Enqueue(LNotification.Clone);
    end;
    Exit;
  end;

  if AMessage is TJRPCResponse then
  begin
    if Assigned(FSession) then
    begin
      var LRes := AMessage as TJRPCResponse;
      Logger.LogDebug('Enqueing response id [%s]', [LRes.Id.AsString]);
      FSession.Inbound.Enqueue(LRes.Clone);
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
        FSession.Inbound.Enqueue(LErr.Clone);
      end;
    end
    else
    begin
      // If the error was generated processing the request, clone the error object
      Logger.LogDebug('Error detected [%s]', [LErr.Error.Message.Value]);
      AResponseQueue.Enqueue(LErr.Clone);
    end;

    Exit;
  end;

  var LRequest := AMessage as TJRPCRequest;
  try
    Logger.LogDebug('Processing request [%s: %s]', [LRequest.Id.AsString, LRequest.Method]);

    FContext.AddContent(LRequest);

    var LMCPConfig := FContext.FindContextDataAs(IMCPConfig) as IMCPConfig;
    if Assigned(LMCPConfig) then
    begin
      if not LMCPConfig.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
        raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LRequest.Method]);
    end
    else if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LRequest.Method]);

    LInstance := LConstructorProxy.ConstructorFunc();
    FGarbage.Add(LInstance);

    // Injects the context inside the instance
    FContext.Inject(LInstance);

    LInvokerCtx.Garbage := FGarbage;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := AResponseQueue;
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
const
  QueueReadTimeout = 500;
var
  LResponseList: TJRPCMessages;

  procedure ProcessQueue(AResponseList: TMCPMessageQueue);
  begin
    AResponseList.Process(
      procedure (AMessage: TJRPCMessage; var ADispose: Boolean)
      begin
        if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
          FResponseWriter.Write(AMessage.ToJson)
        else
        begin
          ADispose := False;
          if AMessage is TJRPCNotification then
          begin
            // TODO: should I add the message to FSession.Outbound also if SSE is not supported?
            if Assigned(FSession) then
              FSession.Outbound.Enqueue(AMessage)
            else
              ADispose := True;
          end
          else
          begin
            LResponseList.AddMessage(AMessage);
          end;
        end;
      end,
      QueueReadTimeout
    );
  end;

begin
  var LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  FGarbage.Add(LRequestList);

  var LResponseQueue := TMCPMessageQueue.Create;
  FGarbage.Add(LResponseQueue);
  FContext.AddContent(LResponseQueue);

  // This list contains the responses in case SSE channel is not active
  LResponseList := TJRPCMessages.Create(True);
  FGarbage.Add(LResponseList);

  var LAsyncExecute := CreateAsyncThread(LRequestList, LResponseQueue);
  try
    if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
    begin
      FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;
      SendResponseHeaders(FResponse);
    end;

    while not LAsyncExecute.Finished do
    begin
      ProcessQueue(LResponseQueue);
    end;
    ProcessQueue(LResponseQueue);

    // If not an event stream response send all the headers and content
    if not FRequest.AcceptsEventStream or not FResponseWriter.SupportsStreaming then
    begin
      if LResponseList.Count = 0 then
        FResponse.Code := HTTP_CODE_ACCEPTED
      else
        FResponse.Code := HTTP_CODE_OK;
      FResponse.ContentType := TMediaType.APPLICATION_JSON;
      FResponse.Content := LResponseList.ToJson;
    end;
  finally
    LAsyncExecute.Free;
  end;

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
    Result := TMCPSessionManager.Instance.CreateSession(FServer);
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

{ TMCPTransportRequest }

constructor TMCPTransportRequest.Create;
begin
  inherited Create;
  FAcceptItems := nil;
end;

destructor TMCPTransportRequest.Destroy;
begin
  FAcceptItems.Free;
  inherited;
end;

function TMCPTransportRequest.GetAccept: string;
begin
  Result := GetHeader('Accept');
end;

function TMCPTransportRequest.GetAcceptItems: TAcceptItemList<TAcceptItem>;
begin
  if not Assigned(FAcceptItems) then
  begin
    FAcceptItems := TAcceptItemList<TAcceptItem>.Create;
    var LAcceptHeader := GetHeader('Accept');
    TAcceptHeaderParser<TAcceptItem>.Parse(LAcceptHeader, FAcceptItems);
  end;
  Result := FAcceptItems;
end;

function TMCPTransportRequest.GetAcceptsEventStream: Boolean;
begin
  Result := AcceptItems.Contains(TMediaType.TEXT_EVENT_STREAM);
end;

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

procedure TMCPTransportRequest.SetAccept(const AValue: string);
begin
  FreeAndNil(FAcceptItems);
  Headers.AddOrSetValue('Accept', AValue);
end;

procedure TMCPTransportHeaders.AddOrSetHeader(const AName, AValue: string);
begin
  Headers.AddOrSetValue(AName, AValue);
end;

constructor TMCPTransportHeaders.Create;
begin
  // Case-insensitive <string,string> dictionary
  Headers := THeaders.Create(TIStringComparer.Ordinal);
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

{ EMCPTransportException }

constructor EMCPTransportException.Create(ACode: Integer; const AMsg: string);
begin
  inherited Create(AMsg);
  FCode := ACode;
end;

function EMCPTransportException.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('code', Self.Code);
    LJSON.AddPair('class', Self.ClassName);
    LJSON.AddPair('message', Self.Message);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

end.
