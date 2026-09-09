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

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON, System.SyncObjs,
  System.IOUtils, System.Net.HttpClient, System.Diagnostics, System.DateUtils,
  System.Generics.Collections, System.Generics.Defaults,
  IdCustomHTTPServer, IdContext, IdGlobal,

  JRPC.Core,
  JRPC.Classes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Transport.AcceptParser,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Server;

const
  HTTP_CODE_OK = 200;
  HTTP_CODE_ACCEPTED = 202;
  HTTP_CODE_NOCONTENT = 204;
  HTTP_CODE_BADREQUEST = 400;
  HTTP_CODE_UNAUTHORIZED = 401;
  HTTP_CODE_FORBIDDEN = 403;
  HTTP_CODE_NOTFOUND = 404;
  HTTP_CODE_NOTALLOWED = 405;
  HTTP_CODE_NOTACCEPTABLE = 406;
  HTTP_CODE_BADGATEWAY = 502;

  /// <summary>Scheme prefix of the "Authorization" header carrying an OAuth access token.</summary>
  BearerPrefix = 'Bearer ';

resourcestring
  SHttpMethodNotAllowed = 'Http method not allowed';
  STransportMethodNotFoundFmt = 'Method "%s" not found';
  SDuplicateAuthorizationHeader = 'Multiple Authorization headers are not allowed';

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
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  TTransportProtocol = (Undefined, Stdio, StreamableHTTP);

  TMCPTransportHeaders = class
  private
  type
    THeaders = class(TList<TPair<string, string>>)
    end;
  private
    FHeaders: THeaders;
  public

    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator<TPair<string, string>>; inline;

    procedure Clear;
    // Returns the index of the first header matching AName (case-insensitive), or -1
    function IndexOf(const AName: string): Integer;
    // Returns the value of the first matching header, or '' if not found
    function Get(const AName: string): string; virtual;
    // Returns all values for headers matching AName (case-insensitive)
    function GetHeaders(const AName: string): TArray<string>;
    // Replaces the first matching header or adds a new one (single-value semantics)
    procedure &Set(const AName, AValue: string);
    // Appends a header without removing duplicates (use for Set-Cookie, etc.)
    procedure Add(const AName, AValue: string); virtual;
    // Removes all headers matching AName (case-insensitive); returns the number removed
    function RemoveHeader(const AName: string): Integer;

    function Count: Integer; inline;
  end;

  TMCPRequestHeaders = class(TMCPTransportHeaders)
  public
    // RFC 6750 §3.1: reject requests with multiple Authorization headers
    procedure Add(const AName, AValue: string); override;
  end;

  TMCPTransportRequest = class(TObject)
  private
    FAcceptItems: TAcceptItemList<TAcceptItem>;
    FProtocol: TTransportProtocol;
    FHeaders: TMCPRequestHeaders;
    function GetAccept: string;
    procedure SetAccept(const AValue: string);
    function GetAcceptItems: TAcceptItemList<TAcceptItem>;
    function GetAcceptsEventStream: Boolean;
    function GetAuthorization: string;
    function GetOrigin: string;
  public
    Url: string;
    Command: string;
    Content: string;
    ContentJSON: TJSONValue;

    function GetHeader(const AName: string): string;
    procedure SetHeader(const AName, AValue: string);

    function GetCookie(const AName: string): string;
    property Accept: string read GetAccept write SetAccept;
    property AcceptItems: TAcceptItemList<TAcceptItem> read GetAcceptItems;
    property AcceptsEventStream: Boolean read GetAcceptsEventStream;
    property Authorization: string read GetAuthorization;
    property Origin: string read GetOrigin;
    property Protocol: TTransportProtocol read FProtocol write FProtocol;
    property Headers: TMCPRequestHeaders read FHeaders;

    constructor Create;
    destructor Destroy; override;
  end;

  TMCPTransportRequestConverter = reference to procedure (ARequest: TMCPTransportRequest);

  TMCPResponseHeaders = class(TMCPTransportHeaders)
  end;

  TMCPTransportResponse = class(TObject)
  private
    FHeaders: TMCPResponseHeaders;
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
    function GetTransferEncoding: string;
    procedure SetTransferEncoding(const AValue: string);
  public
    Content: string;
    Code: Integer;
    Outbund: TQueue<string>;

    procedure SetCookie(const AName, AValue: string; ASecure: Boolean = True);
    procedure ClearCookies();

    function GetHeader(const AName: string): string;
    procedure SetHeader(const AName, AValue: string);

    property Headers: TMCPResponseHeaders read FHeaders;
    property ContentType: string read GetContentType write SetContentType;
    property TransferEncoding: string read GetTransferEncoding write SetTransferEncoding;

    constructor Create;
    destructor Destroy; override;
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
    FAccessToken: TMCPAccessToken;

    /// <summary>
    ///   Held only so that the configuration is created and lands in the
    ///   request context: TCORSMiddleware and the api classes look for it there.
    /// </summary>
    FMCPConfig: TMCPConfig;
    FServer: TMCPServer;
    /// <summary>
    ///   The middleware of the message being handled. Lives only for the span
    ///   of HandleMessage, which is what keeps a middleware's fields private to
    ///   one message; the terminals read it to build the inner chains.
    /// </summary>
    FPipeline: TMiddlewarePipeline;
    /// <summary>
    ///   The queue the message being handled answers into. Like FPipeline it
    ///   lives only for the span of HandleMessage, and it is kept here rather
    ///   than on the middleware context on purpose: the queue is a pipe that
    ///   the thread writing to the client drains as it fills, so handing it to
    ///   a middleware would invite reads that steal the answer. Middleware get
    ///   Emit and Produced instead.
    /// </summary>
    FResponseQueue: TMCPMessageQueue;
    /// <summary>
    ///   Held, like FMCPConfig, only so that the configuration is created and
    ///   lands in the request context: TOAuthMiddleware looks for it there.
    /// </summary>
    FResponseWriter: IMCPTransportWriter;
    FSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    /// <summary>
    ///   The HTTP status the reply must carry because of a protocol error, or
    ///   zero when nothing asked for one.
    /// </summary>
    /// <remarks>
    ///   Two statuses come from the message level. The errors MCP 2026-07-28
    ///   defines for itself - a header that contradicts the body, a "_meta"
    ///   missing a required field, an unsupported protocol version, a
    ///   capability the client never declared - MUST be answered 400 Bad
    ///   Request; a method the server does not implement MUST be answered 404
    ///   Not Found, with the -32601 body that tells a client this is a modern
    ///   server refusing a method rather than a legacy one that does not host
    ///   the endpoint.
    ///
    ///   The JSON-RPC error code alone does not say which is which: -32602 is
    ///   a 400 when it reports a malformed "_meta" and an ordinary 200 when it
    ///   reports an unknown tool name. What separates them is the exception, so
    ///   it is recorded where the exception is seen (HandleMessage) and read
    ///   where the status is decided (HandlePOST).
    ///
    ///   Written on the worker thread and read after the response queue closes,
    ///   which is the happens-before that makes a plain field enough.
    ///
    ///   It cannot help a request answered with a stream: an SSE reply sends
    ///   its headers before the handler runs, so by the time the error exists
    ///   the status is already on the wire. That is inherent to streaming, not
    ///   to this.
    /// </remarks>
    FProtocolStatus: Integer;
  private
    function SelectNeonConfig(const AProxy: TJRPCConstructorProxy): INeonConfiguration;
    procedure HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
    /// <summary>
    ///   Runs the transport chain over one request of the transport. The
    ///   outermost level: it wraps everything, the parsing of the payload
    ///   included, and it runs for requests that carry no message at all - a
    ///   CORS preflight, a metadata request.
    /// </summary>
    procedure RunTransportChain;
    /// <summary>
    ///   Terminal of the transport chain: authorization, then the verb.
    /// </summary>
    procedure DispatchTransport(AContext: TMiddlewareContext);
    /// <summary>
    ///   Terminal of the message chain: sorts the message out by kind, and for
    ///   a request runs the request chain on top of DispatchRequest.
    /// </summary>
    procedure DispatchMessage(AContext: TMiddlewareContext);
    /// <summary>
    ///   Terminal of the request chain: the real work, resolving the method and
    ///   invoking it.
    /// </summary>
    procedure DispatchRequest(AContext: TMiddlewareContext);
    class function MessageKindOf(AMessage: TJRPCMessage): TMiddlewareMessageKind; static;
    class function MethodNameOf(AMessage: TJRPCMessage): string; static;
    procedure SendResponseHeaders(AResponse: TMCPTransportResponse);
    procedure WriteSSEResponse(const AValue: string; const AEventId: string = '');

    /// <summary>
    ///   Whether AMessage is a progress notification for a request that never
    ///   asked for one.
    /// </summary>
    /// <remarks>
    ///   A progress notification may reference only a token an active request
    ///   provided, so one sent for a request that provided none references
    ///   something a tool made up. True only once the request "_meta" has
    ///   actually been read: with that check turned off nobody knows what the
    ///   client asked for, and unverifiable is not the same as wrong.
    ///
    ///   A method rather than a local of HandlePOST, because the queue is
    ///   drained through an anonymous method and one of those cannot capture a
    ///   nested procedure.
    /// </remarks>
    function IsUnsolicitedProgress(AMessage: TJRPCMessage): Boolean;

    procedure HandlePOST;
    procedure HandleOPTIONS;
    function CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
  public
    constructor Create(AServer: TMCPServer; AResponseWriter: IMCPTransportWriter);
    destructor Destroy; override;

    { IMCPHttpHandler }
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
    function GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    procedure SetSendResponseHeadersProc(const Value: TProc<TMCPTransportResponse>);
  end;

implementation

uses
  Logify,
  JRPC.Invoker,
  Neon.Core.Utils,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.Neon;

{ TMCPTransportHandler }

constructor TMCPTransportHandler.Create(AServer: TMCPServer; AResponseWriter: IMCPTransportWriter);
begin
  FRequest := TMCPTransportRequest.Create;
  FResponse := TMCPTransportResponse.Create;
  FAccessToken := TMCPAccessToken.Create;

  FServer := AServer;
  FResponseWriter := AResponseWriter;
  FMCPConfig := FServer.GetConfiguration<TMCPConfig>;
end;

destructor TMCPTransportHandler.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FAccessToken.Free;

  Logger.LogDebug('MCPTransportHandler destroyed');
  inherited;
end;

procedure TMCPTransportHandler.WriteSSEResponse(const AValue, AEventId: string);
begin
  if Assigned(FResponseWriter) then
  begin
    Logger.LogDebug('[SSE] Event Sent [id=%s, size=%d]', [AEventId, Length(AValue)]);
    {$IFDEF FULL_PAYLOAD_LOGGING}
    Logger.LogTrace('[SSE] data: %s', [AValue]);
    {$ENDIF}
    FResponseWriter.Write(AValue, AEventId);
  end;
end;

procedure TMCPTransportHandler.RunTransportChain;
var
  LContext: TMiddlewareContext;
begin
  // A context of kind Transport: there is no message here yet - the payload is
  // parsed by HandlePOST, further down - so no message to carry and no queue to
  // Emit into. What a middleware of this level works on is the request and the
  // response of the transport, both of them in the request context.
  LContext := TMiddlewareContext.Create('', TMiddlewareMessageKind.Transport,
    nil, FContext, FGarbage, nil, False);
  try
    TMiddlewareChain.Run<ITransportMiddleware>(FPipeline, LContext, DispatchTransport);
  finally
    LContext.Free;
  end;
end;

procedure TMCPTransportHandler.DispatchTransport(AContext: TMiddlewareContext);
var
  LFragment: TStopwatch;
begin
  LFragment := TStopwatch.StartNew;
  // GET is deliberately absent: it existed only to open the server-to-client
  // SSE stream, which went with session management. It now falls through to
  // the 405 below like any other verb the server does not implement.
  if FRequest.Command = 'POST' then
    HandlePOST
  else if FRequest.Command = 'OPTIONS' then
    HandleOPTIONS
  else
    raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, SHttpMethodNotAllowed);
  Logger.LogDebug('[PERF] Transport [%s] Dispatch: %d ms', [FRequest.Command, LFragment.ElapsedMilliseconds]);
end;

procedure TMCPTransportHandler.ProcessRequest(
  ARequestConverter: TMCPTransportRequestConverter;
  AResponseConverter: TMCPTransportResponseConverter);
var
  LStopwatch, LFragment: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LFragment := TStopwatch.StartNew;
    ARequestConverter(FRequest);
    {$IFDEF FULL_PAYLOAD_LOGGING}
    Logger.LogTrace('[REQ] %s', [FRequest.Content]);
    {$ENDIF}
    Logger.LogDebug('[PERF] Transport RequestConverter: %d ms', [LFragment.ElapsedMilliseconds]);

    try
      // Built first thing, before any check, because the transport chain runs
      // on it: a middleware of that level finds the request, the response, the
      // server and its configurations there, and so does the token validator,
      // which is why this cannot wait for the OAuth check either. A request
      // that ends in a 401, or refused by a middleware, pays for a context it
      // will not use, which is a cheaper price than a half-built one.
      FGarbage := TGarbageCollector.CreateInstance;
      FContext := TJRPCContext.Create;

      FGarbage.Add(FContext);
      FContext.AddContent(FGarbage);

      FContext.AddContent(FRequest);
      FContext.AddContent(FResponse);

      // The server *and* each of its configurations: see AddApplicationToContext
      // for why adding the server alone is not enough.
      AddApplicationToContext(FContext, FServer);

      FContext.AddContent(FAccessToken);

      // The shared middleware objects have to be put in by hand: AddContent
      // picks up the configurations of an IJRPCApplication on its own, and the
      // middleware list deliberately is not one of them.
      for var LShared in FServer.Middleware.SharedObjects do
        FContext.AddContent(LShared);

      // Built here, and not per message, so that it can live in the request
      // context: it is where the api classes find it to run the hooks of their
      // own operation. What is per message is BeginMessage/EndMessage.
      FPipeline := TMiddlewarePipeline.Create(FServer.Middleware, FContext);
      FGarbage.Add(FPipeline);
      FContext.AddContent(FPipeline);

      RunTransportChain;
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
    try
      AResponseConverter(FResponse);
    finally
      Logger.LogDebug('[PERF] Transport [%s %s] total: %d ms (HTTP: %d)', [FRequest.Command, FRequest.Url, LStopwatch.ElapsedMilliseconds, FResponse.Code]);
      {$IFDEF FULL_PAYLOAD_LOGGING}
      Logger.LogTrace('[RES] %s', [FResponse.Content]);
      {$ENDIF}
    end;
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

function TMCPTransportHandler.GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
begin
  Result := FSendResponseHeadersProc;
end;

function TMCPTransportHandler.CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
begin
  var LAsyncExecute := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        for var LMessage in ARequestList.List do
          HandleMessage(LMessage, AResponseQueue);
      finally
        AResponseQueue.Close;
      end;
    end
  );
  LAsyncExecute.FreeOnTerminate := False;
  LAsyncExecute.Start;
  Result := LAsyncExecute;
end;

function TMCPTransportHandler.SelectNeonConfig(const AProxy: TJRPCConstructorProxy): INeonConfiguration;
var
  LJRPCNeonConfig: TJRPCNeonConfig;
begin
  // Precedence: the configuration the API class was registered with - for every
  // built-in MCP namespace that is MCPNeonConfig, whose camelCase naming and
  // SetMembers([Fields]) are what make an MCP entity serialize to anything at
  // all - then the application-wide IJRPCNeonConfig, then Neon's default, which
  // TJRPCInvokerContext.SelectConfig applies when this returns nil.
  //
  // The middle tier is why this lives here rather than being passed straight in:
  // the JRPC library knows nothing of the plugin configuration system, so its
  // SelectConfig takes the API-level configuration alone.
  Result := AProxy.NeonConfig;
  if Assigned(Result) then
    Exit;

  LJRPCNeonConfig := FContext.FindContextDataAs<TJRPCNeonConfig>;
  if Assigned(LJRPCNeonConfig) then
    Result := LJRPCNeonConfig.NeonConfig;
end;

class function TMCPTransportHandler.MessageKindOf(AMessage: TJRPCMessage): TMiddlewareMessageKind;
begin
  if AMessage is TJRPCNotification then
    Exit(TMiddlewareMessageKind.Notification);
  if AMessage is TJRPCResponse then
    Exit(TMiddlewareMessageKind.Response);
  if AMessage is TJRPCError then
    Exit(TMiddlewareMessageKind.Error);

  Result := TMiddlewareMessageKind.Request;
end;

class function TMCPTransportHandler.MethodNameOf(AMessage: TJRPCMessage): string;
begin
  if AMessage is TJRPCMethod then
    Exit(TJRPCMethod(AMessage).Method);

  Result := '';
end;

procedure TMCPTransportHandler.HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
var
  LContext: TMiddlewareContext;
begin
  LContext := TMiddlewareContext.Create(MethodNameOf(AMessage),
    MessageKindOf(AMessage), AMessage, FContext, FGarbage, AResponseQueue,
    not FPipeline.IsEmpty);
  try
    FResponseQueue := AResponseQueue;
    FPipeline.BeginMessage(LContext);
    try
      // The whole chain sits inside the try, not just the handler, so that a
      // middleware raising to refuse a call lands here the same way the api
      // method does, and an error handling middleware upstream sees what the
      // ones below it raised.
      try
        TMiddlewareChain.Run<IMessageMiddleware>(FPipeline, LContext, DispatchMessage);
      except
        on E: Exception do
        begin
          Logger.LogError(E, Format('TMCPTransportHandler.HandleMessage %s: %s', [E.ClassName, E.Message]));

          // The status this error deserves, recorded in the one place where it
          // is still an exception: the JSON-RPC code alone cannot say, since
          // -32602 is a 400 when it reports a malformed "_meta" and an ordinary
          // 200 when it reports an unknown tool name. First one wins - a batch
          // that produced two is answered 200 anyway, see HandlePOST.
          if FProtocolStatus = 0 then
          begin
            // The whole family in one test, which is what the common ancestor
            // is for: every error the revision defines for itself is a 400.
            if E is EMCPProtocolError then
              FProtocolStatus := HTTP_CODE_BADREQUEST

            // A method this server does not implement is a 404 carrying a
            // -32601 body. Both halves matter: the status is what a dual-era
            // client falls back on, and the body is what tells it this is a
            // modern server answering an unknown method rather than a legacy
            // HTTP+SSE one that does not host the endpoint at all.
            else if E is EJRPCMethodNotFoundError then
              FProtocolStatus := HTTP_CODE_NOTFOUND;
          end;

          // Only a request has somewhere to put an error: anything else keeps
          // the behaviour it had before, which is to let the caller see it.
          if not (AMessage is TJRPCRequest) then
            raise;

          AResponseQueue.Enqueue(TJRPCInvoker.HandleError(E, TJRPCRequest(AMessage).Id));
        end;
      end;
    finally
      FPipeline.EndMessage;
      FResponseQueue := nil;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TMCPTransportHandler.DispatchMessage(AContext: TMiddlewareContext);
begin
  // Every message that is not a Request is dealt with here and here only: the
  // cast in DispatchRequest is unguarded, so anything reaching it that is not a
  // TJRPCRequest raises EInvalidCast, and the client is told 500.

  // A notification is fire-and-forget, and the specification forbids answering
  // one at all. There is no longer an inbound session queue to route it to, so
  // it is accepted and dropped.
  if AContext.Message is TJRPCNotification then
  begin
    Logger.LogDebug('Discarding notification [%s]', [TJRPCNotification(AContext.Message).Method]);
    Exit;
  end;

  // A Response is an answer to a request this server sent. Correlating one
  // needed the session that carried the original request, so there is nothing
  // left to match it against.
  if AContext.Message is TJRPCResponse then
  begin
    Logger.LogDebug('Discarding response id [%s]', [TJRPCResponse(AContext.Message).Id.AsString]);
    Exit;
  end;

  if AContext.Message is TJRPCError then
  begin
    var LErr := AContext.Message as TJRPCError;

    // Request=True marks a message the server must not answer: an Error object
    // the client itself sent, or a notification that failed to parse. Anything
    // else is an error the parser produced for a message that IS waiting for a
    // reply - a malformed element of a batch - and it has to reach the client.
    if LErr.Request then
      Logger.LogDebug('Discarding error [%s]', [LErr.Error.Message.Value])
    else
    begin
      Logger.LogDebug('Error detected [%s]', [LErr.Error.Message.Value]);
      AContext.Emit(LErr.Clone);
    end;

    Exit;
  end;

  TMiddlewareChain.Run<IRequestMiddleware>(FPipeline, AContext, DispatchRequest);
end;

procedure TMCPTransportHandler.DispatchRequest(AContext: TMiddlewareContext);
var
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokerCtx: TJRPCInvokerContext;
begin
  var LRequest := AContext.Message as TJRPCRequest;

  Logger.LogDebug('Processing request [%s: %s]', [LRequest.Id.AsString, LRequest.Method]);

  FContext.AddContent(LRequest);

  var LMCPConfig := FContext.FindContextDataAs(IMCPConfig) as IMCPConfig;
  if Assigned(LMCPConfig) then
  begin
    if not LMCPConfig.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt(STransportMethodNotFoundFmt, [LRequest.Method]);
  end
  else if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
    raise EJRPCMethodNotFoundError.CreateFmt(STransportMethodNotFoundFmt, [LRequest.Method]);

  LInstance := LConstructorProxy.ConstructorFunc();
  FGarbage.Add(LInstance);

  // Injects the context inside the instance
  FContext.Inject(LInstance);

  // The invoker appends what it produces to a TJRPCMessages, while this
  // transport hands responses to a TMCPMessageQueue that the SSE writer drains
  // while the worker thread is still running. The two are bridged here.
  //
  // The scratch list owns what the invoker builds, so an exception raised
  // before the hand-over below still frees it; from then on ownership moves to
  // the queue one message at a time, which is the only owner Process/Destroy
  // knows about.
  var LInvokerResponses := TJRPCMessages.Create(True);
  try
    LInvokerCtx.Garbage := FGarbage;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := LInvokerResponses;
    LInvokerCtx.ApiInstance := LInstance;
    LInvokerCtx.SelectConfig(SelectNeonConfig(LConstructorProxy));

    TJRPCInvoker.Invoke(LInvokerCtx);

    while LInvokerResponses.Count > 0 do
      FResponseQueue.Enqueue(LInvokerResponses.List.Extract(LInvokerResponses.List[0]));
  finally
    LInvokerResponses.Free;
  end;
end;

procedure TMCPTransportHandler.HandleOPTIONS;
begin
  FResponse.Code := HTTP_CODE_NOCONTENT;
  FResponse.Content := '';
end;

function TMCPTransportHandler.IsUnsolicitedProgress(AMessage: TJRPCMessage): Boolean;
var
  LProgress: TMCPProgress;
begin
  if not (AMessage is TJRPCNotification) or
     (TJRPCNotification(AMessage).Method <> MCP_NOTIFY_PROGRESS) then
    Exit(False);

  LProgress := FContext.FindContextDataAs<TMCPProgress>;
  Result := Assigned(LProgress) and LProgress.Known and not LProgress.Wanted;
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
        if IsUnsolicitedProgress(AMessage) then
        begin
          Logger.LogDebug('Progress notification dropped, the request asked for none');
        end
        else if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
        begin
          WriteSSEResponse(AMessage.ToJson);
        end
        else if AMessage is TJRPCNotification then
        begin
          // A server-to-client notification is not a reply to anything, so it has
          // no place in the JSON-RPC payload that answers this POST: per JSON-RPC
          // 2.0 a Request is answered with a Response, and a batch with an array
          // of Responses. SSE is the only channel this transport has for one -
          // there is no GET endpoint since sessions went away - and the client
          // did not ask for it, so this one is dropped rather than spliced into
          // the reply. Leaving ADispose True is what frees it.
          Logger.LogDebug('[SSE] Notification dropped, the client did not ask for a stream [method=%s]',
            [TJRPCNotification(AMessage).Method]);
        end
        else
        begin
          ADispose := False;
          LResponseList.AddMessage(AMessage);
        end;
      end,
      QueueReadTimeout
    );
  end;
var
  LRequestList: TJRPCMessages;
  LFragment: TStopwatch;
begin
  LFragment := TStopwatch.StartNew;
  try
    if Assigned(FRequest.ContentJSON) then
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.ContentJSON)
    else
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  except
    on E: EJRPCException do
    begin
      // Per JSON-RPC 2.0, malformed JSON (parse error), an empty batch, or a
      // top-level value that is neither a Request nor a batch must be answered
      // with a single JSON-RPC error response carrying a null id - never an
      // HTTP 500 or an empty body.
      var LErrorId: TJRPCID;
      LRequestList := TJRPCMessages.Create(True);
      LRequestList.AddMessage(TJRPCError.CreateFromException(E, LErrorId));
    end;
  end;
  Logger.LogDebug('[PERF] Transport CreateFromJSON: %d ms', [LFragment.ElapsedMilliseconds]);

  FGarbage.Add(LRequestList);

  var LResponseQueue := TMCPMessageQueue.Create;
  FGarbage.Add(LResponseQueue);
  FContext.AddContent(LResponseQueue);

  // The progress channel of this request, in the context from here on so that
  // a tool can be injected with one whatever the request turns out to carry.
  // What it carries is told to it by TMCPRequestMetaMiddleware, which is the
  // only thing that reads the token.
  var LProgress := TMCPProgress.Create(LResponseQueue);
  FGarbage.Add(LProgress);
  FContext.AddContent(LProgress);

  // This list contains the responses in case SSE channel is not active
  LResponseList := TJRPCMessages.Create(True);
  FGarbage.Add(LResponseList);

  // The reply must have the same shape as the payload: an object answers an
  // object, an array answers an array - including a batch of exactly one
  // Request, which JSON-RPC 2.0 still answers with a one-element array. Only
  // Single says which one this is, and this path builds its response list by
  // hand instead of going through TJRPCServer.ProcessMessages, which is where
  // the carry-over normally happens (see TJRPCMessages.ToJson).
  LResponseList.Single := LRequestList.Single;

  LFragment := TStopwatch.StartNew;
  var LAsyncExecute := CreateAsyncThread(LRequestList, LResponseQueue);
  try
    if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
    begin
      FResponse.Code := 200;
      FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;
      SendResponseHeaders(FResponse);
    end;

    // The worker thread closes the queue when done, which wakes ProcessQueue
    // immediately: on the happy path no read timeout is ever paid. The loop is
    // still needed because a slow tool can let the timeout expire before
    // producing anything, and the final drain catches messages enqueued between
    // the last timeout and the Closed check.
    while not LResponseQueue.Closed do
    begin
      ProcessQueue(LResponseQueue);
    end;
    ProcessQueue(LResponseQueue);

    // If not an event stream response send all the headers and content
    if not FRequest.AcceptsEventStream or not FResponseWriter.SupportsStreaming then
    begin
      if LResponseList.Count = 0 then
        FResponse.Code := HTTP_CODE_ACCEPTED
      // Only for a reply that is one message: a batch answers with an array of
      // outcomes, and a status can only describe one of them.
      else if (FProtocolStatus <> 0) and LResponseList.Single then
        FResponse.Code := FProtocolStatus
      else
        FResponse.Code := HTTP_CODE_OK;
      FResponse.ContentType := TMediaType.APPLICATION_JSON;
      FResponse.Content := LResponseList.ToJson;
    end;
  finally
    LAsyncExecute.Free;
  end;
  Logger.LogDebug('[PERF] Transport CreateAsyncQueue: %d ms', [LFragment.ElapsedMilliseconds]);

end;

{ TMCPTransportResponse }

constructor TMCPTransportResponse.Create;
begin
  FHeaders := TMCPResponseHeaders.Create;
end;

destructor TMCPTransportResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TMCPTransportResponse.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

function TMCPTransportResponse.GetHeader(const AName: string): string;
begin
  Result := FHeaders.Get(AName);
end;

procedure TMCPTransportResponse.SetContentType(const AValue: string);
begin
  SetHeader('Content-Type', AValue);
end;

function TMCPTransportResponse.GetTransferEncoding: string;
begin
  Result := GetHeader('Transfer-Encoding');
end;

procedure TMCPTransportResponse.SetTransferEncoding(const AValue: string);
begin
  SetHeader('Transfer-Encoding', AValue);
end;

procedure TMCPTransportResponse.SetCookie(const AName, AValue: string; ASecure: Boolean);
var
  LCookie: string;
begin
  // HttpOnly: not readable from JS (mitigates session-id theft via XSS)
  // SameSite=Strict: never sent on cross-site requests (mitigates CSRF)
  // Secure: HTTPS-only transmission; opt out via Security.SetCookieSecure(False) for plain-HTTP/dev setups
  LCookie := Format('%s=%s; Path=/; HttpOnly; SameSite=Strict', [AName, AValue]);
  if ASecure then
    LCookie := LCookie + '; Secure';

  FHeaders.Add('Set-Cookie', LCookie);
end;

procedure TMCPTransportResponse.SetHeader(const AName, AValue: string);
begin
  FHeaders.&Set(AName, AValue);
end;

procedure TMCPTransportResponse.ClearCookies();
begin
  FHeaders.RemoveHeader('Set-Cookie');
end;

{ TMCPTransportRequest }

constructor TMCPTransportRequest.Create;
begin
  inherited Create;
  FAcceptItems := nil;
  FHeaders := TMCPRequestHeaders.Create;
end;

destructor TMCPTransportRequest.Destroy;
begin
  FAcceptItems.Free;
  ContentJSON.Free;
  FHeaders.Free;
  inherited;
end;

function TMCPTransportRequest.GetAccept: string;
begin
  Result := GetHeader('Accept');
end;

function TMCPTransportRequest.GetAuthorization: string;
begin
  Result := GetHeader('Authorization');
end;

function TMCPTransportRequest.GetOrigin: string;
begin
  Result := GetHeader('Origin');
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

function TMCPTransportRequest.GetHeader(const AName: string): string;
begin
  Result := FHeaders.Get(AName);
end;

procedure TMCPTransportRequest.SetAccept(const AValue: string);
begin
  FreeAndNil(FAcceptItems);
  SetHeader('Accept', AValue);
end;

procedure TMCPTransportRequest.SetHeader(const AName, AValue: string);
begin
  FHeaders.&Set(AName, AValue);
end;

procedure TMCPTransportHeaders.Add(const AName, AValue: string);
begin
  FHeaders.Add(TPair<string, string>.Create(AName, AValue));
end;

{ TMCPRequestHeaders }

procedure TMCPRequestHeaders.Add(const AName, AValue: string);
begin
  if SameText(AName, 'Authorization') and (IndexOf('Authorization') >= 0) then
    raise EMCPTransportException.Create(HTTP_CODE_BADREQUEST, SDuplicateAuthorizationHeader);
  inherited Add(AName, AValue);
end;

procedure TMCPTransportHeaders.&Set(const AName, AValue: string);
begin
  RemoveHeader(AName);
  FHeaders.Add(TPair<string, string>.Create(AName, AValue));
end;

function TMCPTransportHeaders.RemoveHeader(const AName: string): Integer;
begin
  Result := 0;
  for var I := FHeaders.Count - 1 downto 0 do
  begin
    if SameText(FHeaders[I].Key, AName) then
    begin
      FHeaders.Delete(I);
      Inc(Result);
    end;
  end;
end;

procedure TMCPTransportHeaders.Clear;
begin
  FHeaders.Clear;
end;

function TMCPTransportHeaders.Count: Integer;
begin
  Result := FHeaders.Count;
end;

constructor TMCPTransportHeaders.Create;
begin
  FHeaders := THeaders.Create;
end;

destructor TMCPTransportHeaders.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TMCPTransportHeaders.Get(const AName: string): string;
begin
  Result := '';
  var I := IndexOf(AName);
  if I >= 0 then
    Result := FHeaders[I].Value;
end;

function TMCPTransportHeaders.GetEnumerator: TEnumerator<TPair<string, string>>;
begin
  Result := FHeaders.GetEnumerator;
end;

function TMCPTransportHeaders.GetHeaders(const AName: string): TArray<string>;
begin
  Result := [];
  for var I := 0 to FHeaders.Count - 1 do
  begin
    if SameText(FHeaders[I].Key, AName) then
      Result := Result + [FHeaders[I].Value];
  end;
end;

function TMCPTransportHeaders.IndexOf(const AName: string): Integer;
begin
  Result := -1;
  for var I := 0 to FHeaders.Count - 1 do
  begin
    if SameText(FHeaders[I].Key, AName) then
      Exit(I);
  end;
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
