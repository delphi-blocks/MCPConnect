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
  MCPConnect.MCP.Types.Subscriptions,
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
  STransportWriterCannotWrite = 'This transport writer does not implement Write';
  STransportClientClosedStream = 'The client closed the connection';
  STransportStreamWriteFailed = 'The response stream could not be written';

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
    /// <summary>
    ///   Writes AValue as one SSE event. Deliberately without an event id:
    ///   ids existed only for resumability, which went with sessions in
    ///   2026-07-28, and this transport has nothing to replay them from.
    /// </summary>
    procedure Write(const AValue: string);

    /// <summary>
    ///   Writes an SSE comment: a line every client ignores and every
    ///   intermediary sees as traffic.
    /// </summary>
    /// <remarks>
    ///   The transport writes one on a "subscriptions/listen" stream that has
    ///   been silent for TMCPSubscriptionsConfig.KeepAliveInterval. It is the
    ///   liveness probe as much as the keep-alive: a dead peer is often not
    ///   visible in Connected, while a write to it fails at once. A writer with
    ///   no notion of a comment (stdio, where it would be a line that is not
    ///   JSON-RPC) writes nothing.
    /// </remarks>
    procedure WriteComment(const AValue: string);

    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>
  ///   What every IMCPTransportWriter has in common, so that a writer only
  ///   declares the parts it actually does differently.
  /// </summary>
  /// <remarks>
  ///   Write is the one method a writer must always answer for itself; the
  ///   other defaults are the harmless ones. SupportsStreaming is False, so a
  ///   writer that says nothing is treated as unable to hold a stream rather
  ///   than accepting one it cannot deliver; Connected is True, so a writer
  ///   that cannot tell does not end every stream the moment it starts; and
  ///   WriteComment writes nothing.
  /// </remarks>
  TMCPTransportWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string); virtual;
    procedure WriteComment(const AValue: string); virtual;
    function Connected: Boolean; virtual;
    function SupportsStreaming: Boolean; virtual;
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
    FCanStream: Boolean;
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

    /// <summary>
    ///   Whether the transport serving this request can stream a reply at all:
    ///   a fact about the deployment, where AcceptsEventStream is a fact about
    ///   the client. Set by the handler from its writer.
    /// </summary>
    property CanStream: Boolean read FCanStream write FCanStream;

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

  /// <summary>
  ///   The requests one connection has in flight, by id: where a
  ///   "notifications/cancelled" finds the request it names.
  /// </summary>
  /// <remarks>
  ///   Per connection, never per server. A JSON-RPC id is unique only within
  ///   one client, so a server-wide map would cancel another client's request
  ///   the moment two of them both used id 1. The transport that owns the
  ///   connection owns the map and hands it to each TMCPTransportHandler it
  ///   creates (TMCPTransportHandler.InFlight).
  ///
  ///   Stdio is one client on one connection, so it has one map. Streamable
  ///   HTTP has none: with no sessions there is nothing to tie a cancellation,
  ///   which arrives as a request of its own, to the request it names - and a
  ///   request is cancelled there by closing its stream instead.
  ///
  ///   Thread safe. Cancel runs the request's cancellation under this map's
  ///   lock, which is what keeps a request that is finishing from freeing its
  ///   cancellation while it is being cancelled: Remove waits for it.
  /// </remarks>
  TMCPInFlightRequests = class
  private
    FLock: TObject;
    FRequests: TDictionary<string, TMCPCancellation>;

    /// <summary>
    ///   The key of an id: its JSON type and its value, so that 7 and "7" are
    ///   different requests, as they are on the wire.
    /// </summary>
    class function KeyOf(const AId: TJRPCID): string; overload; static;
    class function KeyOf(AId: TJSONValue): string; overload; static;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Files ACancellation under AId. False, and nothing filed, when the id is
    ///   null or already in flight: a client reusing the id of a request still
    ///   running cannot cancel either of them unambiguously, so the first keeps
    ///   it.
    /// </summary>
    function Add(const AId: TJRPCID; ACancellation: TMCPCancellation): Boolean;

    /// <summary>
    ///   Removes AId, but only while it is still ACancellation's: a request
    ///   never removes another one's entry.
    /// </summary>
    procedure Remove(const AId: TJRPCID; ACancellation: TMCPCancellation);

    /// <summary>
    ///   Cancels the request in flight under AId, for AReason. Answers whether
    ///   there was one; an unknown id is ignored, which the specification
    ///   allows - the request may simply have finished.
    /// </summary>
    function Cancel(AId: TJSONValue; const AReason: string): Boolean;

    /// <summary>
    ///   How many requests are in flight.
    /// </summary>
    function Count: Integer;
  end;

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

    /// <summary>
    ///   The cancellation state of the request being handled, made by
    ///   HandlePOST and owned by the request's garbage collector. Nil before.
    /// </summary>
    FCancellation: TMCPCancellation;

    /// <summary>
    ///   See InFlight. Not owned.
    /// </summary>
    FInFlight: TMCPInFlightRequests;
  private
    /// <summary>
    ///   Answers a "notifications/cancelled": tells the application through
    ///   MessageHandling.OnCancelled, and cancels the request it names when this
    ///   connection has it in flight.
    /// </summary>
    procedure HandleCancelledNotification(ANotification: TJRPCNotification);

    /// <summary>
    ///   Whether the request being handled has been cancelled.
    /// </summary>
    function Cancelled: Boolean;

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

    /// <summary>
    ///   Turns the response into an SSE stream and sends its headers: the
    ///   status, the media type, and the two that keep an intermediary from
    ///   holding the stream back.
    /// </summary>
    procedure SendSSEResponseHeaders;

    procedure WriteSSEResponse(const AValue: string);

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

    /// <summary>
    ///   Whether AMessage is a log notification the request did not ask for:
    ///   one for a request that sent no log level at all, or one chattier than
    ///   the level it sent.
    /// </summary>
    /// <remarks>
    ///   "If absent, the server MUST NOT send any notifications/message for
    ///   this request." TMCPLog already refuses to build one, so what reaches
    ///   here is a notification enqueued by hand - the same escape hatch, and
    ///   the same last word on it, as for progress. True only once the request
    ///   "_meta" has been read, for the same reason.
    /// </remarks>
    function IsUnsolicitedLog(AMessage: TJRPCMessage): Boolean;

    /// <summary>
    ///   Whether AMessages holds anything a response answers: a request, or the
    ///   error a malformed payload was replaced with. False for a payload of
    ///   nothing but notifications.
    /// </summary>
    /// <remarks>
    ///   JSON-RPC gives a notification no reply, and the Streamable HTTP
    ///   transport answers an accepted one with "202 Accepted" and no body -
    ///   whatever its Accept asked for. So this is what decides the shape of
    ///   the reply, and it can only be asked once the payload has been parsed.
    /// </remarks>
    function ExpectsResponse(AMessages: TJRPCMessages): Boolean;

    /// <summary>
    ///   The "subscriptions/listen" request AMessages is, or nil. A body is a
    ///   single message, so this is a question about its only one.
    /// </summary>
    class function ListenRequestOf(AMessages: TJRPCMessages): TJRPCRequest; static;

    /// <summary>
    ///   Why a listen cannot be given its stream here, or False when it can.
    ///   Asked before dispatch, so that a refusal is a status and a document
    ///   rather than an error frame on a stream that has already said 200.
    /// </summary>
    function ListenRefusal(out AReason: TMCPListenRefusal): Boolean;

    /// <summary>
    ///   Whether the token the stream was opened with has expired since.
    ///   False when it carries no expiry at all - a static token, or none.
    /// </summary>
    function TokenExpired: Boolean;

    /// <summary>
    ///   Holds a "subscriptions/listen" stream open: writes what the registry
    ///   publishes to the subscription, a keep-alive when it has been quiet,
    ///   and, if the subscription ends gracefully, AHeld - the listen result -
    ///   as the last frame.
    /// </summary>
    /// <remarks>
    ///   Never raises. A write that fails is a client that went away, which is
    ///   a disconnect and not a server error: answering it with a 500 would be
    ///   a write down the same dead connection. Leaves the subscription
    ///   closed, never freed - HandlePOST does that in its finally.
    /// </remarks>
    procedure PumpSubscription(ASubscription: TMCPSubscription; AHeld: TJRPCMessage);

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

    /// <summary>
    ///   The requests in flight on the connection this handler serves, or nil
    ///   when the transport has no way to route a cancellation to one (see
    ///   TMCPInFlightRequests). Set by the transport before ProcessRequest; the
    ///   request is filed there while it runs, and a "notifications/cancelled"
    ///   handled here looks there for the request it names.
    /// </summary>
    property InFlight: TMCPInFlightRequests read FInFlight write FInFlight;
  end;

implementation

uses
  Logify,
  JRPC.Invoker,
  Neon.Core.Utils,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.Neon;

{ TMCPTransportWriter }

procedure TMCPTransportWriter.Write(const AValue: string);
begin
  // A writer that says nothing about how it writes is not a writer: a transport
  // that can deliver anything has to say how
  raise EJRPCException.Create(STransportWriterCannotWrite);
end;

procedure TMCPTransportWriter.WriteComment(const AValue: string);
begin
  // Nothing to do, and that is the honest default rather than an oversight: a
  // transport with no notion of a comment is a real case, and a stream held
  // open on it relies on its own framing
end;

function TMCPTransportWriter.Connected: Boolean;
begin
  // Optimistic: a writer that cannot tell whether its peer is still there must
  // not answer False, or every stream would end the moment it started
  Result := True;
end;

function TMCPTransportWriter.SupportsStreaming: Boolean;
begin
  // Pessimistic, the deliberate opposite of Connected: a writer that has not
  // said it can hold a stream is not asked to, so a request that needs one is
  // refused rather than answered into a void
  Result := False;
end;

{ TMCPInFlightRequests }

constructor TMCPInFlightRequests.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FRequests := TDictionary<string, TMCPCancellation>.Create;
end;

destructor TMCPInFlightRequests.Destroy;
begin
  // Holds, never owns: every cancellation belongs to its request
  FRequests.Free;
  FLock.Free;
  inherited;
end;

class function TMCPInFlightRequests.KeyOf(const AId: TJRPCID): string;
begin
  if AId.IsNull then
    Result := ''
  else if AId.IsString then
    Result := 's:' + AId.AsString
  else
    Result := 'n:' + AId.AsString;
end;

class function TMCPInFlightRequests.KeyOf(AId: TJSONValue): string;
begin
  // The two halves have to agree: a TJRPCID renders a number without quotes
  // and a string as its bare value, which is what this reads off the JSON
  if AId is TJSONString then
    Result := 's:' + TJSONString(AId).Value
  else if AId is TJSONNumber then
    Result := 'n:' + TJSONNumber(AId).ToJSON
  else
    Result := '';
end;

function TMCPInFlightRequests.Add(const AId: TJRPCID; ACancellation: TMCPCancellation): Boolean;
var
  LKey: string;
begin
  LKey := KeyOf(AId);
  if LKey.IsEmpty then
    Exit(False);

  TMonitor.Enter(FLock);
  try
    Result := not FRequests.ContainsKey(LKey);
    if Result then
      FRequests.Add(LKey, ACancellation);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TMCPInFlightRequests.Remove(const AId: TJRPCID; ACancellation: TMCPCancellation);
var
  LKey: string;
  LFiled: TMCPCancellation;
begin
  LKey := KeyOf(AId);
  if LKey.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    if FRequests.TryGetValue(LKey, LFiled) and (LFiled = ACancellation) then
      FRequests.Remove(LKey);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPInFlightRequests.Cancel(AId: TJSONValue; const AReason: string): Boolean;
var
  LKey: string;
  LCancellation: TMCPCancellation;
begin
  LKey := KeyOf(AId);
  if LKey.IsEmpty then
    Exit(False);

  // Under the lock, cancellation included: released first, the request could
  // finish and free its cancellation before it was cancelled. Lock order is
  // this map, then the cancellation, then whatever its hook takes.
  TMonitor.Enter(FLock);
  try
    Result := FRequests.TryGetValue(LKey, LCancellation);
    if Result then
      LCancellation.Cancel(AReason);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPInFlightRequests.Count: Integer;
begin
  TMonitor.Enter(FLock);
  try
    Result := FRequests.Count;
  finally
    TMonitor.Exit(FLock);
  end;
end;

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

procedure TMCPTransportHandler.SendSSEResponseHeaders;
begin
  FResponse.Code := HTTP_CODE_OK;
  FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;

  // A stream is not a document: an intermediary that caches it, or holds it
  // until it looks complete, breaks the one thing it is for. Cache-Control is
  // the standard way to say the first; X-Accel-Buffering is nginx's way of
  // being told the second, honoured by enough reverse proxies that the spec
  // makes it a SHOULD on every SSE response.
  FResponse.SetHeader('Cache-Control', 'no-cache');
  FResponse.SetHeader('X-Accel-Buffering', 'no');

  SendResponseHeaders(FResponse);
end;

procedure TMCPTransportHandler.WriteSSEResponse(const AValue: string);
begin
  if Assigned(FResponseWriter) then
  begin
    Logger.LogDebug('[SSE] Event Sent [size=%d]', [Length(AValue)]);
    {$IFDEF FULL_PAYLOAD_LOGGING}
    Logger.LogTrace('[SSE] data: %s', [AValue]);
    {$ENDIF}
    FResponseWriter.Write(AValue);
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
    FRequest.CanStream := Assigned(FResponseWriter) and FResponseWriter.SupportsStreaming;
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
        // A request the client cancelled is answered with nothing at all, an
        // error included: the server MUST NOT send anything further for it.
        // Whatever a cancelled request raises is taken the same way - a tool
        // that noticed in its own fashion, an operation the OnCancel hook
        // aborted - since none of it is anything the client still wants.
        on E: Exception do
        if (AMessage is TJRPCRequest) and ((E is EMCPRequestCancelled) or Cancelled) then
        begin
          Logger.LogDebug('Request [%s] cancelled, nothing is answered: %s',
            [TJRPCRequest(AMessage).Id.AsString, E.Message]);
        end
        else
        begin
          Logger.LogError(E, Format('TMCPTransportHandler.HandleMessage %s: %s', [E.ClassName, E.Message]));

          // The status this error deserves, recorded in the one place where it
          // is still an exception: the JSON-RPC code alone cannot say, since
          // -32602 is a 400 when it reports a malformed "_meta" and an ordinary
          // 200 when it reports an unknown tool name. One message per request, so
          // there is exactly one status to record; the guard below keeps the
          // first anyway, since HandlePOST can have recorded one already for a
          // payload it refused before dispatch.
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
  // one at all. A cancellation is acted on; there is no longer an inbound
  // session queue to route anything else to, so the rest are accepted and
  // dropped.
  if AContext.Message is TJRPCNotification then
  begin
    if TJRPCNotification(AContext.Message).Method = MCP_NOTIFY_CANCELLED then
      HandleCancelledNotification(TJRPCNotification(AContext.Message))
    else
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
    // reply - a body that would not parse, or one this transport refused - and
    // it has to reach the client.
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

procedure TMCPTransportHandler.HandleCancelledNotification(ANotification: TJRPCNotification);
var
  LParams: TCancelledNotificationParams;
  LRequestId, LReason: TJSONValue;
begin
  // Read by hand rather than through Neon: RequestId is raw JSON on purpose, a
  // string or a number, and that is exactly what the params object holds
  LRequestId := nil;
  LReason := nil;
  if ANotification.Params is TJSONObject then
  begin
    LRequestId := TJSONObject(ANotification.Params).GetValue('requestId');
    LReason := TJSONObject(ANotification.Params).GetValue('reason');
  end;

  // REQUIRED, and a string or a number. Without one there is nothing to name,
  // and a notification cannot be answered with an error: dropped.
  if not ((LRequestId is TJSONString) or (LRequestId is TJSONNumber)) then
  begin
    Logger.LogDebug('Discarding a notifications/cancelled that names no request');
    Exit;
  end;

  LParams := TCancelledNotificationParams.Create;
  try
    LParams.RequestId := LRequestId.Clone as TJSONValue;
    if LReason is TJSONString then
      LParams.Reason := TJSONString(LReason).Value;

    // The request first, the hook second: a hook that takes its time must not
    // keep a running tool going for that long
    if Assigned(FInFlight) and FInFlight.Cancel(LParams.RequestId, LParams.Reason.GetValueOrDefault) then
      Logger.LogDebug('Request [%s] cancelled by the client', [LRequestId.ToJSON])
    else
      // Not an error: the request may have finished already, and over HTTP
      // there is no map at all - the stream it ran on is what cancels it
      Logger.LogDebug('notifications/cancelled for [%s]: no such request in flight here',
        [LRequestId.ToJSON]);

    // The application's, and a notification has nobody to report its failure
    // to: logged, so that it does not turn into a 500 for a message the
    // specification says must not be answered
    if Assigned(FMCPConfig) and Assigned(FMCPConfig.MessageHandling.CancelledProc) then
    try
      FMCPConfig.MessageHandling.CancelledProc(FContext, LParams);
    except
      on E: Exception do
        Logger.LogError(E, Format('MessageHandling.OnCancelled %s: %s', [E.ClassName, E.Message]));
    end;
  finally
    LParams.Free;
  end;
end;

function TMCPTransportHandler.Cancelled: Boolean;
begin
  Result := Assigned(FCancellation) and FCancellation.IsCancelled;
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

function TMCPTransportHandler.IsUnsolicitedLog(AMessage: TJRPCMessage): Boolean;
var
  LLog: TMCPLog;
  LParams: TJSONValue;
  LName: string;
  LLevel: TMCPLogLevel;
begin
  if not (AMessage is TJRPCNotification) or
     (TJRPCNotification(AMessage).Method <> MCP_NOTIFY_MESSAGE) then
    Exit(False);

  LLog := FContext.FindContextDataAs<TMCPLog>;
  if not Assigned(LLog) or not LLog.Known then
    Exit(False);

  // No level was asked for: the MUST NOT, and no need to look at the message
  if not LLog.Wanted then
    Exit(True);

  // A level was asked for, and it is a minimum. A notification whose own level
  // cannot be read is let through: the request did ask for logging, and a
  // malformed payload is not this method's to diagnose.
  LParams := TJRPCNotification(AMessage).Params;
  if not (LParams is TJSONObject) then
    Exit(False);

  LName := '';
  var LLevelValue := TJSONObject(LParams).GetValue('level');
  if LLevelValue is TJSONString then
    LName := TJSONString(LLevelValue).Value;

  if not MCPLogLevelFromName(LName, LLevel) then
    Exit(False);

  Result := not LLog.Emits(LLevel);
end;

function TMCPTransportHandler.ExpectsResponse(AMessages: TJRPCMessages): Boolean;
begin
  // Types is the set of what the payload actually held, kept as the list was
  // built - the error a malformed payload was replaced with included, which is
  // very much something to answer.
  Result := (AMessages.Types - [TJRPCMessageType.Notification]) <> [];
end;

class function TMCPTransportHandler.ListenRequestOf(AMessages: TJRPCMessages): TJRPCRequest;
begin
  Result := nil;
  if not Assigned(AMessages) or (AMessages.Count <> 1) then
    Exit;

  if (AMessages.List[0] is TJRPCRequest) and
     (TJRPCRequest(AMessages.List[0]).Method = MCP_SUBSCRIPTIONS_LISTEN) then
    Result := TJRPCRequest(AMessages.List[0]);
end;

function TMCPTransportHandler.ListenRefusal(out AReason: TMCPListenRefusal): Boolean;
begin
  Result := True;

  // The client asked for a document, and a stream that stays open is the only
  // answer a listen has
  if not FRequest.AcceptsEventStream then
    AReason := TMCPListenRefusal.NotAccepted

  // The client did nothing wrong: this deployment cannot hold a response open
  // (WebBroker before 13.1, CGI)
  else if not FRequest.CanStream then
    AReason := TMCPListenRefusal.NoStream

  // Checked again, atomically, when the subscription is opened. This one is so
  // that the common case is refused with a status rather than with an error
  // frame on a stream that already said 200.
  else if FMCPConfig.Subscriptions.Registry.IsFull then
    AReason := TMCPListenRefusal.TooMany

  else
    Result := False;
end;

function TMCPTransportHandler.TokenExpired: Boolean;
var
  LExpiration: TDateTime;
begin
  if not Assigned(FAccessToken) then
    Exit(False);

  LExpiration := FAccessToken.Expiration;
  Result := (LExpiration > 0) and (TTimeZone.Local.ToUniversalTime(Now) >= LExpiration);
end;

procedure TMCPTransportHandler.PumpSubscription(ASubscription: TMCPSubscription; AHeld: TJRPCMessage);
var
  LRegistry: TMCPSubscriptionRegistry;
  LInterval: Integer;
  LMessage: TJRPCMessage;
begin
  LRegistry := FMCPConfig.Subscriptions.Registry;

  LInterval := FMCPConfig.Subscriptions.KeepAliveInterval;
  if LInterval <= 0 then
    LInterval := MCP_SUBSCRIPTION_KEEPALIVE_MS;

  Logger.LogDebug('[SSE] Subscription %s open', [ASubscription.Id.ToJSON]);
  try
    // The response queue was closed the moment the api method returned, and
    // that means nothing here: what ends this loop is the subscription's own
    // queue closing - the registry closing it, or an overflow - or the client
    // going away.
    while True do
    begin
      // Cheap, and catches what the socket does report. A half-open peer is
      // caught by the keep-alive write below failing instead.
      if not FResponseWriter.Connected then
      begin
        Logger.LogDebug('[SSE] Subscription %s: the client disconnected', [ASubscription.Id.ToJSON]);
        LRegistry.Close(ASubscription, False);
        Exit;
      end;

      // The token was checked once, when the stream opened. A client whose
      // access has since lapsed is not to keep listening on it for ever.
      if TokenExpired then
      begin
        Logger.LogDebug('[SSE] Subscription %s: the access token expired', [ASubscription.Id.ToJSON]);
        LRegistry.Close(ASubscription, True);
      end;

      // Returns what is still queued even once the queue is closed, so a
      // notification published just before a close is not lost; nil on an
      // empty closed queue, and nil on the timeout - which is the keep-alive
      // tick, so there is no second timer.
      LMessage := ASubscription.Queue.DequeueWait(LInterval);
      if Assigned(LMessage) then
      begin
        try
          WriteSSEResponse(LMessage.ToJson);
        finally
          LMessage.Free;
        end;
      end
      else if ASubscription.Queue.Closed then
        Break
      else
        FResponseWriter.WriteComment('keep-alive');
    end;

    // The queue closed while the subscription is still live: nobody closed it,
    // so it overflowed. The client has lost a change it will never ask about,
    // and the honest answer is an abrupt end - no result - which is what a
    // client reads as a reason to reconnect and re-read. A result here would
    // tell it the subscription ended cleanly and nothing was missed.
    if ASubscription.State = TMCPSubscriptionState.Live then
    begin
      LRegistry.Close(ASubscription, False);
      Exit;
    end;

    // Ended by the server, gracefully: the result it has been holding back is
    // owed, as the last frame. An abrupt end owes nothing.
    if (ASubscription.State = TMCPSubscriptionState.ClosedGracefully) and Assigned(AHeld) then
      WriteSSEResponse(AHeld.ToJson);
  except
    on E: Exception do
    begin
      // A write on a dead socket. Logged and swallowed: the client is gone, and
      // there is no one to tell.
      Logger.LogDebug('[SSE] Subscription %s ended by a failed write: %s',
        [ASubscription.Id.ToJSON, E.Message]);
      LRegistry.Close(ASubscription, False);
    end;
  end;
end;

procedure TMCPTransportHandler.HandlePOST;
const
  QueueReadTimeout = 500;
var
  LResponseList: TJRPCMessages;

  /// <summary>
  ///   Whether this reply is going out as a stream. Decided once, after the
  ///   parse, and read everywhere the reply is written.
  /// </summary>
  LStreamed: Boolean;

  /// <summary>
  ///   The meeting point with the listen endpoint, for a "subscriptions/listen"
  ///   this transport is going to hold open; nil for every other request.
  /// </summary>
  LListen: TMCPListenStream;

  /// <summary>
  ///   The listen result, kept back while the subscription is live. Owned by
  ///   the garbage collector from the moment it is held.
  /// </summary>
  LHeld: TJRPCMessage;

  procedure ProcessQueue(AResponseList: TMCPMessageQueue);
  begin
    AResponseList.Process(
      procedure (AMessage: TJRPCMessage; var ADispose: Boolean)
      begin
        if Cancelled then
        begin
          // The gate every message passes, which is why it sits here and not in
          // the channels: TMCPProgress, TMCPLog and a tool's own queue all
          // enqueue straight into this queue. Once the request is cancelled the
          // server MUST NOT send anything further for it, the result and the
          // held listen answer included; leaving ADispose True frees it.
          Logger.LogDebug('Message dropped, the request was cancelled');
        end
        else if IsUnsolicitedProgress(AMessage) then
        begin
          Logger.LogDebug('Progress notification dropped, the request asked for none');
        end
        else if IsUnsolicitedLog(AMessage) then
        begin
          Logger.LogDebug('Log notification dropped, the request asked for no logging at this level');
        end
        else if Assigned(LListen) and Assigned(LListen.Subscription) and (AMessage is TJRPCResponse) then
        begin
          // The hold rule. Written now, the result would follow the
          // acknowledgement at once and close the exchange - the graceful-end
          // signal, which a client answers by tearing down and subscribing
          // again, for ever. It is the last frame of the stream instead, and
          // only if the stream ends gracefully.
          //
          // Only a response is held: an error for this id means the listen
          // failed, and that is what the client must be told.
          ADispose := False;
          FGarbage.Add(AMessage);
          LHeld := AMessage;
        end
        else if Assigned(LListen) and (AMessage is TJRPCNotification) and
          (TJRPCNotification(AMessage).Method <> MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED) then
        begin
          // A listen stream carries its acknowledgement and what the registry
          // publishes, and nothing else: progress and log messages are
          // request-scoped, and never travel on one.
          Logger.LogDebug('[SSE] Notification dropped, a listen stream does not carry it [method=%s]',
            [TJRPCNotification(AMessage).Method]);
        end
        else if LStreamed then
        begin
          // A write that fails is a client that went away, which over HTTP is
          // what cancelling a request is. The request is told, and the queue is
          // still drained - into the gate above - until the worker is done.
          try
            WriteSSEResponse(AMessage.ToJson);
          except
            on E: Exception do
            begin
              Logger.LogDebug('[SSE] Write failed, cancelling the request: %s', [E.Message]);
              FCancellation.Cancel(STransportStreamWriteFailed);
            end;
          end;
        end
        else if AMessage is TJRPCNotification then
        begin
          // A server-to-client notification is not a reply to anything, so it has
          // no place in the JSON-RPC payload that answers this POST: per JSON-RPC
          // 2.0 a Request is answered with a Response, and nothing else belongs
          // there. SSE is the only channel this transport has for one -
          // there is no GET endpoint since sessions went away - and there is no
          // stream here, either because the client asked for none or because
          // this payload is answered 202 and carries no reply at all. Dropped
          // rather than spliced into the reply; leaving ADispose True frees it.
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
    // Parsed once per request: the request-headers middleware leaves its tree
    // here when it runs, and this puts one here when it does not, so no body is
    // parsed twice. TMCPTransportRequest owns it either way.
    if not Assigned(FRequest.ContentJSON) and not FRequest.Content.IsEmpty then
      FRequest.ContentJSON := TJSONObject.ParseJSONValue(FRequest.Content);

    // A top-level array is a JSON-RPC batch, and 2026-07-28 requires the body of
    // a request to be a single request or a single notification. Refused here,
    // before anything is dispatched, so nothing downstream ever sees more than
    // one message - and an empty "[]" is refused by the same rule as a full one,
    // rather than as the Invalid Request the JSON-RPC layer would make of it.
    //
    // That layer still reads and answers batches for its own callers: this is a
    // policy of the MCP transport, not a missing capability.
    if FRequest.ContentJSON is TJSONArray then
      raise EMCPBatchNotSupportedError.Create;

    if Assigned(FRequest.ContentJSON) then
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.ContentJSON)
    else
      // Not JSON at all, or empty: let the JSON-RPC layer raise the parse error
      // it raises for every other unreadable body
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  except
    on E: EJRPCException do
    begin
      // Per JSON-RPC 2.0, malformed JSON (parse error) or a top-level value that
      // is not a Request object must be answered with a single JSON-RPC error
      // response carrying a null id - never an HTTP 500 or an empty body.
      var LErrorId: TJRPCID;
      LRequestList := TJRPCMessages.Create(True);
      LRequestList.AddMessage(TJRPCError.CreateFromException(E, LErrorId));

      // The status the refusal deserves, recorded in the one place where it is
      // still an exception - the same rule HandleMessage applies, which never
      // sees this one because it is refused before dispatch. A parse error is
      // not an EMCPProtocolError and keeps its ordinary 200.
      if E is EMCPProtocolError then
        FProtocolStatus := HTTP_CODE_BADREQUEST;
    end;
  end;
  Logger.LogDebug('[PERF] Transport CreateFromJSON: %d ms', [LFragment.ElapsedMilliseconds]);

  FGarbage.Add(LRequestList);

  LListen := nil;
  LHeld := nil;

  // A listen is a request for a stream that stays open, and it is refused here,
  // before anything is dispatched, when this request cannot have one. The
  // alternative - acknowledge, then end the subscription at once - is what a
  // client reads as a clean close, and it reconnects for ever.
  //
  // Over HTTP only. A stdio listen keeps answering at once until that transport
  // can dispatch concurrently: holding one open today would block it for good.
  var LListenRequest := ListenRequestOf(LRequestList);
  if Assigned(LListenRequest) and (FRequest.Protocol = TTransportProtocol.StreamableHTTP) then
  begin
    var LReason: TMCPListenRefusal;
    if ListenRefusal(LReason) then
    begin
      var LRefusal := EMCPListenNotStreamableError.CreateFor(LReason,
        FMCPConfig.Subscriptions.MaxSubscriptions);

      try
        // Answered as the parse errors above are: the request is replaced with
        // the error it earned, which the message chain hands back unchanged,
        // and the status is settled now, before any header goes out
        var LRefusalList := TJRPCMessages.Create(True);
        FGarbage.Add(LRefusalList);
        LRefusalList.AddMessage(TJRPCError.CreateFromException(LRefusal, LListenRequest.Id));
        LRequestList := LRefusalList;
      finally
        LRefusal.Free;
      end;

      // 406 when the client asked for a shape this request cannot be answered
      // in; 400 when it did nothing wrong and this deployment cannot do it
      if LReason = TMCPListenRefusal.NotAccepted then
        FProtocolStatus := HTTP_CODE_NOTACCEPTABLE
      else
        FProtocolStatus := HTTP_CODE_BADREQUEST;
    end
    else
    begin
      // A scope of its own, because the id alone is not one: two clients may
      // both listen with id 1, and with no sessions nothing else separates
      // their streams. The queue the stream is drained from is not made here:
      // the subscription makes and owns it, so the two live and die together.
      LListen := TMCPListenStream.Create(TGUID.NewGuid.ToString);
      FGarbage.Add(LListen);
      FContext.AddContent(LListen);
    end;
  end;

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

  // And the log channel, on the same terms and for the same reason: what the
  // request asked to be told is read by TMCPRequestMetaMiddleware and by
  // nothing else, and until it says otherwise this one emits nothing.
  var LLog := TMCPLog.Create(LResponseQueue);
  FGarbage.Add(LLog);
  FContext.AddContent(LLog);

  // And whether the client still wants the answer, on the same terms: always in
  // the context, so a tool never tests it for nil. Filed under the request's id
  // where the transport can route a "notifications/cancelled" to it.
  FCancellation := TMCPCancellation.Create;
  FGarbage.Add(FCancellation);
  FContext.AddContent(FCancellation);

  var LInFlightRequest: TJRPCRequest := nil;
  if Assigned(FInFlight) and (LRequestList.Count = 1) and (LRequestList.List[0] is TJRPCRequest) then
  begin
    LInFlightRequest := TJRPCRequest(LRequestList.List[0]);
    if not FInFlight.Add(LInFlightRequest.Id, FCancellation) then
    begin
      // A null id, or one already in flight: nothing a cancellation could name
      // unambiguously, so nothing is filed - and nothing removed at the end
      Logger.LogDebug('Request [%s] not filed as in flight: its id is null or already in use',
        [LInFlightRequest.Id.AsString]);
      LInFlightRequest := nil;
    end;
  end;

  // This list contains the responses in case SSE channel is not active
  LResponseList := TJRPCMessages.Create(True);
  FGarbage.Add(LResponseList);

  // No carry-over of the payload's shape: the body is a single message or it was
  // refused, so an object always answers an object. TJRPCMessages.Single is True
  // for a hand-built list, which is what this one is.

  // Decided here - after the parse, before anything is written. A payload of
  // nothing but notifications is answered "202 Accepted" with no body whatever
  // its Accept asked for, so there is no stream to open for one. Deciding this
  // before the parse, on the Accept alone, is what used to answer a notification
  // 200 with an empty event stream.
  //
  // A payload already refused above is answered as a document too, whatever the
  // Accept: its status was settled before any header went out, and it is the one
  // refusal early enough for the reply to carry it. Streaming it would send 200
  // with the refusal in a frame - which is what a status decided *during*
  // dispatch has to settle for, and this one does not.
  LStreamed := (FProtocolStatus = 0) and ExpectsResponse(LRequestList) and
    FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming;

  // Sent before the worker starts, not after: nothing may be written on a stream
  // before the headers that open it, and once the worker runs it can produce at
  // any moment. The status is fixed by now either way - a status decided during
  // dispatch cannot reach a streamed reply.
  if LStreamed then
    SendSSEResponseHeaders;

  LFragment := TStopwatch.StartNew;
  var LAsyncExecute := CreateAsyncThread(LRequestList, LResponseQueue);
  try
    // The worker thread closes the queue when done, which wakes ProcessQueue
    // immediately: on the happy path no read timeout is ever paid. The loop is
    // still needed because a slow tool can let the timeout expire before
    // producing anything, and the final drain catches messages enqueued between
    // the last timeout and the Closed check.
    while not LResponseQueue.Closed do
    begin
      ProcessQueue(LResponseQueue);

      // Between reads, which is at most every QueueReadTimeout while the worker
      // runs: a client that has closed the stream has cancelled the request,
      // and over HTTP that is the way a request is cancelled. Only asked of a
      // stream: Connected describes one, and a writer may have to open a stream
      // to answer it (WebBroker does) - which a reply going out as a document
      // must not get. A document reply has no disconnect to notice, then; its
      // request runs to the end and the answer goes nowhere.
      if LStreamed and not Cancelled and not FResponseWriter.Connected then
      begin
        Logger.LogDebug('The client went away, cancelling the request');
        FCancellation.Cancel(STransportClientClosedStream);
      end;
    end;
    ProcessQueue(LResponseQueue);

    // A listen that opened a subscription: the acknowledgement is out, the
    // result is held, and the stream stays open until the subscription ends.
    // The registry must not be left holding it once this request is gone, so
    // it is closed and freed here whatever the pump did, a listen that failed
    // after opening included - and freed with its queue, which it owns.
    if Assigned(LListen) and Assigned(LListen.Subscription) then
    begin
      var LSubscription := LListen.Subscription;
      try
        // A cancelled listen ends abruptly: the client asked for it to stop,
        // and the server MUST NOT send anything further - the result included.
        // Set before the pump, and run at once if the cancellation came first.
        FCancellation.OnCancel :=
          procedure (ACancellation: TMCPCancellation)
          begin
            FMCPConfig.Subscriptions.Registry.Close(LSubscription, False);
          end;

        if Assigned(LHeld) then
          PumpSubscription(LSubscription, LHeld);
      finally
        // Cleared before the subscription is freed: the hook runs under the
        // cancellation's lock, so once this returns it cannot run again
        FCancellation.OnCancel := nil;

        FMCPConfig.Subscriptions.Registry.Close(LSubscription, False);
        FreeAndNil(LListen.Subscription);
      end;
    end;

    // Not a stream: the reply is a document, or it is nothing at all
    if not LStreamed then
    begin
      if Cancelled then
      begin
        // Nothing is answered for a cancelled request. Over HTTP the client has
        // gone, so this reaches nobody; on stdio an empty body is no line at all,
        // which is the silence the specification asks for.
        FResponse.Code := HTTP_CODE_NOCONTENT;
        FResponse.Content := '';
      end
      else if LResponseList.Count = 0 then
      begin
        // Taken, and there is nothing to say about it: a notification gets no
        // reply, so no body - and with no body, no Content-Type either.
        FResponse.Code := HTTP_CODE_ACCEPTED;
        FResponse.Content := '';
      end
      else
      begin
        // One message in, one reply out, so a protocol status always has exactly
        // one outcome to describe. The guard this used to carry - "only when the
        // reply is single" - existed for batches, which are now refused before
        // they reach here.
        if FProtocolStatus <> 0 then
          FResponse.Code := FProtocolStatus
        else
          FResponse.Code := HTTP_CODE_OK;
        FResponse.ContentType := TMediaType.APPLICATION_JSON;
        FResponse.Content := LResponseList.ToJson;
      end;
    end;
  finally
    // Waits for the worker, which is what makes removing the entry after it
    // safe: nothing of this request runs any more
    LAsyncExecute.Free;

    if Assigned(LInFlightRequest) then
      FInFlight.Remove(LInFlightRequest.Id, FCancellation);
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
