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
unit MCPConnect.MCP.Types.Subscriptions;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,

  JRPC.Core,
  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications;

const
  /// <summary>
  ///   The long-lived request that opens a notification stream. It replaces the
  ///   HTTP GET endpoint, "resources/subscribe" and "resources/unsubscribe".
  /// </summary>
  MCP_SUBSCRIPTIONS_LISTEN = 'subscriptions/listen';

  /// <summary>
  ///   The notification acknowledging which of the requested notification types
  ///   the server will actually send.
  /// </summary>
  MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED = 'notifications/subscriptions/acknowledged';

  /// <summary>
  ///   How many notifications one subscription's queue holds before it is
  ///   closed and the stream ended. The default; see
  ///   TMCPSubscriptionsConfig.QueueCap to change it.
  /// </summary>
  /// <remarks>
  ///   Small on purpose. A subscription's messages are all invalidations of one
  ///   kind or another: a client that is 64 notifications behind is not reading,
  ///   and the queue's default cap of 1000 would let it fall a thousand changes
  ///   behind before anything noticed. Closing is recoverable - the client
  ///   reconnects and re-reads - while a silently dropped "tools changed" leaves
  ///   it with a cache it will never invalidate.
  /// </remarks>
  MCP_SUBSCRIPTION_QUEUE_CAP = 64;

  /// <summary>
  ///   How many subscription streams a server holds at once before it refuses
  ///   another. The default; see TMCPSubscriptionsConfig.MaxSubscriptions.
  /// </summary>
  /// <remarks>
  ///   A deployment-wide budget, not a per-client one: with no sessions there is
  ///   no client to count against, only streams. Each one holds a connection
  ///   and, on Indy or an ISAPI host, a thread - this is what bounds both.
  /// </remarks>
  MCP_SUBSCRIPTION_MAX = 64;

  /// <summary>
  ///   How long, in milliseconds, a subscription stream may stay silent before
  ///   the transport writes a keep-alive comment on it. The default; see
  ///   TMCPSubscriptionsConfig.KeepAliveInterval.
  /// </summary>
  /// <remarks>
  ///   The keep-alive is the liveness probe as much as it is a keep-alive: a
  ///   socket reports itself connected on a half-open TCP connection until a
  ///   send fails, so a write is the only way to find out. Shorter than the 15 s
  ///   WebBroker pings on, so the two do not fight.
  /// </remarks>
  MCP_SUBSCRIPTION_KEEPALIVE_MS = 2000;

resourcestring
  /// <summary>
  ///   Raised when two streams would answer to one subscription id. %s is the id.
  /// </summary>
  SSubscriptionAlreadyOpenFmt = 'A subscription is already open for id %s';

  /// <summary>
  ///   Logged when a subscription's queue overflowed and the stream is being
  ///   ended. %s is the subscription id.
  /// </summary>
  SSubscriptionOverflowFmt =
    'Subscription %s fell behind and its queue overflowed: the stream is being closed';

type
  /// <summary>
  ///   The notification types a client opts in to on a "subscriptions/listen"
  ///   request.
  /// </summary>
  /// <remarks>
  ///   Every type is opt-in: the server MUST NOT send a notification type the
  ///   client did not ask for here.
  /// </remarks>
  TSubscriptionFilter = class
  public
    /// <summary>
    ///   Receive "notifications/tools/list_changed".
    /// </summary>
    ToolsListChanged: NullBoolean;

    /// <summary>
    ///   Receive "notifications/prompts/list_changed".
    /// </summary>
    PromptsListChanged: NullBoolean;

    /// <summary>
    ///   Receive "notifications/resources/list_changed".
    /// </summary>
    ResourcesListChanged: NullBoolean;

    /// <summary>
    ///   Receive "notifications/resources/updated" for these resource uris.
    ///   Replaces the former "resources/subscribe" request.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] ResourceSubscriptions: TArray<string>;
  public
    /// <summary>
    ///   True when the flag is present and set.
    /// </summary>
    function WantsToolsListChanged: Boolean;
    function WantsPromptsListChanged: Boolean;
    function WantsResourcesListChanged: Boolean;

    /// <summary>
    ///   True when AUri was subscribed to, matched case-sensitively.
    /// </summary>
    function WantsResource(const AUri: string): Boolean;

    /// <summary>
    ///   True when nothing at all was requested.
    /// </summary>
    function IsEmpty: Boolean;

    /// <summary>
    ///   Copies the requested types onto ATarget.
    /// </summary>
    procedure AssignTo(ATarget: TSubscriptionFilter);
  end;

  /// <summary>
  ///   Parameters of a "subscriptions/listen" request.
  /// </summary>
  TSubscriptionsListenRequestParams = class(TRequestMetaParams)
  public
    /// <summary>
    ///   REQUIRED. The notifications the client opts in to on this stream.
    /// </summary>
    Notifications: TSubscriptionFilter;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Extends ResultMetaObject with the identifier of the subscription stream
  ///   the result closes.
  /// </summary>
  TSubscriptionsListenResultMeta = class(TResultMetaObject)
  public
    /// <summary>
    ///   REQUIRED. The JSON-RPC id of the "subscriptions/listen" request that
    ///   opened the stream, which equals this response's own id.
    /// </summary>
    /// <remarks>
    ///   A RequestId is a string or an integer, so it is carried as raw JSON;
    ///   set it with SetSubscriptionId.
    /// </remarks>
    [NeonProperty(MCP_META_SUBSCRIPTION_ID)]
    SubscriptionId: TJSONValue;
  public
    destructor Destroy; override;

    procedure SetSubscriptionId(const AId: string); overload;
    procedure SetSubscriptionId(AId: Int64); overload;
  end;

  /// <summary>
  ///   The response to a "subscriptions/listen" request, signalling that the
  ///   subscription ended gracefully - during server shutdown, say.
  /// </summary>
  /// <remarks>
  ///   Because the stream is long-lived, this is sent only when the server
  ///   tears the subscription down; an abrupt transport close carries no
  ///   response at all. The body is otherwise empty.
  /// </remarks>
  TSubscriptionsListenResult = class(TBaseResult)
  public
    constructor Create;

    /// <summary>
    ///   The stream identifier, in the result "_meta", where it is required.
    /// </summary>
    function Meta: TSubscriptionsListenResultMeta;
  end;

  /// <summary>
  ///   Parameters of a "notifications/subscriptions/acknowledged" notification.
  /// </summary>
  TSubscriptionsAcknowledgedNotificationParams = class(TMCPNotificationParams)
  public
    /// <summary>
    ///   REQUIRED. The subset of the requested notification types the server
    ///   agreed to honour. A type the server cannot serve - prompts changing on
    ///   a server with no prompts, say - is left out.
    /// </summary>
    Notifications: TSubscriptionFilter;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   How a subscription ended, which is what decides whether the client is
  ///   owed the "subscriptions/listen" result.
  /// </summary>
  /// <remarks>
  ///   The specification distinguishes the two: a subscription the server tears
  ///   down itself SHOULD be answered with the result before the stream closes,
  ///   while a transport that simply drops carries no response at all and the
  ///   client may read it as a reason to reconnect. Sending the result for both
  ///   would tell that client a lie.
  /// </remarks>
  TMCPSubscriptionState = (
    /// <summary>
    ///   Open: the pump is holding the stream for it.
    /// </summary>
    Live,

    /// <summary>
    ///   Closed by the server, or by a request to close it: the result is owed.
    /// </summary>
    ClosedGracefully,

    /// <summary>
    ///   Closed by a disconnect, by EOF, or by shutdown: no result.
    /// </summary>
    ClosedAbruptly);

  /// <summary>
  ///   One live "subscriptions/listen" stream, as the registry sees it: the
  ///   identifier the client correlates on, the filter this server agreed to
  ///   honour, and the queue its stream is drained from.
  /// </summary>
  /// <remarks>
  ///   The object outlives no request: the transport opens it, pumps it, closes
  ///   it in the registry and frees it. It owns its queue, so the subscription
  ///   and the channel it delivers on live and die together - and the registry
  ///   stops reaching the queue, under its lock, the moment it closes the
  ///   subscription, which is what makes freeing both afterwards safe.
  /// </remarks>
  TMCPSubscription = class
  private
    /// <summary>
    ///   The key the registry filed it under: scope and id.
    /// </summary>
    FKey: string;
    FId: TJSONValue;
    FFilter: TSubscriptionFilter;
    FQueue: TMCPMessageQueue;
    FState: TMCPSubscriptionState;
    FOnClose: TProc<TMCPSubscription>;
    function GetState: TMCPSubscriptionState;
  public
    /// <summary>
    ///   Takes ownership of AId and AFilter, and makes the queue the stream is
    ///   drained from, owned here too.
    /// </summary>
    /// <remarks>
    ///   The queue is made here rather than handed in because a caller that made
    ///   one and then failed to open a subscription would have nowhere to put
    ///   it. AQueueCap is how many notifications it holds before closing itself;
    ///   see Queue.CloseOnDrop.
    /// </remarks>
    constructor Create(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueueCap: Integer);
    destructor Destroy; override;

    /// <summary>
    ///   The subscription id, as it arrived: "the JSON-RPC ID of the
    ///   subscriptions/listen request", keeping its JSON type. Owned here.
    /// </summary>
    function Id: TJSONValue;

    /// <summary>
    ///   The subset of the requested notification types this server agreed to
    ///   send. Owned here, and the only thing a publisher may filter against.
    /// </summary>
    function Filter: TSubscriptionFilter;

    /// <summary>
    ///   Whether this subscription asked for "notifications/&lt;AMethod&gt;":
    ///   the opt-in rule, in the one place that can enforce it.
    /// </summary>
    function Wants(const AMethod: string): Boolean; overload;

    /// <summary>
    ///   Whether this subscription asked for an update about AUri. Exact match:
    ///   see TSubscriptionFilter.WantsResource.
    /// </summary>
    function Wants(const AMethod, AUri: string): Boolean; overload;

    /// <summary>
    ///   The queue this subscription's stream is drained from. Owned here.
    /// </summary>
    property Queue: TMCPMessageQueue read FQueue;

    /// <summary>
    ///   How the subscription ended, or Live.
    /// </summary>
    property State: TMCPSubscriptionState read GetState;

    /// <summary>
    ///   Called once, by the registry, when the subscription closes.
    /// </summary>
    property OnClose: TProc<TMCPSubscription> read FOnClose write FOnClose;
  end;

  /// <summary>
  ///   Whether the transport serving a request could hold a
  ///   "subscriptions/listen" stream open. Put in the request context by the
  ///   transport, for server/discover to read before it promises
  ///   "listChanged".
  /// </summary>
  /// <remarks>
  ///   A fact about the deployment, not about the client: whether this
  ///   request's own Accept asked for a stream does not matter, since the
  ///   listen the promise is about is a request of its own. Absent - a direct
  ///   call, a transport that says nothing - reads as False.
  /// </remarks>
  TMCPListenSupport = class
  private
    FAvailable: Boolean;
  public
    constructor Create(AAvailable: Boolean);

    /// <summary>
    ///   True when a listen arriving on this transport would be held open.
    /// </summary>
    property Available: Boolean read FAvailable;
  end;

  /// <summary>
  ///   The meeting point of a "subscriptions/listen" request and the transport
  ///   that will hold its stream open.
  /// </summary>
  /// <remarks>
  ///   The transport puts one in the request context only when it can actually
  ///   stream the reply. The listen endpoint, finding it, opens the
  ///   subscription and leaves it here for the transport to pump; not finding
  ///   it - a direct call, a transport with no stream - it acknowledges and
  ///   opens nothing, which is what keeps the registry from holding a
  ///   subscription nobody will ever drain.
  ///
  ///   Scope is what keeps two streams apart in the registry when their
  ///   clients chose the same id: a JSON-RPC id is unique only within one
  ///   client, and with no sessions there is nothing else to tell them apart.
  /// </remarks>
  TMCPListenStream = class
  private
    FScope: string;
  public
    /// <summary>
    ///   The subscription the listen endpoint opened, or nil. The transport
    ///   frees it once it has closed it.
    /// </summary>
    Subscription: TMCPSubscription;

    constructor Create(const AScope: string);

    /// <summary>
    ///   What sets this stream apart from every other one: see the remarks.
    /// </summary>
    property Scope: string read FScope;
  end;

  /// <summary>
  ///   The server's live "subscriptions/listen" streams, keyed by the id of the
  ///   listen request that opened each one.
  /// </summary>
  /// <remarks>
  ///   One instance per server, reachable from TMCPSubscriptionsConfig.
  ///
  ///   The registry is where a notification raised by anything at all - a tool,
  ///   a file watcher, a database trigger - becomes per-subscription messages:
  ///   Publish runs under a lock, clones the notification once per subscriber
  ///   that opted in, stamps the subscription id into its "_meta", and enqueues.
  ///   It never blocks on I/O and never touches the transport.
  ///
  ///   Ownership: the registry holds the TMCPSubscription while it is live, and
  ///   the subscription owns its queue. A subscription must not be reachable
  ///   from here once the request that opened it has ended, which is what the
  ///   transport's pump guarantees by closing it in a finally before freeing it.
  /// </remarks>
  TMCPSubscriptionRegistry = class
  private
    FSubscriptions: TDictionary<string, TMCPSubscription>;
    FLock: TObject;
    FMaxCount: Integer;

    /// <summary>
    ///   The key a subscription is filed under. The id keeps its JSON type on
    ///   the wire, but the two types cannot collide as keys: a JSON-RPC id is
    ///   either a string or a number, never both.
    /// </summary>
    class function KeyOf(AId: TJSONValue): string; overload; static;

    /// <summary>
    ///   The key for AId within AScope. An empty scope is the id's own key, so
    ///   a single-client transport (stdio) and the scope-less overloads agree.
    /// </summary>
    class function KeyOf(const AScope: string; AId: TJSONValue): string; overload; static;

    /// <summary>
    ///   Marks ASubscription closed and closes its queue, which is what wakes a
    ///   pump blocked on it. Under the lock; answers the hook to call outside it.
    /// </summary>
    class function MarkClosed(ASubscription: TMCPSubscription;
      AGraceful: Boolean): TProc<TMCPSubscription>; static;

    /// <summary>
    ///   Stamps the subscription id into a notification's "_meta", keeping the
    ///   id's JSON type. Does nothing when the params are absent or are not an
    ///   object - a notification this server builds always carries an object.
    /// </summary>
    class procedure Stamp(AId: TJSONValue; AParams: TJSONValue); static;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Registers a stream for AId and answers the subscription that describes
    ///   it, already carrying the acknowledged filter.
    /// </summary>
    /// <remarks>
    ///   AId is <b>copied</b>, not adopted: the id belongs to the request it
    ///   came from, which frees it with everything else, and a subscription that
    ///   outlived that would be reading freed JSON to stamp its messages. AFilter
    ///   is taken over, being a value the caller has no further use for.
    ///
    ///   Several concurrent subscriptions per server are expected - the
    ///   specification says so, and the id is what demultiplexes them - so
    ///   opening a second one is not an error. Opening one under an id already
    ///   in use is: it would make two streams answer to one identifier, which
    ///   is the one thing the client cannot untangle.
    /// </remarks>
    function Open(AId: TJSONValue; AFilter: TSubscriptionFilter;
      AQueueCap: Integer = MCP_SUBSCRIPTION_QUEUE_CAP): TMCPSubscription; overload;

    /// <summary>
    ///   Open, with the id filed under AScope: two streams whose clients both
    ///   chose the same id are then two subscriptions, not a refusal. Refuses
    ///   with EMCPListenNotStreamableError once MaxCount streams are open.
    /// </summary>
    function Open(const AScope: string; AId: TJSONValue; AFilter: TSubscriptionFilter;
      AQueueCap: Integer = MCP_SUBSCRIPTION_QUEUE_CAP): TMCPSubscription; overload;

    /// <summary>
    ///   The live subscription filed under AId, or nil.
    /// </summary>
    function Find(AId: TJSONValue): TMCPSubscription; overload;

    /// <summary>
    ///   Closes one subscription, marking it with the state, closing its queue
    ///   and calling its OnClose hook. A no-op for an id that is not
    ///   registered, so a close racing a disconnect is not an error. Answers
    ///   whether it found one.
    /// </summary>
    function Close(AId: TJSONValue; AGraceful: Boolean): Boolean; overload;

    /// <summary>
    ///   Closes ASubscription itself, rather than whatever is filed under its
    ///   id: the close a pump makes of its own stream, which must not reach a
    ///   newer subscription that happens to share the key. A no-op when it is
    ///   already closed.
    /// </summary>
    function Close(ASubscription: TMCPSubscription; AGraceful: Boolean): Boolean; overload;

    /// <summary>
    ///   Closes every live subscription with the same state - shutdown, or a
    ///   transport ending, where one result is owed to each stream. Answers how
    ///   many were closed.
    /// </summary>
    /// <remarks>
    ///   Each queue is closed with its subscription, so every pump wakes at
    ///   once rather than at its next keep-alive tick.
    /// </remarks>
    function CloseAll(AGraceful: Boolean): Integer;

    /// <summary>
    ///   Closes every live subscription filed under AScope - the streams of one
    ///   connection, when the transport owns its connections and gives each a
    ///   scope (stdio). Answers how many were closed. A no-op for an empty
    ///   scope, which is not a connection but the scope-less key.
    /// </summary>
    function CloseScope(const AScope: string; AGraceful: Boolean): Integer;

    /// <summary>
    ///   Sends ANotification to every subscription that opted in to it, each
    ///   with its own copy and its own subscription id.
    /// </summary>
    /// <remarks>
    ///   ANotification is a template and is left untouched: the caller owns it
    ///   and may publish it again. Under the lock, each subscriber gets
    ///   TJRPCNotification.Clone, because a TMCPMessageQueue owns the messages
    ///   it holds and frees them both when they are drained and when they are
    ///   dropped - a shared instance would be freed twice.
    ///
    ///   Answers how many subscriptions were sent to, which is what the tests
    ///   and the log read.
    /// </remarks>
    function Publish(ANotification: TJRPCNotification): Integer;

    /// <summary>
    ///   How many subscriptions are live.
    /// </summary>
    function Count: Integer;

    /// <summary>
    ///   Whether MaxCount streams are already open, so that another would be
    ///   refused. Always False when there is no cap.
    /// </summary>
    function IsFull: Boolean;

    /// <summary>
    ///   How many subscriptions Open admits at once; 0 for no cap. Enforced
    ///   under the lock, so concurrent listens cannot overshoot it.
    /// </summary>
    property MaxCount: Integer read FMaxCount write FMaxCount;
  end;

implementation

uses
  Logify,
  MCPConnect.MCP.Types.Errors;

{ TSubscriptionFilter }

function TSubscriptionFilter.WantsToolsListChanged: Boolean;
begin
  Result := ToolsListChanged.HasValue and ToolsListChanged.Value;
end;

function TSubscriptionFilter.WantsPromptsListChanged: Boolean;
begin
  Result := PromptsListChanged.HasValue and PromptsListChanged.Value;
end;

function TSubscriptionFilter.WantsResourcesListChanged: Boolean;
begin
  Result := ResourcesListChanged.HasValue and ResourcesListChanged.Value;
end;

function TSubscriptionFilter.WantsResource(const AUri: string): Boolean;
var
  LUri: string;
begin
  // Resource uris are compared as the identifiers they are: case-sensitively
  for LUri in ResourceSubscriptions do
    if LUri = AUri then
      Exit(True);

  Result := False;
end;

function TSubscriptionFilter.IsEmpty: Boolean;
begin
  Result := not WantsToolsListChanged and
            not WantsPromptsListChanged and
            not WantsResourcesListChanged and
            (Length(ResourceSubscriptions) = 0);
end;

procedure TSubscriptionFilter.AssignTo(ATarget: TSubscriptionFilter);
begin
  ATarget.ToolsListChanged := ToolsListChanged;
  ATarget.PromptsListChanged := PromptsListChanged;
  ATarget.ResourcesListChanged := ResourcesListChanged;
  ATarget.ResourceSubscriptions := Copy(ResourceSubscriptions);
end;

{ TSubscriptionsListenRequestParams }

constructor TSubscriptionsListenRequestParams.Create;
begin
  inherited;
  Notifications := TSubscriptionFilter.Create;
end;

destructor TSubscriptionsListenRequestParams.Destroy;
begin
  Notifications.Free;
  inherited;
end;

{ TSubscriptionsListenResultMeta }

destructor TSubscriptionsListenResultMeta.Destroy;
begin
  SubscriptionId.Free;
  inherited;
end;

procedure TSubscriptionsListenResultMeta.SetSubscriptionId(const AId: string);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONString.Create(AId);
end;

procedure TSubscriptionsListenResultMeta.SetSubscriptionId(AId: Int64);
begin
  SubscriptionId.Free;
  SubscriptionId := TJSONNumber.Create(AId);
end;

{ TSubscriptionsListenResult }

constructor TSubscriptionsListenResult.Create;
begin
  inherited;

  // The result "_meta" has to carry the stream id, which the plain
  // TResultMetaObject built by TBaseResult has no room for
  ResultMeta.Free;
  ResultMeta := TSubscriptionsListenResultMeta.Create;
end;

function TSubscriptionsListenResult.Meta: TSubscriptionsListenResultMeta;
begin
  Result := ResultMeta as TSubscriptionsListenResultMeta;
end;

{ TSubscriptionsAcknowledgedNotificationParams }

constructor TSubscriptionsAcknowledgedNotificationParams.Create;
begin
  inherited;
  Notifications := TSubscriptionFilter.Create;
end;

destructor TSubscriptionsAcknowledgedNotificationParams.Destroy;
begin
  Notifications.Free;
  inherited;
end;

{ TMCPSubscription }

constructor TMCPSubscription.Create(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueueCap: Integer);
begin
  inherited Create;
  FId := AId;
  FFilter := AFilter;
  FState := TMCPSubscriptionState.Live;

  FQueue := TMCPMessageQueue.Create(AQueueCap);

  // A subscription's queue is the one kind that must not drop quietly: every
  // message on it is an invalidation, so losing one leaves a client with a cache
  // it will never invalidate. Closing at the cap is what lets the pump end the
  // stream instead - see TMCPSubscriptionRegistry.Publish.
  FQueue.CloseOnDrop := True;
end;

destructor TMCPSubscription.Destroy;
begin
  // Frees whatever is still queued with it: a notification nobody drained is
  // one nobody will
  FQueue.Free;
  FId.Free;
  FFilter.Free;
  inherited;
end;

function TMCPSubscription.GetState: TMCPSubscriptionState;
begin
  Result := FState;
end;

function TMCPSubscription.Id: TJSONValue;
begin
  Result := FId;
end;

function TMCPSubscription.Filter: TSubscriptionFilter;
begin
  Result := FFilter;
end;

function TMCPSubscription.Wants(const AMethod: string): Boolean;
begin
  // The specification's MUST NOT, taken literally: a notification type the
  // client did not opt into is never sent, whatever the publisher meant.
  if AMethod = MCP_NOTIFY_TOOLS_LIST_CHANGED then
    Exit(FFilter.WantsToolsListChanged);

  if AMethod = MCP_NOTIFY_PROMPTS_LIST_CHANGED then
    Exit(FFilter.WantsPromptsListChanged);

  if AMethod = MCP_NOTIFY_RESOURCES_LIST_CHANGED then
    Exit(FFilter.WantsResourcesListChanged);

  // Nothing else travels on a listen stream: progress and message are
  // request-scoped, and the acknowledged notification is the server's own
  // opening message rather than something published.
  Result := False;
end;

function TMCPSubscription.Wants(const AMethod, AUri: string): Boolean;
begin
  Result := SameText(AMethod, MCP_NOTIFY_RESOURCES_UPDATED) and
    FFilter.WantsResource(AUri);
end;

{ TMCPListenSupport }

constructor TMCPListenSupport.Create(AAvailable: Boolean);
begin
  inherited Create;
  FAvailable := AAvailable;
end;

{ TMCPListenStream }

constructor TMCPListenStream.Create(const AScope: string);
begin
  inherited Create;
  FScope := AScope;
  Subscription := nil;
end;

{ TMCPSubscriptionRegistry }

constructor TMCPSubscriptionRegistry.Create;
begin
  inherited Create;
  FSubscriptions := TDictionary<string, TMCPSubscription>.Create;
  FLock := TObject.Create;
  FMaxCount := 0;
end;

destructor TMCPSubscriptionRegistry.Destroy;
begin
  // Server shutdown: no stream is left to answer, and no hook can run any more.
  // The subscriptions are freed, and their queues with them.
  TMonitor.Enter(FLock);
  try
    for var LSubscription in FSubscriptions.Values do
    begin
      LSubscription.OnClose := nil;
      LSubscription.Free;
    end;
    FSubscriptions.Clear;
  finally
    TMonitor.Exit(FLock);
  end;

  FreeAndNil(FSubscriptions);
  FLock.Free;
  inherited;
end;

class function TMCPSubscriptionRegistry.KeyOf(AId: TJSONValue): string;
begin
  // A JSON-RPC id is a string or a number and never both, so the JSON text of
  // the id is a faithful key: 7 and "7" are different subscriptions, which is
  // exactly what the specification asks for when it says every message carries
  // "the JSON-RPC ID of the subscriptions/listen request".
  if Assigned(AId) then
    Result := AId.ToJSON
  else
    Result := '';
end;

class function TMCPSubscriptionRegistry.KeyOf(const AScope: string; AId: TJSONValue): string;
begin
  // The id's JSON text never starts with a space, so the scope cannot run into
  // it: "a 1" and "a1" stay apart whatever the scope is.
  if AScope.IsEmpty then
    Result := KeyOf(AId)
  else
    Result := AScope + ' ' + KeyOf(AId);
end;

class function TMCPSubscriptionRegistry.MarkClosed(ASubscription: TMCPSubscription; AGraceful: Boolean): TProc<TMCPSubscription>;
begin
  if AGraceful then
    ASubscription.FState := TMCPSubscriptionState.ClosedGracefully
  else
    ASubscription.FState := TMCPSubscriptionState.ClosedAbruptly;

  // Lock order is registry, then queue: this runs under the registry lock and
  // takes the queue's, which is the order Publish takes them in too. Closing is
  // what wakes the pump blocked in DequeueWait, which would otherwise notice
  // only at its next keep-alive tick.
  ASubscription.Queue.Close;

  // Read here, under the lock, and called by the caller outside it: once the
  // lock is released the pump is free to free the subscription, so nothing may
  // read its fields after that - the hook included.
  Result := ASubscription.OnClose;
end;

class procedure TMCPSubscriptionRegistry.Stamp(AId: TJSONValue; AParams: TJSONValue);
var
  LParams, LMeta: TJSONObject;
begin
  if not (AParams is TJSONObject) then
    Exit;

  LParams := TJSONObject(AParams);

  // The "_meta" member is absent from a notification built for no particular
  // subscription: TNotificationMetaObject carries no members until an id is
  // stamped into it, and Neon leaves an empty object out of the JSON. So this
  // creates it rather than patching one that is already there - and it can be
  // created unconditionally, because the copy belongs to one subscriber and
  // nothing else will add to it.
  LMeta := LParams.GetValue('_meta') as TJSONObject;
  if not Assigned(LMeta) then
  begin
    LMeta := TJSONObject.Create;
    LParams.AddPair('_meta', LMeta);
  end;

  // AddPair would leave a previous key in place, and a subscription id is not
  // something to carry twice.
  if Assigned(LMeta.GetValue(MCP_META_SUBSCRIPTION_ID)) then
    LMeta.RemovePair(MCP_META_SUBSCRIPTION_ID);

  LMeta.AddPair(MCP_META_SUBSCRIPTION_ID, AId.Clone as TJSONValue);
end;

function TMCPSubscriptionRegistry.Open(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueueCap: Integer): TMCPSubscription;
begin
  Result := Open('', AId, AFilter, AQueueCap);
end;

function TMCPSubscriptionRegistry.Open(const AScope: string; AId: TJSONValue;
  AFilter: TSubscriptionFilter; AQueueCap: Integer): TMCPSubscription;
var
  LKey: string;
begin
  LKey := KeyOf(AScope, AId);

  TMonitor.Enter(FLock);
  try
    // Both refusals are checked before the subscription is built, so that they
    // do not take AFilter down with them: the caller still owns what it passed.
    if FSubscriptions.ContainsKey(LKey) then
      raise EMCPException.CreateFmt(SSubscriptionAlreadyOpenFmt, [KeyOf(AId)]);

    // Under the lock, so that two listens arriving together cannot both see
    // room for one more
    if (FMaxCount > 0) and (FSubscriptions.Count >= FMaxCount) then
      raise EMCPListenNotStreamableError.CreateFor(TMCPListenRefusal.TooMany, FMaxCount);

    Result := TMCPSubscription.Create(AId.Clone as TJSONValue, AFilter, AQueueCap);
    Result.FKey := LKey;
    FSubscriptions.Add(LKey, Result);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPSubscriptionRegistry.Find(AId: TJSONValue): TMCPSubscription;
begin
  TMonitor.Enter(FLock);
  try
    if not FSubscriptions.TryGetValue(KeyOf(AId), Result) then
      Result := nil;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPSubscriptionRegistry.Close(AId: TJSONValue; AGraceful: Boolean): Boolean;
var
  LKey: string;
  LSubscription: TMCPSubscription;
  LHook: TProc<TMCPSubscription>;
begin
  Result := False;
  LKey := KeyOf(AId);

  // The hook runs outside the lock: it belongs to the caller, and a hook that
  // reached back into the registry under the lock would deadlock on a
  // non-reentrant monitor from another thread.
  TMonitor.Enter(FLock);
  try
    if not FSubscriptions.TryGetValue(LKey, LSubscription) then
      Exit;

    FSubscriptions.Remove(LKey);
    LHook := MarkClosed(LSubscription, AGraceful);

    Result := True;
  finally
    TMonitor.Exit(FLock);
  end;

  if Assigned(LHook) then
    LHook(LSubscription);
end;

function TMCPSubscriptionRegistry.Close(ASubscription: TMCPSubscription; AGraceful: Boolean): Boolean;
var
  LFiled: TMCPSubscription;
  LHook: TProc<TMCPSubscription>;
begin
  Result := False;
  if not Assigned(ASubscription) then
    Exit;

  TMonitor.Enter(FLock);
  try
    // By identity: what is filed under the key may be a newer subscription that
    // reused it after this one was closed by someone else
    if not FSubscriptions.TryGetValue(ASubscription.FKey, LFiled) or (LFiled <> ASubscription) then
      Exit;

    FSubscriptions.Remove(ASubscription.FKey);
    LHook := MarkClosed(ASubscription, AGraceful);

    Result := True;
  finally
    TMonitor.Exit(FLock);
  end;

  if Assigned(LHook) then
    LHook(ASubscription);
end;

function TMCPSubscriptionRegistry.CloseAll(AGraceful: Boolean): Integer;
var
  LClosing: TArray<TMCPSubscription>;
  LHooks: TArray<TProc<TMCPSubscription>>;
begin
  Result := 0;

  TMonitor.Enter(FLock);
  try
    SetLength(LClosing, FSubscriptions.Count);
    SetLength(LHooks, FSubscriptions.Count);

    for var LSubscription in FSubscriptions.Values do
    begin
      LHooks[Result] := MarkClosed(LSubscription, AGraceful);
      LClosing[Result] := LSubscription;
      Inc(Result);
    end;

    FSubscriptions.Clear;
  finally
    TMonitor.Exit(FLock);
  end;

  for var I := 0 to Result - 1 do
    if Assigned(LHooks[I]) then
      LHooks[I](LClosing[I]);

  // The subscriptions stay alive: the pump that owns each one reads its State
  // to decide whether the client is owed the listen result, and unregisters it
  // when it is done. The registry has forgotten it, which is all shutdown needs.
end;

function TMCPSubscriptionRegistry.CloseScope(const AScope: string; AGraceful: Boolean): Integer;
var
  LPrefix: string;
  LKeys: TArray<string>;
  LClosing: TArray<TMCPSubscription>;
  LHooks: TArray<TProc<TMCPSubscription>>;
begin
  Result := 0;
  if AScope.IsEmpty then
    Exit;

  // The key KeyOf builds for this scope starts with it and a space, and no
  // other scope's key can: see KeyOf
  LPrefix := AScope + ' ';

  TMonitor.Enter(FLock);
  try
    LKeys := FSubscriptions.Keys.ToArray;
    SetLength(LClosing, Length(LKeys));
    SetLength(LHooks, Length(LKeys));

    for var LKey in LKeys do
      if LKey.StartsWith(LPrefix) then
      begin
        LClosing[Result] := FSubscriptions[LKey];
        LHooks[Result] := MarkClosed(LClosing[Result], AGraceful);
        FSubscriptions.Remove(LKey);
        Inc(Result);
      end;
  finally
    TMonitor.Exit(FLock);
  end;

  // Outside the lock, as in CloseAll: once it is released the pump may free the
  // subscription, and a hook must not run under a lock it could need
  for var I := 0 to Result - 1 do
    if Assigned(LHooks[I]) then
      LHooks[I](LClosing[I]);
end;

function TMCPSubscriptionRegistry.Publish(ANotification: TJRPCNotification): Integer;
var
  LCopy: TJRPCNotification;
  LUri: string;
  LMatches, LOverflowed: Boolean;
  LOverflowedIds: TArray<string>;
begin
  Result := 0;
  LOverflowedIds := [];

  if not Assigned(ANotification) then
    Exit;

  // Read before the lock: the notification is the publisher's, and holding the
  // registry lock while walking JSON is work nobody else has to wait for.
  // "uri" is only ever present on a resources/updated notification, so an
  // absent member is the ordinary case rather than an error.
  LUri := '';
  var LParams := ANotification.Params as TJSONObject;
  if Assigned(LParams) then
  begin
    var LUriValue := LParams.GetValue('uri');
    if Assigned(LUriValue) and (LUriValue is TJSONString) then
      LUri := TJSONString(LUriValue).Value;
  end;

  TMonitor.Enter(FLock);
  try
    for var LSubscription in FSubscriptions.Values do
    begin
      if LSubscription.State <> TMCPSubscriptionState.Live then
        Continue;

      if SameText(ANotification.Method, MCP_NOTIFY_RESOURCES_UPDATED) then
        LMatches := LSubscription.Wants(ANotification.Method, LUri)
      else
        LMatches := LSubscription.Wants(ANotification.Method);

      if not LMatches then
        Continue;

      // Its own copy: a TMCPMessageQueue frees what it holds, both when a
      // message is drained and when one is dropped at the cap, so a shared
      // instance would be freed twice - once per subscriber.
      LCopy := ANotification.Clone;
      Stamp(LSubscription.Id, LCopy.Params);

      // A queue at its cap drops its oldest message and closes itself, which is
      // the pump's cue to end the subscription: a client that stopped reading
      // has lost a change notification it will never ask about again, and a
      // reconnect is the only honest answer to that. Noticed here because the
      // pump cannot tell an overflow from an ordinary end.
      LOverflowed := not LSubscription.Queue.Closed;
      LSubscription.Queue.Enqueue(LCopy);
      Inc(Result);

      if LOverflowed and LSubscription.Queue.Closed then
        LOverflowedIds := LOverflowedIds + [KeyOf(LSubscription.Id)];
    end;
  finally
    TMonitor.Exit(FLock);
  end;

  // Outside the lock: the log is nobody's critical section, and the pump may
  // close the subscription while a message is being written about it.
  for var LId in LOverflowedIds do
    Logger.LogWarning(SSubscriptionOverflowFmt, [LId]);
end;

function TMCPSubscriptionRegistry.Count: Integer;
begin
  TMonitor.Enter(FLock);
  try
    Result := FSubscriptions.Count;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPSubscriptionRegistry.IsFull: Boolean;
begin
  TMonitor.Enter(FLock);
  try
    Result := (FMaxCount > 0) and (FSubscriptions.Count >= FMaxCount);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
