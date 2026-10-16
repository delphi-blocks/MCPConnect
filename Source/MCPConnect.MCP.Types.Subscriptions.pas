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
    /// <summary>Receive "notifications/tools/list_changed".</summary>
    ToolsListChanged: NullBoolean;

    /// <summary>Receive "notifications/prompts/list_changed".</summary>
    PromptsListChanged: NullBoolean;

    /// <summary>Receive "notifications/resources/list_changed".</summary>
    ResourcesListChanged: NullBoolean;

    /// <summary>
    ///   Receive "notifications/resources/updated" for these resource uris.
    ///   Replaces the former "resources/subscribe" request.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] ResourceSubscriptions: TArray<string>;
  public
    /// <summary>True when the flag is present and set.</summary>
    function WantsToolsListChanged: Boolean;
    function WantsPromptsListChanged: Boolean;
    function WantsResourcesListChanged: Boolean;

    /// <summary>True when AUri was subscribed to, matched case-sensitively.</summary>
    function WantsResource(const AUri: string): Boolean;

    /// <summary>True when nothing at all was requested.</summary>
    function IsEmpty: Boolean;

    /// <summary>Copies the requested types onto ATarget.</summary>
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
    /// <summary>Open: the pump is holding the stream for it.</summary>
    Live,
    /// <summary>Closed by the server, or by a request to close it: the result is owed.</summary>
    ClosedGracefully,
    /// <summary>Closed by a disconnect, by EOF, or by shutdown: no result.</summary>
    ClosedAbruptly);

  /// <summary>
  ///   One live "subscriptions/listen" stream, as the registry sees it: the
  ///   identifier the client correlates on, the filter this server agreed to
  ///   honour, and the queue its stream is drained from.
  /// </summary>
  /// <remarks>
  ///   The object outlives no request: the transport opens it, pumps it, and
  ///   closes it before the request's own garbage collector runs. It therefore
  ///   holds the queue without owning it - see TMCPSubscriptionRegistry.
  /// </remarks>
  TMCPSubscription = class
  private
    FId: TJSONValue;
    FFilter: TSubscriptionFilter;
    FQueue: TMCPMessageQueue;
    FState: TMCPSubscriptionState;
    FOnClose: TProc<TMCPSubscription>;
    function GetState: TMCPSubscriptionState;
  public
    /// <summary>
    ///   Takes ownership of AId and AFilter, and holds AQueue without owning it.
    /// </summary>
    constructor Create(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueue: TMCPMessageQueue);
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
    ///   The queue this subscription's stream is drained from. Not owned here.
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
  ///   Ownership: the registry owns the TMCPSubscription, the request owns the
  ///   queue. A subscription must not be reachable from here once the request
  ///   that opened it has ended, which is what the transport's pump guarantees
  ///   by closing it in a finally.
  /// </remarks>
  TMCPSubscriptionRegistry = class
  private
    FSubscriptions: TDictionary<string, TMCPSubscription>;
    FLock: TObject;

    /// <summary>
    ///   The key a subscription is filed under. The id keeps its JSON type on
    ///   the wire, but the two types cannot collide as keys: a JSON-RPC id is
    ///   either a string or a number, never both.
    /// </summary>
    class function KeyOf(AId: TJSONValue): string; static;

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
      AQueue: TMCPMessageQueue): TMCPSubscription;

    /// <summary>
    ///   The live subscription filed under AId, or nil.
    /// </summary>
    function Find(AId: TJSONValue): TMCPSubscription; overload;

    /// <summary>
    ///   Closes one subscription, marking it with the state and calling its
    ///   OnClose hook. A no-op for an id that is not registered, so a close
    ///   racing a disconnect is not an error. Answers whether it found one.
    /// </summary>
    function Close(AId: TJSONValue; AGraceful: Boolean): Boolean; overload;

    /// <summary>
    ///   Closes every live subscription with the same state - shutdown, or a
    ///   transport ending, where one result is owed to each stream. Answers how
    ///   many were closed.
    /// </summary>
    function CloseAll(AGraceful: Boolean): Integer;

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
  end;

implementation

uses
  Logify;

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

constructor TMCPSubscription.Create(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueue: TMCPMessageQueue);
begin
  inherited Create;
  FId := AId;
  FFilter := AFilter;
  FQueue := AQueue;
  FState := TMCPSubscriptionState.Live;

  // A subscription's queue is the one kind that must not drop quietly: every
  // message on it is an invalidation, so losing one leaves a client with a cache
  // it will never invalidate. Closing at the cap is what lets the pump end the
  // stream instead - see TMCPSubscriptionRegistry.Publish.
  FQueue.CloseOnDrop := True;
end;

destructor TMCPSubscription.Destroy;
begin
  FId.Free;
  FFilter.Free;
  // FQueue is not ours: the request that opened the subscription owns it, and
  // the registry drops its reference when the pump closes the subscription.
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

{ TMCPSubscriptionRegistry }

constructor TMCPSubscriptionRegistry.Create;
begin
  inherited Create;
  FSubscriptions := TDictionary<string, TMCPSubscription>.Create;
  FLock := TObject.Create;
end;

destructor TMCPSubscriptionRegistry.Destroy;
begin
  // Server shutdown: no stream is left to answer, and no hook can run any more.
  // The subscriptions themselves are freed; the queues they point at belong to
  // requests that are being torn down with them.
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

function TMCPSubscriptionRegistry.Open(AId: TJSONValue; AFilter: TSubscriptionFilter; AQueue: TMCPMessageQueue): TMCPSubscription;
var
  LKey: string;
begin
  LKey := KeyOf(AId);

  TMonitor.Enter(FLock);
  try
    // Checked before the subscription is built, so that a duplicate id does not
    // take AFilter down with it: this is a refusal, and the caller still owns
    // what it passed in.
    if FSubscriptions.ContainsKey(LKey) then
      raise EMCPException.CreateFmt(SSubscriptionAlreadyOpenFmt, [LKey]);

    Result := TMCPSubscription.Create(AId.Clone as TJSONValue, AFilter, AQueue);
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
    if AGraceful then
      LSubscription.FState := TMCPSubscriptionState.ClosedGracefully
    else
      LSubscription.FState := TMCPSubscriptionState.ClosedAbruptly;

    Result := True;
  finally
    TMonitor.Exit(FLock);
  end;

  if Assigned(LSubscription.OnClose) then
    LSubscription.OnClose(LSubscription);
end;

function TMCPSubscriptionRegistry.CloseAll(AGraceful: Boolean): Integer;
var
  LClosing: TArray<TMCPSubscription>;
begin
  Result := 0;

  TMonitor.Enter(FLock);
  try
    SetLength(LClosing, FSubscriptions.Count);

    for var LSubscription in FSubscriptions.Values do
    begin
      if AGraceful then
        LSubscription.FState := TMCPSubscriptionState.ClosedGracefully
      else
        LSubscription.FState := TMCPSubscriptionState.ClosedAbruptly;

      LClosing[Result] := LSubscription;
      Inc(Result);
    end;

    FSubscriptions.Clear;
  finally
    TMonitor.Exit(FLock);
  end;

  for var LSubscription in LClosing do
    if Assigned(LSubscription.OnClose) then
      LSubscription.OnClose(LSubscription);

  // The subscriptions stay alive: the pump that owns each one reads its State
  // to decide whether the client is owed the listen result, and unregisters it
  // when it is done. The registry has forgotten it, which is all shutdown needs.
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

end.
