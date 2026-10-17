# Notifications, Subscriptions and Cancellation

MCP 2026-07-28 has no sessions and no server-initiated stream. A server can talk to a client
only inside a request the client opened. This page covers the three ways that happens in
MCPConnect:

- **Request-scoped notifications**: progress and log messages a tool sends while it runs,
  inside the reply to the request that called it.
- **Subscriptions**: a `subscriptions/listen` request the client keeps open, on which the
  server announces that its tools, prompts, resources, or one particular resource changed.
- **Cancellation**: the client telling the server it no longer wants the answer.

::: warning Provisional
This page documents work on the `feature/mcp-2026-07-28` branch. Names and signatures may still
change before release.
:::

## Request-scoped notifications

A tool reports progress through `[Context] FProgress: TMCPProgress` and logs through
`[Context] FLog: TMCPLog`. Both are always in the context, and both send nothing unless the
request asked for it: a progress token, or a `logLevel` in its `_meta`.

```pascal
for I := 1 to LCount do
begin
  DoStep(I);
  FProgress.Report(I, LCount, Format('Step %d', [I]));
end;
FLog.Log(TMCPLogLevel.Info, 'Import finished');
```

Over Streamable HTTP these travel as SSE events in the body of the `POST` that triggered them,
and only when that request's `Accept` includes `text/event-stream`. A client that asked for a
JSON document gets the reply and nothing else. On stdio they are JSON lines on stdout.

These notifications belong to their request. They **never** travel on a subscription stream.

## Subscriptions

### The lifecycle

```
client                                server
  | -- subscriptions/listen (id 7) -->  |
  | <-- notifications/subscriptions/acknowledged   (first, always)
  | <-- notifications/tools/list_changed
  | <-- notifications/resources/updated  { uri }
  |          ...                         |
  | <-- result of subscriptions/listen  (only on a graceful end)
  |  (stream closes)                     |
```

1. **Open.** The client sends `subscriptions/listen` with the notifications it wants:
   `toolsListChanged`, `promptsListChanged`, `resourcesListChanged` and a list of
   `resourceSubscriptions` (uris).
2. **Acknowledged.** The first message on the stream is
   `notifications/subscriptions/acknowledged`. It carries what the server *agreed* to send,
   which can be less than what was asked for. A kind the server has nothing of is left out. So is
   a uri the server does not know or the caller may not read.
3. **Notifications.** Each change the client opted into arrives as its own message. Every
   message carries `_meta["io.modelcontextprotocol/subscriptionId"]`: the id of the listen
   request, with its JSON type kept (`7` and `"7"` are different subscriptions).
4. **End.**
   - When the server ends the subscription gracefully (shutdown, an expired token), it sends the
     listen **result** as the last message. A client reads that as "closed cleanly".
   - When it ends abruptly (a client that hung up, an overflow, a cancellation), it sends no
     result. A client reads that as "reconnect and re-read". An overflow ends abruptly on purpose:
     the client has missed a change it will never ask about.

### Publishing changes

Every change goes through `IMCPConfig.Subscriptions`. It is callable from any thread, so a file
watcher or a database trigger can use it as well as a tool.

| Call | Sends | To subscriptions that asked for |
|---|---|---|
| `ToolsChanged` | `notifications/tools/list_changed` | `toolsListChanged` |
| `PromptsChanged` | `notifications/prompts/list_changed` | `promptsListChanged` |
| `ResourcesChanged` | `notifications/resources/list_changed` | `resourcesListChanged` |
| `ResourceUpdated(AUri)` | `notifications/resources/updated` | that exact uri |

```pascal
type
  TTodoTools = class
  private
    [Context] FConfig: TMCPConfig;
  public
    [McpTool('add_task', 'Add a task')]
    function AddTask(const ATitle: string): string;
  end;

function TTodoTools.AddTask(const ATitle: string): string;
begin
  TodoStore.Add(ATitle);
  // Whoever is watching the summary resource re-reads it
  FConfig.Subscriptions.ResourceUpdated('text://todo/summary');
  Result := 'Added';
end;
```

Publishing to nobody costs nothing. Publishing never blocks: it only queues.

`ResourceUpdated` matches the uri **exactly**. A subscription to `file:///project/` does not
receive `file:///project/readme.md`. A naive prefix match would also match
`file:///project-secrets/`.

**The list notifications are automatic.** Runtime `Register*`, `Unregister*` and `ClearAll` on
the Tools, Prompts and Resources sections publish their own `list_changed`. Registering a class
with twelve tools sends one notification, not twelve. To make several changes count as one,
wrap them in a batch:

```pascal
LConfig.Subscriptions.Batch(
  procedure
  begin
    LConfig.Tools.UnregisterClass(TOldTools);
    LConfig.Tools.RegisterClass(TNewTools);
  end);
```

Registering and unregistering at runtime are still **startup or single-owner operations**: the
registries are not synchronized against a concurrent `tools/list`.

### Advertising it

`server/discover` reports `"listChanged": true` for each of tools, resources and prompts the
server has, but only where the promise can be kept: when the transport serving the request can
hold a listen stream open. On a transport that cannot, the flag is `false`.

- `Server.SetListChanged(False)` never promises it. Use it for a server whose lists do not
  change once it is running.
- `Server.SetCapabilities(...)` is reported **verbatim**, and `SetListChanged` does not touch it.
  A server that sets its own capabilities and sends list changes must set
  `Tools.ListChanged`, `Resources.ListChanged` and `Prompts.ListChanged` itself.
- The `resources.subscribe` capability is **not** set, and that is correct. 2026-07-28 replaced
  `resources/subscribe` with `resourceSubscriptions` on the listen request. Do not "fix" it.

### Interaction with cache hints

`SetCacheHints` defaults to `ttlMs: 0` / `private`: every list is stale the moment it arrives,
so a `list_changed` notification adds nothing. Once a server sets a real TTL, the notification
becomes the invalidation signal. A client may then keep `tools/list` for the TTL and re-fetch
early when told to. That only works for clients that opened a stream. A client that did not
open one relies on the TTL alone, so do not set one longer than you are willing to serve stale.

### Configuration

All on `IMCPConfig.Subscriptions`:

| Setting | Default | Meaning |
|---|---|---|
| `MaxSubscriptions` | 64 (`MCP_SUBSCRIPTION_MAX`) | Streams open at once, **for the whole deployment**; 0 for no cap |
| `KeepAliveInterval` | 2000 ms (`MCP_SUBSCRIPTION_KEEPALIVE_MS`) | Silence before an SSE keep-alive comment |
| `QueueCap` | 64 (`MCP_SUBSCRIPTION_QUEUE_CAP`) | Messages a subscription may hold unread before it is closed |

With no sessions, nothing tells two clients apart, so `MaxSubscriptions` is a deployment-wide
budget and not a per-client one. On Indy each open stream holds one connection thread, and this
cap is what bounds them.

### Refusals

A listen that cannot be given a stream is refused **before anything is dispatched**, with a
status and a JSON-RPC error (`-32600`):

| Status | When |
|---|---|
| `406 Not Acceptable` | the request's `Accept` excludes `text/event-stream` (the client asked for a shape a listen cannot be answered in) |
| `400 Bad Request` | the transport cannot hold a response open (see below) |
| `400 Bad Request` | `MaxSubscriptions` streams are already open |

On stdio the writer always streams, so only the cap can refuse there. On one stdio connection,
a second listen under an id that is already listening is refused too. Two streams answering to
one id are the one thing a client could not untangle.

### Which transports can stream

| Transport | Listen |
|---|---|
| Indy (`TMCPIndyServer`) | Yes. One connection thread per open stream |
| WebBroker, Delphi 13.1+ (`HAS_WEBBROKER_SSE`) | Yes |
| WebBroker before 13.1 | No: `400`, and `listChanged` is `false` |
| ISAPI (13.1+) | Not recommended: each stream holds a pool worker thread, so a handful of listeners exhausts the pool |
| CGI (13.1+) | No in practice: one response per process invocation, so a held stream is a process that does not exit |
| stdio | Yes. Each request runs on a thread of its own, so a listen does not block the next request |

**The library cannot tell an ISAPI or CGI host from a standalone one.** The WebBroker writer
reports what the compiler supports (`HAS_WEBBROKER_SSE`), not how it is hosted. So on 13.1 such
a deployment accepts a listen, and `server/discover` promises `listChanged` there. If you deploy
that way, call `Server.SetListChanged(False)` so the server does not invite listeners, and set
`MaxSubscriptions` well below the pool size (ISAPI) or to 1 (CGI) to bound the damage from a
client that listens anyway.

### Authorization is checked once

A subscription's authorization is the one in force **when it opened**. The `resourceSubscriptions`
are filtered through the access guard at listen time. A publish later runs on whatever thread
raised the change, with no request to check against, so it cannot re-check. **Revoking access
therefore means closing the stream.** The notification carries the uri, so a stream left open
after a revocation would keep disclosing which resources change.

For OAuth, the pump does watch the token's expiry: when the token used to open the stream
expires, the subscription ends gracefully and the client has to reconnect with a fresh one.

## Cancellation

### In a tool

Inject `[Context] FCancel: TMCPCancellation` and check it between steps:

```pascal
for I := 1 to LCount do
begin
  FCancel.WaitFor(1000);     // instead of Sleep: wakes at once on a cancel
  FCancel.CheckCancelled;    // raises EMCPRequestCancelled once cancelled
  FProgress.Report(I, LCount);
end;
```

`CheckCancelled` raises `EMCPRequestCancelled`, and the transport answers that with **nothing**,
not even an error. Once a request is cancelled the server must not send anything more for it.
`IsCancelled`, `Reason` and an `OnCancel` hook complete the object. `OnCancel` runs under the
object's lock, so setting it to `nil` is safe, and a hook set after the cancel runs at once.

A tool that ignores the cancellation runs to the end, and the transport drops what it answers.

### How a request gets cancelled

- **Streamable HTTP: by closing the stream.** The transport asks the writer whether the client is
  still there between reads of the response queue, and a write that fails counts too. This
  applies only to a **streamed** reply. A request answered as a JSON document has no stream to
  watch, so it runs to the end.
- **stdio: by `notifications/cancelled`** naming the request id. The reader handles it on its own
  thread, at once, while the request runs on another. It reads the next line only after the
  previous request has been registered, so a cancel sent right after its request always finds it.
- **Closing stdin cancels nothing.** The client sends nothing more but may still be reading
  stdout (`echo request | server` is a client too), so a request still running finishes and
  answers. Only the connection's listen streams, which would never end on their own, are
  closed, abruptly. The server exits once everything has ended.

### `notifications/cancelled` over HTTP

It is accepted, `MessageHandling.OnCancelled` hears about it, and it cancels nothing. With no
sessions, a cancellation arriving on a `POST` of its own cannot be tied to the request it names,
and a listen occupies its connection for its whole life, so the notification cannot arrive on
the same one. Over HTTP, closing the stream is the way to cancel, which is what the
specification says anyway.

### Ending a subscription

- **The client**, over HTTP: closes the stream. The server notices at its next write (the
  keep-alive at the latest) and drops the subscription with no result.
- **The client**, over stdio: sends `notifications/cancelled` with the listen request's id.
  The subscription ends abruptly, with no result.
- **The server**: ends it gracefully, with the listen result as the last message. The server
  does not *also* send `notifications/cancelled`. That would be legal but redundant.

### The hook

```pascal
.MessageHandling
  .OnCancelled(
    procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
    begin
      Logger.LogInfo('Client cancelled %s: %s',
        [AParams.RequestId.ToJSON, AParams.Reason.GetValueOrDefault]);
    end)
```

It is called for every `notifications/cancelled`, whether or not the request is still running.
It is informational: the request itself is cancelled by the transport. A hook that raises is
logged and does not turn into an error, because a notification cannot be answered.

## Notes

- A `notifications/subscriptions/acknowledged` *received* from a client is accepted and
  discarded. The schema does not define one in that direction, and dropping it costs nothing.
- `Demo/MCPServer` publishes `ResourceUpdated('text://todo/summary')` whenever a task changes.
  `Demo/MCPServerMetrics` publishes `resource://metrics/live` on each harvest and on every tool
  that records. Its `ClientTest/MCPClient.ps1` opens a listen, triggers a change from another
  connection, and shows the notification arriving.
