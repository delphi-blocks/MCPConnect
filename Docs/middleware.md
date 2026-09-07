# Middleware

Middleware lets you apply cross-cutting behaviour — logging, timing, authentication, authorization, error handling, per-caller filtering — to every message your server handles, without touching the classes that implement your tools, resources and prompts.

::: warning Provisional
This page documents a feature still under development on the `feature/mcp-2026-07-28` branch. Names and signatures may still change before release. The full design rationale lives in `Docs/middleware-spec.md`.
:::

## The Onion Model

A middleware wraps the rest of the chain. Whatever you write **before** the call to `AChain.Next` is pre-processing, whatever you write **after** it is post-processing, and not calling `Next` at all suppresses the operation:

```pascal
function TTimingMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
var
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;          // before: pre-processing
  try
    Result := AChain.Next(AContext, AParams); // the rest of the chain
  finally
    Logger.LogInfo('[%s] %d ms',              // after: post-processing
      [AParams.Name, LStopwatch.ElapsedMilliseconds]);
  end;
end;
```

There is no separate declaration of "pre" or "post" — the position in the code is the declaration. Because `Next` sits inside an ordinary `try/finally`, post-processing runs on the error path too.

## Writing a Middleware

A middleware is a class descending from `TMiddleware` that implements the interface of the chain it wants to take part in. The hook method is always called `Handle`:

```pascal
uses
  JRPC.Core,                     // EJRPCInvalidParamsError
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware;

type
  TToolAclMiddleware = class(TMiddleware, ICallToolMiddleware)
  public
    function Handle(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

function TToolAclMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  if not Allowed(AParams.Name) then
    raise EJRPCInvalidParamsError.Create('Tool not available');

  Result := AChain.Next(AContext, AParams);
end;
```

Declaring the interface **is** the registration for that chain: there is no list of hooks to fill in and nothing to override. A middleware that does not implement `ICallToolMiddleware` costs nothing on a `tools/call`, because the pipeline only builds the middleware belonging to the chain it is running.

## Registering Middleware

Middleware is registered on the server itself, in fluent form, and the chain is closed with `BackToApp`:

```pascal
AServer

  .Middleware
    .Add(TAuditMiddleware)
    .Add(TToolAclMiddleware)
  .BackToApp

  .Plugin.Configure<IMCPConfig>
    .Server
      .SetName('delphi-mcp-server')
      .SetVersion('1.0.0')
    .BackToMCP
    .Tools
      .RegisterClass(TMyTools)
    .BackToMCP
  ;
```

A **new instance is built for every message**, so a middleware needs no locking around its own fields. If your middleware needs constructor arguments, register it with a factory instead:

```pascal
.Add(TRateLimitMiddleware,
  function: IMiddleware
  begin
    Result := TRateLimitMiddleware.Create(100, 60);
  end)
```

### Execution Order and Priority

Middleware runs in registration order unless it declares a priority. Lower values run first, which means they sit further out: first to see the request, last to see the response.

| Constant | Value | Meaning |
|---|---|---|
| `MW_PRIORITY_ERROR_HANDLING` | 1000 | Wraps everything, so it can observe any exception raised downstream |
| `MW_PRIORITY_AUTHENTICATION` | 2000 | Identifies the caller |
| `MW_PRIORITY_AUTHORIZATION` | 3000 | Decides what the identified caller may do |
| `MW_PRIORITY_OBSERVABILITY` | 4000 | Logging, timing, metrics on an already validated call |
| `MW_PRIORITY_USER` | 5000 | The default: plain registration order |

A middleware whose position is part of what it *is*, rather than a choice of whoever registers it, declares its own default:

```pascal
type
  TErrorHandlingMiddleware = class(TMiddleware, IRequestMiddleware)
  public
    class function DefaultPriority: Integer; override;
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

class function TErrorHandlingMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_ERROR_HANDLING;
end;
```

Registering with an explicit priority still wins, which is how you move a third-party middleware without touching its source:

```pascal
.Add(TSomeoneElsesMiddleware, MW_PRIORITY_AUTHENTICATION + 100)
```

Middleware sharing a priority keeps its registration order. Priorities order middleware **within a level, never across levels**: an `IMCPMiddleware` always wraps an `ICallToolMiddleware`, whatever priorities the two declare.

### Changing the Chain at Runtime

The chain can be changed while the server is running:

```pascal
AServer.Middleware.Remove(TToolAclMiddleware);
AServer.Middleware.Add(TStricterAclMiddleware);
AServer.Middleware.Clear;

if AServer.Middleware.Contains(TAuditMiddleware) then
  // ...
```

A change takes effect from the next message on. Messages already in flight finish with the chain they started with, so a running request never sees the pipeline shift underneath it.

## Available Hooks

Hooks come in three families. The **transport-level** hook and the **message-level** ones live in `MCPConnect.JRPC.Middleware`: the first sees one whole request of the transport, the others every JSON-RPC message, MCP or not. **Operation-level** hooks live in `MCPConnect.MCP.Middleware` and see one MCP operation each, with its parameters already parsed.

### The Transport-Level Hook

| Interface | Runs for | Chain type |
|---|---|---|
| `ITransportMiddleware` | One whole request of the transport, before the payload is parsed | `TMiddlewareChain` |

The outermost level there is, and the only one that sees a request carrying no JSON-RPC message at all — a CORS preflight, a metadata request. Same signature as the message hooks:

```pascal
procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
```

Its context is of kind `Transport`: there is no `Message` and nothing to `Emit` into, because at this point there is no message yet. What a hook of this level works on is the request and the response of the transport, both in the context:

```pascal
var LResponse: TMCPTransportResponse;
if AContext.TryFind<TMCPTransportResponse>(LResponse) then
  LResponse.SetHeader('X-Server', 'mine');
```

An instance is built once per transport request, and it is not the one the message levels of the same request get: a request may carry a whole batch of messages, so the two scopes are different. Refusing here means not calling `Next`, or raising — an `EMCPTransportException` is how the HTTP transports answer with a status code of their own.

### Message-Level Hooks

| Interface | Runs for | Chain type |
|---|---|---|
| `IMessageMiddleware` | Every message: requests, notifications, responses coming back from the client, and malformed messages too | `TMiddlewareChain` |
| `IRequestMiddleware` | Messages carrying an id, which expect a response | `TMiddlewareChain` |
| `INotificationMiddleware` | Fire-and-forget messages | `TMiddlewareChain` |

All three have the same signature and return nothing — the answer travels through the context, not through a return value:

```pascal
procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
```

### Operation-Level Hooks

| Interface | MCP operation | Params | Result |
|---|---|---|---|
| `IDiscoverMiddleware` | `server/discover` | `TRequestMetaParams` | `TDiscoverResult` |
| `ICallToolMiddleware` | `tools/call` | `TCallToolRequestParams` | `TBaseResult` |
| `IListToolsMiddleware` | `tools/list` | `TPaginatedRequestParams` | `TListToolsResult` |
| `IReadResourceMiddleware` | `resources/read` | `TReadResourceParams` | `TBaseResult` |
| `IListResourcesMiddleware` | `resources/list` | `TPaginatedRequestParams` | `TListResourcesResult` |
| `IGetPromptMiddleware` | `prompts/get` | `TGetPromptRequestParams` | `TBaseResult` |
| `IListPromptsMiddleware` | `prompts/list` | `TPaginatedRequestParams` | `TListPromptsResult` |

Each has a matching chain type named after the operation — `TCallToolChain`, `TListToolsChain`, `TReadResourceChain`, and so on. To take part in all of them at once, see [One Hook for Every Operation](#one-hook-for-every-operation).

::: info Why some hooks return TBaseResult
`ICallToolMiddleware`, `IReadResourceMiddleware` and `IGetPromptMiddleware` return `TBaseResult` rather than the concrete result type, because the operation may answer with a `TInputRequiredResult` instead of its usual result. Handle that case or let it through untouched.
:::

The levels nest: for a `tools/call` request, every `ITransportMiddleware` runs, then every `IMessageMiddleware` inside it, then every `IRequestMiddleware`, then every `ICallToolMiddleware`, and finally the tool itself.

### One Hook for Every Operation

Implementing seven interfaces to watch seven operations is a poor deal when the work is the same in all of them. `IMCPMiddleware` is a single hook that takes part in **every** operation that has a chain. It sees the params already deserialized and the result as a live object, exactly like the specific hooks, and `AContext.Method` says which operation is running:

```pascal
type
  TStampMiddleware = class(TMiddleware, IMCPMiddleware)
  public
    function Handle(AContext: TMiddlewareContext; AParams: TRequestMetaParams;
      const AChain: TMCPChain): TBaseResult;
  end;

function TStampMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams; const AChain: TMCPChain): TBaseResult;
begin
  Logger.LogInfo('-> %s', [AContext.Method]);
  Result := AChain.Next(AContext, AParams);
  if Assigned(Result) then
    Result.ResultMeta.ServerInfo.Name := 'my-server';   // whatever operation ran
end;
```

It works because every MCP params type descends from `TRequestMetaParams` and every result from `TBaseResult`, so the universal hook is simply the most general shape of the same machinery. Registration is the usual `.Add(TStampMiddleware)`.

A universal hook is **a level of its own**, sitting between the request hooks and the operation ones:

```
ITransportMiddleware
  IMessageMiddleware
    IRequestMiddleware
      IMCPMiddleware          <-- here
        ICallToolMiddleware
          the tool
```

::: warning Three things to know
**It does not cover everything.** Four operations have no chain at all — `resources/templates/list`, `completion/complete`, `subscriptions/listen` and the subscriptions acknowledgement — so a universal hook does not see them either. For messages that are not operations, use `IMessageMiddleware`.

**Replacing means owning.** Only the object the operation finally returns reaches the garbage collector of the request. If you return something other than what `Next` gave you, hand the discarded one to `AContext.Own`, or it leaks.

**The result must still fit the operation.** Returning a class the operation does not work on — a `TCallToolResult` from `tools/list`, say — raises `EMCPMiddlewareError`, which reaches the client as an internal error. Three operations (`tools/call`, `resources/read`, `prompts/get`) declare `TBaseResult`, so there any descendant is accepted.
:::

## Taking Part in More Than One Chain

A class can implement as many hooks as it needs. Since they are all called `Handle`, bind them to speaking names with a **method resolution clause**:

```pascal
type
  TAuditMiddleware = class(TMiddleware, IRequestMiddleware, ICallToolMiddleware)
  private
    FMethod: string;
  public
    procedure IRequestMiddleware.Handle = OnRequest;
    function ICallToolMiddleware.Handle = OnCallTool;

    procedure OnRequest(AContext: TMiddlewareContext;
      const AChain: TMiddlewareChain);
    function OnCallTool(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;
```

The **same instance serves every level of one message**, so whatever `OnRequest` works out is still in `FMethod` when `OnCallTool` runs. That is why carrying state between levels needs nothing more than a plain field.

The same applies to `IMCPMiddleware`: a class that takes part in every operation *and* in one specific chain binds both with resolution clauses, and its universal frame encloses its own specific one.

::: warning Two message-level hooks need two clauses
`IMessageMiddleware`, `IRequestMiddleware` and `INotificationMiddleware` share one signature. A class that declares two of them and writes a single `Handle` compiles without a warning, and that one method then serves both chains. Always bind them with resolution clauses. Operation-level hooks are not affected: their signatures differ, so the compiler requires distinct methods.
:::

## The Middleware Context

`TMiddlewareContext` lives for the duration of a single message - or, at the transport level, of a single request of the transport. It is not thread-safe and must not be kept past the call.

| Member | Purpose |
|---|---|
| `Method` | The JSON-RPC method name, e.g. `tools/call`. Empty at the transport level, where no message is parsed yet |
| `Kind` | `Request`, `Notification`, `Response`, `Error`, or `Transport` for the transport level |
| `Timestamp` | When the message entered the chain |
| `Message` | The raw `TJRPCMessage`. Read it, change it if you mean to, never free it. `nil` at the transport level |
| `RPCContext` | The request context: configurations, `[Context]` objects, anything published by other middleware |
| `Emit(AMessage)` | Adds a message to the answer. Takes ownership |
| `Produced` | Copies of what this message has produced so far, meant to be read after `Next` returns |
| `Own(AObject)` | Hands an object to the garbage collector of the request |
| `Find<T>` | Shorthand for `RPCContext.FindContextDataAs<T>`, `nil` when absent |
| `TryFind<T>` | Returns `True` and the object in `out AValue` when present, `False` when absent |

A logging middleware that wants to see both sides of the exchange reads `Produced` after the chain unwinds:

```pascal
procedure TMessageLogMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  Logger.LogDebug('in:  ' + AContext.Message.ToJson());
  try
    AChain.Next(AContext);
  finally
    for var LMessage in AContext.Produced do
      Logger.LogDebug('out: ' + LMessage.ToJson());
  end;
end;
```

## Short-Circuiting a Call

Not calling `Next` suppresses the operation. You then have to answer with something of your own, or raise:

```pascal
function TToolAclMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
begin
  // Raising is the simplest way to refuse: it becomes a JSON-RPC error response
  if not Allowed(AParams.Name) then
    raise EJRPCInvalidParamsError.CreateFmt('Tool "%s" is not available', [AParams.Name]);

  Result := AChain.Next(AContext, AParams);
end;
```

Exceptions raised anywhere in the chain, or by the tool itself, propagate outwards through every middleware that wrapped the call — which is what makes a centralized error-handling middleware possible.

## Calling `Next` More Than Once

`Next` may be called several times, and every call walks **the same tail of the chain**: all the middleware below you, plus the real handler, run again from the beginning of that tail. This is what makes a retry, a fallback, or a "try, inspect, try again with different arguments" possible without any extra machinery.

It works because the chain is a **record cursor passed by value**. Calling `Next` does not move your cursor: it makes an advanced copy for the call and leaves yours exactly where it was. Were the cursor shared and mutable, a second call would resume from wherever the first one stopped and quietly skip everything in between.

With a chain of `TRetryMiddleware` followed by `TAuditMiddleware`, calling `Next` twice gives:

```
TRetryMiddleware : first attempt
  TAuditMiddleware : running
    handler: tool executed
TRetryMiddleware : second attempt
  TAuditMiddleware : running      <-- traversed again, in full
    handler: tool executed
```

A retry on a transient failure is then just a loop:

```pascal
function TRetryMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
var
  LAttempt: Integer;
begin
  Result := nil;
  for LAttempt := 1 to MaxAttempts do
  try
    // Each call walks the whole tail again: the middleware below and the tool
    Result := AChain.Next(AContext, AParams);
    Exit;
  except
    on E: ETransientError do
      if LAttempt = MaxAttempts then
        raise
      else
        Logger.LogWarning('attempt %d failed: %s', [LAttempt, E.Message]);
  end;
end;
```

The same shape covers a fallback, because `AParams` can be changed between attempts — ask for page 1 again with a smaller page size, retry a tool with a default filled in, and so on.

::: warning The operation really does run twice
`Next` re-executes the tail for real: the tool is invoked again, and so is every side effect it has. Only retry operations that are idempotent, or make the retry conditional on a failure that you know left nothing behind.

Two more things to keep in mind:

- **Do not free the result you discard.** Every result is already registered with the garbage collector of the request, so the one you drop is freed when the request ends. Freeing it yourself is a double free.
- **Bound the loop.** Nothing in the pipeline limits how many times you may call `Next`; a retry without a maximum is an infinite loop that holds a request open.
:::

## Middleware State

### Per-Request State

One instance is created per message, so the fields of a middleware are private to the message being processed and need no lock:

```pascal
type
  TCarryOverMiddleware = class(TMiddleware, IMessageMiddleware, IRequestMiddleware)
  private
    FSeenMethod: string;   // per-message: no lock, no state bag
  public
    procedure IMessageMiddleware.Handle = OnMessage;
    procedure IRequestMiddleware.Handle = OnRequest;
    // ...
  end;
```

### Shared State

State that must outlive the request belongs in a shared object, registered once and injected into the middleware with `[Context]`:

```pascal
AServer.Middleware
  .AddShared(TRateLimitStore.Create)   // the server takes ownership
  .Add(TRateLimitMiddleware);

type
  TRateLimitMiddleware = class(TMiddleware, IRequestMiddleware)
  private
    [Context] FStore: TRateLimitStore;  // shared, injected per request
    FSubject: string;                   // per-request, no lock
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;
```

This is the one place where middleware state is shared between requests, so making that object thread-safe is its own business. Use `RemoveShared` to stop handing an object out; it is freed with the server, never underneath a request still holding it.

> **The rule:** a middleware never needs a lock. If you need state that outlives the request, put it in a shared object and inject it with `[Context]`.

## Filtering Lists Per Caller

The list hooks see `TMCPTool`, `TMCPResource` and `TMCPPrompt` objects *before* they are serialized, which is what makes per-caller filtering possible:

```pascal
function TToolFilterMiddleware.Handle(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
begin
  Result := AChain.Next(AContext, AParams);

  for var LIndex := Result.Tools.Count - 1 downto 0 do
    if not Allowed(Result.Tools[LIndex].Name) then
      Result.Tools.Delete(LIndex);
end;
```

::: warning Remove, never modify
Removing an element is safe: the list does not own the tools. **Modifying one is not.** Those are the objects of the server registry, added to the result by reference and shared by every session, so changing a `Description` changes it for every client, permanently. A variant for one caller has to be a copy — and building that copy is currently up to you, since `TMCPTool` has no `Clone` yet.

A list filtered per caller must also stay `ScopePrivate`. That is what results start from, so leaving it alone is the safe move; promoting a filtered list to `ScopePublic` is enough for one caller's list to be served to another.
:::

## Middleware That Ships With MCPConnect

`MCPConnect.MCP.Middleware.Default` holds the middleware the framework provides itself — plus `MCPConnect.MCP.Middleware.OAuth`, which is big enough to live on its own. It is behaviour of MCPConnect written as middleware rather than wired into the transport, so that it can be read, reordered, replaced or removed like any other.

| Middleware | Level | Registered by | What it does |
|---|---|---|---|
| `TCORSMiddleware` | `ITransportMiddleware` | `IMCPConfig.Security` | Answers the CORS headers a browser client needs, and refuses a request whose `Origin` is not in the allowlist |
| `TAuthTokenMiddleware` | `ITransportMiddleware` | `IAuthTokenConfig.SetToken` | Checks the static token every request has to carry, in the header, cookie or `Authorization` scheme the configuration names |
| `TOAuthMiddleware` | `ITransportMiddleware` | `IOAuthConfig.AddAuthorizationServer` | Answers the OAuth discovery endpoints (`/.well-known/oauth-protected-resource`, and the authorization server document when the metadata proxy is on) and validates the bearer token of everything else |

They are **not** registered by merely using the unit. Each is put in by the configuration that turns its feature on, so a server configures a feature and gets the middleware that implements it:

```pascal
FServer.Plugin.Configure<IMCPConfig>
  .Security
    .SetCORS(True)
    .SetAllowedOrigins(['https://app.example.com', 'https://*.example.com'])
    .SetRequireOrigin(True)
  .BackToMCP
.ApplyConfig;
```

Any of those three calls registers `TCORSMiddleware`, once. `SetCORS(False)` registers it too, and that is deliberate: the middleware is the `Origin` check as much as it is the headers, and the two are configured apart — a server that wants the allowlist enforced without writing CORS headers gets exactly that. A server that says nothing about origins pays nothing: the chain stays empty.

The static token check works the same way — `SetToken` is what turns it on, so `SetToken` is what registers `TAuthTokenMiddleware`:

```pascal
FServer.Plugin.Configure<IAuthTokenConfig>
  .SetToken('my-secret-token-12345')
  .SetTokenLocation(TAuthTokenLocation.Bearer)
.ApplyConfig;
```

Saying only *where* a token would be read from configures nothing, and registers nothing. OAuth is a different mechanism with a middleware of its own, and is unaffected; there the switch is `AddAuthorizationServer`, since a resource or a validator without one enforces nothing.

`TOAuthMiddleware` is the one that does more than check: the discovery endpoints a client reads *before* it has a token are answered by it and never reach the dispatcher — it writes the response and does not call `Next`. That is the shape of a middleware that owns a URL, and the transport level is the only one where a URL is still a thing.

The three run in a fixed order, whatever order you configure them in — CORS at `MW_PRIORITY_AUTHENTICATION - 100`, the static token at `MW_PRIORITY_AUTHENTICATION`, OAuth at `MW_PRIORITY_AUTHENTICATION + 100`:

```
TCORSMiddleware
  TAuthTokenMiddleware
    TOAuthMiddleware
      the request
```

That CORS is outermost is not cosmetic — without the headers on a `401` or a `403`, a browser reports a CORS failure instead of the refusal your server actually sent, and an OAuth client cannot read the metadata URL out of the challenge.

Being ordinary middleware, they can be taken out or moved like any other:

```pascal
FServer.Middleware.Remove(TCORSMiddleware);
FServer.Middleware.Add(TMyOwnCORSMiddleware);
```

All three are transport hooks for the same reason: a preflight and a `/.well-known` request carry no JSON-RPC message at all, so no hook of the message levels would ever see them — and a request refused for its `Origin` or its token must not be parsed, let alone dispatched.

## What's Next

Middleware sits above everything else the server does, so it composes with the rest of the configuration rather than replacing any of it: [Tools](./tools) are the operations the tool hooks wrap, [Sessions](./sessions) remain the place for per-caller state that belongs to your application rather than to the pipeline, and [JSON-RPC](./jrpc) is the protocol layer the message hooks sit on. The design document behind this feature, with the reasoning and the alternatives that were discarded, is `Docs/middleware-spec.md`.
