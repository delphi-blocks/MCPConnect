# The request context

Every message a server handles is served with a **request context** — a `TJRPCContext` built by the transport and thrown away when the request ends. It is a registry of objects keyed by their class, and it is how a tool, a resource, a prompt, a middleware or an API class gets hold of anything it did not create itself:

```pascal
type
  TTodoTool = class
  public
    [Context] Token: TMCPAccessToken;     // who is calling
    [Context] Progress: TMCPProgress;     // report progress on this call

    [McpTool('add_task', 'Adds a task')]
    function AddTask(const ATitle: string): string;
  end;
```

The rules are short:

- objects are keyed by their **exact class** — a descendant is a different key — and an interface field is resolved by querying what is in there;
- one object per class: a second `AddContent` of the same class replaces the first;
- a plain `[Context]` field the context cannot resolve **raises** (`ECtxException`) — it is a statement that the object has to be there. For one that may be missing, ask for it with `[Context([TContextOption.Optional])]`, which leaves the field nil instead; the same thing read by hand is `FindContextDataAs<T>` (in an API class, `RPCContext.FindContextDataAs<T>`) or, inside a middleware, `AContext.Find<T>` / `TryFind<T>`;
- lifetime is the request: whatever is put in is owned by the request's garbage collector (or, for a middleware, by `AContext.Own`), not by the reader.

## What is in the context

Listed by the layer that puts it there. The layer is the stable part — line numbers and call sites move, the layers do not.

### Transport, once per request

`MCPConnect.Transport.Base.pas`, `TMCPTransportHandler.ProcessRequest` — built before any check runs, so that the transport middleware and the token validator already find it.

| Object | What it is for |
|---|---|
| `TJRPCContext` | The context itself; it registers itself on construction. Api classes take it as `[Context] RPCContext` to inject their own objects or to read something optional. |
| `TGarbageCollector` | Owns what is created for the request. Add to it what must die with the request. |
| `TMCPTransportRequest` | The request as the transport received it: url, command, headers, body, protocol (HTTP or STDIO). |
| `TMCPTransportResponse` | The response being built: status code, content type, headers, content. |
| `TMCPAccessToken` | The claims of the validated bearer token — subject, scope, client id, expiry. Present but empty when the server takes no token. |
| `TMiddlewarePipeline` | The middleware instances of this request. Api classes run their own hooks on it (`TCallToolChain.Run` and friends). |
| `TMCPServer` | The server component that is answering. |
| every `TJRPCConfiguration` of that server | `TMCPConfig`, `TOAuthConfig`, `TAuthTokenConfig`, `TJRPCNeonConfig`, `TMCPLegacyConfig`, … — each by its own class, and also reachable by its interface (`IMCPConfig`). This is what `[Context] MCPConfig: TMCPConfig` resolves to. See `AddApplicationToContext` in `MCPConnect.Configuration.Core.pas`. |
| shared middleware objects | Whatever the server registered with `Middleware.AddShared` — a cache, a connection pool, anything a middleware and a tool want to share. |

### Transport, once per message

Same unit, after the payload is parsed.

| Object | What it is for |
|---|---|
| `TJRPCRequest` | The JSON-RPC request being dispatched: method, id, params. Also reachable as `CurrentRequest`. |
| `TMCPMessageQueue` | Where a server-to-client message goes: the transport drains it to the SSE stream, or into the response body. |
| `TMCPProgress` | The progress channel of this request. It emits nothing until the request is found to have asked for progress. |
| `TMCPLog` | The log channel, on the same terms: silence until the request states a log level. |

### Middleware

`MCPConnect.MCP.Middleware.RequestMeta.pas` — only for a method in an MCP namespace, and only when meta validation is not `Off` and the request actually carries a `params._meta`.

| Object | What it is for |
|---|---|
| `TRequestMetaObject` | The parsed `_meta`: protocol version, client info, log level, progress token. |
| `TMCPDeclaredCapabilities` | What the client declared it can do, as a set. **Absent means nobody looked** — validation off, or no `_meta` under a lenient one — which is not the same as a client that declared nothing, and is why the MRTR capability check skips rather than refuses when it finds none. |

Both are the case `[Context([TContextOption.Optional])]` exists for: a field declared plainly would raise on every request that carries no `_meta`.

::: tip A legacy client declares nothing here
A client speaking MCP 2025-06-18 states its capabilities in `initialize`, not per request, so neither of these two objects reaches the context of its requests — see `MCPConnect.Configuration.Legacy.pas`.
:::

### Api

`MCPConnect.MCP.Server.Api.pas` — each endpoint publishes the params it was called with, so that a middleware or a tool downstream can read the whole request rather than only the arguments bound to its own method.

| Object | Published by |
|---|---|
| `TCallToolRequestParams` | `tools/call` |
| `TReadResourceParams` | `resources/read` |
| `TGetPromptRequestParams` | `prompts/get` |
| `TPaginatedRequestParams` | `tools/list`, `resources/list`, `resources/templates/list`, `prompts/list` |
| `TCompleteRequestParams` | `completion/complete` |
| `TSubscriptionsListenRequestParams` | `subscriptions/listen` |
| `TRequestMetaParams` | `server/discover` |

## Who gets injected

`Inject` is called on:

- the API class instance of the request (`MCPConnect.Transport.Base.pas`);
- the tool, resource, prompt and completion-provider instances, and their invokers (`MCPConnect.MCP.Server.Api.pas`);
- every middleware instance, as the pipeline creates it (`TMiddlewarePipeline.InstanceOf`).

So a `[Context]` field works the same way in all of them.

## Adding something of your own

From a middleware — the usual place, because it runs before the api class:

```pascal
procedure TTenantMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LTenant: TTenant;
begin
  LTenant := TTenant.Create(AContext.Find<TMCPAccessToken>);
  AContext.Own(LTenant);                   // lifetime: the request
  AContext.RPCContext.AddContent(LTenant); // visibility: [Context] TTenant

  AChain.Next(AContext);
end;
```

For something that lives as long as the server rather than as long as the request, register it once with `Middleware.AddShared` instead: it is put into the context of every request, and the middleware list owns it.
