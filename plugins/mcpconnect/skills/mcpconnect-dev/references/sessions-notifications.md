# Sessions, context injection, notifications, memory and writers

Contents:

- [`[Context]` injection](#context-injection)
- [Sessions](#sessions)
- [Server→client notifications and SSE](#serverclient-notifications-and-sse)
- [Memory ownership and IGarbageCollector](#memory-ownership-and-igarbagecollector)
- [Content writers](#content-writers)
- [Neon serialization](#neon-serialization)

## `[Context]` injection

Tool, resource and prompt classes are instantiated per request, so anything request-scoped has to be handed to them. `[Context]` (from `MCPConnect.JRPC.Classes`) on a **field** does that: before invoking your method, the framework matches the field's class or interface against the objects in the request context and assigns it.

```pascal
uses
  MCPConnect.JRPC.Classes,   // ContextAttribute, IGarbageCollector
  MCPConnect.JRPC.Core,      // TMCPMessageQueue
  MCPConnect.MCP.Types;      // TMCPAccessToken

type
  TMyTool = class
  private
    [Context] FGC: IGarbageCollector;      // request-scoped garbage collector
    [Context] FSession: TShoppingSession;  // your session class
    [Context] FResponses: TMCPMessageQueue;// outbound message queue
    [Context] FToken: TMCPAccessToken;     // validated access token claims
  public
    …
  end;
```

What can be injected:

| Field type | What you get |
|------------|--------------|
| `IGarbageCollector` | Request-scoped collector; everything added is freed when the request ends |
| Your `TMCPSessionBase` descendant, or `TMCPSessionData` | The current session — only where sessions exist |
| `TMCPMessageQueue` | The outbound queue: enqueue notifications to push to the client mid-call |
| `TMCPAccessToken` | Decoded JWT claims (`Subject`, `Name`, `EMail`, `Scope`, `Payload`, …) when OAuth is configured |
| `TJRPCServer` | The server itself, e.g. to reconfigure the published feature set at runtime |
| `TJRPCContext` | The raw request context |

A field whose type is not in the context raises at invocation time, so only ask for what the transport actually provides — the session fields in particular need session configuration (or a stdio server, where the session is implicit).

## Sessions

HTTP is stateless; sessions give a conversation continuity across calls. Configure the `ISessionConfig` plugin (`MCPConnect.Configuration.Session`) *before* the MCP plugin:

```pascal
AServer
  .Plugin.Configure<ISessionConfig>
    .SetLocation(TSessionIdLocation.Header)   // or Cookie
    .SetHeaderName('Mcp-Session-Id')
    .SetTimeout(30)                           // minutes of inactivity
    .SetSessionClass(TShoppingSession)        // optional custom class
    .SetReplayBufferSize(100)                 // SSE events kept for Last-Event-ID replay
  .ApplyConfig

  .Plugin.Configure<IMCPConfig>
    …
```

Without `SetSessionClass` the session is a `TMCPSessionData`, whose `Data: TJSONObject` is a free-form bag. A custom class is usually clearer — descend from `TMCPSessionBase` and hold typed state:

```pascal
uses
  MCPConnect.Session.Core;

type
  TShoppingSession = class(TMCPSessionBase)
  private
    FCart: TObjectDictionary<string, TCartItem>;
  public
    property Cart: TObjectDictionary<string, TCartItem> read FCart;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TShoppingSession.Create;
begin
  inherited Create;
  FCart := TObjectDictionary<string, TCartItem>.Create([doOwnsValues]);
end;
```

`TMCPSessionBase` provides `SessionId`, `CreatedAt`, `LastAccessedAt`, the inbound/outbound queues and the SSE replay buffer. Use it from a tool through `[Context]`:

```pascal
[McpScope('shopping')]
TShoppingCartTool = class
private
  [Context] FSession: TShoppingSession;
public
  [McpTool('cart_add', 'Add an item to the shopping cart')]
  function AddToCart(
    [McpParam('item_id', 'Id of the item to add')] const AItemId: string;
    [McpParam('quantity', 'How many to add')] AQuantity: Integer): string;
end;
```

Two things to keep in mind. Objects you put into the session are owned by the session, not by the request — never hand the framework a session-owned object as a tool result, because it will be freed after serialization; return a copy or a projection instead. And a session can be touched by concurrent requests from the same client, so guard mutable collections if the server may see overlapping calls.

Sessions expire on the configured inactivity timeout, and a request carrying a stale id gets `EMCPSessionExpiredError` (JSON-RPC code `-32002`). Under stdio all of this is implicit: one process, one session, no configuration needed.

## Server→client notifications and SSE

A long-running tool can push messages to the client while it works. Inject the outbound queue and enqueue `TJRPCNotification` descendants:

```pascal
type
  TProgressNotification = class(TJRPCNotification)
  public
    constructor Create(APosition, ASize: Integer);
  end;

constructor TProgressNotification.Create(APosition, ASize: Integer);
begin
  inherited Create;
  Method := 'notification/logging';
  AddNamedParam('position', APosition);
  AddNamedParam('size', ASize);
end;
```

```pascal
type
  TTicketTool = class
  private
    [Context] FResponses: TMCPMessageQueue;
  public
    [McpTool('get_tickets', 'List available tickets')]
    function GetTickets: TTickets;
  end;

function TTicketTool.GetTickets: TTickets;
begin
  Result := TTickets.Create;
  FGC.Add(Result);

  Result.Add(LoadTicket(1));
  FResponses.Enqueue(TProgressNotification.Create(1, 3));
  …
end;
```

What happens to an enqueued message depends on the client: if it sent `Accept: text/event-stream` and the transport supports streaming, the message goes out immediately as an SSE event, tagged with a per-session event id. Otherwise notifications are buffered on the session's outbound queue for later delivery, and ordinary responses are collected into the final JSON-RPC response. Your code does not change either way — enqueue and let the transport decide.

Because each SSE event carries an id and the session keeps a replay buffer (`SetReplayBufferSize`), a client that drops the connection and reconnects with `Last-Event-ID` is replayed the events it missed.

Ready-made notifications for list changes live in `MCPConnect.MCP.Types`: `TToolListChangedNotification`, `TResourceListChangedNotification`, `TPromptListChangedNotification`, `TRootsListChangedNotification`. Enqueue the matching one after registering or unregistering features at runtime, otherwise clients keep using the list they fetched at connect time.

## Memory ownership and IGarbageCollector

The rule in one line: **objects you return are freed by the framework; objects you create and do not return are yours.**

`IGarbageCollector` covers the gap in between. It is request-scoped, injected with `[Context]`, and frees everything registered when the request ends — normally or by exception:

```pascal
FGC.Add(LResult);                       // freed at end of request

FGC.Add(LConnection, procedure          // custom disposal
begin
  LConnection.Close;
  LConnection.Free;
end);

FGC.Add([LFirst, LSecond]);             // several at once
FGC.CollectGarbage;                     // force disposal early
```

`Add` takes a `TValue`, so objects can be passed directly. Duplicate registrations are ignored, which makes it safe to register a `Result` that the framework will also free.

The classic use is exception safety: register the result immediately after creating it, then build it up freely, without the `try Result.Free; raise; except` boilerplate.

## Content writers

A writer teaches MCPConnect how to turn one Delphi type into MCP content. Built-in sets:

- `MCPConnect.Content.Writers.RTL` — `TMCPStringListWriter` (`TStringList`), `TMCPStreamWriter` (`TStream`)
- `MCPConnect.Content.Writers.VCL` — `TMCPImageWriter` (`TGraphic`), `TMCPPictureWriter` (`TPicture`)

Register the ones you use in `.Server.RegisterWriter(...)`; guard VCL writers with `{$IFDEF FRAMEWORK_VCL}` in code that also compiles console-only. Selection is by type with inheritance distance, so the most specific registered writer wins.

To support your own type, descend from `TMCPCustomWriter` and implement the three destinations — the same value has to render as a tool result, a prompt message and a resource body:

```pascal
type
  TMyReportWriter = class(TMCPCustomWriter)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    procedure WriteTool(const AValue: TValue; AContext: TMCPToolContext); override;
    procedure WritePrompt(const AValue: TValue; AContext: TMCPPromptContext); override;
    procedure WriteResource(const AValue: TValue; AContext: TMCPresourceContext); override;
  end;

class function TMyReportWriter.GetTargetInfo: PTypeInfo;
begin
  Result := TMyReport.ClassInfo;
end;

class function TMyReportWriter.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := TypeInfoIs(AType);      // matches TMyReport and its descendants
end;

procedure TMyReportWriter.WriteTool(const AValue: TValue; AContext: TMCPToolContext);
begin
  AContext.Result.Content.AddText((AValue.AsObject as TMyReport).AsPlainText);
end;
```

Each context also carries `Attributes`, the method's attribute list — that is how the built-in resource writers read the MIME type and URI off `MCPResourceAttribute` rather than guessing.

## Neon serialization

Neon converts JSON arguments into parameters, objects into JSON results, and Delphi types into the JSON schemas published in `tools/list`. Its default is camelCase over public members.

Change it globally through the `IJRPCNeonConfig` plugin:

```pascal
uses
  Neon.Core.Persistence,
  MCPConnect.Configuration.Neon;

AServer
  .Plugin.Configure<IJRPCNeonConfig>
    .SetNeonConfig(
      TNeonConfiguration.Default
        .SetMemberCase(TNeonCase.CamelCase)
        .SetVisibility([mvPublic, mvPublished]))
  .ApplyConfig
```

Or only for the generated tool schemas:

```pascal
.Tools
  .SetSchemaNeonConfig(
    TNeonConfiguration.Camel.RegisterSerializer(TJSONValueSerializer))
.BackToMCP
```

Whatever you choose, keep the two consistent: a schema generated with one naming convention and a binder using another produces tools whose arguments silently never arrive. Custom serializers for awkward types are registered on the Neon configuration itself — see the Neon documentation.
