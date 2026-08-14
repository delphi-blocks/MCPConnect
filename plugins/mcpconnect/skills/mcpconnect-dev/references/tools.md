# Tools

A tool is a Delphi **function** decorated with `[McpTool]`, in a class registered in the `.Tools` section. Contents:

- [Anatomy of a tool class](#anatomy-of-a-tool-class)
- [Instance lifetime — no state in fields](#instance-lifetime--no-state-in-fields)
- [Parameters](#parameters)
- [Return types](#return-types)
- [Building rich results with TContentList](#building-rich-results-with-tcontentlist)
- [Scopes](#scopes)
- [Tags](#tags)
- [Reporting errors](#reporting-errors)
- [Programmatic registration (no attributes)](#programmatic-registration-no-attributes)

## Anatomy of a tool class

```pascal
unit MyApp.Tools;

interface

uses
  System.SysUtils, System.Generics.Collections,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  TTicket = class
  private
    FId: Integer;
    FTitle: string;
    FPrice: Currency;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Price: Currency read FPrice write FPrice;
  end;

  TTickets = class(TObjectList<TTicket>);

  [McpScope('tickets')]
  TTicketTool = class
  public
    [McpTool('list', 'List every ticket currently on sale, with id, title and price', 'readonly')]
    function ListTickets: TTickets;

    [McpTool('buy', 'Buy a given quantity of one ticket type; charges the customer immediately')]
    function BuyTicket(
      [McpParam('id', 'Id of the ticket type to buy, as returned by tickets_list')] AId: Integer;
      [McpParam('quantity', 'How many tickets to buy')] AQuantity: Integer
    ): string;

    // No attribute: entirely invisible to MCP, callable only from your own code
    procedure AuditPurchase(AId: Integer);
  end;
```

Register it:

```pascal
.Tools
  .RegisterClass(TTicketTool)
.BackToMCP
```

Published names are `tickets_list` and `tickets_buy`. Names must match `^[a-zA-Z0-9_-]{1,64}$` **after** the scope prefix is applied.

The class needs a parameterless constructor — the default `TObject.Create` is fine — because the framework instantiates it by RTTI.

## Instance lifetime — no state in fields

The framework creates a **fresh instance of the tool class for every single call** and frees it as soon as the call returns. Fields do not survive between calls, and there is no shared instance to race on.

That makes tool classes naturally thread-safe, but it also means anything that must persist across calls has to live somewhere else: a session object injected with `[Context]` (see `sessions-notifications.md`), a database, or a global service the class talks to. A "remember what the user asked last time" field will silently always be empty.

## Parameters

Every parameter must carry `[McpParam(name, description)]`. The `name` is what appears in the JSON schema and what the model sends; the Delphi parameter name is irrelevant to the client. A single un-annotated parameter makes the whole class fail to register with `Non-annotated params are not permitted`.

**Every tool parameter is required.** The generated schema puts all of them in `required`; there is no optional-parameter mechanism. If an argument is genuinely optional, model it explicitly — accept a `string` and treat empty as "not supplied", and say so in the description.

Parameter types are converted from JSON by Neon, so the usable set is what Neon can bind: `string`, integer types, `Int64`, `Boolean`, floating point, `Currency`, `TDateTime`, enumerations, and classes/records that Neon can deserialize. Use `const` on string and object parameters, per the project's Delphi conventions.

The description is the model's only guide to what a valid value looks like. Enumerate allowed values, give the format of dates and ids, and cross-reference the tool that produces them:

```pascal
[McpParam('itemType', 'Category to filter by. One of: ''courses'', ''product'', ''consulting''')] const AItemType: string
```

## Return types

A tool must be a function — a procedure raises `Tool must be a function`. What you return determines the content the client sees:

| Return type | What the client receives |
|-------------|--------------------------|
| `string`, `Integer`, `Boolean`, `Double`, `Currency`, … | A single text content item with the value's string form |
| Any class or generic list (`TObjectList<T>`, …) | Serialized to JSON by Neon, delivered as a text content item |
| `TStream`, `TBytes` | Base64-encoded binary blob (requires `TMCPStreamWriter`) |
| `TPicture` / `TGraphic` | Image content item (requires `TMCPPictureWriter` / `TMCPImageWriter`, VCL only) |
| `TStringList` | Text content (requires `TMCPStringListWriter`) |
| `TContentList` | Passed through untouched — full control, multiple and mixed content items |

Writers are registered in `.Server`:

```pascal
.Server
  .RegisterWriter(TMCPStreamWriter)       // MCPConnect.Content.Writers.RTL
  .RegisterWriter(TMCPStringListWriter)   // MCPConnect.Content.Writers.RTL
  .RegisterWriter(TMCPImageWriter)        // MCPConnect.Content.Writers.VCL
  .RegisterWriter(TMCPPictureWriter)      // MCPConnect.Content.Writers.VCL
.BackToMCP
```

**Ownership:** objects returned from a tool are owned and freed by the framework once serialized. You do not free the result. But the framework only ever sees an object that was actually returned — if an exception fires after `Result := TSomething.Create` and before the method exits, that object leaks. Guard it with the garbage collector:

```pascal
type
  TTicketTool = class
  private
    [Context] FGC: IGarbageCollector;   // MCPConnect.JRPC.Classes
  public
    …
  end;

function TTicketTool.ListTickets: TTickets;
begin
  Result := TTickets.Create;
  FGC.Add(Result);                       // freed at end of request even if we raise below
  Result.Add(LoadTicket(1));
  Result.Add(LoadTicket(2));
end;
```

`FGC.Add` also accepts a custom disposal procedure for handles that need closing before `Free`. Details in `sessions-notifications.md`.

### Structured output

Tag a tool `structured` to have MCPConnect emit an `outputSchema` alongside the result, generated by Neon from the return type, and deliver the value as `structuredContent`:

```pascal
[McpTool('get_person', 'Get a person record by name', 'structured')]
function GetPerson([McpParam('name', 'Person''s full name')] const AName: string): TPerson;
```

The MCP spec limits `outputSchema` and `structuredContent` to JSON **objects**, so a tool returning a bare list or a scalar cannot be `structured` — wrap it in a class with a property if you need schema output.

## Building rich results with TContentList

`TContentList` (in `MCPConnect.MCP.Types`) is a `TObjectList<TToolContent>` with builder-style methods. Return it directly; the framework frees it.

```pascal
function TTicketTool.BuyTicket(AId, AQuantity: Integer): TContentList;
var
  LStream: TFileStream;
begin
  Result := TContentList.Create;
  Result.AddText('Purchase completed. Your ticket is attached.');

  LStream := TFileStream.Create(TPath.Combine(TPath.GetAppPath, 'data\ticket.png'),
    fmOpenRead or fmShareDenyWrite);
  try
    Result.AddImage('image/png', LStream);
  finally
    LStream.Free;      // the stream is read and encoded here; the list keeps base64
  end;
end;
```

| Method | Adds |
|--------|------|
| `AddText(text)` | A text item |
| `AddImage(mime, base64)` / `AddImage(mime, stream)` | An image item |
| `AddAudio(mime, base64)` / `AddAudio(mime, stream)` | An audio item |
| `AddBlob(mime, base64)` / `AddBlob(mime, stream)` | An embedded binary resource |
| `AddLink(mime, uri, description)` | A resource link pointing elsewhere |

Note: `TToolResultBuilder` / `IToolResultBuilder` appear in older documentation and in a doc comment inside `MCPConnect.JRPC.Classes.pas`, but no such class exists in the current source. Use `TContentList` directly.

## Scopes

`[McpScope('name')]` on the class namespaces every tool it publishes as `name_toolname` (the separator is configurable with `.Server.SetScopeSeparator`). Scopes let several classes share method names and give the client a tidier, grouped tool list:

```pascal
[McpScope('auth')]    TAuthService   = class … end;   // auth_login, auth_logout
[McpScope('tickets')] TTicketService = class … end;   // tickets_list, tickets_buy
```

## Tags

The optional third argument of `[McpTool]` is a comma-separated tag string — bare flags or `key=value` pairs:

```pascal
[McpTool('buy', 'Buy tickets', 'icon=cart.png,category=shop,destructive')]
```

| Tag | Effect |
|-----|--------|
| `icon=file.png` | Tool icon, resolved under `.Server.SetIconFolder` |
| `category=name` | Groups the tool in clients that display categories |
| `app=ui://…` | Renders the tool's result inside an MCP App UI (equivalent to the separate `[McpApp('ui://…')]` attribute) |
| `structured` | Emit `outputSchema` and return `structuredContent` (JSON objects only) |
| `disabled` | Registered but hidden from `tools/list` — the switch for temporarily retiring a tool |
| `readonly` | Annotation hint: the tool does not modify anything |
| `destructive` | Annotation hint: the tool may delete or overwrite. Set this honestly — clients use it to decide whether to ask the user first |
| `idempotent` | Annotation hint: repeating the call with the same arguments is safe |
| `openworld` | Annotation hint: the tool touches the outside world (network, third-party services) |

## Reporting errors

Raise an ordinary Delphi exception. The framework catches it and returns a JSON-RPC error carrying the original class name and message — `Tool call Error class: "EMyAppError" - message: "Ticket 42 is sold out"` — so a specific exception class and a message written for the model is what the client, and therefore the LLM, actually gets to read.

```pascal
function TTicketTool.BuyTicket(AId, AQuantity: Integer): string;
begin
  if AQuantity <= 0 then
    raise EArgumentException.Create('quantity must be at least 1');

  if not TicketExists(AId) then
    raise EMyAppError.CreateFmt('No ticket with id %d; call tickets_list for valid ids', [AId]);

  …
end;
```

Write these messages as instructions to a model that will read them and retry: say what was wrong and what to do instead. Do not leak connection strings, stack traces or internal paths into the message — it leaves the process.

## Programmatic registration (no attributes)

Classes that cannot carry Delphi custom attributes — most often C++Builder classes — are registered by describing them at configuration time instead. Both styles mix freely in the same `.Tools` section, and the result is identical.

```pascal
.Tools
  .RegisterTool(TRegisterToolTest, 'RandomNumber', 'random',
      'Generate a random number below the given maximum', 'icon=dice.png')
    .WithParam('AMax', 'range', 'Exclusive upper bound for the generated number')
    .EndTool

  .RegisterClass(TTicketTool)   // attribute-based, same section
.BackToMCP
```

`RegisterTool(AClass, AMethodName, AName, ADescription, ATags)` names the Delphi method to expose and the MCP tool name to expose it as. `WithParam(AParamName, AName, ADescription, ATags)` maps one Delphi parameter — by its real Pascal name — to its MCP name and description; declare one per parameter, in any order, then close with `EndTool`. Every parameter must be described, exactly as with `[McpParam]`.
