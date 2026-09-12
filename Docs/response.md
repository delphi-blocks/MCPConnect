# Answering, and Asking for More

Three MCP requests may not be able to finish in one round trip: `tools/call`, `resources/read` and `prompts/get`. Instead of their normal result, a server may answer any of them with an `input_required` result — a set of requests for the *client* to fulfil (ask the user a question, sample an LLM, list its roots) — and the client then retries the original call with the answers attached. The spec calls this **MRTR**, multi round-trip requests.

This page is about the Delphi side of that: how a method says "here is my answer" or "I need something first" without losing its return type, how the form the user sees is declared, and how the answer comes back.

Everything here lives in `MCPConnect.MCP.Response` and `MCPConnect.MCP.Types.Elicitation`.

::: warning Provisional
This feature is under development on the `feature/mcp-2026-07-28` branch. Names and signatures may still change before release.
:::

## The Problem

Because those three requests can answer in two ways, their invokers accept `TBaseResult` — the ancestor of both the normal result and `TInputRequiredResult`. A method that wants the choice therefore has to declare `TBaseResult`:

```pascal
[McpTool('delete_task', 'Delete a task')]
function DeleteTask(AId: Integer): TBaseResult;   // what does it answer with?
```

And with that, the method stops saying what it answers with. It can no longer be read as "this returns a string" or "this returns a `TTaskItem`", the invoker's ordinary conversions are out of reach — it has to build a `TCallToolResult` by hand — and `outputSchema`, which is generated from the return type, has nothing left to describe.

## TMCPResponse&lt;T&gt;: a Box, Not an Answer

`TMCPResponse<T>` gives the return type back. The method declares what it *normally* answers with, and per call puts one of three things in the box:

```pascal
[McpTool('delete_task', 'Delete a task')]
function DeleteTask(AId: Integer): TMCPResponse<string>;
```

| In the box | Built with | Meaning |
|---|---|---|
| a value of `T` | `Value(x)`, or its alias `Ok(x)` | the normal answer, converted exactly as an unboxed `T` would be |
| a result object | `Ready(AResult)` | a result the method built itself (a tool error, a multi-content reply) |
| an input request | `Needs(AInput)` | "I need this from the client before I can finish" |

```pascal
Result := TMCPResponse<string>.Ok('Task deleted');

Result := TMCPResponse<string>.Ready(TCallToolReply.Fail('No such task'));

Result := TMCPResponse<string>.Needs(
  TMCPInput.New(LState).Confirm('delete', 'Delete task #12?'));
```

**The box never reaches the client.** All four invokers call `TMCPResponse.Unwrap` the moment the method returns; what the box held continues down the ordinary path — same conversion, same garbage collection — and the box frees itself. A box that is never opened (a method that built one and raised before returning it) frees its payload too.

A few details worth knowing:

- `Ready` refuses a nil result, and boxes with the object's **runtime** class, so `Payload` and any serializer see the real type rather than `TObject`.
- A method that returns **nil** is refused with a clear error rather than silently matching the first branch of the invoker: an empty `TValue` satisfies `IsType<T>` for every `T`, which would otherwise turn a forgotten assignment into a confusing result.
- `Unwrap` opens nested boxes, so a helper that returns `TMCPResponse<T>` can be boxed again by its caller without surprises.
- `outputSchema` describes the payload, not the carrier: `WriteOutputSchema` reads `PayloadType` (a `TRttiType`) off the box. A tool tagged `structured` that declares the bare non-generic `TMCPResponse` has no `T` to describe, and is refused at registration.

## Building a Result

When `Ready` is the right answer, the result is built through the vocabulary of the operation that answers — tools-only factories are not offered to a resource or a prompt:

```pascal
// tools/call
TCallToolReply.Text('done');              // one text block
TCallToolReply.Fail('not allowed');       // isError: true
TCallToolReply.Empty;                     // no content
TCallToolReply.Content(LContentList);     // mixed / multi-block
TCallToolReply.Structured(LJson);         // structuredContent

// resources/read
TResourceReply.Text('res://readme', 'text/plain', LText);
TResourceReply.Blob('res://logo', 'image/png', LBytes);

// prompts/get
TPromptReply.User('Summarise this');
TPromptReply.Assistant('Sure');
TPromptReply.Message(TRole.User, '...');
```

## Asking: TMCPInput

`TMCPInput` is the fluent builder of the `input_required` result. Start with `New` (optionally carrying the continuation token), add one request per thing needed, and either `Build` it or hand the builder to `Needs`, which builds it for you:

```pascal
Exit(TMCPResponse<string>.Needs(
  TMCPInput.New(LState)
    .Confirm('delete', 'Delete task #12?')
    .AskText('why', 'Why?', 'reason', 'Reason')));
```

Each key — `'delete'`, `'why'` — is what the client answers under, and what the reading side looks the answer up by.

| Method | Asks for |
|---|---|
| `Ask<T>(key, message)` | a form generated from the record or class `T` |
| `Confirm(key, message)` | one yes/no, read back with `Confirmed` |
| `AskText` / `AskInteger` / `AskNumber` | one scalar |
| `AskChoice<TEnum>` | one member of an enumerated type |
| `AskMultiChoice<TSet>` | any number of the members a set type admits |
| `Elicit(key, message, ASchema)` | a form described by a schema you hold |
| `ElicitParams(key, AParams)` | a request you built yourself |
| `ElicitUrl(key, message, url)` | the user to complete something in a browser |
| `Sample(key, AParams)` | the client to sample an LLM |
| `Roots(key)` | the client's list of roots |

`Build` hands the result over and leaves the builder empty; `TryBuild` does the same without raising on an empty build; `Count` and `IsEmpty` report what is in it.

The builder is a record for the fluent syntax, but its state is a reference-counted object **shared by every copy**. So the result is handed over exactly once, whichever copy asks for it, and a chain that is abandoned after it started holding something frees what it held rather than leaking it.

## The Form

A form-mode elicitation carries a `requestedSchema` — a deliberately restricted subset of JSON Schema: flat primitives only, no nested objects, and no arrays other than the multi-select choice. There are two ways to produce one.

### From a Delphi Type

The preferred way: declare a record (or class) and let Neon's schema generator describe it. The same RTTI reads the answer back, so one declaration serves both directions and nothing spells the member names twice.

```pascal
type
  TDeleteAsk = record
    [JsonSchema('title=Delete the task?, description=This cannot be undone, required')]
    Confirm: Boolean;
    [JsonSchema('title=Reason, description=Kept in the server log, maxLength=80')]
    Reason: string;
  end;
```

```pascal
TMCPInput.New(LState).Ask<TDeleteAsk>('delete', 'Delete task #1?')
```

which asks the client for exactly this:

```json
{
  "type": "object",
  "properties": {
    "confirm": { "type": "boolean", "title": "Delete the task?",
                 "description": "This cannot be undone" },
    "reason":  { "type": "string", "title": "Reason",
                 "description": "Kept in the server log", "maxLength": 80 }
  },
  "required": ["confirm"]
}
```

Everything the user sees comes from `[JsonSchema]` — `title`, `description`, `required`, `minLength`/`maxLength`, `minimum`/`maximum`, `pattern`, `default` — and the member names are Neon's, which is what its reader expects back (`MCPNeonConfig` is camelCase over fields). `TMCPTypeSchema.From<T>` and `TMCPElicitRequest.Form<T>` are the same thing at lower level, for a server that wants the schema or the params rather than a builder step.

What `TMCPTypeSchema` adds on top of Neon is the elicitation restriction, checked when the schema is rendered rather than discovered by a client that cannot draw the form. These are refused:

| Refused | Why |
|---|---|
| a nested record or class member | the form is flat; the member renders as an object |
| a `$ref` / self-referencing type | same, one step removed |
| a `Nullable<T>` member | it renders as a union, `["string","null"]`, which no primitive schema is — declare optionality by leaving the member out of `required` |
| a type that is neither record nor class | there is nothing to turn into properties |
| a type with no member to ask for | an empty form asks nothing |

### Choices Are Types Too

A single choice is an enumerated type, a multiple choice a set of one:

```pascal
type
  [NeonEnumNames('delphi,free-pascal,basic')]
  TLang = (Delphi, Pascal, Basic);

  TPerm = (PermRead, PermWrite, PermAdmin);
  TPerms = set of TPerm;
```

```pascal
TMCPInput.New(LState)
  .AskChoice<TLang>('lang', 'Which language?', 'lang', 'Language')
  .AskMultiChoice<TPerms>('perms', 'Which permissions?', 'perms', 'Permissions');
```

The option values are **the names Neon writes** — `[NeonEnumNames]` when present, otherwise the member names under the configured case — read out of the schema Neon generates rather than walked separately. That matters: the value the client sends back has to be the value Neon's reader accepts, so there is one authority on those names, and `FieldAs<TLang>` reads the answer straight back into `TLang`.

The spec defines three shapes for a set of options, and `TMCPChoiceShape` on the property picks one:

| `Shape` | Renders as |
|---|---|
| `Plain` (default) | `{"type":"string","enum":["delphi","free-pascal","basic"]}` |
| `Titled` | `{"type":"string","oneOf":[{"const":"delphi","title":"delphi"},…]}` |
| `Legacy` | `enum` alongside a parallel `enumNames` — deprecated, and not standard JSON Schema 2020-12 |

In the titled shape the label *is* the value, because the only thing that names a Delphi enum member for JSON is `[NeonEnumNames]`, and what it names is what travels. For a multiple choice the options live under `items` (`items.anyOf` when titled).

### Built at Run Time

When the form's shape is only known at run time — fields read from a database, a questionnaire defined by configuration — `TMCPElicitationSchema` builds the same document property by property:

```pascal
LSchema := TMCPElicitationSchema.Create;
try
  LSchema.AddString('name', 'Your name', True).MinLength := 2;
  LSchema.AddInteger('age', 'Your age').Maximum := 130;
  LSchema.AddBoolean('subscribe', 'Subscribe?');
  LSchema.AddEnum<TLang>('lang', 'Language', True);
  LSchema.AddSet<TPerms>('perms', 'Permissions').MaxItems := 2;

  Result := TMCPResponse<string>.Needs(
    TMCPInput.New(LState).Elicit('form', 'Tell us about yourself', LSchema));
finally
  LSchema.Free;
end;
```

Every `Add*` returns its property, so the JSON Schema keywords that variant allows are set on the result: `MinLength`/`MaxLength`/`Format` on a string, `Minimum`/`Maximum` on a number, `MinItems`/`MaxItems` on a set, `DefaultValue` on any of them. Both classes descend from `TMCPRequestedSchema`, which is what `Elicit` and `TMCPElicitRequest.Form` take, so the two ways in are interchangeable at the call site.

`Elicit` leaves the schema in the caller's hands by default; pass `AOwnsSchema := True` to hand it over instead.

### URL Mode

Some interactions do not fit a form — a payment, an OAuth consent, a signature. `ElicitUrl` asks the client to open one:

```pascal
TMCPInput.New(LState).ElicitUrl('pay', 'Complete the payment', LCheckoutUrl)
```

A url-mode elicitation carries no schema, and a form-mode one must carry one; `TMCPElicitRequest.Validate` enforces both, since one Delphi class carries the two shapes and cannot say so by declaration.

## Reading the Answer

The client retries the original request with `inputResponses` and the `requestState`. The readers are a class helper over `TInputResponses`, reached through the request params the tool has injected with `[Context]`:

```pascal
type
  TTodoTool = class
  private
    [Context] FParams: TCallToolRequestParams;
  ...
```

```pascal
// the whole answer, into the record that asked for it
LAnswer := FParams.InputResponses.StructAs<TDeleteAsk>('delete');
if not LAnswer.Confirm then
  Exit(TMCPResponse<string>.Value('Nothing deleted'));
```

| Reader | Reads |
|---|---|
| `Outcome(key)` | `Absent` / `Accepted` / `Declined` / `Cancelled` |
| `Accepted(key)` | whether it came back accepted |
| `Confirmed(key)` | the yes/no a `Confirm` asked for |
| `FieldAsString` / `Integer` / `Double` / `Boolean` / `Strings` | one member, coerced |
| `TryFieldAs*` | the same, reporting whether it was there |
| `FieldAs<T>` / `TryFieldAs<T>` | one member as any Delphi type — the enum or set a choice asked with |
| `StructAs<T>` / `TryStructAs<T>` | the whole content as the record `Ask<T>` asked with |
| `FieldsAs<T>` | the whole content into a new class instance (the caller owns it) |
| `ElicitationFor` / `SamplingFor` / `RootsFor` | the raw typed result, for anything the helpers do not cover |

**Every reader is forgiving.** The content was written by a client, so a missing key, a missing member, a value of the wrong shape or a malformed document yields the default rather than an exception. A decline or a cancel carries no content at all, so it reads as an empty answer everywhere — which is usually the same branch as an explicit "no", and `Outcome` is there for when the three have to be told apart.

## The requestState

The `requestState` is the opaque token the server hands to the client and gets back on the retry — the natural place for the context of the round trip: which record was asked about, when, what the server must remember. `TMCPRequestState` turns a Delphi object into one:

```pascal
// ask
LContext := TDeleteContext.Create;
try
  LContext.TaskId := AId;
  Exit(TMCPResponse<string>.Needs(
    TMCPInput.New(TMCPRequestState.Encode(LContext))
      .Ask<TDeleteAsk>('delete', Format('Delete task #%d?', [AId]))));
finally
  LContext.Free;
end;

// retry
if not FParams.TryStateAs<TDeleteContext>(LContext) or (LContext.TaskId <> AId) then
  Exit(TMCPResponse<string>.Ready(
    TCallToolReply.Fail('This confirmation belongs to another request')));
```

`Encode` Neon-serializes the object and Base64s it under an `MCPRS1.` marker; `Decode<T>` / `TryDecode<T>` read it back, and `TInputRequestParams.StateAs<T>` / `TryStateAs<T>` are the same thing on the retry's params. `IsRequestState` tells a state this codec wrote from a foreign one before anything tries to decode it.

::: warning The unsigned form is obfuscation, not protection
Base64 is reversible by anyone. **When what the state says can influence what the server does, encode and decode it with a secret**: `Encode(obj, secret)` / `Decode<T>(state, secret)` (marker `MCPRS1S.`) carry an HMAC-SHA256 over the payload, verified before decoding, so a state that was tampered with is refused rather than trusted. The library does not sign it for you.
:::

Note the second half of the retry check above: the state is decoded back into the context and compared, rather than trusting the repeated argument. An answer given to one question says nothing about another.

## What the Client Must Support

An interim result may only ask for what the client said it can do. `TMCPApi.RequireInputCapabilities` refuses one that asks for more with `MissingRequiredClientCapability` (-32021) and HTTP 400, reading the declaration from the per-request `_meta`:

```json
"_meta": {
  "io.modelcontextprotocol/protocolVersion": "2026-07-28",
  "io.modelcontextprotocol/clientCapabilities": { "elicitation": {} }
}
```

A mode-less `"elicitation": {}` counts as declaring every mode. The check is skipped entirely when `_meta` validation is `Off`, since nothing read the declaration in that case.

## A Complete Example

`Demo/MCPServer/MCPServer.Tools.pas` has the whole round trip in one method: `delete_task` is declared `TMCPResponse<string>`, asks with `Ask<TDeleteAsk>` on the first call, carries the task id in a signed-capable `requestState`, reads the retry with `StructAs<TDeleteAsk>`, logs the reason the user gave, and deletes only when the answer says so. The first call answers:

```json
{"result":{"inputRequests":{"delete":{"method":"elicitation/create","params":{
  "message":"Delete task #1?","mode":"form","requestedSchema":{ ... }}}},
  "requestState":"MCPRS1.eyJ0YXNrSWQiOjF9",
  "resultType":"input_required"},"id":1,"jsonrpc":"2.0"}
```

and the retry that brings `{"action":"accept","content":{"confirm":true,"reason":"already bought"}}` answers `Task #1 "Buy milk" deleted`.

`Demo/MCPServerMetricsClient/MCPClient.ps1` drives that exchange from the command line over Streamable HTTP, and doubles as a reference for the 2026-07-28 request contract.

## Notes and Limits

- **The box is a carrier, not a wrapper type.** It exists between the method and the invoker and nowhere else: it is never serialized, never part of a schema, and the payload's own conversion rules apply untouched.
- **`structuredContent` may be any JSON value** since SEP-2106, so a `structured` tool may box a record, a list of rows, a string or a number. `TBytes` is the one exclusion and stays a base64 blob.
- **The elicitation schema subset is the protocol's, not the library's.** When a form needs a nested object, ask for the parts as separate flat members, or use URL mode.
- **Nothing in the round trip is authenticated by default.** The `requestState` must be signed if it influences authorization, and the answers themselves are client input: validate them as you would any other.
