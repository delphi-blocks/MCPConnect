# Resources, templates, App UIs and prompts

Where tools *do* things, resources expose data the client can read, and prompts hand the client ready-made conversation starters. Contents:

- [Class-based resources](#class-based-resources)
- [Resource templates](#resource-templates)
- [Static files](#static-files)
- [MIME types and custom schemes](#mime-types-and-custom-schemes)
- [MCP App UIs](#mcp-app-uis)
- [Prompts](#prompts)
- [Programmatic registration](#programmatic-registration)

All of these are registered in `.Resources` / `.Prompts` and, like tools, get a fresh class instance per request.

## Class-based resources

`[McpResource(name, uri, mimeType, description)]` on a **parameterless function**:

```pascal
type
  TWeatherResource = class
  public
    [McpResource('weather-resource', 'text://weather', 'text/plain',
      'Current weather for the conference venue')]
    function GetWeatherInfo: string;
  end;
```

```pascal
.Resources
  .RegisterClass(TWeatherResource)
.BackToMCP
```

Constraints enforced at registration time:

- the method takes **no parameters** (`Resource's method cannot have parameters`) — a resource that needs an argument is a *template*, see below;
- the URI must contain no `{placeholder}` (`Resource uri cannot have template parameters`);
- if the MIME type is omitted it is inferred from the URI's extension where possible, and registration fails if it cannot be determined.

Return types follow the same rules as tools: scalars and objects are serialized, `TStream` needs `TMCPStreamWriter`, and so on (see `tools.md`).

Tags on resources: `disabled` hides the resource from `resources/list` while leaving it registered.

## Resource templates

A template is a resource with placeholders in its URI, so a client can read one of many items through a single declaration:

```pascal
type
  TWeatherResource = class
  public
    [McpTemplate('weather-city', 'demo://weather.app/{city}', 'text/plain',
      'Weather forecast for the named city')]
    function GetWeatherCity(
      [McpParam('city', 'City to forecast, e.g. "Rome"')] const ACity: string
    ): string;
  end;
```

The rules the registration checks, each with its own error message:

- the URI **must** contain at least one `{placeholder}`;
- the method's parameters must correspond exactly to the placeholders — same count, same names after `[McpParam]` mapping;
- every parameter must carry `[McpParam]`;
- parameter types are restricted to what can be parsed out of a URI segment (strings, integers, and similar scalars).

Placeholder names come from `[McpParam]`, not from the Pascal parameter name, so `{city}` matches `[McpParam('city', …)] const ACity: string`.

## Static files

Files served straight from disk, relative to `SetBasePath`:

```pascal
.Resources
  .SetBasePath(TPath.Combine(ExtractFilePath(ParamStr(0)), 'data'))
  .RegisterFile('index.md', 'Documentation index')
  .RegisterFile('documentation\mcp\mcpconnect.pdf', 'MCPConnect introduction', 'application/pdf')
.BackToMCP
```

The third argument is an explicit MIME type; without it the extension decides, and registration fails with `No MIME type found for [.xyz] extension` for anything unknown. The file must exist when the server starts — registration checks. Remove one later with `UnregisterFile('index.md')`.

## MIME types and custom schemes

```pascal
.Resources
  .AddMimeType(TMimeEncoding.Text, 'text/x-pascal', '.pas')
  .RegisterScheme('docs', 'C:\Data\Documentation')
.BackToMCP
```

`AddMimeType` teaches the resolver a new extension→MIME mapping (`TMimeEncoding.Text` or `.Binary` decides text vs base64 delivery). `RegisterScheme` maps a URI scheme onto a folder so `docs://guide.md` resolves under that root.

## MCP App UIs

An App UI is a resource served under the `ui://` scheme whose content — normally a self-contained HTML page — the client renders as an interactive widget. The scheme is mandatory: anything else raises `Apps UI uri must use the "ui://" scheme`.

```pascal
type
  TTicketAppUI = class
  public
    [McpAppUI('ticket-app', 'ui://myapp/ticket-app', 'Interactive ticket picker')]
    function GetUI: string;
  end;

function TTicketAppUI.GetUI: string;
begin
  Result := TFile.ReadAllText(
    TPath.Combine(TPath.GetAppPath, 'data', 'ticket-app.html'));
end;
```

Register it in `.Resources` like any other resource class. The method takes no parameters (`App's method cannot have parameters`).

To have a tool's result rendered inside that UI, link the two — the two forms are equivalent:

```pascal
[McpTool('get_tickets', 'List available tickets')]
[McpApp('ui://myapp/ticket-app')]
function GetTickets: TTickets;

// or, as a tag
[McpTool('get_tickets', 'List available tickets', 'app=ui://myapp/ticket-app')]
function GetTickets: TTickets;
```

Registering a UI programmatically also lets you set the widget's runtime metadata — content security policy, permissions, preferred border — through the optional configurator:

```pascal
.RegisterUI(TTicketAppUI, 'GetUI', 'ticket-app', 'ui://myapp/ticket-app',
  'Interactive ticket picker', '',
  procedure (AResource: TMCPResource; AUI: TUIResourceUI)
  begin
    AUI.Domain := 'https://myapp.example';
    AUI.PrefersBorder := True;
  end)
```

Since the page is HTML rendered inside someone's client, keep it self-contained and treat anything it displays from your data as untrusted text.

## Prompts

A prompt is a reusable message template the user picks from their client. `[McpPrompt(name, title, description)]`, with arguments annotated `[McpArgument]`:

```pascal
type
  TSamplePrompts = class
  public
    [McpPrompt('simple-prompt', 'Simple Prompt', 'A prompt with no arguments')]
    function SimplePrompt: string;

    [McpPrompt('weather-prompt', 'Weather', 'Ask about the weather in a place')]
    function WeatherPrompt(
      [McpArgument('city', 'Name of the city', 'required')] const ACity: string;
      [McpArgument('country', 'Name of the country')] const ACountry: string
    ): string;
  end;
```

```pascal
.Prompts
  .RegisterClass(TSamplePrompts)
.BackToMCP
```

Unlike tool parameters, **prompt arguments are optional by default** — add the `required` tag to the ones that are not. An argument the user leaves out arrives as an empty value, so handle it:

```pascal
function TSamplePrompts.WeatherPrompt(const ACity, ACountry: string): string;
begin
  var LPlace := IfThen(ACountry.IsEmpty, ACity, ACity + ', ' + ACountry);
  Result := Format('What''s the weather in %s?', [LPlace]);
end;
```

Returning a `string` produces a single user message. For a multi-turn or multi-content prompt, return `TPromptMessages` (or a fully built `TGetPromptResult`) and the framework passes it through untouched:

```pascal
function TSamplePrompts.ReviewPrompt(const AUnit: string): TPromptMessages;
begin
  Result := TPromptMessages.Create;
  Result.AddText('user', 'Review this Delphi unit for correctness and style.');
  Result.AddText('assistant', 'Sure — send me the source.');
  Result.AddText('user', TFile.ReadAllText(AUnit));
end;
```

`TPromptMessages` also has `AddImage`, `AddAudio`, `AddBlob` and `AddLink`, each taking the role (`'user'` or `'assistant'`) first. Prompts support the `icon=` and `disabled` tags.

## Programmatic registration

For classes that cannot carry attributes, describe them at configuration time. Mixes freely with `RegisterClass`.

```pascal
.Resources
  .RegisterResource(TCppResource, 'GetGlobalInfo',
    'info-resource', 'text://info', 'text/plain', 'Global application info')

  .RegisterTemplate(TCppResource, 'GetItem',
    'item', 'res://items/{id}', ['id'], 'application/json', 'Read one item by id')

  .RegisterUI(TCppApp, 'GetUI', 'ticket-app', 'ui://myapp/ticket-app', 'Ticket picker')
.BackToMCP

.Prompts
  .RegisterPrompt(TCppPrompts, 'WeatherPrompt', 'weather-prompt',
    [TMCPPromptArgConfig.New('ACity', 'city', 'Name of the city', True),
     TMCPPromptArgConfig.New('ACountry', 'country', 'Name of the country')],
    'Weather', 'Ask about the weather in a place')
.BackToMCP
```

The shape is consistent throughout: the Delphi class, the Delphi method name, then the MCP-facing name and metadata. In `RegisterTemplate` the `['id']` array maps positionally to the method's parameters and must match the URI placeholders. In `TMCPPromptArgConfig.New` the first argument is the **Pascal parameter name**, the second the MCP argument name, and the trailing `True` marks it required.
