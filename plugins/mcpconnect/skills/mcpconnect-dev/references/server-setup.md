# Server setup

Everything here is verified against `Source/`. Contents:

- [Prerequisites and project search path](#prerequisites-and-project-search-path)
- [Choosing a transport](#choosing-a-transport)
- [Stdio](#stdio)
- [WebBroker](#webbroker)
- [Indy](#indy)
- [The shared configurator unit](#the-shared-configurator-unit)
- [The `IMCPConfig` sections](#the-imcpconfig-sections)
- [Message handling and lifecycle callbacks](#message-handling-and-lifecycle-callbacks)
- [Runtime (un)registration](#runtime-unregistration)

## Prerequisites and project search path

Delphi 11 Alexandria or newer (tested on 11, 12, 13). Two external libraries are required and are not vendored:

- [Neon](https://github.com/paolo-rossi/delphi-neon) — serialization and JSON schema generation
- [Logify](https://github.com/delphi-blocks/Logify) — logging

Add the `Source` directory of MCPConnect, Neon and Logify to the project's search path. In the demos these live as sibling checkouts under `Libs/`.

MCPConnect ships design-time packages under `Packages/11AndLater/` (`MCPConnect.dpk` core, `MCPConnectVCL.dpk` for the VCL content writers), but a server can equally be built by putting the source directories on the search path with no package installed.

## Choosing a transport

| Transport | Unit | Use when |
|-----------|------|----------|
| **stdio** | `MCPConnect.Transport.Stdio` | The MCP client spawns the server process (Claude Desktop and similar local integrations). Console app, one client per process, no network configuration at all. |
| **HTTP / WebBroker** | `MCPConnect.Transport.WebBroker` | The server should be reachable over the network, and you want to deploy as standalone / ISAPI / Apache module / CGI, or to add MCP to an existing WebBroker app. |
| **HTTP / Indy** | `MCPConnect.Transport.Indy` | Network-reachable, self-hosted, and you want direct control of the HTTP server: TLS, bindings, thread pool, connection events. |

Both HTTP transports speak Streamable HTTP and support Server-Sent Events for server→client messages, with resumable delivery via `Last-Event-ID`. WebBroker's SSE support depends on the host being able to stream a response; the Indy transport streams natively.

The MCP-facing code — tool classes, resource classes, the whole `IMCPConfig` block — is identical across all three. Only the host differs, which is why the demos keep configuration in one shared unit and provide three thin hosts around it.

## Stdio

A console application. `TJRPCStdioServer` creates and owns its own `TJRPCServer`, exposed as the `JRPCServer` property.

```pascal
program MyMCPServerStdio;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MCPConnect.JRPC.Server,
  MCPConnect.MCP.Server.Api,      // registers the standard MCP methods
  MCPConnect.Transport.Stdio,
  MCPConnect.Configuration.MCP,
  MCPConnect.Content.Writers.RTL,
  MyApp.Config;                   // TServerConfigurator

procedure StartServer;
var
  LServer: TJRPCStdioServer;
begin
  LServer := TJRPCStdioServer.Create(nil);
  try
    TServerConfigurator.ConfigureServer(LServer.JRPCServer);
    LServer.StartServerAndWait;
  finally
    LServer.Free;
  end;
end;

begin
  try
    StartServer;
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
```

`StartServerAndWait` blocks until the client closes the connection. To interleave MCP processing with other work, drive the loop yourself:

```pascal
LServer.StartServer;
while not LServer.Terminated do
begin
  LServer.ProcessRequests;   // handles pending messages, returns immediately
  Sleep(100);
  DoSomethingElse;
end;
```

Never write to stdout: it is the protocol channel. Diagnostics go to `ErrOutput` or a log file. The transport reads and writes UTF-8 regardless of the ANSI codepage.

Sessions are implicit — one process, one session — so `[Context]` session injection works in a stdio server without any session configuration.

## WebBroker

Create a **Web Server Application** (File → New → Other → Web). `TJRPCDispatcher` registers itself with the owning `TWebModule` through standard component ownership, so creating it with the web module as owner is the whole wiring:

```pascal
uses
  Web.HTTPApp,
  MCPConnect.JRPC.Server,
  MCPConnect.MCP.Server.Api,
  MCPConnect.Transport.WebBroker,
  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,
  MyApp.Config;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FJRPCServer := TJRPCServer.Create(Self);

  TServerConfigurator.ConfigureServer(FJRPCServer);

  FJRPCDispatcher := TJRPCDispatcher.Create(Self);   // Self = TWebModule: auto-registers
  FJRPCDispatcher.PathInfo := '/mcp';
  FJRPCDispatcher.Server := FJRPCServer;
end;
```

For each request WebBroker walks its dispatchers and routes to the one whose `PathInfo` matches, so the MCP endpoint lives at `/mcp` alongside any other web actions the application already has. The deployment target is chosen at project creation and can be changed later without touching MCP code.

## Indy

A VCL (or console) application embedding `TIdCustomHTTPServer`. `TJRPCIndyServer.CreateMCPServer` builds the server with its `JRPCServer` already in place:

```pascal
uses
  MCPConnect.JRPC.Server,
  MCPConnect.MCP.Server.Api,
  MCPConnect.Transport.Indy,
  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL,
  MyApp.Config;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FServer := TJRPCIndyServer.CreateMCPServer(Self);
  TServerConfigurator.ConfigureServer(FServer.JRPCServer);
end;

procedure TfrmMain.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;
```

`TJRPCIndyServer` descends from `TIdCustomHTTPServer`, so every Indy property and event is available: `IOHandler` for TLS, `Bindings`, thread pool settings, and so on.

## The shared configurator unit

Keeping configuration in one class lets the same MCP surface be hosted by any transport, and gives a single obvious place to add a feature later:

```pascal
unit MyApp.Config;

interface

uses
  MCPConnect.JRPC.Server;

type
  TServerConfigurator = class
    class procedure ConfigureServer(AServer: TJRPCServer);
  end;

implementation

uses
  System.IOUtils,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types,
  MCPConnect.Configuration.MCP,
  MCPConnect.Content.Writers.RTL,
  MyApp.Tools,
  MyApp.Resources;

class procedure TServerConfigurator.ConfigureServer(AServer: TJRPCServer);
var
  LDataPath: string;
begin
  LDataPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');

  AServer
    .Plugin.Configure<IMCPConfig>
      .Server
        .SetName('my-mcp-server')
        .SetVersion('1.0.0')
        .SetIconFolder(TPath.Combine(LDataPath, 'icons'))
        .RegisterWriter(TMCPStreamWriter)
        .RegisterWriter(TMCPStringListWriter)
      .BackToMCP

      .Security
        .SetCORS(True)
        .SetAllowedMethods(['POST'])
      .BackToMCP

      .Resources
        .SetBasePath(LDataPath)
        .RegisterClass(TWeatherResource)
        .RegisterFile('index.md', 'Documentation index')
      .BackToMCP

      .Tools
        .RegisterClass(TTicketTool)
      .BackToMCP
  ;
end;

end.
```

## The `IMCPConfig` sections

`TJRPCServer.Plugin.Configure<T>` is a GUID-keyed plugin registry. `IMCPConfig` (`MCPConnect.Configuration.MCP`) is the MCP plugin; each of its sub-sections returns to the parent with `.BackToMCP`, and other plugins close with `.ApplyConfig` before the next `.Plugin.Configure<…>`.

### `.Server`

| Method | Notes |
|--------|-------|
| `SetName(name)` | Server name in the `initialize` response |
| `SetDescription(text)` | Optional server description |
| `SetVersion(version)` | Version string |
| `SetCapabilities(...)` | **Usually omit.** If unset, capabilities are inferred from what is actually registered. Overloads take `TMCPCapabilities` (`[Tools, Resources, Prompts, Tasks, Logging, Completions]`), a `TServerCapabilities` object, or a `TProc<TServerCapabilities>` for fine detail such as `ACapabilities.Tools.ListChanged := True` |
| `SetIconFolder(path)` | Root folder for the icons named by `icon=` tags |
| `SetScopeSeparator(sep)` | Separator between scope and tool name; `_` by default |
| `RegisterWriter(class)` | Content writer — see `sessions-notifications.md` |

### `.Security`

`SetCORS(bool)`, `SetAllowedMethods([...])`, `SetAllowedOrigins([...])`, `SetCookieSecure(bool)`. See `security.md`.

### `.Tools` / `.Resources` / `.Prompts`

Registration surfaces — see `tools.md` and `resources-prompts.md`.

### `.MessageHandling`

Protocol-level hooks — see below.

## Message handling and lifecycle callbacks

```pascal
.MessageHandling
  .OnInitialized(
    procedure (AContext: TJRPCContext)
    begin
      Logger.LogDebug('Client finished the initialize handshake');
    end
  )
  .OnCancelled(
    procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
    begin
      Logger.LogDebug('Client cancelled request %s', [AParams.RequestId.ToString]);
    end
  )
  .OnSetLogLevel(
    procedure (AContext: TJRPCContext; ALevel: TLogSetLevel)
    begin
      SetMinimumSeverity(ALevel);
    end
  )
.BackToMCP
```

`RegisterApi(AClass)` goes further: it registers a class that takes over a whole JSON-RPC namespace, overriding the built-in handler. A class marked `[JRPC('notifications')]` replaces `TMCPNotificationsApi` entirely — and while it is registered the typed `OnInitialized` / `OnCancelled` callbacks above are **not** invoked, because the built-in API that would fire them has been bypassed. Use the callbacks for ordinary work and `RegisterApi` only when you need to reshape the protocol surface itself.

## Runtime (un)registration

Everything registered can be removed while the server runs, which is how a server swaps feature sets in and out:

```pascal
AServer.Plugin.Configure<IMCPConfig>
  .Tools
    .UnregisterClass(TTicketTool)      // or UnregisterTool('tickets_search')
    .ClearAll                          // everything at once
  .BackToMCP
  .Resources
    .UnregisterClass(TWeatherResource)
    .UnregisterFile('index.md')
    .UnregisterResource('text://weather')
    .UnregisterTemplate('res://items/{id}')
  .BackToMCP
  .Prompts
    .UnregisterPrompt('simple-prompt')
  .BackToMCP
;
```

After changing the published set at runtime, tell connected clients by enqueueing the matching notification — `TToolListChangedNotification`, `TResourceListChangedNotification`, `TPromptListChangedNotification` (see `sessions-notifications.md`). Without it, clients keep using the list they fetched at connection time.
