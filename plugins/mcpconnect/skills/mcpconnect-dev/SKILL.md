---
name: mcpconnect-dev
description: Build MCP (Model Context Protocol) servers in Delphi / Object Pascal with the MCPConnect framework — create a new server from scratch (stdio, WebBroker or Indy transport) or add tools, resources, resource templates, MCP App UIs and prompts to an existing one, plus sessions, notifications, content writers and OAuth/token security. Use this skill whenever the work involves MCPConnect, TJRPCServer, IMCPConfig, TJRPCStdioServer / TJRPCIndyServer / TJRPCDispatcher, or the [McpTool] / [McpParam] / [McpResource] / [McpTemplate] / [McpAppUI] / [McpPrompt] / [McpScope] / [Context] attributes — and also whenever someone asks to expose Delphi code to an LLM, write a Delphi MCP server, add a tool to a Delphi MCP server, or connect a Pascal application to Claude / an MCP client, even if they never say "MCPConnect".
---

# Developing with MCPConnect

MCPConnect turns ordinary Delphi classes into an MCP server. You annotate methods with attributes, register the classes in a fluent configuration block, and the framework does the rest: it walks the RTTI, generates JSON schemas via Neon, binds incoming JSON arguments to your parameters, and serializes whatever you return into MCP content.

Two things must always line up, and forgetting either one is the single most common failure:

1. the **attribute** on the method (what the tool/resource/prompt looks like to the client), and
2. the **registration** of its class in the config block (whether the server knows the class exists at all).

An annotated class that is never registered is invisible. A registered class with no annotated methods contributes nothing.

## Which reference to read

Read the reference file that matches the task before writing code — these files carry signatures verified against `Source/`, and the published documentation is known to lag behind the code.

| Task | Read |
|------|------|
| New server project, choosing a transport, wiring `.dpr` / WebModule / form, project search paths | `references/server-setup.md` |
| Adding or changing tools: attributes, parameters, return types, tags, error reporting, programmatic registration | `references/tools.md` |
| Resources, resource templates (`res://items/{id}`), MCP App UIs, static files, prompts | `references/resources-prompts.md` |
| Sessions, `[Context]` injection, server→client notifications and SSE, memory ownership, content writers, Neon tuning | `references/sessions-notifications.md` |
| Static bearer tokens, OAuth 2.1 resource server, JWT validation, CORS | `references/security.md` |

For anything not covered there, read the unit in `Source/` directly — `MCPConnect.Configuration.MCP.pas` is the config surface, `MCPConnect.MCP.Attributes.pas` the attribute list, `MCPConnect.MCP.Types.pas` the protocol types. The `Demo/MCPServer` folder is a working server exercising nearly every feature and is the best place to copy a pattern from.

Delphi style — naming prefixes, `const` params, uses-clause ordering — follows the project's `delphi-conventions` skill. Apply it to every unit you write here.

## The 30-second version

A tool class, and the config that publishes it:

```pascal
unit MyApp.Tools;

interface

uses
  System.SysUtils,
  MCPConnect.MCP.Attributes;

type
  [McpScope('tickets')]
  TTicketTool = class
  public
    [McpTool('search', 'Search tickets by customer name')]
    function Search(
      [McpParam('customer', 'Full or partial customer name')] const ACustomer: string
    ): string;
  end;
```

```pascal
AServer
  .Plugin.Configure<IMCPConfig>
    .Server
      .SetName('my-mcp-server')
      .SetVersion('1.0.0')
    .BackToMCP
    .Tools
      .RegisterClass(TTicketTool)   // publishes tickets_search
    .BackToMCP
  ;
```

That is a complete, working tool. Everything else in this skill is refinement on top of it.

## Working rules

**Extend the existing configuration, don't invent a parallel one.** Real projects concentrate configuration in a single place — in the demos it is `TServerConfigurator.ConfigureServer`. When adding a feature to an existing server, find that block and add a line to the right section rather than creating a second `.Plugin.Configure<IMCPConfig>` chain somewhere else.

**Write descriptions for the LLM, not for the compiler.** The `description` in `[McpTool]` and `[McpParam]` is the only thing a model sees when deciding whether and how to call your code. `'Search tickets by customer name; returns id, title and status for each match'` earns its place; `'search'` does not. This is worth more attention than any other single string in the file.

**Only expose what is safe to expose.** Every registered `[McpTool]` method is callable by a language model on behalf of whoever is connected. Prefer read-only tools; where a tool writes, deletes or spends money, say so in the description and mark it with the `destructive` tag so clients can prompt the user. If a class mixes internal helpers with published methods, that is fine — methods without `[McpTool]` are entirely invisible.

**Check the whole chain when something doesn't show up.** In order: is the attribute present; is the class registered; is the capability advertised; does the client see a fresh `tools/list`. Capabilities are inferred automatically from what is registered, so an explicit `SetCapabilities` that omits `Resources` will hide resources that are otherwise correctly registered — which is why leaving `SetCapabilities` out entirely is usually the better choice.

## Gotchas worth knowing before you write

These are verified against the current source and each one produces a confusing failure:

- **Every tool parameter needs `[McpParam]`.** A single un-annotated parameter makes registration of the whole class raise `Non-annotated params are not permitted` at startup.
- **All tool parameters are required.** The generated input schema lists every parameter in `required`; there is no optional-parameter mechanism for tools. Prompt arguments are the exception — they are optional unless tagged `required`.
- **A tool must be a `function`.** A `procedure` raises `Tool must be a function`. Return `string` if there is nothing meaningful to say.
- **Tool names must match `^[a-zA-Z0-9_-]{1,64}$`.** With `[McpScope('shop')]` the published name becomes `shop_<toolname>`, and the scoped name must still fit the pattern.
- **`TToolResultBuilder` no longer exists.** Older documentation and code comments still show it. Build multi-part results with `TContentList.Create` and its `AddText` / `AddImage` / `AddAudio` / `AddBlob` / `AddLink` methods.
- **Objects you return are freed by the framework**, but only once they are returned. If an exception can fire between creating the object and leaving the method, register it with `[Context] FGC: IGarbageCollector` right after creating it.
- **Add `MCPConnect.MCP.Server.Api` to the uses clause of some unit in the project.** Its `initialization` section is what registers the standard MCP methods (`initialize`, `tools/*`, `resources/*`, `prompts/*`, …). Without it the server answers nothing.
- **The content writers you need must be registered.** `RegisterWriter(TMCPStreamWriter)` for `TStream` results, `TMCPStringListWriter` for `TStringList`, and the VCL pair `TMCPImageWriter` / `TMCPPictureWriter` for images — the latter only in VCL projects.
- **A stdio server must keep stdout clean.** It is the protocol channel. Any stray `Writeln` corrupts the stream; send diagnostics to `ErrOutput` or a log file.

## Verifying

There is no CLI build for this project set — compilation goes through `msbuild` with a Delphi install, or the IDE. When you have changed library code and want a compile check, the test project has ready-made scripts:

```
Tests\MCPConnect.Tests.Build.120.bat   # Delphi 12 Athens
Tests\MCPConnect.Tests.Build.130.bat   # Delphi 13 Florence
```

For changes to a demo or a user's own server, building the relevant `.dproj` with `msbuild` is the equivalent check. If no Delphi toolchain is reachable, say so plainly rather than claiming the code compiles — and in that case re-read the new unit against the reference file, since the usual compile-time safety net is absent.

A running HTTP server can be exercised end to end with the Bruno collections in `Demo/api/MCP`, or by pointing any MCP client at the endpoint.
