# MCPClient - console client demo

A console MCP client built on the MCPConnect client units. It connects to a
2026-07-28 server, walks the client API and prints what comes back:

- `server/discover` (server name, version, protocol, instructions)
- paged `tools/list`, `resources/list`, `resources/templates/list`, `prompts/list`
- a typed `tools/call` (`TCallToolResult`, with every content shape)
- `resources/read` (`TTextResourceContents` / `TBlobResourceContents`)
- `prompts/get` (`TPromptMessage<T>` messages)
- MRTR: `OnElicitation`, `OnSampling` and `OnRoots` answer the server's requests
- over HTTP, a `subscriptions/listen` stream (`IMCPSubscription`)

It replaces the earlier raw-RTL client that used to live in this folder: that
one showed the wire format with no MCPConnect units, this one shows the new
classes. `Demo/api` still has the raw requests.

## Build

From this folder, with Delphi 13 on the path:

```
msbuild MCPClient.dproj /t:Build /p:config=Release /p:platform=Win32
```

or open `MCPClient.dproj` in the IDE. The executable lands in
`Win32\Release\MCPClient.exe`. The project compiles the client units straight
from `..\..\Source`, so there is nothing to install.

## Run

Against the Indy demo server (it listens on `http://localhost:8080/`):

```
MCPServerIndy.exe
MCPClient.exe
```

Against the stdio demo server:

```
MCPClient.exe --stdio "..\MCPServerStdio.exe"
```

The default is a tour: list everything, read the first resource, get the first
prompt, then call `list_tasks` (or the first tool that takes no required
arguments). Each section prints the typed result the client produced.

## Options

```
--http URL          MCP endpoint (default http://localhost:8080/)
--stdio COMMAND     launch COMMAND and speak MCP on its stdin/stdout
--token TOKEN       bearer token for HTTP
--list              only list tools/resources/prompts, then stop
--read URI          read a resource (default: the first one listed)
--call NAME         call a tool
--args JSON         arguments for --call
--args-file FILE    read the arguments from a JSON file (handy on the shell)
--prompt NAME       get a prompt (default: the first one listed)
--prompt-args JSON  arguments for --prompt
--listen SECONDS    open a subscriptions/listen stream and report
--help              usage
```

Examples:

```
MCPClient.exe --call add_task --args "{\"title\":\"Try the demo\"}"
MCPClient.exe --call add_task --args-file add.json
MCPClient.exe --prompt weekly-review --prompt-args "{\"focus_area\":\"work\"}"
MCPClient.exe --read "text://todo/summary"
MCPClient.exe --listen 10
```

## MRTR

The handlers are always installed. The demo server in this repository has its
MRTR tools commented out; to see them, uncomment the registration in
`Demo/MCPServer/MCPServer.Config.pas`:

```pascal
.Tools
  .RegisterClass(TTodoMrtrTools)
  //.RegisterClass(TTodoTools)
```

Then, with the server restarted:

```
MCPClient.exe --call delete_task --args-file delete.json
```

where `delete.json` is `{"task_id":1}`. The client prints the form the server
sends, answers it with a plausible value per field, and retries with the
answer; the server returns the deletion result. `summarize_tasks` exercises
`OnSampling`, and `draft_day_plan` asks for both roots and sampling.

## Listening

`--listen` opens the stream, then adds a task so the demo server publishes
`notifications/resources/updated` for `text://todo/summary`; the notification
is printed and the stream is closed after the requested time. Each listen uses
its own transport (`TMCPClient.TransportFactory`), so the client's own
transport remains free for the `add_task` call. Over stdio, `Listen` is not
implemented yet and the demo reports that instead of failing.
