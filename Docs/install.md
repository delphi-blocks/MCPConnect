# Installing the Server in the MCP Agents

A server is only useful once a client knows about it, and every MCP client keeps its servers somewhere else: Claude Code in `~/.claude.json`, Codex in a TOML file, VS Code in a `mcp.json` with its own key, Cursor in another `mcp.json`. Asking your users to edit those files by hand, with the right format and the right path to the executable, is where most installations go wrong.

MCPConnect can make the server install itself. One line in the `.dpr` and the executable understands a few extra switches:

```
FireLinkMCP.exe /mcp:install
```

registers the server in every MCP agent found on the machine, and `/mcp:uninstall` removes it again.

::: info
The self-installation (`TMCPInstaller`) is available only in the latest version of MCPConnect on [GitHub](https://github.com/delphi-blocks/MCPConnect).
:::

## Enabling the Installer

The installer is opt-in: add the `MCPConnect.Install` and `MCPConnect.Install.Agents` units to the project and call `TMCPInstaller.HandleCommandLine` at the start of the program, before the server is created:

```pascal
program FireLinkMCP;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,

  MCPConnect.Install,
  MCPConnect.Install.Agents,
  MCPConnect.Transport.Stdio;

begin
  try
    // "FireLinkMCP /mcp:install" registers this server in the MCP agents
    // found on the machine (/mcp:help for the options)
    if TMCPInstaller.HandleCommandLine then
      Exit;

    StartServer;
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
```

`HandleCommandLine` returns `True` when the command line held an installer command, which it has already executed: the program should then end without starting the server. With no installer switch it returns `False` and the server starts as usual, so an agent launching the executable is not affected.

::: tip New projects
The MCPConnect project wizard writes these lines for you: for a console application, leave **Install the server in the MCP agents from the command line** checked in the *Options* page (it is checked by default). For an HTTP transport the wizard also configures the url from the port and the path you entered (see [HTTP Servers](#http-servers)).
:::

The installer writes its report to the console, so the program must be a console application. For a STDIO server this is always the case, and printing is safe: at that point the executable is not speaking the MCP protocol yet.

## Commands

| Command | What it does |
|---|---|
| `/mcp:install` | Registers the server in the selected agents |
| `/mcp:uninstall` | Removes the registration from the selected agents |
| `/mcp:status` | Shows, for every agent, whether it is installed and whether the server is registered, in the global and in the project scope |
| `/mcp:agent` | Lists the supported agents and which of them are installed |
| `/mcp:help` | Shows the syntax |

Switches are accepted with `/`, `-` or `--` on every platform (`/mcp:install`, `-mcp:install`, `--mcp:install`), and an option value follows a space or an `=` (`/mcp:scope project`, `/mcp:scope=project`). The help prints `/` on Windows and `--` elsewhere.

## Options

| Option | Values | Default |
|---|---|---|
| `/mcp:agent` | Agent id, repeatable or comma separated | All the installed agents |
| `/mcp:scope` | `global` or `project` | `global` |
| `/mcp:name` | Server name: letters, digits, `-` and `_` | The executable name, without extension |
| `/mcp:env` | `KEY=VALUE`, repeatable | — |

Some examples:

```
FireLinkMCP.exe /mcp:install
FireLinkMCP.exe /mcp:install /mcp:agent claude-code,codex /mcp:scope project
FireLinkMCP.exe /mcp:install /mcp:agent cursor /mcp:agent vscode /mcp:env FB_HOST=localhost /mcp:env FB_PORT=3050
FireLinkMCP.exe /mcp:uninstall /mcp:agent codex
FireLinkMCP.exe /mcp:status
./FireLinkMCP --mcp:install --mcp:name firelink
```

The values of `/mcp:env` are stored in clear text in the configuration of the agent, which passes them to the server process. Keep secrets out of them, or document the risk for your users.

## Supported Agents

| Id | Agent | How the server is registered |
|---|---|---|
| `claude-code` | Claude Code | `claude mcp add-json` / `claude mcp remove` |
| `claude-desktop` | Claude Desktop | `claude_desktop_config.json` (global scope only) |
| `codex` | Codex | `codex mcp add` / `codex mcp remove`; `.codex/config.toml` for the project scope |
| `gemini` | Gemini CLI | `gemini mcp add` / `gemini mcp remove` |
| `opencode` | opencode | `opencode.json` |
| `cursor` | Cursor | `.cursor/mcp.json` |
| `vscode` | VS Code (Copilot) | `mcp.json` of the user profile, `.vscode/mcp.json` |

Ids are case-insensitive and the hyphen is optional: `ClaudeCode` and `VSCode` work too.

Where an agent has a non-interactive command line tool, the installer uses it, so the agent stays in charge of its own file (Claude Code rewrites `~/.claude.json` while it runs). The other agents get their file edited directly: the installer keeps every key it does not manage, saves a `.bak` copy first and writes the new file atomically. If a CLI command fails, the error is reported with the output of the CLI: the installer never falls back on editing the file behind the agent's back.

An agent is considered installed when its CLI is in the `PATH` or its configuration folder exists. Without `/mcp:agent` the installer works on every installed agent and lists the others as skipped; an agent named with `/mcp:agent` that is not installed is an error.

Each agent is handled independently: a failure on one does not stop the others, and the report ends with one line per agent:

```
Installing "FireLinkMCP" (scope: global, transport: stdio)
  Claude Code      installed
  Claude Desktop   installed - restart Claude Desktop
  Codex            installed
  Gemini CLI       ERROR: already registered as "firelink", use uninstall first
  opencode         installed
  Cursor           not detected, skipped
  VS Code          installed
```

The exit code is `1` when any agent failed.

## Global and Project Scope

The **global** scope (the default) makes the server available to the user in every folder. The **project** scope registers it in the configuration of the current folder, the file you commit with the project so that the whole team gets the server:

| Agent | Global | Project |
|---|---|---|
| Claude Code | `~/.claude.json` (scope `user`) | `.mcp.json` (scope `project`) |
| Claude Desktop | `claude_desktop_config.json` in the app data folder | not supported |
| Codex | `~/.codex/config.toml` (or `$CODEX_HOME`) | `.codex/config.toml` |
| Gemini CLI | `~/.gemini/settings.json` | `.gemini/settings.json` |
| opencode | `~/.config/opencode/opencode.json` (or `$XDG_CONFIG_HOME`) | `opencode.json` |
| Cursor | `~/.cursor/mcp.json` | `.cursor/mcp.json` |
| VS Code | `mcp.json` in the user folder of VS Code | `.vscode/mcp.json` |

Claude Code also has a `local` scope (private to the user, tied to a folder). The installer does not write to it, but `status` reports a registration found there, e.g. `no [local: firelink]`.

When the executable lives inside the project folder (`tools\FireLinkMCP.exe`), the project scope registers a path relative to the project, so that the configuration still works after a colleague clones the repository somewhere else. Each agent gets the form it understands:

| Agent | Command registered |
|---|---|
| Claude Code | `${CLAUDE_PROJECT_DIR:-.}/tools/FireLinkMCP.exe` |
| Cursor, VS Code | `${workspaceFolder}/tools/FireLinkMCP.exe` |
| Codex, Gemini CLI, opencode | `./tools/FireLinkMCP.exe` |

Some agents ask for a confirmation before they use a server added to a project: Claude Code asks to approve the servers of `.mcp.json` on first use, and Codex reads `.codex/config.toml` only in a trusted project. The installer reminds you of it after the install.

## Finding an Existing Registration

How the installer recognizes the server among those already registered depends on whether it has a name:

- **With a name**, given with `/mcp:name` or by the developer with `SetServerName`, it looks for that name.
- **Without one**, the server is registered under the executable name, but it is looked for **by executable**: the installer resolves the command of every entry (expanding the project variables and the relative paths) and compares it with the path of the running executable. A registration made by hand under another name is recognized, `status` shows its name (`yes (task)`) and `uninstall` removes it.

`install` fails on an agent where the server is already registered, with a message that names the registration found. With the default name it also fails when that name belongs to a different server; give another one with `/mcp:name`. `uninstall` on an agent where the server is not registered is only a warning.

## HTTP Servers

The installer is not limited to STDIO servers. For an HTTP server the agent does not start a process: it connects to a server that is already running, so the installer registers its **url** instead of the executable:

```pascal
TMCPInstaller.Config
  .SetTransport(TMCPInstallTransport.Http)
  .SetUrl('http://localhost:8080/mcp');   // the default url
```

Any console program can do this, not only the server itself: an Indy server running as a console application can install itself, while a WebBroker server hosted in IIS or a Windows service can ship a small console installer that holds just these lines and the call to `HandleCommandLine`.

The entries written for an HTTP server:

| Agent | Entry |
|---|---|
| Claude Code | `{"type": "http", "url": "…"}` |
| Codex | `url = "…"` (streamable HTTP) |
| Gemini CLI | `{"httpUrl": "…"}` |
| opencode | `{"type": "remote", "url": "…", "enabled": true}` |
| Cursor | `{"url": "…"}` |
| VS Code | `{"type": "http", "url": "…"}` |
| Claude Desktop | not supported: remote servers are added from the Connectors settings of the app |

Without a name the registration is looked for by url (case-insensitive, ignoring a trailing `/`). Environment variables make no sense without a process to pass them to: `/mcp:env` and `AddEnv` are an error with the HTTP transport. The project scope works as for STDIO; there is just no path to make relative.

::: tip Naming the installer
Windows assumes that an executable without a manifest whose name contains words like `install` or `setup` is an installer, and asks for administrator rights before running it. Delphi projects embed a manifest by default; if you turn it off, keep those words out of the name of a separate installer.
:::

## Configuring the Installer

`TMCPInstaller.Config` returns the installer configuration, a singleton with fluent setters. Configure it before calling `HandleCommandLine`:

```pascal
TMCPInstaller.Config
  .SetCommandPrefix('mcp:')         // '' accepts /install, --install, ...
  .SetServerName('firelink')        // default: the executable name, searched by executable
  .AddEnv('FIRELINK_LOG', 'warn')   // always registered, besides /mcp:env (STDIO only)
  .DisableAgent('claude-desktop');  // never touch this agent

if TMCPInstaller.HandleCommandLine then
  Exit;
```

| Method | Description |
|---|---|
| `SetCommandPrefix` | Prefix of the switches, `mcp:` by default. With an empty prefix the installer ignores the switches it does not know, which may belong to the application |
| `SetServerName` | Name of the server. The user can still override it with `/mcp:name` |
| `SetTransport` | `TMCPInstallTransport.Stdio` (default) or `TMCPInstallTransport.Http` |
| `SetUrl` | Url of the HTTP server, `http://localhost:8080/mcp` by default |
| `AddEnv` | Environment variable registered on every install, added to the `/mcp:env` ones |
| `DisableAgent` | Removes a built-in agent |
| `RegisterAgent` | Adds an agent of your own (see below) |

A second overload, `HandleCommandLine(const AArgs: TArray<string>)`, takes the arguments explicitly, which is handy in tests.

## Adding an Agent

An agent the installer does not know can be added with an adapter. When its configuration is a JSON file with an object of servers keyed by name, `TMCPJsonAgentAdapter` does all the work: say where the file is, and override what differs from the defaults (`mcpServers` key, no `type` member, `command` and `env` members):

```pascal
uses
  System.SysUtils, System.IOUtils,

  MCPConnect.Install;

type
  TMyAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetEntryType(ATransport: TMCPInstallTransport): string; override;
  end;

function TMyAgent.GetId: string;
begin
  Result := 'my-agent';
end;

function TMyAgent.GetDisplayName: string;
begin
  Result := 'My Agent';
end;

function TMyAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := TPath.Combine(TMCPInstallUtils.HomeDir, '.myagent\mcp.json');
  else
    Result := TPath.Combine(AProjectDir, '.myagent\mcp.json');
  end;
end;

function TMyAgent.GetEntryType(ATransport: TMCPInstallTransport): string;
const
  EntryTypes: array[TMCPInstallTransport] of string = ('stdio', 'http');
begin
  Result := EntryTypes[ATransport];
end;

...

TMCPInstaller.Config.RegisterAgent(TMyAgent.Create);  // the configuration owns it
```

Other virtual methods cover the remaining differences:

| Method | Default |
|---|---|
| `GetServersKey` | `mcpServers` |
| `GetCliName` | None. When set, the agent is detected when this program is in the `PATH` |
| `GetDetectionPath` | The folder of the global configuration file |
| `GetProjectCommand` | None: the absolute path is registered even when the executable is in the project |
| `SupportsScope`, `SupportsTransport` | Every scope and transport |
| `BuildEntry`, `GetEntryCommand`, `GetEntryUrl` | The `type`/`command`/`env` or `type`/`url` members |
| `GetInstallNotes` | No hint after the install |

An agent with a different file format, or one driven by a command line tool, derives from `TMCPAgentAdapter` and implements `IsDetected`, `GetServers`, `Install` and `Uninstall` itself; `TMCPInstallUtils` has the helpers the built-in adapters use (finding and running a CLI, reading and writing JSON files atomically).

## Limitations

- Configuration files with comments or trailing commas (JSONC) cannot be parsed: the agent is reported as an error and its file is left untouched.
- Only the executable is registered, without arguments.
- After an uninstall, an empty servers object (or an empty file) is left in place.
- Windows is the platform the installer has been tested on. The macOS and Linux paths follow the documentation of each agent.
