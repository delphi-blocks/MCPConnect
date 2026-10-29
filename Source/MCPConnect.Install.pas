{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.Install;

{
  Self-installation of an MCP server into the MCP agents found on the machine
  (Claude Code, Codex, Cursor, ...): "MyServer.exe /mcp:install". A STDIO server
  registers its own executable; an HTTP server registers its url, and can be
  installed by any console program (the server itself or a separate one).
  See Docs/install-spec.md.
}

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON;

resourcestring
  SInstallUnknownSwitchFmt = 'Unknown switch "%s": use %shelp for the syntax';
  SInstallMissingValueFmt = 'Missing value for "%s"';
  SInstallMissingCommand = 'Missing command: use install, uninstall, status or help';
  SInstallTooManyCommands = 'Only one command among install, uninstall, status and help can be given';
  SInstallUnknownAgentFmt = 'Unknown agent "%s"';
  SInstallUnknownScopeFmt = 'Unknown scope "%s": use global or project';
  SInstallInvalidNameFmt = 'Invalid server name "%s": use letters, digits, "-" and "_" only';
  SInstallInvalidEnvFmt = 'Invalid environment variable "%s": use KEY=VALUE';
  SInstallInvalidJsonFmt = 'Cannot parse %s (comments and trailing commas are not supported)';
  SInstallInvalidKeyFmt = '"%s" in %s is not a JSON object';
  SInstallCliNotFoundFmt = '"%s" not found in the PATH';
  SInstallCliFailedFmt = '%s failed: %s';
  SInstallEnvNotSupported = 'Environment variables are not supported by the http transport';
  SInstallMissingUrl = 'Missing url for the http transport';

  SInstallInstallingFmt = 'Installing "%s" (scope: %s, transport: %s)';
  SInstallUninstallingFmt = 'Uninstalling "%s" (scope: %s, transport: %s)';
  SInstallStatusFmt = 'Server "%s" - project folder: %s';
  SInstallStatusByExeFmt = 'Server %s - project folder: %s';
  SInstallInstalled = 'installed';
  SInstallUninstalled = 'removed';
  SInstallUninstalledFmt = 'removed "%s"';
  SInstallAlreadyRegisteredFmt = 'ERROR: already registered as "%s", use uninstall first';
  SInstallNameInUseFmt = 'ERROR: the name "%s" is used by another server, choose one with %sname';
  SInstallNotRegistered = 'warning: not registered';
  SInstallNotDetected = 'not detected, skipped';
  SInstallNotDetectedError = 'ERROR: not detected';
  SInstallScopeNotSupported = 'scope not supported, skipped';
  SInstallTransportNotSupported = 'transport not supported, skipped';
  SInstallErrorFmt = 'ERROR: %s';
  SInstallYes = 'yes';
  SInstallNo = 'no';
  SInstallNotAvailable = 'n/a';
  SInstallError = 'error';
  SInstallDetected = 'detected';
  SInstallAgentsTitle = 'Supported agents:';

  SInstallHelpUsageFmt = 'Usage: %s %sinstall|uninstall|status|agent|help [options]';
  SInstallHelpCommands = 'Commands:';
  SInstallHelpInstall = 'Register the server in the MCP agents';
  SInstallHelpUninstall = 'Remove the server from the MCP agents';
  SInstallHelpStatus = 'Show in which agents the server is registered';
  SInstallHelpAgents = 'List the supported agents';
  SInstallHelpHelp = 'Show this help';
  SInstallHelpOptions = 'Options:';
  SInstallHelpAgentFmt = 'Agents to use, repeatable or comma separated (default: all detected), e.g. %s';
  SInstallHelpScope = 'global (default) or project (the current folder)';
  SInstallHelpNameFmt = 'Server name (default: %s)';
  SInstallHelpEnv = 'Environment variable passed to the server, repeatable';

type
  EMCPInstallError = class(Exception);

  TMCPInstallScope = (Global, Project);
  TMCPInstallTransport = (Stdio, Http);

  /// <summary>What gets registered in an agent, and where.</summary>
  TMCPInstallContext = record
  public
    ServerName: string;

    /// <summary>
    ///   True when neither the user (/mcp:name) nor the developer gave a name:
    ///   ServerName is then the executable name, used to register the server, while
    ///   an existing registration is looked for by executable (by url for the http
    ///   transport), whatever its name.
    /// </summary>
    IsNameDefault: Boolean;

    Transport: TMCPInstallTransport;

    /// <summary>Endpoint of the server, used by the http transport.</summary>
    Url: string;

    /// <summary>Full path of the server executable.</summary>
    ExePath: string;

    /// <summary>The current folder, used by the project scope.</summary>
    ProjectDir: string;

    Scope: TMCPInstallScope;
    Env: TArray<TPair<string, string>>;

    /// <summary>
    ///   True when the scope is project and the executable is inside the project
    ///   folder: the agents then get a path relative to the project.
    /// </summary>
    function IsExeInProject: Boolean;

    /// <summary>Path of the executable relative to the project, with "/" separators.</summary>
    function RelativeExePath: string;

    function ScopeName: string;
    function TransportName: string;
  end;

  /// <summary>A server found in the configuration of an agent.</summary>
  TMCPRegisteredServer = record
    Name: string;

    /// <summary>The command as written in the configuration (variables not expanded).</summary>
    Command: string;

    /// <summary>The url of a remote server, empty for a local one.</summary>
    Url: string;

    class function Create(const AName, ACommand: string; const AUrl: string = ''): TMCPRegisteredServer; static;
  end;

  /// <summary>
  ///   Knows how to detect one agent and how to add, remove and find a server in
  ///   its configuration.
  /// </summary>
  TMCPAgentAdapter = class abstract
  protected
    function GetId: string; virtual; abstract;
    function GetDisplayName: string; virtual; abstract;

    /// <summary>
    ///   How the agent expresses an executable relative to the project folder (e.g.
    ///   "${workspaceFolder}/tools/server.exe"). Empty when it cannot: the
    ///   absolute path is registered instead.
    /// </summary>
    function GetProjectCommand(const ARelativePath: string): string; virtual;

    /// <summary>
    ///   Turns a registered command back into a full path: expands the project
    ///   variables and resolves relative paths from the project folder.
    /// </summary>
    function ResolveCommand(const ACommand: string; const AContext: TMCPInstallContext): string; virtual;

    /// <summary>
    ///   The name of the server matching the context among AServers: by name when
    ///   one was given, otherwise by executable (by url for the http transport).
    ///   Empty if none.
    /// </summary>
    function MatchServer(const AServers: TArray<TMCPRegisteredServer>; const AContext: TMCPInstallContext): string;
  public
    function IsDetected: Boolean; virtual; abstract;
    function SupportsScope(AScope: TMCPInstallScope): Boolean; virtual;
    function SupportsTransport(ATransport: TMCPInstallTransport): Boolean; virtual;

    /// <summary>The servers registered in the configuration of AContext.Scope.</summary>
    function GetServers(const AContext: TMCPInstallContext): TArray<TMCPRegisteredServer>; virtual; abstract;

    /// <summary>The name under which the server is registered, empty if it is not.</summary>
    function FindServer(const AContext: TMCPInstallContext): string;

    /// <summary>True if a server with this name, whatever its command, is registered.</summary>
    function HasServerName(const AContext: TMCPInstallContext; const AName: string): Boolean;

    /// <summary>Registers AContext.ServerName.</summary>
    procedure Install(const AContext: TMCPInstallContext); virtual; abstract;

    /// <summary>Removes AContext.ServerName.</summary>
    procedure Uninstall(const AContext: TMCPInstallContext); virtual; abstract;

    /// <summary>
    ///   Extra information for the status, e.g. a registration in a scope MCPConnect
    ///   does not manage.
    /// </summary>
    function GetStatusNotes(const AContext: TMCPInstallContext): string; virtual;

    /// <summary>Hint shown after a successful install (e.g. "restart the app").</summary>
    function GetInstallNotes(const AContext: TMCPInstallContext): string; virtual;

    /// <summary>The command to register: relative to the project when possible.</summary>
    function GetCommand(const AContext: TMCPInstallContext): string;

    property Id: string read GetId;
    property DisplayName: string read GetDisplayName;
  end;

  /// <summary>
  ///   Base for the agents configured through a JSON file holding an object of
  ///   servers keyed by name ("mcpServers" by default).
  /// </summary>
  TMCPJsonAgentAdapter = class abstract(TMCPAgentAdapter)
  protected
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; virtual; abstract;
    function GetServersKey: string; virtual;

    /// <summary>The value of the "type" member of an entry; empty to leave it out.</summary>
    function GetEntryType(ATransport: TMCPInstallTransport): string; virtual;

    /// <summary>Command line tool of the agent, used to detect it. Empty if none.</summary>
    function GetCliName: string; virtual;

    /// <summary>File or folder whose presence means the agent is installed.</summary>
    function GetDetectionPath: string; virtual;

    function BuildEntry(const AContext: TMCPInstallContext): TJSONObject; virtual;
    function FindServers(AConfig: TJSONObject; const AFileName: string; ACreate: Boolean): TJSONObject; virtual;

    /// <summary>The command of an entry of the servers object.</summary>
    function GetEntryCommand(AEntry: TJSONValue): string; virtual;

    /// <summary>The url of an entry of the servers object, empty for a local server.</summary>
    function GetEntryUrl(AEntry: TJSONValue): string; virtual;

    function ServersFromJson(AServers: TJSONObject): TArray<TMCPRegisteredServer>;
  public
    function IsDetected: Boolean; override;
    function GetServers(const AContext: TMCPInstallContext): TArray<TMCPRegisteredServer>; override;
    procedure Install(const AContext: TMCPInstallContext); override;
    procedure Uninstall(const AContext: TMCPInstallContext); override;
  end;

  /// <summary>File system and process helpers shared by the agent adapters.</summary>
  TMCPInstallUtils = class
  public
    class function HomeDir: string; static;

    /// <summary>%APPDATA% on Windows, ~/Library/Application Support on macOS, ~/.config on Linux.</summary>
    class function AppConfigDir: string; static;

    /// <summary>$XDG_CONFIG_HOME or ~/.config, on every platform.</summary>
    class function XdgConfigDir: string; static;

    /// <summary>Full path of an executable found in the PATH, or empty.</summary>
    class function FindExecutable(const AName: string): string; static;

    /// <summary>Like FindExecutable, but raises when it is missing.</summary>
    class function RequireExecutable(const AName: string): string; static;

    /// <summary>Runs a program and returns its exit code; stdout and stderr go to AOutput.</summary>
    class function RunProcess(const AExe: string; const AArgs: TArray<string>; out AOutput: string): Integer; static;

    /// <summary>Runs a program and raises with its output when it fails.</summary>
    class procedure RunCli(const AExe: string; const AArgs: TArray<string>); static;

    /// <summary>The parsed file, nil if it does not exist; raises if it is not a JSON object.</summary>
    class function LoadJsonFile(const AFileName: string): TJSONObject; static;
    class procedure SaveJsonFile(const AFileName: string; AConfig: TJSONObject); static;

    /// <summary>Writes a UTF-8 file atomically, keeping a .bak copy of the old one.</summary>
    class procedure SaveTextFile(const AFileName, AText: string); static;

    class function EnvToJson(const AEnv: TArray<TPair<string, string>>): TJSONObject; static;
  end;

  TMCPInstallerConfig = class
  private
    FCommandPrefix: string;
    FServerName: string;
    FTransport: TMCPInstallTransport;
    FUrl: string;
    FEnv: TArray<TPair<string, string>>;
    FAgents: TObjectList<TMCPAgentAdapter>;
    FDisabledAgents: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Prefix of the switches, "mcp:" by default. Empty is allowed (/install).</summary>
    function SetCommandPrefix(const APrefix: string): TMCPInstallerConfig;

    /// <summary>Default server name; the executable name when empty.</summary>
    function SetServerName(const AName: string): TMCPInstallerConfig;

    /// <summary>
    ///   How the agents reach the server: stdio (default) runs this executable,
    ///   http connects to the url given with SetUrl.
    /// </summary>
    function SetTransport(ATransport: TMCPInstallTransport): TMCPInstallerConfig;

    /// <summary>Endpoint of the http transport, "http://localhost:8080/mcp" by default.</summary>
    function SetUrl(const AUrl: string): TMCPInstallerConfig;

    /// <summary>Environment variable always registered, besides the /mcp:env ones (stdio only).</summary>
    function AddEnv(const AName, AValue: string): TMCPInstallerConfig;

    function DisableAgent(const AId: string): TMCPInstallerConfig;

    /// <summary>Adds a custom agent. The configuration takes ownership of it.</summary>
    function RegisterAgent(AAgent: TMCPAgentAdapter): TMCPInstallerConfig;

    /// <summary>The enabled agent with this id (case and "-" insensitive), or nil.</summary>
    function FindAgent(const AId: string): TMCPAgentAdapter;

    /// <summary>The enabled agents, in registration order.</summary>
    function GetAgents: TArray<TMCPAgentAdapter>;

    property CommandPrefix: string read FCommandPrefix;
    property ServerName: string read FServerName;
    property Transport: TMCPInstallTransport read FTransport;
    property Url: string read FUrl;
    property Env: TArray<TPair<string, string>> read FEnv;
  end;

  /// <summary>
  ///   Entry point of the self-installation. Call HandleCommandLine in the .dpr
  ///   before creating the server, and exit when it returns True.
  /// </summary>
  TMCPInstaller = class
  private
    class var FConfig: TMCPInstallerConfig;
    class destructor Destroy;
  public
    class function Config: TMCPInstallerConfig; static;

    /// <summary>
    ///   Handles the /mcp:* switches of the command line. Returns True when it did
    ///   (the program should then end without starting the server), False when
    ///   there are none.
    /// </summary>
    class function HandleCommandLine: Boolean; overload; static;
    class function HandleCommandLine(const AArgs: TArray<string>): Boolean; overload; static;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdio,
  {$ENDIF}
  System.IOUtils, System.StrUtils,

  MCPConnect.Install.Agents;

const
  DefaultCommandPrefix = 'mcp:';
  DefaultUrl = 'http://localhost:8080/mcp';
  JsonIndentation = 2;
  BackupExtension = '.bak';
  TempExtension = '.tmp';
  ReadBufferSize = 4096;
  AgentColumnWidth = 16;
  DetectedColumnWidth = 10;
  StatusColumnWidth = 20;
  ScopeNames: array[TMCPInstallScope] of string = ('global', 'project');
  TransportNames: array[TMCPInstallTransport] of string = ('stdio', 'http');

  {$IFDEF MSWINDOWS}
  HelpSwitchChar = '/';
  {$ELSE}
  HelpSwitchChar = '--';
  {$ENDIF}

type
  TMCPInstallCommand = (None, Install, Uninstall, Status, Help, Agents);

const
  // Agents has no name of its own: it is the agent option given without a value
  CommandNames: array[TMCPInstallCommand] of string = ('', 'install', 'uninstall', 'status', 'help', '');
  HelpAgentExamples = 'claude-code, codex, cursor';
  OptionAgent = 'agent';
  OptionScope = 'scope';
  OptionName = 'name';
  OptionEnv = 'env';

type
  /// <summary>One run of the installer: parses the switches and executes the command.</summary>
  TMCPInstallRequest = class
  private
    FConfig: TMCPInstallerConfig;
    FCommand: TMCPInstallCommand;
    FHasOptions: Boolean;
    FAgentsRequested: Boolean;
    FAgents: TArray<TMCPAgentAdapter>;
    FScope: TMCPInstallScope;
    FServerName: string;
    FEnv: TArray<TPair<string, string>>;
    FFailed: Boolean;
    procedure Parse(const AArgs: TArray<string>);
    procedure SetCommand(ACommand: TMCPInstallCommand);
    procedure SetOption(const AName, AValue: string);
    function BuildContext: TMCPInstallContext;
    function SelectedAgents: TArray<TMCPAgentAdapter>;
    function InstallAgent(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
    function UninstallAgent(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
    function InstalledText(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
    procedure DoInstall;
    procedure DoUninstall;
    procedure DoStatus;
    procedure DoHelp;
    procedure DoAgents;
  public
    constructor Create(AConfig: TMCPInstallerConfig);
    function Execute(const AArgs: TArray<string>): Boolean;
  end;

function NormalizeAgentId(const AId: string): string;
begin
  Result := LowerCase(StringReplace(AId.Trim, '-', '', [rfReplaceAll]));
end;

function IsValidServerName(const AName: string): Boolean;
var
  LChar: Char;
begin
  Result := not AName.IsEmpty;
  for LChar in AName do
    if not CharInSet(LChar, ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then
      Exit(False);
end;

function ContainsAgent(const AAgents: TArray<TMCPAgentAdapter>; AAgent: TMCPAgentAdapter): Boolean;
var
  LAgent: TMCPAgentAdapter;
begin
  for LAgent in AAgents do
    if LAgent = AAgent then
      Exit(True);
  Result := False;
end;

function SamePath(const APath1, APath2: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := SameText(APath1, APath2);
  {$ELSE}
  Result := APath1 = APath2;
  {$ENDIF}
end;

function SameUrl(const AUrl1, AUrl2: string): Boolean;
begin
  Result := SameText(AUrl1.Trim.TrimRight(['/']), AUrl2.Trim.TrimRight(['/']));
end;

function PathStartsWith(const APath, ADir: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := StartsText(ADir, APath);
  {$ELSE}
  Result := StartsStr(ADir, APath);
  {$ENDIF}
end;

/// <summary>The switch without its leading "/", "-" or "--"; empty if it is not a switch.</summary>
function StripSwitchChars(const AArg: string): string;
begin
  if AArg.StartsWith('--') then
    Exit(AArg.Substring(2));
  if AArg.StartsWith('/') or AArg.StartsWith('-') then
    Exit(AArg.Substring(1));
  Result := '';
end;

{ TMCPInstallContext }

function TMCPInstallContext.IsExeInProject: Boolean;
begin
  Result := (Scope = TMCPInstallScope.Project) and
    PathStartsWith(ExePath, IncludeTrailingPathDelimiter(ProjectDir));
end;

function TMCPInstallContext.RelativeExePath: string;
begin
  Result := ExePath.Substring(Length(IncludeTrailingPathDelimiter(ProjectDir)));
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

function TMCPInstallContext.ScopeName: string;
begin
  Result := ScopeNames[Scope];
end;

function TMCPInstallContext.TransportName: string;
begin
  Result := TransportNames[Transport];
end;

{ TMCPRegisteredServer }

class function TMCPRegisteredServer.Create(const AName, ACommand: string;
  const AUrl: string): TMCPRegisteredServer;
begin
  Result.Name := AName;
  Result.Command := ACommand;
  Result.Url := AUrl;
end;

{ TMCPAgentAdapter }

function TMCPAgentAdapter.FindServer(const AContext: TMCPInstallContext): string;
begin
  Result := MatchServer(GetServers(AContext), AContext);
end;

function TMCPAgentAdapter.GetStatusNotes(const AContext: TMCPInstallContext): string;
begin
  Result := '';
end;

function TMCPAgentAdapter.HasServerName(const AContext: TMCPInstallContext; const AName: string): Boolean;
var
  LServer: TMCPRegisteredServer;
begin
  for LServer in GetServers(AContext) do
    if LServer.Name = AName then
      Exit(True);
  Result := False;
end;

function TMCPAgentAdapter.MatchServer(const AServers: TArray<TMCPRegisteredServer>;
  const AContext: TMCPInstallContext): string;
var
  LServer: TMCPRegisteredServer;
begin
  for LServer in AServers do
  begin
    if AContext.IsNameDefault then
    begin
      case AContext.Transport of
        TMCPInstallTransport.Stdio:
          if not LServer.Command.IsEmpty and SamePath(ResolveCommand(LServer.Command, AContext), AContext.ExePath) then
            Exit(LServer.Name);
        TMCPInstallTransport.Http:
          if not LServer.Url.IsEmpty and SameUrl(LServer.Url, AContext.Url) then
            Exit(LServer.Name);
      end;
    end
    else if LServer.Name = AContext.ServerName then
      Exit(LServer.Name);
  end;
  Result := '';
end;

function TMCPAgentAdapter.ResolveCommand(const ACommand: string; const AContext: TMCPInstallContext): string;
const
  ProjectVariables: array[0..2] of string = ('${workspaceFolder}', '${CLAUDE_PROJECT_DIR:-.}', '${CLAUDE_PROJECT_DIR}');
var
  LVariable: string;
begin
  Result := ACommand;
  for LVariable in ProjectVariables do
    Result := StringReplace(Result, LVariable, AContext.ProjectDir, [rfReplaceAll, rfIgnoreCase]);
  {$IFDEF MSWINDOWS}
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
  {$ENDIF}
  try
    if TPath.IsRelativePath(Result) then
      Result := TPath.Combine(AContext.ProjectDir, Result);
    Result := ExpandFileName(Result);
  except
    // Not a path (e.g. a command with characters a path cannot hold): compare it as is
    on EArgumentException do
      Result := ACommand;
  end;
end;

function TMCPAgentAdapter.GetCommand(const AContext: TMCPInstallContext): string;
begin
  Result := '';
  if AContext.IsExeInProject then
    Result := GetProjectCommand(AContext.RelativeExePath);
  if Result.IsEmpty then
    Result := AContext.ExePath;
end;

function TMCPAgentAdapter.GetInstallNotes(const AContext: TMCPInstallContext): string;
begin
  Result := '';
end;

function TMCPAgentAdapter.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := '';
end;

function TMCPAgentAdapter.SupportsScope(AScope: TMCPInstallScope): Boolean;
begin
  Result := True;
end;

function TMCPAgentAdapter.SupportsTransport(ATransport: TMCPInstallTransport): Boolean;
begin
  Result := True;
end;

{ TMCPJsonAgentAdapter }

function TMCPJsonAgentAdapter.BuildEntry(const AContext: TMCPInstallContext): TJSONObject;
begin
  Result := TJSONObject.Create;
  if not GetEntryType(AContext.Transport).IsEmpty then
    Result.AddPair('type', GetEntryType(AContext.Transport));
  if AContext.Transport = TMCPInstallTransport.Http then
  begin
    Result.AddPair('url', AContext.Url);
    Exit;
  end;
  Result.AddPair('command', GetCommand(AContext));
  if Length(AContext.Env) > 0 then
    Result.AddPair('env', TMCPInstallUtils.EnvToJson(AContext.Env));
end;

function TMCPJsonAgentAdapter.FindServers(AConfig: TJSONObject; const AFileName: string;
  ACreate: Boolean): TJSONObject;
var
  LValue: TJSONValue;
begin
  LValue := AConfig.GetValue(GetServersKey);
  if LValue is TJSONObject then
    Exit(TJSONObject(LValue));
  if Assigned(LValue) then
    raise EMCPInstallError.CreateFmt(SInstallInvalidKeyFmt, [GetServersKey, AFileName]);

  Result := nil;
  if ACreate then
  begin
    Result := TJSONObject.Create;
    AConfig.AddPair(GetServersKey, Result);
  end;
end;

function TMCPJsonAgentAdapter.GetCliName: string;
begin
  Result := '';
end;

function TMCPJsonAgentAdapter.GetDetectionPath: string;
begin
  Result := ExtractFileDir(GetConfigFile(TMCPInstallScope.Global, ''));
end;

function TMCPJsonAgentAdapter.GetEntryCommand(AEntry: TJSONValue): string;
begin
  Result := '';
  if AEntry is TJSONObject then
    Result := TJSONObject(AEntry).GetValue<string>('command', '');
end;

function TMCPJsonAgentAdapter.GetEntryType(ATransport: TMCPInstallTransport): string;
begin
  Result := '';
end;

function TMCPJsonAgentAdapter.GetEntryUrl(AEntry: TJSONValue): string;
begin
  Result := '';
  if AEntry is TJSONObject then
    Result := TJSONObject(AEntry).GetValue<string>('url', '');
end;

function TMCPJsonAgentAdapter.GetServersKey: string;
begin
  Result := 'mcpServers';
end;

procedure TMCPJsonAgentAdapter.Install(const AContext: TMCPInstallContext);
var
  LFileName: string;
  LConfig: TJSONObject;
begin
  LFileName := GetConfigFile(AContext.Scope, AContext.ProjectDir);
  LConfig := TMCPInstallUtils.LoadJsonFile(LFileName);
  if not Assigned(LConfig) then
    LConfig := TJSONObject.Create;
  try
    FindServers(LConfig, LFileName, True).AddPair(AContext.ServerName, BuildEntry(AContext));
    TMCPInstallUtils.SaveJsonFile(LFileName, LConfig);
  finally
    LConfig.Free;
  end;
end;

function TMCPJsonAgentAdapter.IsDetected: Boolean;
var
  LPath: string;
begin
  if not GetCliName.IsEmpty and not TMCPInstallUtils.FindExecutable(GetCliName).IsEmpty then
    Exit(True);
  LPath := GetDetectionPath;
  Result := FileExists(LPath) or DirectoryExists(LPath);
end;

function TMCPJsonAgentAdapter.GetServers(const AContext: TMCPInstallContext): TArray<TMCPRegisteredServer>;
var
  LFileName: string;
  LConfig: TJSONObject;
  LServers: TJSONObject;
begin
  Result := [];
  LFileName := GetConfigFile(AContext.Scope, AContext.ProjectDir);
  LConfig := TMCPInstallUtils.LoadJsonFile(LFileName);
  if not Assigned(LConfig) then
    Exit;
  try
    LServers := FindServers(LConfig, LFileName, False);
    if Assigned(LServers) then
      Result := ServersFromJson(LServers);
  finally
    LConfig.Free;
  end;
end;

function TMCPJsonAgentAdapter.ServersFromJson(AServers: TJSONObject): TArray<TMCPRegisteredServer>;
var
  LPair: TJSONPair;
begin
  Result := [];
  for LPair in AServers do
    Result := Result + [TMCPRegisteredServer.Create(LPair.JsonString.Value, GetEntryCommand(LPair.JsonValue),
      GetEntryUrl(LPair.JsonValue))];
end;

procedure TMCPJsonAgentAdapter.Uninstall(const AContext: TMCPInstallContext);
var
  LFileName: string;
  LConfig: TJSONObject;
  LServers: TJSONObject;
begin
  LFileName := GetConfigFile(AContext.Scope, AContext.ProjectDir);
  LConfig := TMCPInstallUtils.LoadJsonFile(LFileName);
  if not Assigned(LConfig) then
    Exit;
  try
    LServers := FindServers(LConfig, LFileName, False);
    if not Assigned(LServers) then
      Exit;
    LServers.RemovePair(AContext.ServerName).Free;
    TMCPInstallUtils.SaveJsonFile(LFileName, LConfig);
  finally
    LConfig.Free;
  end;
end;

{ TMCPInstallUtils }

class function TMCPInstallUtils.AppConfigDir: string;
begin
  {$IF Defined(MSWINDOWS)}
  Result := GetEnvironmentVariable('APPDATA');
  {$ELSEIF Defined(MACOS)}
  Result := TPath.Combine(HomeDir, 'Library/Application Support');
  {$ELSE}
  Result := XdgConfigDir;
  {$ENDIF}
end;

class function TMCPInstallUtils.EnvToJson(const AEnv: TArray<TPair<string, string>>): TJSONObject;
var
  LPair: TPair<string, string>;
begin
  Result := TJSONObject.Create;
  for LPair in AEnv do
    Result.AddPair(LPair.Key, LPair.Value);
end;

class function TMCPInstallUtils.FindExecutable(const AName: string): string;
var
  LDir: string;
  LExt: string;
  LCandidate: string;
  LExtensions: TArray<string>;
begin
  {$IFDEF MSWINDOWS}
  // Only the PATHEXT extensions: npm also drops an extension-less shell script
  // next to the .cmd shim, which Windows cannot run.
  LExtensions := GetEnvironmentVariable('PATHEXT').Split([';'], TStringSplitOptions.ExcludeEmpty);
  if Length(LExtensions) = 0 then
    LExtensions := ['.com', '.exe', '.bat', '.cmd'];
  {$ELSE}
  LExtensions := [''];
  {$ENDIF}
  for LDir in GetEnvironmentVariable('PATH').Split([PathSep], TStringSplitOptions.ExcludeEmpty) do
    for LExt in LExtensions do
    begin
      LCandidate := IncludeTrailingPathDelimiter(LDir.Trim.DeQuotedString('"')) + AName + LExt;
      if FileExists(LCandidate) then
        Exit(LCandidate);
    end;
  Result := '';
end;

class function TMCPInstallUtils.HomeDir: string;
begin
  {$IFDEF MSWINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

class function TMCPInstallUtils.LoadJsonFile(const AFileName: string): TJSONObject;
var
  LText: string;
  LValue: TJSONValue;
begin
  if not FileExists(AFileName) then
    Exit(nil);

  LText := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  if LText.Trim.IsEmpty then
    Exit(TJSONObject.Create);

  LValue := TJSONObject.ParseJSONValue(LText, True, False);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    raise EMCPInstallError.CreateFmt(SInstallInvalidJsonFmt, [AFileName]);
  end;
  Result := TJSONObject(LValue);
end;

class function TMCPInstallUtils.RequireExecutable(const AName: string): string;
begin
  Result := FindExecutable(AName);
  if Result.IsEmpty then
    raise EMCPInstallError.CreateFmt(SInstallCliNotFoundFmt, [AName]);
end;

class procedure TMCPInstallUtils.RunCli(const AExe: string; const AArgs: TArray<string>);
var
  LOutput: string;
begin
  if RunProcess(AExe, AArgs, LOutput) <> 0 then
    raise EMCPInstallError.CreateFmt(SInstallCliFailedFmt, [ExtractFileName(AExe), LOutput.Trim]);
end;

{$IFDEF MSWINDOWS}
/// <summary>Quotes an argument the way the MSVC runtime (and Node) splits it back.</summary>
function QuoteArg(const AArg: string): string;
var
  LChar: Char;
  LBackslashes: Integer;
begin
  if not AArg.IsEmpty and (LastDelimiter(' '#9'"', AArg) = 0) then
    Exit(AArg);

  Result := '"';
  LBackslashes := 0;
  for LChar in AArg do
    case LChar of
      '\':
        Inc(LBackslashes);
      '"':
      begin
        Result := Result + StringOfChar('\', LBackslashes * 2 + 1) + '"';
        LBackslashes := 0;
      end;
    else
      Result := Result + StringOfChar('\', LBackslashes) + LChar;
      LBackslashes := 0;
    end;
  Result := Result + StringOfChar('\', LBackslashes * 2) + '"';
end;

class function TMCPInstallUtils.RunProcess(const AExe: string; const AArgs: TArray<string>;
  out AOutput: string): Integer;
var
  LCommandLine: string;
  LArg: string;
  LSecurity: TSecurityAttributes;
  LReadPipe: THandle;
  LWritePipe: THandle;
  LNul: THandle;
  LStartup: TStartupInfo;
  LProcess: TProcessInformation;
  LBuffer: array[0..ReadBufferSize - 1] of Byte;
  LRead: DWORD;
  LExitCode: DWORD;
  LStream: TBytesStream;
begin
  LCommandLine := QuoteArg(AExe);
  for LArg in AArgs do
    LCommandLine := LCommandLine + ' ' + QuoteArg(LArg);

  // npm installs the agent CLIs as .cmd shims, which only cmd.exe can run.
  // /s strips the outer quotes and keeps the rest as is. Note that cmd.exe still
  // expands %VAR% inside the arguments.
  if MatchText(ExtractFileExt(AExe), ['.cmd', '.bat']) then
    LCommandLine := QuoteArg(GetEnvironmentVariable('ComSpec')) + ' /d /s /c "' + LCommandLine + '"';

  LSecurity.nLength := SizeOf(LSecurity);
  LSecurity.lpSecurityDescriptor := nil;
  LSecurity.bInheritHandle := True;
  if not CreatePipe(LReadPipe, LWritePipe, @LSecurity, 0) then
    RaiseLastOSError;
  try
    SetHandleInformation(LReadPipe, HANDLE_FLAG_INHERIT, 0);

    // No console input: a CLI asking a question gets EOF instead of hanging.
    LNul := CreateFile('NUL', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, @LSecurity,
      OPEN_EXISTING, 0, 0);

    FillChar(LStartup, SizeOf(LStartup), 0);
    LStartup.cb := SizeOf(LStartup);
    LStartup.dwFlags := STARTF_USESTDHANDLES;
    LStartup.hStdInput := LNul;
    LStartup.hStdOutput := LWritePipe;
    LStartup.hStdError := LWritePipe;

    UniqueString(LCommandLine);
    try
      if not CreateProcess(nil, PChar(LCommandLine), nil, nil, True, CREATE_NO_WINDOW, nil, nil,
        LStartup, LProcess) then
        RaiseLastOSError;
    finally
      CloseHandle(LWritePipe);
      LWritePipe := 0;
      if LNul <> INVALID_HANDLE_VALUE then
        CloseHandle(LNul);
    end;

    LStream := TBytesStream.Create;
    try
      while ReadFile(LReadPipe, LBuffer, SizeOf(LBuffer), LRead, nil) and (LRead > 0) do
        LStream.WriteBuffer(LBuffer, LRead);

      WaitForSingleObject(LProcess.hProcess, INFINITE);
      GetExitCodeProcess(LProcess.hProcess, LExitCode);
      CloseHandle(LProcess.hThread);
      CloseHandle(LProcess.hProcess);

      AOutput := TEncoding.UTF8.GetString(LStream.Bytes, 0, LStream.Size);
      Result := Integer(LExitCode);
    finally
      LStream.Free;
    end;
  finally
    CloseHandle(LReadPipe);
    if LWritePipe <> 0 then
      CloseHandle(LWritePipe);
  end;
end;
{$ELSE}
function QuoteArg(const AArg: string): string;
begin
  Result := '''' + StringReplace(AArg, '''', '''\''''', [rfReplaceAll]) + '''';
end;

class function TMCPInstallUtils.RunProcess(const AExe: string; const AArgs: TArray<string>;
  out AOutput: string): Integer;
var
  LCommandLine: string;
  LArg: string;
  LPipe: PFILE;
  LBuffer: array[0..ReadBufferSize - 1] of AnsiChar;
  LBytes: RawByteString;
begin
  LCommandLine := QuoteArg(AExe);
  for LArg in AArgs do
    LCommandLine := LCommandLine + ' ' + QuoteArg(LArg);
  LCommandLine := LCommandLine + ' </dev/null 2>&1';

  LPipe := popen(MarshaledAString(UTF8String(LCommandLine)), 'r');
  if not Assigned(LPipe) then
    RaiseLastOSError;

  LBytes := '';
  while Assigned(fgets(@LBuffer[0], ReadBufferSize, LPipe)) do
    LBytes := LBytes + RawByteString(PAnsiChar(@LBuffer[0]));
  AOutput := UTF8ToString(LBytes);
  Result := (pclose(LPipe) shr 8) and $FF;
end;
{$ENDIF}

class procedure TMCPInstallUtils.SaveJsonFile(const AFileName: string; AConfig: TJSONObject);
begin
  SaveTextFile(AFileName, AConfig.Format(JsonIndentation));
end;

class procedure TMCPInstallUtils.SaveTextFile(const AFileName, AText: string);
var
  LTempFile: string;
begin
  ForceDirectories(ExtractFileDir(AFileName));
  if FileExists(AFileName) then
    TFile.Copy(AFileName, AFileName + BackupExtension, True);

  // Write aside and swap, so that an agent reading the file never sees half of it.
  // TEncoding.UTF8.GetBytes leaves out the BOM, which some JSON parsers reject.
  LTempFile := AFileName + TempExtension;
  TFile.WriteAllBytes(LTempFile, TEncoding.UTF8.GetBytes(AText));
  {$IFDEF MSWINDOWS}
  if not MoveFileEx(PChar(LTempFile), PChar(AFileName), MOVEFILE_REPLACE_EXISTING) then
    RaiseLastOSError;
  {$ELSE}
  if not RenameFile(LTempFile, AFileName) then
    RaiseLastOSError;
  {$ENDIF}
end;

class function TMCPInstallUtils.XdgConfigDir: string;
begin
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result.IsEmpty then
    Result := TPath.Combine(HomeDir, '.config');
end;

{ TMCPInstallerConfig }

function TMCPInstallerConfig.AddEnv(const AName, AValue: string): TMCPInstallerConfig;
begin
  FEnv := FEnv + [TPair<string, string>.Create(AName, AValue)];
  Result := Self;
end;

constructor TMCPInstallerConfig.Create;
begin
  inherited Create;
  FCommandPrefix := DefaultCommandPrefix;
  FTransport := TMCPInstallTransport.Stdio;
  FUrl := DefaultUrl;
  FAgents := TObjectList<TMCPAgentAdapter>.Create(True);
  FDisabledAgents := TStringList.Create;
end;

destructor TMCPInstallerConfig.Destroy;
begin
  FDisabledAgents.Free;
  FAgents.Free;
  inherited;
end;

function TMCPInstallerConfig.DisableAgent(const AId: string): TMCPInstallerConfig;
begin
  FDisabledAgents.Add(NormalizeAgentId(AId));
  Result := Self;
end;

function TMCPInstallerConfig.FindAgent(const AId: string): TMCPAgentAdapter;
var
  LAgent: TMCPAgentAdapter;
begin
  for LAgent in GetAgents do
    if NormalizeAgentId(LAgent.Id) = NormalizeAgentId(AId) then
      Exit(LAgent);
  Result := nil;
end;

function TMCPInstallerConfig.GetAgents: TArray<TMCPAgentAdapter>;
var
  LAgent: TMCPAgentAdapter;
begin
  Result := [];
  for LAgent in FAgents do
    if FDisabledAgents.IndexOf(NormalizeAgentId(LAgent.Id)) < 0 then
      Result := Result + [LAgent];
end;

function TMCPInstallerConfig.RegisterAgent(AAgent: TMCPAgentAdapter): TMCPInstallerConfig;
begin
  FAgents.Add(AAgent);
  Result := Self;
end;

function TMCPInstallerConfig.SetCommandPrefix(const APrefix: string): TMCPInstallerConfig;
begin
  FCommandPrefix := APrefix;
  Result := Self;
end;

function TMCPInstallerConfig.SetTransport(ATransport: TMCPInstallTransport): TMCPInstallerConfig;
begin
  FTransport := ATransport;
  Result := Self;
end;

function TMCPInstallerConfig.SetUrl(const AUrl: string): TMCPInstallerConfig;
begin
  FUrl := AUrl;
  Result := Self;
end;

function TMCPInstallerConfig.SetServerName(const AName: string): TMCPInstallerConfig;
begin
  FServerName := AName;
  Result := Self;
end;

{ TMCPInstaller }

class function TMCPInstaller.Config: TMCPInstallerConfig;
begin
  if not Assigned(FConfig) then
  begin
    FConfig := TMCPInstallerConfig.Create;
    RegisterDefaultAgents(FConfig);
  end;
  Result := FConfig;
end;

class destructor TMCPInstaller.Destroy;
begin
  FConfig.Free;
end;

class function TMCPInstaller.HandleCommandLine: Boolean;
var
  LArgs: TArray<string>;
  LIndex: Integer;
begin
  SetLength(LArgs, ParamCount);
  for LIndex := 1 to ParamCount do
    LArgs[LIndex - 1] := ParamStr(LIndex);
  Result := HandleCommandLine(LArgs);
end;

class function TMCPInstaller.HandleCommandLine(const AArgs: TArray<string>): Boolean;
var
  LRequest: TMCPInstallRequest;
begin
  LRequest := TMCPInstallRequest.Create(Config);
  try
    Result := LRequest.Execute(AArgs);
  finally
    LRequest.Free;
  end;
end;

{ TMCPInstallRequest }

function TMCPInstallRequest.BuildContext: TMCPInstallContext;
begin
  Result.ServerName := FServerName;
  if Result.ServerName.IsEmpty then
    Result.ServerName := FConfig.ServerName;
  Result.IsNameDefault := Result.ServerName.IsEmpty;
  if Result.IsNameDefault then
    Result.ServerName := TPath.GetFileNameWithoutExtension(ParamStr(0));
  if not IsValidServerName(Result.ServerName) then
    raise EMCPInstallError.CreateFmt(SInstallInvalidNameFmt, [Result.ServerName]);

  Result.ExePath := ExpandFileName(ParamStr(0));
  Result.ProjectDir := ExcludeTrailingPathDelimiter(GetCurrentDir);
  Result.Scope := FScope;
  Result.Transport := FConfig.Transport;
  Result.Url := FConfig.Url.Trim;
  Result.Env := FConfig.Env + FEnv;
  if Result.Transport = TMCPInstallTransport.Http then
  begin
    // The agents connect to a running server: there is no process to pass them to
    if Length(Result.Env) > 0 then
      raise EMCPInstallError.Create(SInstallEnvNotSupported);
    if Result.Url.IsEmpty then
      raise EMCPInstallError.Create(SInstallMissingUrl);
  end;
end;

constructor TMCPInstallRequest.Create(AConfig: TMCPInstallerConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FScope := TMCPInstallScope.Global;
end;

procedure TMCPInstallRequest.DoAgents;
var
  LAgent: TMCPAgentAdapter;
begin
  Writeln(SInstallAgentsTitle);
  Writeln('  ', ''.PadRight(AgentColumnWidth), ' ', ''.PadRight(AgentColumnWidth), ' ', SInstallDetected);
  for LAgent in FConfig.GetAgents do
    Writeln('  ', LAgent.Id.PadRight(AgentColumnWidth), ' ', LAgent.DisplayName.PadRight(AgentColumnWidth), ' ',
      IfThen(LAgent.IsDetected, SInstallYes, SInstallNo));
end;

procedure TMCPInstallRequest.DoHelp;
var
  LSwitch: string;

  procedure WriteItem(const AName, ADescription: string);
  begin
    Writeln('  ', (LSwitch + AName).PadRight(AgentColumnWidth + Length(LSwitch)), ' ', ADescription);
  end;

begin
  LSwitch := HelpSwitchChar + FConfig.CommandPrefix;

  Writeln(Format(SInstallHelpUsageFmt, [ExtractFileName(ParamStr(0)), LSwitch]));
  Writeln;
  Writeln(SInstallHelpCommands);
  WriteItem(CommandNames[TMCPInstallCommand.Install], SInstallHelpInstall);
  WriteItem(CommandNames[TMCPInstallCommand.Uninstall], SInstallHelpUninstall);
  WriteItem(CommandNames[TMCPInstallCommand.Status], SInstallHelpStatus);
  WriteItem(OptionAgent, SInstallHelpAgents);
  WriteItem(CommandNames[TMCPInstallCommand.Help], SInstallHelpHelp);
  Writeln;
  Writeln(SInstallHelpOptions);
  WriteItem(OptionAgent + ' <id>', Format(SInstallHelpAgentFmt, [HelpAgentExamples]));
  WriteItem(OptionScope + ' <scope>', SInstallHelpScope);
  WriteItem(OptionName + ' <name>', Format(SInstallHelpNameFmt,
    [TPath.GetFileNameWithoutExtension(ParamStr(0))]));
  WriteItem(OptionEnv + ' KEY=VALUE', SInstallHelpEnv);
end;

procedure TMCPInstallRequest.DoInstall;
var
  LContext: TMCPInstallContext;
  LAgent: TMCPAgentAdapter;
begin
  LContext := BuildContext;
  Writeln(Format(SInstallInstallingFmt, [LContext.ServerName, LContext.ScopeName, LContext.TransportName]));
  for LAgent in SelectedAgents do
    Writeln('  ', LAgent.DisplayName.PadRight(AgentColumnWidth), ' ', InstallAgent(LAgent, LContext));
end;

procedure TMCPInstallRequest.DoStatus;
var
  LContext: TMCPInstallContext;
  LAgent: TMCPAgentAdapter;
  LDetected: Boolean;
  LGlobal: string;
  LProject: string;
  LNotes: string;
begin
  LContext := BuildContext;
  if LContext.IsNameDefault and (LContext.Transport = TMCPInstallTransport.Http) then
    Writeln(Format(SInstallStatusByExeFmt, [LContext.Url, LContext.ProjectDir]))
  else if LContext.IsNameDefault then
    Writeln(Format(SInstallStatusByExeFmt, [LContext.ExePath, LContext.ProjectDir]))
  else
    Writeln(Format(SInstallStatusFmt, [LContext.ServerName, LContext.ProjectDir]));
  Writeln('  ', ''.PadRight(AgentColumnWidth), ' ', SInstallDetected.PadRight(DetectedColumnWidth),
    ScopeNames[TMCPInstallScope.Global].PadRight(StatusColumnWidth), ScopeNames[TMCPInstallScope.Project]);
  for LAgent in SelectedAgents do
  begin
    LDetected := LAgent.IsDetected;
    LContext.Scope := TMCPInstallScope.Global;
    LGlobal := InstalledText(LAgent, LContext);
    LContext.Scope := TMCPInstallScope.Project;
    LProject := InstalledText(LAgent, LContext);
    try
      LNotes := LAgent.GetStatusNotes(LContext);
    except
      LNotes := SInstallError;
    end;
    if not LNotes.IsEmpty then
      LProject := LProject + ' [' + LNotes + ']';
    Writeln('  ', LAgent.DisplayName.PadRight(AgentColumnWidth), ' ',
      IfThen(LDetected, SInstallYes, SInstallNo).PadRight(DetectedColumnWidth),
      LGlobal.PadRight(StatusColumnWidth), LProject);
  end;
end;

procedure TMCPInstallRequest.DoUninstall;
var
  LContext: TMCPInstallContext;
  LAgent: TMCPAgentAdapter;
begin
  LContext := BuildContext;
  Writeln(Format(SInstallUninstallingFmt, [LContext.ServerName, LContext.ScopeName, LContext.TransportName]));
  for LAgent in SelectedAgents do
    Writeln('  ', LAgent.DisplayName.PadRight(AgentColumnWidth), ' ', UninstallAgent(LAgent, LContext));
end;

function TMCPInstallRequest.Execute(const AArgs: TArray<string>): Boolean;
begin
  Result := True;
  try
    Parse(AArgs);
    case FCommand of
      TMCPInstallCommand.None:
      begin
        if FHasOptions then
          raise EMCPInstallError.Create(SInstallMissingCommand);
        Result := False;
      end;
      TMCPInstallCommand.Install:
        DoInstall;
      TMCPInstallCommand.Uninstall:
        DoUninstall;
      TMCPInstallCommand.Status:
        DoStatus;
      TMCPInstallCommand.Help:
        DoHelp;
      TMCPInstallCommand.Agents:
        DoAgents;
    else
      raise EMCPInstallError.Create('Unexpected install command');
    end;
  except
    on E: EMCPInstallError do
    begin
      Writeln(ErrOutput, E.Message);
      FFailed := True;
    end;
  end;
  if FFailed then
    ExitCode := 1;
end;

function TMCPInstallRequest.InstallAgent(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
var
  LNotes: string;
  LExisting: string;
begin
  if not AAgent.IsDetected then
  begin
    if Length(FAgents) = 0 then
      Exit(SInstallNotDetected);
    FFailed := True;
    Exit(SInstallNotDetectedError);
  end;
  if not AAgent.SupportsTransport(AContext.Transport) then
    Exit(SInstallTransportNotSupported);
  if not AAgent.SupportsScope(AContext.Scope) then
    Exit(SInstallScopeNotSupported);

  try
    LExisting := AAgent.FindServer(AContext);
    if not LExisting.IsEmpty then
    begin
      FFailed := True;
      Exit(Format(SInstallAlreadyRegisteredFmt, [LExisting]));
    end;
    // Looked for by executable: the default name may still belong to another server
    if AContext.IsNameDefault and AAgent.HasServerName(AContext, AContext.ServerName) then
    begin
      FFailed := True;
      Exit(Format(SInstallNameInUseFmt, [AContext.ServerName, HelpSwitchChar + FConfig.CommandPrefix]));
    end;
    AAgent.Install(AContext);
    Result := SInstallInstalled;
    LNotes := AAgent.GetInstallNotes(AContext);
    if not LNotes.IsEmpty then
      Result := Result + ' - ' + LNotes;
  except
    on E: Exception do
    begin
      FFailed := True;
      Result := Format(SInstallErrorFmt, [E.Message]);
    end;
  end;
end;

function TMCPInstallRequest.InstalledText(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
var
  LName: string;
begin
  if not AAgent.SupportsTransport(AContext.Transport) or not AAgent.SupportsScope(AContext.Scope) then
    Exit(SInstallNotAvailable);
  try
    LName := AAgent.FindServer(AContext);
    if LName.IsEmpty then
      Result := SInstallNo
    else if AContext.IsNameDefault then
      Result := SInstallYes + ' (' + LName + ')'
    else
      Result := SInstallYes;
  except
    Result := SInstallError;
  end;
end;

procedure TMCPInstallRequest.Parse(const AArgs: TArray<string>);
var
  LIndex: Integer;
  LSwitch: string;
  LName: string;
  LValue: string;
  LSeparator: Integer;
  LCommand: TMCPInstallCommand;
  LFound: Boolean;
begin
  LIndex := 0;
  while LIndex < Length(AArgs) do
  begin
    LSwitch := StripSwitchChars(AArgs[LIndex]);
    Inc(LIndex);
    if LSwitch.IsEmpty or not StartsText(FConfig.CommandPrefix, LSwitch) then
      Continue;

    LName := LSwitch.Substring(Length(FConfig.CommandPrefix));
    LValue := '';
    LSeparator := LName.IndexOf('=');
    if LSeparator >= 0 then
    begin
      LValue := LName.Substring(LSeparator + 1);
      LName := LName.Substring(0, LSeparator);
    end;

    LFound := False;
    for LCommand := Succ(TMCPInstallCommand.None) to High(TMCPInstallCommand) do
      if not CommandNames[LCommand].IsEmpty and SameText(LName, CommandNames[LCommand]) then
      begin
        SetCommand(LCommand);
        LFound := True;
      end;
    if LFound then
      Continue;

    if not MatchText(LName, [OptionAgent, OptionScope, OptionName, OptionEnv]) then
    begin
      // Without a prefix the switch may well belong to the application.
      if FConfig.CommandPrefix.IsEmpty then
        Continue;
      raise EMCPInstallError.CreateFmt(SInstallUnknownSwitchFmt,
        [AArgs[LIndex - 1], HelpSwitchChar + FConfig.CommandPrefix]);
    end;

    if LSeparator < 0 then
    begin
      // The agent option alone (no value, or another switch next) lists the agents
      if SameText(LName, OptionAgent) and
        ((LIndex >= Length(AArgs)) or not StripSwitchChars(AArgs[LIndex]).IsEmpty) then
      begin
        FAgentsRequested := True;
        Continue;
      end;
      if LIndex >= Length(AArgs) then
        raise EMCPInstallError.CreateFmt(SInstallMissingValueFmt, [AArgs[LIndex - 1]]);
      LValue := AArgs[LIndex];
      Inc(LIndex);
    end;
    SetOption(LName, LValue);
  end;

  if FAgentsRequested then
  begin
    if FCommand <> TMCPInstallCommand.None then
      raise EMCPInstallError.CreateFmt(SInstallMissingValueFmt, [HelpSwitchChar + FConfig.CommandPrefix + OptionAgent]);
    FCommand := TMCPInstallCommand.Agents;
  end;
end;

function TMCPInstallRequest.SelectedAgents: TArray<TMCPAgentAdapter>;
begin
  if Length(FAgents) > 0 then
    Result := FAgents
  else
    Result := FConfig.GetAgents;
end;

procedure TMCPInstallRequest.SetCommand(ACommand: TMCPInstallCommand);
begin
  if FCommand <> TMCPInstallCommand.None then
    raise EMCPInstallError.Create(SInstallTooManyCommands);
  FCommand := ACommand;
end;

procedure TMCPInstallRequest.SetOption(const AName, AValue: string);
var
  LId: string;
  LAgent: TMCPAgentAdapter;
  LSeparator: Integer;
begin
  FHasOptions := True;
  if SameText(AName, OptionAgent) then
  begin
    for LId in AValue.Split([','], TStringSplitOptions.ExcludeEmpty) do
    begin
      LAgent := FConfig.FindAgent(LId);
      if not Assigned(LAgent) then
        raise EMCPInstallError.CreateFmt(SInstallUnknownAgentFmt, [LId.Trim]);
      if not ContainsAgent(FAgents, LAgent) then
        FAgents := FAgents + [LAgent];
    end;
  end
  else if SameText(AName, OptionScope) then
  begin
    if SameText(AValue, ScopeNames[TMCPInstallScope.Global]) then
      FScope := TMCPInstallScope.Global
    else if SameText(AValue, ScopeNames[TMCPInstallScope.Project]) then
      FScope := TMCPInstallScope.Project
    else
      raise EMCPInstallError.CreateFmt(SInstallUnknownScopeFmt, [AValue]);
  end
  else if SameText(AName, OptionName) then
  begin
    if not IsValidServerName(AValue) then
      raise EMCPInstallError.CreateFmt(SInstallInvalidNameFmt, [AValue]);
    FServerName := AValue;
  end
  else if SameText(AName, OptionEnv) then
  begin
    LSeparator := AValue.IndexOf('=');
    if LSeparator <= 0 then
      raise EMCPInstallError.CreateFmt(SInstallInvalidEnvFmt, [AValue]);
    FEnv := FEnv + [TPair<string, string>.Create(AValue.Substring(0, LSeparator),
      AValue.Substring(LSeparator + 1))];
  end;
end;

function TMCPInstallRequest.UninstallAgent(AAgent: TMCPAgentAdapter; const AContext: TMCPInstallContext): string;
var
  LContext: TMCPInstallContext;
begin
  if not AAgent.IsDetected then
  begin
    if Length(FAgents) = 0 then
      Exit(SInstallNotDetected);
    FFailed := True;
    Exit(SInstallNotDetectedError);
  end;
  if not AAgent.SupportsTransport(AContext.Transport) then
    Exit(SInstallTransportNotSupported);
  if not AAgent.SupportsScope(AContext.Scope) then
    Exit(SInstallScopeNotSupported);

  try
    // Removes the registration found, under whatever name it has
    LContext := AContext;
    LContext.ServerName := AAgent.FindServer(AContext);
    if LContext.ServerName.IsEmpty then
      Exit(SInstallNotRegistered);
    AAgent.Uninstall(LContext);
    if AContext.IsNameDefault then
      Result := Format(SInstallUninstalledFmt, [LContext.ServerName])
    else
      Result := SInstallUninstalled;
  except
    on E: Exception do
    begin
      FFailed := True;
      Result := Format(SInstallErrorFmt, [E.Message]);
    end;
  end;
end;

end.
