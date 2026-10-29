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
unit MCPConnect.Install.Agents;

{
  The agents supported out of the box by TMCPInstaller. Where an agent has a
  non-interactive command line tool it is used to write the configuration, which
  keeps the agent in charge of its own file; otherwise the file is edited
  directly. The current registration is always read from the file.
}

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.JSON,

  MCPConnect.Install;

resourcestring
  SInstallClaudeCodeApproval = 'Claude Code asks to approve the server on first use';
  SInstallClaudeDesktopRestart = 'restart Claude Desktop';
  SInstallCodexTrust = 'Codex reads .codex/config.toml only in trusted projects';
  SInstallOpenCodeV2Fmt = '%s uses the opencode v2 schema (mcp.servers), not supported';
  SInstallClaudeCodeLocalFmt = 'local: %s';

type
  /// <summary>Claude Code, through "claude mcp add-json" and "claude mcp remove".</summary>
  TMCPClaudeCodeAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetEntryType(ATransport: TMCPInstallTransport): string; override;
    function GetCliName: string; override;
    function GetDetectionPath: string; override;
  public
    procedure Install(const AContext: TMCPInstallContext); override;
    procedure Uninstall(const AContext: TMCPInstallContext); override;
    function GetInstallNotes(const AContext: TMCPInstallContext): string; override;

    /// <summary>
    ///   Reports a registration in the "local" scope of Claude Code (private to the
    ///   user, tied to the project folder), which MCPConnect does not install to.
    /// </summary>
    function GetStatusNotes(const AContext: TMCPInstallContext): string; override;
  end;

  /// <summary>Claude Desktop: claude_desktop_config.json, global only.</summary>
  TMCPClaudeDesktopAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
  public
    function SupportsScope(AScope: TMCPInstallScope): Boolean; override;

    /// <summary>
    ///   Stdio only: remote servers are added from the Connectors settings of the
    ///   app, not from the configuration file.
    /// </summary>
    function SupportsTransport(ATransport: TMCPInstallTransport): Boolean; override;
    function GetInstallNotes(const AContext: TMCPInstallContext): string; override;
  end;

  /// <summary>
  ///   Codex: "codex mcp add/remove" for the global scope, a minimal edit of
  ///   .codex/config.toml for the project scope (the CLI has none).
  /// </summary>
  TMCPCodexAgent = class(TMCPAgentAdapter)
  private
    function CodexHome: string;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
    function IsServerTable(const ALine, AName: string): Boolean;
    function ParseServerTable(const ALine: string): string;
    function ParseTomlString(const AValue: string): string;
    procedure AddServerTable(const AFileName: string; const AContext: TMCPInstallContext);
    procedure RemoveServerTable(const AFileName, AName: string);
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
  public
    function IsDetected: Boolean; override;
    function GetServers(const AContext: TMCPInstallContext): TArray<TMCPRegisteredServer>; override;
    procedure Install(const AContext: TMCPInstallContext); override;
    procedure Uninstall(const AContext: TMCPInstallContext); override;
    function GetInstallNotes(const AContext: TMCPInstallContext): string; override;
  end;

  /// <summary>Gemini CLI, through "gemini mcp add" and "gemini mcp remove".</summary>
  TMCPGeminiAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetCliName: string; override;

    /// <summary>"httpUrl" for a streamable HTTP server ("url" is SSE).</summary>
    function GetEntryUrl(AEntry: TJSONValue): string; override;
  public
    procedure Install(const AContext: TMCPInstallContext); override;
    procedure Uninstall(const AContext: TMCPInstallContext); override;
  end;

  /// <summary>opencode: the "mcp" object of opencode.json.</summary>
  TMCPOpenCodeAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetServersKey: string; override;
    function GetCliName: string; override;
    function BuildEntry(const AContext: TMCPInstallContext): TJSONObject; override;
    function FindServers(AConfig: TJSONObject; const AFileName: string; ACreate: Boolean): TJSONObject; override;
    function GetEntryCommand(AEntry: TJSONValue): string; override;
  end;

  /// <summary>Cursor: .cursor/mcp.json.</summary>
  TMCPCursorAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetEntryType(ATransport: TMCPInstallTransport): string; override;
  end;

  /// <summary>
  ///   VS Code (Copilot): mcp.json of the user profile or .vscode/mcp.json. The
  ///   file is always edited directly, as "code --add-mcp" cannot remove a server.
  /// </summary>
  TMCPVSCodeAgent = class(TMCPJsonAgentAdapter)
  protected
    function GetId: string; override;
    function GetDisplayName: string; override;
    function GetProjectCommand(const ARelativePath: string): string; override;
    function GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string; override;
    function GetServersKey: string; override;
    function GetEntryType(ATransport: TMCPInstallTransport): string; override;
    function GetCliName: string; override;
    function GetDetectionPath: string; override;
  end;

/// <summary>Registers the built-in agents; called by TMCPInstaller.Config.</summary>
procedure RegisterDefaultAgents(AConfig: TMCPInstallerConfig);

implementation

uses
  System.IOUtils, System.Generics.Collections;

const
  EntryTypes: array[TMCPInstallTransport] of string = ('stdio', 'http');
  AgentScopes: array[TMCPInstallScope] of string = ('user', 'project');
  CodexServersTable = 'mcp_servers';

procedure RegisterDefaultAgents(AConfig: TMCPInstallerConfig);
begin
  AConfig
    .RegisterAgent(TMCPClaudeCodeAgent.Create)
    .RegisterAgent(TMCPClaudeDesktopAgent.Create)
    .RegisterAgent(TMCPCodexAgent.Create)
    .RegisterAgent(TMCPGeminiAgent.Create)
    .RegisterAgent(TMCPOpenCodeAgent.Create)
    .RegisterAgent(TMCPCursorAgent.Create)
    .RegisterAgent(TMCPVSCodeAgent.Create);
end;

function ProjectFile(const AProjectDir: string; const APath: array of string): string;
var
  LPart: string;
begin
  Result := AProjectDir;
  for LPart in APath do
    Result := TPath.Combine(Result, LPart);
end;

{ TMCPClaudeCodeAgent }

function TMCPClaudeCodeAgent.GetCliName: string;
begin
  Result := 'claude';
end;

function TMCPClaudeCodeAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := TPath.Combine(TMCPInstallUtils.HomeDir, '.claude.json');
    TMCPInstallScope.Project:
      Result := TPath.Combine(AProjectDir, '.mcp.json');
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPClaudeCodeAgent.GetDetectionPath: string;
begin
  Result := GetConfigFile(TMCPInstallScope.Global, '');
end;

function TMCPClaudeCodeAgent.GetDisplayName: string;
begin
  Result := 'Claude Code';
end;

function TMCPClaudeCodeAgent.GetEntryType(ATransport: TMCPInstallTransport): string;
begin
  Result := EntryTypes[ATransport];
end;

function TMCPClaudeCodeAgent.GetId: string;
begin
  Result := 'claude-code';
end;

function TMCPClaudeCodeAgent.GetInstallNotes(const AContext: TMCPInstallContext): string;
begin
  Result := '';
  if AContext.Scope = TMCPInstallScope.Project then
    Result := SInstallClaudeCodeApproval;
end;

function TMCPClaudeCodeAgent.GetStatusNotes(const AContext: TMCPInstallContext): string;
var
  LConfig: TJSONObject;
  LProjects: TJSONObject;
  LPair: TJSONPair;
  LServers: TJSONValue;
  LName: string;
begin
  Result := '';
  if AContext.Scope <> TMCPInstallScope.Project then
    Exit;

  LConfig := TMCPInstallUtils.LoadJsonFile(GetConfigFile(TMCPInstallScope.Global, ''));
  if not Assigned(LConfig) then
    Exit;
  try
    // "projects" is keyed by folder, with "/" separators and a case that can vary
    // between entries of the same folder
    if not (LConfig.GetValue('projects') is TJSONObject) then
      Exit;
    LProjects := TJSONObject(LConfig.GetValue('projects'));
    for LPair in LProjects do
    begin
      if not SameFileName(ExcludeTrailingPathDelimiter(StringReplace(LPair.JsonString.Value, '/', PathDelim, [rfReplaceAll])),
        AContext.ProjectDir) or not (LPair.JsonValue is TJSONObject) then
        Continue;
      LServers := TJSONObject(LPair.JsonValue).GetValue(GetServersKey);
      if not (LServers is TJSONObject) then
        Continue;
      LName := MatchServer(ServersFromJson(TJSONObject(LServers)), AContext);
      if not LName.IsEmpty then
        Exit(Format(SInstallClaudeCodeLocalFmt, [LName]));
    end;
  finally
    LConfig.Free;
  end;
end;

function TMCPClaudeCodeAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  // .mcp.json expands ${VAR:-default}; the default is required by the docs
  Result := '${CLAUDE_PROJECT_DIR:-.}/' + ARelativePath;
end;

procedure TMCPClaudeCodeAgent.Install(const AContext: TMCPInstallContext);
var
  LEntry: TJSONObject;
begin
  LEntry := BuildEntry(AContext);
  try
    TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable(GetCliName),
      ['mcp', 'add-json', '--scope', AgentScopes[AContext.Scope], AContext.ServerName, LEntry.ToJSON]);
  finally
    LEntry.Free;
  end;
end;

procedure TMCPClaudeCodeAgent.Uninstall(const AContext: TMCPInstallContext);
begin
  TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable(GetCliName),
    ['mcp', 'remove', AContext.ServerName, '--scope', AgentScopes[AContext.Scope]]);
end;

{ TMCPClaudeDesktopAgent }

function TMCPClaudeDesktopAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  Result := ProjectFile(TMCPInstallUtils.AppConfigDir, ['Claude', 'claude_desktop_config.json']);
end;

function TMCPClaudeDesktopAgent.GetDisplayName: string;
begin
  Result := 'Claude Desktop';
end;

function TMCPClaudeDesktopAgent.GetId: string;
begin
  Result := 'claude-desktop';
end;

function TMCPClaudeDesktopAgent.GetInstallNotes(const AContext: TMCPInstallContext): string;
begin
  Result := SInstallClaudeDesktopRestart;
end;

function TMCPClaudeDesktopAgent.SupportsScope(AScope: TMCPInstallScope): Boolean;
begin
  Result := AScope = TMCPInstallScope.Global;
end;

function TMCPClaudeDesktopAgent.SupportsTransport(ATransport: TMCPInstallTransport): Boolean;
begin
  Result := ATransport = TMCPInstallTransport.Stdio;
end;

{ TMCPCodexAgent }

procedure TMCPCodexAgent.AddServerTable(const AFileName: string; const AContext: TMCPInstallContext);

  function TomlString(const AValue: string): string;
  begin
    Result := '"' + StringReplace(StringReplace(AValue, '\', '\\', [rfReplaceAll]),
      '"', '\"', [rfReplaceAll]) + '"';
  end;

var
  LLines: TStringList;
  LPair: TPair<string, string>;
  LTable: string;
begin
  LTable := CodexServersTable + '.' + AContext.ServerName;
  LLines := TStringList.Create;
  try
    if FileExists(AFileName) then
      LLines.LoadFromFile(AFileName, TEncoding.UTF8);
    if (LLines.Count > 0) and not LLines[LLines.Count - 1].Trim.IsEmpty then
      LLines.Add('');
    LLines.Add('[' + LTable + ']');
    if AContext.Transport = TMCPInstallTransport.Http then
      LLines.Add('url = ' + TomlString(AContext.Url))
    else
      LLines.Add('command = ' + TomlString(GetCommand(AContext)));
    if Length(AContext.Env) > 0 then
    begin
      LLines.Add('');
      LLines.Add('[' + LTable + '.env]');
      for LPair in AContext.Env do
        LLines.Add(TomlString(LPair.Key) + ' = ' + TomlString(LPair.Value));
    end;
    TMCPInstallUtils.SaveTextFile(AFileName, LLines.Text);
  finally
    LLines.Free;
  end;
end;

function TMCPCodexAgent.CodexHome: string;
begin
  Result := GetEnvironmentVariable('CODEX_HOME');
  if Result.IsEmpty then
    Result := TPath.Combine(TMCPInstallUtils.HomeDir, '.codex');
end;

function TMCPCodexAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := TPath.Combine(CodexHome, 'config.toml');
    TMCPInstallScope.Project:
      Result := ProjectFile(AProjectDir, ['.codex', 'config.toml']);
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPCodexAgent.GetDisplayName: string;
begin
  Result := 'Codex';
end;

function TMCPCodexAgent.GetId: string;
begin
  Result := 'codex';
end;

function TMCPCodexAgent.GetInstallNotes(const AContext: TMCPInstallContext): string;
begin
  Result := '';
  if AContext.Scope = TMCPInstallScope.Project then
    Result := SInstallCodexTrust;
end;

function TMCPCodexAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := './' + ARelativePath;
end;

procedure TMCPCodexAgent.Install(const AContext: TMCPInstallContext);
var
  LArgs: TArray<string>;
  LPair: TPair<string, string>;
begin
  if AContext.Scope = TMCPInstallScope.Project then
  begin
    AddServerTable(GetConfigFile(AContext.Scope, AContext.ProjectDir), AContext);
    Exit;
  end;

  LArgs := ['mcp', 'add', AContext.ServerName];
  if AContext.Transport = TMCPInstallTransport.Http then
    LArgs := LArgs + ['--url', AContext.Url]
  else
  begin
    for LPair in AContext.Env do
      LArgs := LArgs + ['--env', LPair.Key + '=' + LPair.Value];
    LArgs := LArgs + ['--', GetCommand(AContext)];
  end;
  TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable('codex'), LArgs);
end;

function TMCPCodexAgent.IsDetected: Boolean;
begin
  Result := not TMCPInstallUtils.FindExecutable('codex').IsEmpty or DirectoryExists(CodexHome);
end;

function TMCPCodexAgent.GetServers(const AContext: TMCPInstallContext): TArray<TMCPRegisteredServer>;
var
  LFileName: string;
  LLines: TStringList;
  LLine: string;
  LName: string;
  LSeparator: Integer;
  LKey: string;
begin
  Result := [];
  LFileName := GetConfigFile(AContext.Scope, AContext.ProjectDir);
  if not FileExists(LFileName) then
    Exit;

  // Only the tables form ([mcp_servers.name] followed by command = "..." or
  // url = "..."), the one written by "codex mcp add"; inline tables are not recognized.
  LName := '';
  LLines := TStringList.Create;
  try
    LLines.LoadFromFile(LFileName, TEncoding.UTF8);
    for LLine in LLines do
    begin
      if LLine.Trim.StartsWith('[') then
      begin
        LName := ParseServerTable(LLine);
        if not LName.IsEmpty then
          Result := Result + [TMCPRegisteredServer.Create(LName, '')];
        Continue;
      end;

      LSeparator := LLine.IndexOf('=');
      if LName.IsEmpty or (LSeparator < 0) then
        Continue;
      LKey := LLine.Substring(0, LSeparator).Trim;
      if LKey = 'command' then
        Result[High(Result)].Command := ParseTomlString(LLine.Substring(LSeparator + 1))
      else if LKey = 'url' then
        Result[High(Result)].Url := ParseTomlString(LLine.Substring(LSeparator + 1));
    end;
  finally
    LLines.Free;
  end;
end;

function TMCPCodexAgent.IsServerTable(const ALine, AName: string): Boolean;
var
  LLine: string;
begin
  // [mcp_servers.name], [mcp_servers."name"] and their sub-tables ([mcp_servers.name.env])
  LLine := ALine.Trim;
  Result :=
    (LLine = '[' + CodexServersTable + '.' + AName + ']') or
    (LLine = '[' + CodexServersTable + '."' + AName + '"]') or
    LLine.StartsWith('[' + CodexServersTable + '.' + AName + '.') or
    LLine.StartsWith('[' + CodexServersTable + '."' + AName + '".');
end;

function TMCPCodexAgent.ParseServerTable(const ALine: string): string;
var
  LLine: string;
begin
  // The server name of a [mcp_servers.name] or [mcp_servers."name"] header; empty
  // for any other table, sub-tables ([mcp_servers.name.env]) included.
  Result := '';
  LLine := ALine.Trim;
  if LLine.StartsWith('[[') or not LLine.StartsWith('[' + CodexServersTable + '.') or not LLine.EndsWith(']') then
    Exit;

  LLine := LLine.Substring(Length(CodexServersTable) + 2, LLine.Length - Length(CodexServersTable) - 3).Trim;
  if (LLine.Length > 1) and LLine.StartsWith('"') and LLine.EndsWith('"') then
    LLine := LLine.Substring(1, LLine.Length - 2)
  else if LLine.Contains('.') then
    Exit;
  Result := LLine;
end;

function TMCPCodexAgent.ParseTomlString(const AValue: string): string;
var
  LValue: string;
  LIndex: Integer;
begin
  // Basic ("...", with backslash escapes) or literal ('...') string
  LValue := AValue.Trim;
  Result := '';
  if LValue.StartsWith('''') then
    Exit(LValue.Substring(1, LValue.IndexOf('''', 1) - 1));
  if not LValue.StartsWith('"') then
    Exit(LValue);

  LIndex := 1;
  while LIndex < LValue.Length do
  begin
    if LValue.Chars[LIndex] = '"' then
      Break;
    if (LValue.Chars[LIndex] = '\') and (LIndex + 1 < LValue.Length) then
      Inc(LIndex);
    Result := Result + LValue.Chars[LIndex];
    Inc(LIndex);
  end;
end;

procedure TMCPCodexAgent.RemoveServerTable(const AFileName, AName: string);
var
  LLines: TStringList;
  LIndex: Integer;
  LInside: Boolean;
begin
  LLines := TStringList.Create;
  try
    LLines.LoadFromFile(AFileName, TEncoding.UTF8);
    LInside := False;
    LIndex := 0;
    while LIndex < LLines.Count do
    begin
      if LLines[LIndex].Trim.StartsWith('[') then
        LInside := IsServerTable(LLines[LIndex], AName);
      if LInside then
        LLines.Delete(LIndex)
      else
        Inc(LIndex);
    end;
    TMCPInstallUtils.SaveTextFile(AFileName, LLines.Text);
  finally
    LLines.Free;
  end;
end;

procedure TMCPCodexAgent.Uninstall(const AContext: TMCPInstallContext);
begin
  if AContext.Scope = TMCPInstallScope.Project then
    RemoveServerTable(GetConfigFile(AContext.Scope, AContext.ProjectDir), AContext.ServerName)
  else
    TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable('codex'),
      ['mcp', 'remove', AContext.ServerName]);
end;

{ TMCPGeminiAgent }

function TMCPGeminiAgent.GetCliName: string;
begin
  Result := 'gemini';
end;

function TMCPGeminiAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := ProjectFile(TMCPInstallUtils.HomeDir, ['.gemini', 'settings.json']);
    TMCPInstallScope.Project:
      Result := ProjectFile(AProjectDir, ['.gemini', 'settings.json']);
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPGeminiAgent.GetDisplayName: string;
begin
  Result := 'Gemini CLI';
end;

function TMCPGeminiAgent.GetEntryUrl(AEntry: TJSONValue): string;
begin
  Result := '';
  if AEntry is TJSONObject then
    Result := TJSONObject(AEntry).GetValue<string>('httpUrl', '');
  if Result.IsEmpty then
    Result := inherited GetEntryUrl(AEntry);
end;

function TMCPGeminiAgent.GetId: string;
begin
  Result := 'gemini';
end;

function TMCPGeminiAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := './' + ARelativePath;
end;

procedure TMCPGeminiAgent.Install(const AContext: TMCPInstallContext);
var
  LArgs: TArray<string>;
  LPair: TPair<string, string>;
begin
  // The scope is always given: the default of "gemini mcp add" is project
  LArgs := ['mcp', 'add', '--scope', AgentScopes[AContext.Scope]];
  if AContext.Transport = TMCPInstallTransport.Http then
    LArgs := LArgs + ['--transport', EntryTypes[AContext.Transport], AContext.ServerName, AContext.Url]
  else
  begin
    for LPair in AContext.Env do
      LArgs := LArgs + ['-e', LPair.Key + '=' + LPair.Value];
    LArgs := LArgs + [AContext.ServerName, GetCommand(AContext)];
  end;
  TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable(GetCliName), LArgs);
end;

procedure TMCPGeminiAgent.Uninstall(const AContext: TMCPInstallContext);
begin
  TMCPInstallUtils.RunCli(TMCPInstallUtils.RequireExecutable(GetCliName),
    ['mcp', 'remove', '--scope', AgentScopes[AContext.Scope], AContext.ServerName]);
end;

{ TMCPOpenCodeAgent }

function TMCPOpenCodeAgent.BuildEntry(const AContext: TMCPInstallContext): TJSONObject;
var
  LCommand: TJSONArray;
begin
  Result := TJSONObject.Create;
  if AContext.Transport = TMCPInstallTransport.Http then
  begin
    Result.AddPair('type', 'remote');
    Result.AddPair('url', AContext.Url);
    Result.AddPair('enabled', TJSONBool.Create(True));
    Exit;
  end;

  Result.AddPair('type', 'local');
  LCommand := TJSONArray.Create;
  LCommand.Add(GetCommand(AContext));
  Result.AddPair('command', LCommand);
  // A relative cwd is resolved from the workspace, and the relative command from the cwd
  if AContext.IsExeInProject then
    Result.AddPair('cwd', '.');
  if Length(AContext.Env) > 0 then
    Result.AddPair('environment', TMCPInstallUtils.EnvToJson(AContext.Env));
  Result.AddPair('enabled', TJSONBool.Create(True));
end;

function TMCPOpenCodeAgent.FindServers(AConfig: TJSONObject; const AFileName: string;
  ACreate: Boolean): TJSONObject;
begin
  Result := inherited FindServers(AConfig, AFileName, ACreate);
  if Assigned(Result) and (Result.GetValue('servers') is TJSONObject) then
    raise EMCPInstallError.CreateFmt(SInstallOpenCodeV2Fmt, [AFileName]);
end;

function TMCPOpenCodeAgent.GetEntryCommand(AEntry: TJSONValue): string;
var
  LCommand: TJSONValue;
begin
  Result := '';
  if not (AEntry is TJSONObject) then
    Exit;
  LCommand := TJSONObject(AEntry).GetValue('command');
  if (LCommand is TJSONArray) and (TJSONArray(LCommand).Count > 0) then
    Result := TJSONArray(LCommand).Items[0].Value;
end;

function TMCPOpenCodeAgent.GetCliName: string;
begin
  Result := 'opencode';
end;

function TMCPOpenCodeAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := ProjectFile(TMCPInstallUtils.XdgConfigDir, ['opencode', 'opencode.json']);
    TMCPInstallScope.Project:
      Result := TPath.Combine(AProjectDir, 'opencode.json');
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPOpenCodeAgent.GetDisplayName: string;
begin
  Result := 'opencode';
end;

function TMCPOpenCodeAgent.GetId: string;
begin
  Result := 'opencode';
end;

function TMCPOpenCodeAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := './' + ARelativePath;
end;

function TMCPOpenCodeAgent.GetServersKey: string;
begin
  Result := 'mcp';
end;

{ TMCPCursorAgent }

function TMCPCursorAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := ProjectFile(TMCPInstallUtils.HomeDir, ['.cursor', 'mcp.json']);
    TMCPInstallScope.Project:
      Result := ProjectFile(AProjectDir, ['.cursor', 'mcp.json']);
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPCursorAgent.GetDisplayName: string;
begin
  Result := 'Cursor';
end;

function TMCPCursorAgent.GetEntryType(ATransport: TMCPInstallTransport): string;
begin
  // Cursor recognizes a remote server by its url, with no type
  Result := '';
  if ATransport = TMCPInstallTransport.Stdio then
    Result := EntryTypes[ATransport];
end;

function TMCPCursorAgent.GetId: string;
begin
  Result := 'cursor';
end;

function TMCPCursorAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := '${workspaceFolder}/' + ARelativePath;
end;

{ TMCPVSCodeAgent }

function TMCPVSCodeAgent.GetCliName: string;
begin
  Result := 'code';
end;

function TMCPVSCodeAgent.GetConfigFile(AScope: TMCPInstallScope; const AProjectDir: string): string;
begin
  case AScope of
    TMCPInstallScope.Global:
      Result := ProjectFile(TMCPInstallUtils.AppConfigDir, ['Code', 'User', 'mcp.json']);
    TMCPInstallScope.Project:
      Result := ProjectFile(AProjectDir, ['.vscode', 'mcp.json']);
  else
    raise EMCPInstallError.Create('Unexpected scope');
  end;
end;

function TMCPVSCodeAgent.GetDetectionPath: string;
begin
  Result := TPath.Combine(TMCPInstallUtils.AppConfigDir, 'Code');
end;

function TMCPVSCodeAgent.GetDisplayName: string;
begin
  Result := 'VS Code';
end;

function TMCPVSCodeAgent.GetEntryType(ATransport: TMCPInstallTransport): string;
begin
  Result := EntryTypes[ATransport];
end;

function TMCPVSCodeAgent.GetId: string;
begin
  Result := 'vscode';
end;

function TMCPVSCodeAgent.GetProjectCommand(const ARelativePath: string): string;
begin
  Result := '${workspaceFolder}/' + ARelativePath;
end;

function TMCPVSCodeAgent.GetServersKey: string;
begin
  Result := 'servers';
end;

end.
