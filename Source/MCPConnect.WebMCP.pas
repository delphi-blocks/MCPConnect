{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                }
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
unit MCPConnect.WebMCP;

{
  WebMCP: the server's tools, offered to the agent built into the browser

  (This is the port to the 2025 protocol - the main branch, with its in-tree
  JRPC layer, TJRPCServer and optional sessions. The 2026-07-28 branch has its
  own version, for TJRPCServer and the stateless request contract.)

  WebMCP is a browser API (document.modelContext, navigator.modelContext in
  earlier drafts) through which a web page registers JavaScript tools that the
  browser's own agent may call. It has no wire protocol: nothing about it
  reaches a server. What this unit does is make tools already registered on a
  TJRPCServer appear as WebMCP tools on pages the same process serves, with no
  second tool model beside [McpTool]:

    - a tool opts in with the "webmcp" tag ([McpTool('add_task', '...', 'webmcp')]),
      or by name through TMCPWebMCP.Expose. "webmcp=<group>" offers it only on
      the pages that render that group;
    - the page carries the list of those tools as data, in a data- attribute of
      the script tag (see TMCPWebMCPView), so the template never writes code;
    - a fixed script (TMCPWebMCP.ClientScript) registers each tool, and runs it
      as an ordinary tools/call on the MCP endpoint. When the server keeps
      sessions the script performs the initialize handshake by itself, the
      first time the server asks for one, and keeps the session id it is given;
      a session carried in a cookie needs nothing from the script at all, since
      the call is made with the page's credentials.

  After every call the script tells the page, so that it can show what the
  agent did without polling: it dispatches "mcp:toolresult" on document when the
  server answered (detail: name, arguments, result, isError - a tool that
  reported a failure still answered), and "mcp:toolerror" when the call itself
  failed (detail: name, arguments, message). A page listens with
  document.addEventListener("mcp:toolresult", ...).

  Nothing new is needed on the server side: the call goes through /mcp like any
  other, so authorization, sessions and the rest of the transport apply to it.
  The list a page advertises is a convenience, not a gate - a tool left out of
  it can still be called by anyone who may call /mcp - which is why the
  authorization that matters stays where it is, on tools/call.

  The unit depends on the RTL and on MCPConnect only: the view is a plain
  object with properties, which WebStencils reads through RTTI
  (TWebStencilsProcessor.AddVar), and which any other template engine can read
  just as well.

  Needs Delphi 12 or later (multiline string literals), and is not part of the
  runtime package: add it to the host project.

  PROTOTYPE. WebMCP is a W3C Community Group draft that has already moved once
  (navigator -> document); every assumption about its shape is in the client
  script, so that is the only thing a new draft should need to change.
}

interface

{$I MCPConnect.inc}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.Generics.Defaults,
  Web.HTTPApp,

  MCPConnect.MCP.Tools,
  MCPConnect.JRPC.Server;

const
  /// <summary>
  ///   The tool tag that offers a tool to WebMCP. Bare, it offers the tool on
  ///   every page; with a value ("webmcp=tasks") only on the pages that render
  ///   that group.
  /// </summary>
  WEBMCP_TAG = 'webmcp';

  /// <summary>
  ///   The media type the client script is served with.
  /// </summary>
  WEBMCP_SCRIPT_CONTENT_TYPE = 'text/javascript; charset=utf-8';

  /// <summary>
  ///   The Cache-Control the client script is served with. The script is the
  ///   same text for every page, so a browser may keep it for an hour.
  /// </summary>
  WEBMCP_SCRIPT_CACHE_CONTROL = 'public, max-age=3600';

type
  /// <summary>
  ///   Decides, per tool, whether the page being rendered offers it - on top of
  ///   the tag or the name list. The place for what only the application knows:
  ///   the role of the user the page is for, a feature switch.
  /// </summary>
  TMCPWebMCPFilter = reference to function (ATool: TMCPTool): Boolean;

  TMCPWebMCP = class;

  /// <summary>
  ///   What one page needs to offer its tools, as properties a template engine
  ///   can read. Built per request by TMCPWebMCP.View; hand it to WebStencils
  ///   with AddVar('WebMCP', LView, True).
  /// </summary>
  /// <remarks>
  ///   In a WebStencils template:
  ///   <code>
  ///   &lt;script src="@WebMCP.ScriptUrl" data-mcp-endpoint="@WebMCP.Endpoint"
  ///     data-mcp-tools="@WebMCP.Tools" defer&gt;&lt;/script&gt;
  ///   </code>
  ///   or, the same tag in one piece, @\WebMCP.Tag (the backslash turns off
  ///   WebStencils' HTML encoding, since Tag is encoded already). Tools is JSON
  ///   and is meant to go through the engine's HTML encoding into an attribute:
  ///   the browser decodes the attribute, and the script parses what it finds.
  /// </remarks>
  TMCPWebMCPView = class
  private
    FTools: string;
    FEndpoint: string;
    FScriptUrl: string;
    FProtocolVersion: string;
    FToolCount: Integer;
    function GetTag: string;
    function GetHasTools: Boolean;
  public
    /// <summary>
    ///   The tools this page offers, as a JSON array: name, title, description,
    ///   inputSchema and annotations of each - what tools/list says of them.
    /// </summary>
    property Tools: string read FTools;

    /// <summary>
    ///   How many tools the page offers.
    /// </summary>
    property ToolCount: Integer read FToolCount;

    /// <summary>
    ///   Whether the page offers any tool, so a template can leave the script
    ///   out altogether.
    /// </summary>
    property HasTools: Boolean read GetHasTools;

    /// <summary>
    ///   The MCP endpoint the script calls.
    /// </summary>
    property Endpoint: string read FEndpoint;

    /// <summary>
    ///   Where the client script is served.
    /// </summary>
    property ScriptUrl: string read FScriptUrl;

    /// <summary>
    ///   The protocol version the script states on every call.
    /// </summary>
    property ProtocolVersion: string read FProtocolVersion;

    /// <summary>
    ///   The whole script tag, every attribute value HTML-encoded already.
    ///   Write it unencoded (@\WebMCP.Tag in WebStencils). Empty when the page
    ///   offers no tool.
    /// </summary>
    property Tag: string read GetTag;
  end;

  /// <summary>
  ///   Offers the tools of a TJRPCServer to WebMCP on the pages the same process
  ///   serves. One per server, shared by every request; it holds no per-request
  ///   state.
  /// </summary>
  TMCPWebMCP = class
  private
    FServer: TJRPCServer;
    FEndpoint: string;
    FScriptUrl: string;
    FNames: TDictionary<string, string>;
    FFilter: TMCPWebMCPFilter;
    FLock: TObject;
    procedure ActionScript(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    function ToolGroup(ATool: TMCPTool; out AGroup: string): Boolean;
    function GroupFilter(const AGroup: string;
      const AFilter: TMCPWebMCPFilter): TMCPWebMCPFilter;
    function NamesFilter(const ANames: array of string; const AGroup: string;
      const AFilter: TMCPWebMCPFilter): TMCPWebMCPFilter;
    function BuildTools(const AOffered: TMCPWebMCPFilter;
      out ACount: Integer): string;
    function NewView(const AOffered: TMCPWebMCPFilter): TMCPWebMCPView;
  public
    /// <summary>
    ///   Offers the tools of AServer, called at AEndpoint (the PathInfo of the
    ///   TJRPCDispatcher) by a script served at AScriptUrl.
    /// </summary>
    constructor Create(AServer: TJRPCServer; const AEndpoint: string = '/mcp';
      const AScriptUrl: string = '/webmcp.js');
    destructor Destroy; override;

    /// <summary>
    ///   Offers these tools by name, as if each carried the bare "webmcp" tag:
    ///   for a tool whose declaration cannot be edited. A name with
    ///   "=group" appended offers it to that group only.
    /// </summary>
    function Expose(const ANames: array of string): TMCPWebMCP;

    /// <summary>
    ///   Sets the filter every tool must also pass to be offered.
    /// </summary>
    function SetFilter(const AFilter: TMCPWebMCPFilter): TMCPWebMCP;

    /// <summary>
    ///   Whether a page rendering AGroup offers ATool. A tool offered without a
    ///   group is offered on every page; a tool offered to a group only on the
    ///   pages that render that group. A disabled tool is never offered.
    /// </summary>
    function IsOffered(ATool: TMCPTool; const AGroup: string;
      const AFilter: TMCPWebMCPFilter = nil): Boolean;

    /// <summary>
    ///   The tools a page rendering AGroup offers, as the JSON array the client
    ///   script reads. AFilter applies to this page only, after the one set
    ///   with SetFilter.
    /// </summary>
    function ToolsJSON(const AGroup: string = '';
      const AFilter: TMCPWebMCPFilter = nil): string; overload;

    /// <summary>
    ///   The tools a page offers when it names them itself, as the JSON array
    ///   the client script reads. See View(ANames, AGroup).
    /// </summary>
    function ToolsJSON(const ANames: array of string; const AGroup: string = '';
      const AFilter: TMCPWebMCPFilter = nil): string; overload;

    /// <summary>
    ///   A view for one page. The caller owns it - or the template processor
    ///   does, when it is handed over with AddVar(..., True).
    /// </summary>
    function View(const AGroup: string = '';
      const AFilter: TMCPWebMCPFilter = nil): TMCPWebMCPView; overload;

    /// <summary>
    ///   A view for a page that names the tools it offers, so that the tool
    ///   declarations need not know the pages. The page chooses, the tool
    ///   limits: a named tool must still be opted in (the "webmcp" tag or
    ///   Expose) and pass the filter set with SetFilter, and a tool whose tag
    ///   names a group ("webmcp=checkout") is offered only when AGroup is that
    ///   group - so a sensitive tool can keep itself to the one page meant for
    ///   it, whatever list another page carries. Names are matched exactly; one
    ///   that is unknown, disabled or not allowed here is skipped. The caller
    ///   owns the view, as with View(AGroup).
    /// </summary>
    function View(const ANames: array of string; const AGroup: string = '';
      const AFilter: TMCPWebMCPFilter = nil): TMCPWebMCPView; overload;

    /// <summary>
    ///   The client script: registers the tools found on its own script tag and
    ///   runs each one as a tools/call. The same text for every page and every
    ///   server, so it can be served with long cache lifetimes.
    /// </summary>
    class function ClientScript: string; static;

    /// <summary>
    ///   Fills a response with the client script, its media type and its cache
    ///   header. For a host that wires its own route (an Indy server, say);
    ///   Handled and the status are left to the caller.
    /// </summary>
    procedure ServeScript(AResponse: TWebResponse);

    /// <summary>
    ///   Adds GET ScriptUrl to a web module, answered by ServeScript. The path
    ///   is the one the script tag points at, so the two cannot disagree.
    /// </summary>
    /// <remarks>
    ///   The module must not outlive this object: the action calls back into it.
    /// </remarks>
    procedure RegisterRoute(AModule: TCustomWebDispatcher);

    property Endpoint: string read FEndpoint;
    property ScriptUrl: string read FScriptUrl;
  end;

implementation

uses
  System.NetEncoding,

  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types,
  MCPConnect.Configuration.MCP;

const
  // The client script, one JavaScript line per Pascal line, each ending in
  // #10. Kept to ASCII and to double-quoted JavaScript strings, so no line
  // needs a quote escaped. Written as concatenated literals rather than a
  // multiline literal, which needs Delphi 12. Every assumption about the
  // WebMCP draft lives in it.
  CLIENT_SCRIPT =
    '/* MCPConnect WebMCP bridge (2025 protocol) - registers the tools listed on'#10 +
    '   this script tag and runs each one as a tools/call on the MCP endpoint. */'#10 +
    '(function () {'#10 +
    '  "use strict";'#10 +
    '  var script = document.currentScript;'#10 +
    '  var mc = document.modelContext || navigator.modelContext;'#10 +
    '  if (!script || !mc || typeof mc.registerTool !== "function") { return; }'#10 +
    #10 +
    '  var endpoint = script.getAttribute("data-mcp-endpoint") || "/mcp";'#10 +
    '  var version = script.getAttribute("data-mcp-version") || "2025-11-25";'#10 +
    '  var sessionHeader = script.getAttribute("data-mcp-session-header") || "Mcp-Session-Id";'#10 +
    '  var tools;'#10 +
    '  try { tools = JSON.parse(script.getAttribute("data-mcp-tools") || "[]"); }'#10 +
    '  catch (e) { console.warn("WebMCP: unreadable tool list", e); return; }'#10 +
    #10 +
    '  var nextId = 1;'#10 +
    #10 +
    '  // Tells the page what a tool did, so that it can show the change without'#10 +
    '  // polling. A listener that throws is reported by the browser, and does'#10 +
    '  // not reach the agent.'#10 +
    '  function announce(type, detail) {'#10 +
    '    try { document.dispatchEvent(new CustomEvent(type, { detail: detail })); }'#10 +
    '    catch (e) { /* no DOM events here: nothing to tell */ }'#10 +
    '  }'#10 +
    '  // Learned from an initialize, for a server that wants the id in a header.'#10 +
    '  // A session in a cookie needs none of this: the browser sends it.'#10 +
    '  var sessionId = null;'#10 +
    '  var handshake = null;'#10 +
    #10 +
    '  function send(body) {'#10 +
    '    var headers = {'#10 +
    '      "Content-Type": "application/json",'#10 +
    '      "Accept": "application/json",'#10 +
    '      "MCP-Protocol-Version": version'#10 +
    '    };'#10 +
    '    if (sessionId) { headers[sessionHeader] = sessionId; }'#10 +
    '    return fetch(endpoint, {'#10 +
    '      method: "POST", credentials: "same-origin", cache: "no-store",'#10 +
    '      headers: headers, body: JSON.stringify(body)'#10 +
    '    });'#10 +
    '  }'#10 +
    #10 +
    '  // The body of a reply: JSON, or the last data line of an event stream'#10 +
    '  function readMessage(response) {'#10 +
    '    var type = response.headers.get("Content-Type") || "";'#10 +
    '    return response.text().then(function (text) {'#10 +
    '      if (type.indexOf("text/event-stream") >= 0) {'#10 +
    '        var last = null;'#10 +
    '        text.split(/\r?\n/).forEach(function (line) {'#10 +
    '          if (line.indexOf("data:") === 0) { last = line.substring(5).trim(); }'#10 +
    '        });'#10 +
    '        text = last;'#10 +
    '      }'#10 +
    '      try { return JSON.parse(text); }'#10 +
    '      catch (e) { throw new Error("MCP endpoint answered HTTP " + response.status); }'#10 +
    '    });'#10 +
    '  }'#10 +
    #10 +
    '  // initialize, then the initialized notification: what a server that keeps'#10 +
    '  // sessions asks of a client before anything else'#10 +
    '  function initialize() {'#10 +
    '    if (handshake) { return handshake; }'#10 +
    '    sessionId = null;'#10 +
    '    handshake = send({'#10 +
    '      jsonrpc: "2.0", id: nextId++, method: "initialize",'#10 +
    '      params: {'#10 +
    '        protocolVersion: version, capabilities: {},'#10 +
    '        clientInfo: { name: "mcpconnect-webmcp", version: "0.1" }'#10 +
    '      }'#10 +
    '    }).then(function (response) {'#10 +
    '      var id = response.headers.get(sessionHeader);'#10 +
    '      if (id) { sessionId = id; }'#10 +
    '      return readMessage(response);'#10 +
    '    }).then(function (message) {'#10 +
    '      if (message.error) { throw new Error(message.error.message || "initialize failed"); }'#10 +
    '      return send({ jsonrpc: "2.0", method: "notifications/initialized" });'#10 +
    '    }).then(function () { handshake = null; }, function (e) { handshake = null; throw e; });'#10 +
    '    return handshake;'#10 +
    '  }'#10 +
    #10 +
    '  function call(tool, args) {'#10 +
    '    return send({'#10 +
    '      jsonrpc: "2.0", id: nextId++, method: "tools/call",'#10 +
    '      params: { name: tool.name, arguments: args }'#10 +
    '    });'#10 +
    '  }'#10 +
    #10 +
    '  function callTool(tool, args) {'#10 +
    '    args = args || {};'#10 +
    '    return call(tool, args).then(function (response) {'#10 +
    '      // 400 and 404 are how a server that keeps sessions says it has none'#10 +
    '      // for this client (never initialized, or expired): shake hands, once'#10 +
    '      if (response.status === 400 || response.status === 404) {'#10 +
    '        return initialize().then(function () { return call(tool, args); });'#10 +
    '      }'#10 +
    '      return response;'#10 +
    '    }).then(readMessage).then(function (message) {'#10 +
    '      if (message.error) { throw new Error(message.error.message || ("MCP error " + message.error.code)); }'#10 +
    '      return message.result || {};'#10 +
    '    });'#10 +
    '  }'#10 +
    #10 +
    '  tools.forEach(function (tool) {'#10 +
    '    var definition = {'#10 +
    '      name: tool.name,'#10 +
    '      description: tool.description || tool.title || tool.name,'#10 +
    '      inputSchema: tool.inputSchema || { type: "object", properties: {} },'#10 +
    '      execute: function (args) {'#10 +
    '        args = args || {};'#10 +
    '        return callTool(tool, args).then(function (result) {'#10 +
    '          announce("mcp:toolresult", {'#10 +
    '            name: tool.name, arguments: args, result: result, isError: result.isError === true'#10 +
    '          });'#10 +
    '          return result;'#10 +
    '        }, function (e) {'#10 +
    '          announce("mcp:toolerror", {'#10 +
    '            name: tool.name, arguments: args, message: (e && e.message) || String(e)'#10 +
    '          });'#10 +
    '          throw e;'#10 +
    '        });'#10 +
    '      }'#10 +
    '    };'#10 +
    '    if (tool.title) { definition.title = tool.title; }'#10 +
    '    if (tool.annotations) { definition.annotations = tool.annotations; }'#10 +
    '    try { mc.registerTool(definition); }'#10 +
    '    catch (e) { console.warn("WebMCP: cannot register " + tool.name, e); }'#10 +
    '  });'#10 +
    '})();'#10;

type
  /// <summary>
  ///   The four behavior hints of a tool, as a page carries them.
  /// </summary>
  TWebMCPAnnotations = class
  public
    ReadOnlyHint: Nullable<Boolean>;
    DestructiveHint: Nullable<Boolean>;
    IdempotentHint: Nullable<Boolean>;
    OpenWorldHint: Nullable<Boolean>;
  end;

  /// <summary>
  ///   One tool as a page carries it: what tools/list says of it, minus what
  ///   only an MCP client uses (outputSchema, icons, _meta). A class of its own,
  ///   so the shape a page carries does not move with TMCPTool.
  /// </summary>
  TWebMCPTool = class
  public
    Name: string;
    Title: Nullable<string>;
    Description: Nullable<string>;
    InputSchema: TJSONObject;
    [NeonInclude(IncludeIf.NotEmpty)] Annotations: TWebMCPAnnotations;

    destructor Destroy; override;
  end;

{ TWebMCPTool }

destructor TWebMCPTool.Destroy;
begin
  InputSchema.Free;
  Annotations.Free;
  inherited;
end;

{ TMCPWebMCPView }

function TMCPWebMCPView.GetHasTools: Boolean;
begin
  Result := FToolCount > 0;
end;

function TMCPWebMCPView.GetTag: string;

  function Attr(const AValue: string): string;
  begin
    Result := TNetEncoding.HTML.Encode(AValue);
  end;

begin
  if FToolCount = 0 then
    Exit('');

  Result := Format('<script src="%s" data-mcp-endpoint="%s" data-mcp-version="%s" ' +
    'data-mcp-tools="%s" defer></script>',
    [Attr(FScriptUrl), Attr(FEndpoint), Attr(FProtocolVersion), Attr(FTools)]);
end;

{ TMCPWebMCP }

constructor TMCPWebMCP.Create(AServer: TJRPCServer; const AEndpoint, AScriptUrl: string);
begin
  inherited Create;
  if not Assigned(AServer) then
    raise EArgumentNilException.Create('TMCPWebMCP needs a TJRPCServer');

  FServer := AServer;
  FEndpoint := AEndpoint;
  FScriptUrl := AScriptUrl;
  FNames := TDictionary<string, string>.Create;
  FLock := TObject.Create;
end;

destructor TMCPWebMCP.Destroy;
begin
  FNames.Free;
  FLock.Free;
  inherited;
end;

function TMCPWebMCP.Expose(const ANames: array of string): TMCPWebMCP;
var
  LEntry, LName, LGroup: string;
  LPos: Integer;
begin
  TMonitor.Enter(FLock);
  try
    for LEntry in ANames do
    begin
      LPos := LEntry.IndexOf('=');
      if LPos < 0 then
      begin
        LName := LEntry.Trim;
        LGroup := '';
      end
      else
      begin
        LName := LEntry.Substring(0, LPos).Trim;
        LGroup := LEntry.Substring(LPos + 1).Trim;
      end;
      if not LName.IsEmpty then
        FNames.AddOrSetValue(LName, LGroup);
    end;
  finally
    TMonitor.Exit(FLock);
  end;
  Result := Self;
end;

function TMCPWebMCP.SetFilter(const AFilter: TMCPWebMCPFilter): TMCPWebMCP;
begin
  FFilter := AFilter;
  Result := Self;
end;

function TMCPWebMCP.ToolGroup(ATool: TMCPTool; out AGroup: string): Boolean;
begin
  // The tag first: it is where the declaration says so
  if Assigned(ATool.Tags) and ATool.Tags.Exists(WEBMCP_TAG) then
  begin
    AGroup := ATool.Tags.GetValueAs<string>(WEBMCP_TAG).Trim;
    Exit(True);
  end;

  TMonitor.Enter(FLock);
  try
    Result := FNames.TryGetValue(ATool.Name, AGroup);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMCPWebMCP.IsOffered(ATool: TMCPTool; const AGroup: string;
  const AFilter: TMCPWebMCPFilter): Boolean;
var
  LToolGroup: string;
begin
  if (ATool = nil) or ATool.Disabled then
    Exit(False);

  if not ToolGroup(ATool, LToolGroup) then
    Exit(False);

  // Ungrouped is everywhere; grouped only where the page asks for that group
  if not LToolGroup.IsEmpty and not SameText(LToolGroup, AGroup) then
    Exit(False);

  if Assigned(FFilter) and not FFilter(ATool) then
    Exit(False);

  Result := not Assigned(AFilter) or AFilter(ATool);
end;

function TMCPWebMCP.GroupFilter(const AGroup: string;
  const AFilter: TMCPWebMCPFilter): TMCPWebMCPFilter;
begin
  Result :=
    function (ATool: TMCPTool): Boolean
    begin
      Result := IsOffered(ATool, AGroup, AFilter);
    end;
end;

function TMCPWebMCP.NamesFilter(const ANames: array of string; const AGroup: string;
  const AFilter: TMCPWebMCPFilter): TMCPWebMCPFilter;
var
  LNames: TArray<string>;
  LIndex: Integer;
begin
  // An open array cannot be captured: the closure keeps a copy
  SetLength(LNames, Length(ANames));
  for LIndex := 0 to High(ANames) do
    LNames[LIndex] := ANames[LIndex].Trim;

  Result :=
    function (ATool: TMCPTool): Boolean
    var
      LName: string;
    begin
      // IsOffered: opted in, enabled, in this page's group if the tool names
      // one, and through both filters. The list only selects among those.
      Result := False;
      for LName in LNames do
        if LName = ATool.Name then
          Exit(IsOffered(ATool, AGroup, AFilter));
    end;
end;

function ToolToWeb(ATool: TMCPTool): TWebMCPTool;
var
  LHints: TToolAnnotation;
begin
  Result := TWebMCPTool.Create;
  try
    Result.Name := ATool.Name;
    if Assigned(ATool.Annotations) and ATool.Annotations.Title.HasValue and
      not ATool.Annotations.Title.Value.IsEmpty then
      Result.Title := ATool.Annotations.Title.Value;
    Result.Description := ATool.Description;

    if Assigned(ATool.InputSchema) then
      Result.InputSchema := ATool.InputSchema.Clone as TJSONObject
    else
      Result.InputSchema := TJSONObject.ParseJSONValue(
        '{"type":"object","properties":{}}') as TJSONObject;

    LHints := ATool.Annotations;
    if Assigned(LHints) and (LHints.ReadOnlyHint.HasValue or LHints.DestructiveHint.HasValue or
      LHints.IdempotentHint.HasValue or LHints.OpenWorldHint.HasValue) then
    begin
      Result.Annotations := TWebMCPAnnotations.Create;
      Result.Annotations.ReadOnlyHint := LHints.ReadOnlyHint;
      Result.Annotations.DestructiveHint := LHints.DestructiveHint;
      Result.Annotations.IdempotentHint := LHints.IdempotentHint;
      Result.Annotations.OpenWorldHint := LHints.OpenWorldHint;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMCPWebMCP.ToolsJSON(const AGroup: string;
  const AFilter: TMCPWebMCPFilter): string;
var
  LCount: Integer;
begin
  Result := BuildTools(GroupFilter(AGroup, AFilter), LCount);
end;

function TMCPWebMCP.ToolsJSON(const ANames: array of string; const AGroup: string;
  const AFilter: TMCPWebMCPFilter): string;
var
  LCount: Integer;
begin
  Result := BuildTools(NamesFilter(ANames, AGroup, AFilter), LCount);
end;

function TMCPWebMCP.BuildTools(const AOffered: TMCPWebMCPFilter;
  out ACount: Integer): string;
var
  LList: TListToolsResult;
  LTools: TObjectList<TWebMCPTool>;
  LTool: TMCPTool;
begin
  LTools := TObjectList<TWebMCPTool>.Create(True);
  try
    // ListEnabled leaves the disabled out, and hands back the registry's own
    // objects in a list that does not own them
    LList := FServer.GetConfiguration<TMCPConfig>.Tools.ListEnabled(
      function (ATool: TMCPTool): Boolean
      begin
        Result := AOffered(ATool);
      end);
    try
      // Sorted by name, as tools/list is, so the same page renders the same
      // attribute every time
      LList.Tools.Sort(TComparer<TMCPTool>.Construct(
        function (const ALeft, ARight: TMCPTool): Integer
        begin
          Result := CompareStr(ALeft.Name, ARight.Name);
        end));

      for LTool in LList.Tools do
        LTools.Add(ToolToWeb(LTool));
      ACount := LTools.Count;
    finally
      LList.Free;
    end;

    Result := TNeon.ObjectToJSONString(LTools, MCPNeonConfig);
  finally
    LTools.Free;
  end;
end;

function TMCPWebMCP.View(const AGroup: string;
  const AFilter: TMCPWebMCPFilter): TMCPWebMCPView;
begin
  Result := NewView(GroupFilter(AGroup, AFilter));
end;

function TMCPWebMCP.View(const ANames: array of string; const AGroup: string;
  const AFilter: TMCPWebMCPFilter): TMCPWebMCPView;
begin
  Result := NewView(NamesFilter(ANames, AGroup, AFilter));
end;

function TMCPWebMCP.NewView(const AOffered: TMCPWebMCPFilter): TMCPWebMCPView;
begin
  Result := TMCPWebMCPView.Create;
  try
    Result.FTools := BuildTools(AOffered, Result.FToolCount);
    Result.FEndpoint := FEndpoint;
    Result.FScriptUrl := FScriptUrl;
    Result.FProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPWebMCP.ServeScript(AResponse: TWebResponse);
begin
  AResponse.ContentType := WEBMCP_SCRIPT_CONTENT_TYPE;
  AResponse.SetCustomHeader('Cache-Control', WEBMCP_SCRIPT_CACHE_CONTROL);
  AResponse.Content := ClientScript;
end;

procedure TMCPWebMCP.ActionScript(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  ServeScript(Response);
  Handled := True;
end;

procedure TMCPWebMCP.RegisterRoute(AModule: TCustomWebDispatcher);
begin
  var LAction := AModule.Actions.Add;
  LAction.Name := 'WebMCPScript';
  LAction.PathInfo := FScriptUrl;
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionScript;
end;

class function TMCPWebMCP.ClientScript: string;
begin
  // Every line of the constant ends in #10 already, whatever line breaks the
  // source file has, so the text needs no normalizing.
  Result := CLIENT_SCRIPT;
end;

end.
