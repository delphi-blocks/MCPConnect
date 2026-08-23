unit MCPServer.Apps;

{
  ==============================================================================
   MCPConnect demo - MCP Apps (interactive UI served by the server)
  ==============================================================================

  An "MCP App" is an HTML document the server exposes as a resource with the
  ui:// scheme. A client that supports Apps loads it in a sandboxed iframe next
  to the conversation, so the user gets a real UI instead of a wall of text.

  Three pieces are involved:

    1. this class - a method decorated with [McpAppUI(name, uri, description)],
       returning the HTML as a string. The URI *must* use the ui:// scheme;
    2. its registration in MCPServer.Config
       (.Resources.RegisterClass(TDelphiDayAppUI)), or the attribute-free
       equivalent .Resources.RegisterUI(...);
    3. the link from a tool to the app, either with [McpApp('ui://...')] on the
       tool method or with the 'app=ui://...' tag - see TDelphiDayTool.GetTickets
       in MCPServer.Tools.pas.

  The page talks back to the host over window.postMessage using JSON-RPC
  framing: it sends ui/initialize, then ui/notifications/initialized, and from
  then on it can call tools/call and receive ui/notifications/tool-result. The
  reference implementations in the repository Apps folder (vanilla TypeScript
  and Vue) show the same protocol driven by the official Apps SDK.

  Because the HTML is loaded from disk at every call, the page can be edited
  and reloaded in the client without restarting the Delphi server - which makes
  this the fastest way to iterate on the UI.
}

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.IOUtils,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Session.Core;

type
  /// <summary>
  ///   Serves the DelphiDay ticket app.
  ///
  ///   The class is registered as a *resource* provider, not as a tool: apps
  ///   are content the client fetches by URI, exactly like any other resource.
  /// </summary>
  TDelphiDayAppUI = class
  public
    /// <summary>
    ///   [McpAppUI] parameters are (name, uri, description). The MIME type is
    ///   implied - the returned string is served as text/html.
    ///
    ///   Everything the app needs must be inside this single document
    ///   (inline CSS and JS): the client sandbox will not fetch external
    ///   resources for it.
    /// </summary>
    [McpAppUI('ticket-app', 'ui://delphiday/ticket-app', 'Shows some info about the DelphiDay event and tickets')]
    function GetUI: string;
  end;

implementation

{ TDelphiDayAppUI }

function TDelphiDayAppUI.GetUI: string;
var
  LFileName: string;
begin
  // TPath.GetAppPath is an MCPConnect helper (in MCPConnect.JRPC.Classes) that
  // back-fills the RTL method on Delphi versions that do not have it, so this
  // line compiles from Delphi 11 upwards.
  LFileName := TPath.Combine(TPath.GetAppPath, 'data');
  LFileName := TPath.Combine(LFileName, 'delphi-mcp-app.html');
  Result := TFile.ReadAllText(LFileName);
end;

end.
