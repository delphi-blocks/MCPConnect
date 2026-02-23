unit MCPServerDemo.Apps;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.IOUtils,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Core.Utils,
  MCPConnect.Session.Core;

type
  TDeplphiDayAppUI = class
  public
    [McpAppUI('ticket-app', 'ui://delphiday/ticket-app', 'Shows some info about the DelphiDay event and tickets')]
    function GetUI: string;
  end;

implementation

{ TDeplphiDayAppUI }

function TDeplphiDayAppUI.GetUI: string;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(TPath.GetAppPath, 'data', 'delphi-mcp-app.html');
  Result := TFile.ReadAllText(LFileName);
end;

end.
