unit MCPServer.Apps;

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
  TDelphiDayAppUI = class
  public
    [McpAppUI('ticket-app', 'ui://delphiday/ticket-app', 'Shows some info about the DelphiDay event and tickets')]
    function GetUI: string;
  end;

implementation

{ TDelphiDayAppUI }

function TDelphiDayAppUI.GetUI: string;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(TPath.GetAppPath, 'data');
  LFileName := TPath.Combine(LFileName, 'delphi-mcp-app.html');
  Result := TFile.ReadAllText(LFileName);
end;

end.
