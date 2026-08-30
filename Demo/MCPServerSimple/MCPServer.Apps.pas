unit MCPServer.Apps;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils,

  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Attributes;

type
  TTodoAppUI = class
  public
    [McpAppUI('todo-app', 'ui://todo/todo-app', 'Shows the todo list tasks in an interactive UI')]
    function GetUI(): string;
  end;

implementation

{ TTodoAppUI }

function TTodoAppUI.GetUI(): string;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(TPath.GetAppPath, 'data');
  LFileName := TPath.Combine(LFileName, 'todo-app.html');
  Result := TFile.ReadAllText(LFileName);
end;

end.
