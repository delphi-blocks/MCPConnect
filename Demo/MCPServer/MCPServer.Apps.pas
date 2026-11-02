unit MCPServer.Apps;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils,

  MCPConnect.MCP.Attributes;

type
  /// <summary>
  ///   MCP App: the interactive todo list. [McpAppUI] publishes the page as a
  ///   ui:// resource with the text/html;profile=mcp-app type, and the list_tasks
  ///   tool links to it with [McpApp]. Hosts that support MCP Apps render it next
  ///   to the tool result; the others just get the text.
  /// </summary>
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
  // Read at every resources/read: edit the HTML and reload the app in the
  // client without restarting the server
  LFileName := TPath.Combine(TPath.GetAppPath, 'data');
  LFileName := TPath.Combine(LFileName, 'todo-app.html');
  Result := TFile.ReadAllText(LFileName);
end;

end.
