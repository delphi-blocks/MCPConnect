unit MCPServer.Resources;

interface

uses
  System.Classes, System.SysUtils,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  TTodoResource = class
  public
    [McpResource('todo-summary', 'text://todo/summary', 'text/plain',
      'Returns a summary of the current todo list with counts by status')]
    function GetSummary(): string;
  end;

implementation

uses
  MCPServer.Tools;

{ TTodoResource }

function TTodoResource.GetSummary(): string;
begin
  Result := TodoStore.GetSummary();
end;

end.
