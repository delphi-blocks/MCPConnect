unit MCPServer.Resources;

interface

uses
  System.Classes, System.SysUtils,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Attributes;

type
  TTodoResources = class
  public
    [McpResource('todo-summary', 'text://todo/summary', 'text/plain',
      'Returns a summary of the current todo list with counts by status')]
    function GetSummary(): string;
  end;

implementation

uses
  MCPServer.Tools;

{ TTodoResources }

function TTodoResources.GetSummary(): string;
begin
  Result := TodoStore.GetSummary();
end;

end.
