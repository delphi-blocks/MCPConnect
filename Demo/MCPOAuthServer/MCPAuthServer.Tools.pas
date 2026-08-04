unit MCPAuthServer.Tools;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Rtti,

  Vcl.Graphics, Vcl.ExtCtrls, Vcl.Dialogs,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,


  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Classes,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Attributes,
  MCPConnect.Session.Core;

type
  TPerson = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
    constructor Create(const AName: string);
  end;

  TTestTool = class
  public
    [McpTool('get-person', 'Get a person info given his name', 'icon=person.png')]
    function GetPerson(
      [McpParam('name', 'The name of the person to get')] const AName: string
    ): TPerson;

  end;

implementation

{ TTestTool }

function TTestTool.GetPerson(const AName: string): TPerson;
begin
  Result := TPerson.Create(AName);
end;

{ TPerson }

constructor TPerson.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

end.
