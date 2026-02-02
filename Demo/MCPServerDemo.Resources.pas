unit MCPServerDemo.Resources;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Core.Utils,
  MCPConnect.Session.Core;

type
  TWeatherResource = class
  private
    [Context] FGC: IGarbageCollector;
  public
    [McpResource('get_tickets', '', 'text/html', 'Get the list of available tickets for the DelphiDay event in Padova',
    'uri=ui://weather/info,mimetype=text/plain')]
    function GetWeatherInfo: string;
  end;


implementation

{ TWeatherResource }

function TWeatherResource.GetWeatherInfo: string;
begin

end;

end.
