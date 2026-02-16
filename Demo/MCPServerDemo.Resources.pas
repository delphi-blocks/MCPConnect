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
  public
    [McpResource('weather', 'ui://weather/info', 'text/html', 'Shows the weather for the DelphiDay event')]
    function GetWeatherInfo: string;
  end;


implementation

{ TWeatherResource }

function TWeatherResource.GetWeatherInfo: string;
begin
  Result := 'For the Delphi Day is expected a sunny day!';
end;

end.
