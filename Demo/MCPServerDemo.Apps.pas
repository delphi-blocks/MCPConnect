unit MCPServerDemo.Apps;

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
  TWeatherApp = class
  public
    [McpApp('weather-app', 'ui://weather/info', 'Shows the weather for the DelphiDay event')]
    function GetWeatherUI: string;
  end;

implementation

{ TWeatherApp }

function TWeatherApp.GetWeatherUI: string;
begin
  Result := '<html><body><h1>Weather App UI</h1></body></html>';
end;

end.
