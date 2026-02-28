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
    [McpResource('weather-resource', 'text://weather', 'text/plain', 'Shows the weather for the DelphiDay event')]
    function GetWeatherInfo: string;

    [McpTemplate('weather-city', 'demo://weather.app/{city}', 'text/plain', 'Shows the weather for the specified city')]
    function GetWeatherCity(
     [MCPParam('city', 'Forecast city')] const ACity: string): string;


  end;


implementation

{ TWeatherResource }

function TWeatherResource.GetWeatherCity(const ACity: string): string;
begin
  Result := Format('Forecast for %s: It''s going to be a sunny day!!', [ACity]);
end;

function TWeatherResource.GetWeatherInfo: string;
begin
  Result := 'Cloudy skies early, followed by partial clearing. Slight chance of a rain shower. High 31°. Winds light and variable.';
end;

end.
