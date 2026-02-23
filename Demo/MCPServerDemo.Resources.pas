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

    [McpTemplate('weather-city', 'text://weather.city/{city}', 'text/plain', 'Shows the weather for the specified city')]
    function GetWeatherCity(
     [MCPParam('city', 'Forecast city')] const ACity: string): string;


  end;


implementation

{ TWeatherResource }

function TWeatherResource.GetWeatherCity(const ACity: string): string;
begin
  Result := 'Forecast for Piacenza is: Sunny!';
end;

function TWeatherResource.GetWeatherInfo: string;
begin
  Result := 'For the Delphi Day is expected a sunny day!';
end;

end.
