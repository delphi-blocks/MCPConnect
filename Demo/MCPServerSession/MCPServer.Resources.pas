unit MCPServer.Resources;

{
  ==============================================================================
   MCPConnect demo - Resources (read-only content addressed by URI)
  ==============================================================================

  Tools are called by the *model*; resources are pulled in by the *client* (or
  by the user through the client UI). Anything reference-like - documentation,
  catalogues, configuration, a report - is better modelled as a resource: it
  costs no tool slot and the client can cache it.

  MCPConnect knows three flavours, all registered from MCPServer.Config:

    [McpResource]  a fixed URI, resolved by one method          -> resources/list
    [McpTemplate]  an RFC 6570 URI template with placeholders   -> resources/templates/list
    [McpAppUI]     an interactive HTML app on the ui:// scheme  (see MCPServer.Apps.pas)

  and, without any attribute at all:

    .Resources.RegisterFile(path, description)   static files from disk
    .Resources.RegisterResource(class, method, name, uri, mime, description)
    .Resources.RegisterTemplate(...) / .RegisterUI(...)

  The attribute-free calls exist for classes that cannot carry Delphi custom
  attributes - most notably classes compiled with C++Builder - and are freely
  mixable with RegisterClass. TCppResource below is exactly that case.
}

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.JRPC.Classes,
  MCPConnect.Session.Core;

type

  /// <summary>
  ///   A resource provider with *no* attributes at all: the method name is
  ///   passed as a string in MCPServer.Config, and the framework finds it by
  ///   RTTI. This is the pattern to use from C++Builder, or when the URI and
  ///   description come from configuration rather than from source code.
  /// </summary>
  TCppResource = class
    function GetGlobalInfo: string;
  end;

  /// <summary>
  ///   Weather information for the event, in the two attribute-driven flavours.
  /// </summary>
  TWeatherResource = class
  public
    /// <summary>
    ///   A fixed resource. Parameters are (name, uri, mimeType, description).
    ///   The URI scheme is free: MCP does not restrict it, so text://, file://
    ///   or a custom demo:// scheme are all valid as long as clients only ever
    ///   pass it back verbatim.
    /// </summary>
    [McpResource('weather-resource', 'text://weather', 'text/plain', 'Shows the weather for the DelphiDay event')]
    function GetWeatherInfo: string;

    /// <summary>
    ///   A URI *template*: the {city} placeholder is filled by the client, and
    ///   MCPConnect maps the placeholders to the method parameters in order.
    ///   The client discovers it through resources/templates/list, then reads
    ///   e.g. demo://weather.app/Rome.
    ///
    ///   [McpParam] on the parameter documents it for the client exactly as it
    ///   does for a tool.
    /// </summary>
    [McpTemplate('weather-city', 'demo://weather.app/{city}', 'text/plain', 'Shows the weather for the specified city')]
    function GetWeatherCity(
     [MCPParam('city', 'Forecast city')] const ACity: string): string;


  end;


implementation

{ TWeatherResource }

function TWeatherResource.GetWeatherCity(const ACity: string): string;
begin
  // A resource method returns its content; strings are served as-is with the
  // MIME type declared in the attribute. Returning a TStream or TBytes instead
  // produces a base64 blob resource.
  Result := Format('Forecast for %s: It''s going to be a sunny day!!', [ACity]);
end;

function TWeatherResource.GetWeatherInfo: string;
begin
  Result := 'Cloudy skies early, followed by partial clearing. Slight chance of a rain shower. High 31C. Winds light and variable.';
end;

{ TCppResource }

function TCppResource.GetGlobalInfo: string;
begin
  Result := 'Global Information Given';
end;

end.
