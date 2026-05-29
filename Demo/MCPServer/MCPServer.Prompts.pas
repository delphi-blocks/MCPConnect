unit MCPServer.Prompts;

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
  TSamplePrompts = class
  public
    [MCPPrompt('simple-prompt', 'Simple Prompt', 'A prompt with no arguments')]
    function SimplePrompt: string;

    [MCPPrompt('argument-prompt', 'Argument Prompt', 'A prompt with 2 arguments')]
    function ArgumentPrompt(
     [MCPArgument('city', 'Name of the city', 'required')] const ACity: string;
     [MCPArgument('country', 'Name of the country')] const ACountry: string
    ): string;
  end;


implementation

uses
  System.StrUtils;

{ TSamplePrompts }

function TSamplePrompts.SimplePrompt: string;
begin
  Result := 'This is a simple prompt without arguments';
end;

function TSamplePrompts.ArgumentPrompt(const ACity, ACountry: string): string;
begin
  var loc := IfThen(ACountry.IsEmpty, ACity, ACity + ', ' + ACountry);
  Result := Format('What''s the weather in %s?', [loc]);
end;

end.
