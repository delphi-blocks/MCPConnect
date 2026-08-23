unit MCPServer.Prompts;

{
  ==============================================================================
   MCPConnect demo - Prompts (reusable message templates)
  ==============================================================================

  Prompts are the third thing an MCP server can expose, and the least known.
  They are *user-initiated*: the client lists them (prompts/list) and usually
  surfaces them as slash commands, a menu or a template picker, then fetches
  the filled-in text with prompts/get.

  Rule of thumb:
    tool     - the model decides to call it
    resource - the client decides to attach it
    prompt   - the user decides to run it

  So prompts are the right home for the canned workflows of your application:
  "summarise this order", "draft the answer to this ticket", "explain this
  error code". They keep the wording under your control instead of leaving it
  to whatever the user types.

  A prompt method may return a plain string (turned into a single user
  message) or a TMCPGetPromptResult when you need several messages, explicit
  roles or embedded resources.
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
  ///   Registered in MCPServer.Config with .Prompts.RegisterClass(TSamplePrompts).
  ///   As with tools, only the decorated methods become visible.
  /// </summary>
  TSamplePrompts = class
  public
    /// <summary>
    ///   [MCPPrompt] parameters are (name, title, description):
    ///     name        - the protocol identifier, ^[a-zA-Z0-9_-]{1,64}$
    ///     title       - the human-readable label shown by the client
    ///     description - the longer explanation
    /// </summary>
    [MCPPrompt('simple-prompt', 'Simple Prompt', 'A prompt with no arguments')]
    function SimplePrompt: string;

    /// <summary>
    ///   Prompt arguments are declared with [MCPArgument] (an alias of
    ///   [MCPParam]) and published in prompts/list so the client can render a
    ///   form. The third tag argument marks a required one: without
    ///   'required', the client may omit it and the parameter arrives empty -
    ///   which is exactly what the IfThen below copes with.
    /// </summary>
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
  // A string result becomes a single message with role "user".
  Result := 'This is a simple prompt without arguments';
end;

function TSamplePrompts.ArgumentPrompt(const ACity, ACountry: string): string;
begin
  // Note that the optional argument has to be handled defensively: an omitted
  // argument arrives as an empty string, not as an error.
  var loc := IfThen(ACountry.IsEmpty, ACity, ACity + ', ' + ACountry);
  Result := Format('What''s the weather in %s?', [loc]);
end;

end.
