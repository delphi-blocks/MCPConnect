{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.MCP.Middleware;

{
  The MCP operation hooks: tools/call, tools/list, resources/read and the rest.

  They extend MCPConnect.JRPC.Middleware without that unit knowing they exist,
  which is the whole point of one interface per hook: a middleware declares what
  it takes part in by implementing it, and the pipeline asks Supports().

    TToolAclMiddleware = class(TMiddleware, ICallToolMiddleware)

  The same instance serves every level of one message, so what a middleware works
  out in OnRequest is still in its fields when OnCallTool runs.

  The hooks that return TBaseResult do so because the operation may answer with a
  TInputRequiredResult instead of its usual result: a middleware either handles
  that case or lets it through untouched.

  See Docs/middleware.md for the full specification.
}

interface

{$I MCPConnect.inc}

uses
  System.SysUtils,

  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Resources;

type
  ICallToolMiddleware = interface;
  IListToolsMiddleware = interface;
  IReadResourceMiddleware = interface;
  IListResourcesMiddleware = interface;
  IGetPromptMiddleware = interface;
  IListPromptsMiddleware = interface;
  IDiscoverMiddleware = interface;

  /// <summary>The real handler a chain ends on: a method pointer, no closure.</summary>
  TCallToolTerminal = function (AContext: TMiddlewareContext;
    AParams: TCallToolRequestParams): TBaseResult of object;
  TListToolsTerminal = function (AContext: TMiddlewareContext;
    AParams: TPaginatedRequestParams): TListToolsResult of object;
  TReadResourceTerminal = function (AContext: TMiddlewareContext;
    AParams: TReadResourceParams): TBaseResult of object;
  TListResourcesTerminal = function (AContext: TMiddlewareContext;
    AParams: TPaginatedRequestParams): TListResourcesResult of object;
  TGetPromptTerminal = function (AContext: TMiddlewareContext;
    AParams: TGetPromptRequestParams): TBaseResult of object;
  TListPromptsTerminal = function (AContext: TMiddlewareContext;
    AParams: TPaginatedRequestParams): TListPromptsResult of object;
  TDiscoverTerminal = function (AContext: TMiddlewareContext;
    AParams: TRequestMetaParams): TDiscoverResult of object;

  /// <summary>
  ///   Cursor over the tools/call chain. A record passed by value: calling Next
  ///   does not move the caller's cursor, so a middleware may call it more than
  ///   once and walk the same tail of the chain every time.
  /// </summary>
  TCallToolChain = record
  private
    FChain: TArray<ICallToolMiddleware>;
    FIndex: Integer;
    FTerminal: TCallToolTerminal;
  public
    class function Create(const AChain: TArray<ICallToolMiddleware>;
      const ATerminal: TCallToolTerminal): TCallToolChain; static;

    /// <summary>
    ///   Runs the rest of the chain, or the real handler when no middleware is
    ///   left. Not calling it suppresses the operation, and then the middleware
    ///   has to answer with a result of its own or raise.
    /// </summary>
    function Next(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams): TBaseResult;
  end;

  /// <summary>Cursor over the tools/list chain. See TCallToolChain.</summary>
  TListToolsChain = record
  private
    FChain: TArray<IListToolsMiddleware>;
    FIndex: Integer;
    FTerminal: TListToolsTerminal;
  public
    class function Create(const AChain: TArray<IListToolsMiddleware>;
      const ATerminal: TListToolsTerminal): TListToolsChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListToolsResult;
  end;

  /// <summary>Cursor over the resources/read chain. See TCallToolChain.</summary>
  TReadResourceChain = record
  private
    FChain: TArray<IReadResourceMiddleware>;
    FIndex: Integer;
    FTerminal: TReadResourceTerminal;
  public
    class function Create(const AChain: TArray<IReadResourceMiddleware>;
      const ATerminal: TReadResourceTerminal): TReadResourceChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TReadResourceParams): TBaseResult;
  end;

  /// <summary>Cursor over the resources/list chain. See TCallToolChain.</summary>
  TListResourcesChain = record
  private
    FChain: TArray<IListResourcesMiddleware>;
    FIndex: Integer;
    FTerminal: TListResourcesTerminal;
  public
    class function Create(const AChain: TArray<IListResourcesMiddleware>;
      const ATerminal: TListResourcesTerminal): TListResourcesChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListResourcesResult;
  end;

  /// <summary>Cursor over the prompts/get chain. See TCallToolChain.</summary>
  TGetPromptChain = record
  private
    FChain: TArray<IGetPromptMiddleware>;
    FIndex: Integer;
    FTerminal: TGetPromptTerminal;
  public
    class function Create(const AChain: TArray<IGetPromptMiddleware>;
      const ATerminal: TGetPromptTerminal): TGetPromptChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TGetPromptRequestParams): TBaseResult;
  end;

  /// <summary>Cursor over the prompts/list chain. See TCallToolChain.</summary>
  TListPromptsChain = record
  private
    FChain: TArray<IListPromptsMiddleware>;
    FIndex: Integer;
    FTerminal: TListPromptsTerminal;
  public
    class function Create(const AChain: TArray<IListPromptsMiddleware>;
      const ATerminal: TListPromptsTerminal): TListPromptsChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListPromptsResult;
  end;

  /// <summary>Cursor over the server/discover chain. See TCallToolChain.</summary>
  TDiscoverChain = record
  private
    FChain: TArray<IDiscoverMiddleware>;
    FIndex: Integer;
    FTerminal: TDiscoverTerminal;
  public
    class function Create(const AChain: TArray<IDiscoverMiddleware>;
      const ATerminal: TDiscoverTerminal): TDiscoverChain; static;

    function Next(AContext: TMiddlewareContext;
      AParams: TRequestMetaParams): TDiscoverResult;
  end;

  /// <summary>
  ///   Takes part in tools/call. AParams carries the tool name and its
  ///   arguments, so this is where a call is refused, its arguments are
  ///   normalised, or its result is enriched.
  /// </summary>
  ICallToolMiddleware = interface(IMiddleware)
  ['{0D1C7C4A-9F2E-4E63-8F6D-2E3B0A5C9D71}']
    function OnCallTool(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams; const AChain: TCallToolChain): TBaseResult;
  end;

  /// <summary>
  ///   Takes part in tools/list. The result holds TMCPTool objects before they
  ///   are serialised, which is what makes per caller filtering possible.
  /// </summary>
  /// <remarks>
  ///   Removing an element is safe: the list does not own the tools. Changing
  ///   one is NOT: those are the objects of the registry, shared by every
  ///   session, so a variant for one caller has to be a copy. And a result
  ///   filtered per caller must stay CacheScope.ScopePrivate.
  /// </remarks>
  IListToolsMiddleware = interface(IMiddleware)
  ['{4B2A6E18-3C97-4D5B-9A0E-7F81D6C4B23A}']
    function OnListTools(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams; const AChain: TListToolsChain): TListToolsResult;
  end;

  /// <summary>Takes part in resources/read.</summary>
  IReadResourceMiddleware = interface(IMiddleware)
  ['{8E5F0B37-1D64-4A29-B7C3-5A9E2F8D0C46}']
    function OnReadResource(AContext: TMiddlewareContext;
      AParams: TReadResourceParams; const AChain: TReadResourceChain): TBaseResult;
  end;

  /// <summary>Takes part in resources/list. Same caveats as IListToolsMiddleware.</summary>
  IListResourcesMiddleware = interface(IMiddleware)
  ['{2F7D93A5-6C08-41BE-9D52-3A6B7E1C8F04}']
    function OnListResources(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams; const AChain: TListResourcesChain): TListResourcesResult;
  end;

  /// <summary>Takes part in prompts/get.</summary>
  IGetPromptMiddleware = interface(IMiddleware)
  ['{6A1E4D89-B052-4F37-8C96-0D5F2B7A3E18}']
    function OnGetPrompt(AContext: TMiddlewareContext;
      AParams: TGetPromptRequestParams; const AChain: TGetPromptChain): TBaseResult;
  end;

  /// <summary>Takes part in prompts/list. Same caveats as IListToolsMiddleware.</summary>
  IListPromptsMiddleware = interface(IMiddleware)
  ['{9C3B7F52-0E46-4A8D-B1F7-6D2A5C90E48B}']
    function OnListPrompts(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams; const AChain: TListPromptsChain): TListPromptsResult;
  end;

  /// <summary>
  ///   Takes part in server/discover, which is where a client first meets the
  ///   server: the natural place to turn one away before it gets any further.
  /// </summary>
  IDiscoverMiddleware = interface(IMiddleware)
  ['{5D8A2C61-7B39-4E05-A2F8-1C4E6B9D3057}']
    function OnDiscover(AContext: TMiddlewareContext;
      AParams: TRequestMetaParams; const AChain: TDiscoverChain): TDiscoverResult;
  end;

implementation

{ TCallToolChain }

class function TCallToolChain.Create(const AChain: TArray<ICallToolMiddleware>;
  const ATerminal: TCallToolTerminal): TCallToolChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TCallToolChain.Next(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams): TBaseResult;
var
  LNext: TCallToolChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  // Copying the cursor rather than advancing this one is what makes Next
  // repeatable: the caller's chain stays where it was.
  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnCallTool(AContext, AParams, LNext);
end;

{ TListToolsChain }

class function TListToolsChain.Create(const AChain: TArray<IListToolsMiddleware>;
  const ATerminal: TListToolsTerminal): TListToolsChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TListToolsChain.Next(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListToolsResult;
var
  LNext: TListToolsChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnListTools(AContext, AParams, LNext);
end;

{ TReadResourceChain }

class function TReadResourceChain.Create(const AChain: TArray<IReadResourceMiddleware>;
  const ATerminal: TReadResourceTerminal): TReadResourceChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TReadResourceChain.Next(AContext: TMiddlewareContext;
  AParams: TReadResourceParams): TBaseResult;
var
  LNext: TReadResourceChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnReadResource(AContext, AParams, LNext);
end;

{ TListResourcesChain }

class function TListResourcesChain.Create(const AChain: TArray<IListResourcesMiddleware>;
  const ATerminal: TListResourcesTerminal): TListResourcesChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TListResourcesChain.Next(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListResourcesResult;
var
  LNext: TListResourcesChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnListResources(AContext, AParams, LNext);
end;

{ TGetPromptChain }

class function TGetPromptChain.Create(const AChain: TArray<IGetPromptMiddleware>;
  const ATerminal: TGetPromptTerminal): TGetPromptChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TGetPromptChain.Next(AContext: TMiddlewareContext;
  AParams: TGetPromptRequestParams): TBaseResult;
var
  LNext: TGetPromptChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnGetPrompt(AContext, AParams, LNext);
end;

{ TListPromptsChain }

class function TListPromptsChain.Create(const AChain: TArray<IListPromptsMiddleware>;
  const ATerminal: TListPromptsTerminal): TListPromptsChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TListPromptsChain.Next(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListPromptsResult;
var
  LNext: TListPromptsChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnListPrompts(AContext, AParams, LNext);
end;

{ TDiscoverChain }

class function TDiscoverChain.Create(const AChain: TArray<IDiscoverMiddleware>;
  const ATerminal: TDiscoverTerminal): TDiscoverChain;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

function TDiscoverChain.Next(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams): TDiscoverResult;
var
  LNext: TDiscoverChain;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].OnDiscover(AContext, AParams, LNext);
end;

end.
