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

  Every hook is named Handle: what it takes part in is said by the interface, not
  by the name of the method. A middleware implementing more than one hook maps
  them with a method resolution clause, which is where the operation gets named
  again:

    function ICallToolMiddleware.Handle = OnCallTool;

  The same instance serves every level of one message, so what a middleware works
  out in OnRequest is still in its fields when Handle runs.

  The hooks that return TBaseResult do so because the operation may answer with a
  TInputRequiredResult instead of its usual result: a middleware either handles
  that case or lets it through untouched.
}

interface

{$I MCPConnect.inc}

uses
  System.SysUtils, System.TypInfo,

  JRPC.Core,

  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Resources;

resourcestring
  SMCPMiddlewareWrongClass =
    '%s: a universal middleware answered with a %s of class %s, but this ' +
    'operation works on %s';

type
  /// <summary>
  ///   Raised when an IMCPMiddleware hands back an object the operation cannot
  ///   use. It is a server side programming error, so it travels to the client
  ///   as an internal error.
  /// </summary>
  EMCPMiddlewareError = class(EMCPException);

  TMiddlewareTerminal<TParams, TResult> = function(
    AContext: TMiddlewareContext; AParams: TParams): TResult of object;

  IOperationMiddleware<TParams: class; TResult: class> = interface;   // forward

  /// <summary>
  ///   Cursor over the chain of one MCP operation. A record passed by value:
  ///   calling Next does not move the caller's cursor, so a middleware may call
  ///   it more than once and walk the same tail of the chain every time.
  /// </summary>
  /// <remarks>
  ///   The array holds the hooks as the base generic interface, while the
  ///   pipeline selects them by the GUID of the concrete one: see Run.
  /// </remarks>
  TMiddlewareChain<TParams: class; TResult: class> = record
  private
    FChain: TArray<IOperationMiddleware<TParams, TResult>>;
    FIndex: Integer;
    FTerminal: TMiddlewareTerminal<TParams, TResult>;
  public
    class function Create(
      const AChain: TArray<IOperationMiddleware<TParams, TResult>>;
      const ATerminal: TMiddlewareTerminal<TParams, TResult>
    ): TMiddlewareChain<TParams, TResult>; static;

    /// <summary>
    ///   The whole of what an api method does: finds the pipeline of the
    ///   request, goes straight to the handler when there is nothing to run,
    ///   and otherwise builds the chain of TIntf and walks it. TIntf is the
    ///   hook interface carrying the GUID, e.g. ICallToolMiddleware.
    /// </summary>
    /// <remarks>
    ///   The pipeline is looked up rather than injected with [Context], because
    ///   injection raises when the entry is missing, and it is missing on every
    ///   path that has no transport behind it - the api classes are called
    ///   directly by the tests and by the session inbound thread.
    /// </remarks>
    class function Run<TIntf: IOperationMiddleware<TParams, TResult>>(
      ARPCContext: TJRPCContext;
      const ATerminal: TMiddlewareTerminal<TParams, TResult>;
      AParams: TParams): TResult; static;

    /// <summary>
    ///   Runs the rest of the chain, or the real handler when no middleware is
    ///   left. Not calling it suppresses the operation, and then the middleware
    ///   has to answer with a result of its own or raise.
    /// </summary>
    function Next(AContext: TMiddlewareContext; AParams: TParams): TResult;
  end;

  /// <summary>
  ///   Common shape of every MCP operation hook. Never implemented directly: a
  ///   middleware implements one of the interfaces below, which is what gives it
  ///   the GUID the pipeline matches on.
  /// </summary>
  IOperationMiddleware<TParams: class; TResult: class> = interface(IMiddleware)
    function Handle(AContext: TMiddlewareContext; AParams: TParams;
      const AChain: TMiddlewareChain<TParams, TResult>): TResult;
  end;

  /// <summary>
  ///   Takes part in tools/call. AParams carries the tool name and its arguments,
  ///   so this is where a call is refused, its arguments are normalised, or its
  ///   result is enriched.
  /// </summary>
  ICallToolMiddleware = interface(IOperationMiddleware<TCallToolRequestParams, TBaseResult>)
    ['{4F0C6F52-E450-4BA4-887A-DFCA6A518DC4}']
  end;
  TCallToolTerminal = TMiddlewareTerminal<TCallToolRequestParams, TBaseResult>;
  TCallToolChain = TMiddlewareChain<TCallToolRequestParams, TBaseResult>;

  /// <summary>
  ///   Takes part in tools/list. The result holds TMCPTool objects before they are
  ///   serialised, which is what makes per caller filtering possible.
  /// </summary>
  IListToolsMiddleware = interface(IOperationMiddleware<TPaginatedRequestParams, TListToolsResult>)
    ['{DEC41998-26F1-4FE7-AAD8-ED56334E4561}']
  end;
  TListToolsTerminal = TMiddlewareTerminal<TPaginatedRequestParams, TListToolsResult>;
  TListToolsChain = TMiddlewareChain<TPaginatedRequestParams, TListToolsResult>;

  /// <summary>
  ///   Takes part in resources/read.
  /// </summary>
  IReadResourceMiddleware = interface(IOperationMiddleware<TReadResourceParams, TBaseResult>)
    ['{85C13ADB-95C7-457E-A9D1-20BFD72D5109}']
  end;
  TReadResourceTerminal = TMiddlewareTerminal<TReadResourceParams, TBaseResult>;
  TReadResourceChain = TMiddlewareChain<TReadResourceParams, TBaseResult>;

  /// <summary>
  ///   Takes part in resources/list. Same caveats as IListToolsMiddleware.
  /// </summary>
  IListResourcesMiddleware = interface(IOperationMiddleware<TPaginatedRequestParams, TListResourcesResult>)
    ['{770E82EE-F7DA-42AE-A14C-E546C96CA018}']
  end;
  TListResourcesTerminal = TMiddlewareTerminal<TPaginatedRequestParams, TListResourcesResult>;
  TListResourcesChain = TMiddlewareChain<TPaginatedRequestParams, TListResourcesResult>;

  /// <summary>
  ///   Takes part in prompts/get.
  /// </summary>
  IGetPromptMiddleware = interface(IOperationMiddleware<TGetPromptRequestParams, TBaseResult>)
    ['{44A6C7B4-D4C4-4CEA-98E6-24C7155C4136}']
  end;
  TGetPromptTerminal = TMiddlewareTerminal<TGetPromptRequestParams, TBaseResult>;
  TGetPromptChain = TMiddlewareChain<TGetPromptRequestParams, TBaseResult>;

  /// <summary>
  ///   Takes part in prompts/list. Same caveats as IListToolsMiddleware.
  /// </summary>
  IListPromptsMiddleware = interface(IOperationMiddleware<TPaginatedRequestParams, TListPromptsResult>)
    ['{BE6BA734-E7EF-4DDB-998A-1F037D235113}']
  end;
  TListPromptsTerminal = TMiddlewareTerminal<TPaginatedRequestParams, TListPromptsResult>;
  TListPromptsChain = TMiddlewareChain<TPaginatedRequestParams, TListPromptsResult>;

  /// <summary>
  ///   Takes part in server/discover, which is where a client first meets the
  ///   server: the natural place to turn one away before it gets any further.
  /// </summary>
  IDiscoverMiddleware = interface(IOperationMiddleware<TRequestMetaParams, TDiscoverResult>)
    ['{8019F132-38F8-4D3B-A641-727A9825EC87}']
  end;
  TDiscoverTerminal = TMiddlewareTerminal<TRequestMetaParams, TDiscoverResult>;
  TDiscoverChain = TMiddlewareChain<TRequestMetaParams, TDiscoverResult>;

  /// <summary>
  ///   Takes part in every MCP operation that has a chain, whatever it is: one
  ///   hook instead of the seven above. AParams arrives deserialised and the
  ///   result is the live object, before it is serialised, so this is where a
  ///   result is enriched, replaced, or the operation refused outright.
  ///   AContext.Method says which operation is running.
  /// </summary>
  /// <remarks>
  ///   It wraps the hook of the specific operation: what runs here is outside
  ///   ICallToolMiddleware and the rest. Priorities order the middleware within
  ///   a chain, never across two, so a universal hook always encloses a
  ///   specific one however they are registered.
  ///
  ///   A middleware replacing the result, or the params, owns what it discards:
  ///   only the object the operation finally returns reaches the garbage
  ///   collector of the request. Hand the discarded one to AContext.Own.
  ///
  ///   The four operations with no chain of their own - resources/templates/list,
  ///   completion/complete, subscriptions/listen and the subscriptions
  ///   acknowledgement - are NOT covered.
  /// </remarks>
  IMCPMiddleware = interface(IOperationMiddleware<TRequestMetaParams, TBaseResult>)
    ['{1287C269-EC63-4311-AF02-CDE5B7C2B2FD}']
  end;
  TMCPTerminal = TMiddlewareTerminal<TRequestMetaParams, TBaseResult>;
  TMCPChain = TMiddlewareChain<TRequestMetaParams, TBaseResult>;

  /// <summary>
  ///   Checked narrowing towards a generic type parameter. Neither "is" nor
  ///   "as" works on one, so the class is compared through its RTTI.
  /// </summary>
  /// <remarks>
  ///   Internal to the chain machinery. It sits here, and not in the
  ///   implementation section, because a method of a parameterized type
  ///   declared in the interface may not reach for a local symbol (E2506).
  /// </remarks>
  TMCPCast = class
    /// <summary>The operation being run, or blank outside a message.</summary>
    class function MethodOf(AContext: TMiddlewareContext): string; static;

    class function NarrowTo<T: class>(AObject: TObject;
      const AWhat, AMethod: string): T; static;
  end;

  /// <summary>
  ///   Where the universal chain hands over to the chain of the operation. A
  ///   record on the stack of Run: a method pointer needs an owner, and this
  ///   one costs no allocation.
  /// </summary>
  TMCPBridge<TParams: class; TResult: class> = record
  private
    FSpecific: TMiddlewareChain<TParams, TResult>;
  public
    class function Over(
      const ASpecific: TMiddlewareChain<TParams, TResult>): TMCPBridge<TParams, TResult>; static;

    /// <summary>Matches TMCPTerminal.</summary>
    function Proceed(AContext: TMiddlewareContext;
      AParams: TRequestMetaParams): TBaseResult;
  end;

implementation

{ TMCPCast }

class function TMCPCast.MethodOf(AContext: TMiddlewareContext): string;
begin
  if Assigned(AContext) then
    Result := AContext.Method
  else
    Result := '';
end;

class function TMCPCast.NarrowTo<T>(AObject: TObject;
  const AWhat, AMethod: string): T;
var
  LExpected: TClass;
begin
  // nil passes through: suppressing an operation without a result is allowed.
  if not Assigned(AObject) then
    Exit(nil);

  LExpected := GetTypeData(TypeInfo(T)).ClassType;
  if not AObject.InheritsFrom(LExpected) then
    raise EMCPMiddlewareError.CreateFmt(SMCPMiddlewareWrongClass,
      [AMethod, AWhat, AObject.ClassName, LExpected.ClassName]);

  Result := T(AObject);
end;

{ TMCPBridge<TParams, TResult> }

class function TMCPBridge<TParams, TResult>.Over(
  const ASpecific: TMiddlewareChain<TParams, TResult>): TMCPBridge<TParams, TResult>;
begin
  Result.FSpecific := ASpecific;
end;

function TMCPBridge<TParams, TResult>.Proceed(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams): TBaseResult;
begin
  // Both conversions have to be spelled out: the compiler cannot know that
  // TParams descends from TRequestMetaParams, nor TResult from TBaseResult.
  // Narrowing the params and not merely ignoring them is what lets a universal
  // middleware hand a different object down the chain.
  Result := TMCPCast.NarrowTo<TBaseResult>(
    TObject(FSpecific.Next(AContext,
      TMCPCast.NarrowTo<TParams>(AParams, 'params', TMCPCast.MethodOf(AContext)))),
    'result', TMCPCast.MethodOf(AContext));
end;

{ TMiddlewareChain<TParams, TResult> }

class function TMiddlewareChain<TParams, TResult>.Create(
  const AChain: TArray<IOperationMiddleware<TParams, TResult>>;
  const ATerminal: TMiddlewareTerminal<TParams, TResult>): TMiddlewareChain<TParams, TResult>;
begin
  Result.FChain := AChain;
  Result.FIndex := 0;
  Result.FTerminal := ATerminal;
end;

class function TMiddlewareChain<TParams, TResult>.Run<TIntf>(
  ARPCContext: TJRPCContext;
  const ATerminal: TMiddlewareTerminal<TParams, TResult>;
  AParams: TParams): TResult;
var
  LPipeline: TMiddlewarePipeline;
  LHooks: TArray<TIntf>;
  LArray: TArray<IOperationMiddleware<TParams, TResult>>;
  LChain: TMiddlewareChain<TParams, TResult>;
  LUniversal: TArray<IMCPMiddleware>;
  LAnyArray: TArray<IOperationMiddleware<TRequestMetaParams, TBaseResult>>;
  LAnyChain: TMCPChain;
  LBridge: TMCPBridge<TParams, TResult>;
  LIndex: Integer;
begin
  LPipeline := nil;
  if Assigned(ARPCContext) then
    LPipeline := ARPCContext.FindContextDataAs<TMiddlewarePipeline>;

  // No transport behind the call, or nothing registered: the chain would be
  // empty anyway, so the handler is reached without building one.
  if not Assigned(LPipeline) or not Assigned(LPipeline.Context) or LPipeline.IsEmpty then
    Exit(ATerminal(nil, AParams));

  // ChainFor answers TArray<TIntf>, and an array of a descendant interface is
  // not assignment compatible with an array of its ancestor, however compatible
  // the elements are: hence the copy. It is written out here rather than called
  // as a second generic method because the compiler mixes up the two type
  // parameter lists when a constrained generic method calls another one (E2514,
  // reporting the constraint as IOperationMiddleware<TIntf, TResult>).
  LHooks := LPipeline.ChainFor<TIntf>;
  SetLength(LArray, Length(LHooks));
  for LIndex := 0 to High(LHooks) do
    LArray[LIndex] := LHooks[LIndex];

  LChain := Create(LArray, ATerminal);

  // The universal hooks wrap the ones of this operation. Asking for them costs
  // a hit on the chain cache, and when nobody takes part the operation runs
  // exactly as it did before: no bridge, no narrowing, nothing allocated.
  LUniversal := LPipeline.ChainFor<IMCPMiddleware>;
  if Length(LUniversal) = 0 then
    Exit(LChain.Next(LPipeline.Context, AParams));

  SetLength(LAnyArray, Length(LUniversal));
  for LIndex := 0 to High(LUniversal) do
    LAnyArray[LIndex] := LUniversal[LIndex];

  // The bridge lives here, on the stack, for as long as the walk lasts.
  LBridge := TMCPBridge<TParams, TResult>.Over(LChain);
  LAnyChain := TMCPChain.Create(LAnyArray, LBridge.Proceed);

  Result := TMCPCast.NarrowTo<TResult>(
    TObject(LAnyChain.Next(LPipeline.Context,
      TMCPCast.NarrowTo<TRequestMetaParams>(TObject(AParams), 'params',
        TMCPCast.MethodOf(LPipeline.Context)))),
    'result', TMCPCast.MethodOf(LPipeline.Context));
end;

function TMiddlewareChain<TParams, TResult>.Next(AContext: TMiddlewareContext;
  AParams: TParams): TResult;
var
  LNext: TMiddlewareChain<TParams, TResult>;
begin
  if FIndex >= Length(FChain) then
    Exit(FTerminal(AContext, AParams));

  // Copying the cursor rather than advancing this one is what makes Next
  // repeatable: the caller's chain stays where it was.
  LNext := Self;
  Inc(LNext.FIndex);
  Result := FChain[FIndex].Handle(AContext, AParams, LNext);
end;

end.
