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
unit MCPConnect.MCP.Server.Api;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.JSON,
  System.Generics.Collections, System.Generics.Defaults,

  Neon.Core.Nullables,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Completion,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Elicitation,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Types.Subscriptions;

type
  TApiCall<T> = procedure (AContext: TMiddlewareContext; AParams: T) of object;

  /// <summary>
  ///   Reads the sort key - the identity - of one item of a pageable list.
  /// </summary>
  /// <remarks>
  ///   A method pointer and not a TFunc: this compiler refuses an anonymous
  ///   function assigned to a TFunc inside a function whose own Result is of
  ///   another type, which is what all four call sites are. Named key methods
  ///   read better next to the endpoint anyway.
  /// </remarks>
  TMCPKeyOf<T: class> = function (AItem: T): string of object;

  /// <summary>
  ///   Base of the api classes: what every one of them needs from the request,
  ///   and what every result of theirs carries out of it - the server's
  ///   identity and the caching hints.
  /// </summary>
  /// <remarks>
  ///   The middleware wiring is not here: an operation runs its own hooks with
  ///   TMiddlewareChain.Run, which takes the request context and needs nothing
  ///   from a base class.
  /// </remarks>
  TMCPApi = class(TObject)
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    /// <summary>
    ///   Puts the server's name and version into the result's "_meta", where
    ///   the specification asks every result to report who answered it.
    /// </summary>
    /// <remarks>
    ///   Called on the way out of each api method rather than from a
    ///   middleware, and that is the point: three of the operations - the
    ///   template list, completion and the subscription result - have no
    ///   chain of their own, so a middleware would leave exactly those
    ///   unsigned. Here every result of this layer passes through, whether a
    ///   middleware produced it or the method did.
    ///
    ///   A name already set is left alone: a middleware that signs a result
    ///   itself means it, and Server.SetSendServerInfo(False) turns the whole
    ///   thing off. A server that was never named reports nothing - see
    ///   TResultMetaObject.ShouldInclude.
    /// </remarks>
    procedure Identify(AResult: TBaseResult);

    /// <summary>
    ///   Writes the caching hints a cacheable result must carry: the section's
    ///   own if it has any, the server's otherwise, and nothing at all when
    ///   neither was configured - which leaves the conservative pair the result
    ///   was born with, immediately stale and private to the caller.
    /// </summary>
    /// <remarks>
    ///   A result that is not a TCachedResult is left alone, which is how an
    ///   interim "input_required" answer to resources/read ends up carrying no
    ///   hints: it is not a cacheable result, and the specification says so.
    /// </remarks>
    procedure Cache(AResult: TBaseResult; const ASection: TMCPCacheHints);

    /// <summary>
    ///   Marks a result as one a client must not cache: a result produced by
    ///   retrying a request through MRTR depends on inputs - the responses, the
    ///   request state - that are not part of the cache key, so serving it
    ///   again for the same method and params would serve the wrong answer.
    /// </summary>
    /// <remarks>
    ///   Zero and private is the strongest thing the model can say: it has no
    ///   "do not store" of its own, and a zero TTL means immediately stale.
    /// </remarks>
    procedure NoCache(AResult: TBaseResult);

    /// <summary>
    ///   Refuses an interim result that asks the client for something it never
    ///   said it could give: a server MUST NOT put an input request in
    ///   "inputRequests" for a capability the client did not declare.
    /// </summary>
    /// <remarks>
    ///   Refuses rather than quietly drops the request, because dropping it
    ///   would answer with an "input_required" that asks for nothing - and
    ///   because the specification says what this is: a
    ///   MissingRequiredClientCapability (-32021) naming what was missing, which
    ///   the transport answers with a 400. A client that reads it knows to
    ///   declare the capability or to stop calling this tool, where a silent
    ///   drop would leave it looping.
    ///
    ///   Skipped entirely when the request context holds no declared
    ///   capabilities: that means nobody looked - meta validation off, or a
    ///   request that carried no "_meta" under a lenient one - which is not the
    ///   same as a client that declared none.
    ///
    ///   Refusing takes the result with it: nothing downstream will see it, so
    ///   this is where it is freed.
    /// </remarks>
    procedure RequireInputCapabilities(AResult: TBaseResult);
  protected
    /// <summary>
    ///   The page size the answering section asks for, its own if it has one
    ///   and the server's otherwise.
    /// </summary>
    function PageSizeFor(const ASection: TMCPPaging): Integer;

    /// <summary>
    ///   Refuses a cursor this server cannot have issued: one minted for
    ///   another list, one that is not a cursor at all, or any cursor when the
    ///   answering section is unpaged and therefore issues none.
    /// </summary>
    /// <remarks>
    ///   The specification asks for Invalid Params (-32602), and silently
    ///   ignoring a cursor is the one thing a server may not do: a client would
    ///   be handed the first page again and page for ever.
    ///
    ///   Separate from Paginate, and called *before* the list is built, for an
    ///   ownership reason: refusing after the chain has produced a result would
    ///   orphan it - the exception leaves by way of the function's own Result,
    ///   so no caller ever sees the object to free it.
    /// </remarks>
    procedure CheckCursor(AKind: TMCPPageKind; const ACursor: NullString; APageSize: Integer);

    /// <summary>
    ///   Sorts AList by AKeyOf, drops what an incoming cursor says has already
    ///   been sent, trims the rest to one page, and answers with the cursor
    ///   that fetches the page after it - empty when this was the last one.
    /// </summary>
    /// <remarks>
    ///   Sorting happens whether or not the list is paged: the revision asks a
    ///   list to come back in a deterministic order, and the registries are
    ///   dictionaries whose enumeration order is an implementation detail
    ///   (insertion-ordered on Delphi 13, unspecified before it). A cursor that
    ///   names a position also needs the positions to hold still.
    ///
    ///   Raises nothing: CheckCursor has already refused anything this server
    ///   could not have issued, which is why it runs first and this one runs
    ///   on a result that exists.
    /// </remarks>
    function Paginate<T: class>(AList: TObjectList<T>; AKeyOf: TMCPKeyOf<T>;
      AKind: TMCPPageKind; const ACursor: NullString; APageSize: Integer): NullString;
  private
    /// <summary>
    ///   Orders AList by its items' keys. A procedure and not a step of
    ///   Paginate: the comparer is an anonymous function, and this compiler
    ///   only accepts one inside a routine with no Result of its own.
    /// </summary>
    procedure SortByKey<T: class>(AList: TObjectList<T>; AKeyOf: TMCPKeyOf<T>);
  end;


  [JRPC('server')]
  TMCPServerApi = class(TMCPApi)
  private
    function DoDiscover(AContext: TMiddlewareContext;
      AParams: TRequestMetaParams): TDiscoverResult;

    /// <summary>
    ///   Fills in what the server can do from what it has registered, for a
    ///   server that never said so itself.
    /// </summary>
    procedure InferCapabilities(ACapabilities: TServerCapabilities);
  public
    [JRPC('discover')]
    function Discover([JRPCParams] AParams: TRequestMetaParams): TDiscoverResult;
  end;


  [JRPC('tools')]
  TMCPToolsApi = class(TMCPApi)
  private
    /// <summary>A tool's identity, which is what tools/list is ordered and paged by.</summary>
    function ToolKey(AItem: TMCPTool): string;

    function DoToolsList(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListToolsResult;
    function DoCallTool(AContext: TMiddlewareContext;
      AParams: TCallToolRequestParams): TBaseResult;
  public
    [JRPC('list')]
    function ToolsList([JRPCParams] AParams: TPaginatedRequestParams): TListToolsResult;

    [JRPC('call')]
    /// <summary>
    ///   Answers with a TCallToolResult, or a TInputRequiredResult when the
    ///   tool needs more input first. Both are results the schema allows here.
    /// </summary>
    function CallTool([JRPCParams] AParams: TCallToolRequestParams): TBaseResult;
  end;

  [JRPC('resources')]
  TMCPResourcesApi = class(TMCPApi)
  private
    /// <summary>
    ///   A resource's identity: the uri, not the name. The uri is what
    ///   resources/read is keyed by, and two resources may share a display name.
    /// </summary>
    function ResourceKey(AItem: TMCPResource): string;

    /// <summary>A template's identity, the uri template it answers for.</summary>
    function TemplateKey(AItem: TMCPResourceTemplate): string;

    function InternalReadResource(AParams: TReadResourceParams; AResource: TMCPResource): TBaseResult;
    function InternalReadTemplate(AParams: TReadResourceParams; ATemplate: TMCPResourceTemplate): TBaseResult;
    function DoResourcesList(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListResourcesResult;
    function DoReadResource(AContext: TMiddlewareContext;
      AParams: TReadResourceParams): TBaseResult;
  public
    [JRPC('list')]
    function ResourcesList([JRPCParams] AParams: TPaginatedRequestParams): TListResourcesResult;

    [JRPC('templates/list')]
    function TemplatesList([JRPCParams] AParams: TPaginatedRequestParams): TListResourceTemplatesResult;

    [JRPC('read')]
    /// <summary>
    ///   Answers with a TReadResourceResult, or a TInputRequiredResult when the
    ///   resource needs more input first.
    /// </summary>
    function ReadResource([JRPCParams] AParams: TReadResourceParams): TBaseResult;
  end;

  [JRPC('prompts')]
  TMCPPromptsApi = class(TMCPApi)
  private
    /// <summary>A prompt's identity, which prompts/list is ordered and paged by.</summary>
    function PromptKey(AItem: TMCPPrompt): string;

    function DoPromptList(AContext: TMiddlewareContext;
      AParams: TPaginatedRequestParams): TListPromptsResult;
    function DoReadPrompt(AContext: TMiddlewareContext;
      AParams: TGetPromptRequestParams): TBaseResult;
  public
    [JRPC('list')]
    function PromptList([JRPCParams] AParams: TPaginatedRequestParams): TListPromptsResult;

    [JRPC('get')]
    /// <summary>
    ///   Answers with a TGetPromptResult, or a TInputRequiredResult when the
    ///   prompt needs more input first.
    /// </summary>
    function ReadPrompt([JRPCParams] AParams: TGetPromptRequestParams): TBaseResult;
  end;

  [JRPC('completion')]
  TMCPCompletionApi = class(TMCPApi)
  public
    [JRPC('complete')]
    function Complete([JRPCParams] AParams: TCompleteRequestParams): TCompleteResult;
  end;

  [JRPC('notifications')]
  TMCPNotificationsApi = class(TMCPApi)
  public
    /// <summary>
    ///   Client-sent acknowledgement of a subscription. A server normally
    ///   *sends* this notification rather than receiving one, so the handler
    ///   only accepts and discards it.
    /// </summary>
    [JRPC('subscriptions/acknowledged'), JRPCNotification]
    procedure SubAck([JRPCParams] AParams: TSubscriptionsAcknowledgedNotificationParams);
  end;

  [JRPC('subscriptions')]
  TMCPSubscriptionsApi = class(TMCPApi)
  private
    /// <summary>
    ///   The subset of AUris this server actually serves.
    /// </summary>
    function KnownResourceUris(const AUris: TArray<string>): TArray<string>;
  public
    [Context] Responses: TMCPMessageQueue;
    [Context] Request: TJRPCRequest;

    /// <summary>
    ///   Opens the notification stream. This is a long-lived *request*, not a
    ///   notification: it carries an id, the notifications sent on the stream
    ///   are correlated with it, and its response is sent only when the server
    ///   tears the subscription down.
    /// </summary>
    [JRPC('listen')]
    function Listen([JRPCParams] AParams: TSubscriptionsListenRequestParams): TSubscriptionsListenResult;
  end;

implementation

uses
  System.Diagnostics,
  Logify,
  Neon.Core.Utils,
  MCPConnect.MCP.Invoker;

{ TMCPApi }

procedure TMCPApi.Identify(AResult: TBaseResult);
begin
  if not Assigned(AResult) or not Assigned(MCPConfig) then
    Exit;

  if not MCPConfig.Server.SendServerInfo then
    Exit;

  // Whoever got there first keeps it
  if not AResult.ResultMeta.ServerInfo.Name.IsEmpty then
    Exit;

  AResult.ResultMeta.ServerInfo.Name := MCPConfig.Server.Name;
  AResult.ResultMeta.ServerInfo.Version := MCPConfig.Server.Version;

  // The identity's own description, which is not the same thing as the
  // instructions a discovery result carries: this says what the server is, the
  // instructions say how to use it well.
  if not MCPConfig.Server.Description.IsEmpty then
    AResult.ResultMeta.ServerInfo.Description := MCPConfig.Server.Description;
end;

procedure TMCPApi.Cache(AResult: TBaseResult; const ASection: TMCPCacheHints);
begin
  if not Assigned(AResult) or not Assigned(MCPConfig) then
    Exit;

  if ASection.IsAssigned then
    ASection.ApplyTo(AResult)
  else
    MCPConfig.Server.CacheHints.ApplyTo(AResult);
end;

function TMCPApi.PageSizeFor(const ASection: TMCPPaging): Integer;
begin
  if not Assigned(MCPConfig) then
    Exit(0);

  if ASection.IsAssigned then
    Result := ASection.PageSize
  else
    Result := MCPConfig.Server.Paging.PageSize;
end;

procedure TMCPApi.CheckCursor(AKind: TMCPPageKind; const ACursor: NullString; APageSize: Integer);
var
  LAfter: string;
begin
  if not ACursor.HasValue then
    Exit;

  if APageSize <= 0 then
    raise EJRPCInvalidParamsError.CreateFmt(SMCPCursorUnpaged,
      [TMCPCursor.KindNameOf(AKind)]);

  if not TMCPCursor.TryDecode(AKind, ACursor.Value, LAfter) then
    raise EJRPCInvalidParamsError.CreateFmt(SMCPCursorInvalid,
      [TMCPCursor.KindNameOf(AKind)]);
end;

procedure TMCPApi.SortByKey<T>(AList: TObjectList<T>; AKeyOf: TMCPKeyOf<T>);
begin
  // CompareStr and not CompareText: the identities these lists are keyed by
  // are case-sensitive, so a case-insensitive sort would leave two that differ
  // only in case in an arbitrary order relative to each other - which is the
  // thing being fixed.
  AList.Sort(TComparer<T>.Construct(
    function (const ALeft, ARight: T): Integer
    begin
      Result := CompareStr(AKeyOf(ALeft), AKeyOf(ARight));
    end));
end;

function TMCPApi.Paginate<T>(AList: TObjectList<T>; AKeyOf: TMCPKeyOf<T>;
  AKind: TMCPPageKind; const ACursor: NullString; APageSize: Integer): NullString;
var
  LAfter: string;
  LIndex: Integer;
begin
  // Unset, not empty: an empty string is a *valid* cursor a client would
  // follow, so "no next page" has to be an absent member
  Result := nil;
  if not Assigned(AList) then
    Exit;

  // Deterministic before anything else looks at the order
  SortByKey<T>(AList, AKeyOf);

  if ACursor.HasValue and TMCPCursor.TryDecode(AKind, ACursor.Value, LAfter) then
  begin
    // Everything up to and including the key already sent. Extract and not
    // Delete: these lists do not own their items - the registries do - but
    // saying so at the call site is cheaper than trusting it.
    for LIndex := AList.Count - 1 downto 0 do
      if CompareStr(AKeyOf(AList[LIndex]), LAfter) <= 0 then
        AList.Extract(AList[LIndex]);
  end;

  if APageSize <= 0 then
    Exit;

  if AList.Count <= APageSize then
    Exit;

  // One page, and the cursor that resumes after its last item
  Result := TMCPCursor.Encode(AKind, AKeyOf(AList[APageSize - 1]));

  for LIndex := AList.Count - 1 downto APageSize do
    AList.Extract(AList[LIndex]);
end;

procedure TMCPApi.NoCache(AResult: TBaseResult);
begin
  if AResult is TCachedResult then
  begin
    TCachedResult(AResult).TtlMs := 0;
    TCachedResult(AResult).CacheScope := TCacheScope.ScopePrivate;
  end;
end;

procedure TMCPApi.RequireInputCapabilities(AResult: TBaseResult);
var
  LDeclared: TMCPDeclaredCapabilities;
  LEntry: TPair<string, TInputRequest>;
  LNeeded, LMissing: TMCPClientCapabilities;

  // The mode is only worth checking when the client named modes at all: a
  // client that declared "elicitation":{} declared it whole, and reading that
  // as "neither form nor url" would refuse every elicitation there is.
  procedure NeedElicitation(ARequest: TInputRequest);
  const
    Modes = [TMCPClientCapability.ElicitationForm, TMCPClientCapability.ElicitationUrl];
  begin
    Include(LNeeded, TMCPClientCapability.Elicitation);

    if LDeclared.Declared * Modes = [] then
      Exit;

    if Assigned(ARequest.Elicitation) and ARequest.Elicitation.Mode.HasValue and
       SameText(ARequest.Elicitation.Mode.Value, MCP_ELICIT_MODE_URL) then
      Include(LNeeded, TMCPClientCapability.ElicitationUrl)
    else
      // An absent mode means "form"
      Include(LNeeded, TMCPClientCapability.ElicitationForm);
  end;

  procedure NeedSampling(ARequest: TInputRequest);
  begin
    Include(LNeeded, TMCPClientCapability.Sampling);

    if not Assigned(ARequest.Sampling) then
      Exit;

    // The two sub-capabilities are about what the request asks the client to
    // do, not about sampling itself
    if ARequest.Sampling.IncludeContext <> TIncludeContext.None then
      Include(LNeeded, TMCPClientCapability.SamplingContext);

    if Length(ARequest.Sampling.Tools) > 0 then
      Include(LNeeded, TMCPClientCapability.SamplingTools);
  end;

begin
  if not (AResult is TInputRequiredResult) then
    Exit;

  LDeclared := RPCContext.FindContextDataAs<TMCPDeclaredCapabilities>;
  if not Assigned(LDeclared) then
    Exit;

  LNeeded := [];
  for LEntry in TInputRequiredResult(AResult).InputRequests do
  begin
    if LEntry.Value.Method = MCP_INPUT_ELICITATION then
      NeedElicitation(LEntry.Value)
    else if LEntry.Value.Method = MCP_INPUT_SAMPLING then
      NeedSampling(LEntry.Value)
    else if LEntry.Value.Method = MCP_INPUT_ROOTS then
      Include(LNeeded, TMCPClientCapability.Roots);
  end;

  LMissing := LDeclared.Missing(LNeeded);
  if LMissing = [] then
    Exit;

  // Nothing downstream will see it now
  AResult.Free;
  raise EMCPMissingRequiredClientCapabilityError.CreateForCapabilities(LMissing);
end;

{ TMCPToolApi }

function TMCPToolsApi.DoCallTool(AContext: TMiddlewareContext;
  AParams: TCallToolRequestParams): TBaseResult;
var
  LInvoker: TMCPToolInvoker;
  LTool: TMCPTool;
  LToolObj: TObject;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    if not MCPConfig.Tools.Registry.TryGetValue(AParams.Name, LTool) then
      raise EJRPCInvalidParamsError.CreateFmt(SMCPToolNotFound, [AParams.Name]);

    // Instance of the tool class
    LToolObj := TRttiUtils.CreateInstance(LTool.ToolClass);
    try
      RPCContext.Inject(LToolObj);

      LInvoker := TMCPToolInvoker.Create(LToolObj, LTool);
      try
        RPCContext.Inject(LInvoker);
        try
          Result := LInvoker.Invoke(AParams);
        except
          on E: Exception do
          begin
            raise EJRPCException.CreateFmt(SMCPToolCallError, [E.ClassName, E.Message]);
          end;
        end;
      finally
        LInvoker.Free;
      end;
    finally
      LToolObj.Free;
    end;
  finally
    Logger.LogDebug('[PERF] CallTool [%s] total: %d ms', [AParams.Name, LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPToolsApi.DoToolsList(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListToolsResult;
var
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    Result := MCPConfig.Tools.ListEnabled;
  finally
    Logger.LogDebug('[PERF] ToolsList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

procedure TMCPNotificationsApi.SubAck(AParams: TSubscriptionsAcknowledgedNotificationParams);
begin
  // Nothing to do: the acknowledgement travels server to client, so a server
  // receiving one just accepts it rather than rejecting the method
end;

{ TMCPResourcesApi }

function TMCPResourcesApi.InternalReadResource(AParams: TReadResourceParams; AResource: TMCPResource): TBaseResult;
var
  LInvoker: TMCPResourceInvoker;
  LResObj: TObject;
  LFileResult: TReadResourceResult;
begin
  // If it's a static resource serve the file directly
  if AResource.FileName <> '' then
  begin
    LFileResult := TReadResourceResult.Create;
    Result := LFileResult;
    TMCPStaticResource.GetResource(MCPConfig, AResource, LFileResult);
    Exit;
  end;

  // Create an instance of the resource class
  LResObj := TRttiUtils.CreateInstance(AResource.ResourceClass);
  try
    RPCContext.Inject(LResObj);

    LInvoker := TMCPResourceInvoker.Create(LResObj, AResource);
    try
      RPCContext.Inject(LInvoker);
      Result := LInvoker.Invoke(AParams);
    finally
      LInvoker.Free;
    end;
  finally
    LResObj.Free;
  end;
end;

function TMCPResourcesApi.InternalReadTemplate(AParams: TReadResourceParams; ATemplate: TMCPResourceTemplate): TBaseResult;
var
  LInvoker: TMCPTemplateInvoker;
  LTplObj: TObject;
begin
  // Create an instance of the resource class
  LTplObj := TRttiUtils.CreateInstance(ATemplate.ResourceClass);
  try
    RPCContext.Inject(LTplObj);

    LInvoker := TMCPTemplateInvoker.Create(LTplObj, ATemplate);
    try
      RPCContext.Inject(LInvoker);
      Result := LInvoker.Invoke(AParams);
    finally
      LInvoker.Free;
    end;
  finally
    LTplObj.Free;
  end;
end;

function TMCPResourcesApi.DoReadResource(AContext: TMiddlewareContext;
  AParams: TReadResourceParams): TBaseResult;
var
  LRes: TMCPResource;
  LTpl: TMCPResourceTemplate;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LTpl := nil;

    // Try to match the exact resource uri
    LRes := MCPConfig.Resources.GetResource(AParams.Uri);

    // If no resource is found the try to match with templates
    if not Assigned(LRes) then
    begin
      LTpl := MCPConfig.Resources.GetTemplate(AParams.Uri);

      // Resource-not-found is Invalid Params (-32602) since 2026-07-28; the
      // -32002 of earlier revisions MUST NOT be emitted any more.
      if not Assigned(LTpl) then
        raise EJRPCInvalidParamsError.CreateFmt(SMCPResourceNotFound, [AParams.Uri]);
    end;

    if Assigned(LRes) then
      Result := InternalReadResource(AParams, LRes)
    else
      Result := InternalReadTemplate(AParams, LTpl);
  finally
    Logger.LogDebug('[PERF] ReadResource [%s] total: %d ms', [AParams.Uri, LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPResourcesApi.DoResourcesList(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListResourcesResult;
var
  LStopwatch: TStopwatch;
begin
  // The cursor is read by the caller, after the chain: this builds the whole
  // list, and ResourcesList pages what comes back out of it
  LStopwatch := TStopwatch.StartNew;
  try
    Result := TListResourcesResult.Create;
    try
      MCPConfig.Resources.ResourceList(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    Logger.LogDebug('[PERF] ResourcesList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPResourcesApi.TemplatesList(AParams: TPaginatedRequestParams): TListResourceTemplatesResult;
var
  LPageSize: Integer;
begin
  // Its own cursor kind, off the Resources page size: the two lists are paged
  // apart, so a cursor from one is refused by the other
  LPageSize := PageSizeFor(MCPConfig.Resources.Paging);
  CheckCursor(TMCPPageKind.Templates, AParams.Cursor, LPageSize);

  Result := TListResourceTemplatesResult.Create;
  try
    MCPConfig.Resources.TemplateList(Result);

    Result.NextCursor := Paginate<TMCPResourceTemplate>(Result.ResourceTemplates,
      TemplateKey, TMCPPageKind.Templates, AParams.Cursor, LPageSize);

    Identify(Result);
    Cache(Result, MCPConfig.Resources.CacheHints);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPPromptsApi }

function TMCPPromptsApi.DoPromptList(AContext: TMiddlewareContext;
  AParams: TPaginatedRequestParams): TListPromptsResult;
var
  LStopwatch: TStopwatch;
begin
  // See DoResourcesList on where the cursor is read
  LStopwatch := TStopwatch.StartNew;
  try
    Result := MCPConfig.Prompts.ListComplete;
  finally
    Logger.LogDebug('[PERF] PromptList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPPromptsApi.DoReadPrompt(AContext: TMiddlewareContext;
  AParams: TGetPromptRequestParams): TBaseResult;
var
  LInvoker: TMCPPromptInvoker;
  LPrompt: TMCPPrompt;
  LPromptObj: TObject;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    if not MCPConfig.Prompts.Registry.TryGetValue(AParams.Name, LPrompt) then
      raise EJRPCInvalidParamsError.CreateFmt(SMCPPromptNotFound, [AParams.Name]);

    // Create an instance of the tool class
    LPromptObj := TRttiUtils.CreateInstance(LPrompt.PromptClass);
    try
      RPCContext.Inject(LPromptObj);

      LInvoker := TMCPPromptInvoker.Create(LPromptObj, LPrompt);
      try
        RPCContext.Inject(LInvoker);
        Result := LInvoker.Invoke(AParams);
      finally
        LInvoker.Free;
      end;
    finally
      LPromptObj.Free;
    end;
  finally
    Logger.LogDebug('[PERF] ReadPrompt [%s] total: %d ms', [AParams.Name, LStopwatch.ElapsedMilliseconds]);
  end;
end;

{ TMCPCompletionApi }

function TMCPCompletionApi.Complete(AParams: TCompleteRequestParams): TCompleteResult;
var
  LInvoker: TMCPCompletionInvoker;
  LProvider: TMCPCompletionProvider;
  LProviderObj: TObject;
  LTarget: string;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LTarget := AParams.Ref.Target;

    // Per the spec a malformed reference or an unknown prompt/template is
    // Invalid Params, not Method Not Found
    case AParams.Ref.Kind of
      TMCPCompletionRefKind.Prompt:
        if not MCPConfig.Prompts.Registry.ContainsKey(LTarget) then
          raise EJRPCInvalidParamsError.CreateFmt(SMCPPromptNotFound, [LTarget]);

      TMCPCompletionRefKind.ResourceTemplate:
        if not MCPConfig.Resources.TemplateRegistry.ContainsKey(LTarget) and
           not MCPConfig.Resources.Registry.ContainsKey(LTarget) then
          raise EJRPCInvalidParamsError.CreateFmt(SMCPResourceNotFound, [LTarget]);
    else
      raise EJRPCInvalidParamsError.CreateFmt(SMCPCompletionRefUnknownFmt, [AParams.Ref.&Type]);
    end;

    LProvider := MCPConfig.Completions.Find(AParams.Ref.Kind, LTarget, AParams.Argument.Name);

    // A known prompt or template whose argument simply has no provider is not
    // an error: the server just has nothing to suggest for it
    if not Assigned(LProvider) then
    begin
      Result := TCompleteResult.Create;
      Identify(Result);
      Exit;
    end;

    LProviderObj := TRttiUtils.CreateInstance(LProvider.ProviderClass);
    try
      RPCContext.Inject(LProviderObj);

      LInvoker := TMCPCompletionInvoker.Create(LProviderObj, LProvider);
      try
        RPCContext.Inject(LInvoker);
        Result := LInvoker.Invoke(AParams);
        Identify(Result);
      finally
        LInvoker.Free;
      end;
    finally
      LProviderObj.Free;
    end;
  finally
    Logger.LogDebug('[PERF] Complete [%s/%s] total: %d ms',
      [AParams.Ref.Target, AParams.Argument.Name, LStopwatch.ElapsedMilliseconds]);
  end;
end;

{ TMCPServerApi }

function TMCPServerApi.DoDiscover(AContext: TMiddlewareContext; AParams: TRequestMetaParams): TDiscoverResult;
begin
  Result := TDiscoverResult.Create;
  Result.SupportedVersions := MCP_PROTOCOL_SUPPORTED_VERSIONS;
  Result.ResultType := TResultType.Complete;

  // A discovery result says nothing about the caller, so it is shareable. A
  // server that disagrees says so with Server.SetCacheHints, which the Cache
  // call in Discover applies over this.
  Result.CacheScope := TCacheScope.ScopePublic;

  if not MCPConfig.Server.Instructions.IsEmpty then
    Result.Instructions := MCPConfig.Server.Instructions;

  // What the server said it can do, or - having said nothing - what it turns
  // out to have. Server.SetCapabilities is for the server that knows better
  // than its registry: one that registers its tools late, or means to advertise
  // less than it holds.
  if Assigned(MCPConfig.Server.Capabilities) then
    Result.Capabilities.Assign(MCPConfig.Server.Capabilities)
  else
    InferCapabilities(Result.Capabilities);
end;

procedure TMCPServerApi.InferCapabilities(ACapabilities: TServerCapabilities);
begin
  // Presence is the declaration: "tools" in the capabilities is what says this
  // server has tools at all. The listChanged flag beside it is a second
  // statement - that the server will say when they change - and this build
  // cannot: the stream those notifications travel on is the one
  // subscriptions/listen has yet to hold open. False rather than absent, since
  // an empty capability object is dropped on the way out and would take the
  // declaration with it.
  if MCPConfig.Tools.Registry.Count > 0 then
    ACapabilities.Tools.ListChanged := False;

  if (MCPConfig.Resources.Registry.Count > 0) or
     (MCPConfig.Resources.TemplateRegistry.Count > 0) then
    ACapabilities.Resources.ListChanged := False;

  if MCPConfig.Prompts.Registry.Count > 0 then
    ACapabilities.Prompts.ListChanged := False;

  // Only advertised when something can actually answer completion/complete
  if MCPConfig.Completions.HasProviders then
    ACapabilities.EnableCompletions;
end;

{ TMCPSubscriptionsApi }

function TMCPSubscriptionsApi.Listen(AParams: TSubscriptionsListenRequestParams): TSubscriptionsListenResult;
var
  LAck: TSubscriptionsAcknowledgedNotificationParams;
begin
  // Acknowledge with the subset this server can actually serve: a type it has
  // nothing to report on is left out rather than silently never sent
  LAck := TSubscriptionsAcknowledgedNotificationParams.Create;
  try
    if AParams.Notifications.WantsToolsListChanged and (MCPConfig.Tools.Registry.Count > 0) then
      LAck.Notifications.ToolsListChanged := True;

    if AParams.Notifications.WantsPromptsListChanged and (MCPConfig.Prompts.Registry.Count > 0) then
      LAck.Notifications.PromptsListChanged := True;

    if AParams.Notifications.WantsResourcesListChanged and
       ((MCPConfig.Resources.Registry.Count > 0) or (MCPConfig.Resources.TemplateRegistry.Count > 0)) then
      LAck.Notifications.ResourcesListChanged := True;

    LAck.Notifications.ResourceSubscriptions := KnownResourceUris(AParams.Notifications.ResourceSubscriptions);

    if Assigned(Responses) then
      Responses.Enqueue(TMCPNotification.FromParams(MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED, LAck))
    else
      LAck.Free;
  except
    LAck.Free;
    raise;
  end;

  // The stream itself is transport work: until the transport holds this request
  // open, the subscription is torn down as soon as it is acknowledged, which is
  // the graceful teardown this result reports.
  Result := TSubscriptionsListenResult.Create;
  Identify(Result);

  // The stream id is the id of this very request, keeping its JSON type
  if Request.Id.IsString then
    Result.Meta.SetSubscriptionId(Request.Id.AsString)
  else
    Result.Meta.SetSubscriptionId(Int64(Request.Id.AsInteger));
end;

function TMCPSubscriptionsApi.KnownResourceUris(const AUris: TArray<string>): TArray<string>;
var
  LUri: string;
begin
  Result := [];
  for LUri in AUris do
    if MCPConfig.Resources.Registry.ContainsKey(LUri) or
       MCPConfig.Resources.TemplateRegistry.ContainsKey(LUri) then
      Result := Result + [LUri];
end;


function TMCPToolsApi.CallTool(AParams: TCallToolRequestParams): TBaseResult;
begin
  Result := TCallToolChain.Run<ICallToolMiddleware>(RPCContext, DoCallTool, AParams);
  RequireInputCapabilities(Result);
  Identify(Result);
end;

function TMCPToolsApi.ToolKey(AItem: TMCPTool): string;
begin
  Result := AItem.Name;
end;

function TMCPToolsApi.ToolsList(AParams: TPaginatedRequestParams): TListToolsResult;
var
  LPageSize: Integer;
begin
  LPageSize := PageSizeFor(MCPConfig.Tools.Paging);

  // Before anything is built: a refusal from here leaves nothing behind
  CheckCursor(TMCPPageKind.Tools, AParams.Cursor, LPageSize);

  Result := TListToolsChain.Run<IListToolsMiddleware>(RPCContext, DoToolsList, AParams);

  // After the chain, so that a middleware which filters the list is paged over
  // what it actually returns rather than over what the registry holds
  Result.NextCursor := Paginate<TMCPTool>(Result.Tools, ToolKey,
    TMCPPageKind.Tools, AParams.Cursor, LPageSize);

  Identify(Result);

  // Each page is independently cacheable and carries its own hints, which is
  // what the caching section of the revision says of a paged list
  Cache(Result, MCPConfig.Tools.CacheHints);
end;

function TMCPResourcesApi.ReadResource(AParams: TReadResourceParams): TBaseResult;
begin
  Result := TReadResourceChain.Run<IReadResourceMiddleware>(RPCContext, DoReadResource, AParams);
  RequireInputCapabilities(Result);
  Identify(Result);
  Cache(Result, MCPConfig.Resources.CacheHints);

  // The one cacheable operation that can also be an MRTR retry: what it
  // answered depends on the input the client sent back, and none of that is in
  // the cache key, so this reply speaks for this request alone.
  if (AParams.InputResponses.Count > 0) or AParams.RequestState.HasValue then
    NoCache(Result);
end;

function TMCPResourcesApi.ResourceKey(AItem: TMCPResource): string;
begin
  Result := AItem.Uri;
end;

function TMCPResourcesApi.TemplateKey(AItem: TMCPResourceTemplate): string;
begin
  // A template with no uri sorts first and is its own problem; the key only
  // has to be total
  if AItem.UriTemplate.HasValue then
    Result := AItem.UriTemplate.Value
  else
    Result := '';
end;

function TMCPResourcesApi.ResourcesList(AParams: TPaginatedRequestParams): TListResourcesResult;
var
  LPageSize: Integer;
begin
  LPageSize := PageSizeFor(MCPConfig.Resources.Paging);
  CheckCursor(TMCPPageKind.Resources, AParams.Cursor, LPageSize);

  Result := TListResourcesChain.Run<IListResourcesMiddleware>(RPCContext, DoResourcesList, AParams);

  Result.NextCursor := Paginate<TMCPResource>(Result.Resources, ResourceKey,
    TMCPPageKind.Resources, AParams.Cursor, LPageSize);

  Identify(Result);
  Cache(Result, MCPConfig.Resources.CacheHints);
end;

function TMCPPromptsApi.PromptKey(AItem: TMCPPrompt): string;
begin
  Result := AItem.Name;
end;

function TMCPPromptsApi.PromptList(AParams: TPaginatedRequestParams): TListPromptsResult;
var
  LPageSize: Integer;
begin
  LPageSize := PageSizeFor(MCPConfig.Prompts.Paging);
  CheckCursor(TMCPPageKind.Prompts, AParams.Cursor, LPageSize);

  Result := TListPromptsChain.Run<IListPromptsMiddleware>(RPCContext, DoPromptList, AParams);

  Result.NextCursor := Paginate<TMCPPrompt>(Result.Prompts, PromptKey,
    TMCPPageKind.Prompts, AParams.Cursor, LPageSize);

  Identify(Result);
  Cache(Result, MCPConfig.Prompts.CacheHints);
end;

function TMCPPromptsApi.ReadPrompt(AParams: TGetPromptRequestParams): TBaseResult;
begin
  Result := TGetPromptChain.Run<IGetPromptMiddleware>(RPCContext, DoReadPrompt, AParams);
  RequireInputCapabilities(Result);
  Identify(Result);
end;

function TMCPServerApi.Discover(AParams: TRequestMetaParams): TDiscoverResult;
begin
  Result := TDiscoverChain.Run<IDiscoverMiddleware>(RPCContext, DoDiscover, AParams);
  Identify(Result);
  Cache(Result, MCPConfig.Server.CacheHints);
end;

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPServerApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPPromptsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPResourcesApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPCompletionApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPSubscriptionsApi, MCPNeonConfig);

end.
