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
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
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
  MCPConnect.MCP.Types.Subscriptions;

type
  TApiCall<T> = procedure (AContext: TMiddlewareContext; AParams: T) of object;

  /// <summary>
  ///   What an api class needs to run the hooks of its own operation: the
  ///   pipeline of the request and the context of the message being handled.
  /// </summary>
  /// <remarks>
  ///   Resolved from the request context rather than injected with [Context],
  ///   because injection raises when the entry is missing, and it is missing on
  ///   every path that has no transport behind it - the api classes are called
  ///   directly by the tests and by the session inbound thread.
  /// </remarks>
  TMCPApiMiddleware = record
    Pipeline: TMiddlewarePipeline;
    Context: TMiddlewareContext;

    /// <summary>True when there is a chain worth building.</summary>
    function Active: Boolean;

    class function From(ARPCContext: TJRPCContext): TMCPApiMiddleware; static;
  end;

  /// <summary>
  ///   Base of the api classes: what every one of them needs from the request.
  /// </summary>
  /// <remarks>
  ///   The middleware wiring is not shared here on purpose: each operation
  ///   names its own chain type, its own hook interface and its own terminal,
  ///   and a record with no common ancestor is not something a generic method
  ///   could build. What was left to share was a single call, which is not
  ///   worth a method.
  /// </remarks>
  TMCPApi = class(TObject)
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;
  end;


  [JRPC('server')]
  TMCPServerApi = class(TMCPApi)
  private
    function DoDiscover(AContext: TMiddlewareContext;
      AParams: TRequestMetaParams): TDiscoverResult;
  public
    [JRPC('discover')]
    function Discover([JRPCParams] AParams: TRequestMetaParams): TDiscoverResult;
  end;


  [JRPC('tools')]
  TMCPToolsApi = class(TMCPApi)
  private
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
  { TODO -opaolo -c : Read the params 29/08/2026 09:25:00 }
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
  // AParams carries the required _meta - protocol version, client capabilities,
  // log level - which is why this takes params at all. Cursor pagination is not
  // implemented yet, here or on tools/list.
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
begin
  // See ResourcesList on why the params are taken but not yet read
  Result := TListResourceTemplatesResult.Create;
  try
    MCPConfig.Resources.TemplateList(Result);
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
  // See ResourcesList on why the params are taken but not yet read
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
      Exit(TCompleteResult.Create);

    LProviderObj := TRttiUtils.CreateInstance(LProvider.ProviderClass);
    try
      RPCContext.Inject(LProviderObj);

      LInvoker := TMCPCompletionInvoker.Create(LProviderObj, LProvider);
      try
        RPCContext.Inject(LInvoker);
        Result := LInvoker.Invoke(AParams);
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

function TMCPServerApi.DoDiscover(AContext: TMiddlewareContext;
  AParams: TRequestMetaParams): TDiscoverResult;
begin
  Result := TDiscoverResult.Create;
  Result.SupportedVersions := MCP_PROTOCOL_SUPPORTED_VERSIONS;
  Result.ResultType := TResultType.Complete;
  Result.CacheScope := TCacheScope.ScopePublic;
  Result.Capabilities.Tools.ListChanged := True;
  Result.Capabilities.Resources.ListChanged := True;
  Result.Capabilities.Prompts.ListChanged := True;

  // Only advertised when something can actually answer completion/complete
  if MCPConfig.Completions.HasProviders then
    Result.Capabilities.EnableCompletions;
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


{ TMCPApiMiddleware }

class function TMCPApiMiddleware.From(ARPCContext: TJRPCContext): TMCPApiMiddleware;
begin
  Result := Default(TMCPApiMiddleware);
  if not Assigned(ARPCContext) then
    Exit;

  Result.Pipeline := ARPCContext.FindContextDataAs<TMiddlewarePipeline>;
  if Assigned(Result.Pipeline) then
    Result.Context := Result.Pipeline.Context;
end;

function TMCPApiMiddleware.Active: Boolean;
begin
  Result := Assigned(Pipeline) and Assigned(Context) and not Pipeline.IsEmpty;
end;

function TMCPToolsApi.CallTool(AParams: TCallToolRequestParams): TBaseResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TCallToolChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoCallTool(nil, AParams));

  LChain := TCallToolChain.Create(
    LMiddleware.Pipeline.ChainFor<ICallToolMiddleware>, DoCallTool);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPToolsApi.ToolsList(AParams: TPaginatedRequestParams): TListToolsResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TListToolsChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoToolsList(nil, AParams));

  LChain := TListToolsChain.Create(
    LMiddleware.Pipeline.ChainFor<IListToolsMiddleware>, DoToolsList);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPResourcesApi.ReadResource(AParams: TReadResourceParams): TBaseResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TReadResourceChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoReadResource(nil, AParams));

  LChain := TReadResourceChain.Create(
    LMiddleware.Pipeline.ChainFor<IReadResourceMiddleware>, DoReadResource);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPResourcesApi.ResourcesList(AParams: TPaginatedRequestParams): TListResourcesResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TListResourcesChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoResourcesList(nil, AParams));

  LChain := TListResourcesChain.Create(
    LMiddleware.Pipeline.ChainFor<IListResourcesMiddleware>, DoResourcesList);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPPromptsApi.PromptList(AParams: TPaginatedRequestParams): TListPromptsResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TListPromptsChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoPromptList(nil, AParams));

  LChain := TListPromptsChain.Create(
    LMiddleware.Pipeline.ChainFor<IListPromptsMiddleware>, DoPromptList);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPPromptsApi.ReadPrompt(AParams: TGetPromptRequestParams): TBaseResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TGetPromptChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoReadPrompt(nil, AParams));

  LChain := TGetPromptChain.Create(
    LMiddleware.Pipeline.ChainFor<IGetPromptMiddleware>, DoReadPrompt);
  Result := LChain.Next(LMiddleware.Context, AParams);
end;

function TMCPServerApi.Discover(AParams: TRequestMetaParams): TDiscoverResult;
var
  LMiddleware: TMCPApiMiddleware;
  LChain: TDiscoverChain;
begin
  LMiddleware := TMCPApiMiddleware.From(RPCContext);
  if not LMiddleware.Active then
    Exit(DoDiscover(nil, AParams));

  LChain := TDiscoverChain.Create(
    LMiddleware.Pipeline.ChainFor<IDiscoverMiddleware>, DoDiscover);
  Result := LChain.Next(LMiddleware.Context, AParams);
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
