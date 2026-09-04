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
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts;

type

  [JRPC('server')]
  TMCPServerApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('discover')]
    function Discover([JRPCParams] AParams: TRequestMetaParams):  TDiscoverResult;
  end;


  [JRPC('tools')]
  TMCPToolsApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function ToolsList([JRPCParams] AParams: TPaginatedRequestParams): TListToolsResult;

    [JRPC('call')]
    function CallTool([JRPCParams] AParams: TCallToolRequestParams): TCallToolResult;
  end;

  [JRPC('resources')]
  TMCPResourcesApi = class
  private
    function InternalReadResource(AParams: TReadResourceParams; AResource:
        TMCPResource): TReadResourceResult;
    function InternalReadTemplate(AParams: TReadResourceParams; ATemplate:
        TMCPResourceTemplate): TReadResourceResult;
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function ResourcesList: TListResourcesResult;

    [JRPC('templates/list')]
    function TemplatesList: TListResourceTemplatesResult;

    [JRPC('read')]
    function ReadResource([JRPCParams] AParams: TReadResourceParams): TReadResourceResult;
  end;

  [JRPC('prompts')]
  TMCPPromptsApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function PromptList: TListPromptsResult;

    [JRPC('get')]
    function ReadPrompt([JRPCParams] AParams: TGetPromptParams): TGetPromptResult;
  end;

  [JRPC('notifications')]
  TMCPNotificationsApi = class
  private
    [Context] Context: TJRPCContext;
    [Context] FConfig: IMCPConfig;
  public
    [JRPC('subscriptions/acknowledged'), JRPCNotification]
    procedure SubAck();
  end;

  [JRPC('subscriptions')]
  TMCPSubscriptionsApi = class
  private
    [Context] Context: TJRPCContext;
    [Context] FConfig: IMCPConfig;
  public
    [JRPC('listen'), JRPCNotification]
    procedure Listen;
  end;

implementation

uses
  System.Diagnostics,
  Logify,
  Neon.Core.Utils,
  MCPConnect.MCP.Invoker;

{ TMCPToolApi }

function TMCPToolsApi.CallTool(AParams: TCallToolRequestParams): TCallToolResult;
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

function TMCPToolsApi.ToolsList([JRPCParams] AParams: TPaginatedRequestParams): TListToolsResult;
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

procedure TMCPNotificationsApi.SubAck;
begin

end;

{ TMCPResourcesApi }

function TMCPResourcesApi.InternalReadResource(AParams: TReadResourceParams;
    AResource: TMCPResource): TReadResourceResult;
var
  LInvoker: TMCPResourceInvoker;
  LResObj: TObject;
begin
  // If it's a static resource serve the file directly
  if AResource.FileName <> '' then
  begin
    Result := TReadResourceResult.Create;
    TMCPStaticResource.GetResource(MCPConfig, AResource, Result);
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

function TMCPResourcesApi.InternalReadTemplate(AParams: TReadResourceParams;
    ATemplate: TMCPResourceTemplate): TReadResourceResult;
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

function TMCPResourcesApi.ReadResource([JRPCParams] AParams: TReadResourceParams): TReadResourceResult;
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

function TMCPResourcesApi.ResourcesList: TListResourcesResult;
var
  LStopwatch: TStopwatch;
begin
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

function TMCPResourcesApi.TemplatesList: TListResourceTemplatesResult;
begin
  Result := TListResourceTemplatesResult.Create;
  try
    MCPConfig.Resources.TemplateList(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPPromptsApi }

function TMCPPromptsApi.PromptList: TListPromptsResult;
var
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    Result := MCPConfig.Prompts.ListComplete;
  finally
    Logger.LogDebug('[PERF] PromptList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPPromptsApi.ReadPrompt(AParams: TGetPromptParams): TGetPromptResult;
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

{ TMCPServerApi }

function TMCPServerApi.Discover([JRPCParams] AParams: TRequestMetaParams): TDiscoverResult;
begin
  Result := TDiscoverResult.Create;
  Result.SupportedVersions := MCP_PROTOCOL_SUPPORTED_VERSIONS;
  Result.ResultType := TResultType.Complete;
  Result.CacheScope := TCacheScope.ScopePublic;
  Result.Capabilities.Tools.ListChanged := True;
  Result.Capabilities.Resources.ListChanged := True;
  Result.Capabilities.Prompts.ListChanged := True;
end;

{ TMCPSubscriptionsApi }

procedure TMCPSubscriptionsApi.Listen;
begin

end;

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPServerApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPPromptsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPResourcesApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi, MCPNeonConfig);

end.
