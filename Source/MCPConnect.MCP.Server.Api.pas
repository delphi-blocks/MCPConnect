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

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Authorization,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Prompts;

resourcestring
  SMCPAccessDeniedLogFmt = 'Access to %s denied (required scopes: "%s", token scopes: "%s")';

type
  [JRPC('tools')]
  TMCPToolsApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function ToolsList: TListToolsResult;

    [JRPC('call')]
    function CallTool([JRPCParams] AParams: TCallToolParams): TCallToolResult;
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
    [JRPC('initialized'), JRPCNotification]
    procedure Initialized;

    [JRPC('cancelled'), JRPCNotification]
    procedure Cancelled([JRPCParams] ACancelledParams: TCancelledNotificationParams);

  end;

  [JRPC('initialize')]
  TMCPInitializeApi = class
  public
    [Context] MCPConfig: TMCPConfig;

    [JRPC('')]
    function Initialize([JRPCParams] AInitializeParams: TInitializeParams): TInitializeResult;
  end;

  [JRPC('logging')]
  TMCPLoggingApi = class
  public
    [Context] Context: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('setLevel')]
    function SetLevel([JRPCParams] ASetLevelParams: TSetLevelRequestParams): TSetLevelResult;
  end;

  [JRPC('ping')]
  TMCPPingApi = class
  public
    [Context] MCPConfig: TMCPConfig;

    [JRPC('')]
    function Ping(): TJSONObject;
  end;


implementation

uses
  System.Diagnostics,
  Logify,
  Neon.Core.Utils,
  MCPConnect.MCP.Invoker;

function CreateAccessGuard(AContext: TJRPCContext; AConfig: TMCPConfig): TMCPAccessGuard;
begin
  Result := TMCPAccessGuard.Create(AContext, AConfig.Security.CreateAuthorizer);
end;

/// <summary>
///   Raises the same "not found" error as an item that does not exist when the
///   caller is not allowed to use AItem, so that its existence is not disclosed.
///   The real reason goes to the log.
/// </summary>
procedure CheckAccess(AContext: TJRPCContext; AConfig: TMCPConfig;
  const AItem: TMCPAuthItem; const ANotFoundFmt, AKey: string);
var
  LGuard: TMCPAccessGuard;
begin
  LGuard := CreateAccessGuard(AContext, AConfig);
  try
    if LGuard.IsAllowed(AItem) then
      Exit;

    Logger.LogWarning(SMCPAccessDeniedLogFmt,
      [AItem.ToString, string.Join(' ', AItem.RequiredScopes), LGuard.Identity.Scope]);
  finally
    LGuard.Free;
  end;

  raise EMCPException.CreateFmt(ANotFoundFmt, [AKey]);
end;

{ TMCPToolApi }

function TMCPToolsApi.CallTool(AParams: TCallToolParams): TCallToolResult;
var
  LInvoker: TMCPToolInvoker;
  LTool: TMCPTool;
  LToolObj: TObject;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    if not MCPConfig.Tools.Registry.TryGetValue(AParams.Name, LTool) then
      raise EMCPException.CreateFmt(SMCPToolNotFound, [AParams.Name]);

    CheckAccess(RPCContext, MCPConfig, TMCPAuthItem.FromTool(LTool), SMCPToolNotFound, AParams.Name);

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


function TMCPToolsApi.ToolsList: TListToolsResult;
var
  LStopwatch: TStopwatch;
  LGuard: TMCPAccessGuard;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LGuard := CreateAccessGuard(RPCContext, MCPConfig);
    try
      Result := MCPConfig.Tools.ListEnabled(
        function (ATool: TMCPTool): Boolean
        begin
          Result := LGuard.IsAllowed(TMCPAuthItem.FromTool(ATool));
        end);
    finally
      LGuard.Free;
    end;
  finally
    Logger.LogDebug('[PERF] ToolsList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

{ TMCPNotificationsApi }

procedure TMCPNotificationsApi.Cancelled(ACancelledParams: TCancelledNotificationParams);
begin
  if Assigned(FConfig.MessageHandling.CancelledProc) then
    FConfig.MessageHandling.CancelledProc(Context, ACancelledParams);
end;

procedure TMCPNotificationsApi.Initialized;
begin
  if Assigned(FConfig.MessageHandling.InitializedProc) then
    FConfig.MessageHandling.InitializedProc(Context);
end;


{ TMCPInitializeApi }

function TMCPInitializeApi.Initialize(AInitializeParams: TInitializeParams): TInitializeResult;
begin
  Result := TInitializeResult.Create;
  try
    if MatchStr(AInitializeParams.ProtocolVersion, MCP_SUPPORTED_PROTOCOL_VERSIONS) then
      Result.ProtocolVersion := AInitializeParams.ProtocolVersion
    else
      Result.ProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;
    Result.ServerInfo.Name := MCPConfig.Server.Name;
    Result.ServerInfo.Version := MCPConfig.Server.Version;
    Result.ServerInfo.Description := MCPConfig.Server.Description;

    if Assigned(MCPConfig.Server.Capabilities) then
    begin
      Result.Capabilities.Free;
      Result.Capabilities := MCPConfig.Server.Capabilities;
      Result.OwnCapabilities := False;
    end
    else
    begin
      if MCPConfig.Tools.Registry.Count > 0 then
      begin
        Result.Capabilities.Tools.ListChanged := False;
      end;

      if MCPConfig.Resources.Registry.Count + MCPConfig.Resources.TemplateRegistry.Count > 0 then
      begin
        Result.Capabilities.Resources.ListChanged := False;
        Result.Capabilities.Resources.Subscribe := False;
      end;

      if MCPConfig.Prompts.Registry.Count > 0 then
      begin
        Result.Capabilities.Prompts.ListChanged := False;
      end;

    end;

  except
    Result.Free;
    raise;
  end;
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

      if not Assigned(LTpl) then
        raise EMCPException.CreateFmt(SMCPResourceNotFound, [AParams.Uri]);
    end;

    if Assigned(LRes) then
    begin
      CheckAccess(RPCContext, MCPConfig, TMCPAuthItem.FromResource(LRes), SMCPResourceNotFound, AParams.Uri);
      Result := InternalReadResource(AParams, LRes);
    end
    else
    begin
      CheckAccess(RPCContext, MCPConfig, TMCPAuthItem.FromTemplate(LTpl, AParams.Uri), SMCPResourceNotFound, AParams.Uri);
      Result := InternalReadTemplate(AParams, LTpl);
    end;
  finally
    Logger.LogDebug('[PERF] ReadResource [%s] total: %d ms', [AParams.Uri, LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPResourcesApi.ResourcesList: TListResourcesResult;
var
  LStopwatch: TStopwatch;
  LGuard: TMCPAccessGuard;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    Result := TListResourcesResult.Create;
    try
      LGuard := CreateAccessGuard(RPCContext, MCPConfig);
      try
        MCPConfig.Resources.ResourceList(Result,
          function (AResource: TMCPResource): Boolean
          begin
            Result := LGuard.IsAllowed(TMCPAuthItem.FromResource(AResource));
          end);
      finally
        LGuard.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Logger.LogDebug('[PERF] ResourcesList total: %d ms', [LStopwatch.ElapsedMilliseconds]);
  end;
end;

function TMCPResourcesApi.TemplatesList: TListResourceTemplatesResult;
var
  LGuard: TMCPAccessGuard;
begin
  Result := TListResourceTemplatesResult.Create;
  try
    LGuard := CreateAccessGuard(RPCContext, MCPConfig);
    try
      MCPConfig.Resources.TemplateList(Result,
        function (ATemplate: TMCPResourceTemplate): Boolean
        begin
          Result := LGuard.IsAllowed(TMCPAuthItem.FromTemplate(ATemplate));
        end);
    finally
      LGuard.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPLoggingApi }

function TMCPLoggingApi.SetLevel(ASetLevelParams: TSetLevelRequestParams): TSetLevelResult;
begin
  if Assigned(MCPConfig.MessageHandling.SetLogLevelProc) then
    MCPConfig.MessageHandling.SetLogLevelProc(Context, ASetLevelParams.Level);
  Result := TSetLevelResult.Create;
end;

{ TMCPPingApi }

function TMCPPingApi.Ping: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

{ TMCPPromptsApi }

function TMCPPromptsApi.PromptList: TListPromptsResult;
var
  LStopwatch: TStopwatch;
  LGuard: TMCPAccessGuard;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LGuard := CreateAccessGuard(RPCContext, MCPConfig);
    try
      Result := MCPConfig.Prompts.ListComplete(
        function (APrompt: TMCPPrompt): Boolean
        begin
          Result := LGuard.IsAllowed(TMCPAuthItem.FromPrompt(APrompt));
        end);
    finally
      LGuard.Free;
    end;
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
      raise EMCPException.CreateFmt(SMCPPromptNotFound, [AParams.Name]);

    CheckAccess(RPCContext, MCPConfig, TMCPAuthItem.FromPrompt(LPrompt), SMCPPromptNotFound, AParams.Name);

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

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPInitializeApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPPromptsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPResourcesApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPLoggingApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPPingApi, MCPNeonConfig);
end.
