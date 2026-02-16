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
  System.Classes, System.SysUtils, System.JSON,
  MCPConnect.JRPC.Core,
  MCPConnect.Configuration.MCP,

  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Prompts;

type
  [JRPC('tools')]
  TMCPToolsApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function ToolsList: TListToolsResult;

    [JRPC('call')]
    function CallTool([JRPCParams] const AParams: TCallToolParams): TCallToolResult;
  end;

  [JRPC('resources')]
  TMCPResourcesApi = class
  public
    [Context] RPCContext: TJRPCContext;
    [Context] MCPConfig: TMCPConfig;

    [JRPC('list')]
    function ResourcesList: TListResourcesResult;

    [JRPC('templates/list')]
    function TemplatesList: TListResourceTemplatesResult;

    [JRPC('read')]
    function ReadResource([JRPCParams] const AParams: TReadResourceParams): TReadResourceResult;
  end;


  [JRPC('notifications')]
  TMCPNotificationsApi = class
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

{ TMCPToolApi }

uses
  Neon.Core.Utils,
  MCPConnect.MCP.Invoker;

function TMCPToolsApi.CallTool(const AParams: TCallToolParams): TCallToolResult;
var
  LInvoker: IMCPInvokable;
  LTool: TMCPTool;
  LToolObj: TObject;
begin
  if not MCPConfig.Tools.ToolRegistry.TryGetValue(AParams.Name, LTool) then
    raise EMCPException.CreateFmt('Tool [%s] not found', [AParams.Name]);

  // Create an instance of the tool class
  LToolObj := TRttiUtils.CreateInstance(LTool.Classe);
  try
    LInvoker := TMCPMethodInvoker.Create(LToolObj, LTool.Method);
    RPCContext.Inject(LToolObj);
    RPCContext.Inject(LInvoker);

    Result := TCallToolResult.Create;
    try
      LInvoker.InvokeTool(AParams, Result);
    except
      Result.Free;
      raise;
    end;
  finally
    LToolObj.Free;
  end;
end;

procedure TMCPNotificationsApi.Cancelled(ACancelledParams: TCancelledNotificationParams);
begin

end;

procedure TMCPNotificationsApi.Initialized;
begin

end;

function TMCPToolsApi.ToolsList: TListToolsResult;
begin
  Result := MCPConfig.Tools.ListEnabled;
end;

{ TMCPInitializeApi }

function TMCPInitializeApi.Initialize(AInitializeParams: TInitializeParams): TInitializeResult;
begin
  Result := TInitializeResult.Create;
  try
    Result.ProtocolVersion := AInitializeParams.ProtocolVersion;
    Result.ServerInfo.Name := MCPConfig.Server.Name;
    Result.ServerInfo.Version := MCPConfig.Server.Version;
    Result.ServerInfo.Description := MCPConfig.Server.Description;

    if TMCPCapability.Tools in MCPConfig.Server.Capabilities then
      Result.Capabilities.Tools.ListChanged := False;

    if TMCPCapability.Resources in MCPConfig.Server.Capabilities then
    begin
      Result.Capabilities.Resources.ListChanged := False;
      Result.Capabilities.Resources.Subscribe := False;
    end;

    if TMCPCapability.Prompts in MCPConfig.Server.Capabilities then
      Result.Capabilities.Prompts.ListChanged := False;

  except
    Result.Free;
    raise;
  end;
end;

{ TMCPResourcesApi }

function TMCPResourcesApi.ReadResource([JRPCParams] const AParams: TReadResourceParams): TReadResourceResult;
var
  LInvoker: IMCPInvokable;
  LRes: TMCPResource;
  LResObj: TObject;
  LToolName: string;
begin
  if not MCPConfig.Resources.Registry.TryGetValue(AParams.Uri, LRes) then
    raise EMCPException.CreateFmt('Resource [%s] not found', [AParams.Uri]);

  Result := TReadResourceResult.Create;
  try
    // If it's a static resource serve the file directly
    if not LRes.FileName.IsEmpty then
      TMCPStaticResource.GetResource(MCPConfig, LRes, Result)
    else
    begin
      // Create an instance of the resource class
      LResObj := TRttiUtils.CreateInstance(LRes.Classe);
      RPCContext.Inject(LResObj);
      try
        LInvoker.InvokeResource(AParams, Result);
        LInvoker := TMCPMethodInvoker.Create(LResObj, LRes.Method);
        RPCContext.Inject(LInvoker);
      finally
        LResObj.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMCPResourcesApi.ResourcesList: TListResourcesResult;
begin
  Result := MCPConfig.Resources.ListEnabled;
end;

function TMCPResourcesApi.TemplatesList: TListResourceTemplatesResult;
begin
  { TODO -opaolo -c : Finire 16/02/2026 11:59:38 }
  Result := TListResourceTemplatesResult.Create;
end;

{ TMCPLoggingApi }

function TMCPLoggingApi.SetLevel(ASetLevelParams: TSetLevelRequestParams): TSetLevelResult;
begin
  Result := TSetLevelResult.Create;
end;

{ TMCPPingApi }

function TMCPPingApi.Ping: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPInitializeApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPResourcesApi, MCPNeonConfig);

  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPLoggingApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPPingApi, MCPNeonConfig);
end.
