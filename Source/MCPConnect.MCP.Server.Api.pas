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
    function CallTool([JRPCParams] AParams: TCallToolParams): TCallToolResult;
  end;

  [JRPC('resources')]
  TMCPResourcesApi = class
  private
    procedure InternalReadResource(AParams: TReadResourceParams;
      AResource: TMCPResource; AResult: TReadResourceResult);
    procedure InternalReadTemplate(AParams: TReadResourceParams;
      ATemplate: TMCPResourceTemplate; AResult: TReadResourceResult);
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

uses
  Neon.Core.Utils,
  MCPConnect.MCP.Invoker;

{ TMCPToolApi }

function TMCPToolsApi.CallTool(AParams: TCallToolParams): TCallToolResult;
var
  LInvoker: TMCPToolInvoker;
  LTool: TMCPTool;
  LToolObj: TObject;
begin
  if not MCPConfig.Tools.ToolRegistry.TryGetValue(AParams.Name, LTool) then
    raise EMCPException.CreateFmt('Tool [%s] not found', [AParams.Name]);

  // Create an instance of the tool class
  LToolObj := TRttiUtils.CreateInstance(LTool.Classe);
  try
    RPCContext.Inject(LToolObj);

    LInvoker := TMCPToolInvoker.Create(LToolObj, LTool);
    try
      RPCContext.Inject(LInvoker);

      Result := TCallToolResult.Create;
      try
        LInvoker.Invoke(AParams, Result);
      except
        Result.Free;
        raise;
      end;
    finally
      LInvoker.Free;
    end;
  finally
    LToolObj.Free;
  end;
end;


function TMCPToolsApi.ToolsList: TListToolsResult;
begin
  Result := MCPConfig.Tools.ListEnabled;
end;

{ TMCPNotificationsApi }

procedure TMCPNotificationsApi.Cancelled(ACancelledParams: TCancelledNotificationParams);
begin

end;

procedure TMCPNotificationsApi.Initialized;
begin

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

procedure TMCPResourcesApi.InternalReadResource(AParams: TReadResourceParams;
  AResource: TMCPResource; AResult: TReadResourceResult);
var
  LInvoker: TMCPResourceInvoker;
  LResObj: TObject;
begin
  // If it's a static resource serve the file directly
  if AResource.FileName <> '' then
    TMCPStaticResource.GetResource(MCPConfig, AResource, AResult)
  else
  begin
    // Create an instance of the resource class
    LResObj := TRttiUtils.CreateInstance(AResource.Classe);
    try
      RPCContext.Inject(LResObj);

      LInvoker := TMCPResourceInvoker.Create(LResObj, AResource);
      try
        RPCContext.Inject(LInvoker);
        LInvoker.Invoke(AParams, AResult);
      finally
        LInvoker.Free;
      end;
    finally
      LResObj.Free;
    end;
  end;
end;

procedure TMCPResourcesApi.InternalReadTemplate(AParams: TReadResourceParams;
  ATemplate: TMCPResourceTemplate; AResult: TReadResourceResult);
var
  LInvoker: TMCPTemplateInvoker;
  LTplObj: TObject;
begin
  // Create an instance of the resource class
  LTplObj := TRttiUtils.CreateInstance(ATemplate.Classe);
  try
    RPCContext.Inject(LTplObj);

    LInvoker := TMCPTemplateInvoker.Create(LTplObj, ATemplate);
    try
      RPCContext.Inject(LInvoker);
      LInvoker.Invoke(AParams, AResult);
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
begin
  LTpl := nil;

  // Try to match the exact resource uri
  LRes := MCPConfig.Resources.GetResource(AParams.Uri);

  // If no resource is found the try to match with templates
  if not Assigned(LRes) then
  begin
    LTpl := MCPConfig.Resources.GetTemplate(AParams.Uri);

    if not Assigned(LTpl) then
      raise EMCPException.CreateFmt('Resource [%s] not found', [AParams.Uri]);
  end;

  Result := TReadResourceResult.Create;
  try
    if Assigned(LRes) then
      InternalReadResource(AParams, LRes, Result)
    else
      InternalReadTemplate(AParams, LTpl, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPResourcesApi.ResourcesList: TListResourcesResult;
begin
  Result := TListResourcesResult.Create;
  try
    MCPConfig.Resources.ResourceList(Result);
  except
    Result.Free;
    raise;
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
