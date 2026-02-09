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
    function CallTool(
      [JRPCParam('name')] const AName: string;
      [JRPC('arguments')] AArguments: TJSONObject;
      [JRPC('_meta')] Meta: TJSONObject): TCallToolResult;
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
    function ReadResource([JRPCParam('uri')] const AUri: string): TReadResourceResult;
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
  MCPConnect.MCP.Invoker;

function TMCPToolsApi.CallTool(const AName: string; AArguments: TJSONObject; Meta: TJSONObject): TCallToolResult;
var
  LInvoker: IMCPInvokable;
  LTool: TObject;
  LNamespace, LToolName: string;
begin
  Result := TCallToolResult.Create;
  try
    // Find which namespace this tool belongs to
    if not MCPConfig.FindNamespaceForTool(AName, LNamespace, LToolName) then
      raise EJRPCMethodNotFoundError.CreateFmt('Tool "%s" not found (no matching namespace)', [AName]);

    // Create instance of the tool class for this namespace
    LTool := MCPConfig.CreateToolInstance(LNamespace);
    try
      LInvoker := TMCPObjectInvoker.Create(LTool);
      RPCContext.Inject(LInvoker);
      RPCContext.Inject(LTool);

      // Invoke using the tool name without namespace
      if not LInvoker.InvokeTool(LToolName, AArguments, Meta, Result) then
        raise EJRPCMethodNotFoundError.CreateFmt('Tool "%s" not found in namespace "%s"', [LToolName, LNamespace]);
    finally
      LTool.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPNotificationsApi.Cancelled(ACancelledParams: TCancelledNotificationParams);
begin

end;

procedure TMCPNotificationsApi.Initialized;
begin

end;

function TMCPToolsApi.ToolsList: TListToolsResult;
var
  LClassInfo: TMCPClassInfo;
begin
  Result := TListToolsResult.Create;
  try
    for LClassInfo in MCPConfig.Tools.GetClasses do
      TMCPToolsListGenerator.ListTools(LClassInfo.MCPClass, Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPInitializeApi }

function TMCPInitializeApi.Initialize(AInitializeParams: TInitializeParams): TInitializeResult;
begin
  Result := TInitializeResult.Create;
  try
    Result.ProtocolVersion := AInitializeParams.ProtocolVersion;
    Result.Capabilities.Tools.ListChanged := False;
    Result.Capabilities.Resources.ListChanged := False;
    Result.Capabilities.Resources.Subscribe := False;

    // At the moment we do not support prompts
    //Result.Capabilities.Prompts.ListChanged := False;

    Result.ServerInfo.Name := MCPConfig.ServerName;
    Result.ServerInfo.Version := MCPConfig.ServerVersion;
    Result.ServerInfo.Description := MCPConfig.ServerDescription;
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPResourcesApi }

function TMCPResourcesApi.ReadResource(const AUri: string): TReadResourceResult;
var
  LInvoker: IMCPInvokable;
  LTool: TObject;
  LNamespace, LToolName: string;
begin
  Result := TReadResourceResult.Create;
  Result.AddTextContent(AUri, 'text/text', 'Prova Testo');
  Exit;

  { TODO -opaolo -c : Finire 08/02/2026 09:39:18 }
  Result := TReadResourceResult.Create;
  try
    // Create instance of the tool class for this namespace
    LTool := MCPConfig.CreateToolInstance(LNamespace);
    try
      LInvoker := TMCPObjectInvoker.Create(LTool);
      RPCContext.Inject(LInvoker);
      RPCContext.Inject(LTool);
      {
      // Invoke using the tool name without namespace
      if not LInvoker.InvokeTool(LToolName, AArguments, Meta, Result) then
        raise EJRPCMethodNotFoundError.CreateFmt('Tool "%s" not found in namespace "%s"', [LToolName, LNamespace]);
      }
    finally
      LTool.Free;
    end;
  except
    Result.Free;
    raise;
  end;

end;

function TMCPResourcesApi.ResourcesList: TListResourcesResult;
var
  LClassInfo: TMCPClassInfo;
begin
  Result := TListResourcesResult.Create;
  try
    for LClassInfo in MCPConfig.Resources.GetClasses do
      TMCPResourcesListGenerator.ListResources(LClassInfo.MCPClass, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPResourcesApi.TemplatesList: TListResourceTemplatesResult;
var
  LClassInfo: TMCPClassInfo;
begin
  Result := TListResourceTemplatesResult.Create;
  try
    for LClassInfo in MCPConfig.Templates.GetClasses do
      TMCPResourcesListGenerator.ListTemplates(LClassInfo.MCPClass, Result);
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
