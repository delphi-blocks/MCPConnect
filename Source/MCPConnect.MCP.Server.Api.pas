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
    [Context]
    MCPConfig: TMCPConfig;

    [Context]
    Context: TJRPCContext;

    [JRPC('list')]
    function List: TListToolsResult;

    [JRPC('call')]
    function Call(
        [JRPCParam('name')] const AName: string;
        [JRPC('arguments')] AArguments: TJSONObject;
        [JRPC('_meta')] Meta: TJSONObject): TCallToolResult;
  end;

  [JRPC('notifications')]
  TMCPNotificationsApi = class
  public
    [JRPC('initialized')]
    [JRPCNotification]
    procedure Initialized;

    [JRPC('cancelled')]
    [JRPCNotification]
    procedure Cancelled([JRPCParams] ACancelledParams: TCancelledNotificationParams);

  end;

  [JRPC('initialize')]
  TMCPInitializeApi = class
  public
    [Context]
    MCPConfig: TMCPConfig;

    [JRPC('')]
    function Initialize([JRPCParams] AInitializeParams: TInitializeParams): TInitializeResult;
  end;

implementation

{ TMCPToolApi }

uses
  MCPConnect.MCP.Invoker;

function TMCPToolsApi.Call(const AName: string; AArguments: TJSONObject; Meta: TJSONObject): TCallToolResult;
var
  LInvoker: IMCPInvokable;
  LTool: TObject;
begin
  Result := TCallToolResult.Create;
  try
    LTool := MCPConfig.CreateDefaultTool;
    try
      LInvoker := TMCPObjectInvoker.Create(LTool);
      Context.Inject(LInvoker);
      Context.Inject(LTool);
      if not LInvoker.Invoke(AName, AArguments, Meta, Result) then
        raise Exception.CreateFmt('Tool "%s" non found', [AName]);
    finally
      LTool.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPNotificationsApi.Cancelled([JRPCParams] ACancelledParams: TCancelledNotificationParams);
begin

end;

procedure TMCPNotificationsApi.Initialized;
begin
end;

function TMCPToolsApi.List: TListToolsResult;
begin
  Result := TMCPSchemaGenerator.ListTools(MCPConfig.GetDefaultToolClass);
end;

{ TMCPInitializeApi }

function TMCPInitializeApi.Initialize(
  AInitializeParams: TInitializeParams): TInitializeResult;
begin
  Result := TInitializeResult.Create;
  try
    Result.ProtocolVersion := AInitializeParams.ProtocolVersion;
    Result.Capabilities.Tools.ListChanged := False;

    // At the moment we do not support prompts and resources 
    //Result.Capabilities.Prompts.ListChanged := False;
    //Result.Capabilities.Resources.ListChanged := False;
    //Result.Capabilities.Resources.Subscribe := False;

    Result.ServerInfo.Name := MCPConfig.ServerName;
    Result.ServerInfo.Version := MCPConfig.ServerVersion;
  except
    Result.Free;
    raise;
  end;
end;

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPInitializeApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi, MCPNeonConfig);
  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi, MCPNeonConfig);

end.
