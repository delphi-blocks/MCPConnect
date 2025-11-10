unit MCPServerDemo.Api;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  JSON.RPC,

  MCP.Types,
  MCP.Attributes,
  MCP.Tools,
  MCP.Resources,
  MCP.Prompts;

type
  [JRPC('tools')]
  TMCPToolsApi = class
  public
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
    [JRPC('')]
    function Initialize([JRPCParams] AInitializeParams: TInitializeParams): TInitializeResult;
  end;

  TTestTool = class
  public
    [McpTool('double_or_nothing', 'Doubles or zeroes the param value')]
    function TestParam(
    [McpParam('value1', 'Test Parameter 1 for MCP')] AParam1: Int64;
    [McpParam('value2', 'Test Parameter 2 for MCP')] AParam2: Boolean
    ): Integer;

    [McpTool('discounted_items', 'Retrieves a list of discounted items on Wintech-Italia based on the specified item type')]
    function GetDiscountedItems(
      [McpParam('itemType', 'The type of item to filter. Valid values: ''courses'', ''product'', ''consulting''')]
       const AItemType: string
    ): TStringList;
  end;

const
  ServerName = 'delphi-mcp-server';
  ServerVersion = '1.0.0';

implementation

{ TMCPToolApi }

uses MCP.Invoker;

function TMCPToolsApi.Call(const AName: string; AArguments: TJSONObject; Meta: TJSONObject): TCallToolResult;
var
  LInvoker: IMCPInvokable;
  LTestTool: TTestTool;
begin
  Result := TCallToolResult.Create;
  try
    LTestTool := TTestTool.Create;
    try
      LInvoker := TMCPObjectInvoker.Create(LTestTool);
      if not LInvoker.Invoke(AName, AArguments, Meta, Result) then
        raise Exception.CreateFmt('Tool "%s" non found', [AName]);
    finally
      LTestTool.Free;
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
  Result := TMCPSchemaGenerator.ListTools(TTestTool);
end;

{ TMCPInitializeApi }

function TMCPInitializeApi.Initialize(
  AInitializeParams: TInitializeParams): TInitializeResult;
begin
  Result := TInitializeResult.Create;
  try
    Result.ProtocolVersion := AInitializeParams.ProtocolVersion;
    //Result.Capabilities.Prompts.ListChanged := False;
    Result.Capabilities.Tools.ListChanged := False;
    Result.Capabilities.Resources.ListChanged := False;
    Result.Capabilities.Resources.Subscribe := False;
    Result.ServerInfo.Name := ServerName;
    Result.ServerInfo.Version := ServerVersion;
  except
    Result.Free;
    raise;
  end;
end;

{ TTestTool }

function TTestTool.GetDiscountedItems(const AItemType: string): TStringList;
begin
  //'courses'', ''product'', ''consulting';
  Result := TStringList.Create;
  try
    if AItemType = 'courses' then
    begin
      Result.Add('Programmazione ad Oggetti con Delphi');
      Result.Add('Delphi Modern Development');
    end
    else if AItemType = 'product' then
    begin
      Result.Add('Fast Report');
      Result.Add('UniDAC');
    end
    else
      Result.Add('none');
  except
    Result.Free;
    raise;
  end;
end;

function TTestTool.TestParam(AParam1: Int64; AParam2: Boolean): Integer;
begin
  Result := AParam1;
end;

initialization
  TJRPCRegistry.Instance.RegisterClass(TMCPInitializeApi);
  TJRPCRegistry.Instance.RegisterClass(TMCPToolsApi);
  TJRPCRegistry.Instance.RegisterClass(TMCPNotificationsApi);

  //TMCPRegistry.Instance.RegisterClass(TTestTool);

end.
