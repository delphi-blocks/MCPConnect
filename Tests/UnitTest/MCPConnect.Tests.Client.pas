{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{                                                                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///   The MCP client core and the Streamable HTTP transport, against a real
///   Indy-hosted TMCPServer on loopback: discovery, tools/list paging, a tool
///   call with progress and log notifications, and a typed remote error.
/// </summary>
unit MCPConnect.Tests.Client;

interface

uses
  System.SysUtils, System.Classes, System.JSON,

  DUnitX.TestFramework,

  IdSocketHandle,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.Transport.Indy,
  MCPConnect.Client,
  MCPConnect.Client.Transport.HTTP;

type
  /// <summary>
  ///   A tool class the client tests call: one tool reports progress and logs,
  ///   the other answers a plain string.
  /// </summary>
  [McpScope('clienttest')]
  TClientTestTools = class
  private
    [Context] FProgress: TMCPProgress;
    [Context] FLog: TMCPLog;
  public
    [McpTool('ping', 'Answers pong and reports progress')]
    function Ping: string;

    [McpTool('echo', 'Answers echo')]
    function Echo: string;
  end;

  [TestFixture]
  TMCPClientTest = class
  private
    FServer: TMCPIndyServer;
    FTransport: TMCPHTTPTransport;
    FClient: TMCPClient;
    FURL: string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestConnectDiscoversServer;
    [Test] procedure TestListToolsFollowsPaging;
    [Test] procedure TestCallReturnsRawResult;
    [Test] procedure TestProgressAndLogNotifications;
    [Test] procedure TestRemoteErrorIsTyped;
  end;

implementation

uses
  System.Generics.Collections;

{ TClientTestTools }

function TClientTestTools.Ping: string;
begin
  FProgress.Report(1, 2, 'half way');
  FLog.Log(TMCPLogLevel.Warning, 'a warning from the tool');
  Result := 'pong';
end;

function TClientTestTools.Echo: string;
begin
  Result := 'echo';
end;

{ TMCPClientTest }

procedure TMCPClientTest.Setup;
var
  LBinding: TIdSocketHandle;
begin
  FServer := TMCPIndyServer.CreateMCPServer(nil);
  FServer.MCPServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('mcp-client-test')
      .SetVersion('1.0')
      // One tool per page, so tools/list has to follow the cursor.
      .SetPageSize(1)
    .BackToMCP
    .Tools
      .RegisterClass(TClientTestTools)
    .BackToMCP
  .ApplyConfig;

  LBinding := FServer.Bindings.Add;
  LBinding.IP := '127.0.0.1';
  LBinding.Port := 0;
  FServer.Active := True;

  FURL := Format('http://127.0.0.1:%d/mcp', [FServer.Bindings[0].Port]);

  FTransport := TMCPHTTPTransport.Create(FURL);
  FClient := TMCPClient.Create(FTransport);
  FClient.Connect;
end;

procedure TMCPClientTest.TearDown;
begin
  FClient.Free;
  // The client held the only interface reference to the transport.
  FTransport := nil;
  FServer.Active := False;
  FServer.Free;
end;

procedure TMCPClientTest.TestConnectDiscoversServer;
begin
  Assert.AreEqual('2026-07-28', FClient.ProtocolVersion);
  Assert.AreEqual('mcp-client-test', FClient.ServerInfo.Name);
  Assert.AreEqual('1.0', FClient.ServerInfo.Version);
end;

procedure TMCPClientTest.TestListToolsFollowsPaging;
var
  LTools: TMCPTools;
begin
  LTools := FClient.ListTools;
  try
    Assert.AreEqual(2, LTools.Count);
    Assert.AreEqual('clienttest_echo', LTools[0].Name);
    Assert.AreEqual('clienttest_ping', LTools[1].Name);
  finally
    LTools.Free;
  end;
end;

procedure TMCPClientTest.TestCallReturnsRawResult;
var
  LParams: TCallToolRequestParams;
  LResult: TJSONValue;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := 'clienttest_echo';

    LResult := FClient.Call('tools/call', LParams);
    try
      Assert.IsTrue(LResult.ToJSON.Contains('echo'), LResult.ToJSON);
    finally
      LResult.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPClientTest.TestProgressAndLogNotifications;
var
  LParams: TCallToolRequestParams;
  LResult: TJSONValue;
  LMethods: TStringList;
begin
  LMethods := TStringList.Create;
  try
    FClient.OnNotification :=
      procedure(const ANotification: TJRPCNotification)
      begin
        LMethods.Add(ANotification.Method);
      end;

    LParams := TCallToolRequestParams.Create;
    try
      LParams.Name := 'clienttest_ping';
      // Both channels are opt-in per request.
      LParams.RequestMeta.SetProgressToken('progress-1');
      LParams.RequestMeta.LogLevel := TMCPLogLevel.Warning;

      LResult := FClient.Call('tools/call', LParams);
      try
        Assert.IsTrue(LResult.ToJSON.Contains('pong'), LResult.ToJSON);
      finally
        LResult.Free;
      end;
    finally
      LParams.Free;
    end;

    Assert.IsTrue(LMethods.IndexOf('notifications/progress') >= 0,
      'progress notification not seen: ' + LMethods.Text);
    Assert.IsTrue(LMethods.IndexOf('notifications/message') >= 0,
      'log notification not seen: ' + LMethods.Text);
  finally
    FClient.OnNotification := nil;
    LMethods.Free;
  end;
end;

procedure TMCPClientTest.TestRemoteErrorIsTyped;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := 'clienttest_missing';

    try
      FClient.Call('tools/call', LParams).Free;
      Assert.Fail('expected EJRPCRemoteError');
    except
      on E: EJRPCRemoteError do
        // The server answers an unknown tool with Invalid Params.
        Assert.AreEqual(-32602, E.Code, E.Message);
    end;
  finally
    LParams.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPClientTest);

end.
