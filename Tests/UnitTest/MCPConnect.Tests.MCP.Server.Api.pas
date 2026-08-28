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
unit MCPConnect.Tests.MCP.Server.Api;

interface

uses
  System.SysUtils,
  System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types;

type
  [TestFixture]
  TMCPInitializeApiTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FApi: TMCPInitializeApi;

    function Initialize(const AProtocolVersion: string): TInitializeResult;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestInitialize_ClientRequestsSupportedOlderVersion_EchoesItBack;
    [Test]
    procedure TestInitialize_ClientRequestsSupportedLatestVersion_EchoesItBack;
    [Test]
    procedure TestInitialize_ClientRequestsUnsupportedVersion_FallsBackToLatestSupported;
    [Test]
    procedure TestInitialize_ClientRequestsEmptyVersion_FallsBackToLatestSupported;
  end;

  [TestFixture]
  TMCPNotificationTest = class(TObject)
  private
    procedure TestSerialize(ANotification: TJRPCNotification; const AExpectedMethod: string);
    procedure TestDeserialize(const AMethod: string);
    procedure TestRoundTrip(ANotification: TJRPCNotification; const AExpectedMethod: string);
  public
    [Test]
    procedure TestToolListChanged_Serialize;
    [Test]
    procedure TestToolListChanged_Deserialize;
    [Test]
    procedure TestToolListChanged_RoundTrip;

    [Test]
    procedure TestPromptListChanged_Serialize;
    [Test]
    procedure TestPromptListChanged_Deserialize;
    [Test]
    procedure TestPromptListChanged_RoundTrip;

    [Test]
    procedure TestResourceListChanged_Serialize;
    [Test]
    procedure TestResourceListChanged_Deserialize;
    [Test]
    procedure TestResourceListChanged_RoundTrip;

    [Test]
    procedure TestRootsListChanged_Serialize;
    [Test]
    procedure TestRootsListChanged_Deserialize;
    [Test]
    procedure TestRootsListChanged_RoundTrip;
  end;

implementation

procedure TMCPInitializeApiTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;

  FApi := TMCPInitializeApi.Create;
  FApi.MCPConfig := FServer.GetConfiguration<TMCPConfig>;
end;

procedure TMCPInitializeApiTest.TearDown;
begin
  FApi.Free;
  FConfig := nil;
  FServer.Free;
end;

function TMCPInitializeApiTest.Initialize(const AProtocolVersion: string): TInitializeResult;
var
  LParams: TInitializeParams;
begin
  LParams := TInitializeParams.Create;
  try
    LParams.ProtocolVersion := AProtocolVersion;
    Result := FApi.Initialize(LParams);
  finally
    LParams.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsSupportedOlderVersion_EchoesItBack;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize(MCP_PROTOCOL_VERSION_2025_06_18);
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_06_18, LResult.ProtocolVersion,
      'A supported, non-latest version requested by the client should be echoed back unchanged');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsSupportedLatestVersion_EchoesItBack;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize(MCP_PROTOCOL_VERSION_2025_11_25);
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_11_25, LResult.ProtocolVersion,
      'The latest supported version requested by the client should be echoed back unchanged');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsUnsupportedVersion_FallsBackToLatestSupported;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize('1999-01-01');
  try
    Assert.AreEqual(MCP_LATEST_PROTOCOL_VERSION, LResult.ProtocolVersion,
      'An unsupported version requested by the client must not be echoed back; the server should propose its latest supported version instead');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsEmptyVersion_FallsBackToLatestSupported;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize('');
  try
    Assert.AreEqual(MCP_LATEST_PROTOCOL_VERSION, LResult.ProtocolVersion,
      'An empty/missing version should also fall back to the server''s latest supported version');
  finally
    LResult.Free;
  end;
end;

{ TMCPNotificationTest }

procedure TMCPNotificationTest.TestSerialize(ANotification: TJRPCNotification;
  const AExpectedMethod: string);
var
  LObj: TJSONObject;
begin
  try
    LObj := ANotification.ToJsonObject;
    try
      Assert.AreEqual('2.0', LObj.GetValue<string>('jsonrpc'), 'jsonrpc version should be 2.0');
      Assert.AreEqual(AExpectedMethod, LObj.GetValue<string>('method'), 'Method should match');
      Assert.IsNull(LObj.GetValue('params'), 'Params should be nil');
      Assert.IsNull(LObj.GetValue('id'), 'Notification must not have an id field');
    finally
      LObj.Free;
    end;
  finally
    ANotification.Free;
  end;
end;

procedure TMCPNotificationTest.TestDeserialize(const AMethod: string);
var
  LJson: string;
  LMsgs: TJRPCMessages;
  LNotification: TJRPCNotification;
begin
  LJson := '{"jsonrpc":"2.0","method":"' + AMethod + '"}';
  LMsgs := TJRPCMessages.CreateFromJson(LJson);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Message should be a TJRPCNotification');
    LNotification := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('2.0', LNotification.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual(AMethod, LNotification.Method, 'Method should match');
    Assert.IsTrue(LNotification.GetType = TJRPCMessageType.Notification, 'Type should be Notification');
  finally
    LMsgs.Free;
  end;
end;

procedure TMCPNotificationTest.TestRoundTrip(ANotification: TJRPCNotification;
  const AExpectedMethod: string);
var
  LJson: string;
  LMsgs: TJRPCMessages;
  LDeserialized: TJRPCNotification;
begin
  try
    LJson := ANotification.ToJson;
  finally
    ANotification.Free;
  end;

  LMsgs := TJRPCMessages.CreateFromJson(LJson);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Round-tripped message should be a TJRPCNotification');
    LDeserialized := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual(AExpectedMethod, LDeserialized.Method, 'Method should survive round-trip');
    Assert.IsTrue(LDeserialized.GetType = TJRPCMessageType.Notification, 'Type should be Notification');
  finally
    LMsgs.Free;
  end;
end;

procedure TMCPNotificationTest.TestToolListChanged_Serialize;
begin
  TestSerialize(TToolListChangedNotification.Create, 'notifications/tools/list_changed');
end;

procedure TMCPNotificationTest.TestToolListChanged_Deserialize;
begin
  TestDeserialize('notifications/tools/list_changed');
end;

procedure TMCPNotificationTest.TestToolListChanged_RoundTrip;
begin
  TestRoundTrip(TToolListChangedNotification.Create, 'notifications/tools/list_changed');
end;

procedure TMCPNotificationTest.TestPromptListChanged_Serialize;
begin
  TestSerialize(TPromptListChangedNotification.Create, 'notifications/prompts/list_changed');
end;

procedure TMCPNotificationTest.TestPromptListChanged_Deserialize;
begin
  TestDeserialize('notifications/prompts/list_changed');
end;

procedure TMCPNotificationTest.TestPromptListChanged_RoundTrip;
begin
  TestRoundTrip(TPromptListChangedNotification.Create, 'notifications/prompts/list_changed');
end;

procedure TMCPNotificationTest.TestResourceListChanged_Serialize;
begin
  TestSerialize(TResourceListChangedNotification.Create, 'notifications/resources/list_changed');
end;

procedure TMCPNotificationTest.TestResourceListChanged_Deserialize;
begin
  TestDeserialize('notifications/resources/list_changed');
end;

procedure TMCPNotificationTest.TestResourceListChanged_RoundTrip;
begin
  TestRoundTrip(TResourceListChangedNotification.Create, 'notifications/resources/list_changed');
end;

procedure TMCPNotificationTest.TestRootsListChanged_Serialize;
begin
  TestSerialize(TRootsListChangedNotification.Create, 'notifications/roots/list_changed');
end;

procedure TMCPNotificationTest.TestRootsListChanged_Deserialize;
begin
  TestDeserialize('notifications/roots/list_changed');
end;

procedure TMCPNotificationTest.TestRootsListChanged_RoundTrip;
begin
  TestRoundTrip(TRootsListChangedNotification.Create, 'notifications/roots/list_changed');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPInitializeApiTest);
  TDUnitX.RegisterTestFixture(TMCPNotificationTest);

end.
