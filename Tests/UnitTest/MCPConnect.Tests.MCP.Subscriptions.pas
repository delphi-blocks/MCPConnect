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
unit MCPConnect.Tests.MCP.Subscriptions;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Subscriptions,
  MCPConnect.MCP.Server.Api;

type
  /// <summary>
  ///   A server with tools and one resource, but deliberately no prompts, so
  ///   the acknowledgement has something to drop.
  /// </summary>
  TSubscribedFeatures = class
  public
    [McpTool('ping', 'Ping')]
    function Ping: string;

    [McpResource('readme', 'res://readme', 'text/plain')]
    function Readme: string;
  end;

  [TestFixture]
  TSubscriptionFilterTest = class(TObject)
  private
    FFilter: TSubscriptionFilter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEverythingIsOptIn;
    [Test]
    procedure TestFalseIsNotOptIn;
    [Test]
    procedure TestWantsResourceIsCaseSensitive;
    [Test]
    procedure TestIsEmpty;
    [Test]
    procedure TestAssignTo;
    [Test]
    procedure TestDeserializesFromTheWire;
    [Test]
    procedure TestUnrequestedFlagsAreNotEmitted;
  end;

  [TestFixture]
  TSubscriptionsListenResultTest = class(TObject)
  public
    [Test]
    procedure TestSubscriptionIdAsInteger;
    [Test]
    procedure TestSubscriptionIdAsString;
    [Test]
    procedure TestCarriesResultTypeAndServerInfoMeta;
  end;

  /// <summary>
  ///   The subscriptions/listen endpoint.
  /// </summary>
  [TestFixture]
  TMCPSubscriptionsApiTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FApi: TMCPSubscriptionsApi;
    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;
    FQueue: TMCPMessageQueue;
    FRequest: TJRPCRequest;

    function Listen(AParams: TSubscriptionsListenRequestParams): TSubscriptionsListenResult;

    /// <summary>The acknowledgement the call enqueued. Caller owns it.</summary>
    function DequeueAck: TJSONObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestListenIsARequestNotANotification;
    [Test]
    procedure TestAcknowledgesWhatTheServerCanServe;
    [Test]
    procedure TestDropsUnservableNotificationTypes;
    [Test]
    procedure TestDropsUnknownResourceUris;
    [Test]
    procedure TestKeepsKnownResourceUris;
    [Test]
    procedure TestResultCarriesTheRequestId;
  end;

implementation

uses
  System.Rtti,
  Neon.Core.Utils;

{ TSubscribedFeatures }

function TSubscribedFeatures.Ping: string;
begin
  Result := 'pong';
end;

function TSubscribedFeatures.Readme: string;
begin
  Result := 'hello';
end;

{ TSubscriptionFilterTest }

procedure TSubscriptionFilterTest.Setup;
begin
  FFilter := TSubscriptionFilter.Create;
end;

procedure TSubscriptionFilterTest.TearDown;
begin
  FFilter.Free;
end;

procedure TSubscriptionFilterTest.TestEverythingIsOptIn;
begin
  // An absent flag is not a request: the server MUST NOT send that type
  Assert.IsFalse(FFilter.WantsToolsListChanged);
  Assert.IsFalse(FFilter.WantsPromptsListChanged);
  Assert.IsFalse(FFilter.WantsResourcesListChanged);
end;

procedure TSubscriptionFilterTest.TestFalseIsNotOptIn;
begin
  FFilter.ToolsListChanged := False;
  Assert.IsFalse(FFilter.WantsToolsListChanged, 'An explicit false is still not an opt-in');

  FFilter.ToolsListChanged := True;
  Assert.IsTrue(FFilter.WantsToolsListChanged);
end;

procedure TSubscriptionFilterTest.TestWantsResourceIsCaseSensitive;
begin
  FFilter.ResourceSubscriptions := ['res://a', 'res://b'];

  Assert.IsTrue(FFilter.WantsResource('res://a'));
  Assert.IsTrue(FFilter.WantsResource('res://b'));
  Assert.IsFalse(FFilter.WantsResource('RES://A'), 'A uri is an identifier, not a label');
  Assert.IsFalse(FFilter.WantsResource('res://c'));
end;

procedure TSubscriptionFilterTest.TestIsEmpty;
begin
  Assert.IsTrue(FFilter.IsEmpty);

  FFilter.ResourceSubscriptions := ['res://a'];
  Assert.IsFalse(FFilter.IsEmpty);

  FFilter.ResourceSubscriptions := [];
  FFilter.PromptsListChanged := True;
  Assert.IsFalse(FFilter.IsEmpty);
end;

procedure TSubscriptionFilterTest.TestAssignTo;
var
  LTarget: TSubscriptionFilter;
begin
  FFilter.ToolsListChanged := True;
  FFilter.ResourceSubscriptions := ['res://a'];

  LTarget := TSubscriptionFilter.Create;
  try
    FFilter.AssignTo(LTarget);

    Assert.IsTrue(LTarget.WantsToolsListChanged);
    Assert.IsFalse(LTarget.WantsPromptsListChanged);
    Assert.IsTrue(LTarget.WantsResource('res://a'));
  finally
    LTarget.Free;
  end;
end;

procedure TSubscriptionFilterTest.TestDeserializesFromTheWire;
var
  LParams: TSubscriptionsListenRequestParams;
begin
  LParams := TSubscriptionsListenRequestParams.Create;
  try
    TNeon.JSONToObject(LParams,
      '{"notifications":{"toolsListChanged":true,' +
      '"resourceSubscriptions":["res://a","res://b"]}}', MCPNeonConfig);

    Assert.IsTrue(LParams.Notifications.WantsToolsListChanged);
    Assert.IsFalse(LParams.Notifications.WantsPromptsListChanged);
    Assert.AreEqual(2, Length(LParams.Notifications.ResourceSubscriptions));
  finally
    LParams.Free;
  end;
end;

procedure TSubscriptionFilterTest.TestUnrequestedFlagsAreNotEmitted;
var
  LJson: string;
begin
  FFilter.ToolsListChanged := True;

  LJson := TNeon.ObjectToJSONString(FFilter, MCPNeonConfig);
  Assert.IsTrue(LJson.Contains('"toolsListChanged":true'), LJson);
  Assert.IsFalse(LJson.Contains('promptsListChanged'), 'An unset flag must stay absent: ' + LJson);
  Assert.IsFalse(LJson.Contains('resourceSubscriptions'), 'An empty list must stay absent: ' + LJson);
end;

{ TSubscriptionsListenResultTest }

procedure TSubscriptionsListenResultTest.TestSubscriptionIdAsInteger;
var
  LResult: TSubscriptionsListenResult;
  LJson: TJSONObject;
begin
  LResult := TSubscriptionsListenResult.Create;
  try
    LResult.Meta.SetSubscriptionId(Int64(7));

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Assert.AreEqual('7',
        (LJson.GetValue('_meta') as TJSONObject).GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON,
        'The stream id is the listen request id and keeps its JSON type');
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TSubscriptionsListenResultTest.TestSubscriptionIdAsString;
var
  LResult: TSubscriptionsListenResult;
  LJson: TJSONObject;
begin
  LResult := TSubscriptionsListenResult.Create;
  try
    LResult.Meta.SetSubscriptionId('sub-a');

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Assert.AreEqual('"sub-a"',
        (LJson.GetValue('_meta') as TJSONObject).GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON);
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TSubscriptionsListenResultTest.TestCarriesResultTypeAndServerInfoMeta;
var
  LResult: TSubscriptionsListenResult;
  LJson, LMeta: TJSONObject;
begin
  LResult := TSubscriptionsListenResult.Create;
  try
    LResult.Meta.SetSubscriptionId(Int64(1));

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Assert.AreEqual('complete', LJson.GetValue<string>('resultType'));

      // The specialised meta must not lose what ResultMetaObject already carries
      LMeta := LJson.GetValue('_meta') as TJSONObject;
      Assert.IsNotNull(LMeta.GetValue('io.modelcontextprotocol/serverInfo'));
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

{ TMCPSubscriptionsApiTest }

procedure TMCPSubscriptionsApiTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Tools.RegisterClass(TSubscribedFeatures);
  FConfig.Resources.RegisterClass(TSubscribedFeatures);

  FGarbage := TGarbageCollector.Create;
  FQueue := TMCPMessageQueue.Create;
  FRequest := TJRPCRequest.Create;
  FRequest.Id := TJRPCID(7);

  FContext := TJRPCContext.Create;
  FContext.AddContent(TObject(FGarbage));
  FContext.AddContent(FServer.GetConfiguration<TMCPConfig>);
  FContext.AddContent(FQueue);
  FContext.AddContent(FRequest);

  FApi := TMCPSubscriptionsApi.Create;
  FContext.Inject(FApi);
end;

procedure TMCPSubscriptionsApiTest.TearDown;
begin
  FApi.Free;
  FContext.Free;
  FRequest.Free;
  FQueue.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

function TMCPSubscriptionsApiTest.Listen(
  AParams: TSubscriptionsListenRequestParams): TSubscriptionsListenResult;
begin
  try
    Result := FApi.Listen(AParams);
  finally
    AParams.Free;
  end;
end;

function TMCPSubscriptionsApiTest.DequeueAck: TJSONObject;
var
  LMessage: TJRPCMessage;
  LNotification: TJRPCNotification;
begin
  LMessage := FQueue.Dequeue;
  Assert.IsNotNull(LMessage, 'Listen should have enqueued an acknowledgement');
  try
    LNotification := LMessage as TJRPCNotification;
    Assert.AreEqual(MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED, LNotification.Method);

    Result := LNotification.Params.Clone as TJSONObject;
  finally
    LMessage.Free;
  end;
end;

procedure TMCPSubscriptionsApiTest.TestListenIsARequestNotANotification;
var
  LMethod: TRttiMethod;
begin
  // A notification carries no id and gets no response; subscriptions/listen is
  // a long-lived request, so it must not be marked [JRPCNotification]
  LMethod := TRttiUtils.Context.GetType(TMCPSubscriptionsApi).GetMethod('Listen');
  Assert.IsNotNull(LMethod);
  Assert.IsNull(TRttiUtils.FindAttribute<JRPCNotificationAttribute>(LMethod),
    'Listen must not be declared as a notification');
  Assert.IsNotNull(LMethod.ReturnType, 'Listen must return a result');
end;

procedure TMCPSubscriptionsApiTest.TestAcknowledgesWhatTheServerCanServe;
var
  LParams: TSubscriptionsListenRequestParams;
  LAck, LNotifications: TJSONObject;
begin
  LParams := TSubscriptionsListenRequestParams.Create;
  LParams.Notifications.ToolsListChanged := True;
  Listen(LParams).Free;

  LAck := DequeueAck;
  try
    LNotifications := LAck.GetValue('notifications') as TJSONObject;
    Assert.IsTrue(LNotifications.GetValue<Boolean>('toolsListChanged'));
  finally
    LAck.Free;
  end;
end;

procedure TMCPSubscriptionsApiTest.TestDropsUnservableNotificationTypes;
var
  LParams: TSubscriptionsListenRequestParams;
  LAck, LNotifications: TJSONObject;
begin
  // The server has tools and a resource, but no prompts at all
  LParams := TSubscriptionsListenRequestParams.Create;
  LParams.Notifications.ToolsListChanged := True;
  LParams.Notifications.PromptsListChanged := True;
  LParams.Notifications.ResourcesListChanged := True;
  Listen(LParams).Free;

  LAck := DequeueAck;
  try
    LNotifications := LAck.GetValue('notifications') as TJSONObject;
    Assert.IsTrue(LNotifications.GetValue<Boolean>('toolsListChanged'));
    Assert.IsTrue(LNotifications.GetValue<Boolean>('resourcesListChanged'));
    Assert.IsNull(LNotifications.GetValue('promptsListChanged'),
      'A type the server cannot report on must be left out of the acknowledgement');
  finally
    LAck.Free;
  end;
end;

procedure TMCPSubscriptionsApiTest.TestDropsUnknownResourceUris;
var
  LParams: TSubscriptionsListenRequestParams;
  LAck, LNotifications: TJSONObject;
begin
  LParams := TSubscriptionsListenRequestParams.Create;
  LParams.Notifications.ResourceSubscriptions := ['res://nope'];
  Listen(LParams).Free;

  LAck := DequeueAck;
  try
    LNotifications := LAck.GetValue('notifications') as TJSONObject;
    Assert.IsNull(LNotifications.GetValue('resourceSubscriptions'),
      'A uri the server does not serve must not be acknowledged');
  finally
    LAck.Free;
  end;
end;

procedure TMCPSubscriptionsApiTest.TestKeepsKnownResourceUris;
var
  LParams: TSubscriptionsListenRequestParams;
  LAck, LNotifications: TJSONObject;
  LUris: TJSONArray;
begin
  LParams := TSubscriptionsListenRequestParams.Create;
  LParams.Notifications.ResourceSubscriptions := ['res://readme', 'res://nope'];
  Listen(LParams).Free;

  LAck := DequeueAck;
  try
    LNotifications := LAck.GetValue('notifications') as TJSONObject;
    LUris := LNotifications.GetValue('resourceSubscriptions') as TJSONArray;
    Assert.IsNotNull(LUris);
    Assert.AreEqual(1, LUris.Count);
    Assert.AreEqual('res://readme', LUris.Items[0].Value);
  finally
    LAck.Free;
  end;
end;

procedure TMCPSubscriptionsApiTest.TestResultCarriesTheRequestId;
var
  LResult: TSubscriptionsListenResult;
  LJson: TJSONObject;
begin
  LResult := Listen(TSubscriptionsListenRequestParams.Create);
  try
    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      // Setup gave the request the integer id 7
      Assert.AreEqual('7',
        (LJson.GetValue('_meta') as TJSONObject).GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON);
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
    DequeueAck.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSubscriptionFilterTest);
  TDUnitX.RegisterTestFixture(TSubscriptionsListenResultTest);
  TDUnitX.RegisterTestFixture(TMCPSubscriptionsApiTest);

end.
