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
unit MCPConnect.Tests.MCP.Notifications;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Nullables,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications;

type
  /// <summary>
  ///   The notifications a server sends: method name and params shape.
  /// </summary>
  [TestFixture]
  TMCPNotificationTest = class(TObject)
  private
    /// <summary>
    ///   Renders ANotification, frees it, and returns its "params". Caller owns
    ///   the result.
    /// </summary>
    function ParamsOf(ANotification: TJRPCNotification): TJSONObject;

    /// <summary>Renders ANotification, frees it, returns its method.</summary>
    function MethodOf(ANotification: TJRPCNotification): string;
  public
    [Test]
    [TestCase('tools', '0,notifications/tools/list_changed')]
    [TestCase('prompts', '1,notifications/prompts/list_changed')]
    [TestCase('resources', '2,notifications/resources/list_changed')]
    procedure TestListChangedMethods(AWhich: Integer; const AExpected: string);

    [Test]
    procedure TestListChanged_ParamsAreEmpty;

    [Test]
    procedure TestResourceUpdated;

    [Test]
    procedure TestCancelled_IntegerRequestId;
    [Test]
    procedure TestCancelled_StringRequestId;
    [Test]
    procedure TestCancelled_ReasonOmittedWhenEmpty;

    [Test]
    procedure TestProgress_WithoutTotal;
    [Test]
    procedure TestProgress_WithTotalAndMessage;

    [Test]
    procedure TestLogMessage_String;
    [Test]
    procedure TestLogMessage_TakesOwnershipOfJsonData;
    [Test]
    [TestCase('alert', '0,alert')]
    [TestCase('critical', '1,critical')]
    [TestCase('debug', '2,debug')]
    [TestCase('emergency', '3,emergency')]
    [TestCase('error', '4,error')]
    [TestCase('info', '5,info')]
    [TestCase('notice', '6,notice')]
    [TestCase('warning', '7,warning')]
    procedure TestLogLevelsAreLowerCase(ALevel: Integer; const AExpected: string);
  end;

  /// <summary>
  ///   The notification "_meta" and the subscription id every notification
  ///   delivered on a subscriptions/listen stream has to carry.
  /// </summary>
  [TestFixture]
  TMCPNotificationMetaTest = class(TObject)
  public
    [Test]
    procedure TestMetaOmittedWhenEmpty;
    [Test]
    procedure TestSubscriptionIdAsString;
    [Test]
    procedure TestSubscriptionIdAsInteger;
    [Test]
    procedure TestSubscriptionIdReplaced;
    [Test]
    procedure TestHasSubscriptionId;
    [Test]
    procedure TestFromParams_TakesOwnershipOfParams;
  end;

  /// <summary>
  ///   TMCPLogLevel is shared with the per-request log level of
  ///   RequestMetaObject, which 2026-07-28 uses in place of logging/setLevel.
  /// </summary>
  [TestFixture]
  TMCPRequestLogLevelTest = class(TObject)
  public
    [Test]
    procedure TestRequestMetaLogLevelIsLowerCase;
    [Test]
    procedure TestRequestMetaLogLevelRoundTrips;
  end;

implementation

{ TMCPNotificationTest }

function TMCPNotificationTest.ParamsOf(ANotification: TJRPCNotification): TJSONObject;
begin
  try
    Assert.IsNotNull(ANotification.Params, 'The notification should carry params');
    Result := ANotification.Params.Clone as TJSONObject;
  finally
    ANotification.Free;
  end;
end;

function TMCPNotificationTest.MethodOf(ANotification: TJRPCNotification): string;
begin
  try
    Result := ANotification.Method;
  finally
    ANotification.Free;
  end;
end;

procedure TMCPNotificationTest.TestListChangedMethods(AWhich: Integer; const AExpected: string);
var
  LMethod: string;
begin
  case AWhich of
    0: LMethod := MethodOf(TMCPNotification.ToolListChanged);
    1: LMethod := MethodOf(TMCPNotification.PromptListChanged);
  else
    LMethod := MethodOf(TMCPNotification.ResourceListChanged);
  end;

  Assert.AreEqual(AExpected, LMethod);
end;

procedure TMCPNotificationTest.TestListChanged_ParamsAreEmpty;
var
  LParams: TJSONObject;
begin
  LParams := ParamsOf(TMCPNotification.ToolListChanged);
  try
    // A list-changed notification carries nothing but an optional _meta
    Assert.AreEqual(0, LParams.Count, LParams.ToJSON);
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestResourceUpdated;
var
  LNotification: TJRPCNotification;
  LParams: TJSONObject;
begin
  LNotification := TMCPNotification.ResourceUpdated('file:///a.txt');
  Assert.AreEqual(MCP_NOTIFY_RESOURCES_UPDATED, LNotification.Method);

  LParams := ParamsOf(LNotification);
  try
    Assert.AreEqual('file:///a.txt', LParams.GetValue<string>('uri'));
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestCancelled_IntegerRequestId;
var
  LNotification: TJRPCNotification;
  LParams: TJSONObject;
begin
  LNotification := TMCPNotification.Cancelled(42, 'user aborted');
  Assert.AreEqual(MCP_NOTIFY_CANCELLED, LNotification.Method);

  LParams := ParamsOf(LNotification);
  try
    // A RequestId is a string or an integer: the integer must stay unquoted
    Assert.AreEqual('42', LParams.GetValue('requestId').ToJSON);
    Assert.AreEqual('user aborted', LParams.GetValue<string>('reason'));
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestCancelled_StringRequestId;
var
  LParams: TJSONObject;
begin
  LParams := ParamsOf(TMCPNotification.Cancelled('req-7'));
  try
    Assert.AreEqual('"req-7"', LParams.GetValue('requestId').ToJSON);
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestCancelled_ReasonOmittedWhenEmpty;
var
  LParams: TJSONObject;
begin
  LParams := ParamsOf(TMCPNotification.Cancelled(1));
  try
    Assert.IsNull(LParams.GetValue('reason'), 'An unset optional reason must not be emitted');
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestProgress_WithoutTotal;
var
  LNotification: TJRPCNotification;
  LParams: TJSONObject;
begin
  LNotification := TMCPNotification.Progress('tok', 3);
  Assert.AreEqual(MCP_NOTIFY_PROGRESS, LNotification.Method);

  LParams := ParamsOf(LNotification);
  try
    Assert.AreEqual('"tok"', LParams.GetValue('progressToken').ToJSON);
    Assert.AreEqual(3.0, LParams.GetValue<Double>('progress'), 0.0001);
    Assert.IsNull(LParams.GetValue('total'), 'An unknown total must not be emitted');
    Assert.IsNull(LParams.GetValue('message'));
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestProgress_WithTotalAndMessage;
var
  LParams: TJSONObject;
begin
  LParams := ParamsOf(TMCPNotification.Progress(9, 3, 10, 'Indexing'));
  try
    Assert.AreEqual('9', LParams.GetValue('progressToken').ToJSON);
    Assert.AreEqual(3.0, LParams.GetValue<Double>('progress'), 0.0001);
    Assert.AreEqual(10.0, LParams.GetValue<Double>('total'), 0.0001);
    Assert.AreEqual('Indexing', LParams.GetValue<string>('message'));
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestLogMessage_String;
var
  LNotification: TJRPCNotification;
  LParams: TJSONObject;
begin
  LNotification := TMCPNotification.LogMessage(TMCPLogLevel.Warning, 'disk almost full', 'storage');
  Assert.AreEqual(MCP_NOTIFY_MESSAGE, LNotification.Method);

  LParams := ParamsOf(LNotification);
  try
    Assert.AreEqual('warning', LParams.GetValue<string>('level'));
    Assert.AreEqual('storage', LParams.GetValue<string>('logger'));
    Assert.AreEqual('disk almost full', LParams.GetValue<string>('data'));
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestLogMessage_TakesOwnershipOfJsonData;
var
  LParams: TJSONObject;
begin
  // Leak-checked by DUnitX: the params take the object over
  LParams := ParamsOf(TMCPNotification.LogMessage(TMCPLogLevel.Critical,
    TJSONObject.Create.AddPair('code', TJSONNumber.Create(7))));
  try
    Assert.AreEqual('critical', LParams.GetValue<string>('level'));
    Assert.AreEqual(7, (LParams.GetValue('data') as TJSONObject).GetValue<Integer>('code'));
    Assert.IsNull(LParams.GetValue('logger'), 'An unset logger must not be emitted');
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationTest.TestLogLevelsAreLowerCase(ALevel: Integer; const AExpected: string);
var
  LParams: TJSONObject;
begin
  LParams := ParamsOf(TMCPNotification.LogMessage(TMCPLogLevel(ALevel), 'x'));
  try
    Assert.AreEqual(AExpected, LParams.GetValue<string>('level'),
      'The schema names the severities in lower case');
  finally
    LParams.Free;
  end;
end;

{ TMCPNotificationMetaTest }

procedure TMCPNotificationMetaTest.TestMetaOmittedWhenEmpty;
var
  LParams: TListChangedNotificationParams;
  LJson: TJSONObject;
begin
  LParams := TListChangedNotificationParams.Create;
  try
    LJson := LParams.ToJSON;
    try
      Assert.IsNull(LJson.GetValue('_meta'), 'An empty _meta must not be emitted');
    finally
      LJson.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationMetaTest.TestSubscriptionIdAsString;
var
  LParams: TListChangedNotificationParams;
  LJson, LMeta: TJSONObject;
begin
  LParams := TListChangedNotificationParams.Create;
  try
    LParams.OnSubscription('sub-1');

    LJson := LParams.ToJSON;
    try
      LMeta := LJson.GetValue('_meta') as TJSONObject;
      Assert.IsNotNull(LMeta);
      Assert.AreEqual('"sub-1"', LMeta.GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON);
    finally
      LJson.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationMetaTest.TestSubscriptionIdAsInteger;
var
  LParams: TListChangedNotificationParams;
  LJson, LMeta: TJSONObject;
begin
  LParams := TListChangedNotificationParams.Create;
  try
    LParams.OnSubscription(Int64(17));

    LJson := LParams.ToJSON;
    try
      LMeta := LJson.GetValue('_meta') as TJSONObject;
      Assert.AreEqual('17', LMeta.GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON,
        'The id is the JSON-RPC id of the listen request: an integer stays unquoted');
    finally
      LJson.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationMetaTest.TestSubscriptionIdReplaced;
var
  LParams: TListChangedNotificationParams;
  LJson, LMeta: TJSONObject;
begin
  // Leak-checked: setting it twice must free the first value
  LParams := TListChangedNotificationParams.Create;
  try
    LParams.OnSubscription('first');
    LParams.OnSubscription('second');

    LJson := LParams.ToJSON;
    try
      LMeta := LJson.GetValue('_meta') as TJSONObject;
      Assert.AreEqual('"second"', LMeta.GetValue(MCP_META_SUBSCRIPTION_ID).ToJSON);
    finally
      LJson.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationMetaTest.TestHasSubscriptionId;
var
  LParams: TListChangedNotificationParams;
begin
  LParams := TListChangedNotificationParams.Create;
  try
    Assert.IsFalse(LParams.NotificationMeta.HasSubscriptionId);

    LParams.OnSubscription('s');
    Assert.IsTrue(LParams.NotificationMeta.HasSubscriptionId);
  finally
    LParams.Free;
  end;
end;

procedure TMCPNotificationMetaTest.TestFromParams_TakesOwnershipOfParams;
var
  LParams: TProgressNotificationParams;
  LNotification: TJRPCNotification;
begin
  // Leak-checked: FromParams frees the params it rendered
  LParams := TProgressNotificationParams.Create;
  LParams.SetProgressToken(Int64(1));
  LParams.Progress := 50;
  LParams.OnSubscription('sub-1');

  LNotification := TMCPNotification.FromParams(MCP_NOTIFY_PROGRESS, LParams);
  try
    Assert.AreEqual(MCP_NOTIFY_PROGRESS, LNotification.Method);
    Assert.IsTrue((LNotification.Params as TJSONObject).GetValue('_meta') <> nil,
      'FromParams is the path that lets a caller set the subscription id first');
  finally
    LNotification.Free;
  end;
end;

{ TMCPRequestLogLevelTest }

procedure TMCPRequestLogLevelTest.TestRequestMetaLogLevelIsLowerCase;
var
  LMeta: TRequestMetaObject;
  LJson: string;
begin
  // 2026-07-28 dropped logging/setLevel: the level rides on every request's
  // _meta instead, and shares TMCPLogLevel with the log notification
  LMeta := TRequestMetaObject.Create;
  try
    LMeta.ProtocolVersion := MCP_PROTOCOL_VERSION_2026_07_28;
    LMeta.LogLevel := TMCPLogLevel.Notice;

    LJson := TNeon.ObjectToJSONString(LMeta, MCPNeonConfig);
    Assert.IsTrue(LJson.Contains('"io.modelcontextprotocol/logLevel":"notice"'), LJson);
  finally
    LMeta.Free;
  end;
end;

procedure TMCPRequestLogLevelTest.TestRequestMetaLogLevelRoundTrips;
var
  LMeta: TRequestMetaObject;
begin
  LMeta := TRequestMetaObject.Create;
  try
    TNeon.JSONToObject(LMeta,
      '{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/logLevel":"emergency"}', MCPNeonConfig);

    Assert.IsTrue(LMeta.LogLevel.HasValue, 'The level should have been read back');
    Assert.AreEqual(TMCPLogLevel.Emergency, LMeta.LogLevel.Value);
  finally
    LMeta.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPNotificationTest);
  TDUnitX.RegisterTestFixture(TMCPNotificationMetaTest);
  TDUnitX.RegisterTestFixture(TMCPRequestLogLevelTest);

end.
