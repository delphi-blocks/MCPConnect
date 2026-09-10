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

/// <summary>
///   The per-request log level: 2026-07-28 removed "logging/setLevel" and made
///   silence the default, so a server sent no
///   "io.modelcontextprotocol/logLevel" MUST NOT emit "notifications/message"
///   for that request, and a level it was sent is a minimum severity. What is
///   covered here is the rule rather than the plumbing - what leaves the
///   server, at which level, and what never leaves at all.
/// </summary>
unit MCPConnect.Tests.Transport.Logging;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>
  ///   The severity rule on its own: TMCPLogLevel is declared in the
  ///   alphabetical order of its wire names, so nothing about ordering may be
  ///   read off the enum itself.
  /// </summary>
  [TestFixture]
  TMCPLogSeverityTest = class(TObject)
  public
    [Test]
    procedure TestSeverityFollowsRFC5424();
    [Test]
    procedure TestSeverityIsNotTheEnumOrder();

    [Test]
    [TestCase('warning emits error', 'Warning,Error,True')]
    [TestCase('warning emits warning', 'Warning,Warning,True')]
    [TestCase('warning emits emergency', 'Warning,Emergency,True')]
    [TestCase('warning drops notice', 'Warning,Notice,False')]
    [TestCase('warning drops info', 'Warning,Info,False')]
    [TestCase('warning drops debug', 'Warning,Debug,False')]
    [TestCase('debug emits everything', 'Debug,Debug,True')]
    [TestCase('emergency drops alert', 'Emergency,Alert,False')]
    procedure TestEmits(const AMinimum, AMessage: string; AExpected: Boolean);

    [Test]
    procedure TestNamesRoundTrip();
    [Test]
    procedure TestUnknownNameIsRefused();
  end;

  /// <summary>Keeps every SSE frame the handler wrote.</summary>
  TLogRecordingWriter = class(TInterfacedObject, IMCPTransportWriter)
  private
    FFrames: TStringList;
  public
    constructor Create(AFrames: TStringList);

    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TLoggingTest = class(TObject)
  private
    FServer: TMCPServer;
    FFrames: TStringList;
    FHookLevel: string;
    FHookCalls: Integer;

    procedure ConfigureServer(AMeta: TMCPValidationLevel = TMCPValidationLevel.Strict);

    /// <summary>
    ///   Calls ATool over a stream the notifications can travel on. ALevel is
    ///   written into the request "_meta" as the client would; empty asks for
    ///   no logging at all.
    /// </summary>
    function Call(const ATool: string; const ALevel: string = ''): string;

    /// <summary>How many log notifications reached the client.</summary>
    function LogFrames: Integer;

    /// <summary>Everything the client received, frames and reply alike.</summary>
    function Written: string;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestNothingIsSentWhenTheRequestAskedForNoLevel();
    [Test]
    procedure TestAMessageAtTheRequestedLevelIsSent();
    [Test]
    procedure TestAGraverMessageIsSent();
    [Test]
    procedure TestAChattierMessageIsDropped();
    [Test]
    procedure TestTheLoggerNameTravels();

    [Test]
    procedure TestAToolCanAskWhetherAnythingWouldBeSent();

    [Test]
    procedure TestAHandBuiltNotificationIsDroppedWhenNoLevelWasAsked();
    [Test]
    procedure TestAHandBuiltNotificationBelowTheLevelIsDropped();
    [Test]
    procedure TestAHandBuiltNotificationAtTheLevelIsSent();
    [Test]
    procedure TestNothingIsDroppedWhenNobodyReadTheLevel();
    [Test]
    procedure TestTheChannelStaysSilentWhenNobodyReadTheLevel();

    [Test]
    procedure TestTheHookIsCalledWithTheRequestedLevel();
    [Test]
    procedure TestTheHookIsNotCalledWhenNoLevelWasSent();

    [Test]
    procedure TestTheToolStillAnswers();
  end;

implementation

uses
  System.TypInfo,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Notifications;

type
  TLoggingTools = class(TObject)
  private
    [Context] FLog: TMCPLog;
    [Context] FQueue: TMCPMessageQueue;
  public
    /// <summary>Logs one message at each of four levels.</summary>
    [McpTool('chatter', 'Logs at four levels on the way to an answer')]
    function Chatter: string;

    /// <summary>Logs with a logger name.</summary>
    [McpTool('named', 'Logs through a named logger')]
    function Named: string;

    /// <summary>
    ///   Reports what the channel says about the request, so a test can see
    ///   what a tool that skips expensive work would have seen.
    /// </summary>
    [McpTool('inspect', 'Answers what the log channel says about the request')]
    function Inspect: string;

    /// <summary>
    ///   Builds the notification by hand, bypassing the channel - which is
    ///   what the transport has to catch.
    /// </summary>
    [McpTool('freelance', 'Sends a log notification nobody asked for')]
    function Freelance: string;

    /// <summary>A hand-built notification at warning level.</summary>
    [McpTool('freelance_warning', 'Sends a warning nobody asked for')]
    function FreelanceWarning: string;
  end;

function TLoggingTools.Chatter: string;
begin
  FLog.Log(TMCPLogLevel.Debug, 'a debug line');
  FLog.Log(TMCPLogLevel.Info, 'an info line');
  FLog.Log(TMCPLogLevel.Warning, 'a warning line');
  FLog.Log(TMCPLogLevel.Error, 'an error line');
  Result := 'chattered';
end;

function TLoggingTools.Named: string;
begin
  FLog.Log(TMCPLogLevel.Warning, 'a warning line', 'rates');
  Result := 'named';
end;

function TLoggingTools.Inspect: string;
begin
  Result := Format('wanted=%s emits_debug=%s emits_error=%s', [
    BoolToStr(FLog.Wanted, True),
    BoolToStr(FLog.Emits(TMCPLogLevel.Debug), True),
    BoolToStr(FLog.Emits(TMCPLogLevel.Error), True)]);
end;

function TLoggingTools.Freelance: string;
begin
  FQueue.Enqueue(TMCPNotification.LogMessage(TMCPLogLevel.Debug, 'a debug line nobody asked for'));
  Result := 'worked';
end;

function TLoggingTools.FreelanceWarning: string;
begin
  FQueue.Enqueue(TMCPNotification.LogMessage(TMCPLogLevel.Warning, 'a warning line nobody asked for'));
  Result := 'worked';
end;

{ TMCPLogSeverityTest }

procedure TMCPLogSeverityTest.TestSeverityFollowsRFC5424;
begin
  Assert.AreEqual(0, MCPLogSeverity(TMCPLogLevel.Emergency));
  Assert.AreEqual(1, MCPLogSeverity(TMCPLogLevel.Alert));
  Assert.AreEqual(2, MCPLogSeverity(TMCPLogLevel.Critical));
  Assert.AreEqual(3, MCPLogSeverity(TMCPLogLevel.Error));
  Assert.AreEqual(4, MCPLogSeverity(TMCPLogLevel.Warning));
  Assert.AreEqual(5, MCPLogSeverity(TMCPLogLevel.Notice));
  Assert.AreEqual(6, MCPLogSeverity(TMCPLogLevel.Info));
  Assert.AreEqual(7, MCPLogSeverity(TMCPLogLevel.Debug));
end;

procedure TMCPLogSeverityTest.TestSeverityIsNotTheEnumOrder;
begin
  // The enum is alphabetical, so Alert comes first and Emergency fourth while
  // the severities run the other way. Anything that used Ord would be wrong.
  Assert.AreEqual(0, Ord(TMCPLogLevel.Alert));
  Assert.AreEqual(3, Ord(TMCPLogLevel.Emergency));

  Assert.IsTrue(MCPLogSeverity(TMCPLogLevel.Emergency) < MCPLogSeverity(TMCPLogLevel.Alert),
    'Emergency is the graver of the two whatever the declaration says');
end;

procedure TMCPLogSeverityTest.TestEmits(const AMinimum, AMessage: string;
  AExpected: Boolean);
var
  LMinimum, LMessage: TMCPLogLevel;
begin
  LMinimum := TMCPLogLevel(GetEnumValue(TypeInfo(TMCPLogLevel), AMinimum));
  LMessage := TMCPLogLevel(GetEnumValue(TypeInfo(TMCPLogLevel), AMessage));

  Assert.AreEqual(AExpected, MCPLogLevelEmits(LMinimum, LMessage),
    AMinimum + '/' + AMessage);
end;

procedure TMCPLogSeverityTest.TestNamesRoundTrip;
var
  LLevel, LParsed: TMCPLogLevel;
begin
  // Every level the enum has is spelled by a name the parser knows, and the
  // names are the lower-case ones of the wire
  for LLevel := Low(TMCPLogLevel) to High(TMCPLogLevel) do
  begin
    var LName := LowerCase(GetEnumName(TypeInfo(TMCPLogLevel), Ord(LLevel)));
    Assert.IsTrue(MCPLogLevelFromName(LName, LParsed), LName);
    Assert.AreEqual(Ord(LLevel), Ord(LParsed), LName);
  end;
end;

procedure TMCPLogSeverityTest.TestUnknownNameIsRefused;
var
  LLevel: TMCPLogLevel;
begin
  Assert.IsFalse(MCPLogLevelFromName('', LLevel), 'no name is not a level');
  Assert.IsFalse(MCPLogLevelFromName('verbose', LLevel), 'nor is a level MCP has not got');
end;

{ TLogRecordingWriter }

constructor TLogRecordingWriter.Create(AFrames: TStringList);
begin
  inherited Create;
  FFrames := AFrames;
end;

procedure TLogRecordingWriter.Write(const AValue: string);
begin
  FFrames.Add(AValue);
end;

function TLogRecordingWriter.Connected: Boolean;
begin
  Result := True;
end;

function TLogRecordingWriter.SupportsStreaming: Boolean;
begin
  Result := True;
end;

{ TLoggingTest }

procedure TLoggingTest.Setup;
begin
  FFrames := TStringList.Create;
  FHookLevel := '';
  FHookCalls := 0;
end;

procedure TLoggingTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
  FFrames.Free;
end;

procedure TLoggingTest.ConfigureServer(AMeta: TMCPValidationLevel);
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('logging-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(AMeta)
    .BackToMCP
    .MessageHandling
      .OnSetLogLevel(
        procedure (AContext: TJRPCContext; ALevel: TMCPLogLevel)
        begin
          Inc(FHookCalls);
          FHookLevel := GetEnumName(TypeInfo(TMCPLogLevel), Ord(ALevel));
        end)
    .BackToMCP
    .Tools
      .RegisterClass(TLoggingTools)
    .BackToMCP
  .ApplyConfig;
end;

function TLoggingTest.Call(const ATool, ALevel: string): string;
var
  LHandler: TMCPTransportHandler;
  LBody, LMetaLevel, LContent: string;
begin
  if ALevel.IsEmpty then
    LMetaLevel := ''
  else
    LMetaLevel := Format(',"io.modelcontextprotocol/logLevel":"%s"', [ALevel]);

  LBody := Format(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":' +
    '{"name":"%s","arguments":{},"_meta":{' +
    '"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
    '"io.modelcontextprotocol/clientCapabilities":{}%s}}}', [ATool, LMetaLevel]);

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TLogRecordingWriter.Create(FFrames));
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        // A log notification travels on the response stream of the request it
        // belongs to, so there has to be one
        ARequest.Accept := 'application/json, text/event-stream';
        ARequest.Content := LBody;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

function TLoggingTest.LogFrames: Integer;
var
  LFrame: string;
begin
  Result := 0;
  for LFrame in FFrames do
    if LFrame.Contains('notifications/message') then
      Inc(Result);
end;

function TLoggingTest.Written: string;
begin
  Result := FFrames.Text;
end;

procedure TLoggingTest.TestNothingIsSentWhenTheRequestAskedForNoLevel;
begin
  ConfigureServer();

  // The rule: "If absent, the server MUST NOT send any notifications/message
  // for this request." The tool logs all the same - it has no way of knowing -
  // and nothing goes out.
  Call('chatter');

  Assert.AreEqual(0, LogFrames, Written);
end;

procedure TLoggingTest.TestAMessageAtTheRequestedLevelIsSent;
begin
  ConfigureServer();

  Call('chatter', 'warning');

  Assert.Contains(Written, 'a warning line');
  Assert.Contains(Written, '"level":"warning"');
end;

procedure TLoggingTest.TestAGraverMessageIsSent;
begin
  ConfigureServer();

  // The level is a minimum, and error is graver than warning
  Call('chatter', 'warning');

  Assert.Contains(Written, 'an error line');
  Assert.AreEqual(2, LogFrames, 'warning and error, and neither of the other two');
end;

procedure TLoggingTest.TestAChattierMessageIsDropped;
begin
  ConfigureServer();

  Call('chatter', 'warning');

  Assert.DoesNotContain(Written, 'a debug line');
  Assert.DoesNotContain(Written, 'an info line');
end;

procedure TLoggingTest.TestTheLoggerNameTravels;
begin
  ConfigureServer();

  Call('named', 'debug');

  Assert.AreEqual(1, LogFrames, Written);
  Assert.Contains(Written, '"logger":"rates"');
end;

procedure TLoggingTest.TestAToolCanAskWhetherAnythingWouldBeSent;
var
  LAnswer: string;
begin
  ConfigureServer();

  // What a tool with something expensive to prepare *for* the logging asks
  // before preparing it. The answer travels in the tool result, which over a
  // stream is one of the frames rather than the reply body.
  LAnswer := Written + Call('inspect', 'warning');

  Assert.Contains(LAnswer, 'wanted=True');
  Assert.Contains(LAnswer, 'emits_debug=False');
  Assert.Contains(LAnswer, 'emits_error=True');

  FFrames.Clear;
  LAnswer := Written + Call('inspect');
  Assert.Contains(LAnswer, 'wanted=False');
  Assert.Contains(LAnswer, 'emits_error=False');
end;

procedure TLoggingTest.TestAHandBuiltNotificationIsDroppedWhenNoLevelWasAsked;
begin
  ConfigureServer();

  // Not everything goes through the channel: a tool can build a notification
  // itself, and the MUST NOT still applies to it
  Call('freelance');

  Assert.AreEqual(0, LogFrames, Written);
end;

procedure TLoggingTest.TestAHandBuiltNotificationBelowTheLevelIsDropped;
begin
  ConfigureServer();

  Call('freelance', 'warning');

  Assert.AreEqual(0, LogFrames, Written);
end;

procedure TLoggingTest.TestAHandBuiltNotificationAtTheLevelIsSent;
begin
  ConfigureServer();

  // The gate drops what the request did not ask for, not everything built by
  // hand: this one is at the level that was asked for
  Call('freelance_warning', 'warning');

  Assert.AreEqual(1, LogFrames, Written);
  Assert.Contains(Written, 'a warning line nobody asked for');
end;

procedure TLoggingTest.TestNothingIsDroppedWhenNobodyReadTheLevel;
begin
  // With the "_meta" check off nothing read the request's level, so the server
  // cannot tell an unasked-for notification from a wanted one. Unverifiable is
  // not the same as wrong, and dropping here would silence a server that has
  // done nothing out of order.
  ConfigureServer(TMCPValidationLevel.Off);

  Call('freelance');

  Assert.AreEqual(1, LogFrames, Written);
end;

procedure TLoggingTest.TestTheChannelStaysSilentWhenNobodyReadTheLevel;
begin
  // The channel is the other side of the same coin: it was never told what the
  // request asked for, and unasked is unasked
  ConfigureServer(TMCPValidationLevel.Off);

  Call('chatter', 'debug');

  Assert.AreEqual(0, LogFrames, Written);
end;

procedure TLoggingTest.TestTheHookIsCalledWithTheRequestedLevel;
begin
  ConfigureServer();

  Call('chatter', 'notice');

  Assert.AreEqual(1, FHookCalls);
  Assert.AreEqual('Notice', FHookLevel);
end;

procedure TLoggingTest.TestTheHookIsNotCalledWhenNoLevelWasSent;
begin
  ConfigureServer();

  // There is nothing to tell a server about a request that asked for nothing
  Call('chatter');

  Assert.AreEqual(0, FHookCalls);
end;

procedure TLoggingTest.TestTheToolStillAnswers;
var
  LReply: string;
begin
  ConfigureServer();

  LReply := Call('chatter', 'warning');

  Assert.Contains(Written + LReply, 'chattered',
    'the notifications are beside the result, not instead of it');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPLogSeverityTest);
  TDUnitX.RegisterTestFixture(TLoggingTest);

end.
