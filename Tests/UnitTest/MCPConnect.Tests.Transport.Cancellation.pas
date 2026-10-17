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
///   Cancellation: the TMCPCancellation a request carries, the per-connection
///   map a "notifications/cancelled" is routed through, and what the transport
///   does with a request once it is cancelled - which is to send nothing more.
/// </summary>
/// <remarks>
///   The transport tests drive a real TMCPTransportHandler on a thread of its
///   own (TListenRun, from the listen tests), against tools that block until
///   they are cancelled, and act on them from the test's thread.
/// </remarks>
unit MCPConnect.Tests.Transport.Cancellation;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.JSON,
  System.Generics.Collections,
  DUnitX.TestFramework,

  JRPC.Core,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Subscriptions,
  MCPConnect.Transport.Base,
  MCPConnect.Tests.Transport.Subscriptions;

type
  [TestFixture]
  TMCPCancellationTest = class(TObject)
  public
    [Test]
    procedure TestOnlyTheFirstCancelCountsAndKeepsItsReason;
    [Test]
    procedure TestTheHookRunsOnce;
    [Test]
    procedure TestAHookSetAfterTheCancelRunsAtOnce;
    [Test]
    procedure TestAClearedHookDoesNotRun;
    [Test]
    procedure TestWaitForReturnsAsSoonAsTheRequestIsCancelled;
    [Test]
    procedure TestWaitForTimesOutWhenNothingHappens;
    [Test]
    procedure TestCheckCancelledRaisesOnlyOnceCancelled;
  end;

  [TestFixture]
  TMCPInFlightRequestsTest = class(TObject)
  public
    [Test]
    procedure TestTheIdKeepsItsJsonType;
    [Test]
    procedure TestAnIdAlreadyInFlightIsNotFiledTwice;
    [Test]
    procedure TestRemoveSparesAnotherRequestsEntry;
    [Test]
    procedure TestAnUnknownIdIsIgnored;
  end;

  [TestFixture]
  TTransportCancellationTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;
    FInFlight: TMCPInFlightRequests;
    FRuns: TList<TListenRun>;

    /// <summary>
    ///   What MessageHandling.OnCancelled was called with, as JSON and text.
    /// </summary>
    FHookCalls: Integer;
    FHookRequestId: string;
    FHookReason: string;

    procedure ConfigureServer;
    function Registry: TMCPSubscriptionRegistry;

    function CallBody(const AId, ATool: string): string;
    function CancelBody(const ARequestId, AReason: string): string;

    /// <summary>
    ///   Starts a request on its own thread. Owned by the fixture.
    /// </summary>
    function Start(AWriter: IMCPTransportWriter; const ABody: string;
      const AAccept: string = 'application/json, text/event-stream';
      AWithInFlight: Boolean = False): TListenRun;

    /// <summary>
    ///   Sends ABody - a notification - through a handler on this thread, on the
    ///   same connection as the requests started with AWithInFlight. Answers the
    ///   status.
    /// </summary>
    function Post(const ABody: string): Integer;

    /// <summary>
    ///   Starts a request the way the stdio reader hands one to its thread: on
    ///   the fixture's connection, under its listen scope, accepting the stream.
    /// </summary>
    function StartStdio(AWriter: IMCPTransportWriter; const ABody: string;
      const AOnDispatching: TProc = nil): TListenRun;

    function ListenBody(const AId: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestADisconnectCancelsARunningTool;
    [Test]
    procedure TestCheckCancelledSendsNoError;
    [Test]
    procedure TestTheAnswerOfACancelledToolIsNotSent;
    [Test]
    procedure TestAFailedWriteCancelsTheRequest;
    [Test]
    procedure TestANotificationCancelsARequestInFlight;
    [Test]
    procedure TestACancellationNamesTheIdWithItsType;
    [Test]
    procedure TestOnCancelledIsCalledForAnUnknownRequest;
    [Test]
    procedure TestANotificationThatNamesNoRequestIsDropped;
    [Test]
    procedure TestACancelledListenEndsWithoutTheResult;
    [Test]
    procedure TestAFinishedRequestLeavesTheMap;
    [Test]
    procedure TestADocumentReplyNeverAsksWhetherTheClientIsConnected;

    [Test]
    procedure TestAStdioListenHoldsItsStream;
    [Test]
    procedure TestTheEndOfStdinEndsAListenWithoutTheResult;
    [Test]
    procedure TestTheEndOfStdinSparesARunningRequest;
    [Test]
    procedure TestClosingAScopeSparesAnotherConnection;
    [Test]
    procedure TestAListenBeforeTheFirstRegistrationIsStillTold;
    [Test]
    procedure TestAStdioClientCannotListenTwiceUnderOneId;
    [Test]
    procedure TestTwoStdioConnectionsMayShareAnId;
    [Test]
    procedure TestOnDispatchingRunsOnceTheRequestIsFiled;
  end;

implementation

uses
  JRPC.Classes,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.Transport.MediaType;

var
  /// <summary>
  ///   Set by a tool once it is running, so a test acts on it only then.
  /// </summary>
  GStarted: TEvent;

  /// <summary>
  ///   What the tool saw: whether the cancellation reached it, and why.
  /// </summary>
  GSawCancel: Boolean;
  GReason: string;

type
  TCancellableTools = class
  private
    [Context] FCancel: TMCPCancellation;
    [Context] FQueue: TMCPMessageQueue;
  public
    /// <summary>
    ///   Waits to be cancelled, then abandons the request the way a tool is
    ///   meant to: CheckCancelled.
    /// </summary>
    [McpTool('wait_for_cancel', 'Blocks until cancelled')]
    function WaitForCancel: string;

    /// <summary>
    ///   Waits to be cancelled, then answers anyway: the answer must not reach
    ///   the client.
    /// </summary>
    [McpTool('answer_anyway', 'Answers even once cancelled')]
    function AnswerAnyway: string;

    /// <summary>
    ///   Writes a notification on the stream, then waits to be cancelled.
    /// </summary>
    [McpTool('notify_then_wait', 'Notifies, then blocks')]
    function NotifyThenWait: string;

    /// <summary>
    ///   Takes longer than one read of the response queue, so the transport
    ///   loops while it runs.
    /// </summary>
    [McpTool('slow', 'Takes its time')]
    function Slow: string;
  end;

  /// <summary>
  ///   A writer that could stream, and that counts every time it is asked
  ///   whether the client is still there - answering no, so a question asked
  ///   when it should not be cancels the request and shows.
  /// </summary>
  TConnectedProbeWriter = class(TMCPTransportWriter)
  private
    FAsked: Integer;
  public
    procedure Write(const AValue: string); override;
    function Connected: Boolean; override;
    function SupportsStreaming: Boolean; override;
    property Asked: Integer read FAsked;
  end;

procedure TConnectedProbeWriter.Write(const AValue: string);
begin
end;

function TConnectedProbeWriter.Connected: Boolean;
begin
  TInterlocked.Increment(FAsked);
  Result := False;
end;

function TConnectedProbeWriter.SupportsStreaming: Boolean;
begin
  Result := True;
end;

function TCancellableTools.WaitForCancel: string;
begin
  GStarted.SetEvent;
  GSawCancel := FCancel.WaitFor(10000);
  GReason := FCancel.Reason;
  FCancel.CheckCancelled;
  Result := 'finished';
end;

function TCancellableTools.AnswerAnyway: string;
begin
  GStarted.SetEvent;
  GSawCancel := FCancel.WaitFor(10000);
  GReason := FCancel.Reason;
  Result := 'answered after the cancellation';
end;

function TCancellableTools.NotifyThenWait: string;
begin
  FQueue.Enqueue(TMCPNotification.ToolListChanged);
  GStarted.SetEvent;
  GSawCancel := FCancel.WaitFor(10000);
  GReason := FCancel.Reason;
  FCancel.CheckCancelled;
  Result := 'finished';
end;

function TCancellableTools.Slow: string;
begin
  FCancel.WaitFor(1200);
  Result := 'slow answer';
end;

{ TMCPCancellationTest }

procedure TMCPCancellationTest.TestOnlyTheFirstCancelCountsAndKeepsItsReason;
var
  LCancel: TMCPCancellation;
begin
  LCancel := TMCPCancellation.Create;
  try
    Assert.IsFalse(LCancel.IsCancelled);
    Assert.IsTrue(LCancel.Cancel('first'));
    Assert.IsFalse(LCancel.Cancel('second'), 'a second cancel is not a cancellation');
    Assert.IsTrue(LCancel.IsCancelled);
    Assert.AreEqual('first', LCancel.Reason);
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestTheHookRunsOnce;
var
  LCancel: TMCPCancellation;
  LCalls: Integer;
begin
  LCalls := 0;
  LCancel := TMCPCancellation.Create;
  try
    LCancel.OnCancel :=
      procedure (ACancellation: TMCPCancellation)
      begin
        Inc(LCalls);
      end;

    LCancel.Cancel;
    LCancel.Cancel;
    Assert.AreEqual(1, LCalls);
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestAHookSetAfterTheCancelRunsAtOnce;
var
  LCancel: TMCPCancellation;
  LCalls: Integer;
begin
  // The order a transport cannot always choose: the client may cancel before
  // anybody has said what to do about it
  LCalls := 0;
  LCancel := TMCPCancellation.Create;
  try
    LCancel.Cancel;
    LCancel.OnCancel :=
      procedure (ACancellation: TMCPCancellation)
      begin
        Inc(LCalls);
      end;
    Assert.AreEqual(1, LCalls);
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestAClearedHookDoesNotRun;
var
  LCancel: TMCPCancellation;
  LCalls: Integer;
begin
  LCalls := 0;
  LCancel := TMCPCancellation.Create;
  try
    LCancel.OnCancel :=
      procedure (ACancellation: TMCPCancellation)
      begin
        Inc(LCalls);
      end;
    LCancel.OnCancel := nil;

    LCancel.Cancel;
    Assert.AreEqual(0, LCalls);
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestWaitForReturnsAsSoonAsTheRequestIsCancelled;
var
  LCancel: TMCPCancellation;
  LCanceller: TThread;
  LStart: TDateTime;
begin
  LCancel := TMCPCancellation.Create;
  try
    LCanceller := TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Sleep(50);
        LCancel.Cancel('from another thread');
      end);
    LCanceller.FreeOnTerminate := False;

    LStart := Now;
    LCanceller.Start;
    try
      Assert.IsTrue(LCancel.WaitFor(10000));
      Assert.IsTrue((Now - LStart) * MSecsPerDay < 5000, 'WaitFor did not wake on the cancel');
    finally
      LCanceller.WaitFor;
      LCanceller.Free;
    end;
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestWaitForTimesOutWhenNothingHappens;
var
  LCancel: TMCPCancellation;
begin
  LCancel := TMCPCancellation.Create;
  try
    Assert.IsFalse(LCancel.WaitFor(20));
  finally
    LCancel.Free;
  end;
end;

procedure TMCPCancellationTest.TestCheckCancelledRaisesOnlyOnceCancelled;
var
  LCancel: TMCPCancellation;
begin
  LCancel := TMCPCancellation.Create;
  try
    Assert.WillNotRaise(
      procedure
      begin
        LCancel.CheckCancelled;
      end);

    LCancel.Cancel('stop');
    Assert.WillRaise(
      procedure
      begin
        LCancel.CheckCancelled;
      end,
      EMCPRequestCancelled);
  finally
    LCancel.Free;
  end;
end;

{ TMCPInFlightRequestsTest }

procedure TMCPInFlightRequestsTest.TestTheIdKeepsItsJsonType;
var
  LMap: TMCPInFlightRequests;
  LCancel: TMCPCancellation;
  LNumber, LString: TJSONValue;
begin
  LMap := TMCPInFlightRequests.Create;
  LCancel := TMCPCancellation.Create;
  LNumber := TJSONNumber.Create(7);
  LString := TJSONString.Create('7');
  try
    Assert.IsTrue(LMap.Add(TJRPCID('7'), LCancel));

    // 7 and "7" are different requests on the wire, and here
    Assert.IsFalse(LMap.Cancel(LNumber, ''));
    Assert.IsFalse(LCancel.IsCancelled);

    Assert.IsTrue(LMap.Cancel(LString, ''));
    Assert.IsTrue(LCancel.IsCancelled);
  finally
    LString.Free;
    LNumber.Free;
    LMap.Free;
    LCancel.Free;
  end;
end;

procedure TMCPInFlightRequestsTest.TestAnIdAlreadyInFlightIsNotFiledTwice;
var
  LMap: TMCPInFlightRequests;
  LFirst, LSecond: TMCPCancellation;
  LId: TJSONValue;
begin
  LMap := TMCPInFlightRequests.Create;
  LFirst := TMCPCancellation.Create;
  LSecond := TMCPCancellation.Create;
  LId := TJSONNumber.Create(1);
  try
    Assert.IsTrue(LMap.Add(TJRPCID(Int64(1)), LFirst));
    Assert.IsFalse(LMap.Add(TJRPCID(Int64(1)), LSecond), 'an id in use was filed again');

    LMap.Cancel(LId, '');
    Assert.IsTrue(LFirst.IsCancelled, 'the first request keeps the id');
    Assert.IsFalse(LSecond.IsCancelled);
  finally
    LId.Free;
    LMap.Free;
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TMCPInFlightRequestsTest.TestRemoveSparesAnotherRequestsEntry;
var
  LMap: TMCPInFlightRequests;
  LFiled, LOther: TMCPCancellation;
begin
  LMap := TMCPInFlightRequests.Create;
  LFiled := TMCPCancellation.Create;
  LOther := TMCPCancellation.Create;
  try
    LMap.Add(TJRPCID(Int64(1)), LFiled);

    LMap.Remove(TJRPCID(Int64(1)), LOther);
    Assert.AreEqual(1, LMap.Count, 'a request removed an entry that was not its own');

    LMap.Remove(TJRPCID(Int64(1)), LFiled);
    Assert.AreEqual(0, LMap.Count);
  finally
    LMap.Free;
    LOther.Free;
    LFiled.Free;
  end;
end;

procedure TMCPInFlightRequestsTest.TestAnUnknownIdIsIgnored;
var
  LMap: TMCPInFlightRequests;
  LId: TJSONValue;
begin
  LMap := TMCPInFlightRequests.Create;
  LId := TJSONNumber.Create(99);
  try
    Assert.IsFalse(LMap.Cancel(LId, 'nobody'));
    Assert.IsFalse(LMap.Cancel(nil, 'nothing'));
  finally
    LId.Free;
    LMap.Free;
  end;
end;


{ TTransportCancellationTest }

procedure TTransportCancellationTest.Setup;
begin
  FRuns := TList<TListenRun>.Create;
  FInFlight := TMCPInFlightRequests.Create;
  GStarted.ResetEvent;
  GSawCancel := False;
  GReason := '';
  FHookCalls := 0;
  FHookRequestId := '';
  FHookReason := '';
end;

procedure TTransportCancellationTest.TearDown;
begin
  // A run still blocked in a tool is released by cancelling everything it could
  // be waiting on: the streams, and the requests filed in the map
  if Assigned(FConfig) then
    Registry.CloseAll(False);

  for var LRun in FRuns do
    LRun.Free;
  FRuns.Free;

  FInFlight.Free;

  FConfig := nil;
  FServer.Free;
  FServer := nil;
end;

procedure TTransportCancellationTest.ConfigureServer;
begin
  FServer := TMCPServer.Create(nil);

  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig
    .Server
      .SetName('cancel-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TCancellableTools)
    .BackToMCP
    .MessageHandling
      .OnCancelled(
        procedure (AContext: TJRPCContext; AParams: TCancelledNotificationParams)
        begin
          Inc(FHookCalls);
          FHookRequestId := AParams.RequestId.ToJSON;
          FHookReason := AParams.Reason.GetValueOrDefault;
        end)
    .BackToMCP
  .ApplyConfig;

  FConfig.Subscriptions.KeepAliveInterval := 50;
end;

function TTransportCancellationTest.Registry: TMCPSubscriptionRegistry;
begin
  Result := FConfig.Subscriptions.Registry;
end;

function TTransportCancellationTest.CallBody(const AId, ATool: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","id":%s,"method":"tools/call","params":{"name":"%s","arguments":{}}}',
    [AId, ATool]);
end;

function TTransportCancellationTest.CancelBody(const ARequestId, AReason: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":%s,"reason":"%s"}}',
    [ARequestId, AReason]);
end;

function TTransportCancellationTest.Start(AWriter: IMCPTransportWriter;
  const ABody, AAccept: string; AWithInFlight: Boolean): TListenRun;
var
  LInFlight: TMCPInFlightRequests;
begin
  if AWithInFlight then
    LInFlight := FInFlight
  else
    LInFlight := nil;

  Result := TListenRun.Create(FServer, AWriter, ABody, AAccept, LInFlight);
  FRuns.Add(Result);
end;

function TTransportCancellationTest.Post(const ABody: string): Integer;
var
  LTransport: TMCPTransportHandler;
  LHandler: IMCPTransportHandler;
  LCode: Integer;
begin
  LCode := 0;
  LTransport := TMCPTransportHandler.Create(FServer, TRecordingStreamWriter.Create);
  LTransport.InFlight := FInFlight;
  LHandler := LTransport;
  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := '/';
      ARequest.Command := 'POST';
      ARequest.Protocol := TTransportProtocol.StreamableHTTP;
      ARequest.Accept := 'application/json, text/event-stream';
      ARequest.Content := ABody;
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LCode := AResponse.Code;
    end);
  Result := LCode;
end;

function TTransportCancellationTest.StartStdio(AWriter: IMCPTransportWriter;
  const ABody: string; const AOnDispatching: TProc): TListenRun;
begin
  Result := TListenRun.Create(FServer, AWriter, ABody, TMediaType.TEXT_EVENT_STREAM,
    FInFlight, TTransportProtocol.Stdio, 'stdio-connection', AOnDispatching);
  FRuns.Add(Result);
end;

function TTransportCancellationTest.ListenBody(const AId: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","id":%s,"method":"subscriptions/listen",' +
    '"params":{"notifications":{"toolsListChanged":true}}}', [AId]);
end;

procedure TTransportCancellationTest.TestADisconnectCancelsARunningTool;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // Over HTTP, closing the stream is how a request is cancelled
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'wait_for_cancel'));
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000), 'the tool never started');

  LWriter.Disconnect;
  Assert.IsTrue(LRun.WaitFor, 'the request did not end');

  Assert.IsTrue(GSawCancel, 'the tool was not told');
  Assert.AreEqual('The client closed the connection', GReason);
end;

procedure TTransportCancellationTest.TestCheckCancelledSendsNoError;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // The server MUST NOT send anything further for a cancelled request, and an
  // error is something
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'wait_for_cancel'));
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000));

  LWriter.Disconnect;
  Assert.IsTrue(LRun.WaitFor);

  Assert.AreEqual(0, LWriter.FrameCount, 'something was written for a cancelled request');
end;

procedure TTransportCancellationTest.TestTheAnswerOfACancelledToolIsNotSent;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // A tool that ignores the cancellation still runs to the end: what it then
  // answers is dropped by the transport
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'answer_anyway'));
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000));

  LWriter.Disconnect;
  Assert.IsTrue(LRun.WaitFor);

  Assert.IsTrue(GSawCancel);
  Assert.AreEqual(0, LWriter.FrameCount, 'the answer of a cancelled request was sent');
end;

procedure TTransportCancellationTest.TestAFailedWriteCancelsTheRequest;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // A socket that still says it is connected but cannot be written to is a
  // client that went away too - and the run ends without a 500
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;
  LWriter.FailWrites := True;

  LRun := Start(LRef, CallBody('41', 'notify_then_wait'));
  Assert.IsTrue(LRun.WaitFor, 'the request did not end');

  Assert.IsTrue(GSawCancel, 'the tool was not told');
  Assert.AreEqual('The response stream could not be written', GReason);
  Assert.AreNotEqual(500, LRun.Code);
end;

procedure TTransportCancellationTest.TestANotificationCancelsARequestInFlight;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // The stdio shape: one connection, a map of what it has in flight, and the
  // cancellation arriving while the request runs. Answered as a document, so
  // the status and the body say what the client gets: nothing.
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'wait_for_cancel'), 'application/json', True);
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000));
  Assert.AreEqual(1, FInFlight.Count);

  Assert.AreEqual(202, Post(CancelBody('41', 'no longer needed')));
  Assert.IsTrue(LRun.WaitFor, 'the request did not end');

  Assert.IsTrue(GSawCancel);
  Assert.AreEqual('no longer needed', GReason);
  Assert.AreEqual('', LRun.Content, 'a cancelled request was answered');

  Assert.AreEqual(1, FHookCalls);
  Assert.AreEqual('41', FHookRequestId);
  Assert.AreEqual('no longer needed', FHookReason);
end;

procedure TTransportCancellationTest.TestACancellationNamesTheIdWithItsType;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('"41"', 'wait_for_cancel'), 'application/json', True);
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000));

  // The number 41 is not the request "41"
  Post(CancelBody('41', 'wrong one'));
  Assert.IsFalse(LRun.WaitFor(200), 'a cancellation for 41 cancelled "41"');

  Post(CancelBody('"41"', 'right one'));
  Assert.IsTrue(LRun.WaitFor);
  Assert.AreEqual('right one', GReason);
end;

procedure TTransportCancellationTest.TestOnCancelledIsCalledForAnUnknownRequest;
begin
  // The request may have finished already, and over HTTP there is nothing to
  // look it up in: the hook hears about it either way, and the client gets the
  // 202 a notification gets
  ConfigureServer;

  Assert.AreEqual(202, Post(CancelBody('12', 'too late')));

  Assert.AreEqual(1, FHookCalls);
  Assert.AreEqual('12', FHookRequestId);
  Assert.AreEqual('too late', FHookReason);
end;

procedure TTransportCancellationTest.TestANotificationThatNamesNoRequestIsDropped;
begin
  ConfigureServer;

  Assert.AreEqual(202, Post(
    '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"reason":"which?"}}'));
  Assert.AreEqual(0, FHookCalls, 'a cancellation that names no request reached the hook');
end;

procedure TTransportCancellationTest.TestACancelledListenEndsWithoutTheResult;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // How a client ends a subscription where it cannot close the stream: the
  // subscription ends abruptly, and the server sends nothing more - the listen
  // result included
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef,
    '{"jsonrpc":"2.0","id":7,"method":"subscriptions/listen",' +
    '"params":{"notifications":{"toolsListChanged":true}}}',
    'application/json, text/event-stream', True);
  Assert.IsTrue(LWriter.WaitForFrames(1), 'the listen was not acknowledged');

  Post(CancelBody('7', 'done listening'));
  Assert.IsTrue(LRun.WaitFor, 'the stream did not end');

  Assert.AreEqual(1, LWriter.FrameCount, 'something followed the acknowledgement');
  Assert.Contains(LWriter.Frames[0], MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED);
  Assert.AreEqual(0, Registry.Count, 'the subscription was left behind');
end;

procedure TTransportCancellationTest.TestAFinishedRequestLeavesTheMap;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'wait_for_cancel'), 'application/json', True);
  Assert.AreEqual(wrSignaled, GStarted.WaitFor(5000));
  Post(CancelBody('41', ''));
  Assert.IsTrue(LRun.WaitFor);

  Assert.AreEqual(0, FInFlight.Count, 'a finished request is still in flight');
end;

procedure TTransportCancellationTest.TestADocumentReplyNeverAsksWhetherTheClientIsConnected;
var
  LWriter: TConnectedProbeWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // Connected describes a stream, and the WebBroker writer opens one to answer
  // it: asked of a reply going out as a JSON document, it would turn that reply
  // into an event stream - or, below Delphi 13.1, raise. So a client that asked
  // for no stream is never asked about, however long its tool runs.
  ConfigureServer;
  LWriter := TConnectedProbeWriter.Create;
  LRef := LWriter;

  LRun := Start(LRef, CallBody('41', 'slow'), 'application/json');
  Assert.IsTrue(LRun.WaitFor, 'the request did not end');

  Assert.AreEqual(0, LWriter.Asked, 'Connected was asked of a document reply');
  Assert.AreEqual(200, LRun.Code);
  Assert.Contains(LRun.Content, 'slow answer');
end;

procedure TTransportCancellationTest.TestAStdioListenHoldsItsStream;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LFrames: TArray<string>;
begin
  // Stdio used to answer a listen at once; it now holds it like HTTP does: the
  // acknowledgement, what is published, and the result last on a clean end
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := StartStdio(LRef, ListenBody('7'));
  Assert.IsTrue(LWriter.WaitForFrames(1), 'the listen was not acknowledged');
  Assert.IsFalse(LRun.WaitFor(200), 'the stream did not stay open');

  FConfig.Subscriptions.ToolsChanged;
  Assert.IsTrue(LWriter.WaitForFrames(2), 'the notification did not arrive');

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor, 'the stream did not end');

  LFrames := LWriter.Frames;
  Assert.AreEqual(3, Integer(Length(LFrames)));
  Assert.Contains(LFrames[0], MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED);
  Assert.Contains(LFrames[1], MCP_NOTIFY_TOOLS_LIST_CHANGED);
  Assert.Contains(LFrames[2], '"result"');
  Assert.AreEqual(0, Registry.Count);
end;

procedure TTransportCancellationTest.TestTheEndOfStdinEndsAListenWithoutTheResult;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // What the reader does at EOF: close the connection's listens. The listen
  // ends abruptly - the client ended it - and leaves nothing behind in the
  // registry
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := StartStdio(LRef, ListenBody('7'));
  Assert.IsTrue(LWriter.WaitForFrames(1), 'the listen was not acknowledged');

  Assert.AreEqual(1, Registry.CloseScope('stdio-connection', False));
  Assert.IsTrue(LRun.WaitFor, 'the stream did not end');

  Assert.AreEqual(1, LWriter.FrameCount, 'something followed the acknowledgement');
  Assert.AreEqual(0, Registry.Count, 'the subscription was left behind');
  Assert.AreEqual(0, FInFlight.Count);
end;

procedure TTransportCancellationTest.TestTheEndOfStdinSparesARunningRequest;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // "echo request | server" reaches the end of stdin while the request runs,
  // and still reads stdout: closing the listens must not touch the request,
  // which answers as the sequential reader always let it
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := StartStdio(LRef, CallBody('41', 'slow'));
  Registry.CloseScope('stdio-connection', False);

  Assert.IsTrue(LRun.WaitFor, 'the request did not end');
  Assert.AreEqual(1, LWriter.FrameCount, 'the answer was not written');
  Assert.Contains(LWriter.Frames[0], 'slow answer');
end;

procedure TTransportCancellationTest.TestClosingAScopeSparesAnotherConnection;
var
  LFirst, LSecond: TRecordingStreamWriter;
  LFirstRef, LSecondRef: IMCPTransportWriter;
  LFirstRun, LSecondRun: TListenRun;
begin
  // One stdio connection reaching its end is not every connection's end
  ConfigureServer;
  LFirst := TRecordingStreamWriter.Create;
  LFirstRef := LFirst;
  LSecond := TRecordingStreamWriter.Create;
  LSecondRef := LSecond;

  LFirstRun := StartStdio(LFirstRef, ListenBody('7'));
  Assert.IsTrue(LFirst.WaitForFrames(1));

  LSecondRun := TListenRun.Create(FServer, LSecondRef, ListenBody('7'),
    TMediaType.TEXT_EVENT_STREAM, nil, TTransportProtocol.Stdio, 'another-connection');
  FRuns.Add(LSecondRun);
  Assert.IsTrue(LSecond.WaitForFrames(1));

  Assert.AreEqual(1, Registry.CloseScope('stdio-connection', False));
  Assert.IsTrue(LFirstRun.WaitFor, 'the closed connection''s stream did not end');
  Assert.IsFalse(LSecondRun.WaitFor(200), 'the other connection''s stream ended too');
  Assert.AreEqual(1, Registry.Count);
end;

procedure TTransportCancellationTest.TestAListenBeforeTheFirstRegistrationIsStillTold;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
begin
  // A server that registers late says so with explicit capabilities. A client
  // that listens before the first registration is acknowledged for the kind
  // that was promised, and told when it happens
  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig
    .Server
      .SetCapabilities(
        procedure (ACapabilities: TServerCapabilities)
        begin
          ACapabilities.Tools.ListChanged := True;
        end)
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
  .ApplyConfig;

  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  StartStdio(LRef, ListenBody('7'));
  Assert.IsTrue(LWriter.WaitForFrames(1), 'the listen was not acknowledged');
  Assert.Contains(LWriter.Frames[0], '"toolsListChanged":true');

  FConfig.Tools.RegisterClass(TCancellableTools);
  Assert.IsTrue(LWriter.WaitForFrames(2), 'the first registration was not announced');
  Assert.Contains(LWriter.Frames[1], MCP_NOTIFY_TOOLS_LIST_CHANGED);
end;

procedure TTransportCancellationTest.TestAStdioClientCannotListenTwiceUnderOneId;
var
  LFirst, LSecond: TRecordingStreamWriter;
  LFirstRef, LSecondRef: IMCPTransportWriter;
  LFirstRun, LSecondRun: TListenRun;
begin
  // One connection is one client, so its scope is the connection's: two streams
  // answering to one id are what the client could not untangle
  ConfigureServer;
  LFirst := TRecordingStreamWriter.Create;
  LFirstRef := LFirst;
  LSecond := TRecordingStreamWriter.Create;
  LSecondRef := LSecond;

  LFirstRun := StartStdio(LFirstRef, ListenBody('7'));
  Assert.IsTrue(LFirst.WaitForFrames(1), 'the first listen was not acknowledged');

  LSecondRun := StartStdio(LSecondRef, ListenBody('7'));
  Assert.IsTrue(LSecondRun.WaitFor, 'the second listen was held open');
  Assert.AreEqual(1, LSecond.FrameCount);
  Assert.Contains(LSecond.Frames[0], '"error"');

  Assert.AreEqual(1, Registry.Count, 'the first stream was disturbed');
  Assert.IsFalse(LFirstRun.WaitFor(100));
end;

procedure TTransportCancellationTest.TestTwoStdioConnectionsMayShareAnId;
var
  LFirst, LSecond: TRecordingStreamWriter;
  LFirstRef, LSecondRef: IMCPTransportWriter;
  LSecondRun: TListenRun;
begin
  // Two stdio servers in one process are two clients, each with its own scope
  ConfigureServer;
  LFirst := TRecordingStreamWriter.Create;
  LFirstRef := LFirst;
  LSecond := TRecordingStreamWriter.Create;
  LSecondRef := LSecond;

  StartStdio(LFirstRef, ListenBody('7'));
  Assert.IsTrue(LFirst.WaitForFrames(1));

  LSecondRun := TListenRun.Create(FServer, LSecondRef, ListenBody('7'),
    TMediaType.TEXT_EVENT_STREAM, nil, TTransportProtocol.Stdio, 'another-connection');
  FRuns.Add(LSecondRun);
  Assert.IsTrue(LSecond.WaitForFrames(1));
  Assert.Contains(LSecond.Frames[0], MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED);

  Assert.AreEqual(2, Registry.Count);
end;

procedure TTransportCancellationTest.TestOnDispatchingRunsOnceTheRequestIsFiled;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LCalls, LFiled: Integer;
begin
  // What the stdio reader waits for before reading the next line: from then on
  // a cancellation of this request finds it
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;
  LCalls := 0;
  LFiled := -1;

  LRun := StartStdio(LRef, CallBody('41', 'slow'),
    procedure
    begin
      Inc(LCalls);
      LFiled := FInFlight.Count;
    end);
  Assert.IsTrue(LRun.WaitFor, 'the request did not end');

  Assert.AreEqual(1, LCalls);
  Assert.AreEqual(1, LFiled, 'the request was not filed when it was dispatched');
end;

initialization
  GStarted := TEvent.Create(nil, True, False, '');
  TDUnitX.RegisterTestFixture(TMCPCancellationTest);
  TDUnitX.RegisterTestFixture(TMCPInFlightRequestsTest);
  TDUnitX.RegisterTestFixture(TTransportCancellationTest);

finalization
  GStarted.Free;

end.
