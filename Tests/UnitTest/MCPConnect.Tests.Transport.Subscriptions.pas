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
///   The "subscriptions/listen" stream over Streamable HTTP, driven through a
///   real TMCPTransportHandler: what is written on it, in which order, and how
///   it ends.
/// </summary>
/// <remarks>
///   The pump blocks for as long as the subscription is live, so every listen
///   runs on a thread of its own (TListenRun) and the test acts on it from
///   outside - publishing, closing, disconnecting - then waits for it to end
///   and reads the frames the writer recorded.
/// </remarks>
unit MCPConnect.Tests.Transport.Subscriptions;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.JSON,
  System.Generics.Collections,
  DUnitX.TestFramework,

  JRPC.Core,
  MCPConnect.JRPC.Middleware,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Subscriptions,
  MCPConnect.Transport.Base;

type
  /// <summary>
  ///   A streaming writer that records every frame, and that a test can
  ///   disconnect, make fail, or hold inside a write.
  /// </summary>
  /// <remarks>
  ///   Written to from the pump's thread and read from the test's, so
  ///   everything goes through the lock.
  /// </remarks>
  TRecordingStreamWriter = class(TMCPTransportWriter)
  private
    FLock: TCriticalSection;
    FFrames: TStringList;
    FComments: Integer;
    FConnected: Boolean;
    FFailWrites: Boolean;
    FStreams: Boolean;
    FGate: TEvent;
    FWritten: TEvent;
    procedure SetFailWrites(const AValue: Boolean);
    function GetComments: Integer;
  public
    constructor Create(AStreams: Boolean = True);
    destructor Destroy; override;

    { IMCPTransportWriter }
    procedure Write(const AValue: string); override;
    function Connected: Boolean; override;
    function SupportsStreaming: Boolean; override;
    procedure WriteComment(const AValue: string); override;

    /// <summary>
    ///   A copy of the frames written so far.
    /// </summary>
    function Frames: TArray<string>;
    function FrameCount: Integer;

    /// <summary>
    ///   Waits until at least ACount frames were written.
    /// </summary>
    function WaitForFrames(ACount: Integer; ATimeoutMs: Integer = 5000): Boolean;
    function WaitForComments(ACount: Integer; ATimeoutMs: Integer = 5000): Boolean;

    /// <summary>
    ///   Closes the gate: the next write blocks until Release. What makes a
    ///   client that stopped reading, for the overflow test.
    /// </summary>
    procedure Hold;
    procedure Release;

    /// <summary>
    ///   From now on the socket reports itself closed.
    /// </summary>
    procedure Disconnect;

    property FailWrites: Boolean write SetFailWrites;
    property Comments: Integer read GetComments;
  end;

  /// <summary>
  ///   One request through a TMCPTransportHandler, on a thread of its own.
  /// </summary>
  TListenRun = class
  private
    FThread: TThread;
    FDone: TEvent;
    FCode: Integer;
    FContent: string;
  public
    constructor Create(AServer: TMCPServer; AWriter: IMCPTransportWriter;
      const ABody, AAccept: string);
    destructor Destroy; override;

    /// <summary>
    ///   Waits for the request to end.
    /// </summary>
    function WaitFor(ATimeoutMs: Integer = 5000): Boolean;

    /// <summary>
    ///   The status the response converter was handed.
    /// </summary>
    property Code: Integer read FCode;

    /// <summary>
    ///   The document body, empty for a streamed reply.
    /// </summary>
    property Content: string read FContent;
  end;

  /// <summary>
  ///   Emits a progress notification for every request, before the request is
  ///   handled - which, for a listen, puts it in front of the acknowledgement
  ///   in the response queue.
  /// </summary>
  TProgressEmittingMiddleware = class(TMiddleware, IRequestMiddleware)
  public
    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  [TestFixture]
  TListenStreamTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;
    FRuns: TList<TListenRun>;

    procedure ConfigureServer(AWithProgress: Boolean = False);
    function Registry: TMCPSubscriptionRegistry;

    function ListenBody(const AId, ANotifications: string): string;

    /// <summary>
    ///   Starts a listen on AWriter. The run is owned by the fixture, which
    ///   ends every stream still open at TearDown.
    /// </summary>
    function Listen(AWriter: IMCPTransportWriter; const AId: string = '7';
      const ANotifications: string = '"toolsListChanged":true';
      const AAccept: string = 'application/json, text/event-stream'): TListenRun;

    /// <summary>
    ///   Waits until the registry holds ACount subscriptions.
    /// </summary>
    function WaitForCount(ACount: Integer; ATimeoutMs: Integer = 5000): Boolean;

    class function CountContaining(const AFrames: TArray<string>; const AText: string): Integer; static;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestTheAcknowledgementIsTheFirstFrame;
    [Test]
    procedure TestTheAcknowledgementCarriesTheSubscriptionId;
    [Test]
    procedure TestTheResultIsHeldWhileLiveAndIsTheLastFrameOnAGracefulClose;
    [Test]
    procedure TestTheResultSaysWhoAnswered;
    [Test]
    procedure TestEveryFrameCarriesTheId;
    [Test]
    procedure TestAPublishedNotificationArrives;
    [Test]
    procedure TestAnUnrequestedTypeIsNotSent;
    [Test]
    procedure TestADisconnectClosesTheEntryWithoutAResultOrA500;
    [Test]
    procedure TestAFailedWriteIsADisconnect;
    [Test]
    procedure TestCloseAllWritesTheResultAndStops;
    [Test]
    procedure TestAListenWhoseAcceptExcludesSSEIs406;
    [Test]
    procedure TestAListenOnANonStreamingTransportIs400;
    [Test]
    procedure TestAListenOverTheCapIs400;
    [Test]
    procedure TestTheKeepAliveIsSent;
    [Test]
    procedure TestProgressNeverReachesTheStream;
    [Test]
    procedure TestTwoClientsUsingTheSameIdDoNotCollide;
    [Test]
    procedure TestAnOverflowEndsTheStreamAbruptly;
  end;

implementation

uses
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.Transport.MediaType;

type
  TListenFeatures = class
  public
    [McpTool('ping', 'Answers pong')]
    function Ping: string;

    [McpPrompt('greet', 'Greet', 'Greets')]
    function Greet: TPromptMessages;
  end;

function TListenFeatures.Ping: string;
begin
  Result := 'pong';
end;

function TListenFeatures.Greet: TPromptMessages;
begin
  Result := TPromptMessages.Create;
end;

{ TRecordingStreamWriter }

constructor TRecordingStreamWriter.Create(AStreams: Boolean);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FFrames := TStringList.Create;
  FConnected := True;
  FFailWrites := False;
  FStreams := AStreams;
  FComments := 0;
  // Manual reset, open: Hold closes it
  FGate := TEvent.Create(nil, True, True, '');
  FWritten := TEvent.Create(nil, False, False, '');
end;

destructor TRecordingStreamWriter.Destroy;
begin
  FWritten.Free;
  FGate.Free;
  FFrames.Free;
  FLock.Free;
  inherited;
end;

procedure TRecordingStreamWriter.Write(const AValue: string);
begin
  FGate.WaitFor(10000);

  FLock.Enter;
  try
    if FFailWrites then
      raise EInOutError.Create('the socket is gone');
    FFrames.Add(AValue);
  finally
    FLock.Leave;
  end;
  FWritten.SetEvent;
end;

procedure TRecordingStreamWriter.WriteComment(const AValue: string);
begin
  FLock.Enter;
  try
    if FFailWrites then
      raise EInOutError.Create('the socket is gone');
    Inc(FComments);
  finally
    FLock.Leave;
  end;
  FWritten.SetEvent;
end;

function TRecordingStreamWriter.Connected: Boolean;
begin
  FLock.Enter;
  try
    Result := FConnected;
  finally
    FLock.Leave;
  end;
end;

function TRecordingStreamWriter.SupportsStreaming: Boolean;
begin
  Result := FStreams;
end;

procedure TRecordingStreamWriter.Disconnect;
begin
  FLock.Enter;
  try
    FConnected := False;
  finally
    FLock.Leave;
  end;
end;

procedure TRecordingStreamWriter.SetFailWrites(const AValue: Boolean);
begin
  FLock.Enter;
  try
    FFailWrites := AValue;
  finally
    FLock.Leave;
  end;
end;

function TRecordingStreamWriter.GetComments: Integer;
begin
  FLock.Enter;
  try
    Result := FComments;
  finally
    FLock.Leave;
  end;
end;

function TRecordingStreamWriter.Frames: TArray<string>;
begin
  FLock.Enter;
  try
    Result := FFrames.ToStringArray;
  finally
    FLock.Leave;
  end;
end;

function TRecordingStreamWriter.FrameCount: Integer;
begin
  FLock.Enter;
  try
    Result := FFrames.Count;
  finally
    FLock.Leave;
  end;
end;

function TRecordingStreamWriter.WaitForFrames(ACount, ATimeoutMs: Integer): Boolean;
var
  LDeadline: TDateTime;
begin
  LDeadline := Now + ATimeoutMs / MSecsPerDay;
  while FrameCount < ACount do
  begin
    if Now > LDeadline then
      Exit(False);
    FWritten.WaitFor(50);
  end;
  Result := True;
end;

function TRecordingStreamWriter.WaitForComments(ACount, ATimeoutMs: Integer): Boolean;
var
  LDeadline: TDateTime;
begin
  LDeadline := Now + ATimeoutMs / MSecsPerDay;
  while Comments < ACount do
  begin
    if Now > LDeadline then
      Exit(False);
    FWritten.WaitFor(50);
  end;
  Result := True;
end;

procedure TRecordingStreamWriter.Hold;
begin
  FGate.ResetEvent;
end;

procedure TRecordingStreamWriter.Release;
begin
  FGate.SetEvent;
end;

{ TListenRun }

constructor TListenRun.Create(AServer: TMCPServer; AWriter: IMCPTransportWriter;
  const ABody, AAccept: string);
begin
  inherited Create;
  FDone := TEvent.Create(nil, True, False, '');
  FCode := 0;

  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      LHandler: IMCPTransportHandler;
    begin
      try
        LHandler := TMCPTransportHandler.Create(AServer, AWriter);
        LHandler.ProcessRequest(
          procedure (ARequest: TMCPTransportRequest)
          begin
            ARequest.Url := '/';
            ARequest.Command := 'POST';
            ARequest.Protocol := TTransportProtocol.StreamableHTTP;
            ARequest.Accept := AAccept;
            ARequest.Content := ABody;
          end,
          procedure (AResponse: TMCPTransportResponse)
          begin
            FCode := AResponse.Code;
            FContent := AResponse.Content;
          end);
      finally
        LHandler := nil;
        FDone.SetEvent;
      end;
    end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

destructor TListenRun.Destroy;
begin
  FThread.WaitFor;
  FThread.Free;
  FDone.Free;
  inherited;
end;

function TListenRun.WaitFor(ATimeoutMs: Integer): Boolean;
begin
  Result := FDone.WaitFor(ATimeoutMs) = wrSignaled;
end;

{ TProgressEmittingMiddleware }

procedure TProgressEmittingMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
begin
  AContext.Emit(TMCPNotification.Progress('a-token', 1, 2));
  AChain.Next(AContext);
end;

{ TListenStreamTest }

procedure TListenStreamTest.Setup;
begin
  FRuns := TList<TListenRun>.Create;
end;

procedure TListenStreamTest.TearDown;
begin
  // Whatever a failing test left open is ended here, so that no pump outlives
  // the server it is reading from
  if Assigned(FConfig) then
    Registry.CloseAll(False);

  for var LRun in FRuns do
    LRun.Free;
  FRuns.Free;

  FConfig := nil;
  FServer.Free;
  FServer := nil;
end;

procedure TListenStreamTest.ConfigureServer(AWithProgress: Boolean);
begin
  FServer := TMCPServer.Create(nil);

  if AWithProgress then
    FServer.Middleware.Add(TProgressEmittingMiddleware);

  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig
    .Server
      .SetName('listen-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      // Not what these tests are about, and the requests carry neither
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TListenFeatures)
    .BackToMCP
    .Prompts
      .RegisterClass(TListenFeatures)
    .BackToMCP
  .ApplyConfig;

  // Short, so the keep-alive test does not have to wait two seconds
  FConfig.Subscriptions.KeepAliveInterval := 50;
end;

function TListenStreamTest.Registry: TMCPSubscriptionRegistry;
begin
  Result := FConfig.Subscriptions.Registry;
end;

function TListenStreamTest.ListenBody(const AId, ANotifications: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","id":%s,"method":"subscriptions/listen",' +
    '"params":{"notifications":{%s}}}', [AId, ANotifications]);
end;

function TListenStreamTest.Listen(AWriter: IMCPTransportWriter; const AId,
  ANotifications, AAccept: string): TListenRun;
begin
  Result := TListenRun.Create(FServer, AWriter, ListenBody(AId, ANotifications), AAccept);
  FRuns.Add(Result);
end;

function TListenStreamTest.WaitForCount(ACount, ATimeoutMs: Integer): Boolean;
var
  LDeadline: TDateTime;
begin
  LDeadline := Now + ATimeoutMs / MSecsPerDay;
  while Registry.Count <> ACount do
  begin
    if Now > LDeadline then
      Exit(False);
    TThread.Sleep(10);
  end;
  Result := True;
end;

class function TListenStreamTest.CountContaining(const AFrames: TArray<string>;
  const AText: string): Integer;
begin
  Result := 0;
  for var LFrame in AFrames do
    if LFrame.Contains(AText) then
      Inc(Result);
end;

procedure TListenStreamTest.TestTheAcknowledgementIsTheFirstFrame;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(LWriter.WaitForFrames(1), 'nothing was written on the stream');

  Assert.Contains(LWriter.Frames[0], MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED);

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor, 'the stream did not end');
end;

procedure TListenStreamTest.TestTheAcknowledgementCarriesTheSubscriptionId;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  // A string id, so the assertion also says the JSON type survived
  LRun := Listen(LRef, '"sub-a"');
  Assert.IsTrue(LWriter.WaitForFrames(1));

  Assert.Contains(LWriter.Frames[0], '"' + MCP_META_SUBSCRIPTION_ID + '":"sub-a"');

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);
end;

procedure TListenStreamTest.TestTheResultIsHeldWhileLiveAndIsTheLastFrameOnAGracefulClose;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LFrames: TArray<string>;
begin
  // The regression net for the hold rule: written at once, the result closes
  // the exchange right after the acknowledgement, and a client reads that as a
  // clean end and subscribes again - for ever.
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(LWriter.WaitForFrames(1));
  Assert.IsTrue(WaitForCount(1), 'the subscription was never registered');

  // Long enough for a result that was not held to have been written
  TThread.Sleep(200);
  Assert.IsFalse(LRun.WaitFor(0), 'the stream ended while the subscription was live');
  Assert.AreEqual(0, CountContaining(LWriter.Frames, '"result"'),
    'the result was written while the subscription was live');

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);

  LFrames := LWriter.Frames;
  Assert.AreEqual(1, CountContaining(LFrames, '"result"'), 'the result is owed exactly once');
  Assert.Contains(LFrames[High(LFrames)], '"result"', 'and it is the last frame');
  Assert.Contains(LFrames[High(LFrames)], '"id":7');
end;

procedure TListenStreamTest.TestTheResultSaysWhoAnswered;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LFrames: TArray<string>;
begin
  // The result carries a "_meta" of its own (the subscription id), which must
  // not lose the serverInfo every result carries
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));
  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);

  LFrames := LWriter.Frames;
  Assert.Contains(LFrames[High(LFrames)], 'io.modelcontextprotocol/serverInfo');
  Assert.Contains(LFrames[High(LFrames)], 'listen-test');
  Assert.Contains(LFrames[High(LFrames)], '"' + MCP_META_SUBSCRIPTION_ID + '":7');
end;

procedure TListenStreamTest.TestEveryFrameCarriesTheId;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LFrames: TArray<string>;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));

  FConfig.Subscriptions.ToolsChanged;
  Assert.IsTrue(LWriter.WaitForFrames(2));

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);

  LFrames := LWriter.Frames;
  Assert.AreEqual(3, Length(LFrames), 'acknowledgement, notification, result');
  for var LFrame in LFrames do
    Assert.Contains(LFrame, '"' + MCP_META_SUBSCRIPTION_ID + '":7');
end;

procedure TListenStreamTest.TestAPublishedNotificationArrives;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));
  Assert.IsTrue(LWriter.WaitForFrames(1));

  FConfig.Subscriptions.ToolsChanged;

  Assert.IsTrue(LWriter.WaitForFrames(2), 'the notification never reached the stream');
  Assert.Contains(LWriter.Frames[1], MCP_NOTIFY_TOOLS_LIST_CHANGED);

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);
end;

procedure TListenStreamTest.TestAnUnrequestedTypeIsNotSent;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  // Asked for tools only
  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));

  FConfig.Subscriptions.PromptsChanged;
  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);

  Assert.AreEqual(0, CountContaining(LWriter.Frames, MCP_NOTIFY_PROMPTS_LIST_CHANGED));
end;

procedure TListenStreamTest.TestADisconnectClosesTheEntryWithoutAResultOrA500;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));

  LWriter.Disconnect;
  Assert.IsTrue(LRun.WaitFor, 'a disconnect did not end the stream');

  Assert.AreEqual(0, Registry.Count, 'an abrupt end must leave no entry behind');
  Assert.AreEqual(0, CountContaining(LWriter.Frames, '"result"'),
    'an abrupt end owes no result');
  Assert.AreNotEqual(500, LRun.Code);
end;

procedure TListenStreamTest.TestAFailedWriteIsADisconnect;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));
  Assert.IsTrue(LWriter.WaitForFrames(1));

  // A half-open peer: the socket still says connected, the write fails
  LWriter.FailWrites := True;
  FConfig.Subscriptions.ToolsChanged;

  Assert.IsTrue(LRun.WaitFor, 'a failed write did not end the stream');
  Assert.AreEqual(0, Registry.Count);
  Assert.AreNotEqual(500, LRun.Code, 'a dead client is a disconnect, not a server error');
  Assert.AreEqual('', LRun.Content, 'nothing is written down the dead connection');
end;

procedure TListenStreamTest.TestCloseAllWritesTheResultAndStops;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
  LCount: Integer;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));

  Assert.AreEqual(1, Registry.CloseAll(True));
  Assert.IsTrue(LRun.WaitFor, 'CloseAll did not end the stream');
  LCount := LWriter.FrameCount;

  // Nothing after the result: a publish now reaches no one
  FConfig.Subscriptions.ToolsChanged;
  TThread.Sleep(100);
  Assert.AreEqual(LCount, LWriter.FrameCount);
  Assert.AreEqual(0, Registry.Count);
end;

procedure TListenStreamTest.TestAListenWhoseAcceptExcludesSSEIs406;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef, '7', '"toolsListChanged":true', TMediaType.APPLICATION_JSON);
  Assert.IsTrue(LRun.WaitFor, 'a refused listen must answer at once');

  Assert.AreEqual(406, LRun.Code);
  Assert.Contains(LRun.Content, '-32600');
  Assert.Contains(LRun.Content, '"id":7');
  Assert.AreEqual(0, LWriter.FrameCount, 'nothing was acknowledged');
  Assert.AreEqual(0, Registry.Count);
end;

procedure TListenStreamTest.TestAListenOnANonStreamingTransportIs400;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;

  // WebBroker before 13.1: the client asked for a stream, the deployment has none
  LWriter := TRecordingStreamWriter.Create(False);
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(LRun.WaitFor, 'a refused listen must answer at once');

  Assert.AreEqual(400, LRun.Code);
  Assert.Contains(LRun.Content, '-32600');
  Assert.AreEqual(0, LWriter.FrameCount);
  Assert.AreEqual(0, Registry.Count);
end;

procedure TListenStreamTest.TestAListenOverTheCapIs400;
var
  LFirst, LSecond: TRecordingStreamWriter;
  LFirstRef, LSecondRef: IMCPTransportWriter;
  LFirstRun, LSecondRun: TListenRun;
begin
  ConfigureServer;
  FConfig.Subscriptions.MaxSubscriptions := 1;

  LFirst := TRecordingStreamWriter.Create;
  LFirstRef := LFirst;
  LFirstRun := Listen(LFirstRef, '1');
  Assert.IsTrue(WaitForCount(1));

  LSecond := TRecordingStreamWriter.Create;
  LSecondRef := LSecond;
  LSecondRun := Listen(LSecondRef, '2');
  Assert.IsTrue(LSecondRun.WaitFor, 'a listen over the cap must answer at once');

  Assert.AreEqual(400, LSecondRun.Code);
  Assert.Contains(LSecondRun.Content, '-32600');
  Assert.AreEqual(1, Registry.Count, 'the first stream is unaffected');

  Registry.CloseAll(True);
  Assert.IsTrue(LFirstRun.WaitFor);
end;

procedure TListenStreamTest.TestTheKeepAliveIsSent;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));

  Assert.IsTrue(LWriter.WaitForComments(2), 'a silent stream was never kept alive');

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);
end;

procedure TListenStreamTest.TestProgressNeverReachesTheStream;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  // With meta validation off, nothing has read a token, so the progress gate
  // cannot speak. The listen rule still does: a listen stream carries its
  // acknowledgement and its subscription's notifications, nothing else.
  ConfigureServer(True);
  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));
  Assert.IsTrue(LWriter.WaitForFrames(1));

  Registry.CloseAll(True);
  Assert.IsTrue(LRun.WaitFor);

  Assert.AreEqual(0, CountContaining(LWriter.Frames, MCP_NOTIFY_PROGRESS));
  Assert.Contains(LWriter.Frames[0], MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED,
    'and the acknowledgement is still first');
end;

procedure TListenStreamTest.TestTwoClientsUsingTheSameIdDoNotCollide;
var
  LTools, LPrompts: TRecordingStreamWriter;
  LToolsRef, LPromptsRef: IMCPTransportWriter;
  LToolsRun, LPromptsRun: TListenRun;
begin
  // A JSON-RPC id is unique only within one client, and with no sessions two
  // clients are two streams with nothing else to tell them apart
  ConfigureServer;

  LTools := TRecordingStreamWriter.Create;
  LToolsRef := LTools;
  LToolsRun := Listen(LToolsRef, '1', '"toolsListChanged":true');

  LPrompts := TRecordingStreamWriter.Create;
  LPromptsRef := LPrompts;
  LPromptsRun := Listen(LPromptsRef, '1', '"promptsListChanged":true');

  Assert.IsTrue(WaitForCount(2), 'the second listen with the same id was not opened');

  FConfig.Subscriptions.ToolsChanged;
  Assert.IsTrue(LTools.WaitForFrames(2));

  Registry.CloseAll(True);
  Assert.IsTrue(LToolsRun.WaitFor);
  Assert.IsTrue(LPromptsRun.WaitFor);

  Assert.AreEqual(1, CountContaining(LTools.Frames, MCP_NOTIFY_TOOLS_LIST_CHANGED));
  Assert.AreEqual(0, CountContaining(LPrompts.Frames, MCP_NOTIFY_TOOLS_LIST_CHANGED),
    'one stream received the other''s notification');
end;

procedure TListenStreamTest.TestAnOverflowEndsTheStreamAbruptly;
var
  LWriter: TRecordingStreamWriter;
  LRef: IMCPTransportWriter;
  LRun: TListenRun;
begin
  ConfigureServer;
  FConfig.Subscriptions.QueueCap := 2;

  LWriter := TRecordingStreamWriter.Create;
  LRef := LWriter;

  LRun := Listen(LRef);
  Assert.IsTrue(WaitForCount(1));
  Assert.IsTrue(LWriter.WaitForFrames(1));

  // A client that stopped reading: the pump is stuck inside a write while
  // the changes pile up past the cap
  LWriter.Hold;
  for var I := 1 to 6 do
    FConfig.Subscriptions.ToolsChanged;
  LWriter.Release;

  Assert.IsTrue(LRun.WaitFor, 'an overflow did not end the stream');

  // No result: a client told the subscription ended cleanly would not know it
  // missed anything. An abrupt end is what makes it reconnect and re-read.
  Assert.AreEqual(0, CountContaining(LWriter.Frames, '"result"'));
  Assert.AreEqual(0, Registry.Count);
end;

initialization
  TDUnitX.RegisterTestFixture(TListenStreamTest);

end.
