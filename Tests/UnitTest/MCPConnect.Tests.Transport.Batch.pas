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
///   Covers the *shape* of what a POST comes back with, which JSON-RPC 2.0 ties to
///   the shape of the payload rather than to the number of responses: a Request
///   object is answered with a Response object, a batch is answered with an Array
///   even when exactly one element of it is answerable.
/// </summary>
/// <remarks>
///   TMCPTransportHandler.HandlePOST builds its response list by hand instead of
///   going through TJRPCServer.ProcessMessages, so the Single carry-over that
///   TJRPCMessages.ToJson keys off has to be repeated there - and was not, which is
///   how a batch of one came to be answered with a bare object.
/// </remarks>
unit MCPConnect.Tests.Transport.Batch;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.Transport.Base,
  JRPC.Core,
  JRPC.Classes,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Notifications;

type
  /// <summary>A response writer that streams nothing: everything comes back
  ///   through the response converter.</summary>
  TNonStreamingWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>
  ///   Pushes a notification while it runs, the way a long-running tool reports
  ///   progress. Without a stream to put it on there is nowhere for it to go.
  /// </summary>
  TNotifyingTool = class(TObject)
  private
    [Context] FResponses: TMCPMessageQueue;
  public
    [McpTool('notify_then_answer', 'Enqueues a notification, then answers')]
    function NotifyThenAnswer: string;
  end;

  /// <summary>What a POST produced, copied out before the handler is destroyed.</summary>
  TPostOutcome = record
    Code: Integer;
    Content: string;
  end;

  [TestFixture]
  TTransportBatchTest = class(TObject)
  private
    FServer: TMCPServer;
    function Post(const ABody: string): TPostOutcome;

    /// <summary>The parsed reply, which the caller owns. Fails the test when the
    ///   body is not JSON at all.</summary>
    function ParseReply(const AOutcome: TPostOutcome): TJSONValue;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSingleRequest_IsAnsweredWithAnObject;
    [Test]
    procedure TestBatchOfOne_IsAnsweredWithAnArrayOfOne;
    [Test]
    procedure TestBatchOfTwo_IsAnsweredWithAnArrayOfTwo;
    [Test]
    procedure TestBatchWithANotification_IsStillAnsweredWithAnArray;
    [Test]
    procedure TestBatchOfOnlyNotifications_IsAnsweredWithNoContent;
    [Test]
    procedure TestSingleNotification_IsAnsweredWithNoContent;
    [Test]
    procedure TestMalformedJson_IsAnsweredWithAnObject;
    [Test]
    procedure TestEmptyBatch_IsAnsweredWithAnObject;

    [Test]
    procedure TestNotification_IsNotSplicedIntoASingleReply;
    [Test]
    procedure TestNotification_IsNotSplicedIntoABatchReply;
    [Test]
    procedure TestNotifications_DoNotInflateABatchReply;
  end;

implementation

const
  // server/discover needs nothing registered and is answerable on any server, so
  // what a test observes is the reply shape and nothing else.
  Discover = '{"jsonrpc":"2.0","id":%d,"method":"server/discover","params":{}}';
  // No id: a Notification, which per the spec is never answered.
  DiscoverNotification = '{"jsonrpc":"2.0","method":"server/discover","params":{}}';
  // Runs a tool that enqueues a server-to-client notification on its way to a result.
  CallNotifyingTool =
    '{"jsonrpc":"2.0","id":%d,"method":"tools/call",' +
    '"params":{"name":"notify_then_answer","arguments":{}}}';

{ TNotifyingTool }

function TNotifyingTool.NotifyThenAnswer: string;
begin
  FResponses.Enqueue(TMCPNotification.Progress('a-progress-token', 1, 2, 'working'));
  Result := 'done';
end;

{ TNonStreamingWriter }

procedure TNonStreamingWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests: the handler takes the buffered path.
end;

function TNonStreamingWriter.Connected: Boolean;
begin
  Result := False;
end;

function TNonStreamingWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TTransportBatchTest }

procedure TTransportBatchTest.Setup;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('batch-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Tools
      .RegisterClass(TNotifyingTool)
    .BackToMCP
  .ApplyConfig;
end;

procedure TTransportBatchTest.TearDown;
begin
  FServer.Free;
end;

function TTransportBatchTest.Post(const ABody: string): TPostOutcome;
var
  LHandler: TMCPTransportHandler;
  LOutcome: TPostOutcome;
begin
  LOutcome := Default(TPostOutcome);

  LHandler := TMCPTransportHandler.Create(FServer, TNonStreamingWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Content := ABody;
        ARequest.Accept := 'application/json';
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LOutcome.Code := AResponse.Code;
        LOutcome.Content := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LOutcome;
end;

function TTransportBatchTest.ParseReply(const AOutcome: TPostOutcome): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(AOutcome.Content);
  Assert.IsNotNull(Result, 'The reply must be JSON, got: ' + AOutcome.Content);
end;

procedure TTransportBatchTest.TestSingleRequest_IsAnsweredWithAnObject;
var
  LReply: TJSONValue;
begin
  LReply := ParseReply(Post(Format(Discover, [1])));
  try
    Assert.IsTrue(LReply is TJSONObject,
      'A single Request object is answered with a single Response object');
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestBatchOfOne_IsAnsweredWithAnArrayOfOne;
var
  LReply: TJSONValue;
begin
  // The regression: a one-element batch used to come back as a bare object, which a
  // client that sent an array has no reason to accept.
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONArray,
      'A batch is answered with an Array even when it holds exactly one Request');
    Assert.AreEqual(1, TJSONArray(LReply).Count);
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestBatchOfTwo_IsAnsweredWithAnArrayOfTwo;
var
  LReply: TJSONValue;
begin
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ',' + Format(Discover, [2]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONArray, 'A batch is answered with an Array');
    Assert.AreEqual(2, TJSONArray(LReply).Count);
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestBatchWithANotification_IsStillAnsweredWithAnArray;
var
  LReply: TJSONValue;
begin
  // Two elements in, one answerable: the number of responses is what used to decide
  // the shape, and it is the wrong thing to decide it by.
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ',' + DiscoverNotification + ']'));
  try
    Assert.IsTrue(LReply is TJSONArray,
      'A batch that produced one Response is still answered with an Array');
    Assert.AreEqual(1, TJSONArray(LReply).Count);
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestBatchOfOnlyNotifications_IsAnsweredWithNoContent;
var
  LOutcome: TPostOutcome;
begin
  // Nothing to answer: the spec says return nothing at all, not an empty array.
  LOutcome := Post('[' + DiscoverNotification + ']');

  Assert.AreEqual(202, LOutcome.Code);
  Assert.AreEqual('', LOutcome.Content);
end;

procedure TTransportBatchTest.TestSingleNotification_IsAnsweredWithNoContent;
var
  LOutcome: TPostOutcome;
begin
  LOutcome := Post(DiscoverNotification);

  Assert.AreEqual(202, LOutcome.Code);
  Assert.AreEqual('', LOutcome.Content);
end;

procedure TTransportBatchTest.TestMalformedJson_IsAnsweredWithAnObject;
var
  LReply: TJSONValue;
begin
  // A parse error is answered with a bare Response object whatever the payload
  // looked like - there is no batch to mirror once the payload did not parse.
  LReply := ParseReply(Post('[{"jsonrpc":"2.0",'));
  try
    Assert.IsTrue(LReply is TJSONObject, 'A parse error is a single Response object');
    Assert.IsNotNull(TJSONObject(LReply).GetValue('error'));
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestEmptyBatch_IsAnsweredWithAnObject;
var
  LReply: TJSONValue;
begin
  // "[]" has brackets but never produced a batch to reply to element by element.
  LReply := ParseReply(Post('[]'));
  try
    Assert.IsTrue(LReply is TJSONObject, 'An empty batch is Invalid Request, as an object');
    Assert.IsNotNull(TJSONObject(LReply).GetValue('error'));
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestNotification_IsNotSplicedIntoASingleReply;
var
  LReply: TJSONValue;
begin
  // The client did not ask for a stream, so the notification the tool enqueued has
  // nowhere to go. It used to be added to the response list regardless, which made
  // a plain Request come back as a two-element array holding a notification and a
  // response - and a notification is not a reply to anything.
  LReply := ParseReply(Post(Format(CallNotifyingTool, [1])));
  try
    Assert.IsTrue(LReply is TJSONObject,
      'A single Request is answered with a single Response, notifications or not');
    Assert.IsNotNull(TJSONObject(LReply).GetValue('result'));
    Assert.AreEqual(Int64(1), TJSONObject(LReply).GetValue<Int64>('id'));
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestNotification_IsNotSplicedIntoABatchReply;
var
  LReply: TJSONValue;
begin
  LReply := ParseReply(Post('[' + Format(CallNotifyingTool, [1]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONArray, 'A batch is answered with an Array');
    Assert.AreEqual(1, TJSONArray(LReply).Count,
      'One Request in, one Response out: the notification is not an element of the batch reply');
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestNotifications_DoNotInflateABatchReply;
var
  LReply: TJSONValue;
begin
  LReply := ParseReply(Post('[' + Format(CallNotifyingTool, [1]) + ',' +
    Format(CallNotifyingTool, [2]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONArray, 'A batch is answered with an Array');
    Assert.AreEqual(2, TJSONArray(LReply).Count,
      'Two Requests in, two Responses out - not four');
  finally
    LReply.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTransportBatchTest);

end.
