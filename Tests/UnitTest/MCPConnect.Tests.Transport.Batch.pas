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
///   The batch policy of the MCP transport, and the reply shape it leaves behind.
///   2026-07-28 requires the body of a request to be a single JSON-RPC request or
///   notification, so a top-level array is refused outright - and with only one
///   message ever reaching dispatch, a Response object is the only reply shape
///   there is.
/// </summary>
/// <remarks>
///   The refusal is the MCP transport's, not the JSON-RPC layer's: Libs/JRPC still
///   reads and answers batches for its own callers, and its own suite still covers
///   them. What is asserted here is that MCPConnect declines to.
///
///   The single-message tests are the older half of this fixture and stay: they were
///   the regression net for the reply shape (TMCPTransportHandler.HandlePOST builds
///   its response list by hand rather than going through TJRPCServer.ProcessMessages)
///   and they still are.
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
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications;

type
  /// <summary>A response writer that streams nothing: everything comes back
  ///   through the response converter.</summary>
  TNonStreamingWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
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
    procedure TestSingleNotification_IsAnsweredWithNoContent;
    [Test]
    procedure TestMalformedJson_IsAnsweredWithAnObject;
    [Test]
    procedure TestNotification_IsNotSplicedIntoASingleReply;

    [Test]
    procedure TestBatchOfOne_IsRefused;
    [Test]
    procedure TestBatchOfTwo_IsRefused;
    [Test]
    procedure TestBatchOfOnlyNotifications_IsRefusedRatherThanAccepted;
    [Test]
    procedure TestEmptyBatch_IsRefusedByTheSameRule;
    [Test]
    procedure TestRefusalIsInvalidRequest;
    [Test]
    procedure TestRefusalIsAnObjectWithANullId;
    [Test]
    procedure TestRefusalNamesTheRule;
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

procedure TNonStreamingWriter.Write(const AValue: string);
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
    .Security
      // These tests send no request-metadata headers: a batch is refused before
      // anything could mirror one, and the single-message tests are about the
      // reply shape rather than the header contract.
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
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

procedure TTransportBatchTest.TestBatchOfOne_IsRefused;
var
  LOutcome: TPostOutcome;
begin
  // One element is still an array, and an array is still not a single message.
  // Nothing about the count makes a batch acceptable.
  LOutcome := Post('[' + Format(Discover, [1]) + ']');

  Assert.AreEqual(400, LOutcome.Code, LOutcome.Content);
end;

procedure TTransportBatchTest.TestBatchOfTwo_IsRefused;
var
  LOutcome: TPostOutcome;
begin
  LOutcome := Post('[' + Format(Discover, [1]) + ',' + Format(Discover, [2]) + ']');

  Assert.AreEqual(400, LOutcome.Code, LOutcome.Content);
end;

procedure TTransportBatchTest.TestBatchOfOnlyNotifications_IsRefusedRatherThanAccepted;
var
  LOutcome: TPostOutcome;
begin
  // The one case where refusing and accepting look alike from a distance: a batch
  // of notifications would have been answered 202 with no body. It is refused
  // instead, because what is wrong with it is the envelope, before anything asks
  // whether the messages inside want a reply.
  LOutcome := Post('[' + DiscoverNotification + ']');

  Assert.AreEqual(400, LOutcome.Code, LOutcome.Content);
  Assert.AreNotEqual('', LOutcome.Content, 'a refusal says why; an acceptance says nothing');
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

procedure TTransportBatchTest.TestEmptyBatch_IsRefusedByTheSameRule;
var
  LOutcome: TPostOutcome;
begin
  // "[]" used to reach the JSON-RPC layer and come back as its Invalid Request.
  // It is now refused by the transport, one step earlier and by the same rule as
  // a full batch - one rule about the envelope rather than two answers that
  // happen to share a code.
  LOutcome := Post('[]');

  Assert.AreEqual(400, LOutcome.Code, LOutcome.Content);
  Assert.Contains(LOutcome.Content, 'single', 'refused for its shape, not for being empty');
end;

procedure TTransportBatchTest.TestRefusalIsInvalidRequest;
var
  LReply: TJSONValue;
begin
  // -32600: the specification reserves no code for this, and "the JSON sent is
  // not a valid Request object" is what JSON-RPC 2.0 already means by it
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONObject);
    Assert.AreEqual(-32600,
      TJSONObject(LReply).GetValue('error').GetValue<Integer>('code'));
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestRefusalIsAnObjectWithANullId;
var
  LReply: TJSONValue;
begin
  // There is no id to answer on - the server declined to look inside the array -
  // and JSON-RPC 2.0 says such an error carries a null id. An object, not an
  // array: the reply does not mirror the shape of a payload that was refused.
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ']'));
  try
    Assert.IsTrue(LReply is TJSONObject, 'a refused batch is not answered element by element');
    Assert.IsTrue(TJSONObject(LReply).GetValue('id') is TJSONNull);
  finally
    LReply.Free;
  end;
end;

procedure TTransportBatchTest.TestRefusalNamesTheRule;
var
  LReply: TJSONValue;
begin
  // A client that sends a batch has a bug in it, and -32600 alone does not say
  // which: the message is what tells whoever reads the log what to change
  LReply := ParseReply(Post('[' + Format(Discover, [1]) + ']'));
  try
    Assert.Contains(
      TJSONObject(LReply).GetValue('error').GetValue<string>('message'),
      'single JSON-RPC request or notification');
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

initialization
  TDUnitX.RegisterTestFixture(TTransportBatchTest);

end.
