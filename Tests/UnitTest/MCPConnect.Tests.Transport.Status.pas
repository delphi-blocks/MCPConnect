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
///   The HTTP status a Streamable HTTP reply carries, which MCP 2026-07-28 ties
///   to what went wrong rather than to the JSON-RPC code alone: 400 for the
///   errors the revision defines for itself, 404 with a -32601 body for a
///   method the server does not implement, 202 for an accepted notification,
///   405 for the verbs that went with the session era, and 200 for everything
///   else - an ordinary JSON-RPC error included.
/// </summary>
unit MCPConnect.Tests.Transport.Status;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentStatusWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>
  ///   A response writer that can stream, and keeps what it was given. What a
  ///   payload answered 202 must never reach.
  /// </summary>
  TStreamingStatusWriter = class(TInterfacedObject, IMCPTransportWriter)
  private
    FFrames: TStrings;
  public
    constructor Create(AFrames: TStrings);

    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a request answered, as far as these tests care.</summary>
  TStatusAnswer = record
    Code: Integer;
    Content: string;

    /// <summary>Whether the reply opened a stream, and what went out on it.</summary>
    StreamOpened: Boolean;
    Frames: string;

    /// <summary>The JSON-RPC error code of the reply, or 0 when it carries none.</summary>
    function ErrorCode: Integer;
  end;

  [TestFixture]
  TTransportStatusTest = class(TObject)
  private const
    Meta =
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/clientCapabilities":{}}';
  private
    FServer: TMCPServer;

    /// <summary>A request with the _meta a conforming client sends.</summary>
    function Body(const AMethod: string; const AParams: string = ''): string;

    /// <summary>
    ///   Sends ABody. AWantsStream models the whole of the streaming case: a
    ///   client whose Accept includes text/event-stream, talking to a transport
    ///   that can give it one.
    /// </summary>
    function Send(const ABody: string; const ACommand: string = 'POST';
      AWantsStream: Boolean = False): TStatusAnswer;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAnsweredRequestIsOk();
    [Test]
    procedure TestAcceptedNotificationIsAcceptedWithNoBody();
    [Test]
    procedure TestANotificationIsAcceptedEvenWhenAStreamWasOffered();
    [Test]
    procedure TestANotificationThatAcceptsAStreamOpensNone();
    [Test]
    procedure TestABatchOfNotificationsIsRefusedNotAccepted();
    [Test]
    procedure TestARequestThatAcceptsAStreamStillStreams();
    [Test]
    procedure TestARefusedPayloadIsNotStreamed();
    [Test]
    procedure TestAMalformedPayloadIsAnsweredNotAccepted();

    [Test]
    procedure TestUnknownNamespaceIsNotFound();
    [Test]
    procedure TestUnknownMethodOfAKnownNamespaceIsNotFound();
    [Test]
    procedure TestNotFoundStillCarriesTheJsonRpcError();

    [Test]
    procedure TestMalformedMetaIsBadRequest();
    [Test]
    procedure TestUnsupportedVersionIsBadRequest();

    [Test]
    procedure TestAnOrdinaryErrorKeepsTheOrdinaryStatus();
    [Test]
    procedure TestABatchIsRefusedWholeNotElementByElement();

    [Test]
    procedure TestGetIsMethodNotAllowed();
    [Test]
    procedure TestDeleteIsMethodNotAllowed();
  end;

implementation

uses
  JRPC.Core,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Errors;

type
  TStatusTool = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;
  end;

function TStatusTool.Ping: string;
begin
  Result := 'pong';
end;

{ TSilentStatusWriter }

procedure TSilentStatusWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests: a stream sends its headers before the
  // handler runs, so a status decided later could never reach it anyway.
end;

function TSilentStatusWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentStatusWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TStreamingStatusWriter }

constructor TStreamingStatusWriter.Create(AFrames: TStrings);
begin
  inherited Create;
  FFrames := AFrames;
end;

procedure TStreamingStatusWriter.Write(const AValue: string);
begin
  FFrames.Add(AValue);
end;

function TStreamingStatusWriter.Connected: Boolean;
begin
  Result := True;
end;

function TStreamingStatusWriter.SupportsStreaming: Boolean;
begin
  Result := True;
end;

{ TStatusAnswer }

function TStatusAnswer.ErrorCode: Integer;
var
  LValue: TJSONValue;
  LError: TJSONValue;
begin
  Result := 0;

  LValue := TJSONObject.ParseJSONValue(Content);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LError := TJSONObject(LValue).GetValue('error');
    if LError is TJSONObject then
      Result := TJSONObject(LError).GetValue<Integer>('code', 0);
  finally
    LValue.Free;
  end;
end;

{ TTransportStatusTest }

procedure TTransportStatusTest.Setup;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('status-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      // The headers are checked by their own middleware and their own tests;
      // these requests carry none, and what is under test is the status.
      .SetHeaderValidation(TMCPValidationLevel.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TStatusTool)
    .BackToMCP
  .ApplyConfig;
end;

procedure TTransportStatusTest.TearDown;
begin
  FServer.Free;
end;

function TTransportStatusTest.Body(const AMethod, AParams: string): string;
begin
  Result := Format('{"jsonrpc":"2.0","id":3,"method":"%s","params":{%s%s}}',
    [AMethod, AParams, Meta]);
end;

function TTransportStatusTest.Send(const ABody, ACommand: string;
  AWantsStream: Boolean): TStatusAnswer;
var
  // The interface and not the class: SendResponseHeadersProc is declared on
  // IMCPTransportHandler, and the reference counts the handler for us
  LHandler: IMCPTransportHandler;
  LAnswer: TStatusAnswer;
  LWriter: IMCPTransportWriter;
  LFrames: TStringList;
  LAccept: string;
  LOpened: Boolean;
begin
  LAnswer := Default(TStatusAnswer);
  LOpened := False;
  LFrames := TStringList.Create;
  try
    if AWantsStream then
    begin
      LWriter := TStreamingStatusWriter.Create(LFrames);
      LAccept := 'application/json, text/event-stream';
    end
    else
    begin
      LWriter := TSilentStatusWriter.Create;
      LAccept := 'application/json';
    end;

    LHandler := TMCPTransportHandler.Create(FServer, LWriter);

    // Only a stream sends its headers from inside the handler, so this firing
    // at all is what says one was opened
    LHandler.SendResponseHeadersProc :=
      procedure (AResponse: TMCPTransportResponse)
      begin
        LOpened := True;
      end;

    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := ACommand;
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := LAccept;
        ARequest.Content := ABody;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LAnswer.Code := AResponse.Code;
        LAnswer.Content := AResponse.Content;
      end);
    LHandler := nil;

    LAnswer.StreamOpened := LOpened;
    LAnswer.Frames := LFrames.Text;
  finally
    LFrames.Free;
  end;

  Result := LAnswer;
end;

procedure TTransportStatusTest.TestAnsweredRequestIsOk;
var
  LAnswer: TStatusAnswer;
begin
  LAnswer := Send(Body('tools/list'));

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TTransportStatusTest.TestAcceptedNotificationIsAcceptedWithNoBody;
var
  LAnswer: TStatusAnswer;
begin
  // A notification is never answered, so there is nothing to put a status on
  // but the acceptance itself
  LAnswer := Send('{"jsonrpc":"2.0","method":"notifications/cancelled",' +
    '"params":{"requestId":1}}');

  Assert.AreEqual(HTTP_CODE_ACCEPTED, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual('', LAnswer.Content, 'a 202 carries no body');
end;

procedure TTransportStatusTest.TestANotificationIsAcceptedEvenWhenAStreamWasOffered;
var
  LAnswer: TStatusAnswer;
begin
  // "Regardless of Accept": the acceptance of a notification is the whole of
  // the reply, and a client that would have taken a stream does not turn it
  // into something to stream. This used to answer 200 with an empty event
  // stream, because the shape of the reply was decided from the Accept before
  // the payload had been parsed.
  LAnswer := Send('{"jsonrpc":"2.0","method":"notifications/cancelled",' +
    '"params":{"requestId":1}}', 'POST', True);

  Assert.AreEqual(HTTP_CODE_ACCEPTED, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual('', LAnswer.Content, 'a 202 carries no body');
end;

procedure TTransportStatusTest.TestANotificationThatAcceptsAStreamOpensNone;
var
  LAnswer: TStatusAnswer;
begin
  LAnswer := Send('{"jsonrpc":"2.0","method":"notifications/cancelled",' +
    '"params":{"requestId":1}}', 'POST', True);

  Assert.IsFalse(LAnswer.StreamOpened,
    'nothing is streamed for a payload that has no reply, so no stream is opened');
  Assert.AreEqual('', LAnswer.Frames);
end;

procedure TTransportStatusTest.TestABatchOfNotificationsIsRefusedNotAccepted;
var
  LAnswer: TStatusAnswer;
begin
  // The near-miss worth pinning: a batch of nothing but notifications would be
  // answered 202 if the messages inside it were what decided. The envelope
  // decides first, and an array is not a single message.
  LAnswer := Send('[{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}},' +
    '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":2}}]',
    'POST', True);

  Assert.AreEqual(HTTP_CODE_BADREQUEST, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_REQUEST, LAnswer.ErrorCode, LAnswer.Content);
end;

procedure TTransportStatusTest.TestARequestThatAcceptsAStreamStillStreams;
var
  LAnswer: TStatusAnswer;
begin
  // The control: deciding after the parse must not have cost the streaming
  // case, which is the one the whole mechanism exists for
  LAnswer := Send(Body('tools/list'), 'POST', True);

  Assert.IsTrue(LAnswer.StreamOpened, 'a request that asked for a stream gets one');
  Assert.Contains(LAnswer.Frames, '"result"');
  Assert.AreEqual('', LAnswer.Content, 'the reply went out on the stream, not in the body');
end;

procedure TTransportStatusTest.TestARefusedPayloadIsNotStreamed;
var
  LAnswer: TStatusAnswer;
begin
  // A refusal decided before dispatch is answered as a document even to a client
  // that offered a stream: its status was settled before any header went out, so
  // unlike an error raised *during* dispatch it can actually be sent. Streaming
  // it would mean 200 with the refusal in a frame.
  LAnswer := Send('[' + Body('tools/list') + ']', 'POST', True);

  Assert.IsFalse(LAnswer.StreamOpened, 'nothing is streamed for a payload that was refused');
  Assert.AreEqual(HTTP_CODE_BADREQUEST, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '-32600');
end;

procedure TTransportStatusTest.TestAMalformedPayloadIsAnsweredNotAccepted;
var
  LAnswer: TStatusAnswer;
begin
  // The edge the "has anything to answer" test has to get right: a payload that
  // would not parse holds no notification, but it does hold an error to send
  LAnswer := Send('{ not json at all', 'POST', True);

  Assert.AreNotEqual(HTTP_CODE_ACCEPTED, LAnswer.Code,
    'a parse error is answered, not accepted');
  Assert.IsTrue(LAnswer.StreamOpened, 'and it goes out on the stream that was asked for');
  Assert.Contains(LAnswer.Frames, '"error"');
end;

procedure TTransportStatusTest.TestUnknownNamespaceIsNotFound;
var
  LAnswer: TStatusAnswer;
begin
  LAnswer := Send(Body('nosuch/method'));

  Assert.AreEqual(HTTP_CODE_NOTFOUND, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_METHOD_NOT_FOUND, LAnswer.ErrorCode);
end;

procedure TTransportStatusTest.TestUnknownMethodOfAKnownNamespaceIsNotFound;
var
  LAnswer: TStatusAnswer;
begin
  // The other place a method goes missing: the namespace resolves, and the
  // method inside it does not
  LAnswer := Send(Body('tools/nosuch'));

  Assert.AreEqual(HTTP_CODE_NOTFOUND, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_METHOD_NOT_FOUND, LAnswer.ErrorCode);
end;

procedure TTransportStatusTest.TestNotFoundStillCarriesTheJsonRpcError;
var
  LAnswer: TStatusAnswer;
  LValue: TJSONValue;
begin
  // The body is the half that matters as much as the status: it is what tells
  // a dual-era client that this is a modern server refusing a method, rather
  // than a legacy HTTP+SSE server that does not host the endpoint at all
  LAnswer := Send(Body('nosuch/method'));

  LValue := TJSONObject.ParseJSONValue(LAnswer.Content);
  Assert.IsTrue(LValue is TJSONObject, 'a 404 must still answer JSON-RPC: ' + LAnswer.Content);
  try
    Assert.AreEqual('2.0', TJSONObject(LValue).GetValue<string>('jsonrpc', ''));
    Assert.AreEqual(3, TJSONObject(LValue).GetValue<Integer>('id', -1),
      'and it must be correlatable to the request it refuses');
  finally
    LValue.Free;
  end;
end;

procedure TTransportStatusTest.TestMalformedMetaIsBadRequest;
var
  LAnswer: TStatusAnswer;
begin
  LAnswer := Send('{"jsonrpc":"2.0","id":3,"method":"tools/list","params":{}}');

  Assert.AreEqual(HTTP_CODE_BADREQUEST, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TTransportStatusTest.TestUnsupportedVersionIsBadRequest;
var
  LAnswer: TStatusAnswer;
begin
  LAnswer := Send('{"jsonrpc":"2.0","id":3,"method":"tools/list","params":{"_meta":{' +
    '"io.modelcontextprotocol/protocolVersion":"2025-11-25",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}}');

  Assert.AreEqual(HTTP_CODE_BADREQUEST, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(MCP_UNSUPPORTED_PROTOCOL_VERSION, LAnswer.ErrorCode);
end;

procedure TTransportStatusTest.TestAnOrdinaryErrorKeepsTheOrdinaryStatus;
var
  LAnswer: TStatusAnswer;
begin
  // An unknown tool name is Invalid Params too - the same -32602 a malformed
  // "_meta" reports - and it is a plain 200. This is the test that says the
  // status comes from what was raised and not from the code it carries.
  LAnswer := Send(Body('tools/call', '"name":"nosuch","arguments":{},'));

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TTransportStatusTest.TestABatchIsRefusedWholeNotElementByElement;
var
  LAnswer: TStatusAnswer;
begin
  // This used to be the reason the mapping had to stay conservative: a batch came
  // back as an array of outcomes, and one status could not describe several of
  // them. Refusing the batch whole removed the exception - the array is never
  // looked inside, so the unknown method in it is never reported.
  LAnswer := Send('[' + Body('tools/list') + ',' + Body('nosuch/method') + ']');

  Assert.AreEqual(HTTP_CODE_BADREQUEST, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_REQUEST, LAnswer.ErrorCode, LAnswer.Content);
  Assert.DoesNotContain(LAnswer.Content, '-32601',
    'the elements are never dispatched, so nothing reports on them');
end;

procedure TTransportStatusTest.TestGetIsMethodNotAllowed;
var
  LAnswer: TStatusAnswer;
begin
  // GET existed only to open the server-to-client stream, which went with
  // sessions: an older client that tries it is told so
  LAnswer := Send('', 'GET');

  Assert.AreEqual(HTTP_CODE_NOTALLOWED, LAnswer.Code);
end;

procedure TTransportStatusTest.TestDeleteIsMethodNotAllowed;
var
  LAnswer: TStatusAnswer;
begin
  // DELETE terminated a session, and there are none
  LAnswer := Send('', 'DELETE');

  Assert.AreEqual(HTTP_CODE_NOTALLOWED, LAnswer.Code);
end;

initialization
  TDUnitX.RegisterTestFixture(TTransportStatusTest);

end.
