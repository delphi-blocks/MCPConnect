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
///   The response side of a streamed reply: which headers open the stream, and
///   when it is opened at all. A stream is not a document, so an intermediary
///   must be told neither to cache it nor to hold it back until it looks
///   finished - Cache-Control says the first and X-Accel-Buffering the second.
/// </summary>
/// <remarks>
///   The other half of the SSE cleanup, the removal of the event id from
///   IMCPTransportWriter.Write, needs no test of its own: the parameter is
///   gone, so nothing can pass one and no writer can emit an "id:" line. The
///   compiler is the guard.
/// </remarks>
unit MCPConnect.Tests.Transport.SSE;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A writer that streams, and keeps what it was given.</summary>
  TStreamingSSEWriter = class(TInterfacedObject, IMCPTransportWriter)
  private
    FFrames: TStringList;
  public
    constructor Create(AFrames: TStringList);

    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>
  ///   A writer that cannot stream - the WebBroker one on Delphi 11 and 12,
  ///   where the RTL has no SSE response stream.
  /// </summary>
  TNonStreamingSSEWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TSSEResponseTest = class(TObject)
  private
    FServer: TMCPServer;
    FFrames: TStringList;

    /// <summary>The headers as they were at the moment the bridge sent them.</summary>
    FSentHeaders: TStringList;
    FSentCode: Integer;
    FSentContentType: string;
    FHeadersSentAfter: Integer;

    procedure ConfigureServer;

    /// <summary>
    ///   Calls the echo tool. AAccept is the request's Accept; AStreaming says
    ///   whether the transport it goes out on can stream at all.
    /// </summary>
    function Call(const AAccept: string; AStreaming: Boolean = True): string;

    function SentHeader(const AName: string): string;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestStreamedReplyIsOpenedWithTheSSEMediaType();
    [Test]
    procedure TestStreamedReplyForbidsCaching();
    [Test]
    procedure TestStreamedReplyAsksProxiesNotToBuffer();
    [Test]
    procedure TestTheHeadersGoOutBeforeTheFirstFrame();

    [Test]
    procedure TestNoStreamIsOpenedForAClientThatDidNotAskForOne();
    [Test]
    procedure TestNoStreamIsOpenedByATransportThatCannotStream();
    [Test]
    procedure TestTheReplyStillArrivesWithoutAStream();
  end;

implementation

uses
  JRPC.Core,

  MCPConnect.MCP.Attributes,
  MCPConnect.Transport.MediaType;

type
  TSSETools = class(TObject)
  public
    [McpTool('echo', 'Answers something')]
    function Echo: string;
  end;

function TSSETools.Echo: string;
begin
  Result := 'echoed';
end;

{ TStreamingSSEWriter }

constructor TStreamingSSEWriter.Create(AFrames: TStringList);
begin
  inherited Create;
  FFrames := AFrames;
end;

procedure TStreamingSSEWriter.Write(const AValue: string);
begin
  FFrames.Add(AValue);
end;

function TStreamingSSEWriter.Connected: Boolean;
begin
  Result := True;
end;

function TStreamingSSEWriter.SupportsStreaming: Boolean;
begin
  Result := True;
end;

{ TNonStreamingSSEWriter }

procedure TNonStreamingSSEWriter.Write(const AValue: string);
begin
  raise Exception.Create('A writer that says it cannot stream must never be written to');
end;

function TNonStreamingSSEWriter.Connected: Boolean;
begin
  Result := True;
end;

function TNonStreamingSSEWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TSSEResponseTest }

procedure TSSEResponseTest.Setup;
begin
  FFrames := TStringList.Create;
  FSentHeaders := TStringList.Create;
  FSentCode := 0;
  FSentContentType := '';
  FHeadersSentAfter := -1;
end;

procedure TSSEResponseTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
  FSentHeaders.Free;
  FFrames.Free;
end;

procedure TSSEResponseTest.ConfigureServer;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('sse-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TSSETools)
    .BackToMCP
  .ApplyConfig;
end;

function TSSEResponseTest.Call(const AAccept: string; AStreaming: Boolean): string;
var
  LHandler: IMCPTransportHandler;
  LWriter: IMCPTransportWriter;
  LContent: string;
begin
  if AStreaming then
    LWriter := TStreamingSSEWriter.Create(FFrames)
  else
    LWriter := TNonStreamingSSEWriter.Create;

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, LWriter);

  // What a real bridge does with the response headers: this one writes down
  // what they said, and when
  LHandler.SendResponseHeadersProc :=
    procedure (AResponse: TMCPTransportResponse)
    begin
      FSentCode := AResponse.Code;
      FSentContentType := AResponse.ContentType;
      FHeadersSentAfter := FFrames.Count;
      for var LPair in AResponse.Headers do
        FSentHeaders.Values[LPair.Key] := LPair.Value;
    end;

  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := '/';
      ARequest.Command := 'POST';
      ARequest.Protocol := TTransportProtocol.StreamableHTTP;
      ARequest.Accept := AAccept;
      ARequest.Content :=
        '{"jsonrpc":"2.0","id":1,"method":"tools/call",' +
        '"params":{"name":"echo","arguments":{}}}';
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LContent := AResponse.Content;
    end);

  Result := LContent;
end;

function TSSEResponseTest.SentHeader(const AName: string): string;
begin
  Result := FSentHeaders.Values[AName];
end;

procedure TSSEResponseTest.TestStreamedReplyIsOpenedWithTheSSEMediaType;
begin
  ConfigureServer;

  Call('application/json, text/event-stream');

  Assert.AreEqual(200, FSentCode);
  Assert.AreEqual(TMediaType.TEXT_EVENT_STREAM, FSentContentType);
end;

procedure TSSEResponseTest.TestStreamedReplyForbidsCaching;
begin
  ConfigureServer;

  Call('application/json, text/event-stream');

  // A cached stream is a stream that never arrives
  Assert.AreEqual('no-cache', SentHeader('Cache-Control'));
end;

procedure TSSEResponseTest.TestStreamedReplyAsksProxiesNotToBuffer;
begin
  ConfigureServer;

  // The SHOULD of the revision: a reverse proxy that buffers the response until
  // it looks complete defeats the point of streaming it, and this is how
  // nginx - and enough others to be worth sending unconditionally - is told not
  // to
  Call('application/json, text/event-stream');

  Assert.AreEqual('no', SentHeader('X-Accel-Buffering'));
end;

procedure TSSEResponseTest.TestTheHeadersGoOutBeforeTheFirstFrame;
begin
  ConfigureServer;

  Call('application/json, text/event-stream');

  Assert.AreEqual(0, FHeadersSentAfter,
    'the headers open the stream, so they cannot follow anything sent on it');
  Assert.IsTrue(FFrames.Count > 0, 'and something was sent on it');
end;

procedure TSSEResponseTest.TestNoStreamIsOpenedForAClientThatDidNotAskForOne;
begin
  ConfigureServer;

  Call(TMediaType.APPLICATION_JSON);

  Assert.AreEqual(-1, FHeadersSentAfter,
    'nothing opens a stream for a client that asked for a document');
  Assert.AreEqual(0, FFrames.Count);
end;

procedure TSSEResponseTest.TestNoStreamIsOpenedByATransportThatCannotStream;
begin
  ConfigureServer;

  // WebBroker before Delphi 13.1: the client asked for a stream and there is no
  // way to give it one
  Call('application/json, text/event-stream', False);

  Assert.AreEqual(-1, FHeadersSentAfter);
end;

procedure TSSEResponseTest.TestTheReplyStillArrivesWithoutAStream;
var
  LReply: string;
begin
  ConfigureServer;

  LReply := Call(TMediaType.APPLICATION_JSON);

  Assert.Contains(LReply, 'echoed', 'the reply is a JSON document instead');
end;

initialization
  TDUnitX.RegisterTestFixture(TSSEResponseTest);

end.
