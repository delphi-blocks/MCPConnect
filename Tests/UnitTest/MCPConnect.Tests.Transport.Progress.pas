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
///   Progress reporting: a notification may reference only a token an active
///   request provided, so a tool reports through the channel of the request it
///   is serving and gets nothing sent when the client asked for nothing. What
///   is covered here is the rule rather than the plumbing - what leaves the
///   server, on which token, and what never leaves at all.
/// </summary>
unit MCPConnect.Tests.Transport.Progress;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>Keeps every SSE frame the handler wrote.</summary>
  TRecordingWriter = class(TInterfacedObject, IMCPTransportWriter)
  private
    FFrames: TStringList;
  public
    constructor Create(AFrames: TStringList);

    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TProgressTest = class(TObject)
  private
    FServer: TMCPServer;
    FFrames: TStringList;

    procedure ConfigureServer(AMeta: TMCPValidationLevel = TMCPValidationLevel.Strict);

    /// <summary>
    ///   Calls ATool over a stream the notifications can travel on. AToken is
    ///   written into the request "_meta" as the client would; empty asks for
    ///   no progress at all.
    /// </summary>
    function Call(const ATool: string; const AToken: string = ''): string;

    /// <summary>How many progress notifications reached the client.</summary>
    function ProgressFrames: Integer;

    /// <summary>Everything the client received, frames and reply alike.</summary>
    function Written: string;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestProgressTravelsOnTheTokenTheRequestGave();
    [Test]
    procedure TestNothingIsSentWhenTheClientAskedForNone();
    [Test]
    procedure TestATokenIsAnIntegerWhenTheClientSentOne();

    [Test]
    procedure TestAReportThatDoesNotAdvanceIsDropped();

    [Test]
    procedure TestAHandBuiltNotificationOnAMadeUpTokenIsDropped();
    [Test]
    procedure TestNothingIsDroppedWhenNobodyReadTheToken();

    [Test]
    procedure TestTheToolStillAnswers();
  end;

implementation

uses
  JRPC.Core,
  JRPC.Classes,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Notifications;

type
  TProgressTools = class(TObject)
  private
    [Context] FProgress: TMCPProgress;
    [Context] FQueue: TMCPMessageQueue;
  public
    /// <summary>Reports twice on the way to an answer.</summary>
    [McpTool('work', 'Reports progress while it works')]
    function Work: string;

    /// <summary>Reports the same number twice, which the spec forbids.</summary>
    [McpTool('stuck', 'Reports a progress value that does not advance')]
    function Stuck: string;

    /// <summary>
    ///   Builds the notification by hand, on a token of its own invention -
    ///   which is what the transport has to catch.
    /// </summary>
    [McpTool('freelance', 'Sends progress on a token nobody gave it')]
    function Freelance: string;
  end;

function TProgressTools.Work: string;
begin
  FProgress.Report(1, 2, 'halfway');
  FProgress.Report(2, 2, 'done');
  Result := 'worked';
end;

function TProgressTools.Stuck: string;
begin
  FProgress.Report(1, 2, 'halfway');
  FProgress.Report(1, 2, 'still halfway');
  Result := 'worked';
end;

function TProgressTools.Freelance: string;
begin
  FQueue.Enqueue(TMCPNotification.Progress('a-token-of-my-own', 1, 2, 'working'));
  Result := 'worked';
end;

{ TRecordingWriter }

constructor TRecordingWriter.Create(AFrames: TStringList);
begin
  inherited Create;
  FFrames := AFrames;
end;

procedure TRecordingWriter.Write(const AValue: string; const AEventId: string);
begin
  FFrames.Add(AValue);
end;

function TRecordingWriter.Connected: Boolean;
begin
  Result := True;
end;

function TRecordingWriter.SupportsStreaming: Boolean;
begin
  Result := True;
end;

{ TProgressTest }

procedure TProgressTest.Setup;
begin
  FFrames := TStringList.Create;
end;

procedure TProgressTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
  FFrames.Free;
end;

procedure TProgressTest.ConfigureServer(AMeta: TMCPValidationLevel);
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('progress-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(AMeta)
    .BackToMCP
    .Tools
      .RegisterClass(TProgressTools)
    .BackToMCP
  .ApplyConfig;
end;

function TProgressTest.Call(const ATool, AToken: string): string;
var
  LHandler: TMCPTransportHandler;
  LBody, LToken, LContent: string;
begin
  if AToken.IsEmpty then
    LToken := ''
  else
    LToken := Format(',"progressToken":%s', [AToken]);

  LBody := Format(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":' +
    '{"name":"%s","arguments":{},"_meta":{' +
    '"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
    '"io.modelcontextprotocol/clientCapabilities":{}%s}}}', [ATool, LToken]);

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TRecordingWriter.Create(FFrames));
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        // Progress travels on the response stream of the request it belongs to,
        // so there has to be one
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

function TProgressTest.ProgressFrames: Integer;
var
  LFrame: string;
begin
  Result := 0;
  for LFrame in FFrames do
    if LFrame.Contains('notifications/progress') then
      Inc(Result);
end;

function TProgressTest.Written: string;
begin
  Result := FFrames.Text;
end;

procedure TProgressTest.TestProgressTravelsOnTheTokenTheRequestGave;
begin
  ConfigureServer();

  Call('work', '"abc123"');

  Assert.AreEqual(2, ProgressFrames, Written);
  Assert.Contains(Written, '"progressToken":"abc123"');
  Assert.Contains(Written, 'halfway');
end;

procedure TProgressTest.TestNothingIsSentWhenTheClientAskedForNone;
begin
  ConfigureServer();

  // The rule: a progress notification may reference only a token an active
  // request provided, and this request provided none. The tool reports all the
  // same - it has no way of knowing - and nothing goes out.
  Call('work');

  Assert.AreEqual(0, ProgressFrames, Written);
end;

procedure TProgressTest.TestATokenIsAnIntegerWhenTheClientSentOne;
begin
  ConfigureServer();

  // A progress token is a string or an integer, and it goes back as it came
  Call('work', '42');

  Assert.AreEqual(2, ProgressFrames, Written);
  Assert.Contains(Written, '"progressToken":42');
end;

procedure TProgressTest.TestAReportThatDoesNotAdvanceIsDropped;
begin
  ConfigureServer();

  // "The progress value MUST increase with each notification": the second
  // report says 1 again, and a client told that has been told something false
  Call('stuck', '"abc123"');

  Assert.AreEqual(1, ProgressFrames, Written);
  Assert.Contains(Written, 'halfway');
  Assert.DoesNotContain(Written, 'still halfway');
end;

procedure TProgressTest.TestAHandBuiltNotificationOnAMadeUpTokenIsDropped;
begin
  ConfigureServer();

  // Not everything goes through the channel: a tool can build a notification
  // itself, and this one invents a token for a request that asked for nothing
  Call('freelance');

  Assert.AreEqual(0, ProgressFrames, Written);
end;

procedure TProgressTest.TestNothingIsDroppedWhenNobodyReadTheToken;
begin
  // With the "_meta" check off nothing read the request's token, so the server
  // cannot tell an invented token from a real one. Unverifiable is not the same
  // as wrong, and dropping here would silence a server that has done nothing
  // out of order.
  ConfigureServer(TMCPValidationLevel.Off);

  Call('freelance');

  Assert.AreEqual(1, ProgressFrames, Written);
end;

procedure TProgressTest.TestTheToolStillAnswers;
var
  LReply: string;
begin
  ConfigureServer();

  // Whatever happens to the notifications, the result is the point
  LReply := Call('work');

  Assert.Contains(Written + LReply, 'worked');
end;

initialization
  TDUnitX.RegisterTestFixture(TProgressTest);

end.
