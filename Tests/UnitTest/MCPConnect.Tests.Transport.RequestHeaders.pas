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
///   The Streamable HTTP request-metadata headers of MCP 2026-07-28
///   (MCPConnect.MCP.Middleware.Headers): that a header contradicting the body
///   is refused with 400 and HeaderMismatch (-32020), that Strict also requires
///   the headers to be there, and that the shapes the revision says nothing
///   about are let through untouched.
/// </summary>
unit MCPConnect.Tests.Transport.RequestHeaders;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware.Headers,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentHeaderWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a request answered, as far as these tests care.</summary>
  THeaderAnswer = record
    Code: Integer;
    Content: string;

    /// <summary>The JSON-RPC error code of the reply, or 0 when it carries none.</summary>
    function ErrorCode: Integer;

    /// <summary>The JSON-RPC id of the reply, or -1 when it carries none.</summary>
    function ReplyId: Integer;
  end;

  [TestFixture]
  TMCPHeaderValueTest = class(TObject)
  public
    [Test]
    procedure TestPlainAsciiTravelsAsItIs();
    [Test]
    procedure TestNonAsciiIsEncoded();
    [Test]
    procedure TestPaddedValueIsEncoded();
    [Test]
    procedure TestSentinelLookalikeIsEncoded();
    [Test]
    procedure TestDecodeUndoesEncode();
    [Test]
    procedure TestDecodeLeavesAPlainValueAlone();
    [Test]
    procedure TestMalformedSentinelRaises();
    [Test]
    procedure TestSentinelMarkersAreCaseSensitive();
  end;

  TRequestHeadersTest = class(TObject)
  protected const
    ProtocolVersion = MCP_PROTOCOL_VERSION_2026_07_28;
    ToolName = 'echo';
  protected
    FServer: TMCPServer;

    /// <summary>
    ///   How much the server of this fixture enforces. Overridden rather than
    ///   set in a Setup of its own: DUnitX finds [Setup] by attribute, and two
    ///   methods of the same name in one hierarchy is asking for the wrong one
    ///   to run.
    /// </summary>
    function Mode: TMCPValidationLevel; virtual;

    procedure ConfigureServer(AMode: TMCPValidationLevel);
    function Send(const ABody: string;
      const AShape: TMCPTransportRequestConverter = nil): THeaderAnswer;

    /// <summary>A tools/call body for the echo tool of these tests.</summary>
    function CallBody(const AName: string = ToolName;
      const AArguments: string = '{"text":"hi"}'): string;

    /// <summary>
    ///   Marks a parameter of a registered tool with an "x-mcp-header"
    ///   annotation, which is how a tool asks for an argument to be mirrored.
    /// </summary>
    /// <remarks>
    ///   Written into the generated schema by hand even though an [McpParam]
    ///   "header=" tag can put it there now (see
    ///   MCPConnect.Tests.MCP.ParamHeaders): these tests are about what the
    ///   server does with an annotation it finds, and a change to the tag
    ///   vocabulary must not be able to quietly stop exercising them.
    /// </remarks>
    procedure AnnotateParam(const AToolName, AParamName, AHeaderName: string);
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();
  end;

  [TestFixture]
  TStrictHeadersTest = class(TRequestHeadersTest)
  public
    [Test]
    procedure TestMiddlewareIsRegisteredByDefault();
    [Test]
    procedure TestMiddlewareIsRegisteredOnlyOnce();

    [Test]
    procedure TestCompleteHeadersAreAccepted();
    [Test]
    procedure TestMissingMethodHeaderIsRefused();
    [Test]
    procedure TestMissingProtocolVersionHeaderIsRefused();
    [Test]
    procedure TestMissingNameHeaderIsRefused();
    [Test]
    procedure TestMismatchedMethodHeaderIsRefused();
    [Test]
    procedure TestMismatchedNameHeaderIsRefused();
    [Test]
    procedure TestMismatchedProtocolVersionIsRefused();
    [Test]
    procedure TestRefusalCarriesTheRequestId();

    [Test]
    procedure TestEncodedNameIsDecodedBeforeComparing();
    [Test]
    procedure TestEncodedNameThatDisagreesIsRefused();
    [Test]
    procedure TestMalformedEncodedNameIsRefused();

    [Test]
    procedure TestResourcesReadMirrorsTheUri();
    [Test]
    procedure TestMethodWithoutANameNeedsNoNameHeader();

    [Test]
    procedure TestNotificationIsLetThrough();
    [Test]
    procedure TestBatchIsLetThrough();
    [Test]
    procedure TestMalformedBodyIsStillAParseError();
    [Test]
    procedure TestStdioIsLetThrough();
    [Test]
    procedure TestNonPostIsUntouched();
  end;

  [TestFixture]
  TParamHeadersTest = class(TRequestHeadersTest)
  private
    function SendWithParam(const AHeaderName, AHeaderValue, AArguments: string;
      const AToolHeader: string = 'Text'): THeaderAnswer;
  public
    [Test]
    procedure TestMatchingParamHeaderIsAccepted();
    [Test]
    procedure TestMismatchedParamHeaderIsRefused();
    [Test]
    procedure TestMissingParamHeaderIsRefused();
    [Test]
    procedure TestParamHeaderWithoutAnArgumentIsRefused();
    [Test]
    procedure TestUnrecognizedParamHeaderIsIgnored();
    [Test]
    procedure TestNumbersAreComparedNumerically();
    [Test]
    procedure TestHeaderNameIsCaseInsensitive();
  end;

  [TestFixture]
  TLenientHeadersTest = class(TRequestHeadersTest)
  protected
    function Mode: TMCPValidationLevel; override;
  public
    [Test]
    procedure TestMissingHeadersAreTolerated();
    [Test]
    procedure TestMismatchIsStillRefused();
    [Test]
    procedure TestMissingParamHeaderIsTolerated();
  end;

  [TestFixture]
  TOffHeadersTest = class(TRequestHeadersTest)
  protected
    function Mode: TMCPValidationLevel; override;
  public
    [Test]
    procedure TestOffKeepsTheMiddlewareOutOfTheChain();
    [Test]
    procedure TestMismatchIsNotChecked();
  end;

implementation

uses
  System.NetEncoding,

  JRPC.Core,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Errors;

const
  /// <summary>The non-ASCII example of the specification: "Hello, " and the two CJK characters for "world".</summary>
  HelloWorld = 'Hello, '#$4E16#$754C;

  /// <summary>Two names that differ only in an accent, as character codes.</summary>
  CoffeeGrave = 'caff'#$00E8;
  CoffeeAcute = 'caff'#$00E9;

type
  TEchoTool = class(TObject)
  public
    [McpTool('echo', 'Echoes what it is given')]
    function Echo([McpParam('text', 'The text to echo')] const text: string): string;

    [McpTool('add', 'Adds one to a number')]
    function Add([McpParam('value', 'The number')] const value: Integer): Integer;
  end;

function TEchoTool.Echo(const text: string): string;
begin
  Result := text;
end;

function TEchoTool.Add(const value: Integer): Integer;
begin
  Result := value + 1;
end;

{ TSilentHeaderWriter }

procedure TSilentHeaderWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentHeaderWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentHeaderWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ THeaderAnswer }

function THeaderAnswer.ErrorCode: Integer;
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

function THeaderAnswer.ReplyId: Integer;
var
  LValue: TJSONValue;
begin
  Result := -1;

  LValue := TJSONObject.ParseJSONValue(Content);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    Result := TJSONObject(LValue).GetValue<Integer>('id', -1);
  finally
    LValue.Free;
  end;
end;

{ TRequestHeadersTest }

function TRequestHeadersTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Strict;
end;

procedure TRequestHeadersTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  ConfigureServer(Mode);
end;

procedure TRequestHeadersTest.TearDown;
begin
  FServer.Free;
end;

procedure TRequestHeadersTest.ConfigureServer(AMode: TMCPValidationLevel);
begin
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('request-headers-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(AMode)
      // These fixtures are about the headers: the body contract and the origin
      // check have their own middleware, and their own tests.
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TEchoTool)
    .BackToMCP
  .ApplyConfig;
end;

procedure TRequestHeadersTest.AnnotateParam(const AToolName, AParamName,
  AHeaderName: string);
var
  LProperties: TJSONValue;
  LParam: TJSONValue;
begin
  LProperties := FServer.Plugin.Configure<IMCPConfig>
    .Tools.Registry[AToolName].InputSchema.GetValue('properties');
  Assert.IsTrue(LProperties is TJSONObject, 'the generated schema must have properties');

  LParam := TJSONObject(LProperties).GetValue(AParamName);
  Assert.IsTrue(LParam is TJSONObject, 'parameter [' + AParamName + '] must be in the schema');

  TJSONObject(LParam).AddPair(MCP_SCHEMA_HEADER_KEYWORD, AHeaderName);
end;

function TRequestHeadersTest.CallBody(const AName, AArguments: string): string;
begin
  Result := Format(
    '{"jsonrpc":"2.0","id":7,"method":"tools/call",' +
    '"params":{"name":"%s","arguments":%s,' +
    '"_meta":{"io.modelcontextprotocol/protocolVersion":"%s",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}}',
    [AName, AArguments, ProtocolVersion]);
end;

function TRequestHeadersTest.Send(const ABody: string;
  const AShape: TMCPTransportRequestConverter): THeaderAnswer;
var
  LHandler: TMCPTransportHandler;
  LAnswer: THeaderAnswer;
begin
  LAnswer := Default(THeaderAnswer);

  LHandler := TMCPTransportHandler.Create(FServer, TSilentHeaderWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := ABody;

        if Assigned(AShape) then
          AShape(ARequest);
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LAnswer.Code := AResponse.Code;
        LAnswer.Content := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LAnswer;
end;

{ TMCPHeaderValueTest }

procedure TMCPHeaderValueTest.TestPlainAsciiTravelsAsItIs;
begin
  Assert.IsFalse(TMCPHeaderValue.NeedsEncoding('us-west1'));
  Assert.AreEqual('us-west1', TMCPHeaderValue.Encode('us-west1'));
end;

procedure TMCPHeaderValueTest.TestNonAsciiIsEncoded;
begin
  // Written as character codes, not as literal text: the sources of this
  // project are UTF-8 without a BOM, which the compiler reads as ANSI, so a
  // non-ASCII literal would not survive the parse.
  Assert.IsTrue(TMCPHeaderValue.NeedsEncoding(HelloWorld));

  // The example of the specification, verbatim
  Assert.AreEqual('=?base64?SGVsbG8sIOS4lueVjA==?=', TMCPHeaderValue.Encode(HelloWorld));
end;

procedure TMCPHeaderValueTest.TestPaddedValueIsEncoded;
begin
  Assert.IsTrue(TMCPHeaderValue.NeedsEncoding(' padded '));
  Assert.AreEqual('=?base64?IHBhZGRlZCA=?=', TMCPHeaderValue.Encode(' padded '));
end;

procedure TMCPHeaderValueTest.TestSentinelLookalikeIsEncoded;
begin
  // A plain value that looks like the sentinel is encoded too, or the server
  // would decode something that was never encoded
  Assert.IsTrue(TMCPHeaderValue.NeedsEncoding('=?base64?literal?='));
  // The table of the specification prints this one as "...NjQvbGl0...", which is
  // not the Base64 of the value beside it: the '?' of "base64?" is 0x3F, and
  // standard Base64 renders it '/', not 'v'.
  Assert.AreEqual('=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?=',
    TMCPHeaderValue.Encode('=?base64?literal?='));
end;

procedure TMCPHeaderValueTest.TestDecodeUndoesEncode;
const
  Values: array[0..3] of string = (HelloWorld, ' padded ', 'line1'#10'line2', '=?base64?x?=');
var
  LValue: string;
begin
  for LValue in Values do
    Assert.AreEqual(LValue, TMCPHeaderValue.Decode(TMCPHeaderValue.Encode(LValue)));
end;

procedure TMCPHeaderValueTest.TestDecodeLeavesAPlainValueAlone;
begin
  Assert.AreEqual('get_weather', TMCPHeaderValue.Decode('get_weather'));
  Assert.AreEqual('file:///a/b.json', TMCPHeaderValue.Decode('file:///a/b.json'));
end;

procedure TMCPHeaderValueTest.TestMalformedSentinelRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMCPHeaderValue.Decode('=?base64?not base64 at all!?=');
    end,
    EMCPHeaderEncodingError);
end;

procedure TMCPHeaderValueTest.TestSentinelMarkersAreCaseSensitive;
begin
  // "MUST appear exactly as shown (lowercase)": anything else is a plain value
  Assert.IsFalse(TMCPHeaderValue.IsEncoded('=?BASE64?SGVsbG8=?='));
  Assert.AreEqual('=?BASE64?SGVsbG8=?=', TMCPHeaderValue.Decode('=?BASE64?SGVsbG8=?='));
end;

{ TStrictHeadersTest }

procedure TStrictHeadersTest.TestMiddlewareIsRegisteredByDefault;
begin
  // Not opt-in: applying any MCP configuration is what puts it in
  Assert.IsTrue(FServer.Middleware.Contains(TMCPRequestHeadersMiddleware));
end;

procedure TStrictHeadersTest.TestMiddlewareIsRegisteredOnlyOnce;
begin
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetVersion('1.0.1')
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(1, FServer.Middleware.Count);
end;

procedure TStrictHeadersTest.TestCompleteHeadersAreAccepted;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TStrictHeadersTest.TestMissingMethodHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMissingProtocolVersionHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMissingNameHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMismatchedMethodHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  // The case the check exists for: an intermediary routing on "tools/list"
  // while the server would have executed a call
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/list');
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMismatchedNameHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, 'add');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMismatchedProtocolVersionIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, '2025-06-18');
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestRefusalCarriesTheRequestId;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody());

  // A refusal the client can correlate: the id of the request it refused
  Assert.AreEqual(7, LAnswer.ReplyId, LAnswer.Content);
  Assert.Contains(LAnswer.Content, 'jsonrpc');
end;

procedure TStrictHeadersTest.TestEncodedNameIsDecodedBeforeComparing;
var
  LAnswer: THeaderAnswer;
begin
  // A name outside the header-safe set travels in the sentinel, and the server
  // decodes it before comparing. The tool does not exist, so what comes back is
  // an invalid-params error rather than a result - but it is not -32020, which
  // is what this test is about.
  LAnswer := Send(CallBody(CoffeeGrave),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, TMCPHeaderValue.Encode(CoffeeGrave));
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.IsFalse(LAnswer.ErrorCode = MCP_HEADER_MISMATCH, LAnswer.Content);
end;

procedure TStrictHeadersTest.TestEncodedNameThatDisagreesIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(CoffeeGrave),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, TMCPHeaderValue.Encode(CoffeeAcute));
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMalformedEncodedNameIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, '=?base64?not base64!?=');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestResourcesReadMirrorsTheUri;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(
    '{"jsonrpc":"2.0","id":3,"method":"resources/read","params":{"uri":"res://a"}}',
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'resources/read');
      ARequest.SetHeader(MCP_HEADER_NAME, 'res://b');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TStrictHeadersTest.TestMethodWithoutANameNeedsNoNameHeader;
var
  LAnswer: THeaderAnswer;
begin
  // tools/list addresses nothing by name, so Mcp-Name is not required of it
  LAnswer := Send('{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}',
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/list');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
    end);

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TStrictHeadersTest.TestNotificationIsLetThrough;
var
  LAnswer: THeaderAnswer;
begin
  // This revision defines no header rules for a notification POST
  LAnswer := Send('{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}');

  Assert.IsFalse(LAnswer.Code = 400, LAnswer.Content);
end;

procedure TStrictHeadersTest.TestBatchIsLetThrough;
var
  LAnswer: THeaderAnswer;
begin
  // A batch is outside this revision - the body MUST be a single message - so
  // there is nothing for a single set of headers to mirror, and this middleware
  // hands it on untouched rather than calling it a header mismatch.
  //
  // The transport then refuses it for its shape, which is a 400 of its own: what
  // this asserts is the *reason*, not the status. Using the status as a proxy for
  // it stopped working the day a batch became a refusal.
  LAnswer := Send('[{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}]');

  Assert.AreNotEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode, LAnswer.Content);
end;

procedure TStrictHeadersTest.TestMalformedBodyIsStillAParseError;
var
  LAnswer: THeaderAnswer;
begin
  // A body that is not JSON is a parse error, not a header mismatch
  LAnswer := Send('{not json');

  Assert.AreEqual(JRPC_PARSE_ERROR, LAnswer.ErrorCode, LAnswer.Content);
end;

procedure TStrictHeadersTest.TestStdioIsLetThrough;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Protocol := TTransportProtocol.Stdio;
    end);

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TStrictHeadersTest.TestNonPostIsUntouched;
var
  LAnswer: THeaderAnswer;
begin
  // GET went with the session-era stream: still a 405, not a 400
  LAnswer := Send('',
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Command := 'GET';
    end);

  Assert.AreEqual(405, LAnswer.Code);
end;

{ TParamHeadersTest }

function TParamHeadersTest.SendWithParam(const AHeaderName, AHeaderValue,
  AArguments, AToolHeader: string): THeaderAnswer;
begin
  AnnotateParam(ToolName, 'text', AToolHeader);

  Result := Send(CallBody(ToolName, AArguments),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, ToolName);
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);

      if not AHeaderName.IsEmpty then
        ARequest.SetHeader(AHeaderName, AHeaderValue);
    end);
end;

procedure TParamHeadersTest.TestMatchingParamHeaderIsAccepted;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := SendWithParam('Mcp-Param-Text', 'hi', '{"text":"hi"}');

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TParamHeadersTest.TestMismatchedParamHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := SendWithParam('Mcp-Param-Text', 'bye', '{"text":"hi"}');

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TParamHeadersTest.TestMissingParamHeaderIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  // The argument is in the body and the header is not: a non-conforming client
  LAnswer := SendWithParam('', '', '{"text":"hi"}');

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TParamHeadersTest.TestParamHeaderWithoutAnArgumentIsRefused;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := SendWithParam('Mcp-Param-Text', 'hi', '{}');

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TParamHeadersTest.TestUnrecognizedParamHeaderIsIgnored;
var
  LAnswer: THeaderAnswer;
begin
  // A header the server has no annotation for is forwarded and ignored, as
  // RFC 9110 requires of any field a recipient does not recognize
  LAnswer := SendWithParam('Mcp-Param-Region', 'us-west1', '{"text":"hi"}', 'Text');

  Assert.AreEqual(400, LAnswer.Code,
    'Mcp-Param-Text is still missing, so this is a refusal - but for that reason');
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TParamHeadersTest.TestNumbersAreComparedNumerically;
var
  LAnswer: THeaderAnswer;
begin
  AnnotateParam('add', 'value', 'Value');

  LAnswer := Send(CallBody('add', '{"value":42}'),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/call');
      ARequest.SetHeader(MCP_HEADER_NAME, 'add');
      ARequest.SetHeader(MCP_HEADER_PROTOCOL_VERSION, ProtocolVersion);
      ARequest.SetHeader('Mcp-Param-Value', '42.0');
    end);

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
end;

procedure TParamHeadersTest.TestHeaderNameIsCaseInsensitive;
var
  LAnswer: THeaderAnswer;
begin
  // Field names are case-insensitive; only the values are not
  LAnswer := SendWithParam('mcp-param-TEXT', 'hi', '{"text":"hi"}');

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
end;

{ TLenientHeadersTest }

function TLenientHeadersTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Lenient;
end;

procedure TLenientHeadersTest.TestMissingHeadersAreTolerated;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody());

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TLenientHeadersTest.TestMismatchIsStillRefused;
var
  LAnswer: THeaderAnswer;
begin
  // The half of the check that is a security property survives Lenient
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'tools/list');
    end);

  Assert.AreEqual(400, LAnswer.Code);
  Assert.AreEqual(MCP_HEADER_MISMATCH, LAnswer.ErrorCode);
end;

procedure TLenientHeadersTest.TestMissingParamHeaderIsTolerated;
var
  LAnswer: THeaderAnswer;
begin
  AnnotateParam(ToolName, 'text', 'Text');

  LAnswer := Send(CallBody());

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
end;

{ TOffHeadersTest }

function TOffHeadersTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Off;
end;

procedure TOffHeadersTest.TestOffKeepsTheMiddlewareOutOfTheChain;
begin
  // A server that wants none of it pays nothing for it
  Assert.AreEqual(0, FServer.Middleware.Count);
end;

procedure TOffHeadersTest.TestMismatchIsNotChecked;
var
  LAnswer: THeaderAnswer;
begin
  LAnswer := Send(CallBody(),
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader(MCP_HEADER_METHOD, 'prompts/get');
    end);

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPHeaderValueTest);
  TDUnitX.RegisterTestFixture(TStrictHeadersTest);
  TDUnitX.RegisterTestFixture(TParamHeadersTest);
  TDUnitX.RegisterTestFixture(TLenientHeadersTest);
  TDUnitX.RegisterTestFixture(TOffHeadersTest);

end.
