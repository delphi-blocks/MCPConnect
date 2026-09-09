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
///   The per-request "_meta" contract of MCP 2026-07-28
///   (MCPConnect.MCP.Middleware.RequestMeta): that a request missing the
///   protocol version or the client capabilities is refused with Invalid Params
///   and HTTP 400, that a version the server does not speak is refused with
///   UnsupportedProtocolVersion, and that what was parsed reaches the tool.
/// </summary>
/// <remarks>
///   The binding of the "_meta" members themselves - that they deserialize
///   under their protocol names rather than their Delphi ones - is covered by
///   MCPConnect.Tests.MCP.RequestMeta, which needs no server. What is here
///   needs one: the check is a middleware, and the status code is the
///   transport's.
/// </remarks>
unit MCPConnect.Tests.Transport.RequestMeta;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware.RequestMeta,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentMetaWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a request answered, as far as these tests care.</summary>
  TMetaAnswer = record
    Code: Integer;
    Content: string;

    /// <summary>The JSON-RPC error code of the reply, or 0 when it carries none.</summary>
    function ErrorCode: Integer;

    /// <summary>The JSON-RPC id of the reply, or -1 when it carries none.</summary>
    function ReplyId: Integer;

    /// <summary>The "data" of the error, or nil. The caller owns it.</summary>
    function ErrorData: TJSONObject;
  end;

  TRequestMetaTest = class(TObject)
  protected const
    ProtocolVersion = MCP_PROTOCOL_VERSION_2026_07_28;

    /// <summary>A complete _meta, as a conforming client sends it.</summary>
    FullMeta =
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/clientCapabilities":{},' +
      '"io.modelcontextprotocol/clientInfo":{"name":"probe","version":"1.0"}}';
  protected
    FServer: TMCPServer;

    /// <summary>How much this fixture's server enforces. Strict by default.</summary>
    function Mode: TMCPValidationLevel; virtual;

    procedure ConfigureServer(AMode: TMCPValidationLevel);
    function Send(const ABody: string): TMetaAnswer;

    /// <summary>A tools/list request whose params are exactly AParams.</summary>
    function ListBody(const AParams: string): string;

    /// <summary>How many times AClass sits in the transport chain.</summary>
    function Registered(AClass: TMiddlewareClass): Integer;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();
  end;

  [TestFixture]
  TStrictMetaTest = class(TRequestMetaTest)
  public
    [Test]
    procedure TestMiddlewareIsRegisteredByDefault();
    [Test]
    procedure TestMiddlewareIsRegisteredOnlyOnce();

    [Test]
    procedure TestCompleteMetaIsAccepted();
    [Test]
    procedure TestMissingMetaIsRefused();
    [Test]
    procedure TestMissingParamsIsRefused();
    [Test]
    procedure TestMissingProtocolVersionIsRefused();
    [Test]
    procedure TestBlankProtocolVersionIsRefused();
    [Test]
    procedure TestMissingClientCapabilitiesIsRefused();
    [Test]
    procedure TestRefusalCarriesTheRequestId();

    [Test]
    procedure TestUnsupportedVersionIsRefused();
    [Test]
    procedure TestUnsupportedVersionNamesWhatItWantedAndWhatThereIs();

    [Test]
    procedure TestDiscoverIsCheckedLikeAnyOtherMethod();
    [Test]
    procedure TestNonMCPNamespaceIsNotChecked();
    [Test]
    procedure TestNotificationIsLetThrough();

    [Test]
    procedure TestMetaReachesTheToolThroughContext();
  end;

  [TestFixture]
  TLenientMetaTest = class(TRequestMetaTest)
  protected
    function Mode: TMCPValidationLevel; override;
  public
    [Test]
    procedure TestMissingMetaIsTolerated();
    [Test]
    procedure TestMissingFieldsAreTolerated();
    [Test]
    procedure TestUnsupportedVersionIsStillRefused();
    [Test]
    procedure TestMetaStillReachesTheTool();
  end;

  [TestFixture]
  TOffMetaTest = class(TRequestMetaTest)
  protected
    function Mode: TMCPValidationLevel; override;
  public
    [Test]
    procedure TestOffKeepsTheMiddlewareOutOfTheChain();
    [Test]
    procedure TestNothingIsChecked();
  end;

implementation

uses
  JRPC.Core,
  JRPC.Classes,

  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Errors;

type
  /// <summary>
  ///   Reports what the request said about itself, which is the other half of
  ///   the middleware's job: whatever it parsed is published into the request
  ///   context, and [Context] is how a tool reaches it.
  /// </summary>
  TMetaEchoTool = class(TObject)
  private
    [Context] FMeta: TRequestMetaObject;
  public
    [McpTool('meta_echo', 'Reports the protocol metadata of the request')]
    function MetaEcho: string;
  end;

function TMetaEchoTool.MetaEcho: string;
begin
  if not Assigned(FMeta) then
    Exit('no-meta');

  Result := FMeta.ProtocolVersion + '|' + FMeta.ClientInfo.Name;
end;

{ TSilentMetaWriter }

procedure TSilentMetaWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests.
end;

function TSilentMetaWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentMetaWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TMetaAnswer }

function TMetaAnswer.ErrorCode: Integer;
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

function TMetaAnswer.ReplyId: Integer;
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

function TMetaAnswer.ErrorData: TJSONObject;
var
  LValue: TJSONValue;
  LError, LData: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(Content);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LError := TJSONObject(LValue).GetValue('error');
    if not (LError is TJSONObject) then
      Exit;

    LData := TJSONObject(LError).GetValue('data');
    if LData is TJSONObject then
      Result := TJSONObject(LData.Clone);
  finally
    LValue.Free;
  end;
end;

{ TRequestMetaTest }

function TRequestMetaTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Strict;
end;

procedure TRequestMetaTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  ConfigureServer(Mode);
end;

procedure TRequestMetaTest.TearDown;
begin
  FServer.Free;
end;

procedure TRequestMetaTest.ConfigureServer(AMode: TMCPValidationLevel);
begin
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('request-meta-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      // These fixtures are about the body: the headers are checked by their own
      // middleware, and these requests carry none.
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(AMode)
    .BackToMCP
    .Tools
      .RegisterClass(TMetaEchoTool)
    .BackToMCP
  .ApplyConfig;
end;

function TRequestMetaTest.ListBody(const AParams: string): string;
begin
  Result := Format('{"jsonrpc":"2.0","id":5,"method":"tools/list","params":%s}',
    [AParams]);
end;

function TRequestMetaTest.Registered(AClass: TMiddlewareClass): Integer;
var
  LEntry: TMiddlewareEntry;
begin
  Result := 0;
  for LEntry in FServer.Middleware.EntriesFor(IRequestMiddleware) do
    if LEntry.MiddlewareClass = AClass then
      Inc(Result);
end;

function TRequestMetaTest.Send(const ABody: string): TMetaAnswer;
var
  LHandler: TMCPTransportHandler;
  LAnswer: TMetaAnswer;
begin
  LAnswer := Default(TMetaAnswer);

  LHandler := TMCPTransportHandler.Create(FServer, TSilentMetaWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := ABody;
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

{ TStrictMetaTest }

procedure TStrictMetaTest.TestMiddlewareIsRegisteredByDefault;
begin
  // Not opt-in: applying any MCP configuration is what puts it in
  Assert.IsTrue(FServer.Middleware.Contains(TMCPRequestMetaMiddleware));
end;

procedure TStrictMetaTest.TestMiddlewareIsRegisteredOnlyOnce;
begin
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetVersion('1.0.1')
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(1, Registered(TMCPRequestMetaMiddleware));
end;

procedure TStrictMetaTest.TestCompleteMetaIsAccepted;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{' + FullMeta + '}'));

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TStrictMetaTest.TestMissingMetaIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestMissingParamsIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  // No params member at all: there is nowhere for a _meta to be
  LAnswer := Send('{"jsonrpc":"2.0","id":5,"method":"tools/list"}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestMissingProtocolVersionIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{"_meta":{"io.modelcontextprotocol/clientCapabilities":{}}}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestBlankProtocolVersionIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"  ",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestMissingClientCapabilitiesIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  // An absent capabilities object and an empty one deserialize alike, so this
  // is the case that says the check reads the JSON and not the object
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestRefusalCarriesTheRequestId;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{}'));

  Assert.AreEqual(5, LAnswer.ReplyId, LAnswer.Content);
end;

procedure TStrictMetaTest.TestUnsupportedVersionIsRefused;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(MCP_UNSUPPORTED_PROTOCOL_VERSION, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestUnsupportedVersionNamesWhatItWantedAndWhatThereIs;
var
  LAnswer: TMetaAnswer;
  LData: TJSONObject;
begin
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}'));

  LData := LAnswer.ErrorData;
  Assert.IsNotNull(LData, 'the error must carry its data: ' + LAnswer.Content);
  try
    // The whole point of the payload: the client retries with a version from
    // the list rather than guessing again
    Assert.AreEqual('1900-01-01', LData.GetValue<string>('requested', ''));
    Assert.Contains(LData.ToJSON, ProtocolVersion);
  finally
    LData.Free;
  end;
end;

procedure TStrictMetaTest.TestDiscoverIsCheckedLikeAnyOtherMethod;
var
  LAnswer: TMetaAnswer;
begin
  // Tempting to exempt, since discover is how a client learns the versions -
  // but the specification exempts nothing, and the refusal names them anyway
  LAnswer := Send('{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{}}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(JRPC_INVALID_PARAMS, LAnswer.ErrorCode);
end;

procedure TStrictMetaTest.TestNonMCPNamespaceIsNotChecked;
var
  LAnswer: TMetaAnswer;
begin
  // A method outside the namespaces of the specification belongs to an API the
  // server registered next to MCP, and states no protocol version because
  // there is no protocol of ours for it to state. Nothing answers this one, so
  // what comes back is method-not-found - which is the proof that it was
  // dispatched rather than refused by the _meta check.
  LAnswer := Send('{"jsonrpc":"2.0","id":8,"method":"custom/echo","params":{}}');

  Assert.AreEqual(JRPC_METHOD_NOT_FOUND, LAnswer.ErrorCode, LAnswer.Content);
end;

procedure TStrictMetaTest.TestNotificationIsLetThrough;
var
  LAnswer: TMetaAnswer;
begin
  // A notification is not a request: it carries no id, is never answered, and
  // never reaches the request chain
  LAnswer := Send('{"jsonrpc":"2.0","method":"tools/list","params":{}}');

  Assert.IsFalse(LAnswer.Code = 400, LAnswer.Content);
end;

procedure TStrictMetaTest.TestMetaReachesTheToolThroughContext;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(Format(
    '{"jsonrpc":"2.0","id":6,"method":"tools/call","params":' +
    '{"name":"meta_echo","arguments":{},%s}}', [FullMeta]));

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, ProtocolVersion + '|probe',
    'the tool must see what the request said about itself');
end;

{ TLenientMetaTest }

function TLenientMetaTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Lenient;
end;

procedure TLenientMetaTest.TestMissingMetaIsTolerated;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{}'));

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TLenientMetaTest.TestMissingFieldsAreTolerated;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody('{"_meta":{}}'));

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
end;

procedure TLenientMetaTest.TestUnsupportedVersionIsStillRefused;
var
  LAnswer: TMetaAnswer;
begin
  // What Lenient tolerates is absence. A client that states a version this
  // server does not speak is wrong whether or not it had to state one.
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-06-18"}}'));

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(MCP_UNSUPPORTED_PROTOCOL_VERSION, LAnswer.ErrorCode);
end;

procedure TLenientMetaTest.TestMetaStillReachesTheTool;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(Format(
    '{"jsonrpc":"2.0","id":6,"method":"tools/call","params":' +
    '{"name":"meta_echo","arguments":{},%s}}', [FullMeta]));

  Assert.Contains(LAnswer.Content, ProtocolVersion + '|probe');
end;

{ TOffMetaTest }

function TOffMetaTest.Mode: TMCPValidationLevel;
begin
  Result := TMCPValidationLevel.Off;
end;

procedure TOffMetaTest.TestOffKeepsTheMiddlewareOutOfTheChain;
begin
  // Both checks are off in this fixture, so the chain is empty: a server that
  // wants none of it pays nothing for it
  Assert.AreEqual(0, FServer.Middleware.Count);
end;

procedure TOffMetaTest.TestNothingIsChecked;
var
  LAnswer: TMetaAnswer;
begin
  LAnswer := Send(ListBody(
    '{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01"}}'));

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, '"result"');
end;

initialization
  TDUnitX.RegisterTestFixture(TStrictMetaTest);
  TDUnitX.RegisterTestFixture(TLenientMetaTest);
  TDUnitX.RegisterTestFixture(TOffMetaTest);

end.
