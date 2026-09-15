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
///   The legacy compatibility plugin: what a server answers a 2025-06-18
///   client once it is enabled - the initialize handshake and the relaxed
///   validation - and that a server which never enabled it answers exactly as
///   it did before.
/// </summary>
unit MCPConnect.Tests.Configuration.Legacy;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.Legacy,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentLegacyWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TLegacyConfigTest = class(TObject)
  private
    FServer: TMCPServer;

    /// <summary>
    ///   A server with one tool, and the legacy plugin enabled or not. The
    ///   validation levels are left at their default (Strict) when the plugin
    ///   is enabled - relaxing them is what is under test - and turned off
    ///   when it is not, so that a request with no headers reaches the
    ///   dispatcher and is answered by it.
    /// </summary>
    procedure Configure(AEnableLegacy: Boolean);

    /// <summary>
    ///   The body a POST of ABody is answered with, exactly as the transport
    ///   wrote it.
    /// </summary>
    function Post(const ABody: string): string;

    /// <summary>The "result" of a response. The caller owns it.</summary>
    function ResultOf(const AResponse: string): TJSONObject;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestInitializeIsUnknownWithoutThePlugin();

    [Test]
    procedure TestInitializeAnswersTheLegacyVersion();
    [Test]
    procedure TestTheNewerLegacyVersionIsEchoedBack();
    [Test]
    procedure TestAnUnknownVersionFallsBackToTheOldest();
    [Test]
    procedure TestInitializeReportsTheServerIdentity();
    [Test]
    procedure TestInitializeCapabilitiesFollowTheRegistry();
    [Test]
    procedure TestTheResultCarriesNoMeta();
    [Test]
    procedure TestInitializedIsAccepted();

    [Test]
    procedure TestValidationIsRelaxedToLenient();
    [Test]
    procedure TestARequestWithNoHeadersIsAnswered();
    [Test]
    procedure TestTheNotificationsNamespaceStillResolves();
  end;

implementation

uses
  MCPConnect.MCP.Attributes;

type
  TLegacyTestTool = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;
  end;

function TLegacyTestTool.Ping: string;
begin
  Result := 'pong';
end;

{ TSilentLegacyWriter }

procedure TSilentLegacyWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentLegacyWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentLegacyWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TLegacyConfigTest }

procedure TLegacyConfigTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TLegacyConfigTest.Configure(AEnableLegacy: Boolean);
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('legacy-test')
      .SetVersion('1.2.3')
    .BackToMCP
    .Security
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TLegacyTestTool)
    .BackToMCP
  .ApplyConfig;

  if AEnableLegacy then
  begin
    FServer.Plugin.Configure<IMCPLegacyConfig>
      .SetEnabled(True)
      .SetLogWarning(False)
    .ApplyConfig;

    Exit;
  end;

  // No plugin: the request-metadata contracts would refuse a legacy request
  // before the dispatcher ever saw its method, which is not what these tests
  // are about
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
    .BackToMCP
  .ApplyConfig;
end;

function TLegacyConfigTest.Post(const ABody: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
begin
  LContent := '';

  LHandler := TMCPTransportHandler.Create(FServer, TSilentLegacyWriter.Create);
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
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

function TLegacyConfigTest.ResultOf(const AResponse: string): TJSONObject;
var
  LValue, LResult: TJSONValue;
begin
  LValue := TJSONObject.ParseJSONValue(AResponse);
  Assert.IsTrue(LValue is TJSONObject, 'the server must answer JSON: ' + AResponse);
  try
    LResult := TJSONObject(LValue).GetValue('result');
    Assert.IsTrue(LResult is TJSONObject, 'the server must answer a result: ' + AResponse);
    Result := LResult.Clone as TJSONObject;
  finally
    LValue.Free;
  end;
end;

procedure TLegacyConfigTest.TestInitializeIsUnknownWithoutThePlugin;
var
  LResponse: string;
begin
  Configure(False);

  LResponse := Post('{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}');

  // -32601, Method not found: 2026-07-28 removed the handshake, and a server
  // that did not ask for the compatibility keeps saying so
  Assert.Contains(LResponse, '-32601', True, LResponse);
end;

procedure TLegacyConfigTest.TestInitializeAnswersTheLegacyVersion;
var
  LResult: TJSONObject;
begin
  Configure(True);

  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2025-06-18","capabilities":{},' +
    '"clientInfo":{"name":"legacy-client","version":"0.9"}}}'));
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_06_18,
      LResult.GetValue<string>('protocolVersion', ''), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestTheNewerLegacyVersionIsEchoedBack;
var
  LResult: TJSONObject;
begin
  Configure(True);

  // Both revisions the handshake answers for are echoed: a client is told it
  // got the one it asked for
  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2025-11-25","capabilities":{}}}'));
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_11_25,
      LResult.GetValue<string>('protocolVersion', ''), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestAnUnknownVersionFallsBackToTheOldest;
var
  LResult: TJSONObject;
begin
  Configure(True);

  // A revision this plugin knows nothing about is answered with the oldest one
  // it does, rather than echoed back
  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2024-11-05","capabilities":{}}}'));
  try
    Assert.AreEqual(MCP_LEGACY_DEFAULT_VERSION,
      LResult.GetValue<string>('protocolVersion', ''), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestInitializeReportsTheServerIdentity;
var
  LResult: TJSONObject;
begin
  Configure(True);

  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2025-06-18","capabilities":{}}}'));
  try
    Assert.AreEqual('legacy-test',
      LResult.GetValue<string>('serverInfo.name', ''), LResult.ToJSON);
    Assert.AreEqual('1.2.3',
      LResult.GetValue<string>('serverInfo.version', ''), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestInitializeCapabilitiesFollowTheRegistry;
var
  LResult: TJSONObject;
  LCapabilities: TJSONValue;
begin
  Configure(True);

  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2025-06-18","capabilities":{}}}'));
  try
    LCapabilities := LResult.GetValue('capabilities');
    Assert.IsTrue(LCapabilities is TJSONObject, LResult.ToJSON);

    // One tool is registered, and presence is the declaration
    Assert.IsNotNull(TJSONObject(LCapabilities).GetValue('tools'), LResult.ToJSON);
    Assert.IsNull(TJSONObject(LCapabilities).GetValue('prompts'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestTheResultCarriesNoMeta;
var
  LResult: TJSONObject;
begin
  Configure(True);

  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":1,"method":"initialize",' +
    '"params":{"protocolVersion":"2025-06-18","capabilities":{}}}'));
  try
    // The "_meta" of a result is 2026-07-28: a legacy client has no use for it
    Assert.IsNull(LResult.GetValue('_meta'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestInitializedIsAccepted;
var
  LResponse: string;
begin
  Configure(True);

  LResponse := Post('{"jsonrpc":"2.0","method":"notifications/initialized"}');

  // A notification is answered with nothing at all, and above all not with
  // Method not found
  Assert.DoesNotContain(LResponse, 'error', True, LResponse);
end;

procedure TLegacyConfigTest.TestValidationIsRelaxedToLenient;
var
  LConfig: IMCPConfig;
begin
  Configure(True);

  LConfig := FServer.Plugin.Configure<IMCPConfig>;

  // Lenient and not Off: a header that contradicts its body is still refused
  Assert.AreEqual(Ord(TMCPValidationLevel.Lenient),
    Ord(LConfig.Security.HeaderValidation), 'header validation');
  Assert.AreEqual(Ord(TMCPValidationLevel.Lenient),
    Ord(LConfig.Security.MetaValidation), 'meta validation');
end;

procedure TLegacyConfigTest.TestARequestWithNoHeadersIsAnswered;
var
  LResult: TJSONObject;
begin
  Configure(True);

  // No request-metadata headers and no "_meta": what a 2025-06-18 client sends,
  // and what the default Strict validation refuses
  LResult := ResultOf(Post('{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'));
  try
    Assert.IsNotNull(LResult.GetValue('tools'), LResult.ToJSON);
  finally
    LResult.Free;
  end;
end;

procedure TLegacyConfigTest.TestTheNotificationsNamespaceStillResolves;
var
  LResponse: string;
begin
  Configure(True);

  // The legacy class is flat, so "notifications/initialized" is registered
  // under that name alone and does not take the namespace over: what the
  // built-in class answers for is still answered by it
  LResponse := Post('{"jsonrpc":"2.0","method":"notifications/subscriptions/acknowledged",' +
    '"params":{"notifications":{}}}');

  Assert.DoesNotContain(LResponse, '-32601', True, LResponse);
end;

end.
