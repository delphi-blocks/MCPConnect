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
///   "io.modelcontextprotocol/serverInfo" in the "_meta" of every result, which
///   is how a client learns what answered it now that there is no handshake to
///   have asked. Covers the three things that make it useful: that every
///   operation carries it, that a server with nothing to say writes nothing
///   rather than an empty name, and that a server can be configured to withhold
///   it.
/// </summary>
unit MCPConnect.Tests.Transport.ServerInfo;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentInfoWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  [TestFixture]
  TServerInfoTest = class(TObject)
  private const
    ServerName = 'info-test';
    ServerVersion = '4.2.0';
    Meta =
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/clientCapabilities":{}}';
  private
    FServer: TMCPServer;

    /// <summary>
    ///   A server with the tools, resources, templates, prompts and completion
    ///   providers the tests need something registered for.
    /// </summary>
    procedure ConfigureServer(const AName: string = ServerName;
      ASendServerInfo: Boolean = True);

    function Send(const AMethod: string; const AParams: string = ''): string;

    /// <summary>
    ///   The "io.modelcontextprotocol/serverInfo" of the reply, or nil when it
    ///   carries none. The caller owns it.
    /// </summary>
    function ServerInfoOf(const AContent: string): TJSONObject;

    /// <summary>
    ///   Fails unless the answer to AMethod names this server.
    /// </summary>
    procedure AssertSaysWhoAnswered(const AMethod: string;
      const AParams: string = '');
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestEveryResultSaysWhoAnswered();
    [Test]
    procedure TestTheUnchainedOperationsSayItToo();

    [Test]
    procedure TestAnUnnamedServerWritesNothing();
    [Test]
    procedure TestItCanBeWithheld();
    [Test]
    procedure TestAnErrorCarriesNone();
  end;

implementation

uses
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Prompts;

type
  TInfoTools = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;

    [McpResource('doc', 'res://doc', 'text/plain', 'A resource')]
    function Doc: string;

    [McpTemplate('item', 'res://item/{id}')]
    function Item([McpParam('id', 'Which item')] const id: string): string;

    [McpPrompt('greet', 'Greet', 'Greets someone')]
    function Greet([McpArgument('who', 'Who to greet')] const who: string): TPromptMessages;

    [McpComplete('greet', 'who')]
    function CompleteWho(const AValue: string): TArray<string>;
  end;

function TInfoTools.Ping: string;
begin
  Result := 'pong';
end;

function TInfoTools.Doc: string;
begin
  Result := 'the doc';
end;

function TInfoTools.Item(const id: string): string;
begin
  Result := 'item ' + id;
end;

function TInfoTools.Greet(const who: string): TPromptMessages;
begin
  Result := TPromptMessages.Create;
end;

function TInfoTools.CompleteWho(const AValue: string): TArray<string>;
begin
  Result := ['world'];
end;

{ TSilentInfoWriter }

procedure TSilentInfoWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests.
end;

function TSilentInfoWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentInfoWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TServerInfoTest }

procedure TServerInfoTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TServerInfoTest.ConfigureServer(const AName: string;
  ASendServerInfo: Boolean);
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName(AName)
      .SetVersion(ServerVersion)
      .SetSendServerInfo(ASendServerInfo)
    .BackToMCP
    .Security
      // Not what these tests are about, and these requests carry no headers
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TInfoTools)
    .BackToMCP
    .Resources
      .RegisterClass(TInfoTools)
    .BackToMCP
    .Prompts
      .RegisterClass(TInfoTools)
    .BackToMCP
    .Completions
      .RegisterClass(TInfoTools)
    .BackToMCP
  .ApplyConfig;
end;

function TServerInfoTest.Send(const AMethod, AParams: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
  LBody: string;
begin
  if AParams.IsEmpty then
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"%s","params":{%s}}', [AMethod, Meta])
  else
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"%s","params":{%s,%s}}',
      [AMethod, AParams, Meta]);

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TSilentInfoWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
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

function TServerInfoTest.ServerInfoOf(const AContent: string): TJSONObject;
var
  LValue: TJSONValue;
  LResult, LMeta, LInfo: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(AContent);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LResult := TJSONObject(LValue).GetValue('result');
    if not (LResult is TJSONObject) then
      Exit;

    LMeta := TJSONObject(LResult).GetValue('_meta');
    if not (LMeta is TJSONObject) then
      Exit;

    LInfo := TJSONObject(LMeta).GetValue('io.modelcontextprotocol/serverInfo');
    if LInfo is TJSONObject then
      Result := TJSONObject(LInfo.Clone);
  finally
    LValue.Free;
  end;
end;

procedure TServerInfoTest.AssertSaysWhoAnswered(const AMethod, AParams: string);
var
  LContent: string;
  LInfo: TJSONObject;
begin
  LContent := Send(AMethod, AParams);

  LInfo := ServerInfoOf(LContent);
  Assert.IsNotNull(LInfo, AMethod + ' answered without a serverInfo: ' + LContent);
  try
    Assert.AreEqual(ServerName, LInfo.GetValue<string>('name', ''), AMethod);
    Assert.AreEqual(ServerVersion, LInfo.GetValue<string>('version', ''), AMethod);
  finally
    LInfo.Free;
  end;
end;

procedure TServerInfoTest.TestEveryResultSaysWhoAnswered;
begin
  ConfigureServer();

  AssertSaysWhoAnswered('server/discover');
  AssertSaysWhoAnswered('tools/list');
  AssertSaysWhoAnswered('tools/call', '"name":"ping","arguments":{}');
  AssertSaysWhoAnswered('resources/list');
  AssertSaysWhoAnswered('resources/read', '"uri":"res://doc"');
  AssertSaysWhoAnswered('prompts/list');
  AssertSaysWhoAnswered('prompts/get', '"name":"greet","arguments":{"who":"world"}');
end;

procedure TServerInfoTest.TestTheUnchainedOperationsSayItToo;
begin
  ConfigureServer();

  // These three run no middleware chain of their own, so a result-stamping
  // middleware would have left exactly them unsigned. subscriptions/listen is
  // the pointed one: its result carries a _meta of its own, which must not
  // lose the inherited member.
  AssertSaysWhoAnswered('resources/templates/list');
  AssertSaysWhoAnswered('completion/complete',
    '"ref":{"type":"ref/prompt","name":"greet"},"argument":{"name":"who","value":"w"}');
  AssertSaysWhoAnswered('subscriptions/listen', '"notifications":{"toolsListChanged":true}');
end;

procedure TServerInfoTest.TestAnUnnamedServerWritesNothing;
var
  LContent: string;
begin
  // A server that was never named has nothing to report, and reporting an
  // empty name would tell a client something untrue
  ConfigureServer('');

  LContent := Send('tools/list');

  Assert.IsNull(ServerInfoOf(LContent), LContent);
  Assert.DoesNotContain(LContent, 'serverInfo');
end;

procedure TServerInfoTest.TestItCanBeWithheld;
var
  LContent: string;
begin
  // "unless specifically configured not to do so"
  ConfigureServer(ServerName, False);

  LContent := Send('tools/list');

  Assert.IsNull(ServerInfoOf(LContent), LContent);
  Assert.Contains(LContent, '"result"', 'and the result itself is unaffected');
end;

procedure TServerInfoTest.TestAnErrorCarriesNone;
var
  LContent: string;
begin
  ConfigureServer();

  // _meta belongs to a result, and an error object has none: there is nothing
  // to stamp, and nothing that pretends otherwise
  LContent := Send('tools/call', '"name":"nosuch","arguments":{}');

  Assert.Contains(LContent, '"error"');
  Assert.DoesNotContain(LContent, 'serverInfo');
end;

initialization
  TDUnitX.RegisterTestFixture(TServerInfoTest);

end.
