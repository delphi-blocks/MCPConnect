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
///   The caching hints of MCP 2026-07-28: that every cacheable result carries
///   "ttlMs" and "cacheScope", that a server can say what they are - once for
///   itself, or per section - and that the results the specification excludes
///   carry none.
/// </summary>
unit MCPConnect.Tests.Transport.Caching;

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
  TSilentCacheWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>The two hints of a reply, and whether it carried them at all.</summary>
  TCacheAnswer = record
    Content: string;
    TtlMs: Int64;
    Scope: string;
    HasHints: Boolean;
  end;

  [TestFixture]
  TCachingTest = class(TObject)
  private const
    Meta =
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/clientCapabilities":{}}';
  private
    FServer: TMCPServer;

    /// <summary>A server with nothing configured about caching.</summary>
    procedure ConfigureServer;
    function Send(const AMethod: string; const AParams: string = ''): TCacheAnswer;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestEveryCacheableResultCarriesTheHints();
    [Test]
    procedure TestTheDefaultsAreTheConservativeOnes();

    [Test]
    procedure TestTheServerCanSayWhatTheyAre();
    [Test]
    procedure TestASectionOverridesTheServer();
    [Test]
    procedure TestASectionLeavesTheOthersAlone();

    [Test]
    procedure TestAToolCallCarriesNone();
    [Test]
    procedure TestAnMRTRRetryIsNotCacheable();
  end;

implementation

uses
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Mrtr;

type
  TCacheTools = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;

    [McpResource('doc', 'res://doc', 'text/plain', 'A resource')]
    function Doc: string;

    /// <summary>
    ///   Asks for input instead of answering, which is the interim result of an
    ///   MRTR round trip - and not a cacheable one.
    /// </summary>
    [McpResource('needs_input', 'res://needs-input', 'text/plain', 'Asks first')]
    function NeedsInput: TInputRequiredResult;
  end;

function TCacheTools.Ping: string;
begin
  Result := 'pong';
end;

function TCacheTools.Doc: string;
begin
  Result := 'the doc';
end;

function TCacheTools.NeedsInput: TInputRequiredResult;
begin
  Result := TInputRequiredResult.Create;
  Result.RequestState := 'a-state';
end;

{ TSilentCacheWriter }

procedure TSilentCacheWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests.
end;

function TSilentCacheWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentCacheWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TCachingTest }

procedure TCachingTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TCachingTest.ConfigureServer;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('caching-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      // Not what these tests are about, and these requests carry no headers
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TCacheTools)
    .BackToMCP
    .Resources
      .RegisterClass(TCacheTools)
    .BackToMCP
  .ApplyConfig;
end;

function TCachingTest.Send(const AMethod, AParams: string): TCacheAnswer;
var
  LHandler: TMCPTransportHandler;
  LAnswer: TCacheAnswer;
  LBody: string;
  LValue, LResult: TJSONValue;
begin
  LAnswer := Default(TCacheAnswer);

  if AParams.IsEmpty then
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"%s","params":{%s}}', [AMethod, Meta])
  else
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"%s","params":{%s,%s}}',
      [AMethod, AParams, Meta]);

  LHandler := TMCPTransportHandler.Create(FServer, TSilentCacheWriter.Create);
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
        LAnswer.Content := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  LValue := TJSONObject.ParseJSONValue(LAnswer.Content);
  if LValue is TJSONObject then
  try
    LResult := TJSONObject(LValue).GetValue('result');
    if LResult is TJSONObject then
    begin
      LAnswer.HasHints := Assigned(TJSONObject(LResult).GetValue('ttlMs')) and
                          Assigned(TJSONObject(LResult).GetValue('cacheScope'));
      LAnswer.TtlMs := TJSONObject(LResult).GetValue<Int64>('ttlMs', -1);
      LAnswer.Scope := TJSONObject(LResult).GetValue<string>('cacheScope', '');
    end;
  finally
    LValue.Free;
  end;

  Result := LAnswer;
end;

procedure TCachingTest.TestEveryCacheableResultCarriesTheHints;
const
  Cacheable: array[0..4] of string = (
    'server/discover', 'tools/list', 'prompts/list', 'resources/list',
    'resources/templates/list');
var
  LMethod: string;
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();

  // The six the specification names. They MUST carry both hints, whether or
  // not the server has an opinion about their values.
  for LMethod in Cacheable do
  begin
    LAnswer := Send(LMethod);
    Assert.IsTrue(LAnswer.HasHints, LMethod + ' carries no caching hints: ' + LAnswer.Content);
  end;

  LAnswer := Send('resources/read', '"uri":"res://doc"');
  Assert.IsTrue(LAnswer.HasHints, 'resources/read carries no caching hints: ' + LAnswer.Content);
end;

procedure TCachingTest.TestTheDefaultsAreTheConservativeOnes;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();

  // A library cannot know whether a list is the same for every caller, so an
  // unconfigured server promises nothing and shares nothing
  LAnswer := Send('tools/list');

  Assert.AreEqual(Int64(0), LAnswer.TtlMs, 'immediately stale');
  Assert.AreEqual('private', LAnswer.Scope);
end;

procedure TCachingTest.TestTheServerCanSayWhatTheyAre;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCacheHints(300000, TCacheScope.ScopePublic)
    .BackToMCP
  .ApplyConfig;

  LAnswer := Send('tools/list');
  Assert.AreEqual(Int64(300000), LAnswer.TtlMs);
  Assert.AreEqual('public', LAnswer.Scope);

  // Every section that has no hints of its own answers with the server's
  LAnswer := Send('prompts/list');
  Assert.AreEqual(Int64(300000), LAnswer.TtlMs);

  LAnswer := Send('server/discover');
  Assert.AreEqual(Int64(300000), LAnswer.TtlMs);
end;

procedure TCachingTest.TestASectionOverridesTheServer;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCacheHints(300000, TCacheScope.ScopePublic)
    .BackToMCP
    .Resources
      // A read is per caller far more often than a tool list is
      .SetCacheHints(1000, TCacheScope.ScopePrivate)
    .BackToMCP
  .ApplyConfig;

  LAnswer := Send('resources/read', '"uri":"res://doc"');
  Assert.AreEqual(Int64(1000), LAnswer.TtlMs);
  Assert.AreEqual('private', LAnswer.Scope);
end;

procedure TCachingTest.TestASectionLeavesTheOthersAlone;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Tools
      .SetCacheHints(60000, TCacheScope.ScopePublic)
    .BackToMCP
  .ApplyConfig;

  LAnswer := Send('tools/list');
  Assert.AreEqual(Int64(60000), LAnswer.TtlMs);
  Assert.AreEqual('public', LAnswer.Scope);

  // The others keep the defaults: one section speaks for itself only
  LAnswer := Send('prompts/list');
  Assert.AreEqual(Int64(0), LAnswer.TtlMs);
  Assert.AreEqual('private', LAnswer.Scope);
end;

procedure TCachingTest.TestAToolCallCarriesNone;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCacheHints(300000, TCacheScope.ScopePublic)
    .BackToMCP
  .ApplyConfig;

  // tools/call is not in the list of cacheable operations, and its result is
  // not a cacheable one: a call is an action, not a lookup
  LAnswer := Send('tools/call', '"name":"ping","arguments":{}');

  Assert.IsFalse(LAnswer.HasHints, LAnswer.Content);
  Assert.DoesNotContain(LAnswer.Content, 'ttlMs');
end;

procedure TCachingTest.TestAnMRTRRetryIsNotCacheable;
var
  LAnswer: TCacheAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetCacheHints(300000, TCacheScope.ScopePublic)
    .BackToMCP
  .ApplyConfig;

  // An interim answer is an input_required, which is not a cacheable result at
  // all and carries no hints
  LAnswer := Send('resources/read', '"uri":"res://needs-input"');
  Assert.IsFalse(LAnswer.HasHints, 'an interim result carries no hints: ' + LAnswer.Content);
  Assert.Contains(LAnswer.Content, 'input_required');

  // And a read that carries the state back answers with a real result which
  // must not be cached: what produced it is not part of the cache key, so
  // serving it again for the same uri would serve the wrong answer
  LAnswer := Send('resources/read', '"uri":"res://doc","requestState":"a-state"');
  Assert.IsTrue(LAnswer.HasHints, LAnswer.Content);
  Assert.AreEqual(Int64(0), LAnswer.TtlMs, 'the retry is not cacheable');
  Assert.AreEqual('private', LAnswer.Scope);
end;

initialization
  TDUnitX.RegisterTestFixture(TCachingTest);

end.
