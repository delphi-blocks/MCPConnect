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
///   Drives real requests through TMCPTransportHandler to cover the middleware
///   MCPConnect ships with (MCPConnect.MCP.Middleware.Default): that configuring
///   a feature registers the middleware implementing it, that each does what the
///   transport used to do inline, and that the two sit in the right order.
/// </summary>
unit MCPConnect.Tests.Transport.DefaultMiddleware;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.MCP,
  MCPConnect.JRPC.Middleware,
  MCPConnect.MCP.Middleware.Default,
  MCPConnect.MCP.Middleware.OAuth,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentDefaultWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a request answered, as far as these tests care.</summary>
  TTransportAnswer = record
    Code: Integer;
    Content: string;
    Challenge: string;
    AllowOrigin: string;
    AllowMethods: string;
    AllowHeaders: string;
    ExposeHeaders: string;
  end;

  /// <summary>
  ///   Common ground of the two fixtures: a server with one tool, and a way to
  ///   drive a request through the real handler.
  /// </summary>
  TDefaultMiddlewareTest = class(TObject)
  protected
    FServer: TMCPServer;

    /// <summary>
    ///   The server of these tests: nothing but a name and a tool, so that each
    ///   test configures the section it needs and nothing else.
    /// </summary>
    procedure ConfigureServer;
    function Send(const AShape: TMCPTransportRequestConverter): TTransportAnswer;

    /// <summary>
    ///   How many times AClass sits in the transport chain. Counted rather than
    ///   read off Middleware.Count: the chain of any configured server also
    ///   carries the request-metadata header check, which is not opt-in.
    /// </summary>
    function Registered(AClass: TMiddlewareClass): Integer;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();
  end;

  [TestFixture]
  TCORSMiddlewareTest = class(TDefaultMiddlewareTest)
  private const
    AllowedOrigin = 'https://app.example.com';
    ForeignOrigin = 'https://evil.example.net';
  private
    function SendWithOrigin(const ACommand, AOrigin: string;
      const APreflightMethod: string = ''): TTransportAnswer;
  public
    [Test]
    procedure TestSetCORSRegistersTheMiddleware();
    [Test]
    procedure TestSetAllowedOriginsRegistersTheMiddleware();
    [Test]
    procedure TestMiddlewareIsRegisteredOnlyOnce();
    [Test]
    procedure TestNoSecurityConfigLeavesTheChainEmpty();

    [Test]
    procedure TestAllowedOriginIsEchoedBack();
    [Test]
    procedure TestPreflightAnswersTheCORSHeaders();
    [Test]
    procedure TestForeignOriginIsRefused();
    [Test]
    procedure TestWildcardOriginIsAllowed();
    [Test]
    procedure TestNoAllowlistRefusesAForeignOrigin();
    [Test]
    procedure TestNoAllowlistAllowsALoopbackOrigin();
    [Test]
    procedure TestNoAllowlistAllowsTheServersOwnOrigin();
    [Test]
    procedure TestOriginPolicyOffLetsAnyOriginThrough();
    [Test]
    procedure TestMissingOriginIsRefusedWhenRequired();
    [Test]
    procedure TestCORSOffStillChecksTheOrigin();
  end;

  [TestFixture]
  TAuthTokenMiddlewareTest = class(TDefaultMiddlewareTest)
  private const
    Token = 'my-static-secret';
    CustomName = 'X-API-Key';
  private
    procedure EnableToken(ALocation: TAuthTokenLocation;
      const ACustomName: string = '');
    function SendWithHeader(const AName, AValue: string;
      const ACommand: string = 'POST'): TTransportAnswer;
  public
    [Test]
    procedure TestSetTokenRegistersTheMiddleware();
    [Test]
    procedure TestMiddlewareIsRegisteredOnlyOnce();
    [Test]
    procedure TestNoTokenConfiguredLeavesTheChainEmpty();

    [Test]
    procedure TestMatchingBearerTokenIsAccepted();
    [Test]
    procedure TestWrongBearerTokenIsRefused();
    [Test]
    procedure TestMissingTokenIsRefused();
    [Test]
    procedure TestTokenInACustomHeader();
    [Test]
    procedure TestTokenInACookie();
    [Test]
    procedure TestPreflightNeedsNoToken();
    [Test]
    procedure TestRefusalStillCarriesTheCORSHeaders();
  end;

  [TestFixture]
  TOAuthMiddlewareTest = class(TDefaultMiddlewareTest)
  private const
    AuthorizationServer = 'https://login.example.com';
    Resource = 'https://mcp.example.com/mcp';
    MetadataUrl = '/.well-known/oauth-protected-resource';
  private
    procedure EnableOAuth;
  public
    [Test]
    procedure TestAddAuthorizationServerRegistersTheMiddleware();
    [Test]
    procedure TestMiddlewareIsRegisteredOnlyOnce();
    [Test]
    procedure TestChallengeStillCarriesTheCORSHeaders();
    [Test]
    procedure TestMetadataIsServedThroughTheStaticTokenCheck();
  end;

implementation

uses
  MCPConnect.MCP.Attributes;

type
  TDemoTools = class(TObject)
  public
    [McpTool('ping', 'Answers something')]
    function Ping: string;
  end;

function TDemoTools.Ping: string;
begin
  Result := 'pong';
end;

const
  // What a conforming 2026-07-28 client sends: the per-request _meta with the
  // protocol version and the capabilities it declares. These tests are about
  // the other middleware, so the request is complete and passes the two that
  // check the protocol contract.
  DiscoverBody =
    '{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{"_meta":{' +
    '"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
    '"io.modelcontextprotocol/clientCapabilities":{}}}}';

{ TSilentDefaultWriter }

procedure TSilentDefaultWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentDefaultWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentDefaultWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TDefaultMiddlewareTest }

procedure TDefaultMiddlewareTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
end;

procedure TDefaultMiddlewareTest.TearDown;
begin
  FServer.Free;
end;

procedure TDefaultMiddlewareTest.ConfigureServer;
begin
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('default-middleware-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Tools
      .RegisterClass(TDemoTools)
    .BackToMCP
  .ApplyConfig;
end;

function TDefaultMiddlewareTest.Registered(AClass: TMiddlewareClass): Integer;
var
  LEntry: TMiddlewareEntry;
begin
  Result := 0;
  for LEntry in FServer.Middleware.EntriesFor(ITransportMiddleware) do
    if LEntry.MiddlewareClass = AClass then
      Inc(Result);
end;

function TDefaultMiddlewareTest.Send(
  const AShape: TMCPTransportRequestConverter): TTransportAnswer;
var
  LHandler: TMCPTransportHandler;
  LAnswer: TTransportAnswer;
begin
  LHandler := TMCPTransportHandler.Create(FServer, TSilentDefaultWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := DiscoverBody;

        // The request-metadata headers every POST has to carry since
        // 2026-07-28: these tests are about the other middleware, so they send
        // what a conforming client sends and let that one pass them through.
        ARequest.SetHeader('Mcp-Method', 'server/discover');
        ARequest.SetHeader('MCP-Protocol-Version', '2026-07-28');

        AShape(ARequest);
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LAnswer.Code := AResponse.Code;
        LAnswer.Content := AResponse.Content;
        LAnswer.AllowOrigin := AResponse.GetHeader('Access-Control-Allow-Origin');
        LAnswer.AllowMethods := AResponse.GetHeader('Access-Control-Allow-Methods');
        LAnswer.AllowHeaders := AResponse.GetHeader('Access-Control-Allow-Headers');
        LAnswer.ExposeHeaders := AResponse.GetHeader('Access-Control-Expose-Headers');
        LAnswer.Challenge := AResponse.GetHeader('WWW-Authenticate');
      end);
  finally
    LHandler.Free;
  end;

  Result := LAnswer;
end;

{ TCORSMiddlewareTest }

function TCORSMiddlewareTest.SendWithOrigin(const ACommand, AOrigin: string;
  const APreflightMethod: string): TTransportAnswer;
begin
  Result := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Command := ACommand;

      if not AOrigin.IsEmpty then
        ARequest.SetHeader('Origin', AOrigin);

      if not APreflightMethod.IsEmpty then
      begin
        ARequest.SetHeader('Access-Control-Request-Method', APreflightMethod);
        ARequest.SetHeader('Access-Control-Request-Headers', 'content-type, authorization');
      end;
    end);
end;

procedure TCORSMiddlewareTest.TestSetCORSRegistersTheMiddleware;
begin
  // The middleware is the origin check as much as it is the headers, and that
  // one is not opt-in: it is in the chain of any configured server. What
  // SetCORS turns on is the header injection, and registering is idempotent.
  ConfigureServer();
  Assert.AreEqual(1, Registered(TCORSMiddleware));

  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(1, Registered(TCORSMiddleware));
end;

procedure TCORSMiddlewareTest.TestSetAllowedOriginsRegistersTheMiddleware;
begin
  ConfigureServer();

  // The origin check is the middleware too, and it is configured on its own:
  // an allowlist without SetCORS still has to be enforced.
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetAllowedOrigins([AllowedOrigin])
    .BackToMCP
  .ApplyConfig;

  Assert.IsTrue(FServer.Middleware.Contains(TCORSMiddleware));
end;

procedure TCORSMiddlewareTest.TestMiddlewareIsRegisteredOnlyOnce;
begin
  ConfigureServer();

  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      .SetAllowedOrigins([AllowedOrigin])
      .SetRequireOrigin(True)
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(1, Registered(TCORSMiddleware));
end;

procedure TCORSMiddlewareTest.TestNoSecurityConfigLeavesTheChainEmpty;
begin
  ConfigureServer();

  // A server that says nothing about origins still gets the origin check: it
  // is what the Streamable HTTP transport requires of every server, and a
  // request from a page that has no business driving it must not be answered.
  // Turning it off is a deliberate SetOriginPolicy(Off).
  Assert.AreEqual(1, Registered(TCORSMiddleware));

  // And a server that turns it off pays nothing for it
  FServer.Free;
  FServer := TMCPServer.Create(nil);
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(0, Registered(TCORSMiddleware));
end;

procedure TCORSMiddlewareTest.TestAllowedOriginIsEchoedBack;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      .SetAllowedOrigins([AllowedOrigin])
    .BackToMCP
  .ApplyConfig;

  LAnswer := SendWithOrigin('POST', AllowedOrigin);

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code);
  Assert.AreEqual(AllowedOrigin, LAnswer.AllowOrigin);
  Assert.AreEqual('WWW-Authenticate', LAnswer.ExposeHeaders);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TCORSMiddlewareTest.TestPreflightAnswersTheCORSHeaders;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      .SetAllowedOrigins([AllowedOrigin])
    .BackToMCP
  .ApplyConfig;

  // The reason CORS is a transport middleware: a preflight carries no
  // JSON-RPC message, so no hook of the message levels would ever see it.
  LAnswer := SendWithOrigin('OPTIONS', AllowedOrigin, 'POST');

  Assert.AreEqual(HTTP_CODE_NOCONTENT, LAnswer.Code);
  Assert.AreEqual(AllowedOrigin, LAnswer.AllowOrigin);
  Assert.AreEqual('POST', LAnswer.AllowMethods);
  Assert.AreEqual('content-type, authorization', LAnswer.AllowHeaders);
end;

procedure TCORSMiddlewareTest.TestForeignOriginIsRefused;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      .SetAllowedOrigins([AllowedOrigin])
    .BackToMCP
  .ApplyConfig;

  LAnswer := SendWithOrigin('POST', ForeignOrigin);

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, LAnswer.Code);
  Assert.DoesNotContain(LAnswer.Content, '"result"');
end;

procedure TCORSMiddlewareTest.TestWildcardOriginIsAllowed;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetAllowedOrigins(['https://*.example.com'])
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(HTTP_CODE_OK, SendWithOrigin('POST', 'https://sub.example.com').Code);

  // The bare domain is not what the pattern says, and it is not let through.
  LAnswer := SendWithOrigin('POST', 'https://example.com');
  Assert.AreEqual(HTTP_CODE_FORBIDDEN, LAnswer.Code);
end;

procedure TCORSMiddlewareTest.TestNoAllowlistRefusesAForeignOrigin;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();

  // The DNS-rebinding case: a page on someone else's site, driving this server
  // through the browser of its user. It carries its own domain in the Origin
  // whatever that domain resolves to, and that is what gives it away.
  LAnswer := SendWithOrigin('POST', ForeignOrigin);

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, LAnswer.Code, LAnswer.Content);
end;

procedure TCORSMiddlewareTest.TestNoAllowlistAllowsALoopbackOrigin;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();

  // A local page - an inspector, a developer's own tooling - is not what the
  // check defends against, and refusing it would make local work impossible
  Assert.AreEqual(HTTP_CODE_OK, SendWithOrigin('POST', 'http://localhost:3000').Code);
  Assert.AreEqual(HTTP_CODE_OK, SendWithOrigin('POST', 'http://127.0.0.1:5173').Code);

  LAnswer := SendWithOrigin('POST', 'http://[::1]:8080');
  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code, LAnswer.Content);
end;

procedure TCORSMiddlewareTest.TestNoAllowlistAllowsTheServersOwnOrigin;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();

  LAnswer := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader('Origin', 'https://mcp.example.com');
      ARequest.SetHeader('Host', 'mcp.example.com');
    end);

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code, LAnswer.Content);
end;

procedure TCORSMiddlewareTest.TestOriginPolicyOffLetsAnyOriginThrough;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      // What the library did before the check had a default: for a deployment
      // no browser can reach, or behind something that rewrites the header.
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
  .ApplyConfig;

  LAnswer := SendWithOrigin('POST', ForeignOrigin);

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(ForeignOrigin, LAnswer.AllowOrigin);
end;

procedure TCORSMiddlewareTest.TestMissingOriginIsRefusedWhenRequired;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetAllowedOrigins([AllowedOrigin])
      .SetRequireOrigin(True)
    .BackToMCP
  .ApplyConfig;

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, SendWithOrigin('POST', '').Code,
    'no Origin header, and one is required');
  Assert.AreEqual(HTTP_CODE_OK, SendWithOrigin('POST', AllowedOrigin).Code);
end;

procedure TCORSMiddlewareTest.TestCORSOffStillChecksTheOrigin;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(False)
      .SetAllowedOrigins([AllowedOrigin])
    .BackToMCP
  .ApplyConfig;

  // CORS off means no headers written, not an allowlist that stops being one.
  LAnswer := SendWithOrigin('POST', AllowedOrigin);
  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code);
  Assert.AreEqual('', LAnswer.AllowOrigin);

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, SendWithOrigin('POST', ForeignOrigin).Code);
end;

{ TAuthTokenMiddlewareTest }

procedure TAuthTokenMiddlewareTest.EnableToken(ALocation: TAuthTokenLocation;
  const ACustomName: string);
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken(Token)
    .SetTokenLocation(ALocation)
    .SetTokenCustomHeader(ACustomName)
  .ApplyConfig;
end;

function TAuthTokenMiddlewareTest.SendWithHeader(const AName, AValue: string;
  const ACommand: string): TTransportAnswer;
begin
  Result := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Command := ACommand;
      if not AName.IsEmpty then
        ARequest.SetHeader(AName, AValue);
    end);
end;

procedure TAuthTokenMiddlewareTest.TestSetTokenRegistersTheMiddleware;
begin
  ConfigureServer();
  Assert.IsFalse(FServer.Middleware.Contains(TAuthTokenMiddleware),
    'nothing registered before a token is set');

  EnableToken(TAuthTokenLocation.Bearer);

  Assert.IsTrue(FServer.Middleware.Contains(TAuthTokenMiddleware));
end;

procedure TAuthTokenMiddlewareTest.TestMiddlewareIsRegisteredOnlyOnce;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Bearer);
  EnableToken(TAuthTokenLocation.Bearer);

  Assert.AreEqual(1, Registered(TAuthTokenMiddleware));
end;

procedure TAuthTokenMiddlewareTest.TestNoTokenConfiguredLeavesTheChainEmpty;
begin
  ConfigureServer();

  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenLocation(TAuthTokenLocation.Bearer)
  .ApplyConfig;

  // The token is what turns the check on: saying where it would be read from
  // is not configuring one.
  Assert.IsFalse(FServer.Middleware.Contains(TAuthTokenMiddleware));
end;

procedure TAuthTokenMiddlewareTest.TestMatchingBearerTokenIsAccepted;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Bearer);

  LAnswer := SendWithHeader('Authorization', 'Bearer ' + Token);

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code);
  Assert.Contains(LAnswer.Content, '"result"');
end;

procedure TAuthTokenMiddlewareTest.TestWrongBearerTokenIsRefused;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Bearer);

  LAnswer := SendWithHeader('Authorization', 'Bearer not-the-token');

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, LAnswer.Code);
  Assert.DoesNotContain(LAnswer.Content, '"result"');
end;

procedure TAuthTokenMiddlewareTest.TestMissingTokenIsRefused;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Bearer);

  Assert.AreEqual(HTTP_CODE_FORBIDDEN, SendWithHeader('', '').Code);
end;

procedure TAuthTokenMiddlewareTest.TestTokenInACustomHeader;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Header, CustomName);

  Assert.AreEqual(HTTP_CODE_OK, SendWithHeader(CustomName, Token).Code);
  Assert.AreEqual(HTTP_CODE_FORBIDDEN, SendWithHeader(CustomName, 'wrong').Code);

  // The right value in the wrong place is no better than the wrong value.
  Assert.AreEqual(HTTP_CODE_FORBIDDEN,
    SendWithHeader('Authorization', 'Bearer ' + Token).Code);
end;

procedure TAuthTokenMiddlewareTest.TestTokenInACookie;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Cookie, 'SessionId');

  Assert.AreEqual(HTTP_CODE_OK, SendWithHeader('Cookie', 'SessionId=' + Token).Code);
  Assert.AreEqual(HTTP_CODE_FORBIDDEN, SendWithHeader('Cookie', 'SessionId=wrong').Code);
end;

procedure TAuthTokenMiddlewareTest.TestPreflightNeedsNoToken;
begin
  ConfigureServer();
  EnableToken(TAuthTokenLocation.Bearer);

  // A preflight carries no credentials by definition: asking it for one would
  // refuse every browser client before its real request is ever sent.
  Assert.AreEqual(HTTP_CODE_NOCONTENT, SendWithHeader('', '', 'OPTIONS').Code);
end;

procedure TAuthTokenMiddlewareTest.TestRefusalStillCarriesTheCORSHeaders;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
    .BackToMCP
  .ApplyConfig;
  EnableToken(TAuthTokenLocation.Bearer);

  LAnswer := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader('Origin', 'https://app.example.com');
      ARequest.SetHeader('Authorization', 'Bearer not-the-token');
    end);

  // CORS wraps authentication, whatever order the two are configured in:
  // without the headers on the refusal the browser reports a CORS failure
  // instead of the 403.
  Assert.AreEqual(HTTP_CODE_FORBIDDEN, LAnswer.Code);
  Assert.AreEqual('https://app.example.com', LAnswer.AllowOrigin);
end;

{ TOAuthMiddlewareTest }

procedure TOAuthMiddlewareTest.EnableOAuth;
begin
  FServer.Plugin.Configure<IOAuthConfig>
    .SetResource(Resource)
    .AddAuthorizationServer(AuthorizationServer)
  .ApplyConfig;
end;

procedure TOAuthMiddlewareTest.TestAddAuthorizationServerRegistersTheMiddleware;
var
  LOAuth: IOAuthConfig;
begin
  ConfigureServer();

  // A resource on its own enforces nothing - ApplyConfig refuses that
  // configuration outright - so it registers nothing either. The chain is
  // walked a step at a time here to look in between.
  LOAuth := FServer.Plugin.Configure<IOAuthConfig>;
  LOAuth.SetResource(Resource);
  Assert.IsFalse(FServer.Middleware.Contains(TOAuthMiddleware),
    'a resource without an authorization server enforces nothing');

  LOAuth
    .AddAuthorizationServer(AuthorizationServer)
  .ApplyConfig;
  Assert.IsTrue(FServer.Middleware.Contains(TOAuthMiddleware));
end;

procedure TOAuthMiddlewareTest.TestMiddlewareIsRegisteredOnlyOnce;
begin
  ConfigureServer();

  FServer.Plugin.Configure<IOAuthConfig>
    .SetResource(Resource)
    .AddAuthorizationServer(AuthorizationServer)
    .AddAuthorizationServer('https://login.other.example.com')
  .ApplyConfig;

  Assert.AreEqual(1, Registered(TOAuthMiddleware));
end;

procedure TOAuthMiddlewareTest.TestChallengeStillCarriesTheCORSHeaders;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  FServer.Plugin.Configure<IMCPConfig>
    .Security
      .SetCORS(True)
      // Named, so that the origin check lets it through and what this test
      // observes is the challenge rather than a 403
      .SetAllowedOrigins(['https://app.example.com'])
    .BackToMCP
  .ApplyConfig;
  EnableOAuth();

  LAnswer := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader('Origin', 'https://app.example.com');
    end);

  // A browser client reads the metadata URL out of this challenge, so both the
  // challenge and the headers letting it be read have to be there.
  Assert.AreEqual(HTTP_CODE_UNAUTHORIZED, LAnswer.Code);
  Assert.IsNotEmpty(LAnswer.Challenge);
  Assert.AreEqual('https://app.example.com', LAnswer.AllowOrigin);
  Assert.AreEqual('WWW-Authenticate', LAnswer.ExposeHeaders);
end;

procedure TOAuthMiddlewareTest.TestMetadataIsServedThroughTheStaticTokenCheck;
var
  LAnswer: TTransportAnswer;
begin
  ConfigureServer();
  EnableOAuth();
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken('a-static-token')
    .SetTokenLocation(TAuthTokenLocation.Bearer)
  .ApplyConfig;

  // Both checks are in the chain, and the discovery document has to come out of
  // it: a client reads it precisely because it has no token yet.
  LAnswer := Send(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Command := 'GET';
      ARequest.Url := MetadataUrl;
    end);

  Assert.AreEqual(HTTP_CODE_OK, LAnswer.Code);
  Assert.Contains(LAnswer.Content, AuthorizationServer);
end;

initialization
  TDUnitX.RegisterTestFixture(TCORSMiddlewareTest);
  TDUnitX.RegisterTestFixture(TAuthTokenMiddlewareTest);
  TDUnitX.RegisterTestFixture(TOAuthMiddlewareTest);

end.
