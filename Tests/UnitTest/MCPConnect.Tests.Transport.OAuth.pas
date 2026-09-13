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
///   Drives TMCPTransportHandler the way a transport does - one handler per request,
///   a request converter in, a response converter out - so that the OAuth decisions it
///   makes before any JSON-RPC handling are covered: which paths serve the well-known
///   documents, what a request without a token is answered with, and which requests
///   are exempt.
/// </summary>
/// <remarks>
///   These are the checks that live in the transport rather than in the configuration
///   or the validators, and they were unreachable from the test project until
///   MCPConnect.Transport.Base was added to it.
/// </remarks>
unit MCPConnect.Tests.Transport.OAuth;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  DUnitX.TestFramework,

  IdContext, IdHTTPServer, IdCustomHTTPServer,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Security.Token,
  MCPConnect.Transport.Base,
  MCPConnect.MCP.Types.Base,
  JRPC.Core,
  MCPConnect.MCP.Server;

type
  /// <summary>
  ///   A response writer that streams nothing, which is what the WebBroker and plain
  ///   HTTP paths look like: everything comes back through the response converter.
  /// </summary>
  TStubTransportWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a request produced, copied out before the handler is destroyed.</summary>
  TTransportOutcome = record
    Code: Integer;
    Content: string;
    ContentType: string;
    Challenge: string;
    HasChallenge: Boolean;

    /// <summary>The value of an auth-param of the challenge, without its quotes.</summary>
    function ChallengeParam(const AName: string): string;
  end;

  /// <summary>
  ///   A validator that accepts one exact token and rejects everything else, so that
  ///   the transport's own branching is what a test observes rather than any real
  ///   token parsing.
  /// </summary>
  TStubTokenValidator = class(TInterfacedObject, ITokenValidator)
  public const
    GoodToken = 'a-token-this-validator-accepts';
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult;
  end;

  /// <summary>
  ///   A loopback HTTP server standing in for an authorization server. It publishes
  ///   one OIDC discovery document per path, so a single process can host two
  ///   upstreams that are told apart by what they say - which is what the proxy
  ///   cache has to keep separate.
  /// </summary>
  TFakeUpstream = class
  private
    FServer: TIdHTTPServer;
    FDocuments: TDictionary<string, string>;
    FHits: Integer;
    procedure CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>The issuer URL of an authorization server hosted under APath.</summary>
    function BaseUrl(const APath: string): string;

    /// <summary>
    ///   Publishes a discovery document at ADocumentPath declaring AIssuer. Every
    ///   endpoint in it names that issuer, so a document served for the wrong upstream
    ///   is recognisable at a glance. The two are separate arguments because which URL
    ///   a document is published at, and which issuer it claims, are exactly what the
    ///   discovery walk and the RFC 8414 section 3.3 check are about.
    /// </summary>
    procedure Publish(const ADocumentPath, AIssuer: string);

    /// <summary>
    ///   Publishes the discovery document of the authorization server at APath, under
    ///   the OpenID Connect path-appending URL.
    /// </summary>
    procedure PublishIssuer(const APath: string);

    /// <summary>How many requests have reached it: what tells a cache hit apart.</summary>
    property Hits: Integer read FHits;
  end;

  [TestFixture]
  TTransportOAuthTest = class(TObject)
  private const
    // Deliberately not mounted at "/mcp": the route this fixture covers used to assume
    // that path, so a resource that sits anywhere else is what tells the two apart.
    Resource = 'https://mcp.example.com/api/mcp';
    ResourcePath = '/api/mcp';
    Issuer = 'https://idp.example.com';
    MetadataPath = '/.well-known/oauth-protected-resource/api/mcp';
  private
    FServer: TMCPServer;
    function Execute(const AMethod, AUrl: string): TTransportOutcome; overload;
    function Execute(const AMethod, AUrl, AAuthorization: string;
      AProtocol: TTransportProtocol = TTransportProtocol.StreamableHTTP): TTransportOutcome; overload;
    function Execute(const AMethod, AUrl, AHeaderName, AHeaderValue: string;
      AProtocol: TTransportProtocol): TTransportOutcome; overload;
    procedure EnableOAuth;
    procedure EnableStaticToken(const AToken: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestMetadata_IsServedAtThePathInsertionUrl;
    [Test]
    procedure TestMetadata_IsServedAtTheBareWellKnownPath;
    [Test]
    procedure TestMetadata_IsNotServedAtTheOldHardcodedPath;
    [Test]
    procedure TestMetadata_NeedsNoToken;
    [Test]
    procedure TestMetadata_IsNotServedForPost;

    [Test]
    procedure TestRequestWithoutAToken_IsChallenged;
    [Test]
    procedure TestChallenge_QuotesTheMetadataUrl;
    [Test]
    procedure TestRequestWithARejectedToken_ReportsInvalidToken;
    [Test]
    procedure TestRequestWithAnAcceptedToken_IsNotChallenged;

    [Test]
    procedure TestOptions_IsExemptFromTheTokenCheck;
    [Test]
    procedure TestStdio_IsExemptFromTheTokenCheck;
    [Test]
    procedure TestWithoutAnAuthorizationServer_NothingIsEnforced;

    [Test]
    procedure TestOAuth_LowercaseBearerScheme_IsAccepted;
    [Test]
    procedure TestOAuth_MixedCaseBearerScheme_IsAccepted;
    [Test]
    procedure TestOAuth_LowercaseHeaderName_IsAccepted;
    [Test]
    procedure TestOAuth_LowercaseHeaderAndScheme_IsAccepted;

    [Test]
    procedure TestStaticToken_LowercaseBearerScheme_IsAccepted;
    [Test]
    procedure TestStaticToken_LowercaseHeaderName_IsAccepted;
  end;

  /// <summary>
  ///   The metadata proxy: what it republishes, and - the point of the fixture -
  ///   that the document it caches belongs to the server that fetched it. The cache
  ///   is per process, so two servers proxying different authorization servers are
  ///   the case that has to work.
  /// </summary>
  [TestFixture]
  TTransportOAuthProxyTest = class(TObject)
  private const
    Resource = 'https://mcp.example.com/api/mcp';
    ProxyUrl = '/oauth-proxy/.well-known/openid-configuration';
  private
    FUpstream: TFakeUpstream;

    /// <summary>
    ///   Builds a server proxying the upstream hosted at APath and asks it for the
    ///   proxied document, exactly as a client reading discovery would.
    /// </summary>
    function ProxyDocumentOf(const AUpstreamPath: string): TTransportOutcome;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestProxy_RepublishesTheUpstreamDocumentUnderItsOwnIssuer;
    [Test]
    procedure TestProxy_ASecondUpstreamIsNotServedTheFirstsDocument;
    [Test]
    procedure TestProxy_TheSameUpstreamIsFetchedOnce;
    [Test]
    procedure TestProxy_FindsAnAuthorizationServerWithNoOidcDocument;
    [Test]
    procedure TestProxy_RefusesAnIssuerDifferingOnlyInPathCase;
  end;

implementation

uses
  System.SyncObjs, IdSocketHandle,

  MCPConnect.MCP.Middleware.OAuth;

{ TStubTransportWriter }

procedure TStubTransportWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests: a response that reaches here would not be observed,
  // and none of the paths under test take that route.
end;

function TStubTransportWriter.Connected: Boolean;
begin
  Result := False;
end;

function TStubTransportWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TTransportOutcome }

function TTransportOutcome.ChallengeParam(const AName: string): string;
var
  LStart, LEnd: Integer;
begin
  Result := '';

  LStart := Challenge.IndexOf(AName + '="');
  if LStart < 0 then
    Exit;

  Inc(LStart, AName.Length + 2);
  LEnd := Challenge.IndexOf('"', LStart);
  if LEnd < 0 then
    Exit;

  Result := Challenge.Substring(LStart, LEnd - LStart);
end;

{ TStubTokenValidator }

function TStubTokenValidator.Validate(AContext: TJRPCContext; const AToken: string;
  AAccessToken: TMCPAccessToken): TTokenValidationResult;
begin
  if AToken = GoodToken then
  begin
    AAccessToken.FromString('{"sub":"user-42"}');
    Exit(TTokenValidationResult.Ok);
  end;

  Result := TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken,
    'the stub validator did not accept this token');
end;

/// <summary>
///   Drives one request through a server the way a transport does: one handler, a
///   request converter in, a response converter out. Shared by both fixtures.
/// </summary>
function ExecuteOn(AServer: TMCPServer; const AMethod, AUrl, AHeaderName,
  AHeaderValue: string; AProtocol: TTransportProtocol): TTransportOutcome;
var
  LHandler: IMCPTransportHandler;
  LOutcome: TTransportOutcome;
begin
  LOutcome := Default(TTransportOutcome);

  LHandler := TMCPTransportHandler.Create(AServer, TStubTransportWriter.Create);
  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := AUrl;
      ARequest.Command := AMethod;
      ARequest.Protocol := AProtocol;
      if AHeaderValue <> '' then
        ARequest.SetHeader(AHeaderName, AHeaderValue);
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LOutcome.Code := AResponse.Code;
      LOutcome.Content := AResponse.Content;
      LOutcome.ContentType := AResponse.ContentType;
      LOutcome.Challenge := AResponse.GetHeader('WWW-Authenticate');
      LOutcome.HasChallenge := LOutcome.Challenge <> '';
    end
  );

  Result := LOutcome;
end;

{ TFakeUpstream }

constructor TFakeUpstream.Create;
var
  LBinding: TIdSocketHandle;
begin
  inherited Create;
  FDocuments := TDictionary<string, string>.Create;

  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := CommandGet;

  // Loopback and an ephemeral port: nothing outside the machine can reach it, and
  // two test runs at once do not collide over a number.
  LBinding := FServer.Bindings.Add;
  LBinding.IP := '127.0.0.1';
  LBinding.Port := 0;
  FServer.Active := True;
end;

destructor TFakeUpstream.Destroy;
begin
  FServer.Active := False;
  FServer.Free;
  FDocuments.Free;
  inherited;
end;

function TFakeUpstream.BaseUrl(const APath: string): string;
begin
  Result := Format('http://127.0.0.1:%d%s', [FServer.Bindings[0].Port, APath]);
end;

procedure TFakeUpstream.Publish(const ADocumentPath, AIssuer: string);
begin
  FDocuments.AddOrSetValue(ADocumentPath, Format(
    '{"issuer":"%0:s","authorization_endpoint":"%0:s/authorize",' +
    '"token_endpoint":"%0:s/token","jwks_uri":"%0:s/keys"}', [AIssuer]));
end;

procedure TFakeUpstream.PublishIssuer(const APath: string);
begin
  // The last of the three candidates the proxy tries, so a document found here says
  // the whole walk happened. "issuer" has to be the URL it was fetched for or the
  // proxy refuses it (RFC 8414 3.3), which is also what makes each document identify
  // its own upstream.
  Publish(APath + '/.well-known/openid-configuration', BaseUrl(APath));
end;

procedure TFakeUpstream.CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LDocument: string;
begin
  TInterlocked.Increment(FHits);

  if FDocuments.TryGetValue(ARequestInfo.Document, LDocument) then
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.ContentText := LDocument;
  end
  else
    AResponseInfo.ResponseNo := 404;
end;

{ TTransportOAuthTest }

procedure TTransportOAuthTest.Setup;
begin
  FServer := TMCPServer.Create(nil);

  // The handler reads its MCP configuration in the constructor and dereferences it
  // before any OAuth check, so a server without one cannot serve a request at all.
  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('transport-test')
      .SetVersion('1.0.0')
    .BackToMCP
  .ApplyConfig;
end;

procedure TTransportOAuthTest.TearDown;
begin
  FServer.Free;
end;

procedure TTransportOAuthTest.EnableOAuth;
begin
  FServer.Plugin.Configure<IOAuthConfig>
    .SetResource(Resource)
    .AddAuthorizationServer(Issuer)
    .SetTokenValidatorClass(TStubTokenValidator)
  .ApplyConfig;
end;

function TTransportOAuthTest.Execute(const AMethod, AUrl: string): TTransportOutcome;
begin
  Result := Execute(AMethod, AUrl, '');
end;

function TTransportOAuthTest.Execute(const AMethod, AUrl, AAuthorization: string;
  AProtocol: TTransportProtocol): TTransportOutcome;
begin
  Result := Execute(AMethod, AUrl, 'Authorization', AAuthorization, AProtocol);
end;

function TTransportOAuthTest.Execute(const AMethod, AUrl, AHeaderName, AHeaderValue: string;
  AProtocol: TTransportProtocol): TTransportOutcome;
begin
  // One handler per request, built exactly as every transport builds it.
  Result := ExecuteOn(FServer, AMethod, AUrl, AHeaderName, AHeaderValue, AProtocol);
end;

procedure TTransportOAuthTest.TestMetadata_IsServedAtThePathInsertionUrl;
var
  LOutcome: TTransportOutcome;
  LJSON: TJSONObject;
begin
  EnableOAuth;

  // RFC 9728 §3.1: the document lives under the resource's own path.
  LOutcome := Execute('GET', MetadataPath);

  Assert.AreEqual(200, LOutcome.Code);
  Assert.AreEqual('application/json', LOutcome.ContentType);

  LJSON := TJSONObject.ParseJSONValue(LOutcome.Content) as TJSONObject;
  try
    Assert.IsNotNull(LJSON, 'The metadata document must be JSON');
    Assert.AreEqual(Resource, LJSON.GetValue<string>('resource'));
    Assert.AreEqual(Issuer, LJSON.GetValue<TJSONArray>('authorization_servers').Items[0].Value);
  finally
    LJSON.Free;
  end;
end;

procedure TTransportOAuthTest.TestMetadata_IsServedAtTheBareWellKnownPath;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  // Clients fall back to it, and it is the only form an origin-only resource has.
  LOutcome := Execute('GET', '/.well-known/oauth-protected-resource');

  Assert.AreEqual(200, LOutcome.Code);
end;

procedure TTransportOAuthTest.TestMetadata_IsNotServedAtTheOldHardcodedPath;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  // The route used to accept "<well-known>/mcp" whatever the configured resource was.
  // This server's resource is at /api/mcp, so that URL now belongs to no resource here
  // and must be treated as an ordinary request - answering it would hand a client a
  // document describing a resource it did not ask about.
  LOutcome := Execute('GET', '/.well-known/oauth-protected-resource/mcp');

  Assert.AreEqual(401, LOutcome.Code);
end;

procedure TTransportOAuthTest.TestMetadata_NeedsNoToken;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  // It is what a client reads in order to find out how to get a token, so requiring
  // one would close the loop it exists to open.
  LOutcome := Execute('GET', MetadataPath);

  Assert.AreEqual(200, LOutcome.Code);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestMetadata_IsNotServedForPost;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', MetadataPath);

  Assert.AreEqual(401, LOutcome.Code, 'Only GET retrieves the document');
end;

procedure TTransportOAuthTest.TestRequestWithoutAToken_IsChallenged;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath);

  Assert.AreEqual(401, LOutcome.Code);
  Assert.IsTrue(LOutcome.HasChallenge, 'A 401 without a challenge tells a client nothing');
  Assert.IsTrue(LOutcome.Challenge.StartsWith('Bearer '), LOutcome.Challenge);
  Assert.AreEqual('', LOutcome.ChallengeParam('error'),
    'Arriving without a token is not a token error');
end;

procedure TTransportOAuthTest.TestChallenge_QuotesTheMetadataUrl;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath);

  // Quoted, and pointing at the path-insertion URL the document is actually served
  // from: this is the whole discovery path a client follows out of a 401.
  Assert.AreEqual('https://mcp.example.com' + MetadataPath,
    LOutcome.ChallengeParam('resource_metadata'), LOutcome.Challenge);
  Assert.AreEqual('mcp', LOutcome.ChallengeParam('realm'));
end;

procedure TTransportOAuthTest.TestRequestWithARejectedToken_ReportsInvalidToken;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'Bearer not-the-good-token');

  Assert.AreEqual(401, LOutcome.Code);
  Assert.AreEqual('invalid_token', LOutcome.ChallengeParam('error'), LOutcome.Challenge);
  Assert.AreEqual('the stub validator did not accept this token',
    LOutcome.ChallengeParam('error_description'), LOutcome.Challenge);
end;

procedure TTransportOAuthTest.TestRequestWithAnAcceptedToken_IsNotChallenged;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'Bearer ' + TStubTokenValidator.GoodToken);

  // What happens after the token is accepted is JSON-RPC handling, not this test's
  // business: all that matters here is that the request got past the OAuth gate.
  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestOptions_IsExemptFromTheTokenCheck;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  // A CORS preflight carries no Authorization header by definition, so challenging it
  // would stop a browser client before it ever asked for a token.
  LOutcome := Execute('OPTIONS', ResourcePath);

  Assert.AreEqual(204, LOutcome.Code);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestStdio_IsExemptFromTheTokenCheck;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  // STDIO carries no headers to put a token in and no 401 the client would ever see.
  // Enforcing OAuth there rejected every request, silently.
  LOutcome := Execute('POST', '', '', TTransportProtocol.Stdio);

  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestWithoutAnAuthorizationServer_NothingIsEnforced;
var
  LOutcome: TTransportOutcome;
begin
  // No EnableOAuth: with no authorization server configured the whole check is off,
  // which is what lets a server opt into OAuth rather than out of it.
  LOutcome := Execute('POST', ResourcePath);

  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

{ Case-insensitive header/scheme tests — OAuth }

procedure TTransportOAuthTest.TestOAuth_LowercaseBearerScheme_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'bearer ' + TStubTokenValidator.GoodToken);

  Assert.AreNotEqual(401, LOutcome.Code, 'Lowercase "bearer" scheme must be accepted (RFC 7235 §2.1)');
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestOAuth_MixedCaseBearerScheme_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'BEARER ' + TStubTokenValidator.GoodToken);

  Assert.AreNotEqual(401, LOutcome.Code, 'Mixed-case "BEARER" scheme must be accepted (RFC 7235 §2.1)');
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestOAuth_LowercaseHeaderName_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'authorization',
    'Bearer ' + TStubTokenValidator.GoodToken, TTransportProtocol.StreamableHTTP);

  Assert.AreNotEqual(401, LOutcome.Code, 'Lowercase "authorization" header must be accepted (RFC 7230 §3.2)');
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportOAuthTest.TestOAuth_LowercaseHeaderAndScheme_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableOAuth;

  LOutcome := Execute('POST', ResourcePath, 'authorization',
    'bearer ' + TStubTokenValidator.GoodToken, TTransportProtocol.StreamableHTTP);

  Assert.AreNotEqual(401, LOutcome.Code, 'Lowercase header + scheme must be accepted');
  Assert.IsFalse(LOutcome.HasChallenge);
end;

{ Case-insensitive header/scheme tests — static token }

procedure TTransportOAuthTest.EnableStaticToken(const AToken: string);
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken(AToken)
    .SetTokenLocation(TAuthTokenLocation.Bearer)
  .ApplyConfig;
end;

procedure TTransportOAuthTest.TestStaticToken_LowercaseBearerScheme_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableStaticToken('my-static-secret');

  LOutcome := Execute('POST', ResourcePath, 'bearer my-static-secret');

  Assert.AreNotEqual(403, LOutcome.Code, 'Lowercase "bearer" must be accepted for static tokens');
end;

procedure TTransportOAuthTest.TestStaticToken_LowercaseHeaderName_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableStaticToken('my-static-secret');

  LOutcome := Execute('POST', ResourcePath, 'authorization',
    'Bearer my-static-secret', TTransportProtocol.StreamableHTTP);

  Assert.AreNotEqual(403, LOutcome.Code, 'Lowercase "authorization" header must be accepted for static tokens');
end;

{ TTransportOAuthProxyTest }

procedure TTransportOAuthProxyTest.Setup;
begin
  FUpstream := TFakeUpstream.Create;
  FUpstream.PublishIssuer('/idp-a');
  FUpstream.PublishIssuer('/idp-b');

  // The cache outlives a server, so a fixture that did not drop it would pass or
  // fail on the order its tests happened to run in.
  TOAuthMiddleware.ClearProxyCache;
end;

procedure TTransportOAuthProxyTest.TearDown;
begin
  TOAuthMiddleware.ClearProxyCache;
  FUpstream.Free;
end;

function TTransportOAuthProxyTest.ProxyDocumentOf(
  const AUpstreamPath: string): TTransportOutcome;
var
  LServer: TMCPServer;
begin
  LServer := TMCPServer.Create(nil);
  try
    LServer.Plugin.Configure<IMCPConfig>
      .Server
        .SetName('proxy-test')
        .SetVersion('1.0.0')
      .BackToMCP
    .ApplyConfig;

    LServer.Plugin.Configure<IOAuthConfig>
      .SetResource(Resource)
      .EnableMetadataProxy(FUpstream.BaseUrl(AUpstreamPath))
      .SetTokenValidatorClass(TStubTokenValidator)
    .ApplyConfig;

    Result := ExecuteOn(LServer, 'GET', ProxyUrl, '', '',
      TTransportProtocol.StreamableHTTP);
  finally
    LServer.Free;
  end;
end;

procedure TTransportOAuthProxyTest.TestProxy_RepublishesTheUpstreamDocumentUnderItsOwnIssuer;
var
  LOutcome: TTransportOutcome;
  LJSON: TJSONObject;
begin
  LOutcome := ProxyDocumentOf('/idp-a');

  Assert.AreEqual(200, LOutcome.Code, LOutcome.Content);

  LJSON := TJSONObject.ParseJSONValue(LOutcome.Content) as TJSONObject;
  try
    Assert.IsNotNull(LJSON, 'The proxied document must be JSON');

    // The issuer is this proxy, the endpoints are still the upstream's, and S256 is
    // injected for the clients that insist on reading it.
    Assert.AreEqual('https://mcp.example.com/oauth-proxy',
      LJSON.GetValue<string>('issuer'));
    Assert.AreEqual(FUpstream.BaseUrl('/idp-a') + '/authorize',
      LJSON.GetValue<string>('authorization_endpoint'));
    Assert.AreEqual('S256',
      LJSON.GetValue<TJSONArray>('code_challenge_methods_supported').Items[0].Value);
  finally
    LJSON.Free;
  end;
end;

procedure TTransportOAuthProxyTest.TestProxy_ASecondUpstreamIsNotServedTheFirstsDocument;
var
  LFirst, LSecond: TTransportOutcome;

  function AuthorizationEndpointOf(const AOutcome: TTransportOutcome): string;
  var
    LJSON: TJSONObject;
  begin
    LJSON := TJSONObject.ParseJSONValue(AOutcome.Content) as TJSONObject;
    try
      Assert.IsNotNull(LJSON, 'The proxied document must be JSON');
      Result := LJSON.GetValue<string>('authorization_endpoint');
    finally
      LJSON.Free;
    end;
  end;

begin
  // Two servers in one process, each proxying its own authorization server: the
  // cache is shared between them, so it has to be keyed by what it holds. Served
  // from one slot, the second server republishes the first one's endpoints - and
  // sends its clients to someone else's identity provider.
  LFirst := ProxyDocumentOf('/idp-a');
  LSecond := ProxyDocumentOf('/idp-b');

  Assert.AreEqual(200, LFirst.Code, LFirst.Content);
  Assert.AreEqual(200, LSecond.Code, LSecond.Content);

  Assert.AreEqual(FUpstream.BaseUrl('/idp-a') + '/authorize',
    AuthorizationEndpointOf(LFirst));
  Assert.AreEqual(FUpstream.BaseUrl('/idp-b') + '/authorize',
    AuthorizationEndpointOf(LSecond),
    'The second server was served the first one''s cached document');
end;

procedure TTransportOAuthProxyTest.TestProxy_TheSameUpstreamIsFetchedOnce;
var
  LAfterFirst: Integer;
begin
  // Keying the cache must not turn it off: the same upstream asked for a second time
  // does not reach it at all, which is the whole reason the cache is there. Counted
  // as a delta rather than as a total, because how many requests the first answer
  // costs is the discovery walk's business - this upstream publishes at the last of
  // the three URLs it tries.
  Assert.AreEqual(200, ProxyDocumentOf('/idp-a').Code);
  LAfterFirst := FUpstream.Hits;
  Assert.IsTrue(LAfterFirst > 0, 'The first answer must have been fetched');

  Assert.AreEqual(200, ProxyDocumentOf('/idp-a').Code);

  Assert.AreEqual(LAfterFirst, FUpstream.Hits,
    'The second request must have been served from the cache');
end;

procedure TTransportOAuthProxyTest.TestProxy_FindsAnAuthorizationServerWithNoOidcDocument;
var
  LOutcome: TTransportOutcome;
  LJSON: TJSONObject;
begin
  // An OAuth 2.1 authorization server that is not an OpenID Connect provider: it
  // publishes the RFC 8414 document and nothing else. Asking only for the OpenID
  // Connect URL answered 502 with the upstream working perfectly well.
  FUpstream.Publish('/.well-known/oauth-authorization-server/idp-c',
    FUpstream.BaseUrl('/idp-c'));

  LOutcome := ProxyDocumentOf('/idp-c');

  Assert.AreEqual(200, LOutcome.Code, LOutcome.Content);

  LJSON := TJSONObject.ParseJSONValue(LOutcome.Content) as TJSONObject;
  try
    Assert.IsNotNull(LJSON, 'The proxied document must be JSON');
    Assert.AreEqual(FUpstream.BaseUrl('/idp-c') + '/authorize',
      LJSON.GetValue<string>('authorization_endpoint'));
  finally
    LJSON.Free;
  end;

  // The RFC 8414 form is the first candidate, so nothing else was asked for.
  Assert.AreEqual(1, FUpstream.Hits);
end;

procedure TTransportOAuthProxyTest.TestProxy_RefusesAnIssuerDifferingOnlyInPathCase;
var
  LOutcome: TTransportOutcome;
begin
  // The path of an issuer is case sensitive - RFC 3986 makes only the scheme and the
  // authority insensitive - so "/IDP-D" is a different authorization server from
  // "/idp-d". That is the rule TOAuthConfig.SameUri applies everywhere else, and the
  // one a comparison that lowercased the whole URL quietly broke here.
  FUpstream.Publish('/idp-d/.well-known/openid-configuration',
    FUpstream.BaseUrl('/IDP-D'));

  LOutcome := ProxyDocumentOf('/idp-d');

  Assert.AreEqual(502, LOutcome.Code,
    'A document declaring another issuer must not be republished');
  Assert.Contains(LOutcome.Content, '/IDP-D',
    'The refusal must say which issuer the document declared');
end;

initialization
  TDUnitX.RegisterTestFixture(TTransportOAuthTest);
  TDUnitX.RegisterTestFixture(TTransportOAuthProxyTest);

end.
