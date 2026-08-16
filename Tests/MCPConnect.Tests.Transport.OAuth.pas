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

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Security.Token,
  MCPConnect.Transport.Base,
  MCPConnect.MCP.Types,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

type
  /// <summary>
  ///   A response writer that streams nothing, which is what the WebBroker and plain
  ///   HTTP paths look like: everything comes back through the response converter.
  /// </summary>
  TStubTransportWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
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
    FServer: TJRPCServer;
    function Execute(const AMethod, AUrl: string): TTransportOutcome; overload;
    function Execute(const AMethod, AUrl, AAuthorization: string;
      AProtocol: TTransportProtocol = TTransportProtocol.StreamableHTTP): TTransportOutcome; overload;
    procedure EnableOAuth;
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
  end;

implementation

{ TStubTransportWriter }

procedure TStubTransportWriter.Write(const AValue: string; const AEventId: string);
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

{ TTransportOAuthTest }

procedure TTransportOAuthTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);

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
var
  LHandler: IMCPTransportHandler;
  LOutcome: TTransportOutcome;
begin
  LOutcome := Default(TTransportOutcome);

  // One handler per request, built exactly as every transport builds it.
  LHandler := TMCPTransportHandler.Create(FServer, TStubTransportWriter.Create);
  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := AUrl;
      ARequest.Command := AMethod;
      ARequest.Protocol := AProtocol;
      if AAuthorization <> '' then
        ARequest.AddOrSetHeader('Authorization', AAuthorization);
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LOutcome.Code := AResponse.Code;
      LOutcome.Content := AResponse.Content;
      LOutcome.ContentType := AResponse.ContentType;
      LOutcome.HasChallenge :=
        AResponse.Headers.TryGetValue('WWW-Authenticate', LOutcome.Challenge);
    end
  );

  Result := LOutcome;
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

end.
