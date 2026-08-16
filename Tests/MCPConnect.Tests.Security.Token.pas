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
unit MCPConnect.Tests.Security.Token;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.Auth,
  MCPConnect.MCP.Types,
  MCPConnect.Security.Jwks,
  MCPConnect.Security.Token;

type
  /// <summary>
  ///   Metadata provider serving canned documents instead of hitting the network,
  ///   counting the fetches so that the caching rules can be observed.
  /// </summary>
  TFakeMetadataProvider = class(TOAuthMetadataProvider)
  private
    FDocuments: TDictionary<string, string>;
    FFetchedUrls: TStringList;
    FFetchCount: Integer;
    FFailFrom: Integer;
  protected
    function FetchDocument(const AUrl: string): string; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure SetDocument(const AUrl, AContent: string);
    procedure ResetFetchCount;

    /// <summary>
    ///   Whether a URL was requested at all, however the request ended. Says what a
    ///   fetch count cannot once a lookup may try several candidates.
    /// </summary>
    function WasFetched(const AUrl: string): Boolean;

    property FetchCount: Integer read FFetchCount;
    /// <summary>Ordinal of the first fetch that must fail; 0 means "never fail".</summary>
    property FailFrom: Integer read FFailFrom write FFailFrom;
  end;

  /// <summary>
  ///   A validator that implements ITokenValidator and nothing else: no MCPConnect
  ///   base class, no constructor of ours. It is what the plugin contract promises,
  ///   so the tests build it exactly the way the transport does.
  /// </summary>
  TStandaloneValidator = class(TInterfacedObject, ITokenValidator)
  private
    class var FLiveInstances: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult;

    /// <summary>Instances built and not yet destroyed, to observe the lifetime contract.</summary>
    class property LiveInstances: Integer read FLiveInstances;
  end;

  /// <summary>A class that does not implement ITokenValidator at all.</summary>
  TNotAValidator = class(TObject);

  /// <summary>
  ///   Stands in for a real validator: overrides the signature hook, records what it
  ///   was handed and rejects or accepts on demand.
  /// </summary>
  TSignatureCheckingValidator = class(TClaimsTokenValidator)
  private
    class var FAccept: Boolean;
    class var FCalls: Integer;
    class var FSignedMaterial: string;
    class var FKeyId: string;
  protected
    function CheckSignature(const AHeader, APayload, ASignature: string;
      const AKey: TOAuthJsonWebKey): TTokenValidationResult; override;
  public
    class procedure Reset(AAccept: Boolean); static;

    class property Calls: Integer read FCalls;
    /// <summary>What a real implementation would verify the signature over.</summary>
    class property SignedMaterial: string read FSignedMaterial;
    /// <summary>The "kid" of the key the hook received.</summary>
    class property KeyId: string read FKeyId;
  end;

  [TestFixture]
  TTokenValidationResultTest = class(TObject)
  public
    [Test]
    procedure TestOk_IsSuccessWithNoError;
    [Test]
    procedure TestFail_CarriesCodeAndDescription;
    [Test]
    procedure TestErrorCodeToString_UsesRFC6750Names;
  end;

  [TestFixture]
  TBearerChallengeTest = class(TObject)
  private const
    Realm = 'mcp';
    Metadata = 'https://mcp.example.com/.well-known/oauth-protected-resource';
  public
    [Test]
    procedure TestChallenge_QuotesEveryParameterValue;
    [Test]
    procedure TestChallenge_WithoutAnErrorCarriesNoErrorParameters;
    [Test]
    procedure TestChallenge_CarriesTheErrorAndItsDescription;
    [Test]
    procedure TestChallenge_OmitsAnEmptyDescription;
    [Test]
    procedure TestChallenge_DescriptionCannotEndTheQuotedString;
  end;

  [TestFixture]
  TDecodeOnlyTokenValidatorTest = class(TObject)
  private
    FValidator: ITokenValidator;
    FAccessToken: TMCPAccessToken;
    FContext: TJRPCContext;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestValidate_WellFormedTokenSucceedsAndFillsClaims;
    [Test]
    procedure TestValidate_TokenWithoutThreeSegmentsIsRejected;
    [Test]
    procedure TestValidate_TokenWithNonJSONPayloadIsRejected;
    [Test]
    procedure TestValidate_RejectedTokenLeavesClaimsEmpty;
  end;

  [TestFixture]
  TClaimsTokenValidatorTest = class(TObject)
  private const
    Issuer = 'https://idp.example.com';
    Audience = 'https://mcp.example.com/mcp';
    KeyId = 'key-1';
  private
    FFake: TFakeMetadataProvider;
    FServer: TJRPCServer;
    FConfig: IOAuthConfig;
    FContext: TJRPCContext;
    FAccessToken: TMCPAccessToken;

    /// <summary>A token valid in every respect, before the test spoils one claim.</summary>
    function ValidToken: string;
    /// <summary>Runs a validator the same way TMCPTransportHandler does.</summary>
    function Validate(const AToken: string): TTokenValidationResult;
    /// <summary>
    ///   Validates a good token against a server configured from scratch, for the
    ///   cases where what matters is what the configuration is missing. An empty
    ///   argument means "do not configure it at all".
    /// </summary>
    function ValidateWithBareServer(const AResource, AAuthorizationServer: string): TTokenValidationResult;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestValidate_ValidTokenSucceedsAndFillsClaims;
    [Test]
    procedure TestValidate_UnknownIssuerIsRejected;
    [Test]
    procedure TestValidate_IssuerTrailingSlashIsNotPartOfTheIdentity;
    [Test]
    procedure TestValidate_DeclaredTrustedIssuerIsAcceptedAndKeysStayOnTheAuthorizationServer;
    [Test]
    procedure TestValidate_AudienceForAnotherResourceIsRejected;
    [Test]
    procedure TestValidate_AudienceArrayContainingTheResourceIsAccepted;
    [Test]
    procedure TestValidate_ExpiredTokenIsRejected;
    [Test]
    procedure TestValidate_TokenExpiredWithinTheClockSkewIsAccepted;
    [Test]
    procedure TestValidate_TokenWithoutExpirationIsRejected;
    [Test]
    procedure TestValidate_NotYetValidTokenIsRejected;
    [Test]
    procedure TestValidate_AlgorithmNoneIsRejected;
    [Test]
    procedure TestValidate_SymmetricAlgorithmIsRejected;
    [Test]
    procedure TestValidate_UnregisteredAlgorithmIsRejected;
    [Test]
    procedure TestValidate_AlgorithmForAnotherKeyTypeIsRejected;
    [Test]
    procedure TestValidate_AlgorithmOtherThanTheOneTheKeyDeclaresIsRejected;
    [Test]
    procedure TestValidate_AlgorithmTheKeyDeclaresIsAccepted;
    [Test]
    procedure TestValidate_UnpublishedKeyIdIsRejected;
    [Test]
    procedure TestValidate_MissingRequiredScopeIsInsufficientScope;
    [Test]
    procedure TestValidate_GrantedRequiredScopeIsAccepted;
    [Test]
    procedure TestValidate_RejectedTokenLeavesClaimsEmpty;
    [Test]
    procedure TestValidate_WithoutTrustedIssuersEverythingIsRejected;
    [Test]
    procedure TestValidate_WithoutAudienceEverythingIsRejected;
    [Test]
    procedure TestValidate_WithoutAServerInTheContextEverythingIsRejected;

    [Test]
    procedure TestSignatureHook_IsCalledWithTheSignedMaterialAndTheResolvedKey;
    [Test]
    procedure TestSignatureHook_RejectingTheSignatureRejectsTheToken;
    [Test]
    procedure TestSignatureHook_IsNotReachedWhenAClaimAlreadyFailed;
    [Test]
    procedure TestSignatureHook_RejectedTokenLeavesClaimsEmpty;
  end;

  /// <summary>
  ///   The point of the plugin contract: a validator bound to nothing but the
  ///   interface must be registrable, buildable and callable.
  /// </summary>
  [TestFixture]
  TTokenValidatorContractTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IOAuthConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegister_ClassNotDerivedFromTheBaseClassIsAccepted;
    [Test]
    procedure TestRegister_ClassNotImplementingTheInterfaceRaises;
    [Test]
    procedure TestRegister_NilClearsTheValidator;
    [Test]
    procedure TestCreate_StandaloneValidatorIsBuiltThroughRttiAndInvoked;
    [Test]
    procedure TestCreate_ValidatorIsDestroyedWhenTheInterfaceGoesOutOfScope;
  end;

  [TestFixture]
  TOAuthConfigValidationTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IOAuthConfig;
    function GetOAuthConfig: TOAuthConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestValidatorClass_DefaultsToNil;
    [Test]
    procedure TestSetTokenValidatorClass_IsStored;
    [Test]
    procedure TestTrustedIssuers_AreTheConfiguredAuthorizationServers;
    [Test]
    procedure TestTrustedIssuers_AreTheUpstreamWhenMetadataProxyIsEnabled;
    [Test]
    procedure TestAddTrustedIssuer_WidensTheDerivedList;
    [Test]
    procedure TestAddTrustedIssuer_DoesNotDuplicateADerivedIssuer;
    [Test]
    procedure TestAddTrustedIssuer_IgnoresEmptyValues;
    [Test]
    procedure TestResourceMetadata_InsertsTheWellKnownSegmentBeforeThePath;
    [Test]
    procedure TestResourceMetadata_HasNoSuffixForAnOriginOnlyResource;
    [Test]
    procedure TestResourceMetadata_TrailingSlashOnTheResourceIsNotASeparatePath;
    [Test]
    procedure TestResourceMetadata_KeepsAPortAndANestedPath;
    [Test]
    procedure TestResourceMetadata_RequiresAResource;
    [Test]
    procedure TestAudience_DefaultsToResource;
    [Test]
    procedure TestAudience_CanBeOverridden;
    [Test]
    procedure TestRequiredScopesAndClockSkew_AreStored;
    [Test]
    procedure TestClockSkew_HasADefault;
    [Test]
    procedure TestMetadataProvider_IsAlwaysAvailable;
    [Test]
    procedure TestSetMetadataProvider_ReplacesTheDefaultOne;
    [Test]
    procedure TestSetMetadataProvider_IgnoresNil;
  end;

  [TestFixture]
  TOAuthMetadataProviderTest = class(TObject)
  private const
    Issuer = 'https://idp.example.com';
    /// <summary>The first candidate tried: RFC 8414 is preferred over OIDC discovery.</summary>
    DiscoveryUrl = Issuer + '/.well-known/oauth-authorization-server';
    OidcDiscoveryUrl = Issuer + '/.well-known/openid-configuration';
    JwksUrl = Issuer + '/keys';
  private
    FFake: TFakeMetadataProvider;
    FProvider: IOAuthMetadataProvider;
    procedure PublishKeys(const AKeyIds: array of string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestGetServerMetadata_ParsesTheDocument;
    [Test]
    procedure TestGetServerMetadata_IsCachedWithinTheTTL;
    [Test]
    procedure TestGetServerMetadata_IsFetchedAgainAfterTheTTL;
    [Test]
    procedure TestDiscoveryUrls_FollowTheSpecifiedOrderForAnIssuerWithAPath;
    [Test]
    procedure TestDiscoveryUrls_CollapseToTwoForAnOriginOnlyIssuer;
    [Test]
    procedure TestGetServerMetadata_FallsBackToOpenIDConnectDiscovery;
    [Test]
    procedure TestGetServerMetadata_PrefersTheRFC8414Document;
    [Test]
    procedure TestGetServerMetadata_RemembersWhichCandidateWorked;
    [Test]
    procedure TestGetServerMetadata_DocumentDeclaringAnotherIssuerIsRefused;
    [Test]
    procedure TestGetServerMetadata_DocumentWithoutAnIssuerIsRefused;
    [Test]
    procedure TestGetServerMetadata_IssuerTrailingSlashIsNotPartOfTheIdentity;
    [Test]
    procedure TestGetKeys_KeysAreNotFetchedFromAMisdeclaredDocument;
    [Test]
    procedure TestGetKeys_ParsesTheKeySet;
    [Test]
    procedure TestGetKeys_ParsesEveryMappedMember;
    [Test]
    procedure TestGetKeys_KeepsTheKeyEntryAsPublished;
    [Test]
    procedure TestTryGetKey_FindsThePublishedKey;
    [Test]
    procedure TestTryGetKey_UnknownKeyIdRefreshesOnceAndFindsARotatedKey;
    [Test]
    procedure TestTryGetKey_UnknownKeyIdDoesNotRefreshWithinTheRateLimit;
    [Test]
    procedure TestTryGetKey_EmptyKeyIdMatchesTheOnlyPublishedKey;
    [Test]
    procedure TestTryGetKey_EmptyKeyIdIsAmbiguousWithSeveralKeys;
    [Test]
    procedure TestTryGetKey_ReturnsFalseInsteadOfRaisingWhenUnreachable;
    [Test]
    procedure TestGetKeys_KeepsServingTheCachedSetWhenARefreshFails;
    [Test]
    procedure TestGetServerMetadata_RaisesWhenNothingWasEverFetched;
    [Test]
    procedure TestInvalidate_ForcesTheNextFetch;
  end;

/// <summary>
///   Builds and runs a validator exactly as TMCPTransportHandler.ValidateAccessToken
///   does, so the tests exercise the real plugin path and not a shortcut. Shared with
///   the fixtures of the optional validators.
/// </summary>
function RunValidator(AClass: TClass; AContext: TJRPCContext; const AToken: string;
  AAccessToken: TMCPAccessToken): TTokenValidationResult;

implementation

uses
  System.NetEncoding, System.DateUtils,

  Neon.Core.Utils;

function RunValidator(AClass: TClass; AContext: TJRPCContext; const AToken: string;
  AAccessToken: TMCPAccessToken): TTokenValidationResult;
var
  LInstance: TObject;
  LValidator: ITokenValidator;
begin
  LInstance := TRttiUtils.CreateInstance(AClass);
  if not Supports(LInstance, ITokenValidator, LValidator) then
  begin
    LInstance.Free;
    raise Exception.CreateFmt('%s does not implement ITokenValidator', [AClass.ClassName]);
  end;

  Result := LValidator.Validate(AContext, AToken, AAccessToken);
end;

function Base64UrlEncode(const AValue: string): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(AValue));
  Result := Result.Replace(#13, '').Replace(#10, '')
    .Replace('+', '-').Replace('/', '_').TrimRight(['=']);
end;

function BuildToken(const AHeader, APayload: string): string; overload;
begin
  Result := Base64UrlEncode(AHeader) + '.' + Base64UrlEncode(APayload) + '.' +
    Base64UrlEncode('not-a-real-signature');
end;

function BuildToken(const APayload: string): string; overload;
begin
  Result := BuildToken('{"alg":"RS256","kid":"key-1"}', APayload);
end;

/// <summary>Current UTC time as a JWT numeric date.</summary>
function UnixNow: Int64;
begin
  Result := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now), True);
end;

{ TFakeMetadataProvider }

constructor TFakeMetadataProvider.Create;
begin
  inherited Create;
  FDocuments := TDictionary<string, string>.Create;
  FFetchedUrls := TStringList.Create;
  FFetchCount := 0;
  FFailFrom := 0;
end;

destructor TFakeMetadataProvider.Destroy;
begin
  FFetchedUrls.Free;
  FDocuments.Free;
  inherited;
end;

procedure TFakeMetadataProvider.SetDocument(const AUrl, AContent: string);
begin
  FDocuments.AddOrSetValue(AUrl, AContent);
end;

procedure TFakeMetadataProvider.ResetFetchCount;
begin
  FFetchCount := 0;
  FFetchedUrls.Clear;
end;

function TFakeMetadataProvider.WasFetched(const AUrl: string): Boolean;
begin
  Result := FFetchedUrls.IndexOf(AUrl) >= 0;
end;

function TFakeMetadataProvider.FetchDocument(const AUrl: string): string;
begin
  Inc(FFetchCount);
  FFetchedUrls.Add(AUrl);

  if (FailFrom > 0) and (FFetchCount >= FailFrom) then
    raise EOAuthMetadataException.CreateFmt('Fake transport failure for "%s"', [AUrl]);

  if not FDocuments.TryGetValue(AUrl, Result) then
    raise EOAuthMetadataException.CreateFmt('No fake document registered for "%s"', [AUrl]);
end;

{ TStandaloneValidator }

constructor TStandaloneValidator.Create;
begin
  inherited Create;
  Inc(FLiveInstances);
end;

destructor TStandaloneValidator.Destroy;
begin
  Dec(FLiveInstances);
  inherited;
end;

function TStandaloneValidator.Validate(AContext: TJRPCContext;
  const AToken: string; AAccessToken: TMCPAccessToken): TTokenValidationResult;
begin
  // Enough to prove the call reached an implementation that owes nothing to
  // TTokenValidatorBase, and that both the token and the context arrived with it.
  if (AToken = '') or not Assigned(AContext) then
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, 'nothing to validate'));

  AAccessToken.FromString('{"sub":"standalone"}');
  Result := TTokenValidationResult.Ok;
end;

{ TSignatureCheckingValidator }

class procedure TSignatureCheckingValidator.Reset(AAccept: Boolean);
begin
  FAccept := AAccept;
  FCalls := 0;
  FSignedMaterial := '';
  FKeyId := '';
end;

function TSignatureCheckingValidator.CheckSignature(const AHeader, APayload,
  ASignature: string; const AKey: TOAuthJsonWebKey): TTokenValidationResult;
begin
  Inc(FCalls);
  FSignedMaterial := AHeader + '.' + APayload;
  FKeyId := AKey.Kid;

  if FAccept then
    Exit(TTokenValidationResult.Ok);

  Result := Reject(TTokenValidationErrorCode.InvalidToken, 'bad signature',
    'a signature made with the published key', 'something else');
end;

{ TTokenValidationResultTest }

procedure TTokenValidationResultTest.TestOk_IsSuccessWithNoError;
var
  LResult: TTokenValidationResult;
begin
  LResult := TTokenValidationResult.Ok;

  Assert.IsTrue(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.None);
  Assert.AreEqual('', LResult.ErrorDescription);
end;

procedure TTokenValidationResultTest.TestFail_CarriesCodeAndDescription;
var
  LResult: TTokenValidationResult;
begin
  LResult := TTokenValidationResult.Fail(TTokenValidationErrorCode.InsufficientScope, 'missing scope');

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InsufficientScope);
  Assert.AreEqual('missing scope', LResult.ErrorDescription);
end;

procedure TTokenValidationResultTest.TestErrorCodeToString_UsesRFC6750Names;
begin
  Assert.AreEqual('', TokenValidationErrorCodeToString(TTokenValidationErrorCode.None));
  Assert.AreEqual('invalid_request', TokenValidationErrorCodeToString(TTokenValidationErrorCode.InvalidRequest));
  Assert.AreEqual('invalid_token', TokenValidationErrorCodeToString(TTokenValidationErrorCode.InvalidToken));
  Assert.AreEqual('insufficient_scope', TokenValidationErrorCodeToString(TTokenValidationErrorCode.InsufficientScope));
end;

{ TBearerChallengeTest }

procedure TBearerChallengeTest.TestChallenge_QuotesEveryParameterValue;
var
  LChallenge: string;
begin
  // RFC 7235 admits an auth-param value as a token or a quoted-string, and a URL is
  // not a token: unquoted, "resource_metadata" is malformed and a strict client drops
  // it - taking with it the only pointer to the metadata document (RFC 9728 §5.1).
  LChallenge := BuildBearerChallenge(Realm, Metadata, TTokenValidationResult.Ok);

  Assert.AreEqual(Format('Bearer realm="%s", resource_metadata="%s"', [Realm, Metadata]),
    LChallenge);
end;

procedure TBearerChallengeTest.TestChallenge_WithoutAnErrorCarriesNoErrorParameters;
var
  LChallenge: string;
begin
  // A request that simply arrived without a token has not failed validation, so the
  // challenge must not name an error it never produced.
  LChallenge := BuildBearerChallenge(Realm, Metadata,
    TTokenValidationResult.Fail(TTokenValidationErrorCode.None, ''));

  Assert.IsFalse(LChallenge.Contains('error'), 'The bare challenge carries no error');
end;

procedure TBearerChallengeTest.TestChallenge_CarriesTheErrorAndItsDescription;
var
  LChallenge: string;
begin
  LChallenge := BuildBearerChallenge(Realm, Metadata,
    TTokenValidationResult.Fail(TTokenValidationErrorCode.InsufficientScope, 'needs mcp.write'));

  Assert.IsTrue(LChallenge.Contains('error="insufficient_scope"'), LChallenge);
  Assert.IsTrue(LChallenge.Contains('error_description="needs mcp.write"'), LChallenge);
end;

procedure TBearerChallengeTest.TestChallenge_OmitsAnEmptyDescription;
var
  LChallenge: string;
begin
  // Misconfigurations are reported with an empty description on purpose: what this
  // server is missing is none of a caller's business.
  LChallenge := BuildBearerChallenge(Realm, Metadata,
    TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));

  Assert.IsTrue(LChallenge.Contains('error="invalid_token"'), LChallenge);
  Assert.IsFalse(LChallenge.Contains('error_description'), LChallenge);
end;

procedure TBearerChallengeTest.TestChallenge_DescriptionCannotEndTheQuotedString;

  function QuoteCount(const AValue: string): Integer;
  var
    LChar: Char;
  begin
    Result := 0;
    for LChar in AValue do
      if LChar = '"' then
        Inc(Result);
  end;

var
  LChallenge: string;
begin
  // The description comes from a validator implementation, so it is trusted neither
  // to stay inside its quotes nor to stay on one line.
  LChallenge := BuildBearerChallenge(Realm, Metadata,
    TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken,
      'broken" , error="none'#13#10'X-Injected: yes'));

  // Four parameters, one pair of quotes each, and not one more: a quote surviving in
  // a value would end its quoted-string early and turn the rest into parameters of
  // the attacker's choosing.
  Assert.AreEqual(8, QuoteCount(LChallenge), LChallenge);
  Assert.IsFalse(LChallenge.Contains(#13), 'A challenge is a single header line');
  Assert.IsFalse(LChallenge.Contains(#10), 'A challenge is a single header line');
end;

{ TDecodeOnlyTokenValidatorTest }

procedure TDecodeOnlyTokenValidatorTest.Setup;
begin
  FAccessToken := TMCPAccessToken.Create;
  FContext := TJRPCContext.Create;
  FValidator := TDecodeOnlyTokenValidator.Create;
end;

procedure TDecodeOnlyTokenValidatorTest.TearDown;
begin
  FValidator := nil;
  FContext.Free;
  FAccessToken.Free;
end;

procedure TDecodeOnlyTokenValidatorTest.TestValidate_WellFormedTokenSucceedsAndFillsClaims;
var
  LResult: TTokenValidationResult;
begin
  LResult := FValidator.Validate(FContext,
    BuildToken('{"sub":"user-42","name":"Ada Lovelace","iss":"https://idp.example.com"}'),
    FAccessToken);

  Assert.IsTrue(LResult.Success);
  Assert.AreEqual('user-42', FAccessToken.Subject);
  Assert.AreEqual('Ada Lovelace', FAccessToken.Name);
  Assert.AreEqual('https://idp.example.com', FAccessToken.Issuer);
end;

procedure TDecodeOnlyTokenValidatorTest.TestValidate_TokenWithoutThreeSegmentsIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := FValidator.Validate(FContext, 'not-a-token', FAccessToken);

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TDecodeOnlyTokenValidatorTest.TestValidate_TokenWithNonJSONPayloadIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := FValidator.Validate(FContext, BuildToken('this is not json'), FAccessToken);

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TDecodeOnlyTokenValidatorTest.TestValidate_RejectedTokenLeavesClaimsEmpty;
begin
  FValidator.Validate(FContext, BuildToken('this is not json'), FAccessToken);

  Assert.AreEqual('', FAccessToken.Subject, 'A rejected token must not populate the access token');
end;

{ TClaimsTokenValidatorTest }

procedure TClaimsTokenValidatorTest.Setup;
begin
  FFake := TFakeMetadataProvider.Create;
  FFake.SetDocument(Issuer + '/.well-known/openid-configuration',
    Format('{"issuer":"%s","jwks_uri":"%s/keys"}', [Issuer, Issuer]));
  FFake.SetDocument(Issuer + '/keys',
    Format('{"keys":[{"kid":"%s","kty":"RSA","use":"sig","n":"modulus","e":"AQAB"}]}', [KeyId]));

  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IOAuthConfig>
    .SetResource(Audience)
    .AddAuthorizationServer(Issuer)
    .SetMetadataProvider(FFake);

  // The validator reads everything from the configuration, and reaches it exactly
  // the way it will at runtime: through the server carried by the context.
  FContext := TJRPCContext.Create;
  FContext.AddContent(FServer);

  FAccessToken := TMCPAccessToken.Create;
end;

procedure TClaimsTokenValidatorTest.TearDown;
begin
  FAccessToken.Free;
  FContext.Free;
  FConfig := nil;
  FServer.Free;
  FFake := nil;
end;

function TClaimsTokenValidatorTest.Validate(const AToken: string): TTokenValidationResult;
begin
  Result := RunValidator(TClaimsTokenValidator, FContext, AToken, FAccessToken);
end;

function TClaimsTokenValidatorTest.ValidateWithBareServer(
  const AResource, AAuthorizationServer: string): TTokenValidationResult;
var
  LServer: TJRPCServer;
  LConfig: IOAuthConfig;
  LContext: TJRPCContext;
begin
  LServer := TJRPCServer.Create(nil);
  try
    LConfig := LServer.Plugin.Configure<IOAuthConfig>.SetMetadataProvider(FFake);
    if AResource <> '' then
      LConfig.SetResource(AResource);
    if AAuthorizationServer <> '' then
      LConfig.AddAuthorizationServer(AAuthorizationServer);

    LContext := TJRPCContext.Create;
    try
      LContext.AddContent(LServer);
      Result := RunValidator(TClaimsTokenValidator, LContext, ValidToken, FAccessToken);
    finally
      LContext.Free;
    end;
  finally
    LConfig := nil;
    LServer.Free;
  end;
end;

function TClaimsTokenValidatorTest.ValidToken: string;
begin
  Result := BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","sub":"user-42","exp":%d}',
      [Issuer, Audience, UnixNow + 3600]));
end;

procedure TClaimsTokenValidatorTest.TestValidate_ValidTokenSucceedsAndFillsClaims;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(ValidToken);

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
  Assert.AreEqual('user-42', FAccessToken.Subject);
end;

procedure TClaimsTokenValidatorTest.TestValidate_UnknownIssuerIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"https://evil.example.com","aud":"%s","exp":%d}', [Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_IssuerTrailingSlashIsNotPartOfTheIdentity;
var
  LResult: TTokenValidationResult;
begin
  // Authorization servers are inconsistent about the trailing slash between their
  // discovery URL and the "iss" they mint.
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s/","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
end;

procedure TClaimsTokenValidatorTest.TestValidate_DeclaredTrustedIssuerIsAcceptedAndKeysStayOnTheAuthorizationServer;
const
  TokenIssuer = 'https://sts.example.com/tenant-id/';
var
  LResult: TTokenValidationResult;
begin
  // The Entra ID shape: metadata discovered at one URL, tokens minted with another
  // "iss". The fake provider only serves documents for the configured authorization
  // server, so this also proves the key lookup did not follow the token's issuer.
  FConfig.AddTrustedIssuer(TokenIssuer);

  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","sub":"user-42","exp":%d}',
      [TokenIssuer, Audience, UnixNow + 3600])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
  Assert.AreEqual('user-42', FAccessToken.Subject);
end;

procedure TClaimsTokenValidatorTest.TestValidate_AudienceForAnotherResourceIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"https://other.example.com/api","exp":%d}', [Issuer, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_AudienceArrayContainingTheResourceIsAccepted;
var
  LResult: TTokenValidationResult;
begin
  // "aud" is either a string or an array of strings per RFC 7519.
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":["https://other.example.com/api","%s"],"exp":%d}',
      [Issuer, Audience, UnixNow + 3600])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
end;

procedure TClaimsTokenValidatorTest.TestValidate_ExpiredTokenIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow - 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_TokenExpiredWithinTheClockSkewIsAccepted;
var
  LResult: TTokenValidationResult;
begin
  FConfig.SetClockSkew(120);

  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow - 30])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
end;

procedure TClaimsTokenValidatorTest.TestValidate_TokenWithoutExpirationIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s"}', [Issuer, Audience])));

  Assert.IsFalse(LResult.Success, 'A token that never expires must not be accepted');
end;

procedure TClaimsTokenValidatorTest.TestValidate_NotYetValidTokenIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","nbf":%d,"exp":%d}',
      [Issuer, Audience, UnixNow + 3600, UnixNow + 7200])));

  Assert.IsFalse(LResult.Success);
end;

procedure TClaimsTokenValidatorTest.TestValidate_AlgorithmNoneIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    Format('{"alg":"none","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success, 'An unsigned token must never be accepted');
end;

procedure TClaimsTokenValidatorTest.TestValidate_SymmetricAlgorithmIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // The algorithm confusion attack: the issuer signs with a key pair, so a verifier
  // holds its public key - which is published. Were a symmetric algorithm accepted,
  // that public key would be used as the HMAC secret and anyone able to read the
  // JWKS could mint a token this server accepts. Everything else in this token is
  // exactly what a genuine one carries.
  LResult := Validate(BuildToken(
    Format('{"alg":"HS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","sub":"user-42","exp":%d}',
      [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success, 'A symmetric algorithm must never be accepted');
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
  Assert.AreEqual('', FAccessToken.Subject, 'A rejected token must not populate the access token');
end;

procedure TClaimsTokenValidatorTest.TestValidate_UnregisteredAlgorithmIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // "alg" values are case-sensitive (RFC 7515 §4.1.1): a different spelling is not
  // the registered algorithm, and must not slip past the allow list.
  LResult := Validate(BuildToken(
    Format('{"alg":"rs256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success, 'Only the registered spelling of an algorithm is accepted');
end;

procedure TClaimsTokenValidatorTest.TestValidate_AlgorithmForAnotherKeyTypeIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // The issuer publishes an RSA key, so an EC algorithm cannot be what signed this:
  // which algorithm verifies a token is the issuer's decision, not the token's.
  LResult := Validate(BuildToken(
    Format('{"alg":"ES256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_AlgorithmOtherThanTheOneTheKeyDeclaresIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // A key that names its "alg" is to be used with that one only, even though the
  // token's choice is in the same family and would otherwise be allowed.
  FFake.SetDocument(Issuer + '/keys',
    Format('{"keys":[{"kid":"%s","kty":"RSA","alg":"RS512","use":"sig",' +
      '"n":"modulus","e":"AQAB"}]}', [KeyId]));

  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_AlgorithmTheKeyDeclaresIsAccepted;
var
  LResult: TTokenValidationResult;
begin
  FFake.SetDocument(Issuer + '/keys',
    Format('{"keys":[{"kid":"%s","kty":"RSA","alg":"PS384","use":"sig",' +
      '"n":"modulus","e":"AQAB"}]}', [KeyId]));

  LResult := Validate(BuildToken(
    Format('{"alg":"PS384","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","sub":"user-42","exp":%d}',
      [Issuer, Audience, UnixNow + 3600])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
  Assert.AreEqual('user-42', FAccessToken.Subject);
end;

procedure TClaimsTokenValidatorTest.TestValidate_UnpublishedKeyIdIsRejected;
var
  LResult: TTokenValidationResult;
begin
  LResult := Validate(BuildToken(
    '{"alg":"RS256","kid":"key-nobody-published"}',
    Format('{"iss":"%s","aud":"%s","exp":%d}', [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestValidate_MissingRequiredScopeIsInsufficientScope;
var
  LResult: TTokenValidationResult;
begin
  FConfig.AddRequiredScope('mcp.write');

  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","scope":"openid mcp.read","exp":%d}',
      [Issuer, Audience, UnixNow + 3600])));

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InsufficientScope,
    'A genuine token lacking a permission is not an invalid token');
end;

procedure TClaimsTokenValidatorTest.TestValidate_GrantedRequiredScopeIsAccepted;
var
  LResult: TTokenValidationResult;
begin
  FConfig.AddRequiredScope('mcp.read');

  LResult := Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"%s","aud":"%s","scope":"openid mcp.read","exp":%d}',
      [Issuer, Audience, UnixNow + 3600])));

  Assert.IsTrue(LResult.Success, LResult.ErrorDescription);
end;

procedure TClaimsTokenValidatorTest.TestValidate_RejectedTokenLeavesClaimsEmpty;
begin
  Validate(BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"https://evil.example.com","aud":"%s","sub":"intruder","exp":%d}',
      [Audience, UnixNow + 3600])));

  Assert.AreEqual('', FAccessToken.Subject,
    'A rejected token must not reach the request context');
end;

procedure TClaimsTokenValidatorTest.TestValidate_WithoutTrustedIssuersEverythingIsRejected;
begin
  Assert.IsFalse(ValidateWithBareServer(Audience, '').Success,
    'With no issuer to trust, no token can be validated');
end;

procedure TClaimsTokenValidatorTest.TestValidate_WithoutAudienceEverythingIsRejected;
begin
  Assert.IsFalse(ValidateWithBareServer('', Issuer).Success,
    'With no audience configured, a token for any resource would pass');
end;

procedure TClaimsTokenValidatorTest.TestValidate_WithoutAServerInTheContextEverythingIsRejected;
var
  LContext: TJRPCContext;
begin
  LContext := TJRPCContext.Create;
  try
    // No server means no configuration to validate against: the validator must not
    // fall back to "then everything is fine".
    Assert.IsFalse(
      RunValidator(TClaimsTokenValidator, LContext, ValidToken, FAccessToken).Success);
  finally
    LContext.Free;
  end;
end;

procedure TClaimsTokenValidatorTest.TestSignatureHook_IsCalledWithTheSignedMaterialAndTheResolvedKey;
var
  LToken: string;
  LSegments: TArray<string>;
begin
  TSignatureCheckingValidator.Reset(True);
  LToken := ValidToken;
  LSegments := LToken.Split(['.']);

  Assert.IsTrue(RunValidator(TSignatureCheckingValidator, FContext, LToken, FAccessToken).Success);

  Assert.AreEqual(1, TSignatureCheckingValidator.Calls);
  // The signed material must be the segments as they arrived: re-encoding the decoded
  // JSON would change the bytes and no signature would ever verify.
  Assert.AreEqual(LSegments[0] + '.' + LSegments[1], TSignatureCheckingValidator.SignedMaterial);
  Assert.AreEqual(KeyId, TSignatureCheckingValidator.KeyId,
    'The hook must receive the key the token header pointed to, already resolved');
end;

procedure TClaimsTokenValidatorTest.TestSignatureHook_RejectingTheSignatureRejectsTheToken;
var
  LResult: TTokenValidationResult;
begin
  TSignatureCheckingValidator.Reset(False);

  LResult := RunValidator(TSignatureCheckingValidator, FContext, ValidToken, FAccessToken);

  Assert.IsFalse(LResult.Success, 'A token whose signature does not verify must be rejected');
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
end;

procedure TClaimsTokenValidatorTest.TestSignatureHook_IsNotReachedWhenAClaimAlreadyFailed;
begin
  TSignatureCheckingValidator.Reset(True);

  // Verifying the signature of a token that is already known to be unusable would be
  // wasted work - and, with a real implementation, wasted crypto.
  RunValidator(TSignatureCheckingValidator, FContext, BuildToken(
    Format('{"alg":"RS256","kid":"%s"}', [KeyId]),
    Format('{"iss":"https://evil.example.com","aud":"%s","exp":%d}',
      [Audience, UnixNow + 3600])), FAccessToken);

  Assert.AreEqual(0, TSignatureCheckingValidator.Calls);
end;

procedure TClaimsTokenValidatorTest.TestSignatureHook_RejectedTokenLeavesClaimsEmpty;
begin
  TSignatureCheckingValidator.Reset(False);

  RunValidator(TSignatureCheckingValidator, FContext, ValidToken, FAccessToken);

  Assert.AreEqual('', FAccessToken.Subject);
end;

{ TTokenValidatorContractTest }

procedure TTokenValidatorContractTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IOAuthConfig>;
end;

procedure TTokenValidatorContractTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TTokenValidatorContractTest.TestRegister_ClassNotDerivedFromTheBaseClassIsAccepted;
begin
  FConfig.SetTokenValidatorClass(TStandaloneValidator);

  Assert.IsTrue(FServer.GetConfiguration<TOAuthConfig>.TokenValidatorClass = TStandaloneValidator,
    'Implementing ITokenValidator must be the only requirement');
end;

procedure TTokenValidatorContractTest.TestRegister_ClassNotImplementingTheInterfaceRaises;
begin
  // The class reference is untyped now, so this is what replaces the compiler check.
  Assert.WillRaise(
    procedure
    begin
      FConfig.SetTokenValidatorClass(TNotAValidator);
    end,
    EJRPCException);
end;

procedure TTokenValidatorContractTest.TestRegister_NilClearsTheValidator;
begin
  FConfig.SetTokenValidatorClass(TStandaloneValidator);
  FConfig.SetTokenValidatorClass(nil);

  Assert.IsTrue(FServer.GetConfiguration<TOAuthConfig>.TokenValidatorClass = nil);
end;

procedure TTokenValidatorContractTest.TestCreate_StandaloneValidatorIsBuiltThroughRttiAndInvoked;
var
  LAccessToken: TMCPAccessToken;
  LContext: TJRPCContext;
  LResult: TTokenValidationResult;
begin
  LAccessToken := TMCPAccessToken.Create;
  LContext := TJRPCContext.Create;
  try
    LResult := RunValidator(TStandaloneValidator, LContext, 'a-token', LAccessToken);

    Assert.IsTrue(LResult.Success);
    Assert.AreEqual('standalone', LAccessToken.Subject,
      'The validator must receive the access token it is expected to fill');
  finally
    LContext.Free;
    LAccessToken.Free;
  end;
end;

procedure TTokenValidatorContractTest.TestCreate_ValidatorIsDestroyedWhenTheInterfaceGoesOutOfScope;
var
  LAccessToken: TMCPAccessToken;
  LContext: TJRPCContext;
  LBefore: Integer;
begin
  LAccessToken := TMCPAccessToken.Create;
  LContext := TJRPCContext.Create;
  try
    LBefore := TStandaloneValidator.LiveInstances;

    RunValidator(TStandaloneValidator, LContext, 'a-token', LAccessToken);

    // Nobody frees the instance explicitly: reference counting has to, which is the
    // contract a validator implementation signs up to.
    Assert.AreEqual(LBefore, TStandaloneValidator.LiveInstances,
      'The validator instance must not outlive the request');
  finally
    LContext.Free;
    LAccessToken.Free;
  end;
end;

{ TOAuthConfigValidationTest }

procedure TOAuthConfigValidationTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IOAuthConfig>;
end;

procedure TOAuthConfigValidationTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

function TOAuthConfigValidationTest.GetOAuthConfig: TOAuthConfig;
begin
  Result := FServer.GetConfiguration<TOAuthConfig>;
end;

procedure TOAuthConfigValidationTest.TestValidatorClass_DefaultsToNil;
begin
  Assert.IsTrue(GetOAuthConfig.TokenValidatorClass = nil,
    'Without an explicit validator the server must stay fail-closed');
end;

procedure TOAuthConfigValidationTest.TestSetTokenValidatorClass_IsStored;
begin
  FConfig.SetTokenValidatorClass(TDecodeOnlyTokenValidator);

  Assert.IsTrue(GetOAuthConfig.TokenValidatorClass = TDecodeOnlyTokenValidator);
end;

procedure TOAuthConfigValidationTest.TestTrustedIssuers_AreTheConfiguredAuthorizationServers;
var
  LIssuers: TArray<string>;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .AddAuthorizationServer('https://idp.example.com');

  LIssuers := GetOAuthConfig.TrustedIssuers;

  Assert.AreEqual(1, Length(LIssuers));
  Assert.AreEqual('https://idp.example.com', LIssuers[0]);
end;

procedure TOAuthConfigValidationTest.TestTrustedIssuers_AreTheUpstreamWhenMetadataProxyIsEnabled;
var
  LIssuers: TArray<string>;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .EnableMetadataProxy('https://idp.example.com');

  LIssuers := GetOAuthConfig.TrustedIssuers;

  // The advertised authorization server is the local proxy, but tokens keep
  // carrying the "iss" of the upstream authorization server.
  Assert.AreEqual(1, Length(LIssuers));
  Assert.AreEqual('https://idp.example.com', LIssuers[0]);
  Assert.AreEqual('https://mcp.example.com/oauth-proxy', GetOAuthConfig.AuthorizationServers[0]);
end;

procedure TOAuthConfigValidationTest.TestAddTrustedIssuer_WidensTheDerivedList;
var
  LIssuers: TArray<string>;
begin
  // The Entra ID case: discovery on the v2.0 endpoint, tokens issued as v1.0 with a
  // completely different "iss" host.
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .EnableMetadataProxy('https://login.microsoftonline.com/tenant-id/v2.0')
    .AddTrustedIssuer('https://sts.windows.net/tenant-id/');

  LIssuers := GetOAuthConfig.TrustedIssuers;

  Assert.AreEqual(2, Length(LIssuers), 'The declared issuer must widen, not replace');
  Assert.AreEqual('https://login.microsoftonline.com/tenant-id/v2.0', LIssuers[0]);
  Assert.AreEqual('https://sts.windows.net/tenant-id/', LIssuers[1]);
end;

procedure TOAuthConfigValidationTest.TestAddTrustedIssuer_DoesNotDuplicateADerivedIssuer;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .AddAuthorizationServer('https://idp.example.com')
    .AddTrustedIssuer('https://idp.example.com/');

  // Same issuer, trailing slash aside: it must not be listed twice, if only because
  // the list is printed in the log of every rejected token.
  Assert.AreEqual(1, Length(GetOAuthConfig.TrustedIssuers));
end;

procedure TOAuthConfigValidationTest.TestAddTrustedIssuer_IgnoresEmptyValues;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .AddAuthorizationServer('https://idp.example.com')
    .AddTrustedIssuer('')
    .AddTrustedIssuer('   ');

  // The demo passes an environment variable straight in: unset must mean "nothing".
  Assert.AreEqual(1, Length(GetOAuthConfig.TrustedIssuers));
end;

procedure TOAuthConfigValidationTest.TestResourceMetadata_InsertsTheWellKnownSegmentBeforeThePath;
begin
  FConfig.SetResource('https://mcp.example.com/mcp');

  // RFC 9728 §3.1: the well-known segment goes between the authority and the path.
  // Replacing the path instead would publish every server on this origin at one URL.
  Assert.AreEqual('https://mcp.example.com/.well-known/oauth-protected-resource/mcp',
    GetOAuthConfig.ResourceMetadata);
end;

procedure TOAuthConfigValidationTest.TestResourceMetadata_HasNoSuffixForAnOriginOnlyResource;
begin
  FConfig.SetResource('https://mcp.example.com');

  Assert.AreEqual('https://mcp.example.com/.well-known/oauth-protected-resource',
    GetOAuthConfig.ResourceMetadata);
  Assert.AreEqual('', GetOAuthConfig.ResourcePath);
end;

procedure TOAuthConfigValidationTest.TestResourceMetadata_TrailingSlashOnTheResourceIsNotASeparatePath;
begin
  // The same resource written two ways must publish at one URL, not two.
  FConfig.SetResource('https://mcp.example.com/mcp/');

  Assert.AreEqual('https://mcp.example.com/.well-known/oauth-protected-resource/mcp',
    GetOAuthConfig.ResourceMetadata);
end;

procedure TOAuthConfigValidationTest.TestResourceMetadata_KeepsAPortAndANestedPath;
begin
  FConfig.SetResource('http://localhost:8080/api/mcp');

  Assert.AreEqual('http://localhost:8080/.well-known/oauth-protected-resource/api/mcp',
    GetOAuthConfig.ResourceMetadata);
  Assert.AreEqual('/api/mcp', GetOAuthConfig.ResourcePath);
end;

procedure TOAuthConfigValidationTest.TestResourceMetadata_RequiresAResource;
begin
  // There is no metadata URL to advertise without one, and guessing would put the
  // wrong host in every challenge.
  Assert.WillRaise(
    procedure
    begin
      GetOAuthConfig.ResourceMetadata;
    end,
    Exception);
end;

procedure TOAuthConfigValidationTest.TestAudience_DefaultsToResource;
begin
  FConfig.SetResource('https://mcp.example.com/mcp');

  // The fallback lives in the configuration so that no validator has to know it.
  Assert.AreEqual('https://mcp.example.com/mcp', GetOAuthConfig.Audience);
end;

procedure TOAuthConfigValidationTest.TestAudience_CanBeOverridden;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .SetAudience('api://custom-audience');

  Assert.AreEqual('api://custom-audience', GetOAuthConfig.Audience);
end;

procedure TOAuthConfigValidationTest.TestRequiredScopesAndClockSkew_AreStored;
var
  LConfig: TOAuthConfig;
begin
  FConfig
    .SetResource('https://mcp.example.com/mcp')
    .AddRequiredScope('mcp.read')
    .AddRequiredScope('mcp.write')
    .SetClockSkew(120);

  LConfig := GetOAuthConfig;

  Assert.AreEqual(2, Length(LConfig.RequiredScopes));
  Assert.AreEqual('mcp.read', LConfig.RequiredScopes[0]);
  Assert.AreEqual('mcp.write', LConfig.RequiredScopes[1]);
  Assert.AreEqual(120, LConfig.ClockSkewSeconds);
end;

procedure TOAuthConfigValidationTest.TestClockSkew_HasADefault;
begin
  Assert.AreEqual(TOAuthConfig.DefaultClockSkew, GetOAuthConfig.ClockSkewSeconds);
end;

procedure TOAuthConfigValidationTest.TestMetadataProvider_IsAlwaysAvailable;
begin
  Assert.IsNotNull(GetOAuthConfig.MetadataProvider,
    'Validators read the provider from the configuration, so it must never be nil');
end;

procedure TOAuthConfigValidationTest.TestSetMetadataProvider_ReplacesTheDefaultOne;
var
  LProvider: IOAuthMetadataProvider;
begin
  LProvider := TFakeMetadataProvider.Create;

  FConfig.SetMetadataProvider(LProvider);

  Assert.IsTrue(GetOAuthConfig.MetadataProvider = LProvider);
end;

procedure TOAuthConfigValidationTest.TestSetMetadataProvider_IgnoresNil;
var
  LBefore: IOAuthMetadataProvider;
begin
  LBefore := GetOAuthConfig.MetadataProvider;

  FConfig.SetMetadataProvider(nil);

  Assert.IsTrue(GetOAuthConfig.MetadataProvider = LBefore,
    'A validator must always find a provider: nil cannot remove it');
end;

{ TOAuthMetadataProviderTest }

procedure TOAuthMetadataProviderTest.Setup;
begin
  FFake := TFakeMetadataProvider.Create;
  FProvider := FFake;

  FFake.SetDocument(DiscoveryUrl,
    Format('{"issuer":"%s","jwks_uri":"%s","token_endpoint":"%s/token"}', [Issuer, JwksUrl, Issuer]));
  PublishKeys(['key-1']);
end;

procedure TOAuthMetadataProviderTest.TearDown;
begin
  FProvider := nil;
  FFake := nil;
end;

procedure TOAuthMetadataProviderTest.PublishKeys(const AKeyIds: array of string);
var
  LKeys: TArray<string>;
  LKeyId: string;
begin
  LKeys := [];
  for LKeyId in AKeyIds do
    LKeys := LKeys + [Format('{"kid":"%s","kty":"RSA","use":"sig","n":"modulus-%s","e":"AQAB"}',
      [LKeyId, LKeyId])];

  FFake.SetDocument(JwksUrl, Format('{"keys":[%s]}', [string.Join(',', LKeys)]));
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_ParsesTheDocument;
var
  LMetadata: TOAuthServerMetadata;
begin
  LMetadata := FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(Issuer, LMetadata.Issuer);
  Assert.AreEqual(JwksUrl, LMetadata.JwksUri);
  Assert.AreEqual(Issuer + '/token', LMetadata.TokenEndpoint);
  Assert.IsFalse(LMetadata.IsEmpty);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_IsCachedWithinTheTTL;
begin
  FProvider.GetServerMetadata(Issuer);
  FProvider.GetServerMetadata(Issuer);
  FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(1, FFake.FetchCount, 'The metadata document must be fetched once within its TTL');
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_IsFetchedAgainAfterTheTTL;
begin
  FFake.MetadataTTL := 0;

  FProvider.GetServerMetadata(Issuer);
  FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(2, FFake.FetchCount);
end;

procedure TOAuthMetadataProviderTest.TestDiscoveryUrls_FollowTheSpecifiedOrderForAnIssuerWithAPath;
var
  LUrls: TArray<string>;
begin
  LUrls := TOAuthMetadataProvider.DiscoveryUrlsFor('https://login.example.com/tenant-id/v2.0');

  Assert.AreEqual(3, Length(LUrls));
  Assert.AreEqual('https://login.example.com/.well-known/oauth-authorization-server/tenant-id/v2.0',
    LUrls[0], 'RFC 8414 path insertion comes first');
  Assert.AreEqual('https://login.example.com/.well-known/openid-configuration/tenant-id/v2.0',
    LUrls[1], 'then OIDC discovery, inserted the same way');
  Assert.AreEqual('https://login.example.com/tenant-id/v2.0/.well-known/openid-configuration',
    LUrls[2], 'then OIDC discovery appended after the path');
end;

procedure TOAuthMetadataProviderTest.TestDiscoveryUrls_CollapseToTwoForAnOriginOnlyIssuer;
var
  LUrls: TArray<string>;
begin
  // With no path there is nothing to insert around: the two OIDC forms are one URL.
  LUrls := TOAuthMetadataProvider.DiscoveryUrlsFor('https://idp.example.com/');

  Assert.AreEqual(2, Length(LUrls));
  Assert.AreEqual('https://idp.example.com/.well-known/oauth-authorization-server', LUrls[0]);
  Assert.AreEqual('https://idp.example.com/.well-known/openid-configuration', LUrls[1]);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_FallsBackToOpenIDConnectDiscovery;
var
  LMetadata: TOAuthServerMetadata;
begin
  // An OpenID Connect provider publishes nothing at the RFC 8414 URL. Before the
  // fallback chain existed the reverse was fatal: an authorization server that is
  // OAuth 2.1 but not OIDC could not be discovered at all, so every token was rejected.
  FFake := TFakeMetadataProvider.Create;
  FProvider := FFake;
  FFake.SetDocument(OidcDiscoveryUrl,
    Format('{"issuer":"%s","jwks_uri":"%s"}', [Issuer, JwksUrl]));

  LMetadata := FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(JwksUrl, LMetadata.JwksUri);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_PrefersTheRFC8414Document;
var
  LMetadata: TOAuthServerMetadata;
begin
  // Both published: the order is not arbitrary, it is what the MCP authorization
  // specification prescribes.
  FFake.SetDocument(OidcDiscoveryUrl,
    Format('{"issuer":"%s","jwks_uri":"%s/oidc-keys"}', [Issuer, Issuer]));

  LMetadata := FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(JwksUrl, LMetadata.JwksUri, 'The RFC 8414 document wins');
  Assert.AreEqual(1, FFake.FetchCount, 'The preferred candidate answered, so nothing else is tried');
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_RemembersWhichCandidateWorked;
begin
  // Only the OIDC URL answers, so the first candidate costs a wasted request. Once the
  // working one is known a refresh must go straight to it rather than walking the
  // chain again.
  FFake := TFakeMetadataProvider.Create;
  FProvider := FFake;
  FFake.SetDocument(OidcDiscoveryUrl,
    Format('{"issuer":"%s","jwks_uri":"%s"}', [Issuer, JwksUrl]));
  FFake.MetadataTTL := 0;

  FProvider.GetServerMetadata(Issuer);
  Assert.AreEqual(2, FFake.FetchCount, 'The first attempt walks the chain');

  FFake.ResetFetchCount;
  FProvider.GetServerMetadata(Issuer);
  Assert.AreEqual(1, FFake.FetchCount, 'A refresh goes straight to the URL that worked');
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_DocumentDeclaringAnotherIssuerIsRefused;
begin
  // Whoever answers at the well-known URL decides what the document says. Accepting a
  // document that names a different issuer would let it choose "jwks_uri" - i.e. where
  // this server fetches the keys it verifies every token with.
  FFake.SetDocument(DiscoveryUrl,
    Format('{"issuer":"https://evil.example.com","jwks_uri":"%s"}', [JwksUrl]));

  Assert.WillRaise(
    procedure
    begin
      FProvider.GetServerMetadata(Issuer);
    end,
    EOAuthMetadataException);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_DocumentWithoutAnIssuerIsRefused;
begin
  // RFC 8414 makes "issuer" required, and a document that declares none cannot be
  // checked against the URL it came from.
  FFake.SetDocument(DiscoveryUrl, Format('{"jwks_uri":"%s"}', [JwksUrl]));

  Assert.WillRaise(
    procedure
    begin
      FProvider.GetServerMetadata(Issuer);
    end,
    EOAuthMetadataException);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_IssuerTrailingSlashIsNotPartOfTheIdentity;
var
  LMetadata: TOAuthServerMetadata;
begin
  // Same tolerance the "iss" claim check applies: authorization servers are
  // inconsistent about the trailing slash, and it is not what distinguishes them.
  FFake.SetDocument(DiscoveryUrl,
    Format('{"issuer":"%s/","jwks_uri":"%s"}', [Issuer, JwksUrl]));

  LMetadata := FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(JwksUrl, LMetadata.JwksUri);
end;

procedure TOAuthMetadataProviderTest.TestGetKeys_KeysAreNotFetchedFromAMisdeclaredDocument;
begin
  // The point of the issuer check: a document that fails it must never get as far as
  // having its "jwks_uri" fetched.
  FFake.SetDocument(DiscoveryUrl,
    Format('{"issuer":"https://evil.example.com","jwks_uri":"%s"}', [JwksUrl]));
  FFake.ResetFetchCount;

  Assert.WillRaise(
    procedure
    begin
      FProvider.GetKeys(Issuer);
    end,
    EOAuthMetadataException);

  Assert.IsFalse(FFake.WasFetched(JwksUrl),
    'A document that failed the issuer check must never have its "jwks_uri" fetched');
end;

procedure TOAuthMetadataProviderTest.TestGetKeys_ParsesTheKeySet;
var
  LKeys: TArray<TOAuthJsonWebKey>;
begin
  LKeys := FProvider.GetKeys(Issuer);

  Assert.AreEqual(1, Length(LKeys));
  Assert.AreEqual('key-1', LKeys[0].Kid);
  Assert.AreEqual('RSA', LKeys[0].Kty);
  Assert.AreEqual('modulus-key-1', LKeys[0].N);
  Assert.IsTrue(LKeys[0].IsSignatureKey);
end;

procedure TOAuthMetadataProviderTest.TestGetKeys_ParsesEveryMappedMember;
var
  LKeys: TArray<TOAuthJsonWebKey>;
begin
  FFake.SetDocument(JwksUrl,
    '{"keys":[{"kid":"ec-1","kty":"EC","alg":"ES256","use":"sig","crv":"P-256",' +
    '"x":"x-coordinate","y":"y-coordinate","x5c":["cert-one","cert-two"],' +
    '"unmapped_member":"ignored"}]}');

  LKeys := FProvider.GetKeys(Issuer);

  Assert.AreEqual(1, Length(LKeys));
  Assert.AreEqual('ec-1', LKeys[0].Kid);
  Assert.AreEqual('EC', LKeys[0].Kty);
  Assert.AreEqual('ES256', LKeys[0].Alg);
  Assert.AreEqual('sig', LKeys[0].Use);
  Assert.AreEqual('P-256', LKeys[0].Crv);
  Assert.AreEqual('x-coordinate', LKeys[0].X);
  Assert.AreEqual('y-coordinate', LKeys[0].Y);
  Assert.AreEqual(2, Length(LKeys[0].X5c));
  Assert.AreEqual('cert-one', LKeys[0].X5c[0]);
  Assert.AreEqual('cert-two', LKeys[0].X5c[1]);
end;

procedure TOAuthMetadataProviderTest.TestGetKeys_KeepsTheKeyEntryAsPublished;
var
  LKeys: TArray<TOAuthJsonWebKey>;
begin
  LKeys := FProvider.GetKeys(Issuer);

  // Members not mapped onto the record must still reach a validator that needs them.
  Assert.Contains(LKeys[0].Raw, '"kid"');
  Assert.Contains(LKeys[0].Raw, 'key-1');
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_FindsThePublishedKey;
var
  LKey: TOAuthJsonWebKey;
begin
  Assert.IsTrue(FProvider.TryGetKey(Issuer, 'key-1', LKey));
  Assert.AreEqual('key-1', LKey.Kid);
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_UnknownKeyIdRefreshesOnceAndFindsARotatedKey;
var
  LKey: TOAuthJsonWebKey;
begin
  FFake.KeysRefreshInterval := 0;

  Assert.IsTrue(FProvider.TryGetKey(Issuer, 'key-1', LKey));

  // The identity provider rotates its keys before the cached set expires.
  PublishKeys(['key-2']);

  Assert.IsTrue(FProvider.TryGetKey(Issuer, 'key-2', LKey),
    'An unknown key id must trigger a refresh of the key set');
  Assert.AreEqual('key-2', LKey.Kid);
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_UnknownKeyIdDoesNotRefreshWithinTheRateLimit;
var
  LKey: TOAuthJsonWebKey;
begin
  FProvider.TryGetKey(Issuer, 'key-1', LKey);
  FFake.ResetFetchCount;

  // A client sending random key ids must not turn into one outbound request per
  // incoming request: the default refresh interval blocks these.
  FProvider.TryGetKey(Issuer, 'unknown-a', LKey);
  FProvider.TryGetKey(Issuer, 'unknown-b', LKey);
  FProvider.TryGetKey(Issuer, 'unknown-c', LKey);

  Assert.AreEqual(0, FFake.FetchCount);
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_EmptyKeyIdMatchesTheOnlyPublishedKey;
var
  LKey: TOAuthJsonWebKey;
begin
  Assert.IsTrue(FProvider.TryGetKey(Issuer, '', LKey));
  Assert.AreEqual('key-1', LKey.Kid);
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_EmptyKeyIdIsAmbiguousWithSeveralKeys;
var
  LKey: TOAuthJsonWebKey;
begin
  PublishKeys(['key-1', 'key-2']);

  Assert.IsFalse(FProvider.TryGetKey(Issuer, '', LKey),
    'Picking one of several keys for a token without "kid" would be a guess');
end;

procedure TOAuthMetadataProviderTest.TestTryGetKey_ReturnsFalseInsteadOfRaisingWhenUnreachable;
var
  LKey: TOAuthJsonWebKey;
begin
  FFake.FailFrom := 1;

  Assert.IsFalse(FProvider.TryGetKey(Issuer, 'key-1', LKey),
    'An unreachable identity provider must never look like a valid key lookup');
end;

procedure TOAuthMetadataProviderTest.TestGetKeys_KeepsServingTheCachedSetWhenARefreshFails;
var
  LKeys: TArray<TOAuthJsonWebKey>;
begin
  FProvider.GetKeys(Issuer);

  // From now on every fetch fails, and the cached set has expired: the last known
  // keys must keep being served instead of rejecting every request.
  FFake.KeysTTL := 0;
  FFake.FailFrom := FFake.FetchCount + 1;

  LKeys := FProvider.GetKeys(Issuer);

  Assert.AreEqual(1, Length(LKeys));
  Assert.AreEqual('key-1', LKeys[0].Kid);
end;

procedure TOAuthMetadataProviderTest.TestGetServerMetadata_RaisesWhenNothingWasEverFetched;
begin
  FFake.FailFrom := 1;

  Assert.WillRaise(
    procedure
    begin
      FProvider.GetServerMetadata(Issuer);
    end,
    EOAuthMetadataException);
end;

procedure TOAuthMetadataProviderTest.TestInvalidate_ForcesTheNextFetch;
begin
  FProvider.GetServerMetadata(Issuer);
  FProvider.Invalidate(Issuer);
  FProvider.GetServerMetadata(Issuer);

  Assert.AreEqual(2, FFake.FetchCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TTokenValidationResultTest);
  TDUnitX.RegisterTestFixture(TDecodeOnlyTokenValidatorTest);
  TDUnitX.RegisterTestFixture(TClaimsTokenValidatorTest);
  TDUnitX.RegisterTestFixture(TTokenValidatorContractTest);
  TDUnitX.RegisterTestFixture(TOAuthConfigValidationTest);
  TDUnitX.RegisterTestFixture(TOAuthMetadataProviderTest);

end.
