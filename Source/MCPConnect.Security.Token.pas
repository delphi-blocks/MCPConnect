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
unit MCPConnect.Security.Token;

interface

uses
  System.SysUtils, System.Classes, System.JSON,

  MCPConnect.Configuration.Auth,
  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Security.Jwks;

{$SCOPEDENUMS ON}

resourcestring
  STokenMalformed = 'The access token is not a well-formed JWT';
  STokenPayloadInvalid = 'The access token payload is not a valid JSON object';
  STokenAlgorithmNotAllowed = 'The token signing algorithm is not allowed';
  STokenIssuerNotTrusted = 'The token issuer is not trusted';
  STokenAudienceMismatch = 'The token was not issued for this resource';
  STokenExpired = 'The token has expired';
  STokenNotYetValid = 'The token is not valid yet';
  STokenExpirationMissing = 'The token does not declare an expiration';
  STokenKeyUnknown = 'The token signing key is not published by the issuer';
  STokenKeyAlgorithmMismatch = 'The token signing algorithm cannot be used with the issuer key';
  STokenScopeMissing = 'The token does not carry the required scope';

  STokenDecodeOnlyWarning = 'Access token accepted without any verification: ' +
    'TDecodeOnlyTokenValidator is for demo and development purposes only.';

  STokenNoOAuthConfig = 'The request context carries no server: the OAuth configuration ' +
    'cannot be read and no token can be validated';
  STokenNoTrustedIssuers = 'No trusted issuer is configured: no token can be validated';
  STokenNoAudience = 'No audience is configured: call IOAuthConfig.SetResource or SetAudience';
  STokenNoMetadataProvider = 'No metadata provider is available: signing keys cannot be checked';

type
  /// <summary>
  ///   Error codes of RFC 6750 §3.1, reported back to the client in the
  ///   "WWW-Authenticate" challenge of a 401 response.
  /// </summary>
  TTokenValidationErrorCode = (
    /// <summary>No error, or no error worth naming (e.g. a missing Authorization header).</summary>
    None,
    /// <summary>The request itself is malformed, e.g. more than one authentication scheme.</summary>
    InvalidRequest,
    /// <summary>The token is missing, malformed, forged, expired or otherwise unusable.</summary>
    InvalidToken,
    /// <summary>The token is genuine but does not carry the scopes this server requires.</summary>
    InsufficientScope
  );

  /// <summary>
  ///   Outcome of a token validation. Normal outcomes - including an expired or forged
  ///   token - are reported through this record, not through exceptions.
  /// </summary>
  TTokenValidationResult = record
    Success: Boolean;
    ErrorCode: TTokenValidationErrorCode;
    /// <summary>
    ///   Short, non-sensitive explanation, sent in the challenge as "error_description".
    ///   It must never carry internal details (exception text, stack, configuration).
    /// </summary>
    ErrorDescription: string;

    class function Ok: TTokenValidationResult; static;
    class function Fail(AErrorCode: TTokenValidationErrorCode;
      const ADescription: string): TTokenValidationResult; static;
  end;

  /// <summary>
  ///   Validates the bearer token of an incoming request and, on success, fills in the
  ///   access token object injected in the request context.
  /// </summary>
  /// <remarks>
  ///   Implementing this interface is the only requirement: register any class that
  ///   does with IOAuthConfig.SetTokenValidatorClass. Two constraints come with it,
  ///   both of them Delphi's rather than ours - the class needs a parameterless
  ///   constructor (it is built through RTTI) and must be reference counted
  ///   (descending from TInterfacedObject is the usual way), because the transport
  ///   holds it only through this interface.
  ///   One instance is built per request and released at the end of it, so
  ///   implementations need no thread-safety and must not keep state across requests:
  ///   shared state belongs in IOAuthMetadataProvider.
  /// </remarks>
  ITokenValidator = interface
  ['{2B4A0F9D-6E11-4C7A-9E8F-51B3D2A7C604}']
    /// <summary>
    ///   Validates a token and reports the outcome.
    /// </summary>
    /// <param name="AContext">
    ///   The context of the request, from which everything else is reachable: the
    ///   TJRPCServer, and through it the TOAuthConfig carrying the trusted issuers,
    ///   the audience, the required scopes, the clock skew and the metadata provider.
    /// </param>
    /// <param name="AToken">
    ///   The raw token, with the "Bearer " prefix already removed and trimmed.
    /// </param>
    /// <param name="AAccessToken">
    ///   The access token of the request, owned by the caller. It must be filled in
    ///   only when validation succeeds: everything downstream treats a populated
    ///   access token as trusted.
    /// </param>
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult;
  end;

  /// <summary>
  ///   Convenience base class for token validators: reference counting plus the
  ///   plumbing every validator ends up writing. Deriving from it is never required -
  ///   any class implementing ITokenValidator can be registered.
  /// </summary>
  TTokenValidatorBase = class abstract(TInterfacedObject, ITokenValidator)
  protected
    /// <summary>
    ///   Splits a JWT in compact serialization into its three segments, still encoded.
    /// </summary>
    function SplitToken(const AToken: string; out AHeader, APayload, ASignature: string): Boolean;

    /// <summary>Decodes a base64url segment of the token into its JSON text.</summary>
    function DecodeSegment(const ASegment: string): string;

    /// <summary>
    ///   Reads the "kid" of the token header. Returns an empty string when the header
    ///   cannot be decoded or does not declare one.
    /// </summary>
    function GetKeyId(const AHeaderSegment: string): string;

    /// <summary>
    ///   The OAuth configuration of the server serving this request: trusted issuers,
    ///   audience, required scopes, clock skew and metadata provider. Returns nil when
    ///   the context does not carry a server, which a transport should never do.
    /// </summary>
    function GetOAuthConfig(AContext: TJRPCContext): TOAuthConfig;
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult; virtual; abstract;
  end;

  /// <summary>
  ///   Decodes the token payload and accepts it, without verifying the signature, the
  ///   issuer, the audience or the expiration.
  /// </summary>
  /// <remarks>
  ///   DEVELOPMENT ONLY. A token accepted by this validator can be written by anyone
  ///   in a text editor: it proves nothing about who is calling. It exists so that
  ///   running without validation is an explicit, reviewable line of configuration
  ///   rather than the default behaviour.
  /// </remarks>
  TDecodeOnlyTokenValidator = class(TTokenValidatorBase)
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult; override;
  end;

  /// <summary>
  ///   Checks everything about a token except the one thing that makes it
  ///   unforgeable: the issuer ("iss"), the audience ("aud"), the validity window
  ///   ("exp" / "nbf"), the required scopes, the signing algorithm, and that the key
  ///   the token points to ("kid") is actually published by the issuer and can be
  ///   used with the algorithm the token names.
  ///   It does <b>not</b> verify the signature.
  /// </summary>
  /// <remarks>
  ///   Not a replacement for signature verification. Every claim it inspects is
  ///   attacker-controlled: a forged token that copies the "iss", "aud", "kid" and an
  ///   "exp" in the future from a genuine one passes every check here. What it does
  ///   buy, over <see cref="TDecodeOnlyTokenValidator" />, is that expired tokens,
  ///   tokens issued by another authorization server, tokens minted for a different
  ///   resource and tokens naming a key the issuer never published are all rejected -
  ///   which covers the accidents (a stale token, the wrong tenant, a copy-pasted
  ///   token from another API) rather than the attacks.
  ///   Use it as the base class of a real validator: override Validate, verify the
  ///   signature against the key resolved through <see cref="ResolveKey" />, and call
  ///   inherited for the claim checks.
  /// </remarks>
  TClaimsTokenValidator = class(TTokenValidatorBase)
  private
    function GetAlgorithm(const AHeaderSegment: string): string;
    function MatchTrustedIssuer(AConfig: TOAuthConfig; const AIssuer: string;
      out ATrusted: string): Boolean;
    function AudienceMatches(AConfig: TOAuthConfig; const AAudience: TArray<string>): Boolean;
    function ScopesSatisfied(AConfig: TOAuthConfig; const AScope: string;
      out AMissing: string): Boolean;
    function CheckClaims(AConfig: TOAuthConfig; AClaims: TMCPAccessToken;
      const AHeader, APayload, ASignature: string): TTokenValidationResult;
  protected
    /// <summary>
    ///   Logs what was expected against what the token carried, and builds the failed
    ///   result. The client still only gets ADescription: the log is for whoever has
    ///   to work out why a token that "should work" does not.
    /// </summary>
    function Reject(AErrorCode: TTokenValidationErrorCode;
      const ADescription, AExpected, AFound: string): TTokenValidationResult;

    /// <summary>
    ///   Looks up the published key a token header points to, so that a derived class
    ///   verifying the signature does not have to repeat the lookup.
    /// </summary>
    function ResolveKey(AConfig: TOAuthConfig; const AIssuer, AKeyId: string;
      out AKey: TOAuthJsonWebKey): Boolean;

    /// <summary>
    ///   Algorithms accepted in the token header: the asymmetric JWS algorithms of
    ///   RFC 7518, and nothing else.
    /// </summary>
    /// <remarks>
    ///   This is an allow list rather than a "anything but none" rule, and the
    ///   difference is the whole point. A verifier that accepts a symmetric algorithm
    ///   while holding a public key ends up using that key as the HMAC secret - and
    ///   the public key is, by definition, published. Anyone could then mint a token
    ///   this server would accept. Keeping the two families apart at the door is what
    ///   makes <see cref="CheckSignature" /> meaningful.
    ///   Names are compared case sensitively: "alg" values are case-sensitive ASCII
    ///   strings (RFC 7515 §4.1.1), so a spelling that is not exactly the registered
    ///   one is not that algorithm.
    /// </remarks>
    function IsAlgorithmAllowed(const AAlgorithm: string): Boolean; virtual;

    /// <summary>
    ///   Checks that the algorithm named in the token header can be used with the key
    ///   the issuer published under that "kid": the key's own "alg", when it declares
    ///   one, and the family its "kty" belongs to - "RSA" signs with RS*/PS*, "EC"
    ///   with ES*.
    /// </summary>
    /// <remarks>
    ///   Defence in depth behind <see cref="IsAlgorithmAllowed" />: it stops a token
    ///   from choosing which of the issuer's algorithms to be verified with, instead
    ///   of the issuer deciding.
    ///   A key that declares neither "alg" nor "kty" is not rejected here - there is
    ///   nothing to contradict, and the key set comes from the issuer over TLS rather
    ///   than from the caller. Such a key still has to carry usable material for the
    ///   signature check to get anywhere.
    /// </remarks>
    function KeyMatchesAlgorithm(const AKey: TOAuthJsonWebKey;
      const AAlgorithm: string): Boolean; virtual;

    /// <summary>
    ///   Verifies that the token was signed with AKey. This is the hook that turns
    ///   this validator into a real one: the base implementation checks nothing and
    ///   accepts, because signature verification needs a JOSE/crypto library that the
    ///   core package deliberately does not depend on.
    /// </summary>
    /// <param name="AHeader">The header segment, base64url encoded as it arrived.</param>
    /// <param name="APayload">The payload segment, base64url encoded as it arrived.</param>
    /// <param name="ASignature">The signature segment, base64url encoded.</param>
    /// <param name="AKey">
    ///   The key the header's "kid" resolved to, already fetched and cached.
    /// </param>
    /// <remarks>
    ///   The signed material is the two segments as received, joined by a dot -
    ///   AHeader + '.' + APayload - and not the decoded JSON: re-encoding it would
    ///   change the bytes and every signature would fail.
    ///   Called after the algorithm, issuer, audience and validity window have been
    ///   checked and the key resolved, and before the scope check. Report a failure by
    ///   returning Reject(TTokenValidationErrorCode.InvalidToken, ...) rather than by
    ///   raising.
    /// </remarks>
    function CheckSignature(const AHeader, APayload, ASignature: string;
      const AKey: TOAuthJsonWebKey): TTokenValidationResult; virtual;
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AAccessToken: TMCPAccessToken): TTokenValidationResult; override;
  end;

/// <summary>
///   Decodes a base64url encoded string (RFC 4648 §5) into its UTF-8 text,
///   restoring the padding the encoding strips.
/// </summary>
function Base64UrlDecode(const AInput: string): string;

/// <summary>
///   Returns the RFC 6750 name of an error code ('invalid_token', ...), or an empty
///   string for TTokenValidationErrorCode.None.
/// </summary>
function TokenValidationErrorCodeToString(AErrorCode: TTokenValidationErrorCode): string;

/// <summary>
///   Builds the "WWW-Authenticate" challenge sent with a 401: the Bearer scheme, the
///   realm, the URL of this resource's metadata document, and - once validation has
///   actually rejected something - the RFC 6750 error and its description.
/// </summary>
/// <remarks>
///   Every parameter value is quoted and sanitized, so no configured or validator
///   supplied string can produce a malformed header. Lives here, rather than in the
///   transport that sends it, because a challenge is the RFC 6750 rendering of a
///   <see cref="TTokenValidationResult" /> and belongs with the vocabulary it uses.
/// </remarks>
function BuildBearerChallenge(const ARealm, AResourceMetadata: string;
  const AResult: TTokenValidationResult): string;

implementation

uses
  System.NetEncoding, System.DateUtils, System.StrUtils,

  Logify,
  MCPConnect.JRPC.Server;

function Base64UrlDecode(const AInput: string): string;
var
  LValue: string;
begin
  LValue := AInput.Replace('-', '+').Replace('_', '/');
  case Length(LValue) mod 4 of
    2: LValue := LValue + '==';
    3: LValue := LValue + '=';
  end;
  Result := TEncoding.UTF8.GetString(TNetEncoding.Base64.DecodeStringToBytes(LValue));
end;

function TokenValidationErrorCodeToString(AErrorCode: TTokenValidationErrorCode): string;
begin
  case AErrorCode of
    TTokenValidationErrorCode.None: Result := '';
    TTokenValidationErrorCode.InvalidRequest: Result := 'invalid_request';
    TTokenValidationErrorCode.InvalidToken: Result := 'invalid_token';
    TTokenValidationErrorCode.InsufficientScope: Result := 'insufficient_scope';
  else
    Assert(False, 'Unhandled token validation error code');
    Result := '';
  end;
end;

function BuildBearerChallenge(const ARealm, AResourceMetadata: string;
  const AResult: TTokenValidationResult): string;

  // Values travel inside a quoted-string, so a quote or a line break in one would end
  // it early: truncating the challenge, or letting the value inject parameters - or a
  // whole further header - of its own. The realm and the metadata URL come from
  // configuration and the description from a validator, and none of them is trusted
  // enough to skip this.
  function Sanitize(const AValue: string): string;
  begin
    Result := AValue.Replace('"', '''').Replace(#13, ' ').Replace(#10, ' ').Trim;
  end;

begin
  // Every value is quoted, the metadata URL included. RFC 7235 admits an auth-param
  // value as either a bare token or a quoted-string, and a URL is not a token - ":"
  // and "/" are delimiters - so unquoted it is simply malformed, and a strict client
  // parser drops the parameter and never finds the metadata document. RFC 9728 §5.1
  // spells "resource_metadata" out with its quotes for that reason.
  Result := Format('Bearer realm="%s", resource_metadata="%s"',
    [Sanitize(ARealm), Sanitize(AResourceMetadata)]);

  // Nothing to report: a request that simply arrived without a token gets the bare
  // challenge, not an error naming something it never did.
  if AResult.ErrorCode = TTokenValidationErrorCode.None then
    Exit;

  Result := Result + Format(', error="%s"',
    [TokenValidationErrorCodeToString(AResult.ErrorCode)]);

  if AResult.ErrorDescription <> '' then
    Result := Result + Format(', error_description="%s"',
      [Sanitize(AResult.ErrorDescription)]);
end;

{ TTokenValidationResult }

class function TTokenValidationResult.Ok: TTokenValidationResult;
begin
  Result.Success := True;
  Result.ErrorCode := TTokenValidationErrorCode.None;
  Result.ErrorDescription := '';
end;

class function TTokenValidationResult.Fail(AErrorCode: TTokenValidationErrorCode;
  const ADescription: string): TTokenValidationResult;
begin
  Result.Success := False;
  Result.ErrorCode := AErrorCode;
  Result.ErrorDescription := ADescription;
end;

{ TTokenValidatorBase }

function TTokenValidatorBase.GetOAuthConfig(AContext: TJRPCContext): TOAuthConfig;
var
  LServer: TJRPCServer;
begin
  Result := nil;
  if not Assigned(AContext) then
    Exit;

  // Same route the transport handler takes to read its own configurations.
  LServer := AContext.FindContextDataAs<TJRPCServer>;
  if Assigned(LServer) then
    Result := LServer.GetConfiguration<TOAuthConfig>;
end;

function TTokenValidatorBase.SplitToken(const AToken: string;
  out AHeader, APayload, ASignature: string): Boolean;
var
  LParts: TArray<string>;
begin
  AHeader := '';
  APayload := '';
  ASignature := '';

  LParts := AToken.Split(['.']);
  if Length(LParts) <> 3 then
    Exit(False);

  AHeader := LParts[0];
  APayload := LParts[1];
  ASignature := LParts[2];

  Result := (AHeader <> '') and (APayload <> '');
end;

function TTokenValidatorBase.DecodeSegment(const ASegment: string): string;
begin
  Result := Base64UrlDecode(ASegment);
end;

function TTokenValidatorBase.GetKeyId(const AHeaderSegment: string): string;
var
  LJSON: TJSONValue;
begin
  Result := '';

  LJSON := TJSONObject.ParseJSONValue(DecodeSegment(AHeaderSegment));
  try
    if LJSON is TJSONObject then
      Result := (LJSON as TJSONObject).GetValue<string>('kid', '');
  finally
    LJSON.Free;
  end;
end;

{ TDecodeOnlyTokenValidator }

function TDecodeOnlyTokenValidator.Validate(AContext: TJRPCContext;
  const AToken: string; AAccessToken: TMCPAccessToken): TTokenValidationResult;
var
  LHeader, LPayload, LSignature: string;
begin
  if not SplitToken(AToken, LHeader, LPayload, LSignature) then
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, STokenMalformed));

  try
    AAccessToken.FromString(DecodeSegment(LPayload));
  except
    on Exception do
      Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, STokenPayloadInvalid));
  end;

  // Logged on every accepted token, on purpose: a server running unverified must
  // say so for as long as it keeps doing it, not once at startup.
  Logger.LogWarning(STokenDecodeOnlyWarning);

  Result := TTokenValidationResult.Ok;
end;

{ TClaimsTokenValidator }

function TClaimsTokenValidator.IsAlgorithmAllowed(const AAlgorithm: string): Boolean;
const
  // The asymmetric JWS algorithms of RFC 7518 §3.1. Everything else is refused,
  // including "none", an empty "alg", and every symmetric (HS*) algorithm: see the
  // remarks on the declaration for why the symmetric family cannot be let in.
  LAllowed: array[0..8] of string = (
    'RS256', 'RS384', 'RS512',
    'PS256', 'PS384', 'PS512',
    'ES256', 'ES384', 'ES512'
  );
begin
  Result := IndexStr(AAlgorithm, LAllowed) >= 0;
end;

function TClaimsTokenValidator.KeyMatchesAlgorithm(const AKey: TOAuthJsonWebKey; const AAlgorithm: string): Boolean;
begin
  // A key that names its algorithm is to be used with that one only.
  if (AKey.Alg <> '') and (AKey.Alg <> AAlgorithm) then
    Exit(False);

  // "kty" is what the key material actually is, so it decides which signature
  // family can be computed with it at all.
  if SameText(AKey.Kty, 'RSA') then
    Exit(AAlgorithm.StartsWith('RS') or AAlgorithm.StartsWith('PS'));

  if SameText(AKey.Kty, 'EC') then
    Exit(AAlgorithm.StartsWith('ES'));

  // An unknown or undeclared "kty" leaves nothing to compare against - see the
  // remarks on the declaration.
  Result := True;
end;

function TClaimsTokenValidator.GetAlgorithm(const AHeaderSegment: string): string;
var
  LJSON: TJSONValue;
begin
  Result := '';

  LJSON := TJSONObject.ParseJSONValue(DecodeSegment(AHeaderSegment));
  try
    if LJSON is TJSONObject then
      Result := (LJSON as TJSONObject).GetValue<string>('alg', '');
  finally
    LJSON.Free;
  end;
end;

function TClaimsTokenValidator.MatchTrustedIssuer(AConfig: TOAuthConfig;
  const AIssuer: string; out ATrusted: string): Boolean;
var
  LCandidate: string;
begin
  ATrusted := '';
  if AIssuer = '' then
    Exit(False);

  for LCandidate in AConfig.TrustedIssuers do
    if TOAuthConfig.SameIssuer(LCandidate, AIssuer) then
    begin
      ATrusted := LCandidate;
      Exit(True);
    end;

  Result := False;
end;

function TClaimsTokenValidator.AudienceMatches(AConfig: TOAuthConfig;
  const AAudience: TArray<string>): Boolean;
var
  LValue: string;
begin
  // Compared as a URI, so that the case of the path counts: "aud" names the resource a
  // token may be spent at, and two paths differing only in case are two resources.
  for LValue in AAudience do
    if TOAuthConfig.SameUri(LValue, AConfig.Audience) then
      Exit(True);

  Result := False;
end;

function TClaimsTokenValidator.ScopesSatisfied(AConfig: TOAuthConfig;
  const AScope: string; out AMissing: string): Boolean;
var
  LGranted: TArray<string>;
  LRequired, LValue: string;
  LFound: Boolean;
begin
  AMissing := '';
  LGranted := AScope.Split([' '], TStringSplitOptions.ExcludeEmpty);

  for LRequired in AConfig.RequiredScopes do
  begin
    LFound := False;
    for LValue in LGranted do
      if LValue = LRequired then
      begin
        LFound := True;
        Break;
      end;

    if not LFound then
    begin
      AMissing := LRequired;
      Exit(False);
    end;
  end;

  Result := True;
end;

function TClaimsTokenValidator.Reject(AErrorCode: TTokenValidationErrorCode;
  const ADescription, AExpected, AFound: string): TTokenValidationResult;
begin
  Logger.LogWarning('Token rejected - %s (expected: %s, found: %s)',
    [ADescription, AExpected, AFound]);

  Result := TTokenValidationResult.Fail(AErrorCode, ADescription);
end;

function TClaimsTokenValidator.ResolveKey(AConfig: TOAuthConfig;
  const AIssuer, AKeyId: string; out AKey: TOAuthJsonWebKey): Boolean;
begin
  AKey := Default(TOAuthJsonWebKey);
  if not Assigned(AConfig) or not Assigned(AConfig.MetadataProvider) then
    Exit(False);

  Result := AConfig.MetadataProvider.TryGetKey(AIssuer, AKeyId, AKey);
end;

function TClaimsTokenValidator.CheckSignature(const AHeader, APayload, ASignature: string;
  const AKey: TOAuthJsonWebKey): TTokenValidationResult;
begin
  // Deliberately empty: see the remarks on the declaration. Override to verify.
  Result := TTokenValidationResult.Ok;
end;

function TClaimsTokenValidator.CheckClaims(AConfig: TOAuthConfig;
  AClaims: TMCPAccessToken; const AHeader, APayload, ASignature: string): TTokenValidationResult;

  // None of the values reported below is a secret: they are the routing information
  // of the token (who issued it, for whom, until when, with which key), which is
  // exactly what an operator needs to see to fix a misconfiguration. The token
  // itself, and the claims identifying the user, are never logged.
  function OrNone(const AValue: string): string;
  begin
    if AValue = '' then
      Result := '<none>'
    else
      Result := AValue;
  end;

  function AsUTC(const AValue: TDateTime): string;
  begin
    Result := DateToISO8601(AValue, True);
  end;

var
  LAlgorithm, LKeyId, LKeySource, LTrustedIssuer, LMissingScope: string;
  LNowUTC: TDateTime;
  LKey: TOAuthJsonWebKey;
begin
  LAlgorithm := GetAlgorithm(AHeader);
  if not IsAlgorithmAllowed(LAlgorithm) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenAlgorithmNotAllowed,
      'a signing algorithm other than "none"', OrNone(LAlgorithm)));

  if not MatchTrustedIssuer(AConfig, AClaims.Issuer, LTrustedIssuer) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenIssuerNotTrusted,
      string.Join(', ', AConfig.TrustedIssuers), OrNone(AClaims.Issuer)));

  if not AudienceMatches(AConfig, AClaims.Audience) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenAudienceMismatch,
      AConfig.Audience, OrNone(string.Join(', ', AClaims.Audience))));

  // Claim times are UTC (TMCPAccessToken converts them through UnixToDateTime).
  LNowUTC := TTimeZone.Local.ToUniversalTime(Now);

  // A token that never expires is not a session, it is a password with extra steps.
  if AClaims.Expiration = 0 then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenExpirationMissing,
      'an "exp" claim', '<none>'));

  if IncSecond(AClaims.Expiration, AConfig.ClockSkewSeconds) < LNowUTC then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenExpired,
      Format('an expiration at or after %s (clock skew %ds)', [AsUTC(LNowUTC), AConfig.ClockSkewSeconds]),
      AsUTC(AClaims.Expiration)));

  if (AClaims.NotBefore > 0) and (IncSecond(AClaims.NotBefore, -AConfig.ClockSkewSeconds) > LNowUTC) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenNotYetValid,
      Format('a "nbf" at or before %s (clock skew %ds)', [AsUTC(LNowUTC), AConfig.ClockSkewSeconds]),
      AsUTC(AClaims.NotBefore)));

  // Keys are fetched from an authorization server this configuration knows, never
  // from a URL the token pointed us at - see TOAuthConfig.KeySourceFor.
  LKeySource := AConfig.KeySourceFor(LTrustedIssuer);
  LKeyId := GetKeyId(AHeader);
  if not ResolveKey(AConfig, LKeySource, LKeyId, LKey) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenKeyUnknown,
      Format('a key published by %s', [LKeySource]), OrNone(LKeyId)));

  // Which algorithm verifies this token is the issuer's decision, not the token's:
  // the header may only name one the resolved key can actually be used with.
  if not KeyMatchesAlgorithm(LKey, LAlgorithm) then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, STokenKeyAlgorithmMismatch,
      Format('an algorithm usable with key "%s" (kty %s, alg %s)',
        [OrNone(LKey.Kid), OrNone(LKey.Kty), OrNone(LKey.Alg)]),
      LAlgorithm));

  // The hook a real validator overrides. Here rather than earlier because it needs
  // the resolved key, and before the scope check because a token that cannot be
  // proven authentic should not have its permissions discussed.
  Result := CheckSignature(AHeader, APayload, ASignature, LKey);
  if not Result.Success then
    Exit;

  if not ScopesSatisfied(AConfig, AClaims.Scope, LMissingScope) then
    Exit(Reject(TTokenValidationErrorCode.InsufficientScope, STokenScopeMissing,
      LMissingScope, OrNone(AClaims.Scope)));

  Result := TTokenValidationResult.Ok;
end;

function TClaimsTokenValidator.Validate(AContext: TJRPCContext;
  const AToken: string; AAccessToken: TMCPAccessToken): TTokenValidationResult;
var
  LHeader, LPayload, LSignature: string;
  LConfig: TOAuthConfig;
  LClaims: TMCPAccessToken;
begin
  LConfig := GetOAuthConfig(AContext);

  // Misconfigurations are reported to the log, never to the client: what this server
  // is missing is none of a caller's business.
  if not Assigned(LConfig) then
  begin
    Logger.LogWarning(STokenNoOAuthConfig);
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
  end;

  if Length(LConfig.TrustedIssuers) = 0 then
  begin
    Logger.LogWarning(STokenNoTrustedIssuers);
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
  end;

  if LConfig.Audience = '' then
  begin
    Logger.LogWarning(STokenNoAudience);
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
  end;

  if not Assigned(LConfig.MetadataProvider) then
  begin
    Logger.LogWarning(STokenNoMetadataProvider);
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
  end;

  if not SplitToken(AToken, LHeader, LPayload, LSignature) then
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, STokenMalformed));

  // The claims are read into a throwaway token first: the one the request will see
  // must stay empty unless every check passes.
  LClaims := TMCPAccessToken.Create;
  try
    try
      LClaims.FromString(DecodeSegment(LPayload));
    except
      on Exception do
        Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, STokenPayloadInvalid));
    end;

    Result := CheckClaims(LConfig, LClaims, LHeader, LPayload, LSignature);
    if not Result.Success then
      Exit;

    AAccessToken.FromString(LClaims.ToString);
  finally
    LClaims.Free;
  end;
end;

end.
