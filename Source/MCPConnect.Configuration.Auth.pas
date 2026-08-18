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
unit MCPConnect.Configuration.Auth;

interface

uses
  System.Classes, System.SysUtils, System.Net.URLClient,
  MCPConnect.Configuration.Core,
  MCPConnect.Security.Jwks;

{$SCOPEDENUMS ON}

resourcestring
  SOAuthResourceNotSpecified = 'OAuth resource config not specified';
  SOAuthNoValidatorWarning = 'OAuth is enabled but no token validator is registered: ' +
    'every request carrying a bearer token will be rejected. ' +
    'Use IOAuthConfig.SetTokenValidatorClass to register one.';
  SOAuthDecodeOnlyValidatorWarning = 'A decode-only token validator is registered: ' +
    'access tokens are decoded but their signature, issuer, audience and expiration ' +
    'are not verified. Development use only.';
  SOAuthValidatorClassInvalidFmt = 'Class [%s] cannot be used as a token validator: ' +
    'it does not implement ITokenValidator';
  SOAuthResourceRequired = 'OAuth is enabled but no resource URL is configured: call ' +
    'IOAuthConfig.SetResource with the public URL clients connect to. Without it there is no ' +
    'metadata URL to advertise in a challenge and no default audience to validate tokens against.';
  SOAuthInsecureUrlWarningFmt = 'OAuth URL "%s" is not https. Access tokens and authorization ' +
    'codes are bearer credentials: over plain HTTP they are readable in transit. Acceptable on ' +
    'localhost while developing, never off it.';
  SOAuthMetadataProxyWarning = 'The authorization server metadata proxy is enabled. The ' +
    'republished document declares the local proxy URL as its "issuer", so a client that checks ' +
    'that against the URL it fetched the document from (RFC 8414 section 3.3) accepts it - while ' +
    'a client that checks the "iss" of the authorization response (RFC 9207) sees the upstream ' +
    'issuer instead and rejects it. A patch-only proxy cannot satisfy both; if your client fails ' +
    'with an issuer mismatch after the redirect, this is why.';

type
  /// <summary>
  ///   Specifies where the authentication token should be extracted from
  ///   in incoming HTTP requests.
  /// </summary>
  TAuthTokenLocation = (
    /// <summary>HTTP Authorization header with Bearer scheme (e.g., "Authorization: Bearer token")</summary>
    Bearer,
    /// <summary>HTTP Cookie (requires SetTokenCustomHeader to specify cookie name)</summary>
    Cookie,
    /// <summary>Custom HTTP header (requires SetTokenCustomHeader to specify header name)</summary>
    Header
  );

  /// <summary>
  ///   Configuration interface for token-based authentication in JSON-RPC servers.
  ///   Supports Bearer tokens, cookie-based authentication, and custom header tokens.
  ///   Inherits fluent API methods (ApplyConfig, BackToApp) from IJRPCConfiguration.
  /// </summary>
  /// <remarks>
  ///   Token authentication is checked before processing JSON-RPC requests.
  ///   If the token doesn't match, the request is rejected with an authentication error.
  ///   This is a simple token comparison - for more complex authentication schemes,
  ///   implement custom authentication logic in your transport layer.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Bearer token authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('my-secret-token-12345')
  ///     .SetTokenLocation(TAuthTokenLocation.Bearer)
  ///     .ApplyConfig;
  ///   // Client must send: Authorization: Bearer my-secret-token-12345
  ///
  ///   // Cookie-based authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('session-value')
  ///     .SetTokenLocation(TAuthTokenLocation.Cookie)
  ///     .SetTokenCustomHeader('SessionId')
  ///     .ApplyConfig;
  ///   // Client must send: Cookie: SessionId=session-value
  ///
  ///   // Custom header authentication:
  ///   FJRPCServer.Plugin.Configure&lt;IAuthTokenConfig&gt;
  ///     .SetToken('api-key-xyz')
  ///     .SetTokenLocation(TAuthTokenLocation.Header)
  ///     .SetTokenCustomHeader('X-API-Key')
  ///     .ApplyConfig;
  ///   // Client must send: X-API-Key: api-key-xyz
  ///   </code>
  /// </example>
  IAuthTokenConfig = interface(IJRPCConfiguration)
  ['{5E98537E-1A8F-44D9-8100-CB3CCC7C0BFC}']
    /// <summary>
    ///   Sets the authentication token that will be compared against incoming requests.
    /// </summary>
    /// <param name="AToken">The token string to match (case-sensitive comparison)</param>
    /// <returns>Self for fluent chaining</returns>
    function SetToken(const AToken: string): IAuthTokenConfig;

    /// <summary>
    ///   Specifies where to extract the authentication token from HTTP requests.
    /// </summary>
    /// <param name="ALocation">Token location: Bearer (default), Cookie, or Header</param>
    /// <returns>Self for fluent chaining</returns>
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;

    /// <summary>
    ///   Sets the custom header or cookie name when using Cookie or Header locations.
    ///   Required when SetTokenLocation is Cookie or Header, ignored for Bearer.
    /// </summary>
    /// <param name="ACustomHeader">
    ///   For Cookie: the cookie name to extract
    ///   For Header: the HTTP header name to extract (e.g., 'X-API-Key')
    /// </param>
    /// <returns>Self for fluent chaining</returns>
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;
  end;

  IOAuthConfig =  interface(IJRPCConfiguration)
    ['{00A0F0C3-5865-43DF-A710-782815D3989E}']
    function SetRealm(const ARealm: string): IOAuthConfig;
    function SetResource(const AUrl: string): IOAuthConfig;
    function AddAuthorizationServer(const AAuthorizationServer: string): IOAuthConfig;
    function AddScopesSupported(const AScopesSupported: string): IOAuthConfig;

    /// <summary>
    ///   Enables a local proxy for the authorization server's metadata document
    ///   (RFC 8414 / OIDC Discovery). The server will fetch
    ///   "&lt;AUpstreamIssuer&gt;/.well-known/openid-configuration", inject
    ///   "code_challenge_methods_supported": ["S256"] when the upstream document
    ///   does not advertise it, rewrite "issuer" to the local proxy URL, and
    ///   republish the result on a local well-known path. That local path is
    ///   registered as the authorization server instead of the upstream URL
    ///   (replaces a manual AddAuthorizationServer call).
    /// </summary>
    /// <remarks>
    ///   Useful for authorization servers (e.g. Microsoft Entra ID) that support
    ///   PKCE but do not advertise it in their discovery document, which some
    ///   strict MCP OAuth clients require. SetResource must be called first.
    ///   Everything other than those two members is passed through untouched, so the
    ///   authorization and token exchanges still happen directly against the upstream
    ///   server; only the discovery document is patched.
    ///   The "issuer" rewrite is a trade, not a free improvement. It exists so that a
    ///   client checking the document's "issuer" against the URL it was fetched from
    ///   (RFC 8414 3.3) accepts it - which is the whole point of serving it locally.
    ///   The cost is that the upstream server keeps minting its own issuer, so a client
    ///   checking the "iss" of the authorization response (RFC 9207) sees a mismatch.
    ///   A proxy that only patches the document cannot make both true, and which one
    ///   matters depends on the client. Access tokens are unaffected: they carry the
    ///   upstream "iss", and TrustedIssuers already expects exactly that.
    /// </remarks>
    function EnableMetadataProxy(const AUpstreamIssuer: string): IOAuthConfig;

    /// <summary>
    ///   Registers the class used to validate the bearer token of every incoming
    ///   request. One instance is built per request and asked for ITokenValidator;
    ///   everything it needs it reads from the request context.
    /// </summary>
    /// <param name="AClass">
    ///   Any class implementing ITokenValidator (MCPConnect.Security.Token) with a
    ///   parameterless constructor. It must be reference counted - descending from
    ///   TInterfacedObject is the usual way - because the transport holds it only
    ///   through the interface. Deriving from TTokenValidatorBase is convenient but
    ///   never required.
    /// </param>
    /// <exception cref="EJRPCException">
    ///   AClass does not implement ITokenValidator. Checked here, at configuration
    ///   time, because the class reference is untyped and the compiler cannot.
    /// </exception>
    /// <remarks>
    ///   Without a validator class the server is fail-closed: every request carrying a
    ///   bearer token is answered with 401. Register TDecodeOnlyTokenValidator
    ///   (MCPConnect.Security.Token) to reproduce the legacy "decode, do not verify"
    ///   behaviour - development only, it accepts forged tokens.
    /// </remarks>
    function SetTokenValidatorClass(AClass: TClass): IOAuthConfig;

    /// <summary>
    ///   Replaces the source of the authorization server metadata and public keys.
    ///   A default one is always in place, so this is only needed to plug in another
    ///   HTTP stack, a cache shared between servers, or a test double.
    /// </summary>
    /// <remarks>A nil provider is ignored: validators must always find one.</remarks>
    function SetMetadataProvider(const AProvider: IOAuthMetadataProvider): IOAuthConfig;

    /// <summary>
    ///   Declares an issuer whose tokens are accepted, on top of the ones derived from
    ///   the configured authorization servers. Empty values are ignored, and it can be
    ///   called multiple times.
    /// </summary>
    /// <remarks>
    ///   Needed whenever an authorization server mints an "iss" that is not the URL its
    ///   metadata was discovered from. Microsoft Entra ID is the common case: an API
    ///   left at the default "requestedAccessTokenVersion" receives v1.0 access tokens,
    ///   whose "iss" is "https://sts.windows.net/&lt;tenant&gt;/", while discovery happens
    ///   against "https://login.microsoftonline.com/&lt;tenant&gt;/v2.0". Signing keys are
    ///   still fetched from the configured authorization server - only the accepted
    ///   "iss" values are widened.
    /// </remarks>
    function AddTrustedIssuer(const AIssuer: string): IOAuthConfig;

    /// <summary>
    ///   Value the "aud" claim of the token must contain. Defaults to the resource URL
    ///   set with SetResource, which is what RFC 8707 resource indicators produce.
    /// </summary>
    function SetAudience(const AAudience: string): IOAuthConfig;

    /// <summary>
    ///   Adds a scope the token must carry. A token missing any of them is rejected
    ///   with "insufficient_scope". Can be called multiple times.
    /// </summary>
    function AddRequiredScope(const AScope: string): IOAuthConfig;

    /// <summary>
    ///   Tolerance applied to the "exp" and "nbf" claims, in seconds (default 60),
    ///   to absorb the clock drift between this server and the authorization server.
    /// </summary>
    function SetClockSkew(ASeconds: Integer): IOAuthConfig;

    /// <summary>
    ///   Lifetime of the cached JSON Web Key Set, in seconds (default 3600). A key
    ///   rotation is picked up before this expires, through a rate-limited refresh
    ///   triggered by an unknown key id.
    /// </summary>
    function SetKeyCacheTTL(ASeconds: Integer): IOAuthConfig;
  end;

  [Implements(IOAuthConfig)]
  TOAuthConfig = class(TJRPCConfiguration, IOAuthConfig)
  public const
    ProtectedResourcePath = '/.well-known/oauth-protected-resource';
    MetadataProxyPath = '/oauth-proxy';
    DefaultRealm = 'mcp';

    /// <summary>
    ///   Tolerance applied to the "exp" and "nbf" claims, in seconds, when none is
    ///   configured.
    /// </summary>
    DefaultClockSkew = 60;
  private
    FResource: string;
    FRealm: string;
    FAuthorizationServers: TArray<string>;
    FScopesSupported: TArray<string>;
    FMetadataProxyUpstream: string;
    FTokenValidatorClass: TClass;
    FExtraTrustedIssuers: TArray<string>;
    FAudience: string;
    FRequiredScopes: TArray<string>;
    FClockSkewSeconds: Integer;
    FKeyCacheTTL: Integer;
    FMetadataProvider: IOAuthMetadataProvider;
    function GetResourceMetadata: string;
    function GetResourcePath: string;
    function GetMetadataProxyUrl: string;
    function GetMetadataProxyEnabled: Boolean;
    function GetDiscoveryIssuers: TArray<string>;
    function GetTrustedIssuers: TArray<string>;
    function GetAudience: string;
    function GetMetadataProvider: IOAuthMetadataProvider;
  public
    function SetRealm(const ARealm: string): IOAuthConfig;
    function SetResource(const AUrl: string): IOAuthConfig;
    function AddAuthorizationServer(const AAuthorizationServer: string): IOAuthConfig;
    function AddScopesSupported(const AScopesSupported: string): IOAuthConfig;
    function EnableMetadataProxy(const AUpstreamIssuer: string): IOAuthConfig;
    function SetTokenValidatorClass(AClass: TClass): IOAuthConfig;
    function SetMetadataProvider(const AProvider: IOAuthMetadataProvider): IOAuthConfig;
    function AddTrustedIssuer(const AIssuer: string): IOAuthConfig;
    function SetAudience(const AAudience: string): IOAuthConfig;
    function AddRequiredScope(const AScope: string): IOAuthConfig;
    function SetClockSkew(ASeconds: Integer): IOAuthConfig;
    function SetKeyCacheTTL(ASeconds: Integer): IOAuthConfig;

    function ApplyConfig: IJRPCApplication; override;

    /// <summary>
    ///   Compares two issuer identifiers. The trailing slash is not part of the
    ///   identity: authorization servers are inconsistent about it between their
    ///   discovery URL and the "iss" they mint.
    /// </summary>
    class function SameIssuer(const A, B: string): Boolean; static;

    /// <summary>
    ///   Compares two URLs the way the URI they are decides: the scheme and the
    ///   authority case-insensitively (RFC 3986 §3.1 and §3.2.2 define them that way),
    ///   everything after them exactly. A trailing slash is ignored.
    /// </summary>
    /// <remarks>
    ///   Used for both issuer and audience comparison, which are identity checks: two
    ///   paths differing only in case are two different resources, and folding them
    ///   together would let a token minted for one be spent at the other.
    /// </remarks>
    class function SameUri(const A, B: string): Boolean; static;

    /// <summary>
    ///   Whether a URL points at this machine, where plain HTTP is a normal thing to be
    ///   doing while developing.
    /// </summary>
    class function IsLoopbackUrl(const AUrl: string): Boolean; static;

    /// <summary>
    ///   The authorization server whose published keys verify a token issued by
    ///   AIssuer: AIssuer itself when it is one of the configured authorization
    ///   servers, the first configured one otherwise.
    /// </summary>
    /// <remarks>
    ///   Declaring a trusted issuer widens which "iss" is accepted; it must never
    ///   redirect this server to fetch signing keys from a URL it was never configured
    ///   with. So an issuer added through AddTrustedIssuer is verified with the keys of
    ///   the authorization server that was configured, not with keys fetched from
    ///   itself.
    /// </remarks>
    function KeySourceFor(const AIssuer: string): string;

    property Realm: string read FRealm;
    property Resource: string read FResource;
    property AuthorizationServers: TArray<string> read FAuthorizationServers;
    property ScopesSupported: TArray<string> read FScopesSupported;
    /// <summary>
    ///   URL of this resource's metadata document, in the path-insertion form of
    ///   RFC 9728 §3.1: the well-known segment goes between the authority and the
    ///   resource's own path rather than replacing it, so a resource of
    ///   "https://host/mcp" publishes at
    ///   "https://host/.well-known/oauth-protected-resource/mcp".
    /// </summary>
    /// <remarks>
    ///   The path matters: it is what lets two MCP servers on one origin, mounted at
    ///   different paths, each have their own document instead of overwriting a shared
    ///   one. A client that builds this URL itself, rather than reading it from the
    ///   challenge, arrives at the same place.
    /// </remarks>
    property ResourceMetadata: string read GetResourceMetadata;

    /// <summary>
    ///   Path component of the resource URL, without a trailing slash, and empty when
    ///   the resource is just an origin. This is the suffix RFC 9728 §3.1 appends to
    ///   the well-known segment.
    /// </summary>
    property ResourcePath: string read GetResourcePath;
    property MetadataProxyUpstream: string read FMetadataProxyUpstream;
    property MetadataProxyUrl: string read GetMetadataProxyUrl;
    property MetadataProxyEnabled: Boolean read GetMetadataProxyEnabled;

    /// <summary>
    ///   Issuers whose tokens are accepted, matched against the "iss" claim.
    /// </summary>
    /// <remarks>
    ///   This is deliberately not AuthorizationServers. With the metadata proxy enabled,
    ///   AuthorizationServers advertises the local proxy URL - that is what clients must
    ///   discover - while tokens keep being issued by, and carry the "iss" of, the
    ///   upstream authorization server. Anything added with AddTrustedIssuer is appended
    ///   to that, for the authorization servers whose "iss" is not the URL their metadata
    ///   was discovered from.
    /// </remarks>
    property TrustedIssuers: TArray<string> read GetTrustedIssuers;

    /// <summary>Class used to validate bearer tokens, nil when none was registered.</summary>
    property TokenValidatorClass: TClass read FTokenValidatorClass;

    /// <summary>
    ///   Value the "aud" claim must contain: what SetAudience was given, or the
    ///   resource URL. The fallback lives here, and not in every validator, so that
    ///   the rule cannot be reimplemented differently by each of them.
    /// </summary>
    property Audience: string read GetAudience;

    /// <summary>Scopes a token must all carry, else "insufficient_scope".</summary>
    property RequiredScopes: TArray<string> read FRequiredScopes;

    /// <summary>Tolerance applied to the "exp" and "nbf" claims, in seconds.</summary>
    property ClockSkewSeconds: Integer read FClockSkewSeconds;

    /// <summary>
    ///   Shared, thread-safe source of the authorization server metadata and public
    ///   keys, owned by this configuration.
    /// </summary>
    property MetadataProvider: IOAuthMetadataProvider read GetMetadataProvider;

    constructor Create(AApp: IJRPCApplication); override;
  end;


  [Implements(IAuthTokenConfig)]
  TAuthTokenConfig = class(TJRPCConfiguration, IAuthTokenConfig)
  private
    FToken: string;
    FLocation: TAuthTokenLocation;
    FCustomHeader: string;
  public
    function SetToken(const AToken: string): IAuthTokenConfig;
    function SetTokenLocation(ALocation: TAuthTokenLocation): IAuthTokenConfig;
    function SetTokenCustomHeader(const ACustomHeader: string): IAuthTokenConfig;

    property Token: string read FToken write FToken;
    property Location: TAuthTokenLocation read FLocation write FLocation;
    property CustomHeader: string read FCustomHeader write FCustomHeader;

  end;

implementation

uses
  Logify,
  MCPConnect.JRPC.Core,
  // Only for the ITokenValidator type check in SetTokenValidatorClass. It lives in
  // the implementation on purpose: MCPConnect.Security.Token uses this unit in its
  // interface, and the dependency between the two must stay one-way up there.
  MCPConnect.Security.Token;

{ TAuthTokenConfig }

function TAuthTokenConfig.SetToken(
  const AToken: string): IAuthTokenConfig;
begin
  FToken := AToken;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenCustomHeader(
  const ACustomHeader: string): IAuthTokenConfig;
begin
  FCustomHeader := ACustomHeader;
  Result := Self;
end;

function TAuthTokenConfig.SetTokenLocation(
  ALocation: TAuthTokenLocation): IAuthTokenConfig;
begin
  FLocation := ALocation;
  Result := Self;
end;

{ TOAuthConfig }

function TOAuthConfig.AddAuthorizationServer(
  const AAuthorizationServer: string): IOAuthConfig;
begin
  FAuthorizationServers := FAuthorizationServers + [AAuthorizationServer];
  Result := Self;
end;

function TOAuthConfig.AddScopesSupported(
  const AScopesSupported: string): IOAuthConfig;
begin
  FScopesSupported := FScopesSupported + [AScopesSupported];
  Result := Self;
end;

constructor TOAuthConfig.Create(AApp: IJRPCApplication);
begin
  inherited;
  FRealm := DefaultRealm;
  FAuthorizationServers := [];
  FScopesSupported := [];
  FRequiredScopes := [];
  FExtraTrustedIssuers := [];
  FTokenValidatorClass := nil;
  FClockSkewSeconds := DefaultClockSkew;
  FKeyCacheTTL := OAUTH_KEYS_TTL_DEFAULT;

  // Built here, and not on first use (simple and thread-safe)
  FMetadataProvider := TOAuthMetadataProvider.Create;
end;

function TOAuthConfig.ApplyConfig: IJRPCApplication;

  // Plain HTTP is what a developer runs on localhost, so only say something when the
  // host is not one - a warning that fires on every dev machine is a warning nobody
  // reads by the time it matters.
  procedure WarnIfInsecure(const AUrl: string);
  begin
    if AUrl = '' then
      Exit;

    if AUrl.StartsWith('http://', True) and not IsLoopbackUrl(AUrl) then
      Logger.LogWarning(SOAuthInsecureUrlWarningFmt, [AUrl]);
  end;

var
  LUrl: string;
begin
  Result := inherited ApplyConfig;

  if Length(AuthorizationServers) = 0 then
    Exit;

  // Fail here rather than on the first request. Without a resource there is no
  // ResourceMetadata to put in a challenge - it raises, and the generic handler turns
  // that into a 500 - and no Audience either, so every token would be rejected. Both
  // are certain, so there is nothing to be gained by starting.
  if Resource = '' then
    raise EJRPCException.Create(SOAuthResourceRequired);

  if not Assigned(TokenValidatorClass) then
    Logger.LogWarning(SOAuthNoValidatorWarning);

  WarnIfInsecure(Resource);
  WarnIfInsecure(MetadataProxyUpstream);
  for LUrl in AuthorizationServers do
    WarnIfInsecure(LUrl);

  // Once, at startup, rather than left to be discovered halfway through an authorization
  // flow: the failure it causes surfaces in the client, after a redirect, as an issuer
  // mismatch with nothing in this server's log to connect it to.
  if MetadataProxyEnabled then
    Logger.LogWarning(SOAuthMetadataProxyWarning);
end;

function TOAuthConfig.GetResourceMetadata: string;
begin
  if FResource = '' then
    raise Exception.Create(SOAuthResourceNotSpecified);

  var LURI := TURI.Create(FResource);

  // RFC 9728 §3.1 inserts the well-known segment between the authority and the
  // resource's path; it does not replace the path. Dropping it would publish every
  // server on an origin at the same URL, so the last one deployed would answer for
  // all of them - with its own "resource" value, which a client is required to check.
  LURI.Path := ProtectedResourcePath + ResourcePath;

  Result := LURI.ToString;
end;

function TOAuthConfig.GetResourcePath: string;
begin
  if FResource = '' then
    Exit('');

  // TURI reports a resource with no path as '' or '/', and both mean "no suffix".
  // Trimming the trailing slash covers them and the "https://host/mcp/" spelling
  // alike, so the same resource written either way publishes at one URL.
  Result := TURI.Create(FResource).Path.TrimRight(['/']);
end;

function TOAuthConfig.GetMetadataProxyUrl: string;
begin
  if FResource = '' then
    raise Exception.Create(SOAuthResourceNotSpecified);

  var LURI := TURI.Create(FResource);
  LURI.Path := MetadataProxyPath;

  Result := LURI.ToString;
end;

function TOAuthConfig.GetMetadataProxyEnabled: Boolean;
begin
  Result := FMetadataProxyUpstream <> '';
end;

function TOAuthConfig.EnableMetadataProxy(
  const AUpstreamIssuer: string): IOAuthConfig;
begin
  FMetadataProxyUpstream := AUpstreamIssuer;
  AddAuthorizationServer(GetMetadataProxyUrl);
  Result := Self;
end;

function TOAuthConfig.SetRealm(const ARealm: string): IOAuthConfig;
begin
  FRealm := ARealm;
  Result := Self;
end;

function TOAuthConfig.SetResource(const AUrl: string): IOAuthConfig;
begin
  FResource := AUrl;
  Result := Self;
end;

function TOAuthConfig.SetTokenValidatorClass(AClass: TClass): IOAuthConfig;
begin
  // The class reference is untyped, so what the compiler used to guarantee is
  // checked here instead - at startup, rather than on the first request.
  if Assigned(AClass) and (AClass.GetInterfaceEntry(ITokenValidator) = nil) then
    raise EJRPCException.CreateFmt(SOAuthValidatorClassInvalidFmt, [AClass.ClassName]);

  FTokenValidatorClass := AClass;

  if Assigned(AClass) and AClass.InheritsFrom(TDecodeOnlyTokenValidator) then
    Logger.LogWarning(SOAuthDecodeOnlyValidatorWarning);

  Result := Self;
end;

function TOAuthConfig.SetMetadataProvider(const AProvider: IOAuthMetadataProvider): IOAuthConfig;
begin
  // Ignored when nil: a validator must always find a provider in the configuration.
  if Assigned(AProvider) then
    FMetadataProvider := AProvider;

  Result := Self;
end;

function TOAuthConfig.SetAudience(const AAudience: string): IOAuthConfig;
begin
  FAudience := AAudience;
  Result := Self;
end;

function TOAuthConfig.AddRequiredScope(const AScope: string): IOAuthConfig;
begin
  FRequiredScopes := FRequiredScopes + [AScope];
  Result := Self;
end;

function TOAuthConfig.SetClockSkew(ASeconds: Integer): IOAuthConfig;
begin
  FClockSkewSeconds := ASeconds;
  Result := Self;
end;

function TOAuthConfig.SetKeyCacheTTL(ASeconds: Integer): IOAuthConfig;
begin
  FKeyCacheTTL := ASeconds;

  var LSettings: IOAuthCacheableMetadataProvider;
  if Supports(FMetadataProvider, IOAuthCacheableMetadataProvider, LSettings) then
    LSettings.KeysTTL := ASeconds;

  Result := Self;
end;

function TOAuthConfig.AddTrustedIssuer(const AIssuer: string): IOAuthConfig;
begin
  if AIssuer.Trim <> '' then
    FExtraTrustedIssuers := FExtraTrustedIssuers + [AIssuer.Trim];

  Result := Self;
end;

class function TOAuthConfig.SameIssuer(const A, B: string): Boolean;
begin
  Result := SameUri(A, B);
end;

class function TOAuthConfig.SameUri(const A, B: string): Boolean;

  // An absolute URL is "scheme://authority" followed by the rest. Split by hand: TURI
  // would reassemble a default port into the authority, which changes what is being
  // compared.
  procedure Split(const AValue: string; out AAuthority, ARest: string);
  var
    LSchemeEnd, LRestStart: Integer;
  begin
    LSchemeEnd := AValue.IndexOf('://');
    if LSchemeEnd >= 0 then
      LRestStart := AValue.IndexOf('/', LSchemeEnd + 3)
    else
      LRestStart := AValue.IndexOf('/');

    if LRestStart >= 0 then
    begin
      AAuthority := AValue.Substring(0, LRestStart);
      ARest := AValue.Substring(LRestStart);
    end
    else
    begin
      AAuthority := AValue;
      ARest := '';
    end;
  end;

var
  LAuthorityA, LRestA, LAuthorityB, LRestB: string;
begin
  Split(A.Trim.TrimRight(['/']), LAuthorityA, LRestA);
  Split(B.Trim.TrimRight(['/']), LAuthorityB, LRestB);

  Result := SameText(LAuthorityA, LAuthorityB) and (LRestA = LRestB);
end;

class function TOAuthConfig.IsLoopbackUrl(const AUrl: string): Boolean;
var
  LHost: string;
begin
  try
    LHost := TURI.Create(AUrl).Host;
  except
    // A URL this server cannot even parse is not one to stay quiet about.
    on Exception do
      Exit(False);
  end;

  Result := SameText(LHost, 'localhost') or (LHost = '127.0.0.1') or (LHost = '::1');
end;

function TOAuthConfig.GetDiscoveryIssuers: TArray<string>;
begin
  // With the metadata proxy enabled, AuthorizationServers holds this server's own
  // proxy URL, while tokens are issued by - and carry the "iss" of - the upstream
  // authorization server.
  if MetadataProxyEnabled then
    Result := [MetadataProxyUpstream]
  else
    Result := AuthorizationServers;
end;

function TOAuthConfig.GetTrustedIssuers: TArray<string>;

  function AlreadyListed(const AValues: TArray<string>; const AValue: string): Boolean;
  var
    LValue: string;
  begin
    for LValue in AValues do
      if SameIssuer(LValue, AValue) then
        Exit(True);

    Result := False;
  end;

var
  LIssuer: string;
begin
  Result := GetDiscoveryIssuers;

  // Explicitly declared issuers widen that list rather than replacing it: an
  // authorization server can mint an "iss" that is not the URL its metadata was
  // discovered from, and both remain valid.
  for LIssuer in FExtraTrustedIssuers do
    if not AlreadyListed(Result, LIssuer) then
      Result := Result + [LIssuer];
end;

function TOAuthConfig.KeySourceFor(const AIssuer: string): string;
var
  LDiscovery: TArray<string>;
  LCandidate: string;
begin
  LDiscovery := GetDiscoveryIssuers;

  for LCandidate in LDiscovery do
    if SameIssuer(LCandidate, AIssuer) then
      Exit(LCandidate);

  // An issuer that is not one of the configured authorization servers - one declared
  // through AddTrustedIssuer - is verified with the keys of the authorization server
  // that was configured, never with keys fetched from the issuer itself.
  if Length(LDiscovery) > 0 then
    Result := LDiscovery[0]
  else
    Result := '';
end;

function TOAuthConfig.GetAudience: string;
begin
  // The resource URL is what RFC 8707 resource indicators put in "aud", so it is the
  // right default and SetAudience is only needed when an authorization server mints
  // something else.
  if FAudience <> '' then
    Result := FAudience
  else
    Result := Resource;
end;

function TOAuthConfig.GetMetadataProvider: IOAuthMetadataProvider;
begin
  Result := FMetadataProvider;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TAuthTokenConfig);
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TOAuthConfig);

end.
