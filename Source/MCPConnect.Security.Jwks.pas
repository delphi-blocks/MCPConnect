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
unit MCPConnect.Security.Jwks;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.SyncObjs,
  System.Generics.Collections,

  Neon.Core.Attributes;

{$SCOPEDENUMS ON}

const
  /// <summary>Default lifetime of a cached authorization server metadata document (24 hours).</summary>
  OAUTH_METADATA_TTL_DEFAULT = 24 * 60 * 60;

  /// <summary>Default lifetime of a cached JWKS document (1 hour).</summary>
  OAUTH_KEYS_TTL_DEFAULT = 60 * 60;

  /// <summary>
  ///   Minimum interval between two out-of-band JWKS refreshes for the same issuer,
  ///   i.e. refreshes triggered by an unknown key id rather than by TTL expiry.
  /// </summary>
  OAUTH_KEYS_REFRESH_INTERVAL_DEFAULT = 5 * 60;

  /// <summary>Connection and response timeout for metadata/JWKS requests, in milliseconds.</summary>
  OAUTH_METADATA_TIMEOUT_DEFAULT = 10000;

  /// <summary>Well-known segment of the RFC 8414 authorization server metadata document.</summary>
  OAUTH_AS_METADATA_PATH = '/.well-known/oauth-authorization-server';

  /// <summary>Well-known segment of the OpenID Connect Discovery document.</summary>
  OAUTH_DISCOVERY_PATH = '/.well-known/openid-configuration';

resourcestring
  SOAuthIssuerRequired = 'An issuer URL is required to retrieve the authorization server metadata';
  SOAuthMetadataFetchFailedFmt = 'Cannot retrieve the authorization server metadata from "%s": %s';
  SOAuthMetadataInvalidFmt = 'The document retrieved from "%s" is not a valid metadata document';
  SOAuthIssuerMismatchFmt = 'The document retrieved from "%s" declares the issuer "%s": ' +
    'metadata is only usable when published by the issuer it was requested for';
  SOAuthJwksUriMissingFmt = 'The metadata document of "%s" does not declare a "jwks_uri"';
  SOAuthKeysFetchFailedFmt = 'Cannot retrieve the JSON Web Key Set from "%s": %s';
  SOAuthMetadataNotFoundFmt = 'No usable authorization server metadata for "%s". Tried: %s';

type
  /// <summary>
  ///   Exception raised when the authorization server metadata or its key set
  ///   cannot be retrieved or parsed.
  /// </summary>
  EOAuthMetadataException = class(Exception);

  /// <summary>
  ///   A single public key of a JSON Web Key Set (RFC 7517), as published by the
  ///   authorization server. Only the members needed to verify a JWS signature are
  ///   mapped; the whole entry is kept in <c>Raw</c> for anything else.
  /// </summary>
  /// <remarks>
  ///   This is a value type on purpose: keys are copied out of the provider cache,
  ///   so a caller can never observe an entry that another thread is refreshing,
  ///   and there is nothing to free.
  ///   Member names are spelled out with [NeonProperty] rather than derived from a
  ///   naming convention: they are fixed by RFC 7517, not by our own style.
  /// </remarks>
  TOAuthJsonWebKey = record
    /// <summary>Key id ("kid"), matched against the "kid" of the token header.</summary>
    [NeonProperty('kid')]
    Kid: string;
    /// <summary>Key type ("kty"), e.g. 'RSA' or 'EC'.</summary>
    [NeonProperty('kty')]
    Kty: string;
    /// <summary>Intended algorithm ("alg"), e.g. 'RS256'. May be empty.</summary>
    [NeonProperty('alg')]
    Alg: string;
    /// <summary>Intended use ("use"), e.g. 'sig'. May be empty.</summary>
    [NeonProperty('use')]
    Use: string;
    /// <summary>RSA modulus ("n"), base64url encoded.</summary>
    [NeonProperty('n')]
    N: string;
    /// <summary>RSA public exponent ("e"), base64url encoded.</summary>
    [NeonProperty('e')]
    E: string;
    /// <summary>EC curve name ("crv").</summary>
    [NeonProperty('crv')]
    Crv: string;
    /// <summary>EC public key x coordinate ("x"), base64url encoded.</summary>
    [NeonProperty('x')]
    X: string;
    /// <summary>EC public key y coordinate ("y"), base64url encoded.</summary>
    [NeonProperty('y')]
    Y: string;
    /// <summary>X.509 certificate chain ("x5c"), base64 (not base64url) encoded.</summary>
    [NeonProperty('x5c')]
    X5c: TArray<string>;
    /// <summary>The whole key entry as published, for members not mapped above.</summary>
    [NeonIgnore]
    Raw: string;

    /// <summary>True when no key material has been read into this record.</summary>
    function IsEmpty: Boolean;

    /// <summary>True when the key can be used to verify a signature ("use" is 'sig' or absent).</summary>
    function IsSignatureKey: Boolean;

    /// <summary>
    ///   True when the key publishes the components its type is defined by: "n"/"e"
    ///   for RSA, "x"/"y" for EC. RFC 7518 makes those members required, and an
    ///   optional "x5c" chain carries the same public key, so this is what a verifier
    ///   should read first.
    /// </summary>
    function HasKeyComponents: Boolean;

    class function FromJSON(AJSON: TJSONObject): TOAuthJsonWebKey; static;
  end;

  /// <summary>
  ///   The subset of an authorization server metadata document (RFC 8414 /
  ///   OpenID Connect Discovery) MCPConnect needs. See the remark on
  ///   <see cref="TJsonWebKey" /> about this being a value type.
  /// </summary>
  TOAuthServerMetadata = record
    [NeonProperty('issuer')]
    Issuer: string;
    [NeonProperty('jwks_uri')]
    JwksUri: string;
    [NeonProperty('authorization_endpoint')]
    AuthorizationEndpoint: string;
    [NeonProperty('token_endpoint')]
    TokenEndpoint: string;
    /// <summary>The whole document as published, for members not mapped above.</summary>
    [NeonIgnore]
    Raw: string;

    function IsEmpty: Boolean;

    class function FromJSON(AJSON: TJSONObject): TOAuthServerMetadata; static;
  end;

  /// <summary>
  ///   Supplies the authorization server metadata and the public keys used to verify
  ///   access token signatures, caching both so that a token validator does not hit
  ///   the identity provider on every request.
  /// </summary>
  /// <remarks>
  ///   A single instance is shared by every request of a server, so implementations
  ///   must be thread-safe. Token validators, by contrast, are created per request
  ///   and receive this provider: all cross-request state belongs here.
  /// </remarks>
  IOAuthMetadataProvider = interface
  ['{6C7D3A61-9D5B-4F3E-9B15-6C2F8A9D4E77}']
    /// <summary>
    ///   Returns the metadata document of the given issuer, from the cache when fresh.
    /// </summary>
    /// <exception cref="EOAuthMetadataException">
    ///   The document cannot be retrieved and no previously cached copy is available.
    /// </exception>
    function GetServerMetadata(const AIssuer: string): TOAuthServerMetadata;

    /// <summary>
    ///   Returns every published key of the given issuer, from the cache when fresh.
    /// </summary>
    /// <exception cref="EOAuthMetadataException">
    ///   The key set cannot be retrieved and no previously cached copy is available.
    /// </exception>
    function GetKeys(const AIssuer: string): TArray<TOAuthJsonWebKey>;

    /// <summary>
    ///   Looks up a single key by its id. An unknown key id triggers one rate-limited
    ///   refresh of the key set, so that a key rotation is picked up before the cached
    ///   set expires. Pass an empty key id to accept the only published key, if any.
    /// </summary>
    /// <returns>
    ///   False when the key is unknown, or when the key set cannot be retrieved at all:
    ///   this method never raises, so that a caller cannot accidentally turn an
    ///   unreachable identity provider into an accepted token.
    /// </returns>
    function TryGetKey(const AIssuer, AKeyId: string; out AKey: TOAuthJsonWebKey): Boolean;

    /// <summary>
    ///   Drops the cached metadata and keys of the given issuer, or of every issuer
    ///   when called with an empty string.
    /// </summary>
    procedure Invalidate(const AIssuer: string);
  end;

  /// <summary>
  ///   Optional extension of <see cref="IOAuthMetadataProvider" /> that exposes cache
  ///   and timeout settings. The default provider implements it; a custom provider may
  ///   implement it too so that <c>SetKeyCacheTTL</c> and similar configuration calls
  ///   reach it without a class cast.
  /// </summary>
  IOAuthCacheableMetadataProvider = interface(IOAuthMetadataProvider)
  ['{A1E9B4C2-7F3D-4A6E-8C15-3D9F2B7E5A41}']
    function GetMetadataTTL: Integer;
    procedure SetMetadataTTL(AValue: Integer);
    function GetKeysTTL: Integer;
    procedure SetKeysTTL(AValue: Integer);
    function GetKeysRefreshInterval: Integer;
    procedure SetKeysRefreshInterval(AValue: Integer);
    function GetRequestTimeout: Integer;
    procedure SetRequestTimeout(AValue: Integer);

    property MetadataTTL: Integer read GetMetadataTTL write SetMetadataTTL;
    property KeysTTL: Integer read GetKeysTTL write SetKeysTTL;
    property KeysRefreshInterval: Integer read GetKeysRefreshInterval write SetKeysRefreshInterval;
    property RequestTimeout: Integer read GetRequestTimeout write SetRequestTimeout;
  end;

  /// <summary>
  ///   Default <see cref="IOAuthMetadataProvider" />: fetches the documents over HTTP
  ///   and keeps them in a thread-safe cache with separate time-to-live values for the
  ///   metadata document and for the key set.
  /// </summary>
  /// <remarks>
  ///   Documents are fetched outside the lock, so a slow identity provider never blocks
  ///   requests that hit the cache. Two threads racing on the same expired entry may
  ///   both fetch it; the cost is one redundant request, not a stall.
  ///   When a refresh fails but a previously fetched copy is held, that copy keeps being
  ///   served (stale-if-error) and a warning is logged: an unreachable identity provider
  ///   must not silently invalidate every session.
  ///   A metadata document is only accepted when the "issuer" it declares is the one it
  ///   was requested for (RFC 8414 §3.3), and redirects are refused rather than followed,
  ///   so the URL a document is retrieved from is always the URL it was asked for. Those
  ///   two together are what make the "jwks_uri" it names safe to fetch keys from.
  /// </remarks>
  TOAuthMetadataProvider = class(TInterfacedObject, IOAuthMetadataProvider, IOAuthCacheableMetadataProvider)
  private type
    TMetadataEntry = class
      Metadata: TOAuthServerMetadata;
      /// <summary>The candidate URL this document actually came from.</summary>
      DiscoveryUrl: string;
      FetchedAt: TDateTime;
    end;

    TKeysEntry = class
      Keys: TArray<TOAuthJsonWebKey>;
      FetchedAt: TDateTime;
      LastAttemptAt: TDateTime;
    end;
  private
    FLock: TCriticalSection;
    FMetadataCache: TObjectDictionary<string, TMetadataEntry>;
    FKeysCache: TObjectDictionary<string, TKeysEntry>;
    FMetadataTTL: Integer;
    FKeysTTL: Integer;
    FKeysRefreshInterval: Integer;
    FRequestTimeout: Integer;

    class function NormalizeIssuer(const AIssuer: string): string; static;
    class function IsExpired(const AFetchedAt: TDateTime; ATTLSeconds: Integer): Boolean; static;

    function FetchMetadata(const AIssuer: string; out ADiscoveryUrl: string): TOAuthServerMetadata;
    function TryFetchMetadataFrom(const AIssuer, AUrl: string; out AMetadata: TOAuthServerMetadata; out AError: string): Boolean;
    function KnownDiscoveryUrl(const AKey: string): string;
    function FetchKeys(const AIssuer: string): TArray<TOAuthJsonWebKey>;
    function EnsureKeys(const AIssuer: string; AForceRefresh: Boolean): TArray<TOAuthJsonWebKey>;
    procedure StoreMetadata(const AKey: string; const AMetadata: TOAuthServerMetadata; const ADiscoveryUrl: string);
    procedure StoreKeys(const AKey: string; const AKeys: TArray<TOAuthJsonWebKey>);
    function TryGetCachedMetadata(const AKey: string; out AMetadata: TOAuthServerMetadata): Boolean;
  protected
    /// <summary>
    ///   Performs the actual HTTP GET. Overridable so that tests can serve documents
    ///   without a network, and so that a host can plug in its own HTTP stack.
    /// </summary>
    /// <remarks>
    ///   An override must return the document found at AUrl itself and must not follow
    ///   redirects: the caller's guarantee is that a document was retrieved from the URL
    ///   it was asked for, and a redirect quietly breaks it - see the remarks on the class.
    /// </remarks>
    function FetchDocument(const AUrl: string): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { IOAuthMetadataProvider }
    function GetServerMetadata(const AIssuer: string): TOAuthServerMetadata;
    function GetKeys(const AIssuer: string): TArray<TOAuthJsonWebKey>;
    function TryGetKey(const AIssuer, AKeyId: string; out AKey: TOAuthJsonWebKey): Boolean;
    procedure Invalidate(const AIssuer: string);

    { IOAuthCacheableMetadataProvider }
    function GetMetadataTTL: Integer;
    procedure SetMetadataTTL(AValue: Integer);
    function GetKeysTTL: Integer;
    procedure SetKeysTTL(AValue: Integer);
    function GetKeysRefreshInterval: Integer;
    procedure SetKeysRefreshInterval(AValue: Integer);
    function GetRequestTimeout: Integer;
    procedure SetRequestTimeout(AValue: Integer);

    /// <summary>Lifetime of a cached metadata document, in seconds.</summary>
    property MetadataTTL: Integer read GetMetadataTTL write SetMetadataTTL;
    /// <summary>Lifetime of a cached key set, in seconds.</summary>
    property KeysTTL: Integer read GetKeysTTL write SetKeysTTL;
    /// <summary>Minimum interval between two unknown-key-id refreshes, in seconds.</summary>
    property KeysRefreshInterval: Integer read GetKeysRefreshInterval write SetKeysRefreshInterval;
    /// <summary>Connection and response timeout of the HTTP requests, in milliseconds.</summary>
    property RequestTimeout: Integer read GetRequestTimeout write SetRequestTimeout;

    /// <summary>
    ///   The URLs an issuer's metadata document may be published at, in the order the
    ///   MCP authorization specification requires them to be tried: RFC 8414 with the
    ///   well-known segment inserted before the issuer's path, then OpenID Connect
    ///   Discovery inserted the same way, then OpenID Connect Discovery appended after
    ///   the path. An issuer with no path collapses the last two into one, so it yields
    ///   two candidates rather than three.
    /// </summary>
    /// <remarks>
    ///   Trying only the OpenID Connect form would leave an authorization server that is
    ///   OAuth 2.1 but not OpenID Connect undiscoverable - no metadata, so no "jwks_uri",
    ///   so every token rejected.
    /// </remarks>
    class function DiscoveryUrlsFor(const AIssuer: string): TArray<string>; static;
  end;

implementation

uses
  System.DateUtils, System.Net.HttpClient,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Logify;

const
  HTTP_STATUS_OK = 200;

/// <summary>
///   Both documents map their members through explicit [NeonProperty] names, so no
///   member case transformation must be applied on top of them.
/// </summary>
function JwksNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default;
end;

{ TOAuthJsonWebKey }

function TOAuthJsonWebKey.IsEmpty: Boolean;
begin
  Result := (Kty = '') and (N = '') and (X = '') and (Length(X5c) = 0);
end;

function TOAuthJsonWebKey.IsSignatureKey: Boolean;
begin
  Result := (Use = '') or SameText(Use, 'sig');
end;

function TOAuthJsonWebKey.HasKeyComponents: Boolean;
begin
  Result := ((N <> '') and (E <> '')) or ((X <> '') and (Y <> ''));
end;

class function TOAuthJsonWebKey.FromJSON(AJSON: TJSONObject): TOAuthJsonWebKey;
begin
  Result := Default(TOAuthJsonWebKey);
  if not Assigned(AJSON) then
    Exit;

  Result := TNeon.JSONToValue<TOAuthJsonWebKey>(AJSON, JwksNeonConfig);
  // Kept out of the mapping ([NeonIgnore]) because it is the source document
  // itself, not one of its members.
  Result.Raw := AJSON.ToJSON;
end;

{ TOAuthServerMetadata }

function TOAuthServerMetadata.IsEmpty: Boolean;
begin
  Result := (Issuer = '') and (JwksUri = '') and (Raw = '');
end;

class function TOAuthServerMetadata.FromJSON(AJSON: TJSONObject): TOAuthServerMetadata;
begin
  Result := Default(TOAuthServerMetadata);
  if not Assigned(AJSON) then
    Exit;

  Result := TNeon.JSONToValue<TOAuthServerMetadata>(AJSON, JwksNeonConfig);
  Result.Raw := AJSON.ToJSON;
end;

{ TOAuthMetadataProvider }

constructor TOAuthMetadataProvider.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FMetadataCache := TObjectDictionary<string, TMetadataEntry>.Create([doOwnsValues]);
  FKeysCache := TObjectDictionary<string, TKeysEntry>.Create([doOwnsValues]);
  FMetadataTTL := OAUTH_METADATA_TTL_DEFAULT;
  FKeysTTL := OAUTH_KEYS_TTL_DEFAULT;
  FKeysRefreshInterval := OAUTH_KEYS_REFRESH_INTERVAL_DEFAULT;
  FRequestTimeout := OAUTH_METADATA_TIMEOUT_DEFAULT;
end;

destructor TOAuthMetadataProvider.Destroy;
begin
  FKeysCache.Free;
  FMetadataCache.Free;
  FLock.Free;
  inherited;
end;

function TOAuthMetadataProvider.GetMetadataTTL: Integer;
begin
  Result := FMetadataTTL;
end;

procedure TOAuthMetadataProvider.SetMetadataTTL(AValue: Integer);
begin
  FMetadataTTL := AValue;
end;

function TOAuthMetadataProvider.GetKeysTTL: Integer;
begin
  Result := FKeysTTL;
end;

procedure TOAuthMetadataProvider.SetKeysTTL(AValue: Integer);
begin
  FKeysTTL := AValue;
end;

function TOAuthMetadataProvider.GetKeysRefreshInterval: Integer;
begin
  Result := FKeysRefreshInterval;
end;

procedure TOAuthMetadataProvider.SetKeysRefreshInterval(AValue: Integer);
begin
  FKeysRefreshInterval := AValue;
end;

function TOAuthMetadataProvider.GetRequestTimeout: Integer;
begin
  Result := FRequestTimeout;
end;

procedure TOAuthMetadataProvider.SetRequestTimeout(AValue: Integer);
begin
  FRequestTimeout := AValue;
end;

class function TOAuthMetadataProvider.NormalizeIssuer(const AIssuer: string): string;
begin
  Result := AIssuer.Trim.TrimRight(['/']).ToLower;
end;

class function TOAuthMetadataProvider.IsExpired(const AFetchedAt: TDateTime;
  ATTLSeconds: Integer): Boolean;
begin
  Result := SecondsBetween(Now, AFetchedAt) >= ATTLSeconds;
end;

function TOAuthMetadataProvider.FetchDocument(const AUrl: string): string;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LClient := THTTPClient.Create;
  try
    LClient.ConnectionTimeout := RequestTimeout;
    LClient.ResponseTimeout := RequestTimeout;

    // Refused rather than followed, and this is load bearing. Following a redirect
    // would collect the document from a URL this server was never configured with,
    // and the issuer check in FetchMetadata would still pass: whoever answers there
    // decides what the document says about itself, so it can echo the expected
    // "issuer" back while pointing "jwks_uri" anywhere it likes. Refusing to move is
    // what keeps "retrieved from" and "requested for" the same URL, which is the
    // whole basis of RFC 8414 §3.3. A 3xx now falls into the status check below.
    LClient.HandleRedirects := False;

    LResponse := LClient.Get(AUrl);
    if LResponse.StatusCode <> HTTP_STATUS_OK then
      raise EOAuthMetadataException.CreateFmt(SOAuthMetadataFetchFailedFmt,
        [AUrl, Format('HTTP %d', [LResponse.StatusCode])]);

    Result := LResponse.ContentAsString;
  finally
    LClient.Free;
  end;
end;

class function TOAuthMetadataProvider.DiscoveryUrlsFor(const AIssuer: string): TArray<string>;
var
  LIssuer, LOrigin, LPath: string;
  LSchemeEnd, LPathStart: Integer;
begin
  // Split by hand rather than through TURI: an issuer identifier is an absolute URL
  // that RFC 8414 §2 forbids from carrying a query or a fragment, so this is only ever
  // "origin" plus "path" - and reassembling it through TURI risks it normalising a
  // default port back in, which would change the URL being requested.
  LIssuer := AIssuer.Trim.TrimRight(['/']);

  LSchemeEnd := LIssuer.IndexOf('://');
  if LSchemeEnd >= 0 then
    LPathStart := LIssuer.IndexOf('/', LSchemeEnd + 3)
  else
    LPathStart := LIssuer.IndexOf('/');

  if LPathStart >= 0 then
  begin
    LOrigin := LIssuer.Substring(0, LPathStart);
    LPath := LIssuer.Substring(LPathStart);
  end
  else
  begin
    LOrigin := LIssuer;
    LPath := '';
  end;

  // With no path there is nothing to insert or append around, so the two OpenID
  // Connect forms are the same URL and only two candidates remain.
  if LPath = '' then
  begin
    Result := [
      LOrigin + OAUTH_AS_METADATA_PATH,
      LOrigin + OAUTH_DISCOVERY_PATH
    ];
    Exit;
  end;

  Result := [
    LOrigin + OAUTH_AS_METADATA_PATH + LPath,  // RFC 8414 §3.1, path insertion
    LOrigin + OAUTH_DISCOVERY_PATH + LPath,    // OIDC Discovery, path insertion
    LOrigin + LPath + OAUTH_DISCOVERY_PATH     // OIDC Discovery, path appending
  ];
end;

function TOAuthMetadataProvider.TryFetchMetadataFrom(const AIssuer, AUrl: string;
  out AMetadata: TOAuthServerMetadata; out AError: string): Boolean;
var
  LJSON: TJSONValue;
begin
  AMetadata := Default(TOAuthServerMetadata);
  AError := '';

  try
    LJSON := TJSONObject.ParseJSONValue(FetchDocument(AUrl));
  except
    on E: Exception do
    begin
      AError := E.Message;
      Exit(False);
    end;
  end;

  try
    if not (LJSON is TJSONObject) then
    begin
      AError := Format(SOAuthMetadataInvalidFmt, [AUrl]);
      Exit(False);
    end;

    AMetadata := TOAuthServerMetadata.FromJSON(LJSON as TJSONObject);
  finally
    LJSON.Free;
  end;

  // RFC 8414 §3.3 and OIDC Discovery §4.3: the "issuer" the document declares must be
  // the one it was requested for, or the document must not be used. This is what ties
  // the "jwks_uri" about to be trusted back to the configured authorization server -
  // without it, anything that can answer at a well-known URL chooses where this server
  // fetches its signing keys from. It is applied to every candidate, so widening the
  // search cannot widen what is accepted.
  // A document that declares no issuer at all fails here too: normalising an empty
  // string cannot match a non-empty issuer, and RFC 8414 makes the member required.
  if NormalizeIssuer(AMetadata.Issuer) <> NormalizeIssuer(AIssuer) then
  begin
    AError := Format(SOAuthIssuerMismatchFmt, [AUrl, AMetadata.Issuer]);
    AMetadata := Default(TOAuthServerMetadata);
    Exit(False);
  end;

  Result := True;
end;

function TOAuthMetadataProvider.FetchMetadata(const AIssuer: string;
  out ADiscoveryUrl: string): TOAuthServerMetadata;
var
  LCandidates: TArray<string>;
  LOrdered: TArray<string>;
  LKnown, LUrl, LError: string;
  LReport: TArray<string>;
begin
  Result := Default(TOAuthServerMetadata);
  ADiscoveryUrl := '';

  LCandidates := DiscoveryUrlsFor(AIssuer);

  // A URL that already produced a document for this issuer is tried first: a server's
  // well-known layout does not normally change, and rediscovering it on every refresh
  // would turn one request into up to three. It is reordered rather than used alone,
  // so a server that does move is picked up on the next refresh instead of being
  // permanently unreachable.
  LKnown := KnownDiscoveryUrl(NormalizeIssuer(AIssuer));
  if LKnown <> '' then
  begin
    LOrdered := [LKnown];
    for LUrl in LCandidates do
      if LUrl <> LKnown then
        LOrdered := LOrdered + [LUrl];
    LCandidates := LOrdered;
  end;

  LReport := [];
  for LUrl in LCandidates do
  begin
    if TryFetchMetadataFrom(AIssuer, LUrl, Result, LError) then
    begin
      ADiscoveryUrl := LUrl;
      Logger.LogDebug('OAuth metadata fetched from "%s"', [LUrl]);
      Exit;
    end;

    // Every candidate that failed is reported together at the end: with three shapes
    // in play, "not found" on its own leaves an operator guessing which of them this
    // server actually asked for.
    LReport := LReport + [Format('%s (%s)', [LUrl, LError])];
  end;

  raise EOAuthMetadataException.CreateFmt(SOAuthMetadataNotFoundFmt,
    [AIssuer, string.Join('; ', LReport)]);
end;

function TOAuthMetadataProvider.KnownDiscoveryUrl(const AKey: string): string;
var
  LEntry: TMetadataEntry;
begin
  Result := '';

  FLock.Enter;
  try
    if FMetadataCache.TryGetValue(AKey, LEntry) then
      Result := LEntry.DiscoveryUrl;
  finally
    FLock.Leave;
  end;
end;

function TOAuthMetadataProvider.FetchKeys(const AIssuer: string): TArray<TOAuthJsonWebKey>;
var
  LMetadata: TOAuthServerMetadata;
  LJSON: TJSONValue;
  LKeys: TJSONArray;
  LItem: TJSONValue;
begin
  Result := [];

  LMetadata := GetServerMetadata(AIssuer);
  if LMetadata.JwksUri = '' then
    raise EOAuthMetadataException.CreateFmt(SOAuthJwksUriMissingFmt, [AIssuer]);

  LJSON := TJSONObject.ParseJSONValue(FetchDocument(LMetadata.JwksUri));
  if not (LJSON is TJSONObject) then
  begin
    LJSON.Free;
    raise EOAuthMetadataException.CreateFmt(SOAuthMetadataInvalidFmt, [LMetadata.JwksUri]);
  end;

  try
    if not (LJSON as TJSONObject).TryGetValue<TJSONArray>('keys', LKeys) then
      raise EOAuthMetadataException.CreateFmt(SOAuthMetadataInvalidFmt, [LMetadata.JwksUri]);

    for LItem in LKeys do
      if LItem is TJSONObject then
        Result := Result + [TOAuthJsonWebKey.FromJSON(LItem as TJSONObject)];
  finally
    LJSON.Free;
  end;

  Logger.LogDebug('OAuth key set fetched from "%s" (%d keys)', [LMetadata.JwksUri, Length(Result)]);
end;

procedure TOAuthMetadataProvider.StoreMetadata(const AKey: string;
  const AMetadata: TOAuthServerMetadata; const ADiscoveryUrl: string);
var
  LEntry: TMetadataEntry;
begin
  FLock.Enter;
  try
    if not FMetadataCache.TryGetValue(AKey, LEntry) then
    begin
      LEntry := TMetadataEntry.Create;
      FMetadataCache.Add(AKey, LEntry);
    end;
    LEntry.Metadata := AMetadata;
    LEntry.DiscoveryUrl := ADiscoveryUrl;
    LEntry.FetchedAt := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TOAuthMetadataProvider.StoreKeys(const AKey: string; const AKeys: TArray<TOAuthJsonWebKey>);
var
  LEntry: TKeysEntry;
begin
  FLock.Enter;
  try
    if not FKeysCache.TryGetValue(AKey, LEntry) then
    begin
      LEntry := TKeysEntry.Create;
      FKeysCache.Add(AKey, LEntry);
    end;
    LEntry.Keys := AKeys;
    LEntry.FetchedAt := Now;
    LEntry.LastAttemptAt := Now;
  finally
    FLock.Leave;
  end;
end;

function TOAuthMetadataProvider.TryGetCachedMetadata(const AKey: string;
  out AMetadata: TOAuthServerMetadata): Boolean;
var
  LEntry: TMetadataEntry;
begin
  FLock.Enter;
  try
    Result := FMetadataCache.TryGetValue(AKey, LEntry);
    if Result then
      AMetadata := LEntry.Metadata;
  finally
    FLock.Leave;
  end;
end;

function TOAuthMetadataProvider.GetServerMetadata(const AIssuer: string): TOAuthServerMetadata;
var
  LKey: string;
  LEntry: TMetadataEntry;
  LMetadata: TOAuthServerMetadata;
  LDiscoveryUrl: string;
begin
  if AIssuer.Trim = '' then
    raise EOAuthMetadataException.Create(SOAuthIssuerRequired);

  LKey := NormalizeIssuer(AIssuer);

  FLock.Enter;
  try
    if FMetadataCache.TryGetValue(LKey, LEntry) and not IsExpired(LEntry.FetchedAt, MetadataTTL) then
      Exit(LEntry.Metadata);
  finally
    FLock.Leave;
  end;

  // Fetched outside the lock: a slow identity provider must not block the requests
  // that are served from the cache.
  try
    LMetadata := FetchMetadata(AIssuer, LDiscoveryUrl);
  except
    on E: Exception do
    begin
      // stale-if-error: an unreachable identity provider keeps the last known
      // document alive rather than rejecting every request.
      if TryGetCachedMetadata(LKey, Result) then
      begin
        Logger.LogWarning('OAuth metadata refresh failed for "%s", serving the cached document: %s',
          [AIssuer, E.Message]);
        Exit;
      end;

      if E is EOAuthMetadataException then
        raise;
      raise EOAuthMetadataException.CreateFmt(SOAuthMetadataFetchFailedFmt, [AIssuer, E.Message]);
    end;
  end;

  StoreMetadata(LKey, LMetadata, LDiscoveryUrl);
  Result := LMetadata;
end;

function TOAuthMetadataProvider.EnsureKeys(const AIssuer: string;
  AForceRefresh: Boolean): TArray<TOAuthJsonWebKey>;
var
  LKey: string;
  LEntry: TKeysEntry;
  LNeedFetch: Boolean;
  LKeys: TArray<TOAuthJsonWebKey>;
begin
  if AIssuer.Trim = '' then
    raise EOAuthMetadataException.Create(SOAuthIssuerRequired);

  Result := [];
  LKey := NormalizeIssuer(AIssuer);
  LNeedFetch := True;

  FLock.Enter;
  try
    if FKeysCache.TryGetValue(LKey, LEntry) then
    begin
      Result := LEntry.Keys;

      if AForceRefresh then
        // An unknown key id may mean the identity provider rotated its keys, but a
        // client sending random key ids must not be able to make this server issue
        // one request to the identity provider per incoming request.
        LNeedFetch := SecondsBetween(Now, LEntry.LastAttemptAt) >= KeysRefreshInterval
      else
        LNeedFetch := IsExpired(LEntry.FetchedAt, KeysTTL);

      // The attempt is booked while still holding the lock, so that concurrent
      // requests cannot all get past the rate limit at the same time.
      if LNeedFetch then
        LEntry.LastAttemptAt := Now;
    end;
  finally
    FLock.Leave;
  end;

  if not LNeedFetch then
    Exit;

  // Concurrent lookups that all miss do fetch more than once: nothing books the
  // attempt until an entry exists. StoreKeys is thread-safe and its writes are
  // serialized, so the last one wins with an equivalent key set - the cost is a
  // duplicate request, never a corrupted cache, and not worth optimizing away.
  try
    LKeys := FetchKeys(AIssuer);
  except
    on E: Exception do
    begin
      if Length(Result) > 0 then
      begin
        Logger.LogWarning('OAuth key set refresh failed for "%s", serving the cached keys: %s',
          [AIssuer, E.Message]);
        Exit;
      end;

      if E is EOAuthMetadataException then
        raise;
      raise EOAuthMetadataException.CreateFmt(SOAuthKeysFetchFailedFmt, [AIssuer, E.Message]);
    end;
  end;

  StoreKeys(LKey, LKeys);
  Result := LKeys;
end;

function TOAuthMetadataProvider.GetKeys(const AIssuer: string): TArray<TOAuthJsonWebKey>;
begin
  Result := EnsureKeys(AIssuer, False);
end;

function TOAuthMetadataProvider.TryGetKey(const AIssuer, AKeyId: string;
  out AKey: TOAuthJsonWebKey): Boolean;

  function FindKey(const AKeys: TArray<TOAuthJsonWebKey>; out AFound: TOAuthJsonWebKey): Boolean;
  var
    LKey: TOAuthJsonWebKey;
    LCandidates: Integer;
  begin
    if AKeyId <> '' then
    begin
      for LKey in AKeys do
        if (LKey.Kid = AKeyId) and LKey.IsSignatureKey then
        begin
          AFound := LKey;
          Exit(True);
        end;
      Exit(False);
    end;

    // A token without a "kid" can only be matched when the issuer publishes exactly
    // one signature key: guessing among several would be a downgrade waiting to happen.
    LCandidates := 0;
    for LKey in AKeys do
      if LKey.IsSignatureKey then
      begin
        Inc(LCandidates);
        AFound := LKey;
      end;

    Result := LCandidates = 1;
  end;

begin
  AKey := Default(TOAuthJsonWebKey);
  try
    if FindKey(EnsureKeys(AIssuer, False), AKey) then
      Exit(True);

    // Unknown key id: refresh once (rate limited) to pick up a key rotation.
    Result := FindKey(EnsureKeys(AIssuer, True), AKey);
    if not Result then
      Logger.LogWarning('OAuth key "%s" not published by issuer "%s"', [AKeyId, AIssuer]);
  except
    on E: Exception do
    begin
      Logger.LogError('OAuth key lookup failed for issuer "%s": %s', [AIssuer, E.Message]);
      Result := False;
    end;
  end;
end;

procedure TOAuthMetadataProvider.Invalidate(const AIssuer: string);
var
  LKey: string;
begin
  FLock.Enter;
  try
    if AIssuer.Trim = '' then
    begin
      FMetadataCache.Clear;
      FKeysCache.Clear;
      Exit;
    end;

    LKey := NormalizeIssuer(AIssuer);
    FMetadataCache.Remove(LKey);
    FKeysCache.Remove(LKey);
  finally
    FLock.Leave;
  end;
end;

end.
