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

  OAUTH_DISCOVERY_PATH = '/.well-known/openid-configuration';

resourcestring
  SOAuthIssuerRequired = 'An issuer URL is required to retrieve the authorization server metadata';
  SOAuthMetadataFetchFailedFmt = 'Cannot retrieve the authorization server metadata from "%s": %s';
  SOAuthMetadataInvalidFmt = 'The document retrieved from "%s" is not a valid metadata document';
  SOAuthJwksUriMissingFmt = 'The metadata document of "%s" does not declare a "jwks_uri"';
  SOAuthKeysFetchFailedFmt = 'Cannot retrieve the JSON Web Key Set from "%s": %s';

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
  TJsonWebKey = record
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

    class function FromJSON(AJSON: TJSONObject): TJsonWebKey; static;
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
    function GetKeys(const AIssuer: string): TArray<TJsonWebKey>;

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
    function TryGetKey(const AIssuer, AKeyId: string; out AKey: TJsonWebKey): Boolean;

    /// <summary>
    ///   Drops the cached metadata and keys of the given issuer, or of every issuer
    ///   when called with an empty string.
    /// </summary>
    procedure Invalidate(const AIssuer: string);
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
  /// </remarks>
  TOAuthMetadataProvider = class(TInterfacedObject, IOAuthMetadataProvider)
  private type
    TMetadataEntry = class
      Metadata: TOAuthServerMetadata;
      FetchedAt: TDateTime;
    end;

    TKeysEntry = class
      Keys: TArray<TJsonWebKey>;
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

    function FetchMetadata(const AIssuer: string): TOAuthServerMetadata;
    function FetchKeys(const AIssuer: string): TArray<TJsonWebKey>;
    function EnsureKeys(const AIssuer: string; AForceRefresh: Boolean): TArray<TJsonWebKey>;
    procedure StoreMetadata(const AKey: string; const AMetadata: TOAuthServerMetadata);
    procedure StoreKeys(const AKey: string; const AKeys: TArray<TJsonWebKey>);
    function TryGetCachedMetadata(const AKey: string; out AMetadata: TOAuthServerMetadata): Boolean;
  protected
    /// <summary>
    ///   Performs the actual HTTP GET. Overridable so that tests can serve documents
    ///   without a network, and so that a host can plug in its own HTTP stack.
    /// </summary>
    function FetchDocument(const AUrl: string): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { IOAuthMetadataProvider }
    function GetServerMetadata(const AIssuer: string): TOAuthServerMetadata;
    function GetKeys(const AIssuer: string): TArray<TJsonWebKey>;
    function TryGetKey(const AIssuer, AKeyId: string; out AKey: TJsonWebKey): Boolean;
    procedure Invalidate(const AIssuer: string);

    /// <summary>Lifetime of a cached metadata document, in seconds.</summary>
    property MetadataTTL: Integer read FMetadataTTL write FMetadataTTL;
    /// <summary>Lifetime of a cached key set, in seconds.</summary>
    property KeysTTL: Integer read FKeysTTL write FKeysTTL;
    /// <summary>Minimum interval between two unknown-key-id refreshes, in seconds.</summary>
    property KeysRefreshInterval: Integer read FKeysRefreshInterval write FKeysRefreshInterval;
    /// <summary>Connection and response timeout of the HTTP requests, in milliseconds.</summary>
    property RequestTimeout: Integer read FRequestTimeout write FRequestTimeout;
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

{ TJsonWebKey }

function TJsonWebKey.IsEmpty: Boolean;
begin
  Result := (Kty = '') and (N = '') and (X = '') and (Length(X5c) = 0);
end;

function TJsonWebKey.IsSignatureKey: Boolean;
begin
  Result := (Use = '') or SameText(Use, 'sig');
end;

class function TJsonWebKey.FromJSON(AJSON: TJSONObject): TJsonWebKey;
begin
  Result := Default(TJsonWebKey);
  if not Assigned(AJSON) then
    Exit;

  Result := TNeon.JSONToValue<TJsonWebKey>(AJSON, JwksNeonConfig);
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

    LResponse := LClient.Get(AUrl);
    if LResponse.StatusCode <> HTTP_STATUS_OK then
      raise EOAuthMetadataException.CreateFmt(SOAuthMetadataFetchFailedFmt,
        [AUrl, Format('HTTP %d', [LResponse.StatusCode])]);

    Result := LResponse.ContentAsString;
  finally
    LClient.Free;
  end;
end;

function TOAuthMetadataProvider.FetchMetadata(const AIssuer: string): TOAuthServerMetadata;
var
  LUrl: string;
  LJSON: TJSONValue;
begin
  LUrl := AIssuer.TrimRight(['/']) + OAUTH_DISCOVERY_PATH;

  LJSON := TJSONObject.ParseJSONValue(FetchDocument(LUrl));
  if not (LJSON is TJSONObject) then
  begin
    LJSON.Free;
    raise EOAuthMetadataException.CreateFmt(SOAuthMetadataInvalidFmt, [LUrl]);
  end;

  try
    Result := TOAuthServerMetadata.FromJSON(LJSON as TJSONObject);
  finally
    LJSON.Free;
  end;

  Logger.LogDebug('OAuth metadata fetched from "%s"', [LUrl]);
end;

function TOAuthMetadataProvider.FetchKeys(const AIssuer: string): TArray<TJsonWebKey>;
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
        Result := Result + [TJsonWebKey.FromJSON(LItem as TJSONObject)];
  finally
    LJSON.Free;
  end;

  Logger.LogDebug('OAuth key set fetched from "%s" (%d keys)', [LMetadata.JwksUri, Length(Result)]);
end;

procedure TOAuthMetadataProvider.StoreMetadata(const AKey: string;
  const AMetadata: TOAuthServerMetadata);
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
    LEntry.FetchedAt := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TOAuthMetadataProvider.StoreKeys(const AKey: string; const AKeys: TArray<TJsonWebKey>);
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
    LMetadata := FetchMetadata(AIssuer);
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

  StoreMetadata(LKey, LMetadata);
  Result := LMetadata;
end;

function TOAuthMetadataProvider.EnsureKeys(const AIssuer: string;
  AForceRefresh: Boolean): TArray<TJsonWebKey>;
var
  LKey: string;
  LEntry: TKeysEntry;
  LNeedFetch: Boolean;
  LKeys: TArray<TJsonWebKey>;
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

function TOAuthMetadataProvider.GetKeys(const AIssuer: string): TArray<TJsonWebKey>;
begin
  Result := EnsureKeys(AIssuer, False);
end;

function TOAuthMetadataProvider.TryGetKey(const AIssuer, AKeyId: string;
  out AKey: TJsonWebKey): Boolean;

  function FindKey(const AKeys: TArray<TJsonWebKey>; out AFound: TJsonWebKey): Boolean;
  var
    LKey: TJsonWebKey;
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
  AKey := Default(TJsonWebKey);
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
