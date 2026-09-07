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
unit MCPConnect.MCP.Middleware.OAuth;

{
  OAuth enforcement, as one transport middleware: the whole of what an MCP
  server does about OAuth 2.1 in front of a request.

  Two jobs, and they are the same job seen from the two ends of the handshake:

  - it answers the discovery endpoints a client reads before it has a token -
    "/.well-known/oauth-protected-resource" (RFC 9728) and, when the metadata
    proxy is enabled, the authorization server document republished under
    "/oauth-proxy" (RFC 8414 / OIDC Discovery). These never reach the
    dispatcher: the middleware writes the answer and does not call Next;

  - it validates the bearer token of everything else, and refuses with a 401
    carrying the WWW-Authenticate challenge that points a client at the
    discovery document above.

  Like the rest of MCPConnect.MCP.Middleware.Default it is registered by the
  configuration that turns the feature on - IOAuthConfig.AddAuthorizationServer -
  rather than by using this unit, and can be removed or replaced on
  Server.Middleware like any other.
}

interface

{$I MCPConnect.inc}

uses
  System.SysUtils, System.SyncObjs,

  MCPConnect.JRPC.Middleware;

type
  /// <summary>
  ///   Enforces OAuth 2.1 on a transport request, and answers the discovery
  ///   endpoints a client needs before it can carry a token.
  /// </summary>
  /// <remarks>
  ///   Inside the static token check (see TAuthTokenMiddleware) and inside CORS,
  ///   so that a 401 still carries the headers a browser needs to read it. What
  ///   it does is driven by IOAuthConfig: with no authorization server
  ///   configured it does nothing at all, which is what a server not using
  ///   OAuth wants.
  ///
  ///   STDIO is exempt: it carries no headers to put a token in, no URL for the
  ///   client to discover metadata at, and no 401 to answer with. A server
  ///   launched by its client over a pipe already runs with that client's
  ///   authority. A CORS preflight is exempt too - it carries no credentials by
  ///   definition.
  /// </remarks>
  TOAuthMiddleware = class(TMiddleware, ITransportMiddleware)
  private type
    TProxyCacheEntry = class
      Content: string;
      FetchedAt: TDateTime;
    end;

    /// <summary>
    ///   The upstream metadata document, kept for a while. Per process, not per
    ///   request: every client of the server asks for the same document, and it
    ///   changes about never.
    /// </summary>
    TProxyCache = class
    private
      FLock: TCriticalSection;
      FEntry: TProxyCacheEntry;
      FTTLSeconds: Integer;
    public
      constructor Create(ATTLSeconds: Integer);
      destructor Destroy; override;
      function TryGet(out AContent: string): Boolean;
      procedure Store(const AContent: string);
    end;
  private
    class var FProxyCache: TProxyCache;
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>
    ///   Just inside the static token check: both identify the caller, and the
    ///   two are alternatives more often than not, but a server running both
    ///   wants the cheap comparison to have gone first.
    /// </summary>
    class function DefaultPriority: Integer; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

implementation

uses
  System.JSON, System.DateUtils, System.Net.HttpClient,

  Logify,
  JRPC.Core,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Utils,

  MCPConnect.MCP.Types.Base,
  MCPConnect.Configuration.Auth,
  MCPConnect.Security.Token,
  MCPConnect.Transport.MediaType,
  MCPConnect.Transport.Base;

resourcestring
  SOAuthValidatorNotConfigured = 'A bearer token was received but no token validator is ' +
    'registered: the request is rejected. See IOAuthConfig.SetTokenValidatorClass.';
  SOAuthValidatorNotSupportedFmt = 'The registered token validator [%s] does not expose ' +
    'ITokenValidator: the request is rejected';

const
  /// <summary>How long the proxied upstream metadata document is kept, in seconds.</summary>
  ProxyCacheTTL = 300;

{ TOAuthMiddleware.TProxyCache }

constructor TOAuthMiddleware.TProxyCache.Create(ATTLSeconds: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FTTLSeconds := ATTLSeconds;
end;

destructor TOAuthMiddleware.TProxyCache.Destroy;
begin
  FEntry.Free;
  FLock.Free;
  inherited;
end;

function TOAuthMiddleware.TProxyCache.TryGet(out AContent: string): Boolean;
begin
  FLock.Enter;
  try
    Result := Assigned(FEntry) and (SecondsBetween(Now, FEntry.FetchedAt) < FTTLSeconds);
    if Result then
      AContent := FEntry.Content;
  finally
    FLock.Leave;
  end;
end;

procedure TOAuthMiddleware.TProxyCache.Store(const AContent: string);
begin
  FLock.Enter;
  try
    if not Assigned(FEntry) then
      FEntry := TProxyCacheEntry.Create;
    FEntry.Content := AContent;
    FEntry.FetchedAt := Now;
  finally
    FLock.Leave;
  end;
end;

{ TOAuthMiddleware }

class constructor TOAuthMiddleware.Create;
begin
  FProxyCache := TProxyCache.Create(ProxyCacheTTL);
end;

class destructor TOAuthMiddleware.Destroy;
begin
  FProxyCache.Free;
end;

class function TOAuthMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_AUTHENTICATION + 100;
end;

procedure TOAuthMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LConfig: TOAuthConfig;

  function ValidateAccessToken(const AToken: string): TTokenValidationResult;
  var
    LInstance: TObject;
    LValidator: ITokenValidator;
  begin
    // Fail-closed: with no validator registered no token can be trusted, so none is
    // accepted. TOAuthConfig.ApplyConfig logs a warning about this at startup.
    if not Assigned(LConfig.TokenValidatorClass) then
    begin
      Logger.LogWarning(SOAuthValidatorNotConfigured);
      Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
    end;

    try
      // Built through RTTI so that a validator is bound to nothing but ITokenValidator:
      // no base class of ours, no constructor of ours. SetTokenValidatorClass already
      // refused any class that does not implement the interface, so the Supports below
      // is a belt-and-braces check rather than the real gate.
      LInstance := TRttiUtils.CreateInstance(LConfig.TokenValidatorClass);
      if not Supports(LInstance, ITokenValidator, LValidator) then
      begin
        LInstance.Free;
        Logger.LogError(SOAuthValidatorNotSupportedFmt,
          [LConfig.TokenValidatorClass.ClassName]);
        Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
      end;

      // LValidator is the only reference held: the instance is destroyed when this
      // method returns, which is why an implementation has to be reference counted.
      // The access token it fills in is the one already in the request context, so
      // whatever it puts there is what the tools are injected with.
      Result := LValidator.Validate(AContext.RPCContext, AToken,
        AContext.Find<TMCPAccessToken>);
    except
      // A failing validator must look exactly like an invalid token: never a 500, and
      // never a message that tells a client whether it hit a bug or a rejected token.
      on E: Exception do
      begin
        Logger.LogError('Token validation failed with an exception: %s', [E.Message]);
        Result := TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, '');
      end;
    end;
  end;

  procedure SendUnauthorized(const AResult: TTokenValidationResult);
  begin
    LResponse.Code := HTTP_CODE_UNAUTHORIZED;
    LResponse.SetHeader('WWW-Authenticate',
      BuildBearerChallenge(LConfig.Realm, LConfig.ResourceMetadata, AResult));
  end;

  procedure SendProtectedResourceMetadata;
  var
    LMetadata: TOAuthProtectedResourceMetadata;
  begin
    LResponse.Code := HTTP_CODE_OK;
    LResponse.ContentType := TMediaType.APPLICATION_JSON;

    LMetadata := TOAuthProtectedResourceMetadata.Create;
    try
      LMetadata.Resource := LConfig.Resource;
      LMetadata.AuthorizationServers := LConfig.AuthorizationServers;
      LMetadata.ScopesSupported := LConfig.ScopesSupported;
      LResponse.Content := TNeon.ObjectToJSONString(LMetadata, TNeonConfiguration.Snake);
    finally
      LMetadata.Free;
    end;
  end;

  procedure SendMetadataProxy;
  const
    RequestTimeoutMs = 10000;

    // Built through TJSONObject rather than interpolated: an upstream status line or an
    // exception message carrying a quote or a line break would otherwise produce a body
    // that is not JSON at all, and the client would report a parse error instead of what
    // actually went wrong.
    function ErrorBody(const AMessage: string): string;
    var
      LJSON: TJSONObject;
    begin
      LJSON := TJSONObject.Create;
      try
        LJSON.AddPair('error', AMessage);
        Result := LJSON.ToJSON;
      finally
        LJSON.Free;
      end;
    end;

  begin
    LResponse.ContentType := TMediaType.APPLICATION_JSON;

    var LCached: string;
    if FProxyCache.TryGet(LCached) then
    begin
      LResponse.Code := HTTP_CODE_OK;
      LResponse.Content := LCached;
      Exit;
    end;

    var LHttp := THTTPClient.Create;
    try
      LHttp.ConnectionTimeout := RequestTimeoutMs;
      LHttp.ResponseTimeout := RequestTimeoutMs;
      LHttp.HandleRedirects := False;
      try
        var LUpstream := LConfig.MetadataProxyUpstream.TrimRight(['/']);
        var LUpstreamUrl := LUpstream + '/.well-known/openid-configuration';
        var LUpstreamResponse := LHttp.Get(LUpstreamUrl);

        if LUpstreamResponse.StatusCode <> HTTP_CODE_OK then
        begin
          LResponse.Code := HTTP_CODE_BADGATEWAY;
          LResponse.Content := ErrorBody(Format(
            'Failed to fetch upstream authorization server metadata (HTTP %d)',
            [LUpstreamResponse.StatusCode]));
          Exit;
        end;

        var LBody := LUpstreamResponse.ContentAsString;
        if LBody.Length > 1024 * 1024 then
        begin
          LResponse.Code := HTTP_CODE_BADGATEWAY;
          LResponse.Content := ErrorBody('Upstream metadata document exceeds 1 MB size limit');
          Exit;
        end;

        var LJSON := TJSONObject.ParseJSONValue(LBody, True, True) as TJSONObject;
        try
          // RFC 8414 §3.3: verify that the upstream document's issuer matches the
          // configured upstream URL before trusting anything else in the document.
          // Without this, a compromised or redirected upstream can point
          // authorization_endpoint / jwks_uri anywhere it likes.
          var LDocIssuer: string;
          if not LJSON.TryGetValue<string>('issuer', LDocIssuer)
             or (LDocIssuer.Trim.TrimRight(['/']).ToLower <> LUpstream.ToLower) then
          begin
            LResponse.Code := HTTP_CODE_BADGATEWAY;
            LResponse.Content := ErrorBody(Format(
              'Upstream metadata issuer mismatch: expected "%s"', [LUpstream]));
            Exit;
          end;

          var LMethods: TJSONArray;
          if not (LJSON.TryGetValue<TJSONArray>('code_challenge_methods_supported', LMethods) and (LMethods.Count > 0)) then
          begin
            var LExisting := LJSON.RemovePair('code_challenge_methods_supported');
            LExisting.Free;
            var LNewMethods := TJSONArray.Create;
            LNewMethods.Add('S256');
            LJSON.AddPair('code_challenge_methods_supported', LNewMethods);
          end;

          // Per RFC 8414 §3.3, "issuer" must exactly match the URL the metadata
          // document was retrieved from, i.e. this proxy's own URL - not the
          // upstream authorization server's issuer - or strict MCP OAuth clients
          // reject the document with an issuer mismatch.
          //
          // This is a trade rather than a correction, so think before removing it.
          // The upstream server keeps minting its own issuer everywhere else, so a
          // client checking the "iss" of the authorization response (RFC 9207, which
          // OAuth 2.1 requires) now sees a mismatch instead. A proxy that only patches
          // the document cannot make both checks pass; taking this line out just moves
          // the failure from before the redirect to after it. IOAuthConfig.
          // EnableMetadataProxy documents the choice and the server warns about it at
          // startup. Access tokens are unaffected: they carry the upstream "iss", which
          // is what TOAuthConfig.TrustedIssuers resolves to when the proxy is enabled.
          var LExistingIssuer := LJSON.RemovePair('issuer');
          LExistingIssuer.Free;
          LJSON.AddPair('issuer', LConfig.MetadataProxyUrl);

          LResponse.Code := HTTP_CODE_OK;
          LResponse.Content := LJSON.ToJSON;
          FProxyCache.Store(LResponse.Content);
        finally
          LJSON.Free;
        end;
      except
        on E: Exception do
        begin
          LResponse.Code := HTTP_CODE_BADGATEWAY;
          LResponse.Content := ErrorBody(E.Message);
        end;
      end;
    finally
      LHttp.Free;
    end;
  end;

begin
  LConfig := AContext.Find<TOAuthConfig>;

  if not Assigned(LConfig) or (Length(LConfig.AuthorizationServers) < 1) or
     not AContext.TryFind<TMCPTransportRequest>(LRequest) or
     not AContext.TryFind<TMCPTransportResponse>(LResponse) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // STDIO carries none of this, and a preflight carries no credentials.
  if (LRequest.Protocol = TTransportProtocol.Stdio) or
     SameText(LRequest.Command, 'OPTIONS') then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // The discovery endpoints: a client reads these before it has anything to
  // send, so they are answered here, without a token, and go no further.
  if SameText(LRequest.Command, 'GET') and
     LConfig.IsProtectedResourceMetadataUrl(LRequest.Url) then
  begin
    SendProtectedResourceMetadata();
    Exit;
  end;

  if LConfig.MetadataProxyEnabled and (LRequest.Command = 'GET') and
     LConfig.IsMetadataProxyUrl(LRequest.Url) then
  begin
    SendMetadataProxy();
    Exit;
  end;

  var LAuthHeader := LRequest.Authorization;
  if not LAuthHeader.StartsWith(BearerPrefix, True) then
  begin
    SendUnauthorized(TTokenValidationResult.Fail(TTokenValidationErrorCode.None, ''));
    Exit;
  end;

  var LResult := ValidateAccessToken(LAuthHeader.Substring(Length(BearerPrefix)).Trim);
  if not LResult.Success then
  begin
    SendUnauthorized(LResult);
    Exit;
  end;

  AChain.Next(AContext);
end;

end.
