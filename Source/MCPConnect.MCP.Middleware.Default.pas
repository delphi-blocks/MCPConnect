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
unit MCPConnect.MCP.Middleware.Default;

{
  The middleware MCPConnect ships with: behaviour of the framework itself,
  written as middleware rather than wired into the transport, so that it can be
  read, reordered, replaced or removed like any other.

  They are not registered by merely using this unit. Each is put in by the
  configuration that turns the feature on - IMCPConfig.Security.SetCORS adds
  TCORSMiddleware, IAuthTokenConfig.SetToken adds TAuthTokenMiddleware - so
  that a server configures a feature and gets its middleware, and one written
  by hand is registered the usual way:

    Server.Middleware.Add(TCORSMiddleware);
}

interface

{$I MCPConnect.inc}

uses
  System.SysUtils,

  MCPConnect.JRPC.Middleware;

type
  /// <summary>
  ///   Answers the CORS headers of a browser client and refuses a request
  ///   coming from an Origin that is not allowed.
  /// </summary>
  /// <remarks>
  ///   A transport middleware, and it has to be: a CORS preflight is an OPTIONS
  ///   request carrying no JSON-RPC message at all, so a hook of any of the
  ///   message levels would never see it, and the headers of a preflight are
  ///   the whole point of the exercise.
  ///
  ///   What it does is driven by IMCPConfig.Security: SetCORS decides whether
  ///   the headers are written, SetAllowedOrigins and SetRequireOrigin whether
  ///   a request is let through. With no allowed origin configured nothing is
  ///   checked, which is what a server not meant for browsers wants.
  ///
  ///   STDIO has no Origin and no headers to speak of, so on that transport it
  ///   does nothing.
  /// </remarks>
  TCORSMiddleware = class(TMiddleware, ITransportMiddleware)
  private
    /// <summary>
    ///   True when AOrigin is APattern, or matches it as a single-wildcard
    ///   glob: 'https://*.example.com' matches 'https://sub.example.com' but
    ///   never the bare 'https://example.com'.
    /// </summary>
    class function MatchesOriginPattern(const AOrigin, APattern: string): Boolean; static;

    /// <summary>
    ///   The authority of an origin - "host" or "host:port" - or '' when the
    ///   value is not an origin at all.
    /// </summary>
    class function OriginAuthority(const AOrigin: string): string; static;

    /// <summary>
    ///   The same authority with a port the scheme implies removed, so that
    ///   "example.com:443" and "example.com" compare equal for an https origin.
    /// </summary>
    class function WithoutDefaultPort(const AOrigin: string): string; static;

    /// <summary>
    ///   True when AOrigin names the host this very request was addressed to,
    ///   which is what AHost - the "Host" header - says.
    /// </summary>
    class function IsSameOrigin(const AOrigin, AHost: string): Boolean; static;

    /// <summary>
    ///   True when the host of AOrigin is a loopback address or name.
    /// </summary>
    /// <remarks>
    ///   The host of the *origin*, not the address the request arrived on: a
    ///   DNS-rebinding page reaches a local server under its own domain name,
    ///   so its Origin reads "http://evil.example" however that name resolves.
    ///   Reading the origin is what tells the two apart.
    /// </remarks>
    class function IsLoopbackOrigin(const AOrigin: string): Boolean; static;
  public
    /// <summary>
    ///   Just outside authentication, so that it wraps it: a request from an
    ///   Origin that is refused must not reach authentication, and a request
    ///   that authentication refuses must still carry the CORS headers, or the
    ///   browser reports a CORS failure instead of the refusal.
    ///   MW_PRIORITY_ERROR_HANDLING is left free for a middleware that wants to
    ///   wrap even this one.
    /// </summary>
    class function DefaultPriority: Integer; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

  /// <summary>
  ///   The token check (an API key, typically) that every request has to pass:
  ///   compared with the token configured on the server, or handed to the
  ///   validator the application registered. Refuses with 401 and a
  ///   WWW-Authenticate challenge when the token is missing or not accepted.
  /// </summary>
  /// <remarks>
  ///   A transport middleware, because what it looks at is the request of the
  ///   transport - a header, a cookie - and because a request it refuses must
  ///   not be parsed, let alone dispatched. IAuthTokenConfig says where the
  ///   token is read from and how it is checked; with neither a token nor a
  ///   validator configured the middleware does nothing.
  ///
  ///   A validator describes the caller into an identity of its own, copied
  ///   into the TMCPAccessToken of the request only once the token is accepted:
  ///   a refusal, or an exception, leaves nothing behind.
  ///
  ///   It is not OAuth: that is a different mechanism, enforced further in by
  ///   the transport itself, and the two are configured apart. A preflight is
  ///   let through - it carries no credentials by definition - and so are the
  ///   OAuth metadata endpoints when an authorization server is configured,
  ///   since a client has to read those before it has anything to send. STDIO
  ///   carries no headers, and a server spawned by its client already runs with
  ///   that client's authority, so there the check is skipped.
  /// </remarks>
  TAuthTokenMiddleware = class(TMiddleware, ITransportMiddleware)
  public
    /// <summary>
    ///   Identifies the caller, so it sits where authentication belongs: inside
    ///   CORS, outside everything else.
    /// </summary>
    class function DefaultPriority: Integer; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

implementation

uses
  Logify,

  JRPC.Core,

  MCPConnect.MCP.Types.Base,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Transport.Base;

resourcestring
  SCrossOriginBlocked = 'Cross-Origin Request Blocked: Same Origin Policy';
  SErrorRetrievingMCPConfig = 'Error retrieving MCP configuration';
  SInvalidTokenLocation = 'Invalid token location';
  SAuthorizationCheckFailed = 'Authorization check failed';
  SAuthTokenValidatorFailed = 'Auth token validation failed with an exception: %s';
  SAuthTokenRejected = 'Auth token missing or rejected [%s %s]';

const
  // The scheme of the challenge when the token travels outside the Authorization
  // header: there is no registered one for that, and "Bearer" would be a lie.
  ApiKeyScheme = 'ApiKey';
  BearerScheme = 'Bearer';

{ TCORSMiddleware }

class function TCORSMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_AUTHENTICATION - 100;
end;

class function TCORSMiddleware.MatchesOriginPattern(const AOrigin, APattern: string): Boolean;
var
  LStar: Integer;
  LPrefix, LSuffix: string;
begin
  // Exact match (case-insensitive: scheme and host are case-insensitive per RFC 6454)
  if SameText(AOrigin, APattern) then
    Exit(True);

  // Single-wildcard glob, e.g. 'https://*.example.com' matches 'https://sub.example.com'
  // but never the bare 'https://example.com' or a different suffix/prefix.
  LStar := APattern.IndexOf('*');
  if (LStar >= 0) and (APattern.IndexOf('*', LStar + 1) < 0) then
  begin
    LPrefix := APattern.Substring(0, LStar);
    LSuffix := APattern.Substring(LStar + 1);
    Exit(
      (AOrigin.Length > LPrefix.Length + LSuffix.Length) and
      AOrigin.StartsWith(LPrefix, True) and
      AOrigin.EndsWith(LSuffix, True)
    );
  end;

  Result := False;
end;

class function TCORSMiddleware.OriginAuthority(const AOrigin: string): string;
var
  LSchemeEnd: Integer;
begin
  LSchemeEnd := AOrigin.IndexOf('://');
  if LSchemeEnd < 0 then
    Exit('');

  Result := AOrigin.Substring(LSchemeEnd + 3);

  // An origin has no path, but a client that sends one anyway must not be able
  // to smuggle a foreign authority past the comparison
  var LSlash := Result.IndexOf('/');
  if LSlash >= 0 then
    Result := Result.Substring(0, LSlash);
end;

class function TCORSMiddleware.WithoutDefaultPort(const AOrigin: string): string;
begin
  Result := OriginAuthority(AOrigin);

  if AOrigin.StartsWith('http://', True) and Result.EndsWith(':80') then
    Result := Result.Substring(0, Result.Length - 3)
  else if AOrigin.StartsWith('https://', True) and Result.EndsWith(':443') then
    Result := Result.Substring(0, Result.Length - 4);
end;

class function TCORSMiddleware.IsSameOrigin(const AOrigin, AHost: string): Boolean;
var
  LHost: string;
begin
  if AHost.IsEmpty then
    Exit(False);

  LHost := AHost.Trim;

  // The Host header carries no scheme, so the two are compared as authorities:
  // once with the port as each side wrote it, and once with a port the scheme
  // implies dropped from both, since either side may spell it out.
  if SameText(OriginAuthority(AOrigin), LHost) then
    Exit(True);

  if LHost.EndsWith(':80') then
    LHost := LHost.Substring(0, LHost.Length - 3)
  else if LHost.EndsWith(':443') then
    LHost := LHost.Substring(0, LHost.Length - 4);

  Result := not LHost.IsEmpty and SameText(WithoutDefaultPort(AOrigin), LHost);
end;

class function TCORSMiddleware.IsLoopbackOrigin(const AOrigin: string): Boolean;
var
  LHost: string;
  LColon: Integer;
begin
  LHost := OriginAuthority(AOrigin);
  if LHost.IsEmpty then
    Exit(False);

  // An IPv6 literal is bracketed, and the brackets are part of the authority
  if LHost.StartsWith('[') then
  begin
    var LEnd := LHost.IndexOf(']');
    if LEnd < 0 then
      Exit(False);
    LHost := LHost.Substring(1, LEnd - 1);
  end
  else
  begin
    LColon := LHost.IndexOf(':');
    if LColon >= 0 then
      LHost := LHost.Substring(0, LColon);
  end;

  Result := SameText(LHost, 'localhost') or
            SameText(LHost, '::1') or
            // The whole 127.0.0.0/8 block is loopback, not 127.0.0.1 alone
            LHost.StartsWith('127.');
end;

procedure TCORSMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LConfig: TMCPConfig;

  procedure InjectCORS(const ASecurity: TMCPSecurityConfig);
  var
    LHValue: string;
  begin
    if not ASecurity.CORS then
      Exit;

    // Set the allowed origins (from security configuration)
    LHValue := LRequest.Origin;
    if LHValue.IsEmpty then
      Exit;

    LResponse.SetHeader('Access-Control-Allow-Origin', LHValue);

    // Set the allowed methods supported by the server (from security configuration)
    LHValue := LRequest.GetHeader('Access-Control-Request-Method');
    if not LHValue.IsEmpty then
      LResponse.SetHeader('Access-Control-Allow-Methods', string.Join(',', ASecurity.AllowedMethods));

    // Set the allowed headers as requested
    LHValue := LRequest.GetHeader('Access-Control-Request-Headers');
    if not LHValue.IsEmpty then
      LResponse.SetHeader('Access-Control-Allow-Headers', LHValue);

    // Expose the headers browser-based clients need to read from JS - notably to
    // discover the OAuth resource metadata URL from a 401 response.
    if Length(ASecurity.ExposeHeaders) > 0 then
      LHValue := string.Join(', ', ASecurity.ExposeHeaders)
    else
      LHValue := 'WWW-Authenticate';
    LResponse.SetHeader('Access-Control-Expose-Headers', LHValue);
  end;

  function CheckOrigin(const ASecurity: TMCPSecurityConfig): Boolean;
  var
    LOrigin, LHeader: string;
    LHasAllowlist: Boolean;
  begin
    LHasAllowlist := Length(ASecurity.AllowedOrigins) > 0;

    // Nothing configured and the policy turned off: the header is not looked at
    if not LHasAllowlist and (ASecurity.OriginPolicy = TMCPOriginPolicy.Off) then
      Exit(True);

    LHeader := LRequest.Origin.Trim;

    if LHeader.IsEmpty then
    begin
      // Only a browser sends an Origin, and a browser is what the check
      // defends against: a client that sends none is not the threat, and
      // refusing it would lock out every non-browser client there is.
      // SetRequireOrigin(True) is for a deployment that serves browsers only.
      if ASecurity.RequireOrigin then
      begin
        Logger.LogWarning('CheckOrigin: request blocked, missing Origin header');
        Exit(False);
      end;
      Exit(True);
    end;

    if SameText(LHeader, 'null') then
    begin
      Logger.LogWarning('CheckOrigin: request blocked, null Origin header');
      Exit(False);
    end;

    // An explicit allowlist is the policy, whole: it neither gains the
    // same-origin and loopback cases below nor loses what it names.
    if LHasAllowlist then
    begin
      for LOrigin in ASecurity.AllowedOrigins do
        if MatchesOriginPattern(LHeader, LOrigin) then
          Exit(True);

      Logger.LogWarning('CheckOrigin: request blocked, Origin "%s" not in allowlist', [LHeader]);
      Exit(False);
    end;

    // No allowlist, so the default policy decides. Two origins are this
    // server's business without anyone having to say so: its own, and a
    // loopback address - the local page of a developer or an inspector.
    if IsSameOrigin(LHeader, LRequest.GetHeader('Host')) or IsLoopbackOrigin(LHeader) then
      Exit(True);

    Logger.LogWarning(
      'CheckOrigin: request blocked, Origin "%s" is neither this server nor a loopback ' +
      'address, and no allowed origin is configured', [LHeader]);
    Result := False;
  end;

begin
  // Nothing to do outside an HTTP transport: STDIO carries no Origin, and a
  // request that never went through TMCPTransportHandler has neither of these
  // in its context.
  if AContext.TryFind<TMCPTransportRequest>(LRequest) and
     AContext.TryFind<TMCPTransportResponse>(LResponse) and
     (LRequest.Protocol <> TTransportProtocol.Stdio) then
  begin
    LConfig := AContext.Find<TMCPConfig>;
    if not Assigned(LConfig) then
      raise EMCPException.Create(SErrorRetrievingMCPConfig);

    // In this order, and before the chain: the headers are written even for a
    // request that is about to be refused, so that the browser reads the
    // refusal instead of reporting a CORS failure.
    InjectCORS(LConfig.Security);

    if not CheckOrigin(LConfig.Security) then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, SCrossOriginBlocked);
  end;

  AChain.Next(AContext);
end;

{ TAuthTokenMiddleware }

class function TAuthTokenMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_AUTHENTICATION;
end;

procedure TAuthTokenMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LRequest: TMCPTransportRequest;
  LTokenConfig: TAuthTokenConfig;
  LOAuthConfig: TOAuthConfig;

  function IsOAuthMetadataRequest: Boolean;
  begin
    // The endpoints a client reads before it has anything to send. Only when an
    // authorization server is configured: without one they answer nothing, and
    // exempting them would be a hole for no gain.
    if not Assigned(LOAuthConfig) or (Length(LOAuthConfig.AuthorizationServers) = 0) then
      Exit(False);

    Result :=
      (SameText(LRequest.Command, 'GET') and
        LOAuthConfig.IsProtectedResourceMetadataUrl(LRequest.Url)) or
      LOAuthConfig.IsMetadataProxyUrl(LRequest.Url);
  end;

  function ExtractToken: string;
  var
    LAuthHeader: string;
  begin
    case LTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        LAuthHeader := LRequest.Authorization;
        if LAuthHeader.StartsWith(BearerPrefix, True) then
          Result := LAuthHeader.Substring(Length(BearerPrefix)).Trim
        else
          Result := '';
      end;

      TAuthTokenLocation.Cookie:
        Result := LRequest.GetCookie(LTokenConfig.CustomHeader);

      TAuthTokenLocation.Header:
        Result := LRequest.GetHeader(LTokenConfig.CustomHeader);

    else
      raise EJRPCException.Create(SInvalidTokenLocation);
    end;
  end;

  function TokenAccepted: Boolean;
  var
    LIdentity: TMCPAccessToken;
    LTarget: TMCPAccessToken;
  begin
    // The validator writes into an identity of its own: only an accepted token
    // gets to hand it to the request, so that a validator which fills in the
    // caller and then refuses, or raises halfway, leaves nothing behind.
    LIdentity := TMCPAccessToken.Create;
    try
      try
        Result := LTokenConfig.Validate(AContext.RPCContext, ExtractToken, LIdentity);
      except
        // A failing validator must look exactly like a rejected token: never a
        // 500, and never a message telling the client which of the two it hit.
        on E: Exception do
        begin
          Logger.LogError(SAuthTokenValidatorFailed, [E.Message]);
          Result := False;
        end;
      end;

      LTarget := AContext.Find<TMCPAccessToken>;
      if Result and Assigned(LTarget) then
        LTarget.Assign(LIdentity);
    finally
      LIdentity.Free;
    end;
  end;

  procedure Reject;
  var
    LResponse: TMCPTransportResponse;
    LScheme, LRealm: string;
  begin
    // Never the token itself in the log: what a key looked like is not worth
    // turning the log into a place keys can be read from.
    Logger.LogWarning(SAuthTokenRejected, [LRequest.Command, LRequest.Url]);

    if LTokenConfig.Location = TAuthTokenLocation.Bearer then
      LScheme := BearerScheme
    else
      LScheme := ApiKeyScheme;

    if Assigned(LOAuthConfig) then
      LRealm := LOAuthConfig.Realm
    else
      LRealm := TOAuthConfig.DefaultRealm;

    // Written before raising: the transport keeps the headers of the response it
    // turns the exception into, as it does for the CORS ones.
    if AContext.TryFind<TMCPTransportResponse>(LResponse) then
      LResponse.SetHeader('WWW-Authenticate', Format('%s realm="%s"', [LScheme, LRealm]));

    raise EMCPTransportException.Create(HTTP_CODE_UNAUTHORIZED, SAuthorizationCheckFailed);
  end;

begin
  LTokenConfig := AContext.Find<TAuthTokenConfig>;
  LOAuthConfig := AContext.Find<TOAuthConfig>;

  // Nothing configured, nothing to check. Nor is there anything to read a token
  // from outside a transport that has a request, or on STDIO, which carries no
  // headers and runs with the authority of the client that spawned it.
  if Assigned(LTokenConfig) and LTokenConfig.Enabled and
     AContext.TryFind<TMCPTransportRequest>(LRequest) and
     (LRequest.Protocol <> TTransportProtocol.Stdio) then
  begin
    // A preflight carries no credentials by definition, so asking it for any
    // would refuse every browser client before its real request is ever sent.
    if not SameText(LRequest.Command, 'OPTIONS') and not IsOAuthMetadataRequest() and
       not TokenAccepted() then
      Reject;
  end;

  AChain.Next(AContext);
end;

end.
