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
  ///   The static token check: one token, configured on the server, that every
  ///   request has to carry. Refuses with 403 when it does not match.
  /// </summary>
  /// <remarks>
  ///   A transport middleware, because what it looks at is the request of the
  ///   transport - a header, a cookie - and because a request it refuses must
  ///   not be parsed, let alone dispatched. IAuthTokenConfig says where the
  ///   token is read from and what it is; with no token configured the
  ///   middleware does nothing.
  ///
  ///   It is not OAuth: that is a different mechanism, enforced further in by
  ///   the transport itself, and the two are configured apart. A preflight is
  ///   let through - it carries no credentials by definition - and so are the
  ///   OAuth metadata endpoints when an authorization server is configured,
  ///   since a client has to read those before it has anything to send.
  /// </remarks>
  TAuthTokenMiddleware = class(TMiddleware, ITransportMiddleware)
  private
    /// <summary>
    ///   Compares two strings in time that does not depend on where they first
    ///   differ, so that the comparison of a guessed token leaks nothing.
    /// </summary>
    class function ConstantTimeEquals(const A, B: string): Boolean; static;
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
  begin
    if Length(ASecurity.AllowedOrigins) = 0 then
      Exit(True);

    LHeader := LRequest.Origin.Trim;

    if LHeader.IsEmpty then
    begin
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

    for LOrigin in ASecurity.AllowedOrigins do
      if MatchesOriginPattern(LHeader, LOrigin) then
        Exit(True);

    Logger.LogWarning('CheckOrigin: request blocked, Origin "%s" not in allowlist', [LHeader]);
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

class function TAuthTokenMiddleware.ConstantTimeEquals(const A, B: string): Boolean;
var
  I, LMaxLen: Integer;
  LDiff: Integer;
  LCharA, LCharB: Word;
begin
  // Compares the full length of both strings regardless of where they first differ,
  // so the running time does not leak how many leading characters of a guessed
  // token/secret matched (a classic side channel for '<>' / early-exit comparisons).
  LMaxLen := Length(A);
  if Length(B) > LMaxLen then
    LMaxLen := Length(B);

  LDiff := Length(A) xor Length(B);
  for I := 1 to LMaxLen do
  begin
    if I <= Length(A) then LCharA := Word(A[I]) else LCharA := 0;
    if I <= Length(B) then LCharB := Word(B[I]) else LCharB := 0;
    LDiff := LDiff or (LCharA xor LCharB);
  end;

  Result := LDiff = 0;
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

  function TokenMatches: Boolean;
  begin
    Result := True;
    case LTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        var LAuthHeader := LRequest.Authorization;
        if not LAuthHeader.StartsWith(BearerPrefix, True) then
          Exit(False);
        if not ConstantTimeEquals(LAuthHeader.Substring(Length(BearerPrefix)),
             LTokenConfig.Token) then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if not ConstantTimeEquals(LRequest.GetCookie(LTokenConfig.CustomHeader),
             LTokenConfig.Token) then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if not ConstantTimeEquals(LRequest.GetHeader(LTokenConfig.CustomHeader),
             LTokenConfig.Token) then
          Exit(False);
      end;

    else
      raise EJRPCException.Create(SInvalidTokenLocation);
    end;
  end;

begin
  LTokenConfig := AContext.Find<TAuthTokenConfig>;
  LOAuthConfig := AContext.Find<TOAuthConfig>;

  // No token configured, nothing to check. Nor is there anything to read a
  // token from outside a transport that has a request.
  if Assigned(LTokenConfig) and (LTokenConfig.Token <> '') and
     AContext.TryFind<TMCPTransportRequest>(LRequest) then
  begin
    // A preflight carries no credentials by definition, so asking it for any
    // would refuse every browser client before its real request is ever sent.
    if not SameText(LRequest.Command, 'OPTIONS') and not IsOAuthMetadataRequest() and
       not TokenMatches() then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, SAuthorizationCheckFailed);
  end;

  AChain.Next(AContext);
end;

end.
