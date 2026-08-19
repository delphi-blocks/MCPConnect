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
unit MCPConnect.Transport.Base;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  IdCustomHTTPServer, IdContext, IdGlobal,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Transport.AcceptParser,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Session,
  MCPConnect.Security.Token,
  MCPConnect.Session.Core,
  MCPConnect.MCP.Types,
  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

const
  HTTP_CODE_OK = 200;
  HTTP_CODE_ACCEPTED = 202;
  HTTP_CODE_NOCONTENT = 204;
  HTTP_CODE_BADREQUEST = 400;
  HTTP_CODE_UNAUTHORIZED = 401;
  HTTP_CODE_FORBIDDEN = 403;
  HTTP_CODE_NOTFOUND = 404;
  HTTP_CODE_NOTALLOWED = 405;
  HTTP_CODE_NOTACCEPTABLE = 406;
  HTTP_CODE_BADGATEWAY = 502;

  /// <summary>Scheme prefix of the "Authorization" header carrying an OAuth access token.</summary>
  BearerPrefix = 'Bearer ';

resourcestring
  SInvalidTokenLocation = 'Invalid token location';
  SErrorRetrievingMCPConfig = 'Error retrieving MCP configuration';
  SCrossOriginBlocked = 'Cross-Origin Request Blocked: Same Origin Policy';
  SAuthorizationCheckFailed = 'Authorization check failed';
  SSessionIdLocationNotFound = 'SessionId Location not found';
  SHttpMethodNotAllowed = 'Http method not allowed';
  STransportSSENotSupported = 'SSE not supported';
  SEventStreamOnlyForGET = 'Only Event Stream response is supported for GET requests';
  STransportSessionNotFound = 'Session not found';
  STransportMethodNotFoundFmt = 'Method "%s" not found';
  SSessionIdHeaderRequired = 'Mcp-Session-Id header is required';
  SOAuthValidatorNotConfigured = 'A bearer token was received but no token validator is ' +
    'registered: the request is rejected. See IOAuthConfig.SetTokenValidatorClass.';
  SOAuthValidatorNotSupportedFmt = 'The registered token validator [%s] does not expose ' +
    'ITokenValidator: the request is rejected';

type
  /// <summary>
  ///   Exception class for all transport related errors
  /// </summary>
  EMCPTransportException = class(EMCPException)
  private
    FCode: Integer;
  public
    constructor Create(ACode: Integer; const AMsg: string);
    function ToJSON: string;
    property Code: Integer read FCode write FCode;
  end;

  IMCPTransportWriter = interface
    ['{68598454-50C5-4892-B8E0-81687CC2F4DE}']
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  TTransportProtocol = (Undefined, Stdio, StreamableHTTP);

  TMCPTransportHeaders = class
  private type
    THeaders = class(TDictionary<string, string>)
    end;
  public
    Headers: THeaders;

    constructor Create;
    destructor Destroy; override;

    function GetHeader(const AName: string): string;
    procedure AddOrSetHeader(const AName, AValue: string);
  end;

  TMCPTransportRequest = class(TMCPTransportHeaders)
  private
    FAcceptItems: TAcceptItemList<TAcceptItem>;
    FProtocol: TTransportProtocol;
    function GetAccept: string;
    procedure SetAccept(const AValue: string);
    function GetAcceptItems: TAcceptItemList<TAcceptItem>;
    function GetAcceptsEventStream: Boolean;
  public
    Url: string;
    Command: string;
    Content: string;
    ContentJSON: TJSONValue;

    function GetCookie(const AName: string): string;
    property Accept: string read GetAccept write SetAccept;
    property AcceptItems: TAcceptItemList<TAcceptItem> read GetAcceptItems;
    property AcceptsEventStream: Boolean read GetAcceptsEventStream;
    property Protocol: TTransportProtocol read FProtocol write FProtocol;

    constructor Create;
    destructor Destroy; override;
  end;

  TMCPTransportRequestConverter = reference to procedure (ARequest: TMCPTransportRequest);

  TMCPTransportResponse = class(TMCPTransportHeaders)
  private
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
  public
    Content: string;
    Code: Integer;
    Outbund: TQueue<string>;

    procedure SetCookie(const AName, AValue: string; ASecure: Boolean = True);
    property ContentType: string read GetContentType write SetContentType;
  end;

  TMCPTransportResponseConverter = reference to procedure (AResponse: TMCPTransportResponse);

  IMCPTransportHandler = interface
  ['{B2966C2A-7594-4B30-95D9-D702AE20633E}']
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
    function GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    procedure SetSendResponseHeadersProc(const Value: TProc<TMCPTransportResponse>);
    property SendResponseHeadersProc: TProc<TMCPTransportResponse> read GetSendResponseHeadersProc write SetSendResponseHeadersProc;
  end;

  TMCPTransportHandler = class(TInterfacedObject, IMCPTransportHandler)
  private type
    TProxyCacheEntry = class
      Content: string;
      FetchedAt: TDateTime;
    end;

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
    FRequest: TMCPTransportRequest;
    FResponse: TMCPTransportResponse;

    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;
    FSession: TMCPSessionBase;
    FAccessToken: TMCPAccessToken;

    FMCPConfig: TMCPConfig;
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    FOAuthConfig: TOAuthConfig;
    FSessionConfig: TSessionConfig;
    FResponseWriter: IMCPTransportWriter;
    FSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    class var FProxyCache: TProxyCache;
    class constructor Create;
    class destructor Destroy;
  private
    procedure InjectCORS;
    function CheckOrigin: Boolean;
    class function MatchesOriginPattern(const AOrigin, APattern: string): Boolean; static;
    class function ConstantTimeEquals(const A, B: string): Boolean; static;
    function CheckAuthorization: Boolean;
    function CheckOAuth: Boolean;
    function ValidateAccessToken(const AToken: string): TTokenValidationResult;
    procedure SendUnauthorized(const AResult: TTokenValidationResult);
    function IsProtectedResourceMetadataRequest: Boolean;
    function IsMetadataProxyRequest: Boolean;
    procedure HandleMetadataProxy;
    function ExtractSessionId: string;
    function IsInitializeRequest: Boolean;
    function HandleSession: TMCPSessionBase;
    procedure HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
    procedure SendResponseHeaders(AResponse: TMCPTransportResponse);
    procedure WriteSSEResponse(const AValue: string; const AEventId: string = '');

    procedure HandleGET;
    procedure HandlePOST;
    procedure HandleOPTIONS;
    function CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
  public
    constructor Create(AServer: TJRPCServer; AResponseWriter: IMCPTransportWriter);
    destructor Destroy; override;

    { IMCPHttpHandler }
    procedure ProcessRequest(ARequestConverter: TMCPTransportRequestConverter;
      AResponseConverter: TMCPTransportResponseConverter);
    function GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
    procedure SetSendResponseHeadersProc(const Value: TProc<TMCPTransportResponse>);
  end;

implementation

uses
  System.IOUtils, System.Net.HttpClient, System.Diagnostics,
  System.DateUtils,
  Logify,
  Neon.Core.Utils,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Invoker;


{ TMCPTransportHandler.TProxyCache }

constructor TMCPTransportHandler.TProxyCache.Create(ATTLSeconds: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FTTLSeconds := ATTLSeconds;
end;

destructor TMCPTransportHandler.TProxyCache.Destroy;
begin
  FEntry.Free;
  FLock.Free;
  inherited;
end;

function TMCPTransportHandler.TProxyCache.TryGet(out AContent: string): Boolean;
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

procedure TMCPTransportHandler.TProxyCache.Store(const AContent: string);
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

{ TMCPTransportHandler }

class constructor TMCPTransportHandler.Create;
begin
  FProxyCache := TProxyCache.Create(300);
end;

class destructor TMCPTransportHandler.Destroy;
begin
  FProxyCache.Free;
end;

constructor TMCPTransportHandler.Create(AServer: TJRPCServer; AResponseWriter: IMCPTransportWriter);
begin
  FRequest := TMCPTransportRequest.Create;
  FResponse := TMCPTransportResponse.Create;
  FAccessToken := TMCPAccessToken.Create;

  FServer := AServer;
  FResponseWriter := AResponseWriter;
  FMCPConfig := FServer.GetConfiguration<TMCPConfig>;
  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
  FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
  FOAuthConfig := FServer.GetConfiguration<TOAuthConfig>;
end;

destructor TMCPTransportHandler.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FAccessToken.Free;

  Logger.LogDebug('MCPTransportHandler destroyed');
  inherited;
end;

class function TMCPTransportHandler.ConstantTimeEquals(const A, B: string): Boolean;
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

function TMCPTransportHandler.CheckAuthorization: Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    if SameText(FRequest.Command, 'OPTIONS') then
      Exit;
    if (Length(FOAuthConfig.AuthorizationServers) > 0) and
       (IsProtectedResourceMetadataRequest or IsMetadataProxyRequest) then
      Exit;
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        var LAuthHeader := FRequest.GetHeader('Authorization');
        if not LAuthHeader.StartsWith('Bearer ', True) then
          Exit(False);
        if not ConstantTimeEquals(LAuthHeader.Substring(7), FAuthTokenConfig.Token) then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if not ConstantTimeEquals(FRequest.GetCookie(FAuthTokenConfig.CustomHeader), FAuthTokenConfig.Token) then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if not ConstantTimeEquals(FRequest.GetHeader(FAuthTokenConfig.CustomHeader), FAuthTokenConfig.Token) then
          Exit(False);
      end;

    else
      raise EJRPCException.Create(SInvalidTokenLocation);
    end;
  end;
end;

function TMCPTransportHandler.CheckOAuth: Boolean;
begin
  if Length(FOAuthConfig.AuthorizationServers) < 1 then
    Exit(True);

  // OAuth is an HTTP mechanism and STDIO carries none of it: no headers to put a token
  // in, no URL for the client to discover metadata at, and no 401 to answer with. A
  // server launched by its client over a pipe is already running with that client's
  // authority, so there is nothing here for a bearer token to establish. Enforcing it
  // would not secure anything - it would reject every request on the transport,
  // silently, since a stdio client never sees the challenge.
  if FRequest.Protocol = TTransportProtocol.Stdio then
    Exit(True);

  if SameText(FRequest.Command, 'OPTIONS') then
    Exit(True);

  Result := False;
  if IsProtectedResourceMetadataRequest then
  begin
    FResponse.Code := HTTP_CODE_OK;
    FResponse.ContentType := TMediaType.APPLICATION_JSON;

    var LMetadata := TOAuthProtectedResourceMetadata.Create;
    try
      LMetadata.Resource := FOAuthConfig.Resource;
      LMetadata.AuthorizationServers := FOAuthConfig.AuthorizationServers;
      LMetadata.ScopesSupported := FOAuthConfig.ScopesSupported;
      FResponse.Content := TNeon.ObjectToJSONString(LMetadata, TNeonConfiguration.Snake);
    finally
      LMetadata.Free;
    end;
  end
  else if FOAuthConfig.MetadataProxyEnabled and (FRequest.Command = 'GET') and IsMetadataProxyRequest then
  begin
    HandleMetadataProxy;
  end
  else
  begin
    var LAuthHeader := FRequest.GetHeader('Authorization');
    if LAuthHeader.StartsWith(BearerPrefix, True) then
    begin
      var LResult := ValidateAccessToken(LAuthHeader.Substring(Length(BearerPrefix)).Trim);
      if LResult.Success then
        Exit(True);

      SendUnauthorized(LResult);
      Exit(False);
    end;

    SendUnauthorized(TTokenValidationResult.Fail(TTokenValidationErrorCode.None, ''));
  end;
end;

function TMCPTransportHandler.ValidateAccessToken(const AToken: string): TTokenValidationResult;
var
  LInstance: TObject;
  LValidator: ITokenValidator;
begin
  // Fail-closed: with no validator registered no token can be trusted, so none is
  // accepted. TOAuthConfig.ApplyConfig logs a warning about this at startup.
  if not Assigned(FOAuthConfig.TokenValidatorClass) then
  begin
    Logger.LogWarning(SOAuthValidatorNotConfigured);
    Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
  end;

  try
    // Built through RTTI so that a validator is bound to nothing but ITokenValidator:
    // no base class of ours, no constructor of ours. SetTokenValidatorClass already
    // refused any class that does not implement the interface, so the Supports below
    // is a belt-and-braces check rather than the real gate.
    LInstance := TRttiUtils.CreateInstance(FOAuthConfig.TokenValidatorClass);
    if not Supports(LInstance, ITokenValidator, LValidator) then
    begin
      LInstance.Free;
      Logger.LogError(SOAuthValidatorNotSupportedFmt,
        [FOAuthConfig.TokenValidatorClass.ClassName]);
      Exit(TTokenValidationResult.Fail(TTokenValidationErrorCode.InvalidToken, ''));
    end;

    // LValidator is the only reference held: the instance is destroyed when this
    // method returns, which is why an implementation has to be reference counted.
    Result := LValidator.Validate(FContext, AToken, FAccessToken);
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

procedure TMCPTransportHandler.WriteSSEResponse(const AValue, AEventId: string);
begin
  if Assigned(FResponseWriter) then
  begin
    Logger.LogTrace('[SSE] Event Sent [id=%s, size=%d]', [AEventId, Length(AValue)]);
    FResponseWriter.Write(AValue, AEventId);
  end;
end;

procedure TMCPTransportHandler.SendUnauthorized(const AResult: TTokenValidationResult);
begin
  FResponse.Code := HTTP_CODE_UNAUTHORIZED;
  FResponse.Headers.AddOrSetValue('WWW-Authenticate',
    BuildBearerChallenge(FOAuthConfig.Realm, FOAuthConfig.ResourceMetadata, AResult));
end;

function TMCPTransportHandler.IsProtectedResourceMetadataRequest: Boolean;
begin
  if not SameText(FRequest.Command, 'GET') then
    Exit(False);

  // The path-insertion form of RFC 9728 §3.1 is what this server advertises in its
  // challenge, and what a client that builds the URL itself arrives at. The bare
  // well-known path is served too: clients fall back to it, and it is the only form a
  // resource that is just an origin has. Both are derived from the configured
  // resource rather than assuming the endpoint is mounted at "/mcp".
  Result :=
    SameText(FRequest.Url, TOAuthConfig.ProtectedResourcePath + FOAuthConfig.ResourcePath) or
    SameText(FRequest.Url, TOAuthConfig.ProtectedResourcePath);
end;

function TMCPTransportHandler.IsMetadataProxyRequest: Boolean;
begin
  Result :=
    SameText(FRequest.Url, '/.well-known/oauth-authorization-server' + TOAuthConfig.MetadataProxyPath) or
    SameText(FRequest.Url, '/.well-known/openid-configuration' + TOAuthConfig.MetadataProxyPath) or
    SameText(FRequest.Url, TOAuthConfig.MetadataProxyPath + '/.well-known/openid-configuration');
end;

procedure TMCPTransportHandler.HandleMetadataProxy;
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
  FResponse.ContentType := TMediaType.APPLICATION_JSON;

  var LCached: string;
  if FProxyCache.TryGet(LCached) then
  begin
    FResponse.Code := HTTP_CODE_OK;
    FResponse.Content := LCached;
    Exit;
  end;

  var LHttp := THTTPClient.Create;
  try
    LHttp.ConnectionTimeout := RequestTimeoutMs;
    LHttp.ResponseTimeout := RequestTimeoutMs;
    LHttp.HandleRedirects := False;
    try
      var LUpstream := FOAuthConfig.MetadataProxyUpstream.TrimRight(['/']);
      var LUpstreamUrl := LUpstream + '/.well-known/openid-configuration';
      var LResponse := LHttp.Get(LUpstreamUrl);

      if LResponse.StatusCode <> HTTP_CODE_OK then
      begin
        FResponse.Code := HTTP_CODE_BADGATEWAY;
        FResponse.Content := ErrorBody(Format(
          'Failed to fetch upstream authorization server metadata (HTTP %d)',
          [LResponse.StatusCode]));
        Exit;
      end;

      var LBody := LResponse.ContentAsString;
      if LBody.Length > 1024 * 1024 then
      begin
        FResponse.Code := HTTP_CODE_BADGATEWAY;
        FResponse.Content := ErrorBody('Upstream metadata document exceeds 1 MB size limit');
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
          FResponse.Code := HTTP_CODE_BADGATEWAY;
          FResponse.Content := ErrorBody(Format(
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
        LJSON.AddPair('issuer', FOAuthConfig.MetadataProxyUrl);

        FResponse.Code := HTTP_CODE_OK;
        FResponse.Content := LJSON.ToJSON;
        FProxyCache.Store(FResponse.Content);
      finally
        LJSON.Free;
      end;
    except
      on E: Exception do
      begin
        FResponse.Code := HTTP_CODE_BADGATEWAY;
        FResponse.Content := ErrorBody(E.Message);
      end;
    end;
  finally
    LHttp.Free;
  end;
end;

class function TMCPTransportHandler.MatchesOriginPattern(const AOrigin, APattern: string): Boolean;
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

function TMCPTransportHandler.CheckOrigin: Boolean;
var
  LOrigin, LHeader: string;
begin
  if FRequest.Protocol = TTransportProtocol.Stdio then
    Exit(True);

  if not Assigned(FMCPConfig) then
    raise EMCPException.Create(SErrorRetrievingMCPConfig);

  if Length(FMCPConfig.Security.AllowedOrigins) = 0 then
    Exit(True);

  LHeader := FRequest.GetHeader('Origin').Trim;

  // Reject requests with no Origin header, or the opaque "null" origin sent by
  // sandboxed iframes/file:// pages, once an allowlist has been configured.
  if LHeader.IsEmpty or SameText(LHeader, 'null') then
  begin
    Logger.LogWarning('CheckOrigin: request blocked, missing or null Origin header');
    Exit(False);
  end;

  for LOrigin in FMCPConfig.Security.AllowedOrigins do
    if MatchesOriginPattern(LHeader, LOrigin) then
      Exit(True);

  Logger.LogWarning('CheckOrigin: request blocked, Origin "%s" not in allowlist', [LHeader]);
  Result := False;
end;

procedure TMCPTransportHandler.ProcessRequest(
  ARequestConverter: TMCPTransportRequestConverter;
  AResponseConverter: TMCPTransportResponseConverter);
var
  LStopwatch, LFragment: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  try
    LFragment := TStopwatch.StartNew;
    ARequestConverter(FRequest);
    Logger.LogDebug('[PERF] RequestConverter: %d ms', [LFragment.ElapsedMilliseconds]);

    try try
    InjectCORS;

    if not CheckOrigin then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, SCrossOriginBlocked);

    if not CheckAuthorization then
      raise EMCPTransportException.Create(HTTP_CODE_FORBIDDEN, SAuthorizationCheckFailed);

    // Built before the OAuth check, and not after it, because the token validator
    // receives this context: it is where it finds the server and, through it, the
    // OAuth configuration. Requests that end in a 401 pay for a context they will
    // not use, which is a cheaper price than handing the validator a half-built one.
    FGarbage := TGarbageCollector.CreateInstance;
    FContext := TJRPCContext.Create;

    FGarbage.Add(FContext);
    FContext.AddContent(FGarbage);
    FContext.AddContent(FServer);
    FContext.AddContent(FAccessToken);

    if not CheckOAuth then
      Exit;

    LFragment := TStopwatch.StartNew;
    // Handle session (get existing or create new)
    FSession := HandleSession;
    Logger.LogDebug('[PERF] HandleSession: %d ms', [LFragment.ElapsedMilliseconds]);

    if Assigned(FSession) then
    begin
      // Add session to context if available
      FContext.AddContent(FSession);
      // Add session header
      case FSessionConfig.GetLocation of
        TSessionIdLocation.Header:
          FResponse.AddOrSetHeader(FSessionConfig.GetHeaderName, FSession.SessionId);
        TSessionIdLocation.Cookie:
          FResponse.SetCookie(FSessionConfig.GetHeaderName, FSession.SessionId, FMCPConfig.Security.CookieSecure);
        else
          raise EMCPTransportException.Create(500, SSessionIdLocationNotFound);
      end;

    end;

    LFragment := TStopwatch.StartNew;
    if FRequest.Command = 'GET' then
      HandleGET
    else if FRequest.Command = 'POST' then
      HandlePOST
    else if FRequest.Command = 'OPTIONS' then
      HandleOPTIONS
    else
      raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, SHttpMethodNotAllowed);
    Logger.LogDebug('[PERF] HandleCOMMAND: %d ms', [LFragment.ElapsedMilliseconds]);

  except
    on E: EMCPTransportException do
    begin
      FResponse.Code := E.Code;
      FResponse.ContentType := 'application/json';
      FResponse.Content := E.ToJSON;
    end;

    on E: EMCPSessionException do
    begin
      // Per MCP spec: an unknown or expired session ID gets HTTP 404, prompting
      // the client to re-initialize, rather than a generic 500.
      FResponse.Code := HTTP_CODE_NOTFOUND;
      FResponse.ContentType := 'application/json';
      FResponse.Content := E.ToJSON;
    end;

    on E: EJRPCException do
    begin
      FResponse.Code := 500;
      FResponse.ContentType := 'application/json';
      FResponse.Content := E.ToJSON;
    end;

    on E: Exception do
    begin
      FResponse.Code := 500;
      FResponse.ContentType := 'application/json';
      FResponse.Content := Format('{"message": "%s"}', [E.Message]);
    end;
  end;
  finally
    AResponseConverter(FResponse);
  end;
  finally
    Logger.LogDebug('[PERF] %s %s total: %d ms (HTTP: %d)', [FRequest.Command, FRequest.Url, LStopwatch.ElapsedMilliseconds, FResponse.Code]);
  end;
end;

procedure TMCPTransportHandler.SendResponseHeaders(AResponse: TMCPTransportResponse);
begin
  if Assigned(FSendResponseHeadersProc) then
    FSendResponseHeadersProc(AResponse);
end;

procedure TMCPTransportHandler.SetSendResponseHeadersProc(
  const Value: TProc<TMCPTransportResponse>);
begin
  FSendResponseHeadersProc := Value;
end;

function TMCPTransportHandler.ExtractSessionId: string;
begin
  Result := '';

  if not Assigned(FSessionConfig) then
    Exit;

  case FSessionConfig.GetLocation of
    TSessionIdLocation.Header:
      Result := FRequest.GetHeader(FSessionConfig.GetHeaderName);

    TSessionIdLocation.Cookie:
      Result := FRequest.GetCookie(FSessionConfig.GetHeaderName);
  end;

  Result := Result.Trim;
end;

function TMCPTransportHandler.IsInitializeRequest: Boolean;

  function MethodIsInitialize(AObj: TJSONObject): Boolean;
  var
    LMethod: TJSONValue;
  begin
    LMethod := AObj.GetValue('method');
    Result := Assigned(LMethod) and (LMethod is TJSONString) and (LMethod.Value = 'initialize');
  end;

begin
  // Per MCP spec, "initialize" is only ever sent as a POST request
  if (FRequest.Command <> 'POST') or FRequest.Content.Trim.IsEmpty then
    Exit(False);

  try
    FRequest.ContentJSON := TJSONObject.ParseJSONValue(FRequest.Content);
  except
    // Malformed JSON: let the regular request parsing report the error
    FRequest.ContentJSON := nil;
    Exit(False);
  end;

  if not Assigned(FRequest.ContentJSON) then
    Exit(False);

  if FRequest.ContentJSON is TJSONObject then
    Exit(MethodIsInitialize(TJSONObject(FRequest.ContentJSON)))
  else if FRequest.ContentJSON is TJSONArray then
    for var LItem in TJSONArray(FRequest.ContentJSON) do
      if (LItem is TJSONObject) and MethodIsInitialize(TJSONObject(LItem)) then
        Exit(True);

  Result := False;
end;

function TMCPTransportHandler.GetSendResponseHeadersProc: TProc<TMCPTransportResponse>;
begin
  Result := FSendResponseHeadersProc;
end;

function TMCPTransportHandler.CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
begin
  var LAsyncExecute := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        for var LMessage in ARequestList.List do
          HandleMessage(LMessage, AResponseQueue);
      finally
        AResponseQueue.Close;
      end;
    end
  );
  LAsyncExecute.FreeOnTerminate := False;
  LAsyncExecute.Start;
  Result := LAsyncExecute;
end;

procedure TMCPTransportHandler.HandleGET;
const
  QueueReadTimeout = 500;

  procedure ProcessQueue(AQueue: TMCPMessageQueue);
  begin
    AQueue.Process(
      procedure (AMessage: TJRPCMessage; var ADispose: Boolean)
      var
        LJson: string;
        LEventId: Int64;
      begin
        LJson := AMessage.ToJson;
        LEventId := FSession.RecordEvent(LJson);
        WriteSSEResponse(LJson, LEventId.ToString);
      end,
      QueueReadTimeout
    );
  end;

  // Replays events the client missed while disconnected, identified by the
  // "Last-Event-ID" header it sends back on reconnect (SSE resumption).
  procedure ReplayMissedEvents;
  var
    LHeader: string;
    LLastEventId: Int64;
    LEvent: TPair<Int64, string>;
  begin
    LHeader := FRequest.GetHeader('Last-Event-ID').Trim;
    if LHeader.IsEmpty then
      Exit;

    if not TryStrToInt64(LHeader, LLastEventId) then
    begin
      Logger.LogWarning('HandleGET: ignoring malformed Last-Event-ID "%s"', [LHeader]);
      Exit;
    end;

    for LEvent in FSession.GetEventsAfter(LLastEventId) do
      WriteSSEResponse(LEvent.Value, LEvent.Key.ToString);
  end;

begin
  if not FResponseWriter.SupportsStreaming then
    raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, STransportSSENotSupported);

  if not FRequest.AcceptsEventStream then
    raise EMCPTransportException.Create(HTTP_CODE_NOTALLOWED, SEventStreamOnlyForGET);

  // TODO: handle global messages
  if not Assigned(FSession) then
    raise EMCPTransportException.Create(HTTP_CODE_NOTACCEPTABLE, STransportSessionNotFound);

  FResponse.Code := HTTP_CODE_OK;
  FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;
  SendResponseHeaders(FResponse);

  ReplayMissedEvents;

  while FResponseWriter.Connected do
  begin
    ProcessQueue(FSession.Outbound);
  end;
end;

procedure TMCPTransportHandler.HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
var
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokerCtx: TJRPCInvokerContext;
begin
  if (AMessage is TJRPCNotification) then
  begin
    if Assigned(FSession) then
    begin
      var LNotification := AMessage as TJRPCNotification;
      Logger.LogDebug('Enqueing notification [%s]', [LNotification.Method]);
      FSession.Inbound.Enqueue(LNotification.Clone);
    end;
    Exit;
  end;

  if AMessage is TJRPCResponse then
  begin
    if Assigned(FSession) then
    begin
      var LRes := AMessage as TJRPCResponse;
      Logger.LogDebug('Enqueing response id [%s]', [LRes.Id.AsString]);
      FSession.Inbound.Enqueue(LRes.Clone);
    end;
    Exit;
  end;

  if AMessage is TJRPCError then
  begin
    var LErr := AMessage as TJRPCError;

    // If the error is in the JRPC request messages then process internally the error.
    if LErr.Request then
    begin
      if Assigned(FSession) then
      begin
        Logger.LogDebug('Enqueing error [%s]', [LErr.Error.Message.Value]);
        FSession.Inbound.Enqueue(LErr.Clone);
      end;
    end
    else
    begin
      // If the error was generated processing the request, clone the error object
      Logger.LogDebug('Error detected [%s]', [LErr.Error.Message.Value]);
      AResponseQueue.Enqueue(LErr.Clone);
    end;

    Exit;
  end;

  var LRequest := AMessage as TJRPCRequest;
  try
    Logger.LogDebug('Processing request [%s: %s]', [LRequest.Id.AsString, LRequest.Method]);

    FContext.AddContent(LRequest);

    var LMCPConfig := FContext.FindContextDataAs(IMCPConfig) as IMCPConfig;
    if Assigned(LMCPConfig) then
    begin
      if not LMCPConfig.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
        raise EJRPCMethodNotFoundError.CreateFmt(STransportMethodNotFoundFmt, [LRequest.Method]);
    end
    else if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt(STransportMethodNotFoundFmt, [LRequest.Method]);

    LInstance := LConstructorProxy.ConstructorFunc();
    FGarbage.Add(LInstance);

    // Injects the context inside the instance
    FContext.Inject(LInstance);

    LInvokerCtx.Garbage := FGarbage;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := AResponseQueue;
    LInvokerCtx.ApiInstance := LInstance;
    LInvokerCtx.SelectConfig(LConstructorProxy.NeonConfig, FContext.FindContextDataAs<TJRPCNeonConfig>);

    TJRPCInvoker.Invoke(LInvokerCtx);

    if (LRequest.Method = 'initialize') and Assigned(FSession) then
    begin
      if FSessionConfig.GetLocation = TSessionIdLocation.Header then
        FResponse.Headers.AddOrSetValue(FSessionConfig.GetHeaderName, FSession.SessionId)
      else if FSessionConfig.GetLocation = TSessionIdLocation.Cookie then
        FResponse.SetCookie(FSessionConfig.GetHeaderName, FSession.SessionId, FMCPConfig.Security.CookieSecure);
    end;

  except
    on E: Exception do
    begin
      Logger.LogError('TMCPTransportHandler.HandleMessage %s: %s', [E.ClassName, E.Message]);
      var err := TJRPCInvoker.HandleError(E, LRequest.Id);
      AResponseQueue.Enqueue(err);
    end;
  end;

end;

procedure TMCPTransportHandler.HandleOPTIONS;
begin
  FResponse.Code := HTTP_CODE_NOCONTENT;
  FResponse.Content := '';
end;

procedure TMCPTransportHandler.HandlePOST;
const
  QueueReadTimeout = 500;
var
  LResponseList: TJRPCMessages;

  procedure ProcessQueue(AResponseList: TMCPMessageQueue);
  begin
    AResponseList.Process(
      procedure (AMessage: TJRPCMessage; var ADispose: Boolean)
      begin
        if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
        begin
          var LJson := AMessage.ToJson;
          if Assigned(FSession) then
            WriteSSEResponse(LJson, FSession.RecordEvent(LJson).ToString)
          else
            WriteSSEResponse(LJson);
        end
        else
        begin
          ADispose := False;
          if AMessage is TJRPCNotification then
          begin
            // TODO: should I add the message to FSession.Outbound also if SSE is not supported?
            if Assigned(FSession) then
              FSession.Outbound.Enqueue(AMessage)
            else
              ADispose := True;
          end
          else
          begin
            LResponseList.AddMessage(AMessage);
          end;
        end;
      end,
      QueueReadTimeout
    );
  end;
var
  LRequestList: TJRPCMessages;
  LFragment: TStopwatch;
begin
  LFragment := TStopwatch.StartNew;
  if Assigned(FRequest.ContentJSON) then
    LRequestList := TJRPCMessages.CreateFromJson(FRequest.ContentJSON)
  else
    LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  Logger.LogDebug('[PERF] CreateFromJSON total: %d ms', [LFragment.ElapsedMilliseconds]);

  FGarbage.Add(LRequestList);

  var LResponseQueue := TMCPMessageQueue.Create;
  FGarbage.Add(LResponseQueue);
  FContext.AddContent(LResponseQueue);

  // This list contains the responses in case SSE channel is not active
  LResponseList := TJRPCMessages.Create(True);
  FGarbage.Add(LResponseList);

  LFragment := TStopwatch.StartNew;
  var LAsyncExecute := CreateAsyncThread(LRequestList, LResponseQueue);
  try
    if FRequest.AcceptsEventStream and FResponseWriter.SupportsStreaming then
    begin
      FResponse.Code := 200;
      FResponse.ContentType := TMediaType.TEXT_EVENT_STREAM;
      SendResponseHeaders(FResponse);
    end;

    // The worker thread closes the queue when done, which wakes ProcessQueue
    // immediately: on the happy path no read timeout is ever paid. The loop is
    // still needed because a slow tool can let the timeout expire before
    // producing anything, and the final drain catches messages enqueued between
    // the last timeout and the Closed check.
    while not LResponseQueue.Closed do
    begin
      ProcessQueue(LResponseQueue);
    end;
    ProcessQueue(LResponseQueue);

    // If not an event stream response send all the headers and content
    if not FRequest.AcceptsEventStream or not FResponseWriter.SupportsStreaming then
    begin
      if LResponseList.Count = 0 then
        FResponse.Code := HTTP_CODE_ACCEPTED
      else
        FResponse.Code := HTTP_CODE_OK;
      FResponse.ContentType := TMediaType.APPLICATION_JSON;
      FResponse.Content := LResponseList.ToJson;
    end;
  finally
    LAsyncExecute.Free;
  end;
  Logger.LogDebug('[PERF] CreateAsyncQueue total: %d ms', [LFragment.ElapsedMilliseconds]);

end;

function TMCPTransportHandler.HandleSession: TMCPSessionBase;
var
  LSessionId: string;
  LSessionManager: TMCPSessionManager;
begin
  if not Assigned(FSessionConfig) or (not FSessionConfig.IsApplied) then
    Exit(nil);

  LSessionId := ExtractSessionId;
  LSessionManager := (FServer.SessionManager as TMCPSessionManager);

  // If session ID is provided, try to get existing session
  if not LSessionId.IsEmpty then
  begin
    // GetSession will raise exception if expired or not found
    Result := LSessionManager.GetSession(LSessionId);
  end
  else if IsInitializeRequest then
  begin
    // No session ID provided - only "initialize" may auto-create a new session
    Result := LSessionManager.CreateSession;
  end
  else
    raise EMCPTransportException.Create(HTTP_CODE_BADREQUEST, SSessionIdHeaderRequired);
end;

procedure TMCPTransportHandler.InjectCORS;
var
  LHValue: string;
begin
  if not FMCPConfig.Security.CORS then
    Exit;

  // Set the allowed origins (from security configuration)
  LHValue := FRequest.GetHeader('Origin');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Origin', LHValue);

  // Set the allowed methods supported by the server (from security configuration)
  LHValue := FRequest.GetHeader('Access-Control-Request-Method');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Methods', string.Join(',', FMCPConfig.Security.AllowedMethods));

  // Set the allowed headers as requested
  LHValue := FRequest.GetHeader('Access-Control-Request-Headers');
  if not LHValue.IsEmpty then
    FResponse.Headers.AddOrSetValue('Access-Control-Allow-Headers', LHValue);

  // Expose the headers browser-based clients need to read from JS (e.g. to
  // discover the OAuth resource metadata URL from a 401 response, or to pick
  // up the session id when it is returned via header).
  LHValue := 'WWW-Authenticate';
  if Assigned(FSessionConfig) and FSessionConfig.IsApplied and (FSessionConfig.GetLocation = TSessionIdLocation.Header) then
    LHValue := LHValue + ', ' + FSessionConfig.GetHeaderName;
  FResponse.Headers.AddOrSetValue('Access-Control-Expose-Headers', LHValue);

end;

{ TMCPTransportResponse }

function TMCPTransportResponse.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

procedure TMCPTransportResponse.SetContentType(const AValue: string);
begin
  Headers.AddOrSetValue('Content-Type', AValue);
end;

procedure TMCPTransportResponse.SetCookie(const AName, AValue: string; ASecure: Boolean);
var
  LCookie: string;
begin
  // HttpOnly: not readable from JS (mitigates session-id theft via XSS)
  // SameSite=Strict: never sent on cross-site requests (mitigates CSRF)
  // Secure: HTTPS-only transmission; opt out via Security.SetCookieSecure(False) for plain-HTTP/dev setups
  LCookie := Format('%s=%s; Path=/; HttpOnly; SameSite=Strict', [AName, AValue]);
  if ASecure then
    LCookie := LCookie + '; Secure';

  Headers.AddOrSetValue('Set-Cookie', LCookie);
end;

{ TMCPTransportRequest }

constructor TMCPTransportRequest.Create;
begin
  inherited Create;
  FAcceptItems := nil;
end;

destructor TMCPTransportRequest.Destroy;
begin
  FAcceptItems.Free;
  ContentJSON.Free;
  inherited;
end;

function TMCPTransportRequest.GetAccept: string;
begin
  Result := GetHeader('Accept');
end;

function TMCPTransportRequest.GetAcceptItems: TAcceptItemList<TAcceptItem>;
begin
  if not Assigned(FAcceptItems) then
  begin
    FAcceptItems := TAcceptItemList<TAcceptItem>.Create;
    var LAcceptHeader := GetHeader('Accept');
    TAcceptHeaderParser<TAcceptItem>.Parse(LAcceptHeader, FAcceptItems);
  end;
  Result := FAcceptItems;
end;

function TMCPTransportRequest.GetAcceptsEventStream: Boolean;
begin
  Result := AcceptItems.Contains(TMediaType.TEXT_EVENT_STREAM);
end;

function TMCPTransportRequest.GetCookie(const AName: string): string;
begin
  var LCookies := GetHeader('Cookie');
  if LCookies.IsEmpty then
    Exit('');

  var LCookieList := TStringList.Create;
  try
    LCookieList.NameValueSeparator := '=';
    LCookieList.LineBreak := ';';
    LCookieList.Text := LCookies;

    Result := LCookieList.Values[AName];
  finally
    LCookieList.Free;
  end;

end;

procedure TMCPTransportRequest.SetAccept(const AValue: string);
begin
  FreeAndNil(FAcceptItems);
  Headers.AddOrSetValue('Accept', AValue);
end;

procedure TMCPTransportHeaders.AddOrSetHeader(const AName, AValue: string);
begin
  Headers.AddOrSetValue(AName, AValue);
end;

constructor TMCPTransportHeaders.Create;
begin
  // Case-insensitive <string,string> dictionary
  Headers := THeaders.Create(TIStringComparer.Ordinal);
end;

destructor TMCPTransportHeaders.Destroy;
begin
  Headers.Free;
  inherited;
end;

function TMCPTransportHeaders.GetHeader(const AName: string): string;
begin
  if not Headers.TryGetValue(AName, Result) then
    Result := '';
end;

{ EMCPTransportException }

constructor EMCPTransportException.Create(ACode: Integer; const AMsg: string);
begin
  inherited Create(AMsg);
  FCode := ACode;
end;

function EMCPTransportException.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('code', Self.Code);
    LJSON.AddPair('class', Self.ClassName);
    LJSON.AddPair('message', Self.Message);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

end.
