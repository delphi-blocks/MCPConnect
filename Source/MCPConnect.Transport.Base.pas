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

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  IdCustomHTTPServer, IdContext, IdGlobal,

  JRPC.Core,
  JRPC.Classes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Transport.AcceptParser,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Security.Token,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Server;

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
  SHttpMethodNotAllowed = 'Http method not allowed';
  STransportMethodNotFoundFmt = 'Method "%s" not found';
  SDuplicateAuthorizationHeader = 'Multiple Authorization headers are not allowed';
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
  private
  type
    THeaders = class(TList<TPair<string, string>>)
    end;
  private
    FHeaders: THeaders;
  public

    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator<TPair<string, string>>; inline;

    procedure Clear;
    // Returns the index of the first header matching AName (case-insensitive), or -1
    function IndexOf(const AName: string): Integer;
    // Returns the value of the first matching header, or '' if not found
    function Get(const AName: string): string; virtual;
    // Returns all values for headers matching AName (case-insensitive)
    function GetHeaders(const AName: string): TArray<string>;
    // Replaces the first matching header or adds a new one (single-value semantics)
    procedure &Set(const AName, AValue: string);
    // Appends a header without removing duplicates (use for Set-Cookie, etc.)
    procedure Add(const AName, AValue: string); virtual;
    // Removes all headers matching AName (case-insensitive); returns the number removed
    function RemoveHeader(const AName: string): Integer;

    function Count: Integer; inline;
  end;

  TMCPRequestHeaders = class(TMCPTransportHeaders)
  public
    // RFC 6750 §3.1: reject requests with multiple Authorization headers
    procedure Add(const AName, AValue: string); override;
  end;

  TMCPTransportRequest = class(TObject)
  private
    FAcceptItems: TAcceptItemList<TAcceptItem>;
    FProtocol: TTransportProtocol;
    FHeaders: TMCPRequestHeaders;
    function GetAccept: string;
    procedure SetAccept(const AValue: string);
    function GetAcceptItems: TAcceptItemList<TAcceptItem>;
    function GetAcceptsEventStream: Boolean;
    function GetAuthorization: string;
    function GetOrigin: string;
  public
    Url: string;
    Command: string;
    Content: string;
    ContentJSON: TJSONValue;

    function GetHeader(const AName: string): string;
    procedure SetHeader(const AName, AValue: string);

    function GetCookie(const AName: string): string;
    property Accept: string read GetAccept write SetAccept;
    property AcceptItems: TAcceptItemList<TAcceptItem> read GetAcceptItems;
    property AcceptsEventStream: Boolean read GetAcceptsEventStream;
    property Authorization: string read GetAuthorization;
    property Origin: string read GetOrigin;
    property Protocol: TTransportProtocol read FProtocol write FProtocol;
    property Headers: TMCPRequestHeaders read FHeaders;

    constructor Create;
    destructor Destroy; override;
  end;

  TMCPTransportRequestConverter = reference to procedure (ARequest: TMCPTransportRequest);

  TMCPResponseHeaders = class(TMCPTransportHeaders)
  end;

  TMCPTransportResponse = class(TObject)
  private
    FHeaders: TMCPResponseHeaders;
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
    function GetTransferEncoding: string;
    procedure SetTransferEncoding(const AValue: string);
  public
    Content: string;
    Code: Integer;
    Outbund: TQueue<string>;

    procedure SetCookie(const AName, AValue: string; ASecure: Boolean = True);
    procedure ClearCookies();

    function GetHeader(const AName: string): string;
    procedure SetHeader(const AName, AValue: string);

    property Headers: TMCPResponseHeaders read FHeaders;
    property ContentType: string read GetContentType write SetContentType;
    property TransferEncoding: string read GetTransferEncoding write SetTransferEncoding;

    constructor Create;
    destructor Destroy; override;
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
    FAccessToken: TMCPAccessToken;

    FMCPConfig: TMCPConfig;
    FServer: TMCPServer;
    FAuthTokenConfig: TAuthTokenConfig;
    FOAuthConfig: TOAuthConfig;
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
    function SelectNeonConfig(const AProxy: TJRPCConstructorProxy): INeonConfiguration;
    procedure HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
    procedure SendResponseHeaders(AResponse: TMCPTransportResponse);
    procedure WriteSSEResponse(const AValue: string; const AEventId: string = '');

    procedure HandlePOST;
    procedure HandleOPTIONS;
    function CreateAsyncThread(ARequestList: TJRPCMessages; AResponseQueue: TMCPMessageQueue): TThread;
  public
    constructor Create(AServer: TMCPServer; AResponseWriter: IMCPTransportWriter);
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
  JRPC.Invoker,
  Neon.Core.Utils,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.Neon;


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

constructor TMCPTransportHandler.Create(AServer: TMCPServer; AResponseWriter: IMCPTransportWriter);
begin
  FRequest := TMCPTransportRequest.Create;
  FResponse := TMCPTransportResponse.Create;
  FAccessToken := TMCPAccessToken.Create;

  FServer := AServer;
  FResponseWriter := AResponseWriter;
  FMCPConfig := FServer.GetConfiguration<TMCPConfig>;
  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
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
        var LAuthHeader := FRequest.Authorization;
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
    var LAuthHeader := FRequest.Authorization;
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
    Logger.LogDebug('[SSE] Event Sent [id=%s, size=%d]', [AEventId, Length(AValue)]);
    {$IFDEF FULL_PAYLOAD_LOGGING}
    Logger.LogTrace('[SSE] data: %s', [AValue]);
    {$ENDIF}
    FResponseWriter.Write(AValue, AEventId);
  end;
end;

procedure TMCPTransportHandler.SendUnauthorized(const AResult: TTokenValidationResult);
begin
  FResponse.Code := HTTP_CODE_UNAUTHORIZED;
  FResponse.SetHeader('WWW-Authenticate',
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

  LHeader := FRequest.Origin.Trim;

  if LHeader.IsEmpty then
  begin
    if FMCPConfig.Security.RequireOrigin then
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
    {$IFDEF FULL_PAYLOAD_LOGGING}
    Logger.LogTrace('[REQ] %s', [FRequest.Content]);
    {$ENDIF}
    Logger.LogDebug('[PERF] RequestConverter: %d ms', [LFragment.ElapsedMilliseconds]);

    try
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

      // Adding the server is not enough: the context is keyed by exact class, so
      // every configuration has to go in under its own class for [Context]
      // injection and FindContextDataAs to see it. Missing one is not a nil
      // field - TContextManager.Inject resolves through GetContextDataAs, which
      // raises - so this is what makes TMCPConfig, TOAuthConfig, TAuthTokenConfig
      // and TJRPCNeonConfig reachable from an API class or a token validator.
      //
      // The JRPC library leaves the expansion to its host: TJRPCContext ships an
      // AddConfigurations hook, empty because a standalone JRPC server has no
      // configuration system. Subclassing the context to fill it in is not an
      // option here - the hook is not virtual, and a descendant would register
      // *itself* under the descendant's class, which would break the
      // [Context] TJRPCContext field every MCP API class declares.
      for var LConfig in FServer.GetConfigurations do
        FContext.AddContent(LConfig);

      FContext.AddContent(FAccessToken);

      if not CheckOAuth then
        Exit;

      LFragment := TStopwatch.StartNew;
      // GET is deliberately absent: it existed only to open the server-to-client
      // SSE stream, which went with session management. It now falls through to
      // the 405 below like any other verb the server does not implement.
      if FRequest.Command = 'POST' then
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
    try
      AResponseConverter(FResponse);
    finally
      Logger.LogDebug('[PERF] %s %s total: %d ms (HTTP: %d)', [FRequest.Command, FRequest.Url, LStopwatch.ElapsedMilliseconds, FResponse.Code]);
      {$IFDEF FULL_PAYLOAD_LOGGING}
      Logger.LogTrace('[RES] %s', [FResponse.Content]);
      {$ENDIF}
    end;
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

function TMCPTransportHandler.SelectNeonConfig(const AProxy: TJRPCConstructorProxy): INeonConfiguration;
var
  LJRPCNeonConfig: TJRPCNeonConfig;
begin
  // Precedence: the configuration the API class was registered with - for every
  // built-in MCP namespace that is MCPNeonConfig, whose camelCase naming and
  // SetMembers([Fields]) are what make an MCP entity serialize to anything at
  // all - then the application-wide IJRPCNeonConfig, then Neon's default, which
  // TJRPCInvokerContext.SelectConfig applies when this returns nil.
  //
  // The middle tier is why this lives here rather than being passed straight in:
  // the JRPC library knows nothing of the plugin configuration system, so its
  // SelectConfig takes the API-level configuration alone.
  Result := AProxy.NeonConfig;
  if Assigned(Result) then
    Exit;

  LJRPCNeonConfig := FContext.FindContextDataAs<TJRPCNeonConfig>;
  if Assigned(LJRPCNeonConfig) then
    Result := LJRPCNeonConfig.NeonConfig;
end;

procedure TMCPTransportHandler.HandleMessage(AMessage: TJRPCMessage; AResponseQueue: TMCPMessageQueue);
var
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokerCtx: TJRPCInvokerContext;
begin
  // Every message that is not a Request is dealt with here and here only: the
  // cast below is unguarded, so anything reaching it that is not a TJRPCRequest
  // raises EInvalidCast outside the try, and the client is told 500.

  // A notification is fire-and-forget, and the specification forbids answering
  // one at all. There is no longer an inbound session queue to route it to, so
  // it is accepted and dropped.
  if AMessage is TJRPCNotification then
  begin
    Logger.LogDebug('Discarding notification [%s]', [TJRPCNotification(AMessage).Method]);
    Exit;
  end;

  // A Response is an answer to a request this server sent. Correlating one
  // needed the session that carried the original request, so there is nothing
  // left to match it against.
  if AMessage is TJRPCResponse then
  begin
    Logger.LogDebug('Discarding response id [%s]', [TJRPCResponse(AMessage).Id.AsString]);
    Exit;
  end;

  if AMessage is TJRPCError then
  begin
    var LErr := AMessage as TJRPCError;

    // Request=True marks a message the server must not answer: an Error object
    // the client itself sent, or a notification that failed to parse. Anything
    // else is an error the parser produced for a message that IS waiting for a
    // reply - a malformed element of a batch - and it has to reach the client.
    if LErr.Request then
      Logger.LogDebug('Discarding error [%s]', [LErr.Error.Message.Value])
    else
    begin
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

    // The invoker appends what it produces to a TJRPCMessages, while this
    // transport hands responses to a TMCPMessageQueue that the SSE writer drains
    // while the worker thread is still running. The two are bridged here.
    //
    // The scratch list owns what the invoker builds, so an exception raised
    // before the hand-over below still frees it; from then on ownership moves to
    // the queue one message at a time, which is the only owner Process/Destroy
    // knows about.
    var LInvokerResponses := TJRPCMessages.Create(True);
    try
      LInvokerCtx.Garbage := FGarbage;
      LInvokerCtx.Request := LRequest;
      LInvokerCtx.Responses := LInvokerResponses;
      LInvokerCtx.ApiInstance := LInstance;
      LInvokerCtx.SelectConfig(SelectNeonConfig(LConstructorProxy));

      TJRPCInvoker.Invoke(LInvokerCtx);

      while LInvokerResponses.Count > 0 do
        AResponseQueue.Enqueue(LInvokerResponses.List.Extract(LInvokerResponses.List[0]));
    finally
      LInvokerResponses.Free;
    end;

  except
    on E: Exception do
    begin
      Logger.LogError(E, Format('TMCPTransportHandler.HandleMessage %s: %s', [E.ClassName, E.Message]));
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
          WriteSSEResponse(AMessage.ToJson);
        end
        else
        begin
          ADispose := False;
          LResponseList.AddMessage(AMessage);
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
  try
    if Assigned(FRequest.ContentJSON) then
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.ContentJSON)
    else
      LRequestList := TJRPCMessages.CreateFromJson(FRequest.Content);
  except
    on E: EJRPCException do
    begin
      // Per JSON-RPC 2.0, malformed JSON (parse error), an empty batch, or a
      // top-level value that is neither a Request nor a batch must be answered
      // with a single JSON-RPC error response carrying a null id - never an
      // HTTP 500 or an empty body.
      var LErrorId: TJRPCID;
      LRequestList := TJRPCMessages.Create(True);
      LRequestList.AddMessage(TJRPCError.CreateFromException(E, LErrorId));
    end;
  end;
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

procedure TMCPTransportHandler.InjectCORS;
var
  LHValue: string;
begin
  if not FMCPConfig.Security.CORS then
    Exit;

  // Set the allowed origins (from security configuration)
  LHValue := FRequest.Origin;
  if LHValue.IsEmpty then
    Exit;

  FResponse.SetHeader('Access-Control-Allow-Origin', LHValue);

  // Set the allowed methods supported by the server (from security configuration)
  LHValue := FRequest.GetHeader('Access-Control-Request-Method');
  if not LHValue.IsEmpty then
    FResponse.SetHeader('Access-Control-Allow-Methods', string.Join(',', FMCPConfig.Security.AllowedMethods));

  // Set the allowed headers as requested
  LHValue := FRequest.GetHeader('Access-Control-Request-Headers');
  if not LHValue.IsEmpty then
    FResponse.SetHeader('Access-Control-Allow-Headers', LHValue);

  // Expose the headers browser-based clients need to read from JS - notably to
  // discover the OAuth resource metadata URL from a 401 response.
  if Length(FMCPConfig.Security.ExposeHeaders) > 0 then
    LHValue := string.Join(', ', FMCPConfig.Security.ExposeHeaders)
  else
    LHValue := 'WWW-Authenticate';
  FResponse.SetHeader('Access-Control-Expose-Headers', LHValue);
end;

{ TMCPTransportResponse }

constructor TMCPTransportResponse.Create;
begin
  FHeaders := TMCPResponseHeaders.Create;
end;

destructor TMCPTransportResponse.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TMCPTransportResponse.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

function TMCPTransportResponse.GetHeader(const AName: string): string;
begin
  Result := FHeaders.Get(AName);
end;

procedure TMCPTransportResponse.SetContentType(const AValue: string);
begin
  SetHeader('Content-Type', AValue);
end;

function TMCPTransportResponse.GetTransferEncoding: string;
begin
  Result := GetHeader('Transfer-Encoding');
end;

procedure TMCPTransportResponse.SetTransferEncoding(const AValue: string);
begin
  SetHeader('Transfer-Encoding', AValue);
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

  FHeaders.Add('Set-Cookie', LCookie);
end;

procedure TMCPTransportResponse.SetHeader(const AName, AValue: string);
begin
  FHeaders.&Set(AName, AValue);
end;

procedure TMCPTransportResponse.ClearCookies();
begin
  FHeaders.RemoveHeader('Set-Cookie');
end;

{ TMCPTransportRequest }

constructor TMCPTransportRequest.Create;
begin
  inherited Create;
  FAcceptItems := nil;
  FHeaders := TMCPRequestHeaders.Create;
end;

destructor TMCPTransportRequest.Destroy;
begin
  FAcceptItems.Free;
  ContentJSON.Free;
  FHeaders.Free;
  inherited;
end;

function TMCPTransportRequest.GetAccept: string;
begin
  Result := GetHeader('Accept');
end;

function TMCPTransportRequest.GetAuthorization: string;
begin
  Result := GetHeader('Authorization');
end;

function TMCPTransportRequest.GetOrigin: string;
begin
  Result := GetHeader('Origin');
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

function TMCPTransportRequest.GetHeader(const AName: string): string;
begin
  Result := FHeaders.Get(AName);
end;

procedure TMCPTransportRequest.SetAccept(const AValue: string);
begin
  FreeAndNil(FAcceptItems);
  SetHeader('Accept', AValue);
end;

procedure TMCPTransportRequest.SetHeader(const AName, AValue: string);
begin
  FHeaders.&Set(AName, AValue);
end;

procedure TMCPTransportHeaders.Add(const AName, AValue: string);
begin
  FHeaders.Add(TPair<string, string>.Create(AName, AValue));
end;

{ TMCPRequestHeaders }

procedure TMCPRequestHeaders.Add(const AName, AValue: string);
begin
  if SameText(AName, 'Authorization') and (IndexOf('Authorization') >= 0) then
    raise EMCPTransportException.Create(HTTP_CODE_BADREQUEST, SDuplicateAuthorizationHeader);
  inherited Add(AName, AValue);
end;

procedure TMCPTransportHeaders.&Set(const AName, AValue: string);
begin
  RemoveHeader(AName);
  FHeaders.Add(TPair<string, string>.Create(AName, AValue));
end;

function TMCPTransportHeaders.RemoveHeader(const AName: string): Integer;
begin
  Result := 0;
  for var I := FHeaders.Count - 1 downto 0 do
  begin
    if SameText(FHeaders[I].Key, AName) then
    begin
      FHeaders.Delete(I);
      Inc(Result);
    end;
  end;
end;

procedure TMCPTransportHeaders.Clear;
begin
  FHeaders.Clear;
end;

function TMCPTransportHeaders.Count: Integer;
begin
  Result := FHeaders.Count;
end;

constructor TMCPTransportHeaders.Create;
begin
  FHeaders := THeaders.Create;
end;

destructor TMCPTransportHeaders.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TMCPTransportHeaders.Get(const AName: string): string;
begin
  Result := '';
  var I := IndexOf(AName);
  if I >= 0 then
    Result := FHeaders[I].Value;
end;

function TMCPTransportHeaders.GetEnumerator: TEnumerator<TPair<string, string>>;
begin
  Result := FHeaders.GetEnumerator;
end;

function TMCPTransportHeaders.GetHeaders(const AName: string): TArray<string>;
begin
  Result := [];
  for var I := 0 to FHeaders.Count - 1 do
  begin
    if SameText(FHeaders[I].Key, AName) then
      Result := Result + [FHeaders[I].Value];
  end;
end;

function TMCPTransportHeaders.IndexOf(const AName: string): Integer;
begin
  Result := -1;
  for var I := 0 to FHeaders.Count - 1 do
  begin
    if SameText(FHeaders[I].Key, AName) then
      Exit(I);
  end;
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
