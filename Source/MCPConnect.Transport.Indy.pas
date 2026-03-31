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
unit MCPConnect.Transport.Indy;

interface

uses
  System.Classes, System.SysUtils,
  IdCustomHTTPServer, IdContext, IdGlobal,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Core.Utils,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

const
  HTTP_CODE_OK = 200;
  HTTP_CODE_ACCEPTED = 202;
  HTTP_CODE_NOCONTENT = 204;
  HTTP_CODE_UNAUTHORIZED = 401;
  HTTP_CODE_FORBIDDEN = 403;
  HTTP_CODE_NOTFOUND = 404;
  HTTP_CODE_NOTALLOWED = 405;
  HTTP_CODE_NOTACCEPTABLE = 406;

type

  TJRPCIndyBridge = class(TComponent)
  private type
    TMsgContext = record
      GC: IGarbageCollector;
      JRPCCtx: TJRPCContext;
      Session: TMCPSessionBase;
      Request: TJRPCMessage;
      Responses: TJRPCMessages;
    end;
  private
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    FSessionConfig: TSessionConfig;

    procedure SetServer(const Value: TJRPCServer);
    function CheckAuthorization(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
    function ExtractSessionId(ARequestInfo: TIdHTTPRequestInfo): string;
    function HandleSession(ARequestInfo: TIdHTTPRequestInfo; out ASessionCreated: Boolean): TMCPSessionBase;

    procedure HandleMessage(AMsgContext: TMsgContext);

    procedure HandleRequestGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleRequestPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleRequestOPTIONS(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  public
    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    property Server: TJRPCServer read FServer write SetServer;
  end;

  TJRPCIndyServer = class(TIdCustomHTTPServer)
  private
    FServer: TJRPCServer;
    FBridge: TJRPCIndyBridge;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    procedure SetServer(const Value: TJRPCServer);
  public
    property Server: TJRPCServer read FServer write SetServer;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils,
  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Invoker;

function TJRPCIndyBridge.CheckAuthorization(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if ARequestInfo.RawHeaders.Values['Authorization'] <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if ARequestInfo.Cookies.Cookie[FAuthTokenConfig.CustomHeader, ''].CookieName <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if ARequestInfo.RawHeaders.Values[FAuthTokenConfig.CustomHeader] <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      else
        raise EJRPCException.Create('Invalid token location');
    end;
  end;
end;

procedure TJRPCIndyBridge.HandleMessage(AMsgContext: TMsgContext);
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;

  LInvokerCtx: TJRPCInvokerContext;
begin
  if AMsgContext.Request is TJRPCNotification then
  begin
    { TODO -opaolo -c : Enqueue the notification 28/03/2026 15:49:20 }
    Exit;
  end;

  if AMsgContext.Request is TJRPCResponse then
  begin
    { TODO -opaolo -c : Enqueue the response 28/03/2026 15:49:20 }
    Exit;
  end;

  if AMsgContext.Request is TJRPCError then
  begin
    var LOriginal := AMsgContext.Request as TJRPCError;

    // If the error is in the JRPC request messages then process internally the error.
    if LOriginal.Request then
    begin
      { TODO -opaolo -c : Enqueue the error 28/03/2026 15:49:20 }
    end
    else
    begin
      // If the error was generated processing the request, clone the error object
      AMsgContext.Responses.AddMessage(LOriginal.Clone);
    end;
    Exit;
  end;

  LRequest := AMsgContext.Request as TJRPCRequest;
  try
    AMsgContext.JRPCCtx.AddContent(LRequest);

    if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LRequest.Method]);

    LInstance := LConstructorProxy.ConstructorFunc();
    AMsgContext.GC.Add(LInstance);

    // Injects the context inside the instance
    AMsgContext.JRPCCtx.Inject(LInstance);

    LInvokerCtx.GC := AMsgContext.GC;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := AMsgContext.Responses;
    LInvokerCtx.ApiInstance := LInstance;
    LInvokerCtx.SelectConfig(LConstructorProxy.NeonConfig, AMsgContext.JRPCCtx.FindContextDataAs<TJRPCNeonConfig>);

    TJRPCInvoker.Invoke(LInvokerCtx);
  except
    on E: Exception do
    begin
      var err := TJRPCInvoker.HandleError(E, LRequest.Id);
      AMsgContext.Responses.AddMessage(err);
    end;
  end;
end;

procedure TJRPCIndyBridge.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  try
    if not Assigned(FServer) then
      raise EJRPCException.Create('Server not found');

    if not CheckAuthorization(AContext, ARequestInfo, AResponseInfo) then
    begin
      AResponseInfo.ResponseNo := HTTP_CODE_FORBIDDEN;
      AResponseInfo.ContentText := '';
      Exit;
    end;

    case ARequestInfo.CommandType of
      hcGET:    HandleRequestGET(AContext, ARequestInfo, AResponseInfo);
      hcPOST:   HandleRequestPOST(AContext, ARequestInfo, AResponseInfo);
      hcOPTION: HandleRequestOPTIONS(AContext, ARequestInfo, AResponseInfo);
    else
      AResponseInfo.ResponseNo := HTTP_CODE_NOTALLOWED;
    end;
  except
    on E: EJRPCException do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := E.ToJSON;
    end;

    on E: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentType := 'application/json';
      AResponseInfo.ContentText := Format('{"message": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TJRPCIndyBridge.HandleRequestGET(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ResponseNo := HTTP_CODE_NOTALLOWED;
end;

procedure TJRPCIndyBridge.HandleRequestOPTIONS(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ResponseNo := HTTP_CODE_ACCEPTED;
  AResponseInfo.ContentText := '';
end;

procedure TJRPCIndyBridge.HandleRequestPOST(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LGarbage: IGarbageCollector;
  LContext: TJRPCContext;
  LCtx: TMsgContext;
  LEncoding: IIdTextEncoding;
  LRequestContent: string;
  LSession: TMCPSessionBase;
  LSessionCreated: Boolean;

  LRequestList, LResponseList: TJRPCMessages;
  LRequest: TJRPCMessage;
begin
  // HttpRequest:  Headers, PostStream
  // HttpResponse: ContentType, Content, StatusCode


  LSessionCreated := False;
  LGarbage := TGarbageCollector.CreateInstance;
  LContext := TJRPCContext.Create;

  LGarbage.Add(LContext);
  LContext.AddContent(LGarbage);
  LContext.AddContent(FServer);

  // Handle session (get existing or create new)
  LSession := HandleSession(ARequestInfo, LSessionCreated);

  // Add session to context if available
  if Assigned(LSession) then
    LContext.AddContent(LSession);

  if ARequestInfo.CharSet <> '' then
    LEncoding := IndyTextEncoding(ARequestInfo.CharSet)
  else
    LEncoding := IndyTextEncoding_UTF8;

  ARequestInfo.PostStream.Position := 0;
  LRequestContent := ReadStringFromStream(ARequestInfo.PostStream, -1, LEncoding);

  LRequestList := TJRPCMessages.CreateFromJson(LRequestContent);
  LGarbage.Add(LRequestList);

  LResponseList := TJRPCMessages.Create;
  LGarbage.Add(LResponseList);

  for LRequest in LRequestList.List do
  begin
    LCtx.GC := LGarbage;
    LCtx.JRPCCtx := LContext;
    LCtx.Session := LSession;
    LCtx.Request := LRequest;
    LCtx.Responses := LResponseList;

    { TODO -opaolo -c : Use directly JRPCContect! 28/03/2026 17:45:11 }
    HandleMessage(LCtx);
  end;

  { TODO -opaolo -c : To change based on SSE vs JSON reqs 28/03/2026 17:45:47 }
  AResponseInfo.ContentType := 'application/json';
  AResponseInfo.ContentText := LResponseList.ToJson;

  if LResponseList.Count = 0 then
    AResponseInfo.ResponseNo := HTTP_CODE_ACCEPTED
  else
    AResponseInfo.ResponseNo := HTTP_CODE_OK;

end;

procedure TJRPCIndyBridge.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;

  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
  FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
end;

function TJRPCIndyBridge.ExtractSessionId(ARequestInfo: TIdHTTPRequestInfo): string;
begin
  Result := '';

  if not Assigned(FSessionConfig) then
    Exit;

  case FSessionConfig.GetLocation of
    TSessionIdLocation.Header:
      Result := ARequestInfo.RawHeaders.Values[FSessionConfig.GetHeaderName];

    TSessionIdLocation.Cookie:
      Result := ARequestInfo.Cookies.Cookie[FSessionConfig.GetHeaderName, ''].Value;
  end;

  Result := Result.Trim;
end;

function TJRPCIndyBridge.HandleSession(ARequestInfo: TIdHTTPRequestInfo;
  out ASessionCreated: Boolean): TMCPSessionBase;
var
  LSessionId: string;
begin
  Result := nil;
  ASessionCreated := False;

  if not Assigned(FSessionConfig) or (not FSessionConfig.IsApplied) then
    Exit;

  LSessionId := ExtractSessionId(ARequestInfo);

  // If session ID is provided, try to get existing session
  if not LSessionId.IsEmpty then
  begin
    // GetSession will raise exception if expired or not found
    Result := TMCPSessionManager.Instance.GetSession(LSessionId);
  end
  else
  begin
    // No session ID provided - auto-create new session
    Result := TMCPSessionManager.Instance.CreateSession;
    ASessionCreated := True;
  end;
end;

{ TJRPCIndyServer }

constructor TJRPCIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FBridge := TJRPCIndyBridge.Create(nil);

  OnParseAuthentication := ParseAuthentication;
  OnCommandGet := FBridge.HandleRequest;
  OnCommandOther := FBridge.HandleRequest;
end;

destructor TJRPCIndyServer.Destroy;
begin
  FBridge.Free;
  inherited;
end;

procedure TJRPCIndyServer.ParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TJRPCIndyServer.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;
  FBridge.Server := Value;
end;

end.
