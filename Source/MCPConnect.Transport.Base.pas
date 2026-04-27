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

uses
  System.Classes, System.SysUtils,
  IdCustomHTTPServer, IdContext, IdGlobal,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.JRPC.Classes,
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
  IMCPSSEResponseWriter = interface
    ['{68598454-50C5-4892-B8E0-81687CC2F4DE}']
    procedure Write(const AValue: string); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;
    function Connected: Boolean;
  end;

  TMCPSSEResponseWriter = class(TInterfacedObject, IMCPSSEResponseWriter)
  protected
    function SplitString(const AValue: string): TArray<string>;
    function InternalConnected: Boolean; virtual; abstract;
    procedure InternalWriteLine(const AValue: string); virtual; abstract;
  public
    { IMCPSSEResponseWriter }
    procedure Write(const AValue: string); overload;
    procedure Write(const AEvent, AValue: string); overload;
    procedure Write(const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(const AId, AEvent, AValue: string); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string; ARetry: Integer); overload;
    procedure Write(AId: Integer; const AEvent, AValue: string); overload;
    procedure WriteComment(const AValue: string); overload;
    function Connected: Boolean;
  end;

  TMCPSSEResponseWriterProc = reference to procedure (AWriter: IMCPSSEResponseWriter);

  TMCPTransportHeaders = record
    RawHeaders: TArray<TPair<string, string>>;

    function Exists(const AName: string): Boolean;
    function Get(const AName: string): string;
    function GetCookie(const AName: string): string;
    procedure AddOrSet(const AName, AValue: string);
    function Count: Integer;
  end;

  TMCPTransportRequest = record
    Url: string;
    Command: string;
    Headers: TMCPTransportHeaders;
    Content: string;

    function ToString: string;
  end;

  TMCPTransportResponse = record
  private
    FWriterProc: TMCPSSEResponseWriterProc;
    function GetContentType: string;
    procedure SetContentType(const AValue: string);
  public
    Headers: TMCPTransportHeaders;
    Content: string;
    Code: Integer;
    procedure SSEContent(AWriterProc: TMCPSSEResponseWriterProc);
    property WriterProc: TMCPSSEResponseWriterProc read FWriterProc write FWriterProc;
    property ContentType: string read GetContentType write SetContentType;
  end;

  IMCPTransportHandler = interface
  ['{B2966C2A-7594-4B30-95D9-D702AE20633E}']
    procedure HandleRequest(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
  end;

  TJRPCMsgContext = record
    GC: IGarbageCollector;
    JRPCCtx: TJRPCContext;
    Session: TMCPSessionBase;
    Requests: TJRPCMessages;
    Responses: TJRPCMessages;
  end;

  TMCPRequestHandler = class(TInterfacedObject)
  private
    FContext: TJRPCMsgContext;
  private
    procedure HandleMessage(AMessage: TJRPCMessage);
    procedure HandleMessages(AContext: TJRPCMsgContext);
  public
    class procedure HandleJRPCRequest(AContext: TJRPCMsgContext);
  end;

  TMCPTransportHandler = class(TInterfacedObject, IMCPTransportHandler)
  private
    FMCPConfig: TMCPConfig;
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    FSessionConfig: TSessionConfig;
  private
    procedure InjectCORS(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
    function CheckOrigin(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse): Boolean;
    function CheckAuthorization(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse): Boolean;
    function ExtractSessionId(const ARequest: TMCPTransportRequest): string;
    function HandleSession(const ARequest: TMCPTransportRequest; out ASessionCreated: Boolean): TMCPSessionBase;

    procedure HandleGET(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
    procedure HandlePOST(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
    procedure HandleOPTIONS(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
  public
    constructor Create(AServer:TJRPCServer);

    { IMCPHttpHandler }
    procedure HandleRequest(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
  end;

implementation

uses
  System.IOUtils,
  MCPConnect.Configuration.Neon,
  MCPConnect.JRPC.Invoker;


{ TMCPTransportHeaders }

procedure TMCPTransportHeaders.AddOrSet(const AName, AValue: string);
begin
  RawHeaders := RawHeaders + [TPair<string, string>.Create(AName, AValue)];
end;

function TMCPTransportHeaders.Count: Integer;
begin
  Result := Length(RawHeaders);
end;

function TMCPTransportHeaders.Exists(const AName: string): Boolean;
begin
  Result := False;
  for var pair in RawHeaders do
    if SameText(pair.Key, AName) then
      Exit(True);
end;

function TMCPTransportHeaders.Get(const AName: string): string;
begin
  Result := '';
  for var pair in RawHeaders do
    if SameText(pair.Key, AName) then
      Exit(pair.Value.Trim);
end;


function TMCPTransportHeaders.GetCookie(const AName: string): string;
begin
  var sl := TStringList.Create;
  try
    sl.NameValueSeparator := '=';
    sl.LineBreak := ';';
    sl.Text := Get('Cookie');
  finally
    sl.Free;
  end;
end;

{ TMCPTransportHandler }

function TMCPTransportHandler.CheckAuthorization(const ARequest: TMCPTransportRequest;
    var AResponse: TMCPTransportResponse): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if ARequest.Headers.Get('Authorization') <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if ARequest.Headers.GetCookie(FAuthTokenConfig.CustomHeader) <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if ARequest.Headers.Get(FAuthTokenConfig.CustomHeader) <> FAuthTokenConfig.Token then
          Exit(False);
      end;

    else
      raise EJRPCException.Create('Invalid token location');
    end;
  end;
end;

function TMCPTransportHandler.CheckOrigin(const ARequest: TMCPTransportRequest;
  var AResponse: TMCPTransportResponse): Boolean;
var
  LOrigin, LHeader: string;
begin
  if not Assigned(FMCPConfig) then
    raise EJRPCException.Create('Error retrieving MCP configuration');

  Result := True;
  if Length(FMCPConfig.Security.AllowedOrigins) = 0 then
    Exit;

  LHeader := ARequest.Headers.Get('Origin');
  if LHeader.IsEmpty then
    Exit;

  for LOrigin in FMCPConfig.Security.AllowedOrigins do
    if LHeader.StartsWith(LOrigin) then
      Exit;

  Result := False;
end;

procedure TMCPTransportHandler.HandleRequest(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
begin
  try
    if not CheckOrigin(ARequest, AResponse) then
    begin
      AResponse.Code := HTTP_CODE_FORBIDDEN;
      AResponse.Content := '';
      Exit;
    end;

    if not CheckAuthorization(ARequest, AResponse) then
    begin
      AResponse.Code := HTTP_CODE_FORBIDDEN;
      AResponse.Content := '';
      Exit;
    end;

    if ARequest.Command = 'GET' then
      HandleGET(ARequest, AResponse)
    else if ARequest.Command = 'POST' then
      HandlePOST(ARequest, AResponse)
    else if ARequest.Command = 'OPTIONS' then
      HandleOPTIONS(ARequest, AResponse)
    else
      AResponse.Code := HTTP_CODE_NOTALLOWED;

    InjectCORS(ARequest, AResponse);
  except
    on E: EJRPCException do
    begin
      AResponse.Code := 500;
      AResponse.ContentType := 'application/json';
      AResponse.Content := E.ToJSON;
    end;

    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.ContentType := 'application/json';
      AResponse.Content := Format('{"message": "%s"}', [E.Message]);
    end;
  end;
end;

constructor TMCPTransportHandler.Create(AServer: TJRPCServer);
begin
  FServer := AServer;

  FMCPConfig := FServer.GetConfiguration<TMCPConfig>;
  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
  FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
end;

function TMCPTransportHandler.ExtractSessionId(const ARequest: TMCPTransportRequest): string;
begin
  Result := '';

  if not Assigned(FSessionConfig) then
    Exit;

  case FSessionConfig.GetLocation of
    TSessionIdLocation.Header:
      Result := ARequest.Headers.Get(FSessionConfig.GetHeaderName);

    TSessionIdLocation.Cookie:
      Result := ARequest.Headers.GetCookie(FSessionConfig.GetHeaderName);
  end;

  Result := Result.Trim;
end;

procedure TMCPTransportHandler.HandleGET(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
var
  LGarbage: IGarbageCollector;
  LContext: TJRPCContext;
  LSession: TMCPSessionBase;
  LSessionCreated: Boolean;
begin
  //AResponse.Headers.AddOrSet('Allow', 'POST');
  //AResponse.Code := HTTP_CODE_NOTALLOWED;

  LSessionCreated := False;
  LGarbage := TGarbageCollector.CreateInstance;
  LContext := TJRPCContext.Create;

  LGarbage.Add(LContext);
  LContext.AddContent(LGarbage);
  LContext.AddContent(FServer);

  // Handle session (get existing or create new)
  LSession := HandleSession(ARequest, LSessionCreated);

  // Add session to context if available
  if Assigned(LSession) then
    LContext.AddContent(LSession);

  AResponse.SSEContent(
    procedure (AWriter: IMCPSSEResponseWriter)
    begin
      while AWriter.Connected  do
      begin
        // TODO:
        // 1. Get the active session (exit if not found)
        // 2. Tell the session that there's a SSE channel
        // 3. Read messages from notification manager of the current session
        // 4. Serialize the notifications and send to the Writer
      end;
    end
  );
end;

procedure TMCPTransportHandler.HandleOPTIONS(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
begin
  AResponse.Code := HTTP_CODE_NOCONTENT;
  AResponse.Content := '';
end;

procedure TMCPTransportHandler.HandlePOST(const ARequest: TMCPTransportRequest; var AResponse: TMCPTransportResponse);
var
  LGarbage: IGarbageCollector;
  LContext: TJRPCContext;
  LCtx: TJRPCMsgContext;
  LSession: TMCPSessionBase;
  LSessionCreated: Boolean;

  LRequestList, LResponseList: TJRPCMessages;
begin
  LSessionCreated := False;
  LGarbage := TGarbageCollector.CreateInstance;
  LContext := TJRPCContext.Create;

  LGarbage.Add(LContext);
  LContext.AddContent(LGarbage);
  LContext.AddContent(FServer);

  // Handle session (get existing or create new)
  LSession := HandleSession(ARequest, LSessionCreated);

  // Add session to context if available
  if Assigned(LSession) then
    LContext.AddContent(LSession);

  LRequestList := TJRPCMessages.CreateFromJson(ARequest.Content);
  LGarbage.Add(LRequestList);

  LResponseList := TJRPCMessages.Create;
  LGarbage.Add(LResponseList);

  LCtx.GC := LGarbage;
  LCtx.JRPCCtx := LContext;
  LCtx.Session := LSession;
  LCtx.Requests := LRequestList;
  LCtx.Responses := LResponseList;

  TMCPRequestHandler.HandleJRPCRequest(LCtx);

  { TODO -opaolo -c : To change based on SSE vs JSON reqs 28/03/2026 17:45:47 }
  AResponse.ContentType := 'application/json';
  AResponse.Content := LResponseList.ToJson;

  if LResponseList.Count = 0 then
    AResponse.Code := HTTP_CODE_ACCEPTED
  else
    AResponse.Code := HTTP_CODE_OK;
end;

function TMCPTransportHandler.HandleSession(const ARequest: TMCPTransportRequest; out ASessionCreated: Boolean): TMCPSessionBase;
var
  LSessionId: string;
begin
  Result := nil;
  ASessionCreated := False;

  if not Assigned(FSessionConfig) or (not FSessionConfig.IsApplied) then
    Exit;

  LSessionId := ExtractSessionId(ARequest);

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

procedure TMCPTransportHandler.InjectCORS(const ARequest: TMCPTransportRequest;
    var AResponse: TMCPTransportResponse);
var
  LHValue: string;
begin
  if not FMCPConfig.Security.CORS then
    Exit;

  // Set the allowed origins (from security configuration)
  LHValue := ARequest.Headers.Get('Origin');
  if not LHValue.IsEmpty then
    AResponse.Headers.AddOrSet('Access-Control-Allow-Origin', LHValue);

  // Set the allowed methods supported by the server (from security configuration)
  LHValue := ARequest.Headers.Get('Access-Control-Request-Method');
  if not LHValue.IsEmpty then
    AResponse.Headers.AddOrSet('Access-Control-Allow-Methods', string.Join(',', FMCPConfig.Security.AllowedMethods));

  // Set the allowed headers as requested
  LHValue := ARequest.Headers.Get('Access-Control-Request-Headers');
  if not LHValue.IsEmpty then
    AResponse.Headers.AddOrSet('Access-Control-Allow-Headers', LHValue);

end;

{ TMCPTransportResponse }

function TMCPTransportResponse.GetContentType: string;
begin
  Result := Headers.Get('Content-Type');
end;

procedure TMCPTransportResponse.SetContentType(const AValue: string);
begin
  Headers.AddOrSet('Content-Type', AValue);
end;

procedure TMCPTransportResponse.SSEContent(
  AWriterProc: TMCPSSEResponseWriterProc);
begin
  FWriterProc := AWriterProc;
end;

procedure TMCPRequestHandler.HandleMessages(AContext: TJRPCMsgContext);
begin
  FContext := AContext;
  for var LRequest in FContext.Requests.List do
    HandleMessage(LRequest);
end;

class procedure TMCPRequestHandler.HandleJRPCRequest(AContext: TJRPCMsgContext);
var
  LHandler: TMCPRequestHandler;
begin
  LHandler := TMCPRequestHandler.Create;
  try
    LHandler.HandleMessages(AContext);
  finally
    LHandler.Free;
  end;
end;

procedure TMCPRequestHandler.HandleMessage(AMessage: TJRPCMessage);
var
  LRequest: TJRPCRequest;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokerCtx: TJRPCInvokerContext;
begin
  if AMessage is TJRPCNotification then
  begin
    (FContext.Session as TMCPSessionData).Inbound.Notifications.Enqueue(AMessage as TJRPCNotification);
    Exit;
  end;

  if AMessage is TJRPCResponse then
  begin
    (FContext.Session as TMCPSessionData).Inbound.Responses.Enqueue(AMessage as TJRPCResponse);
    Exit;
  end;

  if AMessage is TJRPCError then
  begin
    var LOriginal := AMessage as TJRPCError;

    // If the error is in the JRPC request messages then process internally the error.
    if LOriginal.Request then
    begin
      (FContext.Session as TMCPSessionData).Inbound.Errors.Enqueue(AMessage as TJRPCError);
    end
    else
    begin
      // If the error was generated processing the request, clone the error object
      FContext.Responses.AddMessage(LOriginal.Clone);
    end;
    Exit;
  end;

  LRequest := AMessage as TJRPCRequest;
  try
    FContext.JRPCCtx.AddContent(LRequest);

    if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
      raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LRequest.Method]);

    LInstance := LConstructorProxy.ConstructorFunc();
    FContext.GC.Add(LInstance);

    // Injects the context inside the instance
    FContext.JRPCCtx.Inject(LInstance);

    LInvokerCtx.GC := FContext.GC;
    LInvokerCtx.Request := LRequest;
    LInvokerCtx.Responses := FContext.Responses;
    LInvokerCtx.ApiInstance := LInstance;
    LInvokerCtx.SelectConfig(LConstructorProxy.NeonConfig, FContext.JRPCCtx.FindContextDataAs<TJRPCNeonConfig>);

    TJRPCInvoker.Invoke(LInvokerCtx);
  except
    on E: Exception do
    begin
      var err := TJRPCInvoker.HandleError(E, LRequest.Id);
      FContext.Responses.AddMessage(err);
    end;
  end;
end;

{ TMCPTransportRequest }

function TMCPTransportRequest.ToString: string;
begin
  Result := 'Headers: ' + sLineBreak;
  for var head in Headers.RawHeaders do
    Result := Result + Format('%s: %s', [head.Key, head.Value]) + sLineBreak;

  Result := '------' + sLineBreak + Format('Request: [%s] %s', [Command, Url]) + sLineBreak + Result;
  Result := Result + Format('Content: %s', [Content]) + sLineBreak;
end;

{ TMCPSSEResponseWriter }

function TMCPSSEResponseWriter.Connected: Boolean;
begin
  Result := InternalConnected;
end;

function TMCPSSEResponseWriter.SplitString(
  const AValue: string): TArray<string>;
var
  LLines: TArray<string>;
begin
  LLines := AValue.Split([sLineBreak]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#13]);
  if Length(LLines) = 0 then
    LLines := AValue.Split([#10]);
  if Length(LLines) = 0 then
    LLines := [AValue];

  Result := LLines;
end;

procedure TMCPSSEResponseWriter.Write(const AEvent, AValue: string;
  ARetry: Integer);
begin
  Write('', AEvent, AValue, ARetry);
end;

procedure TMCPSSEResponseWriter.Write(const AEvent, AValue: string);
begin
  Write(AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.Write(const AValue: string);
begin
  Write('', AValue);
end;

procedure TMCPSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string);
begin
  Write(AId.ToString, AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.Write(AId: Integer; const AEvent,
  AValue: string; ARetry: Integer);
begin
  Write(AId.ToString, AEvent, AValue, ARetry);
end;

procedure TMCPSSEResponseWriter.Write(const AId, AEvent, AValue: string;
  ARetry: Integer);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';
  if AId <> '' then
    LMessage := LMessage + 'id: ' + AId + #13#10;
  if AEvent <> '' then
    LMessage := LMessage + 'event: ' + AEvent + #13#10;
  if ARetry > 0 then
    LMessage := LMessage + 'retry: ' + IntToStr(ARetry) + #13#10;

  for LLine in LLines do
    LMessage := LMessage + 'data: ' + LLine + #13#10;

  InternalWriteLine(LMessage);
end;

procedure TMCPSSEResponseWriter.Write(const AId, AEvent, AValue: string);
begin
  Write(AId, AEvent, AValue, 0);
end;

procedure TMCPSSEResponseWriter.WriteComment(const AValue: string);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';

  for LLine in LLines do
    LMessage := LMessage + ': ' + LLine + #13#10;

  InternalWriteLine(LMessage);
end;

end.
