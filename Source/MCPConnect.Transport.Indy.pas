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
  System.Classes, System.SysUtils, System.Generics.Collections,
  IdCustomHTTPServer, IdContext, IdGlobal, IdGlobalProtocols, IdTCPConnection,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Base;

type
  TJRPCIndyBridge = class(TComponent)
  private
    FServer: TJRPCServer;
    procedure LogRequest(const ARequest: TMCPTransportRequest);
    procedure LogResponse(const AResponse: TMCPTransportResponse);
    procedure LogHttpResponse(const AResponse: TIdHTTPResponseInfo);

    procedure SendHeaders(const AResponse: TMCPTransportResponse; AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);

    procedure ConvertRequest(AContext: TIdContext; AHttpRequest: TIdHTTPRequestInfo; var ARequest: TMCPTransportRequest);
    procedure ConvertResponse(const AResponse: TMCPTransportResponse; AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);

    function ReadContentStream(ARequestInfo: TIdHTTPRequestInfo): string;
  public
    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    property Server: TJRPCServer read FServer write FServer;
  end;

  TJRPCIndyServer = class(TIdCustomHTTPServer)
  private
    FServer: TJRPCServer;
    FBridge: TJRPCIndyBridge;
    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    procedure SetServer(const Value: TJRPCServer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Server: TJRPCServer read FServer write SetServer;
  end;

  TMCPSSEResponseWriterIndy = class(TMCPSSEResponseWriter)
  private
    FConnection: TIdTCPConnection;
  protected
    function InternalConnected: Boolean; override;
    procedure InternalWriteLine(const AValue: string); override;
  public
    constructor Create(AConnection: TIdTCPConnection);
  end;

implementation

uses
  System.IOUtils, Logify;

procedure TJRPCIndyBridge.ConvertRequest(AContext: TIdContext; AHttpRequest: TIdHTTPRequestInfo; var ARequest: TMCPTransportRequest);
var
  LIndex: Integer;
begin
  for LIndex := 0 to AHttpRequest.RawHeaders.Count - 1 do
    ARequest.Headers.AddOrSet(AHttpRequest.RawHeaders.KeyNames[LIndex],
      AHttpRequest.RawHeaders.ValueFromIndex[LIndex]);

  ARequest.Url := AHttpRequest.URI;
  ARequest.Command := AHttpRequest.Command;
  ARequest.Content := ReadContentStream(AHttpRequest);

  LogRequest(ARequest);
end;

procedure TJRPCIndyBridge.ConvertResponse(const AResponse: TMCPTransportResponse;
  AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);
var
  LIndex: Integer;
  LWriter: IMCPSSEResponseWriter;
begin
  for LIndex := 0 to AResponse.Headers.Count - 1 do
    AHttpResponse.CustomHeaders.AddPair(AResponse.Headers.RawHeaders[LIndex].Key,
      AResponse.Headers.RawHeaders[LIndex].Value);

  if Assigned(AResponse.WriterProc) then
  begin
    AHttpResponse.ContentType := 'text/event-stream';
    AHttpResponse.ContentLength := -2; // Trick to stop indy to send Content-Length
    SendHeaders(AResponse, AContext, AHttpResponse);

    LWriter := TMCPSSEResponseWriterIndy.Create(AContext.Connection);
    AResponse.WriterProc(LWriter);
  end
  else
  begin
    AHttpResponse.ResponseNo := AResponse.Code;
    AHttpResponse.ContentText := AResponse.Content;
    AHttpResponse.ContentType := AResponse.ContentType;

    LogHttpResponse(AHttpResponse);
  end;

end;

procedure TJRPCIndyBridge.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('JRPC Server not found');

  LMcpHandler := TMCPTransportHandler.Create(FServer);

  ConvertRequest(AContext, ARequestInfo, LRequest);
  LMcpHandler.HandleRequest(LRequest, LResponse);
  ConvertResponse(LResponse, AContext, AResponseInfo);
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

procedure TJRPCIndyBridge.LogHttpResponse(const AResponse: TIdHTTPResponseInfo);
begin
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Debug);
  Logger.Log(Format('Http Code: [%d]', [AResponse.ResponseNo]), TLogLevel.Debug);
  Logger.Log('*** Headers ***', TLogLevel.Debug);
  for var head in AResponse.CustomHeaders do
    Logger.Log(Format('%s', [head]), TLogLevel.Debug);
  Logger.Log(Format('*** Content: %s', [AResponse.ContentText]), TLogLevel.Debug);
end;

procedure TJRPCIndyBridge.LogRequest(const ARequest: TMCPTransportRequest);
begin
  Logger.Log('<--<--<--<--<--<--<-- REQUEST', TLogLevel.Debug);
  Logger.Log(Format('Url: [%s] %s', [ARequest.Command, ARequest.Url]), TLogLevel.Debug);
  Logger.Log('*** Headers ***', TLogLevel.Debug);
  for var head in ARequest.Headers.RawHeaders do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Debug);
  Logger.Log('*** Content: ' + ARequest.Content, TLogLevel.Debug);
end;

procedure TJRPCIndyBridge.LogResponse(const AResponse: TMCPTransportResponse);
begin
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Debug);
  Logger.Log(Format('Http Code: [%d]', [AResponse.Code]), TLogLevel.Debug);
  Logger.Log('*** Headers ***', TLogLevel.Debug);
  for var head in AResponse.Headers.RawHeaders do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Debug);
  Logger.Log(Format('*** Content: %s', [AResponse.Content]), TLogLevel.Debug);
end;

function TJRPCIndyBridge.ReadContentStream(ARequestInfo: TIdHTTPRequestInfo): string;
var
  LEncoding: IIdTextEncoding;
begin
  if not Assigned(ARequestInfo.PostStream) then
    Exit('');

  if ARequestInfo.PostStream.Size = 0 then
    Exit('');

  if ARequestInfo.CharSet <> '' then
    LEncoding := IndyTextEncoding(ARequestInfo.CharSet)
  else
    LEncoding := IndyTextEncoding_UTF8;

  ARequestInfo.PostStream.Position := 0;
  Result := ReadStringFromStream(ARequestInfo.PostStream, -1, LEncoding);
end;

procedure TJRPCIndyBridge.SendHeaders(const AResponse: TMCPTransportResponse;
  AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);

  function IsIndyHeader(const Name: string): Boolean;
  const
    IndyHeaders: array [0..4] of string = ('Date', 'Content-Type', 'Content-Length', 'Connection', 'Transfer-Encoding');
  var
    IndyHeader: string;
  begin
    Result := False;
    for IndyHeader in IndyHeaders do
      if CompareText(Name, IndyHeader) = 0 then
        Exit(True);
  end;

var
  LHeaderPair: TPair<string, string>;
begin
  inherited;
  AHttpResponse.Date := GMTToLocalDateTime(AResponse.Headers.Get('Date'));
  AHttpResponse.CustomHeaders.Clear;

  for LHeaderPair in AResponse.Headers.RawHeaders do
  begin
    if IsIndyHeader(LHeaderPair.Key) then
      Continue;
    AHttpResponse.CustomHeaders.AddValue(LHeaderPair.Key, LHeaderPair.Value);
  end;
  AHttpResponse.TransferEncoding := AResponse.Headers.Get('Transfer-Encoding');

  if AResponse.Headers.Get('Connection') <> '' then
    AHttpResponse.Connection := AResponse.Headers.Get('Connection');

  //SendCookies;

    AHttpResponse.WriteHeader;
end;

{ TMCPSSEResponseWriterIndy }

constructor TMCPSSEResponseWriterIndy.Create(AConnection: TIdTCPConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TMCPSSEResponseWriterIndy.InternalConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TMCPSSEResponseWriterIndy.InternalWriteLine(const AValue: string);
begin
  inherited;
  FConnection.Socket.WriteLn(AValue, IndyTextEncoding_UTF8);
end;

end.
