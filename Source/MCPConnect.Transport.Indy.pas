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
    {$HINTS OFF}
    procedure LogResponse(const AResponse: TMCPTransportResponse);
    {$HINTS ON}
    procedure LogHttpResponse(const AResponse: TIdHTTPResponseInfo);

    procedure SendHeaders(const AResponse: TMCPTransportResponse; AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);

    function ReadContentStream(ARequestInfo: TIdHTTPRequestInfo): string;
  public
    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    property Server: TJRPCServer read FServer write FServer;
  end;

  TJRPCIndyServer = class(TIdCustomHTTPServer)
  private
    FServer: TJRPCServer;
    FBridge: TJRPCIndyBridge;
    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername, VPassword: String; var VHandled: Boolean);
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

procedure TJRPCIndyBridge.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('JRPC Server not found');

  LMcpHandler := TMCPTransportHandler.Create(FServer);

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    var
      LIndex: Integer;
    begin
      for LIndex := 0 to ARequestInfo.RawHeaders.Count - 1 do
      begin
        var n := ARequestInfo.RawHeaders.Names[LIndex];
        var v := ARequestInfo.RawHeaders.Values[n];
        ARequest.Headers.AddOrSetValue(n, v);
      end;

      ARequest.Url := ARequestInfo.URI;
      ARequest.Command := ARequestInfo.Command;
      ARequest.Content := ReadContentStream(ARequestInfo);

      LogRequest(ARequest);
    end,

    procedure (AResponse: TMCPTransportResponse)
    var
      LWriter: IMCPSSEResponseWriter;
    begin
      for var pair in AResponse.Headers do
        AResponseInfo.CustomHeaders.AddValue(pair.Key, pair.Value);

      if Assigned(AResponse.WriterProc) then
      begin
        AResponseInfo.ContentType := 'text/event-stream';
        AResponseInfo.ContentLength := -2; // Trick to stop indy to send Content-Length
        SendHeaders(AResponse, AContext, AResponseInfo);

        LWriter := TMCPSSEResponseWriterIndy.Create(AContext.Connection);
        AResponse.WriterProc(LWriter);
      end
      else
      begin
        AResponseInfo.ResponseNo := AResponse.Code;
        AResponseInfo.ContentText := AResponse.Content;
        AResponseInfo.ContentType := AResponse.ContentType;

        LogHttpResponse(AResponseInfo);
      end;
    end
  );
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
  const AAuthType, AAuthData: string; var VUsername, VPassword: String;
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
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Trace);
  Logger.Log(Format('Http Code: [%d]', [AResponse.ResponseNo]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in AResponse.CustomHeaders do
    Logger.Log(Format('%s', [head]), TLogLevel.Trace);
  Logger.Log(Format('*** Content: %s', [AResponse.ContentText]), TLogLevel.Trace);
end;

procedure TJRPCIndyBridge.LogRequest(const ARequest: TMCPTransportRequest);
begin
  Logger.Log('<--<--<--<--<--<--<-- REQUEST', TLogLevel.Trace);
  Logger.Log(Format('Url: [%s] %s', [ARequest.Command, ARequest.Url]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in ARequest.Headers do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Trace);
  Logger.Log('*** Content: ' + ARequest.Content, TLogLevel.Trace);
end;

procedure TJRPCIndyBridge.LogResponse(const AResponse: TMCPTransportResponse);
begin
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Trace);
  Logger.Log(Format('Http Code: [%d]', [AResponse.Code]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in AResponse.Headers do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Trace);
  Logger.Log(Format('*** Content: %s', [AResponse.Content]), TLogLevel.Trace);
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
  AHttpResponse.Date := GMTToLocalDateTime(AResponse.GetHeader('Date'));
  AHttpResponse.CustomHeaders.Clear;

  for LHeaderPair in AResponse.Headers do
  begin
    if IsIndyHeader(LHeaderPair.Key) then
      Continue;
    AHttpResponse.CustomHeaders.AddValue(LHeaderPair.Key, LHeaderPair.Value);
  end;
  AHttpResponse.TransferEncoding := AResponse.GetHeader('Transfer-Encoding');

  if AResponse.GetHeader('Connection') <> '' then
    AHttpResponse.Connection := AResponse.GetHeader('Connection');

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
