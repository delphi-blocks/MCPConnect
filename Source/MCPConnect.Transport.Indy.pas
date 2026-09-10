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

  JRPC.Core,
  MCPConnect.MCP.Server,
  MCPConnect.Transport.Base,
  MCPConnect.Transport.MediaType;

resourcestring
  SMCPServerNotFound = 'MCP Server not found';

type
  TMCPIndyBridge = class(TComponent)
  private
    FMCPServer: TMCPServer;
    procedure LogRequest(const ARequest: TMCPTransportRequest);
    {$HINTS OFF}
    procedure LogResponse(const AResponse: TMCPTransportResponse);
    {$HINTS ON}
    procedure LogHttpResponse(const AResponse: TIdHTTPResponseInfo);

    function IsIndyHeader(const Name: string): Boolean;
    procedure SendHeaders(const AResponse: TMCPTransportResponse; AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);
    function ReadContentStream(ARequestInfo: TIdHTTPRequestInfo): string;
  public
    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    property MCPServer: TMCPServer read FMCPServer write FMCPServer;
  end;

  TMCPIndyServer = class(TIdCustomHTTPServer)
  private
    FMCPServer: TMCPServer;
    FBridge: TMCPIndyBridge;
    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername, VPassword: String; var VHandled: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property MCPServer: TMCPServer read FMCPServer;
  public
    class function CreateMCPServer(AOwner: TComponent): TMCPIndyServer;
  end;

  TMCPTransportWriterIndy = class(TInterfacedObject, IMCPTransportWriter)
  private
    FConnection: TIdTCPConnection;
    function SplitString(const AValue: string): TArray<string>;
    procedure WriteSSEEvent(const AEvent, AValue: string; ARetry: Integer);
  protected
    { IMCPTransportWriter }
    function Connected: Boolean;
    procedure Write(const AValue: string);
    procedure WriteComment(const AValue: string); overload;
    function SupportsStreaming: Boolean;
  public
    constructor Create(AConnection: TIdTCPConnection);
  end;

implementation

uses
  System.IOUtils, Logify;

procedure TMCPIndyBridge.HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FMCPServer) then
    raise EJRPCException.Create(SMCPServerNotFound);

  LMcpHandler := TMCPTransportHandler.Create(FMCPServer, TMCPTransportWriterIndy.Create(AContext.Connection));

  LMcpHandler.SendResponseHeadersProc :=
    procedure (AResponse: TMCPTransportResponse)
    begin
      SendHeaders(AResponse, AContext, AResponseInfo);
    end;

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    var
      LIndex: Integer;
    begin
      ARequest.Headers.Clear();
      for LIndex := 0 to ARequestInfo.RawHeaders.Count - 1 do
      begin
        var n := ARequestInfo.RawHeaders.Names[LIndex];
        var v := ARequestInfo.RawHeaders.Values[n];
        ARequest.Headers.Add(n, v);
      end;

      ARequest.Url := ARequestInfo.URI;
      ARequest.Command := ARequestInfo.Command;
      ARequest.Content := ReadContentStream(ARequestInfo);
      ARequest.Protocol := TTransportProtocol.StreamableHTTP;

      LogRequest(ARequest);
    end,

    procedure (AResponse: TMCPTransportResponse)
    begin
      AResponseInfo.ResponseNo := AResponse.Code;
      AResponseInfo.ContentText := AResponse.Content;
      // SendHeaders after ContentText so indy can handle Content-Length
      SendHeaders(AResponse, AContext, AResponseInfo);

      LogHttpResponse(AResponseInfo);
    end
  );
end;

{ TMCPIndyServer }

constructor TMCPIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FBridge := TMCPIndyBridge.Create(nil);
  FMCPServer := TMCPServer.Create(nil);
  FBridge.MCPServer := FMCPServer;

  OnParseAuthentication := ParseAuthentication;
  OnCommandGet := FBridge.HandleRequest;
  OnCommandOther := FBridge.HandleRequest;
end;

class function TMCPIndyServer.CreateMCPServer(AOwner: TComponent): TMCPIndyServer;
begin
  Result := TMCPIndyServer.Create(AOwner);
end;

destructor TMCPIndyServer.Destroy;
begin
  FBridge.Free;
  FMCPServer.Free;
  inherited;
end;

procedure TMCPIndyServer.ParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: string; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TMCPIndyBridge.LogHttpResponse(const AResponse: TIdHTTPResponseInfo);
begin
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Trace);
  Logger.Log(Format('Http Code: [%d]', [AResponse.ResponseNo]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in AResponse.CustomHeaders do
    Logger.Log(Format('%s', [head]), TLogLevel.Trace);
  Logger.Log(Format('*** Content: %s', [AResponse.ContentText]), TLogLevel.Trace);
end;

procedure TMCPIndyBridge.LogRequest(const ARequest: TMCPTransportRequest);
begin
  Logger.Log('<--<--<--<--<--<--<-- REQUEST', TLogLevel.Trace);
  Logger.Log(Format('Url: [%s] %s', [ARequest.Command, ARequest.Url]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in ARequest.Headers do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Trace);
  Logger.Log('*** Content: ' + ARequest.Content, TLogLevel.Trace);
end;

procedure TMCPIndyBridge.LogResponse(const AResponse: TMCPTransportResponse);
begin
  Logger.Log('-->-->-->-->-->-->--> RESPONSE', TLogLevel.Trace);
  Logger.Log(Format('Http Code: [%d]', [AResponse.Code]), TLogLevel.Trace);
  Logger.Log('*** Headers ***', TLogLevel.Trace);
  for var head in AResponse.Headers do
    Logger.Log(Format('%s: %s', [head.Key, head.Value]), TLogLevel.Trace);
  Logger.Log(Format('*** Content: %s', [AResponse.Content]), TLogLevel.Trace);
end;

function TMCPIndyBridge.IsIndyHeader(const Name: string): Boolean;
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

function TMCPIndyBridge.ReadContentStream(ARequestInfo: TIdHTTPRequestInfo): string;
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

procedure TMCPIndyBridge.SendHeaders(const AResponse: TMCPTransportResponse;
  AContext: TIdContext; AHttpResponse: TIdHTTPResponseInfo);
var
  LHeaderPair: TPair<string, string>;
begin
  inherited;
  if AHttpResponse.HeaderHasBeenWritten then
    Exit;

  AHttpResponse.Date := GMTToLocalDateTime(AResponse.GetHeader('Date'));
  AHttpResponse.CustomHeaders.Clear;

  for LHeaderPair in AResponse.Headers do
  begin
    if IsIndyHeader(LHeaderPair.Key) then
      Continue;
    AHttpResponse.CustomHeaders.AddValue(LHeaderPair.Key, LHeaderPair.Value);
  end;
  AHttpResponse.ContentType := AResponse.ContentType;
  AHttpResponse.TransferEncoding := AResponse.TransferEncoding;
  AHttpResponse.Connection := AResponse.GetHeader('Connection');
  if AResponse.ContentType = TMediaType.TEXT_EVENT_STREAM then
    AHttpResponse.ContentLength := -2; // Prevents indy from sending Content-Length

  //SendCookies;

  AHttpResponse.WriteHeader;
  AContext.Connection.Socket.WriteBufferFlush;
end;

{ TMCPTransportWriterIndy }

constructor TMCPTransportWriterIndy.Create(AConnection: TIdTCPConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TMCPTransportWriterIndy.Connected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TMCPTransportWriterIndy.SupportsStreaming: Boolean;
begin
  Result := True;
end;

procedure TMCPTransportWriterIndy.Write(const AValue: string);
begin
  WriteSSEEvent('', AValue, -1);
end;

procedure TMCPTransportWriterIndy.WriteComment(const AValue: string);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  LMessage := '';

  for LLine in LLines do
    LMessage := LMessage + ': ' + LLine + #13#10;

  FConnection.Socket.WriteLn(LMessage, IndyTextEncoding_UTF8);
  FConnection.Socket.WriteBufferFlush;
end;

procedure TMCPTransportWriterIndy.WriteSSEEvent(const AEvent, AValue: string; ARetry: Integer);
var
  LLines: TArray<string>;
  LLine: string;
  LMessage: string;
begin
  LLines := SplitString(AValue);

  // No "id:" field, and no parameter to put one in: event ids were how a
  // client resumed a stream, and 2026-07-28 removed resumability along with
  // the sessions it was built on. "event:" and "retry:" are unrelated to that
  // and stay.
  LMessage := '';
  if AEvent <> '' then
    LMessage := LMessage + 'event: ' + AEvent + #13#10;
  if ARetry > 0 then
    LMessage := LMessage + 'retry: ' + IntToStr(ARetry) + #13#10;

  for LLine in LLines do
    LMessage := LMessage + 'data: ' + LLine + #13#10;

  FConnection.Socket.WriteLn(LMessage, IndyTextEncoding_UTF8);
  FConnection.Socket.WriteBufferFlush;
end;

function TMCPTransportWriterIndy.SplitString(
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

end.
