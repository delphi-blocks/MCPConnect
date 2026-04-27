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

    procedure ConvertRequest(AHttpRequest: TIdHTTPRequestInfo; var ARequest: TMCPTransportRequest);
    procedure ConvertResponse(const AResponse: TMCPTransportResponse; AHttpResponse: TIdHTTPResponseInfo);

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

implementation

uses
  System.IOUtils, Logify;

procedure TJRPCIndyBridge.ConvertRequest(AHttpRequest: TIdHTTPRequestInfo; var ARequest: TMCPTransportRequest);
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
  AHttpResponse: TIdHTTPResponseInfo);
var
  LIndex: Integer;
begin
  for LIndex := 0 to AResponse.Headers.Count - 1 do
    AHttpResponse.CustomHeaders.AddPair(AResponse.Headers.RawHeaders[LIndex].Key,
      AResponse.Headers.RawHeaders[LIndex].Value);

  AHttpResponse.ResponseNo := AResponse.Code;
  AHttpResponse.ContentText := AResponse.Content;
  AHttpResponse.ContentType := AResponse.ContentType;

  LogHttpResponse(AHttpResponse);
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

  ConvertRequest(ARequestInfo, LRequest);
  LMcpHandler.HandleRequest(LRequest, LResponse);
  ConvertResponse(LResponse, AResponseInfo);
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

end.
