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
unit MCPConnect.Transport.WebBroker;

{$I MCPConnect.inc}

interface

uses
  System.SysUtils, System.Classes, System.Masks,
  Web.HTTPApp,

  MCPConnect.Transport.Base,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

type
  TJRPCDispatcher = class(TComponent, IWebDispatch)
  private
    FDispatchMask: TMask;
    FPathInfo: string;
    FServer: TJRPCServer;
    procedure SetPathInfo(const Value: string);
    procedure SetServer(const Value: TJRPCServer);
    function CreateMCPRequest(ARequest: TWebRequest): TMCPTransportRequest;
  public
    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(Sender: TObject; Request: TWebRequest; Response: TWebResponse): Boolean;
    function DispatchMask: TMask;

    property PathInfo: string read FPathInfo write SetPathInfo;
    property Server: TJRPCServer read FServer write SetServer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF HAS_WEBBROKER_SSE}
  TMCPSSEResponseWriterWebBroker = class(TInterfacedObject, IMCPSSEResponseWriter)
  private
    FResponse: TWebResponse;
    FSSEStream: TWebResponseStream;
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

    constructor Create(AResponse: TWebResponse);
  end;
  {$ENDIF}

implementation

function DateTimeToHTTPDate(ADateTime: TDateTime): string;
const
  HTTPDateFormat = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  Result := FormatDateTime(HTTPDateFormat, ADateTime, FS);
end;

constructor TJRPCDispatcher.Create(AOwner: TComponent);
begin
  inherited;
  FDispatchMask := nil;
  FPathInfo := 'jrpc';
end;

function TJRPCDispatcher.CreateMCPRequest(
  ARequest: TWebRequest): TMCPTransportRequest;
begin
  {$IFDEF HAS_WEBBROKER_REQUEST_HEADERS}
  for var I := 0 to ARequest.AllHeaders.Count - 1 do
    Result.Headers.AddOrSet(ARequest.AllHeaders.KeyNames[I],
      ARequest.AllHeaders.ValueFromIndex[I]);
  {$ELSE}
  if ARequest.Authorization <> '' then
    Result.Headers.AddOrSet('Authorization', ARequest.Authorization);
  if ARequest.CacheControl <> '' then
    Result.Headers.AddOrSet('Cache-Control', ARequest.CacheControl);
  if ARequest.Cookie <> '' then
    Result.Headers.AddOrSet('Cookie', ARequest.Cookie);
  if ARequest.Date > 0 then
    Result.Headers.AddOrSet('Date', DateTimeToHTTPDate(ARequest.Date));
  if ARequest.Accept <> '' then
    Result.Headers.AddOrSet('Accept', ARequest.Accept);
  if ARequest.From <> '' then
    Result.Headers.AddOrSet('From', ARequest.From);
  if ARequest.Host <> '' then
    Result.Headers.AddOrSet('Host', ARequest.Host);
  if ARequest.IfModifiedSince > 0 then
    Result.Headers.AddOrSet('If-Modified-Since', DateTimeToHTTPDate(ARequest.IfModifiedSince));
  if ARequest.Referer <> '' then
    Result.Headers.AddOrSet('Referer', ARequest.Referer);
  if ARequest.UserAgent <> '' then
    Result.Headers.AddOrSet('User-Agent', ARequest.UserAgent);
  if ARequest.ContentEncoding <> '' then
    Result.Headers.AddOrSet('Content-Encoding', ARequest.ContentEncoding);
  if ARequest.ContentType <> '' then
    Result.Headers.AddOrSet('Content-Type', ARequest.ContentType);
  if ARequest.ContentLength <> 0 then
    Result.Headers.AddOrSet('Content-Length', ARequest.ContentLength.ToString);
  if ARequest.ContentVersion <> '' then
    Result.Headers.AddOrSet('Content-Version', ARequest.ContentVersion);
  if ARequest.DerivedFrom <> '' then
    Result.Headers.AddOrSet('Derived-From', ARequest.DerivedFrom);
  if ARequest.Expires > 0 then
    Result.Headers.AddOrSet('Expires', DateTimeToHTTPDate(ARequest.Expires));
  if ARequest.Title <> '' then
    Result.Headers.AddOrSet('Title', ARequest.Title);
  {$ENDIF}

  Result.Command := ARequest.Method;
  Result.Content := ARequest.Content;
end;

destructor TJRPCDispatcher.Destroy;
begin
  FDispatchMask.Free;
  inherited;
end;

function TJRPCDispatcher.DispatchEnabled: Boolean;
begin
  Result := True;
end;

function TJRPCDispatcher.DispatchMask: TMask;
begin
  if not Assigned(FDispatchMask) then
  begin
    FDispatchMask := TMask.Create(FPathInfo);
  end;
  Result := FDispatchMask;
end;

function TJRPCDispatcher.DispatchMethodType: TMethodType;
begin
  Result := mtAny;
end;

function TJRPCDispatcher.DispatchRequest(Sender: TObject; Request: TWebRequest; Response: TWebResponse): Boolean;
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LMcpHandler: IMCPTransportHandler;
  LWriter: IMCPSSEResponseWriter;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

  LRequest := CreateMCPRequest(Request);

  LMcpHandler := TMCPTransportHandler.Create(FServer);
  LMcpHandler.HandleRequest(LRequest, LResponse);

  for var I := 0 to LResponse.Headers.Count - 1 do
    Response.CustomHeaders.AddPair(LResponse.Headers.RawHeaders[I].Key,
      LResponse.Headers.RawHeaders[I].Value);

  if Assigned(LResponse.WriterProc) then
  begin
    Response.ContentType := 'text/event-stream';

    {$IFDEF HAS_WEBBROKER_SSE}
    LWriter := TMCPSSEResponseWriterWebBroker.Create(Response);
    {$ELSE}
    raise Exception.Create('SSE not supported. Upgrade to Delphi 13.1 or higher');
    {$ENDIF}
    LResponse.WriterProc(LWriter);
  end
  else
  begin
    Response.StatusCode := LResponse.Code;
    Response.Content := LResponse.Content;
    Response.ContentType := LResponse.ContentType;
  end;
  Result := True;
end;

procedure TJRPCDispatcher.SetPathInfo(const Value: string);
begin
  // If the mask is already created should I raise an exception?
  FPathInfo := Value;
end;

procedure TJRPCDispatcher.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;
end;

{ TMCPSSEResponseWriterWebBroker }

{$IFDEF HAS_WEBBROKER_SSE}
function TMCPSSEResponseWriterWebBroker.Connected: Boolean;
begin
  Result := FSSEStream.Connected;
end;

constructor TMCPSSEResponseWriterWebBroker.Create(AResponse: TWebResponse);
begin
  inherited Create;
  FResponse := AResponse;
  FSSEStream := TWebResponseStream.BeginStream(FResponse, 'text/event-stream');
end;

procedure TMCPSSEResponseWriterWebBroker.Write(const AEvent, AValue: string;
  ARetry: Integer);
begin
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.WriteRetry(ARetry);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(const AEvent, AValue: string);
begin
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(const AValue: string);
begin
  FSSEStream.WriteData(AValue);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(const AId, AEvent,
  AValue: string; ARetry: Integer);
begin
  FSSEStream.WriteID(AId);
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.WriteRetry(ARetry);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(AId: Integer; const AEvent,
  AValue: string);
begin
  FSSEStream.WriteID(AId.ToString);
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(AId: Integer; const AEvent,
  AValue: string; ARetry: Integer);
begin
  FSSEStream.WriteID(AId.ToString);
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.WriteRetry(ARetry);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.Write(const AId, AEvent,
  AValue: string);
begin
  FSSEStream.WriteID(AId);
  FSSEStream.WriteEvent(AEvent);
  FSSEStream.WriteData(AValue);
  FSSEStream.EndEvent;
end;

procedure TMCPSSEResponseWriterWebBroker.WriteComment(const AValue: string);
begin
  FSSEStream.WriteComment(AValue);
  FSSEStream.EndEvent;
end;
{$ENDIF}

end.
