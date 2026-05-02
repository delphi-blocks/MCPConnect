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
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

  LMcpHandler := TMCPTransportHandler.Create(FServer);
  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    begin
      {$IFDEF HAS_WEBBROKER_REQUEST_HEADERS}
      for var I := 0 to Request.AllHeaders.Count - 1 do
        ARequest.Headers.AddOrSetValue(Request.AllHeaders.KeyNames[I],
          Request.AllHeaders.ValueFromIndex[I]);
      {$ELSE}
      if Request.Authorization <> '' then
        ARequest.Headers.AddOrSetValue('Authorization', Request.Authorization);
      if Request.CacheControl <> '' then
        ARequest.Headers.AddOrSetValue('Cache-Control', Request.CacheControl);
      if Request.Cookie <> '' then
        ARequest.Headers.AddOrSetValue('Cookie', Request.Cookie);
      if Request.Date > 0 then
        ARequest.Headers.AddOrSetValue('Date', DateTimeToHTTPDate(Request.Date));
      if Request.Accept <> '' then
        ARequest.Headers.AddOrSetValue('Accept', Request.Accept);
      if Request.From <> '' then
        ARequest.Headers.AddOrSetValue('From', Request.From);
      if Request.Host <> '' then
        ARequest.Headers.AddOrSetValue('Host', Request.Host);
      if Request.IfModifiedSince > 0 then
        ARequest.Headers.AddOrSetValue('If-Modified-Since', DateTimeToHTTPDate(Request.IfModifiedSince));
      if Request.Referer <> '' then
        ARequest.Headers.AddOrSet('Referer', Request.Referer);
      if Request.UserAgent <> '' then
        ARequest.Headers.AddOrSetValue('User-Agent', Request.UserAgent);
      if Request.ContentEncoding <> '' then
        ARequest.Headers.AddOrSetValue('Content-Encoding', Request.ContentEncoding);
      if Request.ContentType <> '' then
        ARequest.Headers.AddOrSetValue('Content-Type', Request.ContentType);
      if Request.ContentLength <> 0 then
        ARequest.Headers.AddOrSetValue('Content-Length', Request.ContentLength.ToString);
      if Request.ContentVersion <> '' then
        ARequest.Headers.AddOrSetValue('Content-Version', Request.ContentVersion);
      if Request.DerivedFrom <> '' then
        ARequest.Headers.AddOrSetValue('Derived-From', Request.DerivedFrom);
      if Request.Expires > 0 then
        ARequest.Headers.AddOrSetValue('Expires', DateTimeToHTTPDate(Request.Expires));
      if Request.Title <> '' then
        ARequest.Headers.AddOrSetValue('Title', Request.Title);
      {$ENDIF}

      ARequest.Command := Request.Method;
      ARequest.Content := Request.Content;


      //LogRequest(ARequest);
    end,

    procedure (AResponse: TMCPTransportResponse)
    var
      LWriter: IMCPSSEResponseWriter;
    begin
      for var pair in AResponse.Headers do
        Response.CustomHeaders.AddPair(pair.Key, pair.Value);

      if Assigned(AResponse.WriterProc) then
      begin
        Response.ContentType := 'text/event-stream';

        {$IFDEF HAS_WEBBROKER_SSE}
        LWriter := TMCPSSEResponseWriterWebBroker.Create(Response);
        {$ELSE}
        raise Exception.Create('SSE not supported. Upgrade to Delphi 13.1 or higher');
        {$ENDIF}
        AResponse.WriterProc(LWriter);
      end
      else
      begin
        Response.StatusCode := AResponse.Code;
        Response.Content := AResponse.Content;
        Response.ContentType := AResponse.ContentType;
      end;
    end
  );

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
