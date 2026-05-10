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
    procedure ConvertHeaders(AWebRequest: TWebRequest; AMCPRequest: TMCPTransportRequest);
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


function DateToHttpStr(ADate: TDateTime): string;
const
  sDateFormat = '"%s", dd "%s" yyyy hh":"nn":"ss';
begin
  Result := Format(FormatDateTime(sDateFormat + ' "GMT"', ADate), [DayOfWeekStr(ADate), MonthStr(ADate)]);
end;

function DateTimeToHTTPDate(ADateTime: TDateTime): string;
const
  HTTPDateFormat = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  Result := FormatDateTime(HTTPDateFormat, ADateTime, FS);
end;

procedure TJRPCDispatcher.ConvertHeaders(AWebRequest: TWebRequest; AMCPRequest: TMCPTransportRequest);
begin
  {$IFDEF HAS_WEBBROKER_ALLHEADERS}
  for var I := 0 to AWebRequest.AllHeaders.Count - 1 do
    LMCPRequest.Headers.AddOrSet(AWebRequest.AllHeaders.KeyNames[I],
      AWebRequest.AllHeaders.ValueFromIndex[I]);
  {$ELSE}
    AMCPRequest.Headers.AddOrSetValue('Cache-Control', AWebRequest.CacheControl);
  if AWebRequest.Cookie <> '' then
    AMCPRequest.Headers.AddOrSetValue('Cookie', AWebRequest.Cookie);
  if AWebRequest.Date > 0 then
    AMCPRequest.Headers.AddOrSetValue('Date', DateToHttpStr(AWebRequest.Date));
  if AWebRequest.Accept <> '' then
    AMCPRequest.Headers.AddOrSetValue('Accept', AWebRequest.Accept);
  if AWebRequest.From <> '' then
    AMCPRequest.Headers.AddOrSetValue('From', AWebRequest.From);
  if AWebRequest.Host <> '' then
    AMCPRequest.Headers.AddOrSetValue('Host', AWebRequest.Host);
  if AWebRequest.IfModifiedSince > 0 then
    AMCPRequest.Headers.AddOrSetValue('If-Modified-Since', DateToHttpStr(AWebRequest.IfModifiedSince));
  if AWebRequest.Referer <> '' then
    AMCPRequest.Headers.AddOrSetValue('Referer', AWebRequest.Referer);
  if AWebRequest.UserAgent <> '' then
    AMCPRequest.Headers.AddOrSetValue('User-Agent', AWebRequest.UserAgent);
  if AWebRequest.ContentEncoding <> '' then
    AMCPRequest.Headers.AddOrSetValue('Content-Encoding', AWebRequest.ContentEncoding);
  if AWebRequest.ContentType <> '' then
    AMCPRequest.Headers.AddOrSetValue('Content-Type', AWebRequest.ContentType);
  if AWebRequest.ContentLength <> 0 then
    AMCPRequest.Headers.AddOrSetValue('Content-Length', AWebRequest.ContentLength.ToString);
  if AWebRequest.ContentVersion <> '' then
    AMCPRequest.Headers.AddOrSetValue('Content-Version', AWebRequest.ContentVersion);
  if AWebRequest.DerivedFrom <> '' then
    AMCPRequest.Headers.AddOrSetValue('Derived-From', AWebRequest.DerivedFrom);
  if AWebRequest.Expires > 0 then
    AMCPRequest.Headers.AddOrSetValue('Expires', DateToHttpStr(AWebRequest.Expires));
  if AWebRequest.Title <> '' then
    AMCPRequest.Headers.AddOrSetValue('Title', AWebRequest.Title);
  {$ENDIF}
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
      ConvertHeaders(Request, ARequest);
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
        raise EMCPTransportException.Create(405, 'WebBroker does not support SSE. Upgrade to Delphi 13.1 or higher');
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
