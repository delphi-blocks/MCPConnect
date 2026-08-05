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
  System.SysUtils, System.Classes, System.Masks, System.Diagnostics,
  Web.HTTPApp,

  MCPConnect.Transport.Base,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server;

resourcestring
  SServerNotFound = 'Server not found';
  SWebBrokerSSENotSupported = 'SSE not supported';

type
  TJRPCDispatcher = class(TComponent, IWebDispatch)
  private
    FDispatchMask: TMask;
    FPathInfo: string;
    FServer: TJRPCServer;
    procedure SetPathInfo(const Value: string);
    procedure SetServer(const Value: TJRPCServer);
    procedure ConvertRequestHeaders(AWebRequest: TWebRequest; AMCPRequest: TMCPTransportRequest);
    procedure ConvertResponseHeaders(AWebResponse: TWebResponse; AMCPResponse: TMCPTransportResponse);
  public
    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(Sender: TObject; AWebRequest: TWebRequest; AWebResponse: TWebResponse): Boolean;
    function DispatchMask: TMask;

    property PathInfo: string read FPathInfo write SetPathInfo;
    property Server: TJRPCServer read FServer write SetServer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFNDEF HAS_WEBBROKER_SSE}
  TWebResponseStream = class
  end;
  {$ENDIF}


  TMCPTransportWriterWebBroker = class(TInterfacedObject, IMCPTransportWriter)
  private
    FResponse: TWebResponse;
    {$IFDEF HAS_WEBBROKER_SSE}
    FSSEStream: TWebResponseStream;
    FPing: TStopwatch;
    {$ENDIF}
    procedure WriteSSEEvent(const AId, AEvent, AValue: string; ARetry: Integer);
  public
    function SSEStream: TWebResponseStream;

    { IMCPTransportWriter }
    procedure Write(const AValue: string; const AEventId: string = ''); overload;
    procedure WriteComment(const AValue: string); overload;
    function Connected: Boolean;
    function SupportsStreaming: Boolean;

    constructor Create(AResponse: TWebResponse);
  end;

implementation

uses
  MCPConnect.Configuration.Session;

const
  PingInterval = 15000;

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

procedure TJRPCDispatcher.ConvertRequestHeaders(AWebRequest: TWebRequest; AMCPRequest: TMCPTransportRequest);
begin
  {$IFDEF HAS_WEBBROKER_REQUEST_HEADERS}
  AWebRequest.AllHeaders.NameValueSeparator := ':';
  for var I := 0 to AWebRequest.AllHeaders.Count - 1 do
    AMCPRequest.AddOrSetHeader(AWebRequest.AllHeaders.KeyNames[I],
      AWebRequest.AllHeaders.ValueFromIndex[I]);
  {$ELSE}
  var LSessionConfig := FServer.GetConfiguration<TSessionConfig>;

  if AWebRequest.CacheControl <> '' then  
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
  if AWebRequest.GetFieldByName(LSessionConfig.GetHeaderName) <> '' then
    AMCPRequest.Headers.AddOrSetValue(LSessionConfig.GetHeaderName, AWebRequest.GetFieldByName(LSessionConfig.GetHeaderName));
  {$ENDIF}
end;

procedure TJRPCDispatcher.ConvertResponseHeaders(AWebResponse: TWebResponse;
  AMCPResponse: TMCPTransportResponse);
begin
  for var pair in AMCPResponse.Headers do
    AWebResponse.CustomHeaders.AddPair(pair.Key, pair.Value);
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

function TJRPCDispatcher.DispatchRequest(Sender: TObject; AWebRequest: TWebRequest; AWebResponse: TWebResponse): Boolean;
var
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create(SServerNotFound);

  var LWriter := TMCPTransportWriterWebBroker.Create(AWebResponse);

  LMcpHandler := TMCPTransportHandler.Create(FServer, LWriter);

  LMcpHandler.SendResponseHeadersProc :=
    procedure (AResponse: TMCPTransportResponse)
    begin
      ConvertResponseHeaders(AWebResponse, AResponse);
    end;

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    begin
      ConvertRequestHeaders(AWebRequest, ARequest);
      ARequest.Command := AWebRequest.Method;
      ARequest.Content := AWebRequest.Content;

      //LogRequest(ARequest);
    end,

    procedure (AResponse: TMCPTransportResponse)
    begin
      ConvertResponseHeaders(AWebResponse, AResponse);

      AWebResponse.StatusCode := AResponse.Code;
      AWebResponse.Content := AResponse.Content;
      AWebResponse.ContentType := AResponse.ContentType;
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

{ TMCPTransportWriterWebBroker }

function TMCPTransportWriterWebBroker.Connected: Boolean;
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  Result := SSEStream.Connected;
  // Periodic ping: workaround because WebBroker sometimes does not detect the
  // client disconnection until a write attempt actually fails on the socket.
  if Result and (FPing.ElapsedMilliseconds >= PingInterval) then
  begin
    WriteComment('ping');
    Result := FSSEStream.Connected;
  end;
  {$ELSE}
  raise EJRPCException.Create(SWebBrokerSSENotSupported);
  {$ENDIF}
end;

constructor TMCPTransportWriterWebBroker.Create(AResponse: TWebResponse);
begin
  inherited Create;
  FResponse := AResponse;
end;

function TMCPTransportWriterWebBroker.SSEStream: TWebResponseStream;
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  if not Assigned(FSSEStream) then
  begin
    FSSEStream := TWebResponseStream.BeginStream(FResponse, 'text/event-stream');
    FPing := TStopwatch.StartNew;
  end;
  Result := FSSEStream;
  {$ELSE}
  raise EJRPCException.Create(SWebBrokerSSENotSupported);
  {$ENDIF}
end;

function TMCPTransportWriterWebBroker.SupportsStreaming: Boolean;
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TMCPTransportWriterWebBroker.Write(const AValue: string; const AEventId: string);
begin
  WriteSSEEvent(AEventId, '', AValue, -1);
end;

procedure TMCPTransportWriterWebBroker.WriteSSEEvent(const AId, AEvent,
  AValue: string; ARetry: Integer);
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  if AId <> '' then
    SSEStream.WriteID(AId);
  if AEvent <> '' then
    SSEStream.WriteEvent(AEvent);

  SSEStream.WriteData(AValue);

  if ARetry > 0 then
    SSEStream.WriteRetry(ARetry);

  SSEStream.EndEvent;
  FPing := TStopwatch.StartNew;
  {$ELSE}
  raise EJRPCException.Create(SWebBrokerSSENotSupported);
  {$ENDIF}
end;

procedure TMCPTransportWriterWebBroker.WriteComment(const AValue: string);
begin
  {$IFDEF HAS_WEBBROKER_SSE}
  SSEStream.WriteComment(AValue);
  SSEStream.EndEvent;
  FPing := TStopwatch.StartNew;
  {$ELSE}
  raise EJRPCException.Create(SWebBrokerSSENotSupported);
  {$ENDIF}
end;

end.
