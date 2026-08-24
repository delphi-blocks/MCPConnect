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
  System.SysUtils, System.Classes, System.Masks, System.DateUtils,
  System.Diagnostics, Web.HTTPApp,

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

/// <summary>
/// Converts a TDateTime value to an HTTP-date string in GMT/UTC format.
/// </summary>
/// <param name="ADateTime">
/// The date and time to convert.
/// </param>
/// <param name="AInputIsUTC">
/// Specifies whether ADateTime is already expressed in UTC.
/// If False, ADateTime is assumed to be local time and is converted to UTC.
/// </param>
/// <returns>
/// The date formatted according to the HTTP-date format,
/// for example: "Mon, 24 Aug 2026 08:30:00 GMT".
/// </returns>
function DateTimeToHTTPDate(ADateTime: TDateTime; AInputIsUTC: Boolean = True): string;
const
  HTTPDateFormat = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');

  if not AInputIsUTC then
    ADateTime := TTimeZone.Local.ToUniversalTime(ADateTime);

  Result := FormatDateTime(HTTPDateFormat, ADateTime, FS);
end;

/// <summary>
/// Converts an HTTP-date string to a TDateTime value.
/// </summary>
/// <param name="AValue">
/// The HTTP-date string to convert, for example:
/// "Mon, 24 Aug 2026 08:30:00 GMT".
/// </param>
/// <param name="AReturnUTC">
/// Specifies whether the returned TDateTime should be expressed in UTC.
/// If False, the parsed UTC date is converted to local time.
/// </param>
/// <returns>
/// The parsed date and time. Returns 0 if the value cannot be converted.
/// </returns>
function HTTPDateToDateTime(const AValue: string; AReturnUTC: Boolean = True): TDateTime;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  FS.DateSeparator := ' ';
  FS.ShortDateFormat := 'ddd, dd mmm yyyy';
  FS.ShortTimeFormat := 'hh:nn:ss';

  if TryStrToDateTime(AValue.Replace(' GMT', ''), Result, FS) then
  begin
    if not AReturnUTC then
      Result := TTimeZone.Local.ToLocalTime(Result);
  end
  else
    Result := 0;
end;

procedure TJRPCDispatcher.ConvertRequestHeaders(AWebRequest: TWebRequest; AMCPRequest: TMCPTransportRequest);
begin
  AMCPRequest.Headers.Clear();
  {$IFDEF HAS_WEBBROKER_REQUEST_HEADERS}
  AWebRequest.AllHeaders.NameValueSeparator := ':';
  for var I := 0 to AWebRequest.AllHeaders.Count - 1 do
    AMCPRequest.Headers.Add(AWebRequest.AllHeaders.KeyNames[I],
      AWebRequest.AllHeaders.ValueFromIndex[I].TrimLeft);
  {$ELSE}
  var LSessionConfig := FServer.GetConfiguration<TSessionConfig>;

  if AWebRequest.CacheControl <> '' then  
    AMCPRequest.SetHeader('Cache-Control', AWebRequest.CacheControl);
  if AWebRequest.Cookie <> '' then
    AMCPRequest.SetHeader('Cookie', AWebRequest.Cookie);
  if AWebRequest.Date > 0 then
    AMCPRequest.SetHeader('Date', DateTimeToHTTPDate(AWebRequest.Date));
  if AWebRequest.Accept <> '' then
    AMCPRequest.SetHeader('Accept', AWebRequest.Accept);
  if AWebRequest.From <> '' then
    AMCPRequest.SetHeader('From', AWebRequest.From);
  if AWebRequest.Host <> '' then
    AMCPRequest.SetHeader('Host', AWebRequest.Host);
  if AWebRequest.IfModifiedSince > 0 then
    AMCPRequest.SetHeader('If-Modified-Since', DateTimeToHTTPDate(AWebRequest.IfModifiedSince));
  if AWebRequest.Referer <> '' then
    AMCPRequest.SetHeader('Referer', AWebRequest.Referer);
  if AWebRequest.UserAgent <> '' then
    AMCPRequest.SetHeader('User-Agent', AWebRequest.UserAgent);
  if AWebRequest.ContentEncoding <> '' then
    AMCPRequest.SetHeader('Content-Encoding', AWebRequest.ContentEncoding);
  if AWebRequest.ContentType <> '' then
    AMCPRequest.SetHeader('Content-Type', AWebRequest.ContentType);
  if AWebRequest.ContentLength <> 0 then
    AMCPRequest.SetHeader('Content-Length', AWebRequest.ContentLength.ToString);
  if AWebRequest.ContentVersion <> '' then
    AMCPRequest.SetHeader('Content-Version', AWebRequest.ContentVersion);
  if AWebRequest.DerivedFrom <> '' then
    AMCPRequest.SetHeader('Derived-From', AWebRequest.DerivedFrom);
  if AWebRequest.Expires > 0 then
    AMCPRequest.SetHeader('Expires', DateTimeToHTTPDate(AWebRequest.Expires));
  if AWebRequest.Title <> '' then
    AMCPRequest.SetHeader('Title', AWebRequest.Title);
  if AWebRequest.GetFieldByName(LSessionConfig.GetHeaderName) <> '' then
    AMCPRequest.SetHeader(LSessionConfig.GetHeaderName, AWebRequest.GetFieldByName(LSessionConfig.GetHeaderName));
  {$ENDIF}
end;

procedure TJRPCDispatcher.ConvertResponseHeaders(AWebResponse: TWebResponse;
  AMCPResponse: TMCPTransportResponse);
begin
  AWebResponse.CustomHeaders.Clear();
  for var pair in AMCPResponse.Headers do
  begin
    if SameText(pair.Key, 'WWW-Authenticate') then
      AWebResponse.WWWAuthenticate := pair.Value
    else if SameText(pair.Key, 'Content-Type') then
      AWebResponse.ContentType := pair.Value
    else if SameText(pair.Key, 'Content-Encoding') then
      AWebResponse.ContentEncoding := pair.Value
    else if SameText(pair.Key, 'Content-Version') then
      AWebResponse.ContentVersion := pair.Value
    else if SameText(pair.Key, 'Server') then
      AWebResponse.Server := pair.Value
    else if SameText(pair.Key, 'Realm') then
      AWebResponse.Realm := pair.Value
    else if SameText(pair.Key, 'Allow') then
      AWebResponse.Allow := pair.Value
    else if SameText(pair.Key, 'Location') then
      AWebResponse.Location := pair.Value
    else if SameText(pair.Key, 'Derived-From') then
      AWebResponse.DerivedFrom := pair.Value
    else if SameText(pair.Key, 'Title') then
      AWebResponse.Title := pair.Value
    else if SameText(pair.Key, 'Date') then
      AWebResponse.Date := HTTPDateToDateTime(pair.Value)
    else if SameText(pair.Key, 'Expires') then
      AWebResponse.Expires := HTTPDateToDateTime(pair.Value)
    else if SameText(pair.Key, 'Last-Modified') then
      AWebResponse.LastModified := HTTPDateToDateTime(pair.Value)
    else
      AWebResponse.CustomHeaders.AddPair(pair.Key, pair.Value);
  end;
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
      // InternalPathInfo, not PathInfo: it is what WebBroker itself matches
      // DispatchMask against, so the path the handler routes on is the one that got
      // the request here - including under ISAPI/CGI, where the two differ. Left
      // unset, every path-dependent check downstream sees an empty URL and the OAuth
      // well-known endpoints can never match.
      ARequest.Url := AWebRequest.InternalPathInfo;
      ARequest.Command := AWebRequest.Method;
      ARequest.Content := AWebRequest.Content;
      ARequest.Protocol := TTransportProtocol.StreamableHTTP;

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
