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

interface

{$I 'MCPConnect.inc'}

uses
  System.SysUtils, System.Classes, System.Masks, System.DateUtils,
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
    procedure ConvertHeaders(AWebRequest: TWebRequest;
      var LMCPRequest: TMCPTransportRequest);
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

implementation

function DateToHttpStr(ADate: TDateTime): string;
const
  sDateFormat = '"%s", dd "%s" yyyy hh":"nn":"ss';
begin
  Result := Format(FormatDateTime(sDateFormat + ' "GMT"', ADate), [DayOfWeekStr(ADate), MonthStr(ADate)]);
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
  Result := mtPost;
end;

procedure TJRPCDispatcher.ConvertHeaders(AWebRequest: TWebRequest; var LMCPRequest: TMCPTransportRequest);
begin
  {$IFDEF HAS_WEBBROKER_ALLHEADERS}
  for var I := 0 to AWebRequest.AllHeaders.Count - 1 do
    LMCPRequest.Headers.AddOrSet(AWebRequest.AllHeaders.KeyNames[I],
      AWebRequest.AllHeaders.ValueFromIndex[I]);
  {$ELSE}
    LMCPRequest.Headers.AddOrSet('Cache-Control', AWebRequest.CacheControl);
  if AWebRequest.Cookie <> '' then
    LMCPRequest.Headers.AddOrSet('Cookie', AWebRequest.Cookie);
  if AWebRequest.Date > 0 then
    LMCPRequest.Headers.AddOrSet('Date', DateToHttpStr(AWebRequest.Date));
  if AWebRequest.Accept <> '' then
    LMCPRequest.Headers.AddOrSet('Accept', AWebRequest.Accept);
  if AWebRequest.From <> '' then
    LMCPRequest.Headers.AddOrSet('From', AWebRequest.From);
  if AWebRequest.Host <> '' then
    LMCPRequest.Headers.AddOrSet('Host', AWebRequest.Host);
  if AWebRequest.IfModifiedSince > 0 then
    LMCPRequest.Headers.AddOrSet('If-Modified-Since', DateToHttpStr(AWebRequest.IfModifiedSince));
  if AWebRequest.Referer <> '' then
    LMCPRequest.Headers.AddOrSet('Referer', AWebRequest.Referer);
  if AWebRequest.UserAgent <> '' then
    LMCPRequest.Headers.AddOrSet('User-Agent', AWebRequest.UserAgent);
  if AWebRequest.ContentEncoding <> '' then
    LMCPRequest.Headers.AddOrSet('Content-Encoding', AWebRequest.ContentEncoding);
  if AWebRequest.ContentType <> '' then
    LMCPRequest.Headers.AddOrSet('Content-Type', AWebRequest.ContentType);
  if AWebRequest.ContentLength <> 0 then
    LMCPRequest.Headers.AddOrSet('Content-Length', AWebRequest.ContentLength.ToString);
  if AWebRequest.ContentVersion <> '' then
    LMCPRequest.Headers.AddOrSet('Content-Version', AWebRequest.ContentVersion);
  if AWebRequest.DerivedFrom <> '' then
    LMCPRequest.Headers.AddOrSet('Derived-From', AWebRequest.DerivedFrom);
  if AWebRequest.Expires > 0 then
    LMCPRequest.Headers.AddOrSet('Expires', DateToHttpStr(AWebRequest.Expires));
  if AWebRequest.Title <> '' then
    LMCPRequest.Headers.AddOrSet('Title', AWebRequest.Title);
  {$ENDIF}
end;

function TJRPCDispatcher.DispatchRequest(Sender: TObject; AWebRequest: TWebRequest; AWebResponse: TWebResponse): Boolean;
var
  LMCPRequest: TMCPTransportRequest;
  LMCPResponse: TMCPTransportResponse;
  LMCPHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

  ConvertHeaders(AWebRequest, LMCPRequest);

  LMCPRequest.Command := AWebRequest.Method;
  LMCPRequest.Content := AWebRequest.Content;

  LMCPHandler := TMCPTransportHandler.Create(FServer);
  LMCPHandler.HandleRequest(LMCPRequest, LMCPResponse);

  for var I := 0 to LMCPResponse.Headers.Count - 1 do
    AWebResponse.CustomHeaders.AddPair(LMCPResponse.Headers.RawHeaders[I].Key,
      LMCPResponse.Headers.RawHeaders[I].Value);

  AWebResponse.StatusCode := LMCPResponse.Code;
  AWebResponse.Content := LMCPResponse.Content;
  AWebResponse.ContentType := LMCPResponse.ContentType;
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

end.
