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

uses
  System.SysUtils, System.Classes, System.Masks,
  Web.HTTPApp,


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

implementation

uses
  MCPConnect.Transport.Base;

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

function TJRPCDispatcher.DispatchRequest(Sender: TObject; Request: TWebRequest; Response: TWebResponse): Boolean;
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LMcpHandler: IMCPTransportHandler;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

  for var I := 0 to Request.AllHeaders.Count - 1 do
    LRequest.Headers.AddOrSet(Request.AllHeaders.KeyNames[I],
      Request.AllHeaders.ValueFromIndex[I]);

  LRequest.Command := Request.Method;
  LRequest.Content := Request.Content;

  LMcpHandler := TMCPTransportHandler.Create(FServer);
  LMcpHandler.HandleRequest(LRequest, LResponse);

  for var I := 0 to LResponse.Headers.Count - 1 do
    Response.CustomHeaders.AddPair(LResponse.Headers.RawHeaders[I].Key,
      LResponse.Headers.RawHeaders[I].Value);

  Response.StatusCode := LResponse.Code;
  Response.Content := LRequest.Content;
  Response.ContentType := LResponse.ContentType;
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
