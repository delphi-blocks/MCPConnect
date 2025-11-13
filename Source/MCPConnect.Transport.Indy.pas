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
  System.Classes, System.SysUtils, System.IOUtils,
  IdCustomHTTPServer, IdContext, IdGlobal,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.Configuration.Auth,
  MCPConnect.JRPC.Server;

type
  TJRPCIndyBridge = class(TComponent)
  private
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    function CheckAuthorization(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
    procedure SetServer(const Value: TJRPCServer);
  public
    procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    property Server: TJRPCServer read FServer write SetServer;
  end;

  TJRPCIndyServer = class(TIdCustomHTTPServer)
  private
    FServer: TJRPCServer;
    FBridge: TJRPCIndyBridge;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    procedure SetServer(const Value: TJRPCServer);
  public
    property Server: TJRPCServer read FServer write SetServer;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

uses
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Invoker,
  MCPConnect.Core.Utils;

{ TJRPCIndyBridge }

function TJRPCIndyBridge.CheckAuthorization(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if ARequestInfo.RawHeaders.Values['Authorization'] <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if ARequestInfo.Cookies.Cookie[FAuthTokenConfig.CustomHeader, ''].CookieName <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if ARequestInfo.RawHeaders.Values[FAuthTokenConfig.CustomHeader] <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      else
        raise EJSONRPCException.Create('Invalid token location');
    end;
  end;
end;

procedure TJRPCIndyBridge.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LGarbageCollector: IGarbageCollector;
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
  LContext: TJRPCContext;
  LEncoding: IIdTextEncoding;
  LRequestContent: string;
begin
  if not Assigned(FServer) then
    raise EJSONRPCException.Create('Server not found');

  if not CheckAuthorization(AContext, ARequestInfo, AResponseInfo) then
  begin
    AResponseInfo.ResponseNo := 403;
    AResponseInfo.ContentText := '';
    Exit;
  end;

  LGarbageCollector := TGarbageCollector.CreateInstance;

  LResponse := TJRPCResponse.Create;
  LGarbageCollector.Add(LResponse);

  if ARequestInfo.CharSet <> '' then
    LEncoding := IndyTextEncoding(ARequestInfo.CharSet)
  else
    LEncoding := IndyTextEncoding_UTF8;
  ARequestInfo.PostStream.Position := 0;
  LRequestContent := ReadStringFromStream(ARequestInfo.PostStream, -1, LEncoding);
  LRequest := TNeon.JSONToObject<TJRPCRequest>(LRequestContent, JRPCNeonConfig);
  LGarbageCollector.Add(LRequest);

  if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '';
    Exit;
  end;
  LInstance := LConstructorProxy.ConstructorFunc();
  LGarbageCollector.Add(LInstance);

  LContext := TJRPCContext.Create;
  LGarbageCollector.Add(LContext);

  LContext.AddContent(LRequest);
  LContext.AddContent(LResponse);
  LContext.AddContent(FServer);

  // Injects the context inside the instance
  LContext.Inject(LInstance);

  LInvokable := TJRPCObjectInvoker.Create(LInstance);
  LInvokable.NeonConfig := LConstructorProxy.NeonConfig;
  if not LInvokable.Invoke(LContext, LRequest, LResponse) then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '';
    Exit;
  end;

  AResponseInfo.ContentType := 'application/json';
  if LResponse.IsNotification then
  begin
    AResponseInfo.ResponseNo := 204;
    AResponseInfo.ContentText := '';
  end
  else
  begin
    AResponseInfo.ContentText := TNeon.ObjectToJSONString(LResponse, JRPCNeonConfig);
  end;
end;

procedure TJRPCIndyBridge.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;
  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
end;

{ TJRPCIndyServer }

constructor TJRPCIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FServer := TJRPCServer.Create(nil);
  FBridge := TJRPCIndyBridge.Create(nil);
  FBridge.Server := FServer;

  OnParseAuthentication := ParseAuthentication;
  OnCommandGet := FBridge.HandleRequest;
  OnCommandOther := FBridge.HandleRequest;
end;

destructor TJRPCIndyServer.Destroy;
begin
  FServer.Free;
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

end.
