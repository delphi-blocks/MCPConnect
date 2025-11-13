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

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Invoker,
  MCPConnect.JRPC.Server,

  MCPConnect.Configuration.Auth;

type
  TJRPCDispatcher = class(TComponent, IWebDispatch)
  private
    FDispatchMask: TMask;
    FPathInfo: string;
    FServer: TJRPCServer;
    FAuthTokenConfig: TAuthTokenConfig;
    procedure SetPathInfo(const Value: string);
    procedure SetServer(const Value: TJRPCServer);
    function CheckAuthorization(Request: TWebRequest; Response: TWebResponse): Boolean;
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
  MCPConnect.Core.Utils;

{ TJRPCDispatcher }

function TJRPCDispatcher.CheckAuthorization(Request: TWebRequest; Response: TWebResponse): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) and (FAuthTokenConfig.Token <> '') then
  begin
    case FAuthTokenConfig.Location of
      TAuthTokenLocation.Bearer:
      begin
        if Request.Authorization <> 'Bearer ' + FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Cookie:
      begin
        if Request.CookieFields.Values[FAuthTokenConfig.CustomHeader] <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      TAuthTokenLocation.Header:
      begin
        if Request.GetFieldByName(FAuthTokenConfig.CustomHeader) <> FAuthTokenConfig.Token then
          Exit(False);
      end;

      else
        raise EJSONRPCException.Create('Invalid token location');
    end;
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
  Result := mtPost;
end;

function TJRPCDispatcher.DispatchRequest(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse): Boolean;
var
  LGarbageCollector: IGarbageCollector;
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
  LContext: TJRPCContext;
begin
  if not Assigned(FServer) then
    raise EJSONRPCException.Create('Server not found');

  if not CheckAuthorization(Request, Response) then
  begin
    Response.StatusCode := 403;
    Response.Content := '';
    Exit(True);
  end;

  LGarbageCollector := TGarbageCollector.CreateInstance;

  LResponse := TJRPCResponse.Create;
  LGarbageCollector.Add(LResponse);

  LRequest := TNeon.JSONToObject<TJRPCRequest>(Request.Content, JRPCNeonConfig);
  LGarbageCollector.Add(LRequest);

  if not TJRPCRegistry.Instance.GetConstructorProxy(LRequest.Method, LConstructorProxy) then
  begin
    Exit(False);
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
    Exit(False);
  end;

  Response.ContentType := 'application/json';
  if LResponse.IsNotification then
  begin
    Response.StatusCode := 204;
    Response.Content := '';
  end
  else
  begin
    Response.Content := TNeon.ObjectToJSONString(LResponse, JRPCNeonConfig);
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

  FAuthTokenConfig := FServer.GetConfiguration<TAuthTokenConfig>;
end;

end.
