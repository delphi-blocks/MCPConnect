unit JSON.RPC.Dispacher;

interface

uses
  System.SysUtils, System.Classes, System.Masks,
  Web.HTTPApp,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JSON.RPC,
  JSON.RPC.Invoker,
  JRPC.Server, JRPC.Configuration.Authentication;

type
  TJRPCDispacher = class(TComponent, IWebDispatch)
  private
    FDispachMask: TMask;
    FPathInfo: string;
    FServer: TJRPCServer;
    FAuthTokenConfig: TJRCPAuthTokenConfig;
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
  MCP.Utils;

{ TJRPCDispacher }

function TJRPCDispacher.CheckAuthorization(Request: TWebRequest; Response: TWebResponse): Boolean;
begin
  Result := True;
  if Assigned(FAuthTokenConfig) then
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

constructor TJRPCDispacher.Create(AOwner: TComponent);
begin
  inherited;
  FDispachMask := nil;
  FPathInfo := 'jrpc';
end;

destructor TJRPCDispacher.Destroy;
begin
  FDispachMask.Free;
  inherited;
end;

function TJRPCDispacher.DispatchEnabled: Boolean;
begin
  Result := True;
end;

function TJRPCDispacher.DispatchMask: TMask;
begin
  if not Assigned(FDispachMask) then
  begin
    FDispachMask := TMask.Create(FPathInfo);
  end;
  Result := FDispachMask;
end;

function TJRPCDispacher.DispatchMethodType: TMethodType;
begin
  Result := mtPost;
end;

function TJRPCDispacher.DispatchRequest(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse): Boolean;
var
  LGarbageCollector: IGarbageCollector;
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
begin
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

  LInvokable := TJRPCObjectInvoker.Create(LInstance);
  LInvokable.NeonConfig := LConstructorProxy.NeonConfig;
  if not LInvokable.Invoke(LRequest, LResponse) then
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

procedure TJRPCDispacher.SetPathInfo(const Value: string);
begin
  // If the mask is already created should I raise an exception?
  FPathInfo := Value;
end;

procedure TJRPCDispacher.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;

  FAuthTokenConfig := FServer.GetConfiguration<TJRCPAuthTokenConfig>;
end;

end.
