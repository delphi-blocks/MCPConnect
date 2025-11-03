unit JSON.RPC.Dispacher;

interface

uses
  System.SysUtils, System.Classes, System.Masks,
  Web.HTTPApp,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JSON.RPC,
  JSON.RPC.Invoker;

type
  TJRPCDispacher = class(TComponent, IWebDispatch)
  private
    FDispachMask: TMask;
    FPathInfo: string;
    procedure SetPathInfo(const Value: string);
  public
    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(Sender: TObject; Request: TWebRequest; Response: TWebResponse): Boolean;
    function DispatchMask: TMask;

    property PathInfo: string read FPathInfo write SetPathInfo;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TJRPCDispacher }

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
  LGarbageCollector: IJRPCGarbageCollector;
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
begin
  LGarbageCollector := TJRPCGarbageCollector.CreateInstance;

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

end.
