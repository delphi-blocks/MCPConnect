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
  public
    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(Sender: TObject; Request: TWebRequest; Response: TWebResponse): Boolean;
    function DispatchMask: TMask;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TJRPCDispacher }

constructor TJRPCDispacher.Create(AOwner: TComponent);
begin
  inherited;
  FDispachMask := nil;
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
    FDispachMask := TMask.Create('*');
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
  LInvokable.Invoke(LRequest, LResponse);

  Response.Content := TNeon.ObjectToJSONString(LResponse, JRPCNeonConfig);
  Result := True;
end;

end.
