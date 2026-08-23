unit CppTestClassesBridge;

interface

uses
  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types;

type
  TIntegerQueueHandle = Pointer;
  TMCPQueueHandle = Pointer;

function CreateIntegerQueue(AQueueDepth: Integer): TIntegerQueueHandle;
procedure FreeIntegerQueue(AQueue: TIntegerQueueHandle);
procedure IntegerQueueEnqueue(AQueue: TIntegerQueueHandle; AValue: Integer);
function IntegerQueueCount(AQueue: TIntegerQueueHandle): NativeInt;

function CreateMCPQueue(AQueueDepth: Integer): TMCPQueueHandle;
procedure FreeMCPQueue(AQueue: TMCPQueueHandle);
procedure MCPQueueEnqueue(AQueue: TMCPQueueHandle; AValue: TJRPCNotification);
function MCPQueueDequeue(AQueue: TMCPQueueHandle): TJRPCNotification;
function MCPQueueCount(AQueue: TMCPQueueHandle): NativeInt;

procedure SetJRPCErrorDetails(AError: TJRPCErrorDetails; ACode: Integer;
  const AMessage: string);
procedure SetPromptsListChanged(AResult: TInitializeResult; AValue: Boolean);
procedure SetRootsListChanged(AParams: TInitializeParams; AValue: Boolean);

implementation

uses
  System.Generics.Collections,
  System.SysUtils;

type
  TIntegerQueue = TThreadedQueue<Integer>;
  TMCPQueue = TMCPMessageQueueBase<TJRPCNotification>;

function IntegerQueueFromHandle(AQueue: TIntegerQueueHandle): TIntegerQueue;
begin
  if not Assigned(AQueue) then
    raise EArgumentNilException.Create('AQueue');
  Result := TIntegerQueue(AQueue);
end;

function MCPQueueFromHandle(AQueue: TMCPQueueHandle): TMCPQueue;
begin
  if not Assigned(AQueue) then
    raise EArgumentNilException.Create('AQueue');
  Result := TMCPQueue(AQueue);
end;

function CreateIntegerQueue(AQueueDepth: Integer): TIntegerQueueHandle;
begin
  Result := TIntegerQueue.Create(AQueueDepth);
end;

procedure FreeIntegerQueue(AQueue: TIntegerQueueHandle);
begin
  if Assigned(AQueue) then
    TIntegerQueue(AQueue).Free;
end;

procedure IntegerQueueEnqueue(AQueue: TIntegerQueueHandle; AValue: Integer);
begin
  IntegerQueueFromHandle(AQueue).Enqueue(AValue);
end;

function IntegerQueueCount(AQueue: TIntegerQueueHandle): NativeInt;
begin
  Result := IntegerQueueFromHandle(AQueue).Count;
end;

function CreateMCPQueue(AQueueDepth: Integer): TMCPQueueHandle;
begin
  Result := TMCPQueue.Create(AQueueDepth);
end;

procedure FreeMCPQueue(AQueue: TMCPQueueHandle);
begin
  if Assigned(AQueue) then
    TMCPQueue(AQueue).Free;
end;

procedure MCPQueueEnqueue(AQueue: TMCPQueueHandle; AValue: TJRPCNotification);
begin
  MCPQueueFromHandle(AQueue).Enqueue(AValue);
end;

function MCPQueueDequeue(AQueue: TMCPQueueHandle): TJRPCNotification;
begin
  Result := MCPQueueFromHandle(AQueue).Dequeue;
end;

function MCPQueueCount(AQueue: TMCPQueueHandle): NativeInt;
begin
  Result := MCPQueueFromHandle(AQueue).Count;
end;

procedure SetJRPCErrorDetails(AError: TJRPCErrorDetails; ACode: Integer;
  const AMessage: string);
begin
  if not Assigned(AError) then
    raise EArgumentNilException.Create('AError');
  AError.Code := ACode;
  AError.Message := AMessage;
end;

procedure SetPromptsListChanged(AResult: TInitializeResult; AValue: Boolean);
begin
  if not Assigned(AResult) then
    raise EArgumentNilException.Create('AResult');
  AResult.Capabilities.Prompts.ListChanged := AValue;
end;

procedure SetRootsListChanged(AParams: TInitializeParams; AValue: Boolean);
begin
  if not Assigned(AParams) then
    raise EArgumentNilException.Create('AParams');
  AParams.Capabilities.Roots.ListChanged := AValue;
end;

end.
