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
unit MCPConnect.Transport.Stdio;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,

  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.JRPC.Server;

type
  TWorkerThread = class;

  EJRPCStdioServerError = class(Exception)
  end;

  TJRPCStdioServer = class(TComponent)
  private
    FServer: TJRPCServer;
    FActive: Boolean;
    FWorker: TWorkerThread;
    procedure SetServer(const Value: TJRPCServer);
    procedure SetActive(const Value: Boolean);
    function GetTerminated: Boolean;
  public
    procedure BeforeDestruction; override;
    procedure ProcessRequests;
    procedure StartServer;
    procedure StartServerAndWait;
    procedure StopServer;
    property Server: TJRPCServer read FServer write SetServer;
    property Active: Boolean read FActive write SetActive;
    property Terminated: Boolean read GetTerminated;
  end;

  TStdInReader = class(TObject)
  private
    FReader: TStreamReader;
    function GetEndOfStream: Boolean;
  public
    procedure Close;
    function ReadLine: string;
    function ReadToEnd: string;
    property EndOfStream: Boolean read GetEndOfStream;

    constructor Create;
    destructor Destroy; override;
  end;

  TStdOutWriter = class(TObject)
  private
    FWriter: TStreamWriter;
  public
    procedure Close;
    procedure Flush;
    procedure WriteLine(const Value: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TStdErrWriter = class(TObject)
  private
    FWriter: TStreamWriter;
  public
    procedure Close;
    procedure Flush;
    procedure WriteLine(const Value: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TWorkerThread = class(TThread)
  private
    FServer: TJRPCServer;
    FSession: TSessionBase;
    FSessionConfig: TSessionConfig;
    procedure SetServer(const Value: TJRPCServer);
    procedure HandleRequest(const ARequestContent: string; out AResponseContent: string);
  protected
    procedure Execute; override;
  public
    property Server: TJRPCServer read FServer write SetServer;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.Core.Utils,
  MCPConnect.JRPC.Invoker,
  MCPConnect.JRPC.Core;

function StdInHandle: THandle;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdHandle(STD_INPUT_HANDLE);
  {$ENDIF}

  {$IFDEF POSIX}
  Result := 0;
  {$ENDIF}
end;

function StdOutHandle: THandle;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ENDIF}

  {$IFDEF POSIX}
  Result := 1;
  {$ENDIF}
end;

function StdErrHandle: THandle;
begin
  {$IFDEF MSWINDOWS}
  Result := GetStdHandle(STD_ERROR_HANDLE);
  {$ENDIF}

  {$IFDEF POSIX}
  Result := 2;
  {$ENDIF}
end;

function RemoveLineBreaks(const LValue: string): string;
var
  I: Integer;
  LChar: Char;
begin
  SetLength(Result, Length(LValue));
  I := 0;
  for LChar in LValue do
  begin
    if (LChar <> #13) and (LChar <> #10) then
    begin
      Inc(I);
      Result[I] := LChar;
    end;
  end;
  SetLength(Result, I);
end;

{ TStdInReader }

procedure TStdInReader.Close;
begin
  FReader.Close;
end;

constructor TStdInReader.Create;
begin
  inherited;
  FReader := TStreamReader.Create(THandleStream.Create(StdInHandle), TEncoding.Default);
  FReader.OwnStream;
end;

destructor TStdInReader.Destroy;
begin
  FReader.Free;
  inherited;
end;

function TStdInReader.GetEndOfStream: Boolean;
begin
  Result := FReader.EndOfStream;
end;

// Reads all characters until a line break is found (ASCII 10)
// Uses a buffer to improve performance
function TStdInReader.ReadLine: string;
const
  BufferSize = 1024;
var
  LBuffer: TBytes;
  LIndex: Integer;
begin
  LIndex := 0;
  while True do
  begin
    var LValue := FReader.Read;
    if LValue = 10 then
    begin
      Exit(TEncoding.UTF8.GetString(LBuffer, 0, LIndex));
    end;
    if Length(LBuffer) < LIndex + 1 then
      SetLength(LBuffer, Length(LBuffer) + BufferSize);
    LBuffer[LIndex] := LValue;
    Inc(LIndex);
  end;
end;

function TStdInReader.ReadToEnd: string;
begin
  Result := FReader.ReadToEnd;
end;

{ TStdOutWriter }

procedure TStdOutWriter.Close;
begin
  FWriter.Close;
end;

constructor TStdOutWriter.Create;
begin
  inherited;
  FWriter := TStreamWriter.Create(THandleStream.Create(StdOutHandle), TEncoding.Default);
  FWriter.OwnStream;
  FWriter.NewLine := #10;
end;

destructor TStdOutWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TStdOutWriter.Flush;
begin
  FWriter.Flush;
end;

procedure TStdOutWriter.WriteLine(const Value: string);
begin
  FWriter.WriteLine(Value);
  FWriter.Flush;
end;

{ TStdErrWriter }

procedure TStdErrWriter.Close;
begin
  FWriter.Close;
end;

constructor TStdErrWriter.Create;
begin
  inherited;
  FWriter := TStreamWriter.Create(THandleStream.Create(StdErrHandle), TEncoding.Default);
  FWriter.OwnStream;
  FWriter.NewLine := #10;
end;

destructor TStdErrWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TStdErrWriter.Flush;
begin
  FWriter.Flush;
end;

procedure TStdErrWriter.WriteLine(const Value: string);
begin
  FWriter.WriteLine(Value);
end;

{ TJRPCStdioServer }

procedure TJRPCStdioServer.BeforeDestruction;
begin
  inherited;
  StopServer;
end;

function TJRPCStdioServer.GetTerminated: Boolean;
begin
  Result := not Assigned(FWorker) or FWorker.Terminated;
end;

procedure TJRPCStdioServer.ProcessRequests;
begin
  // TODO: Implement synchronous request processing
end;

procedure TJRPCStdioServer.SetActive(const Value: Boolean);
begin
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TJRPCStdioServer.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;
end;

procedure TJRPCStdioServer.StartServer;
begin
  if not FActive then
  begin
    FActive := True;
    FWorker := TWorkerThread.Create(True);
    FWorker.Server := FServer;
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  end;
end;

procedure TJRPCStdioServer.StartServerAndWait;
begin
  StartServer;
  if Assigned(FWorker) then
    FWorker.WaitFor;
end;

procedure TJRPCStdioServer.StopServer;
begin
  if FActive then
  begin
    FActive := False;
    if Assigned(FWorker) then
    begin
      FWorker.Terminate;
      FreeAndNil(FWorker);
    end;
  end;
end;

{ TWorkerThread }

procedure TWorkerThread.Execute;
var
  LReader: TStdInReader;
  LWriter: TStdOutWriter;
  LError: TStdErrWriter;
  LRequest, LResponse: string;
  LSessionId: string;
begin
  inherited;

  // Create session for this STDIO connection (implicit session per connection)
  if Assigned(FSessionConfig) then
  begin
    FSession := TSessionManager.Instance.CreateSession;
    LSessionId := FSession.SessionId;  // Save ID before thread ends
  end;

  try
    LReader := TStdInReader.Create;
    try
      LWriter := TStdOutWriter.Create;
      try
        LError := TStdErrWriter.Create;
        try
          while not LReader.EndOfStream do
          begin
            LRequest := LReader.ReadLine;
            try
              HandleRequest(LRequest, LResponse);
              if LResponse <> '' then
              begin
                LWriter.WriteLine(LResponse);
              end;
            except
              on E: Exception do
              begin
                LError.WriteLine(E.Message);
              end;
            end;
          end;
        finally
          LError.Free;
        end;
      finally
        LWriter.Free;
      end;

    finally
      LReader.Free;
    end;
  finally
    // Destroy session when STDIO connection ends
    // Note: This is done for consistency and explicit resource cleanup.
    // The session would be automatically destroyed by TSessionManager's destructor
    // at application termination, but we clean it up here for good practice.
    if not LSessionId.IsEmpty then
      TSessionManager.Instance.DestroySession(LSessionId);
  end;

  Terminate;
end;

procedure TWorkerThread.HandleRequest(const ARequestContent: string;
  out AResponseContent: string);
var
  LGarbageCollector: IGarbageCollector;
  LJRPCRequest: TJRPCRequest;
  LJRPCResponse: TJRPCResponse;
  LConstructorProxy: TJRPCConstructorProxy;
  LInstance: TObject;
  LInvokable: IJRPCInvokable;
  LContext: TJRPCContext;
  LId: TJRPCID;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

//  if not CheckAuthorization(Request, Response) then
//  begin
//    Response.StatusCode := 403;
//    Response.Content := '';
//    Exit(True);
//  end;

  LGarbageCollector := TGarbageCollector.CreateInstance;

  LJRPCResponse := TJRPCResponse.Create;
  LGarbageCollector.Add(LJRPCResponse);

  try
    LJRPCRequest := TNeon.JSONToObject<TJRPCRequest>(ARequestContent, JRPCNeonConfig);
    LGarbageCollector.Add(LJRPCRequest);
    LId := LJRPCRequest.Id;

    if not TJRPCRegistry.Instance.GetConstructorProxy(LJRPCRequest.Method, LConstructorProxy) then
    begin
      raise EJRPCMethodNotFoundError.CreateFmt('Method "%s" not found', [LJRPCRequest.Method]);
    end;

    LInstance := LConstructorProxy.ConstructorFunc();
    LGarbageCollector.Add(LInstance);

    LContext := TJRPCContext.Create;
    LGarbageCollector.Add(LContext);

    LContext.AddContent(LJRPCRequest);
    LContext.AddContent(LJRPCResponse);
    LContext.AddContent(FServer);

    // Add session to context if available (implicit session per STDIO connection)
    if Assigned(FSession) then
      LContext.AddContent(FSession);

    // Injects the context inside the instance
    LContext.Inject(LInstance);

    LInvokable := TJRPCObjectInvoker.Create(LInstance);
    LInvokable.NeonConfig := LConstructorProxy.NeonConfig;
    if not LInvokable.Invoke(LContext, LJRPCRequest, LJRPCResponse) then
    begin
      raise EJRPCMethodNotFoundError.CreateFmt('Cannot invoke method "%s"', [LJRPCRequest.Method]);
    end;
  except
    on E: Exception do
      TJRPCObjectInvoker.HandleException(E, LId, LJRPCResponse);
  end;

  if LJRPCResponse.IsNotification then
  begin
    AResponseContent := '';
  end
  else
  begin
    AResponseContent := RemoveLineBreaks(TNeon.ObjectToJSONString(LJRPCResponse, JRPCNeonConfig));
  end;
end;

procedure TWorkerThread.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;

  if Assigned(FServer) then
    FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
end;

end.
