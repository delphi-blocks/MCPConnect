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

  MCPConnect.Transport.Base,
  MCPConnect.Transport.MediaType,
  MCPConnect.Configuration.Session,
  MCPConnect.Session.Core,
  MCPConnect.JRPC.Server;

type
  TWorkerThread = class;

  EJRPCStdioServerError = class(Exception);

  TJRPCStdioServer = class(TComponent)
  private
    FJRPCServer: TJRPCServer;
    FActive: Boolean;
    FWorker: TWorkerThread;
    procedure SetActive(const Value: Boolean);
    function GetTerminated: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure ProcessRequests;
    procedure StartServer;
    procedure StartServerAndWait;
    procedure StopServer;
    property Active: Boolean read FActive write SetActive;
    property Terminated: Boolean read GetTerminated;

    property JRPCServer: TJRPCServer read FJRPCServer;
  public
    class function CreateMCPServer(AOwner: TComponent): TJRPCStdioServer;
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
    FSession: TMCPSessionBase;
    FSessionConfig: TSessionConfig;
    procedure SetServer(const Value: TJRPCServer);
    procedure HandleRequest(const ARequestContent: string; out AResponseContent: string; AStdOutWriter: TStdOutWriter);
  protected
    procedure Execute; override;
  public
    property Server: TJRPCServer read FServer write SetServer;
  end;

  TMCPTransportWriterStdio = class(TInterfacedObject, IMCPTransportWriter)
  private
    FStdOutWriter: TStdOutWriter;
  public
    { IMCPTransportWriter }
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;

    constructor Create(AStdOutWriter: TStdOutWriter);
  end;


implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  MCPConnect.JRPC.Classes,
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
    if LValue < 0 then
      Exit('');
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

constructor TJRPCStdioServer.Create(AOwner: TComponent);
begin
  inherited;
  FJRPCServer := TJRPCServer.Create(nil);
end;

class function TJRPCStdioServer.CreateMCPServer(AOwner: TComponent): TJRPCStdioServer;
begin
  Result := TJRPCStdioServer.Create(nil);
end;

destructor TJRPCStdioServer.Destroy;
begin
  FJRPCServer.Free;
  inherited;
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

procedure TJRPCStdioServer.StartServer;
begin
  if not FActive then
  begin
    FActive := True;
    FWorker := TWorkerThread.Create(True);
    FWorker.Server := FJRPCServer;
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
  LSessionManager: TMCPSessionManager;
begin
  inherited;

  LSessionManager := FServer.SessionManager as TMCPSessionManager;

  // Create session for this STDIO connection (implicit session per connection)
  if Assigned(FSessionConfig) then
  begin
    FSession := LSessionManager.CreateSession;
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
              HandleRequest(LRequest, LResponse, LWriter);
              if LResponse <> '' then
                LWriter.WriteLine(LResponse);
            except
              on E: Exception do
                LError.WriteLine(E.Message);
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
      LSessionManager.DestroySession(LSessionId);
  end;

  Terminate;
end;

procedure TWorkerThread.HandleRequest(const ARequestContent: string; out AResponseContent: string; AStdOutWriter: TStdOutWriter);
var
  LMcpHandler: IMCPTransportHandler;
  LRes: string;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create('Server not found');

  // Auth??

  LMcpHandler := TMCPTransportHandler.Create(FServer, TMCPTransportWriterStdio.Create(AStdOutWriter));

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Headers.AddOrSetValue('Accept', 'application/json');

      ARequest.Command := 'POST';
      ARequest.Content := ARequestContent;
      ARequest.Accept := TMediaType.TEXT_EVENT_STREAM;
    end,

    procedure (AResponse: TMCPTransportResponse)
    begin
      LRes := AResponse.Content;
    end
  );

  AResponseContent := LRes;
end;

procedure TWorkerThread.SetServer(const Value: TJRPCServer);
begin
  FServer := Value;

  if Assigned(FServer) then
    FSessionConfig := FServer.GetConfiguration<TSessionConfig>;
end;

{ TMCPTransportWriterStdio }

constructor TMCPTransportWriterStdio.Create(AStdOutWriter: TStdOutWriter);
begin
  inherited Create;
  FStdOutWriter := AStdOutWriter;
end;

function TMCPTransportWriterStdio.SupportsStreaming: Boolean;
begin
  Result := True;
end;

function TMCPTransportWriterStdio.Connected: Boolean;
begin
  Result := True;
end;

procedure TMCPTransportWriterStdio.Write(const AValue: string; const AEventId: string);
begin
  FStdOutWriter.WriteLine(RemoveLineBreaks(AValue));
end;

end.
