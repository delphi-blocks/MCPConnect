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
  System.SysUtils, System.Classes, System.IOUtils, System.JSON, System.SyncObjs,

  Logify,
  JRPC.Server,
  MCPConnect.MCP.Server,
  MCPConnect.Transport.Base,
  MCPConnect.Transport.MediaType;

resourcestring
  SServerNotFound = 'Server not found';
  SStdioInputClosed = 'The client closed the standard input';

type
  TWorkerThread = class;

  EJRPCStdioServerError = class(Exception);

  TMCPStdioServer = class(TComponent)
  private
    FMCPServer: TMCPServer;
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

    property MCPServer: TMCPServer read FMCPServer;
  public
    class function CreateMCPServer(AOwner: TComponent): TMCPStdioServer;
  end;

  /// <summary>
  ///   Reads the JSON-RPC messages from the standard input, one line each.
  /// </summary>
  /// <remarks>
  ///   Stdin carries UTF-8, so the bytes are taken from the handle as they are
  ///   and decoded only once a whole line has been read. Decoding them one at a
  ///   time (what a TStreamReader does) breaks every multi-byte character.
  /// </remarks>
  TStdInReader = class(TObject)
  private const
    /// <summary>Bytes taken from the handle in a single read.</summary>
    CHUNK_SIZE = 4096;
    /// <summary>Bytes added to the line buffer whenever it runs out of room.</summary>
    LINE_GROWTH = 1024;
    LF = 10;
    CR = 13;
  private
    FStream: TStream;
    FChunk: TBytes;
    FChunkCount: Integer;
    FChunkPos: Integer;
    FEndOfStream: Boolean;
    function GetEndOfStream: Boolean;
    /// <summary>
    ///   Makes sure at least one byte is available, reading from the handle when
    ///   the chunk has been consumed. Blocks until stdin has something to give.
    /// </summary>
    /// <returns>False once the stream is exhausted.</returns>
    function FillChunk: Boolean;
    /// <summary>Takes the next byte out of the chunk.</summary>
    /// <returns>False once the stream is exhausted.</returns>
    function NextByte(out AByte: Byte): Boolean;
  public
    procedure Close;
    function ReadLine: string;
    function ReadToEnd: string;
    property EndOfStream: Boolean read GetEndOfStream;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Writes one line at a time to a standard handle, always as UTF-8.
  /// </summary>
  /// <remarks>
  ///   The bytes go to the handle without passing through a codepage: the
  ///   protocol is UTF-8, while converting to the ANSI codepage of the machine
  ///   raises an EEncodingError on every character it cannot represent (an
  ///   arrow, a dash, an emoji) and mangles several of the ones it can.
  ///
  ///   Thread safe: requests are handled concurrently, and every one of them
  ///   writes here. A line goes out whole, under a lock, so two messages never
  ///   interleave inside a line - while whole messages of different requests
  ///   and subscriptions may follow one another in any order, which stdio
  ///   allows since each carries its own id.
  /// </remarks>
  TStdWriter = class(TObject)
  private const
    LF = 10;
  private
    FStream: TStream;
    FLock: TObject;
  protected
    constructor Create(AHandle: THandle);
  public
    destructor Destroy; override;

    procedure Close;
    /// <summary>
    ///   Kept for the callers: every line is handed to the handle as it is
    ///   written, so there is nothing left to push out.
    /// </summary>
    procedure Flush;
    procedure WriteLine(const AValue: string);
  end;

  TStdOutWriter = class(TStdWriter)
  private
    FHungUp: Boolean;
  public
    constructor Create;

    /// <summary>
    ///   Records that the client closed its end: stdin reached EOF.
    /// </summary>
    procedure HangUp;

    /// <summary>
    ///   Whether the client closed its end. Stdout may well still be read, but
    ///   a client that sends nothing more has gone, and a stream held open for
    ///   it is held for nobody.
    /// </summary>
    property HungUp: Boolean read FHungUp;
  end;

  TStdErrWriter = class(TStdWriter)
  public
    constructor Create;
  end;

  /// <summary>
  ///   The reader of the connection: takes each line off stdin and hands it to
  ///   a thread of its own, so that a request that takes its time - a long
  ///   tool, a "subscriptions/listen" held open - does not hold up the next.
  /// </summary>
  /// <remarks>
  ///   A "notifications/cancelled" is the exception, handled on this thread at
  ///   once: queued behind the request it names, it would arrive too late.
  ///
  ///   Ordering: the next line is read only once the previous request has been
  ///   filed in FInFlight (TMCPTransportHandler.OnDispatching), so a client
  ///   that cancels a request it has just sent always finds it there.
  ///
  ///   At the end of stdin the client is gone: every request in flight is
  ///   cancelled - a listen among them closes its subscription abruptly - and
  ///   the reader waits for all of them to end before freeing what they write
  ///   to.
  /// </remarks>
  TWorkerThread = class(TThread)
  private
    FServer: TMCPServer;

    /// <summary>
    ///   The requests in flight on this connection - stdio is one client, so
    ///   one map - where a "notifications/cancelled" finds the one it names.
    /// </summary>
    FInFlight: TMCPInFlightRequests;

    /// <summary>
    ///   The registry scope of this connection's listen streams: one client,
    ///   so a second listen under an id already listening is refused.
    /// </summary>
    FListenScope: string;

    /// <summary>
    ///   How many requests are being handled, and the event set whenever that
    ///   count is zero. Guarded by FActiveLock.
    /// </summary>
    FActive: Integer;
    FActiveLock: TObject;
    FIdle: TEvent;

    procedure BeginDispatch;
    procedure EndDispatch;

    /// <summary>
    ///   Hands ALine to a thread of its own, and returns once the request is
    ///   filed and about to run - or has already ended.
    /// </summary>
    procedure DispatchLine(const ALine: string; AWriter: TStdOutWriter; AError: TStdErrWriter);

    /// <summary>
    ///   Whether ALine is a "notifications/cancelled", which the reader handles
    ///   itself rather than dispatching.
    /// </summary>
    class function IsCancellation(const ALine: string): Boolean; static;

    procedure HandleRequest(const ARequestContent: string; out AResponseContent: string;
      AStdOutWriter: TStdOutWriter; const AOnDispatching: TProc);
  protected
    procedure Execute; override;
  public
    property Server: TMCPServer read FServer write FServer;
  end;

  /// <summary>
  ///   Writes each message as one JSON line on stdout.
  /// </summary>
  /// <remarks>
  ///   WriteComment keeps the base class's no-op: a comment is an SSE concept,
  ///   and on stdio it would be a line the client cannot parse as JSON-RPC.
  ///   A listen stream therefore gets no keep-alive here, and needs none: there
  ///   is no intermediary to time it out, and the end of stdin is what says
  ///   the client has gone (Connected).
  /// </remarks>
  TMCPTransportWriterStdio = class(TMCPTransportWriter)
  private
    FStdOutWriter: TStdOutWriter;
  public
    procedure Write(const AValue: string); override;
    function Connected: Boolean; override;
    function SupportsStreaming: Boolean; override;

    constructor Create(AStdOutWriter: TStdOutWriter);
  end;


implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  JRPC.Core,
  MCPConnect.MCP.Types.Notifications,
  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

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
  FEndOfStream := True;
  FChunkCount := 0;
  FChunkPos := 0;
  FreeAndNil(FStream);
end;

constructor TStdInReader.Create;
begin
  inherited;
  FStream := THandleStream.Create(StdInHandle);
  SetLength(FChunk, CHUNK_SIZE);
end;

destructor TStdInReader.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TStdInReader.FillChunk: Boolean;
var
  LRead: Integer;
begin
  if FChunkPos < FChunkCount then
    Exit(True);

  if FEndOfStream or not Assigned(FStream) then
    Exit(False);

  FChunkPos := 0;
  FChunkCount := 0;

  LRead := FStream.Read(FChunk[0], Length(FChunk));
  if LRead <= 0 then
  begin
    FEndOfStream := True;
    Exit(False);
  end;

  FChunkCount := LRead;
  Result := True;
end;

function TStdInReader.GetEndOfStream: Boolean;
begin
  Result := not FillChunk();
end;

function TStdInReader.NextByte(out AByte: Byte): Boolean;
begin
  if not FillChunk() then
    Exit(False);

  AByte := FChunk[FChunkPos];
  Inc(FChunkPos);
  Result := True;
end;

// Reads all bytes until a line break is found (ASCII 10) and decodes them as
// UTF-8. A CR right before the LF belongs to the line terminator, not to the
// message, so it is dropped. A last line without its LF is returned as it is.
function TStdInReader.ReadLine: string;
var
  LBuffer: TBytes;
  LByte: Byte;
  LIndex: Integer;
begin
  LIndex := 0;
  while NextByte(LByte) do
  begin
    if LByte = LF then
    begin
      if (LIndex > 0) and (LBuffer[LIndex - 1] = CR) then
        Dec(LIndex);
      Exit(TEncoding.UTF8.GetString(LBuffer, 0, LIndex));
    end;

    if Length(LBuffer) < LIndex + 1 then
      SetLength(LBuffer, Length(LBuffer) + LINE_GROWTH);
    LBuffer[LIndex] := LByte;
    Inc(LIndex);
  end;

  Result := TEncoding.UTF8.GetString(LBuffer, 0, LIndex);
end;

function TStdInReader.ReadToEnd: string;
var
  LBuffer: TBytes;
  LByte: Byte;
  LIndex: Integer;
begin
  LIndex := 0;
  while NextByte(LByte) do
  begin
    if Length(LBuffer) < LIndex + 1 then
      SetLength(LBuffer, Length(LBuffer) + LINE_GROWTH);
    LBuffer[LIndex] := LByte;
    Inc(LIndex);
  end;

  Result := TEncoding.UTF8.GetString(LBuffer, 0, LIndex);
end;

{ TStdWriter }

procedure TStdWriter.Close;
begin
  TMonitor.Enter(FLock);
  try
    FreeAndNil(FStream);
  finally
    TMonitor.Exit(FLock);
  end;
end;

constructor TStdWriter.Create(AHandle: THandle);
begin
  inherited Create;
  FLock := TObject.Create;
  FStream := THandleStream.Create(AHandle);
end;

destructor TStdWriter.Destroy;
begin
  FStream.Free;
  FLock.Free;
  inherited;
end;

procedure TStdWriter.Flush;
begin
end;

procedure TStdWriter.WriteLine(const AValue: string);
var
  LBytes: TBytes;
  LLength: Integer;
begin
  // Encoded outside the lock, written inside it, in one call: the line is the
  // unit a reader on the other end parses, so it must never be split
  LBytes := TEncoding.UTF8.GetBytes(AValue);
  LLength := Length(LBytes);
  SetLength(LBytes, LLength + 1);
  LBytes[LLength] := LF;

  TMonitor.Enter(FLock);
  try
    if Assigned(FStream) then
      FStream.WriteBuffer(LBytes[0], LLength + 1);
  finally
    TMonitor.Exit(FLock);
  end;
end;

{ TStdOutWriter }

constructor TStdOutWriter.Create;
begin
  inherited Create(StdOutHandle);
end;

procedure TStdOutWriter.HangUp;
begin
  FHungUp := True;
end;

{ TStdErrWriter }

constructor TStdErrWriter.Create;
begin
  inherited Create(StdErrHandle);
end;

{ TMCPStdioServer }

procedure TMCPStdioServer.BeforeDestruction;
begin
  inherited;
  StopServer;
end;

constructor TMCPStdioServer.Create(AOwner: TComponent);
begin
  inherited;
  FMCPServer := TMCPServer.Create(nil);
end;

class function TMCPStdioServer.CreateMCPServer(AOwner: TComponent): TMCPStdioServer;
begin
  Result := TMCPStdioServer.Create(nil);
end;

destructor TMCPStdioServer.Destroy;
begin
  FMCPServer.Free;
  inherited;
end;

function TMCPStdioServer.GetTerminated: Boolean;
begin
  Result := not Assigned(FWorker) or FWorker.Terminated;
end;

procedure TMCPStdioServer.ProcessRequests;
begin
  // TODO: Implement synchronous request processing
end;

procedure TMCPStdioServer.SetActive(const Value: Boolean);
begin
  if Value then
    StartServer
  else
    StopServer;
end;

procedure TMCPStdioServer.StartServer;
begin
  if not FActive then
  begin
    FActive := True;
    FWorker := TWorkerThread.Create(True);
    FWorker.Server := FMCPServer;
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  end;
end;

procedure TMCPStdioServer.StartServerAndWait;
begin
  StartServer;
  if Assigned(FWorker) then
    FWorker.WaitFor;
end;

procedure TMCPStdioServer.StopServer;
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
begin
  inherited;
  try
    FInFlight := TMCPInFlightRequests.Create;
    FListenScope := TGUID.NewGuid.ToString;
    FActive := 0;
    FActiveLock := TObject.Create;
    FIdle := TEvent.Create(nil, True, True, '');
    LReader := TStdInReader.Create;
    try
      LWriter := TStdOutWriter.Create;
      try
        LError := TStdErrWriter.Create;
        try
          try
            while not LReader.EndOfStream do
            begin
              LRequest := LReader.ReadLine;
              try
                if IsCancellation(LRequest) then
                begin
                  // At once, on this thread: the request it names is running
                  // on another, and a cancellation that waited its turn would
                  // reach it when it no longer mattered
                  HandleRequest(LRequest, LResponse, LWriter, nil);
                  if LResponse <> '' then
                    LWriter.WriteLine(LResponse);
                end
                else
                  DispatchLine(LRequest, LWriter, LError);
              except
                on E: Exception do
                  LError.WriteLine(E.Message);
              end;
            end;
          finally
            // The end of stdin is the client going away. Nothing still running
            // can deliver anything, and a listen would wait for ever on a queue
            // nobody fills: everything in flight is cancelled - a listen closes
            // its subscription abruptly - and this thread waits until they have
            // all ended, since they write through what it is about to free.
            LWriter.HangUp;
            FInFlight.CancelAll(SStdioInputClosed);
            FIdle.WaitFor(INFINITE);
          end;
        finally
          LError.Free;
        end;
      finally
        LWriter.Free;
      end;
    finally
      LReader.Free;
      FreeAndNil(FIdle);
      FreeAndNil(FActiveLock);
      FreeAndNil(FInFlight);
    end;

    Terminate;
  except
    on E: Exception do
    begin
      logger.LogError('TWorkerThread - %s: %s', [E.ClassName, E.Message]);
      raise;
    end;
  end;
end;

procedure TWorkerThread.BeginDispatch;
begin
  TMonitor.Enter(FActiveLock);
  try
    Inc(FActive);
    FIdle.ResetEvent;
  finally
    TMonitor.Exit(FActiveLock);
  end;
end;

procedure TWorkerThread.EndDispatch;
begin
  TMonitor.Enter(FActiveLock);
  try
    Dec(FActive);
    if FActive = 0 then
      FIdle.SetEvent;
  finally
    TMonitor.Exit(FActiveLock);
  end;
end;

procedure TWorkerThread.DispatchLine(const ALine: string; AWriter: TStdOutWriter; AError: TStdErrWriter);
var
  LStarted: TEvent;
  LSignalled: Integer;
  LSignal: TProc;
  LThread: TThread;
begin
  // A method of its own, not a block of Execute: the anonymous methods below
  // capture these locals, and captured locals of a loop are one variable shared
  // by every iteration. Here each line gets its own.
  LSignalled := 0;
  LStarted := TEvent.Create(nil, True, False, '');
  try
    // Signalled once the request is filed (OnDispatching) or, should it never
    // get that far, once it has ended - whichever comes first, and only the
    // first touches the event: by the second, this method may have freed it
    LSignal :=
      procedure
      begin
        if TInterlocked.CompareExchange(LSignalled, 1, 0) = 0 then
          LStarted.SetEvent;
      end;

    BeginDispatch;
    try
      LThread := TThread.CreateAnonymousThread(
        procedure
        var
          LResponse: string;
        begin
          try
            try
              HandleRequest(ALine, LResponse, AWriter, LSignal);
              if LResponse <> '' then
                AWriter.WriteLine(LResponse);
            except
              on E: Exception do
                AError.WriteLine(E.Message);
            end;
          finally
            LSignal();
            // Both anonymous methods live on one frame, and LSignal is a field
            // of it holding the frame itself: cleared, or the frame never goes
            LSignal := nil;
            // Last: once the count reaches zero the reader may free the writers
            EndDispatch;
          end;
        end);
      LThread.Start;
    except
      // No thread, so nobody else will clear it
      LSignal := nil;
      EndDispatch;
      raise;
    end;

    LStarted.WaitFor(INFINITE);
  finally
    LStarted.Free;
  end;
end;

class function TWorkerThread.IsCancellation(const ALine: string): Boolean;
var
  LJSON: TJSONValue;
  LMethod: TJSONValue;
begin
  // The cheap test first: most lines are not one, and parsing every line twice
  // would be paying for the rare case on every message
  if not ALine.Contains(MCP_NOTIFY_CANCELLED) then
    Exit(False);

  LJSON := TJSONObject.ParseJSONValue(ALine);
  try
    Result := False;
    if LJSON is TJSONObject then
    begin
      LMethod := TJSONObject(LJSON).GetValue('method');
      Result := (LMethod is TJSONString) and (TJSONString(LMethod).Value = MCP_NOTIFY_CANCELLED) and
        not Assigned(TJSONObject(LJSON).GetValue('id'));
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TWorkerThread.HandleRequest(const ARequestContent: string; out AResponseContent: string;
  AStdOutWriter: TStdOutWriter; const AOnDispatching: TProc);
var
  LMcpHandler: IMCPTransportHandler;
  LRes: string;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create(SServerNotFound);

  // Auth??

  var LHandler := TMCPTransportHandler.Create(FServer, TMCPTransportWriterStdio.Create(AStdOutWriter));
  LHandler.InFlight := FInFlight;
  LHandler.ListenScope := FListenScope;
  LHandler.OnDispatching := AOnDispatching;
  LMcpHandler := LHandler;

  LMcpHandler.ProcessRequest(

    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.SetHeader('Accept', 'application/json');

      ARequest.Command := 'POST';
      ARequest.Content := ARequestContent;
      ARequest.Accept := TMediaType.TEXT_EVENT_STREAM;
      ARequest.Protocol := TTransportProtocol.Stdio;
    end,

    procedure (AResponse: TMCPTransportResponse)
    begin
      LRes := AResponse.Content;
    end
  );

  AResponseContent := LRes;
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
  // Stdio has no socket to ask: the client is there until it closes stdin
  Result := not FStdOutWriter.HungUp;
end;

procedure TMCPTransportWriterStdio.Write(const AValue: string);
begin
  FStdOutWriter.WriteLine(RemoveLineBreaks(AValue));
end;

end.
