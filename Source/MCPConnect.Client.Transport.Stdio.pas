{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{                                                                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}
unit MCPConnect.Client.Transport.Stdio;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,

  Winapi.Windows,

  JRPC.Core,
  JRPC.Connection;

resourcestring
  SMCPStdioPipeFailed = 'Could not create the stdio pipes: Windows error %d';
  SMCPStdioStartFailed = 'Could not start the stdio server [%s]: Windows error %d';
  SMCPStdioWriteFailed = 'Could not write to the stdio server: Windows error %d';
  SMCPStdioClosed = 'The stdio transport is closed';

type
  /// <summary>
  ///   A child process spoken to over its standard streams: one UTF-8 JSON
  ///   message per line, the MCP stdio transport.
  /// </summary>
  /// <remarks>
  ///   Implements IJRPCMessageTransport, so it is driven by TJRPCConnection:
  ///   the connection's reader thread blocks in Receive, and every line is one
  ///   JSON-RPC message. The child's stderr is drained on a thread of its own
  ///   and discarded - a full pipe would block the child, and stdout is the
  ///   protocol channel.
  ///
  ///   The child runs in a job object of its own, created suspended and
  ///   assigned before its first instruction, so everything it starts - the
  ///   real server behind a "cmd /c" or "npx" wrapper, a helper - is in the
  ///   job too. Closing shuts the child's stdin (EOF ends a well-behaved stdio
  ///   server), waits CloseTimeout for it, then terminates the whole job: a
  ///   survivor anywhere in the tree would keep the stdout and stderr pipes
  ///   open and the reads waiting forever. The job is also set to kill on
  ///   close, so the tree goes with the transport even if this process dies.
  ///   Where a job cannot be used (an enclosing job that forbids nesting),
  ///   only the child itself is terminated, and reads still blocked are
  ///   cancelled rather than waited for.
  ///
  ///   The pipe handles are closed by the destructor, after the connection
  ///   has joined its reader.
  /// </remarks>
  TMCPStdioTransport = class(TInterfacedObject, IJRPCMessageTransport)
  private
    FProcess: THandle;
    FJob: THandle;
    FStdin: THandle;
    FStdout: THandle;
    FStderr: THandle;
    FBuffer: TBytes;
    FSendLock: TCriticalSection;
    FStderrThread: TThread;
    FClosed: Boolean;
    FCloseTimeout: Cardinal;

    function CreateKillOnCloseJob: THandle;
    procedure JoinStderrThread;

    function ReadChunk: Boolean;
    function ExtractLine(out AText: string): Boolean;
    procedure StartDrainStderr;
  public
    /// <summary>
    ///   Starts ACommandLine, with its standard streams redirected to the
    ///   transport. AWorkingDir, when given, is the child's working directory.
    /// </summary>
    /// <remarks>
    ///   The command line is passed to CreateProcess as it is: quote the
    ///   executable path when it contains spaces.
    /// </remarks>
    constructor Create(const ACommandLine: string;
      const AWorkingDir: string = '');
    destructor Destroy; override;

    /// <summary>
    ///   Blocks until the next line arrives and returns it decoded as UTF-8.
    ///   False when the child closed its stdout or the transport was closed.
    /// </summary>
    function Receive(out AText: string): Boolean;

    /// <summary>
    ///   Writes one message as one UTF-8 line. Raises EJRPCClientError when
    ///   the child has closed its input.
    /// </summary>
    procedure Send(const AText: string);

    /// <summary>
    ///   Ends the child and everything it started: stdin closes first, and
    ///   whatever is still running after CloseTimeout is terminated.
    ///   Idempotent.
    /// </summary>
    procedure Close;

    /// <summary>The child's process id, or 0 when it is not running.</summary>
    function ProcessId: Cardinal;

    /// <summary>
    ///   Milliseconds Close gives the child to exit on its own once its stdin
    ///   is closed. Default 5000.
    /// </summary>
    property CloseTimeout: Cardinal read FCloseTimeout write FCloseTimeout;
  end;

implementation

type
  /// <summary>
  ///   Drains the child's stderr so it never fills and blocks the child.
  ///   The bytes are discarded: stdout is the protocol channel.
  /// </summary>
  TMCPStdErrThread = class(TThread)
  private
    FHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: THandle);
  end;

{ TMCPStdErrThread }

constructor TMCPStdErrThread.Create(AHandle: THandle);
begin
  // Create(True) leaves it suspended; the owner calls Start after the
  // constructor returns. Starting it here would make AfterConstruction start
  // it a second time, which raises EThread.
  inherited Create(True);
  FHandle := AHandle;
  FreeOnTerminate := False;
end;

procedure TMCPStdErrThread.Execute;
var
  LBuffer: array[0..4095] of Byte;
  LRead: Cardinal;
begin
  while not Terminated do
  begin
    if not ReadFile(FHandle, LBuffer, SizeOf(LBuffer), LRead, nil) then
      Break;
    if LRead = 0 then
      Break;
  end;
end;

{ TMCPStdioTransport }

constructor TMCPStdioTransport.Create(const ACommandLine: string;
  const AWorkingDir: string);
var
  LSecurity: TSecurityAttributes;
  LStdinRead, LStdinWrite: THandle;
  LStdoutRead, LStdoutWrite: THandle;
  LStderrRead, LStderrWrite: THandle;
  LStartup: TStartupInfo;
  LProcess: TProcessInformation;
  LCommandLine, LWorkingDir: string;
  LCurrentDir: PChar;
  LError: Cardinal;
begin
  inherited Create;

  FSendLock := TCriticalSection.Create;
  FProcess := 0;
  FJob := 0;
  FStdin := 0;
  FStdout := 0;
  FStderr := 0;
  FClosed := False;
  FCloseTimeout := 5000;

  FillChar(LSecurity, SizeOf(LSecurity), 0);
  LSecurity.nLength := SizeOf(LSecurity);
  LSecurity.bInheritHandle := True;

  if not CreatePipe(LStdinRead, LStdinWrite, @LSecurity, 0) then
    raise EJRPCClientError.CreateFmt(SMCPStdioPipeFailed, [GetLastError]);

  if not CreatePipe(LStdoutRead, LStdoutWrite, @LSecurity, 0) then
  begin
    LError := GetLastError;
    CloseHandle(LStdinRead);
    CloseHandle(LStdinWrite);
    raise EJRPCClientError.CreateFmt(SMCPStdioPipeFailed, [LError]);
  end;

  if not CreatePipe(LStderrRead, LStderrWrite, @LSecurity, 0) then
  begin
    LError := GetLastError;
    CloseHandle(LStdinRead);
    CloseHandle(LStdinWrite);
    CloseHandle(LStdoutRead);
    CloseHandle(LStdoutWrite);
    raise EJRPCClientError.CreateFmt(SMCPStdioPipeFailed, [LError]);
  end;

  // The parent's ends must not leak into the child (a grandchild would hold
  // them open and the pipes would never reach EOF).
  SetHandleInformation(LStdinWrite, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(LStdoutRead, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(LStderrRead, HANDLE_FLAG_INHERIT, 0);

  FillChar(LStartup, SizeOf(LStartup), 0);
  LStartup.cb := SizeOf(LStartup);
  LStartup.dwFlags := STARTF_USESTDHANDLES;
  LStartup.hStdInput := LStdinRead;
  LStartup.hStdOutput := LStdoutWrite;
  LStartup.hStdError := LStderrWrite;

  LCommandLine := ACommandLine;
  UniqueString(LCommandLine);

  LWorkingDir := AWorkingDir;
  if LWorkingDir = '' then
    LCurrentDir := nil
  else
    LCurrentDir := PChar(LWorkingDir);

  // Created suspended: the child is put in the job before it runs a single
  // instruction, so nothing it starts can be born outside it.
  if not CreateProcessW(nil, PChar(LCommandLine), nil, nil, True,
    CREATE_NO_WINDOW or CREATE_SUSPENDED, nil, LCurrentDir, LStartup, LProcess) then
  begin
    LError := GetLastError;

    CloseHandle(LStdinRead);
    CloseHandle(LStdinWrite);
    CloseHandle(LStdoutRead);
    CloseHandle(LStdoutWrite);
    CloseHandle(LStderrRead);
    CloseHandle(LStderrWrite);

    raise EJRPCClientError.CreateFmt(SMCPStdioStartFailed,
      [ACommandLine, LError]);
  end;

  // Best effort: without a job (one could not be created, or an enclosing job
  // refuses nesting) Close falls back to terminating the child alone.
  FJob := CreateKillOnCloseJob;
  if (FJob <> 0) and not AssignProcessToJobObject(FJob, LProcess.hProcess) then
  begin
    CloseHandle(FJob);
    FJob := 0;
  end;

  ResumeThread(LProcess.hThread);
  CloseHandle(LProcess.hThread);

  // The child's ends stay only in the child.
  CloseHandle(LStdinRead);
  CloseHandle(LStdoutWrite);
  CloseHandle(LStderrWrite);

  FProcess := LProcess.hProcess;
  FStdin := LStdinWrite;
  FStdout := LStdoutRead;
  FStderr := LStderrRead;

  StartDrainStderr;
end;

destructor TMCPStdioTransport.Destroy;
begin
  Close;

  if FStdout <> 0 then
    CloseHandle(FStdout);
  if FStderr <> 0 then
    CloseHandle(FStderr);
  if FProcess <> 0 then
    CloseHandle(FProcess);
  // Kill-on-close: anything Close could not reach goes with the last handle
  if FJob <> 0 then
    CloseHandle(FJob);

  FSendLock.Free;
  inherited;
end;

function TMCPStdioTransport.CreateKillOnCloseJob: THandle;
var
  LLimits: TJobObjectExtendedLimitInformation;
begin
  Result := CreateJobObject(nil, nil);
  if Result = 0 then
    Exit;

  FillChar(LLimits, SizeOf(LLimits), 0);
  LLimits.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  if not SetInformationJobObject(Result, JobObjectExtendedLimitInformation,
    @LLimits, SizeOf(LLimits)) then
  begin
    CloseHandle(Result);
    Result := 0;
  end;
end;

procedure TMCPStdioTransport.JoinStderrThread;
begin
  if not Assigned(FStderrThread) then
    Exit;

  FStderrThread.Terminate;

  // Once the tree is gone the pipe reaches EOF and the thread ends by itself.
  // A process outside the job (one that broke away, or inherited the pipe
  // from somewhere else) could still hold its write end: then the blocked
  // read is cancelled instead of waited for - repeatedly, since a cancel that
  // lands between two reads has nothing to cancel.
  while WaitForSingleObject(FStderrThread.Handle, 200) <> WAIT_OBJECT_0 do
    CancelSynchronousIo(FStderrThread.Handle);

  FreeAndNil(FStderrThread);
end;

procedure TMCPStdioTransport.StartDrainStderr;
begin
  FStderrThread := TMCPStdErrThread.Create(FStderr);
  FStderrThread.Start;
end;

function TMCPStdioTransport.ExtractLine(out AText: string): Boolean;
var
  LIndex, LEnd, LRemaining: Integer;
  LLine: TBytes;
begin
  AText := '';
  LIndex := -1;

  for var I := 0 to High(FBuffer) do
    if FBuffer[I] = 10 then
    begin
      LIndex := I;
      Break;
    end;

  if LIndex < 0 then
    Exit(False);

  LEnd := LIndex;
  if (LEnd > 0) and (FBuffer[LEnd - 1] = 13) then
    Dec(LEnd);

  if LEnd > 0 then
  begin
    SetLength(LLine, LEnd);
    Move(FBuffer[0], LLine[0], LEnd);
    AText := TEncoding.UTF8.GetString(LLine);
  end;

  LRemaining := Length(FBuffer) - (LIndex + 1);
  if LRemaining > 0 then
    Move(FBuffer[LIndex + 1], FBuffer[0], LRemaining);
  SetLength(FBuffer, LRemaining);

  Result := True;
end;

function TMCPStdioTransport.ReadChunk: Boolean;
var
  LChunk: array[0..4095] of Byte;
  LRead, LOld: Cardinal;
begin
  if FStdout = 0 then
    Exit(False);

  if not ReadFile(FStdout, LChunk, SizeOf(LChunk), LRead, nil) then
    Exit(False);
  if LRead = 0 then
    Exit(False);

  LOld := Length(FBuffer);
  SetLength(FBuffer, LOld + LRead);
  Move(LChunk, FBuffer[LOld], LRead);

  Result := True;
end;

function TMCPStdioTransport.Receive(out AText: string): Boolean;
begin
  AText := '';

  if FClosed then
    Exit(False);

  while True do
  begin
    if ExtractLine(AText) then
      Exit(True);
    if not ReadChunk then
      Exit(False);
  end;
end;

procedure TMCPStdioTransport.Send(const AText: string);
var
  LBytes: TBytes;
  LWritten: Cardinal;
begin
  if FClosed or (FStdin = 0) then
    raise EJRPCClientError.Create(SMCPStdioClosed);

  LBytes := TEncoding.UTF8.GetBytes(AText + #10);

  FSendLock.Enter;
  try
    if not WriteFile(FStdin, LBytes[0], Length(LBytes), LWritten, nil) or
      (LWritten <> Cardinal(Length(LBytes))) then
      raise EJRPCClientError.CreateFmt(SMCPStdioWriteFailed, [GetLastError]);
  finally
    FSendLock.Leave;
  end;
end;

procedure TMCPStdioTransport.Close;
begin
  if FClosed then
    Exit;

  FClosed := True;

  // EOF ends a well-behaved stdio server; a stuck one is terminated.
  if FStdin <> 0 then
  begin
    CloseHandle(FStdin);
    FStdin := 0;
  end;

  // The grace period, for a server that ends on EOF
  if FProcess <> 0 then
    WaitForSingleObject(FProcess, FCloseTimeout);

  // Then whatever is left of the tree. Not only a stuck child: behind a
  // wrapper the direct child may be "cmd" while the real server lives on as
  // its child, holding the write ends of stdout and stderr - terminating the
  // wrapper alone would leave the reads waiting on it forever.
  if FJob <> 0 then
    TerminateJobObject(FJob, 1)
  else if (FProcess <> 0) and (WaitForSingleObject(FProcess, 0) <> WAIT_OBJECT_0) then
    TerminateProcess(FProcess, 1);

  if FProcess <> 0 then
    WaitForSingleObject(FProcess, 5000);

  JoinStderrThread;

  // The connection's reader may be blocked on stdout just the same; with the
  // tree gone it sees EOF, and this covers a writer outside the job.
  if FStdout <> 0 then
    CancelIoEx(FStdout, nil);
end;

function TMCPStdioTransport.ProcessId: Cardinal;
begin
  Result := GetProcessId(FProcess);
end;

end.
