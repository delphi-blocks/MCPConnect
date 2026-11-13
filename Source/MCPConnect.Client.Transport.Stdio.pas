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
  ///   Closing shuts the child's stdin (EOF ends a well-behaved stdio server),
  ///   waits up to five seconds, then terminates it. The pipe handles are
  ///   closed by the destructor, after the connection has joined its reader.
  /// </remarks>
  TMCPStdioTransport = class(TInterfacedObject, IJRPCMessageTransport)
  private
    FProcess: THandle;
    FStdin: THandle;
    FStdout: THandle;
    FStderr: THandle;
    FBuffer: TBytes;
    FSendLock: TCriticalSection;
    FStderrThread: TThread;
    FClosed: Boolean;

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
    ///   Ends the child: stdin closes first, then a termination after the
    ///   grace period. Idempotent.
    /// </summary>
    procedure Close;

    /// <summary>The child's process id, or 0 when it is not running.</summary>
    function ProcessId: Cardinal;
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
  FStdin := 0;
  FStdout := 0;
  FStderr := 0;
  FClosed := False;

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

  if not CreateProcessW(nil, PChar(LCommandLine), nil, nil, True,
    CREATE_NO_WINDOW, nil, LCurrentDir, LStartup, LProcess) then
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

  FSendLock.Free;
  inherited;
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

  if FProcess <> 0 then
  begin
    if WaitForSingleObject(FProcess, 5000) <> WAIT_OBJECT_0 then
    begin
      TerminateProcess(FProcess, 1);
      WaitForSingleObject(FProcess, 5000);
    end;
  end;

  // The child is gone, so its stderr reached EOF; the thread can be joined.
  if Assigned(FStderrThread) then
  begin
    FStderrThread.WaitFor;
    FreeAndNil(FStderrThread);
  end;
end;

function TMCPStdioTransport.ProcessId: Cardinal;
begin
  Result := GetProcessId(FProcess);
end;

end.
