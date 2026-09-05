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

  Logify,
  MCPConnect.Transport.Base,
  MCPConnect.Transport.MediaType,
  MCPConnect.JRPC.Server;

resourcestring
  SServerNotFound = 'Server not found';

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
  /// </remarks>
  TStdWriter = class(TObject)
  private const
    LF = 10;
  private
    FStream: TStream;
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
  public
    constructor Create;
  end;

  TStdErrWriter = class(TStdWriter)
  public
    constructor Create;
  end;

  TWorkerThread = class(TThread)
  private
    FServer: TJRPCServer;
    procedure HandleRequest(const ARequestContent: string; out AResponseContent: string; AStdOutWriter: TStdOutWriter);
  protected
    procedure Execute; override;
  public
    property Server: TJRPCServer read FServer write FServer;
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
  FreeAndNil(FStream);
end;

constructor TStdWriter.Create(AHandle: THandle);
begin
  inherited Create;
  FStream := THandleStream.Create(AHandle);
end;

destructor TStdWriter.Destroy;
begin
  FStream.Free;
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
  if not Assigned(FStream) then
    Exit;

  LBytes := TEncoding.UTF8.GetBytes(AValue);
  LLength := Length(LBytes);
  SetLength(LBytes, LLength + 1);
  LBytes[LLength] := LF;

  FStream.WriteBuffer(LBytes[0], LLength + 1);
end;

{ TStdOutWriter }

constructor TStdOutWriter.Create;
begin
  inherited Create(StdOutHandle);
end;

{ TStdErrWriter }

constructor TStdErrWriter.Create;
begin
  inherited Create(StdErrHandle);
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
begin
  inherited;
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

    Terminate;
  except
    on E: Exception do
    begin
      logger.LogError('TWorkerThread - %s: %s', [E.ClassName, E.Message]);
      raise;
    end;
  end;
end;

procedure TWorkerThread.HandleRequest(const ARequestContent: string; out AResponseContent: string; AStdOutWriter: TStdOutWriter);
var
  LMcpHandler: IMCPTransportHandler;
  LRes: string;
begin
  if not Assigned(FServer) then
    raise EJRPCException.Create(SServerNotFound);

  // Auth??

  LMcpHandler := TMCPTransportHandler.Create(FServer, TMCPTransportWriterStdio.Create(AStdOutWriter));

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
  Result := True;
end;

procedure TMCPTransportWriterStdio.Write(const AValue: string; const AEventId: string);
begin
  FStdOutWriter.WriteLine(RemoveLineBreaks(AValue));
end;

end.
