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
unit MCPConnect.Client.Transport.HTTP;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.JSON,
  System.Net.HttpClient, System.Net.URLClient,

  JRPC.Core,
  JRPC.Client,

  MCPConnect.MCP.Types.Base;

const
  /// <summary>The MCP methods that carry an Mcp-Name header.</summary>
  MCP_METHOD_TOOLS_CALL = 'tools/call';
  MCP_METHOD_RESOURCES_READ = 'resources/read';
  MCP_METHOD_PROMPTS_GET = 'prompts/get';

resourcestring
  SMCPHTTPTransportError = 'HTTP request to [%s] failed: %s';
  SMCPHTTPStatusError = 'HTTP %d from [%s]: %s';

type
  /// <summary>
  ///   Streamable HTTP transport for the MCP client: a POST per request, with
  ///   the Streamable HTTP request headers and an SSE reader for the answers
  ///   that stream.
  /// </summary>
  /// <remarks>
  ///   The body is consumed as it arrives through THTTPClient's receive-data
  ///   callback, so notifications reach the client while the call is still
  ///   open. A body is classified from its first non-whitespace byte: an
  ///   object/array is a single JSON answer, anything else is treated as SSE.
  ///   That avoids depending on the response Content-Type, which is not
  ///   available to the callback.
  ///
  ///   Abort interrupts the call in progress at the next chunk; a transport is
  ///   meant to serve one long-lived stream at a time (a listen), while
  ///   ordinary calls can share one.
  /// </remarks>
  TMCPHTTPTransport = class(TInterfacedObject, 
    IJRPCClientTransport,
    IJRPCStreamingTransport, 
	IJRPCAbortableTransport)
  private
    FURL: string;
    FToken: string;
    FTimeout: Cardinal;
    FAborted: Integer;

    function BuildHeaders(const ARequest: string): TNetHeaders;
    function ExtractField(const ARequest, AName: string): string;
    function ExtractParam(const ARequest, AName: string): string;
    function ExtractVersion(const ARequest: string): string;
    function ExtractName(const ARequest, AMethod: string): string;
  public
    /// <summary>
    ///   A transport for AURL. Timeout is in milliseconds; 0 means no
    ///   timeout, which is what a long-lived stream wants.
    /// </summary>
    constructor Create(const AURL: string; const AToken: string = ''; ATimeout: Cardinal = 30000);

    /// <summary>
    ///   The whole answer as one string. Not used by a streaming client; the
    ///   last frame is returned.
    /// </summary>
    function SendAndReceive(const ARequest: string): string;

    /// <summary>
    ///   Sends a notification and ignores the (empty) answer.
    /// </summary>
    procedure Send(const ARequest: string);

    /// <summary>
    ///   Posts ARequest and hands every JSON-RPC message of the answer to
    ///   AOnFrame as it arrives: SSE events live, a plain JSON body as one
    ///   frame at the end.
    /// </summary>
    procedure SendAndReceiveFrames(const ARequest: string; const AOnFrame: TJRPCFrameProc);

    /// <summary>
    ///   Interrupts the call in progress at its next chunk.
    /// </summary>
    procedure Abort;

    property URL: string read FURL write FURL;
    property Token: string read FToken write FToken;
    property Timeout: Cardinal read FTimeout write FTimeout;
  end;

implementation

uses
  System.NetEncoding;

type
  TMCPHTTPMode = (Unknown, Json, SSE);

  /// <summary>
  ///   The receive side of one call: classifies the body, parses SSE events
  ///   and emits them as JSON-RPC frames.
  /// </summary>
  TMCPHTTPReceiver = class
  private
    FOnFrame: TJRPCFrameProc;
    FAborted: PInteger;
    FBuffer: TBytes;
    FMode: TMCPHTTPMode;
    FStopped: Boolean;
    FEmitted: Boolean;
    procedure Append(const AChunk: Pointer; ACount: Cardinal);
    procedure Classify;
    procedure ExtractEvents;
    procedure Emit(const AJSON: string);
    function FindEventEnd(AStart: Integer; out ASeparator: Integer): Integer;
    function DataOf(const ABlock: TBytes): string;
  public
    constructor Create(const AOnFrame: TJRPCFrameProc; AAborted: PInteger);

    procedure ReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64;
      AChunk: Pointer; AChunkLength: Cardinal; var AAbort: Boolean);

    /// <summary>
    ///   Ends the body: a JSON body is emitted here, an unfinished SSE event is
    ///   discarded as the specification asks.
    /// </summary>
    procedure Finish;

    property Stopped: Boolean read FStopped;
    property Emitted: Boolean read FEmitted;
  end;

{ TMCPHTTPReceiver }

constructor TMCPHTTPReceiver.Create(const AOnFrame: TJRPCFrameProc;
  AAborted: PInteger);
begin
  inherited Create;
  FOnFrame := AOnFrame;
  FAborted := AAborted;
  FMode := TMCPHTTPMode.Unknown;
end;

procedure TMCPHTTPReceiver.Append(const AChunk: Pointer; ACount: Cardinal);
var
  LOld: Integer;
begin
  if ACount = 0 then
    Exit;

  LOld := Length(FBuffer);
  SetLength(FBuffer, LOld + Integer(ACount));
  Move(AChunk^, FBuffer[LOld], ACount);
end;

procedure TMCPHTTPReceiver.Classify;
var
  LIndex: Integer;
  LByte: Byte;
begin
  if FMode <> TMCPHTTPMode.Unknown then
    Exit;

  LIndex := 0;

  // UTF-8 BOM, if the server sent one.
  if (Length(FBuffer) >= 3) and (FBuffer[0] = $EF) and (FBuffer[1] = $BB) and
    (FBuffer[2] = $BF) then
    LIndex := 3;

  while LIndex < Length(FBuffer) do
  begin
    LByte := FBuffer[LIndex];
    if (LByte = $20) or (LByte = $09) or (LByte = $0A) or (LByte = $0D) then
      Inc(LIndex)
    else
    begin
      if (LByte = Ord('{')) or (LByte = Ord('[')) then
        FMode := TMCPHTTPMode.Json
      else
        FMode := TMCPHTTPMode.SSE;
      Exit;
    end;
  end;
end;

procedure TMCPHTTPReceiver.ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; AChunk: Pointer; AChunkLength: Cardinal;
  var AAbort: Boolean);
begin
  if (FAborted^ <> 0) or FStopped then
  begin
    AAbort := True;
    Exit;
  end;

  if AChunkLength > 0 then
  begin
    Append(AChunk, AChunkLength);
    Classify;
    if FMode = TMCPHTTPMode.SSE then
      ExtractEvents;
  end;

  AAbort := (FAborted^ <> 0) or FStopped;
end;

function TMCPHTTPReceiver.FindEventEnd(AStart: Integer; out ASeparator: Integer): Integer;
var
  LIndex: Integer;
begin
  ASeparator := 0;
  for LIndex := AStart to High(FBuffer) - 1 do
  begin
    if (FBuffer[LIndex] = $0A) and (FBuffer[LIndex + 1] = $0A) then
    begin
      Result := LIndex;
      ASeparator := 2;
      Exit;
    end;

    if (LIndex + 3 <= High(FBuffer)) and
      (FBuffer[LIndex] = $0D) and (FBuffer[LIndex + 1] = $0A) and
      (FBuffer[LIndex + 2] = $0D) and (FBuffer[LIndex + 3] = $0A) then
    begin
      Result := LIndex;
      ASeparator := 4;
      Exit;
    end;
  end;

  Result := -1;
end;

function TMCPHTTPReceiver.DataOf(const ABlock: TBytes): string;
var
  LLines, LData: TStringList;
  LText: string;
  LLine: string;
  LIndex: Integer;
begin
  Result := '';
  if Length(ABlock) = 0 then
    Exit;

  LText := TEncoding.UTF8.GetString(ABlock);
  LLines := TStringList.Create;
  LData := TStringList.Create;
  try
    LLines.Text := LText;

    for LIndex := 0 to LLines.Count - 1 do
    begin
      LLine := LLines[LIndex];
      // A comment (":") and the event/id/retry fields carry nothing the
      // client needs.
      if LLine.StartsWith('data:') then
        LData.Add(LLine.Substring(5).TrimLeft([' ']));
    end;

    for LIndex := 0 to LData.Count - 1 do
      if LIndex = 0 then
        Result := LData[LIndex]
      else
        Result := Result + #10 + LData[LIndex];
  finally
    LData.Free;
    LLines.Free;
  end;
end;

procedure TMCPHTTPReceiver.Emit(const AJSON: string);
begin
  if FStopped then
    Exit;

  if Assigned(FOnFrame) then
  begin
    FEmitted := True;
    if not FOnFrame(AJSON) then
      FStopped := True;
  end;
end;

procedure TMCPHTTPReceiver.ExtractEvents;
var
  LStart, LEnd, LSeparator, LRemaining: Integer;
  LBlock: TBytes;
  LData: string;
begin
  LStart := 0;

  while True do
  begin
    LEnd := FindEventEnd(LStart, LSeparator);
    if LEnd < 0 then
      Break;

    LBlock := Copy(FBuffer, LStart, LEnd - LStart);
    LData := DataOf(LBlock);
    LStart := LEnd + LSeparator;

    if LData <> '' then
    begin
      Emit(LData);
      if FStopped then
        Break;
    end;
  end;

  if LStart > 0 then
  begin
    LRemaining := Length(FBuffer) - LStart;
    if LRemaining > 0 then
      Move(FBuffer[LStart], FBuffer[0], LRemaining);

    SetLength(FBuffer, LRemaining);
  end;
end;

procedure TMCPHTTPReceiver.Finish;
var
  LText: string;
begin
  if FStopped or (FMode = TMCPHTTPMode.SSE) then
    Exit;

  if Length(FBuffer) = 0 then
    Exit;

  LText := TEncoding.UTF8.GetString(FBuffer);
  SetLength(FBuffer, 0);

  if not LText.Trim.IsEmpty then
    Emit(LText);
end;

{ TMCPHTTPTransport }

constructor TMCPHTTPTransport.Create(const AURL: string; const AToken: string; ATimeout: Cardinal);
begin
  inherited Create;
  FURL := AURL;
  FToken := AToken;
  FTimeout := ATimeout;
  FAborted := 0;
end;

function TMCPHTTPTransport.ExtractField(const ARequest, AName: string): string;
var
  LValue: TJSONValue;
  LField: TJSONValue;
begin
  Result := '';

  LValue := TJSONObject.ParseJSONValue(ARequest);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LField := TJSONObject(LValue).GetValue(AName);
    if Assigned(LField) then
      Result := LField.Value;
  finally
    LValue.Free;
  end;
end;

function TMCPHTTPTransport.ExtractParam(const ARequest, AName: string): string;
var
  LValue: TJSONValue;
  LParams: TJSONValue;
  LParam: TJSONValue;
begin
  Result := '';

  LValue := TJSONObject.ParseJSONValue(ARequest);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LParams := TJSONObject(LValue).GetValue('params');
    if LParams is TJSONObject then
    begin
      LParam := TJSONObject(LParams).GetValue(AName);
      if Assigned(LParam) then
        Result := LParam.Value;
    end;
  finally
    LValue.Free;
  end;
end;

function TMCPHTTPTransport.ExtractVersion(const ARequest: string): string;
var
  LValue: TJSONValue;
  LParams, LMeta: TJSONValue;
  LVersion: TJSONValue;
begin
  Result := '';

  LValue := TJSONObject.ParseJSONValue(ARequest);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LParams := TJSONObject(LValue).GetValue('params');
    if LParams is TJSONObject then
    begin
      LMeta := TJSONObject(LParams).GetValue('_meta');
      if LMeta is TJSONObject then
      begin
        // The key contains dots: Values[] matches it whole, GetValue would
        // read it as a path.
        LVersion := TJSONObject(LMeta).Values[MCP_META_PROTOCOL_VERSION];
        if Assigned(LVersion) then
          Result := LVersion.Value;
      end;
    end;
  finally
    LValue.Free;
  end;
end;

function TMCPHTTPTransport.ExtractName(const ARequest, AMethod: string): string;
begin
  if (AMethod = MCP_METHOD_TOOLS_CALL) or (AMethod = MCP_METHOD_PROMPTS_GET) then
    Result := ExtractParam(ARequest, 'name')
  else if AMethod = MCP_METHOD_RESOURCES_READ then
    Result := ExtractParam(ARequest, 'uri')
  else
    Result := '';
end;

function TMCPHTTPTransport.BuildHeaders(const ARequest: string): TNetHeaders;
var
  LMethod, LVersion, LName: string;
begin
  LMethod := ExtractField(ARequest, 'method');
  LVersion := ExtractVersion(ARequest);
  if LVersion = '' then
    LVersion := MCP_LATEST_PROTOCOL_VERSION;
  LName := ExtractName(ARequest, LMethod);

  Result := [
    TNameValuePair.Create('Content-Type', 'application/json'),
    TNameValuePair.Create('Accept', 'application/json, text/event-stream'),
    TNameValuePair.Create('MCP-Protocol-Version', LVersion),
    TNameValuePair.Create('Mcp-Method', LMethod)
  ];

  if LName <> '' then
    Result := Result + [TNameValuePair.Create('Mcp-Name', LName)];

  if FToken <> '' then
    Result := Result + [TNameValuePair.Create('Authorization', 'Bearer ' + FToken)];
end;

procedure TMCPHTTPTransport.SendAndReceiveFrames(const ARequest: string; const AOnFrame: TJRPCFrameProc);
var
  LHTTP: THTTPClient;
  LSource: TStringStream;
  LResponse: IHTTPResponse;
  LReceiver: TMCPHTTPReceiver;
begin
  // A new call starts clean: Abort only speaks for the stream in progress.
  TInterlocked.Exchange(FAborted, 0);

  LReceiver := TMCPHTTPReceiver.Create(AOnFrame, @FAborted);
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ConnectionTimeout := FTimeout;
    LHTTP.ResponseTimeout := FTimeout;
    LHTTP.OnReceiveDataEx := LReceiver.ReceiveData;

    LSource := TStringStream.Create(ARequest, TEncoding.UTF8);
    try
      try
        LResponse := LHTTP.Post(FURL, LSource, nil, BuildHeaders(ARequest));
      except
        // A frame callback that raised - a remote error, an id mismatch, an
        // unreadable frame - is not a transport failure: let it through as the
        // type it already has.
        on E: EJRPCException do
          raise;
        on E: Exception do
        begin
          // Aborting the read is not a failure: the caller asked for it.
          if (FAborted <> 0) or LReceiver.Stopped then
            Exit;
          raise EJRPCClientError.CreateFmt(SMCPHTTPTransportError,
            [FURL, E.Message]);
        end;
      end;
    finally
      LSource.Free;
    end;

    LReceiver.Finish;

    if Assigned(LResponse) and (LResponse.StatusCode >= 400) and
      (not LReceiver.Emitted) then
      raise EJRPCClientError.CreateFmt(SMCPHTTPStatusError,
        [LResponse.StatusCode, FURL, LResponse.StatusText]);
  finally
    LHTTP.Free;
    LReceiver.Free;
  end;
end;

procedure TMCPHTTPTransport.Send(const ARequest: string);
var
  LHTTP: THTTPClient;
  LSource: TStringStream;
  LResponse: IHTTPResponse;
begin
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ConnectionTimeout := FTimeout;
    LHTTP.ResponseTimeout := FTimeout;

    LSource := TStringStream.Create(ARequest, TEncoding.UTF8);
    try
      try
        LResponse := LHTTP.Post(FURL, LSource, nil, BuildHeaders(ARequest));
      except
        on E: Exception do
          raise EJRPCClientError.CreateFmt(SMCPHTTPTransportError,
            [FURL, E.Message]);
      end;
    finally
      LSource.Free;
    end;

    if Assigned(LResponse) and (LResponse.StatusCode >= 400) then
      raise EJRPCClientError.CreateFmt(SMCPHTTPStatusError,
        [LResponse.StatusCode, FURL, LResponse.StatusText]);
  finally
    LHTTP.Free;
  end;
end;

function TMCPHTTPTransport.SendAndReceive(const ARequest: string): string;
var
  LReply: string;
begin
  LReply := '';
  SendAndReceiveFrames(ARequest,
    function(const AJSON: string): Boolean
    begin
      LReply := AJSON;
      Result := True;
    end);
  Result := LReply;
end;

procedure TMCPHTTPTransport.Abort;
begin
  TInterlocked.Exchange(FAborted, 1);
end;

end.
