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
  System.Generics.Collections,
  System.Net.HttpClient, System.Net.URLClient,

  JRPC.Core,
  JRPC.Client,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.Client;

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
  ///   One tool argument to mirror into a request header.
  /// </summary>
  TMCPParamHeader = record
    /// <summary>
    ///   The "x-mcp-header" value: the header is "Mcp-Param-" + Name.
    /// </summary>
    Name: string;

    /// <summary>
    ///   Where the argument sits in "arguments", one "properties" key per step.
    /// </summary>
    Path: TArray<string>;
  end;

  /// <summary>
  ///   Streamable HTTP transport for the MCP client: a POST per request, with
  ///   the Streamable HTTP request headers and an SSE reader for the answers
  ///   that stream.
  /// </summary>
  /// <remarks>
  ///   The body is consumed as it arrives, through a response content stream
  ///   that THTTPClient writes chunk by chunk (the same on Delphi 11 to 13), so
  ///   notifications reach the client while the call is still open. A body is
  ///   classified from its first non-whitespace byte: an object/array is a
  ///   single JSON answer, anything else is treated as SSE. That avoids
  ///   depending on the response Content-Type, which is not available while
  ///   the body is being read.
  ///
  ///   A status of 300 or more is never streamed: its body is emitted at the
  ///   end only when it is a JSON-RPC message (a -32020 HeaderMismatch, a
  ///   -32022 version refusal), and anything else becomes an HTTP status error.
  ///
  ///   Abort interrupts the call in progress at the next chunk; a transport is
  ///   meant to serve one long-lived stream at a time (a listen), while
  ///   ordinary calls can share one.
  ///
  ///   A tools/call carries an Mcp-Param-* header for every argument the
  ///   tool's inputSchema annotates with "x-mcp-header". The annotations come
  ///   from the client (IMCPToolHeadersTransport), which hands over every
  ///   tools/list it reads.
  /// </remarks>
  TMCPHTTPTransport = class(TInterfacedObject,
    IJRPCClientTransport,
    IJRPCStreamingTransport,
    IJRPCAbortableTransport,
    IMCPToolHeadersTransport)
  private
    FURL: string;
    FToken: string;
    FTimeout: Cardinal;
    FAborted: Integer;
    FToolHeaders: TDictionary<string, TArray<TMCPParamHeader>>;
    FToolHeadersKnown: Boolean;
    FToolHeadersLock: TCriticalSection;

    function BuildHeaders(const ARequest: string): TNetHeaders;
    function BuildParamHeaders(const ARequest: string): TNetHeaders;
    function ParamHeadersOf(const ATool: string): TArray<TMCPParamHeader>;
    class procedure CollectParamHeaders(ASchema: TJSONObject;
      const APath: TArray<string>; var AHeaders: TArray<TMCPParamHeader>); static;
    class function ArgumentAt(AArguments: TJSONObject;
      const APath: TArray<string>): TJSONValue; static;
    class function ParamHeaderValue(AValue: TJSONValue; out AText: string): Boolean; static;
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
    destructor Destroy; override;

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

    /// <summary>
    ///   Replaces the known "x-mcp-header" annotations with those of ATools.
    ///   Thread safe; the list stays the caller's.
    /// </summary>
    procedure UpdateToolHeaders(ATools: TMCPTools);

    /// <summary>
    ///   True once UpdateToolHeaders has run.
    /// </summary>
    function ToolHeadersKnown: Boolean;

    property URL: string read FURL write FURL;
    property Token: string read FToken write FToken;
    property Timeout: Cardinal read FTimeout write FTimeout;
  end;

implementation

uses
  System.NetEncoding,

  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Middleware.Headers;

type
  TMCPHTTPMode = (Unknown, Json, SSE);

  /// <summary>
  ///   The receive side of one call: classifies the body, parses SSE events
  ///   and emits them as JSON-RPC frames.
  /// </summary>
  /// <remarks>
  ///   The bytes arrive through TMCPHTTPSink, the response content stream,
  ///   which THTTPClient writes chunk by chunk as it reads them. Progress is
  ///   THTTPClient's receive-data event: the RTL raises it only for a status
  ///   below 300, so it is also what says the body may be emitted live. Until
  ///   it has fired, the body is only collected, and Finish decides once the
  ///   status is known.
  /// </remarks>
  TMCPHTTPReceiver = class
  private
    FOnFrame: TJRPCFrameProc;
    FAborted: PInteger;
    FBuffer: TBytes;
    FMode: TMCPHTTPMode;
    FLive: Boolean;
    FStopped: Boolean;
    FEmitted: Boolean;
    procedure Append(const AChunk: Pointer; ACount: Cardinal);
    procedure Classify;
    procedure ExtractEvents;
    procedure Emit(const AJSON: string);
    function FindEventEnd(AStart: Integer; out ASeparator: Integer): Integer;
    function DataOf(const ABlock: TBytes): string;
    function BodyText: string;
    class function IsJsonRpcMessage(const AText: string): Boolean; static;
  public
    constructor Create(const AOnFrame: TJRPCFrameProc; AAborted: PInteger);

    /// <summary>
    ///   A chunk of the body, as the sink received it.
    /// </summary>
    procedure Write(AChunk: Pointer; ACount: Cardinal);

    /// <summary>
    ///   Drops what was collected: THTTPClient rewinds the content stream
    ///   before it retries a request (a redirect, an authentication challenge).
    /// </summary>
    procedure Discard;

    /// <summary>
    ///   THTTPClient's OnReceiveData: switches to live emission and asks the
    ///   read to stop once the caller stopped or aborted.
    /// </summary>
    procedure Progress(const Sender: TObject; AContentLength, AReadCount: Int64;
      var AAbort: Boolean);

    /// <summary>
    ///   Ends the body. Below 300 a JSON body is emitted here, as are SSE
    ///   events never emitted live, and an unfinished SSE event is discarded
    ///   as the specification asks. From 300 up the body is emitted only when
    ///   it is a JSON-RPC message, which is how a server words a refusal; an
    ///   HTML page or a gateway's JSON is left for the status error.
    /// </summary>
    procedure Finish(AStatusCode: Integer);

    property Stopped: Boolean read FStopped;
    property Emitted: Boolean read FEmitted;
  end;

  /// <summary>
  ///   The response content stream: hands every chunk THTTPClient writes to
  ///   the receiver instead of keeping it.
  /// </summary>
  /// <remarks>
  ///   This is what makes streaming work on Delphi 11 too: OnReceiveDataEx,
  ///   the event that carries the chunk, only exists from Delphi 12, while
  ///   every version writes the content stream as the data arrives.
  /// </remarks>
  TMCPHTTPSink = class(TStream)
  private
    FReceiver: TMCPHTTPReceiver;
    FPosition: Int64;
    FSize: Int64;
  protected
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(AReceiver: TMCPHTTPReceiver);

    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

{ TMCPHTTPSink }

constructor TMCPHTTPSink.Create(AReceiver: TMCPHTTPReceiver);
begin
  inherited Create;
  FReceiver := AReceiver;
end;

function TMCPHTTPSink.Read(var Buffer; Count: Longint): Longint;
begin
  // Nothing is kept: the body went to the receiver
  Result := 0;
end;

function TMCPHTTPSink.Write(const Buffer; Count: Longint): Longint;
begin
  if Count > 0 then
  begin
    FReceiver.Write(@Buffer, Count);
    Inc(FPosition, Count);
    if FPosition > FSize then
      FSize := FPosition;
  end;
  Result := Count;
end;

function TMCPHTTPSink.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: FPosition := FPosition + Offset;
    soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TMCPHTTPSink.SetSize(const NewSize: Int64);
begin
  // A shrink is THTTPClient rewinding for a retry: what the earlier answer
  // wrote belongs to a response that has been thrown away.
  if NewSize < FSize then
    FReceiver.Discard;

  FSize := NewSize;
  if FPosition > FSize then
    FPosition := FSize;
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

procedure TMCPHTTPReceiver.Write(AChunk: Pointer; ACount: Cardinal);
begin
  if FStopped or (ACount = 0) then
    Exit;

  Append(AChunk, ACount);

  if FLive then
  begin
    Classify;
    if FMode = TMCPHTTPMode.SSE then
      ExtractEvents;
  end;
end;

procedure TMCPHTTPReceiver.Discard;
begin
  SetLength(FBuffer, 0);
  FMode := TMCPHTTPMode.Unknown;
  // The retried request has a status of its own
  FLive := False;
end;

procedure TMCPHTTPReceiver.Progress(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  // Raised before the first chunk too, and only for a status below 300:
  // from here on the body is an answer to stream, not a refusal to judge.
  if not FLive then
  begin
    FLive := True;
    // Whatever arrived before the event fired is caught up with now
    Classify;
    if FMode = TMCPHTTPMode.SSE then
      ExtractEvents;
  end;

  AAbort := (FAborted^ <> 0) or FStopped;
end;

function TMCPHTTPReceiver.BodyText: string;
var
  LStart: Integer;
begin
  LStart := 0;
  // UTF-8 BOM, if the server sent one: it is not part of the JSON.
  if (Length(FBuffer) >= 3) and (FBuffer[0] = $EF) and (FBuffer[1] = $BB) and
    (FBuffer[2] = $BF) then
    LStart := 3;

  Result := TEncoding.UTF8.GetString(FBuffer, LStart, Length(FBuffer) - LStart);
end;

class function TMCPHTTPReceiver.IsJsonRpcMessage(const AText: string): Boolean;
var
  LValue: TJSONValue;
begin
  LValue := TJSONObject.ParseJSONValue(AText);
  try
    Result := (LValue is TJSONObject) and
      Assigned(TJSONObject(LValue).GetValue('jsonrpc'));
  finally
    LValue.Free;
  end;
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

procedure TMCPHTTPReceiver.Finish(AStatusCode: Integer);
var
  LText: string;
begin
  if FStopped or (Length(FBuffer) = 0) then
    Exit;

  if AStatusCode >= 300 then
  begin
    // A refusal is answered as a document, never streamed. Only a JSON-RPC
    // message is the server speaking: anything else (a proxy's page, an OAuth
    // error object) would be misread as a malformed reply and hide the status.
    LText := BodyText.Trim;
    SetLength(FBuffer, 0);
    if IsJsonRpcMessage(LText) then
      Emit(LText);
    Exit;
  end;

  Classify;
  if FMode = TMCPHTTPMode.SSE then
  begin
    // The complete events not emitted live yet; an unfinished one is dropped
    ExtractEvents;
    Exit;
  end;

  LText := BodyText;
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
  FToolHeaders := TDictionary<string, TArray<TMCPParamHeader>>.Create;
  FToolHeadersKnown := False;
  FToolHeadersLock := TCriticalSection.Create;
end;

destructor TMCPHTTPTransport.Destroy;
begin
  FToolHeadersLock.Free;
  FToolHeaders.Free;
  inherited;
end;

class procedure TMCPHTTPTransport.CollectParamHeaders(ASchema: TJSONObject;
  const APath: TArray<string>; var AHeaders: TArray<TMCPParamHeader>);
var
  LProperties: TJSONValue;
  LPair: TJSONPair;
  LAnnotation: TJSONValue;
  LHeader: TMCPParamHeader;
  LKnown: TMCPParamHeader;
  LDuplicate: Boolean;
begin
  if not Assigned(ASchema) then
    Exit;

  LProperties := ASchema.GetValue('properties');
  if not (LProperties is TJSONObject) then
    Exit;

  for LPair in TJSONObject(LProperties) do
  begin
    if not (LPair.JsonValue is TJSONObject) then
      Continue;

    LHeader.Path := APath + [LPair.JsonString.Value];

    LAnnotation := TJSONObject(LPair.JsonValue).GetValue(MCP_SCHEMA_HEADER_KEYWORD);
    if (LAnnotation is TJSONString) and (TJSONString(LAnnotation).Value <> '') then
    begin
      LHeader.Name := TJSONString(LAnnotation).Value;

      // Case-insensitively unique within one schema. The server keeps the
      // first of two and ignores the other, and so does the client, so both
      // sides agree on which argument the header speaks for.
      LDuplicate := False;
      for LKnown in AHeaders do
        if SameText(LKnown.Name, LHeader.Name) then
        begin
          LDuplicate := True;
          Break;
        end;

      if not LDuplicate then
        AHeaders := AHeaders + [LHeader];
    end;

    // Nested objects are reachable as long as every step is a "properties" key
    CollectParamHeaders(TJSONObject(LPair.JsonValue), LHeader.Path, AHeaders);
  end;
end;

procedure TMCPHTTPTransport.UpdateToolHeaders(ATools: TMCPTools);
var
  LTool: TMCPTool;
  LHeaders: TArray<TMCPParamHeader>;
begin
  FToolHeadersLock.Enter;
  try
    FToolHeaders.Clear;

    if Assigned(ATools) then
      for LTool in ATools do
      begin
        LHeaders := [];
        CollectParamHeaders(LTool.InputSchema, [], LHeaders);
        // Only the tools that annotate something: the rest need no lookup
        if Length(LHeaders) > 0 then
          FToolHeaders.AddOrSetValue(LTool.Name, LHeaders);
      end;

    FToolHeadersKnown := True;
  finally
    FToolHeadersLock.Leave;
  end;
end;

function TMCPHTTPTransport.ToolHeadersKnown: Boolean;
begin
  FToolHeadersLock.Enter;
  try
    Result := FToolHeadersKnown;
  finally
    FToolHeadersLock.Leave;
  end;
end;

function TMCPHTTPTransport.ParamHeadersOf(const ATool: string): TArray<TMCPParamHeader>;
begin
  FToolHeadersLock.Enter;
  try
    if not FToolHeaders.TryGetValue(ATool, Result) then
      Result := [];
  finally
    FToolHeadersLock.Leave;
  end;
end;

class function TMCPHTTPTransport.ArgumentAt(AArguments: TJSONObject;
  const APath: TArray<string>): TJSONValue;
var
  LIndex: Integer;
begin
  Result := AArguments;
  for LIndex := 0 to High(APath) do
  begin
    if not (Result is TJSONObject) then
      Exit(nil);
    // Values[] matches the key whole: a property name may contain dots
    Result := TJSONObject(Result).Values[APath[LIndex]];
  end;
end;

class function TMCPHTTPTransport.ParamHeaderValue(AValue: TJSONValue;
  out AText: string): Boolean;
begin
  AText := '';

  // Value is the number as the body spells it: the header mirrors the body
  // text, not a re-rendering. The server compares numbers numerically anyway.
  if AValue is TJSONNumber then
    AText := TJSONNumber(AValue).Value
  else if AValue is TJSONString then
    AText := TJSONString(AValue).Value
  else if AValue is TJSONBool then
    AText := BoolToStr(TJSONBool(AValue).AsBoolean, True).ToLower
  else
    // Absent and null mean no header; objects and arrays cannot be mirrored
    Exit(False);

  Result := True;
end;

function TMCPHTTPTransport.BuildParamHeaders(const ARequest: string): TNetHeaders;
var
  LDocument: TJSONValue;
  LParams, LName, LArguments, LValue: TJSONValue;
  LHeaders: TArray<TMCPParamHeader>;
  LHeader: TMCPParamHeader;
  LText: string;
begin
  Result := [];

  LDocument := TJSONObject.ParseJSONValue(ARequest);
  try
    if not (LDocument is TJSONObject) then
      Exit;

    LParams := TJSONObject(LDocument).GetValue('params');
    if not (LParams is TJSONObject) then
      Exit;

    LName := TJSONObject(LParams).GetValue('name');
    if not (LName is TJSONString) then
      Exit;

    LHeaders := ParamHeadersOf(TJSONString(LName).Value);
    if Length(LHeaders) = 0 then
      Exit;

    LArguments := TJSONObject(LParams).GetValue('arguments');
    if not (LArguments is TJSONObject) then
      Exit;

    for LHeader in LHeaders do
    begin
      LValue := ArgumentAt(TJSONObject(LArguments), LHeader.Path);
      if not ParamHeaderValue(LValue, LText) then
        Continue;

      if LText = '' then
        // An empty header is indistinguishable from an absent one: the empty
        // string travels in the Base64 sentinel, which decodes back to it.
        LText := MCP_HEADER_ENCODED_PREFIX + MCP_HEADER_ENCODED_SUFFIX
      else
        // Non-ASCII, or whitespace at either end, has to be wrapped
        LText := TMCPHeaderValue.Encode(LText);

      Result := Result + [TNameValuePair.Create(
        MCP_HEADER_PARAM_PREFIX + LHeader.Name, LText)];
    end;
  finally
    LDocument.Free;
  end;
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

  if LMethod = MCP_METHOD_TOOLS_CALL then
    Result := Result + BuildParamHeaders(ARequest);

  if FToken <> '' then
    Result := Result + [TNameValuePair.Create('Authorization', 'Bearer ' + FToken)];
end;

procedure TMCPHTTPTransport.SendAndReceiveFrames(const ARequest: string; const AOnFrame: TJRPCFrameProc);
var
  LHTTP: THTTPClient;
  LSource: TStringStream;
  LResponse: IHTTPResponse;
  LReceiver: TMCPHTTPReceiver;
  LSink: TMCPHTTPSink;
begin
  // A new call starts clean: Abort only speaks for the stream in progress.
  TInterlocked.Exchange(FAborted, 0);

  LReceiver := TMCPHTTPReceiver.Create(AOnFrame, @FAborted);
  LSink := TMCPHTTPSink.Create(LReceiver);
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ConnectionTimeout := FTimeout;
    LHTTP.ResponseTimeout := FTimeout;
    LHTTP.OnReceiveData := LReceiver.Progress;

    LSource := TStringStream.Create(ARequest, TEncoding.UTF8);
    try
      try
        LResponse := LHTTP.Post(FURL, LSource, LSink, BuildHeaders(ARequest));
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

    if Assigned(LResponse) then
      LReceiver.Finish(LResponse.StatusCode)
    else
      LReceiver.Finish(0);

    if Assigned(LResponse) and (LResponse.StatusCode >= 400) and
      (not LReceiver.Emitted) then
      raise EJRPCClientError.CreateFmt(SMCPHTTPStatusError,
        [LResponse.StatusCode, FURL, LResponse.StatusText]);
  finally
    LHTTP.Free;
    LSink.Free;
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
