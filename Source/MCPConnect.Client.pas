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
unit MCPConnect.Client;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.TypInfo,
  System.Generics.Collections,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Utils,

  JRPC.Core,
  JRPC.Client,
  JRPC.Connection,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Errors;

const
  /// <summary>
  ///   The name this client reports in the request "_meta" clientInfo.
  /// </summary>
  MCP_CLIENT_NAME = 'MCPConnect';

  /// <summary>
  ///   The version this client reports in the request "_meta" clientInfo.
  /// </summary>
  MCP_CLIENT_VERSION = '1.0';

  /// <summary>
  ///   How many input rounds one tool call may take before the client gives up.
  /// </summary>
  MCP_CLIENT_MAX_ROUNDS = 10;

resourcestring
  SMCPClientParamsRequired = 'The request params are required: they carry the per-request "_meta"';
  SMCPClientParamsSerialization = 'The request params could not be serialized to JSON';
  SMCPClientResultNotObject = 'The tool result is not a JSON object';
  SMCPClientUnknownContent = 'Unknown tool result content type [%s]';
  SMCPClientInputRequired =
    'The server requires input this client cannot provide: no handler is configured for it';
  SMCPClientUnknownInputRequest = 'Unknown input request [%s]';
  SMCPClientInputHandlerNil = 'The input handler for [%s] returned no result';
  SMCPClientTooManyRounds = 'The tool call still requires input after %d rounds';

type
  /// <summary>
  ///   Called with an "elicitation/create" request from the server.
  /// </summary>
  /// <remarks>
  ///   Return a result the client takes ownership of. The request belongs to
  ///   the client and is valid only for the duration of the call.
  /// </remarks>
  TMCPElicitationHandler = reference to function(
    const ARequest: TElicitRequestParams): TElicitResult;

  /// <summary>
  ///   Called with a "sampling/createMessage" request from the server.
  /// </summary>
  TMCPSamplingHandler = reference to function(
    const ARequest: TCreateMessageRequestParams): TCreateMessageResult;

  /// <summary>
  ///   Called with a "roots/list" request from the server.
  /// </summary>
  TMCPRootsHandler = reference to function(
    const ARequest: TListRootsParams): TListRootsResult;

  /// <summary>
  ///   Raised when tools/call answers "input_required" and the client has no
  ///   handler for one of the requests in it. The raw interim result is
  ///   carried so nothing the server sent is lost.
  /// </summary>
  EMCPClientInputRequiredError = class(EJRPCException)
  private
    FPayload: TJSONObject;
  public
    /// <summary>
    ///   Takes ownership of the "input_required" result object.
    /// </summary>
    constructor Create(APayload: TJSONObject);
    destructor Destroy; override;

    /// <summary>
    ///   The interim result, owned by the exception: inputRequests and
    ///   requestState are there for a caller that wants to continue by hand.
    /// </summary>
    property Payload: TJSONObject read FPayload;
  end;

  /// <summary>
  ///   Decodes the "data" payloads the MCP errors carry.
  /// </summary>
  /// <remarks>
  ///   The peer error stays an EJRPCRemoteError; these helpers only turn its
  ///   data into the types the specification defines for it.
  /// </remarks>
  TMCPClientErrors = class
  public
    /// <summary>
    ///   The -32022 payload: the requested version and the supported ones.
    ///   True only when E is that error and its data parses; the caller owns
    ///   the result.
    /// </summary>
    class function TryUnsupportedVersion(E: EJRPCRemoteError;
      out AData: TMCPUnsupportedVersionErrorData): Boolean;

    /// <summary>
    ///   The -32021 payload: the client capabilities the request required.
    /// </summary>
    class function TryRequiredCapabilities(E: EJRPCRemoteError;
      out ACapabilities: TMCPClientCapabilities): Boolean;
  end;

  /// <summary>
  ///   An MCP client over a streaming JSON-RPC transport: one request at a
  ///   time, with the protocol version, capabilities and "_meta" the
  ///   2026-07-28 revision requires.
  /// </summary>
  /// <remarks>
  ///   The transport owns the framing (HTTP/SSE today, stdio next) and the
  ///   JRPC client owns the envelope and the streaming dispatch. This class
  ///   owns the MCP half: the version negotiation of server/discover, the
  ///   "_meta" every request must carry, and the typed results.
  ///
  ///   CallTool parses a tools/call result into content of the right concrete
  ///   class, structuredContent and isError. An "input_required" answer is
  ///   fulfilled through OnElicitation / OnSampling / OnRoots and retried with
  ///   the answers and the requestState, up to MCP_CLIENT_MAX_ROUNDS times;
  ///   missing handlers raise EMCPClientInputRequiredError.
  /// </remarks>
  TMCPClient = class
  private
    FJRPC: TJRPCClient;
    FConnection: TJRPCConnection;
    FCallTimeout: Cardinal;
    FProtocolVersion: string;
    FServerInfo: TImplementation;
    FCapabilities: TServerCapabilities;
    FInstructions: string;
    FOnNotification: TJRPCNotificationProc;
    FOnError: TJRPCErrorProc;
    FOnElicitation: TMCPElicitationHandler;
    FOnSampling: TMCPSamplingHandler;
    FOnRoots: TMCPRootsHandler;

    procedure InitState;
    procedure ForwardNotification(const ANotification: TJRPCNotification);
    procedure ForwardError(E: Exception);
    procedure ApplyMeta(AParams: TRequestMetaParams);
    procedure PatchCapabilities(AValue: TJSONObject);
    function BuildCapabilitiesJSON: TJSONObject;
    function BuildRequest(const AMethod: string; AParams: TRequestMetaParams): TJRPCRequest;
    function ExecuteRequest(ARequest: TJRPCRequest): TJSONValue;
    function DeserializeResult<T>(AValue: TJSONValue): T;
    function Discover: TDiscoverResult;
    function TryNegotiateVersion(E: EJRPCRemoteError): Boolean;
    function ParseCallToolResult(AValue: TJSONObject): TCallToolResult;
    function ParseInputRequired(AValue: TJSONObject): TInputRequiredResult;
    function Fulfill(AParams: TCallToolRequestParams;
      AResult: TInputRequiredResult): Boolean;
    class function ParseContent(AValue: TJSONObject): TToolContent; static;
    class function IsInputRequired(AValue: TJSONObject): Boolean; static;
  public
    /// <summary>
    ///   A client over a streaming transport (Streamable HTTP): each call is
    ///   one request and its streamed answer.
    /// </summary>
    constructor Create(const ATransport: IJRPCStreamingTransport); overload;

    /// <summary>
    ///   A client over a bidirectional message transport (stdio): the client
    ///   owns a TJRPCConnection, whose reader thread multiplexes replies,
    ///   notifications and peer requests.
    /// </summary>
    constructor Create(const ATransport: IJRPCMessageTransport); overload;

    destructor Destroy; override;

    /// <summary>
    ///   Calls server/discover, negotiates the protocol version and records
    ///   the server identity and capabilities.
    /// </summary>
    /// <remarks>
    ///   A server that answers -32022 is asked again with a version both sides
    ///   speak; when there is none, the peer's error is raised unchanged.
    /// </remarks>
    procedure Connect;

    /// <summary>
    ///   The whole tools/list, following nextCursor until the list ends.
    ///   The caller owns the list and its tools.
    /// </summary>
    function ListTools: TMCPTools;

    /// <summary>
    ///   Sends AMethod with AParams and returns the raw "result" (caller
    ///   owns it). The per-request "_meta" is filled in here.
    /// </summary>
    /// <remarks>
    ///   AParams is required and owned by the caller: it is the carrier of the
    ///   progress token and log level the request asks for.
    /// </remarks>
    function Call(const AMethod: string; AParams: TRequestMetaParams): TJSONValue;

    /// <summary>
    ///   The same call, deserialized into T with the MCP Neon configuration.
    /// </summary>
    function ExecuteAs<T>(const AMethod: string; AParams: TRequestMetaParams): T;

    /// <summary>
    ///   Calls a tool and returns its typed result: content items of their
    ///   concrete class, structuredContent and isError.
    /// </summary>
    /// <remarks>
    ///   The arguments are cloned, so the caller keeps ownership of
    ///   AArguments. A tool failure (isError) is part of the result, not an
    ///   exception; an "input_required" answer raises
    ///   EMCPClientInputRequiredError.
    /// </remarks>
    function CallTool(const AName: string; AArguments: TJSONObject = nil): TCallToolResult; overload;

    /// <summary>
    ///   The same call with params the caller built, which is how a progress
    ///   token or a log level is attached to a typed call. AParams stays the
    ///   caller's.
    /// </summary>
    /// <remarks>
    ///   The object is the carrier of the round trip: the client writes the
    ///   answers it receives into InputResponses and the server's
    ///   RequestState into it before retrying.
    /// </remarks>
    function CallTool(AParams: TCallToolRequestParams): TCallToolResult; overload;

    /// <summary>The version negotiated by Connect.</summary>
    property ProtocolVersion: string read FProtocolVersion;

    /// <summary>
    ///   Milliseconds Call waits for an answer on a connection transport; 0
    ///   waits forever. The HTTP transport has its own timeouts.
    /// </summary>
    property CallTimeout: Cardinal read FCallTimeout write FCallTimeout;

    /// <summary>The server identity reported in server/discover.</summary>
    property ServerInfo: TImplementation read FServerInfo;

    /// <summary>The capabilities reported in server/discover.</summary>
    property Capabilities: TServerCapabilities read FCapabilities;

    /// <summary>The instructions reported in server/discover, empty when none.</summary>
    property Instructions: string read FInstructions;

    /// <summary>
    ///   Notifications that arrive while a call streams: progress and log
    ///   messages, in the order the server sent them.
    /// </summary>
    property OnNotification: TJRPCNotificationProc read FOnNotification write FOnNotification;

    /// <summary>
    ///   Answers "elicitation/create" requests the server embeds in an interim
    ///   tool result. Assigning it also declares the elicitation/form client
    ///   capability in every request.
    /// </summary>
    property OnElicitation: TMCPElicitationHandler
      read FOnElicitation write FOnElicitation;

    /// <summary>
    ///   Answers "sampling/createMessage" requests. Assigning it declares the
    ///   sampling capability.
    /// </summary>
    property OnSampling: TMCPSamplingHandler read FOnSampling write FOnSampling;

    /// <summary>
    ///   Answers "roots/list" requests. Assigning it declares the roots
    ///   capability.
    /// </summary>
    property OnRoots: TMCPRootsHandler read FOnRoots write FOnRoots;

    /// <summary>
    ///   A failure of the call itself, reported once per call.
    /// </summary>
    property OnError: TJRPCErrorProc read FOnError write FOnError;
  end;

implementation

{ TMCPClient }

procedure TMCPClient.InitState;
begin
  FProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;
  FServerInfo := TImplementation.Create;
  FCapabilities := TServerCapabilities.Create;
  FCallTimeout := 0;
end;

constructor TMCPClient.Create(const ATransport: IJRPCStreamingTransport);
begin
  inherited Create;
  InitState;

  FJRPC := TJRPCClient.Create(nil);
  FJRPC.Transport := ATransport;
  // The MCP type model is made of fields written in camel case.
  FJRPC.NeonConfig := MCPNeonConfig;
  FJRPC.OnNotification := ForwardNotification;
  FJRPC.OnError := ForwardError;
end;

constructor TMCPClient.Create(const ATransport: IJRPCMessageTransport);
begin
  inherited Create;
  InitState;

  FConnection := TJRPCConnection.Create(ATransport);
  FConnection.OnNotification := ForwardNotification;
  FConnection.OnError := ForwardError;
  FConnection.Open;
end;

destructor TMCPClient.Destroy;
begin
  FConnection.Free;
  FJRPC.Free;
  FCapabilities.Free;
  FServerInfo.Free;
  inherited;
end;

procedure TMCPClient.ForwardNotification(const ANotification: TJRPCNotification);
begin
  if Assigned(FOnNotification) then
    FOnNotification(ANotification);
end;

procedure TMCPClient.ForwardError(E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(E);
end;

procedure TMCPClient.ApplyMeta(AParams: TRequestMetaParams);
begin
  if not Assigned(AParams) then
    raise EJRPCClientError.Create(SMCPClientParamsRequired);

  AParams.RequestMeta.ProtocolVersion := FProtocolVersion;
  AParams.RequestMeta.ClientInfo.Name := MCP_CLIENT_NAME;
  AParams.RequestMeta.ClientInfo.Version := MCP_CLIENT_VERSION;

  // Capabilities and the progress token are the caller's: a request that asks
  // for progress or logging must be able to say so before it is sent.
end;

function TMCPClient.BuildRequest(const AMethod: string; AParams: TRequestMetaParams): TJRPCRequest;
var
  LParamsJSON: TJSONValue;
begin
  ApplyMeta(AParams);

  LParamsJSON := TJSONObject.ParseJSONValue(
    TNeon.ObjectToJSONString(AParams, MCPNeonConfig));
  if not (LParamsJSON is TJSONObject) then
  begin
    LParamsJSON.Free;
    raise EJRPCClientError.Create(SMCPClientParamsSerialization);
  end;

  // Declare only the capabilities the configured handlers actually provide.
  PatchCapabilities(TJSONObject(LParamsJSON));

  Result := TJRPCRequest.Create;
  try
    Result.Method := AMethod;
    // SetParams takes ownership: the request frees the params with itself.
    Result.Params := LParamsJSON;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPClient.PatchCapabilities(AValue: TJSONObject);
var
  LMeta: TJSONValue;
  LOld: TJSONPair;
  LCaps: TJSONObject;
begin
  LMeta := AValue.GetValue('_meta');
  if not (LMeta is TJSONObject) then
    Exit;

  LCaps := BuildCapabilitiesJSON;
  try
    LOld := TJSONObject(LMeta).RemovePair(MCP_META_CLIENT_CAPABILITIES);
    LOld.Free;
    TJSONObject(LMeta).AddPair(MCP_META_CLIENT_CAPABILITIES, LCaps);
    // The meta object owns it now.
    LCaps := nil;
  finally
    LCaps.Free;
  end;
end;

function TMCPClient.BuildCapabilitiesJSON: TJSONObject;
var
  LElicitation: TJSONObject;
begin
  Result := TJSONObject.Create;

  if Assigned(FOnElicitation) then
  begin
    // The form mode only: url elicitation is not implemented.
    LElicitation := TJSONObject.Create;
    LElicitation.AddPair('form', TJSONObject.Create);
    Result.AddPair('elicitation', LElicitation);
  end;

  if Assigned(FOnSampling) then
    Result.AddPair('sampling', TJSONObject.Create);

  if Assigned(FOnRoots) then
    Result.AddPair('roots', TJSONObject.Create);
end;

function TMCPClient.Discover: TDiscoverResult;
var
  LParams: TRequestMetaParams;
begin
  LParams := TRequestMetaParams.Create;
  try
    Result := ExecuteAs<TDiscoverResult>('server/discover', LParams);
  finally
    LParams.Free;
  end;
end;

function TMCPClient.TryNegotiateVersion(E: EJRPCRemoteError): Boolean;
var
  LData: TMCPUnsupportedVersionErrorData;
  LVersion: string;
  LCandidate: string;
begin
  Result := False;

  if not TMCPClientErrors.TryUnsupportedVersion(E, LData) then
    Exit;

  try
    for LVersion in LData.Supported do
      for LCandidate in MCP_PROTOCOL_SUPPORTED_VERSIONS do
        if LVersion = LCandidate then
        begin
          FProtocolVersion := LVersion;
          Exit(True);
        end;
  finally
    LData.Free;
  end;
end;

procedure TMCPClient.Connect;
var
  LResult: TDiscoverResult;
begin
  FProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;

  try
    LResult := Discover;
  except
    on E: EJRPCRemoteError do
    begin
      if not TryNegotiateVersion(E) then
        raise;

      LResult := Discover;
    end;
  end;

  try
    FCapabilities.Assign(LResult.Capabilities);

    if Assigned(LResult.ResultMeta.ServerInfo) then
    begin
      FServerInfo.Name := LResult.ResultMeta.ServerInfo.Name;
      FServerInfo.Version := LResult.ResultMeta.ServerInfo.Version;
      FServerInfo.Description := LResult.ResultMeta.ServerInfo.Description;
    end;

    if LResult.Instructions.HasValue then
      FInstructions := LResult.Instructions.Value;
  finally
    LResult.Free;
  end;
end;

function TMCPClient.ListTools: TMCPTools;
var
  LParams: TPaginatedRequestParams;
  LResult: TListToolsResult;
  LCursor: string;
  LTool: TMCPTool;
begin
  Result := TMCPTools.Create(True);

  try
    LCursor := '';
    repeat
      LParams := TPaginatedRequestParams.Create;
      try
        if LCursor <> '' then
          LParams.Cursor := LCursor;

        LResult := ExecuteAs<TListToolsResult>('tools/list', LParams);
        try
          for LTool in LResult.Tools do
            Result.Add(LTool);

          // The tools moved to the caller's list; the result must not free
          // them when it goes.
          LResult.Tools.OwnsObjects := False;

          if LResult.NextCursor.HasValue then
            LCursor := LResult.NextCursor.Value
          else
            LCursor := '';
        finally
          LResult.Free;
        end;
      finally
        LParams.Free;
      end;
    until LCursor = '';
  except
    Result.Free;
    raise;
  end;
end;

function TMCPClient.ExecuteRequest(ARequest: TJRPCRequest): TJSONValue;
var
  LResponse: TJRPCResponse;
begin
  if Assigned(FConnection) then
  begin
    LResponse := FConnection.Call(ARequest, FCallTimeout);
    try
      Result := LResponse.Result.Clone as TJSONValue;
    finally
      LResponse.Free;
    end;
  end
  else
    Result := FJRPC.ExecuteAsJSONValue(ARequest);
end;

function TMCPClient.DeserializeResult<T>(AValue: TJSONValue): T;
var
  LType: TRttiType;
  LObject: TObject;
  LResult: TValue;
begin
  LType := TRttiUtils.Context.GetType(TypeInfo(T));
  if not Assigned(LType) then
    raise EJRPCClientError.Create(SJRPCClientUnknownResultType);

  // The same split the JRPC client uses: a class goes through JSONToObject
  // and is created here, anything else through JSONToValue.
  if LType.IsInstance then
  begin
    LObject := TNeon.JSONToObject(LType, AValue, MCPNeonConfig);
    TValue.Make(@LObject, LType.Handle, LResult);
    Result := LResult.AsType<T>;
  end
  else
    Result := TNeon.JSONToValue<T>(AValue, MCPNeonConfig);
end;

function TMCPClient.Call(const AMethod: string; AParams: TRequestMetaParams): TJSONValue;
var
  LRequest: TJRPCRequest;
begin
  LRequest := BuildRequest(AMethod, AParams);
  try
    Result := ExecuteRequest(LRequest);
  finally
    LRequest.Free;
  end;
end;

function TMCPClient.ExecuteAs<T>(const AMethod: string; AParams: TRequestMetaParams): T;
var
  LRequest: TJRPCRequest;
  LValue: TJSONValue;
begin
  LRequest := BuildRequest(AMethod, AParams);
  try
    if Assigned(FConnection) then
    begin
      LValue := ExecuteRequest(LRequest);
      try
        Result := DeserializeResult<T>(LValue);
      finally
        LValue.Free;
      end;
    end
    else
      Result := FJRPC.ExecuteAs<T>(LRequest);
  finally
    LRequest.Free;
  end;
end;

function TMCPClient.CallTool(const AName: string; AArguments: TJSONObject): TCallToolResult;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := AName;
    if Assigned(AArguments) then
    begin
      // The params constructor already made an empty Arguments object; release
      // it before taking the clone, or it leaks with the params.
      LParams.Arguments.Free;
      LParams.Arguments := AArguments.Clone as TJSONObject;
    end;

    Result := CallTool(LParams);
  finally
    LParams.Free;
  end;
end;

function TMCPClient.CallTool(AParams: TCallToolRequestParams): TCallToolResult;
var
  LValue: TJSONValue;
  LInput: TInputRequiredResult;
  LRounds: Integer;
begin
  LRounds := 0;

  while True do
  begin
    LValue := Call('tools/call', AParams);
    try
      if not (LValue is TJSONObject) then
        raise EJRPCClientError.Create(SMCPClientResultNotObject);

      if not IsInputRequired(TJSONObject(LValue)) then
        Exit(ParseCallToolResult(TJSONObject(LValue)));

      Inc(LRounds);
      if LRounds > MCP_CLIENT_MAX_ROUNDS then
        raise EJRPCClientError.CreateFmt(SMCPClientTooManyRounds, [LRounds]);

      LInput := ParseInputRequired(TJSONObject(LValue));
      try
        if not Fulfill(AParams, LInput) then
          // Nobody configured for this kind: the interim result is kept whole
          // so the caller can continue by hand or report precisely.
          raise EMCPClientInputRequiredError.Create(
            TJSONObject(LValue).Clone as TJSONObject);
      finally
        LInput.Free;
      end;
    finally
      LValue.Free;
    end;
  end;
end;

function TMCPClient.ParseInputRequired(AValue: TJSONObject): TInputRequiredResult;
begin
  Result := TInputRequiredResult.Create;
  try
    TNeon.JSONToObject(Result, AValue, MCPNeonConfig);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPClient.Fulfill(AParams: TCallToolRequestParams;
  AResult: TInputRequiredResult): Boolean;
var
  LPair: TPair<string, TInputRequest>;
  LRequest: TInputRequest;
  LResponses: TInputResponses;
  LAnswer: TObject;
begin
  Result := False;
  LResponses := TInputResponses.Create([doOwnsValues]);
  try
    for LPair in AResult.InputRequests do
    begin
      LRequest := LPair.Value;
      if not LRequest.Resolve then
        raise EJRPCClientError.CreateFmt(SMCPClientUnknownInputRequest, [LPair.Key]);

      if LRequest.Method = MCP_INPUT_ELICITATION then
      begin
        if not Assigned(FOnElicitation) then
          Exit;
        LAnswer := FOnElicitation(LRequest.AsElicitation);
      end
      else if LRequest.Method = MCP_INPUT_SAMPLING then
      begin
        if not Assigned(FOnSampling) then
          Exit;
        LAnswer := FOnSampling(LRequest.AsSampling);
      end
      else if LRequest.Method = MCP_INPUT_ROOTS then
      begin
        if not Assigned(FOnRoots) then
          Exit;
        LAnswer := FOnRoots(LRequest.AsRoots);
      end
      else
        raise EJRPCClientError.CreateFmt(SMCPClientUnknownInputRequest, [LPair.Key]);

      if not Assigned(LAnswer) then
        raise EJRPCClientError.CreateFmt(SMCPClientInputHandlerNil, [LPair.Key]);

      if LRequest.Method = MCP_INPUT_ELICITATION then
        LResponses.AddElicitation(LPair.Key, TElicitResult(LAnswer))
      else if LRequest.Method = MCP_INPUT_SAMPLING then
        LResponses.AddSampling(LPair.Key, TCreateMessageResult(LAnswer))
      else
        LResponses.AddRoots(LPair.Key, TListRootsResult(LAnswer));
    end;

    // The next round carries these answers and the server's continuation token.
    AParams.InputResponses.Free;
    AParams.InputResponses := LResponses;
    LResponses := nil;
    AParams.RequestState := AResult.RequestState;

    Result := True;
  finally
    LResponses.Free;
  end;
end;

function TMCPClient.ParseCallToolResult(AValue: TJSONObject): TCallToolResult;
var
  LContent, LMeta, LStructured, LIsError, LItem: TJSONValue;
begin
  Result := TCallToolResult.Create;
  try
    LMeta := AValue.GetValue('_meta');
    if LMeta is TJSONObject then
      TNeon.JSONToObject(Result.ResultMeta, TJSONObject(LMeta), MCPNeonConfig);

    LContent := AValue.GetValue('content');
    if LContent is TJSONArray then
      for LItem in TJSONArray(LContent) do
        if LItem is TJSONObject then
          Result.Content.Add(ParseContent(TJSONObject(LItem)));

    LStructured := AValue.GetValue('structuredContent');
    if Assigned(LStructured) then
      Result.StructuredContent := LStructured.Clone as TJSONValue;

    LIsError := AValue.GetValue('isError');
    if LIsError is TJSONBool then
      Result.IsError := TJSONBool(LIsError).AsBoolean;
  except
    Result.Free;
    raise;
  end;
end;

class function TMCPClient.ParseContent(AValue: TJSONObject): TToolContent;
var
  LType: string;
  LTypeValue, LResource, LBlob: TJSONValue;
begin
  LTypeValue := AValue.GetValue('type');
  if Assigned(LTypeValue) then
    LType := LTypeValue.Value
  else
    LType := '';

  if LType = MCP_CONTENT_TEXT then
    Result := TTextContent.Create
  else if LType = MCP_CONTENT_IMAGE then
    Result := TImageContent.Create
  else if LType = MCP_CONTENT_AUDIO then
    Result := TAudioContent.Create
  else if LType = 'resource_link' then
    Result := TResourceLink.Create
  else if LType = 'resource' then
  begin
    // One content type with two shapes: the embedded resource is text or blob.
    LResource := AValue.GetValue('resource');
    if LResource is TJSONObject then
      LBlob := TJSONObject(LResource).GetValue('blob')
    else
      LBlob := nil;

    if Assigned(LBlob) then
      Result := TEmbeddedResourceBlob.Create
    else
      Result := TEmbeddedResourceText.Create;
  end
  else
    raise EJRPCClientError.CreateFmt(SMCPClientUnknownContent, [LType]);

  try
    // The concrete class is known now, so Neon binds its own fields instead
    // of dropping the members of a base TToolContent.
    TNeon.JSONToObject(Result, AValue, MCPNeonConfig);
  except
    Result.Free;
    raise;
  end;
end;

class function TMCPClient.IsInputRequired(AValue: TJSONObject): Boolean;
var
  LType: TJSONValue;
begin
  LType := AValue.GetValue('resultType');
  Result := Assigned(LType) and (LType.Value = 'input_required');
end;

{ EMCPClientInputRequiredError }

constructor EMCPClientInputRequiredError.Create(APayload: TJSONObject);
begin
  inherited Create(SMCPClientInputRequired);
  FPayload := APayload;
end;

destructor EMCPClientInputRequiredError.Destroy;
begin
  FPayload.Free;
  inherited;
end;

{ TMCPClientErrors }

class function TMCPClientErrors.TryUnsupportedVersion(E: EJRPCRemoteError; out AData: TMCPUnsupportedVersionErrorData): Boolean;
begin
  AData := nil;
  Result := False;

  if (E.Code <> MCP_UNSUPPORTED_PROTOCOL_VERSION) or
    (not (E.ErrorData is TJSONObject)) then
    Exit;

  try
    AData := TNeon.JSONToObject<TMCPUnsupportedVersionErrorData>(
      E.ErrorData as TJSONObject, MCPNeonConfig);
  except
    AData := nil;
  end;

  Result := Assigned(AData);
end;

class function TMCPClientErrors.TryRequiredCapabilities(E: EJRPCRemoteError; out ACapabilities: TMCPClientCapabilities): Boolean;
var
  LValue: TJSONValue;
begin
  ACapabilities := [];
  Result := False;

  if (E.Code <> MCP_MISSING_REQUIRED_CLIENT_CAPABILITY) or
    (not (E.ErrorData is TJSONObject)) then
    Exit;

  LValue := TJSONObject(E.ErrorData).GetValue('requiredCapabilities');
  if LValue is TJSONObject then
  begin
    ACapabilities := MCPClientCapabilitiesFromJSON(TJSONObject(LValue));
    Result := True;
  end;
end;

end.
