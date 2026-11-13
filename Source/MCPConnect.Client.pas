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
  System.SysUtils, System.Classes, System.JSON, System.Rtti, System.TypInfo,
  System.Generics.Collections, System.SyncObjs,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Utils,

  JRPC.Core,
  JRPC.Client,
  JRPC.Connection,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Completion,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Subscriptions,
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
  SMCPClientResultNotObject = 'The result is not a JSON object';
  SMCPClientUnknownContent = 'Unknown tool result content type [%s]';
  SMCPClientInputRequired = 'The server requires input this client cannot provide: no handler is configured for it';
  SMCPClientUnknownInputRequest = 'Unknown input request [%s]';
  SMCPClientInputHandlerNil = 'The input handler for [%s] returned no result';
  SMCPClientTooManyRounds = 'The tool call still requires input after %d rounds';
  SMCPClientInvalidPromptMessage = 'A prompt message is missing its content';
  SMCPListenOverConnection = 'subscriptions/listen is not implemented over a connection transport yet';
  SMCPListenNoTransport = 'No transport factory for subscriptions/listen';
  SMCPListenFilterRequired = 'A subscription filter is required';
  SMCPListenAckTimeout = 'The server did not acknowledge the subscription in time';
  SMCPListenInvalidFrame = 'The subscription stream carried an unreadable message';
  SMCPListenRefused = 'The subscription was refused: %s';

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
  ///   Called with a notification that arrived on a subscription stream.
  /// </summary>
  /// <remarks>
  ///   The object belongs to the stream and is valid only for the duration of
  ///   the callback.
  /// </remarks>
  TMCPSubscriptionNotification = reference to procedure(const ANotification: TJRPCNotification);

  /// <summary>
  ///   An open subscriptions/listen stream.
  /// </summary>
  /// <remarks>
  ///   Listen waits for the server's acknowledgement before returning one of
  ///   these, and raises when the server refuses the stream instead.
  ///   Blocking is a read on the stream's own thread; Close aborts it, and
  ///   Wait blocks until the stream has ended for any reason.
  /// </remarks>
  IMCPSubscription = interface
  ['{B4C5D6E7-F809-4A12-9BDE-3F4A5B6C7D8E}']
    /// <summary>
    ///   Stops the stream (aborting the read). Idempotent.
    /// </summary>
    procedure Close;

    /// <summary>
    ///   True once the stream has ended, locally or by the server.
    /// </summary>
    function Completed: Boolean;

    /// <summary>
    ///   Waits for the stream to end. False on timeout; 0 waits forever.
    /// </summary>
    function Wait(ATimeout: Cardinal): Boolean; overload;

    /// <summary>
    ///   Waits for the stream to end, with no timeout.
    /// </summary>
    function Wait: Boolean; overload;

    /// <summary>
    ///   True once the server acknowledged the subscription.
    /// </summary>
    function Acknowledged: Boolean;
  end;

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
    FTransportFactory: TFunc<IJRPCStreamingTransport>;
    FListenTimeout: Cardinal;

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
    function ExecuteMrtr(const AMethod: string; AParams: TInputRequestParams): TJSONObject;
    function Fulfill(AParams: TInputRequestParams; AResult: TInputRequiredResult): Boolean;
    class function ParseResourceContent(AValue: TJSONObject): TResourceContents; static;
    function ParseReadResourceResult(AValue: TJSONObject): TReadResourceResult;
    function ParseGetPromptResult(AValue: TJSONObject): TGetPromptResult;
    function ParsePromptMessage(AValue: TJSONObject): TPromptMessageBase;
    function NewPromptMessage<T: TToolContent, constructor>(ARole: TRole; AContent: TJSONObject): TPromptMessageBase;
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
    ///   The whole resources/list, following nextCursor until the list ends.
    ///   The caller owns the list and its resources.
    /// </summary>
    function ListResources: TMCPResources;

    /// <summary>
    ///   The whole resources/templates/list, following nextCursor. The caller
    ///   owns the list and its templates.
    /// </summary>
    function ListResourceTemplates: TMCPTemplates;

    /// <summary>
    ///   Reads a resource and returns its typed contents: each item is either
    ///   a TTextResourceContents or a TBlobResourceContents.
    /// </summary>
    /// <remarks>
    ///   resources/read may answer "input_required"; the client runs the MRTR
    ///   round trip with the same handlers tools/call uses.
    /// </remarks>
    function ReadResource(const AUri: string): TReadResourceResult;

    /// <summary>
    ///   The whole prompts/list, following nextCursor until the list ends.
    ///   The caller owns the list and its prompts.
    /// </summary>
    function ListPrompts: TMCPPrompts;

    /// <summary>
    ///   Gets a prompt with its arguments and returns the typed messages:
    ///   each is a TPromptMessage<T> with the concrete content class.
    /// </summary>
    /// <remarks>
    ///   prompts/get may answer "input_required"; the client runs the MRTR
    ///   round trip with the same handlers tools/call uses.
    /// </remarks>
    function GetPrompt(const AName: string; AArguments: TJSONObject = nil): TGetPromptResult; overload;

    /// <summary>
    ///   The same get with params the caller built. AParams stays the caller's.
    /// </summary>
    function GetPrompt(AParams: TGetPromptRequestParams): TGetPromptResult; overload;

    /// <summary>
    ///   Asks for completions with params the caller built. AParams stays the
    ///   caller's.
    /// </summary>
    function Complete(AParams: TCompleteRequestParams): TCompleteResult; overload;

    /// <summary>
    ///   Asks for completions of one argument of ARef, copying the reference's
    ///   fields into the request.
    /// </summary>
    function Complete(const ARef: TMCPCompletionReference; const AArgumentName, AArgumentValue: string): TCompleteResult; overload;

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

    /// <summary>
    ///   Opens a subscriptions/listen stream and returns once the server has
    ///   acknowledged it.
    /// </summary>
    /// <remarks>
    ///   Each listen needs a transport of its own because the stream stays
    ///   open: assign TransportFactory (a new HTTP transport with no read
    ///   timeout) before listening while other calls are in flight. Without a
    ///   factory the client's own transport is reused, which is only safe when
    ///   nothing else is called until the subscription closes.
    ///
    ///   Over a connection transport (stdio) listen is not implemented yet and
    ///   raises EJRPCClientError.
    /// </remarks>
    function Listen(const AFilter: TSubscriptionFilter; const AOnNotification: TMCPSubscriptionNotification): IMCPSubscription;

    /// <summary>The version negotiated by Connect.</summary>
    property ProtocolVersion: string read FProtocolVersion;

    /// <summary>
    ///   Milliseconds Call waits for an answer on a connection transport; 0
    ///   waits forever. The HTTP transport has its own timeouts.
    /// </summary>
    property CallTimeout: Cardinal read FCallTimeout write FCallTimeout;

    /// <summary>
    ///   Milliseconds Listen waits for the server's acknowledgement; 0 waits
    ///   forever. Default 10000.
    /// </summary>
    property ListenTimeout: Cardinal read FListenTimeout write FListenTimeout;

    /// <summary>
    ///   Creates the transport a subscription stream runs on. Each listen
    ///   needs its own transport (one long-lived HTTP request), so set this
    ///   before listening while other calls are in flight.
    /// </summary>
    property TransportFactory: TFunc<IJRPCStreamingTransport> read FTransportFactory write FTransportFactory;

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

type
  /// <summary>
  ///   The stream behind IMCPSubscription: frames arrive on the JRPC stream
  ///   thread, the acknowledgement wakes Listen, and Wait polls the stream's
  ///   completion.
  /// </summary>
  TMCPSubscription = class(TInterfacedObject, IMCPSubscription)
  private
    FClient: TJRPCClient;
    FStream: IJRPCStream;
    FOnNotification: TMCPSubscriptionNotification;
    FWake: TEvent;
    FAcknowledged: Boolean;
    FCompleted: Boolean;
    FErrorMessage: string;
    FClosed: Integer;
    function HandleFrame(const AJSON: string): Boolean;
    procedure HandleError(E: Exception);
    procedure Fail(const AMessage: string);
  public
    constructor Create(AClient: TJRPCClient; const AOnNotification: TMCPSubscriptionNotification);
    destructor Destroy; override;

    procedure Attach(const AStream: IJRPCStream);
    function WaitForAck(ATimeout: Cardinal): Boolean;

    procedure Close;
    function Completed: Boolean;
    function Wait(ATimeout: Cardinal): Boolean; overload;
    function Wait: Boolean; overload;
    function Acknowledged: Boolean;
  end;

{ TMCPClient }

procedure TMCPClient.InitState;
begin
  FProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;
  FServerInfo := TImplementation.Create;
  FCapabilities := TServerCapabilities.Create;
  FCallTimeout := 0;
  FListenTimeout := 10000;
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

function TMCPClient.ExecuteMrtr(const AMethod: string; AParams: TInputRequestParams): TJSONObject;
var
  LValue: TJSONValue;
  LInput: TInputRequiredResult;
  LRounds: Integer;
begin
  LRounds := 0;

  while True do
  begin
    LValue := Call(AMethod, AParams);
    try
      if not (LValue is TJSONObject) then
        raise EJRPCClientError.Create(SMCPClientResultNotObject);

      if not IsInputRequired(TJSONObject(LValue)) then
      begin
        // Hand the result to the caller; the finally must not free it.
        Result := TJSONObject(LValue);
        LValue := nil;
        Exit;
      end;

      Inc(LRounds);
      if LRounds > MCP_CLIENT_MAX_ROUNDS then
        raise EJRPCClientError.CreateFmt(SMCPClientTooManyRounds,
          [MCP_CLIENT_MAX_ROUNDS]);

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

function TMCPClient.CallTool(AParams: TCallToolRequestParams): TCallToolResult;
var
  LValue: TJSONObject;
begin
  LValue := ExecuteMrtr('tools/call', AParams);
  try
    Result := ParseCallToolResult(LValue);
  finally
    LValue.Free;
  end;
end;

function TMCPClient.ListResources: TMCPResources;
var
  LParams: TPaginatedRequestParams;
  LPage: TListResourcesResult;
  LResource: TMCPResource;
  LCursor: string;
begin
  Result := TMCPResources.Create(True);
  try
    LCursor := '';
    repeat
      LParams := TPaginatedRequestParams.Create;
      try
        if LCursor <> '' then
          LParams.Cursor := LCursor;

        LPage := ExecuteAs<TListResourcesResult>('resources/list', LParams);
        try
          // The page gives its items up to the caller's list.
          LPage.Resources.OwnsObjects := False;
          for LResource in LPage.Resources do
            Result.Add(LResource);

          if LPage.NextCursor.HasValue then
            LCursor := LPage.NextCursor.Value
          else
            LCursor := '';
        finally
          LPage.Free;
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

function TMCPClient.ListResourceTemplates: TMCPTemplates;
var
  LParams: TPaginatedRequestParams;
  LPage: TListResourceTemplatesResult;
  LTemplate: TMCPResourceTemplate;
  LCursor: string;
begin
  Result := TMCPTemplates.Create(True);
  try
    LCursor := '';
    repeat
      LParams := TPaginatedRequestParams.Create;
      try
        if LCursor <> '' then
          LParams.Cursor := LCursor;

        LPage := ExecuteAs<TListResourceTemplatesResult>(
          'resources/templates/list', LParams);
        try
          LPage.ResourceTemplates.OwnsObjects := False;
          for LTemplate in LPage.ResourceTemplates do
            Result.Add(LTemplate);

          if LPage.NextCursor.HasValue then
            LCursor := LPage.NextCursor.Value
          else
            LCursor := '';
        finally
          LPage.Free;
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

function TMCPClient.ReadResource(const AUri: string): TReadResourceResult;
var
  LParams: TReadResourceParams;
  LValue: TJSONObject;
begin
  LParams := TReadResourceParams.Create;
  try
    LParams.Uri := AUri;

    LValue := ExecuteMrtr('resources/read', LParams);
    try
      Result := ParseReadResourceResult(LValue);
    finally
      LValue.Free;
    end;
  finally
    LParams.Free;
  end;
end;

function TMCPClient.ListPrompts: TMCPPrompts;
var
  LParams: TPaginatedRequestParams;
  LPage: TListPromptsResult;
  LPrompt: TMCPPrompt;
  LCursor: string;
begin
  Result := TMCPPrompts.Create(True);
  try
    LCursor := '';
    repeat
      LParams := TPaginatedRequestParams.Create;
      try
        if LCursor <> '' then
          LParams.Cursor := LCursor;

        LPage := ExecuteAs<TListPromptsResult>('prompts/list', LParams);
        try
          LPage.Prompts.OwnsObjects := False;
          for LPrompt in LPage.Prompts do
            Result.Add(LPrompt);

          if LPage.NextCursor.HasValue then
            LCursor := LPage.NextCursor.Value
          else
            LCursor := '';
        finally
          LPage.Free;
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

function TMCPClient.GetPrompt(const AName: string; AArguments: TJSONObject): TGetPromptResult;
var
  LParams: TGetPromptRequestParams;
begin
  LParams := TGetPromptRequestParams.Create;
  try
    LParams.Name := AName;

    if Assigned(AArguments) then
    begin
      LParams.Arguments.Free;
      LParams.Arguments := AArguments.Clone as TJSONObject;
    end;

    Result := GetPrompt(LParams);
  finally
    LParams.Free;
  end;
end;

function TMCPClient.GetPrompt(AParams: TGetPromptRequestParams): TGetPromptResult;
var
  LValue: TJSONObject;
begin
  LValue := ExecuteMrtr('prompts/get', AParams);
  try
    Result := ParseGetPromptResult(LValue);
  finally
    LValue.Free;
  end;
end;

function TMCPClient.Complete(AParams: TCompleteRequestParams): TCompleteResult;
begin
  Result := ExecuteAs<TCompleteResult>('completion/complete', AParams);
end;

function TMCPClient.Complete(const ARef: TMCPCompletionReference; const AArgumentName, AArgumentValue: string): TCompleteResult;
var
  LParams: TCompleteRequestParams;
begin
  LParams := TCompleteRequestParams.Create;
  try
    LParams.Ref.&Type := ARef.&Type;
    LParams.Ref.Name := ARef.Name;
    LParams.Ref.Title := ARef.Title;
    LParams.Ref.Uri := ARef.Uri;

    LParams.Argument.Name := AArgumentName;
    LParams.Argument.Value := AArgumentValue;

    Result := Complete(LParams);
  finally
    LParams.Free;
  end;
end;

class function TMCPClient.ParseResourceContent(AValue: TJSONObject): TResourceContents;
var
  LBlob: TJSONValue;
begin
  LBlob := AValue.GetValue('blob');
  if LBlob is TJSONString then
    Result := TBlobResourceContents.Create
  else
    Result := TTextResourceContents.Create;

  try
    // The concrete class is known now, so Neon binds "text" or "blob"
    // instead of dropping it on the base TResourceContents.
    TNeon.JSONToObject(Result, AValue, MCPNeonConfig);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPClient.ParseReadResourceResult(AValue: TJSONObject): TReadResourceResult;
var
  LContents, LMeta, LTtl, LScope, LItem: TJSONValue;
begin
  Result := TReadResourceResult.Create;
  try
    LMeta := AValue.GetValue('_meta');
    if LMeta is TJSONObject then
      TNeon.JSONToObject(Result.ResultMeta, TJSONObject(LMeta), MCPNeonConfig);

    LTtl := AValue.GetValue('ttlMs');
    if LTtl is TJSONNumber then
      Result.TtlMs := UInt64(TJSONNumber(LTtl).AsInt64);

    LScope := AValue.GetValue('cacheScope');
    if LScope is TJSONString then
    begin
      if SameText(LScope.Value, 'public') then
        Result.CacheScope := TCacheScope.ScopePublic
      else if SameText(LScope.Value, 'private') then
        Result.CacheScope := TCacheScope.ScopePrivate;
    end;

    LContents := AValue.GetValue('contents');
    if LContents is TJSONArray then
      for LItem in TJSONArray(LContents) do
        if LItem is TJSONObject then
          Result.AddContent(ParseResourceContent(TJSONObject(LItem)));
  except
    Result.Free;
    raise;
  end;
end;

function TMCPClient.ParseGetPromptResult(AValue: TJSONObject): TGetPromptResult;
var
  LMessages, LMeta, LDescription, LItem: TJSONValue;
begin
  Result := TGetPromptResult.Create;
  try
    LMeta := AValue.GetValue('_meta');
    if LMeta is TJSONObject then
      TNeon.JSONToObject(Result.ResultMeta, TJSONObject(LMeta), MCPNeonConfig);

    LDescription := AValue.GetValue('description');
    if LDescription is TJSONString then
      Result.Description := TJSONString(LDescription).Value;

    LMessages := AValue.GetValue('messages');
    if LMessages is TJSONArray then
      for LItem in TJSONArray(LMessages) do
        if LItem is TJSONObject then
          Result.Messages.Add(ParsePromptMessage(TJSONObject(LItem)));
  except
    Result.Free;
    raise;
  end;
end;

function TMCPClient.ParsePromptMessage(AValue: TJSONObject): TPromptMessageBase;
var
  LRole: TRole;
  LRoleValue, LContentValue, LTypeValue, LResource, LBlob: TJSONValue;
  LType: string;
begin
  LRole := TRole.Assistant;
  LRoleValue := AValue.GetValue('role');
  if (LRoleValue is TJSONString) and SameText(LRoleValue.Value, 'user') then
    LRole := TRole.User;

  LContentValue := AValue.GetValue('content');
  if not (LContentValue is TJSONObject) then
    raise EJRPCClientError.Create(SMCPClientInvalidPromptMessage);

  LTypeValue := TJSONObject(LContentValue).GetValue('type');
  if Assigned(LTypeValue) then
    LType := LTypeValue.Value
  else
    LType := '';

  if LType = MCP_CONTENT_TEXT then
    Result := NewPromptMessage<TTextContent>(LRole, TJSONObject(LContentValue))
  else if LType = MCP_CONTENT_IMAGE then
    Result := NewPromptMessage<TImageContent>(LRole, TJSONObject(LContentValue))
  else if LType = MCP_CONTENT_AUDIO then
    Result := NewPromptMessage<TAudioContent>(LRole, TJSONObject(LContentValue))
  else if LType = 'resource_link' then
    Result := NewPromptMessage<TResourceLink>(LRole, TJSONObject(LContentValue))
  else if LType = 'resource' then
  begin
    // An embedded resource is text or blob, discriminated inside "resource".
    LResource := TJSONObject(LContentValue).GetValue('resource');
    if LResource is TJSONObject then
      LBlob := TJSONObject(LResource).GetValue('blob')
    else
      LBlob := nil;

    if Assigned(LBlob) then
      Result := NewPromptMessage<TEmbeddedResourceBlob>(LRole,
        TJSONObject(LContentValue))
    else
      Result := NewPromptMessage<TEmbeddedResourceText>(LRole,
        TJSONObject(LContentValue));
  end
  else
    raise EJRPCClientError.CreateFmt(SMCPClientUnknownContent, [LType]);
end;

function TMCPClient.NewPromptMessage<T>(ARole: TRole; AContent: TJSONObject): TPromptMessageBase;
var
  LMessage: TPromptMessage<T>;
begin
  LMessage := TPromptMessage<T>.Create(ARole);
  try
    TNeon.JSONToObject(LMessage.Content, AContent, MCPNeonConfig);
  except
    LMessage.Free;
    raise;
  end;

  Result := LMessage;
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

function TMCPClient.Fulfill(AParams: TInputRequestParams; AResult: TInputRequiredResult): Boolean;
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

function TMCPClient.Listen(const AFilter: TSubscriptionFilter; const AOnNotification: TMCPSubscriptionNotification): IMCPSubscription;
var
  LTransport: IJRPCStreamingTransport;
  LJRPC: TJRPCClient;
  LSub: TMCPSubscription;
  LRequest: TJRPCRequest;
  LParams: TSubscriptionsListenRequestParams;
  LReturned: Boolean;
begin
  if Assigned(FConnection) then
    raise EJRPCClientError.Create(SMCPListenOverConnection);

  if not Assigned(AFilter) then
    raise EJRPCClientError.Create(SMCPListenFilterRequired);

  if Assigned(FTransportFactory) then
    LTransport := FTransportFactory()
  else if Assigned(FJRPC) then
  begin
    // The client's own transport: safe only while nothing else is called. A
    // transport without streaming support cannot hold a listen stream.
    if not Supports(FJRPC.Transport, IJRPCStreamingTransport, LTransport) then
      LTransport := nil;
  end
  else
    LTransport := nil;

  if not Assigned(LTransport) then
    raise EJRPCClientError.Create(SMCPListenNoTransport);

  LSub := nil;
  LJRPC := nil;
  LRequest := nil;
  LReturned := False;
  try
    LJRPC := TJRPCClient.Create(nil);

    LParams := TSubscriptionsListenRequestParams.Create;
    try
      AFilter.AssignTo(LParams.Notifications);
      LRequest := BuildRequest('subscriptions/listen', LParams);
    finally
      LParams.Free;
    end;

    LJRPC.Transport := LTransport;
    LJRPC.NeonConfig := MCPNeonConfig;

    LSub := TMCPSubscription.Create(LJRPC, AOnNotification);
    LJRPC.OnError := LSub.HandleError;

    // The frame proc is a method of LSub: keep the subscription alive until
    // the stream completes (Close and Wait before releasing it).
    LSub.Attach(LJRPC.ExecuteStream(LRequest,
      function(const AJSON: string): Boolean
      begin
        Result := LSub.HandleFrame(AJSON);
      end));

    if not LSub.WaitForAck(FListenTimeout) then
    begin
      LSub.Close;
      raise EJRPCClientError.Create(SMCPListenAckTimeout);
    end;

    Result := LSub;
    LReturned := True;
  finally
    LRequest.Free;

    // Once created, LSub owns the client; when the subscription is handed to
    // the caller it owns both.
    if LReturned then
      // The caller owns LSub and, through it, LJRPC.
    else if Assigned(LSub) then
      LSub.Free
    else
      LJRPC.Free;
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

{ TMCPSubscription }

constructor TMCPSubscription.Create(AClient: TJRPCClient; const AOnNotification: TMCPSubscriptionNotification);
begin
  inherited Create;
  FClient := AClient;
  FOnNotification := AOnNotification;
  FWake := TEvent.Create(nil, True, False, '');
end;

destructor TMCPSubscription.Destroy;
begin
  // The contract is Close and Wait before releasing, but a caller that drops
  // the interface still gets a clean shutdown: abort, wait for the reader,
  // then free the client it was using.
  if Assigned(FStream) then
  begin
    FStream.Close;
    if not FStream.Completed then
      FStream.Wait(5000);
  end;

  FStream := nil;
  FClient.Free;
  FWake.Free;
  inherited;
end;

procedure TMCPSubscription.Attach(const AStream: IJRPCStream);
begin
  FStream := AStream;
end;

procedure TMCPSubscription.Fail(const AMessage: string);
begin
  if FErrorMessage = '' then
    FErrorMessage := AMessage;
  FCompleted := True;
  FWake.SetEvent;
end;

procedure TMCPSubscription.HandleError(E: Exception);
begin
  Fail(E.Message);
end;

function TMCPSubscription.HandleFrame(const AJSON: string): Boolean;
var
  LDocument: TJSONValue;
  LMessages: TJRPCMessages;
  LMessage: TJRPCMessage;
  LNotification: TJRPCNotification;
begin
  Result := FClosed = 0;
  if not Result then
    Exit;

  LDocument := TJSONObject.ParseJSONValue(AJSON);
  if not Assigned(LDocument) then
  begin
    Fail(SMCPListenInvalidFrame);
    Exit(False);
  end;

  try
    LMessages := TJRPCMessages.CreateFromJson(LDocument);
    try
      if LMessages.Count <> 1 then
      begin
        Fail(SMCPListenInvalidFrame);
        Exit(False);
      end;

      LMessage := LMessages.List[0];

      if LMessage is TJRPCNotification then
      begin
        LNotification := LMessage as TJRPCNotification;

        if LNotification.Method = MCP_NOTIFY_SUBSCRIPTIONS_ACKNOWLEDGED then
        begin
          FAcknowledged := True;
          FWake.SetEvent;
        end
        else if Assigned(FOnNotification) then
          FOnNotification(LNotification);
      end
      else if LMessage is TJRPCResponse then
      begin
        // The server closed the stream gracefully: the listen result.
        FCompleted := True;
        FWake.SetEvent;
        Result := False;
      end
      else if LMessage is TJRPCError then
      begin
        Fail(Format(SMCPListenRefused,
          [(LMessage as TJRPCError).Error.Message.Value]));
        Result := False;
      end
      else
      begin
        Fail(SMCPListenInvalidFrame);
        Result := False;
      end;
    finally
      LMessages.Free;
    end;
  finally
    LDocument.Free;
  end;

  if FClosed <> 0 then
    Result := False;
end;

function TMCPSubscription.WaitForAck(ATimeout: Cardinal): Boolean;
var
  LWait: TWaitResult;
begin
  if ATimeout = 0 then
    LWait := FWake.WaitFor($FFFFFFFF)
  else
    LWait := FWake.WaitFor(ATimeout);

  if LWait <> wrSignaled then
    Exit(False);

  if FErrorMessage <> '' then
    raise EJRPCClientError.Create(FErrorMessage);

  Result := FAcknowledged;
end;

procedure TMCPSubscription.Close;
begin
  if TInterlocked.Exchange(FClosed, 1) = 0 then
  begin
    if Assigned(FStream) then
      FStream.Close;
    FWake.SetEvent;
  end;
end;

function TMCPSubscription.Completed: Boolean;
begin
  if FCompleted then
    Exit(True);

  if Assigned(FStream) and FStream.Completed then
  begin
    FCompleted := True;
    Exit(True);
  end;

  Result := False;
end;

function TMCPSubscription.Wait(ATimeout: Cardinal): Boolean;
var
  LStart: UInt64;
begin
  LStart := TThread.GetTickCount64;

  while not Completed do
  begin
    if (ATimeout <> 0) and
      (TThread.GetTickCount64 - LStart >= UInt64(ATimeout)) then
      Exit(False);
    Sleep(20);
  end;

  Result := True;
end;

function TMCPSubscription.Wait: Boolean;
begin
  Result := Wait(0);
end;

function TMCPSubscription.Acknowledged: Boolean;
begin
  Result := FAcknowledged;
end;

end.
