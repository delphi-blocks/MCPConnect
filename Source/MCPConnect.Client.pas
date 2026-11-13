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
  System.SysUtils, System.JSON,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JRPC.Core,
  JRPC.Client,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
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

resourcestring
  SMCPClientParamsRequired = 'The request params are required: they carry the per-request "_meta"';
  SMCPClientParamsSerialization = 'The request params could not be serialized to JSON';

type
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
  ///   Step 1 covers discovery and tools/list. Tool calls are possible through
  ///   Call, which returns the raw JSON result; the typed content model (and
  ///   MRTR) is the next step.
  /// </remarks>
  TMCPClient = class
  private
    FJRPC: TJRPCClient;
    FProtocolVersion: string;
    FServerInfo: TImplementation;
    FCapabilities: TServerCapabilities;
    FInstructions: string;
    FOnNotification: TJRPCNotificationProc;
    FOnError: TJRPCErrorProc;

    procedure ForwardNotification(const ANotification: TJRPCNotification);
    procedure ForwardError(E: Exception);
    procedure ApplyMeta(AParams: TRequestMetaParams);
    function BuildRequest(const AMethod: string; AParams: TRequestMetaParams): TJRPCRequest;
    function Discover: TDiscoverResult;
    function TryNegotiateVersion(E: EJRPCRemoteError): Boolean;
  public
    /// <summary>
    ///   Takes the transport. Connect performs discovery and version
    ///   negotiation.
    /// </summary>
    constructor Create(const ATransport: IJRPCStreamingTransport);
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

    /// <summary>The version negotiated by Connect.</summary>
    property ProtocolVersion: string read FProtocolVersion;

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
    ///   A failure of the call itself, reported once per call.
    /// </summary>
    property OnError: TJRPCErrorProc read FOnError write FOnError;
  end;

implementation

{ TMCPClient }

constructor TMCPClient.Create(const ATransport: IJRPCStreamingTransport);
begin
  inherited Create;

  FProtocolVersion := MCP_LATEST_PROTOCOL_VERSION;
  FServerInfo := TImplementation.Create;
  FCapabilities := TServerCapabilities.Create;

  FJRPC := TJRPCClient.Create(nil);
  FJRPC.Transport := ATransport;
  // The MCP type model is made of fields written in camel case.
  FJRPC.NeonConfig := MCPNeonConfig;
  FJRPC.OnNotification := ForwardNotification;
  FJRPC.OnError := ForwardError;
end;

destructor TMCPClient.Destroy;
begin
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

  if E.Code <> MCP_UNSUPPORTED_PROTOCOL_VERSION then
    Exit;

  if not (E.ErrorData is TJSONObject) then
    Exit;

  try
    LData := TNeon.JSONToObject<TMCPUnsupportedVersionErrorData>(
      E.ErrorData as TJSONObject, MCPNeonConfig);
  except
    // A payload that does not have the documented shape buys no retry.
    Exit(False);
  end;

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

function TMCPClient.Call(const AMethod: string; AParams: TRequestMetaParams): TJSONValue;
var
  LRequest: TJRPCRequest;
begin
  LRequest := BuildRequest(AMethod, AParams);
  try
    Result := FJRPC.ExecuteAsJSONValue(LRequest);
  finally
    LRequest.Free;
  end;
end;

function TMCPClient.ExecuteAs<T>(const AMethod: string; AParams: TRequestMetaParams): T;
var
  LRequest: TJRPCRequest;
begin
  LRequest := BuildRequest(AMethod, AParams);
  try
    Result := FJRPC.ExecuteAs<T>(LRequest);
  finally
    LRequest.Free;
  end;
end;

end.
