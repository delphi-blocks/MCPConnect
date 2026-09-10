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
unit MCPConnect.MCP.Types.Errors;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.JSON,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  JRPC.Core,
  MCPConnect.MCP.Types.Base;

const
  /// <summary>
  ///   JSON-RPC 2.0 reserves -32000..-32099 for implementation-defined server
  ///   errors. MCP partitions that range: -32000..-32019 is legacy (codes
  ///   allocated by implementations before the policy existed; new code MUST
  ///   NOT be allocated there), and -32020..-32099 is reserved for the MCP
  ///   specification itself.
  /// </summary>
  MCP_ERROR_RESERVED_LOW = -32099;
  MCP_ERROR_RESERVED_HIGH = -32020;

  MCP_ERROR_LEGACY_LOW = -32019;
  MCP_ERROR_LEGACY_HIGH = -32000;

  /// <summary>
  ///   The values in the request's HTTP headers do not match the corresponding
  ///   values in the request body, or a required header is missing/malformed.
  /// </summary>
  MCP_HEADER_MISMATCH = -32020;

  /// <summary>
  ///   Processing the request requires a capability the client did not declare
  ///   in the "io.modelcontextprotocol/clientCapabilities" request _meta key.
  /// </summary>
  MCP_MISSING_REQUIRED_CLIENT_CAPABILITY = -32021;

  /// <summary>
  ///   The request's protocol version is unknown to the server or unsupported.
  /// </summary>
  MCP_UNSUPPORTED_PROTOCOL_VERSION = -32022;

  /// <summary>
  ///   Resource-not-found as defined by 2025-11-25 and earlier. A server
  ///   implementing 2026-07-28 MUST NOT emit it - resource-not-found is now
  ///   JRPC_INVALID_PARAMS (-32602) - but a client SHOULD still accept it from
  ///   servers implementing an earlier revision.
  /// </summary>
  MCP_RETIRED_RESOURCE_NOT_FOUND = -32002;

  /// <summary>
  ///   URL elicitation required, as defined by 2025-11-25 only. Retired: it
  ///   MUST NOT be emitted by a server implementing 2026-07-28.
  /// </summary>
  MCP_RETIRED_URL_ELICITATION_REQUIRED = -32042;

resourcestring
  // MCPConnect.MCP.Types.Errors
  SMCPHeaderMismatch = 'Request headers do not match the request body';
  SMCPHeaderMismatchFmt = 'Header [%s] does not match the request body';
  SMCPHeaderMissingFmt = 'Required header [%s] is missing or malformed';
  SMCPRequestMetaMissingFmt = 'The request "_meta" is missing the required [%s] member';
  SMCPMissingClientCapability = 'The request requires a client capability that was not declared';
  SMCPMissingClientCapabilityFmt = 'The request requires the [%s] client capability, not declared by the client';
  SMCPUnsupportedProtocolVersionFmt = 'Protocol version [%s] is not supported by this server';
  SMCPBatchNotSupported = 'The body of a request must be a single JSON-RPC request or notification, not a batch';

type
  /// <summary>
  ///   The client capabilities a request may require, in the granularity the
  ///   "requiredCapabilities" error payload can express.
  /// </summary>
  TMCPClientCapability = (
    Elicitation, ElicitationForm, ElicitationUrl,
    Roots,
    Sampling, SamplingContext, SamplingTools
  );
  TMCPClientCapabilities = set of TMCPClientCapability;

  /// <summary>
  ///   What the client declared it can do, for the request being served. Put
  ///   into the request context by TMCPRequestMetaMiddleware, which is the only
  ///   place that still has the raw "_meta" to read it from.
  /// </summary>
  /// <remarks>
  ///   A class, so that it can live in the context and be injected with
  ///   [Context]; a set on its own could not. Absent from the context means the
  ///   server never looked - meta validation turned off, or a request that
  ///   carried no "_meta" under a lenient one - which is not the same as a
  ///   client that declared nothing, and is why the MRTR check skips rather
  ///   than refuses when it finds none.
  /// </remarks>
  TMCPDeclaredCapabilities = class
  private
    FDeclared: TMCPClientCapabilities;
  public
    constructor Create(ADeclared: TMCPClientCapabilities);

    /// <summary>
    ///   Whichever of ACapabilities the client did not declare. Empty when it
    ///   declared them all, which is the only case a server may proceed in.
    /// </summary>
    function Missing(ACapabilities: TMCPClientCapabilities): TMCPClientCapabilities;

    property Declared: TMCPClientCapabilities read FDeclared;
  end;

  /// <summary>
  ///   The "data" member of an UnsupportedProtocolVersionError (-32022).
  /// </summary>
  /// <remarks>
  ///   The other error carrying a payload, MissingRequiredClientCapability
  ///   (-32021), has no entity of its own: its data is
  ///   {"requiredCapabilities": ClientCapabilities}, which adds no field to the
  ///   existing TClientCapabilities, and it is built by
  ///   MCPClientCapabilitiesToJSON for the reason documented there.
  /// </remarks>
  TMCPUnsupportedVersionErrorData = class
  public
    /// <summary>
    ///   REQUIRED. The protocol version that was requested by the client.
    /// </summary>
    Requested: string;

    /// <summary>
    ///   REQUIRED. Protocol versions the server supports. The client should
    ///   choose a mutually supported version from this list and retry.
    /// </summary>
    Supported: TArray<string>;
  end;

  /// <summary>
  ///   Base class for the errors of the MCP specification that an HTTP
  ///   transport MUST answer with a 400 Bad Request.
  /// </summary>
  /// <remarks>
  ///   The single "is" test is the whole point of the ancestor: the transport
  ///   maps the family in one place rather than keeping a list of codes. Most
  ///   of them live in the code range the specification reserves for itself
  ///   (-32099..-32020), but not all - a request whose per-request "_meta" is
  ///   missing a required field is a plain Invalid Params (-32602) that must
  ///   still be answered 400, and -32602 on its own says nothing about the
  ///   status (an unknown tool name is one too, and that is an ordinary 200).
  /// </remarks>
  EMCPProtocolError = class(EJRPCException);

  /// <summary>
  ///   Raised when the per-request "_meta" is absent or is missing one of the
  ///   fields every 2026-07-28 request MUST carry - the protocol version and
  ///   the client capabilities. Reported as Invalid Params (-32602), which is
  ///   what the specification assigns to a malformed request.
  /// </summary>
  EMCPInvalidRequestMetaError = class(EMCPProtocolError)
  public
    procedure AfterConstruction; override;

    /// <summary>Names the "_meta" member that is missing.</summary>
    constructor CreateForField(const AFieldName: string);
  end;

  /// <summary>
  ///   Raised when the values in the request's HTTP headers do not match the
  ///   corresponding values in the request body. Carries no "data" payload.
  /// </summary>
  EMCPHeaderMismatchError = class(EMCPProtocolError)
  public
    procedure AfterConstruction; override;

    /// <summary>Header present but contradicting the request body.</summary>
    constructor Create(const AHeaderName: string);
  end;

  /// <summary>
  ///   Raised when the values in the request's HTTP headers is
  ///   missing or malformed. Carries no "data" payload.
  /// </summary>
  EMCPHeaderMissingError = class(EMCPProtocolError)
  public
    procedure AfterConstruction; override;

    /// <summary>Required header missing or malformed.</summary>
    constructor Create(const AHeaderName: string);
  end;

  /// <summary>
  ///   Raised when the body carries a JSON-RPC batch. 2026-07-28 requires the
  ///   body of a request to be a single request or a single notification, so a
  ///   top-level array is not something this transport can serve.
  /// </summary>
  /// <remarks>
  ///   Reported as Invalid Request (-32600), the code JSON-RPC 2.0 assigns to a
  ///   Request object that is not valid - the specification reserves no code of
  ///   its own for this. It is an EMCPProtocolError so that the transport
  ///   answers it 400 like every other violation of the transport's own
  ///   contract: what is wrong is the envelope, not the parameters inside it.
  ///
  ///   The JSON-RPC layer underneath (Libs/JRPC) still reads and answers
  ///   batches; this is a policy of the MCP transport, not a missing
  ///   capability.
  /// </remarks>
  EMCPBatchNotSupportedError = class(EMCPProtocolError)
  public
    procedure AfterConstruction; override;

    constructor Create; reintroduce;
  end;

  /// <summary>
  ///   Raised when processing a request requires a client capability the client
  ///   did not declare in its per-request capabilities.
  /// </summary>
  EMCPMissingRequiredClientCapabilityError = class(EMCPProtocolError)
  private
    FRequired: TMCPClientCapabilities;
  public
    procedure AfterConstruction; override;

    /// <summary>
    ///   Builds the exception and its message from the required capabilities.
    /// </summary>
    constructor CreateForCapabilities(ACapabilities: TMCPClientCapabilities);

    function CreateErrorData: TJSONObject; override;

    /// <summary>
    ///   The capabilities the server requires from the client, rendered into
    ///   the "requiredCapabilities" member of the error data.
    /// </summary>
    property Required: TMCPClientCapabilities read FRequired write FRequired;
  end;

  /// <summary>
  ///   Raised when the protocol version carried by the request is unknown to
  ///   the server or unsupported.
  /// </summary>
  EMCPUnsupportedProtocolVersionError = class(EMCPProtocolError)
  private
    FRequested: string;
    FSupported: TArray<string>;
  public
    procedure AfterConstruction; override;

    /// <summary>
    ///   Reports the versions this build of the server speaks
    ///   (MCP_PROTOCOL_SUPPORTED_VERSIONS).
    /// </summary>
    constructor CreateForVersion(const ARequested: string); overload;

    /// <summary>
    ///   Reports an explicit set of supported versions, for a server that
    ///   narrows or extends the built-in list.
    /// </summary>
    constructor CreateForVersion(const ARequested: string; const ASupported: TArray<string>); overload;

    function CreateErrorData: TJSONObject; override;

    /// <summary>The protocol version that was requested by the client.</summary>
    property Requested: string read FRequested write FRequested;

    /// <summary>The protocol versions the server supports.</summary>
    property Supported: TArray<string> read FSupported write FSupported;
  end;

/// <summary>
///   True when ACode falls in the sub-range the MCP specification reserves for
///   itself (-32099..-32020).
/// </summary>
/// <remarks>
///   Over HTTP these MUST be answered with a 400 Bad Request. The check is by
///   code rather than by exception class because a transport writing the
///   response only has the serialized error to work from.
/// </remarks>
function IsMCPProtocolErrorCode(ACode: Integer): Boolean;

/// <summary>
///   The name of a client capability as it appears in a ClientCapabilities
///   object, e.g. "elicitation.form".
/// </summary>
function MCPClientCapabilityName(ACapability: TMCPClientCapability): string;

/// <summary>
///   Renders a set of required client capabilities as a ClientCapabilities
///   JSON object, e.g. {"elicitation":{"form":{}}}. The caller owns the result.
/// </summary>
/// <remarks>
///   Built directly rather than through TClientCapabilities because an empty
///   object is what declares a capability here, and Neon's IncludeIf.NotEmpty
///   - correct for the declaration use of that class - would drop it.
/// </remarks>
function MCPClientCapabilitiesToJSON(ACapabilities: TMCPClientCapabilities): TJSONObject;

/// <summary>
///   Reads back what a client declared: the inverse of the above, over the
///   "io.modelcontextprotocol/clientCapabilities" object of a request's _meta.
/// </summary>
/// <remarks>
///   Read from the JSON and not from a deserialized TClientCapabilities, and
///   for the same reason the required-field check of the request _meta is: the
///   declaration is made by the *presence* of a member, and an object that is
///   always allocated cannot tell an absent capability from an empty one.
/// </remarks>
function MCPClientCapabilitiesFromJSON(AJSON: TJSONObject): TMCPClientCapabilities;

implementation

function IsMCPProtocolErrorCode(ACode: Integer): Boolean;
begin
  Result := (ACode >= MCP_ERROR_RESERVED_LOW) and (ACode <= MCP_ERROR_RESERVED_HIGH);
end;

function MCPClientCapabilitiesFromJSON(AJSON: TJSONObject): TMCPClientCapabilities;

  // A capability is declared by the presence of its member, whatever it holds
  function Declared(AParent: TJSONObject; const AName: string): TJSONObject;
  var
    LValue: TJSONValue;
  begin
    Result := nil;
    if not Assigned(AParent) then
      Exit;

    LValue := AParent.GetValue(AName);
    if LValue is TJSONObject then
      Result := TJSONObject(LValue);
  end;

var
  LChild: TJSONObject;
begin
  Result := [];
  if not Assigned(AJSON) then
    Exit;

  LChild := Declared(AJSON, 'elicitation');
  if Assigned(LChild) then
  begin
    Include(Result, TMCPClientCapability.Elicitation);
    if Assigned(Declared(LChild, 'form')) then
      Include(Result, TMCPClientCapability.ElicitationForm);
    if Assigned(Declared(LChild, 'url')) then
      Include(Result, TMCPClientCapability.ElicitationUrl);
  end;

  LChild := Declared(AJSON, 'sampling');
  if Assigned(LChild) then
  begin
    Include(Result, TMCPClientCapability.Sampling);
    if Assigned(Declared(LChild, 'context')) then
      Include(Result, TMCPClientCapability.SamplingContext);
    if Assigned(Declared(LChild, 'tools')) then
      Include(Result, TMCPClientCapability.SamplingTools);
  end;

  if Assigned(Declared(AJSON, 'roots')) then
    Include(Result, TMCPClientCapability.Roots);
end;

{ TMCPDeclaredCapabilities }

constructor TMCPDeclaredCapabilities.Create(ADeclared: TMCPClientCapabilities);
begin
  inherited Create;
  FDeclared := ADeclared;
end;

function TMCPDeclaredCapabilities.Missing(
  ACapabilities: TMCPClientCapabilities): TMCPClientCapabilities;
begin
  Result := ACapabilities - FDeclared;
end;

function MCPClientCapabilityName(ACapability: TMCPClientCapability): string;
begin
  case ACapability of
    TMCPClientCapability.Elicitation:     Result := 'elicitation';
    TMCPClientCapability.ElicitationForm: Result := 'elicitation.form';
    TMCPClientCapability.ElicitationUrl:  Result := 'elicitation.url';
    TMCPClientCapability.Roots:           Result := 'roots';
    TMCPClientCapability.Sampling:        Result := 'sampling';
    TMCPClientCapability.SamplingContext: Result := 'sampling.context';
    TMCPClientCapability.SamplingTools:   Result := 'sampling.tools';
  else
    Result := '';
  end;
end;

function MCPClientCapabilitiesToJSON(ACapabilities: TMCPClientCapabilities): TJSONObject;

  // Returns the named member of AParent, adding it as an empty object first if
  // it isn't there yet. Declaring a capability with no settings is exactly an
  // empty object, so the parent doubles as both marker and container.
  function EnsureObject(AParent: TJSONObject; const AName: string): TJSONObject;
  var
    LValue: TJSONValue;
  begin
    LValue := AParent.GetValue(AName);
    if LValue is TJSONObject then
      Exit(TJSONObject(LValue));

    Result := TJSONObject.Create;
    AParent.AddPair(AName, Result);
  end;

begin
  Result := TJSONObject.Create;
  try
    if TMCPClientCapability.Elicitation in ACapabilities then
      EnsureObject(Result, 'elicitation');
    if TMCPClientCapability.ElicitationForm in ACapabilities then
      EnsureObject(EnsureObject(Result, 'elicitation'), 'form');
    if TMCPClientCapability.ElicitationUrl in ACapabilities then
      EnsureObject(EnsureObject(Result, 'elicitation'), 'url');

    if TMCPClientCapability.Roots in ACapabilities then
      EnsureObject(Result, 'roots');

    if TMCPClientCapability.Sampling in ACapabilities then
      EnsureObject(Result, 'sampling');
    if TMCPClientCapability.SamplingContext in ACapabilities then
      EnsureObject(EnsureObject(Result, 'sampling'), 'context');
    if TMCPClientCapability.SamplingTools in ACapabilities then
      EnsureObject(EnsureObject(Result, 'sampling'), 'tools');
  except
    Result.Free;
    raise;
  end;
end;

{ EMCPHeaderMismatchError }

procedure EMCPHeaderMismatchError.AfterConstruction;
begin
  inherited;
  FCode := MCP_HEADER_MISMATCH;
end;

constructor EMCPHeaderMismatchError.Create(const AHeaderName: string);
begin
  CreateFmt(SMCPHeaderMismatchFmt, [AHeaderName]);
end;

{ EMCPBatchNotSupportedError }

procedure EMCPBatchNotSupportedError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_INVALID_REQUEST;
end;

constructor EMCPBatchNotSupportedError.Create;
begin
  inherited Create(SMCPBatchNotSupported);
end;

{ EMCPInvalidRequestMetaError }

procedure EMCPInvalidRequestMetaError.AfterConstruction;
begin
  inherited;
  FCode := JRPC_INVALID_PARAMS;
end;

constructor EMCPInvalidRequestMetaError.CreateForField(const AFieldName: string);
begin
  CreateFmt(SMCPRequestMetaMissingFmt, [AFieldName]);
end;

{ EMCPMissingRequiredClientCapabilityError }

procedure EMCPMissingRequiredClientCapabilityError.AfterConstruction;
begin
  inherited;
  FCode := MCP_MISSING_REQUIRED_CLIENT_CAPABILITY;
end;

constructor EMCPMissingRequiredClientCapabilityError.CreateForCapabilities(
  ACapabilities: TMCPClientCapabilities);
var
  LCapability: TMCPClientCapability;
  LNames: string;
begin
  LNames := '';
  for LCapability := Low(TMCPClientCapability) to High(TMCPClientCapability) do
    if LCapability in ACapabilities then
    begin
      if LNames <> '' then
        LNames := LNames + ', ';
      LNames := LNames + MCPClientCapabilityName(LCapability);
    end;

  if LNames = '' then
    Create(SMCPMissingClientCapability)
  else
    CreateFmt(SMCPMissingClientCapabilityFmt, [LNames]);

  FRequired := ACapabilities;
end;

function EMCPMissingRequiredClientCapabilityError.CreateErrorData: TJSONObject;
begin
  // "data" is REQUIRED on this error, so it is emitted even when no capability
  // was named: an empty object then says "no optional capability declared".
  Result := TJSONObject.Create;
  try
    Result.AddPair('requiredCapabilities', MCPClientCapabilitiesToJSON(FRequired));
  except
    Result.Free;
    raise;
  end;
end;

{ EMCPUnsupportedProtocolVersionError }

procedure EMCPUnsupportedProtocolVersionError.AfterConstruction;
begin
  inherited;
  FCode := MCP_UNSUPPORTED_PROTOCOL_VERSION;
end;

constructor EMCPUnsupportedProtocolVersionError.CreateForVersion(const ARequested: string);
begin
  CreateForVersion(ARequested, MCP_PROTOCOL_SUPPORTED_VERSIONS);
end;

constructor EMCPUnsupportedProtocolVersionError.CreateForVersion(const ARequested:
    string; const ASupported: TArray<string>);
begin
  CreateFmt(SMCPUnsupportedProtocolVersionFmt, [ARequested]);
  FRequested := ARequested;
  FSupported := ASupported;
end;

function EMCPUnsupportedProtocolVersionError.CreateErrorData: TJSONObject;
var
  LData: TMCPUnsupportedVersionErrorData;
begin
  LData := TMCPUnsupportedVersionErrorData.Create;
  try
    LData.Requested := FRequested;
    LData.Supported := FSupported;

    Result := TNeon.ObjectToJSON(LData, MCPNeonConfig) as TJSONObject;
  finally
    LData.Free;
  end;
end;

{ EMCPHeaderMissingError }

procedure EMCPHeaderMissingError.AfterConstruction;
begin
  inherited;
  FCode := MCP_HEADER_MISMATCH;
end;

constructor EMCPHeaderMissingError.Create(const AHeaderName: string);
begin
  CreateFmt(SMCPHeaderMissingFmt, [AHeaderName]);
end;

end.
