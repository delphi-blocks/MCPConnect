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

  MCPConnect.JRPC.Core,
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
  SMCPMissingClientCapability = 'The request requires a client capability that was not declared';
  SMCPMissingClientCapabilityFmt = 'The request requires the [%s] client capability, not declared by the client';
  SMCPUnsupportedProtocolVersionFmt = 'Protocol version [%s] is not supported by this server';

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
  ///   Base class for the errors the MCP specification defines in the code
  ///   range it reserves for itself (-32099..-32020).
  /// </summary>
  /// <remarks>
  ///   Over HTTP all of them MUST be answered with a 400 Bad Request, which is
  ///   what makes the common ancestor worth having: a transport can map the
  ///   whole family with a single "is" test.
  /// </remarks>
  EMCPProtocolError = class(EJRPCException);

  /// <summary>
  ///   Raised when the values in the request's HTTP headers do not match the
  ///   corresponding values in the request body, or when a required header is
  ///   missing or malformed. Carries no "data" payload.
  /// </summary>
  EMCPHeaderMismatchError = class(EMCPProtocolError)
  public
    procedure AfterConstruction; override;

    /// <summary>Header present but contradicting the request body.</summary>
    constructor CreateForHeader(const AHeaderName: string);

    /// <summary>Required header missing or malformed.</summary>
    constructor CreateForMissingHeader(const AHeaderName: string);
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

implementation

function IsMCPProtocolErrorCode(ACode: Integer): Boolean;
begin
  Result := (ACode >= MCP_ERROR_RESERVED_LOW) and (ACode <= MCP_ERROR_RESERVED_HIGH);
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

constructor EMCPHeaderMismatchError.CreateForHeader(const AHeaderName: string);
begin
  CreateFmt(SMCPHeaderMismatchFmt, [AHeaderName]);
end;

constructor EMCPHeaderMismatchError.CreateForMissingHeader(const AHeaderName: string);
begin
  CreateFmt(SMCPHeaderMissingFmt, [AHeaderName]);
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

end.
