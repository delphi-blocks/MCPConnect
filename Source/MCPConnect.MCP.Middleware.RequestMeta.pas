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
unit MCPConnect.MCP.Middleware.RequestMeta;

{
  The per-request protocol fields of MCP 2026-07-28 (specification, "Basic",
  section "Per-request protocol fields").

  The revision has no handshake: a server may not infer the protocol version,
  the client's identity or its capabilities from anything that came before, so
  every request states them itself, in the "_meta" of its params. Four keys are
  defined there -

    io.modelcontextprotocol/protocolVersion     REQUIRED
    io.modelcontextprotocol/clientCapabilities  REQUIRED
    io.modelcontextprotocol/clientInfo          optional
    io.modelcontextprotocol/logLevel            optional

  - and a request missing one of the two required ones is malformed: it MUST be
  refused with Invalid Params (-32602). A version the server does not implement
  MUST be refused with UnsupportedProtocolVersion (-32022), whose data carries
  the versions it does implement. Over HTTP both are a 400.

  This middleware does that, and then publishes what it parsed: the
  TRequestMetaObject goes into the request context, so a tool class reaches it
  with [Context] and the framework has one place to read the log level, the
  progress token and the client capabilities from - which is what gates log
  notifications, progress notifications and the MRTR input requests.

  A note on scope. It runs on the request chain, which carries every JSON-RPC
  request the server answers, not only the MCP ones: a server may register an
  API of its own next to the MCP namespaces, and a plain JSON-RPC method has no
  "_meta" contract to hold it to. Only the namespaces the specification defines
  are checked - see MCP_METHOD_NAMESPACES.
}

interface

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.JSON,

  MCPConnect.MCP.Types.Base,
  MCPConnect.JRPC.Middleware;

const
  /// <summary>
  ///   The JSON-RPC namespaces the MCP specification owns. A method outside
  ///   them is not an MCP request: it belongs to an API the server registered
  ///   alongside the protocol, and states no protocol version because there is
  ///   no protocol of ours for it to state.
  /// </summary>
  MCP_METHOD_NAMESPACES: array[0..6] of string = (
    'server', 'tools', 'resources', 'prompts', 'completion', 'subscriptions',
    'notifications'
  );

type
  /// <summary>
  ///   Enforces the per-request "_meta" contract of MCP 2026-07-28 and hands
  ///   the parsed metadata to the rest of the request.
  /// </summary>
  /// <remarks>
  ///   How much is enforced comes from IMCPConfig.Security.SetMetaValidation.
  ///   Strict is the revision as written: the protocol version and the client
  ///   capabilities are required, and a request missing either is Invalid
  ///   Params. Lenient requires neither but still refuses a version the server
  ///   does not speak - a client that says "2025-11-25" is wrong about this
  ///   server whether or not it was obliged to say anything. Off keeps the
  ///   middleware out of the chain.
  ///
  ///   server/discover is checked like every other method. It is the request a
  ///   client uses to learn which versions the server speaks, so exempting it
  ///   is tempting - but the specification exempts nothing, and it does not
  ///   need to: UnsupportedProtocolVersionError carries the supported versions
  ///   in its data, so a client that probes with the wrong version learns the
  ///   right one from the refusal itself.
  /// </remarks>
  TMCPRequestMetaMiddleware = class(TMiddleware, IRequestMiddleware)
  private
    /// <summary>
    ///   Whether AMethod belongs to one of the namespaces the specification
    ///   defines, and so carries the "_meta" contract.
    /// </summary>
    class function IsMCPMethod(const AMethod: string): Boolean; static;

    /// <summary>
    ///   Whether AVersion is one this build of the server speaks
    ///   (MCP_PROTOCOL_SUPPORTED_VERSIONS).
    /// </summary>
    class function IsSupportedVersion(const AVersion: string): Boolean; static;
  public
    /// <summary>
    ///   Inside authentication and inside the header check, since it is the
    ///   same three facts read off the body: a request refused for its
    ///   credentials, or for headers that contradict it, must not be described
    ///   as a malformed "_meta" instead.
    /// </summary>
    class function DefaultPriority: Integer; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

implementation

uses
  JRPC.Core,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.Configuration.MCP;

{ TMCPRequestMetaMiddleware }

class function TMCPRequestMetaMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_AUTHENTICATION + 300;
end;

class function TMCPRequestMetaMiddleware.IsMCPMethod(const AMethod: string): Boolean;
var
  LSeparator: Integer;
  LNamespace: string;
  LKnown: string;
begin
  LSeparator := AMethod.IndexOf('/');
  if LSeparator <= 0 then
    Exit(False);

  LNamespace := AMethod.Substring(0, LSeparator);

  for LKnown in MCP_METHOD_NAMESPACES do
    if SameText(LNamespace, LKnown) then
      Exit(True);

  Result := False;
end;

class function TMCPRequestMetaMiddleware.IsSupportedVersion(const AVersion: string): Boolean;
var
  LVersion: string;
begin
  for LVersion in MCP_PROTOCOL_SUPPORTED_VERSIONS do
    if AVersion = LVersion then
      Exit(True);

  Result := False;
end;

procedure TMCPRequestMetaMiddleware.Handle(AContext: TMiddlewareContext;
  const AChain: TMiddlewareChain);
var
  LRequest: TJRPCRequest;
  LConfig: TMCPConfig;
  LLevel: TMCPValidationLevel;
  LParams: TJSONValue;
  LMetaJSON: TJSONValue;
  LMeta: TRequestMetaObject;
  LDeclared: TMCPDeclaredCapabilities;
  LProgress: TMCPProgress;
begin
  LConfig := AContext.Find<TMCPConfig>;

  // Not an MCP server, or one that wants none of this
  if not Assigned(LConfig) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LLevel := LConfig.Security.MetaValidation;
  if LLevel = TMCPValidationLevel.Off then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // The request chain only ever carries requests, but the cast is guarded all
  // the same: a middleware has no business raising EInvalidCast at anyone.
  if not (AContext.Message is TJRPCRequest) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LRequest := TJRPCRequest(AContext.Message);
  if not IsMCPMethod(LRequest.Method) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LParams := LRequest.Params;
  if LParams is TJSONObject then
    LMetaJSON := TJSONObject(LParams).GetValue('_meta')
  else
    // Params by position, or none at all: MCP names its params, so there is
    // nowhere for a "_meta" to be
    LMetaJSON := nil;

  if not (LMetaJSON is TJSONObject) then
  begin
    if LLevel = TMCPValidationLevel.Strict then
      raise EMCPInvalidRequestMetaError.CreateForField('_meta');

    AChain.Next(AContext);
    Exit;
  end;

  // Owned by the request rather than by this middleware: it is published into
  // the context below and read for as long as the request runs, which outlives
  // the chain instance.
  LMeta := TRequestMetaObject.Create;
  AContext.Own(LMeta);
  TNeon.JSONToObject(LMeta, LMetaJSON, MCPNeonConfig);

  if LLevel = TMCPValidationLevel.Strict then
  begin
    if LMeta.ProtocolVersion.Trim.IsEmpty then
      raise EMCPInvalidRequestMetaError.CreateForField(MCP_META_PROTOCOL_VERSION);

    // Looked up in the JSON and not on LMeta: TClientCapabilities is a class
    // the object always holds, so an absent member and an empty one are the
    // same object once deserialized, and only the raw JSON still knows which
    // of the two the client sent.
    if not Assigned(TJSONObject(LMetaJSON).GetValue(MCP_META_CLIENT_CAPABILITIES)) then
      raise EMCPInvalidRequestMetaError.CreateForField(MCP_META_CLIENT_CAPABILITIES);
  end;

  // At every level: this is about the version the client stated, not about
  // whether it was obliged to state one.
  if not LMeta.ProtocolVersion.Trim.IsEmpty and
     not IsSupportedVersion(LMeta.ProtocolVersion) then
    raise EMCPUnsupportedProtocolVersionError.CreateForVersion(LMeta.ProtocolVersion);

  // What the rest of the request reads: [Context] on a tool class, and the
  // framework itself for the log level, the progress token and the client
  // capabilities.
  AContext.RPCContext.AddContent(LMeta);

  // The capabilities again, as a set read from the JSON rather than from the
  // object: a declaration is made by the presence of a member, and
  // TClientCapabilities holds its members whether the client sent them or not.
  // TMCPApi.RequireInputCapabilities is what reads this.
  LDeclared := TMCPDeclaredCapabilities.Create(
    MCPClientCapabilitiesFromJSON(
      TJSONObject(LMetaJSON).GetValue(MCP_META_CLIENT_CAPABILITIES) as TJSONObject));
  AContext.Own(LDeclared);
  AContext.RPCContext.AddContent(LDeclared);

  // And the progress token to the channel the transport put in the context.
  // Told even when there is none: "the client asked for no progress" is a fact
  // worth knowing, and it is what lets the transport tell an unsolicited
  // progress notification from an unverifiable one.
  LProgress := AContext.Find<TMCPProgress>;
  if Assigned(LProgress) then
    LProgress.Declare(LMeta.ProgressToken);

  AChain.Next(AContext);
end;

end.
