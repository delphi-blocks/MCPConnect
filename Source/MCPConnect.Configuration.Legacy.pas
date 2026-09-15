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
unit MCPConnect.Configuration.Legacy;

{
  Minimal compatibility with the MCP revisions that predate 2026-07-28 -
  2025-06-18 and 2025-11-25 - for a deployment that still has to answer clients
  written against them.

  MCPConnect implements 2026-07-28, and the revisions disagree on how a session
  starts and on what every request has to carry:

    - the older ones open with an "initialize" request answered by the server's
      identity and capabilities, followed by a "notifications/initialized"
      acknowledgement. 2026-07-28 has no handshake at all: it replaced both with
      "server/discover", which a client may call or skip.
    - 2026-07-28 requires the request-metadata headers of the Streamable HTTP
      transport (MCP-Protocol-Version, Mcp-Method, ...) and a per-request
      "_meta" naming the protocol version and the client capabilities. A client
      that predates the revision sends none of them and is refused by the
      default Strict validation.

  Enabling this plugin puts back the handshake and relaxes both validations to
  Lenient - which is enough, and less than turning them off: Lenient still
  refuses a request whose headers *contradict* its body, and a protocol version
  a request does state and this server does not speak.

  What it does not do, which is why enabling it logs a warning: every result is
  still shaped as 2026-07-28. Paginated lists, the "_meta" each result carries,
  the input requests of MRTR and the caching hints have no equivalent in the
  older revisions, and what those revisions have and this build does not -
  the tasks and the icons of 2025-11-25, say - is not brought back by echoing
  their version in the handshake. A legacy client ignores what it does not
  know, and that is the whole of the compatibility on offer.
}

interface

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,

  JRPC.Core,
  JRPC.Classes,

  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Types.Base;

const
  MCP_PROTOCOL_VERSION_2025_06_18 = '2025-06-18';
  MCP_PROTOCOL_VERSION_2025_11_25 = '2025-11-25';

  /// <summary>
  ///   The revisions the legacy handshake answers for. A client that asks for
  ///   one of these is told it got it; anything else is answered with the
  ///   oldest, which is the one a client that asked for something unknown is
  ///   most likely to be able to speak.
  /// </summary>
  /// <remarks>
  ///   The two differ in what they add - 2025-11-25 brought tasks, the
  ///   icons and the default elicitation mode - and this plugin implements
  ///   neither of those additions. Echoing the version back is a statement
  ///   about the handshake, not a claim to the whole revision: what the server
  ///   actually serves is what it serves either way.
  /// </remarks>
  MCP_LEGACY_SUPPORTED_VERSIONS: array[0..1] of string = (
    MCP_PROTOCOL_VERSION_2025_06_18,
    MCP_PROTOCOL_VERSION_2025_11_25
  );

  /// <summary>
  ///   The revision answered to a client that asked for one this plugin does
  ///   not know.
  /// </summary>
  MCP_LEGACY_DEFAULT_VERSION = MCP_PROTOCOL_VERSION_2025_06_18;

resourcestring
  SMCPLegacyPartialSupportFmt =
    'MCP legacy compatibility (%s) is enabled: support is partial. The ' +
    'initialize handshake is served and header/_meta validation is relaxed to ' +
    'Lenient, but every result is still shaped as %s. Silence this warning ' +
    'with SetLogWarning(False).';

type
  /// <summary>
  ///   Who is speaking, in the shape 2025-06-18 gives it: the "clientInfo" of
  ///   an initialize request and the "serverInfo" of its result.
  /// </summary>
  TLegacyImplementation = class
    Name: string;
    Version: string;

    [NeonInclude(IncludeIf.NotEmpty)]
    Title: NullString;
  end;

  /// <summary>
  ///   The params of a 2025-06-18 "initialize" request.
  /// </summary>
  /// <remarks>
  ///   The client capabilities are kept as raw JSON rather than modelled: this
  ///   build has nothing to ask of a legacy client - the input requests that
  ///   would need them are a 2026-07-28 feature - so reading them into a class
  ///   would only be a way of losing what it sent.
  /// </remarks>
  TLegacyInitializeParams = class
    ProtocolVersion: string;

    [NeonInclude(IncludeIf.NotEmpty)]
    Capabilities: TJSONObject;

    [NeonInclude(IncludeIf.NotEmpty)]
    ClientInfo: TLegacyImplementation;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   The result of a 2025-06-18 "initialize" request.
  /// </summary>
  /// <remarks>
  ///   Deliberately not a TBaseResult: that one carries the "_meta" of
  ///   2026-07-28 - the server identity, the result type, the caching hints -
  ///   and none of it is defined in the revision this answers for.
  /// </remarks>
  TLegacyInitializeResult = class
    ProtocolVersion: string;
    Capabilities: TServerCapabilities;
    ServerInfo: TLegacyImplementation;

    [NeonInclude(IncludeIf.NotEmpty)]
    Instructions: NullString;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   The two methods 2026-07-28 removed, served again for a legacy client.
  /// </summary>
  /// <remarks>
  ///   A flat class - [JRPC('')] and fully qualified method names - and not a
  ///   [JRPC('notifications')] one: a class registered under a path takes over
  ///   the whole namespace, which would leave "subscriptions/acknowledged"
  ///   unanswered. A flat method is registered under its own name alone.
  ///
  ///   Registered on the server's own registry by TMCPLegacyConfig, never
  ///   globally: a server that does not enable the plugin must keep answering
  ///   "initialize" with Method Not Found.
  /// </remarks>
  [JRPC('')]
  TMCPLegacyApi = class
  private
    /// <summary>
    ///   Fills in what the server can do from what it has registered, in the
    ///   subset 2025-06-18 defines.
    /// </summary>
    /// <remarks>
    ///   The same inference as TMCPServerApi.InferCapabilities, minus
    ///   completions: the older revision declares that one differently, and a
    ///   legacy client that read it here would call an endpoint shaped for the
    ///   newer one.
    /// </remarks>
    procedure InferCapabilities(ACapabilities: TServerCapabilities);
  public
    [Context] MCPConfig: TMCPConfig;

    [JRPC('initialize')]
    function Initialize([JRPCParams] AParams: TLegacyInitializeParams): TLegacyInitializeResult;

    /// <summary>
    ///   The client's acknowledgement of the handshake. Accepted and discarded:
    ///   there is no session to open, and refusing it with Method Not Found is
    ///   the one thing that would break the client that sent it.
    /// </summary>
    [JRPC('notifications/initialized'), JRPCNotification]
    procedure Initialized;
  end;

  /// <summary>
  ///   Turns the legacy compatibility on, and says whether enabling it is worth
  ///   a line in the log.
  /// </summary>
  /// <example>
  ///   <code>
  ///   AServer.Plugin.Configure&lt;IMCPLegacyConfig&gt;
  ///     .SetEnabled(True)
  ///     .ApplyConfig;
  ///   </code>
  /// </example>
  IMCPLegacyConfig = interface(IJRPCConfiguration)
  ['{6C1E4F0B-9D2A-4B57-8E31-2A7F5C0D9E44}']

    /// <summary>
    ///   Whether the legacy compatibility is applied when this configuration
    ///   is. Default: False - the plugin costs nothing until it is asked for.
    /// </summary>
    function SetEnabled(AEnable: Boolean): IMCPLegacyConfig;

    /// <summary>
    ///   Whether enabling the plugin writes a warning saying how partial the
    ///   compatibility is. Default: True.
    /// </summary>
    function SetLogWarning(AEnable: Boolean): IMCPLegacyConfig;

    function GetEnabled: Boolean;
    function GetLogWarning: Boolean;

    property Enabled: Boolean read GetEnabled;
    property LogWarning: Boolean read GetLogWarning;
  end;

  [Implements(IMCPLegacyConfig)]
  TMCPLegacyConfig = class(TJRPCConfiguration, IMCPLegacyConfig)
  private
    FEnabled: Boolean;
    FLogWarning: Boolean;

    /// <summary>
    ///   Whether the compatibility has already been carried over to the MCP
    ///   configuration. ApplyConfig may be called more than once along a fluent
    ///   chain, and registering the same API class twice raises.
    /// </summary>
    FApplied: Boolean;

    /// <summary>
    ///   Relaxes the two validations and registers the legacy API on the MCP
    ///   configuration of this server.
    /// </summary>
    procedure ApplyToMCPConfig;
  public
    constructor Create(AApp: IJRPCApplication); override;

    function ApplyConfig: IJRPCApplication; override;

    { IMCPLegacyConfig }
    function SetEnabled(AEnable: Boolean): IMCPLegacyConfig;
    function SetLogWarning(AEnable: Boolean): IMCPLegacyConfig;
    function GetEnabled: Boolean;
    function GetLogWarning: Boolean;

    property Enabled: Boolean read GetEnabled;
    property LogWarning: Boolean read GetLogWarning;
  end;

implementation

uses
  System.StrUtils,

  Logify;

{ TLegacyInitializeParams }

constructor TLegacyInitializeParams.Create;
begin
  inherited Create;
  ClientInfo := TLegacyImplementation.Create;
end;

destructor TLegacyInitializeParams.Destroy;
begin
  Capabilities.Free;
  ClientInfo.Free;
  inherited;
end;

{ TLegacyInitializeResult }

constructor TLegacyInitializeResult.Create;
begin
  inherited Create;
  Capabilities := TServerCapabilities.Create;
  ServerInfo := TLegacyImplementation.Create;
end;

destructor TLegacyInitializeResult.Destroy;
begin
  ServerInfo.Free;
  Capabilities.Free;
  inherited;
end;

{ TMCPLegacyApi }

procedure TMCPLegacyApi.InferCapabilities(ACapabilities: TServerCapabilities);
begin
  // Presence is the declaration, and the listChanged flag beside it is a second
  // statement - that the server will say when they change - which this build
  // cannot make. False rather than absent, since an empty capability object is
  // dropped on the way out and would take the declaration with it.
  if MCPConfig.Tools.Registry.Count > 0 then
    ACapabilities.Tools.ListChanged := False;

  if (MCPConfig.Resources.Registry.Count > 0) or
     (MCPConfig.Resources.TemplateRegistry.Count > 0) then
  begin
    ACapabilities.Resources.ListChanged := False;
    ACapabilities.Resources.Subscribe := False;
  end;

  if MCPConfig.Prompts.Registry.Count > 0 then
    ACapabilities.Prompts.ListChanged := False;
end;

function TMCPLegacyApi.Initialize(AParams: TLegacyInitializeParams): TLegacyInitializeResult;
begin
  Result := TLegacyInitializeResult.Create;
  try
    // The revision the client asked for when the handshake answers for it, and
    // the oldest one otherwise: answering with a version this plugin knows
    // nothing about would be worse than answering with one it does.
    if MatchStr(AParams.ProtocolVersion, MCP_LEGACY_SUPPORTED_VERSIONS) then
      Result.ProtocolVersion := AParams.ProtocolVersion
    else
      Result.ProtocolVersion := MCP_LEGACY_DEFAULT_VERSION;

    Result.ServerInfo.Name := MCPConfig.Server.Name;
    Result.ServerInfo.Version := MCPConfig.Server.Version;

    if not MCPConfig.Server.Instructions.IsEmpty then
      Result.Instructions := MCPConfig.Server.Instructions;

    // What the server said it can do, or - having said nothing - what it turns
    // out to have. A copy either way: the configured capabilities belong to the
    // configuration, and this result frees its own.
    if Assigned(MCPConfig.Server.Capabilities) then
      Result.Capabilities.Assign(MCPConfig.Server.Capabilities)
    else
      InferCapabilities(Result.Capabilities);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPLegacyApi.Initialized;
begin
  // Nothing to do: see the declaration
end;

{ TMCPLegacyConfig }

constructor TMCPLegacyConfig.Create(AApp: IJRPCApplication);
begin
  inherited;
  FEnabled := False;
  FLogWarning := True;
  FApplied := False;
end;

procedure TMCPLegacyConfig.ApplyToMCPConfig;
var
  LConfig: IMCPConfig;
begin
  // FApplication, and not BackToApp: BackToApp is ApplyConfig, and this runs
  // from inside it. Configure<IMCPConfig> creates the MCP configuration if this
  // server has none yet.
  LConfig := FApplication.Plugin.Configure<IMCPConfig>;

  // Lenient and not Off: what is a property of the request rather than of the
  // revision - a header that contradicts the body, a protocol version the
  // request states and this server does not speak - is still refused.
  LConfig.Security
    .SetHeaderValidation(TMCPValidationLevel.Lenient)
    .SetMetaValidation(TMCPValidationLevel.Lenient);

  // On this server's own registry, which TMCPConfig.GetConstructorProxy reads
  // before the global one
  LConfig.MessageHandling.RegisterApi(TMCPLegacyApi);
end;

function TMCPLegacyConfig.ApplyConfig: IJRPCApplication;
begin
  if Enabled and not FApplied then
  begin
    if LogWarning then
      Logger.LogWarning(SMCPLegacyPartialSupportFmt,
        [string.Join('/', MCP_LEGACY_SUPPORTED_VERSIONS),
         MCP_PROTOCOL_VERSION_2026_07_28]);

    ApplyToMCPConfig();
    FApplied := True;
  end;

  Result := inherited;
end;

function TMCPLegacyConfig.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMCPLegacyConfig.GetLogWarning: Boolean;
begin
  Result := FLogWarning;
end;

function TMCPLegacyConfig.SetEnabled(AEnable: Boolean): IMCPLegacyConfig;
begin
  FEnabled := AEnable;
  Result := Self;
end;

function TMCPLegacyConfig.SetLogWarning(AEnable: Boolean): IMCPLegacyConfig;
begin
  FLogWarning := AEnable;
  Result := Self;
end;

initialization
  TJRPCConfigClassRegistry.Instance.RegisterConfigClass(TMCPLegacyConfig);

end.
