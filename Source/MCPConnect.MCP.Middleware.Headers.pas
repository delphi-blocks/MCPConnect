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
unit MCPConnect.MCP.Middleware.Headers;

{
  The request-metadata headers of the Streamable HTTP transport (MCP
  2026-07-28, "Basic / Transports / Streamable HTTP" section "Request
  Metadata").

  The transport mirrors selected fields of the JSON-RPC body into HTTP headers
  so that load balancers, gateways and observability tooling can route and
  inspect a request without parsing its body:

    MCP-Protocol-Version: 2026-07-28
    Mcp-Method:           tools/call
    Mcp-Name:             get_weather
    Mcp-Param-Region:     us-west1

  The point of the exercise is that the two sources of truth must agree. A
  proxy that routes on the header while the server executes on the body is a
  vulnerability, not an inconsistency, which is why the spec makes the check a
  MUST: a header that contradicts the body - or, in Strict mode, a required
  header that is not there at all - is answered with HTTP 400 and the JSON-RPC
  error HeaderMismatch (-32020).

  Written as a transport middleware because what it compares lives on both
  sides of the parse: the headers of the transport request and the body it
  carries. It parses the body itself and leaves the tree on the request, where
  TMCPTransportHandler.HandlePOST picks it up rather than parsing it a second
  time.

  It is registered by IMCPConfig itself rather than by a Set... call, because
  unlike CORS or the static token this is not a feature a server opts into: it
  is what the revision requires of every HTTP server. Security.SetHeaderValidation
  chooses how much of it is enforced, TMCPValidationLevel.Off included, and
  Off is the one value that keeps the middleware out of the chain entirely.
}

interface

{$I MCPConnect.inc}
{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.JSON, System.Generics.Collections,

  MCPConnect.MCP.Types.Base,
  MCPConnect.JRPC.Middleware;

const
  /// <summary>Mirrors the "io.modelcontextprotocol/protocolVersion" request _meta key.</summary>
  MCP_HEADER_PROTOCOL_VERSION = 'MCP-Protocol-Version';

  /// <summary>Mirrors the JSON-RPC "method" member. Required on every request.</summary>
  MCP_HEADER_METHOD = 'Mcp-Method';

  /// <summary>
  ///   Mirrors "params.name" (tools/call, prompts/get) or "params.uri"
  ///   (resources/read).
  /// </summary>
  MCP_HEADER_NAME = 'Mcp-Name';

  /// <summary>
  ///   Prefix of a header mirroring a tool argument, as designated by the
  ///   "x-mcp-header" annotation in the tool's inputSchema: an annotation of
  ///   "Region" is carried as "Mcp-Param-Region".
  /// </summary>
  MCP_HEADER_PARAM_PREFIX = 'Mcp-Param-';

  /// <summary>
  ///   Sentinel wrapping a Base64-encoded header value, for values that cannot
  ///   travel as plain ASCII. Case-sensitive, and spelled exactly like this.
  /// </summary>
  MCP_HEADER_ENCODED_PREFIX = '=?base64?';
  MCP_HEADER_ENCODED_SUFFIX = '?=';

  /// <summary>
  ///   The JSON Schema annotation naming the header a tool parameter is
  ///   mirrored into.
  /// </summary>
  MCP_SCHEMA_HEADER_KEYWORD = 'x-mcp-header';

  /// <summary>The methods that carry an Mcp-Name, and where its value comes from.</summary>
  MCP_METHOD_TOOLS_CALL = 'tools/call';
  MCP_METHOD_RESOURCES_READ = 'resources/read';
  MCP_METHOD_PROMPTS_GET = 'prompts/get';

resourcestring
  SMCPHeaderMalformedEncodingFmt = 'Header [%s] carries a malformed Base64 sentinel value';
  SMCPHeaderDuplicateAnnotationFmt =
    'Tool [%s] declares the x-mcp-header value [%s] more than once: the duplicate is ignored';

type
  /// <summary>
  ///   Raised by TMCPHeaderValue.Decode when a value announces the Base64
  ///   sentinel and then does not carry valid Base64. Never reaches a client:
  ///   the middleware turns it into a HeaderMismatch like any other malformed
  ///   header.
  /// </summary>
  EMCPHeaderEncodingError = class(EMCPException);

  /// <summary>
  ///   The value-encoding rules of the transport: which values may travel as
  ///   they are, and how the rest are wrapped in the "=?base64?...?=" sentinel.
  /// </summary>
  /// <remarks>
  ///   Encoding is the client's job, so only Decode is on the server's path.
  ///   Encode is here because the rule is one rule: a server that has to decide
  ///   whether a value would have been encoded needs the same predicate the
  ///   client used, and a test that cannot build an encoded value cannot check
  ///   the decoder against anything.
  /// </remarks>
  TMCPHeaderValue = record
  public
    /// <summary>Whether AValue is wrapped in the Base64 sentinel.</summary>
    class function IsEncoded(const AValue: string): Boolean; static;

    /// <summary>
    ///   Whether AValue has to be encoded before it can be a header value:
    ///   anything outside visible ASCII, anything padded with whitespace, and
    ///   any plain value that would otherwise be mistaken for the sentinel.
    /// </summary>
    class function NeedsEncoding(const AValue: string): Boolean; static;

    /// <summary>
    ///   Whether AValue is admissible as it stands in an HTTP field value:
    ///   visible ASCII (0x21-0x7E), space and horizontal tab, and no leading or
    ///   trailing whitespace.
    /// </summary>
    class function IsSafe(const AValue: string): Boolean; static;

    /// <summary>The value as a client must put it on the wire.</summary>
    class function Encode(const AValue: string): string; static;

    /// <summary>
    ///   The value as the body must carry it: the sentinel unwrapped and
    ///   decoded from UTF-8, or AValue unchanged when it carries no sentinel.
    /// </summary>
    class function Decode(const AValue: string): string; static;
  end;

  /// <summary>
  ///   Checks the Streamable HTTP request-metadata headers of MCP 2026-07-28
  ///   against the body they mirror, and refuses the request with 400 and
  ///   HeaderMismatch (-32020) when the two disagree.
  /// </summary>
  /// <remarks>
  ///   How much is enforced comes from IMCPConfig.Security.SetHeaderValidation:
  ///   Strict is the revision as written - a missing required header is a
  ///   refusal - while Lenient checks only the headers a request actually
  ///   carries, which is what a deployment still talking to clients written
  ///   against an earlier revision wants.
  ///
  ///   Three shapes are deliberately let through untouched:
  ///
  ///   - anything that is not a single JSON-RPC Request object. A batch is
  ///     refused outright by the transport (the body MUST be one request or one
  ///     notification), the header rules of a notification POST are explicitly
  ///     undefined by this revision, and a body that is not JSON at all is a
  ///     parse error for the handler to answer as one. Answering any of those
  ///     with -32020 would describe them wrongly - and this middleware runs
  ///     before the transport refuses the batch, so it has to say so itself.
  ///   - STDIO, which has no headers to mirror.
  ///   - anything that is not a POST: a CORS preflight, and the OAuth metadata
  ///     endpoints, carry no JSON-RPC body.
  /// </remarks>
  TMCPRequestHeadersMiddleware = class(TMiddleware, ITransportMiddleware)
  private
    /// <summary>
    ///   The value of AName in AObject when it is a string, '' in every other
    ///   case - absent, null, or of another type.
    /// </summary>
    /// <remarks>
    ///   Deliberately not TJSONObject.GetValue&lt;string&gt;: that one reads
    ///   its argument as a *path*, so a key such as
    ///   "io.modelcontextprotocol/protocolVersion" is taken apart at the dots
    ///   and never found. The non-generic GetValue matches the pair by name.
    /// </remarks>
    class function JsonStr(AObject: TJSONObject; const AName: string): string; static;

    /// <summary>The value of AName in AObject when it is an object, nil otherwise.</summary>
    class function JsonObj(AObject: TJSONObject; const AName: string): TJSONObject; static;

    /// <summary>
    ///   Collects the "x-mcp-header" annotations of a tool's inputSchema:
    ///   annotation value -> the chain of property names leading to it.
    /// </summary>
    /// <remarks>
    ///   Only properties statically reachable from the root through a chain of
    ///   "properties" keys are collected, which is exactly what the spec allows
    ///   an annotation to sit on: never through "items", a composition or
    ///   conditional keyword, or a "$ref". Walking only "properties" is
    ///   therefore both the lookup and the reachability rule.
    /// </remarks>
    class procedure CollectHeaderParams(ASchema: TJSONObject; const APath: TArray<string>;
      AMap: TDictionary<string, TArray<string>>; const AToolName: string); static;

    /// <summary>
    ///   The value at APath in AArguments, or nil when any step of the chain is
    ///   missing or is not an object.
    /// </summary>
    class function ValueAtPath(AArguments: TJSONObject;
      const APath: TArray<string>): TJSONValue; static;

    /// <summary>
    ///   Whether a decoded header value says the same thing as the JSON value
    ///   it mirrors. Numbers are compared numerically, so that "42" and 42.0
    ///   agree, and booleans by name.
    /// </summary>
    class function SameScalar(AValue: TJSONValue; const AHeader: string): Boolean; static;
  public
    /// <summary>
    ///   Just inside authentication: a request that is refused for its
    ///   credentials must not be described as a header mismatch, and the check
    ///   costs a JSON parse that an unauthenticated caller should not be able
    ///   to make the server pay for.
    /// </summary>
    class function DefaultPriority: Integer; override;

    procedure Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
  end;

implementation

uses
  System.NetEncoding, System.Math,

  Logify,

  JRPC.Core,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Tool,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.Configuration.MCP,
  MCPConnect.Transport.MediaType,
  MCPConnect.Transport.Base;

resourcestring
  SErrorRetrievingMCPConfig = 'Error retrieving MCP configuration';

{ TMCPHeaderValue }

class function TMCPHeaderValue.IsEncoded(const AValue: string): Boolean;
begin
  // Case-sensitive, per the spec: the markers "MUST appear exactly as shown".
  Result := AValue.StartsWith(MCP_HEADER_ENCODED_PREFIX, False) and
            AValue.EndsWith(MCP_HEADER_ENCODED_SUFFIX, False) and
            (Length(AValue) >= Length(MCP_HEADER_ENCODED_PREFIX) + Length(MCP_HEADER_ENCODED_SUFFIX));
end;

class function TMCPHeaderValue.IsSafe(const AValue: string): Boolean;
var
  LChar: Char;

  function IsBlank(AChar: Char): Boolean;
  begin
    Result := (AChar = ' ') or (AChar = #9);
  end;

begin
  if AValue.IsEmpty then
    Exit(True);

  // No leading or trailing whitespace: a header value carrying any is
  // indistinguishable from one an intermediary trimmed, which is why the spec
  // has the client encode it instead.
  if IsBlank(AValue.Chars[0]) or IsBlank(AValue.Chars[AValue.Length - 1]) then
    Exit(False);

  // Visible ASCII, space and horizontal tab (RFC 9110 field values)
  for LChar in AValue do
    if (LChar <> #9) and ((LChar < ' ') or (LChar > #$7E)) then
      Exit(False);

  Result := True;
end;

class function TMCPHeaderValue.NeedsEncoding(const AValue: string): Boolean;
begin
  // A plain value that looks like the sentinel is encoded too, or the server
  // would decode something the client never encoded.
  Result := not IsSafe(AValue) or IsEncoded(AValue);
end;

class function TMCPHeaderValue.Encode(const AValue: string): string;
begin
  if not NeedsEncoding(AValue) then
    Exit(AValue);

  Result := MCP_HEADER_ENCODED_PREFIX +
    TNetEncoding.Base64String.EncodeBytesToString(TEncoding.UTF8.GetBytes(AValue)) +
    MCP_HEADER_ENCODED_SUFFIX;
end;

class function TMCPHeaderValue.Decode(const AValue: string): string;
var
  LInner: string;
begin
  if not IsEncoded(AValue) then
    Exit(AValue);

  LInner := AValue.Substring(
    Length(MCP_HEADER_ENCODED_PREFIX),
    Length(AValue) - Length(MCP_HEADER_ENCODED_PREFIX) - Length(MCP_HEADER_ENCODED_SUFFIX));

  try
    Result := TEncoding.UTF8.GetString(TNetEncoding.Base64String.DecodeStringToBytes(LInner));
  except
    on E: Exception do
      raise EMCPHeaderEncodingError.Create(E.Message);
  end;
end;

{ TMCPRequestHeadersMiddleware }

class function TMCPRequestHeadersMiddleware.DefaultPriority: Integer;
begin
  Result := MW_PRIORITY_AUTHENTICATION + 200;
end;

class function TMCPRequestHeadersMiddleware.JsonStr(AObject: TJSONObject;
  const AName: string): string;
var
  LValue: TJSONValue;
begin
  Result := '';
  if not Assigned(AObject) then
    Exit;

  LValue := AObject.GetValue(AName);
  if LValue is TJSONString then
    Result := TJSONString(LValue).Value;
end;

class function TMCPRequestHeadersMiddleware.JsonObj(AObject: TJSONObject;
  const AName: string): TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;

  LValue := AObject.GetValue(AName);
  if LValue is TJSONObject then
    Result := TJSONObject(LValue);
end;

class procedure TMCPRequestHeadersMiddleware.CollectHeaderParams(ASchema: TJSONObject;
  const APath: TArray<string>; AMap: TDictionary<string, TArray<string>>;
  const AToolName: string);
var
  LProperties: TJSONObject;
  LPair: TJSONPair;
  LProperty: TJSONObject;
  LHeaderName: string;
  LChildPath: TArray<string>;
  LKnown: TArray<string>;
begin
  LProperties := JsonObj(ASchema, 'properties');
  if not Assigned(LProperties) then
    Exit;

  for LPair in LProperties do
  begin
    if not (LPair.JsonValue is TJSONObject) then
      Continue;

    LProperty := TJSONObject(LPair.JsonValue);
    LChildPath := APath + [LPair.JsonString.Value];

    LHeaderName := JsonStr(LProperty, MCP_SCHEMA_HEADER_KEYWORD);
    if not LHeaderName.IsEmpty then
    begin
      // Case-insensitively unique within one inputSchema. A duplicate makes the
      // whole tool definition invalid for a client; a server has no way to say
      // which of the two a header meant, so it declines to mirror either and
      // says so in the log.
      if AMap.TryGetValue(LHeaderName.ToLower, LKnown) then
        Logger.LogWarning(SMCPHeaderDuplicateAnnotationFmt, [AToolName, LHeaderName])
      else
        AMap.Add(LHeaderName.ToLower, LChildPath);
    end;

    // Nested objects are reachable as long as every step is a "properties" key
    CollectHeaderParams(LProperty, LChildPath, AMap, AToolName);
  end;
end;

class function TMCPRequestHeadersMiddleware.ValueAtPath(AArguments: TJSONObject;
  const APath: TArray<string>): TJSONValue;
var
  LCurrent: TJSONObject;
  LIndex: Integer;
begin
  Result := nil;
  LCurrent := AArguments;

  for LIndex := 0 to High(APath) do
  begin
    if not Assigned(LCurrent) then
      Exit(nil);

    if LIndex = High(APath) then
      Exit(LCurrent.GetValue(APath[LIndex]));

    LCurrent := JsonObj(LCurrent, APath[LIndex]);
  end;
end;

class function TMCPRequestHeadersMiddleware.SameScalar(AValue: TJSONValue;
  const AHeader: string): Boolean;
var
  LNumber: Double;
begin
  if AValue is TJSONString then
    Exit(TJSONString(AValue).Value = AHeader);

  if AValue is TJSONBool then
    Exit(SameText(BoolToStr(TJSONBool(AValue).AsBoolean, True), AHeader));

  if AValue is TJSONNumber then
  begin
    // Numerically, so that a client sending "42" for a body carrying 42.0 is
    // not refused over a rendering difference.
    if not TryStrToFloat(AHeader, LNumber, TFormatSettings.Invariant) then
      Exit(False);
    Exit(SameValue(LNumber, TJSONNumber(AValue).AsDouble));
  end;

  // Objects, arrays and anything else cannot be mirrored into a header at all
  Result := False;
end;

procedure TMCPRequestHeadersMiddleware.Handle(AContext: TMiddlewareContext; const AChain: TMiddlewareChain);
var
  LRequest: TMCPTransportRequest;
  LResponse: TMCPTransportResponse;
  LConfig: TMCPConfig;
  LMode: TMCPValidationLevel;
  LBody: TJSONObject;
  LParams: TJSONObject;
  LMethod: string;

  procedure FailMismatch(const AHeaderName: string);
  begin
    raise EMCPHeaderMismatchError.Create(AHeaderName);
  end;

  procedure FailMissing(const AHeaderName: string);
  begin
    raise EMCPHeaderMissingError.Create(AHeaderName);
  end;

  /// The header as the body would have to spell it: sentinel unwrapped, and
  /// refused outright when it carries what an HTTP field value may not.
  function DecodedHeader(const AHeaderName: string): string;
  begin
    Result := LRequest.GetHeader(AHeaderName);
    if Result.IsEmpty then
      Exit;

    if not TMCPHeaderValue.IsSafe(Result) then
      FailMismatch(AHeaderName);

    try
      Result := TMCPHeaderValue.Decode(Result);
    except
      on EMCPHeaderEncodingError do
      begin
        Logger.LogWarning(SMCPHeaderMalformedEncodingFmt, [AHeaderName]);
        FailMismatch(AHeaderName);
      end;
    end;
  end;

  /// One mirrored header against the body value it claims to carry. ABodyValue
  /// empty means the body says nothing on the subject, which is not this
  /// middleware's business: an absent name or protocol version is answered
  /// further in, as the invalid params it is. ARequired is about the header
  /// alone, and only Strict acts on it.
  procedure CheckMirrored(const AHeaderName, ABodyValue: string; ARequired: Boolean);
  var
    LHeader: string;
  begin
    LHeader := DecodedHeader(AHeaderName);

    if LHeader.IsEmpty then
    begin
      if ARequired and (LMode = TMCPValidationLevel.Strict) then
        FailMissing(AHeaderName);
      Exit;
    end;

    if ABodyValue.IsEmpty then
      Exit;

    if LHeader <> ABodyValue then
      FailMismatch(AHeaderName);
  end;

  /// Mcp-Name mirrors params.name, or params.uri for resources/read, and only
  /// the three methods that address something by name carry it. Required when
  /// the body names something: a request that names nothing is invalid params,
  /// not a header mismatch.
  procedure CheckName;
  var
    LBodyName: string;
  begin
    if SameText(LMethod, MCP_METHOD_TOOLS_CALL) or SameText(LMethod, MCP_METHOD_PROMPTS_GET) then
      LBodyName := JsonStr(LParams, 'name')
    else if SameText(LMethod, MCP_METHOD_RESOURCES_READ) then
      LBodyName := JsonStr(LParams, 'uri')
    else
      Exit;

    CheckMirrored(MCP_HEADER_NAME, LBodyName, not LBodyName.IsEmpty);
  end;

  /// The tool arguments a tool asked to have mirrored, through the
  /// "x-mcp-header" annotations of its own inputSchema. A Mcp-Param-* header
  /// the server does not recognize is forwarded and ignored, as RFC 9110
  /// requires of any header a recipient does not know.
  procedure CheckParamHeaders;
  var
    LTool: TMCPTool;
    LMap: TDictionary<string, TArray<string>>;
    LArguments: TJSONObject;
    LEntry: TPair<string, TArray<string>>;
    LHeaderName, LHeader: string;
    LValue: TJSONValue;
    LHasValue: Boolean;
  begin
    if not SameText(LMethod, MCP_METHOD_TOOLS_CALL) then
      Exit;

    // An unknown tool has no schema to read the annotations from, and is
    // answered as invalid params further in
    if not LConfig.Tools.Registry.TryGetValue(JsonStr(LParams, 'name'), LTool) then
      Exit;

    LMap := TDictionary<string, TArray<string>>.Create;
    try
      CollectHeaderParams(LTool.InputSchema, [], LMap, LTool.Name);
      if LMap.Count = 0 then
        Exit;

      LArguments := JsonObj(LParams, 'arguments');

      for LEntry in LMap do
      begin
        LHeaderName := MCP_HEADER_PARAM_PREFIX + LEntry.Key;
        LHeader := LRequest.GetHeader(LHeaderName);

        LValue := ValueAtPath(LArguments, LEntry.Value);
        // A null is the same as an absent argument here: the client is told to
        // omit the header for it, and the server not to expect one.
        LHasValue := Assigned(LValue) and not (LValue is TJSONNull);

        if LHeader.IsEmpty then
        begin
          // The argument is there and the header is not: a non-conforming
          // client, which Strict refuses and Lenient lets through.
          if LHasValue and (LMode = TMCPValidationLevel.Strict) then
            FailMissing(LHeaderName);
          Continue;
        end;

        if not LHasValue then
          FailMismatch(LHeaderName);

        if not SameScalar(LValue, DecodedHeader(LHeaderName)) then
          FailMismatch(LHeaderName);
      end;
    finally
      LMap.Free;
    end;
  end;

  /// The refusal: 400, and a JSON-RPC error response carrying the id of the
  /// request that was refused, so the client can correlate it.
  procedure SendHeaderMismatch(E: Exception);
  var
    LError: TJRPCError;
  begin
    Logger.LogWarning('Request refused: %s', [E.Message]);

    LResponse.Code := HTTP_CODE_BADREQUEST;
    LResponse.ContentType := TMediaType.APPLICATION_JSON;

    LError := TJRPCError.CreateFromException(E, LBody);
    try
      LResponse.Content := TNeon.ObjectToJSONString(LError, JRPCNeonConfig);
    finally
      LError.Free;
    end;
  end;

begin
  if not AContext.TryFind<TMCPTransportRequest>(LRequest) or
     not AContext.TryFind<TMCPTransportResponse>(LResponse) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // STDIO carries no headers, and only a POST carries a JSON-RPC body: a
  // preflight and the OAuth metadata endpoints have nothing to mirror.
  if (LRequest.Protocol = TTransportProtocol.Stdio) or
     not SameText(LRequest.Command, 'POST') then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LConfig := AContext.Find<TMCPConfig>;
  if not Assigned(LConfig) then
    raise EMCPException.Create(SErrorRetrievingMCPConfig);

  LMode := LConfig.Security.HeaderValidation;
  if LMode = TMCPValidationLevel.Off then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  // Parsed here and left on the request: HandlePOST takes the tree over the
  // raw content whenever it finds one, so the body is parsed once per request
  // rather than once per reader. A body that is not JSON leaves it nil, and
  // the parse error is answered where every other parse error is.
  if not Assigned(LRequest.ContentJSON) and not LRequest.Content.IsEmpty then
    LRequest.ContentJSON := TJSONObject.ParseJSONValue(LRequest.Content);

  if not (LRequest.ContentJSON is TJSONObject) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  LBody := TJSONObject(LRequest.ContentJSON);
  LMethod := JsonStr(LBody, 'method');
  LParams := JsonObj(LBody, 'params');

  // A message with no method is not a request, and one with no id is a
  // notification: this revision defines no header rules for either.
  if LMethod.IsEmpty or not Assigned(LBody.GetValue('id')) then
  begin
    AChain.Next(AContext);
    Exit;
  end;

  try
    // Both are required of every POST, whatever the body carries: they are the
    // transport's own metadata, not a mirror of something optional.
    CheckMirrored(MCP_HEADER_METHOD, LMethod, True);
    CheckMirrored(MCP_HEADER_PROTOCOL_VERSION,
      JsonStr(JsonObj(LParams, '_meta'), MCP_META_PROTOCOL_VERSION), True);
    CheckName;
    CheckParamHeaders;
  except
    on E: EMCPProtocolError do
    begin
      SendHeaderMismatch(E);
      Exit;
    end;
  end;

  AChain.Next(AContext);
end;

end.
