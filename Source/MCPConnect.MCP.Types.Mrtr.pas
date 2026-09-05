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
unit MCPConnect.MCP.Types.Mrtr;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,

  Neon.Core.Tags,
  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Serializers.RTL,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tool;

const
  /// <summary>
  ///   The requests a server may embed in an InputRequiredResult for the
  ///   client to fulfill before it retries the original request.
  /// </summary>
  MCP_INPUT_ELICITATION = 'elicitation/create';
  MCP_INPUT_SAMPLING = 'sampling/createMessage';
  MCP_INPUT_ROOTS = 'roots/list';

type

  /// <summary>
  ///   Controls tool selection behavior for sampling requests.
  /// </summary>
  TToolChoiceMode = (
    /// <summary>
    ///   Model decides whether to use tools (default)
    /// </summary>
    Auto,
    /// <summary>
    ///   Model MUST use at least one tool before completing.
    /// </summary>
    Required,
    /// <summary>
    ///   Model MUST NOT use any tools.
    /// </summary>
    None
  );

  TToolChoice = record
    Mode: TToolChoiceMode;
  end;

  /// <summary>
  /// ToolUseContent represents a request from the assistant to call a tool within a sampling message.
  /// It must have Type set to "tool_use".
  /// </summary>
  TToolUseContent = class(TMetaClass)
  public

    /// <summary>
    /// Must be "tool_use".
    /// </summary>
    [NeonProperty('type')]
    &Type: string;

    /// <summary>
    /// ID is a unique identifier for this tool use, used to match tool results to their corresponding tool uses.
    /// </summary>
    Id: string;

    /// <summary>
    /// Name is the name of the tool to call.
    /// </summary>
    Name: string;

    /// <summary>
    ///   Input contains the arguments to pass to the tool, conforming to the
    ///   tool's input schema.
    /// </summary>
    /// <remarks>
    ///   Like Arguments in TCallToolParams
    /// </remarks>
    Input: TJSONObject;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// ToolResultContent represents the result of a tool invocation within a sampling message.
  /// It must have Type set to "tool_result".
  /// </summary>
  TToolResultContent = class(TMetaClass)
  public

    /// <summary>
    /// Must be "tool_result".
    /// </summary>
    &Type: string;

    /// <summary>
    /// ToolUseID is the ID of the tool use this result corresponds to.
    /// This MUST match the ID from a previous ToolUseContent.
    /// </summary>
    ToolUseId: string;

    /// <summary>
    /// Content is the unstructured result content of the tool use.
    /// </summary>
    Content: TContentList;

    /// <summary>
    ///   An optional structured result value.
    /// </summary>
    /// <remarks>
    ///   This can be any JSON value (object, array, string, number, boolean, or
    ///   null). If the tool defined an outputSchema, this SHOULD conform to
    ///   that schema.
    /// </remarks>
    [NeonInclude(IncludeIf.NotEmpty)]
    StructuredContent: TJSONObject;

    /// <summary>
    /// Whether the tool use resulted in an error.
    /// </summary>
    IsError: NullBoolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContent(AContent: TToolContent);
  end;


  /// <summary>
  ///   Represents a root directory or file that the server can operate on.
  /// </summary>
  TRoot = class(TMetaClass)
  public

    /// <summary>
    ///   An optional name for the root. This can be used to provide a
    ///   human-readable identifier for the root, which may be useful for
    ///   display purposes or for referencing the root in other parts of the
    ///   application.
    /// </summary>
    Name: NullString;

    /// <summary>
    ///   The URI identifying the root.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     This must start with file:// for now.
    ///   </para>
    ///   <para>
    ///     This restriction may be relaxed in future versions of the protocol
    ///     toallow other URI schemes.
    ///   </para>
    /// </remarks>
    Uri: string;
  end;

  TRoots = class(TObjectList<TRoot>);

  TListRootsParams = class(TMetaClass);

  TListRootsResult = class
  public

    /// <summary>
    ///   Array of Root objects, each representing a root
    ///   directory or file that the server can operate on.
    /// </summary>
    Roots: TRoots;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TODO -opaolo -c : Hot to free the object(s) in TValue 29/08/2026 11:11:54 }
  TSamplingMessage = class(TMetaClass)
  private
    [NeonIgnore] Text: TObjectList<TTextContent>;
    [NeonIgnore] Image: TObjectList<TImageContent>;
    [NeonIgnore] Audio: TObjectList<TAudioContent>;
    [NeonIgnore] ToolUse: TObjectList<TToolUseContent>;
    [NeonIgnore] ToolResult: TObjectList<TToolResultContent>;
    //Array<anyOf [TextContent, ImageContent, AudioContent, ToolUseContent, ToolResultContent]>]
  public
    [NeonIgnore] Single: NullBoolean;

    /// <summary>
    ///   The sender or recipient of messages and data in a conversation.
    /// </summary>
    Role: TRole;

    Content: TJSONValue;
  public
    constructor Create;
    destructor Destroy; override;

    {
    procedure AddContent(AContent: TToolContent); overload;
    procedure AddContent(AContent: TToolUseContent); overload;
    procedure AddContent(AContent: TToolResultContent); overload;

    function GetClass: TClass;
    }
  end;


  /// <summary>
  ///   Hints to use for model selection.
  ///
  ///   Keys not declared here are currently left unspecified by the spec and are
  ///   up to the client to interpret.
  /// </summary>
  TModelHint = class(TMetaClass)
  public
    /// <summary>
    ///   A hint for a model name.
    ///
    ///   The client SHOULD treat this as a substring of a model name; for example:
    ///   - `claude-3-5-sonnet` should match `claude-3-5-sonnet-20241022`
    ///   - `sonnet` should match `claude-3-5-sonnet-20241022`, `claude-3-sonnet-20240229`, etc.
    ///   - `claude` should match any Claude model
    ///
    ///   The client MAY also map the string to a different provider's model name
    ///   or a different model family, as long as it fills a similar niche; for
    ///   example:
    ///   - `gemini-1.5-flash` could match `claude-3-haiku-20240307`
    /// </summary>
    Name: NullString;
  end;

  /// <summary>
  ///   The server's preferences for model selection, requested of the client
  ///   during sampling.
  ///
  ///   Because LLMs can vary along multiple dimensions, choosing the "best" model
  ///   is rarely straightforward. Different models excel in different areas � some
  ///   are faster but less capable, others are more capable but more expensive,
  ///   and so on. This interface allows servers to express their priorities across
  ///   multiple dimensions to help clients make an appropriate selection for their
  ///   use case.
  ///
  ///   These preferences are always advisory. The client MAY ignore them. It is
  ///   also up to the client to decide how to interpret these preferences and how
  ///   to balance them against other considerations.
  /// </summary>
  TModelPreferences = class(TMetaClass)
  public
    /// <summary>
    ///   Optional hints to use for model selection.
    ///
    ///   If multiple hints are specified, the client MUST evaluate them in order
    ///   (such that the first match is taken).
    ///
    ///   The client SHOULD prioritize these hints over the numeric priorities, but
    ///   MAY still use the priorities to select from ambiguous matches.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Hints: TObjectList<TModelHint>;

    /// <summary>
    ///   How much to prioritize cost when selecting a model. A value of 0 means
    ///   cost is not important, while a value of 1 means cost is the most
    ///   important factor.
    /// </summary>
    CostPriority: NullDouble;

    /// <summary>
    ///   How much to prioritize sampling speed (latency) when selecting a model.
    ///   A value of 0 means speed is not important, while a value of 1 means speed
    ///   is the most important factor.
    /// </summary>
    SpeedPriority: NullDouble;

    /// <summary>
    ///   How much to prioritize intelligence and capabilities when selecting a
    ///   model. A value of 0 means intelligence is not important, while a value
    ///   of 1 means intelligence is the most important factor.
    /// </summary>
    IntelligencePriority: NullDouble;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TCreateMessageResult = class(TSamplingMessage)
  public

    /// <summary>
    ///   The name of the model that generated the message.
    /// </summary>
    Model: string;

    /// <summary>
    ///   The reason why sampling stopped, if known.
    /// </summary>
    StopReason: NullString;
  end;

  TIncludeContext = (None, ThisServer, AllServers);

  /// <summary>
  ///   Parameters for the sampling/createMessage request.
  /// </summary>
  TCreateMessageRequestParams = class(TMetaClass)
  public
    /// <summary>
    ///   The messages to sample.
    /// </summary>
    Messages: TObjectList<TSamplingMessage>;

    /// <summary>
    ///   The server's preferences for which model to select.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    ModelPreferences: TModelPreferences;

    /// <summary>
    ///   An optional system prompt the server wants to use for sampling.
    /// </summary>
    SystemPrompt: NullString;

    /// <summary>
    ///   How much of the conversation history to include.
    /// </summary>
    IncludeContext: TIncludeContext;

    /// <summary>
    ///   The temperature to use for sampling.
    /// </summary>
    Temperature: NullCurrency;

    /// <summary>
    ///   The maximum number of tokens to sample.
    /// </summary>
    MaxTokens: Integer;

    /// <summary>
    ///   Sequences the model should stop generating at.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    StopSequences: TArray<string>;

    /// <summary>
    ///   Request-scoped metadata.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Metadata: TJSONObject;

    /// <summary>
    ///   Tools the model may use during generation.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    Tools: TArray<TMCPTool>;

    /// <summary>
    ///   Controls how the model uses tools during generation.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)]
    ToolChoice: TToolChoice;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  /// <summary>
  ///   InputRequest is a single server-initiated request embedded in an
  ///   [InputRequiredResult]. Exactly one of its fields is populated, selected
  ///   by the Method field.
  /// </summary>
  TInputRequest = class
  public
    // Method is the JSON-RPC method of this request: one of the MCP_INPUT_*
    // constants. It selects which of the typed fields below is populated.
    [NeonIgnore] Method: string;

    // Elicitation holds the params of an elicitation/create input request.
    [NeonIgnore] Elicitation: TElicitRequestParams;

    // Sampling holds the params of a sampling/createMessage input request.
    [NeonIgnore] Sampling: TCreateMessageRequestParams;

    // Roots holds the params of a roots/list input request.
    [NeonIgnore] Roots: TListRootsParams;

    // raw preserves the original JSON so that a response can be round-tripped
    // and decoded against the method of the request it answers.
    [NeonUnwrapped] Raw: TJSONValue;
  public
    destructor Destroy; override;

    /// <summary>
    ///   Populates this request as an "elicitation/create", taking ownership of
    ///   AParams and rendering the {method, params} envelope into Raw.
    /// </summary>
    procedure SetElicitation(AParams: TElicitRequestParams);

    /// <summary>
    ///   Populates this request as a "sampling/createMessage", taking ownership
    ///   of AParams.
    /// </summary>
    procedure SetSampling(AParams: TCreateMessageRequestParams);

    /// <summary>
    ///   Populates this request as a "roots/list". AParams is optional and, when
    ///   given, is owned by the request.
    /// </summary>
    procedure SetRoots(AParams: TListRootsParams = nil);
  end;

  /// <summary>
  ///   A map of server-initiated requests that the client must fulfill. Keys
  ///   are server-assigned identifiers; values are the request objects.
  /// </summary>
  /// <remarks>
  ///   The map owns the requests it is given, so the Add* helpers hand their
  ///   result over: use it to configure the request further, do not free it.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   LResult := TInputRequiredResult.Create;
  ///   LResult.InputRequests.AddElicitation('who',
  ///     TMCPElicitRequest.Form('Who are you?', LSchema));
  ///   LResult.RequestState := SignedState;
  ///   </code>
  /// </example>
  TInputRequests = class(TObjectDictionary<string, TInputRequest>)
  public
    /// <summary>Asks the client to collect input from the user.</summary>
    function AddElicitation(const AKey: string; AParams: TElicitRequestParams): TInputRequest;

    /// <summary>Asks the client to sample an LLM.</summary>
    function AddSampling(const AKey: string; AParams: TCreateMessageRequestParams): TInputRequest;

    /// <summary>Asks the client for its list of roots.</summary>
    function AddRoots(const AKey: string): TInputRequest;

    /// <summary>
    ///   The method of the request stored under AKey, or an empty string. This
    ///   is what says how the client's answer under the same key decodes.
    /// </summary>
    function MethodOf(const AKey: string): string;
  end;

  /// <summary>
  ///   InputResponse is a client's answer to a single [InputRequest]. Exactly
  ///   one of its fields is populated, matching the method of the request it
  ///   answers. <br />
  /// </summary>
  /// <remarks>
  ///   Nothing inside the JSON says which of the three shapes it is: the server
  ///   knows, because it is the one that asked. Decode with the As* method
  ///   matching the method of the request this answers.
  /// </remarks>
  TInputResponse = class

    // Elicitation answers an elicitation/create input request.
    [NeonIgnore] Elicitation: TElicitResult;

    // Sampling answers a sampling/createMessage input request.
    [NeonIgnore] Sampling: TCreateMessageResult;

    // Roots answers a roots/list input request.
    [NeonIgnore] Roots: TListRootsResult;

    // Raw preserves the original JSON so that a response can be round-tripped
    // and decoded against the method of the request it answers.
    [NeonUnwrapped] Raw: TJSONValue;
  public
    destructor Destroy; override;

    /// <summary>
    ///   Decodes Raw as the answer to an "elicitation/create". The result is
    ///   owned by this response, and decoded only once.
    /// </summary>
    function AsElicitation: TElicitResult;

    /// <summary>Decodes Raw as the answer to a "sampling/createMessage".</summary>
    function AsSampling: TCreateMessageResult;

    /// <summary>Decodes Raw as the answer to a "roots/list".</summary>
    function AsRoots: TListRootsResult;
  end;

  /// <summary>
  ///   A map of client responses to server-initiated requests. Keys correspond
  ///   to the keys in the InputRequests map; values are the client's result for
  ///   each request.
  /// </summary>
  TInputResponses = class(TObjectDictionary<string, TInputResponse>)
  public
    /// <summary>The response stored under AKey, or nil.</summary>
    function Find(const AKey: string): TInputResponse;

    /// <summary>
    ///   The elicitation answer under AKey, or nil when the key is absent.
    /// </summary>
    function ElicitationFor(const AKey: string): TElicitResult;

    /// <summary>The sampling answer under AKey, or nil.</summary>
    function SamplingFor(const AKey: string): TCreateMessageResult;

    /// <summary>The roots answer under AKey, or nil.</summary>
    function RootsFor(const AKey: string): TListRootsResult;
  end;


  TInputRequestParams = class(TRequestMetaParams)
  public
    /// <summary>
    ///   A map of client responses to server-initiated requests. Keys
    ///   correspond to the keys in the InputRequests map; values are the
    ///   client's result for each request.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] InputResponses: TInputResponses;

    /// <summary>
    ///   Integrity-protected continuation token used to support multi
    ///   round-trip requests (MRTR) in MCP.
    /// </summary>
    RequestState: NullString;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMrtrRequestParams = class(TInputRequestParams)
  public
    /// <summary>
    ///   Name for the params
    /// </summary>
    Name: string;

    /// <summary>
    ///   Arguments for the tool
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] Arguments: TJSONObject;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   The interim result a server returns when it needs more information from
  ///   the client before it can complete the request. Carries "resultType":
  ///   "input_required" and, from TBaseResult, the result "_meta".
  /// </summary>
  TInputRequiredResult = class(TBaseResult)
  public
    /// <summary>
    ///   A map of server-initiated requests that the client must fulfill. Keys
    ///   are server-assigned identifiers; values are the request objects.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] InputRequests: TInputRequests;

    /// <summary>
    ///   RequestState is an opaque, untrusted continuation token used to
    ///   securely manage state during Multi Round-Trip Requests (MRTR)
    /// </summary>
    RequestState: NullString;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TModelPreferences }

constructor TModelPreferences.Create;
begin
  inherited;
  Hints := TObjectList<TModelHint>.Create(True);
end;

destructor TModelPreferences.Destroy;
begin
  Hints.Free;
  inherited;
end;

{ TCreateMessageRequestParams }

constructor TCreateMessageRequestParams.Create;
begin
  inherited;
  Messages := TObjectList<TSamplingMessage>.Create(True);
end;

destructor TCreateMessageRequestParams.Destroy;
begin
  Messages.Free;
  inherited;
end;

{ TListRootsResult }

constructor TListRootsResult.Create;
begin
  Roots := TRoots.Create;
end;

destructor TListRootsResult.Destroy;
begin
  Roots.Free;
  inherited;
end;



{ TToolUseContent }

constructor TToolUseContent.Create;
begin
  Input := TJSONObject.Create;
  &Type := 'tool_use';
end;

destructor TToolUseContent.Destroy;
begin
  Input.Free;
  inherited;
end;

{ TToolResultContent }

procedure TToolResultContent.AddContent(AContent: TToolContent);
begin
  Content.Add(AContent);
end;

constructor TToolResultContent.Create;
begin
  Content := TContentList.Create;
  StructuredContent := TJSONObject.Create;
  &type := 'tool_result';
end;

destructor TToolResultContent.Destroy;
begin
  StructuredContent.Free;
  Content.Free;
  inherited;
end;

{ TSamplingMessage }

constructor TSamplingMessage.Create;
begin

end;

destructor TSamplingMessage.Destroy;
begin
  Content.Free;
  inherited;
end;

{ TInputRequest }

destructor TInputRequest.Destroy;
begin
  Roots.Free;
  Sampling.Free;
  Elicitation.Free;
  Raw.Free;
  inherited;
end;

/// <summary>
///   Renders the {method, params} envelope an InputRequest is on the wire. The
///   schema's InputRequest is a whole request object, not just its params.
/// </summary>
function BuildInputRequestRaw(const AMethod: string; AParams: TObject): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('method', AMethod);

    // roots/list is the one whose params are optional
    if Assigned(AParams) then
      Result.AddPair('params', TNeon.ObjectToJSON(AParams, MCPNeonConfig));
  except
    Result.Free;
    raise;
  end;
end;

procedure TInputRequest.SetElicitation(AParams: TElicitRequestParams);
begin
  Elicitation.Free;
  Elicitation := AParams;

  Method := MCP_INPUT_ELICITATION;
  Raw.Free;
  Raw := BuildInputRequestRaw(Method, AParams);
end;

procedure TInputRequest.SetSampling(AParams: TCreateMessageRequestParams);
begin
  Sampling.Free;
  Sampling := AParams;

  Method := MCP_INPUT_SAMPLING;
  Raw.Free;
  Raw := BuildInputRequestRaw(Method, AParams);
end;

procedure TInputRequest.SetRoots(AParams: TListRootsParams);
begin
  Roots.Free;
  Roots := AParams;

  Method := MCP_INPUT_ROOTS;
  Raw.Free;
  Raw := BuildInputRequestRaw(Method, AParams);
end;

{ TInputRequests }

function TInputRequests.AddElicitation(const AKey: string;
  AParams: TElicitRequestParams): TInputRequest;
begin
  Result := TInputRequest.Create;
  try
    Result.SetElicitation(AParams);
    Add(AKey, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInputRequests.AddSampling(const AKey: string; AParams: TCreateMessageRequestParams): TInputRequest;
begin
  Result := TInputRequest.Create;
  try
    Result.SetSampling(AParams);
    Add(AKey, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInputRequests.AddRoots(const AKey: string): TInputRequest;
begin
  Result := TInputRequest.Create;
  try
    Result.SetRoots;
    Add(AKey, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInputRequests.MethodOf(const AKey: string): string;
var
  LRequest: TInputRequest;
begin
  if TryGetValue(AKey, LRequest) then
    Result := LRequest.Method
  else
    Result := '';
end;

{ TInputResponse }

destructor TInputResponse.Destroy;
begin
  Roots.Free;
  Sampling.Free;
  Elicitation.Free;
  Raw.Free;
  inherited;
end;

function TInputResponse.AsElicitation: TElicitResult;
begin
  if not Assigned(Elicitation) and (Raw is TJSONObject) then
  begin
    Elicitation := TElicitResult.Create;
    TNeon.JSONToObject(Elicitation, Raw as TJSONObject, MCPNeonConfig);
  end;

  Result := Elicitation;
end;

function TInputResponse.AsSampling: TCreateMessageResult;
begin
  if not Assigned(Sampling) and (Raw is TJSONObject) then
  begin
    Sampling := TCreateMessageResult.Create;
    TNeon.JSONToObject(Sampling, Raw as TJSONObject, MCPNeonConfig);
  end;

  Result := Sampling;
end;

function TInputResponse.AsRoots: TListRootsResult;
begin
  if not Assigned(Roots) and (Raw is TJSONObject) then
  begin
    Roots := TListRootsResult.Create;
    TNeon.JSONToObject(Roots, Raw as TJSONObject, MCPNeonConfig);
  end;

  Result := Roots;
end;

{ TInputResponses }

function TInputResponses.Find(const AKey: string): TInputResponse;
begin
  if not TryGetValue(AKey, Result) then
    Result := nil;
end;

function TInputResponses.ElicitationFor(const AKey: string): TElicitResult;
var
  LResponse: TInputResponse;
begin
  LResponse := Find(AKey);
  if Assigned(LResponse) then
    Result := LResponse.AsElicitation
  else
    Result := nil;
end;

function TInputResponses.SamplingFor(const AKey: string): TCreateMessageResult;
var
  LResponse: TInputResponse;
begin
  LResponse := Find(AKey);
  if Assigned(LResponse) then
    Result := LResponse.AsSampling
  else
    Result := nil;
end;

function TInputResponses.RootsFor(const AKey: string): TListRootsResult;
var
  LResponse: TInputResponse;
begin
  LResponse := Find(AKey);
  if Assigned(LResponse) then
    Result := LResponse.AsRoots
  else
    Result := nil;
end;

{ TMrtrRequestParams }

constructor TMrtrRequestParams.Create;
begin
  inherited;
  Arguments := TJSONObject.Create;
end;

destructor TMrtrRequestParams.Destroy;
begin
  Arguments.Free;
  inherited;
end;

{ TInputRequiredResult }

constructor TInputRequiredResult.Create;
begin
  inherited;
  ResultType := TResultType.InputRequired;
  InputRequests := TInputRequests.Create([doOwnsValues]);
end;

destructor TInputRequiredResult.Destroy;
begin
  InputRequests.Free;
  inherited;
end;

{ TInputRequestParams }

constructor TInputRequestParams.Create;
begin
  inherited;
  InputResponses := TInputResponses.Create([doOwnsValues]);
end;

destructor TInputRequestParams.Destroy;
begin
  InputResponses.Free;
  inherited;
end;

end.
