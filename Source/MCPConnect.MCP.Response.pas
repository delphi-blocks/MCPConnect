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

/// <summary>
///   The answer side of the three requests that may not be able to finish in
///   one round trip - tools/call, resources/read and prompts/get.
/// </summary>
/// <remarks>
///   <para>
///     Those three return TBaseResult, because a method may answer either with
///     its normal result or with a TInputRequiredResult asking the client for
///     more (MRTR). That freedom costs the method its return type: declared
///     TBaseResult, it can no longer say "I answer with a string" or "with a
///     TTaskItem", and the invoker's ordinary conversions - the generated
///     outputSchema with them - are out of reach.
///   </para>
///   <para>
///     TMCPResponse&lt;T&gt; gives them back. It is a box, not an answer: a
///     method declares it as its return type to say what it normally answers
///     with, puts either a value of T, a ready-made result or an input request
///     in it, and the invoker opens the box before anything else looks at the
///     result. None of it reaches the client.
///   </para>
///   <para>
///     TMCPInput is the other half: the fluent builder that fills the
///     TInputRequiredResult, so that asking the user a yes/no question is one
///     line rather than a schema, a params object and a map entry.
///   </para>
///   <para>
///     Building the *result* an operation actually answers with is the third
///     piece: TCallToolReply, TResourceReply and TPromptReply are the
///     operation-scoped one-liners (a tool error, a text resource, a user
///     message), so the vocabulary that only makes sense for one operation is
///     not offered by the other two.
///   </para>
/// </remarks>
unit MCPConnect.MCP.Response;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Rtti, System.TypInfo, System.JSON,

  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Elicitation,
  MCPConnect.MCP.Types.Mrtr;

const
  /// <summary>
  ///   The property a TMCPInput.Confirm elicitation asks for, and the one
  ///   TInputResponses.Confirmed reads back.
  /// </summary>
  MCP_CONFIRM_PROPERTY = 'confirm';

  /// <summary>Version marker of the unsigned requestState TMCPRequestState writes.</summary>
  MCP_REQUEST_STATE_PREFIX = 'MCPRS1.';
  /// <summary>Version marker of the signed requestState TMCPRequestState writes.</summary>
  MCP_REQUEST_STATE_SIGNED_PREFIX = 'MCPRS1S.';

resourcestring
  SMCPResponseKeyEmpty = 'An input request needs a key, which is what the client answers under';
  SMCPResponseNoRequests = 'An input_required result must carry at least one request';
  SMCPResponseNilResult = 'A response cannot box a nil result: build the result first, or answer with a value';
  SMCPResponseUninitialized = 'This TMCPInput has no state: create it with TMCPInput.New, or assign one built by New';
  /// <summary>
  ///   Raised by the invoker when a feature method left the box empty (it
  ///   returned nil, and an empty TValue matches every type).
  /// </summary>
  SMCPResponseNoPayload = 'The feature method returned no value (nil): there is nothing to send back';
  SMCPRequestStateNil = 'A request state cannot be built from a nil object';
  SMCPRequestStateInvalid = 'The request state is not one written by TMCPRequestState, or its signature does not match the secret';

type
  /// <summary>How an elicitation the server asked for came back.</summary>
  TElicitationOutcome = (Absent, Accepted, Declined, Cancelled);

  /// <summary>
  ///   Fluent builder of the TInputRequiredResult a server returns when it
  ///   needs something from the client before it can finish the request.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Start with New, add one request per thing needed, and end with Build -
  ///     which hands the result over - or pass the builder to
  ///     TMCPResponse&lt;T&gt;.Needs, which builds it for you.
  ///   </para>
  ///   <para>
  ///     The builder is a record for the fluent syntax, but its state is a
  ///     reference counted object shared by every copy: Build hands the result
  ///     over once, to whichever copy asks, and the state frees a result that
  ///     was never built. A chain abandoned after it started holding something
  ///     therefore does not leak, and two copies can never build the same
  ///     result twice.
  ///   </para>
  /// </remarks>
  /// <example>
  ///   <code>
  ///   Exit(TMCPResponse&lt;string&gt;.Needs(
  ///     TMCPInput.New(LState).Confirm('delete', 'Delete task #12?')));
  ///   </code>
  /// </example>
  TMCPInput = record
  private
    FState: IInterface;
    function EnsureState: IInterface;
    function Target: TInputRequiredResult;
    procedure CheckKey(const AKey: string);
    function GetCount: Integer;
  public
    /// <summary>
    ///   A new builder, optionally carrying the continuation token the client
    ///   sends back with its answers.
    /// </summary>
    /// <remarks>
    ///   RequestState is not integrity-protected by the library: sign it
    ///   yourself if what it says can influence authorization.
    /// </remarks>
    class function New(const ARequestState: string = ''): TMCPInput; static;

    /// <summary>Sets (or replaces) the continuation token.</summary>
    function State(const ARequestState: string): TMCPInput;

    /// <summary>
    ///   Asks the client to render a form for ASchema. The schema is rendered
    ///   here and, unless AOwnsSchema says otherwise, stays the caller's.
    /// </summary>
    function Elicit(const AKey, AMessage: string; ASchema: TMCPElicitationSchema;
      AOwnsSchema: Boolean = False): TMCPInput;

    /// <summary>Asks with a request the caller already built.</summary>
    /// <remarks>AParams' ownership passes to the result.</remarks>
    function ElicitParams(const AKey: string; AParams: TElicitRequestParams): TMCPInput;

    /// <summary>
    ///   Asks the client to open a URL to complete the interaction.
    /// </summary>
    function ElicitUrl(const AKey, AMessage, AUrl: string): TMCPInput;

    /// <summary>
    ///   Asks the user a yes/no question: a one-property form schema, read back
    ///   with TInputResponses.Confirmed.
    /// </summary>
    function Confirm(const AKey, AMessage: string;
      const AProperty: string = MCP_CONFIRM_PROPERTY): TMCPInput;

    /// <summary>
    ///   Asks the user for one free-text value, read back with
    ///   TInputResponses.FieldAsString.
    /// </summary>
    function AskText(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;

    /// <summary>
    ///   Asks the user for one integer, read back with
    ///   TInputResponses.FieldAsInteger.
    /// </summary>
    function AskInteger(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;

    /// <summary>
    ///   Asks the user for one number, read back with
    ///   TInputResponses.FieldAsDouble.
    /// </summary>
    function AskNumber(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;

    /// <summary>
    ///   Asks the user to pick one of AValues, read back with
    ///   TInputResponses.FieldAsString.
    /// </summary>
    function AskChoice(const AKey, AMessage, AProperty, ATitle: string;
      const AValues: TArray<string>): TMCPInput;

    /// <summary>
    ///   Asks the user to pick one or more of AValues, read back with
    ///   TInputResponses.FieldAsStrings.
    /// </summary>
    function AskMultiChoice(const AKey, AMessage, AProperty, ATitle: string;
      const AValues: TArray<string>): TMCPInput;

    /// <summary>
    ///   Asks the client to sample an LLM. The params become the request's.
    /// </summary>
    function Sample(const AKey: string; AParams: TCreateMessageRequestParams): TMCPInput;

    /// <summary>Asks the client for its list of roots.</summary>
    function Roots(const AKey: string): TMCPInput;

    /// <summary>
    ///   The result, whose ownership passes to the caller. The builder is empty
    ///   afterwards - every copy of it sees the transfer - and a build with
    ///   nothing in it raises rather than stalling the call.
    /// </summary>
    function Build: TInputRequiredResult;

    /// <summary>
    ///   As TryBuild: hands the result over, or answers False and leaves
    ///   AResult nil when there is nothing to ask. Never raises.
    /// </summary>
    function TryBuild(out AResult: TInputRequiredResult): Boolean;

    /// <summary>True when the builder holds no request yet.</summary>
    function IsEmpty: Boolean;

    /// <summary>How many requests have been added so far.</summary>
    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///   The box a tool, resource or prompt method returns when it wants to
  ///   choose, per call, between its normal answer and an input request.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     A method usually declares the generic TMCPResponse&lt;T&gt;, which
  ///     additionally says what T the normal answer has; this non-generic base
  ///     is what the invoker knows, and what a method with no single normal
  ///     shape can still use.
  ///   </para>
  ///   <para>
  ///     The box owns what it is given until the invoker opens it (Unwrap),
  ///     from which point the payload is on the invoker's ordinary path: the
  ///     same conversion, the same garbage collection, the same result as if
  ///     the method had returned it directly.
  ///   </para>
  /// </remarks>
  TMCPResponse = class(TBaseResult)
  private
    FPayload: TValue;
    FOwnsPayload: Boolean;
  protected
    /// <summary>Boxes AValue, owned until the box is opened.</summary>
    procedure Box(const AValue: TValue; AOwnsValue: Boolean);

    /// <summary>
    ///   The TValue a box holds for a result object: typed by the object's
    ///   runtime class, so Payload.TypeInfo and a serializer see the real type
    ///   rather than TObject. A nil result is refused, because boxing it would
    ///   produce an empty TValue that IsType matches against every type.
    /// </summary>
    class function Boxed(AResult: TObject): TValue; static;
  public
    destructor Destroy; override;

    /// <summary>
    ///   The type a TMCPResponse&lt;T&gt; normally answers with, which is what
    ///   an outputSchema describes. Nil here: the non-generic box says nothing
    ///   about its content, so a tool using it cannot ask for a schema.
    /// </summary>
    class function PayloadTypeInfo: PTypeInfo; virtual;

    /// <summary>
    ///   Opens the box in AValue, replacing it with what it holds, and frees
    ///   it. A value that is not a box is left alone, and a box holding another
    ///   box is opened until the payload is not one. True when at least one was
    ///   opened.
    /// </summary>
    class function Unwrap(var AValue: TValue): Boolean; static;

    /// <summary>Boxes any value: a record, an entity, a number, a string.</summary>
    class function Value(const AValue: TValue): TMCPResponse;

    /// <summary>Alias of Value, for the one-liners that read better with it.</summary>
    class function Ok(const AValue: TValue): TMCPResponse;

    /// <summary>
    ///   Boxes a plain text answer.
    /// </summary>
    /// <remarks>
    ///   Only on the non-generic box: the generic one declares a payload type,
    ///   and a string is not always it.
    /// </remarks>
    class function Text(const AText: string): TMCPResponse;

    /// <summary>
    ///   Boxes a result the method built itself - a TCallToolResult, a
    ///   TContentList, a TReadResourceResult, a TGetPromptResult. Ownership
    ///   follows the value, as it would have had it been returned directly. A
    ///   nil result is refused rather than boxed as an empty payload.
    /// </summary>
    class function Ready(AResult: TObject): TMCPResponse;

    /// <summary>Boxes the input requests built with AInput.</summary>
    class function Needs(const AInput: TMCPInput): TMCPResponse; overload;

    /// <summary>Boxes a hand-built input_required result.</summary>
    class function Needs(const AResult: TInputRequiredResult): TMCPResponse; overload;

    /// <summary>What the box holds, for a middleware that looks inside.</summary>
    property Payload: TValue read FPayload;
  end;

  /// <summary>
  ///   Metaclass of the box, which is how the tool configuration asks a
  ///   declared return type what it normally answers with.
  /// </summary>
  TMCPResponseClass = class of TMCPResponse;

  /// <summary>
  ///   A TMCPResponse that also declares what the method normally answers
  ///   with: T is what the invoker converts, and what the tool's outputSchema
  ///   describes when the tool carries the "structured" tag.
  /// </summary>
  /// <example>
  ///   <code>
  ///   [McpTool('delete_task', 'Deletes a task', 'destructive')]
  ///   function DeleteTask(AId: Integer): TMCPResponse&lt;string&gt;;
  ///   begin
  ///     if FParams.InputResponses.Outcome('delete') = TElicitationOutcome.Absent then
  ///       Exit(TMCPResponse&lt;string&gt;.Needs(
  ///         TMCPInput.New('delete:' + AId.ToString)
  ///           .Confirm('delete', 'Delete task #' + AId.ToString + '?')));
  ///
  ///     Result := TMCPResponse&lt;string&gt;.Ok('Task deleted');
  ///   end;
  ///   </code>
  /// </example>
  TMCPResponse<T> = class(TMCPResponse)
  public
    class function PayloadTypeInfo: PTypeInfo; override;

    /// <summary>Boxes the normal answer.</summary>
    class function Value(const AValue: T): TMCPResponse<T>; reintroduce;

    /// <summary>Alias of Value.</summary>
    class function Ok(const AValue: T): TMCPResponse<T>; reintroduce;

    class function Ready(AResult: TObject): TMCPResponse<T>; reintroduce;
    class function Needs(const AInput: TMCPInput): TMCPResponse<T>; reintroduce; overload;
    class function Needs(const AResult: TInputRequiredResult): TMCPResponse<T>; reintroduce; overload;
  end;

type
  /// <summary>
  ///   The construction vocabulary of a tools/call answer. Scoped to the
  ///   operation on purpose: isError and structured content mean nothing to a
  ///   resources/read or a prompts/get.
  /// </summary>
  TCallToolReply = class sealed
  public
    /// <summary>
    ///   A text-only successful answer.
    /// </summary>
    class function Text(const AText: string): TCallToolResult;

    /// <summary>
    ///   A tools/call answer that reports a failure to the model.
    /// </summary>
    /// <remarks>
    ///   Errors the *client* should see as protocol errors are exceptions, not
    ///   this.
    /// </remarks>
    class function Fail(const AText: string): TCallToolResult;

    /// <summary>
    ///   A successful answer with no content.
    /// </summary>
    class function Empty: TCallToolResult;

    /// <summary>
    ///   Wraps a content list the caller built.
    /// </summary>
    class function Content(AContent: TContentList): TCallToolResult;

    /// <summary>
    ///   A structured answer, plus an optional text copy of it. Ownership of
    ///   AStructured passes to the result.
    /// </summary>
    class function Structured(AStructured: TJSONValue;
      const AText: string = ''): TCallToolResult;
  end;

  /// <summary>
  ///   The construction vocabulary of a resources/read answer.
  /// </summary>
  TResourceReply = class sealed
  public
    class function Text(const AUri, AMimeType, AText: string): TReadResourceResult;
    class function Blob(const AUri, AMimeType: string; const AData: TBytes): TReadResourceResult;
  end;

  /// <summary>
  ///   The construction vocabulary of a prompts/get answer.
  /// </summary>
  TPromptReply = class sealed
  public
    class function Message(ARole: TRole; const AText: string): TGetPromptResult;
    class function User(const AText: string): TGetPromptResult;
    class function Assistant(const AText: string): TGetPromptResult;
  end;

  /// <summary>
  ///   The reading half of MRTR: what the client sent back under the keys the
  ///   server chose when it asked.
  /// </summary>
  /// <remarks>
  ///   Every reader is forgiving: a value of the wrong shape, a missing member
  ///   or a malformed document yields the default rather than an exception,
  ///   because the content comes from a client. Outcome is what tells an answer
  ///   apart from a decline or a cancellation, which read as absent everywhere
  ///   else.
  /// </remarks>
  TMCPInputResponsesHelper = class helper for TInputResponses
  private
    /// <summary>
    ///   The accepted content object under AKey, or nil.
    /// </summary>
    function AcceptedContent(const AKey: string): TJSONObject;
    /// <summary>
    ///   The member of the accepted content under AKey, or nil.
    /// </summary>
    function RawField(const AKey, AProperty: string): TJSONValue;
  public
    /// <summary>
    ///   How the elicitation under AKey came back.
    /// </summary>
    function Outcome(const AKey: string): TElicitationOutcome;

    /// <summary>
    ///   Whether the elicitation under AKey came back accepted.
    /// </summary>
    function Accepted(const AKey: string): Boolean;

    /// <summary>
    ///   Whether the user answered yes to the Confirm asked under AKey. False
    ///   for every other outcome: declined, cancelled, never asked.
    /// </summary>
    function Confirmed(const AKey: string;
      const AProperty: string = MCP_CONFIRM_PROPERTY): Boolean;

    function FieldAsString(const AKey, AProperty: string; const ADefault: string = ''): string;
    function FieldAsInteger(const AKey, AProperty: string; ADefault: Integer = 0): Integer;
    function FieldAsDouble(const AKey, AProperty: string; ADefault: Double = 0): Double;
    function FieldAsBoolean(const AKey, AProperty: string; ADefault: Boolean = False): Boolean;
    /// <summary>
    ///   A multi-value answer - the fields of a multi-select form, or a single
    ///   scalar read as a one-element array.
    /// </summary>
    function FieldAsStrings(const AKey, AProperty: string): TArray<string>;

    /// <summary>
    ///   True when the member exists and reads as a scalar.
    /// </summary>
    function TryFieldAsString(const AKey, AProperty: string; out AValue: string): Boolean;
    function TryFieldAsInteger(const AKey, AProperty: string; out AValue: Integer): Boolean;
    function TryFieldAsDouble(const AKey, AProperty: string; out AValue: Double): Boolean;
    function TryFieldAsBoolean(const AKey, AProperty: string; out AValue: Boolean): Boolean;

    /// <summary>
    ///   The accepted content under AKey read into a new T through Neon, or nil
    ///   when there is no accepted answer. The caller owns the instance.
    /// </summary>
    function FieldsAs<T: class, constructor>(const AKey: string): T;
  end;

  /// <summary>
  ///   Codec for the opaque requestState an MRTR round trip carries: any Delphi
  ///   object, serialized by Neon, Base64-encoded, and optionally signed so a
  ///   client cannot tamper with the context it carries.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     A requestState is what the server hands to the client and gets back on
  ///     the retry, so it is the natural place for the lifecycle data of one
  ///     request: which record was asked about, when, what the server must
  ///     remember. Turning that into a Delphi object keeps it checked at compile
  ///     time. The two markers make a state this codec wrote recognizable, so a
  ///     server can tell its own state from a foreign one before decoding it.
  ///   </para>
  ///   <para>
  ///     The unsigned form is obfuscation, not protection: Base64 is reversible
  ///     by anyone. When what the state says can influence what the server does,
  ///     encode it with a secret and decode it with the same secret: the payload
  ///     carries an HMAC-SHA256 over it, verified before decoding, so a state
  ///     that was tampered with is refused rather than trusted.
  ///   </para>
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // ask: the context travels as the requestState
  ///   LCtx := TDeleteContext.Create;
  ///   try
  ///     LCtx.TaskId := AId;
  ///     Exit(TMCPResponse&lt;string&gt;.Needs(
  ///       TMCPInput.New(TMCPRequestState.Encode(LCtx, Secret))
  ///         .Confirm('delete', 'Delete task #' + AId.ToString + '?')));
  ///   finally
  ///     LCtx.Free;
  ///   end;
  ///
  ///   // retry: read it back, and trust it only because the secret verified
  ///   LCtx := FParams.StateAs&lt;TDeleteContext&gt;(Secret);
  ///   try
  ///     ...
  ///   finally
  ///     LCtx.Free;
  ///   end;
  ///   </code>
  /// </example>
  TMCPRequestState = class sealed
  private
    class function ConfigOrDefault(AConfig: INeonConfiguration): INeonConfiguration; static;
    class function EncodeBase64(const AText: string): string; static;
    class function DecodeBase64(const AText: string): string; static;
    class function Signature(const APayload, ASecret: string): string; static;
    class function SameDigest(const ALeft, ARight: string): Boolean; static;
    class function DecodePayload<T: class, constructor>(const APayload: string;
      AConfig: INeonConfiguration): T; static;
  public
    /// <summary>
    ///   The state for AValue: MCPRS1. followed by the Base64 of its Neon JSON.
    /// </summary>
    class function Encode(AValue: TObject; AConfig: INeonConfiguration = nil): string; overload;

    /// <summary>
    ///   The signed state for AValue: MCPRS1S. followed by the Base64 of its
    ///   Neon JSON, a '.', and the Base64 of an HMAC-SHA256 over that payload.
    /// </summary>
    class function Encode(AValue: TObject; const ASecret: string;
      AConfig: INeonConfiguration = nil): string; overload;

    /// <summary>
    ///   True when AState carries one of this codec's markers.
    /// </summary>
    class function IsRequestState(const AState: string): Boolean;

    /// <summary>
    ///   Decodes an unsigned state into a new T. False, with AValue nil, when
    ///   AState was not written by Encode or does not decode.
    /// </summary>
    class function TryDecode<T: class, constructor>(const AState: string;
      out AValue: T; AConfig: INeonConfiguration = nil): Boolean; overload;

    /// <summary>
    ///   Decodes a signed state, refusing one whose signature does not match.
    /// </summary>
    class function TryDecode<T: class, constructor>(const AState, ASecret: string;
      out AValue: T; AConfig: INeonConfiguration = nil): Boolean; overload;

    /// <summary>
    ///   As the unsigned TryDecode, raising when it fails.
    /// </summary>
    class function Decode<T: class, constructor>(const AState: string;
      AConfig: INeonConfiguration = nil): T; overload;

    /// <summary>
    ///   As the signed TryDecode, raising when it fails.
    /// </summary>
    class function Decode<T: class, constructor>(const AState, ASecret: string;
      AConfig: INeonConfiguration = nil): T; overload;
  end;

  /// <summary>
  ///   Reads the requestState of an MRTR retry as the object it was made from.
  /// </summary>
  TMCPRequestStateHelper = class helper for TInputRequestParams
  public
    function StateAs<T: class, constructor>(AConfig: INeonConfiguration = nil): T; overload;
    function StateAs<T: class, constructor>(const ASecret: string;
      AConfig: INeonConfiguration = nil): T; overload;
    function TryStateAs<T: class, constructor>(out AValue: T;
      AConfig: INeonConfiguration = nil): Boolean; overload;
    function TryStateAs<T: class, constructor>(const ASecret: string;
      out AValue: T; AConfig: INeonConfiguration = nil): Boolean; overload;
  end;

implementation

uses
  System.Hash,
  System.NetEncoding;

type
  /// <summary>
  ///   The mutable half of TMCPInput, shared by every copy of the record. It
  ///   owns a result that was created but never built, so an abandoned chain
  ///   frees it.
  /// </summary>
  IMCPInputState = interface
    ['{7B2C1E64-3D5A-4C9E-9E2B-8A1F4C6D5E70}']
    function GetResult: TInputRequiredResult;
    procedure SetResult(const AValue: TInputRequiredResult);
    function GetRequestState: string;
    procedure SetRequestState(const AValue: string);
  end;

  TMCPInputState = class(TInterfacedObject, IMCPInputState)
  private
    FResult: TInputRequiredResult;
    FRequestState: string;
    function GetResult: TInputRequiredResult;
    procedure SetResult(const AValue: TInputRequiredResult);
    function GetRequestState: string;
    procedure SetRequestState(const AValue: string);
  public
    constructor Create(const ARequestState: string);
    destructor Destroy; override;
  end;

{ TMCPInputState }

constructor TMCPInputState.Create(const ARequestState: string);
begin
  inherited Create;
  FRequestState := ARequestState;
end;

destructor TMCPInputState.Destroy;
begin
  FResult.Free;
  inherited;
end;

function TMCPInputState.GetResult: TInputRequiredResult;
begin
  Result := FResult;
end;

procedure TMCPInputState.SetResult(const AValue: TInputRequiredResult);
begin
  FResult := AValue;
end;

function TMCPInputState.GetRequestState: string;
begin
  Result := FRequestState;
end;

procedure TMCPInputState.SetRequestState(const AValue: string);
begin
  FRequestState := AValue;
  if Assigned(FResult) then
    FResult.RequestState := AValue;
end;

{ TMCPInput }

class function TMCPInput.New(const ARequestState: string): TMCPInput;
begin
  Result.FState := TMCPInputState.Create(ARequestState);
end;

function TMCPInput.EnsureState: IInterface;
var
  LState: IMCPInputState;
begin
  if (FState <> nil) and Supports(FState, IMCPInputState, LState) then
    Exit(FState);

  LState := TMCPInputState.Create('');
  FState := LState;
  Result := FState;
end;

function TMCPInput.Target: TInputRequiredResult;
var
  LState: IMCPInputState;
begin
  EnsureState;
  if not Supports(FState, IMCPInputState, LState) then
    raise EMCPException.Create(SMCPResponseUninitialized);

  Result := LState.GetResult;
  if not Assigned(Result) then
  begin
    Result := TInputRequiredResult.Create;
    if LState.GetRequestState <> '' then
      Result.RequestState := LState.GetRequestState;
    LState.SetResult(Result);
  end;
end;

procedure TMCPInput.CheckKey(const AKey: string);
begin
  if AKey.Trim.IsEmpty then
    raise EMCPException.Create(SMCPResponseKeyEmpty);
end;

function TMCPInput.State(const ARequestState: string): TMCPInput;
var
  LState: IMCPInputState;
begin
  EnsureState;
  if Supports(FState, IMCPInputState, LState) then
    LState.SetRequestState(ARequestState);
  Result := Self;
end;

function TMCPInput.Elicit(const AKey, AMessage: string; ASchema: TMCPElicitationSchema;
  AOwnsSchema: Boolean): TMCPInput;
begin
  try
    CheckKey(AKey);
    Target.InputRequests.AddElicitation(AKey, TMCPElicitRequest.Form(AMessage, ASchema));
  finally
    if AOwnsSchema then
      ASchema.Free;
  end;
  Result := Self;
end;

function TMCPInput.ElicitParams(const AKey: string; AParams: TElicitRequestParams): TMCPInput;
begin
  CheckKey(AKey);
  Target.InputRequests.AddElicitation(AKey, AParams);
  Result := Self;
end;

function TMCPInput.ElicitUrl(const AKey, AMessage, AUrl: string): TMCPInput;
begin
  CheckKey(AKey);
  Target.InputRequests.AddElicitation(AKey, TMCPElicitRequest.Url(AMessage, AUrl));
  Result := Self;
end;

function TMCPInput.Confirm(const AKey, AMessage, AProperty: string): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddBoolean(AProperty, AMessage, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.AskText(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddString(AProperty, ATitle, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.AskInteger(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddInteger(AProperty, ATitle, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.AskNumber(const AKey, AMessage, AProperty, ATitle: string): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddNumber(AProperty, ATitle, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.AskChoice(const AKey, AMessage, AProperty, ATitle: string;
  const AValues: TArray<string>): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddEnum(AProperty, ATitle, AValues, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.AskMultiChoice(const AKey, AMessage, AProperty, ATitle: string;
  const AValues: TArray<string>): TMCPInput;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddMultiEnum(AProperty, ATitle, AValues, True);
    Result := Elicit(AKey, AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInput.Sample(const AKey: string; AParams: TCreateMessageRequestParams): TMCPInput;
begin
  CheckKey(AKey);
  Target.InputRequests.AddSampling(AKey, AParams);
  Result := Self;
end;

function TMCPInput.Roots(const AKey: string): TMCPInput;
begin
  CheckKey(AKey);
  Target.InputRequests.AddRoots(AKey);
  Result := Self;
end;

function TMCPInput.GetCount: Integer;
var
  LState: IMCPInputState;
begin
  if (FState <> nil) and Supports(FState, IMCPInputState, LState)
    and Assigned(LState.GetResult) then
    Result := LState.GetResult.InputRequests.Count
  else
    Result := 0;
end;

function TMCPInput.Build: TInputRequiredResult;
var
  LState: IMCPInputState;
begin
  Result := nil;

  // The state is shared, so the transfer is visible to every copy: two copies
  // can never hand the same result out twice
  if (FState <> nil) and Supports(FState, IMCPInputState, LState) then
  begin
    Result := LState.GetResult;
    LState.SetResult(nil);
  end;

  // An input_required asking for nothing is a request the client cannot
  // fulfill and a retry the server cannot recognize: it would stall the call
  if not Assigned(Result) or (Result.InputRequests.Count = 0) then
  begin
    Result.Free;
    raise EMCPException.Create(SMCPResponseNoRequests);
  end;
end;

function TMCPInput.TryBuild(out AResult: TInputRequiredResult): Boolean;
var
  LState: IMCPInputState;
begin
  AResult := nil;
  Result := (FState <> nil) and Supports(FState, IMCPInputState, LState)
    and Assigned(LState.GetResult) and (LState.GetResult.InputRequests.Count > 0);
  if Result then
    AResult := Build;
end;

function TMCPInput.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

{ TMCPResponse }

class function TMCPResponse.Boxed(AResult: TObject): TValue;
var
  LTypeInfo: PTypeInfo;
begin
  if not Assigned(AResult) then
    raise EMCPException.Create(SMCPResponseNilResult);

  LTypeInfo := PTypeInfo(AResult.ClassInfo);
  if Assigned(LTypeInfo) then
    Result := TValue.From(LTypeInfo, AResult)
  else
    Result := TValue.From<TObject>(AResult);
end;

destructor TMCPResponse.Destroy;
begin
  // Only reached when the box was never opened: an invoker that unwraps takes
  // the payload with it
  if FOwnsPayload and FPayload.IsObjectInstance then
    FPayload.AsObject.Free;

  inherited;
end;

procedure TMCPResponse.Box(const AValue: TValue; AOwnsValue: Boolean);
begin
  FPayload := AValue;
  FOwnsPayload := AOwnsValue;
end;

class function TMCPResponse.PayloadTypeInfo: PTypeInfo;
begin
  Result := nil;
end;

class function TMCPResponse.Unwrap(var AValue: TValue): Boolean;
var
  LBox: TMCPResponse;
begin
  Result := False;

  // A box may hold a box (a middleware wrapping another): open until what is
  // left is not one. An empty value is not a box, and AsObject on it is nil.
  while (not AValue.IsEmpty) and AValue.IsObject and (AValue.AsObject is TMCPResponse) do
  begin
    LBox := TMCPResponse(AValue.AsObject);
    AValue := LBox.FPayload;
    LBox.FOwnsPayload := False;
    LBox.Free;
    Result := True;
  end;
end;

class function TMCPResponse.Value(const AValue: TValue): TMCPResponse;
begin
  Result := TMCPResponse.Create;
  Result.Box(AValue, True);
end;

class function TMCPResponse.Ok(const AValue: TValue): TMCPResponse;
begin
  Result := Value(AValue);
end;

class function TMCPResponse.Text(const AText: string): TMCPResponse;
begin
  Result := TMCPResponse.Create;
  Result.Box(TValue.From<string>(AText), False);
end;

class function TMCPResponse.Ready(AResult: TObject): TMCPResponse;
var
  LPayload: TValue;
begin
  // Boxed refuses nil, and must do so before the box exists: creating it first
  // would leak it on that path
  LPayload := Boxed(AResult);
  Result := TMCPResponse.Create;
  Result.Box(LPayload, True);
end;

class function TMCPResponse.Needs(const AInput: TMCPInput): TMCPResponse;
var
  LInput: TMCPInput;
begin
  LInput := AInput;
  Result := TMCPResponse.Ready(LInput.Build);
end;

class function TMCPResponse.Needs(const AResult: TInputRequiredResult): TMCPResponse;
begin
  Result := TMCPResponse.Ready(AResult);
end;

{ TMCPResponse<T> }

class function TMCPResponse<T>.PayloadTypeInfo: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

class function TMCPResponse<T>.Value(const AValue: T): TMCPResponse<T>;
begin
  Result := TMCPResponse<T>.Create;
  Result.Box(TValue.From<T>(AValue), True);
end;

class function TMCPResponse<T>.Ok(const AValue: T): TMCPResponse<T>;
begin
  Result := Value(AValue);
end;

class function TMCPResponse<T>.Ready(AResult: TObject): TMCPResponse<T>;
var
  LPayload: TValue;
begin
  LPayload := Boxed(AResult);
  Result := TMCPResponse<T>.Create;
  Result.Box(LPayload, True);
end;

class function TMCPResponse<T>.Needs(const AInput: TMCPInput): TMCPResponse<T>;
var
  LInput: TMCPInput;
begin
  LInput := AInput;
  Result := TMCPResponse<T>.Ready(LInput.Build);
end;

class function TMCPResponse<T>.Needs(const AResult: TInputRequiredResult): TMCPResponse<T>;
begin
  Result := TMCPResponse<T>.Ready(AResult);
end;

{ TCallToolReply }

class function TCallToolReply.Text(const AText: string): TCallToolResult;
begin
  Result := TCallToolResult.Create;
  Result.Content.AddText(AText);
end;

class function TCallToolReply.Fail(const AText: string): TCallToolResult;
begin
  Result := TCallToolResult.Create;
  Result.Content.AddText(AText);
  Result.IsError := True;
end;

class function TCallToolReply.Empty: TCallToolResult;
begin
  Result := TCallToolResult.Create;
end;

class function TCallToolReply.Content(AContent: TContentList): TCallToolResult;
begin
  if not Assigned(AContent) then
    raise EMCPException.Create(SMCPResponseNilResult);
  Result := TCallToolResult.Create(AContent);
end;

class function TCallToolReply.Structured(AStructured: TJSONValue;
  const AText: string): TCallToolResult;
begin
  Result := TCallToolResult.Create;
  if not AText.IsEmpty then
    Result.Content.AddText(AText);
  Result.StructuredContent := AStructured;
end;

{ TResourceReply }

class function TResourceReply.Text(const AUri, AMimeType, AText: string): TReadResourceResult;
begin
  Result := TReadResourceResult.Create;
  Result.AddTextContent(AUri, AMimeType, AText);
end;

class function TResourceReply.Blob(const AUri, AMimeType: string;
  const AData: TBytes): TReadResourceResult;
begin
  Result := TReadResourceResult.Create;
  Result.AddBase64Content(AUri, AMimeType,
    TNetEncoding.Base64.EncodeBytesToString(AData));
end;

{ TPromptReply }

class function TPromptReply.Message(ARole: TRole; const AText: string): TGetPromptResult;
begin
  Result := TGetPromptResult.Create;
  Result.Messages.AddText(ARole, AText);
end;

class function TPromptReply.User(const AText: string): TGetPromptResult;
begin
  Result := Message(TRole.User, AText);
end;

class function TPromptReply.Assistant(const AText: string): TGetPromptResult;
begin
  Result := Message(TRole.Assistant, AText);
end;

{ TMCPInputResponsesHelper }

function TMCPInputResponsesHelper.AcceptedContent(const AKey: string): TJSONObject;
var
  LResult: TElicitResult;
begin
  Result := nil;
  LResult := ElicitationFor(AKey);
  if not Assigned(LResult) or (LResult.Action <> TElicitAction.Accept) then
    Exit;

  if LResult.Content is TJSONObject then
    Result := TJSONObject(LResult.Content);
end;

function TMCPInputResponsesHelper.RawField(const AKey, AProperty: string): TJSONValue;
var
  LContent: TJSONObject;
begin
  Result := nil;
  LContent := AcceptedContent(AKey);
  if Assigned(LContent) then
    Result := LContent.GetValue(AProperty);
end;

function TMCPInputResponsesHelper.Outcome(const AKey: string): TElicitationOutcome;
var
  LResult: TElicitResult;
begin
  LResult := ElicitationFor(AKey);
  if not Assigned(LResult) then
    Exit(TElicitationOutcome.Absent);

  case LResult.Action of
    TElicitAction.Accept: Result := TElicitationOutcome.Accepted;
    TElicitAction.Decline: Result := TElicitationOutcome.Declined;
    TElicitAction.Cancel: Result := TElicitationOutcome.Cancelled;
  else
    Result := TElicitationOutcome.Absent;
  end;
end;

function TMCPInputResponsesHelper.Accepted(const AKey: string): Boolean;
begin
  Result := Outcome(AKey) = TElicitationOutcome.Accepted;
end;

function TMCPInputResponsesHelper.Confirmed(const AKey, AProperty: string): Boolean;
begin
  Result := FieldAsBoolean(AKey, AProperty);
end;

/// <summary>One JSON scalar as text; a container keeps its JSON.</summary>
function JsonScalarToString(AValue: TJSONValue): string;
begin
  if not Assigned(AValue) or (AValue is TJSONNull) then
    Exit('');
  if AValue is TJSONString then
    Result := TJSONString(AValue).Value
  else if AValue is TJSONBool then
    Result := BoolToStr(TJSONBool(AValue).AsBoolean, True)
  else if AValue is TJSONNumber then
    Result := TJSONNumber(AValue).ToString
  else
    Result := AValue.ToJSON;
end;

function TMCPInputResponsesHelper.FieldAsString(const AKey, AProperty: string;
  const ADefault: string): string;
begin
  if not TryFieldAsString(AKey, AProperty, Result) then
    Result := ADefault;
end;

function TMCPInputResponsesHelper.FieldAsInteger(const AKey, AProperty: string;
  ADefault: Integer): Integer;
begin
  if not TryFieldAsInteger(AKey, AProperty, Result) then
    Result := ADefault;
end;

function TMCPInputResponsesHelper.FieldAsDouble(const AKey, AProperty: string;
  ADefault: Double): Double;
begin
  if not TryFieldAsDouble(AKey, AProperty, Result) then
    Result := ADefault;
end;

function TMCPInputResponsesHelper.FieldAsBoolean(const AKey, AProperty: string;
  ADefault: Boolean): Boolean;
begin
  if not TryFieldAsBoolean(AKey, AProperty, Result) then
    Result := ADefault;
end;

function TMCPInputResponsesHelper.FieldAsStrings(const AKey, AProperty: string): TArray<string>;
var
  LValue: TJSONValue;
  LArray: TJSONArray;
  LItem: TJSONValue;
  LIndex: Integer;
begin
  Result := nil;
  LValue := RawField(AKey, AProperty);
  if not Assigned(LValue) or (LValue is TJSONNull) then
    Exit;

  if LValue is TJSONArray then
  begin
    LArray := TJSONArray(LValue);
    SetLength(Result, LArray.Count);
    LIndex := 0;
    for LItem in LArray do
    begin
      Result[LIndex] := JsonScalarToString(LItem);
      Inc(LIndex);
    end;
  end
  else
    Result := [JsonScalarToString(LValue)];
end;

function TMCPInputResponsesHelper.TryFieldAsString(const AKey, AProperty: string;
  out AValue: string): Boolean;
var
  LValue: TJSONValue;
begin
  AValue := '';
  LValue := RawField(AKey, AProperty);
  Result := Assigned(LValue) and not (LValue is TJSONNull)
    and not (LValue is TJSONObject) and not (LValue is TJSONArray);
  if Result then
    AValue := JsonScalarToString(LValue);
end;

function TMCPInputResponsesHelper.TryFieldAsInteger(const AKey, AProperty: string;
  out AValue: Integer): Boolean;
var
  LValue: TJSONValue;
begin
  AValue := 0;
  LValue := RawField(AKey, AProperty);
  if not Assigned(LValue) then
    Exit(False);

  if LValue is TJSONNumber then
  begin
    AValue := Round(TJSONNumber(LValue).AsDouble);
    Result := True;
  end
  else if LValue is TJSONBool then
  begin
    if TJSONBool(LValue).AsBoolean then
      AValue := 1
    else
      AValue := 0;
    Result := True;
  end
  else if LValue is TJSONString then
    Result := TryStrToInt(TJSONString(LValue).Value, AValue)
  else
    Result := False;
end;

function TMCPInputResponsesHelper.TryFieldAsDouble(const AKey, AProperty: string;
  out AValue: Double): Boolean;
var
  LValue: TJSONValue;
begin
  AValue := 0;
  LValue := RawField(AKey, AProperty);
  if not Assigned(LValue) then
    Exit(False);

  if LValue is TJSONNumber then
  begin
    AValue := TJSONNumber(LValue).AsDouble;
    Result := True;
  end
  else if LValue is TJSONBool then
  begin
    if TJSONBool(LValue).AsBoolean then
      AValue := 1
    else
      AValue := 0;
    Result := True;
  end
  else if LValue is TJSONString then
    Result := TryStrToFloat(TJSONString(LValue).Value, AValue)
  else
    Result := False;
end;

function TMCPInputResponsesHelper.TryFieldAsBoolean(const AKey, AProperty: string;
  out AValue: Boolean): Boolean;
var
  LValue: TJSONValue;
  LText: string;
begin
  AValue := False;
  LValue := RawField(AKey, AProperty);
  if not Assigned(LValue) then
    Exit(False);

  if LValue is TJSONBool then
  begin
    AValue := TJSONBool(LValue).AsBoolean;
    Result := True;
  end
  else if LValue is TJSONNumber then
  begin
    AValue := TJSONNumber(LValue).AsDouble <> 0;
    Result := True;
  end
  else if LValue is TJSONString then
  begin
    LText := TJSONString(LValue).Value;
    Result := True;
    if SameText(LText, 'true') or (LText = '1') then
      AValue := True
    else if SameText(LText, 'false') or (LText = '0') then
      AValue := False
    else
      Result := False;
  end
  else
    Result := False;
end;

function TMCPInputResponsesHelper.FieldsAs<T>(const AKey: string): T;
var
  LContent: TJSONObject;
begin
  Result := nil;
  LContent := AcceptedContent(AKey);
  if not Assigned(LContent) then
    Exit;

  Result := T.Create;
  try
    TNeon.JSONToObject(Result, LContent, MCPNeonConfig);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPRequestState }

class function TMCPRequestState.ConfigOrDefault(AConfig: INeonConfiguration): INeonConfiguration;
begin
  if Assigned(AConfig) then
    Result := AConfig
  else
    Result := MCPNeonConfig;
end;

class function TMCPRequestState.EncodeBase64(const AText: string): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(AText));
end;

class function TMCPRequestState.DecodeBase64(const AText: string): string;
begin
  Result := TEncoding.UTF8.GetString(TNetEncoding.Base64.DecodeStringToBytes(AText));
end;

class function TMCPRequestState.Signature(const APayload, ASecret: string): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(
    THashSHA2.GetHMACAsBytes(APayload, ASecret));
end;

class function TMCPRequestState.SameDigest(const ALeft, ARight: string): Boolean;
var
  LIndex, LDifference: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);

  LDifference := 0;
  for LIndex := 1 to Length(ALeft) do
    LDifference := LDifference or (Ord(ALeft[LIndex]) xor Ord(ARight[LIndex]));
  Result := LDifference = 0;
end;

class function TMCPRequestState.DecodePayload<T>(const APayload: string;
  AConfig: INeonConfiguration): T;
begin
  Result := TNeon.JSONToObject<T>(DecodeBase64(APayload), ConfigOrDefault(AConfig));
end;

class function TMCPRequestState.Encode(AValue: TObject; AConfig: INeonConfiguration): string;
begin
  if not Assigned(AValue) then
    raise EMCPException.Create(SMCPRequestStateNil);

  Result := MCP_REQUEST_STATE_PREFIX +
    EncodeBase64(TNeon.ObjectToJSONString(AValue, ConfigOrDefault(AConfig)));
end;

class function TMCPRequestState.Encode(AValue: TObject; const ASecret: string;
  AConfig: INeonConfiguration): string;
var
  LPayload: string;
begin
  if not Assigned(AValue) then
    raise EMCPException.Create(SMCPRequestStateNil);

  LPayload := EncodeBase64(TNeon.ObjectToJSONString(AValue, ConfigOrDefault(AConfig)));
  Result := MCP_REQUEST_STATE_SIGNED_PREFIX + LPayload + '.' +
    Signature(LPayload, ASecret);
end;

class function TMCPRequestState.IsRequestState(const AState: string): Boolean;
begin
  Result := AState.StartsWith(MCP_REQUEST_STATE_PREFIX) or
    AState.StartsWith(MCP_REQUEST_STATE_SIGNED_PREFIX);
end;

class function TMCPRequestState.TryDecode<T>(const AState: string; out AValue: T;
  AConfig: INeonConfiguration): Boolean;
var
  LPayload: string;
begin
  AValue := nil;
  if not AState.StartsWith(MCP_REQUEST_STATE_PREFIX) then
    Exit(False);

  LPayload := Copy(AState, Length(MCP_REQUEST_STATE_PREFIX) + 1, MaxInt);
  try
    AValue := DecodePayload<T>(LPayload, AConfig);
    Result := True;
  except
    AValue := nil;
    Result := False;
  end;
end;

class function TMCPRequestState.TryDecode<T>(const AState, ASecret: string;
  out AValue: T; AConfig: INeonConfiguration): Boolean;
var
  LBody, LPayload, LGiven, LExpected: string;
  LSplit: Integer;
begin
  AValue := nil;
  if not AState.StartsWith(MCP_REQUEST_STATE_SIGNED_PREFIX) then
    Exit(False);

  LBody := Copy(AState, Length(MCP_REQUEST_STATE_SIGNED_PREFIX) + 1, MaxInt);
  LSplit := LBody.LastIndexOf('.');
  if LSplit < 1 then
    Exit(False);

  LPayload := Copy(LBody, 1, LSplit);
  LGiven := Copy(LBody, LSplit + 2, MaxInt);
  LExpected := Signature(LPayload, ASecret);
  if not SameDigest(LGiven, LExpected) then
    Exit(False);

  try
    AValue := DecodePayload<T>(LPayload, AConfig);
    Result := True;
  except
    AValue := nil;
    Result := False;
  end;
end;

class function TMCPRequestState.Decode<T>(const AState: string;
  AConfig: INeonConfiguration): T;
begin
  if not TryDecode<T>(AState, Result, AConfig) then
    raise EMCPException.Create(SMCPRequestStateInvalid);
end;

class function TMCPRequestState.Decode<T>(const AState, ASecret: string;
  AConfig: INeonConfiguration): T;
begin
  if not TryDecode<T>(AState, ASecret, Result, AConfig) then
    raise EMCPException.Create(SMCPRequestStateInvalid);
end;

{ TMCPRequestStateHelper }

function TMCPRequestStateHelper.StateAs<T>(AConfig: INeonConfiguration): T;
begin
  Result := TMCPRequestState.Decode<T>(RequestState.GetValueOrDefault, AConfig);
end;

function TMCPRequestStateHelper.StateAs<T>(const ASecret: string;
  AConfig: INeonConfiguration): T;
begin
  Result := TMCPRequestState.Decode<T>(RequestState.GetValueOrDefault, ASecret, AConfig);
end;

function TMCPRequestStateHelper.TryStateAs<T>(out AValue: T;
  AConfig: INeonConfiguration): Boolean;
begin
  Result := TMCPRequestState.TryDecode<T>(RequestState.GetValueOrDefault, AValue, AConfig);
end;

function TMCPRequestStateHelper.TryStateAs<T>(const ASecret: string;
  out AValue: T; AConfig: INeonConfiguration): Boolean;
begin
  Result := TMCPRequestState.TryDecode<T>(RequestState.GetValueOrDefault, ASecret,
    AValue, AConfig);
end;

end.
