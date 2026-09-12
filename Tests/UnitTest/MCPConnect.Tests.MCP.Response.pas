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
///   TMCPResponse&lt;T&gt;, the box a method returns when it wants to choose
///   per call between its normal answer and an input request, and TMCPInput,
///   the builder that fills the second.
/// </summary>
unit MCPConnect.Tests.MCP.Response;

interface

uses
  System.SysUtils, System.Rtti, System.TypInfo, System.JSON,
  System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Response,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentResponseWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  TBoxedRow = record
    Id: Integer;
    Name: string;
  end;

  /// <summary>The context a delete carries through its requestState.</summary>
  TDeleteContext = class(TObject)
  public
    TaskId: Integer;
    Reason: string;
  end;

  /// <summary>
  ///   The builder on its own: what it puts in the result, and the two things
  ///   it refuses to build.
  /// </summary>
  [TestFixture]
  TMCPInputBuilderTest = class(TObject)
  public
    [Test]
    procedure TestConfirmIsAFormElicitation();
    [Test]
    procedure TestStateIsCarried();
    [Test]
    procedure TestSeveralRequestsInOneResult();
    [Test]
    procedure TestAnEmptyBuildIsRefused();
    [Test]
    procedure TestAnEmptyKeyIsRefused();
    [Test]
    procedure TestABuilderHandsItsResultOverOnce();
    [Test]
    procedure TestTryBuild();
    [Test]
    procedure TestNeedsConsumesTheBuilder();
    [Test]
    procedure TestAskMethodsFillTheSchema();
  end;

  /// <summary>
  ///   The box itself: what Unwrap leaves behind, and what a box that is never
  ///   opened frees.
  /// </summary>
  [TestFixture]
  TMCPResponseBoxTest = class(TObject)
  public
    [Test]
    procedure TestUnwrapReplacesTheValue();
    [Test]
    procedure TestUnwrapLeavesAPlainValueAlone();
    [Test]
    procedure TestPayloadTypeInfoIsT();
    [Test]
    procedure TestBarePayloadTypeInfoIsNil();
    [Test]
    procedure TestReadyRefusesNil();
    [Test]
    procedure TestReadyKeepsTheRuntimeClass();
    [Test]
    procedure TestUnwrapLeavesAnEmptyValueAlone();
    [Test]
    procedure TestOkIsValue();
  end;

  /// <summary>
  ///   A tool answering through the box, end to end: the box is opened by the
  ///   invoker and nothing of it reaches the client.
  /// </summary>
  [TestFixture]
  TMCPResponseToolTest = class(TObject)
  private
    FServer: TMCPServer;

    procedure ConfigureServer;
    function Call(const ATool: string; const AParams: string = '{}'): string;

    /// <summary>The "result" of the reply. Caller owns it.</summary>
    function ResultOf(const AContent: string): TJSONObject;

    /// <summary>The text of the first content block, or ''.</summary>
    function FirstTextOf(const AContent: string): string;

    /// <summary>The "outputSchema" tools/list reports for ATool, or nil.</summary>
    function OutputSchemaOf(const ATool: string): TJSONObject;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAValueIsConvertedAsUsual();
    [Test]
    procedure TestAReadyResultIsUsedAsIs();
    [Test]
    procedure TestAnErrorIsAToolError();
    [Test]
    procedure TestAStructuredValueIsStructuredContent();
    [Test]
    procedure TestTheOutputSchemaDescribesTheValueNotTheBox();
    [Test]
    procedure TestNeedsAnswersInputRequired();
    [Test]
    procedure TestTheAnswerIsReadBackOnTheRetry();
    [Test]
    procedure TestANilResultIsRefused();
  end;

  /// <summary>
  ///   The other two requests that may not finish in one round trip: a
  ///   resource and a prompt answer through the same box.
  /// </summary>
  [TestFixture]
  TMCPResponseFeatureTest = class(TObject)
  private
    FServer: TMCPServer;

    procedure ConfigureServer;
    function Call(const ABody: string): string;

    /// <summary>The "result" of the reply. Caller owns it.</summary>
    function ResultOf(const AContent: string): TJSONObject;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAResourceValueIsConverted();
    [Test]
    procedure TestAPromptAsksForInput();
  end;

  /// <summary>The operation-scoped reply builders.</summary>
  [TestFixture]
  TMCPReplyBuilderTest = class(TObject)
  public
    [Test]
    procedure TestToolFailIsAnError();
    [Test]
    procedure TestResourceReplyBuildsContents();
    [Test]
    procedure TestPromptReplyBuildsAMessage();
  end;

  /// <summary>The forgiving readers over TInputResponses.</summary>
  [TestFixture]
  TMCPInputResponsesTest = class(TObject)
  private
    procedure AddResponse(AResponses: TInputResponses; const AKey, AJson: string);
  public
    [Test]
    procedure TestOutcomeTellsTheThreeApart();
    [Test]
    procedure TestScalarReadersCoerceAndDefault();
    [Test]
    procedure TestMultiValueAndTriReaders();
  end;

  /// <summary>The typed requestState codec.</summary>
  [TestFixture]
  TMCPRequestStateTest = class(TObject)
  private
    function NewContext: TDeleteContext;
  public
    [Test]
    procedure TestRoundTrip();
    [Test]
    procedure TestMarkerAndForeignValues();
    [Test]
    procedure TestSignatureIsVerified();
    [Test]
    procedure TestParamsHelperReadsTheState();
    [Test]
    procedure TestInputCarriesTheState();
  end;

implementation

uses
  Neon.Core.Persistence.JSON,

  JRPC.Classes,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Elicitation;

type
  TResponseTools = class(TObject)
  private
    [Context] FParams: TCallToolRequestParams;
  public
    [McpTool('boxed_text', 'Answers with a boxed string')]
    function BoxedText: TMCPResponse<string>;

    [McpTool('boxed_ready', 'Answers with a result it built itself')]
    function BoxedReady: TMCPResponse<string>;

    [McpTool('boxed_error', 'Answers with a tool error')]
    function BoxedError: TMCPResponse<string>;

    [McpTool('boxed_nil', 'Answers with a nil result, which must be refused')]
    function BoxedNil: TMCPResponse<string>;

    [McpTool('boxed_row', 'Answers with a boxed record', 'structured')]
    function BoxedRow: TMCPResponse<TBoxedRow>;

    [McpTool('boxed_needs', 'Asks before it answers')]
    function BoxedNeeds: TMCPResponse<string>;
  end;

  TResponseResources = class(TObject)
  public
    [McpResource('boxed', 'resource://boxed', 'text/plain', 'A boxed resource')]
    function Boxed: TMCPResponse<string>;
  end;

  TResponsePrompts = class(TObject)
  public
    [McpPrompt('boxed_prompt', 'Boxed prompt', 'A prompt that asks first')]
    function BoxedPrompt: TMCPResponse<string>;
  end;

function TResponseResources.Boxed: TMCPResponse<string>;
begin
  Result := TMCPResponse<string>.Value('resource body');
end;

function TResponsePrompts.BoxedPrompt: TMCPResponse<string>;
begin
  Result := TMCPResponse<string>.Needs(
    TMCPInput.New.AskText('topic', 'What should I summarize?', 'topic', 'Topic'));
end;

function TResponseTools.BoxedText: TMCPResponse<string>;
begin
  Result := TMCPResponse<string>.Value('hello');
end;

function TResponseTools.BoxedReady: TMCPResponse<string>;
var
  LResult: TCallToolResult;
begin
  LResult := TCallToolResult.Create;
  LResult.Content.AddText('built by hand');
  Result := TMCPResponse<string>.Ready(LResult);
end;

function TResponseTools.BoxedError: TMCPResponse<string>;
begin
  Result := TMCPResponse<string>.Ready(TCallToolReply.Fail('it went wrong'));
end;

function TResponseTools.BoxedNil: TMCPResponse<string>;
begin
  Result := TMCPResponse<string>.Ready(nil);
end;

function TResponseTools.BoxedRow: TMCPResponse<TBoxedRow>;
var
  LRow: TBoxedRow;
begin
  LRow.Id := 7;
  LRow.Name := 'Alice';
  Result := TMCPResponse<TBoxedRow>.Value(LRow);
end;

function TResponseTools.BoxedNeeds: TMCPResponse<string>;
begin
  // The first call asks, the retry - carrying the client's answer under the
  // same key - is answered
  if FParams.InputResponses.Count = 0 then
    Exit(TMCPResponse<string>.Needs(
      TMCPInput.New('delete:1').Confirm('confirm', 'Are you sure?')));

  if FParams.InputResponses.Confirmed('confirm') then
    Result := TMCPResponse<string>.Value('confirmed')
  else
    Result := TMCPResponse<string>.Value('declined');
end;

{ TSilentResponseWriter }

procedure TSilentResponseWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentResponseWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentResponseWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TMCPInputBuilderTest }

procedure TMCPInputBuilderTest.TestConfirmIsAFormElicitation;
var
  LResult: TInputRequiredResult;
  LJson: TJSONObject;
begin
  LResult := TMCPInput.New.Confirm('confirm', 'Are you sure?').Build;
  try
    Assert.AreEqual(TResultType.InputRequired, LResult.ResultType);
    Assert.AreEqual(MCP_INPUT_ELICITATION, LResult.InputRequests.MethodOf('confirm'));

    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Assert.IsNotNull(LJson.FindValue('inputRequests.confirm.params.requestedSchema.properties.confirm'),
        'The confirm property should be in the requested schema');
      Assert.AreEqual('boolean',
        LJson.GetValue<string>('inputRequests.confirm.params.requestedSchema.properties.confirm.type'));
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TMCPInputBuilderTest.TestStateIsCarried;
var
  LResult: TInputRequiredResult;
begin
  LResult := TMCPInput.New('token-1').Confirm('c', 'Sure?').Build;
  try
    Assert.AreEqual('token-1', LResult.RequestState.Value);
  finally
    LResult.Free;
  end;

  LResult := TMCPInput.New.Confirm('c', 'Sure?').State('token-2').Build;
  try
    Assert.AreEqual('token-2', LResult.RequestState.Value);
  finally
    LResult.Free;
  end;
end;

procedure TMCPInputBuilderTest.TestSeveralRequestsInOneResult;
var
  LResult: TInputRequiredResult;
begin
  LResult := TMCPInput.New
    .Confirm('confirm', 'Sure?')
    .AskText('name', 'Who are you?', 'name', 'Your name')
    .Roots('roots')
    .Build;
  try
    Assert.AreEqual(3, LResult.InputRequests.Count);
    Assert.AreEqual(MCP_INPUT_ELICITATION, LResult.InputRequests.MethodOf('name'));
    Assert.AreEqual(MCP_INPUT_ROOTS, LResult.InputRequests.MethodOf('roots'));
  finally
    LResult.Free;
  end;
end;

procedure TMCPInputBuilderTest.TestAnEmptyBuildIsRefused;
begin
  Assert.WillRaise(
    procedure
    var
      LInput: TMCPInput;
      LResult: TInputRequiredResult;
    begin
      LInput := TMCPInput.New;
      LResult := LInput.Build;
      LResult.Free;
    end,
    EMCPException,
    'An input_required asking for nothing stalls the call');
end;

procedure TMCPInputBuilderTest.TestAnEmptyKeyIsRefused;
begin
  Assert.WillRaise(
    procedure
    var
      LInput: TMCPInput;
    begin
      LInput := TMCPInput.New;
      LInput.Confirm('', 'Sure?');
    end,
    EMCPException,
    'A request the client cannot answer under any key is no request');
end;

procedure TMCPInputBuilderTest.TestABuilderHandsItsResultOverOnce;
var
  LInput: TMCPInput;
  LResult: TInputRequiredResult;
begin
  LInput := TMCPInput.New;
  LInput.Confirm('c', 'Sure?');
  LResult := LInput.Build;
  LResult.Free;

  // Empty again: the result is the caller's, and the builder holds no
  // reference to it
  Assert.AreEqual(0, LInput.Count);
  Assert.WillRaise(
    procedure
    begin
      LInput.Build;
    end,
    EMCPException);
end;

procedure TMCPInputBuilderTest.TestTryBuild;
var
  LInput: TMCPInput;
  LResult: TInputRequiredResult;
begin
  LInput := TMCPInput.New;
  Assert.IsFalse(LInput.TryBuild(LResult), 'An empty builder has nothing to hand over');
  Assert.IsFalse(Assigned(LResult));

  LInput.Confirm('c', 'Sure?');
  Assert.IsTrue(LInput.TryBuild(LResult));
  try
    Assert.IsNotNull(LResult);
    Assert.AreEqual(1, LResult.InputRequests.Count);
  finally
    LResult.Free;
  end;
end;

procedure TMCPInputBuilderTest.TestNeedsConsumesTheBuilder;
var
  LInput: TMCPInput;
  LResponse: TMCPResponse<string>;
begin
  LInput := TMCPInput.New('state').Confirm('c', 'Sure?');
  LResponse := TMCPResponse<string>.Needs(LInput);
  try
    // The state is shared, so the caller's builder is empty too: the same
    // result can never be handed over twice
    Assert.AreEqual(0, LInput.Count);
    Assert.WillRaise(
      procedure
      begin
        LInput.Build;
      end,
      EMCPException);
  finally
    LResponse.Free;   // the box owns the result until it is opened
  end;
end;

procedure TMCPInputBuilderTest.TestAskMethodsFillTheSchema;
var
  LResult: TInputRequiredResult;
begin
  LResult := TMCPInput.New
    .AskInteger('age', 'How old?', 'age', 'Age')
    .AskNumber('score', 'Score?', 'score', 'Score')
    .AskMultiChoice('tags', 'Tags?', 'tags', 'Tags', ['a', 'b'])
    .Build;
  try
    Assert.AreEqual(3, LResult.InputRequests.Count);
    Assert.AreEqual(MCP_INPUT_ELICITATION, LResult.InputRequests.MethodOf('age'));
    Assert.AreEqual(MCP_INPUT_ELICITATION, LResult.InputRequests.MethodOf('tags'));
  finally
    LResult.Free;
  end;
end;

{ TMCPResponseBoxTest }

procedure TMCPResponseBoxTest.TestUnwrapReplacesTheValue;
var
  LValue: TValue;
begin
  LValue := TValue.From<TMCPResponse<string>>(TMCPResponse<string>.Value('boxed'));

  Assert.IsTrue(TMCPResponse.Unwrap(LValue), 'A box should be opened');
  Assert.AreEqual('boxed', LValue.AsString);
end;

procedure TMCPResponseBoxTest.TestUnwrapLeavesAPlainValueAlone;
var
  LValue: TValue;
begin
  LValue := TValue.From<string>('plain');

  Assert.IsFalse(TMCPResponse.Unwrap(LValue), 'A value that is not a box is not one');
  Assert.AreEqual('plain', LValue.AsString);
end;

procedure TMCPResponseBoxTest.TestPayloadTypeInfoIsT;
begin
  Assert.IsTrue(TMCPResponse<string>.PayloadTypeInfo = TypeInfo(string));
  Assert.IsTrue(TMCPResponse<TBoxedRow>.PayloadTypeInfo = TypeInfo(TBoxedRow));
end;

procedure TMCPResponseBoxTest.TestBarePayloadTypeInfoIsNil;
begin
  Assert.IsTrue(TMCPResponse.PayloadTypeInfo = nil,
    'The bare box says nothing about what it holds');
end;

procedure TMCPResponseBoxTest.TestReadyRefusesNil;
begin
  Assert.WillRaise(
    procedure
    var
      LBox: TMCPResponse;
    begin
      LBox := TMCPResponse.Ready(nil);
      LBox.Free;
    end,
    EMCPException,
    'Nil would box as an empty payload, which IsType matches against every type');
end;

procedure TMCPResponseBoxTest.TestReadyKeepsTheRuntimeClass;
var
  LResult: TCallToolResult;
  LBox: TMCPResponse;
begin
  LResult := TCallToolResult.Create;
  LBox := TMCPResponse.Ready(LResult);
  try
    Assert.IsTrue(PTypeInfo(TCallToolResult.ClassInfo) = LBox.Payload.TypeInfo,
      'Ready must box the runtime class, not TObject');
  finally
    LBox.Free;
  end;
end;

procedure TMCPResponseBoxTest.TestUnwrapLeavesAnEmptyValueAlone;
var
  LValue: TValue;
begin
  LValue := TValue.Empty;
  Assert.IsFalse(TMCPResponse.Unwrap(LValue));
  Assert.IsTrue(LValue.IsEmpty);
end;

procedure TMCPResponseBoxTest.TestOkIsValue;
var
  LBox: TMCPResponse<string>;
begin
  LBox := TMCPResponse<string>.Ok('inferred');
  try
    Assert.IsTrue(LBox.PayloadTypeInfo = TypeInfo(string));
    Assert.AreEqual('inferred', LBox.Payload.AsString);
  finally
    LBox.Free;
  end;
end;

{ TMCPResponseToolTest }

procedure TMCPResponseToolTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TMCPResponseToolTest.ConfigureServer;
begin
  if Assigned(FServer) then
    Exit;

  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('response-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TResponseTools)
    .BackToMCP
  .ApplyConfig;
end;

function TMCPResponseToolTest.Call(const ATool, AParams: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent, LBody: string;
begin
  ConfigureServer();

  if ATool.IsEmpty then
    LBody := '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
  else
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"tools/call",' +
      '"params":{"name":"%s","arguments":%s}}', [ATool, AParams]);

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TSilentResponseWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := LBody;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

function TMCPResponseToolTest.ResultOf(const AContent: string): TJSONObject;
var
  LValue, LResult: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(AContent);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LResult := TJSONObject(LValue).GetValue('result');
    if LResult is TJSONObject then
      Result := LResult.Clone as TJSONObject;
  finally
    LValue.Free;
  end;
end;

function TMCPResponseToolTest.FirstTextOf(const AContent: string): string;
var
  LResult: TJSONObject;
begin
  Result := '';
  LResult := ResultOf(AContent);
  if not Assigned(LResult) then
    Exit;
  try
    Result := LResult.GetValue<string>('content[0].text', '');
  finally
    LResult.Free;
  end;
end;

function TMCPResponseToolTest.OutputSchemaOf(const ATool: string): TJSONObject;
var
  LResult: TJSONObject;
  LTools, LSchema: TJSONValue;
  LTool: TJSONValue;
begin
  Result := nil;

  LResult := ResultOf(Call(''));
  if not Assigned(LResult) then
    Exit;

  try
    LTools := LResult.GetValue('tools');
    if not (LTools is TJSONArray) then
      Exit;

    for LTool in TJSONArray(LTools) do
    begin
      if not (LTool is TJSONObject) then
        Continue;
      if TJSONObject(LTool).GetValue<string>('name', '') <> ATool then
        Continue;

      LSchema := TJSONObject(LTool).GetValue('outputSchema');
      if LSchema is TJSONObject then
        Result := LSchema.Clone as TJSONObject;
      Break;
    end;
  finally
    LResult.Free;
  end;
end;

procedure TMCPResponseToolTest.TestAValueIsConvertedAsUsual;
begin
  Assert.AreEqual('hello', FirstTextOf(Call('boxed_text')));
end;

procedure TMCPResponseToolTest.TestAReadyResultIsUsedAsIs;
begin
  Assert.AreEqual('built by hand', FirstTextOf(Call('boxed_ready')));
end;

procedure TMCPResponseToolTest.TestAnErrorIsAToolError;
var
  LResult: TJSONObject;
begin
  LResult := ResultOf(Call('boxed_error'));
  try
    Assert.IsNotNull(LResult);
    Assert.IsTrue(LResult.GetValue<Boolean>('isError', False), 'The box should carry isError');
    Assert.AreEqual('it went wrong', LResult.GetValue<string>('content[0].text', ''));
  finally
    LResult.Free;
  end;
end;

procedure TMCPResponseToolTest.TestAStructuredValueIsStructuredContent;
var
  LResult: TJSONObject;
begin
  LResult := ResultOf(Call('boxed_row'));
  try
    Assert.IsNotNull(LResult);
    Assert.AreEqual(7, LResult.GetValue<Integer>('structuredContent.id', 0));
    Assert.AreEqual('Alice', LResult.GetValue<string>('structuredContent.name', ''));
  finally
    LResult.Free;
  end;
end;

procedure TMCPResponseToolTest.TestTheOutputSchemaDescribesTheValueNotTheBox;
var
  LSchema: TJSONObject;
begin
  LSchema := OutputSchemaOf('boxed_row');
  try
    Assert.IsNotNull(LSchema, 'A structured tool should carry an outputSchema');
    Assert.AreEqual('object', LSchema.GetValue<string>('type', ''));
    Assert.IsNotNull(LSchema.FindValue('properties.id'), 'The schema should describe the record');
    Assert.IsNull(LSchema.FindValue('properties.resultMeta'),
      'The box is a carrier and describes nothing');
  finally
    LSchema.Free;
  end;
end;

procedure TMCPResponseToolTest.TestNeedsAnswersInputRequired;
var
  LResult: TJSONObject;
begin
  LResult := ResultOf(Call('boxed_needs'));
  try
    Assert.IsNotNull(LResult);
    Assert.AreEqual('input_required', LResult.GetValue<string>('resultType', ''));
    Assert.AreEqual('delete:1', LResult.GetValue<string>('requestState', ''));
    Assert.AreEqual(MCP_INPUT_ELICITATION,
      LResult.GetValue<string>('inputRequests.confirm.method', ''));
  finally
    LResult.Free;
  end;
end;

procedure TMCPResponseToolTest.TestTheAnswerIsReadBackOnTheRetry;
var
  LRetry: string;
begin
  ConfigureServer();

  // The retry carries the client's answer under the key the server chose
  LRetry := Format('{"jsonrpc":"2.0","id":2,"method":"tools/call","params":' +
    '{"name":"boxed_needs","arguments":{},"requestState":"delete:1",' +
    '"inputResponses":{"confirm":{"action":"accept","content":{"confirm":true}}}}}', []);

  var LHandler := TMCPTransportHandler.Create(FServer, TSilentResponseWriter.Create);
  var LContent := '';
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := LRetry;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Assert.AreEqual('confirmed', FirstTextOf(LContent));
end;

procedure TMCPResponseToolTest.TestANilResultIsRefused;
begin
  // A method that boxes nil gets a clear error instead of a nil response
  Assert.IsTrue(Call('boxed_nil').Contains('"error"'),
    'A nil result must not become a nil response');
end;

{ TMCPResponseFeatureTest }

procedure TMCPResponseFeatureTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TMCPResponseFeatureTest.ConfigureServer;
begin
  if Assigned(FServer) then
    Exit;

  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('response-feature-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Resources
      .RegisterClass(TResponseResources)
    .BackToMCP
    .Prompts
      .RegisterClass(TResponsePrompts)
    .BackToMCP
  .ApplyConfig;
end;

function TMCPResponseFeatureTest.Call(const ABody: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent: string;
begin
  ConfigureServer();

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TSilentResponseWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := ABody;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

function TMCPResponseFeatureTest.ResultOf(const AContent: string): TJSONObject;
var
  LValue, LResult: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(AContent);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LResult := TJSONObject(LValue).GetValue('result');
    if LResult is TJSONObject then
      Result := LResult.Clone as TJSONObject;
  finally
    LValue.Free;
  end;
end;

procedure TMCPResponseFeatureTest.TestAResourceValueIsConverted;
var
  LResult: TJSONObject;
begin
  LResult := ResultOf(Call('{"jsonrpc":"2.0","id":1,"method":"resources/read",' +
    '"params":{"uri":"resource://boxed"}}'));
  try
    Assert.IsNotNull(LResult);
    Assert.AreEqual('resource body', LResult.GetValue<string>('contents[0].text', ''));
  finally
    LResult.Free;
  end;
end;

procedure TMCPResponseFeatureTest.TestAPromptAsksForInput;
var
  LResult: TJSONObject;
begin
  LResult := ResultOf(Call('{"jsonrpc":"2.0","id":1,"method":"prompts/get",' +
    '"params":{"name":"boxed_prompt","arguments":{}}}'));
  try
    Assert.IsNotNull(LResult);
    Assert.AreEqual('input_required', LResult.GetValue<string>('resultType', ''));
    Assert.AreEqual(MCP_INPUT_ELICITATION,
      LResult.GetValue<string>('inputRequests.topic.method', ''));
  finally
    LResult.Free;
  end;
end;

{ TMCPReplyBuilderTest }

procedure TMCPReplyBuilderTest.TestToolFailIsAnError;
var
  LResult: TCallToolResult;
begin
  LResult := TCallToolReply.Fail('boom');
  try
    Assert.IsTrue(LResult.IsError.GetValueOrDefault);
    Assert.AreEqual(1, LResult.Content.Count);
    Assert.IsTrue(LResult.Content[0] is TTextContent);
  finally
    LResult.Free;
  end;
end;

procedure TMCPReplyBuilderTest.TestResourceReplyBuildsContents;
var
  LResult: TReadResourceResult;
begin
  LResult := TResourceReply.Text('resource://x', 'text/plain', 'body');
  try
    Assert.AreEqual(1, LResult.Contents.Count);
    Assert.IsTrue(LResult.Contents[0] is TTextResourceContents);
  finally
    LResult.Free;
  end;
end;

procedure TMCPReplyBuilderTest.TestPromptReplyBuildsAMessage;
var
  LResult: TGetPromptResult;
begin
  LResult := TPromptReply.User('hello');
  try
    Assert.AreEqual(1, LResult.Messages.Count);
  finally
    LResult.Free;
  end;
end;

{ TMCPInputResponsesTest }

procedure TMCPInputResponsesTest.AddResponse(AResponses: TInputResponses;
  const AKey, AJson: string);
var
  LResponse: TInputResponse;
begin
  LResponse := TInputResponse.Create;
  LResponse.Raw := TJSONObject.ParseJSONValue(AJson);
  AResponses.Add(AKey, LResponse);
end;

procedure TMCPInputResponsesTest.TestOutcomeTellsTheThreeApart;
var
  LResponses: TInputResponses;
begin
  LResponses := TInputResponses.Create([doOwnsValues]);
  try
    AddResponse(LResponses, 'accept', '{"action":"accept","content":{}}');
    AddResponse(LResponses, 'decline', '{"action":"decline"}');
    AddResponse(LResponses, 'cancel', '{"action":"cancel"}');

    Assert.AreEqual(TElicitationOutcome.Accepted, LResponses.Outcome('accept'));
    Assert.AreEqual(TElicitationOutcome.Declined, LResponses.Outcome('decline'));
    Assert.AreEqual(TElicitationOutcome.Cancelled, LResponses.Outcome('cancel'));
    Assert.AreEqual(TElicitationOutcome.Absent, LResponses.Outcome('missing'));
  finally
    LResponses.Free;
  end;
end;

procedure TMCPInputResponsesTest.TestScalarReadersCoerceAndDefault;
var
  LResponses: TInputResponses;
begin
  LResponses := TInputResponses.Create([doOwnsValues]);
  try
    AddResponse(LResponses, 'form',
      '{"action":"accept","content":{"confirm":true,"name":"Ada","age":42,"score":3.5}}');

    Assert.IsTrue(LResponses.Confirmed('form'));
    Assert.AreEqual('Ada', LResponses.FieldAsString('form', 'name'));
    Assert.AreEqual(42, LResponses.FieldAsInteger('form', 'age'));
    Assert.AreEqual(3.5, LResponses.FieldAsDouble('form', 'score'), 0.0001);
    Assert.IsTrue(LResponses.FieldAsBoolean('form', 'confirm'));

    // Missing or malformed reads as the default, never as an exception
    Assert.AreEqual('n/a', LResponses.FieldAsString('form', 'missing', 'n/a'));
    Assert.AreEqual(7, LResponses.FieldAsInteger('form', 'missing', 7));
    Assert.IsFalse(LResponses.Accepted('missing'));
  finally
    LResponses.Free;
  end;
end;

procedure TMCPInputResponsesTest.TestMultiValueAndTriReaders;
var
  LResponses: TInputResponses;
  LValues: TArray<string>;
  LText: string;
  LNumber: Integer;
begin
  LResponses := TInputResponses.Create([doOwnsValues]);
  try
    AddResponse(LResponses, 'form',
      '{"action":"accept","content":{"tags":["a","b"],"one":"x"}}');

    LValues := LResponses.FieldAsStrings('form', 'tags');
    Assert.AreEqual(2, Length(LValues));
    Assert.AreEqual('a', LValues[0]);
    Assert.AreEqual('b', LValues[1]);

    // A single scalar reads as a one-element array
    Assert.AreEqual(1, Length(LResponses.FieldAsStrings('form', 'one')));

    Assert.IsTrue(LResponses.TryFieldAsString('form', 'one', LText));
    Assert.AreEqual('x', LText);
    Assert.IsFalse(LResponses.TryFieldAsInteger('form', 'missing', LNumber));
  finally
    LResponses.Free;
  end;
end;

{ TMCPRequestStateTest }

function TMCPRequestStateTest.NewContext: TDeleteContext;
begin
  Result := TDeleteContext.Create;
  Result.TaskId := 7;
  Result.Reason := 'because';
end;

procedure TMCPRequestStateTest.TestRoundTrip;
var
  LContext: TDeleteContext;
  LState: string;
  LBack: TDeleteContext;
begin
  LContext := NewContext;
  LState := TMCPRequestState.Encode(LContext);
  LContext.Free;

  Assert.IsTrue(TMCPRequestState.IsRequestState(LState));
  Assert.IsTrue(LState.StartsWith(MCP_REQUEST_STATE_PREFIX));

  LBack := TMCPRequestState.Decode<TDeleteContext>(LState);
  try
    Assert.AreEqual(7, LBack.TaskId);
    Assert.AreEqual('because', LBack.Reason);
  finally
    LBack.Free;
  end;
end;

procedure TMCPRequestStateTest.TestMarkerAndForeignValues;
var
  LBack: TDeleteContext;
begin
  Assert.IsFalse(TMCPRequestState.IsRequestState('not-a-state'));
  Assert.IsFalse(TMCPRequestState.TryDecode<TDeleteContext>('not-a-state', LBack));
  Assert.IsFalse(Assigned(LBack));

  Assert.WillRaise(
    procedure
    begin
      TMCPRequestState.Decode<TDeleteContext>('not-a-state');
    end,
    EMCPException);
end;

procedure TMCPRequestStateTest.TestSignatureIsVerified;
var
  LContext: TDeleteContext;
  LState, LTampered: string;
  LBack: TDeleteContext;
  LIndex: Integer;
begin
  LContext := NewContext;
  LState := TMCPRequestState.Encode(LContext, 's3cret');
  LContext.Free;

  Assert.IsTrue(LState.StartsWith(MCP_REQUEST_STATE_SIGNED_PREFIX));

  Assert.IsTrue(TMCPRequestState.TryDecode<TDeleteContext>(LState, 's3cret', LBack));
  LBack.Free;

  // The wrong secret is not the server's signature
  Assert.IsFalse(TMCPRequestState.TryDecode<TDeleteContext>(LState, 'other', LBack));
  Assert.IsFalse(Assigned(LBack));

  // A tampered payload no longer matches the signature
  LTampered := LState;
  LIndex := Length(MCP_REQUEST_STATE_SIGNED_PREFIX) + 2;
  if LTampered[LIndex] = 'A' then
    LTampered[LIndex] := 'B'
  else
    LTampered[LIndex] := 'A';
  Assert.IsFalse(TMCPRequestState.TryDecode<TDeleteContext>(LTampered, 's3cret', LBack));
  Assert.IsFalse(Assigned(LBack));
end;

procedure TMCPRequestStateTest.TestParamsHelperReadsTheState;
var
  LContext: TDeleteContext;
  LParams: TInputRequestParams;
  LBack: TDeleteContext;
begin
  LParams := TInputRequestParams.Create;
  try
    LContext := NewContext;
    try
      LParams.RequestState := TMCPRequestState.Encode(LContext);
    finally
      LContext.Free;
    end;

    LBack := LParams.StateAs<TDeleteContext>;
    try
      Assert.AreEqual(7, LBack.TaskId);
    finally
      LBack.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestStateTest.TestInputCarriesTheState;
var
  LContext: TDeleteContext;
  LInput: TMCPInput;
  LResult: TInputRequiredResult;
  LBack: TDeleteContext;
begin
  LContext := NewContext;
  try
    LInput := TMCPInput.New(TMCPRequestState.Encode(LContext)).Confirm('c', 'Sure?');
  finally
    LContext.Free;
  end;

  LResult := LInput.Build;
  try
    LBack := TMCPRequestState.Decode<TDeleteContext>(LResult.RequestState.GetValueOrDefault);
    try
      Assert.AreEqual(7, LBack.TaskId);
    finally
      LBack.Free;
    end;
  finally
    LResult.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPInputBuilderTest);
  TDUnitX.RegisterTestFixture(TMCPResponseBoxTest);
  TDUnitX.RegisterTestFixture(TMCPResponseToolTest);
  TDUnitX.RegisterTestFixture(TMCPResponseFeatureTest);
  TDUnitX.RegisterTestFixture(TMCPReplyBuilderTest);
  TDUnitX.RegisterTestFixture(TMCPInputResponsesTest);
  TDUnitX.RegisterTestFixture(TMCPRequestStateTest);

end.
