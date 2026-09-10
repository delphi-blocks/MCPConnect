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
unit MCPConnect.Tests.MCP.Mrtr;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  JRPC.Classes,
  JRPC.Core,
  MCPConnect.MCP.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Elicitation,
  MCPConnect.MCP.Types.Mrtr;

type
  /// <summary>
  ///   Emitting an InputRequiredResult: the server side of a multi round-trip
  ///   request.
  /// </summary>
  [TestFixture]
  TMCPInputRequestsTest = class(TObject)
  private
    FResult: TInputRequiredResult;

    /// <summary>The rendered request under AKey. Caller owns it.</summary>
    function RequestJson(const AKey: string): TJSONObject;

    function NewFormParams(const AMessage: string): TElicitRequestParams;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestResultTypeIsInputRequired;
    [Test]
    procedure TestAddElicitation;
    [Test]
    procedure TestAddSampling;
    [Test]
    procedure TestAddRoots_OmitsOptionalParams;
    [Test]
    procedure TestMethodOf;
    [Test]
    procedure TestRequestsAreAMapKeyedByTheServer;
    [Test]
    procedure TestRequestStateIsCarried;
  end;

  /// <summary>
  ///   TSamplingMessage's content: the SamplingMessageContentBlock union, which
  ///   is one block or an array of them, and the typed access over the raw JSON
  ///   it is held as.
  /// </summary>
  [TestFixture]
  TMCPSamplingMessageTest = class(TObject)
  private
    /// <summary>A result read from AJson. Caller owns it.</summary>
    function ResultFrom(const AJson: string): TCreateMessageResult;
  public
    [Test]
    procedure TestCreateLeavesMetaAssigned;
    [Test]
    procedure TestTagsAreAssignedToo;

    [Test]
    procedure TestSingleBlockIsOneBlock;
    [Test]
    procedure TestArrayOfBlocksIsCounted;
    [Test]
    procedure TestNoContentIsNoBlocks;

    [Test]
    procedure TestSingleBlockIsReadableAsBlockZero;
    [Test]
    procedure TestEveryBlockTypeIsReadable;
    [Test]
    procedure TestTheWrongTypeAnswersNil;
    [Test]
    procedure TestAskingTwiceAnswersTheSameInstance;
    [Test]
    procedure TestABlockThatIsNotThereAnswersNil;

    [Test]
    procedure TestModelAndStopReasonSurvive;
    [Test]
    procedure TestMetaSurvivesTheRoundTrip;

    [Test]
    procedure TestAddContentMakesASingleBlock;
    [Test]
    procedure TestASecondBlockPromotesToAnArray;
    [Test]
    procedure TestPromotionKeepsTheFirstBlockAtIndexZero;
    [Test]
    procedure TestAddTextIsATextBlock;
  end;

  /// <summary>
  ///   Decoding InputResponses: the client's answers on the retry.
  /// </summary>
  [TestFixture]
  TMCPInputResponsesTest = class(TObject)
  private
    FParams: TMrtrRequestParams;

    procedure Decode(const AJson: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRawIsCapturedForEveryAnswer;
    [Test]
    procedure TestDecodeElicitation;
    [Test]
    procedure TestDecodeRoots;
    [Test]
    procedure TestDecodeIsCached;
    [Test]
    procedure TestMissingKeyDecodesToNil;
    [Test]
    procedure TestFind;
    [Test]
    procedure TestRetryCarriesRequestState;
  end;

  TMrtrTools = class
  public
    [McpTool('who', 'Needs to know who you are first')]
    function Who: TInputRequiredResult;

    [McpTool('plain', 'An ordinary tool')]
    function Plain: string;
  end;

  /// <summary>
  ///   tools/call answers anyOf[InputRequiredResult|CallToolResult]. The
  ///   discrimination is the result class itself, carried on the common
  ///   TBaseResult the endpoint returns.
  /// </summary>
  [TestFixture]
  TMCPToolCallResultUnionTest = class(TObject)
  private
    FServer: TMCPServer;
    FConfig: IMCPConfig;
    FApi: TMCPToolsApi;
    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;

    function CallTool(const AName: string): TBaseResult;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestOrdinaryToolReturnsCallToolResult;
    [Test]
    procedure TestToolCanAnswerInputRequired;
    [Test]
    procedure TestInputRequiredSerializesByItsRuntimeClass;
  end;

implementation

{ TMCPInputRequestsTest }

procedure TMCPInputRequestsTest.Setup;
begin
  FResult := TInputRequiredResult.Create;
end;

procedure TMCPInputRequestsTest.TearDown;
begin
  FResult.Free;
end;

function TMCPInputRequestsTest.NewFormParams(const AMessage: string): TElicitRequestParams;
var
  LSchema: TMCPElicitationSchema;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddString('name', 'Your name', True);
    Result := TMCPElicitRequest.Form(AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

function TMCPInputRequestsTest.RequestJson(const AKey: string): TJSONObject;
var
  LJson, LRequests: TJSONObject;
begin
  LJson := TNeon.ObjectToJSON(FResult, MCPNeonConfig) as TJSONObject;
  try
    LRequests := LJson.GetValue('inputRequests') as TJSONObject;
    Assert.IsNotNull(LRequests, 'The result should carry inputRequests');

    Result := (LRequests.GetValue(AKey) as TJSONObject).Clone as TJSONObject;
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestResultTypeIsInputRequired;
var
  LJson: TJSONObject;
begin
  LJson := TNeon.ObjectToJSON(FResult, MCPNeonConfig) as TJSONObject;
  try
    Assert.AreEqual('input_required', LJson.GetValue<string>('resultType'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestAddElicitation;
var
  LRequest: TInputRequest;
  LJson: TJSONObject;
begin
  LRequest := FResult.InputRequests.AddElicitation('who', NewFormParams('Who are you?'));
  Assert.AreEqual(MCP_INPUT_ELICITATION, LRequest.Method);
  Assert.IsNotNull(LRequest.Elicitation, 'The typed params stay reachable on the request');

  LJson := RequestJson('who');
  try
    // An InputRequest is a whole request object, not just its params
    Assert.AreEqual('elicitation/create', LJson.GetValue<string>('method'));
    Assert.AreEqual('Who are you?',
      (LJson.GetValue('params') as TJSONObject).GetValue<string>('message'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestAddSampling;
var
  LRequest: TInputRequest;
  LParams: TCreateMessageRequestParams;
  LJson: TJSONObject;
begin
  LParams := TCreateMessageRequestParams.Create;
  LParams.MaxTokens := 256;

  LRequest := FResult.InputRequests.AddSampling('ask', LParams);
  Assert.AreEqual(MCP_INPUT_SAMPLING, LRequest.Method);

  LJson := RequestJson('ask');
  try
    Assert.AreEqual('sampling/createMessage', LJson.GetValue<string>('method'));
    Assert.AreEqual(256, (LJson.GetValue('params') as TJSONObject).GetValue<Integer>('maxTokens'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestAddRoots_OmitsOptionalParams;
var
  LRequest: TInputRequest;
  LJson: TJSONObject;
begin
  LRequest := FResult.InputRequests.AddRoots('where');
  Assert.AreEqual(MCP_INPUT_ROOTS, LRequest.Method);

  LJson := RequestJson('where');
  try
    Assert.AreEqual('roots/list', LJson.GetValue<string>('method'));
    Assert.IsNull(LJson.GetValue('params'), 'roots/list is the one whose params are optional');
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestMethodOf;
begin
  FResult.InputRequests.AddElicitation('who', NewFormParams('Who?'));
  FResult.InputRequests.AddRoots('where');

  // This is what tells the server how the matching answer decodes
  Assert.AreEqual(MCP_INPUT_ELICITATION, FResult.InputRequests.MethodOf('who'));
  Assert.AreEqual(MCP_INPUT_ROOTS, FResult.InputRequests.MethodOf('where'));
  Assert.AreEqual('', FResult.InputRequests.MethodOf('absent'));
end;

procedure TMCPInputRequestsTest.TestRequestsAreAMapKeyedByTheServer;
var
  LJson, LRequests: TJSONObject;
begin
  FResult.InputRequests.AddElicitation('who', NewFormParams('Who?'));
  FResult.InputRequests.AddRoots('where');

  LJson := TNeon.ObjectToJSON(FResult, MCPNeonConfig) as TJSONObject;
  try
    LRequests := LJson.GetValue('inputRequests') as TJSONObject;
    Assert.AreEqual(2, LRequests.Count);
    Assert.IsNotNull(LRequests.GetValue('who'));
    Assert.IsNotNull(LRequests.GetValue('where'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPInputRequestsTest.TestRequestStateIsCarried;
var
  LJson: TJSONObject;
begin
  FResult.RequestState := 'signed-state';

  LJson := TNeon.ObjectToJSON(FResult, MCPNeonConfig) as TJSONObject;
  try
    Assert.AreEqual('signed-state', LJson.GetValue<string>('requestState'));
  finally
    LJson.Free;
  end;
end;

{ TMCPInputResponsesTest }

procedure TMCPInputResponsesTest.Setup;
begin
  FParams := TMrtrRequestParams.Create;
end;

procedure TMCPInputResponsesTest.TearDown;
begin
  FParams.Free;
end;

procedure TMCPInputResponsesTest.Decode(const AJson: string);
begin
  TNeon.JSONToObject(FParams, AJson, MCPNeonConfig);
end;

procedure TMCPInputResponsesTest.TestRawIsCapturedForEveryAnswer;
var
  LResponse: TInputResponse;
begin
  // Nothing inside an answer says which shape it is, so the raw JSON is what
  // survives until the server decodes it against the request it asked
  Decode('{"inputResponses":{"who":{"action":"accept","content":{"name":"Paolo"}}}}');

  LResponse := FParams.InputResponses.Find('who');
  Assert.IsNotNull(LResponse);
  Assert.IsNotNull(LResponse.Raw);
  Assert.AreEqual('{"action":"accept","content":{"name":"Paolo"}}', LResponse.Raw.ToJSON);
end;

procedure TMCPInputResponsesTest.TestDecodeElicitation;
var
  LElicit: TElicitResult;
begin
  Decode('{"inputResponses":{"who":{"action":"accept","content":{"name":"Paolo"}}}}');

  LElicit := FParams.InputResponses.ElicitationFor('who');
  Assert.IsNotNull(LElicit);
  Assert.AreEqual(TElicitAction.Accept, LElicit.Action);
  Assert.AreEqual('{"name":"Paolo"}', LElicit.Content.ToJSON);
end;

procedure TMCPInputResponsesTest.TestDecodeRoots;
var
  LRoots: TListRootsResult;
begin
  Decode('{"inputResponses":{"where":{"roots":[{"uri":"file:///src","name":"src"}]}}}');

  LRoots := FParams.InputResponses.RootsFor('where');
  Assert.IsNotNull(LRoots);
  Assert.AreEqual(1, LRoots.Roots.Count);
  Assert.AreEqual('file:///src', LRoots.Roots[0].Uri);
end;

procedure TMCPInputResponsesTest.TestDecodeIsCached;
var
  LFirst, LSecond: TElicitResult;
begin
  Decode('{"inputResponses":{"who":{"action":"decline"}}}');

  LFirst := FParams.InputResponses.ElicitationFor('who');
  LSecond := FParams.InputResponses.ElicitationFor('who');

  Assert.IsTrue(LFirst = LSecond, 'Decoding twice must not build a second result');
  Assert.AreEqual(TElicitAction.Decline, LFirst.Action);
end;

procedure TMCPInputResponsesTest.TestMissingKeyDecodesToNil;
begin
  Decode('{"inputResponses":{"who":{"action":"accept"}}}');

  Assert.IsNull(FParams.InputResponses.ElicitationFor('absent'));
  Assert.IsNull(FParams.InputResponses.SamplingFor('absent'));
  Assert.IsNull(FParams.InputResponses.RootsFor('absent'));
end;

procedure TMCPInputResponsesTest.TestFind;
begin
  Decode('{"inputResponses":{"who":{"action":"cancel"}}}');

  Assert.IsNotNull(FParams.InputResponses.Find('who'));
  Assert.IsNull(FParams.InputResponses.Find('absent'));
end;

procedure TMCPInputResponsesTest.TestRetryCarriesRequestState;
begin
  // The client retries the original request with the state the server signed
  Decode('{"name":"my_tool","requestState":"signed-state",' +
         '"inputResponses":{"who":{"action":"accept"}}}');

  Assert.AreEqual('my_tool', FParams.Name);
  Assert.AreEqual('signed-state', FParams.RequestState.Value);
  Assert.AreEqual(1, FParams.InputResponses.Count);
end;

{ TMrtrTools }

function TMrtrTools.Who: TInputRequiredResult;
var
  LSchema: TMCPElicitationSchema;
begin
  Result := TInputRequiredResult.Create;
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddString('name', 'Your name', True);
    Result.InputRequests.AddElicitation('who', TMCPElicitRequest.Form('Who are you?', LSchema));
  finally
    LSchema.Free;
  end;
  Result.RequestState := 'signed';
end;

function TMrtrTools.Plain: string;
begin
  Result := 'ok';
end;

{ TMCPToolCallResultUnionTest }

procedure TMCPToolCallResultUnionTest.Setup;
begin
  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Tools.RegisterClass(TMrtrTools);

  FGarbage := TGarbageCollector.Create;
  FContext := TJRPCContext.Create;
  FContext.AddContent(TObject(FGarbage));
  FContext.AddContent(FServer.GetConfiguration<TMCPConfig>);

  FApi := TMCPToolsApi.Create;
  FContext.Inject(FApi);
end;

procedure TMCPToolCallResultUnionTest.TearDown;
begin
  FApi.Free;
  FContext.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

function TMCPToolCallResultUnionTest.CallTool(const AName: string): TBaseResult;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := AName;
    Result := FApi.CallTool(LParams);
  finally
    LParams.Free;
  end;
end;

procedure TMCPToolCallResultUnionTest.TestOrdinaryToolReturnsCallToolResult;
var
  LResult: TBaseResult;
begin
  LResult := CallTool('plain');
  try
    Assert.IsTrue(LResult is TCallToolResult);
    Assert.AreEqual(TResultType.Complete, LResult.ResultType);
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolCallResultUnionTest.TestToolCanAnswerInputRequired;
var
  LResult: TBaseResult;
begin
  // tools/call is anyOf[InputRequiredResult|CallToolResult]; a tool that needs
  // more from the client answers with the other arm
  LResult := CallTool('who');
  try
    Assert.IsTrue(LResult is TInputRequiredResult);
    Assert.AreEqual(TResultType.InputRequired, LResult.ResultType);
    Assert.AreEqual('signed', TInputRequiredResult(LResult).RequestState.Value);
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolCallResultUnionTest.TestInputRequiredSerializesByItsRuntimeClass;
var
  LResult: TBaseResult;
  LJson: TJSONObject;
begin
  // The declared return type is TBaseResult; the fields of the actual class
  // still have to reach the wire
  LResult := CallTool('who');
  try
    LJson := TNeon.ObjectToJSON(LResult, MCPNeonConfig) as TJSONObject;
    try
      Assert.AreEqual('input_required', LJson.GetValue<string>('resultType'));
      Assert.IsNotNull(LJson.GetValue('inputRequests'));
      Assert.AreEqual('signed', LJson.GetValue<string>('requestState'));
      Assert.IsNull(LJson.GetValue('content'), 'It is not a CallToolResult');
    finally
      LJson.Free;
    end;
  finally
    LResult.Free;
  end;
end;

{ TMCPSamplingMessageTest }

function TMCPSamplingMessageTest.ResultFrom(const AJson: string): TCreateMessageResult;
var
  LTree: TJSONValue;
begin
  Result := TCreateMessageResult.Create;
  try
    LTree := TJSONObject.ParseJSONValue(AJson);
    Assert.IsTrue(LTree is TJSONObject, 'the fixture own JSON must parse: ' + AJson);
    try
      TNeon.JSONToObject(Result, LTree as TJSONObject, MCPNeonConfig);
    finally
      // The message takes a clone of the content, so the tree it was read from
      // is the fixture's to free
      LTree.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPSamplingMessageTest.TestCreateLeavesMetaAssigned;
var
  LMsg: TSamplingMessage;
begin
  // The constructor used not to call inherited, which left this nil: harmless
  // on the way in, and an access violation for anyone building a message to
  // send
  LMsg := TSamplingMessage.Create;
  try
    Assert.IsNotNull(LMsg.Meta);
    LMsg.Meta.AddPair('k', 'v');
    Assert.AreEqual(1, LMsg.Meta.Count, 'and it is usable, not merely non-nil');
  finally
    LMsg.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestTagsAreAssignedToo;
var
  LMsg: TSamplingMessage;
begin
  LMsg := TSamplingMessage.Create;
  try
    Assert.IsNotNull(LMsg.Tags);
  finally
    LMsg.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestSingleBlockIsOneBlock;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"}}');
  try
    Assert.AreEqual(1, LRes.ContentCount);
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestArrayOfBlocksIsCounted;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m","content":' +
    '[{"type":"text","text":"a"},{"type":"text","text":"b"},{"type":"text","text":"c"}]}');
  try
    Assert.AreEqual(3, LRes.ContentCount);
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestNoContentIsNoBlocks;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m"}');
  try
    Assert.AreEqual(0, LRes.ContentCount);
    Assert.AreEqual('', LRes.ContentTypeAt(0));
    Assert.IsNull(LRes.AsText(0));
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestSingleBlockIsReadableAsBlockZero;
var
  LRes: TCreateMessageResult;
begin
  // A caller that does not care whether the server wrapped its content in an
  // array should not have to ask
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"}}');
  try
    Assert.AreEqual(MCP_CONTENT_TEXT, LRes.ContentTypeAt);
    Assert.IsNotNull(LRes.AsText);
    Assert.AreEqual('hi', LRes.AsText.Text);
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestEveryBlockTypeIsReadable;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m","content":[' +
    '{"type":"text","text":"hi"},' +
    '{"type":"image","data":"AA==","mimeType":"image/png"},' +
    '{"type":"audio","data":"BB==","mimeType":"audio/wav"},' +
    '{"type":"tool_use","id":"t1","name":"search","input":{"q":"x"}},' +
    '{"type":"tool_result","toolUseId":"t1","isError":false}]}');
  try
    Assert.AreEqual(5, LRes.ContentCount);

    Assert.AreEqual('hi', LRes.AsText(0).Text);

    Assert.AreEqual('AA==', LRes.AsImage(1).Data);
    Assert.AreEqual('image/png', LRes.AsImage(1).MimeType);

    Assert.AreEqual('BB==', LRes.AsAudio(2).Data);

    Assert.AreEqual('search', LRes.AsToolUse(3).Name);
    Assert.IsNotNull(LRes.AsToolUse(3).Input, 'the tool input is raw JSON and survives');

    Assert.AreEqual('t1', LRes.AsToolResult(4).ToolUseId);
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestTheWrongTypeAnswersNil;
var
  LRes: TCreateMessageResult;
begin
  // The discriminator decides, so asking for the wrong class is answered rather
  // than guessed at - and nothing is materialized for it
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"}}');
  try
    Assert.IsNull(LRes.AsImage(0));
    Assert.IsNull(LRes.AsToolUse(0));
    Assert.IsNotNull(LRes.AsText(0), 'and the right one still works afterwards');
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestAskingTwiceAnswersTheSameInstance;
var
  LRes: TCreateMessageResult;
begin
  // The message owns what it materializes, so a caller in a loop neither leaks
  // nor has to free anything
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"}}');
  try
    Assert.AreSame(LRes.AsText(0), LRes.AsText(0));
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestABlockThatIsNotThereAnswersNil;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"}}');
  try
    Assert.IsNull(LRes.AsText(1), 'a single block has no index one');
    Assert.IsNull(LRes.AsText(-1));
    Assert.AreEqual('', LRes.ContentTypeAt(9));
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestModelAndStopReasonSurvive;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"claude","stopReason":"endTurn",' +
    '"content":{"type":"text","text":"hi"}}');
  try
    Assert.AreEqual('claude', LRes.Model);
    Assert.AreEqual('endTurn', LRes.StopReason.Value);
    Assert.AreEqual(Ord(TRole.Assistant), Ord(LRes.Role));
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestMetaSurvivesTheRoundTrip;
var
  LRes: TCreateMessageResult;
begin
  LRes := ResultFrom('{"role":"assistant","model":"m","content":{"type":"text","text":"hi"},' +
    '"_meta":{"k":"v"}}');
  try
    Assert.IsNotNull(LRes.Meta);
    Assert.AreEqual('v', LRes.Meta.GetValue<string>('k'));
  finally
    LRes.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestAddContentMakesASingleBlock;
var
  LMsg: TSamplingMessage;
begin
  LMsg := TSamplingMessage.Create;
  try
    LMsg.AddContent(TTextContent.CreateWithText('hi'));

    Assert.AreEqual(1, LMsg.ContentCount);
    Assert.IsTrue(LMsg.Content is TJSONObject, 'one block is an object, not an array of one');
    Assert.AreEqual('hi', LMsg.AsText.Text);
  finally
    LMsg.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestASecondBlockPromotesToAnArray;
var
  LMsg: TSamplingMessage;
begin
  LMsg := TSamplingMessage.Create;
  try
    LMsg.AddText('a');
    LMsg.AddText('b');

    Assert.AreEqual(2, LMsg.ContentCount);
    Assert.IsTrue(LMsg.Content is TJSONArray);
    Assert.AreEqual('a', LMsg.AsText(0).Text);
    Assert.AreEqual('b', LMsg.AsText(1).Text);
  finally
    LMsg.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestPromotionKeepsTheFirstBlockAtIndexZero;
var
  LMsg: TSamplingMessage;
  LFirst: TTextContent;
begin
  // Reading a block and then appending another must not move what was read: the
  // materialized instance is keyed by index, and a shifted index would hand the
  // caller the wrong block
  LMsg := TSamplingMessage.Create;
  try
    LMsg.AddText('a');
    LFirst := LMsg.AsText(0);

    LMsg.AddText('b');

    Assert.AreSame(LFirst, LMsg.AsText(0));
    Assert.AreEqual('a', LMsg.AsText(0).Text);
  finally
    LMsg.Free;
  end;
end;

procedure TMCPSamplingMessageTest.TestAddTextIsATextBlock;
var
  LMsg: TSamplingMessage;
begin
  LMsg := TSamplingMessage.Create;
  try
    LMsg.AddText('hello');

    Assert.AreEqual(MCP_CONTENT_TEXT, LMsg.ContentTypeAt,
      'the block declares the type its class set');
  finally
    LMsg.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPInputRequestsTest);
  TDUnitX.RegisterTestFixture(TMCPSamplingMessageTest);
  TDUnitX.RegisterTestFixture(TMCPInputResponsesTest);
  TDUnitX.RegisterTestFixture(TMCPToolCallResultUnionTest);

end.
