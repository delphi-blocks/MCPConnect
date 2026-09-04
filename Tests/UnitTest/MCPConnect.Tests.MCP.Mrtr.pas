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

initialization
  TDUnitX.RegisterTestFixture(TMCPInputRequestsTest);
  TDUnitX.RegisterTestFixture(TMCPInputResponsesTest);

end.
