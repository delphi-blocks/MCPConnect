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
///   MRTR server requirement 7: a server MUST NOT ask the client for something
///   it never declared it could give. What it must answer instead is a
///   MissingRequiredClientCapability (-32021) naming what was missing, which
///   this fixture drives through the transport - capabilities in, status and
///   error payload out.
/// </summary>
unit MCPConnect.Tests.Transport.MrtrCapabilities;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentMrtrWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string; const AEventId: string = '');
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  /// <summary>What a call answered, as far as these tests care.</summary>
  TMrtrAnswer = record
    Code: Integer;
    Content: string;

    function ErrorCode: Integer;

    /// <summary>Whether the error data names ACapability as required.</summary>
    function Requires(const ACapability: string): Boolean;
  end;

  [TestFixture]
  TMrtrCapabilitiesTest = class(TObject)
  private
    FServer: TMCPServer;

    procedure ConfigureServer(AMeta: TMCPValidationLevel = TMCPValidationLevel.Strict);

    /// <summary>
    ///   Calls ATool with the capabilities ACapabilities declares, written as
    ///   the client would write them.
    /// </summary>
    function Call(const ATool, ACapabilities: string): TMrtrAnswer;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAnAskIsAnsweredWhenTheClientCanGiveIt();
    [Test]
    procedure TestAnAskIsRefusedWhenTheClientDeclaredNothing();
    [Test]
    procedure TestTheRefusalNamesWhatWasMissing();
    [Test]
    procedure TestSamplingNeedsSampling();
    [Test]
    procedure TestRootsNeedsRoots();

    [Test]
    procedure TestAModeTheClientDidNotDeclareIsRefused();
    [Test]
    procedure TestADeclarationWithoutModesTakesAnyMode();

    [Test]
    procedure TestASubCapabilityOfSamplingIsChecked();

    [Test]
    procedure TestAnOrdinaryResultIsNeverRefused();
    [Test]
    procedure TestNothingIsCheckedWhenNobodyLooked();
  end;

implementation

uses
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Elicitation,
  MCPConnect.MCP.Types.Errors,
  MCPConnect.MCP.Types.Mrtr;

type
  TMrtrTools = class(TObject)
  public
    [McpTool('ask_form', 'Asks the user to fill in a form')]
    function AskForm: TInputRequiredResult;

    [McpTool('ask_url', 'Asks the user to visit a url')]
    function AskUrl: TInputRequiredResult;

    [McpTool('ask_model', 'Asks the client to sample a model')]
    function AskModel: TInputRequiredResult;

    [McpTool('ask_model_in_context', 'Asks for a sample that includes context')]
    function AskModelInContext: TInputRequiredResult;

    [McpTool('ask_roots', 'Asks the client for its roots')]
    function AskRoots: TInputRequiredResult;

    [McpTool('plain', 'Answers without asking anything')]
    function Plain: string;
  end;

function TMrtrTools.AskForm: TInputRequiredResult;
var
  LSchema: TMCPElicitationSchema;
begin
  Result := TInputRequiredResult.Create;
  try
    LSchema := TMCPElicitationSchema.Create;
    try
      LSchema.AddString('name', 'Your name', True);
      Result.InputRequests.AddElicitation('who',
        TMCPElicitRequest.Form('Who are you?', LSchema));
    finally
      LSchema.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMrtrTools.AskUrl: TInputRequiredResult;
begin
  Result := TInputRequiredResult.Create;
  try
    Result.InputRequests.AddElicitation('who',
      TMCPElicitRequest.Url('Sign in first', 'https://login.example.com'));
  except
    Result.Free;
    raise;
  end;
end;

function TMrtrTools.AskModel: TInputRequiredResult;
var
  LParams: TCreateMessageRequestParams;
begin
  Result := TInputRequiredResult.Create;
  try
    LParams := TCreateMessageRequestParams.Create;
    LParams.MaxTokens := 100;
    Result.InputRequests.AddSampling('draft', LParams);
  except
    Result.Free;
    raise;
  end;
end;

function TMrtrTools.AskModelInContext: TInputRequiredResult;
var
  LParams: TCreateMessageRequestParams;
begin
  Result := TInputRequiredResult.Create;
  try
    LParams := TCreateMessageRequestParams.Create;
    LParams.MaxTokens := 100;
    LParams.IncludeContext := TIncludeContext.ThisServer;
    Result.InputRequests.AddSampling('draft', LParams);
  except
    Result.Free;
    raise;
  end;
end;

function TMrtrTools.AskRoots: TInputRequiredResult;
begin
  Result := TInputRequiredResult.Create;
  try
    Result.InputRequests.AddRoots('where');
  except
    Result.Free;
    raise;
  end;
end;

function TMrtrTools.Plain: string;
begin
  Result := 'nothing needed';
end;

{ TSilentMrtrWriter }

procedure TSilentMrtrWriter.Write(const AValue: string; const AEventId: string);
begin
  // Nothing streams in these tests.
end;

function TSilentMrtrWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentMrtrWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TMrtrAnswer }

function TMrtrAnswer.ErrorCode: Integer;
var
  LValue, LError: TJSONValue;
begin
  Result := 0;

  LValue := TJSONObject.ParseJSONValue(Content);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LError := TJSONObject(LValue).GetValue('error');
    if LError is TJSONObject then
      Result := TJSONObject(LError).GetValue<Integer>('code', 0);
  finally
    LValue.Free;
  end;
end;

function TMrtrAnswer.Requires(const ACapability: string): Boolean;
var
  LValue, LError, LData: TJSONValue;
  LRequired: TJSONValue;
begin
  Result := False;

  LValue := TJSONObject.ParseJSONValue(Content);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LError := TJSONObject(LValue).GetValue('error');
    if not (LError is TJSONObject) then
      Exit;

    LData := TJSONObject(LError).GetValue('data');
    if not (LData is TJSONObject) then
      Exit;

    LRequired := TJSONObject(LData).GetValue('requiredCapabilities');
    if LRequired is TJSONObject then
      Result := Assigned(TJSONObject(LRequired).GetValue(ACapability));
  finally
    LValue.Free;
  end;
end;

{ TMrtrCapabilitiesTest }

procedure TMrtrCapabilitiesTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TMrtrCapabilitiesTest.ConfigureServer(AMeta: TMCPValidationLevel);
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('mrtr-capabilities-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
      .SetMetaValidation(AMeta)
    .BackToMCP
    .Tools
      .RegisterClass(TMrtrTools)
    .BackToMCP
  .ApplyConfig;
end;

function TMrtrCapabilitiesTest.Call(const ATool, ACapabilities: string): TMrtrAnswer;
var
  LHandler: TMCPTransportHandler;
  LAnswer: TMrtrAnswer;
  LBody: string;
begin
  LAnswer := Default(TMrtrAnswer);

  LBody := Format(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":' +
    '{"name":"%s","arguments":{},"_meta":{' +
    '"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
    '"io.modelcontextprotocol/clientCapabilities":%s}}}', [ATool, ACapabilities]);

  LHandler := TMCPTransportHandler.Create(FServer, TSilentMrtrWriter.Create);
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
        LAnswer.Code := AResponse.Code;
        LAnswer.Content := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LAnswer;
end;

procedure TMrtrCapabilitiesTest.TestAnAskIsAnsweredWhenTheClientCanGiveIt;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  LAnswer := Call('ask_form', '{"elicitation":{"form":{}}}');

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, 'input_required');
end;

procedure TMrtrCapabilitiesTest.TestAnAskIsRefusedWhenTheClientDeclaredNothing;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // The case the requirement is about: the tool asks, and the client never
  // said it could answer
  LAnswer := Call('ask_form', '{}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(MCP_MISSING_REQUIRED_CLIENT_CAPABILITY, LAnswer.ErrorCode);
end;

procedure TMrtrCapabilitiesTest.TestTheRefusalNamesWhatWasMissing;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // The payload is the useful half: a client that reads it knows what to
  // declare, or that it should stop calling this tool
  LAnswer := Call('ask_form', '{}');

  Assert.IsTrue(LAnswer.Requires('elicitation'),
    'the error must name the capability: ' + LAnswer.Content);
end;

procedure TMrtrCapabilitiesTest.TestSamplingNeedsSampling;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // Declaring one capability says nothing about another
  LAnswer := Call('ask_model', '{"elicitation":{}}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.IsTrue(LAnswer.Requires('sampling'), LAnswer.Content);

  Assert.AreEqual(200, Call('ask_model', '{"sampling":{}}').Code);
end;

procedure TMrtrCapabilitiesTest.TestRootsNeedsRoots;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  LAnswer := Call('ask_roots', '{"sampling":{}}');
  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.IsTrue(LAnswer.Requires('roots'), LAnswer.Content);

  Assert.AreEqual(200, Call('ask_roots', '{"roots":{}}').Code);
end;

procedure TMrtrCapabilitiesTest.TestAModeTheClientDidNotDeclareIsRefused;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // A client that named its modes was specific, and url is not among them
  LAnswer := Call('ask_url', '{"elicitation":{"form":{}}}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.IsTrue(LAnswer.Requires('elicitation'), LAnswer.Content);

  // ...while the mode it did name is answered
  Assert.AreEqual(200, Call('ask_form', '{"elicitation":{"form":{}}}').Code);
end;

procedure TMrtrCapabilitiesTest.TestADeclarationWithoutModesTakesAnyMode;
begin
  ConfigureServer();

  // "elicitation":{} declares elicitation whole. Reading it as "neither form
  // nor url" would refuse every elicitation there is, which is not what a
  // client saying that means.
  Assert.AreEqual(200, Call('ask_form', '{"elicitation":{}}').Code);
  Assert.AreEqual(200, Call('ask_url', '{"elicitation":{}}').Code);
end;

procedure TMrtrCapabilitiesTest.TestASubCapabilityOfSamplingIsChecked;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // includeContext is something the server asks the *client* to do, and this
  // one never said it could
  LAnswer := Call('ask_model_in_context', '{"sampling":{}}');

  Assert.AreEqual(400, LAnswer.Code, LAnswer.Content);
  Assert.AreEqual(MCP_MISSING_REQUIRED_CLIENT_CAPABILITY, LAnswer.ErrorCode);

  Assert.AreEqual(200, Call('ask_model_in_context', '{"sampling":{"context":{}}}').Code);
end;

procedure TMrtrCapabilitiesTest.TestAnOrdinaryResultIsNeverRefused;
var
  LAnswer: TMrtrAnswer;
begin
  ConfigureServer();

  // A tool that asks for nothing needs nothing declared
  LAnswer := Call('plain', '{}');

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, 'nothing needed');
end;

procedure TMrtrCapabilitiesTest.TestNothingIsCheckedWhenNobodyLooked;
var
  LAnswer: TMrtrAnswer;
begin
  // With the _meta check off nothing parsed the capabilities, and an absent
  // declaration is not a declaration of absence: the server has no grounds to
  // refuse, so it does not.
  ConfigureServer(TMCPValidationLevel.Off);

  LAnswer := Call('ask_form', '{}');

  Assert.AreEqual(200, LAnswer.Code, LAnswer.Content);
  Assert.Contains(LAnswer.Content, 'input_required');
end;

initialization
  TDUnitX.RegisterTestFixture(TMrtrCapabilitiesTest);

end.
