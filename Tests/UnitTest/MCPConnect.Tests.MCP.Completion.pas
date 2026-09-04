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
unit MCPConnect.Tests.MCP.Completion;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Completion,
  MCPConnect.MCP.Server.Api;

type
  /// <summary>
  ///   A prompt class that also carries its own completion providers, one per
  ///   supported method arity.
  /// </summary>
  [McpScope('demo')]
  TCompletionPrompts = class
  public
    [McpPrompt('code_review', 'Code review', 'Reviews code')]
    function CodeReview([McpArgument('language', 'Language')] const language: string): TPromptMessages;

    [McpTemplate('file', 'file:///{path}')]
    function GetFile([McpParam('path', 'The path')] const path: string): string;

    /// <summary>Arity 1: only the value typed so far.</summary>
    [McpComplete('code_review', 'language')]
    function CompleteLanguage(const AValue: string): TArray<string>;

    /// <summary>Arity 2: the value plus the already-resolved arguments.</summary>
    [McpComplete('code_review', 'framework')]
    function CompleteFramework(const AValue: string; AContext: TMCPCompletionContext): TArray<string>;

    /// <summary>Arity 0, and a TStrings return.</summary>
    [McpCompleteTemplate('file:///{path}', 'path')]
    function CompletePath: TStringList;

    /// <summary>Registered programmatically, so it carries no attribute.</summary>
    function CompleteManyValues(const AValue: string): TArray<string>;
  end;

  [TestFixture]
  TMCPCompletionReferenceTest = class(TObject)
  public
    [Test]
    procedure TestPromptReference_KindAndTarget;
    [Test]
    procedure TestResourceReference_KindAndTarget;
    [Test]
    procedure TestUnknownReference_KindAndTarget;
    [Test]
    procedure TestReferenceSerializesBackToTheShapeItCameFrom;
  end;

  [TestFixture]
  TMCPCompleteResultTest = class(TObject)
  public
    [Test]
    procedure TestSetValues_BelowCap_ReportsExactTotal;
    [Test]
    procedure TestSetValues_AboveCap_TruncatesAndFlagsHasMore;
    [Test]
    procedure TestResultTypeIsComplete;
  end;

  [TestFixture]
  TMCPCompletionsConfigTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegisterClass_FindsBothAttributes;
    [Test]
    procedure TestRegisterClass_ScopesPromptNamesButNotUris;
    [Test]
    procedure TestRegisterCompletion_WithoutAttribute;
    [Test]
    procedure TestRegisterDuplicate_Raises;
    [Test]
    procedure TestUnregisterCompletion;
    [Test]
    procedure TestUnregisterCompletion_Unknown_Raises;
    [Test]
    procedure TestUnregisterClass_RemovesEveryProvider;
    [Test]
    procedure TestClearAll;
    [Test]
    procedure TestHasProviders;
    [Test]
    procedure TestFind_IgnoresDisabledProvider;
  end;

  [TestFixture]
  TMCPCompletionApiTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FApi: TMCPCompletionApi;
    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;

    function BuildParams(const ARefType, ATarget, AArgument, AValue: string;
      const AContextJson: string = ''): TCompleteRequestParams;
    function Complete(const ARefType, ATarget, AArgument, AValue: string;
      const AContextJson: string = ''): TArray<string>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestComplete_FiltersOnTypedValue;
    [Test]
    procedure TestComplete_UsesContextArguments;
    [Test]
    procedure TestComplete_TemplateProviderReturningTStrings;
    [Test]
    procedure TestComplete_UnknownPrompt_RaisesInvalidParams;
    [Test]
    procedure TestComplete_UnknownTemplate_RaisesInvalidParams;
    [Test]
    procedure TestComplete_UnknownRefType_RaisesInvalidParams;
    [Test]
    procedure TestComplete_KnownRefWithoutProvider_ReturnsEmpty;
    [Test]
    procedure TestComplete_CapsAtHundredValues;
  end;

  [TestFixture]
  TMCPCompletionCapabilityTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;

    function DiscoverCapabilities: TJSONObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCapabilityAbsentWithoutProviders;
    [Test]
    procedure TestCapabilityPresentWithProviders;
    [Test]
    procedure TestSetCapabilitiesHonoursCompletions;
  end;

implementation

uses
  Neon.Core.Persistence.JSON;

{ TCompletionPrompts }

function TCompletionPrompts.CodeReview(const language: string): TPromptMessages;
begin
  Result := TPromptMessages.Create;
end;

function TCompletionPrompts.GetFile(const path: string): string;
begin
  Result := 'contents of ' + path;
end;

function TCompletionPrompts.CompleteLanguage(const AValue: string): TArray<string>;
var
  LAll: TArray<string>;
  LItem: string;
begin
  LAll := ['python', 'pytorch', 'pascal', 'perl'];
  Result := [];
  for LItem in LAll do
    if LItem.StartsWith(AValue) then
      Result := Result + [LItem];
end;

function TCompletionPrompts.CompleteFramework(const AValue: string;
  AContext: TMCPCompletionContext): TArray<string>;
begin
  if AContext.ArgumentValue('language') = 'python' then
    Result := ['flask', 'django']
  else
    Result := ['none'];
end;

function TCompletionPrompts.CompletePath: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('alpha');
  Result.Add('beta');
end;

function TCompletionPrompts.CompleteManyValues(const AValue: string): TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, 250);
  for I := 0 to 249 do
    Result[I] := 'v' + I.ToString;
end;

{ TMCPCompletionReferenceTest }

procedure TMCPCompletionReferenceTest.TestPromptReference_KindAndTarget;
var
  LRef: TMCPCompletionReference;
begin
  LRef := TMCPCompletionReference.Create;
  try
    LRef.&Type := MCP_REF_PROMPT;
    LRef.Name := 'code_review';

    Assert.AreEqual(TMCPCompletionRefKind.Prompt, LRef.Kind);
    Assert.AreEqual('code_review', LRef.Target);
  finally
    LRef.Free;
  end;
end;

procedure TMCPCompletionReferenceTest.TestResourceReference_KindAndTarget;
var
  LRef: TMCPCompletionReference;
begin
  LRef := TMCPCompletionReference.Create;
  try
    LRef.&Type := MCP_REF_RESOURCE;
    LRef.Uri := 'file:///{path}';

    Assert.AreEqual(TMCPCompletionRefKind.ResourceTemplate, LRef.Kind);
    Assert.AreEqual('file:///{path}', LRef.Target);
  finally
    LRef.Free;
  end;
end;

procedure TMCPCompletionReferenceTest.TestUnknownReference_KindAndTarget;
var
  LRef: TMCPCompletionReference;
begin
  LRef := TMCPCompletionReference.Create;
  try
    LRef.&Type := 'ref/bogus';
    LRef.Name := 'code_review';

    Assert.AreEqual(TMCPCompletionRefKind.Unknown, LRef.Kind);
    Assert.AreEqual('', LRef.Target, 'An unrecognised type must not resolve to a target');
  finally
    LRef.Free;
  end;
end;

procedure TMCPCompletionReferenceTest.TestReferenceSerializesBackToTheShapeItCameFrom;
var
  LParams: TCompleteRequestParams;
  LJson: string;
begin
  // One class models the PromptReference | ResourceTemplateReference union:
  // the fields belonging to the other shape must drop out as empty
  LParams := TCompleteRequestParams.Create;
  try
    TNeon.JSONToObject(LParams,
      '{"ref":{"type":"ref/resource","uri":"file:///{path}"},' +
      '"argument":{"name":"path","value":"src/"}}', MCPNeonConfig);

    Assert.AreEqual(TMCPCompletionRefKind.ResourceTemplate, LParams.Ref.Kind);
    Assert.AreEqual('path', LParams.Argument.Name);
    Assert.AreEqual('src/', LParams.Argument.Value);

    LJson := TNeon.ObjectToJSONString(LParams.Ref, MCPNeonConfig);
    Assert.IsTrue(LJson.Contains('"uri":"file:///{path}"'), LJson);
    Assert.IsFalse(LJson.Contains('"name"'), 'The prompt-side fields must not be emitted: ' + LJson);
  finally
    LParams.Free;
  end;
end;

{ TMCPCompleteResultTest }

procedure TMCPCompleteResultTest.TestSetValues_BelowCap_ReportsExactTotal;
var
  LResult: TCompleteResult;
begin
  LResult := TCompleteResult.Create;
  try
    LResult.SetValues(['a', 'b', 'c']);

    Assert.AreEqual(3, Length(LResult.Completion.Values));
    Assert.AreEqual(3, LResult.Completion.Total.Value);
    Assert.IsFalse(LResult.Completion.HasMore.Value);
  finally
    LResult.Free;
  end;
end;

procedure TMCPCompleteResultTest.TestSetValues_AboveCap_TruncatesAndFlagsHasMore;
var
  LResult: TCompleteResult;
  LValues: TArray<string>;
  I: Integer;
begin
  SetLength(LValues, 250);
  for I := 0 to 249 do
    LValues[I] := 'v' + I.ToString;

  LResult := TCompleteResult.Create;
  try
    LResult.SetValues(LValues);

    Assert.AreEqual(MCP_COMPLETION_MAX_VALUES, Length(LResult.Completion.Values),
      'The schema caps values at 100 items');
    Assert.AreEqual(250, LResult.Completion.Total.Value, 'total reports every match, not just the ones sent');
    Assert.IsTrue(LResult.Completion.HasMore.Value);
    Assert.AreEqual('v0', LResult.Completion.Values[0]);
    Assert.AreEqual('v99', LResult.Completion.Values[MCP_COMPLETION_MAX_VALUES - 1]);
  finally
    LResult.Free;
  end;
end;

procedure TMCPCompleteResultTest.TestResultTypeIsComplete;
var
  LResult: TCompleteResult;
  LJson: string;
begin
  LResult := TCompleteResult.Create;
  try
    LResult.SetValues(['a']);
    Assert.AreEqual(TResultType.Complete, LResult.ResultType);

    LJson := TNeon.ObjectToJSONString(LResult, MCPNeonConfig);
    Assert.IsTrue(LJson.Contains('"resultType":"complete"'), LJson);
    Assert.IsFalse(LJson.Contains('"ttlMs"'), 'CompleteResult is a Result, not a CacheableResult: ' + LJson);
  finally
    LResult.Free;
  end;
end;

{ TMCPCompletionsConfigTest }

procedure TMCPCompletionsConfigTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPCompletionsConfigTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPCompletionsConfigTest.TestRegisterClass_FindsBothAttributes;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);

  Assert.AreEqual(3, FConfig.Completions.Registry.Count,
    'Two [McpComplete] and one [McpCompleteTemplate] method should be registered');
end;

procedure TMCPCompletionsConfigTest.TestRegisterClass_ScopesPromptNamesButNotUris;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);

  // The prompt reference names a prompt, which carries the [McpScope] prefix
  Assert.IsNotNull(FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'demo_code_review', 'language'),
    'Prompt completions must be keyed by the scoped prompt name');
  Assert.IsNull(FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'code_review', 'language'),
    'The unscoped name must not resolve');

  // A resource reference is a uri, which is never scoped
  Assert.IsNotNull(FConfig.Completions.Find(TMCPCompletionRefKind.ResourceTemplate, 'file:///{path}', 'path'),
    'Template completions must be keyed by the verbatim uri');
end;

procedure TMCPCompletionsConfigTest.TestRegisterCompletion_WithoutAttribute;
var
  LProvider: TMCPCompletionProvider;
begin
  FConfig.Completions.RegisterCompletion(TCompletionPrompts, 'CompleteManyValues', 'other_prompt', 'arg');

  LProvider := FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'other_prompt', 'arg');
  Assert.IsNotNull(LProvider);
  Assert.AreEqual(TClass(TCompletionPrompts), LProvider.ProviderClass);
  Assert.AreEqual('CompleteManyValues', LProvider.Method.Name);
end;

procedure TMCPCompletionsConfigTest.TestRegisterDuplicate_Raises;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);

  Assert.WillRaise(
    procedure
    begin
      FConfig.Completions.RegisterCompletion(TCompletionPrompts, 'CompleteLanguage',
        'demo_code_review', 'language');
    end,
    EMCPException);
end;

procedure TMCPCompletionsConfigTest.TestUnregisterCompletion;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);
  FConfig.Completions.UnregisterCompletion('demo_code_review', 'language');

  Assert.AreEqual(2, FConfig.Completions.Registry.Count);
  Assert.IsNull(FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'demo_code_review', 'language'));
end;

procedure TMCPCompletionsConfigTest.TestUnregisterCompletion_Unknown_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Completions.UnregisterCompletion('nope', 'arg');
    end,
    EMCPException);
end;

procedure TMCPCompletionsConfigTest.TestUnregisterClass_RemovesEveryProvider;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);
  FConfig.Completions.RegisterCompletion(TCompletionPrompts, 'CompleteManyValues', 'other_prompt', 'arg');

  FConfig.Completions.UnregisterClass(TCompletionPrompts);

  Assert.AreEqual(0, FConfig.Completions.Registry.Count,
    'Both attribute and programmatic registrations must go');
end;

procedure TMCPCompletionsConfigTest.TestClearAll;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);
  FConfig.Completions.ClearAll;

  Assert.AreEqual(0, FConfig.Completions.Registry.Count);
end;

procedure TMCPCompletionsConfigTest.TestHasProviders;
begin
  Assert.IsFalse(FConfig.Completions.HasProviders);

  FConfig.Completions.RegisterClass(TCompletionPrompts);
  Assert.IsTrue(FConfig.Completions.HasProviders);

  FConfig.Completions.ClearAll;
  Assert.IsFalse(FConfig.Completions.HasProviders);
end;

procedure TMCPCompletionsConfigTest.TestFind_IgnoresDisabledProvider;
var
  LProvider: TMCPCompletionProvider;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);

  LProvider := FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'demo_code_review', 'language');
  Assert.IsNotNull(LProvider);

  LProvider.Disabled := True;
  Assert.IsNull(FConfig.Completions.Find(TMCPCompletionRefKind.Prompt, 'demo_code_review', 'language'));
  Assert.IsTrue(FConfig.Completions.HasProviders, 'The other providers are still enabled');

  for var LPair in FConfig.Completions.Registry do
    LPair.Value.Disabled := True;

  Assert.IsFalse(FConfig.Completions.HasProviders,
    'With every provider disabled the capability must not be advertised');
end;

{ TMCPCompletionApiTest }

procedure TMCPCompletionApiTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Prompts.RegisterClass(TCompletionPrompts);
  FConfig.Resources.RegisterClass(TCompletionPrompts);
  FConfig.Completions
    .RegisterClass(TCompletionPrompts)
    .RegisterCompletion(TCompletionPrompts, 'CompleteManyValues', 'demo_code_review', 'many');

  FGarbage := TGarbageCollector.Create;
  FContext := TJRPCContext.Create;
  FContext.AddContent(TObject(FGarbage));
  FContext.AddContent(FServer.GetConfiguration<TMCPConfig>);

  FApi := TMCPCompletionApi.Create;
  FApi.MCPConfig := FServer.GetConfiguration<TMCPConfig>;
  FApi.RPCContext := FContext;
end;

procedure TMCPCompletionApiTest.TearDown;
begin
  FApi.Free;
  FContext.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

function TMCPCompletionApiTest.BuildParams(const ARefType, ATarget, AArgument,
  AValue, AContextJson: string): TCompleteRequestParams;
begin
  Result := TCompleteRequestParams.Create;
  try
    Result.Ref.&Type := ARefType;
    if ARefType = MCP_REF_RESOURCE then
      Result.Ref.Uri := ATarget
    else
      Result.Ref.Name := ATarget;

    Result.Argument.Name := AArgument;
    Result.Argument.Value := AValue;

    if AContextJson <> '' then
    begin
      Result.Context.Arguments.Free;
      Result.Context.Arguments := TJSONObject.ParseJSONValue(AContextJson) as TJSONObject;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMCPCompletionApiTest.Complete(const ARefType, ATarget, AArgument,
  AValue, AContextJson: string): TArray<string>;
var
  LParams: TCompleteRequestParams;
  LResult: TCompleteResult;
begin
  LParams := BuildParams(ARefType, ATarget, AArgument, AValue, AContextJson);
  try
    LResult := FApi.Complete(LParams);
    try
      Result := LResult.Completion.Values;
    finally
      LResult.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMCPCompletionApiTest.TestComplete_FiltersOnTypedValue;
var
  LValues: TArray<string>;
begin
  LValues := Complete(MCP_REF_PROMPT, 'demo_code_review', 'language', 'py');

  Assert.AreEqual(2, Length(LValues));
  Assert.AreEqual('python', LValues[0]);
  Assert.AreEqual('pytorch', LValues[1]);
end;

procedure TMCPCompletionApiTest.TestComplete_UsesContextArguments;
var
  LWithContext, LWithout: TArray<string>;
begin
  // The arity-2 provider reads the arguments the client already resolved
  LWithContext := Complete(MCP_REF_PROMPT, 'demo_code_review', 'framework', 'fl',
    '{"language":"python"}');
  Assert.AreEqual(2, Length(LWithContext));
  Assert.AreEqual('flask', LWithContext[0]);

  LWithout := Complete(MCP_REF_PROMPT, 'demo_code_review', 'framework', '');
  Assert.AreEqual(1, Length(LWithout));
  Assert.AreEqual('none', LWithout[0]);
end;

procedure TMCPCompletionApiTest.TestComplete_TemplateProviderReturningTStrings;
var
  LValues: TArray<string>;
begin
  // Arity 0 and a TStrings return, on a resource-template reference
  LValues := Complete(MCP_REF_RESOURCE, 'file:///{path}', 'path', 's');

  Assert.AreEqual(2, Length(LValues));
  Assert.AreEqual('alpha', LValues[0]);
  Assert.AreEqual('beta', LValues[1]);
end;

procedure TMCPCompletionApiTest.TestComplete_UnknownPrompt_RaisesInvalidParams;
begin
  Assert.WillRaise(
    procedure
    begin
      Complete(MCP_REF_PROMPT, 'no_such_prompt', 'language', 'p');
    end,
    EJRPCInvalidParamsError, 'The spec asks for -32602 on an invalid prompt name');
end;

procedure TMCPCompletionApiTest.TestComplete_UnknownTemplate_RaisesInvalidParams;
begin
  Assert.WillRaise(
    procedure
    begin
      Complete(MCP_REF_RESOURCE, 'file:///no/such/{thing}', 'thing', 'x');
    end,
    EJRPCInvalidParamsError);
end;

procedure TMCPCompletionApiTest.TestComplete_UnknownRefType_RaisesInvalidParams;
begin
  Assert.WillRaise(
    procedure
    begin
      Complete('ref/bogus', 'demo_code_review', 'language', 'p');
    end,
    EJRPCInvalidParamsError);
end;

procedure TMCPCompletionApiTest.TestComplete_KnownRefWithoutProvider_ReturnsEmpty;
var
  LValues: TArray<string>;
begin
  // A known prompt whose argument simply has nothing to suggest is not an error
  LValues := Complete(MCP_REF_PROMPT, 'demo_code_review', 'unprovided', 'x');

  Assert.AreEqual(0, Length(LValues));
end;

procedure TMCPCompletionApiTest.TestComplete_CapsAtHundredValues;
var
  LParams: TCompleteRequestParams;
  LResult: TCompleteResult;
begin
  LParams := BuildParams(MCP_REF_PROMPT, 'demo_code_review', 'many', '');
  try
    LResult := FApi.Complete(LParams);
    try
      Assert.AreEqual(MCP_COMPLETION_MAX_VALUES, Length(LResult.Completion.Values));
      Assert.AreEqual(250, LResult.Completion.Total.Value);
      Assert.IsTrue(LResult.Completion.HasMore.Value);
    finally
      LResult.Free;
    end;
  finally
    LParams.Free;
  end;
end;

{ TMCPCompletionCapabilityTest }

procedure TMCPCompletionCapabilityTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPCompletionCapabilityTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

function TMCPCompletionCapabilityTest.DiscoverCapabilities: TJSONObject;
var
  LApi: TMCPServerApi;
  LResult: TDiscoverResult;
begin
  LApi := TMCPServerApi.Create;
  try
    LApi.MCPConfig := FServer.GetConfiguration<TMCPConfig>;

    LResult := LApi.Discover(nil);
    try
      Result := TNeon.ObjectToJSON(LResult.Capabilities, MCPNeonConfig) as TJSONObject;
    finally
      LResult.Free;
    end;
  finally
    LApi.Free;
  end;
end;

procedure TMCPCompletionCapabilityTest.TestCapabilityAbsentWithoutProviders;
var
  LCapabilities: TJSONObject;
begin
  LCapabilities := DiscoverCapabilities;
  try
    Assert.IsNull(LCapabilities.GetValue('completions'),
      'A server with no providers must not advertise completions');
  finally
    LCapabilities.Free;
  end;
end;

procedure TMCPCompletionCapabilityTest.TestCapabilityPresentWithProviders;
var
  LCapabilities: TJSONObject;
begin
  FConfig.Completions.RegisterClass(TCompletionPrompts);

  LCapabilities := DiscoverCapabilities;
  try
    // The capability is declared by an *empty* object, which is exactly the
    // case IncludeIf.NotEmpty used to drop
    Assert.IsNotNull(LCapabilities.GetValue('completions'));
    Assert.AreEqual('{}', LCapabilities.GetValue('completions').ToJSON);
  finally
    LCapabilities.Free;
  end;
end;

procedure TMCPCompletionCapabilityTest.TestSetCapabilitiesHonoursCompletions;
var
  LJson: TJSONObject;
begin
  FConfig.Server.SetCapabilities([TMCPCapability.Tools, TMCPCapability.Completions]);

  LJson := TNeon.ObjectToJSON(FConfig.Server.Capabilities, MCPNeonConfig) as TJSONObject;
  try
    Assert.IsNotNull(LJson.GetValue('completions'),
      'SetCapabilities must honour TMCPCapability.Completions');
  finally
    LJson.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPCompletionReferenceTest);
  TDUnitX.RegisterTestFixture(TMCPCompleteResultTest);
  TDUnitX.RegisterTestFixture(TMCPCompletionsConfigTest);
  TDUnitX.RegisterTestFixture(TMCPCompletionApiTest);
  TDUnitX.RegisterTestFixture(TMCPCompletionCapabilityTest);

end.
