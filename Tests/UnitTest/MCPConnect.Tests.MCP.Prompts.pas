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
unit MCPConnect.Tests.MCP.Prompts;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Classes,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Attributes,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types.Mrtr,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Prompts;

type
  /// <summary>
  ///   TGetPromptResult is a Result: "resultType" is required on every result
  ///   in 2026-07-28, and prompts/get is one of the three requests that may
  ///   answer "input_required" instead of completing.
  /// </summary>
  [TestFixture]
  TMCPGetPromptResultTest = class(TObject)
  private
    function ResultJson(AResult: TGetPromptResult): TJSONObject;
  public
    [Test]
    procedure TestIsAResult;
    [Test]
    procedure TestEmitsResultType;
    [Test]
    procedure TestIsNotCacheable;
    [Test]
    procedure TestHasResultMeta;
    [Test]
    procedure TestMessagesConstructorAlsoHasResultMeta;
  end;

  TPromptWithArgs = class
  public
    [McpPrompt('greet', 'Greet', 'Greets someone')]
    function Greet([McpArgument('who', 'Who to greet')] const who: string): TPromptMessages;
  end;

  /// <summary>
  ///   prompts/get takes the MRTR params, so a request's _meta and continuation
  ///   token reach it.
  /// </summary>
  [TestFixture]
  TMCPReadPromptParamsTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FApi: TMCPPromptsApi;
    FContext: TJRPCContext;
    FGarbage: IGarbageCollector;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestReadPromptTakesTheMrtrParams;
  end;

implementation

{ TMCPGetPromptResultTest }

function TMCPGetPromptResultTest.ResultJson(AResult: TGetPromptResult): TJSONObject;
begin
  try
    Result := TNeon.ObjectToJSON(AResult, MCPNeonConfig) as TJSONObject;
  finally
    AResult.Free;
  end;
end;

procedure TMCPGetPromptResultTest.TestIsAResult;
begin
  Assert.IsTrue(TGetPromptResult.InheritsFrom(TBaseResult),
    'It used to descend from TMetaClass, which has no resultType');
end;

procedure TMCPGetPromptResultTest.TestEmitsResultType;
var
  LJson: TJSONObject;
begin
  LJson := ResultJson(TGetPromptResult.Create);
  try
    Assert.AreEqual('complete', LJson.GetValue<string>('resultType'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPGetPromptResultTest.TestIsNotCacheable;
var
  LJson: TJSONObject;
begin
  LJson := ResultJson(TGetPromptResult.Create);
  try
    // prompts/get is a Result, not a CacheableResult
    Assert.IsNull(LJson.GetValue('ttlMs'));
    Assert.IsNull(LJson.GetValue('cacheScope'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPGetPromptResultTest.TestHasResultMeta;
var
  LResult: TGetPromptResult;
begin
  // The parameterless constructor never called inherited, so the result _meta
  // was left nil
  LResult := TGetPromptResult.Create;
  try
    Assert.IsNotNull(LResult.ResultMeta);
    Assert.IsNotNull(LResult.Messages);
  finally
    LResult.Free;
  end;
end;

procedure TMCPGetPromptResultTest.TestMessagesConstructorAlsoHasResultMeta;
var
  LResult: TGetPromptResult;
begin
  LResult := TGetPromptResult.Create(TPromptMessages.Create);
  try
    Assert.IsNotNull(LResult.ResultMeta);
    Assert.AreEqual(TResultType.Complete, LResult.ResultType);
  finally
    LResult.Free;
  end;
end;

{ TPromptWithArgs }

function TPromptWithArgs.Greet(const who: string): TPromptMessages;
begin
  Result := TPromptMessages.Create;
  Result.AddText(TRole.User, 'hello ' + who);
end;

{ TMCPReadPromptParamsTest }

procedure TMCPReadPromptParamsTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
  FConfig.Prompts.RegisterClass(TPromptWithArgs);

  FGarbage := TGarbageCollector.Create;
  FContext := TJRPCContext.Create;
  FContext.AddContent(TObject(FGarbage));
  FContext.AddContent(FServer.GetConfiguration<TMCPConfig>);

  FApi := TMCPPromptsApi.Create;
  FContext.Inject(FApi);
end;

procedure TMCPReadPromptParamsTest.TearDown;
begin
  FApi.Free;
  FContext.Free;
  FGarbage := nil;
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPReadPromptParamsTest.TestReadPromptTakesTheMrtrParams;
var
  LParams: TGetPromptRequestParams;
  LResult: TGetPromptResult;
begin
  // prompts/get used to take the pre-MRTR TGetPromptParams, which had no _meta,
  // no inputResponses and no requestState
  LParams := TGetPromptRequestParams.Create;
  try
    TNeon.JSONToObject(LParams,
      '{"name":"greet","arguments":{"who":"world"},' +
      '"requestState":"signed",' +
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/clientCapabilities":{}}}', MCPNeonConfig);

    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion,
      'The request _meta reaches prompts/get now');
    Assert.AreEqual('signed', LParams.RequestState.Value);

    LResult := FApi.ReadPrompt(LParams);
    try
      Assert.AreEqual(1, LResult.Messages.Count);
      Assert.AreEqual(TResultType.Complete, LResult.ResultType);
    finally
      LResult.Free;
    end;
  finally
    LParams.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPGetPromptResultTest);
  TDUnitX.RegisterTestFixture(TMCPReadPromptParamsTest);

end.
