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
unit MCPConnect.Tests.MCP.RequestMeta;

interface

uses
  System.SysUtils, System.Rtti, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Utils,
  Neon.Core.Persistence.JSON,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Notifications,
  MCPConnect.MCP.Types.Tools,
  MCPConnect.MCP.Types.Prompts,
  MCPConnect.MCP.Types.Resources,
  MCPConnect.MCP.Types.Completion;

type
  /// <summary>
  ///   Every 2026-07-28 request carries a required "_meta" holding the protocol
  ///   version and the client's capabilities. The member has to bind under that
  ///   name, not under the Delphi field name.
  /// </summary>
  [TestFixture]
  TMCPRequestMetaTest = class(TObject)
  private
    const REQUEST_META =
      '"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28",' +
      '"io.modelcontextprotocol/logLevel":"debug",' +
      '"io.modelcontextprotocol/clientInfo":{"name":"probe","version":"1.0"},' +
      '"io.modelcontextprotocol/clientCapabilities":{}}';
  public
    [Test]
    procedure TestCallToolParamsBindMeta;
    [Test]
    procedure TestPaginatedParamsBindMeta;
    [Test]
    procedure TestReadResourceParamsBindMeta;
    [Test]
    procedure TestCompleteParamsBindMeta;
    [Test]
    procedure TestPromptParamsBindMeta;

    [Test]
    procedure TestMetaIsWrittenBackUnderTheProtocolName;
    [Test]
    procedure TestClientInfoAndLogLevelSurvive;

    [Test]
    [TestCase('resources/list', 'resources/list')]
    [TestCase('resources/templates/list', 'resources/templates/list')]
    [TestCase('prompts/list', 'prompts/list')]
    [TestCase('tools/list', 'tools/list')]
    procedure TestListEndpointsDeclareTheirParams(const AMethod: string);

    [Test]
    procedure TestListEndpointsStillWorkWithoutParams;
  end;

  /// <summary>
  ///   A ProgressToken is a string or an integer, and is absent unless the
  ///   client actually asked for progress.
  /// </summary>
  [TestFixture]
  TMCPProgressTokenTest = class(TObject)
  private
    FMeta: TRequestMetaObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAbsentByDefault;
    [Test]
    procedure TestReadsIntegerToken;
    [Test]
    procedure TestReadsStringToken;
    [Test]
    procedure TestIntegerTokenIsWrittenBackUnquoted;
    [Test]
    procedure TestSettersReplaceTheToken;
    [Test]
    procedure TestNotificationCopiesTheTokenRatherThanAdoptingIt;
  end;

implementation

{ TMCPRequestMetaTest }

procedure TMCPRequestMetaTest.TestCallToolParamsBindMeta;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    TNeon.JSONToObject(LParams, '{"name":"my_tool",' + REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('my_tool', LParams.Name);
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestPaginatedParamsBindMeta;
var
  LParams: TPaginatedRequestParams;
begin
  LParams := TPaginatedRequestParams.Create;
  try
    TNeon.JSONToObject(LParams, '{"cursor":"abc",' + REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('abc', LParams.Cursor.Value);
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestReadResourceParamsBindMeta;
var
  LParams: TReadResourceParams;
begin
  LParams := TReadResourceParams.Create;
  try
    TNeon.JSONToObject(LParams, '{"uri":"res://a",' + REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('res://a', LParams.Uri);
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestCompleteParamsBindMeta;
var
  LParams: TCompleteRequestParams;
begin
  LParams := TCompleteRequestParams.Create;
  try
    TNeon.JSONToObject(LParams,
      '{"ref":{"type":"ref/prompt","name":"p"},"argument":{"name":"a","value":"v"},' +
      REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('p', LParams.Ref.Target);
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestPromptParamsBindMeta;
var
  LParams: TGetPromptRequestParams;
begin
  LParams := TGetPromptRequestParams.Create;
  try
    TNeon.JSONToObject(LParams, '{"name":"code_review",' + REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('code_review', LParams.Name);
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LParams.RequestMeta.ProtocolVersion);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestMetaIsWrittenBackUnderTheProtocolName;
var
  LParams: TCallToolRequestParams;
  LJson: string;
begin
  LParams := TCallToolRequestParams.Create;
  try
    LParams.Name := 'my_tool';
    LParams.RequestMeta.ProtocolVersion := MCP_PROTOCOL_VERSION_2026_07_28;

    LJson := TNeon.ObjectToJSONString(LParams, MCPNeonConfig);
    Assert.IsTrue(LJson.Contains('"_meta"'), LJson);
    Assert.IsFalse(LJson.Contains('requestMeta'),
      'The Delphi field name must not reach the wire: ' + LJson);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestClientInfoAndLogLevelSurvive;
var
  LParams: TCallToolRequestParams;
begin
  LParams := TCallToolRequestParams.Create;
  try
    TNeon.JSONToObject(LParams, '{"name":"t",' + REQUEST_META + '}', MCPNeonConfig);

    Assert.AreEqual('probe', LParams.RequestMeta.ClientInfo.Name);
    Assert.AreEqual('1.0', LParams.RequestMeta.ClientInfo.Version);
    Assert.IsTrue(LParams.RequestMeta.LogLevel.HasValue,
      'The per-request log level replaced logging/setLevel and has to arrive');
    Assert.AreEqual(TMCPLogLevel.Debug, LParams.RequestMeta.LogLevel.Value);
  finally
    LParams.Free;
  end;
end;

procedure TMCPRequestMetaTest.TestListEndpointsDeclareTheirParams(const AMethod: string);
var
  LApiClass: TClass;
  LMethodName: string;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
begin
  // Every list endpoint has to declare params: PaginatedRequestParams._meta is
  // required, and a method taking none simply discards it
  if AMethod = 'resources/list' then
  begin
    LApiClass := TMCPResourcesApi; LMethodName := 'ResourcesList';
  end
  else if AMethod = 'resources/templates/list' then
  begin
    LApiClass := TMCPResourcesApi; LMethodName := 'TemplatesList';
  end
  else if AMethod = 'prompts/list' then
  begin
    LApiClass := TMCPPromptsApi; LMethodName := 'PromptList';
  end
  else
  begin
    LApiClass := TMCPToolsApi; LMethodName := 'ToolsList';
  end;

  LMethod := TRttiUtils.Context.GetType(LApiClass).GetMethod(LMethodName);
  Assert.IsNotNull(LMethod, LMethodName);

  LParams := LMethod.GetParameters;
  Assert.AreEqual(1, Length(LParams), LMethodName + ' should take exactly one params object');
  Assert.AreEqual('TPaginatedRequestParams', LParams[0].ParamType.Name);
  Assert.IsNotNull(TRttiUtils.FindAttribute<JRPCParamsAttribute>(LParams[0]),
    LMethodName + ' params should be marked [JRPCParams]');
end;

procedure TMCPRequestMetaTest.TestListEndpointsStillWorkWithoutParams;
var
  LParams: TPaginatedRequestParams;
begin
  // A request may omit "params" entirely; the invoker builds a default object
  // rather than handing Neon a nil, which used to fault
  LParams := TRttiUtils.CreateInstance(
    TRttiUtils.Context.GetType(TPaginatedRequestParams)) as TPaginatedRequestParams;
  try
    Assert.IsNotNull(LParams.RequestMeta, 'A defaulted params object still has its _meta');
    Assert.AreEqual('', LParams.RequestMeta.ProtocolVersion);
    Assert.IsFalse(LParams.Cursor.HasValue);
  finally
    LParams.Free;
  end;
end;

{ TMCPProgressTokenTest }

procedure TMCPProgressTokenTest.Setup;
begin
  FMeta := TRequestMetaObject.Create;
end;

procedure TMCPProgressTokenTest.TearDown;
begin
  FMeta.Free;
end;

procedure TMCPProgressTokenTest.TestAbsentByDefault;
begin
  // A server must not send progress the client never asked for
  Assert.IsFalse(FMeta.HasProgressToken);
  Assert.IsNull(FMeta.ProgressToken);
end;

procedure TMCPProgressTokenTest.TestReadsIntegerToken;
begin
  TNeon.JSONToObject(FMeta, '{"progressToken":42}', MCPNeonConfig);

  Assert.IsTrue(FMeta.HasProgressToken);
  Assert.AreEqual('42', FMeta.ProgressToken.ToJSON, 'An integer token must not be flattened to a string');
end;

procedure TMCPProgressTokenTest.TestReadsStringToken;
begin
  TNeon.JSONToObject(FMeta, '{"progressToken":"tok-a"}', MCPNeonConfig);

  Assert.IsTrue(FMeta.HasProgressToken);
  Assert.AreEqual('"tok-a"', FMeta.ProgressToken.ToJSON);
end;

procedure TMCPProgressTokenTest.TestIntegerTokenIsWrittenBackUnquoted;
var
  LJson: string;
begin
  FMeta.SetProgressToken(Int64(7));

  LJson := TNeon.ObjectToJSONString(FMeta, MCPNeonConfig);
  Assert.IsTrue(LJson.Contains('"progressToken":7'), LJson);
end;

procedure TMCPProgressTokenTest.TestSettersReplaceTheToken;
begin
  // Leak-checked: setting twice must free the first token
  FMeta.SetProgressToken('first');
  FMeta.SetProgressToken(Int64(2));

  Assert.AreEqual('2', FMeta.ProgressToken.ToJSON);
end;

procedure TMCPProgressTokenTest.TestNotificationCopiesTheTokenRatherThanAdoptingIt;
var
  LNotification: TJRPCNotification;
begin
  FMeta.SetProgressToken(Int64(42));

  LNotification := TMCPNotification.Progress(FMeta.ProgressToken, 5, 10, 'half way');
  try
    Assert.IsTrue((LNotification.Params as TJSONObject).ToJSON.Contains('"progressToken":42'),
      LNotification.Params.ToJSON);

    // The request params still own theirs: freeing the notification must not
    // take the token with it
    Assert.IsTrue(FMeta.HasProgressToken);
    Assert.AreEqual('42', FMeta.ProgressToken.ToJSON);
  finally
    LNotification.Free;
  end;

  Assert.AreEqual('42', FMeta.ProgressToken.ToJSON, 'The token survives the notification');
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPRequestMetaTest);
  TDUnitX.RegisterTestFixture(TMCPProgressTokenTest);

end.
