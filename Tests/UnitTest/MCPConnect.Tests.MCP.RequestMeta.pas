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
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base,
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

initialization
  TDUnitX.RegisterTestFixture(TMCPRequestMetaTest);

end.
