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
unit MCPConnect.Tests.MCP.Server.Api;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Server,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server.Api,
  MCPConnect.MCP.Types;

type
  [TestFixture]
  TMCPInitializeApiTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
    FApi: TMCPInitializeApi;

    function Initialize(const AProtocolVersion: string): TInitializeResult;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestInitialize_ClientRequestsSupportedOlderVersion_EchoesItBack;
    [Test]
    procedure TestInitialize_ClientRequestsSupportedLatestVersion_EchoesItBack;
    [Test]
    procedure TestInitialize_ClientRequestsUnsupportedVersion_FallsBackToLatestSupported;
    [Test]
    procedure TestInitialize_ClientRequestsEmptyVersion_FallsBackToLatestSupported;
  end;

implementation

procedure TMCPInitializeApiTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;

  FApi := TMCPInitializeApi.Create;
  FApi.MCPConfig := FServer.GetConfiguration<TMCPConfig>;
end;

procedure TMCPInitializeApiTest.TearDown;
begin
  FApi.Free;
  FConfig := nil;
  FServer.Free;
end;

function TMCPInitializeApiTest.Initialize(const AProtocolVersion: string): TInitializeResult;
var
  LParams: TInitializeParams;
begin
  LParams := TInitializeParams.Create;
  try
    LParams.ProtocolVersion := AProtocolVersion;
    Result := FApi.Initialize(LParams);
  finally
    LParams.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsSupportedOlderVersion_EchoesItBack;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize(MCP_PROTOCOL_VERSION_2025_06_18);
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_06_18, LResult.ProtocolVersion,
      'A supported, non-latest version requested by the client should be echoed back unchanged');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsSupportedLatestVersion_EchoesItBack;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize(MCP_PROTOCOL_VERSION_2025_11_25);
  try
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2025_11_25, LResult.ProtocolVersion,
      'The latest supported version requested by the client should be echoed back unchanged');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsUnsupportedVersion_FallsBackToLatestSupported;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize('1999-01-01');
  try
    Assert.AreEqual(MCP_LATEST_PROTOCOL_VERSION, LResult.ProtocolVersion,
      'An unsupported version requested by the client must not be echoed back; the server should propose its latest supported version instead');
  finally
    LResult.Free;
  end;
end;

procedure TMCPInitializeApiTest.TestInitialize_ClientRequestsEmptyVersion_FallsBackToLatestSupported;
var
  LResult: TInitializeResult;
begin
  LResult := Initialize('');
  try
    Assert.AreEqual(MCP_LATEST_PROTOCOL_VERSION, LResult.ProtocolVersion,
      'An empty/missing version should also fall back to the server''s latest supported version');
  finally
    LResult.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPInitializeApiTest);

end.
