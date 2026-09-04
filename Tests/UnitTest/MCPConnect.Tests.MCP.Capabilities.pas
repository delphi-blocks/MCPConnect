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
unit MCPConnect.Tests.MCP.Capabilities;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  Neon.Core.Persistence.JSON,

  MCPConnect.MCP.Types.Base;

type
  /// <summary>
  ///   2026-07-28 turned the client's "sampling" capability from a bare marker
  ///   object into a structured one with "context" and "tools" sub-capabilities.
  /// </summary>
  [TestFixture]
  TMCPClientSamplingTest = class(TObject)
  private
    FCapabilities: TClientCapabilities;

    procedure Read(const AJson: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSamplingIsTyped;
    [Test]
    procedure TestReadsContextSubCapability;
    [Test]
    procedure TestReadsToolsSubCapability;
    [Test]
    procedure TestReadsBothSubCapabilities;
    [Test]
    procedure TestBareSamplingObjectStillReads;
    [Test]
    procedure TestRoundTripsSubCapabilities;
    [Test]
    procedure TestElicitationKeepsItsShape;
  end;

implementation

{ TMCPClientSamplingTest }

procedure TMCPClientSamplingTest.Setup;
begin
  FCapabilities := TClientCapabilities.Create;
end;

procedure TMCPClientSamplingTest.TearDown;
begin
  FCapabilities.Free;
end;

procedure TMCPClientSamplingTest.Read(const AJson: string);
begin
  TNeon.JSONToObject(FCapabilities, AJson, MCPNeonConfig);
end;

procedure TMCPClientSamplingTest.TestSamplingIsTyped;
begin
  // It used to be a bare TJSONObject, which round-tripped but could not be asked
  // what the client actually supports
  Assert.IsNotNull(FCapabilities.Sampling);
  Assert.IsTrue(FCapabilities.Sampling is TMCPSampling);
end;

procedure TMCPClientSamplingTest.TestReadsContextSubCapability;
begin
  Read('{"sampling":{"context":{}}}');

  Assert.IsTrue(FCapabilities.Sampling.SupportsContext);
  Assert.IsNotNull(FCapabilities.Sampling.Context);
end;

procedure TMCPClientSamplingTest.TestReadsToolsSubCapability;
begin
  Read('{"sampling":{"tools":{}}}');

  Assert.IsTrue(FCapabilities.Sampling.SupportsTools);
end;

procedure TMCPClientSamplingTest.TestReadsBothSubCapabilities;
begin
  Read('{"sampling":{"context":{},"tools":{}}}');

  Assert.IsTrue(FCapabilities.Sampling.SupportsContext);
  Assert.IsTrue(FCapabilities.Sampling.SupportsTools);
end;

procedure TMCPClientSamplingTest.TestBareSamplingObjectStillReads;
begin
  // A client declaring plain sampling, with no sub-capabilities, must not break
  Read('{"sampling":{}}');

  Assert.IsNotNull(FCapabilities.Sampling);
end;

procedure TMCPClientSamplingTest.TestRoundTripsSubCapabilities;
var
  LJson: string;
begin
  FCapabilities.Sampling.Tools.AddPair('maxTools', TJSONNumber.Create(4));

  LJson := TNeon.ObjectToJSONString(FCapabilities, MCPNeonConfig);
  Assert.IsTrue(LJson.Contains('"sampling":{"tools":{"maxTools":4}}'), LJson);
end;

procedure TMCPClientSamplingTest.TestElicitationKeepsItsShape;
begin
  // Sampling was modelled on elicitation, which must go on working the same way
  Read('{"elicitation":{"form":{}}}');

  Assert.IsTrue(FCapabilities.Elicitation.SupportsForm);
  Assert.IsTrue(FCapabilities.Elicitation.SupportsUrl);
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPClientSamplingTest);

end.
