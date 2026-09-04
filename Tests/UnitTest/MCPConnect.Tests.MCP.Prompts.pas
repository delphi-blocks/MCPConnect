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

initialization
  TDUnitX.RegisterTestFixture(TMCPGetPromptResultTest);

end.
