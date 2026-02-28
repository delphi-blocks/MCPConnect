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
unit MCPConnect.Tests.Core.Utils;

interface

uses
  System.SysUtils, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.Core.Utils;

type
  [TestFixture]
  TRouteMatcherTest = class(TObject)
  private
    FMatcher: TRouteMatcher;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TestExactMatch;
    [Test]
    procedure TestSingleParamMatch;
    [Test]
    procedure TestMultipleParamsMatch;
    [Test]
    procedure TestParamValueExtraction;
    [Test]
    procedure TestUrlDecodedParam;
    [Test]
    procedure TestCaseInsensitivity;
    [Test]
    procedure TestNonMatching;
    [Test]
    procedure TestPartialMatchShouldFail;
    [Test]
    procedure TestSpecialCharactersInTemplate;
    [Test]
    procedure TestParamAtStart;
    [Test]
    procedure TestParamAtEnd;
    [Test]
    procedure TestEmptyTemplateAndInput;

    [Test]
    procedure TestCreateNewValue;
  end;

implementation

uses
  System.TypInfo;

{ TRouteMatcherTest }

procedure TRouteMatcherTest.Setup;
begin
  FMatcher := TRouteMatcher.Create;
end;

procedure TRouteMatcherTest.Teardown;
begin
  FMatcher.Free;
end;

procedure TRouteMatcherTest.TestExactMatch;
begin
  Assert.IsTrue(FMatcher.Match('/api/v1/tools', '/api/v1/tools'), 'Exact match should succeed');
  Assert.AreEqual(0, FMatcher.Params.Count, 'Exact match should have 0 parameters');
end;

procedure TRouteMatcherTest.TestSingleParamMatch;
begin
  Assert.IsTrue(FMatcher.Match('/users/{id}', '/users/123'), 'Single parameter match should succeed');
  Assert.AreEqual(1, FMatcher.Params.Count, 'Should have 1 parameter');
  Assert.AreEqual('123', FMatcher.Params['id'], 'Parameter "id" should be "123"');
end;

procedure TRouteMatcherTest.TestMultipleParamsMatch;
begin
  Assert.IsTrue(FMatcher.Match('/users/{id}/orders/{orderId}', '/users/42/orders/999'), 'Multiple parameters match should succeed');
  Assert.AreEqual(2, FMatcher.Params.Count, 'Should have 2 parameters');
  Assert.AreEqual('42', FMatcher.Params['id'], 'Parameter "id" should be "42"');
  Assert.AreEqual('999', FMatcher.Params['orderId'], 'Parameter "orderId" should be "999"');
end;

procedure TRouteMatcherTest.TestParamValueExtraction;
begin
  FMatcher.Match('/data/{name}/{category}', '/data/sensors/temperature');
  Assert.AreEqual('sensors', FMatcher.Params['name']);
  Assert.AreEqual('temperature', FMatcher.Params['category']);
end;

procedure TRouteMatcherTest.TestUrlDecodedParam;
begin
  // URL encoded space is %20 or +
  Assert.IsTrue(FMatcher.Match('/search/{query}', '/search/hello%20world'), 'URL encoded parameter match should succeed');
  Assert.AreEqual('hello world', FMatcher.Params['query'], 'Parameter value should be URL decoded');
end;

procedure TRouteMatcherTest.TestCaseInsensitivity;
begin
  Assert.IsTrue(FMatcher.Match('/API/V1/TOOLS', '/api/v1/tools'), 'Match should be case-insensitive');
  Assert.IsTrue(FMatcher.Match('/users/{ID}', '/users/abc'), 'Parameter name matching should be case-insensitive');
  Assert.AreEqual('abc', FMatcher.Params['ID'], 'Should extract parameter with original case name if defined in template');
end;

procedure TRouteMatcherTest.TestNonMatching;
begin
  Assert.IsFalse(FMatcher.Match('/users/{id}', '/orders/123'), 'Different path should not match');
  Assert.IsFalse(FMatcher.Match('/users/{id}', '/users/123/extra'), 'Longer path should not match');
  Assert.IsFalse(FMatcher.Match('/users/{id}/extra', '/users/123'), 'Shorter path should not match');
end;

procedure TRouteMatcherTest.TestPartialMatchShouldFail;
begin
  // The BuildRegex adds ^ and $ so it should not match partially
  Assert.IsFalse(FMatcher.Match('/api', '/api/v1'), 'Partial match at start should fail');
  Assert.IsFalse(FMatcher.Match('/v1', '/api/v1'), 'Partial match at end should fail');
end;

procedure TRouteMatcherTest.TestSpecialCharactersInTemplate;
begin
  Assert.IsTrue(FMatcher.Match('/api.v1/tools-list', '/api.v1/tools-list'), 'Special characters like dots and hyphens should work');
end;

procedure TRouteMatcherTest.TestParamAtStart;
begin
  Assert.IsTrue(FMatcher.Match('{tenant}/users', 'acme/users'), 'Parameter at start should match');
  Assert.AreEqual('acme', FMatcher.Params['tenant']);
end;

procedure TRouteMatcherTest.TestParamAtEnd;
begin
  Assert.IsTrue(FMatcher.Match('/users/{id}', '/users/42'), 'Parameter at end should match');
  Assert.AreEqual('42', FMatcher.Params['id']);
end;

procedure TRouteMatcherTest.TestEmptyTemplateAndInput;
begin
  Assert.IsTrue(FMatcher.Match('', ''), 'Empty template and input should match');
  Assert.IsFalse(FMatcher.Match('', '/api'), 'Empty template should not match non-empty input');
  Assert.IsFalse(FMatcher.Match('/api', ''), 'Non-empty template should not match empty input');
end;

procedure TRouteMatcherTest.TestCreateNewValue;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LValue: TValue;
begin
  LContext := TRttiContext.Create;
  try
    // Test Integer
    LType := LContext.GetType(TypeInfo(Integer));
    LValue := CreateNewValue(LType);
    Assert.AreEqual(0, LValue.AsInteger, 'Default integer should be 0');

    // Test string
    LType := LContext.GetType(TypeInfo(string));
    LValue := CreateNewValue(LType);
    Assert.AreEqual('', LValue.AsString, 'Default string should be empty');

    // Test Double
    LType := LContext.GetType(TypeInfo(Double));
    LValue := CreateNewValue(LType);
    Assert.AreEqual(0.0, LValue.AsExtended, 'Default float should be 0.0');
  finally
    LContext.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRouteMatcherTest);

end.
