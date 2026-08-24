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
unit MCPConnect.Tests.Transport.Headers;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  MCPConnect.Transport.Base;

type
  [TestFixture]
  TTransportHeadersTest = class(TObject)
  private
    FHeaders: TMCPTransportHeaders;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestGet_ReturnsEmptyForMissing();
    [Test]
    procedure TestSet_StoresValue();
    [Test]
    procedure TestSet_OverwritesExisting();
    [Test]
    procedure TestSet_IsCaseInsensitive();
    [Test]
    procedure TestAdd_AllowsDuplicates();
    [Test]
    procedure TestAdd_GetReturnsFirst();
    [Test]
    procedure TestIndexOf_ReturnsNegativeForMissing();
    [Test]
    procedure TestIndexOf_IsCaseInsensitive();
    [Test]
    procedure TestSet_AfterAdd_RemovesAllDuplicates();
    [Test]
    procedure TestRemoveHeader_RemovesAllMatches();
    [Test]
    procedure TestRemoveHeader_ReturnsCount();
    [Test]
    procedure TestRemoveHeader_ReturnsZeroForMissing();
    [Test]
    procedure TestGetHeaders_ReturnsAllValues();
    [Test]
    procedure TestGetHeaders_ReturnsEmptyForMissing();
    [Test]
    procedure TestClear_RemovesAll();
    [Test]
    procedure TestGetEnumerator();
  end;

  [TestFixture]
  TTransportRequestHeadersTest = class(TObject)
  private
    FRequest: TMCPTransportRequest;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAdd_DuplicateAuthorization_Raises();
    [Test]
    procedure TestAdd_SingleAuthorization_Allowed();
    [Test]
    procedure TestSetHeader_AuthorizationOverwrite_Allowed();
    [Test]
    procedure TestGetHeader_DelegatesToHeaders();
    [Test]
    procedure TestSetHeader_DelegatesToHeaders();
  end;

  [TestFixture]
  TTransportResponseHeadersTest = class(TObject)
  private
    FResponse: TMCPTransportResponse;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestSetCookie_AddsDuplicateHeaders();
    [Test]
    procedure TestSetContentType_OverwritesPrevious();
    [Test]
    procedure TestHeadersIteration_PreservesInsertionOrder();
    [Test]
    procedure TestClearCookies_RemovesAllCookieHeaders();
    [Test]
    procedure TestClearCookies_PreservesOtherHeaders();
    [Test]
    procedure TestClearCookies_OnEmptyHeaders();
    [Test]
    procedure TestGetHeader_ReturnsValue();
    [Test]
    procedure TestSetHeader_DelegatesToHeaders();
  end;

implementation

{ TTransportHeadersTest }

procedure TTransportHeadersTest.Setup();
begin
  FHeaders := TMCPTransportHeaders.Create();
end;

procedure TTransportHeadersTest.TearDown();
begin
  FHeaders.Free();
end;

procedure TTransportHeadersTest.TestGet_ReturnsEmptyForMissing();
begin
  Assert.AreEqual('', FHeaders.Get('X-Missing'));
end;

procedure TTransportHeadersTest.TestSet_StoresValue();
begin
  FHeaders.&Set('Content-Type', 'application/json');
  Assert.AreEqual('application/json', FHeaders.Get('Content-Type'));
end;

procedure TTransportHeadersTest.TestSet_OverwritesExisting();
begin
  FHeaders.&Set('Content-Type', 'text/plain');
  FHeaders.&Set('Content-Type', 'application/json');

  Assert.AreEqual('application/json', FHeaders.Get('Content-Type'));
  Assert.AreEqual(1, FHeaders.Count);
end;

procedure TTransportHeadersTest.TestSet_IsCaseInsensitive();
begin
  FHeaders.&Set('Content-Type', 'text/plain');
  FHeaders.&Set('content-type', 'application/json');

  Assert.AreEqual('application/json', FHeaders.Get('CONTENT-TYPE'));
  Assert.AreEqual(1, FHeaders.Count);
end;

procedure TTransportHeadersTest.TestAdd_AllowsDuplicates();
begin
  FHeaders.Add('Set-Cookie', 'a=1');
  FHeaders.Add('Set-Cookie', 'b=2');

  Assert.AreEqual(2, FHeaders.Count);
end;

procedure TTransportHeadersTest.TestAdd_GetReturnsFirst();
begin
  FHeaders.Add('Set-Cookie', 'a=1');
  FHeaders.Add('Set-Cookie', 'b=2');

  Assert.AreEqual('a=1', FHeaders.Get('Set-Cookie'));
end;

procedure TTransportHeadersTest.TestIndexOf_ReturnsNegativeForMissing();
begin
  Assert.AreEqual(-1, FHeaders.IndexOf('X-Missing'));
end;

procedure TTransportHeadersTest.TestIndexOf_IsCaseInsensitive();
begin
  FHeaders.&Set('Authorization', 'Bearer token');
  Assert.IsTrue(FHeaders.IndexOf('authorization') >= 0);
end;

procedure TTransportHeadersTest.TestSet_AfterAdd_RemovesAllDuplicates();
begin
  FHeaders.Add('X-Custom', 'first');
  FHeaders.Add('X-Custom', 'second');
  FHeaders.&Set('X-Custom', 'replaced');

  Assert.AreEqual(1, FHeaders.Count);
  Assert.AreEqual('replaced', FHeaders.Get('X-Custom'));
end;

procedure TTransportHeadersTest.TestRemoveHeader_RemovesAllMatches();
begin
  FHeaders.Add('X-Custom', 'first');
  FHeaders.Add('X-Custom', 'second');
  FHeaders.Add('X-Other', 'keep');

  FHeaders.RemoveHeader('X-Custom');

  Assert.AreEqual(1, FHeaders.Count);
  Assert.AreEqual('keep', FHeaders.Get('X-Other'));
end;

procedure TTransportHeadersTest.TestRemoveHeader_ReturnsCount();
begin
  FHeaders.Add('X-Custom', 'first');
  FHeaders.Add('X-Custom', 'second');
  FHeaders.Add('X-Custom', 'third');

  Assert.AreEqual(3, FHeaders.RemoveHeader('X-Custom'));
end;

procedure TTransportHeadersTest.TestRemoveHeader_ReturnsZeroForMissing();
begin
  Assert.AreEqual(0, FHeaders.RemoveHeader('X-Missing'));
end;

procedure TTransportHeadersTest.TestGetHeaders_ReturnsAllValues();
var
  LValues: TArray<string>;
begin
  FHeaders.Add('Set-Cookie', 'a=1');
  FHeaders.Add('Set-Cookie', 'b=2');
  FHeaders.Add('Set-Cookie', 'c=3');

  LValues := FHeaders.GetHeaders('Set-Cookie');

  Assert.AreEqual(3, Length(LValues));
  Assert.AreEqual('a=1', LValues[0]);
  Assert.AreEqual('b=2', LValues[1]);
  Assert.AreEqual('c=3', LValues[2]);
end;

procedure TTransportHeadersTest.TestGetHeaders_ReturnsEmptyForMissing();
begin
  Assert.AreEqual(0, Length(FHeaders.GetHeaders('X-Missing')));
end;

procedure TTransportHeadersTest.TestClear_RemovesAll();
begin
  FHeaders.Add('X-First', '1');
  FHeaders.Add('X-Second', '2');

  FHeaders.Clear();

  Assert.AreEqual(0, FHeaders.Count);
end;

procedure TTransportHeadersTest.TestGetEnumerator();
var
  LCount: Integer;
begin
  FHeaders.Add('X-First', '1');
  FHeaders.Add('X-Second', '2');

  LCount := 0;
  for var LPair in FHeaders do
  begin
    Inc(LCount);
    Assert.IsFalse(LPair.Key.IsEmpty());
  end;

  Assert.AreEqual(2, LCount);
end;

{ TTransportRequestHeadersTest }

procedure TTransportRequestHeadersTest.Setup();
begin
  FRequest := TMCPTransportRequest.Create();
end;

procedure TTransportRequestHeadersTest.TearDown();
begin
  FRequest.Free();
end;

procedure TTransportRequestHeadersTest.TestAdd_DuplicateAuthorization_Raises();
begin
  FRequest.Headers.Add('Authorization', 'Bearer token1');

  Assert.WillRaise(
    procedure
    begin
      FRequest.Headers.Add('authorization', 'Bearer token2');
    end,
    EMCPTransportException
  );
end;

procedure TTransportRequestHeadersTest.TestAdd_SingleAuthorization_Allowed();
begin
  FRequest.Headers.Add('Authorization', 'Bearer token1');
  Assert.AreEqual('Bearer token1', FRequest.GetHeader('Authorization'));
end;

procedure TTransportRequestHeadersTest.TestSetHeader_AuthorizationOverwrite_Allowed();
begin
  FRequest.Headers.Add('Authorization', 'Bearer token1');
  FRequest.SetHeader('Authorization', 'Bearer token2');
  Assert.AreEqual('Bearer token2', FRequest.GetHeader('Authorization'));
  Assert.AreEqual(1, FRequest.Headers.Count);
end;

procedure TTransportRequestHeadersTest.TestGetHeader_DelegatesToHeaders();
begin
  FRequest.Headers.Add('X-Custom', 'value');
  Assert.AreEqual('value', FRequest.GetHeader('X-Custom'));
end;

procedure TTransportRequestHeadersTest.TestSetHeader_DelegatesToHeaders();
begin
  FRequest.SetHeader('X-Custom', 'value');
  Assert.AreEqual('value', FRequest.Headers.Get('X-Custom'));
end;

{ TTransportResponseHeadersTest }

procedure TTransportResponseHeadersTest.Setup();
begin
  FResponse := TMCPTransportResponse.Create();
end;

procedure TTransportResponseHeadersTest.TearDown();
begin
  FResponse.Free();
end;

procedure TTransportResponseHeadersTest.TestSetCookie_AddsDuplicateHeaders();
var
  LValues: TArray<string>;
begin
  FResponse.SetCookie('session', 'abc123', False);
  FResponse.SetCookie('theme', 'dark', False);

  LValues := FResponse.Headers.GetHeaders('Set-Cookie');
  Assert.AreEqual(2, Length(LValues));
end;

procedure TTransportResponseHeadersTest.TestSetContentType_OverwritesPrevious();
begin
  FResponse.ContentType := 'text/plain';
  FResponse.ContentType := 'application/json';

  Assert.AreEqual(1, Length(FResponse.Headers.GetHeaders('Content-Type')));
  Assert.AreEqual('application/json', FResponse.GetHeader('Content-Type'));
end;

procedure TTransportResponseHeadersTest.TestHeadersIteration_PreservesInsertionOrder();
var
  LKeys: TArray<string>;
begin
  FResponse.SetHeader('X-First', '1');
  FResponse.SetHeader('X-Second', '2');
  FResponse.SetHeader('X-Third', '3');

  SetLength(LKeys, 0);
  for var LPair in FResponse.Headers do
    LKeys := LKeys + [LPair.Key];

  Assert.AreEqual(3, Length(LKeys));
  Assert.AreEqual('X-First', LKeys[0]);
  Assert.AreEqual('X-Second', LKeys[1]);
  Assert.AreEqual('X-Third', LKeys[2]);
end;

procedure TTransportResponseHeadersTest.TestClearCookies_RemovesAllCookieHeaders();
begin
  FResponse.SetCookie('session', 'abc', False);
  FResponse.SetCookie('theme', 'dark', False);

  FResponse.ClearCookies();

  Assert.AreEqual('', FResponse.GetHeader('Set-Cookie'));
end;

procedure TTransportResponseHeadersTest.TestClearCookies_PreservesOtherHeaders();
begin
  FResponse.ContentType := 'application/json';
  FResponse.SetCookie('session', 'abc', False);
  FResponse.SetHeader('X-Custom', 'value');

  FResponse.ClearCookies();

  Assert.AreEqual('application/json', FResponse.GetHeader('Content-Type'));
  Assert.AreEqual('value', FResponse.GetHeader('X-Custom'));
  Assert.AreEqual(2, FResponse.Headers.Count);
end;

procedure TTransportResponseHeadersTest.TestClearCookies_OnEmptyHeaders();
begin
  FResponse.ClearCookies();
  Assert.AreEqual(0, FResponse.Headers.Count);
end;

procedure TTransportResponseHeadersTest.TestGetHeader_ReturnsValue();
begin
  FResponse.SetHeader('X-Custom', 'value');
  Assert.AreEqual('value', FResponse.GetHeader('X-Custom'));
end;

procedure TTransportResponseHeadersTest.TestSetHeader_DelegatesToHeaders();
begin
  FResponse.SetHeader('X-Custom', 'value');
  Assert.AreEqual('value', FResponse.Headers.Get('X-Custom'));
end;

end.
