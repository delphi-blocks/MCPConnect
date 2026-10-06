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
unit MCPConnect.Tests.MCP.AccessToken;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,

  MCPConnect.MCP.Types.Base;

type
  /// <summary>
  ///   The setters of TMCPAccessToken, which let a validator that is not decoding
  ///   a JWT (an API key validator, typically) describe the caller.
  /// </summary>
  [TestFixture]
  TMCPAccessTokenSetterTest = class(TObject)
  private
    FToken: TMCPAccessToken;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestStringClaims_RoundTrip;
    [Test]
    procedure TestSetter_ReplacesInsteadOfDuplicating;
    [Test]
    procedure TestEmptyString_RemovesTheClaim;
    [Test]
    procedure TestValueWithQuotes_StaysValidJson;
    [Test]
    procedure TestAudience_RoundTrip;
    [Test]
    procedure TestDates_RoundTrip;
    [Test]
    procedure TestClientId_ClearsAuthorizedParty;
    [Test]
    procedure TestScope_ClearsEntraScope;
    [Test]
    procedure TestEmailVerified_RoundTrip;
    [Test]
    procedure TestAssign_CopiesWhatTheSettersWrote;
  end;

implementation

uses
  System.JSON, System.DateUtils;

{ TMCPAccessTokenSetterTest }

procedure TMCPAccessTokenSetterTest.Setup;
begin
  FToken := TMCPAccessToken.Create;
end;

procedure TMCPAccessTokenSetterTest.TearDown;
begin
  FToken.Free;
end;

procedure TMCPAccessTokenSetterTest.TestStringClaims_RoundTrip;
begin
  FToken.Subject := 'customer-42';
  FToken.Name := 'Acme';
  FToken.EMail := 'ops@acme.example';
  FToken.Scope := 'tasks:read tasks:write';
  FToken.PreferredUsername := 'acme';
  FToken.GivenName := 'Wile';
  FToken.FamilyName := 'Coyote';
  FToken.Issuer := 'https://keys.example';

  Assert.AreEqual('customer-42', FToken.Subject);
  Assert.AreEqual('Acme', FToken.Name);
  Assert.AreEqual('ops@acme.example', FToken.EMail);
  Assert.AreEqual('tasks:read tasks:write', FToken.Scope);
  Assert.AreEqual('acme', FToken.PreferredUsername);
  Assert.AreEqual('Wile', FToken.GivenName);
  Assert.AreEqual('Coyote', FToken.FamilyName);
  Assert.AreEqual('https://keys.example', FToken.Issuer);
  Assert.AreEqual('customer-42', FToken.Payload.GetValue<string>(TMCPAccessToken.ClaimSubject));
end;

procedure TMCPAccessTokenSetterTest.TestSetter_ReplacesInsteadOfDuplicating;
begin
  FToken.Subject := 'first';
  FToken.Subject := 'second';

  Assert.AreEqual(1, FToken.Payload.Count);
  Assert.AreEqual('second', FToken.Subject);
end;

procedure TMCPAccessTokenSetterTest.TestEmptyString_RemovesTheClaim;
begin
  FToken.Subject := 'customer-42';
  FToken.Subject := '';

  Assert.AreEqual(0, FToken.Payload.Count);
  Assert.AreEqual('', FToken.Subject);
end;

procedure TMCPAccessTokenSetterTest.TestValueWithQuotes_StaysValidJson;
var
  LCopy: TMCPAccessToken;
begin
  FToken.Subject := 'evil", "scope": "admin';

  LCopy := TMCPAccessToken.Create;
  try
    LCopy.FromString(FToken.ToString);
    Assert.AreEqual('evil", "scope": "admin', LCopy.Subject);
    Assert.AreEqual('', LCopy.Scope, 'A value must never be able to inject a claim');
  finally
    LCopy.Free;
  end;
end;

procedure TMCPAccessTokenSetterTest.TestAudience_RoundTrip;
var
  LAudience: TArray<string>;
begin
  FToken.Audience := ['https://a.example', 'https://b.example'];
  LAudience := FToken.Audience;

  Assert.AreEqual(2, Integer(Length(LAudience)));
  Assert.AreEqual('https://b.example', LAudience[1]);

  FToken.Audience := [];
  Assert.AreEqual(0, Integer(Length(FToken.Audience)));
  Assert.AreEqual(0, FToken.Payload.Count);
end;

procedure TMCPAccessTokenSetterTest.TestDates_RoundTrip;
var
  LWhen: TDateTime;
begin
  LWhen := EncodeDateTime(2026, 10, 4, 12, 30, 0, 0);

  FToken.Expiration := LWhen;
  FToken.IssuedAt := LWhen;
  FToken.NotBefore := LWhen;

  Assert.AreEqual(LWhen, FToken.Expiration);
  Assert.AreEqual(LWhen, FToken.IssuedAt);
  Assert.AreEqual(LWhen, FToken.NotBefore);
  Assert.AreEqual(DateTimeToUnix(LWhen), FToken.Payload.GetValue<Int64>(TMCPAccessToken.ClaimExpiration));

  FToken.Expiration := 0;
  Assert.AreEqual(Double(0), Double(FToken.Expiration));
end;

procedure TMCPAccessTokenSetterTest.TestClientId_ClearsAuthorizedParty;
begin
  FToken.FromString('{"azp":"old-client"}');

  FToken.ClientId := 'new-client';
  Assert.AreEqual('new-client', FToken.ClientId);

  FToken.ClientId := '';
  Assert.AreEqual('', FToken.ClientId, 'A stale "azp" must not resurface');
end;

procedure TMCPAccessTokenSetterTest.TestScope_ClearsEntraScope;
begin
  // Entra ID puts the scopes in "scp", which the getter falls back to
  FToken.FromString('{"scp":"old.read"}');
  Assert.AreEqual('old.read', FToken.Scope);

  FToken.Scope := 'tasks:read';
  Assert.AreEqual('tasks:read', FToken.Scope);

  FToken.Scope := '';
  Assert.AreEqual('', FToken.Scope, 'A stale "scp" must not resurface');
end;

procedure TMCPAccessTokenSetterTest.TestEmailVerified_RoundTrip;
begin
  FToken.EmailVerified := True;
  Assert.IsTrue(FToken.EmailVerified);

  FToken.EmailVerified := False;
  Assert.IsFalse(FToken.EmailVerified);
  Assert.AreEqual(1, FToken.Payload.Count);
end;

procedure TMCPAccessTokenSetterTest.TestAssign_CopiesWhatTheSettersWrote;
var
  LCopy: TMCPAccessToken;
begin
  FToken.Subject := 'customer-42';
  FToken.Scope := 'tasks:read';

  LCopy := TMCPAccessToken.Create;
  try
    LCopy.Name := 'to be replaced';
    LCopy.Assign(FToken);

    Assert.AreEqual('customer-42', LCopy.Subject);
    Assert.AreEqual('tasks:read', LCopy.Scope);
    Assert.AreEqual('', LCopy.Name, 'Assign replaces the claims, it does not merge them');

    // A copy, not a shared payload
    FToken.Subject := 'changed';
    Assert.AreEqual('customer-42', LCopy.Subject);
  finally
    LCopy.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPAccessTokenSetterTest);

end.
