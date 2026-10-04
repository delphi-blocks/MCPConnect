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

/// <summary>
///   Covers IAuthTokenConfig (API keys): the fixed token, the validator function
///   and class, the identity they produce, and how the transport answers a request
///   whose token is missing or rejected.
/// </summary>
unit MCPConnect.Tests.Transport.AuthToken;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Auth,
  MCPConnect.Transport.Base,
  MCPConnect.MCP.Types,
  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Server,
  MCPConnect.Tests.Transport.OAuth;

type
  /// <summary>Accepts one exact key and names its holder.</summary>
  TStubAuthTokenValidator = class(TInterfacedObject, IAuthTokenValidator)
  public const
    GoodKey = 'key-accepted-by-the-class';
    Subject = 'class-customer';
  public
    function Validate(AContext: TJRPCContext; const AToken: string;
      AIdentity: TMCPAccessToken): Boolean;
  end;

  [TestFixture]
  TTransportAuthTokenTest = class(TObject)
  private const
    Url = '/mcp';
    KeyHeader = 'X-API-Key';
  private
    FServer: TJRPCServer;
    function Config: TAuthTokenConfig;
    function Execute(const AHeaderName, AHeaderValue: string;
      AProtocol: TTransportProtocol = TTransportProtocol.StreamableHTTP): TTransportOutcome;
    function ValidateWithConfig(const AToken: string; out ASubject: string): Boolean;
    procedure EnableHeaderKey(const AKey: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestStaticKey_IsAccepted;
    [Test]
    procedure TestMissingKey_IsChallengedWith401;
    [Test]
    procedure TestWrongKey_IsChallengedWith401;
    [Test]
    procedure TestBearerLocation_ChallengesWithBearerScheme;
    [Test]
    procedure TestEmptyKey_IsNeverAccepted;

    [Test]
    procedure TestValidatorFunc_DecidesAndFillsIdentity;
    [Test]
    procedure TestValidatorFunc_TakesPrecedenceOverToken;
    [Test]
    procedure TestValidatorFunc_ExceptionIsA401;
    [Test]
    procedure TestValidatorFunc_ReceivesTheContext;
    [Test]
    procedure TestValidatorClass_DecidesAndFillsIdentity;
    [Test]
    procedure TestValidatorClass_RejectsClassWithoutInterface;

    [Test]
    procedure TestStdio_IsExempt;
    [Test]
    procedure TestOptions_IsExempt;
    [Test]
    procedure TestHeaderLocationWithoutName_RaisesOnApply;
    [Test]
    procedure TestEmptyConfig_EnforcesNothing;
  end;

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
    procedure TestEmailVerified_RoundTrip;
  end;

implementation

uses
  System.JSON, System.DateUtils;

{ TStubAuthTokenValidator }

function TStubAuthTokenValidator.Validate(AContext: TJRPCContext;
  const AToken: string; AIdentity: TMCPAccessToken): Boolean;
begin
  Result := AToken = GoodKey;
  if Result then
    AIdentity.FromString('{"sub":"' + Subject + '"}');
end;

{ TTransportAuthTokenTest }

procedure TTransportAuthTokenTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('auth-token-test')
      .SetVersion('1.0.0')
    .BackToMCP
  .ApplyConfig;
end;

procedure TTransportAuthTokenTest.TearDown;
begin
  FServer.Free;
end;

function TTransportAuthTokenTest.Config: TAuthTokenConfig;
begin
  Result := FServer.GetConfiguration<TAuthTokenConfig>;
end;

procedure TTransportAuthTokenTest.EnableHeaderKey(const AKey: string);
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken(AKey)
    .SetTokenLocation(TAuthTokenLocation.Header)
    .SetTokenCustomHeader(KeyHeader)
  .ApplyConfig;
end;

function TTransportAuthTokenTest.Execute(const AHeaderName, AHeaderValue: string;
  AProtocol: TTransportProtocol): TTransportOutcome;
var
  LHandler: IMCPTransportHandler;
  LOutcome: TTransportOutcome;
begin
  LOutcome := Default(TTransportOutcome);

  LHandler := TMCPTransportHandler.Create(FServer, TStubTransportWriter.Create);
  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := Url;
      ARequest.Command := 'POST';
      ARequest.Protocol := AProtocol;
      if AHeaderValue <> '' then
        ARequest.SetHeader(AHeaderName, AHeaderValue);
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LOutcome.Code := AResponse.Code;
      LOutcome.Content := AResponse.Content;
      LOutcome.ContentType := AResponse.ContentType;
      LOutcome.Challenge := AResponse.GetHeader('WWW-Authenticate');
      LOutcome.HasChallenge := LOutcome.Challenge <> '';
    end
  );

  Result := LOutcome;
end;

function TTransportAuthTokenTest.ValidateWithConfig(const AToken: string;
  out ASubject: string): Boolean;
var
  LIdentity: TMCPAccessToken;
begin
  LIdentity := TMCPAccessToken.Create;
  try
    Result := Config.Validate(nil, AToken, LIdentity);
    ASubject := LIdentity.Subject;
  finally
    LIdentity.Free;
  end;
end;

procedure TTransportAuthTokenTest.TestStaticKey_IsAccepted;
var
  LOutcome: TTransportOutcome;
begin
  EnableHeaderKey('secret-1');

  LOutcome := Execute(KeyHeader, 'secret-1');

  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
  Assert.IsFalse(LOutcome.HasChallenge);
end;

procedure TTransportAuthTokenTest.TestMissingKey_IsChallengedWith401;
var
  LOutcome: TTransportOutcome;
begin
  EnableHeaderKey('secret-1');

  LOutcome := Execute(KeyHeader, '');

  Assert.AreEqual(401, LOutcome.Code);
  Assert.IsTrue(LOutcome.Challenge.StartsWith(ApiKeyScheme + ' '), LOutcome.Challenge);
end;

procedure TTransportAuthTokenTest.TestWrongKey_IsChallengedWith401;
var
  LOutcome: TTransportOutcome;
begin
  EnableHeaderKey('secret-1');

  LOutcome := Execute(KeyHeader, 'secret-2');

  Assert.AreEqual(401, LOutcome.Code);
  Assert.IsTrue(LOutcome.HasChallenge);
end;

procedure TTransportAuthTokenTest.TestBearerLocation_ChallengesWithBearerScheme;
var
  LOutcome: TTransportOutcome;
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken('secret-1')
    .SetTokenLocation(TAuthTokenLocation.Bearer)
  .ApplyConfig;

  LOutcome := Execute('Authorization', 'Bearer wrong');

  Assert.AreEqual(401, LOutcome.Code);
  Assert.IsTrue(LOutcome.Challenge.StartsWith('Bearer '), LOutcome.Challenge);
end;

procedure TTransportAuthTokenTest.TestEmptyKey_IsNeverAccepted;
var
  LSubject: string;
begin
  EnableHeaderKey('secret-1');

  Assert.IsFalse(ValidateWithConfig('', LSubject));
  Assert.IsTrue(ValidateWithConfig('secret-1', LSubject));
end;

procedure TTransportAuthTokenTest.TestValidatorFunc_DecidesAndFillsIdentity;
var
  LSubject: string;
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenValidator(
      function (AContext: TJRPCContext; const AToken: string;
        AIdentity: TMCPAccessToken): Boolean
      begin
        Result := AToken.StartsWith('db-');
        if Result then
          AIdentity.FromString('{"sub":"' + AToken.Substring(3) + '"}');
      end)
  .ApplyConfig;

  Assert.IsTrue(ValidateWithConfig('db-acme', LSubject));
  Assert.AreEqual('acme', LSubject);
  Assert.IsFalse(ValidateWithConfig('acme', LSubject));
  Assert.IsFalse(ValidateWithConfig('', LSubject), 'An empty token must never reach the validator');
end;

procedure TTransportAuthTokenTest.TestValidatorFunc_TakesPrecedenceOverToken;
var
  LSubject: string;
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetToken('static-key')
    .SetTokenValidator(
      function (AContext: TJRPCContext; const AToken: string;
        AIdentity: TMCPAccessToken): Boolean
      begin
        Result := AToken = 'validated-key';
      end)
  .ApplyConfig;

  Assert.IsFalse(ValidateWithConfig('static-key', LSubject));
  Assert.IsTrue(ValidateWithConfig('validated-key', LSubject));
end;

procedure TTransportAuthTokenTest.TestValidatorFunc_ExceptionIsA401;
var
  LOutcome: TTransportOutcome;
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenLocation(TAuthTokenLocation.Header)
    .SetTokenCustomHeader(KeyHeader)
    .SetTokenValidator(
      function (AContext: TJRPCContext; const AToken: string;
        AIdentity: TMCPAccessToken): Boolean
      begin
        raise Exception.Create('key store unreachable');
      end)
  .ApplyConfig;

  LOutcome := Execute(KeyHeader, 'any-key');

  Assert.AreEqual(401, LOutcome.Code);
  Assert.IsFalse(LOutcome.Content.Contains('unreachable'), 'Validator internals must not leak');
end;

procedure TTransportAuthTokenTest.TestValidatorFunc_ReceivesTheContext;
var
  LServerFound: Boolean;
  LOutcome: TTransportOutcome;
begin
  LServerFound := False;

  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenLocation(TAuthTokenLocation.Header)
    .SetTokenCustomHeader(KeyHeader)
    .SetTokenValidator(
      function (AContext: TJRPCContext; const AToken: string;
        AIdentity: TMCPAccessToken): Boolean
      begin
        LServerFound := Assigned(AContext) and Assigned(AIdentity);
        Result := True;
      end)
  .ApplyConfig;

  LOutcome := Execute(KeyHeader, 'any-key');

  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
  Assert.IsTrue(LServerFound, 'The validator must receive the request context and identity');
end;

procedure TTransportAuthTokenTest.TestValidatorClass_DecidesAndFillsIdentity;
var
  LSubject: string;
begin
  FServer.Plugin.Configure<IAuthTokenConfig>
    .SetTokenValidatorClass(TStubAuthTokenValidator)
  .ApplyConfig;

  Assert.IsTrue(ValidateWithConfig(TStubAuthTokenValidator.GoodKey, LSubject));
  Assert.AreEqual(TStubAuthTokenValidator.Subject, LSubject);
  Assert.IsFalse(ValidateWithConfig('other', LSubject));
end;

procedure TTransportAuthTokenTest.TestValidatorClass_RejectsClassWithoutInterface;
begin
  Assert.WillRaise(
    procedure
    begin
      FServer.Plugin.Configure<IAuthTokenConfig>
        .SetTokenValidatorClass(TStringList);
    end,
    EJRPCException);
end;

procedure TTransportAuthTokenTest.TestStdio_IsExempt;
var
  LOutcome: TTransportOutcome;
begin
  EnableHeaderKey('secret-1');

  LOutcome := Execute(KeyHeader, '', TTransportProtocol.Stdio);

  Assert.AreNotEqual(401, LOutcome.Code, LOutcome.Content);
end;

procedure TTransportAuthTokenTest.TestOptions_IsExempt;
var
  LHandler: IMCPTransportHandler;
  LCode: Integer;
begin
  EnableHeaderKey('secret-1');

  LCode := 0;
  LHandler := TMCPTransportHandler.Create(FServer, TStubTransportWriter.Create);
  LHandler.ProcessRequest(
    procedure (ARequest: TMCPTransportRequest)
    begin
      ARequest.Url := Url;
      ARequest.Command := 'OPTIONS';
    end,
    procedure (AResponse: TMCPTransportResponse)
    begin
      LCode := AResponse.Code;
    end
  );

  Assert.AreNotEqual(401, LCode);
end;

procedure TTransportAuthTokenTest.TestHeaderLocationWithoutName_RaisesOnApply;
begin
  Assert.WillRaise(
    procedure
    begin
      FServer.Plugin.Configure<IAuthTokenConfig>
        .SetToken('secret-1')
        .SetTokenLocation(TAuthTokenLocation.Header)
      .ApplyConfig;
    end,
    EJRPCException);
end;

procedure TTransportAuthTokenTest.TestEmptyConfig_EnforcesNothing;
begin
  Assert.AreNotEqual(401, Execute(KeyHeader, '').Code);
end;

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
  Assert.AreEqual('customer-42', FToken.Payload.GetValue<string>('sub'));
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
  Assert.AreEqual(DateTimeToUnix(LWhen), FToken.Payload.GetValue<Int64>('exp'));

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

procedure TMCPAccessTokenSetterTest.TestEmailVerified_RoundTrip;
begin
  FToken.EmailVerified := True;
  Assert.IsTrue(FToken.EmailVerified);

  FToken.EmailVerified := False;
  Assert.IsFalse(FToken.EmailVerified);
  Assert.AreEqual(1, FToken.Payload.Count);
end;

end.
