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
unit MCPConnect.Tests.Security.Token.JOSE;

{$I 'MCPConnect.inc' }

interface

{$IFDEF DELPHI_JOSE_JWT}

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  JRPC.Core,
  MCPConnect.MCP.Server,
  MCPConnect.Configuration.Core,
  MCPConnect.Configuration.Auth,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Security.Jwks,
  MCPConnect.Security.Token,
  MCPConnect.Security.Token.JOSE,

  MCPConnect.Tests.Security.Token;

type
  /// <summary>Exposes the protected members the tests need to reach.</summary>
  TJoseValidatorAccess = class(TJoseTokenValidator);

  /// <summary>
  ///   A validator whose key material always reaches the JOSE layer: the public key
  ///   is handed over as armoured bytes JOSE holds as a key and cannot verify with,
  ///   so the failure happens inside the library rather than before it.
  /// </summary>
  TUnprocessableKeyValidator = class(TJoseTokenValidator)
  protected
    function PublicKeyFrom(const AKey: TOAuthJsonWebKey): TBytes; override;
  end;

  /// <summary>
  ///   What can be verified without OpenSSL on the machine running the suite: the
  ///   PEM assembly, which of the published members a key is read from, and that a key
  ///   which cannot yield a public key is rejected rather than waved through. The
  ///   cryptographic verification itself is exercised against a live identity provider,
  ///   not here.
  /// </summary>
  [TestFixture]
  TJoseTokenValidatorTest = class(TObject)
  private const
    Issuer = 'https://idp.example.com';
    Audience = 'https://mcp.example.com/mcp';
    KeyId = 'key-1';
  private
    FFake: TFakeMetadataProvider;
    FServer: TMCPServer;
    FConfig: IOAuthConfig;
    FContext: TJRPCContext;
    FAccessToken: TMCPAccessToken;

    procedure PublishKey(const AKeyJSON: string);
    function ValidateValidTokenWith(AValidatorClass: TClass): TTokenValidationResult;
    function ValidateValidToken: TTokenValidationResult;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegister_IsAcceptedAsATokenValidator;
    [Test]
    procedure TestRegister_DeclaresThatItVerifiesSignatures;
    [Test]
    procedure TestCertificateToPEM_IsWrappedInArmourAtSixtyFourColumns;
    [Test]
    procedure TestCertificateToPEM_PreservesTheCertificate;
    [Test]
    procedure TestValidate_KeyWithoutKeyMaterialIsRejected;
    [Test]
    procedure TestValidate_KeyWithBareComponentsReachesTheSignatureCheck;
    [Test]
    procedure TestValidate_UnreadableCertificateIsRejected;
    [Test]
    procedure TestValidate_KeyTheJoseLayerCannotProcessIsRejectedNotRaised;
  end;

{$ENDIF}

implementation

{$IFDEF DELPHI_JOSE_JWT}

uses
  System.DateUtils, System.NetEncoding;

function Base64UrlEncode(const AValue: string): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(AValue));
  Result := Result.Replace(#13, '').Replace(#10, '')
    .Replace('+', '-').Replace('/', '_').TrimRight(['=']);
end;

{ TUnprocessableKeyValidator }

function TUnprocessableKeyValidator.PublicKeyFrom(const AKey: TOAuthJsonWebKey): TBytes;
begin
  // PEM armour around bytes that are not a public key: enough to get past the
  // "no key material" rejection and reach TJOSE.Verify, never enough to verify with.
  Result := TEncoding.ANSI.GetBytes(
    '-----BEGIN PUBLIC KEY-----'#13#10 +
    'bm90LWEtcHVibGljLWtleQ=='#13#10 +
    '-----END PUBLIC KEY-----'#13#10);
end;

{ TJoseTokenValidatorTest }

procedure TJoseTokenValidatorTest.Setup;
begin
  FFake := TFakeMetadataProvider.Create;
  FFake.SetDocument(Issuer + '/.well-known/openid-configuration',
    Format('{"issuer":"%s","jwks_uri":"%s/keys"}', [Issuer, Issuer]));

  FServer := TMCPServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IOAuthConfig>
    .SetResource(Audience)
    .AddAuthorizationServer(Issuer)
    .SetMetadataProvider(FFake);

  FContext := TJRPCContext.Create;
  AddApplicationToContext(FContext, FServer);

  FAccessToken := TMCPAccessToken.Create;
end;

procedure TJoseTokenValidatorTest.TearDown;
begin
  FAccessToken.Free;
  FContext.Free;
  FConfig := nil;
  FServer.Free;
  FFake := nil;
end;

procedure TJoseTokenValidatorTest.PublishKey(const AKeyJSON: string);
begin
  FFake.SetDocument(Issuer + '/keys', Format('{"keys":[%s]}', [AKeyJSON]));
end;

function TJoseTokenValidatorTest.ValidateValidTokenWith(
  AValidatorClass: TClass): TTokenValidationResult;
var
  LToken: string;
begin
  // Every claim is in order: whatever the outcome, it comes from the signature step.
  LToken :=
    Base64UrlEncode(Format('{"alg":"RS256","kid":"%s"}', [KeyId])) + '.' +
    Base64UrlEncode(Format('{"iss":"%s","aud":"%s","sub":"user-42","exp":%d}',
      [Issuer, Audience, DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now), True) + 3600])) + '.' +
    Base64UrlEncode('not-a-real-signature');

  Result := RunValidator(AValidatorClass, FContext, LToken, FAccessToken);
end;

function TJoseTokenValidatorTest.ValidateValidToken: TTokenValidationResult;
begin
  Result := ValidateValidTokenWith(TJoseTokenValidator);
end;

procedure TJoseTokenValidatorTest.TestRegister_IsAcceptedAsATokenValidator;
begin
  FConfig.SetTokenValidatorClass(TJoseTokenValidator);

  Assert.IsTrue(FServer.GetConfiguration<TOAuthConfig>.TokenValidatorClass = TJoseTokenValidator);
end;

procedure TJoseTokenValidatorTest.TestRegister_DeclaresThatItVerifiesSignatures;
begin
  // What keeps this one out of the startup warning that TClaimsTokenValidator earns:
  // it is the validator in this library that actually verifies a signature, and it
  // has to say so where the configuration can read it.
  Assert.IsTrue(TJoseTokenValidator.VerifiesSignature);
  Assert.IsFalse(TClaimsTokenValidator.VerifiesSignature,
    'The base it derives from checks no signature and must not claim to');
end;

procedure TJoseTokenValidatorTest.TestCertificateToPEM_IsWrappedInArmourAtSixtyFourColumns;
var
  LPEM: TStringList;
  I: Integer;
begin
  LPEM := TStringList.Create;
  try
    LPEM.Text := TJoseValidatorAccess.CertificateToPEM(StringOfChar('A', 150));

    Assert.AreEqual('-----BEGIN CERTIFICATE-----', LPEM[0]);
    Assert.AreEqual('-----END CERTIFICATE-----', LPEM[LPEM.Count - 1]);

    // "x5c" publishes the certificate as a single long line; OpenSSL reads canonical
    // PEM, so the body is rewrapped.
    for I := 1 to LPEM.Count - 2 do
      Assert.IsTrue(Length(LPEM[I]) <= 64, Format('Line %d is %d characters long', [I, Length(LPEM[I])]));
  finally
    LPEM.Free;
  end;
end;

procedure TJoseTokenValidatorTest.TestCertificateToPEM_PreservesTheCertificate;
var
  LCertificate: string;
  LPEM: TStringList;
  LBody: string;
  I: Integer;
begin
  LCertificate := StringOfChar('A', 100) + StringOfChar('B', 45);

  LPEM := TStringList.Create;
  try
    LPEM.Text := TJoseValidatorAccess.CertificateToPEM(LCertificate);

    LBody := '';
    for I := 1 to LPEM.Count - 2 do
      LBody := LBody + LPEM[I];

    Assert.AreEqual(LCertificate, LBody, 'Wrapping must not lose or alter a character');
  finally
    LPEM.Free;
  end;
end;

procedure TJoseTokenValidatorTest.TestValidate_KeyWithoutKeyMaterialIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // Neither the components nor a chain: there is nothing to build a public key from,
  // and "cannot verify" must never collapse into "verified".
  PublishKey(Format('{"kid":"%s","kty":"RSA","use":"sig"}', [KeyId]));

  LResult := ValidateValidToken;

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
  Assert.AreEqual(SJoseKeyWithoutMaterial, LResult.ErrorDescription);
  Assert.AreEqual('', FAccessToken.Subject);
end;

procedure TJoseTokenValidatorTest.TestValidate_KeyWithBareComponentsReachesTheSignatureCheck;
var
  LResult: TTokenValidationResult;
begin
  // "n"/"e" without an "x5c" is what RFC 7518 requires and what most identity providers
  // publish. The token is still rejected - its signature is made up - but it must be
  // rejected by the signature check, not for lack of a certificate.
  PublishKey(Format('{"kid":"%s","kty":"RSA","use":"sig","n":"AQIDBAUG","e":"AQAB"}', [KeyId]));

  LResult := ValidateValidToken;

  Assert.IsFalse(LResult.Success);
  Assert.AreNotEqual(SJoseKeyWithoutMaterial, LResult.ErrorDescription,
    'The key components must be read as key material');
  Assert.AreEqual('', FAccessToken.Subject);
end;

procedure TJoseTokenValidatorTest.TestValidate_UnreadableCertificateIsRejected;
var
  LResult: TTokenValidationResult;
begin
  // No components, so the chain is what the key is read from - and this one is a
  // well-formed key whose certificate is not one. Whether OpenSSL is installed decides
  // where this fails, never whether the token is accepted.
  PublishKey(Format('{"kid":"%s","kty":"RSA","use":"sig","x5c":["bm90LWEtY2VydGlmaWNhdGU="]}',
    [KeyId]));

  LResult := ValidateValidToken;

  Assert.IsFalse(LResult.Success);
  Assert.AreEqual(SJoseKeyUnreadable, LResult.ErrorDescription);
  Assert.AreEqual('', FAccessToken.Subject);
end;

procedure TJoseTokenValidatorTest.TestValidate_KeyTheJoseLayerCannotProcessIsRejectedNotRaised;
var
  LResult: TTokenValidationResult;
begin
  // TJOSE.Verify does not raise when verification fails inside the library: it frees
  // the token, swallows the exception and answers nil. A validator that read the
  // result of that unguarded would raise an access violation where its contract
  // promises a record - and a host calling a validator outside the OAuth middleware
  // has no blanket "except" to turn that back into a rejection.
  PublishKey(Format('{"kid":"%s","kty":"RSA","use":"sig","n":"AQIDBAUG","e":"AQAB"}', [KeyId]));

  try
    LResult := ValidateValidTokenWith(TUnprocessableKeyValidator);
  except
    on E: Exception do
      raise Exception.CreateFmt(
        'Validate must answer a failure record, not raise %s: %s', [E.ClassName, E.Message]);
  end;

  Assert.IsFalse(LResult.Success);
  Assert.IsTrue(LResult.ErrorCode = TTokenValidationErrorCode.InvalidToken);
  Assert.AreEqual(SJoseSignatureInvalid, LResult.ErrorDescription);
  Assert.AreEqual('', FAccessToken.Subject);
end;

{$ENDIF}

initialization

{$IFDEF DELPHI_JOSE_JWT}
  TDUnitX.RegisterTestFixture(TJoseTokenValidatorTest);
{$ENDIF}

end.
