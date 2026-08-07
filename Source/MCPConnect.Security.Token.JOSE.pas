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
unit MCPConnect.Security.Token.JOSE;

{$I 'MCPConnect.inc' }

interface

{$IFDEF DELPHI_JOSE_JWT}

uses
  System.SysUtils, System.Classes,

  MCPConnect.Security.Jwks,
  MCPConnect.Security.Token;

resourcestring
  SJoseKeyWithoutCertificate = 'The signing key does not publish a certificate chain';
  SJoseCertificateUnreadable = 'The signing certificate cannot be read';
  SJoseSignatureInvalid = 'The token signature does not verify';

type
  /// <summary>
  ///   Token validator that completes <see cref="TClaimsTokenValidator" /> with the
  ///   one check it leaves open: the signature, verified with the
  ///   <see href="https://github.com/paolo-rossi/delphi-jose-jwt">Delphi JOSE</see>
  ///   library against the public key the identity provider publishes.
  /// </summary>
  /// <remarks>
  ///   This is the validator to register on a server exposed to untrusted clients: a
  ///   token that reaches a tool has been proven to come from a trusted issuer, for
  ///   this resource, within its validity window, signed by a key that issuer
  ///   publishes.
  ///   The public key is taken from the "x5c" certificate chain of the JSON Web Key.
  ///   Keys published as bare RSA parameters ("n"/"e") without a certificate are
  ///   rejected rather than silently accepted - see the remark on
  ///   <see cref="PublicKeyFrom" /> if your identity provider does that.
  ///   JOSE signing needs the OpenSSL libraries at run time; they are not required to
  ///   compile.
  /// </remarks>
  TJoseTokenValidator = class(TClaimsTokenValidator)
  protected
    /// <summary>
    ///   Wraps the first certificate of a key's "x5c" chain in PEM armour, the form
    ///   OpenSSL reads. The base64 is rewrapped at 64 characters per line: "x5c"
    ///   publishes it as one long line, which is valid JSON but not canonical PEM.
    /// </summary>
    class function CertificateToPEM(const ACertificate: string): string; static;

    /// <summary>
    ///   Extracts the public key to verify with, from the certificate chain of AKey.
    ///   Returns an empty array when the key publishes no chain.
    /// </summary>
    /// <remarks>
    ///   Override to support an identity provider that publishes only the raw key
    ///   parameters: building an RSA public key out of "n" and "e" is possible, but it
    ///   means assembling DER by hand, which does not belong in this class.
    /// </remarks>
    function PublicKeyFrom(const AKey: TJsonWebKey): TBytes; virtual;

    function CheckSignature(const AHeader, APayload, ASignature: string;
      const AKey: TJsonWebKey): TTokenValidationResult; override;
  end;

{$ENDIF}

implementation

{$IFDEF DELPHI_JOSE_JWT}

uses
  System.StrUtils,

  JOSE.Core.Builder,
  JOSE.Core.JWK,
  JOSE.Core.JWT,
  JOSE.Signing.Base,
  Logify;

const
  PEM_LINE_LENGTH = 64;
  PEM_CERTIFICATE_BEGIN = '-----BEGIN CERTIFICATE-----';
  PEM_CERTIFICATE_END = '-----END CERTIFICATE-----';

{ TJoseTokenValidator }

class function TJoseTokenValidator.CertificateToPEM(const ACertificate: string): string;
var
  LBody: TStringBuilder;
  LIndex: Integer;
begin
  LBody := TStringBuilder.Create;
  try
    LBody.AppendLine(PEM_CERTIFICATE_BEGIN);

    LIndex := 1;
    while LIndex <= Length(ACertificate) do
    begin
      LBody.AppendLine(Copy(ACertificate, LIndex, PEM_LINE_LENGTH));
      Inc(LIndex, PEM_LINE_LENGTH);
    end;

    LBody.AppendLine(PEM_CERTIFICATE_END);
    Result := LBody.ToString;
  finally
    LBody.Free;
  end;
end;

function TJoseTokenValidator.PublicKeyFrom(const AKey: TJsonWebKey): TBytes;
begin
  Result := [];
  if Length(AKey.X5c) = 0 then
    Exit;

  // The chain is ordered leaf first: the token is signed with that certificate, the
  // rest of the chain only proves who issued it, which is not our question here.
  Result := TSigningBase.PublicKeyFromCertificate(
    TEncoding.ANSI.GetBytes(CertificateToPEM(AKey.X5c[0])));
end;

function TJoseTokenValidator.CheckSignature(const AHeader, APayload, ASignature: string;
  const AKey: TJsonWebKey): TTokenValidationResult;
var
  LPublicKey: TBytes;
  LJWK: TJWK;
  LJWT: TJWT;
begin
  try
    LPublicKey := PublicKeyFrom(AKey);
  except
    on E: Exception do
    begin
      Logger.LogError('Cannot read the certificate of key "%s": %s', [AKey.Kid, E.Message]);
      Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseCertificateUnreadable,
        'a readable X.509 certificate', AKey.Kid));
    end;
  end;

  if Length(LPublicKey) = 0 then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseKeyWithoutCertificate,
      'a key publishing an "x5c" certificate chain', AKey.Kid));

  LJWK := TJWK.Create(LPublicKey);
  try
    try
      // Rebuilt from the segments as received: the signature covers those exact
      // bytes, so anything re-encoded from the decoded JSON would never verify.
      LJWT := TJOSE.Verify(LJWK, AHeader + '.' + APayload + '.' + ASignature);
    except
      // An algorithm JOSE cannot handle, or OpenSSL missing at run time: neither is
      // a reason to let a token through.
      on E: Exception do
      begin
        Logger.LogError('Signature verification failed for key "%s": %s', [AKey.Kid, E.Message]);
        Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseSignatureInvalid,
          'a verifiable signature', E.ClassName));
      end;
    end;

    try
      if not LJWT.Verified then
        Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseSignatureInvalid,
          Format('a signature made with key "%s"', [AKey.Kid]), 'one that does not verify'));

      Result := TTokenValidationResult.Ok;
    finally
      LJWT.Free;
    end;
  finally
    LJWK.Free;
  end;
end;

{$ENDIF}

end.
