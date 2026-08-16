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
  SJoseKeyWithoutMaterial = 'The signing key publishes no usable public key';
  SJoseKeyUnreadable = 'The public key of the signing key cannot be read';
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
  ///   Both the key types used to sign access tokens are supported, read from the key
  ///   components RFC 7518 makes required - "n"/"e" for RSA, "crv"/"x"/"y" for EC -
  ///   and, for the identity providers that publish nothing else, from an "x5c"
  ///   certificate chain.
  ///   The algorithm is picked from the token header by the JOSE library, so it is
  ///   <see cref="TClaimsTokenValidator.IsAlgorithmAllowed" /> and
  ///   <see cref="TClaimsTokenValidator.KeyMatchesAlgorithm" /> - both of which have
  ///   already run by the time CheckSignature is called - that keep a token from
  ///   naming a symmetric algorithm and having this public key used as an HMAC secret.
  ///   Widening either of them here re-opens that.
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
    ///   Builds the public key to verify with, as the PEM document JOSE reads, from
    ///   the key the identity provider published. Returns an empty array when that key
    ///   carries neither its components nor a certificate chain.
    /// </summary>
    /// <remarks>
    ///   Override for an identity provider that publishes its keys in some other form:
    ///   anything that yields a PEM public key is accepted here.
    /// </remarks>
    function PublicKeyFrom(const AKey: TOAuthJsonWebKey): TBytes; virtual;

    function CheckSignature(const AHeader, APayload, ASignature: string;
      const AKey: TOAuthJsonWebKey): TTokenValidationResult; override;
  end;

{$ENDIF}

implementation

{$IFDEF DELPHI_JOSE_JWT}

uses
  JOSE.Types.Bytes,
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

function TJoseTokenValidator.PublicKeyFrom(const AKey: TOAuthJsonWebKey): TBytes;
var
  LKey: TJSONWebKey;
begin
  Result := [];

  // The components are the normative key material of RFC 7518, so they come first:
  // JOSE reads the published JWK as it stands and rebuilds the PEM from it, RSA and
  // EC alike. The entry is passed on as received - Raw - rather than reassembled from
  // the members mapped on TOAuthJsonWebKey, which are only the ones we look at.
  if AKey.HasKeyComponents then
  begin
    LKey := TJSONWebKey.FromJSON(AKey.Raw);
    try
      // The public half alone: this validator verifies signatures, it never makes them.
      Result := LKey.ToPEM(False).AsBytes;
    finally
      LKey.Free;
    end;
    Exit;
  end;

  // Publishing only a chain does not meet RFC 7518, but some identity providers do it
  // and the leaf certificate carries the same public key. The chain is ordered leaf
  // first: the rest of it only proves who issued that certificate, which is not our
  // question here.
  // Nothing about the certificate is validated - not its expiry, not its chain. That
  // is deliberate: what is being trusted is the JWKS, fetched over TLS from an issuer
  // whose identity the metadata check already established, and the certificate is only
  // the envelope its public key arrived in. A key the issuer stops publishing is gone
  // at the next refresh, which is the revocation that matters here; an expired
  // certificate around a key the issuer still publishes says nothing about the tokens.
  if Length(AKey.X5c) > 0 then
    Result := TSigningBase.PublicKeyFromCertificate(
      TEncoding.ANSI.GetBytes(CertificateToPEM(AKey.X5c[0])));
end;

function TJoseTokenValidator.CheckSignature(const AHeader, APayload, ASignature: string;
  const AKey: TOAuthJsonWebKey): TTokenValidationResult;
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
      Logger.LogError('Cannot read the public key of key "%s": %s', [AKey.Kid, E.Message]);
      Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseKeyUnreadable,
        'a readable public key', AKey.Kid));
    end;
  end;

  if Length(LPublicKey) = 0 then
    Exit(Reject(TTokenValidationErrorCode.InvalidToken, SJoseKeyWithoutMaterial,
      'a key publishing its components or an "x5c" certificate chain', AKey.Kid));

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
