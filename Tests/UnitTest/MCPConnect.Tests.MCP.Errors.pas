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
unit MCPConnect.Tests.MCP.Errors;

interface

uses
  System.SysUtils, System.Rtti, System.JSON, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Errors;

type
  /// <summary>
  ///   The MCP-reserved error codes and the "data" payloads the 2026-07-28
  ///   schema requires on them.
  /// </summary>
  [TestFixture]
  TMCPProtocolErrorTest = class(TObject)
  private
    /// <summary>
    ///   Maps AException the way the transport does and returns the "error"
    ///   member of the JSON-RPC response. Caller owns the result.
    /// </summary>
    function ErrorObjectFor(AException: Exception): TJSONObject;
  public
    [Test]
    procedure TestHeaderMismatch_HasCodeAndNoData;
    [Test]
    procedure TestHeaderMismatch_MissingHeaderVariant;
    [Test]
    procedure TestMissingCapability_HasCodeAndRequiredCapabilities;
    [Test]
    procedure TestMissingCapability_EmptySetStillEmitsData;
    [Test]
    procedure TestUnsupportedVersion_HasCodeRequestedAndSupported;
    [Test]
    procedure TestUnsupportedVersion_DefaultsToServerSupportedVersions;

    [Test]
    procedure TestAllProtocolErrorsShareTheBaseClass;

    [Test]
    [TestCase('HeaderMismatch', '-32020,True')]
    [TestCase('MissingCapability', '-32021,True')]
    [TestCase('UnsupportedVersion', '-32022,True')]
    [TestCase('ReservedUpperBound', '-32099,True')]
    [TestCase('LegacyRange', '-32019,False')]
    [TestCase('LegacyResourceNotFound', '-32002,False')]
    [TestCase('InvalidParams', '-32602,False')]
    [TestCase('BelowReserved', '-32100,False')]
    procedure TestIsMCPProtocolErrorCode(ACode: Integer; AExpected: Boolean);
  end;

  /// <summary>
  ///   The "data" member added to TJRPCErrorDetails: ownership, and the deep
  ///   copy TJRPCError.Clone has to make of it.
  /// </summary>
  [TestFixture]
  TJRPCErrorDataTest = class(TObject)
  public
    [Test]
    procedure TestPlainError_HasNoData;
    [Test]
    procedure TestClone_CopiesDataObject;
    [Test]
    procedure TestSetData_FreesThePreviousObject;
    [Test]
    procedure TestSetData_SameInstanceIsNotFreed;
  end;

implementation

{ TMCPProtocolErrorTest }

function TMCPProtocolErrorTest.ErrorObjectFor(AException: Exception): TJSONObject;
var
  LError: TJRPCError;
  LJson: TJSONObject;
begin
  LError := TJRPCError.CreateFromException(AException, TJRPCID(1));
  try
    LJson := LError.ToJsonObject;
    try
      Result := LJson.GetValue('error').Clone as TJSONObject;
    finally
      LJson.Free;
    end;
  finally
    LError.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestHeaderMismatch_HasCodeAndNoData;
var
  LException: EMCPHeaderMismatchError;
  LError: TJSONObject;
begin
  LException := EMCPHeaderMismatchError.CreateForHeader('Mcp-Method');
  try
    Assert.AreEqual(MCP_HEADER_MISMATCH, LException.Code);
    Assert.AreEqual(-32020, LException.Code, 'Renumbered from -32001 in 2026-07-28');

    LError := ErrorObjectFor(LException);
    try
      Assert.AreEqual(-32020, LError.GetValue<Integer>('code'));
      Assert.IsTrue(LError.GetValue<string>('message').Contains('Mcp-Method'));
      Assert.IsNull(LError.GetValue('data'), 'HeaderMismatchError defines no data payload');
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestHeaderMismatch_MissingHeaderVariant;
var
  LException: EMCPHeaderMismatchError;
begin
  LException := EMCPHeaderMismatchError.CreateForMissingHeader('MCP-Protocol-Version');
  try
    Assert.AreEqual(MCP_HEADER_MISMATCH, LException.Code);
    Assert.IsTrue(LException.Message.Contains('MCP-Protocol-Version'));
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestMissingCapability_HasCodeAndRequiredCapabilities;
var
  LException: EMCPMissingRequiredClientCapabilityError;
  LError, LData, LRequired: TJSONObject;
begin
  LException := EMCPMissingRequiredClientCapabilityError.CreateForCapabilities(
    [TMCPClientCapability.ElicitationForm, TMCPClientCapability.SamplingTools]);
  try
    Assert.AreEqual(-32021, LException.Code);

    LError := ErrorObjectFor(LException);
    try
      LData := LError.GetValue('data') as TJSONObject;
      Assert.IsNotNull(LData, 'data is REQUIRED on MissingRequiredClientCapabilityError');

      LRequired := LData.GetValue('requiredCapabilities') as TJSONObject;
      Assert.IsNotNull(LRequired);

      // A capability is declared by an empty object, so the nesting itself is
      // the assertion: {"elicitation":{"form":{}},"sampling":{"tools":{}}}
      Assert.IsNotNull((LRequired.GetValue('elicitation') as TJSONObject).GetValue('form'),
        'elicitation.form must survive serialization as an empty object');
      Assert.IsNotNull((LRequired.GetValue('sampling') as TJSONObject).GetValue('tools'),
        'sampling.tools must survive serialization as an empty object');
      Assert.IsNull(LRequired.GetValue('roots'), 'Capabilities not asked for must be absent');
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestMissingCapability_EmptySetStillEmitsData;
var
  LException: EMCPMissingRequiredClientCapabilityError;
  LError, LData: TJSONObject;
begin
  LException := EMCPMissingRequiredClientCapabilityError.CreateForCapabilities([]);
  try
    LError := ErrorObjectFor(LException);
    try
      LData := LError.GetValue('data') as TJSONObject;
      Assert.IsNotNull(LData, 'data stays REQUIRED even when no capability is named');
      Assert.IsNotNull(LData.GetValue('requiredCapabilities'));
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestUnsupportedVersion_HasCodeRequestedAndSupported;
var
  LException: EMCPUnsupportedProtocolVersionError;
  LError, LData: TJSONObject;
  LSupported: TJSONArray;
begin
  LException := EMCPUnsupportedProtocolVersionError.CreateForVersion(
    '1999-01-01', ['2026-07-28', '2025-11-25']);
  try
    Assert.AreEqual(-32022, LException.Code);

    LError := ErrorObjectFor(LException);
    try
      LData := LError.GetValue('data') as TJSONObject;
      Assert.IsNotNull(LData);
      Assert.AreEqual('1999-01-01', LData.GetValue<string>('requested'));

      LSupported := LData.GetValue('supported') as TJSONArray;
      Assert.IsNotNull(LSupported);
      Assert.AreEqual(2, LSupported.Count);
      Assert.AreEqual('2026-07-28', LSupported.Items[0].Value);
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestUnsupportedVersion_DefaultsToServerSupportedVersions;
var
  LException: EMCPUnsupportedProtocolVersionError;
begin
  LException := EMCPUnsupportedProtocolVersionError.CreateForVersion('2025-06-18');
  try
    Assert.AreEqual(Length(MCP_PROTOCOL_SUPPORTED_VERSIONS), Length(LException.Supported));
    Assert.AreEqual(MCP_PROTOCOL_VERSION_2026_07_28, LException.Supported[0]);
  finally
    LException.Free;
  end;
end;

procedure TMCPProtocolErrorTest.TestAllProtocolErrorsShareTheBaseClass;
begin
  // The transport maps the whole family to HTTP 400 with a single "is" test
  Assert.IsTrue(EMCPHeaderMismatchError.InheritsFrom(EMCPProtocolError));
  Assert.IsTrue(EMCPMissingRequiredClientCapabilityError.InheritsFrom(EMCPProtocolError));
  Assert.IsTrue(EMCPUnsupportedProtocolVersionError.InheritsFrom(EMCPProtocolError));
  Assert.IsTrue(EMCPProtocolError.InheritsFrom(EJRPCException));
end;

procedure TMCPProtocolErrorTest.TestIsMCPProtocolErrorCode(ACode: Integer; AExpected: Boolean);
begin
  Assert.AreEqual(AExpected, IsMCPProtocolErrorCode(ACode));
end;

{ TJRPCErrorDataTest }

procedure TJRPCErrorDataTest.TestPlainError_HasNoData;
var
  LException: EJRPCInvalidParamsError;
  LError: TJRPCError;
  LJson: TJSONObject;
begin
  // An error whose specification defines no payload must not grow one
  LException := EJRPCInvalidParamsError.Create('Resource [x] not found');
  try
    LError := TJRPCError.CreateFromException(LException, TJRPCID(1));
    try
      Assert.AreEqual(JRPC_INVALID_PARAMS, LError.Error.Code.Value);

      LJson := LError.ToJsonObject;
      try
        Assert.IsNull((LJson.GetValue('error') as TJSONObject).GetValue('data'));
      finally
        LJson.Free;
      end;
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TJRPCErrorDataTest.TestClone_CopiesDataObject;
var
  LException: EMCPUnsupportedProtocolVersionError;
  LError, LClone: TJRPCError;
  LOriginalData: TObject;
begin
  LException := EMCPUnsupportedProtocolVersionError.CreateForVersion('1999-01-01');
  try
    LError := TJRPCError.CreateFromException(LException, TJRPCID(1));
    try
      LOriginalData := LError.Error.Data.AsObject;
      Assert.IsNotNull(LOriginalData);

      LClone := LError.Clone;
      try
        Assert.IsTrue(LClone.Error.Data.IsObject, 'The clone must carry its own data');
        Assert.IsFalse(LOriginalData = LClone.Error.Data.AsObject,
          'Clone must deep-copy the data: a shared pointer would be freed twice');
        Assert.AreEqual(
          (LOriginalData as TJSONObject).ToJSON,
          (LClone.Error.Data.AsObject as TJSONObject).ToJSON);
      finally
        LClone.Free;
      end;
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

procedure TJRPCErrorDataTest.TestSetData_FreesThePreviousObject;
var
  LDetails: TJRPCErrorDetails;
begin
  // Leak-checked by DUnitX: assigning twice must not orphan the first object
  LDetails := TJRPCErrorDetails.Create;
  try
    LDetails.Data := TValue.From<TJSONObject>(TJSONObject.Create.AddPair('a', '1'));
    LDetails.Data := TValue.From<TJSONObject>(TJSONObject.Create.AddPair('b', '2'));

    Assert.AreEqual('{"b":"2"}', (LDetails.Data.AsObject as TJSONObject).ToJSON);
  finally
    LDetails.Free;
  end;
end;

procedure TJRPCErrorDataTest.TestSetData_SameInstanceIsNotFreed;
var
  LDetails: TJRPCErrorDetails;
  LData: TJSONObject;
begin
  LDetails := TJRPCErrorDetails.Create;
  try
    LData := TJSONObject.Create.AddPair('a', '1');
    LDetails.Data := TValue.From<TJSONObject>(LData);

    // Re-assigning the very same instance must not free it out from under us
    LDetails.Data := TValue.From<TJSONObject>(LData);

    Assert.AreEqual('{"a":"1"}', (LDetails.Data.AsObject as TJSONObject).ToJSON);
  finally
    LDetails.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPProtocolErrorTest);
  TDUnitX.RegisterTestFixture(TJRPCErrorDataTest);

end.
