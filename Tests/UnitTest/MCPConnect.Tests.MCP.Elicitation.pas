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
unit MCPConnect.Tests.MCP.Elicitation;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Elicitation;

type
  /// <summary>
  ///   The PrimitiveSchemaDefinition family: every variant must render the
  ///   exact shape the 2026-07-28 schema defines for it.
  /// </summary>
  [TestFixture]
  TMCPPrimitiveSchemaTest = class(TObject)
  private
    FSchema: TMCPElicitationSchema;

    /// <summary>The rendered schema of one property. Caller owns it.</summary>
    function PropertyJson(const AName: string): TJSONObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestStringSchema;
    [Test]
    [TestCase('date', '0,date')]
    [TestCase('date-time', '1,date-time')]
    [TestCase('email', '2,email')]
    [TestCase('uri', '3,uri')]
    procedure TestStringSchemaFormats(AFormat: Integer; const AExpected: string);
    [Test]
    procedure TestStringSchema_NoFormatByDefault;

    [Test]
    procedure TestIntegerSchema_BoundsHaveNoFractionalPart;
    [Test]
    procedure TestNumberSchema;
    [Test]
    procedure TestBooleanSchema;

    [Test]
    procedure TestUntitledSingleSelectEnum;
    [Test]
    procedure TestTitledSingleSelectEnum;
    [Test]
    procedure TestLegacyTitledEnum;
    [Test]
    procedure TestUntitledMultiSelectEnum;
    [Test]
    procedure TestTitledMultiSelectEnum;
    [Test]
    procedure TestEnumWithoutOptions_Raises;
  end;

  /// <summary>
  ///   The requestedSchema object itself.
  /// </summary>
  [TestFixture]
  TMCPElicitationSchemaTest = class(TObject)
  private
    FSchema: TMCPElicitationSchema;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSchemaIsAnObjectSchema;
    [Test]
    procedure TestPropertiesKeepDeclarationOrder;
    [Test]
    procedure TestRequiredListsOnlyRequiredProperties;
    [Test]
    procedure TestRequiredOmittedWhenNothingIsRequired;
    [Test]
    procedure TestDuplicatePropertyName_Raises;
    [Test]
    procedure TestEmptyPropertyName_Raises;
    [Test]
    procedure TestFind;
  end;

  /// <summary>
  ///   The form/url split of ElicitRequestParams, which one Delphi class
  ///   carries and therefore cannot enforce by declaration.
  /// </summary>
  [TestFixture]
  TMCPElicitRequestTest = class(TObject)
  public
    [Test]
    procedure TestForm_SetsModeAndSchema;
    [Test]
    procedure TestForm_NilSchema_Raises;
    [Test]
    procedure TestUrl_SetsModeAndUrl;
    [Test]
    procedure TestUrl_EmptyUrl_Raises;

    [Test]
    procedure TestValidate_AbsentModeMeansForm;
    [Test]
    procedure TestValidate_FormWithoutSchema_Raises;
    [Test]
    procedure TestValidate_UrlWithoutUrl_Raises;
    [Test]
    procedure TestValidate_UrlCarryingSchema_Raises;
    [Test]
    procedure TestValidate_UnknownMode_Raises;
  end;

implementation

{ TMCPPrimitiveSchemaTest }

procedure TMCPPrimitiveSchemaTest.Setup;
begin
  FSchema := TMCPElicitationSchema.Create;
end;

procedure TMCPPrimitiveSchemaTest.TearDown;
begin
  FSchema.Free;
end;

function TMCPPrimitiveSchemaTest.PropertyJson(const AName: string): TJSONObject;
var
  LProperty: TMCPPrimitiveSchema;
begin
  LProperty := FSchema.Find(AName);
  Assert.IsNotNull(LProperty, 'Property [' + AName + '] should be declared');

  Result := LProperty.ToJSON;
end;

procedure TMCPPrimitiveSchemaTest.TestStringSchema;
var
  LProperty: TMCPStringSchema;
  LJson: TJSONObject;
begin
  LProperty := FSchema.AddString('name', 'Your name', True);
  LProperty.Description := 'As it appears on your badge';
  LProperty.MinLength := 2;
  LProperty.MaxLength := 40;
  LProperty.DefaultValue := 'anon';

  LJson := PropertyJson('name');
  try
    Assert.AreEqual('string', LJson.GetValue<string>('type'));
    Assert.AreEqual('Your name', LJson.GetValue<string>('title'));
    Assert.AreEqual('As it appears on your badge', LJson.GetValue<string>('description'));
    Assert.AreEqual(2, LJson.GetValue<Integer>('minLength'));
    Assert.AreEqual(40, LJson.GetValue<Integer>('maxLength'));
    Assert.AreEqual('anon', LJson.GetValue<string>('default'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestStringSchemaFormats(AFormat: Integer; const AExpected: string);
var
  LJson: TJSONObject;
begin
  // TMCPStringFormat.None is 0, so the four real formats start at 1
  FSchema.AddString('f', '').Format := TMCPStringFormat(AFormat + 1);

  LJson := PropertyJson('f');
  try
    Assert.AreEqual(AExpected, LJson.GetValue<string>('format'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestStringSchema_NoFormatByDefault;
var
  LJson: TJSONObject;
begin
  FSchema.AddString('f', '');

  LJson := PropertyJson('f');
  try
    Assert.IsNull(LJson.GetValue('format'), 'TMCPStringFormat.None must emit no "format"');
    Assert.IsNull(LJson.GetValue('title'), 'An empty title must not be emitted');
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestIntegerSchema_BoundsHaveNoFractionalPart;
var
  LProperty: TMCPNumberSchema;
  LJson: TJSONObject;
begin
  LProperty := FSchema.AddInteger('age', 'Age');
  LProperty.Minimum := 0;
  LProperty.Maximum := 130;
  LProperty.DefaultValue := 18;

  LJson := PropertyJson('age');
  try
    Assert.AreEqual('integer', LJson.GetValue<string>('type'));

    // "minimum": 0, not "minimum": 0.0
    Assert.AreEqual('0', LJson.GetValue('minimum').ToJSON);
    Assert.AreEqual('130', LJson.GetValue('maximum').ToJSON);
    Assert.AreEqual('18', LJson.GetValue('default').ToJSON);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestNumberSchema;
var
  LProperty: TMCPNumberSchema;
  LJson: TJSONObject;
begin
  LProperty := FSchema.AddNumber('rating', 'Rating');
  LProperty.DefaultValue := 3.5;

  LJson := PropertyJson('rating');
  try
    Assert.AreEqual('number', LJson.GetValue<string>('type'));
    Assert.AreEqual(3.5, LJson.GetValue<Double>('default'), 0.0001);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestBooleanSchema;
var
  LJson: TJSONObject;
begin
  FSchema.AddBoolean('subscribe', 'Subscribe?').DefaultValue := True;

  LJson := PropertyJson('subscribe');
  try
    Assert.AreEqual('boolean', LJson.GetValue<string>('type'));
    Assert.IsTrue(LJson.GetValue<Boolean>('default'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestUntitledSingleSelectEnum;
var
  LJson: TJSONObject;
  LEnum: TJSONArray;
begin
  FSchema.AddEnum('lang', 'Language', ['delphi', 'pascal']);

  LJson := PropertyJson('lang');
  try
    Assert.AreEqual('string', LJson.GetValue<string>('type'));

    LEnum := LJson.GetValue('enum') as TJSONArray;
    Assert.IsNotNull(LEnum, 'Options with no titles render as a bare "enum"');
    Assert.AreEqual(2, LEnum.Count);
    Assert.AreEqual('delphi', LEnum.Items[0].Value);
    Assert.IsNull(LJson.GetValue('oneOf'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestTitledSingleSelectEnum;
var
  LJson: TJSONObject;
  LOneOf: TJSONArray;
  LEntry: TJSONObject;
begin
  FSchema.AddEnum('level', 'Level', []).AddOption('b', 'Beginner').AddOption('a', 'Advanced');

  LJson := PropertyJson('level');
  try
    Assert.AreEqual('string', LJson.GetValue<string>('type'));
    Assert.IsNull(LJson.GetValue('enum'), 'A titled enum renders as "oneOf", not "enum"');

    LOneOf := LJson.GetValue('oneOf') as TJSONArray;
    Assert.IsNotNull(LOneOf);
    Assert.AreEqual(2, LOneOf.Count);

    LEntry := LOneOf.Items[0] as TJSONObject;
    Assert.AreEqual('b', LEntry.GetValue<string>('const'));
    Assert.AreEqual('Beginner', LEntry.GetValue<string>('title'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestLegacyTitledEnum;
var
  LProperty: TMCPEnumSchema;
  LJson: TJSONObject;
  LNames: TJSONArray;
begin
  LProperty := FSchema.AddEnum('legacy', 'Legacy', []);
  LProperty.AddOption('x', 'Ex').AddOption('y', 'Why');
  LProperty.Legacy := True;

  LJson := PropertyJson('legacy');
  try
    // The deprecated shape: parallel "enum" and "enumNames" instead of "oneOf"
    Assert.IsNotNull(LJson.GetValue('enum'));
    Assert.IsNull(LJson.GetValue('oneOf'));

    LNames := LJson.GetValue('enumNames') as TJSONArray;
    Assert.IsNotNull(LNames);
    Assert.AreEqual('Ex', LNames.Items[0].Value);
    Assert.AreEqual('Why', LNames.Items[1].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestUntitledMultiSelectEnum;
var
  LProperty: TMCPMultiEnumSchema;
  LJson, LItems: TJSONObject;
  LDefault: TJSONArray;
begin
  LProperty := FSchema.AddMultiEnum('tags', 'Tags', ['a', 'b', 'c']);
  LProperty.MinItems := 1;
  LProperty.MaxItems := 2;
  LProperty.DefaultValue := ['a'];

  LJson := PropertyJson('tags');
  try
    Assert.AreEqual('array', LJson.GetValue<string>('type'));

    LItems := LJson.GetValue('items') as TJSONObject;
    Assert.IsNotNull(LItems);
    Assert.AreEqual('string', LItems.GetValue<string>('type'));
    Assert.AreEqual(3, (LItems.GetValue('enum') as TJSONArray).Count);

    Assert.AreEqual(1, LJson.GetValue<Integer>('minItems'));
    Assert.AreEqual(2, LJson.GetValue<Integer>('maxItems'));

    LDefault := LJson.GetValue('default') as TJSONArray;
    Assert.IsNotNull(LDefault);
    Assert.AreEqual('a', LDefault.Items[0].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestTitledMultiSelectEnum;
var
  LJson, LItems, LEntry: TJSONObject;
  LAnyOf: TJSONArray;
begin
  FSchema.AddMultiEnum('perms', 'Permissions', []).AddOption('r', 'Read').AddOption('w', 'Write');

  LJson := PropertyJson('perms');
  try
    Assert.AreEqual('array', LJson.GetValue<string>('type'));

    LItems := LJson.GetValue('items') as TJSONObject;
    Assert.IsNull(LItems.GetValue('enum'), 'A titled multi-select renders items.anyOf');

    LAnyOf := LItems.GetValue('anyOf') as TJSONArray;
    Assert.IsNotNull(LAnyOf);
    Assert.AreEqual(2, LAnyOf.Count);

    LEntry := LAnyOf.Items[1] as TJSONObject;
    Assert.AreEqual('w', LEntry.GetValue<string>('const'));
    Assert.AreEqual('Write', LEntry.GetValue<string>('title'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestEnumWithoutOptions_Raises;
begin
  FSchema.AddEnum('empty', 'Empty', []);

  Assert.WillRaise(
    procedure
    var
      LJson: TJSONObject;
    begin
      LJson := FSchema.Find('empty').ToJSON;
      LJson.Free;
    end,
    EMCPException, 'An enum with no options cannot render a valid schema');
end;

{ TMCPElicitationSchemaTest }

procedure TMCPElicitationSchemaTest.Setup;
begin
  FSchema := TMCPElicitationSchema.Create;
end;

procedure TMCPElicitationSchemaTest.TearDown;
begin
  FSchema.Free;
end;

procedure TMCPElicitationSchemaTest.TestSchemaIsAnObjectSchema;
var
  LJson: TJSONObject;
begin
  FSchema.AddString('a', 'A');

  LJson := FSchema.ToJSON;
  try
    Assert.AreEqual('object', LJson.GetValue<string>('type'));
    Assert.IsNotNull(LJson.GetValue('properties'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPElicitationSchemaTest.TestPropertiesKeepDeclarationOrder;
var
  LJson, LProperties: TJSONObject;
begin
  FSchema.AddString('first', '');
  FSchema.AddString('second', '');
  FSchema.AddString('third', '');

  LJson := FSchema.ToJSON;
  try
    LProperties := LJson.GetValue('properties') as TJSONObject;
    Assert.AreEqual(3, LProperties.Count);

    // The form is rendered in this order, so declaration order is the API
    Assert.AreEqual('first', LProperties.Pairs[0].JsonString.Value);
    Assert.AreEqual('second', LProperties.Pairs[1].JsonString.Value);
    Assert.AreEqual('third', LProperties.Pairs[2].JsonString.Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPElicitationSchemaTest.TestRequiredListsOnlyRequiredProperties;
var
  LJson: TJSONObject;
  LRequired: TJSONArray;
begin
  FSchema.AddString('name', 'Name', True);
  FSchema.AddString('nickname', 'Nickname');
  FSchema.AddBoolean('agree', 'Agree', True);

  LJson := FSchema.ToJSON;
  try
    LRequired := LJson.GetValue('required') as TJSONArray;
    Assert.IsNotNull(LRequired);
    Assert.AreEqual(2, LRequired.Count);
    Assert.AreEqual('name', LRequired.Items[0].Value);
    Assert.AreEqual('agree', LRequired.Items[1].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPElicitationSchemaTest.TestRequiredOmittedWhenNothingIsRequired;
var
  LJson: TJSONObject;
begin
  FSchema.AddString('optional', '');

  LJson := FSchema.ToJSON;
  try
    Assert.IsNull(LJson.GetValue('required'),
      'An empty "required" would read as a deliberate choice rather than absence');
  finally
    LJson.Free;
  end;
end;

procedure TMCPElicitationSchemaTest.TestDuplicatePropertyName_Raises;
begin
  FSchema.AddString('name', 'Name');

  Assert.WillRaise(
    procedure
    begin
      FSchema.AddInteger('name', 'Name again');
    end,
    EMCPException);
end;

procedure TMCPElicitationSchemaTest.TestEmptyPropertyName_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      FSchema.AddString('', 'No name');
    end,
    EMCPException);
end;

procedure TMCPElicitationSchemaTest.TestFind;
begin
  FSchema.AddString('name', 'Name');

  Assert.IsNotNull(FSchema.Find('name'));
  Assert.IsNull(FSchema.Find('Name'), 'Property names are JSON members: case-sensitive');
  Assert.IsNull(FSchema.Find('missing'));
end;

{ TMCPElicitRequestTest }

procedure TMCPElicitRequestTest.TestForm_SetsModeAndSchema;
var
  LSchema: TMCPElicitationSchema;
  LParams: TElicitRequestParams;
begin
  LSchema := TMCPElicitationSchema.Create;
  try
    LSchema.AddString('name', 'Name', True);

    LParams := TMCPElicitRequest.Form('Tell us your name', LSchema);
    try
      Assert.AreEqual('Tell us your name', LParams.Message);
      Assert.AreEqual(MCP_ELICIT_MODE_FORM, LParams.Mode.Value);
      Assert.IsNotNull(LParams.RequestedSchema.GetValue('properties'));
      Assert.IsFalse(LParams.Url.HasValue);

      // The caller keeps its schema: the params got a rendered copy
      Assert.IsNotNull(LSchema.Find('name'));
    finally
      LParams.Free;
    end;
  finally
    LSchema.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestForm_NilSchema_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMCPElicitRequest.Form('No schema', nil).Free;
    end,
    EMCPException);
end;

procedure TMCPElicitRequestTest.TestUrl_SetsModeAndUrl;
var
  LParams: TElicitRequestParams;
begin
  LParams := TMCPElicitRequest.Url('Finish sign-in', 'https://example.com/auth');
  try
    Assert.AreEqual(MCP_ELICIT_MODE_URL, LParams.Mode.Value);
    Assert.AreEqual('https://example.com/auth', LParams.Url.Value);
    Assert.AreEqual(0, LParams.RequestedSchema.Count, 'A url elicitation carries no schema');
  finally
    LParams.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestUrl_EmptyUrl_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMCPElicitRequest.Url('No url', '').Free;
    end,
    EMCPException);
end;

procedure TMCPElicitRequestTest.TestValidate_AbsentModeMeansForm;
var
  LParams: TElicitRequestParams;
begin
  LParams := TElicitRequestParams.Create;
  try
    LParams.Message := 'Mode left absent';
    LParams.RequestedSchema.AddPair('type', 'object');

    // No mode at all is legal and means "form", so this must pass
    TMCPElicitRequest.Validate(LParams);
    Assert.Pass;
  finally
    LParams.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestValidate_FormWithoutSchema_Raises;
var
  LParams: TElicitRequestParams;
begin
  LParams := TElicitRequestParams.Create;
  try
    LParams.Message := 'Form with nothing to fill in';

    Assert.WillRaise(
      procedure
      begin
        TMCPElicitRequest.Validate(LParams);
      end,
      EMCPException);
  finally
    LParams.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestValidate_UrlWithoutUrl_Raises;
var
  LParams: TElicitRequestParams;
begin
  LParams := TElicitRequestParams.Create;
  try
    LParams.Message := 'Url mode, no url';
    LParams.Mode := MCP_ELICIT_MODE_URL;

    Assert.WillRaise(
      procedure
      begin
        TMCPElicitRequest.Validate(LParams);
      end,
      EMCPException);
  finally
    LParams.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestValidate_UrlCarryingSchema_Raises;
var
  LParams: TElicitRequestParams;
begin
  LParams := TElicitRequestParams.Create;
  try
    LParams.Message := 'Url mode with a form schema';
    LParams.Mode := MCP_ELICIT_MODE_URL;
    LParams.Url := 'https://example.com';
    LParams.RequestedSchema.AddPair('type', 'object');

    Assert.WillRaise(
      procedure
      begin
        TMCPElicitRequest.Validate(LParams);
      end,
      EMCPException);
  finally
    LParams.Free;
  end;
end;

procedure TMCPElicitRequestTest.TestValidate_UnknownMode_Raises;
var
  LParams: TElicitRequestParams;
begin
  LParams := TElicitRequestParams.Create;
  try
    LParams.Message := 'Unknown mode';
    LParams.Mode := 'telepathy';

    Assert.WillRaise(
      procedure
      begin
        TMCPElicitRequest.Validate(LParams);
      end,
      EMCPException);
  finally
    LParams.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPPrimitiveSchemaTest);
  TDUnitX.RegisterTestFixture(TMCPElicitationSchemaTest);
  TDUnitX.RegisterTestFixture(TMCPElicitRequestTest);

end.
