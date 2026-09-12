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

  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence.JSON.Schema,

  MCPConnect.MCP.Types.Base,
  MCPConnect.MCP.Types.Elicitation;

type
  /// <summary>
  ///   A choice whose option values are the member names under the configured
  ///   case, which MCPNeonConfig makes camelCase.
  /// </summary>
  TTestLang = (Delphi, Pascal, Basic);

  /// <summary>
  ///   A choice that names its own options, the only thing on a Delphi enum that
  ///   can: what [NeonEnumNames] says is what the schema offers and what the
  ///   client sends back.
  /// </summary>
  [NeonEnumNames('delphi,free-pascal,basic')]
  TTestNamedLang = (NamedDelphi, NamedPascal, NamedBasic);

  [NeonEnumNames('read,write,admin')]
  TTestPerm = (PermRead, PermWrite, PermAdmin);

  /// <summary>A multiple choice: a set of the type above.</summary>
  TTestPerms = set of TTestPerm;

  /// <summary>
  ///   What a form asks for, declared once: the schema comes from this RTTI and
  ///   so does the answer.
  /// </summary>
  TTestSignup = record
    [JsonSchema('title=Your name, description=As on your badge, required, minLength=2')]
    Name: string;
    [JsonSchema('title=Your age, minimum=0, maximum=130')]
    Age: Integer;
    [JsonSchema('title=Language')]
    Lang: TTestLang;
    [JsonSchema('title=Permissions')]
    Perms: TTestPerms;
    [JsonSchema('title=Subscribe?')]
    Subscribe: Boolean;
  end;

  /// <summary>A class asks for the same thing as a record.</summary>
  TTestSignupClass = class
  public
    [JsonSchema('title=Your name, required')]
    Name: string;
  end;

  /// <summary>A nested structure: not a primitive an elicitation may ask for.</summary>
  TTestNested = record
    [JsonSchema('title=Who')]
    Who: TTestSignup;
  end;

  /// <summary>
  ///   A Nullable member, which renders as a union of types - optionality is
  ///   "required", not "null".
  /// </summary>
  TTestNullableAsk = record
    [JsonSchema('title=Nickname')]
    Nickname: NullString;
  end;

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
    procedure TestSingleSelectEnumUsesTheNeonNames;
    [Test]
    procedure TestTitledSingleSelectEnum;
    [Test]
    procedure TestLegacyTitledEnum;
    [Test]
    procedure TestUntitledMultiSelectEnum;
    [Test]
    procedure TestTitledMultiSelectEnum;
    [Test]
    procedure TestChoiceNeedsAnEnumType_Raises;
    [Test]
    procedure TestMultiChoiceNeedsASetType_Raises;
    [Test]
    procedure TestTypeWithoutOptions_Raises;
  end;

  /// <summary>
  ///   The requestedSchema generated from a Delphi type: Neon writes the
  ///   document, this class only refuses what an elicitation may not ask for.
  /// </summary>
  [TestFixture]
  TMCPTypeSchemaTest = class(TObject)
  private
    /// <summary>The rendered schema of T. Caller owns it.</summary>
    function SchemaOf<T>: TJSONObject;
    /// <summary>The rendered schema of one member of T. Caller owns nothing.</summary>
    function MemberOf(AJson: TJSONObject; const AName: string): TJSONObject;
  public
    [Test]
    procedure TestRecordIsAFlatObjectSchema;
    [Test]
    procedure TestMemberKeywordsComeFromTheAttribute;
    [Test]
    procedure TestRequiredComesFromTheAttribute;
    [Test]
    procedure TestEnumMemberIsASingleChoice;
    [Test]
    procedure TestSetMemberIsAMultipleChoice;
    [Test]
    procedure TestClassIsAskedForLikeARecord;
    [Test]
    procedure TestNestedStructure_Raises;
    [Test]
    procedure TestNullableMember_Raises;
    [Test]
    procedure TestNonStructType_Raises;
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
    procedure TestFormOfType_SetsModeAndSchema;
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
  FSchema.AddEnum<TTestLang>('lang', 'Language');

  LJson := PropertyJson('lang');
  try
    Assert.AreEqual('string', LJson.GetValue<string>('type'));

    LEnum := LJson.GetValue('enum') as TJSONArray;
    Assert.IsNotNull(LEnum, 'The plain shape is a bare "enum"');
    Assert.AreEqual(3, LEnum.Count);

    // The names Neon writes, which is what the client has to send back:
    // MCPNeonConfig is camelCase
    Assert.AreEqual('delphi', LEnum.Items[0].Value);
    Assert.AreEqual('pascal', LEnum.Items[1].Value);
    Assert.IsNull(LJson.GetValue('oneOf'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestSingleSelectEnumUsesTheNeonNames;
var
  LJson: TJSONObject;
  LEnum: TJSONArray;
begin
  FSchema.AddEnum<TTestNamedLang>('lang', 'Language');

  LJson := PropertyJson('lang');
  try
    LEnum := LJson.GetValue('enum') as TJSONArray;

    // [NeonEnumNames] wins over the case conversion, for the reader and the
    // writer as much as for the schema
    Assert.AreEqual('delphi', LEnum.Items[0].Value);
    Assert.AreEqual('free-pascal', LEnum.Items[1].Value);
    Assert.AreEqual('basic', LEnum.Items[2].Value);
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
  FSchema.AddEnum<TTestNamedLang>('lang', 'Language').Shape := TMCPChoiceShape.Titled;

  LJson := PropertyJson('lang');
  try
    Assert.AreEqual('string', LJson.GetValue<string>('type'));
    Assert.IsNull(LJson.GetValue('enum'), 'A titled enum renders as "oneOf", not "enum"');

    LOneOf := LJson.GetValue('oneOf') as TJSONArray;
    Assert.IsNotNull(LOneOf);
    Assert.AreEqual(3, LOneOf.Count);

    // The label is the value: the only thing that names the members of a Delphi
    // enum for JSON is [NeonEnumNames], and what it names is what travels
    LEntry := LOneOf.Items[1] as TJSONObject;
    Assert.AreEqual('free-pascal', LEntry.GetValue<string>('const'));
    Assert.AreEqual('free-pascal', LEntry.GetValue<string>('title'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestLegacyTitledEnum;
var
  LJson: TJSONObject;
  LNames: TJSONArray;
begin
  FSchema.AddEnum<TTestNamedLang>('legacy', 'Legacy').Shape := TMCPChoiceShape.Legacy;

  LJson := PropertyJson('legacy');
  try
    // The deprecated shape: parallel "enum" and "enumNames" instead of "oneOf"
    Assert.IsNotNull(LJson.GetValue('enum'));
    Assert.IsNull(LJson.GetValue('oneOf'));

    LNames := LJson.GetValue('enumNames') as TJSONArray;
    Assert.IsNotNull(LNames);
    Assert.AreEqual('delphi', LNames.Items[0].Value);
    Assert.AreEqual('free-pascal', LNames.Items[1].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestUntitledMultiSelectEnum;
var
  LProperty: TMCPSetSchema;
  LJson, LItems: TJSONObject;
  LDefault: TJSONArray;
begin
  LProperty := FSchema.AddSet<TTestPerms>('perms', 'Permissions');
  LProperty.MinItems := 1;
  LProperty.MaxItems := 2;
  LProperty.DefaultValue := ['read'];

  LJson := PropertyJson('perms');
  try
    Assert.AreEqual('array', LJson.GetValue<string>('type'));

    LItems := LJson.GetValue('items') as TJSONObject;
    Assert.IsNotNull(LItems);
    Assert.AreEqual('string', LItems.GetValue<string>('type'));
    Assert.AreEqual(3, (LItems.GetValue('enum') as TJSONArray).Count);
    Assert.AreEqual('read', (LItems.GetValue('enum') as TJSONArray).Items[0].Value);

    // Neon's own set schema carries "uniqueItems"; the primitive schema family
    // has no room for it
    Assert.IsNull(LJson.GetValue('uniqueItems'));

    Assert.AreEqual(1, LJson.GetValue<Integer>('minItems'));
    Assert.AreEqual(2, LJson.GetValue<Integer>('maxItems'));

    LDefault := LJson.GetValue('default') as TJSONArray;
    Assert.IsNotNull(LDefault);
    Assert.AreEqual('read', LDefault.Items[0].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestTitledMultiSelectEnum;
var
  LJson, LItems, LEntry: TJSONObject;
  LAnyOf: TJSONArray;
begin
  FSchema.AddSet<TTestPerms>('perms', 'Permissions').Shape := TMCPChoiceShape.Titled;

  LJson := PropertyJson('perms');
  try
    Assert.AreEqual('array', LJson.GetValue<string>('type'));

    LItems := LJson.GetValue('items') as TJSONObject;
    Assert.IsNull(LItems.GetValue('enum'), 'A titled multi-select renders items.anyOf');
    Assert.IsNull(LItems.GetValue('type'), 'A titled option carries its own "const"');

    LAnyOf := LItems.GetValue('anyOf') as TJSONArray;
    Assert.IsNotNull(LAnyOf);
    Assert.AreEqual(3, LAnyOf.Count);

    LEntry := LAnyOf.Items[1] as TJSONObject;
    Assert.AreEqual('write', LEntry.GetValue<string>('const'));
    Assert.AreEqual('write', LEntry.GetValue<string>('title'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPPrimitiveSchemaTest.TestChoiceNeedsAnEnumType_Raises;
begin
  // The wrong kind of type is the call site's mistake, so it is refused there
  Assert.WillRaise(
    procedure
    begin
      FSchema.AddEnum<Integer>('nope', 'Nope');
    end,
    EMCPException, 'A choice takes its options from an enumerated type');
end;

procedure TMCPPrimitiveSchemaTest.TestMultiChoiceNeedsASetType_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      FSchema.AddSet<TTestLang>('nope', 'Nope');
    end,
    EMCPException, 'A multiple choice takes its options from a set type');
end;

procedure TMCPPrimitiveSchemaTest.TestTypeWithoutOptions_Raises;
begin
  // A Boolean is an enumeration to the compiler but a boolean to Neon, so it
  // offers no options - which only shows when the schema renders
  FSchema.AddEnum<Boolean>('empty', 'Empty');

  Assert.WillRaise(
    procedure
    var
      LJson: TJSONObject;
    begin
      LJson := FSchema.Find('empty').ToJSON;
      LJson.Free;
    end,
    EMCPException, 'A choice with no options cannot render a valid schema');
end;

{ TMCPTypeSchemaTest }

function TMCPTypeSchemaTest.SchemaOf<T>: TJSONObject;
var
  LSchema: TMCPTypeSchema;
begin
  LSchema := TMCPTypeSchema.From<T>;
  try
    Result := LSchema.ToJSON;
  finally
    LSchema.Free;
  end;
end;

function TMCPTypeSchemaTest.MemberOf(AJson: TJSONObject; const AName: string): TJSONObject;
var
  LProperties: TJSONObject;
begin
  LProperties := AJson.GetValue('properties') as TJSONObject;
  Assert.IsNotNull(LProperties);

  Result := LProperties.GetValue(AName) as TJSONObject;
  Assert.IsNotNull(Result, 'Member [' + AName + '] should be in the schema');
end;

procedure TMCPTypeSchemaTest.TestRecordIsAFlatObjectSchema;
var
  LJson: TJSONObject;
begin
  LJson := SchemaOf<TTestSignup>;
  try
    Assert.AreEqual('object', LJson.GetValue<string>('type'));
    Assert.AreEqual(5, (LJson.GetValue('properties') as TJSONObject).Count);

    // The member names are Neon's, which is what the answer comes back under
    Assert.AreEqual('string', MemberOf(LJson, 'name').GetValue<string>('type'));
    Assert.AreEqual('integer', MemberOf(LJson, 'age').GetValue<string>('type'));
    Assert.AreEqual('boolean', MemberOf(LJson, 'subscribe').GetValue<string>('type'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestMemberKeywordsComeFromTheAttribute;
var
  LJson, LMember: TJSONObject;
begin
  LJson := SchemaOf<TTestSignup>;
  try
    LMember := MemberOf(LJson, 'name');
    Assert.AreEqual('Your name', LMember.GetValue<string>('title'));
    Assert.AreEqual('As on your badge', LMember.GetValue<string>('description'));
    Assert.AreEqual(2, LMember.GetValue<Integer>('minLength'));

    // Neon writes a tag bound as a number, fraction and all ("minimum": 0.0),
    // where the hand-built TMCPNumberSchema trims it for an integer property.
    // Both validate the same instances, so the value is asserted, not the text
    LMember := MemberOf(LJson, 'age');
    Assert.AreEqual(0.0, LMember.GetValue<Double>('minimum'), 0.0001);
    Assert.AreEqual(130.0, LMember.GetValue<Double>('maximum'), 0.0001);
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestRequiredComesFromTheAttribute;
var
  LJson: TJSONObject;
  LRequired: TJSONArray;
begin
  LJson := SchemaOf<TTestSignup>;
  try
    LRequired := LJson.GetValue('required') as TJSONArray;
    Assert.IsNotNull(LRequired);
    Assert.AreEqual(1, LRequired.Count, 'Only the member whose tag says "required"');
    Assert.AreEqual('name', LRequired.Items[0].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestEnumMemberIsASingleChoice;
var
  LJson, LMember: TJSONObject;
  LEnum: TJSONArray;
begin
  LJson := SchemaOf<TTestSignup>;
  try
    LMember := MemberOf(LJson, 'lang');
    Assert.AreEqual('string', LMember.GetValue<string>('type'));

    LEnum := LMember.GetValue('enum') as TJSONArray;
    Assert.IsNotNull(LEnum);
    Assert.AreEqual(3, LEnum.Count);
    Assert.AreEqual('delphi', LEnum.Items[0].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestSetMemberIsAMultipleChoice;
var
  LJson, LMember, LItems: TJSONObject;
begin
  LJson := SchemaOf<TTestSignup>;
  try
    LMember := MemberOf(LJson, 'perms');
    Assert.AreEqual('array', LMember.GetValue<string>('type'));

    LItems := LMember.GetValue('items') as TJSONObject;
    Assert.IsNotNull(LItems);
    Assert.AreEqual('read', (LItems.GetValue('enum') as TJSONArray).Items[0].Value);
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestClassIsAskedForLikeARecord;
var
  LJson: TJSONObject;
begin
  LJson := SchemaOf<TTestSignupClass>;
  try
    Assert.AreEqual('object', LJson.GetValue<string>('type'));
    Assert.AreEqual('Your name', MemberOf(LJson, 'name').GetValue<string>('title'));
  finally
    LJson.Free;
  end;
end;

procedure TMCPTypeSchemaTest.TestNestedStructure_Raises;
begin
  // A client renders a flat form: a nested object is not something it could ask
  // the user for, so the schema is refused here rather than sent
  Assert.WillRaise(
    procedure
    begin
      SchemaOf<TTestNested>.Free;
    end,
    EMCPException, 'A nested structure is not a primitive an elicitation may ask for');
end;

procedure TMCPTypeSchemaTest.TestNullableMember_Raises;
begin
  // A Nullable renders as ["string","null"], and no primitive schema is a union:
  // optionality is declared by leaving the member out of "required"
  Assert.WillRaise(
    procedure
    begin
      SchemaOf<TTestNullableAsk>.Free;
    end,
    EMCPException, 'A union of types is not a primitive schema');
end;

procedure TMCPTypeSchemaTest.TestNonStructType_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      TMCPTypeSchema.From<Integer>.Free;
    end,
    EMCPException, 'A form is generated from a record or a class');
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

procedure TMCPElicitRequestTest.TestFormOfType_SetsModeAndSchema;
var
  LParams: TElicitRequestParams;
  LProperties: TJSONObject;
begin
  LParams := TMCPElicitRequest.Form<TTestSignup>('Tell us about yourself');
  try
    Assert.AreEqual('Tell us about yourself', LParams.Message);
    Assert.AreEqual(MCP_ELICIT_MODE_FORM, LParams.Mode.Value);

    LProperties := LParams.RequestedSchema.GetValue('properties') as TJSONObject;
    Assert.IsNotNull(LProperties);
    Assert.AreEqual(5, LProperties.Count);
  finally
    LParams.Free;
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
