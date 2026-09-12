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
///   The requestedSchema of a form-mode elicitation, and the two shapes of
///   params the protocol defines for elicitation/create.
/// </summary>
/// <remarks>
///   <para>
///     There are two ways to say what the server wants filled in.
///     TMCPTypeSchema takes a Delphi record or class and has Neon generate the
///     schema from its RTTI - the same RTTI that reads the answer back, so one
///     declaration serves both directions, with [JsonSchema] carrying the
///     per-member title, description, bounds and required flag.
///     TMCPElicitationSchema builds the same document property by property, for
///     a form whose shape is only known at run time.
///   </para>
///   <para>
///     A choice is a Delphi type too: an enumeration for a single choice, a set
///     for a multiple one. Neon's schema generator and its serializer agree on
///     the names of the members ([NeonEnumNames], else the member names under
///     the configured case), so the value a client sends back deserializes into
///     the very type that described it.
///   </para>
/// </remarks>
unit MCPConnect.MCP.Types.Elicitation;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.TypInfo, System.Rtti, System.JSON,
  System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Utils,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema,

  MCPConnect.MCP.Types.Base;

const
  /// <summary>
  ///   The elicitation modes. An absent "mode" means "form".
  /// </summary>
  MCP_ELICIT_MODE_FORM = 'form';
  MCP_ELICIT_MODE_URL = 'url';

resourcestring
  // MCPConnect.MCP.Types.Elicitation
  SMCPElicitFormNeedsSchema = 'A form-mode elicitation requires a requestedSchema';
  SMCPElicitUrlNeedsUrl = 'A url-mode elicitation requires a url';
  SMCPElicitUrlHasSchema = 'A url-mode elicitation cannot carry a requestedSchema';
  SMCPElicitModeUnknownFmt = 'Elicitation mode [%s] is not supported';
  SMCPElicitPropertyNameEmpty = 'An elicitation schema property must have a name';
  SMCPElicitPropertyDuplicateFmt = 'Property [%s] is already declared on this elicitation schema';
  SMCPElicitChoiceNeedsEnumFmt = 'A single-choice property takes its options from an enumerated type, and [%s] is not one';
  SMCPElicitChoiceNeedsSetFmt = 'A multiple-choice property takes its options from a set type, and [%s] is not one';
  SMCPElicitChoiceNoOptionsFmt = 'Type [%s] offers no options a choice property could ask for';
  SMCPElicitChoiceNotStringFmt = 'The options of [%s] are not strings, which every shape of the elicitation enum schema requires: the Neon configuration writes enums as integers';
  SMCPElicitTypeNoRtti = 'The type has no RTTI, so no schema can be generated from it';
  SMCPElicitTypeNeedsStructFmt = 'An elicitation schema is generated from a record or a class, and [%s] is neither';
  SMCPElicitTypeNotFlatFmt = 'The schema generated for [%s] is not flat: %s. An elicitation may only ask for primitive properties';
  SMCPElicitReasonDefs = 'it hoists definitions into "$defs"';
  SMCPElicitReasonAllOf = 'it carries an "allOf" branch';
  SMCPElicitReasonNoProperties = 'it declares no properties';
  // Kept under 256 characters: D11 takes no longer string literal
  SMCPElicitMemberNotPrimitiveFmt = 'Member [%s] of [%s] renders as [%s], and an elicitation asks only for a string, number, integer, boolean, single choice (enum) or multiple choice (array of enum). A Nullable renders as a union of types: say optional with "required" instead';

type
  /// <summary>
  ///   The "format" values a StringSchema may declare.
  /// </summary>
  TMCPStringFormat = (None, Date, DateTime, Email, Uri);

  /// <summary>
  ///   Which of the three shapes the schema defines for a set of choices is
  ///   emitted.
  /// </summary>
  /// <remarks>
  ///   Plain is the untitled shape, a bare "enum" of the option values. Titled
  ///   is the one that pairs every value with a label, and the label is the
  ///   value: the only thing that names the members of a Delphi enum for JSON
  ///   is [NeonEnumNames], and what it names is what the client sends back.
  ///   Legacy is the deprecated predecessor of Titled - "enum" alongside a
  ///   parallel "enumNames" - for clients that have not caught up;
  ///   "enumNames" is not standard JSON Schema 2020-12.
  /// </remarks>
  TMCPChoiceShape = (Plain, Titled, Legacy);

  /// <summary>
  ///   Base of the schema's PrimitiveSchemaDefinition family: the restricted
  ///   subset of JSON Schema an elicitation may ask a user to fill in. Only
  ///   primitives are allowed - no nested objects, and no arrays other than the
  ///   multi-select enum.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     These are emission types: MCPConnect is a server, and a
  ///     requestedSchema travels server to client. They render themselves to
  ///     JSON rather than going through Neon, because the eight variants form a
  ///     discriminated union that Neon has no polymorphic writer for, and
  ///     because a schema keyword such as "oneOf" or "items" has a fixed shape
  ///     that is clearer built than annotated.
  ///   </para>
  /// </remarks>
  TMCPPrimitiveSchema = class abstract
  public
    /// <summary>
    ///   The property name inside requestedSchema.properties.
    /// </summary>
    Name: string;

    /// <summary>
    ///   Label shown to the user.
    /// </summary>
    Title: NullString;

    /// <summary>Longer explanation shown to the user.</summary>
    Description: NullString;

    /// <summary>
    ///   Whether the client must collect a value for this property.
    /// </summary>
    Required: Boolean;

    /// <summary>
    ///   The Neon configuration the properties whose content Neon generates (the
    ///   choices) run under. Nil means MCPNeonConfig, and the owning schema is
    ///   what sets it.
    /// </summary>
    Config: INeonConfiguration;
  protected
    /// <summary>
    ///   The JSON Schema "type" of this property.
    /// </summary>
    function SchemaType: string; virtual; abstract;

    /// <summary>
    ///   Writes the variant-specific keywords. AJson already has "type".
    /// </summary>
    procedure WriteKeywords(AJson: TJSONObject); virtual;
  public
    /// <summary>
    ///   Renders the property schema. The caller owns the returned object.
    /// </summary>
    function ToJSON: TJSONObject;
  end;

  /// <summary>
  ///   A free-text property.
  /// </summary>
  TMCPStringSchema = class(TMCPPrimitiveSchema)
  public
    MinLength: NullInteger;
    MaxLength: NullInteger;
    Format: TMCPStringFormat;
    DefaultValue: NullString;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   A numeric property, integer or real depending on IsInteger.
  /// </summary>
  TMCPNumberSchema = class(TMCPPrimitiveSchema)
  public
    IsInteger: Boolean;
    Minimum: NullDouble;
    Maximum: NullDouble;
    DefaultValue: NullDouble;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   A yes/no property.
  /// </summary>
  TMCPBooleanSchema = class(TMCPPrimitiveSchema)
  public
    DefaultValue: NullBoolean;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   Common part of the two choice properties: the Delphi type the options
  ///   come from, and which shape they render as.
  /// </summary>
  /// <remarks>
  ///   The options are the names Neon writes for the type's members, read out of
  ///   the schema its generator produces rather than walked here: the value a
  ///   client sends back has to be the one Neon's reader accepts, so there is
  ///   one authority on those names and it is not this unit.
  /// </remarks>
  TMCPChoiceSchema = class abstract(TMCPPrimitiveSchema)
  public
    /// <summary>
    ///   The type whose members are the options: an enumeration for a single
    ///   choice, a set for a multiple one.
    /// </summary>
    ChoiceType: TRttiType;

    /// <summary>Which of the three shapes the options render as.</summary>
    Shape: TMCPChoiceShape;

    /// <summary>The option values, in declaration order.</summary>
    function Values: TArray<string>; virtual; abstract;
  protected
    /// <summary>
    ///   Writes the options into ATarget: as "enum" (plus a parallel
    ///   "enumNames" when Legacy), or as ATitledKeyword when Titled - "oneOf"
    ///   for a single choice, "anyOf" for the items of a multiple one.
    /// </summary>
    procedure WriteOptions(ATarget: TJSONObject; const ATitledKeyword: string);
  end;

  /// <summary>
  ///   A single-choice property generated from an enumerated type:
  ///   SingleSelectEnumSchema, rendered untitled as
  ///   {"type":"string","enum":[...]} or titled as
  ///   {"type":"string","oneOf":[{"const":...,"title":...}]}.
  /// </summary>
  TMCPEnumSchema = class(TMCPChoiceSchema)
  public
    DefaultValue: NullString;

    function Values: TArray<string>; override;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   A multiple-choice property generated from a set type:
  ///   MultiSelectEnumSchema, an array of the allowed values, rendered untitled
  ///   as {"type":"array","items":{"type":"string","enum":[...]}} or titled as
  ///   {"type":"array","items":{"anyOf":[{"const":...,"title":...}]}}.
  /// </summary>
  TMCPSetSchema = class(TMCPChoiceSchema)
  public
    MinItems: NullInteger;
    MaxItems: NullInteger;
    DefaultValue: TArray<string>;

    function Values: TArray<string>; override;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   Something that renders the requestedSchema of a form-mode elicitation:
  ///   either generated from a Delphi type (TMCPTypeSchema) or built property by
  ///   property (TMCPElicitationSchema).
  /// </summary>
  TMCPRequestedSchema = class abstract
  public
    /// <summary>
    ///   Renders the object schema. The caller owns the returned object.
    /// </summary>
    function ToJSON: TJSONObject; virtual; abstract;
    function ToJSONString(APrettyPrint: Boolean = False): string;
  end;

  /// <summary>
  ///   The requestedSchema generated from one Delphi record or class: the type
  ///   that declares what is being asked for, and that the answer deserializes
  ///   back into.
  /// </summary>
  /// <remarks>
  ///   The document is Neon's, so the per-member keywords come from
  ///   [JsonSchema] - title, description, required, minLength, minimum, pattern,
  ///   default - and the member names from the configured case and
  ///   [NeonProperty], which are the names Neon's reader expects back. What this
  ///   class adds is the elicitation restriction: the protocol allows only flat
  ///   primitives, so a type that generates anything else is refused here rather
  ///   than sent to a client that could not render it.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   type
  ///     TSignup = record
  ///       [JsonSchema('title=Your name, required, minLength=2')]
  ///       Name: string;
  ///       [JsonSchema('title=Language')]
  ///       Lang: TLang;
  ///     end;
  ///
  ///   LSchema := TMCPTypeSchema.From&lt;TSignup&gt;;
  ///   </code>
  /// </example>
  TMCPTypeSchema = class(TMCPRequestedSchema)
  private
    FSourceType: TRttiType;
    FConfig: INeonConfiguration;
  public
    /// <summary>
    ///   The schema of ASourceType, a record or a class. AConfig nil means
    ///   MCPNeonConfig.
    /// </summary>
    constructor Create(ASourceType: TRttiType; AConfig: INeonConfiguration = nil);

    /// <summary>
    ///   The schema of T. The caller owns the result.
    /// </summary>
    class function From<T>(AConfig: INeonConfiguration = nil): TMCPTypeSchema; static;

    /// <summary>
    ///   Raises EMCPException unless AJson is the flat object schema an
    ///   elicitation may ask for. ATypeName only names the offender in the
    ///   message.
    /// </summary>
    class procedure CheckIsPrimitive(AJson: TJSONObject; const ATypeName: string); static;

    function ToJSON: TJSONObject; override;

    /// <summary>
    ///   The record or class the schema is generated from.
    /// </summary>
    property SourceType: TRttiType read FSourceType;

    /// <summary>
    ///   The Neon configuration the generator runs under, never nil.
    /// </summary>
    property Config: INeonConfiguration read FConfig;
  end;

  /// <summary>
  ///   The object schema of a form-mode elicitation built property by property,
  ///   for a form whose shape is only known at run time. A form that has a
  ///   Delphi type is better declared as one - see TMCPTypeSchema.
  /// </summary>
  /// <example>
  ///   <code>
  ///   LSchema := TMCPElicitationSchema.Create;
  ///   LSchema.AddString('name', 'Your name', True).MinLength := 2;
  ///   LSchema.AddInteger('age', 'Your age');
  ///   LSchema.AddEnum&lt;TLang&gt;('lang', 'Language', True);
  ///   LSchema.AddSet&lt;TPermissions&gt;('perms', 'Permissions');
  ///   </code>
  /// </example>
  TMCPElicitationSchema = class(TMCPRequestedSchema)
  private
    FProperties: TObjectList<TMCPPrimitiveSchema>;
    FConfig: INeonConfiguration;
    function AddProperty<T: TMCPPrimitiveSchema, constructor>(const AName, ATitle: string;
      ARequired: Boolean): T;
  public
    /// <summary>
    ///   AConfig is the Neon configuration the choice properties generate their
    ///   options under; nil means MCPNeonConfig.
    /// </summary>
    constructor Create(AConfig: INeonConfiguration = nil);
    destructor Destroy; override;

    /// <summary>
    ///   Declares a free-text property.
    /// </summary>
    function AddString(const AName, ATitle: string; ARequired: Boolean = False): TMCPStringSchema;

    /// <summary>
    ///   Declares a whole-number property.
    /// </summary>
    function AddInteger(const AName, ATitle: string; ARequired: Boolean = False): TMCPNumberSchema;

    /// <summary>
    ///   Declares a real-number property.
    /// </summary>
    function AddNumber(const AName, ATitle: string; ARequired: Boolean = False): TMCPNumberSchema;

    /// <summary>
    ///   Declares a yes/no property.
    /// </summary>
    function AddBoolean(const AName, ATitle: string; ARequired: Boolean = False): TMCPBooleanSchema;

    /// <summary>
    ///   Declares a single-choice property offering the members of the
    ///   enumerated type T, which is also what the answer deserializes into.
    /// </summary>
    function AddEnum<T>(const AName, ATitle: string; ARequired: Boolean = False): TMCPEnumSchema;

    /// <summary>
    ///   Declares a multiple-choice property offering the members of the base
    ///   type of the set T, which is also what the answer deserializes into.
    /// </summary>
    function AddSet<T>(const AName, ATitle: string; ARequired: Boolean = False): TMCPSetSchema;

    /// <summary>
    ///   The property declared under AName, or nil.
    /// </summary>
    function Find(const AName: string): TMCPPrimitiveSchema;

    function ToJSON: TJSONObject; override;

    /// <summary>
    ///   The declared properties, in declaration order.
    /// </summary>
    property Properties: TObjectList<TMCPPrimitiveSchema> read FProperties;

    /// <summary>
    ///   The Neon configuration the choice properties generate under; nil means
    ///   MCPNeonConfig.
    /// </summary>
    property Config: INeonConfiguration read FConfig write FConfig;
  end;

  /// <summary>
  ///   Builds the two shapes of "elicitation/create" params the schema defines,
  ///   ElicitRequestFormParams and ElicitRequestURLParams, and checks a
  ///   hand-built one for the same invariants.
  /// </summary>
  /// <remarks>
  ///   TElicitRequestParams is a single class carrying both shapes, so nothing
  ///   in its declaration can say "requestedSchema is required in form mode,
  ///   url in url mode". These entry points are where that is enforced.
  /// </remarks>
  TMCPElicitRequest = class
  public
    /// <summary>
    ///   Form-mode params rendering ASchema. The caller keeps ownership of
    ///   ASchema and owns the returned params.
    /// </summary>
    class function Form(const AMessage: string; ASchema: TMCPRequestedSchema): TElicitRequestParams; overload; static;

    /// <summary>
    ///   Form-mode params asking for the record or class T. The caller owns the
    ///   returned params.
    /// </summary>
    class function Form<T>(const AMessage: string; AConfig: INeonConfiguration = nil): TElicitRequestParams; overload; static;

    /// <summary>
    ///   Url-mode params. The caller owns the returned params.
    /// </summary>
    class function Url(const AMessage, AUrl: string): TElicitRequestParams; static;

    /// <summary>
    ///   Raises EMCPException when AParams does not satisfy the invariants of
    ///   the mode it declares.
    /// </summary>
    class procedure Validate(AParams: TElicitRequestParams); static;
  end;

implementation

/// <summary>AConfig, or the library default when it is nil.</summary>
function SchemaConfig(AConfig: INeonConfiguration): INeonConfiguration;
begin
  if Assigned(AConfig) then
    Result := AConfig
  else
    Result := MCPNeonConfig;
end;

/// <summary>
///   The schema Neon generates for AType. The caller owns it.
/// </summary>
function GeneratedSchema(AType: TRttiType; AConfig: INeonConfiguration): TJSONObject;
begin
  if not Assigned(AType) then
    raise EMCPException.Create(SMCPElicitTypeNoRtti);

  Result := TNeonSchemaGenerator.TypeToJSONSchema(AType, SchemaConfig(AConfig),
    TNeonJSchemaVersion.None);
end;

/// <summary>
///   The option values of a choice type, taken from the schema Neon generates
///   for it. AInItems reads them out of "items" - where the generator puts the
///   members of a set - instead of out of the schema itself.
/// </summary>
function ChoiceValues(AType: TRttiType; AConfig: INeonConfiguration; AInItems: Boolean): TArray<string>;
var
  LSchema, LHolder: TJSONObject;
  LOptions: TJSONArray;
  LIndex: Integer;
begin
  LSchema := GeneratedSchema(AType, AConfig);
  try
    LHolder := LSchema;
    if AInItems then
      LHolder := LSchema.GetValue('items') as TJSONObject;

    LOptions := nil;
    if Assigned(LHolder) then
      LOptions := LHolder.GetValue('enum') as TJSONArray;

    // A Boolean is an enumeration to the compiler but a boolean to Neon, and a
    // set whose base type has no RTTI gets integer items: neither offers the
    // options a choice property is made of
    if not Assigned(LOptions) or (LOptions.Count = 0) then
      raise EMCPException.CreateFmt(SMCPElicitChoiceNoOptionsFmt, [AType.Name]);

    SetLength(Result, LOptions.Count);
    for LIndex := 0 to LOptions.Count - 1 do
    begin
      // Every shape of the enum schema carries string values. EnumAsInt writes
      // numbers, which a client could render but this schema cannot declare
      if not (LOptions.Items[LIndex] is TJSONString) then
        raise EMCPException.CreateFmt(SMCPElicitChoiceNotStringFmt, [AType.Name]);

      Result[LIndex] := LOptions.Items[LIndex].Value;
    end;
  finally
    LSchema.Free;
  end;
end;

{ TMCPPrimitiveSchema }

procedure TMCPPrimitiveSchema.WriteKeywords(AJson: TJSONObject);
begin
  // Nothing beyond the common keywords by default
end;

function TMCPPrimitiveSchema.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('type', SchemaType);

    if Title.HasValue and not Title.Value.IsEmpty then
      Result.AddPair('title', Title.Value);
    if Description.HasValue and not Description.Value.IsEmpty then
      Result.AddPair('description', Description.Value);

    WriteKeywords(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPStringSchema }

function TMCPStringSchema.SchemaType: string;
begin
  Result := 'string';
end;

procedure TMCPStringSchema.WriteKeywords(AJson: TJSONObject);
begin
  if MinLength.HasValue then
    AJson.AddPair('minLength', TJSONNumber.Create(MinLength.Value));
  if MaxLength.HasValue then
    AJson.AddPair('maxLength', TJSONNumber.Create(MaxLength.Value));

  case Format of
    TMCPStringFormat.Date:     AJson.AddPair('format', 'date');
    TMCPStringFormat.DateTime: AJson.AddPair('format', 'date-time');
    TMCPStringFormat.Email:    AJson.AddPair('format', 'email');
    TMCPStringFormat.Uri:      AJson.AddPair('format', 'uri');
  end;

  if DefaultValue.HasValue then
    AJson.AddPair('default', DefaultValue.Value);
end;

{ TMCPNumberSchema }

function TMCPNumberSchema.SchemaType: string;
begin
  if IsInteger then
    Result := 'integer'
  else
    Result := 'number';
end;

procedure TMCPNumberSchema.WriteKeywords(AJson: TJSONObject);

  // An integer property's bounds and default are written without a fractional
  // part: "minimum": 0 rather than "minimum": 0.0
  function Number(const AValue: Double): TJSONNumber;
  begin
    if IsInteger then
      Result := TJSONNumber.Create(Trunc(AValue))
    else
      Result := TJSONNumber.Create(AValue);
  end;

begin
  if Minimum.HasValue then
    AJson.AddPair('minimum', Number(Minimum.Value));
  if Maximum.HasValue then
    AJson.AddPair('maximum', Number(Maximum.Value));
  if DefaultValue.HasValue then
    AJson.AddPair('default', Number(DefaultValue.Value));
end;

{ TMCPBooleanSchema }

function TMCPBooleanSchema.SchemaType: string;
begin
  Result := 'boolean';
end;

procedure TMCPBooleanSchema.WriteKeywords(AJson: TJSONObject);
begin
  if DefaultValue.HasValue then
    AJson.AddPair('default', TJSONBool.Create(DefaultValue.Value));
end;

{ TMCPChoiceSchema }

procedure TMCPChoiceSchema.WriteOptions(ATarget: TJSONObject; const ATitledKeyword: string);
var
  LValues: TArray<string>;
  LOptions, LTitles: TJSONArray;
  LEntry: TJSONObject;
  LValue: string;
begin
  LValues := Values;

  if Shape = TMCPChoiceShape.Titled then
  begin
    LOptions := TJSONArray.Create;
    ATarget.AddPair(ATitledKeyword, LOptions);
    for LValue in LValues do
    begin
      LEntry := TJSONObject.Create;
      LOptions.AddElement(LEntry);
      LEntry.AddPair('const', LValue);
      LEntry.AddPair('title', LValue);
    end;
    Exit;
  end;

  LOptions := TJSONArray.Create;
  ATarget.AddPair('enum', LOptions);
  for LValue in LValues do
    LOptions.Add(LValue);

  if Shape = TMCPChoiceShape.Legacy then
  begin
    LTitles := TJSONArray.Create;
    ATarget.AddPair('enumNames', LTitles);
    for LValue in LValues do
      LTitles.Add(LValue);
  end;
end;

{ TMCPEnumSchema }

function TMCPEnumSchema.SchemaType: string;
begin
  Result := 'string';
end;

function TMCPEnumSchema.Values: TArray<string>;
begin
  Result := ChoiceValues(ChoiceType, Config, False);
end;

procedure TMCPEnumSchema.WriteKeywords(AJson: TJSONObject);
begin
  WriteOptions(AJson, 'oneOf');

  if DefaultValue.HasValue then
    AJson.AddPair('default', DefaultValue.Value);
end;

{ TMCPSetSchema }

function TMCPSetSchema.SchemaType: string;
begin
  Result := 'array';
end;

function TMCPSetSchema.Values: TArray<string>;
begin
  // Neon renders a set as an array, so its members are under "items"
  Result := ChoiceValues(ChoiceType, Config, True);
end;

procedure TMCPSetSchema.WriteKeywords(AJson: TJSONObject);
var
  LItems: TJSONObject;
  LDefaults: TJSONArray;
  LValue: string;
begin
  LItems := TJSONObject.Create;
  AJson.AddPair('items', LItems);

  // A titled option carries its own "const", so the titled shape declares no
  // "type" for the items; the untitled one makes them a string enum. (Neon's own
  // set schema also carries "uniqueItems", which the primitive schema family has
  // no room for - and a multi-select is unique by construction anyway)
  if Shape <> TMCPChoiceShape.Titled then
    LItems.AddPair('type', 'string');

  WriteOptions(LItems, 'anyOf');

  if MinItems.HasValue then
    AJson.AddPair('minItems', TJSONNumber.Create(MinItems.Value));
  if MaxItems.HasValue then
    AJson.AddPair('maxItems', TJSONNumber.Create(MaxItems.Value));

  if Length(DefaultValue) > 0 then
  begin
    LDefaults := TJSONArray.Create;
    AJson.AddPair('default', LDefaults);
    for LValue in DefaultValue do
      LDefaults.Add(LValue);
  end;
end;

{ TMCPRequestedSchema }

function TMCPRequestedSchema.ToJSONString(APrettyPrint: Boolean): string;
var
  LJson: TJSONObject;
begin
  LJson := ToJSON;
  try
    if APrettyPrint then
      Result := LJson.Format
    else
      Result := LJson.ToJSON;
  finally
    LJson.Free;
  end;
end;

{ TMCPTypeSchema }

constructor TMCPTypeSchema.Create(ASourceType: TRttiType; AConfig: INeonConfiguration);
begin
  inherited Create;

  if not Assigned(ASourceType) then
    raise EMCPException.Create(SMCPElicitTypeNoRtti);

  // A form asks for a set of named values, which is what a record or a class is.
  // Refused in the constructor rather than at render time: the call site is
  // where the wrong type was named
  if not (ASourceType.TypeKind in [tkRecord, tkMRecord, tkClass]) then
    raise EMCPException.CreateFmt(SMCPElicitTypeNeedsStructFmt, [ASourceType.Name]);

  FSourceType := ASourceType;
  FConfig := SchemaConfig(AConfig);
end;

class function TMCPTypeSchema.From<T>(AConfig: INeonConfiguration): TMCPTypeSchema;
begin
  Result := TMCPTypeSchema.Create(TRttiUtils.Context.GetType(System.TypeInfo(T)), AConfig);
end;

class procedure TMCPTypeSchema.CheckIsPrimitive(AJson: TJSONObject; const ATypeName: string);
var
  LProperties, LMember: TJSONObject;
  LPair: TJSONPair;
  LType, LItems: TJSONValue;
  LRejected, LName: string;
  LIndex: Integer;
begin
  // Definitions and allOf branches are what the generator emits for a type that
  // is not flat - a self-reference, an unwrapped member - and what they stand for
  // is not in "properties" at all
  if Assigned(AJson.GetValue('$defs')) or Assigned(AJson.GetValue('definitions')) then
    raise EMCPException.CreateFmt(SMCPElicitTypeNotFlatFmt, [ATypeName, SMCPElicitReasonDefs]);

  if Assigned(AJson.GetValue('allOf')) then
    raise EMCPException.CreateFmt(SMCPElicitTypeNotFlatFmt, [ATypeName, SMCPElicitReasonAllOf]);

  LProperties := AJson.GetValue('properties') as TJSONObject;
  if not Assigned(LProperties) or (LProperties.Count = 0) then
    raise EMCPException.CreateFmt(SMCPElicitTypeNotFlatFmt, [ATypeName, SMCPElicitReasonNoProperties]);

  for LIndex := 0 to LProperties.Count - 1 do
  begin
    LPair := LProperties.Pairs[LIndex];
    LMember := LPair.JsonValue as TJSONObject;
    LRejected := '';

    if not Assigned(LMember) then
      LRejected := 'not a schema'
    else if Assigned(LMember.GetValue('$ref')) then
      // A reference means the member is (or contains) another structure
      LRejected := '$ref'
    else
    begin
      LType := LMember.GetValue('type');

      if LType is TJSONArray then
        // A Nullable<T> renders as a union: ["string", "null"]
        LRejected := 'a union of types'
      else if not (LType is TJSONString) then
        LRejected := 'no type'
      else
      begin
        LName := LType.Value;

        if LName = 'array' then
        begin
          // The one array an elicitation may ask for is the multi-select: items
          // is a string enum, or the titled anyOf of consts
          LItems := LMember.GetValue('items');
          if not (LItems is TJSONObject) or
             (not Assigned(TJSONObject(LItems).GetValue('enum')) and
              not Assigned(TJSONObject(LItems).GetValue('anyOf'))) then
            LRejected := 'an array of something other than choices';
        end
        else if (LName <> 'string') and (LName <> 'integer') and
                (LName <> 'number') and (LName <> 'boolean') then
          LRejected := LName;
      end;
    end;

    if LRejected <> '' then
      raise EMCPException.CreateFmt(SMCPElicitMemberNotPrimitiveFmt,
        [LPair.JsonString.Value, ATypeName, LRejected]);
  end;
end;

function TMCPTypeSchema.ToJSON: TJSONObject;
begin
  Result := GeneratedSchema(FSourceType, FConfig);
  try
    CheckIsPrimitive(Result, FSourceType.Name);
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPElicitationSchema }

constructor TMCPElicitationSchema.Create(AConfig: INeonConfiguration);
begin
  inherited Create;
  FProperties := TObjectList<TMCPPrimitiveSchema>.Create(True);
  FConfig := AConfig;
end;

destructor TMCPElicitationSchema.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TMCPElicitationSchema.AddProperty<T>(const AName, ATitle: string;
  ARequired: Boolean): T;
begin
  if AName.IsEmpty then
    raise EMCPException.Create(SMCPElicitPropertyNameEmpty);
  if Assigned(Find(AName)) then
    raise EMCPException.CreateFmt(SMCPElicitPropertyDuplicateFmt, [AName]);

  Result := T.Create;
  try
    Result.Name := AName;
    Result.Required := ARequired;
    Result.Config := FConfig;
    if not ATitle.IsEmpty then
      Result.Title := ATitle;

    FProperties.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPElicitationSchema.AddString(const AName, ATitle: string;
  ARequired: Boolean): TMCPStringSchema;
begin
  Result := AddProperty<TMCPStringSchema>(AName, ATitle, ARequired);
end;

function TMCPElicitationSchema.AddInteger(const AName, ATitle: string;
  ARequired: Boolean): TMCPNumberSchema;
begin
  Result := AddProperty<TMCPNumberSchema>(AName, ATitle, ARequired);
  Result.IsInteger := True;
end;

function TMCPElicitationSchema.AddNumber(const AName, ATitle: string;
  ARequired: Boolean): TMCPNumberSchema;
begin
  Result := AddProperty<TMCPNumberSchema>(AName, ATitle, ARequired);
end;

function TMCPElicitationSchema.AddBoolean(const AName, ATitle: string;
  ARequired: Boolean): TMCPBooleanSchema;
begin
  Result := AddProperty<TMCPBooleanSchema>(AName, ATitle, ARequired);
end;

function TMCPElicitationSchema.AddEnum<T>(const AName, ATitle: string;
  ARequired: Boolean): TMCPEnumSchema;
var
  LType: TRttiType;
begin
  LType := TRttiUtils.Context.GetType(System.TypeInfo(T));
  if not Assigned(LType) then
    raise EMCPException.Create(SMCPElicitTypeNoRtti);

  // The wrong kind of type is the call site's mistake, so it is refused there
  // rather than when the schema renders
  if LType.TypeKind <> tkEnumeration then
    raise EMCPException.CreateFmt(SMCPElicitChoiceNeedsEnumFmt, [LType.Name]);

  Result := AddProperty<TMCPEnumSchema>(AName, ATitle, ARequired);
  Result.ChoiceType := LType;
end;

function TMCPElicitationSchema.AddSet<T>(const AName, ATitle: string;
  ARequired: Boolean): TMCPSetSchema;
var
  LType: TRttiType;
begin
  LType := TRttiUtils.Context.GetType(System.TypeInfo(T));
  if not Assigned(LType) then
    raise EMCPException.Create(SMCPElicitTypeNoRtti);

  if LType.TypeKind <> tkSet then
    raise EMCPException.CreateFmt(SMCPElicitChoiceNeedsSetFmt, [LType.Name]);

  Result := AddProperty<TMCPSetSchema>(AName, ATitle, ARequired);
  Result.ChoiceType := LType;
end;

function TMCPElicitationSchema.Find(const AName: string): TMCPPrimitiveSchema;
var
  LProperty: TMCPPrimitiveSchema;
begin
  // Property names are JSON member names: matched case-sensitively
  for LProperty in FProperties do
    if LProperty.Name = AName then
      Exit(LProperty);

  Result := nil;
end;

function TMCPElicitationSchema.ToJSON: TJSONObject;
var
  LProperties: TJSONObject;
  LRequired: TJSONArray;
  LProperty: TMCPPrimitiveSchema;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('type', 'object');

    LProperties := TJSONObject.Create;
    Result.AddPair('properties', LProperties);
    for LProperty in FProperties do
      LProperties.AddPair(LProperty.Name, LProperty.ToJSON);

    // "required" is omitted rather than emitted empty: an empty array would read
    // as a deliberate "nothing is required" rather than as absence
    LRequired := nil;
    for LProperty in FProperties do
      if LProperty.Required then
      begin
        if not Assigned(LRequired) then
        begin
          LRequired := TJSONArray.Create;
          Result.AddPair('required', LRequired);
        end;
        LRequired.Add(LProperty.Name);
      end;
  except
    Result.Free;
    raise;
  end;
end;

{ TMCPElicitRequest }

class function TMCPElicitRequest.Form(const AMessage: string;
  ASchema: TMCPRequestedSchema): TElicitRequestParams;
begin
  if not Assigned(ASchema) then
    raise EMCPException.Create(SMCPElicitFormNeedsSchema);

  Result := TElicitRequestParams.Create;
  try
    Result.Message := AMessage;
    Result.Mode := MCP_ELICIT_MODE_FORM;

    Result.RequestedSchema.Free;
    Result.RequestedSchema := ASchema.ToJSON;
  except
    Result.Free;
    raise;
  end;
end;

class function TMCPElicitRequest.Form<T>(const AMessage: string;
  AConfig: INeonConfiguration): TElicitRequestParams;
var
  LSchema: TMCPTypeSchema;
begin
  LSchema := TMCPTypeSchema.From<T>(AConfig);
  try
    Result := Form(AMessage, LSchema);
  finally
    LSchema.Free;
  end;
end;

class function TMCPElicitRequest.Url(const AMessage, AUrl: string): TElicitRequestParams;
begin
  if AUrl.IsEmpty then
    raise EMCPException.Create(SMCPElicitUrlNeedsUrl);

  Result := TElicitRequestParams.Create;
  try
    Result.Message := AMessage;
    Result.Mode := MCP_ELICIT_MODE_URL;
    Result.Url := AUrl;
  except
    Result.Free;
    raise;
  end;
end;

class procedure TMCPElicitRequest.Validate(AParams: TElicitRequestParams);
var
  LMode: string;
begin
  // An absent mode means "form"
  LMode := MCP_ELICIT_MODE_FORM;
  if AParams.Mode.HasValue and not AParams.Mode.Value.IsEmpty then
    LMode := AParams.Mode.Value;

  if LMode = MCP_ELICIT_MODE_FORM then
  begin
    if not Assigned(AParams.RequestedSchema) or (AParams.RequestedSchema.Count = 0) then
      raise EMCPException.Create(SMCPElicitFormNeedsSchema);
  end
  else if LMode = MCP_ELICIT_MODE_URL then
  begin
    if not AParams.Url.HasValue or AParams.Url.Value.IsEmpty then
      raise EMCPException.Create(SMCPElicitUrlNeedsUrl);

    if Assigned(AParams.RequestedSchema) and (AParams.RequestedSchema.Count > 0) then
      raise EMCPException.Create(SMCPElicitUrlHasSchema);
  end
  else
    raise EMCPException.CreateFmt(SMCPElicitModeUnknownFmt, [LMode]);
end;

end.
