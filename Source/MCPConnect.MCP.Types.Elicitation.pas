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
unit MCPConnect.MCP.Types.Elicitation;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.JSON, System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Nullables,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

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
  SMCPElicitEnumEmpty = 'An enum property must offer at least one option';

type
  /// <summary>
  ///   The "format" values a StringSchema may declare.
  /// </summary>
  TMCPStringFormat = (None, Date, DateTime, Email, Uri);

  /// <summary>
  ///   One choice of an enum property: the value written back by the client,
  ///   and the label shown to the user.
  /// </summary>
  /// <remarks>
  ///   An empty Title selects the untitled form of the schema, which carries
  ///   the bare values and lets the client render them as it sees fit.
  /// </remarks>
  TMCPEnumOption = record
    Value: string;
    Title: string;

    class function New(const AValue: string; const ATitle: string = ''): TMCPEnumOption; static;
  end;

  TMCPEnumOptions = TArray<TMCPEnumOption>;

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
    /// <summary>The property name inside requestedSchema.properties.</summary>
    Name: string;

    /// <summary>Label shown to the user.</summary>
    Title: NullString;

    /// <summary>Longer explanation shown to the user.</summary>
    Description: NullString;

    /// <summary>Whether the client must collect a value for this property.</summary>
    Required: Boolean;
  protected
    /// <summary>The JSON Schema "type" of this property.</summary>
    function SchemaType: string; virtual; abstract;

    /// <summary>Writes the variant-specific keywords. AJson already has "type".</summary>
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
  ///   Common part of the two enum shapes: the options, and whether any of them
  ///   carries a display title.
  /// </summary>
  TMCPEnumSchemaBase = class(TMCPPrimitiveSchema)
  public
    Options: TMCPEnumOptions;
  public
    /// <summary>Appends one option, returning Self so options can be chained.</summary>
    function AddOption(const AValue: string; const ATitle: string = ''): TMCPEnumSchemaBase;

    /// <summary>
    ///   True when at least one option carries a title, which is what selects
    ///   the titled shape of the schema over the untitled one.
    /// </summary>
    function HasTitles: Boolean;

    /// <summary>The option values, in declaration order.</summary>
    function Values: TArray<string>;
  end;

  /// <summary>
  ///   A single-choice property: SingleSelectEnumSchema, rendered untitled as
  ///   {"type":"string","enum":[...]} or titled as
  ///   {"type":"string","oneOf":[{"const":...,"title":...}]}.
  /// </summary>
  TMCPEnumSchema = class(TMCPEnumSchemaBase)
  public
    DefaultValue: NullString;

    /// <summary>
    ///   Emits the deprecated LegacyTitledEnumSchema shape - "enum" alongside a
    ///   parallel "enumNames" - instead of "oneOf". Only for clients that have
    ///   not caught up; "enumNames" is not standard JSON Schema 2020-12.
    /// </summary>
    Legacy: Boolean;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   A multiple-choice property: MultiSelectEnumSchema, an array of the
  ///   allowed values, rendered untitled as
  ///   {"type":"array","items":{"type":"string","enum":[...]}} or titled as
  ///   {"type":"array","items":{"anyOf":[{"const":...,"title":...}]}}.
  /// </summary>
  TMCPMultiEnumSchema = class(TMCPEnumSchemaBase)
  public
    MinItems: NullInteger;
    MaxItems: NullInteger;
    DefaultValue: TArray<string>;
  protected
    function SchemaType: string; override;
    procedure WriteKeywords(AJson: TJSONObject); override;
  end;

  /// <summary>
  ///   The object schema of a form-mode elicitation: the "requestedSchema" the
  ///   server sends and the client renders.
  /// </summary>
  /// <example>
  ///   <code>
  ///   LSchema := TMCPElicitationSchema.Create;
  ///   LSchema.AddString('name', 'Your name', True).MinLength := 2;
  ///   LSchema.AddInteger('age', 'Your age');
  ///   LSchema.AddEnum('lang', 'Language', ['delphi', 'pascal'], True);
  ///   </code>
  /// </example>
  TMCPElicitationSchema = class
  private
    FProperties: TObjectList<TMCPPrimitiveSchema>;
    function AddProperty<T: TMCPPrimitiveSchema, constructor>(const AName, ATitle: string;
      ARequired: Boolean): T;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Declares a free-text property.</summary>
    function AddString(const AName, ATitle: string; ARequired: Boolean = False): TMCPStringSchema;

    /// <summary>Declares a whole-number property.</summary>
    function AddInteger(const AName, ATitle: string; ARequired: Boolean = False): TMCPNumberSchema;

    /// <summary>Declares a real-number property.</summary>
    function AddNumber(const AName, ATitle: string; ARequired: Boolean = False): TMCPNumberSchema;

    /// <summary>Declares a yes/no property.</summary>
    function AddBoolean(const AName, ATitle: string; ARequired: Boolean = False): TMCPBooleanSchema;

    /// <summary>
    ///   Declares a single-choice property from bare values. Add titled options
    ///   with AddOption on the returned schema.
    /// </summary>
    function AddEnum(const AName, ATitle: string; const AValues: TArray<string>;
      ARequired: Boolean = False): TMCPEnumSchema;

    /// <summary>Declares a multiple-choice property from bare values.</summary>
    function AddMultiEnum(const AName, ATitle: string; const AValues: TArray<string>;
      ARequired: Boolean = False): TMCPMultiEnumSchema;

    /// <summary>The property declared under AName, or nil.</summary>
    function Find(const AName: string): TMCPPrimitiveSchema;

    /// <summary>
    ///   Renders the whole object schema. The caller owns the returned object.
    /// </summary>
    function ToJSON: TJSONObject;
    function ToJSONString(APrettyPrint: Boolean = False): string;

    /// <summary>The declared properties, in declaration order.</summary>
    property Properties: TObjectList<TMCPPrimitiveSchema> read FProperties;
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
    class function Form(const AMessage: string; ASchema: TMCPElicitationSchema): TElicitRequestParams; static;

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

{ TMCPEnumOption }

class function TMCPEnumOption.New(const AValue, ATitle: string): TMCPEnumOption;
begin
  Result.Value := AValue;
  Result.Title := ATitle;
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

{ TMCPEnumSchemaBase }

function TMCPEnumSchemaBase.AddOption(const AValue, ATitle: string): TMCPEnumSchemaBase;
begin
  Options := Options + [TMCPEnumOption.New(AValue, ATitle)];
  Result := Self;
end;

function TMCPEnumSchemaBase.HasTitles: Boolean;
var
  LOption: TMCPEnumOption;
begin
  for LOption in Options do
    if not LOption.Title.IsEmpty then
      Exit(True);

  Result := False;
end;

function TMCPEnumSchemaBase.Values: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Length(Options));
  for I := 0 to High(Options) do
    Result[I] := Options[I].Value;
end;

{ TMCPEnumSchema }

function TMCPEnumSchema.SchemaType: string;
begin
  Result := 'string';
end;

procedure TMCPEnumSchema.WriteKeywords(AJson: TJSONObject);
var
  LValues, LNames, LOneOf: TJSONArray;
  LOption: TMCPEnumOption;
  LEntry: TJSONObject;
begin
  if Length(Options) = 0 then
    raise EMCPException.Create(SMCPElicitEnumEmpty);

  if Legacy or not HasTitles then
  begin
    // UntitledSingleSelectEnumSchema, or its deprecated titled predecessor
    LValues := TJSONArray.Create;
    AJson.AddPair('enum', LValues);
    for LOption in Options do
      LValues.Add(LOption.Value);

    if Legacy and HasTitles then
    begin
      LNames := TJSONArray.Create;
      AJson.AddPair('enumNames', LNames);
      for LOption in Options do
        LNames.Add(LOption.Title);
    end;
  end
  else
  begin
    // TitledSingleSelectEnumSchema
    LOneOf := TJSONArray.Create;
    AJson.AddPair('oneOf', LOneOf);
    for LOption in Options do
    begin
      LEntry := TJSONObject.Create;
      LOneOf.AddElement(LEntry);
      LEntry.AddPair('const', LOption.Value);
      LEntry.AddPair('title', LOption.Title);
    end;
  end;

  if DefaultValue.HasValue then
    AJson.AddPair('default', DefaultValue.Value);
end;

{ TMCPMultiEnumSchema }

function TMCPMultiEnumSchema.SchemaType: string;
begin
  Result := 'array';
end;

procedure TMCPMultiEnumSchema.WriteKeywords(AJson: TJSONObject);
var
  LItems, LEntry: TJSONObject;
  LValues, LAnyOf, LDefaults: TJSONArray;
  LOption: TMCPEnumOption;
  LValue: string;
begin
  if Length(Options) = 0 then
    raise EMCPException.Create(SMCPElicitEnumEmpty);

  LItems := TJSONObject.Create;
  AJson.AddPair('items', LItems);

  if HasTitles then
  begin
    // TitledMultiSelectEnumSchema: the choices live under items.anyOf
    LAnyOf := TJSONArray.Create;
    LItems.AddPair('anyOf', LAnyOf);
    for LOption in Options do
    begin
      LEntry := TJSONObject.Create;
      LAnyOf.AddElement(LEntry);
      LEntry.AddPair('const', LOption.Value);
      LEntry.AddPair('title', LOption.Title);
    end;
  end
  else
  begin
    // UntitledMultiSelectEnumSchema: items is itself a string enum
    LItems.AddPair('type', 'string');
    LValues := TJSONArray.Create;
    LItems.AddPair('enum', LValues);
    for LOption in Options do
      LValues.Add(LOption.Value);
  end;

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

{ TMCPElicitationSchema }

constructor TMCPElicitationSchema.Create;
begin
  FProperties := TObjectList<TMCPPrimitiveSchema>.Create(True);
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

function TMCPElicitationSchema.AddEnum(const AName, ATitle: string;
  const AValues: TArray<string>; ARequired: Boolean): TMCPEnumSchema;
var
  LValue: string;
begin
  Result := AddProperty<TMCPEnumSchema>(AName, ATitle, ARequired);
  for LValue in AValues do
    Result.AddOption(LValue);
end;

function TMCPElicitationSchema.AddMultiEnum(const AName, ATitle: string;
  const AValues: TArray<string>; ARequired: Boolean): TMCPMultiEnumSchema;
var
  LValue: string;
begin
  Result := AddProperty<TMCPMultiEnumSchema>(AName, ATitle, ARequired);
  for LValue in AValues do
    Result.AddOption(LValue);
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

    // "required" is omitted rather than emitted empty: an empty array would
    // read as a deliberate "nothing is required" rather than as absence
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

function TMCPElicitationSchema.ToJSONString(APrettyPrint: Boolean): string;
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

{ TMCPElicitRequest }

class function TMCPElicitRequest.Form(const AMessage: string;
  ASchema: TMCPElicitationSchema): TElicitRequestParams;
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
