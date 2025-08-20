unit MCP.Tools.Schema;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs,
  System.TypInfo, System.Generics.Collections, System.Generics.Defaults,
  System.JSON, Data.DB,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.TypeInfo,
  Neon.Core.Utils,

  MCP.Attributes,
  MCP.Tools;

type
  /// <summary>
  ///   JSON Schema (MCP version) generator
  /// </summary>
  TMCPSchemaGenerator = class(TNeonBase)
  private
    /// <summary>
    ///   Writer for members of objects and records
    /// </summary>
    procedure WriteMembers(AType: TRttiType; AResult: TJSONObject);

    /// <summary>
    ///   Writer for string types
    /// </summary>
    function WriteString(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for Boolean types
    /// </summary>
    function WriteBoolean(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for enums types <br />
    /// </summary>
    function WriteEnum(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for Integer types <br />
    /// </summary>
    function WriteInteger(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for Integer types <br />
    /// </summary>
    function WriteInt64(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for float types
    /// </summary>
    function WriteFloat(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
    function WriteDouble(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for TDate* types
    /// </summary>
    function WriteDate(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
    function WriteDateTime(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for Variant types
    /// </summary>
    /// <remarks>
    ///   The variant will be written as string
    /// </remarks>
    function WriteVariant(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for static and dynamic arrays
    /// </summary>
    function WriteArray(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
    function WriteDynArray(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for the set type
    /// </summary>
    /// <remarks>
    ///   The output is a string with the values comma separated and enclosed by square brackets
    /// </remarks>
    /// <returns>[First,Second,Third]</returns>
    function WriteSet(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for a record type
    /// </summary>
    /// <remarks>
    ///   For records the engine serialize the fields by default
    /// </remarks>
    function WriteRecord(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for a standard TObject (descendants)  type (no list, stream or streamable)
    /// </summary>
    function WriteObject(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for an Interface type
    /// </summary>
    /// <remarks>
    ///   The object that implements the interface is serialized
    /// </remarks>
    function WriteInterface(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for TStream (descendants) objects
    /// </summary>
    function WriteStream(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for TDataSet (descendants) objects
    /// </summary>
    function WriteDataSet(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;

    /// <summary>
    ///   Writer for "Enumerable" objects (Lists, Generic Lists, TStrings, etc...)
    /// </summary>
    /// <remarks>
    ///   Objects must have GetEnumerator, Clear, Add methods
    /// </remarks>
    function WriteEnumerable(AType: TRttiType; ANeonObject: TNeonRttiObject; AList: INeonTypeInfoList): TJSONObject;
    function IsEnumerable(AType: TRttiType; out AList: INeonTypeInfoList): Boolean;

    /// <summary>
    ///   Writer for "Dictionary" objects (TDictionary, TObjectDictionary)
    /// </summary>
    /// <remarks>
    ///   Objects must have Keys, Values, GetEnumerator, Clear, Add methods
    /// </remarks>
    function WriteEnumerableMap(AType: TRttiType; ANeonObject: TNeonRttiObject; AMap: INeonTypeInfoMap): TJSONObject;
    function IsEnumerableMap(AType: TRttiType; out AMap: INeonTypeInfoMap): Boolean;

    /// <summary>
    ///   Writer for "Streamable" objects
    /// </summary>
    /// <remarks>
    ///   Objects must have LoadFromStream and SaveToStream methods
    /// </remarks>
    function WriteStreamable(AType: TRttiType; ANeonObject: TNeonRttiObject; AStream: INeonTypeInfoStream): TJSONObject;
    function IsStreamable(AType: TRttiType; out AStream: INeonTypeInfoStream): Boolean;

    /// <summary>
    ///   Writer for "Nullable" records
    /// </summary>
    /// <remarks>
    ///   Record must have HasValue and GetValue methods
    /// </remarks>
    function WriteNullable(AType: TRttiType; ANeonObject: TNeonRttiObject; ANullable: INeonTypeInfoNullable): TJSONObject;

    /// <summary>
    ///   Writer for a method's params
    /// </summary>
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);

    /// <summary>
    ///   Writer for Integer types <br />
    /// </summary>
    ///
    function WriteMethod(AMethod: TRttiMethod): TJSONObject;
    function WriteMethods(AType: TRttiType): TJSONArray;



    function IsNullable(AType: TRttiType; out ANullable: INeonTypeInfoNullable): Boolean;
    function FindAttribute<T: TCustomAttribute>(const Attributes: TArray<TCustomAttribute>): T;
  protected
    /// <summary>
    ///   Function to be called by a custom serializer method (ISerializeContext)
    /// </summary>
    function WriteDataMember(AType: TRttiType): TJSONObject; overload;

    /// <summary>
    ///   This method chooses the right Writer based on the Kind of the AValue parameter
    /// </summary>
    function WriteDataMember(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject; overload;
  public
    constructor Create(const AConfig: INeonConfiguration);

    /// <summary>
    ///   Serialize any Delphi type into a JSONValue, the Delphi type must be passed as a TRttiType
    /// </summary>
    class function TypeToJSONSchema(AType: TRttiType): TJSONObject; overload;
    class function TypeToJSONSchema(AType: TRttiType; AConfig: INeonConfiguration): TJSONObject; overload;

    /// <summary>
    ///   Serialize any Delphi type into a JSONValue, the Delphi type must be passed as a TRttiType
    /// </summary>
    class function ClassToJSONSchema(AClass: TClass): TJSONObject; overload;
    class function ClassToJSONSchema(AClass: TClass; AConfig: INeonConfiguration): TJSONObject; overload;

    /// <summary>
    ///   Serialize any Delphi type into a JSONValue, the Delphi type must be passed as a TRttiType
    /// </summary>
    class function MethodToTool(AMethod: TRttiMethod): TJSONObject; overload;
    class function MethodToTool(AMethod: TRttiMethod; AConfig: INeonConfiguration): TJSONObject; overload;

    /// <summary>
    ///   Serialize any Delphi type into a JSONValue, the Delphi type must be passed as a TRttiType
    /// </summary>
    class function TypeToTools(AType: TRttiType): TJSONValue; overload;
    class function TypeToTools(AType: TRttiType; AConfig: INeonConfiguration): TJSONValue; overload;

    /// <summary>
    ///   Serialize any Delphi type into a JSONValue, the Delphi type must be passed as a TRttiType
    /// </summary>
    class function ClassToTools(AClass: TClass): TJSONValue; overload;
    class function ClassToTools(AClass: TClass; AConfig: INeonConfiguration): TJSONValue; overload;

  end;


implementation

uses
  System.Variants;





{ TMCPSchemaGenerator }

function TMCPSchemaGenerator.FindAttribute<T>(const Attributes: TArray<TCustomAttribute>): T;
var
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  for LAttribute in Attributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      Result := LAttribute as T;

      Break;
    end;
  end;
end;

class function TMCPSchemaGenerator.ClassToJSONSchema(AClass: TClass): TJSONObject;
begin
  Result := TypeToJSONSchema(TRttiUtils.Context.GetType(AClass), TNeonConfiguration.Default);
end;

class function TMCPSchemaGenerator.ClassToJSONSchema(AClass: TClass; AConfig: INeonConfiguration): TJSONObject;
begin
  Result := TypeToJSONSchema(TRttiUtils.Context.GetType(AClass), AConfig);
end;

class function TMCPSchemaGenerator.ClassToTools(AClass: TClass; AConfig: INeonConfiguration): TJSONValue;
begin

end;

class function TMCPSchemaGenerator.ClassToTools(AClass: TClass): TJSONValue;
begin
  Result := TypeToTools(TRttiUtils.Context.GetType(AClass), TNeonConfiguration.Default);
end;

constructor TMCPSchemaGenerator.Create(const AConfig: INeonConfiguration);
begin
  inherited Create(AConfig);
  FOperation := TNeonOperation.Serialize;
end;

function TMCPSchemaGenerator.IsEnumerable(AType: TRttiType; out AList: INeonTypeInfoList): Boolean;
begin
  AList := TNeonTypeInfoList.GuessType(AType);
  Result := Assigned(AList);
end;

function TMCPSchemaGenerator.IsEnumerableMap(AType: TRttiType; out AMap: INeonTypeInfoMap): Boolean;
begin
  AMap := TNeonTypeInfoMap.GuessType(AType);
  Result := Assigned(AMap);
end;

function TMCPSchemaGenerator.IsNullable(AType: TRttiType; out ANullable: INeonTypeInfoNullable): Boolean;
begin
  ANullable := TNeonTypeInfoNullable.GuessType(AType);
  Result := Assigned(ANullable);
end;

function TMCPSchemaGenerator.IsStreamable(AType: TRttiType; out AStream: INeonTypeInfoStream): Boolean;
begin
  AStream := TNeonTypeInfoStream.GuessType(AType);
  Result := Assigned(AStream);
end;

class function TMCPSchemaGenerator.MethodToTool(AMethod: TRttiMethod; AConfig: INeonConfiguration): TJSONObject;
var
  LGenerator: TMCPSchemaGenerator;
begin
  LGenerator := TMCPSchemaGenerator.Create(AConfig);
  try
    Result := LGenerator.WriteMethod(AMethod);
  finally
    LGenerator.Free;
  end;
end;

class function TMCPSchemaGenerator.MethodToTool(AMethod: TRttiMethod):
    TJSONObject;
begin
  Result := MethodToTool(AMethod, TNeonConfiguration.Default);
end;

class function TMCPSchemaGenerator.TypeToJSONSchema(AType: TRttiType; AConfig: INeonConfiguration): TJSONObject;
var
  LGenerator: TMCPSchemaGenerator;
begin
  LGenerator := TMCPSchemaGenerator.Create(AConfig);
  try
    Result := LGenerator.WriteDataMember(AType);
  finally
    LGenerator.Free;
  end;
end;

class function TMCPSchemaGenerator.TypeToTools(AType: TRttiType; AConfig: INeonConfiguration): TJSONValue;
var
  LGenerator: TMCPSchemaGenerator;
begin
  LGenerator := TMCPSchemaGenerator.Create(AConfig);
  try
    Result := LGenerator.WriteMethods(AType);
  finally
    LGenerator.Free;
  end;
end;

class function TMCPSchemaGenerator.TypeToTools(AType: TRttiType): TJSONValue;
begin
  Result := TypeToTools(AType, TNeonConfiguration.Default);
end;

class function TMCPSchemaGenerator.TypeToJSONSchema(AType: TRttiType): TJSONObject;
begin
  Result := TypeToJSONSchema(AType, TNeonConfiguration.Default);
end;

function TMCPSchemaGenerator.WriteArray(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LItems: TJSONObject;
begin
  LItems := WriteDataMember((AType as TRttiArrayType).ElementType);
  Result := TJSONObject.Create
    .AddPair('type', 'array')
    .AddPair('items', LItems)
end;

function TMCPSchemaGenerator.WriteBoolean(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'boolean');
end;

function TMCPSchemaGenerator.WriteDataMember(AType: TRttiType): TJSONObject;
var
  LNeonObject: TNeonRttiObject;
begin
  LNeonObject := TNeonRttiObject.Create(AType, FOperation);
  LNeonObject.ParseAttributes;
  try
    Result := WriteDataMember(AType, LNeonObject);
  finally
    LNeonObject.Free;
  end;
end;

function TMCPSchemaGenerator.WriteDataMember(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LNeonTypeInfo: INeonTypeInfo;

  LNeonMap: INeonTypeInfoMap absolute LNeonTypeInfo;
  LNeonList: INeonTypeInfoList absolute LNeonTypeInfo;
  LNeonStream: INeonTypeInfoStream absolute LNeonTypeInfo;
  LNeonNullable: INeonTypeInfoNullable absolute LNeonTypeInfo;
begin
  Result := nil;

  case AType.TypeKind of
    tkChar,
    tkWChar,
    tkString,
    tkLString,
    tkWString,
    tkUString:
    begin
      Result := WriteString(AType, ANeonObject);
    end;

    tkEnumeration:
    begin
      if AType.Handle = System.TypeInfo(Boolean) then
        Result := WriteBoolean(AType, ANeonObject)
      else
        Result := WriteEnum(AType, ANeonObject);
    end;

    tkInteger:
    begin
      Result := WriteInteger(AType, ANeonObject);
    end;

    tkInt64:
    begin
      Result := WriteInt64(AType, ANeonObject);
    end;

    tkFloat:
    begin
      if AType.Handle = TypeInfo(Single) then
        Result := WriteFloat(AType, ANeonObject)
      else if AType.Handle = TypeInfo(TDateTime) then
        Result := WriteDateTime(AType, ANeonObject)
      else if AType.Handle = TypeInfo(TTime) then
        Result := WriteDateTime(AType, ANeonObject)
      else if AType.Handle = TypeInfo(TDate) then
        Result := WriteDate(AType, ANeonObject)
      else
        Result := WriteDouble(AType, ANeonObject);
    end;

    tkClass:
    begin
      if AType.IsInstance and AType.AsInstance.MetaclassType.InheritsFrom(TDataSet) then
        Result := WriteDataSet(AType, ANeonObject)
      else if AType.IsInstance and AType.AsInstance.MetaclassType.InheritsFrom(TStream) then
        Result := WriteStream(AType, ANeonObject)
      else if IsEnumerableMap(AType, LNeonMap) then
        Result := WriteEnumerableMap(AType, ANeonObject, LNeonMap)
      else if IsEnumerable(AType, LNeonList) then
        Result := WriteEnumerable(AType, ANeonObject, LNeonList)
      else if IsStreamable(AType, LNeonStream) then
        Result := WriteStreamable(AType, ANeonObject, LNeonStream)
      else
        Result := WriteObject(AType, ANeonObject);
    end;

    tkArray:
    begin
      Result := WriteArray(AType, ANeonObject);
    end;

    tkDynArray:
    begin
      Result := WriteDynArray(AType, ANeonObject);
    end;

    tkSet:
    begin
      Result := WriteSet(AType, ANeonObject);
    end;

    tkRecord:
    begin
      if IsNullable(AType, LNeonNullable) then
        Result := WriteNullable(AType, ANeonObject, LNeonNullable)
      else
        Result := WriteRecord(AType, ANeonObject);
    end;

    tkInterface:
    begin
      Result := WriteInterface(AType, ANeonObject);
    end;

    tkVariant:
    begin
      Result := WriteVariant(AType, ANeonObject);
    end;
  end;
end;

function TMCPSchemaGenerator.WriteDataSet(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LJSONProps: TJSONObject;
begin
  //Result := TDataSetUtils.RecordToJSONSchema(AValue.AsObject as TDataSet, FConfig);

  LJSONProps := TJSONObject.Create;
  Result := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LJSONProps);
end;

function TMCPSchemaGenerator.WriteDate(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string')
    .AddPair('format', 'date');
end;

function TMCPSchemaGenerator.WriteDateTime(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string')
    .AddPair('format', 'date-time');
end;

function TMCPSchemaGenerator.WriteDouble(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'number')
    .AddPair('format', 'double');
end;

function TMCPSchemaGenerator.WriteDynArray(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LItems: TJSONObject;
begin
  LItems := WriteDataMember((AType as TRttiDynamicArrayType).ElementType);
  Result := TJSONObject.Create
    .AddPair('type', 'array')
    .AddPair('items', LItems)
end;

function TMCPSchemaGenerator.WriteEnum(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LTypeData: PTypeData;
  LIndex: Integer;
  LEnumArray: TJSONArray;
begin
  LTypeData := GetTypeData(AType.Handle);

  LEnumArray := TJSONArray.Create;
  for LIndex := LTypeData.MinValue to LTypeData.MaxValue do
    LEnumArray.Add(TTypeInfoUtils.EnumToString(AType.Handle, LIndex, ANeonObject));

  Result := TJSONObject.Create
    .AddPair('type', 'string')
    .AddPair('enum', LEnumArray);
end;

function TMCPSchemaGenerator.WriteFloat(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'number')
    .AddPair('format', 'float');
end;

function TMCPSchemaGenerator.WriteInt64(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'integer')
    .AddPair('format', 'int64');
end;

function TMCPSchemaGenerator.WriteInteger(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'integer')
    .AddPair('format', 'int32');
end;

function TMCPSchemaGenerator.WriteInterface(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := nil;
end;

procedure TMCPSchemaGenerator.WriteMembers(AType: TRttiType; AResult: TJSONObject);
var
  LJSONObj: TJSONObject;
  LMembers: TNeonRttiMembers;
  LNeonMember: TNeonRttiMember;
begin
  LMembers := GetNeonMembers(AType);
  LMembers.FilterSerialize(nil);

  for LNeonMember in LMembers do
  begin
    if LNeonMember.Serializable then
    begin
      try
        LJSONObj := WriteDataMember(LNeonMember.RttiType, LNeonMember);

        if Assigned(LJSONObj) then
          (AResult as TJSONObject).AddPair(GetNameFromMember(LNeonMember), LJSONObj);
      except
        LogError(Format('Error converting property [%s] of object [%s]',
          [LNeonMember.Name, AType.Name]));
      end;
    end;
  end;
end;

function TMCPSchemaGenerator.WriteMethod(AMethod: TRttiMethod): TJSONObject;
var
  LToolName, LToolDesc: string;
  LProps, LSchema: TJSONObject;
  LAttr: MCPToolAttribute;
  LRequired: TJSONArray;
begin
  LProps := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    WriteParams(AMethod, LProps, LRequired);
  except
    LProps.Free;
    LRequired.Free;
    raise;
  end;

  LSchema := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProps)
    .AddPair('additionalProperties', False)
    .AddPair('$schema', 'http://json-schema.org/draft-07/schema#');

  LToolName := AMethod.Name;
  LToolDesc := AMethod.Name;

  LAttr := FindAttribute<MCPToolAttribute>(AMethod.GetAttributes);
  if Assigned(LAttr) then
  begin
    if LAttr.Name <> '' then
      LToolName := LAttr.Name;

    if LAttr.Description <> '' then
      LToolDesc := LAttr.Description;
  end;

  Result := TJSONObject.Create
    .AddPair('name', LToolName)
    .AddPair('description', LToolDesc)
    .AddPair('inputSchema', LSchema);

  if LRequired.Count > 0 then
    Result.AddPair('required', LRequired);
end;

function TMCPSchemaGenerator.WriteMethods(AType: TRttiType): TJSONArray;
begin
  var methods := AType.GetMethods;
  var tool: TJSONObject;

  Result := TJSONArray.Create;

  for var m in methods do
    if Assigned(m.GetAttribute(MCPToolAttribute)) then
    begin
      tool := WriteMethod(m);
      Result.AddElement(tool);
    end;

end;

function TMCPSchemaGenerator.WriteNullable(AType: TRttiType; ANeonObject: TNeonRttiObject; ANullable: INeonTypeInfoNullable): TJSONObject;
begin
  Result := nil;

  if Assigned(ANullable) then
    Result := WriteDataMember(ANullable.GetBaseType)
end;

function TMCPSchemaGenerator.WriteObject(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LProperties: TJSONObject;
begin
  LProperties := TJSONObject.Create;

  WriteMembers(AType, LProperties);

  Result := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProperties);
end;

procedure TMCPSchemaGenerator.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
  LAttr: MCPParamAttribute;
begin
  for LParam in AMethod.GetParameters do
  begin
    LAttr := FindAttribute<MCPParamAttribute>(LParam.GetAttributes);
      if not Assigned(LAttr) then
        raise Exception.Create('Non annotated params are not permitted');
    try
      LJSONObj := WriteDataMember(LParam.ParamType);
      LJSONObj.AddPair('description', TJSONString.Create(LAttr.Description));
      AProps.AddPair(LAttr.Name, LJSONObj);
      ARequired.Add(LAttr.Name);
    except
      LogError(Format('Error converting param [%s] of method [%s]',
        [LParam.Name, AMethod.Name]));
    end;
  end;
end;

function TMCPSchemaGenerator.WriteEnumerable(AType: TRttiType; ANeonObject: TNeonRttiObject; AList: INeonTypeInfoList): TJSONObject;
var
  LJSONItems: TJSONObject;
begin
  // Is not an Enumerable compatible object
  if not Assigned(AList) then
    Exit(nil);

  LJSONItems := WriteDataMember(AList.GetItemType);

  Result := TJSONObject.Create
    .AddPair('type', 'array')
    .AddPair('items', LJSONItems);
end;

function TMCPSchemaGenerator.WriteEnumerableMap(AType: TRttiType; ANeonObject: TNeonRttiObject; AMap: INeonTypeInfoMap): TJSONObject;
var
  LValueJSON: TJSONObject;
begin
  // Is not an EnumerableMap-compatible object
  if not Assigned(AMap) then
    Exit(nil);

  LValueJSON := WriteDataMember(AMap.GetValueType);
  Result := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('additionalProperties', LValueJSON);
end;

function TMCPSchemaGenerator.WriteRecord(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
var
  LProperties: TJSONObject;
begin
  LProperties := TJSONObject.Create;

  WriteMembers(AType, LProperties);

  Result := TJSONObject.Create
    .AddPair('type', 'object')
    .AddPair('properties', LProperties);
end;

function TMCPSchemaGenerator.WriteSet(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string');
end;

function TMCPSchemaGenerator.WriteStream(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string')
    .AddPair('format', 'byte');
end;

function TMCPSchemaGenerator.WriteStreamable(AType: TRttiType; ANeonObject: TNeonRttiObject; AStream: INeonTypeInfoStream): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string')
    .AddPair('format', 'byte');
end;

function TMCPSchemaGenerator.WriteString(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
  Result := TJSONObject.Create
    .AddPair('type', 'string');
end;

function TMCPSchemaGenerator.WriteVariant(AType: TRttiType; ANeonObject: TNeonRttiObject): TJSONObject;
begin
{
  case ANeonObject.NeonInclude.Value of
    Include.NotNull:
    begin
      if VarIsNull(AValue.AsVariant) then
        Exit(nil);
    end;
    Include.NotEmpty:
    begin
      if VarIsEmpty(AValue.AsVariant) then
        Exit(nil);
    end;
  end;
}
  Result :=nil;
  //TJSONString.Create(AValue.AsVariant);
end;

end.
