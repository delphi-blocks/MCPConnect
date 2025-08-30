unit MCP.Tools.Schema;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.JSON,
  System.TypInfo, System.Generics.Collections,

  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON.Schema,
  Neon.Core.Utils,

  MCP.Attributes,
  MCP.Tools;

type
  /// <summary>
  ///   JSON Schema (MCP version) generator
  /// </summary>
  TMCPSchemaGenerator = class(TNeonSchemaGenerator)
  protected
    /// <summary>
    ///   Writer for a method's params
    /// </summary>
    procedure WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);

    /// <summary>
    ///   Writer for Integer types
    /// </summary>
    function WriteMethod(AMethod: TRttiMethod): TJSONObject;
    function WriteMethods(AType: TRttiType): TJSONArray;
  public
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

class function TMCPSchemaGenerator.ClassToTools(AClass: TClass; AConfig: INeonConfiguration): TJSONValue;
begin
  Result := TypeToTools(TRttiUtils.Context.GetType(AClass), AConfig);
end;

class function TMCPSchemaGenerator.ClassToTools(AClass: TClass): TJSONValue;
begin
  Result := ClassToTools(AClass, TNeonConfiguration.Default);
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

class function TMCPSchemaGenerator.MethodToTool(AMethod: TRttiMethod): TJSONObject;
begin
  Result := MethodToTool(AMethod, TNeonConfiguration.Default);
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

  LAttr := AMethod.GetAttribute<MCPToolAttribute>;
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
    Result.AddPair('required', LRequired)
  else
    LRequired.Free;
end;

function TMCPSchemaGenerator.WriteMethods(AType: TRttiType): TJSONArray;
var
  LMethod: TRttiMethod;
  LMethods: TArray<TRttiMethod>;
  LTool: TJSONObject;
begin
  Result := TJSONArray.Create;
  try
    LMethods := AType.GetMethods;
    for LMethod in LMethods do
      if Assigned(LMethod.GetAttribute(MCPToolAttribute)) then
      begin
        LTool := WriteMethod(LMethod);
        Result.AddElement(LTool);
      end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMCPSchemaGenerator.WriteParams(AMethod: TRttiMethod; AProps: TJSONObject; ARequired: TJSONArray);
var
  LJSONObj: TJSONObject;
  LParam: TRttiParameter;
  LAttr: MCPParamAttribute;
begin
  for LParam in AMethod.GetParameters do
  begin
    LAttr := LParam.GetAttribute<MCPParamAttribute>;
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

end.
