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
unit MCPConnect.Tests.MCP.Configuration;

interface

uses
  System.SysUtils, System.JSON, System.Rtti,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Server,
  MCPConnect.JRPC.Core,
  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  // Plain class with no MCP attributes at all: this is the class shape a
  // C++ Builder consumer would use, registered purely through the fluent
  // RegisterTool/WithParam/EndTool API instead of [McpTool]/[McpParam].
  TManualToolClass = class
  public
    function DoubleOrZero(AValue: Integer; ADouble: Boolean): Integer;
    function Concat(const AFirst, ASecond: string): string;
    function NoParams: string;
  end;

  TManualPerson = record
    Name: string;
    Age: Integer;
  end;

  TManualStructuredClass = class
  public
    function GetPerson: TManualPerson;
  end;

  // Attribute-based class carrying an [McpApp] tag, used to reproduce/verify
  // the fix for the RTTI-cached-attribute mutation bug in RegisterClass.
  TAppToolClass = class
  public
    [McpTool('app_tool', 'A tool with an attached UI')]
    [McpApp('ui://test/app')]
    function ShowApp: string;
  end;

  [TestFixture]
  TMCPToolsConfigRegisterToolTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegisterTool_RegistersToolWithNameAndDescription;
    [Test]
    procedure TestRegisterTool_CapturesClassAndMethod;
    [Test]
    procedure TestRegisterTool_InputSchemaUsesConfiguredParamNamesAndDescriptions;
    [Test]
    procedure TestRegisterTool_NoParamsMethodProducesEmptySchema;
    [Test]
    procedure TestRegisterTool_TagsApplyCategoryAndAnnotations;
    [Test]
    procedure TestRegisterTool_StructuredTagProducesOutputSchema;
    [Test]
    procedure TestRegisterTool_UnknownMethodRaises;
    [Test]
    procedure TestRegisterTool_MissingParamConfigurationRaises;
    [Test]
    procedure TestRegisterTool_WrongParamNameRaises;
    [Test]
    procedure TestRegisterTool_RegisteredMethodIsInvokable;
    [Test]
    procedure TestRegisterTool_AppearsInListEnabled;
    [Test]
    procedure TestRegisterTool_DisabledTagHidesFromListEnabled;
    [Test]
    procedure TestRegisterTool_AbandonedChainWithoutEndToolIsNotRegistered;

    [Test]
    procedure TestRegisterClass_SameClassTwiceWithAppAttributeDoesNotRaise;
  end;

implementation

{ TManualToolClass }

function TManualToolClass.DoubleOrZero(AValue: Integer; ADouble: Boolean): Integer;
begin
  if ADouble then
    Result := AValue * 2
  else
    Result := 0;
end;

function TManualToolClass.Concat(const AFirst, ASecond: string): string;
begin
  Result := AFirst + ASecond;
end;

function TManualToolClass.NoParams: string;
begin
  Result := 'ok';
end;

{ TManualStructuredClass }

function TManualStructuredClass.GetPerson: TManualPerson;
begin
  Result.Name := 'Ada';
  Result.Age := 36;
end;

{ TAppToolClass }

function TAppToolClass.ShowApp: string;
begin
  Result := 'app';
end;

{ TMCPToolsConfigRegisterToolTest }

procedure TMCPToolsConfigRegisterToolTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPToolsConfigRegisterToolTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_RegistersToolWithNameAndDescription;
var
  LTool: TMCPTool;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'double_or_zero', 'Doubles or zeroes the value')
    .WithParam('AValue', 'value', 'The value to process')
    .WithParam('ADouble', 'double', 'Whether to double it')
    .EndTool;

  Assert.IsTrue(FConfig.Tools.Registry.ContainsKey('double_or_zero'), 'Tool should be registered under its configured name');
  LTool := FConfig.Tools.Registry['double_or_zero'];
  Assert.AreEqual('double_or_zero', LTool.Name);
  Assert.AreEqual('Doubles or zeroes the value', LTool.Description.Value);
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_CapturesClassAndMethod;
var
  LTool: TMCPTool;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'double_or_zero', 'Doubles or zeroes the value')
    .WithParam('AValue', 'value', 'The value to process')
    .WithParam('ADouble', 'double', 'Whether to double it')
    .EndTool;

  LTool := FConfig.Tools.Registry['double_or_zero'];
  Assert.AreEqual(TClass(TManualToolClass), LTool.Classe, 'Classe should point back to the registered class');
  Assert.AreEqual('DoubleOrZero', LTool.Method.Name, 'Method should be the RTTI method for the configured method name');
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_InputSchemaUsesConfiguredParamNamesAndDescriptions;
var
  LTool: TMCPTool;
  LProps: TJSONObject;
  LRequired: TJSONArray;
  LValueProp: TJSONObject;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'double_or_zero', 'Doubles or zeroes the value')
    .WithParam('AValue', 'value', 'The value to process')
    .WithParam('ADouble', 'double', 'Whether to double it')
    .EndTool;

  LTool := FConfig.Tools.Registry['double_or_zero'];
  LProps := LTool.InputSchema.GetValue('properties') as TJSONObject;
  Assert.IsNotNull(LProps, 'InputSchema should have a properties object');

  // The JSON-facing property name comes from WithParam's AName argument,
  // not the Delphi parameter name (AValue/ADouble).
  Assert.IsNull(LProps.GetValue('AValue'), 'Delphi parameter name should not leak into the schema');
  LValueProp := LProps.GetValue('value') as TJSONObject;
  Assert.IsNotNull(LValueProp, 'Configured param name "value" should be a schema property');
  Assert.AreEqual('The value to process', (LValueProp.GetValue('description') as TJSONString).Value);

  Assert.IsNotNull(LProps.GetValue('double'), 'Configured param name "double" should be a schema property');

  LRequired := LTool.InputSchema.GetValue('required') as TJSONArray;
  Assert.IsNotNull(LRequired, 'Required array should be present');
  Assert.AreEqual(2, LRequired.Count);
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_NoParamsMethodProducesEmptySchema;
var
  LTool: TMCPTool;
  LProps: TJSONObject;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'NoParams', 'no_params', 'A tool with no parameters')
    .EndTool;

  LTool := FConfig.Tools.Registry['no_params'];
  LProps := LTool.InputSchema.GetValue('properties') as TJSONObject;
  Assert.IsNotNull(LProps);
  Assert.AreEqual(0, LProps.Count, 'Properties should be empty for a parameterless method');
  Assert.IsNull(LTool.InputSchema.GetValue('required'), 'Required array should be omitted when there are no params');
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_TagsApplyCategoryAndAnnotations;
var
  LTool: TMCPTool;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'NoParams', 'tagged_tool', 'A tagged tool',
    'category=demo,readonly,destructive,idempotent,openworld')
    .EndTool;

  LTool := FConfig.Tools.Registry['tagged_tool'];
  Assert.AreEqual('demo', LTool.Category);
  Assert.IsTrue(LTool.Annotations.ReadOnlyHint.HasValue and LTool.Annotations.ReadOnlyHint.Value);
  Assert.IsTrue(LTool.Annotations.DestructiveHint.HasValue and LTool.Annotations.DestructiveHint.Value);
  Assert.IsTrue(LTool.Annotations.IdempotentHint.HasValue and LTool.Annotations.IdempotentHint.Value);
  Assert.IsTrue(LTool.Annotations.OpenWorldHint.HasValue and LTool.Annotations.OpenWorldHint.Value);
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_StructuredTagProducesOutputSchema;
var
  LTool: TMCPTool;
  LSchemaProps: TJSONObject;
begin
  FConfig.Tools.RegisterTool(TManualStructuredClass, 'GetPerson', 'get_person', 'Returns person info', 'structured')
    .EndTool;

  LTool := FConfig.Tools.Registry['get_person'];
  Assert.AreEqual('object', (LTool.OutputSchema.GetValue('type') as TJSONString).Value);
  LSchemaProps := LTool.OutputSchema.GetValue('properties') as TJSONObject;
  Assert.IsNotNull(LSchemaProps);
  Assert.IsNotNull(LSchemaProps.GetValue('name'));
  Assert.IsNotNull(LSchemaProps.GetValue('age'));
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_UnknownMethodRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterTool(TManualToolClass, 'NoSuchMethod', 'x', 'y');
    end,
    EMCPException
  );
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_MissingParamConfigurationRaises;
begin
  // DoubleOrZero has two parameters, only one is configured via WithParam
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'incomplete_tool', 'y')
        .WithParam('AValue', 'value', 'desc')
        .EndTool;
    end,
    EJRPCException
  );
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_WrongParamNameRaises;
begin
  // Same param count, but one configured name doesn't match the method's actual parameter name
  Assert.WillRaise(
    procedure
    begin
      FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'mismatched_tool', 'y')
        .WithParam('WrongName', 'value', 'desc')
        .WithParam('ADouble', 'double', 'desc2')
        .EndTool;
    end,
    EJRPCException
  );
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_RegisteredMethodIsInvokable;
var
  LTool: TMCPTool;
  LInstance: TManualToolClass;
  LResult: TValue;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'double_or_zero', 'Doubles or zeroes the value')
    .WithParam('AValue', 'value', 'The value to process')
    .WithParam('ADouble', 'double', 'Whether to double it')
    .EndTool;

  LTool := FConfig.Tools.Registry['double_or_zero'];
  LInstance := TManualToolClass.Create;
  try
    LResult := LTool.Method.Invoke(LInstance, [TValue.From<Integer>(21), TValue.From<Boolean>(True)]);
    Assert.AreEqual(42, LResult.AsInteger, 'The registered RTTI method should actually run the business logic');
  finally
    LInstance.Free;
  end;
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_AppearsInListEnabled;
var
  LList: TListToolsResult;
  LFound: Boolean;
  LTool: TMCPTool;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'NoParams', 'enabled_tool', 'y')
    .EndTool;

  LList := FConfig.Tools.ListEnabled;
  try
    LFound := False;
    for LTool in LList.Tools do
      if LTool.Name = 'enabled_tool' then
        LFound := True;
    Assert.IsTrue(LFound, 'Tool registered via RegisterTool should appear in ListEnabled');
  finally
    LList.Free;
  end;
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_DisabledTagHidesFromListEnabled;
var
  LList: TListToolsResult;
  LTool: TMCPTool;
begin
  FConfig.Tools.RegisterTool(TManualToolClass, 'NoParams', 'disabled_tool', 'y', 'disabled')
    .EndTool;

  Assert.IsTrue(FConfig.Tools.Registry.ContainsKey('disabled_tool'), 'Disabled tool should still be in the registry');
  Assert.IsTrue(FConfig.Tools.Registry['disabled_tool'].Disabled, 'Disabled flag should be set from the "disabled" tag');

  LList := FConfig.Tools.ListEnabled;
  try
    for LTool in LList.Tools do
      Assert.AreNotEqual('disabled_tool', LTool.Name, 'Disabled tool should be filtered out of ListEnabled');
  finally
    LList.Free;
  end;
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterTool_AbandonedChainWithoutEndToolIsNotRegistered;
begin
  // .EndTool is never called: the in-progress TMCPToolConfig stays only in
  // TMCPToolsConfig.Configs and must be cleaned up (BackToMCP/Destroy) without
  // ever reaching the tool Registry.
  FConfig.Tools.RegisterTool(TManualToolClass, 'DoubleOrZero', 'abandoned_tool', 'y')
    .WithParam('AValue', 'value', 'd')
    .WithParam('ADouble', 'double', 'd2');

  Assert.IsFalse(FConfig.Tools.Registry.ContainsKey('abandoned_tool'), 'Tool should not be registered until EndTool is called');
end;

procedure TMCPToolsConfigRegisterToolTest.TestRegisterClass_SameClassTwiceWithAppAttributeDoesNotRaise;
var
  LServer1, LServer2: TJRPCServer;
  LConfig1, LConfig2: IMCPConfig;
begin
  // Regression test: MCPToolAttribute instances are cached by Delphi's RTTI
  // for the process lifetime, so RegisterClass must not mutate the shared
  // Tags.TagMap in a way that breaks a second, independent registration of
  // the same [McpApp]-tagged class.
  LServer1 := TJRPCServer.Create(nil);
  LServer2 := TJRPCServer.Create(nil);
  try
    LConfig1 := LServer1.Plugin.Configure<IMCPConfig>;
    LConfig2 := LServer2.Plugin.Configure<IMCPConfig>;

    LConfig1.Tools.RegisterClass(TAppToolClass);

    Assert.WillNotRaise(
      procedure
      begin
        LConfig2.Tools.RegisterClass(TAppToolClass);
      end
    );

    Assert.AreEqual('ui://test/app', LConfig1.Tools.Registry['app_tool'].UI.ResourceUri.Value);
    Assert.AreEqual('ui://test/app', LConfig2.Tools.Registry['app_tool'].UI.ResourceUri.Value);
  finally
    LConfig1 := nil;
    LConfig2 := nil;
    LServer1.Free;
    LServer2.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPToolsConfigRegisterToolTest);

end.
