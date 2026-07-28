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
  MCPConnect.MCP.Resources,
  MCPConnect.MCP.Prompts,
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

  // Plain classes with no MCP attributes at all, registered purely through the
  // one-shot RegisterResource/RegisterTemplate/RegisterUI API instead of
  // [McpResource]/[McpTemplate]/[McpParam]/[McpAppUI].
  TManualResourceClass = class
  public
    function GetReadme: string;
  end;

  TManualTemplateClass = class
  public
    function GetItem(const AId: string): string;
    function GetItemDetail(const AId, AField: string): string;
  end;

  TManualUIClass = class
  public
    function ShowWidget: string;
  end;

  [TestFixture]
  TMCPResourcesConfigRegisterResourceTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegisterResource_RegistersUnderConfiguredUri;
    [Test]
    procedure TestRegisterResource_CapturesClassAndMethod;
    [Test]
    procedure TestRegisterResource_RegisteredMethodIsInvokable;
    [Test]
    procedure TestRegisterResource_TagsApplyCategoryAndDisabled;
    [Test]
    procedure TestRegisterResource_UnknownMethodRaises;
    [Test]
    procedure TestRegisterResource_ReturnsResourcesConfigForChaining;

    [Test]
    procedure TestRegisterTemplate_RegistersUnderConfiguredUriTemplate;
    [Test]
    procedure TestRegisterTemplate_ParamNamesMapRttiParamsToUriPlaceholders;
    [Test]
    procedure TestRegisterTemplate_TooFewParamNamesRaises;
    [Test]
    procedure TestRegisterTemplate_UriPlaceholderMismatchRaises;

    [Test]
    procedure TestRegisterUI_RegistersUnderConfiguredUri;
    [Test]
    procedure TestRegisterUI_UIConfigCallbackWritesMeta;
    [Test]
    procedure TestRegisterUI_NonUiSchemeRaises;
  end;

  // Plain class with no MCP attributes at all, registered purely through the
  // one-shot RegisterPrompt API instead of [McpPrompt]/[McpArgument].
  TManualPromptClass = class
  public
    function Greet(const AName: string): string;
    function NoArgs: string;
  end;

  [TestFixture]
  TMCPPromptsConfigRegisterPromptTest = class(TObject)
  private
    FServer: TJRPCServer;
    FConfig: IMCPConfig;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestRegisterPrompt_RegistersUnderConfiguredName;
    [Test]
    procedure TestRegisterPrompt_CapturesClassAndMethod;
    [Test]
    procedure TestRegisterPrompt_ArgumentsUseConfiguredNamesDescriptionsAndRequired;
    [Test]
    procedure TestRegisterPrompt_NoArgsMethodProducesEmptyArguments;
    [Test]
    procedure TestRegisterPrompt_TagsApplyCategoryAndDisabled;
    [Test]
    procedure TestRegisterPrompt_UnknownMethodRaises;
    [Test]
    procedure TestRegisterPrompt_MissingArgumentConfigurationRaises;
    [Test]
    procedure TestRegisterPrompt_WrongParamNameRaises;
    [Test]
    procedure TestRegisterPrompt_RegisteredMethodIsInvokable;
    [Test]
    procedure TestRegisterPrompt_ReturnsPromptsConfigForChaining;
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
  Assert.AreEqual(TClass(TManualToolClass), LTool.ToolClass, 'ToolClass should point back to the registered class');
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
  FConfig.Tools
    .RegisterTool(TManualToolClass, 'NoParams', 'disabled_tool', 'y', 'disabled')
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

{ TManualResourceClass }

function TManualResourceClass.GetReadme: string;
begin
  Result := 'readme contents';
end;

{ TManualTemplateClass }

function TManualTemplateClass.GetItem(const AId: string): string;
begin
  Result := 'item:' + AId;
end;

function TManualTemplateClass.GetItemDetail(const AId, AField: string): string;
begin
  Result := 'item:' + AId + ':' + AField;
end;

{ TManualUIClass }

function TManualUIClass.ShowWidget: string;
begin
  Result := '<html/>';
end;

{ TMCPResourcesConfigRegisterResourceTest }

procedure TMCPResourcesConfigRegisterResourceTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPResourcesConfigRegisterResourceTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_RegistersUnderConfiguredUri;
var
  LRes: TMCPResource;
begin
  FConfig.Resources.RegisterResource(TManualResourceClass, 'GetReadme', 'readme', 'res://readme',
    'text/plain', 'The readme file');

  Assert.IsTrue(FConfig.Resources.Registry.ContainsKey('res://readme'), 'Resource should be registered under its configured uri');
  LRes := FConfig.Resources.Registry['res://readme'];
  Assert.AreEqual('readme', LRes.Name);
  Assert.AreEqual('The readme file', LRes.Description.Value);
  Assert.AreEqual('text/plain', LRes.MimeType.Value);
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_CapturesClassAndMethod;
var
  LRes: TMCPResource;
begin
  FConfig.Resources.RegisterResource(TManualResourceClass, 'GetReadme', 'readme', 'res://readme');

  LRes := FConfig.Resources.Registry['res://readme'];
  Assert.AreEqual(TClass(TManualResourceClass), LRes.Classe, 'Classe should point back to the registered class');
  Assert.AreEqual('GetReadme', LRes.Method.Name, 'Method should be the RTTI method for the configured method name');
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_RegisteredMethodIsInvokable;
var
  LRes: TMCPResource;
  LInstance: TManualResourceClass;
  LResult: TValue;
begin
  FConfig.Resources.RegisterResource(TManualResourceClass, 'GetReadme', 'readme', 'res://readme');

  LRes := FConfig.Resources.Registry['res://readme'];
  LInstance := TManualResourceClass.Create;
  try
    LResult := LRes.Method.Invoke(LInstance, []);
    Assert.AreEqual('readme contents', LResult.AsString);
  finally
    LInstance.Free;
  end;
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_TagsApplyCategoryAndDisabled;
var
  LRes: TMCPResource;
begin
  FConfig.Resources.RegisterResource(TManualResourceClass, 'GetReadme', 'readme', 'res://readme',
    '', '', 'category=docs,disabled');

  LRes := FConfig.Resources.Registry['res://readme'];
  Assert.AreEqual('docs', LRes.Category);
  Assert.IsTrue(LRes.Disabled);
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_UnknownMethodRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Resources.RegisterResource(TManualResourceClass, 'NoSuchMethod', 'x', 'res://x');
    end,
    EMCPException
  );
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterResource_ReturnsResourcesConfigForChaining;
begin
  // A single call fully registers the resource, so the result can be chained
  // straight into further Resources configuration calls.
  FConfig.Resources
    .RegisterResource(TManualResourceClass, 'GetReadme', 'readme', 'res://readme')
    .RegisterResource(TManualResourceClass, 'GetReadme', 'readme2', 'res://readme2');

  Assert.IsTrue(FConfig.Resources.Registry.ContainsKey('res://readme'));
  Assert.IsTrue(FConfig.Resources.Registry.ContainsKey('res://readme2'));
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterTemplate_RegistersUnderConfiguredUriTemplate;
var
  LTpl: TMCPResourceTemplate;
begin
  FConfig.Resources.RegisterTemplate(TManualTemplateClass, 'GetItem', 'item', 'res://items/{id}', ['id'], '', 'An item');

  Assert.IsTrue(FConfig.Resources.TemplateRegistry.ContainsKey('res://items/{id}'), 'Template should be registered under its configured uri template');
  LTpl := FConfig.Resources.TemplateRegistry['res://items/{id}'];
  Assert.AreEqual('item', LTpl.Name);
  Assert.AreEqual(TClass(TManualTemplateClass), LTpl.Classe);
  Assert.AreEqual('GetItem', LTpl.Method.Name);
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterTemplate_ParamNamesMapRttiParamsToUriPlaceholders;
var
  LTpl: TMCPResourceTemplate;
  LParam: TMCPResTemplateParam;
begin
  FConfig.Resources.RegisterTemplate(TManualTemplateClass, 'GetItem', 'item', 'res://items/{id}', ['id']);

  LTpl := FConfig.Resources.TemplateRegistry['res://items/{id}'];
  LParam := LTpl.FindMCPParam('AId');
  Assert.IsNotNull(LParam, 'The Delphi parameter name (AId) should resolve to a configured template param');
  Assert.AreEqual('id', LParam.Name, 'The uri placeholder name comes from AParamNames, positionally matched to the RTTI parameters');
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterTemplate_TooFewParamNamesRaises;
begin
  // GetItemDetail has two uri placeholders ({id}/{field}), only one name is supplied
  Assert.WillRaise(
    procedure
    begin
      FConfig.Resources.RegisterTemplate(TManualTemplateClass, 'GetItemDetail', 'item_detail', 'res://items/{id}/{field}', ['id']);
    end,
    EMCPException
  );
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterTemplate_UriPlaceholderMismatchRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Resources.RegisterTemplate(TManualTemplateClass, 'GetItem', 'item', 'res://items/{id}', ['not_a_placeholder']);
    end,
    EMCPException
  );
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterUI_RegistersUnderConfiguredUri;
var
  LApp: TMCPResource;
begin
  FConfig.Resources.RegisterUI(TManualUIClass, 'ShowWidget', 'widget', 'ui://widget', 'A widget');

  Assert.IsTrue(FConfig.Resources.Registry.ContainsKey('ui://widget'), 'UI resource should be registered under its configured uri');
  LApp := FConfig.Resources.Registry['ui://widget'];
  Assert.AreEqual('widget', LApp.Name);
  Assert.AreEqual(TClass(TManualUIClass), LApp.Classe);
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterUI_UIConfigCallbackWritesMeta;
var
  LApp: TMCPResource;
  LUIMeta: TJSONObject;
begin
  FConfig.Resources.RegisterUI(TManualUIClass, 'ShowWidget', 'widget', 'ui://widget', '', '',
    procedure(AResource: TMCPResource; AUI: TUIResourceUI)
    begin
      AUI.Domain := 'example.com';
    end);

  LApp := FConfig.Resources.Registry['ui://widget'];
  LUIMeta := LApp.Meta.GetValue('ui') as TJSONObject;
  Assert.IsNotNull(LUIMeta, 'Meta should carry a "ui" object once the AUIConfig callback sets a value');
  Assert.AreEqual('example.com', (LUIMeta.GetValue('domain') as TJSONString).Value);
end;

procedure TMCPResourcesConfigRegisterResourceTest.TestRegisterUI_NonUiSchemeRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Resources.RegisterUI(TManualUIClass, 'ShowWidget', 'widget', 'res://widget');
    end,
    EMCPException
  );
end;

{ TManualPromptClass }

function TManualPromptClass.Greet(const AName: string): string;
begin
  Result := 'Hello, ' + AName + '!';
end;

function TManualPromptClass.NoArgs: string;
begin
  Result := 'hi';
end;

{ TMCPPromptsConfigRegisterPromptTest }

procedure TMCPPromptsConfigRegisterPromptTest.Setup;
begin
  FServer := TJRPCServer.Create(nil);
  FConfig := FServer.Plugin.Configure<IMCPConfig>;
end;

procedure TMCPPromptsConfigRegisterPromptTest.TearDown;
begin
  FConfig := nil;
  FServer.Free;
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_RegistersUnderConfiguredName;
var
  LPrompt: TMCPPrompt;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'greet',
    [TMCPPromptArgConfig.New('AName', 'name', 'The name to greet')], 'Greeting', 'Greets someone by name');

  Assert.IsTrue(FConfig.Prompts.Registry.ContainsKey('greet'), 'Prompt should be registered under its configured name');
  LPrompt := FConfig.Prompts.Registry['greet'];
  Assert.AreEqual('greet', LPrompt.Name);
  Assert.AreEqual('Greeting', LPrompt.Title.Value);
  Assert.AreEqual('Greets someone by name', LPrompt.Description.Value);
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_CapturesClassAndMethod;
var
  LPrompt: TMCPPrompt;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'greet',
    [TMCPPromptArgConfig.New('AName', 'name')]);

  LPrompt := FConfig.Prompts.Registry['greet'];
  Assert.AreEqual(TClass(TManualPromptClass), LPrompt.Classe, 'Classe should point back to the registered class');
  Assert.AreEqual('Greet', LPrompt.Method.Name, 'Method should be the RTTI method for the configured method name');
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_ArgumentsUseConfiguredNamesDescriptionsAndRequired;
var
  LPrompt: TMCPPrompt;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'greet',
    [TMCPPromptArgConfig.New('AName', 'name', 'The name to greet', True)]);

  LPrompt := FConfig.Prompts.Registry['greet'];
  Assert.AreEqual(1, Length(LPrompt.Arguments));

  // The MCP-facing argument name comes from TMCPPromptArgConfig.Name, not the
  // Delphi parameter name (AName).
  Assert.AreEqual('name', LPrompt.Arguments[0].Name);
  Assert.AreEqual('The name to greet', LPrompt.Arguments[0].Description.Value);
  Assert.IsTrue(LPrompt.Arguments[0].Required.HasValue and LPrompt.Arguments[0].Required.Value);
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_NoArgsMethodProducesEmptyArguments;
var
  LPrompt: TMCPPrompt;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'NoArgs', 'no_args', []);

  LPrompt := FConfig.Prompts.Registry['no_args'];
  Assert.AreEqual(0, Length(LPrompt.Arguments));
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_TagsApplyCategoryAndDisabled;
var
  LPrompt: TMCPPrompt;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'NoArgs', 'tagged_prompt', [], '', '',
    'category=demo,disabled');

  LPrompt := FConfig.Prompts.Registry['tagged_prompt'];
  Assert.AreEqual('demo', LPrompt.Category);
  Assert.IsTrue(LPrompt.Disabled);
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_UnknownMethodRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'NoSuchMethod', 'x', []);
    end,
    EMCPException
  );
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_MissingArgumentConfigurationRaises;
begin
  // Greet has one parameter (AName), no arguments are supplied
  Assert.WillRaise(
    procedure
    begin
      FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'incomplete_prompt', []);
    end,
    EMCPException
  );
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_WrongParamNameRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'mismatched_prompt',
        [TMCPPromptArgConfig.New('WrongName', 'name')]);
    end,
    EMCPException
  );
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_RegisteredMethodIsInvokable;
var
  LPrompt: TMCPPrompt;
  LInstance: TManualPromptClass;
  LResult: TValue;
begin
  FConfig.Prompts.RegisterPrompt(TManualPromptClass, 'Greet', 'greet',
    [TMCPPromptArgConfig.New('AName', 'name')]);

  LPrompt := FConfig.Prompts.Registry['greet'];
  LInstance := TManualPromptClass.Create;
  try
    LResult := LPrompt.Method.Invoke(LInstance, [TValue.From<string>('Ada')]);
    Assert.AreEqual('Hello, Ada!', LResult.AsString);
  finally
    LInstance.Free;
  end;
end;

procedure TMCPPromptsConfigRegisterPromptTest.TestRegisterPrompt_ReturnsPromptsConfigForChaining;
begin
  // A single call fully registers the prompt, so the result can be chained
  // straight into further Prompts configuration calls.
  FConfig.Prompts
    .RegisterPrompt(TManualPromptClass, 'NoArgs', 'no_args', [])
    .RegisterPrompt(TManualPromptClass, 'Greet', 'greet', [TMCPPromptArgConfig.New('AName', 'name')]);

  Assert.IsTrue(FConfig.Prompts.Registry.ContainsKey('no_args'));
  Assert.IsTrue(FConfig.Prompts.Registry.ContainsKey('greet'));
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPToolsConfigRegisterToolTest);
  TDUnitX.RegisterTestFixture(TMCPResourcesConfigRegisterResourceTest);
  TDUnitX.RegisterTestFixture(TMCPPromptsConfigRegisterPromptTest);

end.
