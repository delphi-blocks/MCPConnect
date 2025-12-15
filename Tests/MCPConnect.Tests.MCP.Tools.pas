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
unit MCPConnect.Tests.MCP.Tools;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  // Test helper class with MCP-annotated methods
  TTestToolClass = class
  public
    [McpTool('simple_tool', 'A simple test tool')]
    function SimpleTool(
      [McpParam('text', 'Input text to process')] const AText: string
    ): string;

    [McpTool('math_add', 'Add two numbers')]
    function MathAdd(
      [McpParam('a', 'First number')] AFirst: Integer;
      [McpParam('b', 'Second number')] ASecond: Integer
    ): Integer;

    [McpTool('readonly_tool', 'A readonly tool', 'readonly')]
    function ReadOnlyTool: string;

    [McpTool('destructive_tool', 'A destructive tool', 'destructive')]
    procedure DestructiveTool(
      [McpParam('confirm', 'Confirmation flag')] AConfirm: Boolean
    );
  end;

  [TestFixture]
  TMCPToolsTest = class(TObject)
  public
    // TMCPTool tests
    [Test]
    procedure TestMCPToolCreate();
    [Test]
    procedure TestMCPToolProperties();
    [Test]
    procedure TestMCPToolExchangeInputSchema();
    [Test]
    procedure TestMCPToolToJSON();

    // TListToolsResult tests
    [Test]
    procedure TestListToolsResultCreate();
    [Test]
    procedure TestListToolsResultToJSON();

    // TCallToolResult tests
    [Test]
    procedure TestCallToolResultCreate();
    [Test]
    procedure TestCallToolResultAddContent();

    // TMCPSchemaGenerator tests
    [Test]
    procedure TestSchemaGeneratorMethodToTool();
    [Test]
    procedure TestSchemaGeneratorListTools();
    [Test]
    procedure TestSchemaGeneratorWithNamespace();
    [Test]
    procedure TestSchemaGeneratorWithCustomNamespace();
    [Test]
    procedure TestSchemaGeneratorToolAnnotations();
    [Test]
    procedure TestSchemaGeneratorRequiredParams();
  end;

implementation

{ TTestToolClass }

function TTestToolClass.SimpleTool(const AText: string): string;
begin
  Result := AText;
end;

function TTestToolClass.MathAdd(AFirst, ASecond: Integer): Integer;
begin
  Result := AFirst + ASecond;
end;

function TTestToolClass.ReadOnlyTool: string;
begin
  Result := 'readonly';
end;

procedure TTestToolClass.DestructiveTool(AConfirm: Boolean);
begin
  // Do nothing
end;

{ TMCPToolsTest }

{ TMCPTool Tests }

procedure TMCPToolsTest.TestMCPToolCreate;
var
  LTool: TMCPTool;
begin
  LTool := TMCPTool.Create;
  try
    Assert.IsNotNull(LTool, 'TMCPTool should be created');
    Assert.IsNotNull(LTool.InputSchema, 'InputSchema should be initialized');
    Assert.IsNotNull(LTool.Annotations, 'Annotations should be initialized');
    Assert.IsNotNull(LTool.OutputSchema, 'OutputSchema should be initialized');
  finally
    LTool.Free;
  end;
end;

procedure TMCPToolsTest.TestMCPToolProperties;
var
  LTool: TMCPTool;
begin
  LTool := TMCPTool.Create;
  try
    LTool.Name := 'test_tool';
    LTool.Description := 'Test description';

    Assert.AreEqual('test_tool', LTool.Name, 'Name should be set correctly');
    Assert.AreEqual('Test description', LTool.Description.Value, 'Description should be set correctly');
  finally
    LTool.Free;
  end;
end;

procedure TMCPToolsTest.TestMCPToolExchangeInputSchema;
var
  LTool: TMCPTool;
  LNewSchema: TJSONObject;
  LOldSchema: TJSONObject;
begin
  LTool := TMCPTool.Create;
  try
    LOldSchema := LTool.InputSchema;
    LNewSchema := TJSONObject.Create;
    LNewSchema.AddPair('type', 'object');

    LTool.ExchangeInputSchema(LNewSchema);

    Assert.AreNotEqual(LOldSchema, LTool.InputSchema, 'InputSchema should be replaced');
    Assert.AreEqual(LNewSchema, LTool.InputSchema, 'InputSchema should be the new schema');
    Assert.IsTrue(LTool.InputSchema.ToString.Contains('"type"'), 'New schema should contain type field');
  finally
    LTool.Free;
  end;
end;

procedure TMCPToolsTest.TestMCPToolToJSON;
var
  LTool: TMCPTool;
  LJson: string;
begin
  LTool := TMCPTool.Create;
  try
    LTool.Name := 'test_tool';
    LTool.Description := 'A test tool';
    LTool.InputSchema.AddPair('type', 'object');

    LJson := LTool.ToJSON(True);

    Assert.IsNotEmpty(LJson, 'JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"name"'), 'JSON should contain name field');
    Assert.IsTrue(LJson.Contains('test_tool'), 'JSON should contain tool name');
    Assert.IsTrue(LJson.Contains('"description"'), 'JSON should contain description field');
    Assert.IsTrue(LJson.Contains('"inputSchema"'), 'JSON should contain inputSchema field');
  finally
    LTool.Free;
  end;
end;

{ TListToolsResult Tests }

procedure TMCPToolsTest.TestListToolsResultCreate;
var
  LResult: TListToolsResult;
begin
  LResult := TListToolsResult.Create;
  try
    Assert.IsNotNull(LResult, 'TListToolsResult should be created');
    Assert.IsNotNull(LResult.Tools, 'Tools list should be initialized');
    Assert.AreEqual(0, LResult.Tools.Count, 'Tools list should be empty initially');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestListToolsResultToJSON;
var
  LResult: TListToolsResult;
  LTool: TMCPTool;
  LJson: string;
begin
  LResult := TListToolsResult.Create;
  try
    LTool := TMCPTool.Create;
    LTool.Name := 'test_tool';
    LTool.Description := 'Test';
    LResult.Tools.Add(LTool);

    LJson := LResult.ToJSON(True);

    Assert.IsNotEmpty(LJson, 'JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"tools"'), 'JSON should contain tools array');
    Assert.IsTrue(LJson.Contains('test_tool'), 'JSON should contain tool name');
  finally
    LResult.Free;
  end;
end;

{ TCallToolResult Tests }

procedure TMCPToolsTest.TestCallToolResultCreate;
var
  LResult: TCallToolResult;
begin
  LResult := TCallToolResult.Create;
  try
    Assert.IsNotNull(LResult, 'TCallToolResult should be created');
    Assert.IsNotNull(LResult.Content, 'Content list should be initialized');
    Assert.IsNotNull(LResult.StructuredContent, 'StructuredContent should be initialized');
    Assert.AreEqual(0, LResult.Content.Count, 'Content list should be empty initially');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestCallToolResultAddContent;
var
  LResult: TCallToolResult;
  LContent: TTextContent;
begin
  LResult := TCallToolResult.Create;
  try
    LContent := TTextContent.Create;
    LContent.Text := 'Test content';

    LResult.AddContent(LContent);

    Assert.AreEqual(1, LResult.Content.Count, 'Content list should have one item');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content item should be TTextContent');
    Assert.AreEqual('Test content', (LResult.Content[0] as TTextContent).Text, 'Content text should match');
  finally
    LResult.Free;
  end;
end;

{ TMCPSchemaGenerator Tests }

procedure TMCPToolsTest.TestSchemaGeneratorMethodToTool;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LTool: TMCPTool;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TTestToolClass);
    LMethod := LType.GetMethod('SimpleTool');

    Assert.IsNotNull(LMethod, 'Method should be found');

    LTool := TMCPSchemaGenerator.MethodToTool(LMethod);
    try
      Assert.AreEqual('simple_tool', LTool.Name, 'Tool name should match attribute');
      Assert.AreEqual('A simple test tool', LTool.Description.Value, 'Tool description should match attribute');
      Assert.IsNotNull(LTool.InputSchema, 'InputSchema should be generated');
      Assert.IsTrue(LTool.InputSchema.ToString.Contains('"properties"'), 'InputSchema should have properties');
    finally
      LTool.Free;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TMCPToolsTest.TestSchemaGeneratorListTools;
var
  LResult: TListToolsResult;
begin
  LResult := TMCPSchemaGenerator.ListTools(TTestToolClass);
  try
    Assert.IsNotNull(LResult, 'ListTools result should not be null');
    Assert.AreEqual(4, LResult.Tools.Count, 'Should find 4 annotated methods');

    // Verify tool names
    Assert.IsTrue(LResult.Tools[0].Name = 'simple_tool', 'First tool should be simple_tool');
    Assert.IsTrue(LResult.Tools[1].Name = 'math_add', 'Second tool should be math_add');
    Assert.IsTrue(LResult.Tools[2].Name = 'readonly_tool', 'Third tool should be readonly_tool');
    Assert.IsTrue(LResult.Tools[3].Name = 'destructive_tool', 'Fourth tool should be destructive_tool');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestSchemaGeneratorWithCustomNamespace;
var
  LResult: TListToolsResult;
begin
  LResult := TListToolsResult.Create;
  try
    TMCPSchemaGenerator.ListTools(TTestToolClass, LResult, 'my_test', '--');

    Assert.AreEqual(4, LResult.Tools.Count, 'Should find 4 annotated methods');
    Assert.AreEqual('my_test--simple_tool', LResult.Tools[0].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('my_test--math_add', LResult.Tools[1].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('my_test--readonly_tool', LResult.Tools[2].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('my_test--destructive_tool', LResult.Tools[3].Name, 'Tool name should have namespace prefix');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestSchemaGeneratorWithNamespace;
var
  LResult: TListToolsResult;
begin
  LResult := TListToolsResult.Create;
  try
    TMCPSchemaGenerator.ListTools(TTestToolClass, LResult, 'test', '_');

    Assert.AreEqual(4, LResult.Tools.Count, 'Should find 4 annotated methods');
    Assert.AreEqual('test_simple_tool', LResult.Tools[0].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('test_math_add', LResult.Tools[1].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('test_readonly_tool', LResult.Tools[2].Name, 'Tool name should have namespace prefix');
    Assert.AreEqual('test_destructive_tool', LResult.Tools[3].Name, 'Tool name should have namespace prefix');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestSchemaGeneratorToolAnnotations;
var
  LResult: TListToolsResult;
  LReadOnlyTool, LDestructiveTool: TMCPTool;
begin
  LResult := TMCPSchemaGenerator.ListTools(TTestToolClass);
  try
    // Find readonly_tool and destructive_tool
    LReadOnlyTool := nil;
    LDestructiveTool := nil;

    for var LTool in LResult.Tools do
    begin
      if LTool.Name = 'readonly_tool' then
        LReadOnlyTool := LTool
      else if LTool.Name = 'destructive_tool' then
        LDestructiveTool := LTool;
    end;

    Assert.IsNotNull(LReadOnlyTool, 'ReadOnly tool should be found');
    Assert.IsNotNull(LDestructiveTool, 'Destructive tool should be found');

    Assert.IsTrue(LReadOnlyTool.Annotations.ReadOnlyHint.HasValue, 'ReadOnly annotation should have ReadOnlyHint');
    Assert.IsTrue(LReadOnlyTool.Annotations.ReadOnlyHint.Value, 'ReadOnlyHint should be True');

    Assert.IsTrue(LDestructiveTool.Annotations.DestructiveHint.HasValue, 'Destructive annotation should have DestructiveHint');
    Assert.IsTrue(LDestructiveTool.Annotations.DestructiveHint.Value, 'DestructiveHint should be True');
  finally
    LResult.Free;
  end;
end;

procedure TMCPToolsTest.TestSchemaGeneratorRequiredParams;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LTool: TMCPTool;
  LRequired: TJSONArray;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TTestToolClass);
    LMethod := LType.GetMethod('MathAdd');

    LTool := TMCPSchemaGenerator.MethodToTool(LMethod);
    try
      Assert.IsNotNull(LTool.InputSchema, 'InputSchema should exist');

      // Check for required array in schema
      LRequired := LTool.InputSchema.GetValue<TJSONArray>('required');
      Assert.IsNotNull(LRequired, 'Required array should exist in schema');
      Assert.AreEqual(2, LRequired.Count, 'Should have 2 required parameters');
      Assert.AreEqual('a', LRequired.Items[0].Value, 'First required param should be "a"');
      Assert.AreEqual('b', LRequired.Items[1].Value, 'Second required param should be "b"');
    finally
      LTool.Free;
    end;
  finally
    LContext.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPToolsTest);

end.
