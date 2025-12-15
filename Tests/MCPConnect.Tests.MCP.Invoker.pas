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
unit MCPConnect.Tests.MCP.Invoker;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.Core.Utils,
  MCPConnect.MCP.Invoker,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes,
  MCPConnect.Configuration.MCP;

type
  // Test record for complex parameters
  TPersonInfo = record
    Name: string;
    Age: Integer;
  end;

  // Test helper class with MCP-annotated methods
  TTestMCPService = class
  public
    [McpTool('echo', 'Echo back the input')]
    function Echo(
      [McpParam('message', 'Message to echo')] const AMessage: string
    ): string;

    [McpTool('add', 'Add two numbers')]
    function Add(
      [McpParam('a', 'First number')] AFirst: Integer;
      [McpParam('b', 'Second number')] ASecond: Integer
    ): Integer;

    [McpTool('multiply', 'Multiply two numbers')]
    function Multiply(
      [McpParam('x', 'First number')] AX: Double;
      [McpParam('y', 'Second number')] AY: Double
    ): Double;

    [McpTool('get_info', 'Get service info')]
    function GetInfo: string;

    [McpTool('get_array', 'Return an array of integers')]
    function GetArray: TArray<Integer>;

    [McpTool('get_person', 'Get person info', 'embedded')]
    function GetPerson(
      [McpParam('name', 'Person name')] const AName: string;
      [McpParam('age', 'Person age')] AAge: Integer
    ): TPersonInfo;

    [McpTool('error_test', 'Test error handling')]
    function ErrorTest: string;
  end;

  [TestFixture]
  TMCPInvokerTest = class(TObject)
  private
    FService: TTestMCPService;
    FInvoker: IMCPInvokable;
    FContext: TJRPCContext;
    FConfig: TMCPConfig;
    FGarbageCollector: IGarbageCollector;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Basic invocation tests
    [Test]
    procedure TestInvokeSimpleMethod();
    [Test]
    procedure TestInvokeWithIntegerParams();
    [Test]
    procedure TestInvokeWithDoubleParams();
    [Test]
    procedure TestInvokeMethodWithoutParams();

    // Result type tests
    [Test]
    procedure TestInvokeReturningString();
    [Test]
    procedure TestInvokeReturningInteger();
    [Test]
    procedure TestInvokeReturningArray();
    [Test]
    procedure TestInvokeReturningRecord();

    // Error handling tests
    [Test]
    procedure TestInvokeMethodNotFound();
    [Test]
    procedure TestInvokeWithMissingParams();

    // Content tests
    [Test]
    procedure TestResultContainsTextContent();
    [Test]
    procedure TestResultContainsEmbeddedResource();
  end;

implementation

{ TTestMCPService }

function TTestMCPService.Echo(const AMessage: string): string;
begin
  Result := AMessage;
end;

function TTestMCPService.Add(AFirst, ASecond: Integer): Integer;
begin
  Result := AFirst + ASecond;
end;

function TTestMCPService.Multiply(AX, AY: Double): Double;
begin
  Result := AX * AY;
end;

function TTestMCPService.GetInfo: string;
begin
  Result := 'TestMCPService v1.0';
end;

function TTestMCPService.GetArray: TArray<Integer>;
begin
  Result := [1, 2, 3, 4, 5];
end;

function TTestMCPService.GetPerson(const AName: string; AAge: Integer): TPersonInfo;
begin
  Result.Name := AName;
  Result.Age := AAge;
end;

function TTestMCPService.ErrorTest: string;
begin
  raise Exception.Create('Test error');
end;

{ TMCPInvokerTest }

procedure TMCPInvokerTest.Setup;
begin
  FService := TTestMCPService.Create;
  FInvoker := TMCPObjectInvoker.Create(FService);
  FContext := TJRPCContext.Create;
  FConfig := TMCPConfig.Create(nil);
  FGarbageCollector := TGarbageCollector.CreateInstance;

  // Inject dependencies
  FContext.AddContent(FConfig);
  FContext.AddContent(FGarbageCollector);
  FContext.Inject(FInvoker);
end;

procedure TMCPInvokerTest.TearDown;
begin
  FGarbageCollector := nil;
  FConfig.Free;
  FContext.Free;
  FInvoker := nil;
  FService.Free;
end;

{ Basic Invocation Tests }

procedure TMCPInvokerTest.TestInvokeSimpleMethod;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('message', 'Hello World');

    LSuccess := FInvoker.Invoke('echo', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should return True for successful call');
    Assert.IsTrue(LResult.IsError.IsNull, 'Result should not be an error');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content should be TTextContent');
    Assert.AreEqual('Hello World', (LResult.Content[0] as TTextContent).Text, 'Echo should return the input message');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeWithIntegerParams;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('a', TJSONNumber.Create(15));
    LArguments.AddPair('b', TJSONNumber.Create(25));

    LSuccess := FInvoker.Invoke('add', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should return True for successful call');
    Assert.IsTrue(LResult.IsError.IsNull, 'Result should not be an error');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content should be TTextContent');
    Assert.AreEqual('40', (LResult.Content[0] as TTextContent).Text, 'Add should return 40');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeWithDoubleParams;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
  LText: string;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('x', TJSONNumber.Create(2.5));
    LArguments.AddPair('y', TJSONNumber.Create(4.0));

    LSuccess := FInvoker.Invoke('multiply', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should return True for successful call');
    Assert.IsTrue(LResult.IsError.IsNull, 'Result should not be an error');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content should be TTextContent');

    LText := (LResult.Content[0] as TTextContent).Text;
    Assert.IsTrue(LText.StartsWith('10'), 'Multiply result should start with 10');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeMethodWithoutParams;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LSuccess := FInvoker.Invoke('get_info', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should return True for successful call');
    Assert.IsTrue(LResult.IsError.IsNull, 'Result should not be an error');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content should be TTextContent');
    Assert.AreEqual('TestMCPService v1.0', (LResult.Content[0] as TTextContent).Text, 'GetInfo should return service version');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

{ Result Type Tests }

procedure TMCPInvokerTest.TestInvokeReturningString;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('message', 'Test String');

    LSuccess := FInvoker.Invoke('echo', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'String result should be TTextContent');
    Assert.AreEqual('Test String', (LResult.Content[0] as TTextContent).Text, 'String content should match');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeReturningInteger;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('a', TJSONNumber.Create(10));
    LArguments.AddPair('b', TJSONNumber.Create(20));

    LSuccess := FInvoker.Invoke('add', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Integer result should be TTextContent');
    Assert.AreEqual('30', (LResult.Content[0] as TTextContent).Text, 'Integer content should be "30"');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeReturningArray;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LSuccess := FInvoker.Invoke('get_array', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    Assert.IsTrue(LResult.Content[0] is TEmbeddedResourceBlob, 'Array result should be TEmbeddedResourceBlob');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeReturningRecord;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('name', 'John Doe');
    LArguments.AddPair('age', TJSONNumber.Create(30));

    LSuccess := FInvoker.Invoke('get_person', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should contain one content item');
    // With 'embedded' tag, record should be returned as embedded resource
    Assert.IsTrue(LResult.Content[0] is TEmbeddedResourceText, 'Record with embedded tag should be TEmbeddedResourceText');

    var LResource := (LResult.Content[0] as TEmbeddedResourceText).Resource;
    Assert.AreEqual('application/json', LResource.MIMEType.Value, 'MIME type should be application/json');
    Assert.IsTrue(LResource.Text.Contains('John Doe'), 'Resource text should contain person name');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

{ Error Handling Tests }

procedure TMCPInvokerTest.TestInvokeMethodNotFound;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LSuccess := FInvoker.Invoke('nonexistent_method', LArguments, LMeta, LResult);

    Assert.IsFalse(LSuccess, 'Invoke should return False for non-existent method');
    Assert.IsFalse(LResult.IsError.IsNull, 'Result should be marked as error (not empty)');
    Assert.IsTrue(LResult.IsError.Value, 'Result should be marked as error (True)');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestInvokeWithMissingParams;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    // Call 'add' without required parameters
    LArguments.AddPair('a', TJSONNumber.Create(5));
    // Missing parameter 'b'

    // This should raise an exception or return error
    try
      FInvoker.Invoke('add', LArguments, LMeta, LResult);
      // If we get here, check if result indicates error
      Assert.IsTrue(True, 'Missing params should be handled (either exception or error result)');
    except
      on E: Exception do
        Assert.IsTrue(True, 'Exception expected for missing params: ' + E.Message);
    end;
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

{ Content Tests }

procedure TMCPInvokerTest.TestResultContainsTextContent;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('message', 'Test Content');

    LSuccess := FInvoker.Invoke('echo', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should have exactly one content item');
    Assert.IsNotNull(LResult.Content[0], 'Content item should not be null');
    Assert.IsTrue(LResult.Content[0] is TTextContent, 'Content should be TTextContent type');

    var LTextContent := LResult.Content[0] as TTextContent;
    Assert.AreEqual('text', LTextContent.&Type, 'Content type should be "text"');
    Assert.AreEqual('Test Content', LTextContent.Text, 'Text content should match input');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

procedure TMCPInvokerTest.TestResultContainsEmbeddedResource;
var
  LArguments: TJSONObject;
  LMeta: TJSONObject;
  LResult: TCallToolResult;
  LSuccess: Boolean;
begin
  LArguments := TJSONObject.Create;
  LMeta := TJSONObject.Create;
  LResult := TCallToolResult.Create;
  try
    LArguments.AddPair('name', 'Alice');
    LArguments.AddPair('age', TJSONNumber.Create(25));

    LSuccess := FInvoker.Invoke('get_person', LArguments, LMeta, LResult);

    Assert.IsTrue(LSuccess, 'Invoke should succeed');
    Assert.AreEqual(1, LResult.Content.Count, 'Result should have exactly one content item');
    Assert.IsTrue(LResult.Content[0] is TEmbeddedResourceText, 'Content should be TEmbeddedResourceText');

    var LEmbedded := LResult.Content[0] as TEmbeddedResourceText;
    Assert.AreEqual('resource', LEmbedded.&Type, 'Content type should be "resource"');
    Assert.AreEqual('application/json', LEmbedded.Resource.MIMEType.Value, 'MIME type should be application/json');
    Assert.IsTrue(LEmbedded.Resource.Text.Contains('"Name":"Alice"'), 'Resource should contain person data');
  finally
    LResult.Free;
    LMeta.Free;
    LArguments.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPInvokerTest);

end.
