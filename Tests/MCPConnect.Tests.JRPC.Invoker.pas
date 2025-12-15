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
unit MCPConnect.Tests.JRPC.Invoker;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core,
  MCPConnect.JRPC.Invoker;

type
  // Test helper class with JRPC-annotated methods
  [JRPC('TestService', 'separator=/')]
  TTestService = class
  public
    [JRPCMethod('echo', 'Echo back the input')]
    function Echo(
      [JRPCParam('message', 'Message to echo')] const AMessage: string
    ): string;

    [JRPCMethod('add', 'Add two numbers')]
    function Add(
      [JRPCParam('a', 'First number')] AFirst: Integer;
      [JRPCParam('b', 'Second number')] ASecond: Integer
    ): Integer;

    [JRPCMethod('multiply', 'Multiply two numbers')]
    function Multiply(
      [JRPCParam('x', 'First number')] AX: Double;
      [JRPCParam('y', 'Second number')] AY: Double
    ): Double;

    [JRPCMethod('get_info', 'Get service info')]
    function GetInfo: string;

    [JRPCMethod('divide', 'Divide two numbers')]
    function Divide(
      [JRPCParam('numerator', 'Numerator')] ANumerator: Double;
      [JRPCParam('denominator', 'Denominator')] ADenominator: Double
    ): Double;

    [JRPCNotification]
    [JRPCMethod('log', 'Log a message')]
    procedure Log(
      [JRPCParam('message', 'Message to log')] const AMessage: string
    );
  end;

  [TestFixture]
  TJRPCInvokerTest = class(TObject)
  private
    FService: TTestService;
    FInvoker: IJRPCInvokable;
    FContext: TJRPCContext;
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
    [Test]
    procedure TestInvokeNotification();

    // Parameter types tests
    [Test]
    procedure TestInvokeWithNamedParams();
    [Test]
    procedure TestInvokeWithPositionalParams();

    // Error handling tests
    [Test]
    procedure TestInvokeMethodNotFound();
    [Test]
    procedure TestInvokeWithInvalidParamsByName();
    [Test]
    procedure TestInvokeWithInvalidParamsByPos();
    [Test]
    procedure TestHandleJRPCException();
    [Test]
    procedure TestHandleGenericException();

    // Separator tests
    [Test]
    procedure TestMethodNameWithSeparator();
  end;

implementation

{ TTestService }

function TTestService.Echo(const AMessage: string): string;
begin
  Result := AMessage;
end;

function TTestService.Add(AFirst, ASecond: Integer): Integer;
begin
  Result := AFirst + ASecond;
end;

function TTestService.Multiply(AX, AY: Double): Double;
begin
  Result := AX * AY;
end;

function TTestService.GetInfo: string;
begin
  Result := 'TestService v1.0';
end;

function TTestService.Divide(ANumerator, ADenominator: Double): Double;
begin
  if ADenominator = 0 then
    raise EJRPCException.Create('Division by zero');
  Result := ANumerator / ADenominator;
end;

procedure TTestService.Log(const AMessage: string);
begin
  // Do nothing - just a notification
end;

{ TJRPCInvokerTest }

procedure TJRPCInvokerTest.Setup;
begin
  FService := TTestService.Create;
  FInvoker := TJRPCObjectInvoker.Create(FService);
  FContext := TJRPCContext.Create;
end;

procedure TJRPCInvokerTest.TearDown;
begin
  FContext.Free;
  FInvoker := nil;
  FService.Free;
end;

{ Basic Invocation Tests }

procedure TJRPCInvokerTest.TestInvokeSimpleMethod;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/echo';
    LRequest.Id := 1;
    LRequest.AddNamedParam('message', TValue.From<string>('Hello World'));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should return True for successful call');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.AreEqual('Hello World', LResponse.Result.GetValue<string>, 'Echo should return the input message');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithIntegerParams;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 2;
    LRequest.AddNamedParam('a', TValue.From<Integer>(10));
    LRequest.AddNamedParam('b', TValue.From<Integer>(20));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should return True for successful call');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.AreEqual(30, LResponse.Result.GetValue<Integer>, 'Add should return 30');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithDoubleParams;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/multiply';
    LRequest.Id := 3;
    LRequest.AddNamedParam('x', TValue.From<Double>(2.5));
    LRequest.AddNamedParam('y', TValue.From<Double>(4.0));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should return True for successful call');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.AreEqual(Double(10.0), LResponse.Result.GetValue<Double>, 'Multiply should return 10.0');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeMethodWithoutParams;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/get_info';
    LRequest.Id := 4;

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should return True for successful call');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.AreEqual('TestService v1.0', LResponse.Result.GetValue<string>, 'GetInfo should return service version');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeNotification;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/log';
    LRequest.Id := 5;
    LRequest.AddNamedParam('message', TValue.From<string>('Test log'));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should return True for notification');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.IsTrue(LResponse.IsNotification, 'Response should not a notification');
    Assert.IsNull(LResponse.Result, 'Notification should return null result');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

{ Parameter Types Tests }

procedure TJRPCInvokerTest.TestInvokeWithNamedParams;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 6;
    LRequest.AddNamedParam('a', TValue.From<Integer>(5));
    LRequest.AddNamedParam('b', TValue.From<Integer>(7));

    Assert.AreEqual(TJRPCParamsType.ByName, LRequest.ParamsType, 'Request should have named params');

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should succeed with named params');
    Assert.AreEqual(12, LResponse.Result.GetValue<Integer>, 'Add should work with named params');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithPositionalParams;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 7;
    LRequest.AddPositionParam(TValue.From<Integer>(8));
    LRequest.AddPositionParam(TValue.From<Integer>(9));

    Assert.AreEqual(TJRPCParamsType.ByPos, LRequest.ParamsType, 'Request should have positional params');

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should succeed with positional params');
    Assert.AreEqual(17, LResponse.Result.GetValue<Integer>, 'Add should work with positional params');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

{ Error Handling Tests }

procedure TJRPCInvokerTest.TestInvokeMethodNotFound;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/nonexistent';
    LRequest.Id := 8;

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsFalse(LResult, 'Invoke should return False for non-existent method');
    Assert.IsTrue(LResponse.IsError, 'Response should be an error');
    Assert.AreEqual(JRPC_METHOD_NOT_FOUND, LResponse.Error.Code.Value, 'Error code should be METHOD_NOT_FOUND');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithInvalidParamsByName;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 9;
    // Missing required parameters
    LRequest.AddNamedParam('a', TValue.From<Integer>(5));
    // Parameter 'b' is missing

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);
    FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResponse.IsError, 'Response should be an error for invalid params');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithInvalidParamsByPos;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 9;
    // Missing required parameters
    LRequest.AddPositionParam(TValue.From<Integer>(5));
    // Parameter 'b' is missing

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);
    FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResponse.IsError, 'Response should be an error for invalid params');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestHandleJRPCException;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    LRequest.Method := 'TestService/divide';
    LRequest.Id := 10;
    LRequest.AddNamedParam('numerator', TValue.From<Double>(10.0));
    LRequest.AddNamedParam('denominator', TValue.From<Double>(0.0));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResponse.IsError, 'Response should be an error for division by zero');
    Assert.AreEqual(JRPC_INTERNAL_ERROR, LResponse.Error.Code.Value, 'Error code should be INTERNAL_ERROR');
    Assert.IsTrue(LResponse.Error.Message.Value.Contains('Division by zero'), 'Error message should mention division by zero');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestHandleGenericException;
var
  LResponse: TJRPCResponse;
  LId: TJRPCID;
  LException: Exception;
begin
  LResponse := TJRPCResponse.Create;
  try
    LId := 11;
    LException := Exception.Create('Generic error');
    try
      TJRPCObjectInvoker.HandleException(LException, LId, LResponse);

      Assert.IsTrue(LResponse.IsError, 'Response should be an error');
      Assert.AreEqual(JRPC_INVALID_REQUEST, LResponse.Error.Code.Value, 'Error code should be INVALID_REQUEST for generic exceptions');
      Assert.AreEqual('Generic error', LResponse.Error.Message.Value, 'Error message should match exception message');
      Assert.AreEqual('Exception', LResponse.Error.Data.AsType<string>, 'Error data should contain exception class name');
    finally
      LException.Free;
    end;
  finally
    LResponse.Free;
  end;
end;

{ Separator Tests }

procedure TJRPCInvokerTest.TestMethodNameWithSeparator;
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LResult: Boolean;
begin
  LRequest := TJRPCRequest.Create;
  LResponse := TJRPCResponse.Create;
  try
    // Test with separator in method name
    LRequest.Method := 'TestService/echo';
    LRequest.Id := 12;
    LRequest.AddNamedParam('message', TValue.From<string>('Test'));

    FContext.AddContent(LRequest);
    FContext.AddContent(LResponse);

    LResult := FInvoker.Invoke(FContext, LRequest, LResponse);

    Assert.IsTrue(LResult, 'Invoke should handle method name with separator');
    Assert.AreEqual('Test', LResponse.Result.GetValue<string>, 'Method should be invoked correctly with separator');
  finally
    LResponse.Free;
    LRequest.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJRPCInvokerTest);

end.
