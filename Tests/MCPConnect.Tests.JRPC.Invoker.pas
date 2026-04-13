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

  MCPConnect.JRPC.Classes,
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

    function BuildContext(ARequest: TJRPCRequest; AResponses: TJRPCMessages): TJRPCInvokerContext;
    procedure InvokeWithErrorHandling(ARequest: TJRPCRequest; AResponses: TJRPCMessages);
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

function TJRPCInvokerTest.BuildContext(ARequest: TJRPCRequest;
  AResponses: TJRPCMessages): TJRPCInvokerContext;
begin
  Result.GC := TGarbageCollector.CreateInstance;
  Result.Request := ARequest;
  Result.Responses := AResponses;
  Result.ApiInstance := FService;
  Result.NeonConfig := nil;
end;

procedure TJRPCInvokerTest.InvokeWithErrorHandling(ARequest: TJRPCRequest;
  AResponses: TJRPCMessages);
var
  LCtx: TJRPCInvokerContext;
begin
  LCtx := BuildContext(ARequest, AResponses);
  try
    TJRPCInvoker.Invoke(LCtx);
  except
    on E: Exception do
      AResponses.AddMessage(TJRPCInvoker.HandleError(E, ARequest.Id));
  end;
end;

procedure TJRPCInvokerTest.Setup;
begin
  FService := TTestService.Create;
end;

procedure TJRPCInvokerTest.TearDown;
begin
  FService.Free;
end;

{ Basic Invocation Tests }

procedure TJRPCInvokerTest.TestInvokeSimpleMethod;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/echo';
    LRequest.Id := 1;
    LRequest.AddNamedParam('message', TValue.From<string>('Hello World'));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Response should not be an error');
    Assert.AreEqual('Hello World', LResponse.Result.GetValue<string>, 'Echo should return the input message');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithIntegerParams;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 2;
    LRequest.AddNamedParam('a', TValue.From<Integer>(10));
    LRequest.AddNamedParam('b', TValue.From<Integer>(20));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Response should not be an error');
    Assert.AreEqual(30, LResponse.Result.GetValue<Integer>, 'Add should return 30');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithDoubleParams;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/multiply';
    LRequest.Id := 3;
    LRequest.AddNamedParam('x', TValue.From<Double>(2.5));
    LRequest.AddNamedParam('y', TValue.From<Double>(4.0));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Response should not be an error');
    Assert.AreEqual(Double(10.0), LResponse.Result.GetValue<Double>, 'Multiply should return 10.0');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeMethodWithoutParams;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/get_info';
    LRequest.Id := 4;

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Response should not be an error');
    Assert.AreEqual('TestService v1.0', LResponse.Result.GetValue<string>, 'GetInfo should return service version');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeNotification;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/log';
    LRequest.Id := 5;
    LRequest.AddNamedParam('message', TValue.From<string>('Test log'));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Notification response should not be an error');
    Assert.IsNull(LResponse.Result, 'Notification should return null result');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

{ Parameter Types Tests }

procedure TJRPCInvokerTest.TestInvokeWithNamedParams;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 6;
    LRequest.AddNamedParam('a', TValue.From<Integer>(5));
    LRequest.AddNamedParam('b', TValue.From<Integer>(7));

    Assert.AreEqual(TJRPCParamsType.ByName, LRequest.ParamsType, 'Request should have named params');

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.AreEqual(12, LResponse.Result.GetValue<Integer>, 'Add should work with named params');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithPositionalParams;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 7;
    LRequest.AddPositionParam(TValue.From<Integer>(8));
    LRequest.AddPositionParam(TValue.From<Integer>(9));

    Assert.AreEqual(TJRPCParamsType.ByPos, LRequest.ParamsType, 'Request should have positional params');

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.AreEqual(17, LResponse.Result.GetValue<Integer>, 'Add should work with positional params');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

{ Error Handling Tests }

procedure TJRPCInvokerTest.TestInvokeMethodNotFound;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LError: TJRPCError;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/nonexistent';
    LRequest.Id := 8;

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one error response');
    Assert.IsTrue(LResponses.List[0] is TJRPCError, 'Message should be a TJRPCError');
    LError := LResponses.List[0] as TJRPCError;
    Assert.AreEqual(JRPC_METHOD_NOT_FOUND, LError.Error.Code.Value, 'Error code should be METHOD_NOT_FOUND');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithInvalidParamsByName;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 9;
    LRequest.AddNamedParam('a', TValue.From<Integer>(5));
    // Parameter 'b' is missing

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one error response');
    Assert.IsTrue(LResponses.List[0] is TJRPCError, 'Message should be a TJRPCError for invalid params');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestInvokeWithInvalidParamsByPos;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/add';
    LRequest.Id := 9;
    LRequest.AddPositionParam(TValue.From<Integer>(5));
    // Parameter 'b' is missing

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one error response');
    Assert.IsTrue(LResponses.List[0] is TJRPCError, 'Message should be a TJRPCError for invalid params');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestHandleJRPCException;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LError: TJRPCError;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/divide';
    LRequest.Id := 10;
    LRequest.AddNamedParam('numerator', TValue.From<Double>(10.0));
    LRequest.AddNamedParam('denominator', TValue.From<Double>(0.0));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one error response');
    Assert.IsTrue(LResponses.List[0] is TJRPCError, 'Message should be a TJRPCError');
    LError := LResponses.List[0] as TJRPCError;
    Assert.AreEqual(JRPC_INTERNAL_ERROR, LError.Error.Code.Value, 'Error code should be INTERNAL_ERROR');
    Assert.IsTrue(LError.Error.Message.Value.Contains('Division by zero'), 'Error message should mention division by zero');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

procedure TJRPCInvokerTest.TestHandleGenericException;
var
  LError: TJRPCError;
  LId: TJRPCID;
  LException: Exception;
begin
  LId := 11;
  LException := Exception.Create('Generic error');
  try
    LError := TJRPCInvoker.HandleError(LException, LId);
    try
      Assert.IsNotNull(LError, 'Error response should not be nil');
      Assert.AreEqual(JRPC_INVALID_REQUEST, LError.Error.Code.Value, 'Error code should be INVALID_REQUEST for generic exceptions');
      Assert.AreEqual('Generic error', LError.Error.Message.Value, 'Error message should match exception message');
      Assert.AreEqual('Exception', LError.Error.Data.AsType<string>, 'Error data should contain exception class name');
    finally
      LError.Free;
    end;
  finally
    LException.Free;
  end;
end;

{ Separator Tests }

procedure TJRPCInvokerTest.TestMethodNameWithSeparator;
var
  LRequest: TJRPCRequest;
  LResponses: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LRequest := TJRPCRequest.Create;
  LResponses := TJRPCMessages.Create;
  try
    LRequest.Method := 'TestService/echo';
    LRequest.Id := 12;
    LRequest.AddNamedParam('message', TValue.From<string>('Test'));

    InvokeWithErrorHandling(LRequest, LResponses);

    Assert.AreEqual(NativeInt(1), LResponses.Count, 'Should have one response');
    Assert.IsTrue(LResponses.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LResponses.List[0] as TJRPCResponse;
    Assert.AreEqual('Test', LResponse.Result.GetValue<string>, 'Method should be invoked correctly with separator');
  finally
    LResponses.Free;
    LRequest.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJRPCInvokerTest);

end.
