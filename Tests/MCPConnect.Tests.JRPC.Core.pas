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
unit MCPConnect.Tests.JRPC.Core;

interface

uses
  System.SysUtils, System.JSON, System.Rtti, System.Generics.Collections,
  DUnitX.TestFramework,

  MCPConnect.JRPC.Core;

type
  [TestFixture]
  TJRPCCoreTest = class(TObject)
  public
    // TJRPCRequest tests
    [Test]
    procedure TestRequestWithNamedParams();
    [Test]
    procedure TestRequestWithPositionParams();
    [Test]
    procedure TestRequestWithNullParams();
    [Test]
    procedure TestRequestAddPositionParam();
    [Test]
    procedure TestRequestAddNamedParam();
    [Test]
    procedure TestRequestSerializeToJson();
    [Test]
    procedure TestRequestIdInteger();
    [Test]
    procedure TestRequestIdString();
    [Test]
    procedure TestRequestParamsType();
    [Test]
    procedure TestRequestParamsCount();

    // TJRPCResponse tests
    [Test]
    procedure TestResponseWithResult();
    [Test]
    procedure TestResponseWithError();
    [Test]
    procedure TestResponseIsError();
    [Test]
    procedure TestResponseIsNotification();
    [Test]
    procedure TestResponseSerializeResult();
    [Test]
    procedure TestResponseSerializeError();
    [Test]
    procedure TestResponseDeserializeResult();
    [Test]
    procedure TestResponseDeserializeError();

    // TJRPCID tests
    [Test]
    procedure TestJRPCIDInteger();
    [Test]
    procedure TestJRPCIDString();
  end;

implementation

{ TJRPCCoreTest }

const
  SumRequestNamed = '''
  {
    "jsonrpc": "2.0",
    "id": 42,
    "method": "math/sum",
    "params": {
      "a": 1,
      "b": 5
    }
  }
  ''';

  SumRequestPosition = '''
  {
    "jsonrpc": "2.0",
    "id": 43,
    "method": "math/sum",
    "params": [1, 5]
  }
  ''';

  RequestNoParams = '''
  {
    "jsonrpc": "2.0",
    "id": 44,
    "method": "ping"
  }
  ''';

  ResponseWithResult = '''
  {
    "jsonrpc": "2.0",
    "id": 1,
    "result": 42
  }
  ''';

  ResponseWithError = '''
  {
    "jsonrpc": "2.0",
    "id": 2,
    "error": {
      "code": -32601,
      "message": "Method not found"
    }
  }
  ''';

{ TJRPCRequest Tests }

procedure TJRPCCoreTest.TestRequestWithNamedParams;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.CreateFromJson(SumRequestNamed);
  try
    Assert.AreEqual('2.0', LRequest.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual('math/sum', LRequest.Method, 'Method should be math/sum');
    Assert.AreEqual(Integer(42), Integer(LRequest.Id), 'Id should be 42');
    Assert.AreEqual(TJRPCParamsType.ByName, LRequest.ParamsType, 'ParamsType should be ByName');
    Assert.AreEqual(2, LRequest.ParamsCount, 'ParamsCount should be 2');
    Assert.AreEqual(1, LRequest.Params.GetValue<Integer>('a'), 'Parameter a should be 1');
    Assert.AreEqual(5, LRequest.Params.GetValue<Integer>('b'), 'Parameter b should be 5');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestWithPositionParams;
var
  LRequest: TJRPCRequest;
  LParams: TJSONArray;
begin
  LRequest := TJRPCRequest.CreateFromJson(SumRequestPosition);
  try
    Assert.AreEqual('2.0', LRequest.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual('math/sum', LRequest.Method, 'Method should be math/sum');
    Assert.AreEqual(Integer(43), Integer(LRequest.Id), 'Id should be 43');
    Assert.AreEqual(TJRPCParamsType.ByPos, LRequest.ParamsType, 'ParamsType should be ByPos');
    Assert.AreEqual(2, LRequest.ParamsCount, 'ParamsCount should be 2');

    LParams := LRequest.Params as TJSONArray;
    Assert.AreEqual(1, LParams.Items[0].AsType<Integer>, 'First parameter should be 1');
    Assert.AreEqual(5, LParams.Items[1].AsType<Integer>, 'Second parameter should be 5');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestWithNullParams;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.CreateFromJson(RequestNoParams);
  try
    Assert.AreEqual('2.0', LRequest.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual('ping', LRequest.Method, 'Method should be ping');
    Assert.AreEqual(Integer(44), Integer(LRequest.Id), 'Id should be 44');
    Assert.AreEqual(TJRPCParamsType.Null, LRequest.ParamsType, 'ParamsType should be Null');
    Assert.AreEqual(0, LRequest.ParamsCount, 'ParamsCount should be 0');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestAddPositionParam;
var
  LRequest: TJRPCRequest;
  LParams: TJSONArray;
begin
  LRequest := TJRPCRequest.Create;
  try
    LRequest.Method := 'test';
    LRequest.Id := 1;
    LRequest.AddPositionParam(TValue.From<Integer>(10));
    LRequest.AddPositionParam(TValue.From<string>('hello'));
    LRequest.AddPositionParam(TValue.From<Boolean>(True));

    Assert.AreEqual(TJRPCParamsType.ByPos, LRequest.ParamsType, 'ParamsType should be ByPos after adding position params');
    Assert.AreEqual(3, LRequest.ParamsCount, 'ParamsCount should be 3');

    LParams := LRequest.Params as TJSONArray;
    Assert.AreEqual(10, LParams.Items[0].AsType<Integer>, 'First param should be 10');
    Assert.AreEqual('hello', LParams.Items[1].AsType<string>, 'Second param should be "hello"');
    Assert.AreEqual(True, LParams.Items[2].AsType<Boolean>, 'Third param should be True');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestAddNamedParam;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.Create;
  try
    LRequest.Method := 'test';
    LRequest.Id := 1;
    LRequest.AddNamedParam('num', TValue.From<Integer>(10));
    LRequest.AddNamedParam('text', TValue.From<string>('hello'));
    LRequest.AddNamedParam('flag', TValue.From<Boolean>(True));

    Assert.AreEqual(TJRPCParamsType.ByName, LRequest.ParamsType, 'ParamsType should be ByName after adding named params');
    Assert.AreEqual(3, LRequest.ParamsCount, 'ParamsCount should be 3');
    Assert.AreEqual(10, LRequest.Params.GetValue<Integer>('num'), 'Parameter "num" should be 10');
    Assert.AreEqual('hello', LRequest.Params.GetValue<string>('text'), 'Parameter "text" should be "hello"');
    Assert.AreEqual(True, LRequest.Params.GetValue<Boolean>('flag'), 'Parameter "flag" should be True');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestSerializeToJson;
var
  LRequest: TJRPCRequest;
  LJson: string;
begin
  LRequest := TJRPCRequest.Create;
  try
    LRequest.Method := 'test_method';
    LRequest.Id := 99;
    LRequest.AddNamedParam('param1', TValue.From<Integer>(42));

    LJson := LRequest.ToJson;
    Assert.IsNotEmpty(LJson, 'Serialized JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"jsonrpc"'), 'JSON should contain "jsonrpc" field');
    Assert.IsTrue(LJson.Contains('"method"'), 'JSON should contain "method" field');
    Assert.IsTrue(LJson.Contains('test_method'), 'JSON should contain method name "test_method"');
    Assert.IsTrue(LJson.Contains('"id"'), 'JSON should contain "id" field');
    Assert.IsTrue(LJson.Contains('"params"'), 'JSON should contain "params" field');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestIdInteger;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.Create;
  try
    LRequest.Id := 123;
    Assert.AreEqual(Integer(123), Integer(LRequest.Id));
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestIdString;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.Create;
  try
    LRequest.Id := 'test-id-456';
    Assert.AreEqual('test-id-456', string(LRequest.Id));
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestParamsType;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.Create;
  try
    // Initially null
    Assert.AreEqual(TJRPCParamsType.Null, LRequest.ParamsType, 'Initial ParamsType should be Null');

    // Add position param
    LRequest.AddPositionParam(TValue.From<Integer>(1));
    Assert.AreEqual(TJRPCParamsType.ByPos, LRequest.ParamsType, 'ParamsType should be ByPos after adding position param');
  finally
    LRequest.Free;
  end;

  LRequest := TJRPCRequest.Create;
  try
    // Add named param
    LRequest.AddNamedParam('test', TValue.From<Integer>(1));
    Assert.AreEqual(TJRPCParamsType.ByName, LRequest.ParamsType, 'ParamsType should be ByName after adding named param');
  finally
    LRequest.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestParamsCount;
var
  LRequest: TJRPCRequest;
begin
  LRequest := TJRPCRequest.Create;
  try
    Assert.AreEqual(0, LRequest.ParamsCount, 'Initial ParamsCount should be 0');

    LRequest.AddPositionParam(TValue.From<Integer>(1));
    Assert.AreEqual(1, LRequest.ParamsCount, 'ParamsCount should be 1 after adding first param');

    LRequest.AddPositionParam(TValue.From<Integer>(2));
    Assert.AreEqual(2, LRequest.ParamsCount, 'ParamsCount should be 2 after adding second param');
  finally
    LRequest.Free;
  end;
end;

{ TJRPCResponse Tests }

procedure TJRPCCoreTest.TestResponseWithResult;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.Create;
  try
    LResponse.Id := 1;
    LResponse.Result := TJSONNumber.Create(42);

    Assert.IsFalse(LResponse.IsError, 'Response with result should not be an error');
    Assert.IsFalse(LResponse.IsNotification, 'Response with result should not be a notification');
    Assert.AreEqual(42, LResponse.Result.GetValue<Integer>, 'Result value should be 42');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseWithError;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.Create;
  try
    LResponse.Id := 2;
    LResponse.Error.Code := -32601;
    LResponse.Error.Message := 'Method not found';

    Assert.IsTrue(LResponse.IsError, 'Response with error should be marked as error');
    Assert.IsFalse(LResponse.IsNotification, 'Response with error should not be a notification');
    Assert.AreEqual(-32601, LResponse.Error.Code.Value, 'Error code should be -32601');
    Assert.AreEqual('Method not found', LResponse.Error.Message.Value, 'Error message should be "Method not found"');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseIsError;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.Create;
  try
    // No error set
    Assert.IsFalse(LResponse.IsError, 'Response without error should return False for IsError');

    // Set error
    LResponse.Error.Code := -32600;
    LResponse.Error.Message := 'Invalid Request';
    Assert.IsTrue(LResponse.IsError, 'Response with error code and message should return True for IsError');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseIsNotification;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.Create;
  try
    // Initially is notification (no result, no error)
    LResponse.Result := nil;
    Assert.IsTrue(LResponse.IsNotification, 'Response without result and error should be a notification');

    // Add result
    LResponse.Result := TJSONNumber.Create(1);
    Assert.IsFalse(LResponse.IsNotification, 'Response with result should not be a notification');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseSerializeResult;
var
  LResponse: TJRPCResponse;
  LJson: string;
begin
  LResponse := TJRPCResponse.Create;
  try
    LResponse.Id := 1;
    LResponse.Result := TJSONNumber.Create(100);

    LJson := LResponse.ToJson;
    Assert.IsNotEmpty(LJson, 'Serialized JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"jsonrpc"'), 'JSON should contain "jsonrpc" field');
    Assert.IsTrue(LJson.Contains('"id"'), 'JSON should contain "id" field');
    Assert.IsTrue(LJson.Contains('"result"'), 'JSON should contain "result" field');
    Assert.IsFalse(LJson.Contains('"error"'), 'JSON should not contain "error" field when result is present');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseSerializeError;
var
  LResponse: TJRPCResponse;
  LJson: string;
begin
  LResponse := TJRPCResponse.Create;
  try
    LResponse.Id := 2;
    LResponse.Error.Code := -32700;
    LResponse.Error.Message := 'Parse error';

    LJson := LResponse.ToJson;
    Assert.IsNotEmpty(LJson, 'Serialized JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"jsonrpc"'), 'JSON should contain "jsonrpc" field');
    Assert.IsTrue(LJson.Contains('"id"'), 'JSON should contain "id" field');
    Assert.IsTrue(LJson.Contains('"error"'), 'JSON should contain "error" field');
    Assert.IsTrue(LJson.Contains('"code"'), 'JSON error should contain "code" field');
    Assert.IsTrue(LJson.Contains('"message"'), 'JSON error should contain "message" field');
    Assert.IsFalse(LJson.Contains('"result"'), 'JSON should not contain "result" field when error is present');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseDeserializeResult;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.CreateFromJson(ResponseWithResult);
  try
    //raise Exception.Create(LResponse.ToJson);
    Assert.AreEqual('2.0', LResponse.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual(1, Integer(LResponse.Id), 'Id should be 1');
    Assert.IsFalse(LResponse.IsError, 'Response should not be an error');
    Assert.AreEqual(42, LResponse.Result.GetValue<Integer>, 'Result value should be 42');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseDeserializeError;
var
  LResponse: TJRPCResponse;
begin
  LResponse := TJRPCResponse.CreateFromJson(ResponseWithError);
  try
    Assert.AreEqual('2.0', LResponse.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual(Integer(2), Integer(LResponse.Id), 'Id should be 2');
    Assert.IsTrue(LResponse.IsError, 'Response should be marked as error');
    Assert.AreEqual(-32601, LResponse.Error.Code.Value, 'Error code should be -32601');
    Assert.AreEqual('Method not found', LResponse.Error.Message.Value, 'Error message should be "Method not found"');
  finally
    LResponse.Free;
  end;
end;

{ TJRPCID Tests }

procedure TJRPCCoreTest.TestJRPCIDInteger;
var
  LId: TJRPCID;
  LValue: Integer;
begin
  LId := 42;
  LValue := LId;
  Assert.AreEqual(42, LValue, 'TJRPCID should correctly convert to/from Integer');
end;

procedure TJRPCCoreTest.TestJRPCIDString;
var
  LId: TJRPCID;
  LValue: string;
begin
  LId := 'test-id-123';
  LValue := LId;
  Assert.AreEqual('test-id-123', LValue, 'TJRPCID should correctly convert to/from String');
end;

initialization
  TDUnitX.RegisterTestFixture(TJRPCCoreTest);

end.
