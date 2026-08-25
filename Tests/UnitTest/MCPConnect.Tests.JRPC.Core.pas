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

    // TJRPCMessages parsing tests
    [Test]
    procedure TestMessagesFromMalformedJSON();
    [Test]
    procedure TestMessagesFromScalarJSON();
    [Test]
    procedure TestMessagesFromEmptyBatch();
    [Test]
    procedure TestMessagesFromBatchWithInvalidElement();
    [Test]
    procedure TestMessagesFromResultAndError();
    [Test]
    procedure TestErrorSerializeNullId();

    // TJRPCNotification serialization/deserialization tests
    [Test]
    procedure TestNotificationSerializeWithoutParams();
    [Test]
    procedure TestNotificationSerializeWithNamedParams();
    [Test]
    procedure TestNotificationSerializeWithPositionParams();
    [Test]
    procedure TestNotificationDeserializeWithoutParams();
    [Test]
    procedure TestNotificationDeserializeWithNamedParams();
    [Test]
    procedure TestNotificationDeserializeWithPositionParams();
    [Test]
    procedure TestNotificationRoundTripWithoutParams();
    [Test]
    procedure TestNotificationRoundTripWithNamedParams();
    [Test]
    procedure TestNotificationHasNoId();

    // jsonrpc version validation tests
    [Test]
    procedure TestRequestWithInvalidJsonRpcVersion();
    [Test]
    procedure TestMessagesWithInvalidJsonRpcVersion();
    [Test]
    procedure TestNotificationWithInvalidJsonRpcVersion();

    // id handling tests
    [Test]
    procedure TestRequestWithNullId();
    [Test]
    procedure TestRequestWithBooleanId();
    [Test]
    procedure TestRequestWithFractionalId();
    [Test]
    procedure TestRequestWithLargeId();
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

  NotificationNoParams = '''
  {
    "jsonrpc": "2.0",
    "method": "notifications/initialized"
  }
  ''';

  NotificationWithNamedParams = '''
  {
    "jsonrpc": "2.0",
    "method": "notifications/resources/updated",
    "params": {
      "uri": "file:///data.json",
      "reason": "modified"
    }
  }
  ''';

  NotificationWithPositionParams = '''
  {
    "jsonrpc": "2.0",
    "method": "notify_sum",
    "params": [1, 2, 3]
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

    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Error, 'Response with result should not be an error');
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Notification, 'Response with result should not be a notification');
    Assert.AreEqual(42, LResponse.Result.GetValue<Integer>, 'Result value should be 42');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseWithError;
var
  LResponse: TJRPCError;
begin
  LResponse := TJRPCError.Create;
  try
    LResponse.Id := 2;
    LResponse.Error.Code := -32601;
    LResponse.Error.Message := 'Method not found';

    Assert.IsTrue(LResponse.GetType = TJRPCMessageType.Error, 'Response with error should be marked as error');
    Assert.IsFalse(LResponse.GetType = TJRPCMessageType.Notification, 'Response with error should not be a notification');
    Assert.AreEqual(-32601, LResponse.Error.Code.Value, 'Error code should be -32601');
    Assert.AreEqual('Method not found', LResponse.Error.Message.Value, 'Error message should be "Method not found"');
  finally
    LResponse.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseIsError;
var
  LError: TJRPCError;
begin
  LError := TJRPCError.Create;
  try
    Assert.IsTrue(LError.GetType = TJRPCMessageType.Error, 'TJRPCError type should always be Error');
    LError.Error.Code := -32600;
    LError.Error.Message := 'Invalid Request';
    Assert.AreEqual(-32600, LError.Error.Code.Value, 'Error code should be set correctly');
    Assert.AreEqual('Invalid Request', LError.Error.Message.Value, 'Error message should be set correctly');
  finally
    LError.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseIsNotification;
var
  LNotification: TJRPCNotification;
  LResponse: TJRPCResponse;
begin
  LNotification := TJRPCNotification.Create;
  try
    Assert.IsTrue(LNotification.GetType = TJRPCMessageType.Notification, 'TJRPCNotification type should be Notification');
  finally
    LNotification.Free;
  end;

  LResponse := TJRPCResponse.Create;
  try
    Assert.IsTrue(LResponse.GetType = TJRPCMessageType.Response, 'TJRPCResponse type should be Response, not Notification');
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
  LError: TJRPCError;
  LJson: string;
begin
  LError := TJRPCError.Create;
  try
    LError.Id := 2;
    LError.Error.Code := -32700;
    LError.Error.Message := 'Parse error';

    LJson := LError.ToJson;
    Assert.IsNotEmpty(LJson, 'Serialized JSON should not be empty');
    Assert.IsTrue(LJson.Contains('"jsonrpc"'), 'JSON should contain "jsonrpc" field');
    Assert.IsTrue(LJson.Contains('"id"'), 'JSON should contain "id" field');
    Assert.IsTrue(LJson.Contains('"error"'), 'JSON should contain "error" field');
    Assert.IsTrue(LJson.Contains('"code"'), 'JSON error should contain "code" field');
    Assert.IsTrue(LJson.Contains('"message"'), 'JSON error should contain "message" field');
    Assert.IsFalse(LJson.Contains('"result"'), 'JSON should not contain "result" field when error is present');
  finally
    LError.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseDeserializeResult;
var
  LMessages: TJRPCMessages;
  LResponse: TJRPCResponse;
begin
  LMessages := TJRPCMessages.CreateFromJson(ResponseWithResult);
  try
    Assert.AreEqual(NativeInt(1), LMessages.Count, 'Should contain one message');
    Assert.IsTrue(LMessages.List[0] is TJRPCResponse, 'Message should be a TJRPCResponse');
    LResponse := LMessages.List[0] as TJRPCResponse;
    Assert.AreEqual('2.0', LResponse.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual(1, Integer(LResponse.Id), 'Id should be 1');
    Assert.IsTrue(LResponse.GetType = TJRPCMessageType.Response, 'Deserialized response should be of type Response');
    Assert.AreEqual(42, LResponse.Result.GetValue<Integer>, 'Result value should be 42');
  finally
    LMessages.Free;
  end;
end;

procedure TJRPCCoreTest.TestResponseDeserializeError;
var
  LMessages: TJRPCMessages;
  LError: TJRPCError;
begin
  LMessages := TJRPCMessages.CreateFromJson(ResponseWithError);
  try
    Assert.AreEqual(NativeInt(1), LMessages.Count, 'Should contain one message');
    Assert.IsTrue(LMessages.List[0] is TJRPCError, 'Message should be a TJRPCError');
    LError := LMessages.List[0] as TJRPCError;
    Assert.AreEqual('2.0', LError.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual(Integer(2), Integer(LError.Id), 'Id should be 2');
    Assert.IsTrue(LError.GetType = TJRPCMessageType.Error, 'Deserialized error should be of type Error');
    Assert.AreEqual(-32601, LError.Error.Code.Value, 'Error code should be -32601');
    Assert.AreEqual('Method not found', LError.Error.Message.Value, 'Error message should be "Method not found"');
  finally
    LMessages.Free;
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

{ TJRPCMessages Parsing Tests }

procedure TJRPCCoreTest.TestMessagesFromMalformedJSON;
begin
  Assert.WillRaise(
    procedure
    begin
      var LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "2.0", "method":');
      LMsgs.Free;
    end,
    EJRPCParseError,
    'Malformed JSON should raise a parse error'
  );
end;

procedure TJRPCCoreTest.TestMessagesFromScalarJSON;
begin
  Assert.WillRaise(
    procedure
    begin
      var LMsgs := TJRPCMessages.CreateFromJson('5');
      LMsgs.Free;
    end,
    EJRPCInvalidRequestError,
    'A top-level scalar is neither a Request nor a batch: it should raise Invalid Request'
  );
end;

procedure TJRPCCoreTest.TestMessagesFromEmptyBatch;
begin
  Assert.WillRaise(
    procedure
    begin
      var LMsgs := TJRPCMessages.CreateFromJson('[]');
      LMsgs.Free;
    end,
    EJRPCInvalidRequestError,
    'An empty batch should raise Invalid Request'
  );
end;

procedure TJRPCCoreTest.TestMessagesFromBatchWithInvalidElement;
var
  LMsgs: TJRPCMessages;
begin
  LMsgs := TJRPCMessages.CreateFromJson('''
    [
      1,
      {"jsonrpc": "2.0", "id": 7, "method": "math/sum", "params": {"a": 1, "b": 5}}
    ]
  ''');
  try
    Assert.AreEqual(NativeInt(2), LMsgs.Count, 'Both elements must be processed');
    Assert.IsTrue(LMsgs.List[0] is TJRPCError, 'Invalid element must produce an error');
    Assert.AreEqual(JRPC_INVALID_REQUEST, (LMsgs.List[0] as TJRPCError).Error.Code.Value, 'Error code must be -32600');
    Assert.IsTrue((LMsgs.List[0] as TJRPCError).Id.IsNull, 'Error id must be null');
    Assert.IsTrue(LMsgs.List[1] is TJRPCRequest, 'Valid element must still be parsed');
    Assert.AreEqual('math/sum', (LMsgs.List[1] as TJRPCRequest).Method, 'Valid request method must round-trip');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestMessagesFromResultAndError;
var
  LMsgs: TJRPCMessages;
begin
  // A Response object MUST contain exactly one of "result" and "error"; a
  // message with both is an Invalid Request and neither member is dropped.
  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "2.0", "id": 1, "result": 42, "error": {"code": -32601, "message": "Method not found"}}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCError, 'Message with both result and error must produce an error');
    Assert.AreEqual(JRPC_INVALID_REQUEST, (LMsgs.List[0] as TJRPCError).Error.Code.Value, 'Error code must be -32600');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestErrorSerializeNullId;
var
  LError: TJRPCError;
  LJson: string;
  LObj: TJSONObject;
  LPair: TJSONPair;
  LIdCount: Integer;
begin
  LError := TJRPCError.Create;
  try
    LError.Error.Code := JRPC_INVALID_REQUEST;
    LError.Error.Message := 'Invalid Request';
    LJson := LError.ToJson;

    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      LIdCount := 0;
      for LPair in LObj do
        if LPair.JsonString.Value = 'id' then
          Inc(LIdCount);

      Assert.AreEqual(1, LIdCount, 'Error serialization must emit exactly one "id" member');
      Assert.IsTrue(LObj.GetValue<TJSONValue>('id') is TJSONNull, '"id" must be null');
      Assert.AreEqual(JRPC_INVALID_REQUEST, LObj.GetValue<Integer>('error.code'), 'Error code should round-trip');
    finally
      LObj.Free;
    end;
  finally
    LError.Free;
  end;
end;

{ TJRPCNotification Serialization/Deserialization Tests }

procedure TJRPCCoreTest.TestNotificationSerializeWithoutParams;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LObj: TJSONObject;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'notifications/initialized';

    LJson := LNotification.ToJson;
    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      Assert.AreEqual('2.0', LObj.GetValue<string>('jsonrpc'), 'jsonrpc version should be 2.0');
      Assert.AreEqual('notifications/initialized', LObj.GetValue<string>('method'), 'Method should match');
      Assert.IsNull(LObj.GetValue('id'), 'Notification must not have an id field');
    finally
      LObj.Free;
    end;
  finally
    LNotification.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationSerializeWithNamedParams;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LObj: TJSONObject;
  LParams: TJSONObject;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'notifications/resources/updated';
    LNotification.AddNamedParam('uri', TValue.From<string>('file:///data.json'));
    LNotification.AddNamedParam('reason', TValue.From<string>('modified'));

    LJson := LNotification.ToJson;
    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      Assert.AreEqual('notifications/resources/updated', LObj.GetValue<string>('method'), 'Method should match');
      Assert.IsNull(LObj.GetValue('id'), 'Notification must not have an id field');
      LParams := LObj.GetValue<TJSONObject>('params');
      Assert.IsNotNull(LParams, 'Params should be present');
      Assert.AreEqual('file:///data.json', LParams.GetValue<string>('uri'), 'Param uri should match');
      Assert.AreEqual('modified', LParams.GetValue<string>('reason'), 'Param reason should match');
    finally
      LObj.Free;
    end;
  finally
    LNotification.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationSerializeWithPositionParams;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LObj: TJSONObject;
  LParams: TJSONArray;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'notify_sum';
    LNotification.AddPositionParam(TValue.From<Integer>(1));
    LNotification.AddPositionParam(TValue.From<Integer>(2));
    LNotification.AddPositionParam(TValue.From<Integer>(3));

    LJson := LNotification.ToJson;
    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      Assert.AreEqual('notify_sum', LObj.GetValue<string>('method'), 'Method should match');
      Assert.IsNull(LObj.GetValue('id'), 'Notification must not have an id field');
      LParams := LObj.GetValue<TJSONArray>('params');
      Assert.IsNotNull(LParams, 'Params should be present');
      Assert.AreEqual(3, LParams.Count, 'Params should have 3 elements');
      Assert.AreEqual(1, LParams.Items[0].AsType<Integer>, 'First param should be 1');
      Assert.AreEqual(2, LParams.Items[1].AsType<Integer>, 'Second param should be 2');
      Assert.AreEqual(3, LParams.Items[2].AsType<Integer>, 'Third param should be 3');
    finally
      LObj.Free;
    end;
  finally
    LNotification.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationDeserializeWithoutParams;
var
  LMsgs: TJRPCMessages;
  LNotification: TJRPCNotification;
begin
  LMsgs := TJRPCMessages.CreateFromJson(NotificationNoParams);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Message should be a TJRPCNotification');
    LNotification := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('2.0', LNotification.JsonRpc, 'JsonRpc version should be 2.0');
    Assert.AreEqual('notifications/initialized', LNotification.Method, 'Method should match');
    Assert.IsTrue(LNotification.GetType = TJRPCMessageType.Notification, 'Type should be Notification');
    Assert.AreEqual(TJRPCParamsType.Null, LNotification.ParamsType, 'ParamsType should be Null');
    Assert.AreEqual(0, LNotification.ParamsCount, 'ParamsCount should be 0');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationDeserializeWithNamedParams;
var
  LMsgs: TJRPCMessages;
  LNotification: TJRPCNotification;
begin
  LMsgs := TJRPCMessages.CreateFromJson(NotificationWithNamedParams);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Message should be a TJRPCNotification');
    LNotification := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('notifications/resources/updated', LNotification.Method, 'Method should match');
    Assert.IsTrue(LNotification.GetType = TJRPCMessageType.Notification, 'Type should be Notification');
    Assert.AreEqual(TJRPCParamsType.ByName, LNotification.ParamsType, 'ParamsType should be ByName');
    Assert.AreEqual(2, LNotification.ParamsCount, 'ParamsCount should be 2');
    Assert.AreEqual('file:///data.json', LNotification.Params.GetValue<string>('uri'), 'Param uri should match');
    Assert.AreEqual('modified', LNotification.Params.GetValue<string>('reason'), 'Param reason should match');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationDeserializeWithPositionParams;
var
  LMsgs: TJRPCMessages;
  LNotification: TJRPCNotification;
  LParams: TJSONArray;
begin
  LMsgs := TJRPCMessages.CreateFromJson(NotificationWithPositionParams);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Message should be a TJRPCNotification');
    LNotification := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('notify_sum', LNotification.Method, 'Method should match');
    Assert.IsTrue(LNotification.GetType = TJRPCMessageType.Notification, 'Type should be Notification');
    Assert.AreEqual(TJRPCParamsType.ByPos, LNotification.ParamsType, 'ParamsType should be ByPos');
    Assert.AreEqual(3, LNotification.ParamsCount, 'ParamsCount should be 3');
    LParams := LNotification.Params as TJSONArray;
    Assert.AreEqual(1, LParams.Items[0].AsType<Integer>, 'First param should be 1');
    Assert.AreEqual(2, LParams.Items[1].AsType<Integer>, 'Second param should be 2');
    Assert.AreEqual(3, LParams.Items[2].AsType<Integer>, 'Third param should be 3');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationRoundTripWithoutParams;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LMsgs: TJRPCMessages;
  LDeserialized: TJRPCNotification;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'notifications/initialized';
    LJson := LNotification.ToJson;
  finally
    LNotification.Free;
  end;

  LMsgs := TJRPCMessages.CreateFromJson(LJson);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Round-tripped message should be a TJRPCNotification');
    LDeserialized := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('notifications/initialized', LDeserialized.Method, 'Method should survive round-trip');
    Assert.AreEqual(TJRPCParamsType.Null, LDeserialized.ParamsType, 'ParamsType should be Null after round-trip');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationRoundTripWithNamedParams;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LMsgs: TJRPCMessages;
  LDeserialized: TJRPCNotification;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'notifications/resources/updated';
    LNotification.AddNamedParam('uri', TValue.From<string>('file:///data.json'));
    LJson := LNotification.ToJson;
  finally
    LNotification.Free;
  end;

  LMsgs := TJRPCMessages.CreateFromJson(LJson);
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should contain one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCNotification, 'Round-tripped message should be a TJRPCNotification');
    LDeserialized := LMsgs.List[0] as TJRPCNotification;
    Assert.AreEqual('notifications/resources/updated', LDeserialized.Method, 'Method should survive round-trip');
    Assert.AreEqual(TJRPCParamsType.ByName, LDeserialized.ParamsType, 'ParamsType should be ByName after round-trip');
    Assert.AreEqual('file:///data.json', LDeserialized.Params.GetValue<string>('uri'), 'Param uri should survive round-trip');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationHasNoId;
var
  LNotification: TJRPCNotification;
  LJson: string;
  LObj: TJSONObject;
  LPair: TJSONPair;
begin
  LNotification := TJRPCNotification.Create;
  try
    LNotification.Method := 'test/notify';
    LJson := LNotification.ToJson;

    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      for LPair in LObj do
        Assert.AreNotEqual('id', LPair.JsonString.Value, 'Notification JSON must not contain an "id" field');
    finally
      LObj.Free;
    end;
  finally
    LNotification.Free;
  end;
end;

{ jsonrpc Version Validation Tests }

procedure TJRPCCoreTest.TestRequestWithInvalidJsonRpcVersion;
begin
  Assert.WillRaise(
    procedure
    begin
      var LReq := TJRPCRequest.CreateFromJson('{"jsonrpc": "1.0", "id": 1, "method": "math/sum"}');
      LReq.Free;
    end,
    EJRPCInvalidRequestError,
    'A request with a jsonrpc version other than 2.0 must be rejected'
  );
end;

procedure TJRPCCoreTest.TestMessagesWithInvalidJsonRpcVersion;
var
  LMsgs: TJRPCMessages;
begin
  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "1.0", "id": 1, "method": "math/sum"}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCError, 'Invalid version must produce an error');
    Assert.AreEqual(JRPC_INVALID_REQUEST, (LMsgs.List[0] as TJRPCError).Error.Code.Value, 'Error code must be -32600');
    Assert.AreEqual(1, Integer((LMsgs.List[0] as TJRPCError).Id), 'Request id must be echoed');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestNotificationWithInvalidJsonRpcVersion;
var
  LMsgs: TJRPCMessages;
begin
  // Notifications bypass TJRequestSerializer (they are a sibling class), so the
  // version check must happen at classification time (GetMessageType).
  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "1.0", "method": "notify"}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCError, 'Invalid version must produce an error');
    Assert.AreEqual(JRPC_INVALID_REQUEST, (LMsgs.List[0] as TJRPCError).Error.Code.Value, 'Error code must be -32600');
  finally
    LMsgs.Free;
  end;
end;

{ id Handling Tests }

procedure TJRPCCoreTest.TestRequestWithNullId;
var
  LMsgs: TJRPCMessages;
  LReq: TJRPCRequest;
  LJson: string;
  LObj: TJSONObject;
begin
  // Deserialization: "id": null stays null (it must not become the string "null").
  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "2.0", "id": null, "method": "ping"}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    LReq := LMsgs.List[0] as TJRPCRequest;
    Assert.IsTrue(LReq.Id.IsNull, 'A null id must stay null');

    // Serialization: round-trips back as a single "id": null.
    LJson := LReq.ToJson;
    LObj := TJSONObject.ParseJSONValue(LJson) as TJSONObject;
    try
      Assert.IsTrue(Assigned(LObj.GetValue('id')), '"id" member must be present');
      Assert.IsTrue(LObj.GetValue<TJSONValue>('id') is TJSONNull, '"id" must serialize as null');
    finally
      LObj.Free;
    end;
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestWithBooleanId;
var
  LMsgs: TJRPCMessages;
begin
  Assert.WillRaise(
    procedure
    begin
      var LReq := TJRPCRequest.CreateFromJson('{"jsonrpc": "2.0", "id": true, "method": "ping"}');
      LReq.Free;
    end,
    EJRPCInvalidRequestError,
    'A boolean id must be rejected as an Invalid Request'
  );

  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "2.0", "id": true, "method": "ping"}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    Assert.IsTrue(LMsgs.List[0] is TJRPCError, 'Invalid id must produce an error');
    Assert.AreEqual(JRPC_INVALID_REQUEST, (LMsgs.List[0] as TJRPCError).Error.Code.Value, 'Error code must be -32600');
    Assert.IsTrue((LMsgs.List[0] as TJRPCError).Id.IsNull, 'Error id must be null');
  finally
    LMsgs.Free;
  end;
end;

procedure TJRPCCoreTest.TestRequestWithFractionalId;
begin
  Assert.WillRaise(
    procedure
    begin
      var LReq := TJRPCRequest.CreateFromJson('{"jsonrpc": "2.0", "id": 1.5, "method": "ping"}');
      LReq.Free;
    end,
    EJRPCInvalidRequestError,
    'A fractional id must be rejected rather than silently truncated'
  );
end;

procedure TJRPCCoreTest.TestRequestWithLargeId;
var
  LMsgs: TJRPCMessages;
  LReq: TJRPCRequest;
begin
  LMsgs := TJRPCMessages.CreateFromJson('{"jsonrpc": "2.0", "id": 2147483648, "method": "ping"}');
  try
    Assert.AreEqual(NativeInt(1), LMsgs.Count, 'Should produce one message');
    LReq := LMsgs.List[0] as TJRPCRequest;
    Assert.AreEqual('2147483648', LReq.Id.AsString, 'Ids beyond Integer range must survive as Int64');
  finally
    LMsgs.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJRPCCoreTest);

end.
