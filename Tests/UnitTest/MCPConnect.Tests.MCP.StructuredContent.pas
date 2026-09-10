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

/// <summary>
///   Structured tool output after SEP-2106: "structuredContent" may be any JSON
///   value - an object, and equally an array of rows, a string or a number -
///   and an "outputSchema" may be any JSON Schema 2020-12 rather than an object
///   schema. Both used to be objects or an exception.
/// </summary>
unit MCPConnect.Tests.MCP.StructuredContent;

interface

uses
  System.SysUtils, System.JSON,
  DUnitX.TestFramework,

  MCPConnect.Configuration.MCP,
  MCPConnect.MCP.Server,
  MCPConnect.MCP.Types.Base,
  MCPConnect.Transport.Base;

type
  /// <summary>A response writer that streams nothing.</summary>
  TSilentStructuredWriter = class(TInterfacedObject, IMCPTransportWriter)
  public
    procedure Write(const AValue: string);
    function Connected: Boolean;
    function SupportsStreaming: Boolean;
  end;

  TRow = record
    Id: Integer;
    Name: string;
  end;

  [TestFixture]
  TStructuredContentTest = class(TObject)
  private
    FServer: TMCPServer;

    procedure ConfigureServer;
    function Call(const ATool: string): string;

    /// <summary>The "structuredContent" of the reply, or nil. Caller owns it.</summary>
    function StructuredOf(const AContent: string): TJSONValue;

    /// <summary>The "outputSchema" a tools/list reports for ATool, or nil.</summary>
    function OutputSchemaOf(const ATool: string): TJSONObject;
  public
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAnObjectIsStructuredContent();
    [Test]
    procedure TestAnArrayIsStructuredContentToo();
    [Test]
    procedure TestAStringIsStructuredContentToo();
    [Test]
    procedure TestANumberIsStructuredContentToo();

    [Test]
    procedure TestAnUntaggedToolCarriesNone();
    [Test]
    procedure TestBytesStayABlob();

    [Test]
    procedure TestTheTextBlockIsStillThere();

    [Test]
    procedure TestAnOutputSchemaDescribesWhateverIsReturned();
  end;

implementation

uses
  MCPConnect.MCP.Attributes;

type
  TStructuredTools = class(TObject)
  public
    [McpTool('one_row', 'Answers with a record', 'structured')]
    function OneRow: TRow;

    [McpTool('many_rows', 'Answers with a list of records', 'structured')]
    function ManyRows: TArray<TRow>;

    [McpTool('a_name', 'Answers with a string', 'structured')]
    function AName: string;

    [McpTool('a_count', 'Answers with a number', 'structured')]
    function ACount: Integer;

    [McpTool('some_bytes', 'Answers with bytes', 'structured')]
    function SomeBytes: TBytes;

    [McpTool('plain_rows', 'Answers with a list, and says nothing about it')]
    function PlainRows: TArray<TRow>;
  end;

function TStructuredTools.OneRow: TRow;
begin
  Result.Id := 1;
  Result.Name := 'Alice';
end;

function TStructuredTools.ManyRows: TArray<TRow>;
begin
  SetLength(Result, 2);
  Result[0].Id := 1;
  Result[0].Name := 'Alice';
  Result[1].Id := 2;
  Result[1].Name := 'Bob';
end;

function TStructuredTools.AName: string;
begin
  Result := 'Alice';
end;

function TStructuredTools.ACount: Integer;
begin
  Result := 42;
end;

function TStructuredTools.SomeBytes: TBytes;
begin
  Result := TEncoding.UTF8.GetBytes('binary');
end;

function TStructuredTools.PlainRows: TArray<TRow>;
begin
  SetLength(Result, 1);
  Result[0].Id := 9;
  Result[0].Name := 'Nobody';
end;

{ TSilentStructuredWriter }

procedure TSilentStructuredWriter.Write(const AValue: string);
begin
  // Nothing streams in these tests.
end;

function TSilentStructuredWriter.Connected: Boolean;
begin
  Result := True;
end;

function TSilentStructuredWriter.SupportsStreaming: Boolean;
begin
  Result := False;
end;

{ TStructuredContentTest }

procedure TStructuredContentTest.TearDown;
begin
  FServer.Free;
  FServer := nil;
end;

procedure TStructuredContentTest.ConfigureServer;
begin
  FServer := TMCPServer.Create(nil);

  FServer.Plugin.Configure<IMCPConfig>
    .Server
      .SetName('structured-test')
      .SetVersion('1.0.0')
    .BackToMCP
    .Security
      .SetHeaderValidation(TMCPValidationLevel.Off)
      .SetMetaValidation(TMCPValidationLevel.Off)
      .SetOriginPolicy(TMCPOriginPolicy.Off)
    .BackToMCP
    .Tools
      .RegisterClass(TStructuredTools)
    .BackToMCP
  .ApplyConfig;
end;

function TStructuredContentTest.Call(const ATool: string): string;
var
  LHandler: TMCPTransportHandler;
  LContent, LBody: string;
begin
  if ATool.IsEmpty then
    LBody := '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
  else
    LBody := Format('{"jsonrpc":"2.0","id":1,"method":"tools/call",' +
      '"params":{"name":"%s","arguments":{}}}', [ATool]);

  LContent := '';
  LHandler := TMCPTransportHandler.Create(FServer, TSilentStructuredWriter.Create);
  try
    LHandler.ProcessRequest(
      procedure (ARequest: TMCPTransportRequest)
      begin
        ARequest.Url := '/';
        ARequest.Command := 'POST';
        ARequest.Protocol := TTransportProtocol.StreamableHTTP;
        ARequest.Accept := 'application/json';
        ARequest.Content := LBody;
      end,
      procedure (AResponse: TMCPTransportResponse)
      begin
        LContent := AResponse.Content;
      end);
  finally
    LHandler.Free;
  end;

  Result := LContent;
end;

function TStructuredContentTest.StructuredOf(const AContent: string): TJSONValue;
var
  LValue, LResult, LStructured: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(AContent);
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LResult := TJSONObject(LValue).GetValue('result');
    if not (LResult is TJSONObject) then
      Exit;

    LStructured := TJSONObject(LResult).GetValue('structuredContent');
    if Assigned(LStructured) then
      Result := LStructured.Clone as TJSONValue;
  finally
    LValue.Free;
  end;
end;

function TStructuredContentTest.OutputSchemaOf(const ATool: string): TJSONObject;
var
  LValue, LResult, LTools, LSchema: TJSONValue;
  LTool: TJSONValue;
begin
  Result := nil;

  LValue := TJSONObject.ParseJSONValue(Call(''));
  if not (LValue is TJSONObject) then
  begin
    LValue.Free;
    Exit;
  end;

  try
    LResult := TJSONObject(LValue).GetValue('result');
    if not (LResult is TJSONObject) then
      Exit;

    LTools := TJSONObject(LResult).GetValue('tools');
    if not (LTools is TJSONArray) then
      Exit;

    for LTool in TJSONArray(LTools) do
    begin
      if not (LTool is TJSONObject) then
        Continue;
      if TJSONObject(LTool).GetValue<string>('name', '') <> ATool then
        Continue;

      LSchema := TJSONObject(LTool).GetValue('outputSchema');
      if LSchema is TJSONObject then
        Result := LSchema.Clone as TJSONObject;
      Break;
    end;
  finally
    LValue.Free;
  end;
end;

procedure TStructuredContentTest.TestAnObjectIsStructuredContent;
var
  LStructured: TJSONValue;
begin
  ConfigureServer();

  LStructured := StructuredOf(Call('one_row'));
  Assert.IsNotNull(LStructured);
  try
    Assert.IsTrue(LStructured is TJSONObject, LStructured.ToJSON);
    Assert.Contains(LStructured.ToJSON, 'Alice');
  finally
    LStructured.Free;
  end;
end;

procedure TStructuredContentTest.TestAnArrayIsStructuredContentToo;
var
  LStructured: TJSONValue;
begin
  ConfigureServer();

  // The case that used to raise, and the one the specification illustrates: a
  // tool that answers with a list of rows
  LStructured := StructuredOf(Call('many_rows'));
  Assert.IsNotNull(LStructured, 'a list is structured content');
  try
    Assert.IsTrue(LStructured is TJSONArray, LStructured.ToJSON);
    Assert.AreEqual(2, TJSONArray(LStructured).Count);
    Assert.Contains(LStructured.ToJSON, 'Bob');
  finally
    LStructured.Free;
  end;
end;

procedure TStructuredContentTest.TestAStringIsStructuredContentToo;
var
  LStructured: TJSONValue;
begin
  ConfigureServer();

  LStructured := StructuredOf(Call('a_name'));
  Assert.IsNotNull(LStructured);
  try
    Assert.IsTrue(LStructured is TJSONString, LStructured.ToJSON);
    Assert.AreEqual('Alice', TJSONString(LStructured).Value);
  finally
    LStructured.Free;
  end;
end;

procedure TStructuredContentTest.TestANumberIsStructuredContentToo;
var
  LStructured: TJSONValue;
begin
  ConfigureServer();

  LStructured := StructuredOf(Call('a_count'));
  Assert.IsNotNull(LStructured);
  try
    Assert.IsTrue(LStructured is TJSONNumber, LStructured.ToJSON);
    Assert.AreEqual(42, TJSONNumber(LStructured).AsInt);
  finally
    LStructured.Free;
  end;
end;

procedure TStructuredContentTest.TestAnUntaggedToolCarriesNone;
var
  LContent: string;
begin
  ConfigureServer();

  // Structured output is what the "structured" tag asks for, and this tool
  // did not
  LContent := Call('plain_rows');

  Assert.IsNull(StructuredOf(LContent), LContent);
  Assert.DoesNotContain(LContent, 'structuredContent');
end;

procedure TStructuredContentTest.TestBytesStayABlob;
var
  LContent: string;
begin
  ConfigureServer();

  // Bytes go out as a base64 blob because they are binary; a structured copy
  // would be the same bytes spelled differently
  LContent := Call('some_bytes');

  Assert.IsNull(StructuredOf(LContent), LContent);
  Assert.Contains(LContent, 'blob');
end;

procedure TStructuredContentTest.TestTheTextBlockIsStillThere;
var
  LContent: string;
begin
  ConfigureServer();

  // "For backwards compatibility, a tool that returns structured content SHOULD
  // also return the serialized JSON in a TextContent block."
  LContent := Call('one_row');

  Assert.Contains(LContent, '"content"');
  Assert.Contains(LContent, 'Alice');
end;

procedure TStructuredContentTest.TestAnOutputSchemaDescribesWhateverIsReturned;
var
  LSchema: TJSONObject;
begin
  ConfigureServer();

  // An outputSchema may be any JSON Schema 2020-12 now, so the one for a list
  // describes a list. It used to be refused at registration.
  LSchema := OutputSchemaOf('many_rows');
  Assert.IsNotNull(LSchema, 'the tool must advertise an output schema');
  try
    Assert.AreEqual('array', LSchema.GetValue<string>('type', ''), LSchema.ToJSON);
  finally
    LSchema.Free;
  end;

  LSchema := OutputSchemaOf('a_name');
  Assert.IsNotNull(LSchema);
  try
    Assert.AreEqual('string', LSchema.GetValue<string>('type', ''), LSchema.ToJSON);
  finally
    LSchema.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStructuredContentTest);

end.
