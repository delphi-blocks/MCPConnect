unit MCP.Form.Main;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,
  Vcl.Graphics, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  JRPC.Classes,

  MCP.Attributes,
  MCP.Tools,
  MCP.Tools.Schema,
  MCP.Invoker,

  Neon.Core.Types,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Serializers.RTL,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema;

type
  TForm1 = class(TForm)
    btnEnvelope: TButton;
    mmoLog: TMemo;
    btnId: TButton;
    btnResponse: TButton;
    btnRequestDes: TButton;
    btnRequestNamed: TButton;
    btnRtti: TButton;
    btnResponseDes: TButton;
    btnRequest: TButton;
    Panel1: TPanel;
    mmoSnippets: TMemo;
    lblLog: TLabel;
    lblSnippets: TLabel;
    btnToolSerialize: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnRequestClick(Sender: TObject);
    procedure btnRequestDesClick(Sender: TObject);
    procedure btnRequestNamedClick(Sender: TObject);
    procedure btnResponseClick(Sender: TObject);
    procedure btnResponseDesClick(Sender: TObject);
    procedure btnEnvelopeClick(Sender: TObject);
    procedure btnIdClick(Sender: TObject);
    procedure btnRttiClick(Sender: TObject);
    procedure btnToolSerializeClick(Sender: TObject);
  private
    ctx: TRttiContext;
    tools: TArray<TRttiMethod>;
    procedure FilterTools;
    function GetNeonConfig: INeonConfiguration;
    function GetNeonConfig2: INeonConfiguration;
  public
    [McpTool('double_or_nothing', 'Doubles or zeroes the param value')] function TestParam(
      [McpParam('value1', 'Test Parameter 1 for MCP', true)] AParam1: Integer;
      [McpParam('value2', 'Test Parameter 2 for MCP', true)] AParam2: Boolean
    ): Integer;


    [McpTool('hello', 'Test Function')] function TestFunc(): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ctx := TRttiContext.Create;
  FilterTools;
end;

{ TForm1 }

procedure TForm1.btnRequestClick(Sender: TObject);
begin
  var r := TJRPCRequest.Create;
  r.Id := 1;
  r.Method := 'TestParam';
  r.AddPositionParam(12);
  r.AddPositionParam(23);

  var s := TNeon.ObjectToJSONString(r, GetNeonConfig);
  mmoLog.Lines.Add(s);
end;

procedure TForm1.btnRequestDesClick(Sender: TObject);
begin
  var r := TNeon.JSONToObject<TJRPCRequest>(mmoLog.Lines.Text, GetNeonConfig);

  mmoLog.Lines.Add('method name: ' + r.Method);
  mmoLog.Lines.Add('param count: ' + r.Params.Count.ToString);
end;

procedure TForm1.btnRequestNamedClick(Sender: TObject);
begin
  var r := TJRPCRequest.Create;
  r.Id := 1;
  r.Method := 'sum';
  r.AddNamedParam('first', 12);
  r.AddNamedParam('second', 'Paolo Rossi');

  var s := TNeon.ObjectToJSONString(r, GetNeonConfig);
  mmoLog.Lines.Add(s);
end;

procedure TForm1.btnResponseClick(Sender: TObject);
begin
  var r := TJRPCResponse.Create;
  r.Id := 1;
  //r.Error.Code := 123;
  //r.Error.Message := 'Call Error';

  var l := TStringList.Create;
  l.Add('Paolo');
  l.Add('Rossi');
  r.Result := l;

  var s := TNeon.ObjectToJSONString(r, GetNeonConfig);
  mmoLog.Lines.Add(s);
end;

procedure TForm1.btnResponseDesClick(Sender: TObject);
begin
  var r := TNeon.JSONToObject<TJRPCResponse>(mmoLog.Lines.Text, GetNeonConfig);

  if r.IsError then
    mmoLog.Lines.Add('Error detected: ' + r.Error.Message)
  else
    mmoLog.Lines.Add('Result (as JSON) is a: ' + r.Result.AsObject.ClassName);
end;

procedure TForm1.btnRttiClick(Sender: TObject);
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LMethodInvoker: IMCPInvokable;
begin
  // Build the test request
  LRequest := TJRPCRequest.Create;
  LRequest.Id := 1;
  LRequest.Method := 'double';
  //LRequest.AddPositionParam(12);
  LRequest.AddNamedParam('value', 12);

  // Build the response object
  LResponse := TJRPCResponse.Create;
  LResponse.Id := 1;

  LMethodInvoker := TMCPObjectInvoker.Create(Self);
  LMethodInvoker.Invoke(LRequest, LResponse);

  // Show response
  var s := TNeon.ObjectToJSONString(LResponse, GetNeonConfig);
  mmoLog.Lines.Add(s);

  LRequest.Free;
  LResponse.Free;
end;

procedure TForm1.btnEnvelopeClick(Sender: TObject);
begin
  var env := TJRPCEnvelope.Create;
  //env.ID := 'paolo';

  var s := TNeon.ObjectToJSONString(env, GetNeonConfig);
  mmoLog.Lines.Add(s);
end;

procedure TForm1.btnIdClick(Sender: TObject);
begin
  var a: TJRPCID; // := 12;

  var s := TNeon.ValueToJSONString(TValue.From<TJRPCID>(a), GetNeonConfig);
  mmoLog.Lines.Add(s);
end;

procedure TForm1.btnToolSerializeClick(Sender: TObject);
begin
  var typ := ctx.GetType(Self.ClassType);
  var m := typ.GetMethod('TestParam');

  var schema := TMCPSchemaGenerator.MethodToJSONSchema(m);
  {
  var j := TNeon.ObjectToJSON(t, GetNeonConfig2) as TJSONObject;

  j.RemovePair('inputSchema');
  j.AddPair('inputSchema', schema);
  }
  mmoLog.Lines.Add(TNeon.Print(schema, true));
end;

procedure TForm1.FilterTools;
begin
  var typ := ctx.GetType(Self.ClassType);

  var methods := typ.GetMethods;

  for var m in methods do
    if Assigned(m.GetAttribute(MCPToolAttribute)) then
      tools := tools + [m];
end;

function TForm1.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TTValueSerializer)
    .RegisterSerializer(TJParamsSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;

function TForm1.GetNeonConfig2: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .SetMembers([TNeonMembers.Fields])
    .RegisterSerializer(TTValueSerializer)
end;

function TForm1.TestParam([McpParam('value1', 'Test Parameter 1 for MCP',
    true)] AParam1: Integer; [McpParam('value2', 'Test Parameter 2 for MCP',
    true)] AParam2: Boolean): Integer;
begin
  Result := AParam1 * 2;
end;

function TForm1.TestFunc: string;
begin
  Result := 'Hello World';
end;

end.
