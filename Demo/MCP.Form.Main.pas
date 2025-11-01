unit MCP.Form.Main;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  System.TypInfo, System.JSON,
  Vcl.Graphics, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  JSON.RPC, System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList,
  Vcl.CategoryButtons, Vcl.ComCtrls, Vcl.ToolWin,

  MCP.Attributes,
  MCP.Types,
  MCP.Tools,
  MCP.Resources,
  MCP.Prompts,

  JSON.RPC.Invoker,

  Neon.Core.Tags,
  Neon.Core.Types,
  Neon.Core.Nullables,
  Neon.Core.Attributes,
  Neon.Core.Persistence,
  Neon.Core.Serializers.RTL,
  Neon.Core.Persistence.JSON,
  Neon.Core.Persistence.JSON.Schema;

type
  [Test.Abc()]
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FDeveloper: Boolean;
  public
    constructor Create(const AName: string);

    [NeonProperty('nome')]
    [JsonSchema('description=Name of the person,required=true,readOnly')]
    property Name: string read FName write FName;

    [JsonSchema('description=This is the age of the person,min=0')]
    property Age: Integer read FAge write FAge;

    [JsonSchema('description=True if the person is a developer')]
    property Developer: Boolean read FDeveloper write FDeveloper;
  end;

  TfrmMain = class(TForm)
    mmoLog: TMemo;
    Panel1: TPanel;
    mmoSnippets: TMemo;
    CategoryButtons1: TCategoryButtons;
    actListMain: TActionList;
    actJRPCEnvelope: TAction;
    actJRPCID: TAction;
    actRequestPos: TAction;
    actRequestNamed: TAction;
    actRequestDes: TAction;
    actResponse: TAction;
    actResponseDes: TAction;
    actToolSingle: TAction;
    actToolList: TAction;
    actInitializeRequest: TAction;
    actInitializeResult: TAction;
    actStructTags: TAction;
    actCallToolParams: TAction;
    actRttiCall: TAction;
    actInvokeRequest: TAction;
    tlbMain: TToolBar;
    actClearLog: TAction;
    btnClearLog: TToolButton;
    ilMain: TImageList;
    splMemo: TSplitter;
    actResource: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actRequestPosExecute(Sender: TObject);
    procedure actRequestDesExecute(Sender: TObject);
    procedure actRequestNamedExecute(Sender: TObject);
    procedure actResponseExecute(Sender: TObject);
    procedure actResponseDesExecute(Sender: TObject);
    procedure actJRPCEnvelopeExecute(Sender: TObject);
    procedure actJRPCIDExecute(Sender: TObject);
    procedure InitializeResultExecute(Sender: TObject);
    procedure actInitializeRequestExecute(Sender: TObject);
    procedure actRttiCallExecute(Sender: TObject);
    procedure actToolSingleExecute(Sender: TObject);
    procedure actInvokeRequestExecute(Sender: TObject);
    procedure actStructTagsExecute(Sender: TObject);
    procedure actToolListExecute(Sender: TObject);
    procedure actCallToolParamsExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actResourceExecute(Sender: TObject);
  private
    ctx: TRttiContext;
    tools: TArray<TRttiMethod>;
    procedure FilterTools;
    function GetNeonConfig: INeonConfiguration;
  public

    [McpTool('double_or_nothing', 'Doubles or zeroes the param value')] function TestParam(
      [McpParam('value1', 'Test Parameter 1 for MCP')] AParam1: Int64;
      [McpParam('value2', 'Test Parameter 2 for MCP')] AParam2: Boolean
    ): Integer;

    [McpTool('hello', 'Says: Hello World!')] function TestFunc(): string;

    [McpTool('double', 'Test Function')] function DoubleValue(
      [McpParam('value', 'Test Parameter')] AValue: Integer
    ): Integer;

    [McpTool('subtract', 'Subtracts "b" from "a"', 'readOnly,idempotent')]
    function Sub(
      [McpParam('minuend', 'minuend', 'required')] a: Integer;
      [McpParam('subtrahend', 'subtrahend')] b: Integer
    ): Integer;

    [McpTool('getname', 'Retrieves the name of the person', 'required,min=12')]
    function GetPersonName(
      [McpParam('person', 'The person object')] p: TPerson
    ): string;

    [McpTool('createperson', 'Test function returning a "person" object')]
    function CreatePerson(
      [McpParam('name', 'The person object')] const AName: string
    ): TPerson;

    [McpSchema('name=testing123,description=testing the new Attribute format')]
    function TestTags(
      [McpSchema('name', 'Name of the person',  'required,regex=![\n]')] const Name: string;
      [McpSchema('name=age,description=Age of the person,required,min=0')] Age: Integer;
      [McpSchema('name=active,description=If true the magic happens,required')] Active: Boolean
    ): TDateTime;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ctx := TRttiContext.Create;
  FilterTools;
end;

{ TForm1 }

procedure TfrmMain.actRequestPosExecute(Sender: TObject);
begin
  var r := TJRPCRequest.Create;
  r.Id := 1;
  r.Method := 'TestParam';
  r.AddPositionParam(12);
  r.AddPositionParam(23);

  mmoLog.Lines.Add(r.ToJson);

  r.free;
end;

procedure TfrmMain.actRequestDesExecute(Sender: TObject);
begin
  var r :=  TJRPCRequest.CreateFromJson(mmoLog.Lines.Text);
  try
    mmoLog.Lines.Add('method name: ' + r.Method);
    mmoLog.Lines.Add('param count: ' + r.ParamsCount.ToString);
  finally
    r.Free;
  end;
end;

procedure TfrmMain.actRequestNamedExecute(Sender: TObject);
begin
  var r := TJRPCRequest.Create;
  try
    r.Id := 1;
    r.Method := 'sum';
    r.AddNamedParam('first', 12);
    r.AddNamedParam('second', 'Paolo Rossi');

    mmoLog.Lines.Add(r.ToJson);
  finally
    r.Free;
  end;
end;

procedure TfrmMain.actResponseExecute(Sender: TObject);
begin
  var res := TJRPCResponse.Create;
  res.Id := 1;
  //r.Error.Code := 123;
  //r.Error.Message := 'Call Error';

  var list := TStringList.Create;
  list.Add('Paolo');
  list.Add('Rossi');
  res.Result := TNeon.ObjectToJSON(list);
  list.free;

  mmoLog.Lines.Add(res.ToJson);
  res.Free;
end;

procedure TfrmMain.actResponseDesExecute(Sender: TObject);
begin
  var res := TJRPCResponse.CreateFromJson(mmoLog.Lines.Text);

  if res.IsError then
    mmoLog.Lines.Add('Error detected: ' + res.Error.Message)
  else
    mmoLog.Lines.Add('Result (as JSON) is a: ' + res.Result.ClassName);

  res.Free;
end;

procedure TfrmMain.actRttiCallExecute(Sender: TObject);
var
  LRequest: TJRPCRequest;
  LResponse: TJRPCResponse;
  LMethodInvoker: IJRPCInvokable;
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

  LMethodInvoker := TJRPCObjectInvoker.Create(Self);
  LMethodInvoker.Invoke(LRequest, LResponse);

  // Show response
  mmoLog.Lines.Add(LResponse.ToJson);

  LRequest.Free;
  LResponse.Free;
end;

procedure TfrmMain.actJRPCEnvelopeExecute(Sender: TObject);
begin
  var env := TJRPCEnvelope.Create;
  env.ID := 'paolo';

  mmoLog.Lines.Add(env.ToJson);

  env.Free;
end;

procedure TfrmMain.actJRPCIDExecute(Sender: TObject);
var
  s: string;
begin

  var a: TJRPCID; // := 12;
  //a.Init;
  s := TNeon.ValueToJSONString(TValue.From<TJRPCID>(a), GetNeonConfig);
  mmoLog.Lines.Add(s);


  var v: TValue;
  v := 123;
  var v2 := v;
  s := TNeon.ValueToJSONString(TValue.From<TValue>(v), GetNeonConfig);
  mmoLog.Lines.Add(s);

end;

procedure TfrmMain.InitializeResultExecute(Sender: TObject);
begin
  var res := TInitializeResult.Create;

  res.ProtocolVersion := '1.0';
  res.Capabilities.Prompts.ListChanged := True;
  res.ServerInfo.Name := 'Server MCP';
  res.ServerInfo.Version := '0.9';

  mmoLog.Lines.Add(res.ToJSON(True));

  res.Free;
end;

procedure TfrmMain.actInitializeRequestExecute(Sender: TObject);
begin
  var pars := TInitializeParams.Create;
  pars.ProtocolVersion := '2025-06-18';
  pars.ClientInfo.Name := 'Delphi MCPLib';
  pars.ClientInfo.Version := '0.8';
  pars.Capabilities.Roots.ListChanged := True;

  var j := TNeon.ObjectToJSON(pars, MCPNeonConfig);
  mmoLog.Lines.Add(j.ToJson);

  var req := TJRPCRequest.Create;
  req.Method := MethodInitialize;
  req.Params := j;

  pars.Free;
  mmoLog.Lines.Add(req.ToJson);

  req.free;
end;

procedure TfrmMain.actInvokeRequestExecute(Sender: TObject);
begin
  var GC := TJRPCGarbageCollector.CreateInstance;

  var LRequest := TNeon.JSONToObject<TJRPCRequest>(mmoLog.Lines.Text, JRPCNeonConfig);
  GC.Add(LRequest);
  mmoLog.Lines.Add('------ SERIALIZED REQUEST ------');
  mmoLog.Lines.Add( TNeon.ObjectToJSONString(LRequest, JRPCNeonConfig) );

  var LResponse := TJRPCResponse.Create;
  GC.Add(LResponse);
  var LMethodInvoker: IJRPCInvokable := TJRPCObjectInvoker.Create(Self);
  LMethodInvoker.NeonConfig := TNeonConfiguration.Camel;
  LMethodInvoker.Invoke(LRequest, LResponse);

  mmoLog.Lines.Add('------ SERIALIZED RESPONSE ------');
  var LStringResponse := TNeon.ObjectToJSONString(LResponse, JRPCNeonConfig);
  mmoLog.Lines.Add(LStringResponse);

end;

procedure TfrmMain.actStructTagsExecute(Sender: TObject);
begin
  var tags := TAttributeTags.Create();
  tags.Parse('title=Come stai??,description=sto bene,readOnly,age=12');

  for var tag in tags.TagMap do
  begin
    mmoLog.Lines.add('Name: ' + tag.Key);
    mmoLog.Lines.add('Value: ' + tag.Value);
    mmoLog.Lines.add('----------------');
  end;

  var b: Boolean := Tags.GetValueAs<Boolean>('readOnly');

  if b then
    mmoLog.Lines.add('It is True!!');

  tags.free;
end;

procedure TfrmMain.actToolListExecute(Sender: TObject);
begin
  var tools := TMCPSchemaGenerator.ListTools(Self.ClassType);

  mmoLog.Lines.Add(tools.ToJSON(True));

  tools.Free;
end;

procedure TfrmMain.actToolSingleExecute(Sender: TObject);
begin
  var typ := ctx.GetType(Self.ClassType);
  var m := typ.GetMethod('TestParam');

  var schema := TMCPSchemaGenerator.MethodToTool(m);
  {
  var j := TNeon.ObjectToJSON(t, GetNeonConfig2) as TJSONObject;

  j.RemovePair('inputSchema');
  j.AddPair('inputSchema', schema);
  }
  //mmoLog.Lines.Add(TNeon.Print(schema, true));

  mmoLog.Lines.Add(schema.ToJSON(True));

  schema.Free;
end;

procedure TfrmMain.actCallToolParamsExecute(Sender: TObject);
begin
  var c := TCallToolParams.Create;

  c.Name := 'Somma';
  c.Arguments.AddPair('arg1', TNeon.ValueToJSON(12));
  c.Arguments.AddPair('arg2', TNeon.ValueToJSON(Now()));


  var s := TNeon.ObjectToJSONString(c, MCPNeonConfig);
  mmoLog.Lines.Add(s);
  c.free;

  mmoLog.Lines.Add('------------------');

  c :=  TNeon.JSONToObject<TCallToolParams>(s, MCPNeonConfig);
  mmoLog.Lines.Add('Method: ' + c.Name);
  for var arg in c.Arguments do
  begin
    mmoLog.Lines.Add('Argument Name: ' + arg.JsonString.Value);
    mmoLog.Lines.Add('Argument Value: ' + arg.JsonValue.Value);
    mmoLog.Lines.Add('--');
  end;

  c.Free;

end;

procedure TfrmMain.actClearLogExecute(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TfrmMain.actResourceExecute(Sender: TObject);
begin
  var res := TMCPResource.Create;
  res.Name := 'Clients';
  res.URI := '/resources/clients';
  res.MIMEType := 'application/json';
  mmoLog.Lines.Add(TNeon.ObjectToJSONString(res, MCPNeonConfig));
  res.Free;

  mmoLog.Lines.Add('----------------------');

  var tpl := TMCPResourceTemplate.Create;
  tpl.Name := 'Article Template';
  tpl.URITemplate := '/templates/article';
  tpl.MIMEType := 'application/json';
  mmoLog.Lines.Add(TNeon.ObjectToJSONString(tpl, MCPNeonConfig));
  tpl.Free;

  mmoLog.Lines.Add('----------------------');

  var lst := TListResourcesResult.Create;
  lst.AddResource('clients', '/resources/clients', 'application/json');
  lst.AddResource('orders', '/resources/orders', 'application/xml');
  mmoLog.Lines.Add(TNeon.ObjectToJSONString(lst, MCPNeonConfig));
  tpl.Free;


end;

function TfrmMain.CreatePerson(const AName: string): TPerson;
begin
  Result := TPerson.Create(AName);
end;

function TfrmMain.DoubleValue(AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;

procedure TfrmMain.FilterTools;
begin
  var typ := ctx.GetType(Self.ClassType);

  var methods := typ.GetMethods;

  for var m in methods do
    if Assigned(m.GetAttribute(MCPToolAttribute)) then
      tools := tools + [m];
end;

function TfrmMain.GetNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Default
    .RegisterSerializer(TTValueSerializer)
    .RegisterSerializer(TJRequestSerializer)
    .RegisterSerializer(TJResponseSerializer);
end;

function TfrmMain.GetPersonName(p: TPerson): string;
begin
  Result := p.Name;
end;

function TfrmMain.Sub(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function TfrmMain.TestParam(AParam1: Int64; AParam2: Boolean): Integer;
begin
  Result := AParam1 * 2;
end;

function TfrmMain.TestTags(const Name: string; Age: Integer; Active: Boolean): TDateTime;
begin
  Result := Now();
end;

function TfrmMain.TestFunc: string;
begin
  Result := 'Hello World!';
end;

{ TPerson }

constructor TPerson.Create(const AName: string);
begin
  FName := AName;
end;

end.
