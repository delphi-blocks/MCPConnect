unit MCPServerDemo.MainForm;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,System.Types,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp,
  IdContext,
  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Invoker,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    memoLog: TMemo;
    Label1: TLabel;
    Button1: TButton;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
  public
    { Public declarations }

    procedure TestInvoke(const AName: string);

    [MCPTool('TestInt', 'Test Result')]
    function TestInt: Integer;

    [MCPTool('TestFloat', 'Test Result')]
    function TestFloat: Double;

    [MCPTool('TestEnum', 'Test Result')]
    function TestEnum: TShiftStateItem;

    [MCPTool('TestSet', 'Test Result')]
    function TestSet: TShiftState;

    [MCPTool('TestChar', 'Test Result')]
    function TestChar: Char;

    [MCPTool('TestString', 'Test Result')]
    function TestString: string;

    [MCPTool('TestRecord', 'Test Result')]
    function TestRecord: TPoint;

    [MCPTool('TestDate', 'Test Result')]
    function TestDate: TDateTime;

    [MCPTool('TestObject', 'Test Result')]
    function TestObject: TStringList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi,
{$ENDIF}
  System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
{
  var res := TCallToolResult.Create;
  var inv := TMCPObjectInvoker.Create(Self);

  TestInvoke('TestInt');
  TestInvoke('TestFloat');
  TestInvoke('TestEnum');
  TestInvoke('TestSet');
  TestInvoke('TestChar');
  TestInvoke('TestString');
  TestInvoke('TestDate');
  TestInvoke('TestRecord');
  TestInvoke('TestObject');

  res.Free;
  inv.Free;
}
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  var cfg := TNeonConfiguration.Camel.SetMembers([TNeonMembers.Fields]);
  var emb := TEmbeddedResource.Create;
  TBlobResourceContents(emb.Resource).Blob := 'ieydwg3rf7fgrg76ergf67wrgfrgf6gr2f3';
  TBlobResourceContents(emb.Resource).MIMEType := 'application/octect-stream';
  //var s := TNeon.ObjectToJSONString(emb, cfg);
  //memoLog.Lines.Add(s);


  var res := TCallToolResult.Create;
  res.Content.Add(emb);
  var ss := TNeon.ObjectToJSONString(res, cfg);
  memoLog.Lines.Add(ss);
  res.Free;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LURL: string;
{$ENDIF}
begin
  StartServer;
{$IFDEF MSWINDOWS}
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
{$ENDIF}
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  FServer.OnParseAuthentication := ParseAuthentication;
  StartServer;
end;

procedure TForm1.ParseAuthentication(AContext: TIdContext; const AAuthType,
  AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TForm1.TestInvoke(const AName: string);
var
  LContent: TBaseContent;
  LText: TTextContent absolute LContent;
  LBlob: TEmbeddedResource absolute LContent;
begin
  var res := TCallToolResult.Create;
  var inv := TMCPObjectInvoker.Create(Self);


  inv.Invoke(AName, nil, nil, res);
  for LContent in res.Content do
  begin
    if LContent is TTextContent then
      memoLog.Lines.Add(AName + ': ' + LText.Text)
    else if LContent is TEmbeddedResource then
      memoLog.Lines.Add(AName + ': ' + (LBlob.Resource as TBlobResourceContents).Blob);

    memoLog.Lines.Add('----------------------');
  end;

  res.Free;
  inv.Free;
end;

function TForm1.TestObject: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('First');
  Result.Add('Second');
end;

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

function TForm1.TestChar: Char;
begin
  Result := 'A';
end;

function TForm1.TestDate: TDateTime;
begin
  Result  := Now;
end;

function TForm1.TestEnum: TShiftStateItem;
begin
  Result := ssCtrl;
end;

function TForm1.TestFloat: Double;
begin
  Result := 1234.789;
end;

function TForm1.TestInt: Integer;
begin
  Result := 42;
end;

function TForm1.TestRecord: TPoint;
begin
  Result.X := 12;
  Result.Y := 33;
end;

function TForm1.TestSet: TShiftState;
begin
  Result := [ssMiddle, ssDouble];
end;

function TForm1.TestString: string;
begin
  Result := 'MCP';
end;

end.
