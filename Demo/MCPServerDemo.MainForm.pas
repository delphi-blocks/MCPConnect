unit MCPServerDemo.MainForm;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,System.Types,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp,
  IdContext,

  MCPServerDemo.WebModule,

  Neon.Core.Nullables,
  Neon.Core.Tags,
  Neon.Core.Attributes,

  MCPConnect.MCP.Tools,
  MCPConnect.MCP.Prompts,
  MCPConnect.MCP.Resources,

  MCPConnect.MCP.Config,
  MCPConnect.MCP.Invoker,
  MCPConnect.MCP.Types,
  MCPConnect.MCP.Attributes;

type
   THeaderTest = class
  private
    Fresults: string;
    procedure Setresults(const Value: string);
  public
     [NeonProperty('results')]
     property results: string read Fresults write Setresults;
   end;


   TTest = class
  private
    Fd: THeaderTest;
    procedure Setd(const Value: THeaderTest);
  public
     constructor Create;
     destructor Destroy; override;

     [NeonProperty('d')]
     property d: THeaderTest read Fd write Setd;
   end;

  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    memoLog: TMemo;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    btnConfig: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnConfigClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
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
  System.RegularExpressions,

  Neon.Core.Types,
  Neon.Core.Serializers.RTL,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPServerDemo.Resources,
  MCPServerDemo.Tools,
  MCPConnect.Configuration.Neon,
  MCPConnect.MCP.Server.Api;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TForm1.btnConfigClick(Sender: TObject);
begin
  var mcp := TMCPConfig.Create;

  var remote := TMCPConfigServerRemote.Create;
  remote.&Type := 'http';
  remote.Url := 'http://localhost:8080/mcp';
  remote.Headers.Add('Authorization', 'Bearer ' + '378eye6t.e3y883yee3eu8yg32e63.93ue983u');
  mcp.Servers.Add('mcp-connect-remote', remote);

  var local := TMCPConfigServerLocal.Create;
  local.&Type := 'stdio';
  local.Command := 'mcp.exe';
  local.Args := ['-v', './data'];
  local.Env.Add('KEY', 'aabbccdd');
  mcp.Servers.Add('mcp-connect-local', local);

  var s := TNeon.ObjectToJSONString(mcp,
    TNeonConfiguration
      .Camel
      .SetMembers([TNeonMembers.Fields])
      .SetPrettyPrint(True)
      .SetMemberSort(TNeonSort.RttiReverse)
  );

  memoLog.Lines.Add(s);

  mcp.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  var res: TArray<string> := [];
  var matches := TRegEx.Matches('demo://weather/dynamic/{city}/{celsius}', '[^{\}]+(?=})');

  for var match in matches do
  begin
    res := res + [match.Value];
    memoLog.Lines.Add(match.Value);
  end;

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

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

{ THeaderTest }

procedure THeaderTest.Setresults(const Value: string);
begin
  Fresults := Value;
end;

{ TTest }

constructor TTest.Create;
begin
  fd := THeaderTest.Create;
end;

destructor TTest.Destroy;
begin
  d.Free;
  inherited;
end;

procedure TTest.Setd(const Value: THeaderTest);
begin
  Fd := Value;
end;

end.
