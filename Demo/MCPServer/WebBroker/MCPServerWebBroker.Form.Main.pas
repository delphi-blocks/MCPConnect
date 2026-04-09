unit MCPServerWebBroker.Form.Main;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,System.Types,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp,
  IdContext,

  MCPServerWebBroker.WebModule,

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
  TfrmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    memoLog: TMemo;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    btnConfig: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnConfigClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi,
{$ENDIF}
  System.Generics.Collections,

  Neon.Core.Types,
  Neon.Core.Serializers.RTL,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON,

  MCPServer.Resources,
  MCPServer.Tools,
  MCPConnect.JRPC.Classes,
  MCPConnect.Configuration.Neon,
  MCPConnect.MCP.Server.Api;

{ TfrmMain }

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TfrmMain.btnConfigClick(Sender: TObject);
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

procedure TfrmMain.ButtonOpenBrowserClick(Sender: TObject);
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

procedure TfrmMain.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TfrmMain.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  FServer.OnParseAuthentication := ParseAuthentication;
  StartServer;
end;

procedure TfrmMain.ParseAuthentication(AContext: TIdContext; const AAuthType,
  AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TfrmMain.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

end.
