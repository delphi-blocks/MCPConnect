unit MCPServerIndy.Form.Main;

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls,

  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Indy;

type
  TfrmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TJRPCIndyServer;

    procedure StartServer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  WinApi.Windows, Winapi.ShellApi,
  Logify, Logify.Adapter.Debug,
  MCPServer.Config;

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TfrmMain.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0, nil, PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TfrmMain.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TfrmMain.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  Logger.Log('MCP Server Stopped', TLogLevel.Debug);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FServer := TJRPCIndyServer.CreateMCPServer(Self);
  TServerConfigurator.ConfigureServer(FServer.JRPCServer);

  StartServer;
end;

procedure TfrmMain.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
    Logger.Log('MCP Server Started', TLogLevel.Debug);
  end;
end;

initialization
  TLoggerAdapterRegistry.Instance.RegisterFactory(
    TLogifyAdapterDebugFactory.CreateAdapterFactory('Debug log', TLogLevel.Debug));

end.
