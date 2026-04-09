unit MCPServerIndy.Form.Main;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdGlobal, Web.HTTPApp,
  IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer,

  MCPConnect.JRPC.Server,
  MCPConnect.Transport.Indy,

  MCPConnect.MCP.Server.Api,

  MCPConnect.Configuration.MCP,
  MCPConnect.Configuration.Session,
  MCPConnect.Configuration.Auth,

  MCPConnect.Content.Writers.RTL,
  MCPConnect.Content.Writers.VCL;

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
    IdHTTPServer1: TJRPCIndyServer;
    FJRPCServer: TJRPCServer;
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
  ButtonStart.Enabled := not IdHTTPServer1.Active;
  ButtonStop.Enabled := IdHTTPServer1.Active;
  EditPort.Enabled := not IdHTTPServer1.Active;
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
  IdHTTPServer1.Active := False;
  Logger.Log('MCP Server Stopped', TLogLevel.Debug);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IdHTTPServer1 := TJRPCIndyServer.Create(Self);

  FJRPCServer := TJRPCServer.Create(Self);

  TServerConfigurator.ConfigureServer(FJRPCServer);

  IdHTTPServer1.Server := FJRPCServer;

  StartServer;
end;

procedure TfrmMain.StartServer;
begin
  if not IdHTTPServer1.Active then
  begin
    IdHTTPServer1.Bindings.Clear;
    IdHTTPServer1.DefaultPort := StrToInt(EditPort.Text);
    IdHTTPServer1.Active := True;
    Logger.Log('MCP Server Started', TLogLevel.Debug);
  end;
end;

initialization
  TLoggerAdapterRegistry.Instance.RegisterFactory(
    TLogifyAdapterDebugFactory.CreateAdapterFactory('Debug log', TLogLevel.Info));

end.
