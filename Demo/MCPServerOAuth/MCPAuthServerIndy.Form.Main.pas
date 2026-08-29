unit MCPAuthServerIndy.Form.Main;

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls,

  Logify,
  Logify.Adapter.Buffer,

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
    memoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TJRPCIndyServer;
    FLogifyAdapterFactory: ILoggerAdapterFactory;
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
  MCPAuthServer.Config;

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
  // Route MCPConnect's internal logging to the memo: the buffer adapter
  // collects messages from background threads and flushes them to the
  // TStrings target (memoLog.Lines) on the main thread via a timer.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(TLogLevel.Debug, memoLog.Lines);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FLogifyAdapterFactory);

  FServer := TJRPCIndyServer.CreateMCPServer(Self);
  TServerConfigurator.ConfigureServer(FServer.JRPCServer);

  StartServer;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Unregister before the memo is destroyed, otherwise background threads
  // still logging would write to a freed TStrings and cause an AV.
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FLogifyAdapterFactory);
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

end.
