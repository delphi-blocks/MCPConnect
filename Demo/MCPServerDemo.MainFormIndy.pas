unit MCPServerDemo.MainFormIndy;

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
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  WinApi.Windows, Winapi.ShellApi,
  MCPServerDemo.Tools,
  System.Generics.Collections;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not IdHTTPServer1.Active;
  ButtonStop.Enabled := IdHTTPServer1.Active;
  EditPort.Enabled := not IdHTTPServer1.Active;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0, nil, PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  IdHTTPServer1.Active := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IdHTTPServer1 := TJRPCIndyServer.Create(Self);

  FJRPCServer := TJRPCServer.Create(Self);

  FJRPCServer
    .Plugin.Configure<IAuthTokenConfig>
      .SetToken('my-secret-token')
      .ApplyConfig

    .Plugin.Configure<ISessionConfig>
      .SetLocation(TSessionIdLocation.Header)
      .SetHeaderName('Mcp-Session-Id')
      .SetTimeout(30)  // 30 minutes timeout
      .SetSessionClass(TShoppingSession)  // Use custom typed session
      .ApplyConfig

    .Plugin.Configure<IMCPConfig>
      .SetServerName('delphi-mcp-server')
      .SetServerVersion('2.0.0')
      .RegisterToolClass('test', TTestTool)
      .RegisterToolClass('delphi_day', TDelphiDayTool)
      .RegisterToolClass('shopping', TShoppingCartTool)  // Session-based shopping cart
      .ApplyConfig;

//    .Plugin.Configure<IJRPCNeonConfig>
//      .SetNeonConfig(MCPNeonConfig)
//      .ApplyConfig;

  IdHTTPServer1.Server := FJRPCServer;

  StartServer;
end;

procedure TForm1.StartServer;
begin
  if not IdHTTPServer1.Active then
  begin
    IdHTTPServer1.Bindings.Clear;
    IdHTTPServer1.DefaultPort := StrToInt(EditPort.Text);
    IdHTTPServer1.Active := True;
  end;
end;

end.
