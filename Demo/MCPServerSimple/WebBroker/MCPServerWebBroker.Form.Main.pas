unit MCPServerWebBroker.Form.Main;

{
  ==============================================================================
   MCPConnect demo - standalone host for the WebBroker web module
  ==============================================================================

  This form contains no MCP code at all: it is the standard Delphi "standalone
  web server" harness, hosting the web module through TIdHTTPWebBrokerBridge.
  The MCP wiring lives one unit away, in MCPServerWebBroker.WebModule.pas.

  That separation is the point of the WebBroker transport: the same web module
  is deployed unchanged as ISAPI, Apache module, CGI or FastCGI, and this form
  exists only so the thing can be run and debugged from the IDE.

  Endpoint with the default port:  http://localhost:8080/mcp
  (the path comes from TJRPCDispatcher.PathInfo in the web module)
}

interface

uses
  Winapi.Messages, System.SysUtils, System.Types, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls,
  IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp, IdContext,

  Logify,
  Logify.Adapter.Buffer,
  MCPServerWebBroker.WebModule;

type
  TfrmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    memoLog: TMemo;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    /// <summary>
    ///   Indy-based WebBroker host. It instantiates the web module registered
    ///   in the .dpr (WebRequestHandler.WebModuleClass) once per request
    ///   thread, which is what makes each request find its own TJRPCServer -
    ///   see the note in MCPServerWebBroker.WebModule.pas.
    /// </summary>
    FServer: TIdHTTPWebBrokerBridge;
    FLogifyAdapterFactory: ILoggerAdapterFactory;
    procedure StartServer;

    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi;
{$ENDIF}

{ TfrmMain }

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TfrmMain.ButtonOpenBrowserClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LURL: string;
{$ENDIF}
begin
  StartServer;
{$IFDEF MSWINDOWS}
  // Opens the site root, not /mcp: the root is served by the web module
  // default handler, whereas the MCP endpoint expects POST and would answer a
  // browser GET with an error.
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
  // Route MCPConnect's internal logging to the memo: the buffer adapter
  // collects messages from background threads and flushes them to the
  // TStrings target (memoLog.Lines) on the main thread via a timer.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(TLogLevel.Debug, memoLog.Lines);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FLogifyAdapterFactory);


  FServer := TIdHTTPWebBrokerBridge.Create(Self);

  // Without this handler Indy tries to parse the Authorization header itself
  // and rejects any scheme it does not implement - including "Bearer", which
  // is exactly what MCP token auth and OAuth use. Claiming the header as
  // handled leaves it untouched in the request, so MCPConnect can read it.
  FServer.OnParseAuthentication := ParseAuthentication;

  StartServer;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Unregister before the memo is destroyed, otherwise background threads
  // still logging would write to a freed TStrings and cause an AV.
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FLogifyAdapterFactory);
end;

procedure TfrmMain.ParseAuthentication(AContext: TIdContext; const AAuthType,
  AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  // See the comment in FormCreate: this deliberately does nothing except stop
  // Indy from failing the request.
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
