unit MCPServerIndy.Form.Main;

{
  ==============================================================================
   MCPConnect demo - Indy transport host
  ==============================================================================

  This form is the *whole* transport layer of the demo. Everything that makes
  the server an MCP server (tools, resources, prompts, apps, sessions, ...) is
  declared in the shared unit MCPServer.Config, which is identical for the
  WebBroker, Stdio and Windows Service flavours of this same demo.

  The Indy flavour is the one to pick when you want:

    - full control over the HTTP layer (bindings, SSL/TLS, thread pool);
    - Server-Sent Events (server -> client notifications) on *any* supported
      Delphi version (the WebBroker transport can only stream on D13.1+);
    - a self-contained .exe with no web server to deploy in front of it.

  Endpoint: TJRPCIndyServer answers on every path, so with the default port
  the MCP endpoint is simply

      http://localhost:8080/

  Point an MCP client at it (MCPJam Inspector, LM Studio, or Claude Desktop
  through `npx mcp-remote http://localhost:8080/`).
}

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls,

  Logify,
  Logify.Adapter.Buffer,

  // TJRPCServer is the protocol engine: it owns the JSON-RPC dispatch, the
  // plugin/configuration chain and the session manager. It is transport
  // agnostic - the unit below is what plugs it into HTTP.
  MCPConnect.JRPC.Server,

  // TJRPCIndyServer = TIdCustomHTTPServer + an MCP request handler.
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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    /// <summary>
    ///   The HTTP transport. It descends from TIdCustomHTTPServer, so every
    ///   Indy property (Bindings, DefaultPort, IOHandler for SSL, the
    ///   Scheduler, MaxConnections, ...) is available here as usual.
    ///   Its JRPCServer property exposes the protocol engine to configure.
    /// </summary>
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

  // The shared, transport-independent server definition.
  MCPServer.Config;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Route MCPConnect's internal logging to the memo: the buffer adapter
  // collects messages from background threads and flushes them to the
  // TStrings target (memoLog.Lines) on the main thread via a timer.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(TLogLevel.Debug, memoLog.Lines);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FLogifyAdapterFactory);

  // 1) Build the transport.
  //    CreateMCPServer is a convenience factory: it creates the Indy server,
  //    creates and owns a TJRPCServer, and wires the MCP request handler
  //    (CORS, sessions, SSE, OAuth gate) into Indy's OnCommandGet/Other.
  //    Using the plain constructor instead would leave you to do that by hand.
  FServer := TJRPCIndyServer.CreateMCPServer(Self);

  // 2) Declare *what* the server exposes. Everything - name, version,
  //    capabilities, tools, resources, prompts, sessions, security - happens
  //    inside this single call. See MCPServer.Config.pas.
  TServerConfigurator.ConfigureServer(FServer.JRPCServer);

  // 3) Open the socket.
  StartServer;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Unregister before the memo is destroyed, otherwise background threads
  // still logging would write to a freed TStrings and cause an AV.
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FLogifyAdapterFactory);

  // Registrations live in the server's configuration objects, which are owned
  // by FServer and freed with it - so this call is *not* required for correct
  // shutdown. It is here to demonstrate the runtime unregistration API
  // (UnregisterClass / UnregisterFile / ClearAll), which is what you would use
  // to add or remove features from a *running* server, for example after a
  // login, when a licence expires, or when a plugin is unloaded.
  //
  // Whenever you change the feature set at runtime, remember to tell the
  // client by enqueuing a TToolListChangedNotification (or the resource /
  // prompt equivalent) so it refreshes its cached list.
  TServerConfigurator.UnregisterFeatures(FServer.JRPCServer);
end;

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  // Plain VCL UI state handling; FServer.Active is Indy's own Active property.
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TfrmMain.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;

  // NOTE: the MCP endpoint speaks JSON-RPC over POST (and SSE over GET with
  // an "Accept: text/event-stream" header). A browser issuing a plain GET
  // will therefore get a 405 - this button is only a shortcut to check that
  // the port is actually listening.
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0, nil, PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TfrmMain.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TfrmMain.ButtonStopClick(Sender: TObject);
begin
  // Closing the listening socket does not destroy the configuration: the
  // server can be restarted (below) with the same tools already registered.
  FServer.Active := False;
  Logger.Log('MCP Server Stopped', TLogLevel.Debug);
end;

procedure TfrmMain.StartServer;
begin
  if not FServer.Active then
  begin
    // Bindings.Clear + DefaultPort => listen on every local interface on the
    // chosen port. To bind a single address (or to add an HTTPS binding with
    // an IOHandler) fill FServer.Bindings explicitly instead.
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
    Logger.Log('MCP Server Started', TLogLevel.Debug);
  end;
end;

end.
