unit MetricsWeb.Form.Main;

{
  ==============================================================================
   The standalone host for the WebStencils dashboard

   This form contains no MCP and no template code: it is the usual Delphi
   "standalone web server" harness around TIdHTTPWebBrokerBridge, plus the two
   timers that make the dashboard possible:

     TimerSample    every 2 s - samples the host gauges and files one history
                    entry, so the charts have a past and the fold sees each
                    window before it closes;
     TimerHarvest   every 30 s - the delta harvest the file exporters and the
                    fold are about (Harvest(True): exports, then clears).

   Everything the page shows is wired in MetricsWeb.WebModule.pas; the templates
   are in the templates folder beside this project.
}

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ExtCtrls,
  IdHTTPWebBrokerBridge, IdGlobal, IdContext, Web.HTTPApp,

  Logify,
  Logify.Adapter.Buffer,

  MetricsWeb.WebModule;

type
  TfrmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonOpenDashboard: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    LabelHint: TLabel;
    memoLog: TMemo;
    ApplicationEvents1: TApplicationEvents;
    TimerSample: TTimer;
    TimerHarvest: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenDashboardClick(Sender: TObject);
    procedure TimerSampleTimer(Sender: TObject);
    procedure TimerHarvestTimer(Sender: TObject);
    /// <summary>
    ///   A request that raises must not put a modal dialog on the main thread:
    ///   in a web application that dialog is a hung request. The exception is
    ///   logged instead, and the client gets WebBroker's 500.
    /// </summary>
    procedure ApplicationException(Sender: TObject; E: Exception);
  private
    FServer: TIdHTTPWebBrokerBridge;
    FLogifyAdapterFactory: ILoggerAdapterFactory;
    /// <summary>
    ///   The three targets the metrics hub's own exporters write into. The hub
    ///   needs them to exist and to outlive it; the page does not read them.
    /// </summary>
    FTextTarget: TStrings;
    FJsonTarget: TStrings;
    FSlowestTarget: TStrings;
    procedure StartServer;
    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String;
      var VUsername, VPassword: String; var VHandled: Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  Winapi.ShellApi,
  System.IOUtils,

  MCPConnect.Metrics,

  Server.Metrics,
  Server.Metrics.Document;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // MCPConnect's own logging goes to the memo, through the buffer adapter that
  // collects messages from background threads and flushes them on the main one.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(
    TLogLevel.Debug, memoLog.Lines);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FLogifyAdapterFactory);

  // Where the templates are: -templates <path> wins, else the templates folder
  // beside the executable.
  for var LIndex := 1 to ParamCount - 1 do
    if SameText(ParamStr(LIndex), '-templates') then
      TemplatesRoot := ParamStr(LIndex + 1);
  LabelHint.Caption := 'templates: ' + ResolveTemplatesRoot;

  // The hub: instruments, the seven exporters, and the reports the dashboard
  // shows besides the numbers. The three targets must outlive it.
  FTextTarget := TStringList.Create;
  FJsonTarget := TStringList.Create;
  FSlowestTarget := TStringList.Create;
  TServerMetrics.Instance.Start(FTextTarget, FJsonTarget, FSlowestTarget);

  // The Indy bridge: WebBroker creates one web module per request, so nothing
  // of the dashboard lives here.
  FServer := TIdHTTPWebBrokerBridge.Create(Self);

  // Without this handler Indy parses the Authorization header itself and rejects
  // any scheme it does not implement - including "Bearer", which is what MCP
  // token auth and OAuth use.
  FServer.OnParseAuthentication := ParseAuthentication;

  // Delphi's default is a message box, which for a web request means a request
  // that never gets an answer. Log it and answer 500.
  Application.OnException := ApplicationException;

  StartServer;

  TimerSample.Enabled := True;
  TimerHarvest.Enabled := True;

  Logger.Log(Format('Dashboard: http://localhost:%s%s (templates %s)', [
    EditPort.Text, DASHBOARD_PATH, ResolveTemplatesRoot]), TLogLevel.Info);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Timers first: they write into the history and the hub.
  TimerSample.Enabled := False;
  TimerHarvest.Enabled := False;

  if Assigned(FServer) then
  begin
    FServer.Active := False;
    FServer.Bindings.Clear;
  end;

  // Detach the exporters (this closes metrics.jsonl and metrics.log) and release
  // the hub before the targets it writes into go away.
  TServerMetrics.Instance.Stop;
  TServerMetrics.Release;
  FreeAndNil(FTextTarget);
  FreeAndNil(FJsonTarget);
  FreeAndNil(FSlowestTarget);

  // Unregister before the memo is destroyed: a background thread still logging
  // would otherwise write to a freed TStrings.
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FLogifyAdapterFactory);
end;

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
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

procedure TfrmMain.ButtonOpenDashboardClick(Sender: TObject);
const
  // Any process may take the foreground; Winapi.Windows does not declare it
  ASFW_ANY = DWORD(-1);
var
  LUrl: string;
  LResult: HINST;
begin
  StartServer;
  LUrl := Format('http://localhost:%s%s', [EditPort.Text, DASHBOARD_PATH]);

  // The browser is another process, and Windows only lets a process bring its
  // window to the front when the foreground one allows it. Without this, and
  // with SW_SHOWNOACTIVATE as it used to be, the tab did open - in a browser
  // window left behind this form, which looked exactly like nothing happening.
  AllowSetForegroundWindow(ASFW_ANY);
  LResult := ShellExecute(Handle, 'open', PChar(LUrl), nil, nil, SW_SHOWNORMAL);

  // ShellExecute reports failure as a value of 32 or less, and raises nothing
  if LResult <= 32 then
    Logger.Log(Format('Could not open %s in the browser (ShellExecute error %d)',
      [LUrl, LResult]), TLogLevel.Error);
end;

procedure TfrmMain.ApplicationException(Sender: TObject; E: Exception);
var
  LLog: string;
begin
  LLog := Format('[%s] %s: %s%s%s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    E.ClassName, E.Message, sLineBreak, E.StackTrace]);
  Logger.Log(Format('%s: %s', [E.ClassName, E.Message]), TLogLevel.Error);

  // Beside the executable as well: an exception raised on a request thread is
  // exactly the one nobody is looking at the memo for.
  try
    TFile.AppendAllText(TPath.Combine(ExtractFilePath(ParamStr(0)), 'web-error.log'),
      LLog + sLineBreak, TEncoding.UTF8);
  except
    // Nothing useful to do: the log is diagnostics, not the application.
  end;
end;

procedure TfrmMain.TimerSampleTimer(Sender: TObject);
var
  LReport: TMetricsReport;
begin
  // Host gauges first, so the report being filed carries this tick's values.
  TServerMetrics.Instance.SampleSystem;

  LReport := BuildMetricsReport(SERVICE_NAME, SERVICE_VERSION, SERVICE_DESCRIPTION);
  try
    MetricsHistory.Add(LReport);
  finally
    LReport.Free;
  end;
end;

procedure TfrmMain.TimerHarvestTimer(Sender: TObject);
begin
  // A delta harvest: the exporters get what happened since the previous tick and
  // the provider is cleared. The dashboard's counters stay monotonic across it
  // because the document state folds the window away as it closes.
  if Length(TMetrics.Collect) > 0 then
    TServerMetrics.Instance.HarvestDelta;
end;

procedure TfrmMain.ParseAuthentication(AContext: TIdContext; const AAuthType,
  AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  // Deliberately does nothing but stop Indy from failing the request.
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
