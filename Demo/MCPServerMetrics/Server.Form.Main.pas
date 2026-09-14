unit Server.Form.Main;

{
  ==============================================================================
   MCPConnect demo - Indy transport host, with the telemetry showcase
  ==============================================================================

  This form is the transport layer of the demo plus the wiring of the metrics
  showcase. Everything that makes the server an MCP server (tools, resources,
  prompts, middleware) is declared in the shared unit MCPServer.Config - this
  form then adds the metrics-specific middleware and features and starts the
  telemetry hub.

  The Indy flavour is the one to pick when you want:

    - full control over the HTTP layer (bindings, SSL/TLS, thread pool);
    - Server-Sent Events (server -> client notifications) on *any* supported
      Delphi version (the WebBroker transport can only stream on D13.1+);
    - a self-contained .exe with no web server to deploy in front of it.

  Endpoint: TMCPIndyServer answers on every path, so with the default port
  the MCP endpoint is simply

      http://localhost:8080/

  Point an MCP client at it (MCPJam Inspector, LM Studio, or Claude Desktop
  through 'npx mcp-remote http://localhost:8080/').

  The metrics showcase lives in four units next to this one:

    Server.Metrics.pas             instruments, exporters, reports
    Server.Metrics.Middleware.pas  protocol telemetry from the chain
    Server.Metrics.Features.pas    tools, resource and prompt over it
    Server.Dashboard.pas           the tabbed dashboard (a design-time frame)

  On top of the todo tools shared with the other flavours, the client also gets
  metrics_snapshot, metrics_snapshot_json, metrics_instrument, metrics_slowest,
  metrics_exporters, metrics_use_cases, metrics_simulate_workload,
  metrics_simulate_errors, metrics_reset, metrics_cardinality and
  metrics_separate_provider, plus the resource://metrics/live resource and the
  analyse-metrics prompt.
}

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ExtCtrls,

  Logify,
  Logify.Adapter.Buffer,

  MCPConnect.Configuration.MCP,
  MCPConnect.Logging.Memory,
  MCPConnect.MCP.Server,
  MCPConnect.Metrics,
  MCPConnect.Transport.Indy,

  Server.Dashboard;

type
  TfrmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    pnlBottom: TPanel;
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
    FServer: TMCPIndyServer;
    FLogifyAdapterFactory: ILoggerAdapterFactory;
    /// <summary>
    ///   Periodic delta harvest: every tick the collected counters are exported
    ///   to metrics.jsonl and metrics.log through the file sinks, and the [PERF]
    ///   measurements are written to perf.log.
    /// </summary>
    FHarvestTimer: TTimer;
    /// <summary>
    ///   The tabbed metrics screen. It is a design-time frame laid out in
    ///   Server.Dashboard.dfm, docked under the transport controls.
    /// </summary>
    FFrameDashboard: TFrameMetricsDashboard;
    /// <summary>
    ///   Where the [PERF] lines end up. The memory adapter below parses each
    ///   one into a measurement and aggregates it per key; the store survives
    ///   the memo being cleared and is what SavePerfReport prints.
    /// </summary>
    FPerfLog: TMCPMemoryLog;
    FPerfLogFactory: ILoggerAdapterFactory;
    procedure HarvestTelemetry(Sender: TObject);
    procedure SavePerfReport;
    procedure StartServer;
    procedure RegisterMetricsFeatures;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  WinApi.Windows, Winapi.ShellApi,
  System.DateUtils, System.IOUtils,

  // The shared, transport-independent server definition.
  Server.Config,
  // The metrics showcase.
  Server.Metrics,
  Server.Metrics.Middleware,
  Server.Metrics.Features;

/// <summary>
///   The [PERF] report, next to the .exe. Rewritten in full on every tick and
///   at shutdown, so it always holds the latest snapshot rather than a history.
/// </summary>
function PerfReportFileName: string;
begin
  Result := TPath.Combine(TPath.GetAppPath, 'perf.log');
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // 0) The screen. It is the design-time dashboard frame (Server.Dashboard.dfm),
  //    docked under the transport controls. It owns the log memo, so it comes up
  //    before the logging adapters that write into it.
  FFrameDashboard := TFrameMetricsDashboard.Create(Self);
  FFrameDashboard.Parent := pnlBottom;
  FFrameDashboard.Align := alClient;
  //FFrameDashboard.Height := 608;

  // Route MCPConnect's internal logging to the frame's Log tab: the buffer
  // adapter collects messages from background threads and flushes them to the
  // TStrings target (the memo's Lines) on the main thread via a timer.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(TLogLevel.Trace, FFrameDashboard.LogLines);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FLogifyAdapterFactory);

  // A second adapter on the same log, this one keeping it in memory. It is
  // registered at Debug because that is the level the [PERF] lines are logged
  // at - at Info or above nothing would be captured. The store is perf-only:
  // every other line is counted and dropped, so the memo stays the place to
  // read the log and this one only accumulates measurements.
  FPerfLog := TMCPMemoryLog.Create;
  FPerfLogFactory := TLogifyAdapterMemoryFactory.CreateAdapterFactory(
    'Perf log', TLogLevel.Debug, FPerfLog);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FPerfLogFactory);

  // 1) Build the transport.
  //    CreateMCPServer is a convenience factory: it creates the Indy server,
  //    creates and owns a TMCPServer, and wires the MCP request handler
  //    (CORS, sessions, SSE, OAuth gate) into Indy's OnCommandGet/Other.
  //    Using the plain constructor instead would leave you to do that by hand.
  FServer := TMCPIndyServer.CreateMCPServer(Self);

  // 2) Declare *what* the server exposes. Everything - name, version,
  //    capabilities, tools, resources, prompts, security - happens inside this
  //    single call. See MCPServer.Config.pas.
  TServerConfigurator.ConfigureServer(FServer.MCPServer);

  // 3) The metrics showcase: one middleware for protocol telemetry and the
  //    tools/resource/prompt that expose the numbers.
  RegisterMetricsFeatures;

  // 4) Start the hub: it attaches the seven exporters to the default provider
  //    (two files, an in-memory snapshot store, a slowest-histograms renderer,
  //    two latest-value screen views and a Logify sink) and creates the
  //    instruments. Recording then happens in the middleware and in the tools;
  //    the frame samples host gauges and refreshes the grid.
  TServerMetrics.Instance.Start(FFrameDashboard.TextTarget, FFrameDashboard.JsonTarget,
    FFrameDashboard.SlowestTarget);

  // 5) Point the screen at the running hub: static tabs, grid and timers.
  FFrameDashboard.RefreshAll;

  // 6) Open the socket.
  StartServer;

  // 7) One delta per 30 s feeds metrics.jsonl / metrics.log. The hub's
  //    Harvest(True) resets afterwards, so each line holds what happened since
  //    the previous tick - which is also what metrics_report reports.
  FHarvestTimer := TTimer.Create(Self);
  FHarvestTimer.Interval := 30000;
  FHarvestTimer.OnTimer := HarvestTelemetry;
  FHarvestTimer.Enabled := True;

  Logger.Log(Format('Metrics: delta harvest every %d ms to %s and %s', [
    FHarvestTimer.Interval,
    TPath.Combine(TPath.GetAppPath, 'metrics.jsonl'),
    TPath.Combine(TPath.GetAppPath, 'metrics.log')]), TLogLevel.Debug);
  Logger.Log(Format('Perf: [PERF] measurements captured in memory, report written to %s', [
    PerfReportFileName]), TLogLevel.Debug);
end;

procedure TfrmMain.RegisterMetricsFeatures;
begin
  // Protocol telemetry: every request is timed and labelled by method, every
  // tools/call by tool name. Added with the observability priority the class
  // declares, so it sits inside whatever the shared config registered.
  FServer.MCPServer.Middleware.Add(TMetricsMCPMiddleware);

  // The features that put the snapshot back on the wire. They are registered
  // here rather than in MCPServer.Config so the shared server definition stays
  // exactly what the WebBroker, Stdio and Windows Service flavours build.
  FServer.MCPServer.Plugin.Configure<IMCPConfig>
    .Tools
      .RegisterClass(TMetricsShowcaseTools)
    .BackToMCP
    .Resources
      .RegisterClass(TMetricsShowcaseResource)
    .BackToMCP
    .Prompts
      .RegisterClass(TMetricsShowcasePrompts)
    .BackToMCP
    .BackToApp;
end;

procedure TfrmMain.HarvestTelemetry(Sender: TObject);
begin
  // A delta per tick would still write an empty "[]" line when nothing was
  // recorded since the previous tick, so only harvest when there is data.
  if Length(TMetrics.Collect) > 0 then
    TServerMetrics.Instance.HarvestDelta;

  SavePerfReport;
end;

procedure TfrmMain.SavePerfReport;
var
  LReport: TStringBuilder;
  LSample: TMCPPerfSample;
begin
  // Nothing measured yet (no request has been served): writing now would only
  // overwrite a previous, useful snapshot with an empty one.
  if FPerfLog.PerfCount = 0 then
    Exit;

  LReport := TStringBuilder.Create;
  try
    LReport.AppendLine('MCPConnect - Indy demo');
    LReport.AppendLine('[PERF] snapshot of ' + DateToISO8601(Now, False));
    LReport.AppendLine;

    // The statistics cover every measurement since the server started: they
    // are folded in as the lines arrive and survive the sample ring rolling
    // over, which the raw list below does not.
    LReport.AppendLine(FPerfLog.PerfReport);

    LReport.AppendLine(Format('Last %d measurements, oldest first', [Length(FPerfLog.PerfSamples)]));
    LReport.AppendLine(StringOfChar('-', 107));
    for LSample in FPerfLog.PerfSamples do
      LReport.AppendLine(Format('%s [%d] %s', [
        DateToISO8601(LSample.Timestamp, False),
        UInt64(LSample.ThreadId),
        LSample.ToString]));

    try
      TFile.WriteAllText(PerfReportFileName, LReport.ToString, TEncoding.UTF8);
    except
      // This runs on a timer and again while the form is closing: a file held
      // open by an editor must not take the demo down with it.
      on E: Exception do
        Logger.LogWarning(E, 'Could not write ' + PerfReportFileName);
    end;
  finally
    LReport.Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Stop the periodic harvest and the screen timers, then detach the exporters
  // - releasing them closes metrics.jsonl and metrics.log.
  FHarvestTimer.Enabled := False;
  if Assigned(FFrameDashboard) then
    FFrameDashboard.Deactivate;
  TServerMetrics.Instance.Stop;

  // Last snapshot, covering whatever happened since the final tick, then the
  // adapter goes before the store it writes into: a background thread still
  // logging would otherwise reach a freed TMCPMemoryLog.
  SavePerfReport;
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FPerfLogFactory);
  FPerfLogFactory := nil;
  FreeAndNil(FPerfLog);

  // Unregister before the frame (and the log memo it owns) is destroyed,
  // otherwise background threads still logging would write to a freed TStrings
  // and cause an AV.
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FLogifyAdapterFactory);
  FreeAndNil(FFrameDashboard);

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
  TServerConfigurator.UnregisterFeatures(FServer.MCPServer);
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
