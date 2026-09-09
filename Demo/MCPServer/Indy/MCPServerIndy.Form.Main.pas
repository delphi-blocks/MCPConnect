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

  Endpoint: TMCPIndyServer answers on every path, so with the default port
  the MCP endpoint is simply

      http://localhost:8080/

  Point an MCP client at it (MCPJam Inspector, LM Studio, or Claude Desktop
  through `npx mcp-remote http://localhost:8080/`).
}

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ExtCtrls,

  Logify,
  Logify.Adapter.Buffer,

  MCPConnect.Logging.Memory,
  MCPConnect.MCP.Server,
  MCPConnect.Metrics,
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
    FServer: TMCPIndyServer;
    FLogifyAdapterFactory: ILoggerAdapterFactory;
    /// <summary>
    ///   Periodic harvester of the demo telemetry: every tick the collected
    ///   counters are exported to metrics.jsonl (delta per tick) through the
    ///   file sink, and the [PERF] measurements are written to perf.log.
    /// </summary>
    FTimer: TTimer;
    /// <summary>The TMetricFileExporter writing metrics.jsonl next to the .exe.</summary>
    FMetricsExporter: IMetricExporter;
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
  MCPServer.Config,
  MCPConnect.Metrics.Exporters.Files;

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
  // Route MCPConnect's internal logging to the memo: the buffer adapter
  // collects messages from background threads and flushes them to the
  // TStrings target (memoLog.Lines) on the main thread via a timer.
  FLogifyAdapterFactory := TLogifyAdapterBufferFactory.CreateAdapterFactory(TLogLevel.Trace, memoLog.Lines);
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
  //    capabilities, tools, resources, prompts, sessions, security - happens
  //    inside this single call. See MCPServer.Config.pas.
  TServerConfigurator.ConfigureServer(FServer.MCPServer);

  // 3) Open the socket.
  StartServer;

  // 4) Metrics: append the collected measurements to a JSON-lines file next
  //    to the .exe and export them as deltas every 30 seconds. Recording
  //    happens in the demo tools (MCPServer.Tools.pas), so nothing is written
  //    until a tool is called; the same default provider backs the
  //    metrics_report tool, which therefore reports what happened since the
  //    last tick.
  FMetricsExporter := TMetricFileExporter.Create(
    TPath.Combine(TPath.GetAppPath, 'metrics.jsonl'));
  TMetrics.AddExporter(FMetricsExporter);

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30000;
  FTimer.OnTimer := HarvestTelemetry;
  FTimer.Enabled := True;

  Logger.Log(Format('Metrics: exporting a delta every %d ms to %s', [
    FTimer.Interval, TPath.Combine(TPath.GetAppPath, 'metrics.jsonl')]), TLogLevel.Debug);
  Logger.Log(Format('Perf: [PERF] measurements captured in memory, report written to %s', [
    PerfReportFileName]), TLogLevel.Debug);
end;

procedure TfrmMain.HarvestTelemetry(Sender: TObject);
begin
  // A delta per tick would still write an empty "[]" line when nothing was
  // recorded since the previous tick, so only harvest when there is data.
  if Length(TMetrics.Collect) > 0 then
    TMetrics.Harvest(True);

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
  // Stop the periodic harvest and close metrics.jsonl (releasing the exporter
  // closes the file it opened).
  FTimer.Enabled := False;
  if Assigned(FMetricsExporter) then
  begin
    TMetrics.RemoveExporter(FMetricsExporter);
    FMetricsExporter := nil;
  end;

  // Last snapshot, covering whatever happened since the final tick, then the
  // adapter goes before the store it writes into: a background thread still
  // logging would otherwise reach a freed TMCPMemoryLog.
  SavePerfReport;
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FPerfLogFactory);
  FPerfLogFactory := nil;
  FreeAndNil(FPerfLog);

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
