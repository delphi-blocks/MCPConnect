unit MetricsWeb.WebModule;

{
  ==============================================================================
   The WebBroker web module: the MCP endpoint and the WebStencils dashboard

   Two audiences, served from one process because WebBroker routes by path:

     TMCPDispatcher   POST /mcp - for the model. It exposes the application's
                      tools and nothing else: the todo list (Server.Tools.pas),
                      as a real server exposes its business operations. The
                      dispatcher registers itself with its owning module, so
                      there is no action to write.
     WebStencils      GET /dashboard, /dashboard/live, /dashboard.css and
                      /metrics - for whoever runs the server. They show those
                      tool calls as they happen, as HTML for a browser and as
                      JSON for anything else.

   The metrics are recorded by TMetricsMCPMiddleware on the MCP endpoint, so the
   tools never mention them and the model is never shown them: observing the
   server is the operator's business, not the client's.

   The dashboard is rendered server-side from the metrics of *this* process, so
   there is no HTTP hop between the page and the numbers, and nothing to
   configure for CORS. The view model is built per request and handed to a
   processor the module owns, so a page can never show a stale object.

   Templates live in the templates folder next to the executable (or wherever
   -templates points), which is also what the CSS is served from.
}

interface

uses
  System.SysUtils,
  System.Classes,

  Web.HTTPApp,
  Web.Stencils,

  MCPConnect.MCP.Server,

  MetricsWeb.History,
  MetricsWeb.View;

const
  /// <summary>The MCP endpoint, as TMCPDispatcher.PathInfo sees it.</summary>
  MCP_PATH = '/mcp';
  /// <summary>The page, also served at '/'.</summary>
  DASHBOARD_PATH = '/dashboard';
  /// <summary>The part of the page the script swaps in, without the layout.</summary>
  LIVE_PATH = '/dashboard/live';
  /// <summary>The stylesheet, served verbatim: it never goes through the template parser.</summary>
  CSS_PATH = '/dashboard.css';
  /// <summary>
  ///   Images and other binaries under the templates folder, served from
  ///   templates\assets. A wildcard action, since an asset has no name this unit
  ///   could know in advance, and a masked path rather than a file dispatcher
  ///   because it is one route and one directory.
  /// </summary>
  ASSETS_PATH = '/assets';
  /// <summary>The same document the JSON endpoint of the Indy demo serves.</summary>
  METRICS_PATH = '/metrics';

  SERVICE_NAME = 'todo-mcp-server';
  SERVICE_VERSION = '1.0.0';
  SERVICE_DESCRIPTION = 'A todo list over MCP, observed through a WebStencils dashboard';

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    /// <summary>One page or fragment, rendered from a view model built for this request.</summary>
    function Render(const ATemplateFile: string; ARequest: TWebRequest): string;
    /// <summary>
    ///   A template that cannot be processed is not fatal: WebStencils writes the
    ///   directive into the output and carries on, so the page would be the only
    ///   symptom. Log it too. A method of this module rather than of the shared
    ///   engine, because the processor it is assigned to lives exactly as long as
    ///   the module does.
    /// </summary>
    procedure EngineError(Sender: TObject; const AMessage: string);

    procedure ActionDashboard(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionLive(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionCss(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    /// <summary>Serves one file from templates\assets, by name and nothing else.</summary>
    procedure ActionAsset(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionMetrics(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionNotFound(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

  /// <summary>
  ///   The MCP server every request module shares: WebBroker builds one module
  ///   per request, so the protocol engine cannot live in one of them.
  /// </summary>
  ServerMCP: TMCPServer;

  /// <summary>The samples the dashboard's charts are drawn from, one per timer tick.</summary>
  MetricsHistory: TMetricsHistory;

  /// <summary>The template engine, shared by every processor.</summary>
  MetricsEngine: TWebStencilsEngine;

  /// <summary>Where the templates are. The form sets it before the server starts.</summary>
  TemplatesRoot: string;

  /// <summary>How often the page asks for the fragment again.</summary>
  DashboardRefreshSeconds: Integer = 5;

/// <summary>
///   The template folder: whatever the caller set, else "templates" beside the
///   executable - the convention every other demo uses for its data folder.
/// </summary>
function ResolveTemplatesRoot: string;

/// <summary>
///   Creates the process-wide MCP server and registers the todo tools on it. Called by the host form *before* the socket opens: doing it here on the
///   first request would put the one-time cost - creating the engine, scanning
///   the annotated classes, registering every type - on whichever client
///   happened to be first, and would run it beside the host's own timers.
/// </summary>
procedure ConfigureMCPServer;

implementation

uses
  System.IOUtils,
  System.Generics.Collections,

  Logify,

  MCPConnect.Configuration.MCP,
  MCPConnect.Transport.WebBroker,
  MCPConnect.Metrics,

  Server.Tools,
  Server.Metrics,
  Server.Metrics.Middleware,
  Server.Metrics.Document;

var
  FResolvedRoot: string;

// The web module's DFM, compiled into the executable. Without this line
// TWebModule1.Create raises EResNotFound ("resource TWebModule1 not found") when
// it streams its own resource, WebBroker answers nothing, and Indy fills the
// response with its default "200 OK" page.
{$R *.dfm}

// The two narrative panels. TServerMetrics has its own, but they describe the
// VCL showcase - its metrics_* tools and its Simulate button - and this demo has
// neither.

function AboutText: string;
begin
  Result :=
    'MCPServerMetricsWeb - a todo MCP server and its dashboard' + sLineBreak +
    StringOfChar('=', 74) + sLineBreak +
    sLineBreak +
    'One process, two audiences:' + sLineBreak +
    sLineBreak +
    '  POST /mcp          the model: the todo tools (add, list, complete, delete,' + sLineBreak +
    '                     summarize, import, plan) and nothing else' + sLineBreak +
    '  GET  /dashboard    the operator: this page, rendered with WebStencils' + sLineBreak +
    '  GET  /metrics      the same numbers as JSON, for a script or a scraper' + sLineBreak +
    sLineBreak +
    'Connect any MCP client to /mcp, work with the todo list, and watch the' + sLineBreak +
    'calls arrive here. The tools know nothing about metrics: a middleware on' + sLineBreak +
    'the MCP endpoint records every call.' + sLineBreak +
    sLineBreak +
    'Read Docs/metrics.md for the reference and WebStencils/README.md for how' + sLineBreak +
    'this page is rendered.';
end;

function UseCasesText: string;
begin
  Result :=
    'What the dashboard is watching' + sLineBreak +
    StringOfChar('=', 74) + sLineBreak +
    sLineBreak +
    '1. Protocol telemetry from middleware (TMetricsMCPMiddleware)' + sLineBreak +
    '   request.count / request.duration_ms / request.errors per MCP method, and' + sLineBreak +
    '   tool.count / tool.duration_ms / tool.errors per tool, recorded without a' + sLineBreak +
    '   single counter in the business classes.' + sLineBreak +
    sLineBreak +
    '2. Application metrics from the todo tools' + sLineBreak +
    '   todo.tool.calls, todo.tool.duration_ms and the todo.tasks.total gauge.' + sLineBreak +
    sLineBreak +
    '3. Host gauges sampled on a timer' + sLineBreak +
    '   uptime, CPU, handles and memory: the "how is the box" view.' + sLineBreak +
    sLineBreak +
    '4. The same numbers without MCP' + sLineBreak +
    '   GET /metrics answers an ordinary JSON document. It is outside every MCP' + sLineBreak +
    '   gate, so it is public: put it behind authentication or bind it to' + sLineBreak +
    '   loopback before deploying this anywhere real.';
end;

function ResolveTemplatesRoot: string;
begin
  if FResolvedRoot <> '' then
    Exit(FResolvedRoot);

  if TemplatesRoot <> '' then
    FResolvedRoot := IncludeTrailingPathDelimiter(TemplatesRoot)
  else
    FResolvedRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'templates');

  Result := FResolvedRoot;
end;

procedure TWebModule1.EngineError(Sender: TObject; const AMessage: string);
begin
  // A template that cannot be processed is not fatal: WebStencils writes the
  // directive into the output and carries on, so the page is the only symptom
  // unless this says so. Both sinks, because the memo is no use to whoever reads
  // the page from somewhere else.
  Logger.Log(Format('WebStencils: %s', [AMessage]), TLogLevel.Warning);
  try
    TFile.AppendAllText(TPath.Combine(ExtractFilePath(ParamStr(0)), 'web-error.log'),
      Format('[%s] WebStencils: %s%s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
        AMessage, sLineBreak]), TEncoding.UTF8);
  except
    // Diagnostics, not the application.
  end;
end;

{ TWebModule1 -------------------------------------------------------------- }

procedure ConfigureMCPServer;
begin
  if Assigned(ServerMCP) then
    Exit;

  ServerMCP := TMCPServer.Create(nil);

  ServerMCP.Middleware.Add(TMetricsMCPMiddleware);
  ServerMCP.Plugin.Configure<IMCPConfig>
    .Server
      .SetName(SERVICE_NAME)
      .SetVersion(SERVICE_VERSION)
      .SetDescription(SERVICE_DESCRIPTION)
    .BackToMCP
    .Tools
      .RegisterClass(TTodoTool)
      // The one tool on TTodoTool that reports on the server rather than on the
      // todo list. Its numbers are on /dashboard and /metrics, for the operator;
      // a model has no use for them.
      .UnregisterTool('metrics_report')
    .BackToMCP
    .Security
      .SetCORS(True)
      .SetAllowedMethods(['GET', 'POST'])
    .BackToMCP
    .BackToApp;
end;

{ TWebModule1 -------------------------------------------------------------- }

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LDispatcher: TMCPDispatcher;
  LAction: TWebActionItem;
begin
  // The engine is shared, and its root is looked up on every request so that
  // moving the templates folder does not need a restart.
  MetricsEngine.RootDirectory := ResolveTemplatesRoot;

  // --- the MCP endpoint -------------------------------------------------
  // One protocol engine for the whole process, whatever module serves the
  // request. The form has already built it; this is the safety net for a host
  // that does not (an ISAPI deployment, say).
  ConfigureMCPServer;

  LDispatcher := TMCPDispatcher.Create(Self);
  LDispatcher.PathInfo := MCP_PATH;
  LDispatcher.Server := ServerMCP;

  // --- the dashboard ----------------------------------------------------
  // The page and its fragments. The page is complete without JavaScript; the
  // script in the layout only swaps the fragment in.
  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'Dashboard';
  LAction.PathInfo := '/';
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionDashboard;

  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'DashboardPage';
  LAction.PathInfo := DASHBOARD_PATH;
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionDashboard;

  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'DashboardLive';
  LAction.PathInfo := LIVE_PATH;
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionLive;

  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'DashboardCss';
  LAction.PathInfo := CSS_PATH;
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionCss;

  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'Assets';
  LAction.PathInfo := ASSETS_PATH + '/*';
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionAsset;

  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'MetricsJson';
  LAction.PathInfo := METRICS_PATH;
  LAction.MethodType := mtGet;
  LAction.OnAction := ActionMetrics;

  // Anything else gets a page that says where to look, rather than WebBroker's
  // own "action not found" text.
  LAction := TWebActionItem(Actions.Add);
  LAction.Name := 'NotFound';
  LAction.Default := True;
  LAction.OnAction := ActionNotFound;
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  // Nothing to do: the engine, the history and the MCP server outlive the
  // per-request modules that use them, and are freed in this unit's finalization.
end;

function TWebModule1.Render(const ATemplateFile: string; ARequest: TWebRequest): string;
var
  LProcessor: TWebStencilsProcessor;
  LViewModel: TMetricsViewModel;
  LReport: TMetricsReport;
begin
  // Sample the host first: a harvest reset clears the gauges until the next
  // sample, and a page rendered in between would show none.
  TServerMetrics.Instance.SampleSystem;
  LReport := BuildMetricsReport(SERVICE_NAME, SERVICE_VERSION, SERVICE_DESCRIPTION);
  try
    LViewModel := BuildMetricsViewModel(LReport, MetricsHistory,
      TServerMetrics.Instance.ExportersReport, UseCasesText, AboutText,
      DashboardRefreshSeconds);
  finally
    LReport.Free;
  end;

  LProcessor := TWebStencilsProcessor.Create(Self);
  try
    LProcessor.Engine := MetricsEngine;
    // Fully qualified: Content uses InputFileName as it stands, and only the
    // files it imports (@Import, @LayoutPage) are resolved against the engine's
    // RootDirectory. A bare name would be looked up in the current directory.
    LProcessor.InputFileName := TPath.Combine(ResolveTemplatesRoot, ATemplateFile);
    LProcessor.WebRequest := ARequest;
    LProcessor.OnError := EngineError;
    // The processor owns the view model, and the module owns the processor, so
    // the whole tree dies when the request does.
    LProcessor.AddVar('Metrics', LViewModel, True);
    Result := LProcessor.Content;
  finally
    LProcessor.Free;
  end;
end;

procedure TWebModule1.ActionDashboard(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html; charset=utf-8';
  Response.SetCustomHeader('Cache-Control', 'no-store');
  Response.Content := Render('dashboard.html', Request);
  Handled := True;
end;

procedure TWebModule1.ActionLive(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  // The fragment: the same partials the page imports, without the layout. The
  // script in the layout swaps it into #live.
  Response.ContentType := 'text/html; charset=utf-8';
  Response.SetCustomHeader('Cache-Control', 'no-store');
  Response.Content := Render('_live.html', Request);
  Handled := True;
end;

procedure TWebModule1.ActionCss(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LFileName: string;
begin
  Handled := True;
  LFileName := TPath.Combine(ResolveTemplatesRoot, 'dashboard.css');
  if not TFile.Exists(LFileName) then
  begin
    Response.StatusCode := 404;
    Response.Content := '/* dashboard.css not found: ' + LFileName + ' */';
    Exit;
  end;

  // Served as a file, not as a template: '@media' is not a WebStencils directive
  // and must never be looked at by the parser.
  Response.ContentType := 'text/css; charset=utf-8';
  Response.Content := TFile.ReadAllText(LFileName, TEncoding.UTF8);
end;

procedure TWebModule1.ActionAsset(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LName, LFileName, LExtension: string;
begin
  Handled := True;

  // The name and nothing else. The mask matches any path under /assets, so this
  // is where a request for "/assets/../../MCPServerMetricsWeb.dproj" stops: only
  // a plain file name survives, and it is then looked up in one directory.
  LName := Request.PathInfo;
  if LName.StartsWith(ASSETS_PATH + '/', True) then
    LName := LName.Substring(Length(ASSETS_PATH) + 1);
  if (LName = '') or (LName <> TPath.GetFileName(LName)) or LName.Contains('..') then
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain; charset=utf-8';
    Response.Content := 'No such asset.';
    Exit;
  end;

  LFileName := TPath.Combine(TPath.Combine(ResolveTemplatesRoot, 'assets'), LName);
  if not TFile.Exists(LFileName) then
  begin
    Response.StatusCode := 404;
    Response.ContentType := 'text/plain; charset=utf-8';
    Response.Content := 'No such asset: ' + LName;
    Exit;
  end;

  LExtension := TPath.GetExtension(LFileName).ToLower;
  if LExtension = '.png' then
    Response.ContentType := 'image/png'
  else if LExtension = '.jpg' then
    Response.ContentType := 'image/jpeg'
  else if LExtension = '.svg' then
    Response.ContentType := 'image/svg+xml'
  else if LExtension = '.ico' then
    Response.ContentType := 'image/x-icon'
  else if LExtension = '.css' then
    Response.ContentType := 'text/css; charset=utf-8'
  else
    Response.ContentType := 'application/octet-stream';

  // Binaries go through ContentStream: Content is a UnicodeString, and reading a
  // PNG into one would re-encode every byte above 127. The response owns and
  // frees the stream.
  Response.ContentStream := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
  Response.SetCustomHeader('Cache-Control', 'public, max-age=3600');
end;

procedure TWebModule1.ActionMetrics(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LReport: TMetricsReport;
begin
  // As the page does: a poll landing just after a harvest reset still gets the
  // host gauges (the Vue dashboard polls this endpoint).
  TServerMetrics.Instance.SampleSystem;
  LReport := BuildMetricsReport(SERVICE_NAME, SERVICE_VERSION, SERVICE_DESCRIPTION);
  try
    Response.ContentType := 'application/json; charset=utf-8';
    Response.SetCustomHeader('Cache-Control', 'no-store');
    Response.Content := MetricsReportToJson(LReport);
  finally
    LReport.Free;
  end;
  Handled := True;
end;

procedure TWebModule1.ActionNotFound(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.StatusCode := 404;
  Response.ContentType := 'text/html; charset=utf-8';
  Response.Content :=
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Not found</title></head>' +
    '<body style="font:14px system-ui;background:#10141a;color:#e6ecf3;padding:2rem">' +
    '<h1>Not found</h1>' +
    '<p><code>' + Request.PathInfo + '</code> is not served here.</p>' +
    '<ul>' +
    '<li><a style="color:#4aa3ff" href="' + DASHBOARD_PATH + '">' + DASHBOARD_PATH +
    '</a> - the metrics dashboard</li>' +
    '<li><a style="color:#4aa3ff" href="' + METRICS_PATH + '">' + METRICS_PATH +
    '</a> - the same numbers as JSON</li>' +
    '<li><code>POST ' + MCP_PATH + '</code> - the MCP endpoint</li>' +
    '</ul></body></html>';
  Handled := True;
end;

initialization
  MetricsHistory := TMetricsHistory.Create(300);
  MetricsEngine := TWebStencilsEngine.Create(nil);
  ServerMCP := nil;

finalization
  ServerMCP.Free;
  MetricsEngine.Free;
  MetricsHistory.Free;

end.
