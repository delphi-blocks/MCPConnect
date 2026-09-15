{******************************************************************************}
{                                                                              }
{  MCPConnect - IndyMetrics showcase: the telemetry hub                        }
{                                                                              }
{  Copyright (c) Paolo Rossi <dev@paolorossi.net>                              }
{                Luca Minuti <code@lucaminuti.it>                              }
{  All rights reserved.                                                        }
{                                                                              }
{  https://github.com/delphi-blocks/MCPConnect                                 }
{                                                                              }
{  Licensed under the MIT license                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///   The telemetry hub of the IndyMetrics showcase.
///
///   It is the single place that knows which instruments the demo records, which
///   exporters are attached to the default provider, how a harvest is turned into
///   files and screen, and which "use cases" a visitor can trigger. Everything
///   else in the demo (the MCP middleware, the showcase tools, the dashboard)
///   talks to this class.
///
///   What it demonstrates, beyond the one-liner style of MCPConnect.Metrics:
///
///   * the three instrument kinds used for what they are meant for - a Counter
///     for totals (requests, tools, errors, series), a Gauge for "right now"
///     (requests in flight, process CPU, queue depth), a Histogram for
///     distributions (latency, order value, argument and batch sizes);
///   * labels splitting one instrument into several series (by method, by tool,
///     by region and outcome, by error kind) and the cardinality budget that
///     protects memory when a label carries an unbounded value;
///   * named meters keeping unrelated instruments apart (mcp.server,
///     demo.system, demo.workload, demo);
///   * instruments cached in fields for the hot path, and looked up through the
///     TMetrics facade everywhere else;
///   * four custom IMetricExporter implementations (a snapshot store, a Logify
///     sink, a latest-value screen exporter and a slowest-histograms exporter)
///     next to the shipped file and text/JSON exporters;
///   * delta harvesting (Harvest(True)) for the files, cumulative harvesting for
///     on-demand reports, and Collect for the live screen;
///   * a second, independent provider to show isolation and the label budget.
///
///   Threading: recording methods are called from Indy worker threads and are
///   thread safe. Harvest and reporting methods that touch a TStrings are for the
///   main thread (the demo drives them from VCL timers and from a tool that only
///   ever calls Collect).
/// </summary>
unit Server.Metrics;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.JSON,
  System.Generics.Collections, System.Generics.Defaults,

  Logify,

  JRPC.Core,

  MCPConnect.Metrics,
  MCPConnect.Metrics.Exporters;

type
  /// <summary>
  ///   Keeps the points of the most recent harvest, so the screen and the
  ///   report tools do not have to re-collect, and keeps a bounded history of
  ///   "how many series did each harvest carry". A custom exporter is just this:
  ///   an IMetricExporter that does something useful with the typed snapshot
  ///   Harvest hands it.
  /// </summary>
  TMetricSnapshotExporter = class(TInterfacedObject, IMetricExporter)
  private
    FLock: TCriticalSection;
    FPoints: TArray<TMetricPoint>;
    FHarvests: Int64;
    FLastHarvest: TDateTime;
    FHistory: TList<Integer>;
    FHistoryLimit: Integer;
  public
    constructor Create(AHistoryLimit: Integer = 60);
    destructor Destroy; override;

    procedure Export(const APoints: TArray<TMetricPoint>);

    /// <summary>Copy of the points of the last harvest, oldest series first.</summary>
    function Snapshot: TArray<TMetricPoint>;
    procedure Stats(out AHarvests: Int64; out ALastHarvest: TDateTime);
    /// <summary>The series count of the last harvests, oldest first.</summary>
    function HistoryText: string;
  end;

  /// <summary>
  ///   Writes one line per harvest through Logify. The point of this exporter is
  ///   to show that an exporter may log, and that it runs outside the provider's
  ///   lock so it could record metrics of its own (the hub records the harvest
  ///   counters just before harvesting, which is the correct place when the
  ///   harvest resets).
  /// </summary>
  TMetricLogifyExporter = class(TInterfacedObject, IMetricExporter)
  public
    procedure Export(const APoints: TArray<TMetricPoint>);
  end;

  /// <summary>
  ///   Replaces the content of a TStrings target with the latest harvest, either
  ///   as the human readable block or as the JSON document. Meant for a screen
  ///   view: because it replaces rather than appends, the target stays bounded
  ///   however often the demo harvests. The rendering itself is the shipped
  ///   MetricsToText / MetricsToJson free functions.
  /// </summary>
  TMetricLatestExporter = class(TInterfacedObject, IMetricExporter)
  private
    FTarget: TStrings;
    FAsJson: Boolean;
  public
    constructor Create(ATarget: TStrings; AAsJson: Boolean);
    procedure Export(const APoints: TArray<TMetricPoint>);

    property Target: TStrings read FTarget;
    property AsJson: Boolean read FAsJson;
  end;

  /// <summary>
  ///   Renders the slowest histograms of each harvest into a TStrings target, to
  ///   show a consumer interpreting points by kind and reading labels.
  /// </summary>
  TMetricSlowestExporter = class(TInterfacedObject, IMetricExporter)
  private
    FTarget: TStrings;
    FTopN: Integer;
  public
    constructor Create(ATarget: TStrings; ATopN: Integer = 10);
    procedure Export(const APoints: TArray<TMetricPoint>);
  end;

/// <summary>
///   Top-N histograms by average, one line each. Shared by the exporter above
///   and by the metrics_slowest tool.
/// </summary>
function MetricSlowestReport(const APoints: TArray<TMetricPoint>; ATopN: Integer): string;

/// <summary>"region=eu status=accepted", empty when unlabeled.</summary>
function LabelsToString(const ALabels: TArray<TMetricLabel>): string;

type
  /// <summary>
  ///   Process-wide hub behind the showcase. Singleton because the MCP
  ///   middleware is instanced per request and the tools are created by the
  ///   invoker: none of them can be handed the hub by construction.
  /// </summary>
  TServerMetrics = class
  public const
    METER_MCP = 'mcp.server';
    METER_SYSTEM = 'demo.system';
    METER_WORKLOAD = 'demo.workload';
    METER_DEMO = 'demo';
  strict private
    class var FInstance: TServerMetrics;
    class function GetInstance: TServerMetrics; static;
  private
    FOn: Boolean;
    FStartedAt: TDateTime;
    FInFlight: Integer;

    // Exporters attached to the default provider, and their description.
    FFileJson: IMetricExporter;
    FFileText: IMetricExporter;
    FSnapshot: TMetricSnapshotExporter;
    FLogify: IMetricExporter;
    FLatestText: IMetricExporter;
    FLatestJson: IMetricExporter;
    FSlowest: IMetricExporter;
    FOtlp: IMetricExporter;
    FExporterLines: TStringList;

    FTextTarget: TStrings;
    FJsonTarget: TStrings;
    FSlowestTarget: TStrings;

    // Meters and the instruments the hot path caches in fields.
    FMeterMcp, FMeterSystem, FMeterWorkload, FMeterDemo: IMeter;
    FReqCount: ICounter;
    FReqDuration: IHistogram;
    FReqErrors: ICounter;
    FReqInFlight: IGauge;
    FToolCount: ICounter;
    FToolDuration: IHistogram;
    FToolErrors: ICounter;
    FToolParamsBytes: IHistogram;
    FHarvests: ICounter;
    FHarvestSeries: IHistogram;
    FExportedSeries: ICounter;
    FUptime: IGauge;
    FCpuPercent: IGauge;
    FHandles: IGauge;
    FMemoryLoad: IGauge;
    FMemoryAvailable: IGauge;
    FLastCpuTicks: UInt64;
    FLastSampleTicks: UInt64;

    procedure InitialiseInstruments;
    procedure AttachExporters;
    procedure ReleaseExporters;
    constructor Create;
  public
    destructor Destroy; override;

    class function Instance: TServerMetrics;
    /// <summary>Releases the hub (called by the form on shutdown).</summary>
    class procedure Release;

    /// <summary>
    ///   Attaches the exporters and creates the instruments. ATextTarget and
    ///   AJsonTarget show the latest harvest; ASlowestTarget the slowest
    ///   histograms. All three are caller owned and must outlive the hub.
    /// </summary>
    procedure Start(const ATextTarget, AJsonTarget, ASlowestTarget: TStrings);
    procedure Stop;
    property Active: Boolean read FOn;

    { Recording: the middleware }
    procedure RequestStarted(const AMethod: string);
    procedure RequestSucceeded(const AMethod: string; AElapsedMs: Double);
    procedure RequestFailed(const AMethod: string; AElapsedMs: Double; AException: Exception);
    procedure ToolStarted(const AToolName: string; AArguments: TJSONObject);
    procedure ToolSucceeded(const AToolName: string; AElapsedMs: Double);
    procedure ToolFailed(const AToolName: string; AElapsedMs: Double; AException: Exception);

    { Recording: host metrics and the workload generator }
    procedure SampleSystem;
    procedure SimulateWorkload(AOperations: Integer; const ARegions: string);
    procedure SimulateErrors(AOperations: Integer);

    { Harvesting }
    /// <summary>Delta harvest: files get what happened since the previous one.</summary>
    procedure HarvestDelta;
    /// <summary>Cumulative harvest: everything since the last reset.</summary>
    procedure HarvestCumulative;

    { Reports, all built from Collect so they never disturb the accumulation }
    function TextReport: string;
    function JsonReport: string;
    function InstrumentReport(const AMeter, AName: string): string;
    function SlowestReport(ATopN: Integer = 10): string;
    function ExportersReport: string;
    function SnapshotSize: Integer;

    { Narrative shown on screen and returned by the use-case tool }
    function UseCasesReport: string;
    function AboutText: string;

    { The two API demos }
    function CardinalityDemo: string;
    function SeparateProviderDemo: string;
  end;

implementation

uses
  System.Math,
  System.IOUtils,
  System.DateUtils,

  Winapi.Windows,

  MCPConnect.Metrics.Exporters.Files,
  MCPConnect.Metrics.Exporters.Otlp;

/// <summary>Kernel+User time of a TFileTime as one 100 ns tick count.</summary>
function FileTimeToTicks(const ATime: TFileTime): UInt64;
begin
  Result := (UInt64(ATime.dwHighDateTime) shl 32) or UInt64(ATime.dwLowDateTime);
end;

/// <summary>Splits "eu, us, apac" into trimmed non-empty names; falls back to eu.</summary>
function SplitRegions(const ARegions: string): TArray<string>;
var
  LParts: TArray<string>;
  LPart: string;
  LCount: Integer;
begin
  Result := ['eu'];
  if ARegions.Trim.IsEmpty then
    Exit;

  LParts := ARegions.Split([',']);
  SetLength(Result, Length(LParts));
  LCount := 0;
  for LPart in LParts do
    if not LPart.Trim.IsEmpty then
    begin
      Result[LCount] := LPart.Trim;
      Inc(LCount);
    end;
  SetLength(Result, LCount);
  if Length(Result) = 0 then
    Result := ['eu'];
end;

function LabelsToString(const ALabels: TArray<TMetricLabel>): string;
var
  LLabel: TMetricLabel;
begin
  Result := '';
  for LLabel in ALabels do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + LLabel.Key + '=' + LLabel.Value;
  end;
end;

function MetricSlowestReport(const APoints: TArray<TMetricPoint>; ATopN: Integer): string;
var
  LHistograms: TList<TMetricPoint>;
  LPoint: TMetricPoint;
  LBuilder: TStringBuilder;
  I: Integer;
begin
  LHistograms := TList<TMetricPoint>.Create;
  try
    for LPoint in APoints do
      if LPoint.Kind = TMetricKind.Histogram then
        LHistograms.Add(LPoint);

    // Slowest first. TList.Sort is not stable, which is fine here: equal
    // averages may come out in any order.
    LHistograms.Sort(TComparer<TMetricPoint>.Construct(
      function(const ALeft, ARight: TMetricPoint): Integer
      var
        LLeft, LRight: Double;
      begin
        LLeft := 0;
        LRight := 0;
        if ALeft.Count > 0 then
          LLeft := ALeft.Sum / ALeft.Count;
        if ARight.Count > 0 then
          LRight := ARight.Sum / ARight.Count;
        if LLeft > LRight then
          Result := -1
        else if LLeft < LRight then
          Result := 1
        else
          Result := 0;
      end));

    LBuilder := TStringBuilder.Create;
    try
      LBuilder.AppendFormat('Slowest %d histogram series by average (of %d)',
        [ATopN, LHistograms.Count]).AppendLine;
      LBuilder.AppendLine(StringOfChar('-', 96));
      for I := 0 to LHistograms.Count - 1 do
      begin
        if I >= ATopN then
          Break;
        LPoint := LHistograms[I];
        LBuilder.AppendFormat('  %-30s %-6s avg=%9s max=%9s n=%-6d %s', [
          LPoint.Meter + '/' + LPoint.Name,
          LPoint.UnitName,
          FormatFloat('0.###', LPoint.Sum / LPoint.Count, TFormatSettings.Invariant),
          FormatFloat('0.###', LPoint.Max, TFormatSettings.Invariant),
          LPoint.Count,
          LabelsToString(LPoint.Labels)]).AppendLine;
      end;
      if LHistograms.Count = 0 then
        LBuilder.AppendLine('  (no histogram recorded yet)');
      Result := LBuilder.ToString;
    finally
      LBuilder.Free;
    end;
  finally
    LHistograms.Free;
  end;
end;

{ TMetricSnapshotExporter }

constructor TMetricSnapshotExporter.Create(AHistoryLimit: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FHistory := TList<Integer>.Create;
  FHistoryLimit := AHistoryLimit;
end;

destructor TMetricSnapshotExporter.Destroy;
begin
  FHistory.Free;
  FLock.Free;
  inherited;
end;

procedure TMetricSnapshotExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  FLock.Enter;
  try
    FPoints := APoints; // a copy of the array header; points are read only
    Inc(FHarvests);
    FLastHarvest := Now;
    FHistory.Add(Length(APoints));
    while FHistory.Count > FHistoryLimit do
      FHistory.Delete(0);
  finally
    FLock.Leave;
  end;
end;

function TMetricSnapshotExporter.Snapshot: TArray<TMetricPoint>;
begin
  FLock.Enter;
  try
    Result := FPoints;
  finally
    FLock.Leave;
  end;
end;

procedure TMetricSnapshotExporter.Stats(out AHarvests: Int64;
  out ALastHarvest: TDateTime);
begin
  FLock.Enter;
  try
    AHarvests := FHarvests;
    ALastHarvest := FLastHarvest;
  finally
    FLock.Leave;
  end;
end;

function TMetricSnapshotExporter.HistoryText: string;
var
  LBuilder: TStringBuilder;
  LValue: Integer;
  LIndex: Integer;
begin
  FLock.Enter;
  try
    LBuilder := TStringBuilder.Create;
    try
      if FHistory.Count = 0 then
        Exit('  (no harvest yet)');
      LIndex := 0;
      for LValue in FHistory do
      begin
        Inc(LIndex);
        if LIndex > 1 then
          LBuilder.Append(', ');
        LBuilder.Append(LValue);
      end;
      Result := '  ' + LBuilder.ToString;
    finally
      LBuilder.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TMetricLogifyExporter }

procedure TMetricLogifyExporter.Export(const APoints: TArray<TMetricPoint>);
var
  LCounters, LGauges, LHistograms: Integer;
  LPoint: TMetricPoint;
begin
  LCounters := 0;
  LGauges := 0;
  LHistograms := 0;
  for LPoint in APoints do
    case LPoint.Kind of
      TMetricKind.Counter: Inc(LCounters);
      TMetricKind.Gauge: Inc(LGauges);
      TMetricKind.Histogram: Inc(LHistograms);
    end;

  Logger.Log(Format(
    '[metrics] harvest: %d series (%d counters, %d gauges, %d histograms)',
    [Length(APoints), LCounters, LGauges, LHistograms]), TLogLevel.Debug);
end;

{ TMetricLatestExporter }

constructor TMetricLatestExporter.Create(ATarget: TStrings; AAsJson: Boolean);
begin
  inherited Create;
  FTarget := ATarget;
  FAsJson := AAsJson;
end;

procedure TMetricLatestExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if not Assigned(FTarget) then
    Exit;
  if FAsJson then
    FTarget.Text := MetricsToJson(APoints)
  else
    FTarget.Text := MetricsToText(APoints);
end;

{ TMetricSlowestExporter }

constructor TMetricSlowestExporter.Create(ATarget: TStrings; ATopN: Integer);
begin
  inherited Create;
  FTarget := ATarget;
  FTopN := ATopN;
end;

procedure TMetricSlowestExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if Assigned(FTarget) then
    FTarget.Text := MetricSlowestReport(APoints, FTopN);
end;

{ TIndyMetrics }

constructor TServerMetrics.Create;
begin
  inherited Create;
  FExporterLines := TStringList.Create;
end;

destructor TServerMetrics.Destroy;
begin
  Stop;
  FExporterLines.Free;
  inherited;
end;

class function TServerMetrics.GetInstance: TServerMetrics;
begin
  if FInstance = nil then
    FInstance := TServerMetrics.Create;
  Result := FInstance;
end;

class function TServerMetrics.Instance: TServerMetrics;
begin
  Result := GetInstance;
end;

class procedure TServerMetrics.Release;
begin
  FreeAndNil(FInstance);
end;

procedure TServerMetrics.InitialiseInstruments;
begin
  // --- MCP protocol telemetry: the middleware records here ------------------
  FMeterMcp := TMetrics.Meter(METER_MCP);
  FReqCount := FMeterMcp.Counter('request.count',
    'MCP requests handled, by method', 'requests');
  FReqDuration := FMeterMcp.Histogram('request.duration_ms',
    'MCP request latency, by method', 'ms');
  FReqErrors := FMeterMcp.Counter('request.errors',
    'MCP requests that ended in an error', 'errors');
  FReqInFlight := FMeterMcp.Gauge('request.in_flight',
    'Requests being served right now', 'requests');
  FToolCount := FMeterMcp.Counter('tool.count',
    'tools/call invocations, by tool', 'calls');
  FToolDuration := FMeterMcp.Histogram('tool.duration_ms',
    'Tool latency, by tool', 'ms');
  FToolErrors := FMeterMcp.Counter('tool.errors',
    'Tool calls that raised, by tool', 'errors');
  FToolParamsBytes := FMeterMcp.Histogram('tool.params_bytes',
    'Size of the JSON arguments of a tool call', 'bytes');

  // --- The subsystem observing itself ---------------------------------------
  FMeterDemo := TMetrics.Meter(METER_DEMO);
  FHarvests := FMeterDemo.Counter('metrics.harvests',
    'Harvests run by the demo', 'harvests');
  FHarvestSeries := FMeterDemo.Histogram('metrics.harvest_series',
    'Series delivered by one harvest', 'series');
  FExportedSeries := FMeterDemo.Counter('metrics.exported_series',
    'Series delivered in total', 'series');

  // --- Host gauges, sampled by the dashboard timer --------------------------
  FMeterSystem := TMetrics.Meter(METER_SYSTEM);
  FUptime := FMeterSystem.Gauge('process.uptime_s',
    'Process uptime', 's');
  FCpuPercent := FMeterSystem.Gauge('process.cpu_percent',
    'Process CPU since the previous sample', '%');
  FHandles := FMeterSystem.Gauge('process.handles',
    'Handles held by the process', 'handles');
  FMemoryLoad := FMeterSystem.Gauge('system.memory_load_percent',
    'System physical memory in use', '%');
  FMemoryAvailable := FMeterSystem.Gauge('system.memory_available_mib',
    'System physical memory available', 'MiB');

  // The workload meter is used through the facade (a meter lookup is cached);
  // naming it here only documents where those instruments live.
  FMeterWorkload := TMetrics.Meter(METER_WORKLOAD);
end;

procedure TServerMetrics.AttachExporters;
var
  LOtlp: TOtlpMetricExporter;
begin
  // A JSON-lines file next to the .exe: one delta per harvest.
  FFileJson := TMetricFileExporter.Create(
    TPath.Combine(TPath.GetAppPath, 'metrics.jsonl'));
  FExporterLines.Add('metrics.jsonl  - TMetricFileExporter, JSON array per harvest');

  // The same harvest rendered as readable blocks.
  FFileText := TMetricFileExporter.Create(
    TPath.Combine(TPath.GetAppPath, 'metrics.log'), True);
  FExporterLines.Add('metrics.log    - TMetricFileExporter, text blocks per harvest');

  // The two custom exporters that keep state the screen reads.
  FSnapshot := TMetricSnapshotExporter.Create;
  FExporterLines.Add('snapshot       - custom: last harvest, harvest count, series history');

  FSlowest := TMetricSlowestExporter.Create(FSlowestTarget, 15);
  FExporterLines.Add('slowest        - custom: slowest histogram series by average');

  // The two custom exporters that keep a screen target at the latest harvest.
  FLatestText := TMetricLatestExporter.Create(FTextTarget, False);
  FExporterLines.Add('text view      - custom: latest harvest as MetricsToText');

  FLatestJson := TMetricLatestExporter.Create(FJsonTarget, True);
  FExporterLines.Add('json view      - custom: latest harvest as MetricsToJson');

  // A sink that goes through the application's own log.
  FLogify := TMetricLogifyExporter.Create;
  FExporterLines.Add('logify         - custom: one summary line per harvest');

  // An OpenTelemetry collector, only when one is configured the OpenTelemetry
  // way (otel\docker-compose.yml starts a local one). The exporter keeps its
  // own running totals, so the delta and cumulative harvests this demo mixes
  // still reach the collector as one cumulative stream.
  if (GetEnvironmentVariable('OTEL_EXPORTER_OTLP_ENDPOINT') <> '') or
     (GetEnvironmentVariable('OTEL_EXPORTER_OTLP_METRICS_ENDPOINT') <> '') then
  begin
    try
      LOtlp := TOtlpMetricExporter.FromEnvironment;
      FOtlp := LOtlp;
      if GetEnvironmentVariable('OTEL_SERVICE_NAME') = '' then
        LOtlp.AddResourceAttribute('service.name', 'mcpconnect-metrics-demo');
      LOtlp.OnError :=
        procedure(AMessage: string)
        begin
          Logger.Log('[metrics] ' + AMessage, TLogLevel.Warning);
        end;
      FExporterLines.Add('otlp           - TOtlpMetricExporter, POST to ' + LOtlp.Endpoint);
    except
      on E: Exception do
        Logger.Log('[metrics] OTLP exporter not attached: ' + E.Message, TLogLevel.Error);
    end;
  end;

  TMetrics.AddExporter(FFileJson);
  TMetrics.AddExporter(FFileText);
  TMetrics.AddExporter(FSnapshot);
  TMetrics.AddExporter(FSlowest);
  TMetrics.AddExporter(FLatestText);
  TMetrics.AddExporter(FLatestJson);
  TMetrics.AddExporter(FLogify);
  if Assigned(FOtlp) then
    TMetrics.AddExporter(FOtlp);
end;

procedure TServerMetrics.ReleaseExporters;
begin
  if Assigned(FFileJson) then TMetrics.RemoveExporter(FFileJson);
  if Assigned(FFileText) then TMetrics.RemoveExporter(FFileText);
  if Assigned(FSnapshot) then TMetrics.RemoveExporter(FSnapshot);
  if Assigned(FSlowest) then TMetrics.RemoveExporter(FSlowest);
  if Assigned(FLatestText) then TMetrics.RemoveExporter(FLatestText);
  if Assigned(FLatestJson) then TMetrics.RemoveExporter(FLatestJson);
  if Assigned(FLogify) then TMetrics.RemoveExporter(FLogify);
  if Assigned(FOtlp) then TMetrics.RemoveExporter(FOtlp);

  FOtlp := nil;
  FFileJson := nil;
  FFileText := nil;
  FSnapshot := nil;
  FSlowest := nil;
  FLatestText := nil;
  FLatestJson := nil;
  FLogify := nil;
  FExporterLines.Clear;
end;

procedure TServerMetrics.Start(const ATextTarget, AJsonTarget, ASlowestTarget: TStrings);
begin
  if FOn then
    Exit;

  FTextTarget := ATextTarget;
  FJsonTarget := AJsonTarget;
  FSlowestTarget := ASlowestTarget;

  FStartedAt := Now;
  FInFlight := 0;
  FLastCpuTicks := 0;
  FLastSampleTicks := 0;

  InitialiseInstruments;
  AttachExporters;
  FOn := True;

  // Prime the CPU delta so the first real sample has a previous reading.
  SampleSystem;

  Logger.Log(Format('[metrics] showcase started: %d exporters, %d instruments on 4 meters',
    [FExporterLines.Count, 19]), TLogLevel.Debug);
end;

procedure TServerMetrics.Stop;
begin
  if not FOn then
    Exit;
  FOn := False;
  ReleaseExporters;
  FTextTarget := nil;
  FJsonTarget := nil;
  FSlowestTarget := nil;
  FReqCount := nil;
  FReqDuration := nil;
  FReqErrors := nil;
  FReqInFlight := nil;
  FToolCount := nil;
  FToolDuration := nil;
  FToolErrors := nil;
  FToolParamsBytes := nil;
  FHarvests := nil;
  FHarvestSeries := nil;
  FExportedSeries := nil;
  FUptime := nil;
  FCpuPercent := nil;
  FHandles := nil;
  FMemoryLoad := nil;
  FMemoryAvailable := nil;
  FMeterMcp := nil;
  FMeterSystem := nil;
  FMeterWorkload := nil;
  FMeterDemo := nil;
end;

{ Recording: MCP middleware ------------------------------------------------ }

procedure TServerMetrics.RequestStarted(const AMethod: string);
begin
  if not FOn then
    Exit;
  FReqCount.Add(1, ['method', AMethod]);
  FReqInFlight.SetValue(TInterlocked.Increment(FInFlight), ['scope', 'server']);
end;

procedure TServerMetrics.RequestSucceeded(const AMethod: string; AElapsedMs: Double);
begin
  if not FOn then
    Exit;
  FReqDuration.Observe(AElapsedMs, ['method', AMethod]);
  FReqInFlight.SetValue(TInterlocked.Decrement(FInFlight), ['scope', 'server']);
end;

procedure TServerMetrics.RequestFailed(const AMethod: string; AElapsedMs: Double;
  AException: Exception);
var
  LError: string;
begin
  if not FOn then
    Exit;
  if AException is EJRPCException then
    LError := EJRPCException(AException).Code.ToString
  else
    LError := AException.ClassName;
  FReqErrors.Add(1, ['method', AMethod, 'error', LError]);
  FReqDuration.Observe(AElapsedMs, ['method', AMethod]);
  FReqInFlight.SetValue(TInterlocked.Decrement(FInFlight), ['scope', 'server']);
end;

procedure TServerMetrics.ToolStarted(const AToolName: string; AArguments: TJSONObject);
var
  LBytes: Integer;
begin
  if not FOn then
    Exit;
  FToolCount.Add(1, ['tool', AToolName]);
  if Assigned(AArguments) then
  begin
    // The JSON text, as UTF-16 code units turned into an approximation of the
    // bytes that crossed the wire.
    LBytes := Length(AArguments.ToJSON) * SizeOf(Char);
    FToolParamsBytes.Observe(LBytes, ['tool', AToolName]);
  end;
end;

procedure TServerMetrics.ToolSucceeded(const AToolName: string; AElapsedMs: Double);
begin
  if not FOn then
    Exit;
  FToolDuration.Observe(AElapsedMs, ['tool', AToolName]);
end;

procedure TServerMetrics.ToolFailed(const AToolName: string; AElapsedMs: Double;
  AException: Exception);
begin
  if not FOn then
    Exit;
  FToolErrors.Add(1, ['tool', AToolName]);
  FToolDuration.Observe(AElapsedMs, ['tool', AToolName]);
end;

{ Recording: host metrics and workload ------------------------------------- }

procedure TServerMetrics.SampleSystem;
var
  LStatus: TMemoryStatusEx;
  LHandles: DWORD;
  LCreation, LExit, LKernel, LUser: TFileTime;
  LCpu, LWall: UInt64;
begin
  if not FOn then
    Exit;

  FUptime.SetValue((Now - FStartedAt) * SecsPerDay, ['scope', 'process']);

  if GetProcessHandleCount(GetCurrentProcess, LHandles) then
    FHandles.SetValue(LHandles, ['scope', 'process']);

  LStatus.dwLength := SizeOf(LStatus);
  if GlobalMemoryStatusEx(LStatus) then
  begin
    FMemoryLoad.SetValue(LStatus.dwMemoryLoad, ['scope', 'system']);
    FMemoryAvailable.SetValue(LStatus.ullAvailPhys / (1024 * 1024), ['scope', 'system']);
  end;

  if GetProcessTimes(GetCurrentProcess, LCreation, LExit, LKernel, LUser) then
  begin
    LCpu := FileTimeToTicks(LKernel) + FileTimeToTicks(LUser);
    LWall := GetTickCount64 - FLastSampleTicks;
    // LCpu is in 100 ns units, LWall in ms (1 ms = 10 000 * 100 ns)
    if (FLastSampleTicks > 0) and (LWall > 0) and (LCpu >= FLastCpuTicks) then
      FCpuPercent.SetValue(((LCpu - FLastCpuTicks) / (LWall * 10000)) * 100,
        ['scope', 'process']);
    FLastCpuTicks := LCpu;
    FLastSampleTicks := GetTickCount64;
  end;
end;

procedure TServerMetrics.SimulateWorkload(AOperations: Integer; const ARegions: string);
var
  LRegions: TArray<string>;
  LRegion: string;
  LOrder: Integer;
  LValue: Double;
begin
  if AOperations <= 0 then
    Exit;
  LRegions := SplitRegions(ARegions);

  for LOrder := 1 to AOperations do
  begin
    LRegion := LRegions[(LOrder - 1) mod Length(LRegions)];

    // One counter split by two labels: region and outcome.
    if (LOrder mod 11) = 0 then
      FMeterWorkload.Counter('orders.created', 'Orders created, by region and outcome', 'orders')
        .Add(1, ['region', LRegion, 'status', 'rejected'])
    else
      FMeterWorkload.Counter('orders.created', 'Orders created, by region and outcome', 'orders')
        .Add(1, ['region', LRegion, 'status', 'accepted']);

    // A value distribution: a histogram is not only for durations.
    LValue := 10 + Random(990) + Random;
    FMeterWorkload.Histogram('order.value_usd', 'Order value', 'USD')
      .Observe(LValue, ['region', LRegion]);
  end;

  // Gauges are snapshots of a current value, not totals.
  FMeterWorkload.Gauge('queue.depth', 'Orders waiting to be processed', 'orders')
    .SetValue(Random(40), ['queue', 'orders']);
  FMeterWorkload.Gauge('worker.pool_size', 'Worker threads in the pool', 'workers')
    .SetValue(8, ['pool', 'default']);

  // Two counters under the same instrument name, told apart by a label.
  FMeterWorkload.Counter('cache.events', 'Cache lookups, by outcome', 'lookups')
    .Add(AOperations * 3, ['cache', 'orders', 'result', 'hit']);
  FMeterWorkload.Counter('cache.events', 'Cache lookups, by outcome', 'lookups')
    .Add(AOperations, ['cache', 'orders', 'result', 'miss']);

  // A histogram of a size: rows per batch.
  FMeterWorkload.Histogram('batch.size', 'Rows processed per batch', 'rows')
    .Observe(AOperations);

  Logger.Log(Format('[metrics] simulated %d operations across %d region(s)',
    [AOperations, Length(LRegions)]), TLogLevel.Debug);
end;

procedure TServerMetrics.SimulateErrors(AOperations: Integer);
const
  KINDS: array [0 .. 2] of string = ('timeout', 'validation', 'upstream');
var
  LIndex: Integer;
begin
  if AOperations <= 0 then
    Exit;
  for LIndex := 1 to AOperations do
    FMeterWorkload.Counter('errors', 'Simulated errors, by kind', 'errors')
      .Add(1, ['kind', KINDS[(LIndex - 1) mod Length(KINDS)]]);
  Logger.Log(Format('[metrics] simulated %d error(s)', [AOperations]), TLogLevel.Debug);
end;

{ Harvesting --------------------------------------------------------------- }

procedure TServerMetrics.HarvestDelta;
var
  LSeries: Integer;
begin
  if not FOn then
    Exit;

  // The harvest counters are recorded *before* harvesting, so this harvest
  // carries them; Harvest(True) then resets everything, which is exactly the
  // delta semantics the files rely on. Recording them from an exporter instead
  // would work too, but the reset would immediately wipe them.
  LSeries := Length(TMetrics.Collect);
  FHarvests.Add(1);
  FHarvestSeries.Observe(LSeries);
  FExportedSeries.Add(LSeries);

  TMetrics.Harvest(True);
  Logger.Log(Format('[metrics] delta harvest: %d series before the stats, files appended',
    [LSeries]), TLogLevel.Debug);
end;

procedure TServerMetrics.HarvestCumulative;
begin
  if not FOn then
    Exit;
  TMetrics.Harvest(False);
  Logger.Log('[metrics] cumulative harvest written', TLogLevel.Debug);
end;

{ Reports ------------------------------------------------------------------ }

function TServerMetrics.TextReport: string;
begin
  Result := MetricsToText(TMetrics.Collect);
end;

function TServerMetrics.JsonReport: string;
begin
  Result := MetricsToJson(TMetrics.Collect);
end;

function TServerMetrics.InstrumentReport(const AMeter, AName: string): string;
var
  LPoints: TArray<TMetricPoint>;
  LPoint: TMetricPoint;
  LBuilder: TStringBuilder;
  LSeries: Integer;
  LFirst: Boolean;
begin
  LPoints := TMetrics.Collect;
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat('Instrument %s/%s', [AMeter, AName]).AppendLine;
    LBuilder.AppendLine(StringOfChar('-', 96));
    LSeries := 0;
    LFirst := True;
    for LPoint in LPoints do
      if (LPoint.Meter = AMeter) and (LPoint.Name = AName) then
      begin
        Inc(LSeries);
        if LFirst then
        begin
          LBuilder.AppendFormat('kind=%s  unit=%s  description="%s"',
            [MetricKindToStr(LPoint.Kind), LPoint.UnitName, LPoint.Description]).AppendLine;
          LFirst := False;
        end;
        LBuilder.Append('  ').Append(LPoint.ToString).AppendLine;
      end;

    if LSeries = 0 then
      LBuilder.AppendLine('  (no series: unknown instrument, or nothing recorded yet)')
    else
      LBuilder.AppendFormat('  %d series, held by the instrument on the default provider',
        [LSeries]).AppendLine;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TServerMetrics.SlowestReport(ATopN: Integer): string;
begin
  Result := MetricSlowestReport(TMetrics.Collect, ATopN);
end;

function TServerMetrics.ExportersReport: string;
var
  LBuilder: TStringBuilder;
  LHarvests: Int64;
  LLast: TDateTime;
  LIndex: Integer;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendLine('Exporters on the default provider, in the order Harvest calls them:');
    for LIndex := 0 to FExporterLines.Count - 1 do
      LBuilder.AppendFormat('  %d. %s', [LIndex + 1, FExporterLines[LIndex]]).AppendLine;

    LBuilder.AppendLine;
    if Assigned(FSnapshot) then
    begin
      FSnapshot.Stats(LHarvests, LLast);
      if LHarvests = 0 then
        LBuilder.AppendLine('No harvest has run yet.')
      else
      begin
        LBuilder.AppendFormat('Harvests: %d   last: %s', [LHarvests, DateTimeToStr(LLast)]).AppendLine;
        LBuilder.AppendLine;
        LBuilder.AppendLine('Series per harvest (oldest first, capped at 60):');
        LBuilder.AppendLine(FSnapshot.HistoryText);
      end;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TServerMetrics.SnapshotSize: Integer;
begin
  Result := Length(TMetrics.Collect);
end;

{ Narratives --------------------------------------------------------------- }

function TServerMetrics.UseCasesReport: string;
begin
  Result :=
    'Metrics use cases this demo wires together' + sLineBreak +
    StringOfChar('=', 74) + sLineBreak +
    sLineBreak +
    '1. Protocol telemetry from middleware (TMetricsMCPMiddleware)' + sLineBreak +
    '   Every request that runs an MCP operation goes through the universal' + sLineBreak +
    '   hook, so request.count / request.duration_ms / request.errors are' + sLineBreak +
    '   recorded once per call, labelled by method, without a single counter in' + sLineBreak +
    '   the business classes. The tool hook adds tool.count / tool.duration_ms /' + sLineBreak +
    '   tool.errors labelled by tool name and a histogram of the argument size.' + sLineBreak +
    sLineBreak +
    '2. Application metrics from the tools themselves (MCPServer.Tools.pas and' + sLineBreak +
    '   the metrics_showcase tools)' + sLineBreak +
    '   todo.tool.calls, todo.tool.duration_ms and the todo.tasks.total gauge are' + sLineBreak +
    '   recorded by the todo tools; simulate_workload records orders.created split' + sLineBreak +
    '   by region and outcome, order.value_usd, cache.events, batch.size and the' + sLineBreak +
    '   queue.depth / worker.pool_size gauges.' + sLineBreak +
    sLineBreak +
    '3. Host gauges sampled on a timer (demo.system)' + sLineBreak +
    '   process.uptime_s, process.cpu_percent, process.handles and the system' + sLineBreak +
    '   memory gauges are sampled every few seconds, the classic "how is the box"' + sLineBreak +
    '   view a Gauge exists for.' + sLineBreak +
    sLineBreak +
    '4. The subsystem observing itself (demo)' + sLineBreak +
    '   metrics.harvests, metrics.harvest_series and metrics.exported_series make' + sLineBreak +
    '   the exporter pipeline measurable, and show a delta harvest.' + sLineBreak +
    sLineBreak +
    '5. Seven exporters at once, four of them custom' + sLineBreak +
    '   two file sinks (JSON-lines and text), an in-memory snapshot store, a' + sLineBreak +
    '   slowest-histograms renderer, two latest-value screen views and a Logify' + sLineBreak +
    '   sink. Open the Exporters tab to see them and the series-per-harvest' + sLineBreak +
    '   history.' + sLineBreak +
    sLineBreak +
    '6. Exposing the numbers back over MCP' + sLineBreak +
    '   metrics_snapshot, metrics_snapshot_json, metrics_instrument, metrics_slowest,' + sLineBreak +
    '   metrics_exporters, metrics_reset and the resource://metrics/live resource' + sLineBreak +
    '   make the same snapshot readable by a model.' + sLineBreak +
    sLineBreak +
    '7. Cardinality and isolation' + sLineBreak +
    '   metrics_cardinality builds a private provider with a budget of 8 series and' + sLineBreak +
    '   shows DroppedSeries; metrics_separate_provider proves two providers never mix' + sLineBreak +
    '   even when instrument names collide.';
end;

function TServerMetrics.AboutText: string;
begin
  Result :=
    'IndyMetrics - the MCPConnect telemetry showcase' + sLineBreak +
    StringOfChar('=', 74) + sLineBreak +
    sLineBreak +
    'This demo is the Indy flavour of the MCP server demo, extended into a tour of' + sLineBreak +
    'MCPConnect.Metrics. The server itself is the shared one (MCPServer.Config);' + sLineBreak +
    'everything metrics-specific lives in:' + sLineBreak +
    sLineBreak +
    '  MCPServerIndy.Metrics.pas            instruments, exporters, reports, demos' + sLineBreak +
    '  MCPServerIndy.Metrics.Middleware.pas protocol telemetry as middleware' + sLineBreak +
    '  MCPServerIndy.Metrics.Features.pas   MCP tools/resource/prompt over the data' + sLineBreak +
    '  MCPServerIndy.Dashboard.pas          the tabs and the live grid' + sLineBreak +
    sLineBreak +
    'Start the server, call the todo tools from any MCP client (or press Simulate' + sLineBreak +
    'workload) and watch the Metrics tab. metrics.jsonl and metrics.log next to the' + sLineBreak +
    '.exe hold the same harvests for a collector to pick up.' + sLineBreak +
    sLineBreak +
    'Read Docs/metrics.md for the reference and Demo/MCPServer/IndyMetrics/README.md' + sLineBreak +
    'for the guided tour.';
end;

{ API demonstrations ------------------------------------------------------- }

function TServerMetrics.CardinalityDemo: string;
const
  BUDGET = 8;
var
  LProvider: IMeterProvider;
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
  LIndex: Integer;
begin
  // A private provider, so the tiny budget here cannot affect the demo's own
  // instruments on the default provider. The budget is captured by a meter when
  // it is created, so it is set before GetMeter.
  LProvider := TMetrics.CreateProvider;
  LProvider.MaxSeriesPerInstrument := BUDGET;
  LCounter := LProvider.GetMeter('demo.cardinality')
    .Counter('events', 'Events by simulated tenant', 'events');

  for LIndex := 1 to 50 do
    LCounter.Add(1, ['tenant', 'tenant-' + LIndex.ToString]);

  LPoints := LCounter.Collect;
  Result :=
    'Cardinality: a private provider with a deliberately tiny label budget.' + sLineBreak +
    StringOfChar('-', 74) + sLineBreak +
    Format('  MaxSeriesPerInstrument : %d', [BUDGET]) + sLineBreak +
    '  recordings             : 50, each with a distinct tenant label' + sLineBreak +
    Format('  series kept            : %d', [Length(LPoints)]) + sLineBreak +
    Format('  series dropped         : %d', [LCounter.DroppedSeries]) + sLineBreak +
    sLineBreak +
    'The dropped samples are gone - only the counter remembers them - which is why' + sLineBreak +
    'DroppedSeries > 0 means the snapshot is incomplete. Keep labels bounded (a' + sLineBreak +
    'status, a method, a tool) and never put an unbounded id in one.';
end;

function TServerMetrics.SeparateProviderDemo: string;
var
  LProvider: IMeterProvider;
  LPrivatePoints, LCollisions: Integer;
  LPoint: TMetricPoint;
begin
  LProvider := TMetrics.CreateProvider;
  // Deliberately the same meter and instrument name the demo itself uses.
  LProvider.GetMeter(METER_MCP)
    .Counter('request.count', 'MCP requests handled, by method', 'requests')
    .Add(999, ['method', 'tools/call']);

  LPrivatePoints := Length(LProvider.Collect);

  LCollisions := 0;
  for LPoint in TMetrics.Collect do
    if (LPoint.Meter = METER_MCP) and (LPoint.Name = 'request.count') then
    begin
      // Whatever the default provider holds, it cannot hold the 999 above.
      if LPoint.Sum >= 999 then
        LCollisions := 1;
    end;

  Result :=
    'Isolation: TMetrics.CreateProvider returns a provider of its own.' + sLineBreak +
    StringOfChar('-', 74) + sLineBreak +
    Format('  series in the private provider           : %d', [LPrivatePoints]) + sLineBreak +
    Format('  of its 999 adds visible on the default   : %s',
      [BoolToStr(LCollisions = 1, True)]) + sLineBreak +
    sLineBreak +
    'Instruments of two providers never mix, whatever their names; only the' + sLineBreak +
    'exporters registered on each provider ever see its points. The facade (TMetrics)' + sLineBreak +
    'is just a shortcut to one particular provider.';
end;

initialization

finalization
  TServerMetrics.Release;

end.
