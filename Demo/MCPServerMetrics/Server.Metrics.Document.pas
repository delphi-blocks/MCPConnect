{******************************************************************************}
{                                                                              }
{  MCPConnect - IndyMetrics showcase: the metrics document                     }
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
///   The metrics of this process as one document, transport agnostic.
///
///   Two dashboards read this - the plain-HTTP endpoint (Server.Metrics.Http.pas,
///   which serves it as JSON) and the WebStencils pages (WebStencils/, which render
///   it as HTML) - so the arithmetic lives here rather than in either of them, and
///   the two cannot drift apart.
///
///   ## What the document is
///
///   BuildMetricsReport takes a snapshot of the default provider, folds it across
///   the host's resets, and derives from it the rollups a dashboard actually
///   draws: one row per instrument, one row per (instrument, label key) with its
///   values, the slowest histograms by average, and the raw point arrays - both
///   the live snapshot and the cumulative one.
///
///   ## Why there is a fold at all
///
///   A host that harvests with a reset (Harvest(True), which is what makes the
///   file exporters useful) makes the provider's counters fall back to zero on
///   every tick. Read raw, that is a sawtooth a dashboard cannot tell from a
///   gauge. TMetricsDocumentState keeps a monotonic copy alongside: it closes each
///   window exactly when the host is about to reset the provider - through an
///   exporter it registers for itself, which Harvest calls *before* the Clear -
///   and falls back to detecting a reset from the snapshot itself (a series whose
///   FirstSeen moved or whose values went backwards) for a reset nobody announced,
///   such as a tool calling TMetrics.Clear. The "cumulative" array is what a chart
///   should be drawn from; "live" is what the provider happens to hold right now.
///
///   The algorithm is the one TOtlpMetricExporter.Accumulate already uses.
/// </summary>
unit Server.Metrics.Document;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,

  MCPConnect.Metrics;

const
  /// <summary>Document version, so a consumer can tell what it is reading.</summary>
  METRICS_SCHEMA = 'mcpconnect.metrics/v1';

  /// <summary>
  ///   How often a polling client is told to come back. Nothing enforces it; it
  ///   is what the dashboards use as their default.
  /// </summary>
  SUGGESTED_POLL_MS = 2000;

  /// <summary>How many histogram series the "slowest" section carries.</summary>
  SLOWEST_TOP_N = 10;

type
  /// <summary>One instrument summed over all of its label sets.</summary>
  TMetricInstrumentRollup = record
    Meter: string;
    Name: string;
    Kind: TMetricKind;
    UnitName: string;
    Description: string;
    SeriesCount: Integer;
    Count: Double;
    Sum: Double;
    Min: Double;
    Max: Double;
    Last: Double;
  end;

  /// <summary>One label value of one instrument, aggregated over the other labels.</summary>
  TMetricLabelRollup = record
    LabelValue: string;
    SeriesCount: Integer;
    Count: Double;
    Sum: Double;
    Min: Double;
    Max: Double;
    Last: Double;
  end;

  /// <summary>
  ///   One instrument rolled up by one of its label keys - "requests by method",
  ///   "orders by region". An instrument with two label keys appears once per key,
  ///   each time aggregated over the other key, which is what makes a dashboard
  ///   able to offer a picker without knowing the labels in advance.
  /// </summary>
  TMetricGroupRollup = record
    Meter: string;
    Name: string;
    Kind: TMetricKind;
    LabelKey: string;
    Values: TArray<TMetricLabelRollup>;
  end;

  /// <summary>One histogram series, as the "slowest" list needs it.</summary>
  TMetricSlowEntry = record
    Meter: string;
    Name: string;
    Labels: string;
    Count: Double;
    Sum: Double;
    Min: Double;
    Max: Double;
    Avg: Double;
  end;

  /// <summary>
  ///   Everything a consumer of the numbers needs, already aggregated. Produced
  ///   by BuildMetricsReport; the caller owns it.
  /// </summary>
  TMetricsReport = class
  public
    Schema: string;

    ServiceName: string;
    ServiceVersion: string;
    ServiceDescription: string;
    StartedAt: TDateTime;
    GeneratedAt: TDateTime;
    UptimeSeconds: Double;

    PollCount: Int64;
    ResetsDetected: Int64;
    SuggestedIntervalMs: Integer;

    SeriesCount: Integer;
    InstrumentCount: Integer;
    Meters: TArray<string>;

    CounterSeries: Integer;
    CounterSum: Double;
    HistogramSeries: Integer;
    HistogramCount: Double;
    HistogramSum: Double;
    HistogramMax: Double;
    GaugeSeries: Integer;

    Instruments: TArray<TMetricInstrumentRollup>;
    Groups: TArray<TMetricGroupRollup>;
    Slowest: TArray<TMetricSlowEntry>;

    /// <summary>What the provider holds right now, i.e. since its last reset.</summary>
    Live: TArray<TMetricPoint>;
    /// <summary>The same series, monotonic since the document state was created.</summary>
    Cumulative: TArray<TMetricPoint>;

    /// <summary>
    ///   The rollup of one instrument, or a zeroed one when it is not there yet -
    ///   a dashboard asks for the instruments it knows by name and should not have
    ///   to guard every one of them.
    /// </summary>
    function Instrument(const AMeter, AName: string): TMetricInstrumentRollup;

    /// <summary>Every cumulative series of one instrument.</summary>
    function SeriesOf(const AMeter, AName: string): TArray<TMetricPoint>;

    /// <summary>The live series of one instrument, for "what is it right now".</summary>
    function LiveSeriesOf(const AMeter, AName: string): TArray<TMetricPoint>;
  end;

  /// <summary>
  ///   The fold behind the report's "cumulative" array: per series, everything
  ///   the provider has reported since the state was created, across the host's
  ///   resets.
  /// </summary>
  /// <remarks>
  ///   A process-wide singleton, because the middleware that serves the endpoint
  ///   is instanced per request and the numbers it exists to remember have to
  ///   outlive one.
  /// </remarks>
  TMetricsDocumentState = class
  strict private type
    /// <summary>
    ///   One series: what is left after the resets already folded in (Base), and
    ///   the latest snapshot taken (Live). The reported value is the two merged.
    /// </summary>
    TSeriesFold = record
      Base: TMetricPoint;
      HasBase: Boolean;
      Live: TMetricPoint;
      HasLive: Boolean;
    end;
  strict private
    class var FInstance: TMetricsDocumentState;
  private
    FLock: TCriticalSection;
    FSeries: TDictionary<string, TSeriesFold>;
    /// <summary>
    ///   Kept as the interface it was registered as, which is also what keeps it
    ///   alive: an exporter handed to AddExporter as a bare instance would not be
    ///   reference counted.
    /// </summary>
    FFoldExporter: IMetricExporter;
    FStartedAt: TDateTime;
    FPolls: Int64;
    FResets: Int64;

    function SeriesKey(const APoint: TMetricPoint): string;
    procedure FoldIntoBase(var AState: TSeriesFold);
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TMetricsDocumentState;
    class procedure Release;

    /// <summary>
    ///   Closes the open window: everything APoints carries becomes part of the
    ///   base and is reported from there on. Called by the fold exporter the
    ///   moment the host is about to reset the provider, which is the one
    ///   observation the reading side cannot make for itself.
    /// </summary>
    procedure CloseWindow(const APoints: TArray<TMetricPoint>);

    /// <summary>
    ///   Folds APoints into the running totals and returns the cumulative
    ///   snapshot: the same series, with the values the provider has accumulated
    ///   since the state was created. Gauges are passed through - a current value
    ///   has nothing to accumulate.
    /// </summary>
    function Snapshot(const APoints: TArray<TMetricPoint>): TArray<TMetricPoint>;

    /// <summary>Snapshots served, series-level resets folded in, and when the fold started.</summary>
    procedure Stats(out APolls, AResets: Int64; out AStartedAt: TDateTime);
  end;

/// <summary>
///   Harvests the provider and aggregates it into a report. AName, AVersion and
///   ADescription identify the server; the caller owns the result.
/// </summary>
function BuildMetricsReport(const AName, AVersion, ADescription: string): TMetricsReport;

/// <summary>
///   The JSON document the /metrics endpoint serves. Its shape is a contract:
///   the WebDashboard in this folder reads it member for member.
/// </summary>
function MetricsReportToJson(const AReport: TMetricsReport): string;

/// <summary>The lowercase name of a metric kind, as the JSON member writes it.</summary>
function MetricKindToStr(const AKind: TMetricKind): string;

/// <summary>A label set as "method=tools/call scope=...", for display.</summary>
function LabelsToString(const ALabels: TArray<TMetricLabel>): string;

/// <summary>The average of one series, 0 when nothing was recorded.</summary>
function AverageOf(const APoint: TMetricPoint): Double;

/// <summary>The same, from the two totals, for a rollup rather than one series.</summary>
function AverageOf2(const ASum, ACount: Double): Double;

implementation

uses
  System.DateUtils,
  System.Generics.Defaults,
  System.JSON,
  System.Math,

  MCPConnect.Metrics.Exporters;

var
  /// <summary>
  ///   Guards the creation of the singleton. Two request threads, or a request
  ///   thread and the host's own sampling timer, can reach Instance at the same
  ///   moment - and a bare "if not Assigned then create" would build two states,
  ///   one of which leaks while both fight over the fold.
  /// </summary>
  FInstanceLock: TCriticalSection;

{ Shared helpers ----------------------------------------------------------- }

function MetricKindToStr(const AKind: TMetricKind): string;
begin
  case AKind of
    TMetricKind.Counter: Result := 'counter';
    TMetricKind.Gauge: Result := 'gauge';
    TMetricKind.Histogram: Result := 'histogram';
  else
    Result := 'unknown';
  end;
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

function AverageOf(const APoint: TMetricPoint): Double;
begin
  if APoint.Count > 0 then
    Result := APoint.Sum / APoint.Count
  else
    Result := 0;
end;

function AverageOf2(const ASum, ACount: Double): Double;
begin
  if ACount > 0 then
    Result := ASum / ACount
  else
    Result := 0;
end;

function UtcIso(const ALocal: TDateTime): string;
begin
  Result := DateToISO8601(TTimeZone.Local.ToUniversalTime(ALocal), True);
end;

/// <summary>What folding two observations of one series into one looks like.</summary>
function MergeSeries(const ABase, ALive: TMetricPoint): TMetricPoint;
begin
  Result := ALive;
  Result.Count := ABase.Count + ALive.Count;
  Result.Sum := ABase.Sum + ALive.Sum;
  Result.Min := System.Math.Min(ABase.Min, ALive.Min);
  Result.Max := System.Math.Max(ABase.Max, ALive.Max);
  if ABase.FirstSeen < ALive.FirstSeen then
    Result.FirstSeen := ABase.FirstSeen;
end;

procedure AccumulateInstrument(var ARollup: TMetricInstrumentRollup; const APoint: TMetricPoint);
begin
  if ARollup.SeriesCount = 0 then
  begin
    ARollup.Meter := APoint.Meter;
    ARollup.Name := APoint.Name;
    ARollup.Kind := APoint.Kind;
    ARollup.UnitName := APoint.UnitName;
    ARollup.Description := APoint.Description;
    ARollup.Min := APoint.Min;
    ARollup.Max := APoint.Max;
  end
  else
  begin
    ARollup.Min := System.Math.Min(ARollup.Min, APoint.Min);
    ARollup.Max := System.Math.Max(ARollup.Max, APoint.Max);
  end;

  Inc(ARollup.SeriesCount);
  ARollup.Count := ARollup.Count + APoint.Count;
  ARollup.Sum := ARollup.Sum + APoint.Sum;
  ARollup.Last := APoint.Last;
end;

procedure AccumulateLabel(var ARollup: TMetricLabelRollup; const APoint: TMetricPoint);
begin
  if ARollup.SeriesCount = 0 then
  begin
    ARollup.Min := APoint.Min;
    ARollup.Max := APoint.Max;
  end
  else
  begin
    ARollup.Min := System.Math.Min(ARollup.Min, APoint.Min);
    ARollup.Max := System.Math.Max(ARollup.Max, APoint.Max);
  end;

  Inc(ARollup.SeriesCount);
  ARollup.Count := ARollup.Count + APoint.Count;
  ARollup.Sum := ARollup.Sum + APoint.Sum;
  ARollup.Last := APoint.Last;
end;

{ TMetricsFoldExporter ----------------------------------------------------- }

type
  /// <summary>
  ///   The harvest-side half of the fold. A host that harvests with a reset
  ///   throws away the window between two of its own harvests, and nothing the
  ///   reading side can see afterwards knows what was in it - folding on read
  ///   alone would under-count by whatever happened between the last read and
  ///   the reset.
  ///
  ///   This exporter is called by every Harvest, *before* the provider is cleared
  ///   (MCPConnect.Metrics exports first and clears after), so it can close the
  ///   window with the values that are about to be lost. Closing it also forgets
  ///   the live observation, so the reading side cannot fold the same values a
  ///   second time.
  /// </summary>
  TMetricsFoldExporter = class(TInterfacedObject, IMetricExporter, IMetricHarvestExporter)
  public
    /// <summary>
    ///   A harvest that carries no info is taken as not resetting the provider:
    ///   the window stays open and the reading side keeps reporting it.
    /// </summary>
    procedure Export(const APoints: TArray<TMetricPoint>);
    procedure ExportHarvest(const APoints: TArray<TMetricPoint>; const AInfo: TMetricHarvestInfo);
  end;

procedure TMetricsFoldExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  // No info, so nothing says the provider is about to be cleared: the window
  // stays open and Snapshot reports it as it stands.
end;

procedure TMetricsFoldExporter.ExportHarvest(const APoints: TArray<TMetricPoint>;
  const AInfo: TMetricHarvestInfo);
begin
  if AInfo.Reset then
    TMetricsDocumentState.Instance.CloseWindow(APoints);
end;

{ TMetricsDocumentState ---------------------------------------------------- }

constructor TMetricsDocumentState.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSeries := TDictionary<string, TSeriesFold>.Create;
  FStartedAt := Now;

  // Registered here rather than by the host: the fold needs the harvest boundary
  // to be announced, and the first report is the first moment the provider is
  // certainly up and being recorded into.
  FFoldExporter := TMetricsFoldExporter.Create;
  TMetrics.AddExporter(FFoldExporter);
end;

destructor TMetricsDocumentState.Destroy;
begin
  if Assigned(FFoldExporter) then
  begin
    TMetrics.RemoveExporter(FFoldExporter);
    FFoldExporter := nil;
  end;

  FSeries.Free;
  FLock.Free;
  inherited;
end;

class function TMetricsDocumentState.Instance: TMetricsDocumentState;
begin
  if Assigned(FInstance) then
    Exit(FInstance);

  FInstanceLock.Enter;
  try
    if not Assigned(FInstance) then
      FInstance := TMetricsDocumentState.Create;
  finally
    FInstanceLock.Leave;
  end;

  Result := FInstance;
end;

class procedure TMetricsDocumentState.Release;
begin
  FInstanceLock.Enter;
  try
    FreeAndNil(FInstance);
  finally
    FInstanceLock.Leave;
  end;
end;

function TMetricsDocumentState.SeriesKey(const APoint: TMetricPoint): string;
begin
  // The identity the metrics subsystem itself uses: meter, instrument and kind,
  // then the label set, which is stored sorted.
  Result := APoint.Meter + #2 + APoint.Name + #2 + IntToStr(Ord(APoint.Kind)) +
    #2 + LabelsToString(APoint.Labels);
end;

procedure TMetricsDocumentState.FoldIntoBase(var AState: TSeriesFold);
begin
  if not AState.HasLive then
    Exit;

  if AState.HasBase then
    AState.Base := MergeSeries(AState.Base, AState.Live)
  else
  begin
    AState.Base := AState.Live;
    AState.HasBase := True;
  end;
  AState.HasLive := False;
end;

procedure TMetricsDocumentState.CloseWindow(const APoints: TArray<TMetricPoint>);
var
  LPoint: TMetricPoint;
  LKey: string;
  LState: TSeriesFold;
begin
  FLock.Enter;
  try
    Inc(FResets);

    for LPoint in APoints do
    begin
      // A gauge is a current value: nothing to carry over.
      if LPoint.Kind = TMetricKind.Gauge then
        Continue;

      LKey := SeriesKey(LPoint);
      if not FSeries.TryGetValue(LKey, LState) then
        LState := Default(TSeriesFold);

      if LState.HasBase then
        LState.Base := MergeSeries(LState.Base, LPoint)
      else
      begin
        LState.Base := LPoint;
        LState.HasBase := True;
      end;

      // The provider is cleared right after this: the observation the reading
      // side held is now part of the base, and forgetting it is what keeps the
      // two from both counting the same window.
      LState.HasLive := False;
      FSeries.AddOrSetValue(LKey, LState);
    end;
  finally
    FLock.Leave;
  end;
end;

function TMetricsDocumentState.Snapshot(const APoints: TArray<TMetricPoint>): TArray<TMetricPoint>;
var
  LResult: TList<TMetricPoint>;
  LSeen: TDictionary<string, Boolean>;
  LStale: TList<string>;
  LPoint: TMetricPoint;
  LKey: string;
  LState: TSeriesFold;
  LReset: Boolean;
begin
  LResult := TList<TMetricPoint>.Create;
  LSeen := TDictionary<string, Boolean>.Create;
  try
    FLock.Enter;
    try
      Inc(FPolls);

      for LPoint in APoints do
      begin
        // A gauge is a current value: nothing to accumulate, nothing to keep.
        if LPoint.Kind = TMetricKind.Gauge then
        begin
          LResult.Add(LPoint);
          Continue;
        end;

        LKey := SeriesKey(LPoint);
        LSeen.AddOrSetValue(LKey, True);
        if not FSeries.TryGetValue(LKey, LState) then
          LState := Default(TSeriesFold);

        if LState.HasLive then
        begin
          // The provider was cleared between the two observations: its FirstSeen
          // moved, or the values went backwards. What Live held did happen, so it
          // joins the base instead of being lost.
          LReset := (LPoint.FirstSeen <> LState.Live.FirstSeen) or
            (LPoint.Count < LState.Live.Count) or
            ((LPoint.Kind = TMetricKind.Counter) and (LPoint.Sum < LState.Live.Sum));

          if LReset then
          begin
            Inc(FResets);
            FoldIntoBase(LState);
          end;
        end;

        LState.Live := LPoint;
        LState.HasLive := True;
        FSeries.AddOrSetValue(LKey, LState);

        if LState.HasBase then
          LResult.Add(MergeSeries(LState.Base, LState.Live))
        else
          LResult.Add(LState.Live);
      end;

      // The series the provider no longer reports at all. A Clear empties the
      // instruments' series dictionaries rather than zeroing them, so a series
      // that was recorded before the reset simply is not in the snapshot - it
      // would otherwise be dropped from the report and its total lost. The last
      // observation is folded into the base here and the series keeps being
      // reported from the base alone, until it is recorded into again.
      //
      // A series that was cleared *and* recorded into again between two reads
      // cannot be accounted for - nothing observed the values in between - and
      // that is the price of folding on read instead of at the harvest.
      LStale := TList<string>.Create;
      try
        for LKey in FSeries.Keys do
          if not LSeen.ContainsKey(LKey) then
            LStale.Add(LKey);

        for LKey in LStale do
        begin
          LState := FSeries[LKey];
          if LState.HasLive then
          begin
            Inc(FResets);
            FoldIntoBase(LState);
            FSeries[LKey] := LState;
          end;

          if LState.HasBase then
            LResult.Add(LState.Base);
        end;
      finally
        LStale.Free;
      end;
    finally
      FLock.Leave;
    end;

    Result := LResult.ToArray;

    // Sorted the way the provider sorts its own snapshots, so the two arrays in
    // the document line up point by point across reads.
    TArray.Sort<TMetricPoint>(Result, TComparer<TMetricPoint>.Construct(
      function(const ALeft, ARight: TMetricPoint): Integer
      begin
        Result := CompareStr(ALeft.Meter, ARight.Meter);
        if Result = 0 then
          Result := CompareStr(ALeft.Name, ARight.Name);
        if Result = 0 then
          Result := Ord(ALeft.Kind) - Ord(ARight.Kind);
        if Result = 0 then
          Result := CompareStr(LabelsToString(ALeft.Labels), LabelsToString(ARight.Labels));
      end));
  finally
    LSeen.Free;
    LResult.Free;
  end;
end;

procedure TMetricsDocumentState.Stats(out APolls, AResets: Int64; out AStartedAt: TDateTime);
begin
  FLock.Enter;
  try
    APolls := FPolls;
    AResets := FResets;
    AStartedAt := FStartedAt;
  finally
    FLock.Leave;
  end;
end;

{ TMetricsReport ----------------------------------------------------------- }

function TMetricsReport.Instrument(const AMeter, AName: string): TMetricInstrumentRollup;
var
  LItem: TMetricInstrumentRollup;
begin
  for LItem in Instruments do
    if (LItem.Meter = AMeter) and (LItem.Name = AName) then
      Exit(LItem);

  Result := Default(TMetricInstrumentRollup);
  Result.Meter := AMeter;
  Result.Name := AName;
end;

function TMetricsReport.SeriesOf(const AMeter, AName: string): TArray<TMetricPoint>;
var
  LPoint: TMetricPoint;
  LResult: TList<TMetricPoint>;
begin
  LResult := TList<TMetricPoint>.Create;
  try
    for LPoint in Cumulative do
      if (LPoint.Meter = AMeter) and (LPoint.Name = AName) then
        LResult.Add(LPoint);
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function TMetricsReport.LiveSeriesOf(const AMeter, AName: string): TArray<TMetricPoint>;
var
  LPoint: TMetricPoint;
  LResult: TList<TMetricPoint>;
begin
  LResult := TList<TMetricPoint>.Create;
  try
    for LPoint in Live do
      if (LPoint.Meter = AMeter) and (LPoint.Name = AName) then
        LResult.Add(LPoint);
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

{ BuildMetricsReport ------------------------------------------------------- }

function BuildMetricsReport(const AName, AVersion, ADescription: string): TMetricsReport;
var
  LLive, LCumulative: TArray<TMetricPoint>;
  LInstrumentRollups: TList<TMetricInstrumentRollup>;
  LGroups: TList<TMetricGroupRollup>;
  LSlow: TList<TMetricSlowEntry>;
  LInstrument: TMetricInstrumentRollup;
  LSlowEntry: TMetricSlowEntry;
  LGroup: TMetricGroupRollup;
  LByLabel: TObjectDictionary<string, TDictionary<string, TMetricLabelRollup>>;
  LValueMap: TDictionary<string, TMetricLabelRollup>;
  LLabelRollup: TMetricLabelRollup;
  LMeterSet: TDictionary<string, Boolean>;
  LLabel: TMetricLabel;
  LKey: string;
  LLabelKeys, LValueKeys, LMeterKeys: TArray<string>;
  LPolls, LResets: Int64;
  LStartedAt: TDateTime;
  I, J, K: Integer;
begin
  LLive := TMetrics.Collect;
  LCumulative := TMetricsDocumentState.Instance.Snapshot(LLive);
  TMetricsDocumentState.Instance.Stats(LPolls, LResets, LStartedAt);

  LInstrumentRollups := TList<TMetricInstrumentRollup>.Create;
  LGroups := TList<TMetricGroupRollup>.Create;
  LSlow := TList<TMetricSlowEntry>.Create;
  LMeterSet := TDictionary<string, Boolean>.Create;
  Result := TMetricsReport.Create;
  try
    // The points are sorted by meter, instrument and label set, so one pass over
    // the runs collects the per-instrument rollups, the label groups and the
    // slowest histograms together.
    I := 0;
    while I <= High(LCumulative) do
    begin
      J := I;
      while (J <= High(LCumulative)) and (LCumulative[J].Meter = LCumulative[I].Meter) and
        (LCumulative[J].Name = LCumulative[I].Name) and
        (LCumulative[J].Kind = LCumulative[I].Kind) do
        Inc(J);

      // One instrument: the points of [I, J)
      LInstrument := Default(TMetricInstrumentRollup);
      for K := I to J - 1 do
        AccumulateInstrument(LInstrument, LCumulative[K]);
      LInstrumentRollups.Add(LInstrument);
      LMeterSet.AddOrSetValue(LInstrument.Meter, True);

      case LInstrument.Kind of
        TMetricKind.Counter:
          begin
            Inc(Result.CounterSeries, LInstrument.SeriesCount);
            Result.CounterSum := Result.CounterSum + LInstrument.Sum;
          end;
        TMetricKind.Histogram:
          begin
            Inc(Result.HistogramSeries, LInstrument.SeriesCount);
            Result.HistogramCount := Result.HistogramCount + LInstrument.Count;
            Result.HistogramSum := Result.HistogramSum + LInstrument.Sum;
            Result.HistogramMax := System.Math.Max(Result.HistogramMax, LInstrument.Max);
          end;
        TMetricKind.Gauge:
          Inc(Result.GaugeSeries, LInstrument.SeriesCount);
      end;

      // Every label key this instrument uses, each of its values aggregated over
      // the other keys - what turns "by method" or "by tool" into a table.
      LByLabel := TObjectDictionary<string, TDictionary<string, TMetricLabelRollup>>.Create([doOwnsValues]);
      try
        for K := I to J - 1 do
          for LLabel in LCumulative[K].Labels do
          begin
            if not LByLabel.TryGetValue(LLabel.Key, LValueMap) then
            begin
              LValueMap := TDictionary<string, TMetricLabelRollup>.Create;
              LByLabel.Add(LLabel.Key, LValueMap);
            end;

            if not LValueMap.TryGetValue(LLabel.Value, LLabelRollup) then
              LLabelRollup := Default(TMetricLabelRollup);
            AccumulateLabel(LLabelRollup, LCumulative[K]);
            LLabelRollup.LabelValue := LLabel.Value;
            LValueMap.AddOrSetValue(LLabel.Value, LLabelRollup);
          end;

        LLabelKeys := LByLabel.Keys.ToArray;
        TArray.Sort<string>(LLabelKeys);
        for LKey in LLabelKeys do
        begin
          LValueKeys := LByLabel[LKey].Keys.ToArray;
          TArray.Sort<string>(LValueKeys);
          SetLength(LGroup.Values, Length(LValueKeys));
          for K := 0 to High(LValueKeys) do
            LGroup.Values[K] := LByLabel[LKey][LValueKeys[K]];

          LGroup.Meter := LInstrument.Meter;
          LGroup.Name := LInstrument.Name;
          LGroup.Kind := LInstrument.Kind;
          LGroup.LabelKey := LKey;
          LGroups.Add(LGroup);
        end;
      finally
        LByLabel.Free;
      end;

      // The slowest list is about latency-shaped instruments only
      if LInstrument.Kind = TMetricKind.Histogram then
        for K := I to J - 1 do
        begin
          LSlowEntry := Default(TMetricSlowEntry);
          LSlowEntry.Meter := LCumulative[K].Meter;
          LSlowEntry.Name := LCumulative[K].Name;
          LSlowEntry.Labels := LabelsToString(LCumulative[K].Labels);
          LSlowEntry.Count := LCumulative[K].Count;
          LSlowEntry.Sum := LCumulative[K].Sum;
          LSlowEntry.Min := LCumulative[K].Min;
          LSlowEntry.Max := LCumulative[K].Max;
          LSlowEntry.Avg := AverageOf(LCumulative[K]);
          LSlow.Add(LSlowEntry);
        end;

      I := J;
    end;

    LSlow.Sort(TComparer<TMetricSlowEntry>.Construct(
      function(const ALeft, ARight: TMetricSlowEntry): Integer
      begin
        Result := CompareValue(ARight.Avg, ALeft.Avg);
      end));

    Result.Schema := METRICS_SCHEMA;
    Result.ServiceName := AName;
    Result.ServiceVersion := AVersion;
    Result.ServiceDescription := ADescription;
    Result.StartedAt := LStartedAt;
    Result.GeneratedAt := Now;
    Result.UptimeSeconds := (Result.GeneratedAt - LStartedAt) * SecsPerDay;
    Result.PollCount := LPolls;
    Result.ResetsDetected := LResets;
    Result.SuggestedIntervalMs := SUGGESTED_POLL_MS;

    Result.SeriesCount := Length(LCumulative);
    Result.InstrumentCount := LInstrumentRollups.Count;

    LMeterKeys := LMeterSet.Keys.ToArray;
    TArray.Sort<string>(LMeterKeys);
    Result.Meters := LMeterKeys;

    Result.Instruments := LInstrumentRollups.ToArray;
    Result.Groups := LGroups.ToArray;

    SetLength(Result.Slowest, System.Math.Min(SLOWEST_TOP_N, LSlow.Count));
    for I := 0 to High(Result.Slowest) do
      Result.Slowest[I] := LSlow[I];

    Result.Live := LLive;
    Result.Cumulative := LCumulative;
  finally
    LSlow.Free;
    LMeterSet.Free;
    LGroups.Free;
    LInstrumentRollups.Free;
  end;
end;

{ MetricsReportToJson ------------------------------------------------------ }

function InstrumentToJson(const ARollup: TMetricInstrumentRollup): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('meter', ARollup.Meter);
  Result.AddPair('name', ARollup.Name);
  Result.AddPair('kind', MetricKindToStr(ARollup.Kind));
  Result.AddPair('unit', ARollup.UnitName);
  Result.AddPair('description', ARollup.Description);
  Result.AddPair('series', TJSONNumber.Create(ARollup.SeriesCount));
  Result.AddPair('count', TJSONNumber.Create(ARollup.Count));
  Result.AddPair('sum', TJSONNumber.Create(ARollup.Sum));
  Result.AddPair('min', TJSONNumber.Create(ARollup.Min));
  Result.AddPair('max', TJSONNumber.Create(ARollup.Max));
  Result.AddPair('avg', TJSONNumber.Create(AverageOf2(ARollup.Sum, ARollup.Count)));
  Result.AddPair('last', TJSONNumber.Create(ARollup.Last));
end;

function GroupToJson(const AGroup: TMetricGroupRollup): TJSONObject;
var
  LValues: TJSONArray;
  LItem: TMetricLabelRollup;
  LObject: TJSONObject;
begin
  LValues := TJSONArray.Create;
  for LItem in AGroup.Values do
  begin
    LObject := TJSONObject.Create;
    LObject.AddPair('value', LItem.LabelValue);
    LObject.AddPair('series', TJSONNumber.Create(LItem.SeriesCount));
    LObject.AddPair('count', TJSONNumber.Create(LItem.Count));
    LObject.AddPair('sum', TJSONNumber.Create(LItem.Sum));
    LObject.AddPair('min', TJSONNumber.Create(LItem.Min));
    LObject.AddPair('max', TJSONNumber.Create(LItem.Max));
    LObject.AddPair('avg', TJSONNumber.Create(AverageOf2(LItem.Sum, LItem.Count)));
    LObject.AddPair('last', TJSONNumber.Create(LItem.Last));
    LValues.AddElement(LObject);
  end;

  Result := TJSONObject.Create;
  Result.AddPair('meter', AGroup.Meter);
  Result.AddPair('name', AGroup.Name);
  Result.AddPair('kind', MetricKindToStr(AGroup.Kind));
  Result.AddPair('label', AGroup.LabelKey);
  Result.AddPair('values', LValues);
end;

/// <summary>A JSON array, empty rather than nil when AText is not one.</summary>
function ParseArray(const AText: string): TJSONArray;
var
  LValue: TJSONValue;
begin
  LValue := TJSONObject.ParseJSONValue(AText);
  if LValue is TJSONArray then
    Result := TJSONArray(LValue)
  else
  begin
    LValue.Free;
    Result := TJSONArray.Create;
  end;
end;

function MetricsReportToJson(const AReport: TMetricsReport): string;
var
  LRoot, LService, LPoll, LFold, LSummary, LSection: TJSONObject;
  LInstruments, LGroups, LSlowest, LMeters: TJSONArray;
  LInstrument: TMetricInstrumentRollup;
  LGroup: TMetricGroupRollup;
  LSlow: TMetricSlowEntry;
  LMeter: string;
begin
  LRoot := TJSONObject.Create;
  try
    LRoot.AddPair('schema', AReport.Schema);

    LService := TJSONObject.Create;
    LService.AddPair('name', AReport.ServiceName);
    LService.AddPair('version', AReport.ServiceVersion);
    LService.AddPair('description', AReport.ServiceDescription);
    LService.AddPair('startedAt', UtcIso(AReport.StartedAt));
    LService.AddPair('uptimeSeconds', TJSONNumber.Create(AReport.UptimeSeconds));
    LRoot.AddPair('service', LService);

    LRoot.AddPair('generatedAt', UtcIso(AReport.GeneratedAt));

    LPoll := TJSONObject.Create;
    LPoll.AddPair('count', TJSONNumber.Create(AReport.PollCount));
    LPoll.AddPair('suggestedIntervalMs', TJSONNumber.Create(AReport.SuggestedIntervalMs));
    LRoot.AddPair('poll', LPoll);

    LFold := TJSONObject.Create;
    LFold.AddPair('cumulative', TJSONBool.Create(True));
    LFold.AddPair('resetsDetected', TJSONNumber.Create(AReport.ResetsDetected));
    LFold.AddPair('note',
      'cumulative sums every observation since the endpoint started, across the ' +
      'host''s Harvest(True) resets');
    LRoot.AddPair('fold', LFold);

    LSummary := TJSONObject.Create;
    LSummary.AddPair('series', TJSONNumber.Create(AReport.SeriesCount));
    LSummary.AddPair('instruments', TJSONNumber.Create(AReport.InstrumentCount));

    LMeters := TJSONArray.Create;
    for LMeter in AReport.Meters do
      LMeters.Add(LMeter);
    LSummary.AddPair('meters', LMeters);

    LSection := TJSONObject.Create;
    LSection.AddPair('series', TJSONNumber.Create(AReport.CounterSeries));
    LSection.AddPair('sum', TJSONNumber.Create(AReport.CounterSum));
    LSummary.AddPair('counters', LSection);

    LSection := TJSONObject.Create;
    LSection.AddPair('series', TJSONNumber.Create(AReport.HistogramSeries));
    LSection.AddPair('count', TJSONNumber.Create(AReport.HistogramCount));
    LSection.AddPair('sum', TJSONNumber.Create(AReport.HistogramSum));
    LSection.AddPair('max', TJSONNumber.Create(AReport.HistogramMax));
    LSection.AddPair('avg', TJSONNumber.Create(
      AverageOf2(AReport.HistogramSum, AReport.HistogramCount)));
    LSummary.AddPair('histograms', LSection);

    LSection := TJSONObject.Create;
    LSection.AddPair('series', TJSONNumber.Create(AReport.GaugeSeries));
    LSummary.AddPair('gauges', LSection);

    LRoot.AddPair('summary', LSummary);

    LInstruments := TJSONArray.Create;
    for LInstrument in AReport.Instruments do
      LInstruments.AddElement(InstrumentToJson(LInstrument));
    LRoot.AddPair('instruments', LInstruments);

    LGroups := TJSONArray.Create;
    for LGroup in AReport.Groups do
      LGroups.AddElement(GroupToJson(LGroup));
    LRoot.AddPair('groups', LGroups);

    LSlowest := TJSONArray.Create;
    for LSlow in AReport.Slowest do
    begin
      LSection := TJSONObject.Create;
      LSection.AddPair('meter', LSlow.Meter);
      LSection.AddPair('name', LSlow.Name);
      LSection.AddPair('labels', LSlow.Labels);
      LSection.AddPair('count', TJSONNumber.Create(LSlow.Count));
      LSection.AddPair('sum', TJSONNumber.Create(LSlow.Sum));
      LSection.AddPair('min', TJSONNumber.Create(LSlow.Min));
      LSection.AddPair('max', TJSONNumber.Create(LSlow.Max));
      LSection.AddPair('avg', TJSONNumber.Create(LSlow.Avg));
      LSlowest.AddElement(LSection);
    end;
    LRoot.AddPair('slowest', LSlowest);

    // The raw points as well: everything above is a convenience, these two are
    // the data, in exactly the shape MetricsToJson documents.
    LRoot.AddPair('cumulative', ParseArray(MetricsToJson(AReport.Cumulative)));
    LRoot.AddPair('live', ParseArray(MetricsToJson(AReport.Live)));

    Result := LRoot.ToJSON;
  finally
    LRoot.Free;
  end;
end;

initialization
  FInstanceLock := TCriticalSection.Create;

finalization
  TMetricsDocumentState.Release;
  FInstanceLock.Free;

end.
