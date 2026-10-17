unit MetricsWeb.View;

{
  ==============================================================================
   The view model the WebStencils templates read

   WebStencils resolves '@Metrics.Something' by RTTI against the object handed to
   it, so everything the page needs is a plain Delphi object with public
   properties - and no formatting happens in the template. The numbers come from
   Server.Metrics.Document (the same aggregation the JSON endpoint serves) and
   from MetricsWeb.History (the samples this process keeps for the charts), so
   the templates are nothing but @if, @foreach and property reads.

   Lists are System.Classes.TObjectList, one of the containers WebStencils
   iterates natively (Web.Stencils.pas:3995). A list owns its items and the view
   model owns the lists, so dropping the view model frees the whole tree.
}

interface

uses
  System.SysUtils,
  System.Classes,
  // The non-generic TObjectList: it is the container WebStencils iterates
  // natively, and it lives here rather than in System.Classes.
  System.Contnrs,
  System.Generics.Collections,

  MCPConnect.Metrics,

  Server.Metrics.Document,
  MetricsWeb.History;

{ Formatting helpers ------------------------------------------------------- }

/// <summary>A count: thousands separators, no decimals.</summary>
function FmtCount(const AValue: Double): string;

/// <summary>A measurement: enough precision to be useful, never a wall of digits.</summary>
function FmtValue(const AValue: Double): string;

/// <summary>A per-second rate, which is almost always small.</summary>
function FmtRate(const AValue: Double): string;

/// <summary>Uptime as "2h 04m 11s".</summary>
function FmtUptime(const ASeconds: Double): string;

/// <summary>A wall-clock time, for "generated at".</summary>
function FmtClock(const ATime: TDateTime): string;

type
  /// <summary>One card in the KPI row.</summary>
  TKpiView = class
  private
    FLabelText: string;
    FValueText: string;
    FUnitText: string;
    FHint: string;
    FTone: string;
  public
    constructor Create(const ALabelText, AValueText, AUnitText, AHint: string;
      const ATone: string = '');
    property LabelText: string read FLabelText;
    property ValueText: string read FValueText;
    property UnitText: string read FUnitText;
    property Hint: string read FHint;
    /// <summary>'' or 'warn' or 'bad' - what the card does to its value colour.</summary>
    property Tone: string read FTone;
  end;

  /// <summary>One polyline of one chart, in a 0..100 by 0..34 view box.</summary>
  TChartSeriesView = class
  private
    FName: string;
    FColour: string;
    FPoints: string;
    FAreaPoints: string;
    FLastText: string;
    FMaxText: string;
  public
    constructor Create(const AName, AColour: string);
    property Name: string read FName;
    property Colour: string read FColour;
    property Points: string read FPoints write FPoints;
    property AreaPoints: string read FAreaPoints write FAreaPoints;
    property LastText: string read FLastText write FLastText;
    property MaxText: string read FMaxText write FMaxText;
  end;

  /// <summary>A chart: zero or more series over the same window.</summary>
  TChartView = class
  private
    FSeries: TObjectList;
    FCaption: string;
    FEmptyText: string;
    FWindowText: string;
  public
    constructor Create(const ACaption, AEmptyText: string);
    destructor Destroy; override;
    /// <summary>True when at least one series has something to draw.</summary>
    function GetHasData: Boolean;
    property Caption: string read FCaption;
    property EmptyText: string read FEmptyText;
    property WindowText: string read FWindowText write FWindowText;
    property Series: TObjectList read FSeries;
    property HasData: Boolean read GetHasData;
  end;

  /// <summary>One row of the instruments table.</summary>
  TInstrumentView = class
  private
    FMeter: string;
    FName: string;
    FKind: string;
    FUnitText: string;
    FDescription: string;
    FSeriesText: string;
    FCountText: string;
    FSumText: string;
    FAvgText: string;
    FMaxText: string;
    FLastText: string;
  public
    constructor Create(const ARollup: TMetricInstrumentRollup);
    property Meter: string read FMeter;
    property Name: string read FName;
    property Kind: string read FKind;
    property UnitText: string read FUnitText;
    property Description: string read FDescription;
    property SeriesText: string read FSeriesText;
    property CountText: string read FCountText;
    property SumText: string read FSumText;
    property AvgText: string read FAvgText;
    property MaxText: string read FMaxText;
    property LastText: string read FLastText;
  end;

  /// <summary>One label value inside a breakdown card.</summary>
  TLabelValueView = class
  private
    FValue: string;
    FCountText: string;
    FSumText: string;
    FAvgText: string;
    FMaxText: string;
    FBarPercent: string;
  public
    constructor Create(const ARollup: TMetricLabelRollup; const ABarPercent: Integer);
    property Value: string read FValue;
    property CountText: string read FCountText;
    property SumText: string read FSumText;
    property AvgText: string read FAvgText;
    property MaxText: string read FMaxText;
    /// <summary>Width of the CSS bar, 0..100, relative to the largest value in the group.</summary>
    property BarPercent: string read FBarPercent;
  end;

  /// <summary>One instrument rolled up by one label key.</summary>
  TGroupView = class
  private
    FMeter: string;
    FName: string;
    FKind: string;
    FLabelKey: string;
    FTitle: string;
    FMeasure: string;
    FValues: TObjectList;
  public
    constructor Create(const AGroup: TMetricGroupRollup);
    destructor Destroy; override;
    property Meter: string read FMeter;
    property Name: string read FName;
    property Kind: string read FKind;
    property LabelKey: string read FLabelKey;
    property Title: string read FTitle;
    /// <summary>'total' or 'average' - which of the two the bar is drawn from.</summary>
    property Measure: string read FMeasure;
    property Values: TObjectList read FValues;
  end;

  /// <summary>
  ///   One gauge, with the shape of the window this process kept, drawn as a dial:
  ///   a half circle in a 0..120 by 0..70 view box whose arc is the value and
  ///   whose thin outer arc is the range the window covered.
  /// </summary>
  TGaugeView = class
  private
    FMeter: string;
    FName: string;
    FUnitText: string;
    FValueText: string;
    FRangeText: string;
    FSparkPoints: string;
    FTrend: string;
    FHasSpark: Boolean;
    FDialPath: string;
    FRangePath: string;
    FScaleMaxText: string;
    FTone: string;
  public
    constructor Create(const AInstrument: TMetricInstrumentRollup; const AValues: TArray<Double>);
    property Meter: string read FMeter;
    property Name: string read FName;
    property UnitText: string read FUnitText;
    property ValueText: string read FValueText;
    property RangeText: string read FRangeText;
    property SparkPoints: string read FSparkPoints;

    /// <summary>
    ///   'up', 'down' or 'flat' - what the sparkline did over the window.
    /// </summary>
    property Trend: string read FTrend;

    property HasSpark: Boolean read FHasSpark;

    /// <summary>
    ///   The SVG path of the value arc, '' for a value at the bottom of the scale.
    /// </summary>
    property DialPath: string read FDialPath;

    /// <summary>
    ///   The SVG path of the window's min..max arc, '' with no history.
    /// </summary>
    property RangePath: string read FRangePath;

    /// <summary>
    ///   The top of the dial's scale: 100 for a percentage, otherwise a round
    ///   number above the largest value the window saw.
    /// </summary>
    property ScaleMaxText: string read FScaleMaxText;

    /// <summary>
    ///   '', 'warn' or 'bad' - set only for a percentage, the one scale whose
    ///   top means "full".
    /// </summary>
    property Tone: string read FTone;
  end;

  /// <summary>One label value's share of a counter, a segment of the split bar.</summary>
  TSplitView = class
  private
    FValue: string;
    FTotalText: string;
    FPercent: string;
    FPercentText: string;
    FColour: string;
  public
    constructor Create(const AValue, ATotalText: string; const APercent: Double;
      const AColour: string);
    property Value: string read FValue;
    property TotalText: string read FTotalText;

    /// <summary>
    ///   The segment's width, 0..100 with a '.' decimal, for a CSS width.
    /// </summary>
    property Percent: string read FPercent;

    property PercentText: string read FPercentText;
    property Colour: string read FColour;
  end;

  /// <summary>
  ///   One counter: its total, its rate over the window and how its first label
  ///   key splits it.
  /// </summary>
  TCounterView = class
  private
    FMeter: string;
    FName: string;
    FUnitText: string;
    FDescription: string;
    FTotalText: string;
    FRateText: string;
    FPeakRateText: string;
    FSeriesText: string;
    FSparkPoints: string;
    FSparkArea: string;
    FSplitKey: string;
    FSplits: TObjectList;
    function GetHasSpark: Boolean;
    function GetHasSplits: Boolean;
  public
    constructor Create(const AInstrument: TMetricInstrumentRollup;
      const ATotals: TArray<Double>; const ATimes: TArray<TDateTime>;
      const AGroups: TArray<TMetricGroupRollup>);
    destructor Destroy; override;
    property Meter: string read FMeter;
    property Name: string read FName;
    property UnitText: string read FUnitText;
    property Description: string read FDescription;
    property TotalText: string read FTotalText;

    /// <summary>
    ///   The rate over the latest sample interval, per second.
    /// </summary>
    property RateText: string read FRateText;

    property PeakRateText: string read FPeakRateText;
    property SeriesText: string read FSeriesText;
    property SparkPoints: string read FSparkPoints;
    property SparkArea: string read FSparkArea;
    property HasSpark: Boolean read GetHasSpark;

    /// <summary>
    ///   The label key the split bar is drawn by, '' when the counter has none.
    /// </summary>
    property SplitKey: string read FSplitKey;

    property Splits: TObjectList read FSplits;
    property HasSplits: Boolean read GetHasSplits;
  end;

  /// <summary>
  ///   One histogram: what it has seen since the start, the min..max band with
  ///   the average marked on it, and its average interval by interval.
  /// </summary>
  /// <remarks>
  ///   A histogram here keeps count, sum, min and max - no buckets - so the band
  ///   is the honest picture of its distribution: where it starts, where it ends
  ///   and where the mean falls between.
  /// </remarks>
  THistogramView = class
  private
    FMeter: string;
    FName: string;
    FUnitText: string;
    FDescription: string;
    FCountText: string;
    FAvgText: string;
    FMinText: string;
    FMaxText: string;
    FWindowText: string;
    FBandLeft: string;
    FBandWidth: string;
    FAvgLeft: string;
    FHasBand: Boolean;
    FSparkPoints: string;
    FSparkArea: string;
    function GetHasSpark: Boolean;
  public
    constructor Create(const AInstrument: TMetricInstrumentRollup;
      const ASums, ACounts: TArray<Double>);
    property Meter: string read FMeter;
    property Name: string read FName;
    property UnitText: string read FUnitText;
    property Description: string read FDescription;
    property CountText: string read FCountText;
    property AvgText: string read FAvgText;
    property MinText: string read FMinText;
    property MaxText: string read FMaxText;

    /// <summary>
    ///   The average and count of what was observed inside the window alone.
    /// </summary>
    property WindowText: string read FWindowText;

    /// <summary>
    ///   Where the min..max band starts, 0..100 of the scale (which ends at max).
    /// </summary>
    property BandLeft: string read FBandLeft;

    property BandWidth: string read FBandWidth;

    /// <summary>
    ///   Where the average marker sits, 0..100 of the scale.
    /// </summary>
    property AvgLeft: string read FAvgLeft;

    property HasBand: Boolean read FHasBand;
    property SparkPoints: string read FSparkPoints;
    property SparkArea: string read FSparkArea;
    property HasSpark: Boolean read GetHasSpark;
  end;

  /// <summary>One row of the slowest-histograms table.</summary>
  TSlowView = class
  private
    FRank: Integer;
    FMeter: string;
    FName: string;
    FLabels: string;
    FCountText: string;
    FAvgText: string;
    FMaxText: string;
    FMinText: string;
  public
    constructor Create(const ARank: Integer; const AEntry: TMetricSlowEntry);
    property Rank: Integer read FRank;
    property Meter: string read FMeter;
    property Name: string read FName;
    property Labels: string read FLabels;
    property CountText: string read FCountText;
    property AvgText: string read FAvgText;
    property MaxText: string read FMaxText;
    property MinText: string read FMinText;
  end;

  /// <summary>Everything the page reads, under one name: @Metrics.</summary>
  /// <remarks>
  ///   Every member the templates touch is a *property*, not a field: WebStencils
  ///   resolves '@Metrics.X' against the object's properties by RTTI, public
  ///   fields included. A field would silently render as its own name.
  /// </remarks>
  TMetricsViewModel = class
  private
    FKpis: TObjectList;
    FCharts: TObjectList;
    FInstruments: TObjectList;
    FGroups: TObjectList;
    FGauges: TObjectList;
    FSlowest: TObjectList;
    FCounters: TObjectList;
    FHistograms: TObjectList;

    FServiceName: string;
    FServiceVersion: string;
    FServiceDescription: string;
    FHasServiceDescription: Boolean;
    FSchema: string;
    FStartedText: string;
    FGeneratedText: string;
    FUptimeText: string;
    FPollsText: string;
    FResetsText: string;
    FSeriesText: string;
    FInstrumentsText: string;
    FMetersText: string;
    FMeterList: string;
    FSampleCountText: string;
    FHistorySpanText: string;
    FThroughput: TChartView;
    FLatency: TChartView;
    FExportersText: string;
    FUseCasesText: string;
    FAboutText: string;
    FHasExporters: Boolean;
    FHasUseCases: Boolean;
    FHasAbout: Boolean;
    FRefreshSeconds: Integer;

    function GetHasInstruments: Boolean;
    function GetHasGroups: Boolean;
    function GetHasGauges: Boolean;
    function GetHasSlowest: Boolean;
    function GetHasCounters: Boolean;
    function GetHasHistograms: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // identity
    property ServiceName: string read FServiceName write FServiceName;
    property ServiceVersion: string read FServiceVersion write FServiceVersion;
    /// <summary>The server's own description, as the MCP configuration states it.</summary>
    property ServiceDescription: string read FServiceDescription write FServiceDescription;
    property HasServiceDescription: Boolean read FHasServiceDescription write FHasServiceDescription;
    property Schema: string read FSchema write FSchema;
    property StartedText: string read FStartedText write FStartedText;
    property GeneratedText: string read FGeneratedText write FGeneratedText;
    property UptimeText: string read FUptimeText write FUptimeText;
    property PollsText: string read FPollsText write FPollsText;
    property ResetsText: string read FResetsText write FResetsText;
    property SeriesText: string read FSeriesText write FSeriesText;
    property InstrumentsText: string read FInstrumentsText write FInstrumentsText;
    property MetersText: string read FMetersText write FMetersText;
    property MeterList: string read FMeterList write FMeterList;
    property SampleCountText: string read FSampleCountText write FSampleCountText;
    property HistorySpanText: string read FHistorySpanText write FHistorySpanText;

    // charts
    property Throughput: TChartView read FThroughput write FThroughput;
    property Latency: TChartView read FLatency write FLatency;

    // the in-process reports a server-rendered page can afford to show
    property ExportersText: string read FExportersText write FExportersText;
    property UseCasesText: string read FUseCasesText write FUseCasesText;
    /// <summary>What the hub says it is - the About tab of the VCL dashboard.</summary>
    property AboutText: string read FAboutText write FAboutText;
    property HasExporters: Boolean read FHasExporters write FHasExporters;
    property HasUseCases: Boolean read FHasUseCases write FHasUseCases;
    property HasAbout: Boolean read FHasAbout write FHasAbout;

    /// <summary>How often the page's script asks for the fragment again.</summary>
    property RefreshSeconds: Integer read FRefreshSeconds write FRefreshSeconds;

    property Kpis: TObjectList read FKpis;
    /// <summary>The charts, in the order the page shows them.</summary>
    property Charts: TObjectList read FCharts;
    property Instruments: TObjectList read FInstruments;
    property Groups: TObjectList read FGroups;
    property Gauges: TObjectList read FGauges;
    property Slowest: TObjectList read FSlowest;

    /// <summary>
    ///   One card per counter instrument.
    /// </summary>
    property Counters: TObjectList read FCounters;

    /// <summary>
    ///   One card per histogram instrument.
    /// </summary>
    property Histograms: TObjectList read FHistograms;

    /// <summary>What an @if in the template needs, so it never loops over nothing.</summary>
    property HasInstruments: Boolean read GetHasInstruments;
    property HasGroups: Boolean read GetHasGroups;
    property HasGauges: Boolean read GetHasGauges;
    property HasSlowest: Boolean read GetHasSlowest;
    property HasCounters: Boolean read GetHasCounters;
    property HasHistograms: Boolean read GetHasHistograms;
  end;

/// <summary>
///   Builds the view model for one page render. The caller owns the result.
/// </summary>
function BuildMetricsViewModel(const AReport: TMetricsReport; const AHistory: TMetricsHistory;
  const AExportersText, AUseCasesText, AAboutText: string;
  const ARefreshSeconds: Integer): TMetricsViewModel;

implementation

uses
  System.DateUtils,
  System.Math,
  System.StrUtils,
  System.Generics.Defaults;

const
  METER_MCP = 'mcp.server';

  /// <summary>The SVG view box every sparkline and chart is drawn in.</summary>
  CHART_WIDTH = 100;
  CHART_HEIGHT = 34;

  /// <summary>
  ///   The separator between values in a hint, written as a code point and not as
  ///   the character itself.
  /// </summary>
  /// <remarks>
  ///   Delphi source files here carry no BOM, and a BOM-less file is read by the
  ///   compiler with the ANSI code page: a literal middle dot (U+00B7, two bytes
  ///   in UTF-8) would compile to *two* characters - a capital A with a
  ///   circumflex followed by the dot - and the page, which is served as UTF-8,
  ///   would then show that pair verbatim. The templates are unaffected, being
  ///   read as UTF-8 files, so the fault appears only in strings built here -
  ///   which is exactly how it first showed up. Logify guards its own literals
  ///   the same way; see CAUSE_BRANCH in Logify.pas.
  /// </remarks>
  SEPARATOR = ' ' + #$00B7 + ' ';

var
  /// <summary>
  ///   Invariant display formats: the numbers should not change shape with the
  ///   locale of whatever machine happens to run the server.
  /// </summary>
  DisplayFormat: TFormatSettings;

{ Formatting helpers ------------------------------------------------------- }

function FmtCount(const AValue: Double): string;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit('-');
  Result := FormatFloat('#,##0', AValue, DisplayFormat);
end;

function FmtValue(const AValue: Double): string;
var
  LMagnitude: Double;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit('-');
  if AValue = 0 then
    Exit('0');

  LMagnitude := Abs(AValue);
  if LMagnitude >= 1000 then
    Result := FormatFloat('#,##0', AValue, DisplayFormat)
  else if LMagnitude >= 10 then
    Result := FormatFloat('#,##0.0', AValue, DisplayFormat)
  else if LMagnitude >= 0.01 then
    Result := FormatFloat('#,##0.00', AValue, DisplayFormat)
  else
    Result := FormatFloat('0.0e-0', AValue, DisplayFormat);
end;

function FmtRate(const AValue: Double): string;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit('-');
  Result := FormatFloat('#,##0.00', AValue, DisplayFormat);
end;

function FmtUptime(const ASeconds: Double): string;
var
  LTotal, LDays, LHours, LMinutes, LSeconds: Integer;
begin
  if IsNan(ASeconds) or (ASeconds < 0) then
    Exit('-');

  LTotal := Trunc(ASeconds);
  LDays := LTotal div 86400;
  LHours := (LTotal mod 86400) div 3600;
  LMinutes := (LTotal mod 3600) div 60;
  LSeconds := LTotal mod 60;

  if LDays > 0 then
    Result := Format('%dd %.2dh %.2dm', [LDays, LHours, LMinutes])
  else if LHours > 0 then
    Result := Format('%dh %.2dm %.2ds', [LHours, LMinutes, LSeconds])
  else if LMinutes > 0 then
    Result := Format('%dm %.2ds', [LMinutes, LSeconds])
  else
    Result := Format('%ds', [LSeconds]);
end;

function FmtClock(const ATime: TDateTime): string;
begin
  if ATime = 0 then
    Exit('-');
  Result := FormatDateTime('hh:nn:ss', ATime);
end;

{ Chart geometry ----------------------------------------------------------- }

/// <summary>
///   A polyline (and its filled area) over a 0..100 by 0..34 view box, drawn
///   from zero: a chart that rescales to its own minimum is a chart that lies.
/// </summary>
procedure BuildPolyline(const AValues: TArray<Double>; out APoints, AAreaPoints: string;
  out AMax: Double);
var
  LMax, LValue, LX, LY: Double;
  LIndex: Integer;
  LPoint: string;
begin
  APoints := '';
  AAreaPoints := '';

  LMax := 0;
  for LValue in AValues do
    if not IsNan(LValue) and (LValue > LMax) then
      LMax := LValue;
  AMax := LMax;

  if (Length(AValues) < 2) or (LMax <= 0) then
    Exit;

  for LIndex := 0 to High(AValues) do
  begin
    LX := (LIndex / (Length(AValues) - 1)) * CHART_WIDTH;
    LValue := AValues[LIndex];
    if IsNan(LValue) or (LValue < 0) then
      LValue := 0;
    LY := CHART_HEIGHT - 2 - ((LValue / LMax) * (CHART_HEIGHT - 6));

    LPoint := FormatFloat('0.0', LX, DisplayFormat) + ',' +
      FormatFloat('0.0', LY, DisplayFormat);
    if APoints <> '' then
      APoints := APoints + ' ';
    APoints := APoints + LPoint;
  end;

  AAreaPoints := '0,' + FormatFloat('0.0', CHART_HEIGHT, DisplayFormat) + ' ' + APoints +
    ' ' + FormatFloat('0.0', CHART_WIDTH, DisplayFormat) + ',' +
    FormatFloat('0.0', CHART_HEIGHT, DisplayFormat);
end;

/// <summary>The points of a sparkline, or '' when there is nothing to draw.</summary>
function BuildSparkline(const AValues: TArray<Double>): string;
var
  LPoints, LArea: string;
  LMax: Double;
begin
  BuildPolyline(AValues, LPoints, LArea, LMax);
  Result := LPoints;
end;

/// <summary>One scalar field of a sample, by name - what a rate is derived from.</summary>
function FieldOf(const ASample: TMetricsSample; const AField: string): Double;
begin
  if SameText(AField, 'requests') then
    Result := ASample.Requests
  else if SameText(AField, 'errors') then
    Result := ASample.Errors
  else if SameText(AField, 'tools') then
    Result := ASample.ToolCalls
  else if SameText(AField, 'inflight') then
    Result := ASample.InFlight
  else
    Result := 0;
end;

/// <summary>
///   The per-second rate of one counter across the samples: the difference
///   between consecutive totals over the time between them. The first sample has
///   no predecessor and is therefore not a point of the series - a rate chart
///   with a fabricated zero at the left edge is worse than one point fewer.
/// </summary>
function RateSeries(const ASamples: TArray<TMetricsSample>; const AField: string): TArray<Double>;
var
  LResult: TList<Double>;
  LSeconds, LPrevious, LCurrent, LDelta: Double;
  LIndex: Integer;
begin
  LResult := TList<Double>.Create;
  try
    for LIndex := 1 to High(ASamples) do
    begin
      LSeconds := (ASamples[LIndex].At - ASamples[LIndex - 1].At) * SecsPerDay;
      if LSeconds <= 0 then
        Continue;

      LPrevious := FieldOf(ASamples[LIndex - 1], AField);
      LCurrent := FieldOf(ASamples[LIndex], AField);
      LDelta := LCurrent - LPrevious;
      // A total that went backwards means a restart inside the window: the rate
      // is unknown, not negative.
      if LDelta < 0 then
        Continue;

      LResult.Add(LDelta / LSeconds);
    end;
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

/// <summary>The last value of a series, 0 when it is empty.</summary>
function LastOf(const AValues: TArray<Double>): Double;
begin
  if Length(AValues) = 0 then
    Exit(0);
  Result := AValues[High(AValues)];
end;

/// <summary>A 0..100 percentage for a CSS length: one decimal, always a '.'.</summary>
function FmtPercent(const AValue: Double): string;
begin
  Result := FormatFloat('0.0', EnsureRange(AValue, 0, 100), DisplayFormat);
end;

/// <summary>The smallest of 1, 2 or 5 times a power of ten that is not below AValue.</summary>
function NiceCeiling(const AValue: Double): Double;
var
  LPower, LMantissa: Double;
begin
  if IsNan(AValue) or (AValue <= 0) then
    Exit(1);
  LPower := Power(10, Floor(Log10(AValue)));
  LMantissa := AValue / LPower;
  if LMantissa <= 1 then
    LMantissa := 1
  else if LMantissa <= 2 then
    LMantissa := 2
  else if LMantissa <= 5 then
    LMantissa := 5
  else
    LMantissa := 10;
  Result := LMantissa * LPower;
end;

const
  /// <summary>The centre of a dial's half circle, in its 0..120 by 0..70 view box.</summary>
  DIAL_CX = 60;
  DIAL_CY = 62;

  /// <summary>The radius of the value arc, and of the thinner window-range arc outside it.</summary>
  DIAL_RADIUS = 46;
  DIAL_RANGE_RADIUS = 55;

  /// <summary>The colours of a counter's split bar, in order; the rest is "other".</summary>
  SPLIT_COLOURS: array[0..4] of string = ('#4aa3ff', '#3fbf7f', '#e0a13a', '#b58cff', '#5fd0d0');
  SPLIT_OTHER_COLOUR = '#5b6676';

/// <summary>
///   An SVG arc along a dial, from one fraction of the scale to another: 0 is the
///   left end of the half circle, 1 the right. '' when the two coincide.
/// </summary>
function DialArc(const AFrom, ATo, ARadius: Double): string;

  function PointAt(const AFraction: Double): string;
  var
    LAngle: Double;
  begin
    LAngle := Pi * (1 - AFraction);
    Result := FormatFloat('0.0', DIAL_CX + ARadius * Cos(LAngle), DisplayFormat) + ',' +
      FormatFloat('0.0', DIAL_CY - ARadius * Sin(LAngle), DisplayFormat);
  end;

var
  LFrom, LTo: Double;
begin
  LFrom := EnsureRange(AFrom, 0, 1);
  LTo := EnsureRange(ATo, 0, 1);
  if LTo - LFrom < 0.002 then
    Exit('');
  // Never more than half a circle, so the large-arc flag is always 0; sweep 1
  // runs clockwise on screen, from the left end over the top.
  Result := 'M' + PointAt(LFrom) + ' A' + IntToStr(Round(ARadius)) + ',' +
    IntToStr(Round(ARadius)) + ' 0 0 1 ' + PointAt(LTo);
end;

/// <summary>
///   The per-second rate of a running total across its samples, skipping the
///   intervals where it is unknown (before the instrument existed, or across a
///   restart that made the total go backwards).
/// </summary>
function TotalRates(const ATotals: TArray<Double>; const ATimes: TArray<TDateTime>): TArray<Double>;
var
  LResult: TList<Double>;
  LIndex: Integer;
  LSeconds, LDelta: Double;
begin
  LResult := TList<Double>.Create;
  try
    for LIndex := 1 to High(ATotals) do
    begin
      if IsNan(ATotals[LIndex]) or IsNan(ATotals[LIndex - 1]) then
        Continue;
      LSeconds := (ATimes[LIndex] - ATimes[LIndex - 1]) * SecsPerDay;
      LDelta := ATotals[LIndex] - ATotals[LIndex - 1];
      if (LSeconds <= 0) or (LDelta < 0) then
        Continue;
      LResult.Add(LDelta / LSeconds);
    end;
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

{ TKpiView ----------------------------------------------------------------- }

constructor TKpiView.Create(const ALabelText, AValueText, AUnitText, AHint: string;
  const ATone: string);
begin
  inherited Create;
  FLabelText := ALabelText;
  FValueText := AValueText;
  FUnitText := AUnitText;
  FHint := AHint;
  FTone := ATone;
end;

{ TChartSeriesView --------------------------------------------------------- }

constructor TChartSeriesView.Create(const AName, AColour: string);
begin
  inherited Create;
  FName := AName;
  FColour := AColour;
end;

{ TChartView --------------------------------------------------------------- }

constructor TChartView.Create(const ACaption, AEmptyText: string);
begin
  inherited Create;
  FSeries := TObjectList.Create(True);
  FCaption := ACaption;
  FEmptyText := AEmptyText;
end;

destructor TChartView.Destroy;
begin
  FSeries.Free;
  inherited;
end;

function TChartView.GetHasData: Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FSeries.Count - 1 do
    if TChartSeriesView(FSeries[LIndex]).Points <> '' then
      Exit(True);
  Result := False;
end;

{ TInstrumentView ---------------------------------------------------------- }

constructor TInstrumentView.Create(const ARollup: TMetricInstrumentRollup);
begin
  inherited Create;
  FMeter := ARollup.Meter;
  if FMeter = '' then
    FMeter := '(default)';
  FName := ARollup.Name;
  FKind := MetricKindToStr(ARollup.Kind);
  FUnitText := ARollup.UnitName;
  FDescription := ARollup.Description;
  FSeriesText := FmtCount(ARollup.SeriesCount);
  FCountText := FmtCount(ARollup.Count);
  FSumText := FmtValue(ARollup.Sum);
  FAvgText := FmtValue(AverageOf2(ARollup.Sum, ARollup.Count));
  FMaxText := FmtValue(ARollup.Max);
  FLastText := FmtValue(ARollup.Last);
end;

{ TLabelValueView ---------------------------------------------------------- }

constructor TLabelValueView.Create(const ARollup: TMetricLabelRollup; const ABarPercent: Integer);
begin
  inherited Create;
  FValue := ARollup.LabelValue;
  FCountText := FmtCount(ARollup.Count);
  FSumText := FmtValue(ARollup.Sum);
  FAvgText := FmtValue(AverageOf2(ARollup.Sum, ARollup.Count));
  FMaxText := FmtValue(ARollup.Max);
  FBarPercent := IntToStr(ABarPercent);
end;

{ TGroupView --------------------------------------------------------------- }

constructor TGroupView.Create(const AGroup: TMetricGroupRollup);
var
  LItem: TMetricLabelRollup;
  LValues: TObjectList;
  LReference, LValue: Double;
  LPercent: Integer;

  function MeasureOf(const ARollup: TMetricLabelRollup): Double;
  begin
    // A histogram's total grows with the number of samples it happened to take,
    // so its average is the comparable number; a counter's total is the point.
    if AGroup.Kind = TMetricKind.Histogram then
      Result := AverageOf2(ARollup.Sum, ARollup.Count)
    else
      Result := ARollup.Sum;
  end;

begin
  inherited Create;
  FMeter := AGroup.Meter;
  if FMeter = '' then
    FMeter := '(default)';
  FName := AGroup.Name;
  FKind := MetricKindToStr(AGroup.Kind);
  FLabelKey := AGroup.LabelKey;
  FTitle := FMeter + '/' + FName + ' by ' + FLabelKey;
  FMeasure := IfThen(AGroup.Kind = TMetricKind.Histogram, 'average', 'total');

  LReference := 0;
  for LItem in AGroup.Values do
  begin
    LValue := MeasureOf(LItem);
    if LValue > LReference then
      LReference := LValue;
  end;

  LValues := TObjectList.Create(True);
  FValues := LValues;
  for LItem in AGroup.Values do
  begin
    if LReference > 0 then
      LPercent := Round((MeasureOf(LItem) / LReference) * 100)
    else
      LPercent := 0;
    LValues.Add(TLabelValueView.Create(LItem, LPercent));
  end;
end;

destructor TGroupView.Destroy;
begin
  FValues.Free;
  inherited;
end;

{ TGaugeView --------------------------------------------------------------- }

constructor TGaugeView.Create(const AInstrument: TMetricInstrumentRollup;
  const AValues: TArray<Double>);
var
  LMin, LMax, LFirst, LLast: Double;
  LValue, LScale: Double;
  LHasValue, LIsPercent: Boolean;
begin
  inherited Create;
  FMeter := AInstrument.Meter;
  if FMeter = '' then
    FMeter := '(default)';
  FName := AInstrument.Name;
  FUnitText := AInstrument.UnitName;
  FValueText := FmtValue(AInstrument.Last);

  LMin := 0;
  LMax := 0;
  LFirst := 0;
  LLast := 0;
  LHasValue := False;
  for LValue in AValues do
  begin
    if not LHasValue then
    begin
      LMin := LValue;
      LMax := LValue;
      LFirst := LValue;
      LHasValue := True;
    end;
    LMin := System.Math.Min(LMin, LValue);
    LMax := System.Math.Max(LMax, LValue);
    LLast := LValue;
  end;

  if LHasValue then
    FRangeText := 'min ' + FmtValue(LMin) + SEPARATOR + 'max ' + FmtValue(LMax)
  else
    FRangeText := 'no history yet';

  FSparkPoints := BuildSparkline(AValues);
  FHasSpark := FSparkPoints <> '';

  if Length(AValues) < 2 then
    FTrend := 'flat'
  else if LLast > LFirst then
    FTrend := 'up'
  else if LLast < LFirst then
    FTrend := 'down'
  else
    FTrend := 'flat';

  // The dial's scale. A percentage has a top that means something; anything
  // else is scaled to a round number above the largest value seen, so the arc
  // moves with the window instead of sitting pinned at one end.
  LIsPercent := (Pos('%', FUnitText) > 0) or SameText(FUnitText, 'percent') or
    FName.EndsWith('percent', True);
  if LIsPercent then
    LScale := 100
  else
    LScale := NiceCeiling(System.Math.Max(LMax, AInstrument.Last));
  FScaleMaxText := FmtValue(LScale);

  FDialPath := DialArc(0, AInstrument.Last / LScale, DIAL_RADIUS);
  if LHasValue then
    FRangePath := DialArc(LMin / LScale, LMax / LScale, DIAL_RANGE_RADIUS);

  FTone := '';
  if LIsPercent then
  begin
    if AInstrument.Last >= 90 then
      FTone := 'bad'
    else if AInstrument.Last >= 75 then
      FTone := 'warn';
  end;
end;

{ TSplitView --------------------------------------------------------------- }

constructor TSplitView.Create(const AValue, ATotalText: string; const APercent: Double;
  const AColour: string);
begin
  inherited Create;
  FValue := AValue;
  FTotalText := ATotalText;
  FPercent := FmtPercent(APercent);
  FPercentText := FormatFloat('0', APercent, DisplayFormat) + '%';
  FColour := AColour;
end;

{ TCounterView ------------------------------------------------------------- }

constructor TCounterView.Create(const AInstrument: TMetricInstrumentRollup;
  const ATotals: TArray<Double>; const ATimes: TArray<TDateTime>;
  const AGroups: TArray<TMetricGroupRollup>);
var
  LRates: TArray<Double>;
  LPeak: Double;
  LGroup: TMetricGroupRollup;
  LValues: TArray<TMetricLabelRollup>;
  LTotal, LOther: Double;
  LIndex: Integer;
begin
  inherited Create;
  FSplits := TObjectList.Create(True);
  FMeter := AInstrument.Meter;
  if FMeter = '' then
    FMeter := '(default)';
  FName := AInstrument.Name;
  FUnitText := AInstrument.UnitName;
  FDescription := AInstrument.Description;
  FTotalText := FmtCount(AInstrument.Sum);
  FSeriesText := FmtCount(AInstrument.SeriesCount);

  LRates := TotalRates(ATotals, ATimes);
  BuildPolyline(LRates, FSparkPoints, FSparkArea, LPeak);
  if Length(LRates) > 0 then
  begin
    FRateText := FmtRate(LastOf(LRates));
    FPeakRateText := FmtRate(LPeak);
  end
  else
  begin
    FRateText := '-';
    FPeakRateText := '-';
  end;

  // The split is drawn by the first label key the counter has. A group lists
  // its values in no promised order, so they are ranked here, largest first.
  for LGroup in AGroups do
  begin
    if (LGroup.Meter <> AInstrument.Meter) or (LGroup.Name <> AInstrument.Name) then
      Continue;

    LValues := Copy(LGroup.Values);
    TArray.Sort<TMetricLabelRollup>(LValues, TComparer<TMetricLabelRollup>.Construct(
      function(const ALeft, ARight: TMetricLabelRollup): Integer
      begin
        Result := CompareValue(ARight.Sum, ALeft.Sum);
      end));

    LTotal := 0;
    for LIndex := 0 to High(LValues) do
      LTotal := LTotal + LValues[LIndex].Sum;
    if LTotal <= 0 then
      Break;

    FSplitKey := LGroup.LabelKey;
    LOther := 0;
    for LIndex := 0 to High(LValues) do
      if LIndex <= High(SPLIT_COLOURS) then
        FSplits.Add(TSplitView.Create(LValues[LIndex].LabelValue, FmtCount(LValues[LIndex].Sum),
          (LValues[LIndex].Sum / LTotal) * 100, SPLIT_COLOURS[LIndex]))
      else
        LOther := LOther + LValues[LIndex].Sum;
    if LOther > 0 then
      FSplits.Add(TSplitView.Create('other', FmtCount(LOther), (LOther / LTotal) * 100,
        SPLIT_OTHER_COLOUR));
    Break;
  end;
end;

destructor TCounterView.Destroy;
begin
  FSplits.Free;
  inherited;
end;

function TCounterView.GetHasSpark: Boolean;
begin
  Result := FSparkPoints <> '';
end;

function TCounterView.GetHasSplits: Boolean;
begin
  Result := FSplits.Count > 0;
end;

{ THistogramView ----------------------------------------------------------- }

constructor THistogramView.Create(const AInstrument: TMetricInstrumentRollup;
  const ASums, ACounts: TArray<Double>);
var
  LAverages: TList<Double>;
  LIndex, LFirst, LLength: Integer;
  LDeltaSum, LDeltaCount, LPeak, LAvg: Double;
  LWindowCount: Double;
begin
  inherited Create;
  FMeter := AInstrument.Meter;
  if FMeter = '' then
    FMeter := '(default)';
  FName := AInstrument.Name;
  FUnitText := AInstrument.UnitName;
  FDescription := AInstrument.Description;
  FCountText := FmtCount(AInstrument.Count);
  LAvg := AverageOf2(AInstrument.Sum, AInstrument.Count);
  FAvgText := FmtValue(LAvg);
  FMinText := FmtValue(AInstrument.Min);
  FMaxText := FmtValue(AInstrument.Max);

  // The band runs on a scale from zero to the largest observation, so its left
  // edge is where the smallest one sits and the marker is the mean between.
  FHasBand := (AInstrument.Count > 0) and (AInstrument.Max > 0);
  if FHasBand then
  begin
    FBandLeft := FmtPercent((AInstrument.Min / AInstrument.Max) * 100);
    FBandWidth := FmtPercent(100 - (AInstrument.Min / AInstrument.Max) * 100);
    FAvgLeft := FmtPercent((LAvg / AInstrument.Max) * 100);
  end;

  // The average of each interval alone: the change in the sum over the change
  // in the count. An interval with no observation repeats the previous value,
  // so the line keeps its shape instead of dropping to a zero nobody measured.
  LAverages := TList<Double>.Create;
  try
    LFirst := -1;
    // The two tracks are recorded together, so they are always as long as each
    // other; anything else is read as no history rather than indexed past.
    if Length(ASums) = Length(ACounts) then
      LLength := Length(ASums)
    else
      LLength := 0;
    for LIndex := 0 to LLength - 1 do
    begin
      if IsNan(ASums[LIndex]) or IsNan(ACounts[LIndex]) then
        Continue;
      if LFirst < 0 then
      begin
        LFirst := LIndex;
        Continue;
      end;
      LDeltaSum := ASums[LIndex] - ASums[LIndex - 1];
      LDeltaCount := ACounts[LIndex] - ACounts[LIndex - 1];
      if LDeltaCount > 0 then
        LAverages.Add(LDeltaSum / LDeltaCount)
      else if LAverages.Count > 0 then
        LAverages.Add(LAverages[LAverages.Count - 1]);
    end;
    BuildPolyline(LAverages.ToArray, FSparkPoints, FSparkArea, LPeak);
  finally
    LAverages.Free;
  end;

  if LFirst >= 0 then
    LWindowCount := ACounts[High(ACounts)] - ACounts[LFirst]
  else
    LWindowCount := 0;
  if LWindowCount > 0 then
    FWindowText := 'window avg ' + FmtValue((ASums[High(ASums)] - ASums[LFirst]) / LWindowCount) +
      SEPARATOR + FmtCount(LWindowCount) + ' observed'
  else
    FWindowText := 'nothing observed in the window';
end;

function THistogramView.GetHasSpark: Boolean;
begin
  Result := FSparkPoints <> '';
end;

{ TSlowView ---------------------------------------------------------------- }

constructor TSlowView.Create(const ARank: Integer; const AEntry: TMetricSlowEntry);
begin
  inherited Create;
  FRank := ARank;
  FMeter := AEntry.Meter;
  FName := AEntry.Name;
  FLabels := AEntry.Labels;
  if FLabels = '' then
    FLabels := '(no labels)';
  FCountText := FmtCount(AEntry.Count);
  FAvgText := FmtValue(AEntry.Avg);
  FMaxText := FmtValue(AEntry.Max);
  FMinText := FmtValue(AEntry.Min);
end;

{ TMetricsViewModel -------------------------------------------------------- }

constructor TMetricsViewModel.Create;
begin
  inherited Create;
  FKpis := TObjectList.Create(True);
  FCharts := TObjectList.Create(True);
  FInstruments := TObjectList.Create(True);
  FGroups := TObjectList.Create(True);
  FGauges := TObjectList.Create(True);
  FSlowest := TObjectList.Create(True);
  FCounters := TObjectList.Create(True);
  FHistograms := TObjectList.Create(True);
  RefreshSeconds := 5;
end;

destructor TMetricsViewModel.Destroy;
begin
  FHistograms.Free;
  FCounters.Free;
  FSlowest.Free;
  FGauges.Free;
  FGroups.Free;
  FInstruments.Free;
  FCharts.Free;
  FKpis.Free;
  inherited;
end;

function TMetricsViewModel.GetHasInstruments: Boolean;
begin
  Result := FInstruments.Count > 0;
end;

function TMetricsViewModel.GetHasGroups: Boolean;
begin
  Result := FGroups.Count > 0;
end;

function TMetricsViewModel.GetHasGauges: Boolean;
begin
  Result := FGauges.Count > 0;
end;

function TMetricsViewModel.GetHasSlowest: Boolean;
begin
  Result := FSlowest.Count > 0;
end;

function TMetricsViewModel.GetHasCounters: Boolean;
begin
  Result := FCounters.Count > 0;
end;

function TMetricsViewModel.GetHasHistograms: Boolean;
begin
  Result := FHistograms.Count > 0;
end;

{ BuildMetricsViewModel ---------------------------------------------------- }

function BuildMetricsViewModel(const AReport: TMetricsReport; const AHistory: TMetricsHistory;
  const AExportersText, AUseCasesText, AAboutText: string;
  const ARefreshSeconds: Integer): TMetricsViewModel;
var
  LModel: TMetricsViewModel;
  LInstrument, LRequests, LErrors, LTools, LInFlight, LLatency: TMetricInstrumentRollup;
  LGroup: TMetricGroupRollup;
  LSlow: TMetricSlowEntry;
  LSamples: TArray<TMetricsSample>;
  LRates: TArray<Double>;
  LTracks: TArray<TArray<Double>>;
  LTimes: TArray<TDateTime>;
  LSeries: TChartSeriesView;
  LPoints, LArea: string;
  LMax: Double;
  LIndex: Integer;
  LMeter: string;
  LMeterList: string;
begin
  LModel := TMetricsViewModel.Create;
  try
    LModel.ServiceName := AReport.ServiceName;
    if LModel.ServiceName = '' then
      LModel.ServiceName := 'unnamed server';
    LModel.ServiceVersion := AReport.ServiceVersion;
    LModel.ServiceDescription := AReport.ServiceDescription;
    LModel.HasServiceDescription := not AReport.ServiceDescription.IsEmpty;
    LModel.Schema := AReport.Schema;
    LModel.StartedText := FmtClock(AReport.StartedAt);
    LModel.GeneratedText := FmtClock(AReport.GeneratedAt);
    LModel.UptimeText := FmtUptime(AReport.UptimeSeconds);
    LModel.PollsText := FmtCount(AReport.PollCount);
    LModel.ResetsText := FmtCount(AReport.ResetsDetected);
    LModel.SeriesText := FmtCount(AReport.SeriesCount);
    LModel.InstrumentsText := FmtCount(AReport.InstrumentCount);
    LModel.MetersText := FmtCount(Length(AReport.Meters));
    LModel.RefreshSeconds := ARefreshSeconds;
    LModel.ExportersText := AExportersText;
    LModel.UseCasesText := AUseCasesText;
    LModel.AboutText := AAboutText;
    LModel.HasExporters := not AExportersText.IsEmpty;
    LModel.HasUseCases := not AUseCasesText.IsEmpty;
    LModel.HasAbout := not AAboutText.IsEmpty;

    LMeterList := '';
    for LMeter in AReport.Meters do
    begin
      if LMeterList <> '' then
        LMeterList := LMeterList + ', ';
      LMeterList := LMeterList + LMeter;
    end;
    LModel.MeterList := LMeterList;

    // The instruments the KPI row is about. Instrument() answers a zeroed rollup
    // for one that has not been recorded yet, so nothing here needs a guard.
    LRequests := AReport.Instrument(METER_MCP, 'request.count');
    LErrors := AReport.Instrument(METER_MCP, 'request.errors');
    LTools := AReport.Instrument(METER_MCP, 'tool.count');
    LInFlight := AReport.Instrument(METER_MCP, 'request.in_flight');
    LLatency := AReport.Instrument(METER_MCP, 'request.duration_ms');

    LSamples := AHistory.Snapshot;
    LRates := RateSeries(LSamples, 'requests');

    // --- KPI row ----------------------------------------------------------
    LModel.Kpis.Add(TKpiView.Create('Uptime', LModel.UptimeText, '',
      LModel.ServiceName + ' ' + LModel.ServiceVersion));
    LModel.Kpis.Add(TKpiView.Create('Requests', FmtCount(LRequests.Sum), '',
      FmtRate(LastOf(LRates)) + '/s' + SEPARATOR + FmtCount(LRequests.SeriesCount) + ' series'));
    LModel.Kpis.Add(TKpiView.Create('Errors', FmtCount(LErrors.Sum), '',
      FmtRate(LastOf(RateSeries(LSamples, 'errors'))) + '/s' + SEPARATOR +
      FmtCount(LErrors.SeriesCount) + ' series',
      IfThen(LErrors.Sum > 0, 'bad', '')));
    LModel.Kpis.Add(TKpiView.Create('Tool calls', FmtCount(LTools.Sum), '',
      FmtRate(LastOf(RateSeries(LSamples, 'tools'))) + '/s' + SEPARATOR +
      FmtCount(LTools.SeriesCount) + ' series'));
    LModel.Kpis.Add(TKpiView.Create('In flight', FmtCount(LInFlight.Last), '',
      'requests being served right now', IfThen(LInFlight.Last > 0, 'warn', '')));
    LModel.Kpis.Add(TKpiView.Create('Latency avg',
      FmtValue(AverageOf2(LLatency.Sum, LLatency.Count)), 'ms',
      'max ' + FmtValue(LLatency.Max) + ' ms over ' + FmtCount(LLatency.Count) + ' calls'));
    LModel.Kpis.Add(TKpiView.Create('Series', LModel.SeriesText, '',
      LModel.InstrumentsText + ' instruments' + SEPARATOR + LModel.MetersText + ' meters' +
      SEPARATOR + LModel.ResetsText + ' resets folded'));

    // --- charts -----------------------------------------------------------
    LModel.Throughput := TChartView.Create('Throughput', 'Waiting for a second sample.');
    LSeries := TChartSeriesView.Create('requests/s', '#4aa3ff');
    BuildPolyline(LRates, LPoints, LArea, LMax);
    LSeries.Points := LPoints;
    LSeries.AreaPoints := LArea;
    LSeries.MaxText := FmtRate(LMax);
    if Length(LRates) > 0 then
      LSeries.LastText := FmtRate(LRates[High(LRates)]);
    LModel.Throughput.Series.Add(LSeries);

    LSeries := TChartSeriesView.Create('tool calls/s', '#3fbf7f');
    BuildPolyline(RateSeries(LSamples, 'tools'), LPoints, LArea, LMax);
    LSeries.Points := LPoints;
    LSeries.AreaPoints := LArea;
    LSeries.MaxText := FmtRate(LMax);
    LModel.Throughput.Series.Add(LSeries);

    LModel.Latency := TChartView.Create('Latency', 'Waiting for a second sample.');
    LSeries := TChartSeriesView.Create('average', '#4aa3ff');
    BuildPolyline(AHistory.LatencySeries('avg'), LPoints, LArea, LMax);
    LSeries.Points := LPoints;
    LSeries.AreaPoints := LArea;
    LSeries.MaxText := FmtValue(LMax);
    LModel.Latency.Series.Add(LSeries);

    LSeries := TChartSeriesView.Create('max', '#e0a13a');
    BuildPolyline(AHistory.LatencySeries('max'), LPoints, LArea, LMax);
    LSeries.Points := LPoints;
    LSeries.MaxText := FmtValue(LMax);
    LModel.Latency.Series.Add(LSeries);

    // --- tables -----------------------------------------------------------
    for LInstrument in AReport.Instruments do
      LModel.Instruments.Add(TInstrumentView.Create(LInstrument));

    for LGroup in AReport.Groups do
      LModel.Groups.Add(TGroupView.Create(LGroup));

    for LSlow in AReport.Slowest do
      LModel.Slowest.Add(TSlowView.Create(LModel.Slowest.Count + 1, LSlow));

    for LInstrument in AReport.Instruments do
      case LInstrument.Kind of
        TMetricKind.Gauge:
          LModel.Gauges.Add(TGaugeView.Create(LInstrument,
            AHistory.GaugeSeries(LInstrument.Meter, LInstrument.Name)));
        TMetricKind.Counter:
          begin
            LTracks := AHistory.Track(LInstrument.Meter, LInstrument.Name, [TRACK_SUM], LTimes);
            LModel.Counters.Add(TCounterView.Create(LInstrument, LTracks[0], LTimes,
              AReport.Groups));
          end;
        TMetricKind.Histogram:
          begin
            LTracks := AHistory.Track(LInstrument.Meter, LInstrument.Name,
              [TRACK_SUM, TRACK_COUNT], LTimes);
            LModel.Histograms.Add(THistogramView.Create(LInstrument, LTracks[0], LTracks[1]));
          end;
      end;

    LModel.SampleCountText := FmtCount(Length(LSamples));
    if Length(LSamples) >= 2 then
      LModel.HistorySpanText :=
        FmtUptime((LSamples[High(LSamples)].At - LSamples[0].At) * SecsPerDay)
    else
      LModel.HistorySpanText := 'no history yet';

    LModel.Throughput.WindowText := LModel.SampleCountText + ' samples over ' +
      LModel.HistorySpanText;
    LModel.Latency.WindowText := LModel.Throughput.WindowText;

    // The page loops one list of charts rather than naming them, so adding one
    // here is all it takes for it to appear.
    LModel.Charts.Add(LModel.Throughput);
    LModel.Charts.Add(LModel.Latency);

    Result := LModel;
  except
    LModel.Free;
    raise;
  end;
end;

initialization
  DisplayFormat := TFormatSettings.Create('en-US');
  DisplayFormat.ThousandSeparator := ',';
  DisplayFormat.DecimalSeparator := '.';

end.
