{******************************************************************************}
{                                                                              }
{  Delphi MCP Connect Library                                                  }
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
///   A small, interface based metrics subsystem, deliberately modelled on
///   OpenTelemetry and deliberately limited to metrics.
///
///   The shapes are the OpenTelemetry ones - a MeterProvider owns Meters, a
///   Meter creates instruments (Counter, Gauge, Histogram), every measurement
///   may carry key/value labels - but there is no OTLP, no traces, no baggage
///   and no exporter protocol: measurements accumulate in process, thread
///   safely, and are read back (or "harvested") at any later point through the
///   typed snapshot returned by Collect, or pushed to IMetricExporter
///   implementations registered on the provider.
///
///   Recording is meant to be a one-liner. Instruments are cached: repeated
///   lookups of the same meter/instrument name return the same instance, so a
///   short-lived reference can be used over and over without bookkeeping:
///   <code>
///   uses MCPConnect.Metrics;
///
///   // count tool invocations, split by tool name
///   TMetrics.Counter('tool.calls', 'Tools invoked', 'calls')
///     .Add(1, ['tool', 'get_weather']);
///
///   // a latency sample
///   TMetrics.Histogram('tool.duration_ms', 'Tool latency', 'ms')
///     .Observe(42.5, ['tool', 'get_weather']);
///
///   // a current-value snapshot
///   TMetrics.Gauge('active.requests').SetValue(FActive);
///   </code>
///   Harvesting later:
///   <code>
///   var LPoints: TArray&lt;TMetricPoint&gt;;
///   begin
///     LPoints := TMetrics.Collect;           // typed snapshot
///     TMetrics.Harvest;                      // or push to registered exporters
///   end;
///   </code>
///   See MCPConnect.Metrics.Exporters for ready-made text and JSON exporters.
///
///   Values are aggregated per instrument and per distinct label set. A
///   Counter keeps the running total (monotonic, negative adds are rejected),
///   a Gauge the last value set, a Histogram count/sum/min/max. Everything is
///   stored as Double; per series the record keeps Count/Sum/Min/Max/Last plus
///   first and last seen timestamps, so one TMetricPoint shape serves all
///   three instrument kinds and consumers interpret it by Kind.
///
///   The core unit has no dependencies outside the RTL; the sample exporters
///   in MCPConnect.Metrics.Exporters build on Neon, which the library already
///   uses for JSON.
/// </summary>
unit MCPConnect.Metrics;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,

  Neon.Core.Attributes;

type
  /// <summary>
  ///   Raised on misuse: recording on an instrument of the wrong kind,
  ///   decrementing a counter, an odd number of label strings, a name already
  ///   taken by an instrument of a different kind.
  /// </summary>
  EMetricsError = class(Exception);

  /// <summary>
  ///   The three instrument kinds the subsystem knows (the OpenTelemetry ones
  ///   worth having in process; UpDownCounter is deliberately absent - use a
  ///   Gauge when a value can go down as well as up).
  /// </summary>
  /// <remarks>
  ///   The NeonEnumNames are the JSON spelling of a kind, not decoration: Neon
  ///   reads them off the type (not off a member), so they are what an exported
  ///   point carries in "kind". Keep them in sync with MetricKindToStr.
  /// </remarks>
  [NeonEnumNames('counter,gauge,histogram')]
  TMetricKind = (Counter, Gauge, Histogram);

  /// <summary>One key/value label of a measurement (an OpenTelemetry attribute).</summary>
  TMetricLabel = record
    Key: string;
    Value: string;
  end;

/// <summary>Stable display name of a kind: "counter", "gauge", "histogram".</summary>
function MetricKindToStr(AKind: TMetricKind): string;

type
  /// <summary>
  ///   <para>
  ///     The aggregated state of one instrument label set (one "series"), as
  ///     read by Collect / delivered to exporters. Fields are interpreted by
  ///     Kind:
  ///   </para>
  ///   <list type="bullet">
  ///     <item>
  ///       Counter: Sum is the running total, Count the number of adds.
  ///     </item>
  ///     <item>
  ///       Gauge: Last is the current value.
  ///     </item>
  ///     <item>
  ///       Histogram: Count, Sum, Min, Max describe the recorded samples.
  ///     </item>
  ///   </list>
  ///   Count/Sum/Min/Max/Last are filled for every kind anyway, Min/Max/Last
  ///   just mean less on counters and gauges. A returned point must be treated
  ///   as read only.
  /// </summary>
  TMetricPoint = record
  public
    /// <summary>
    ///   Name of the meter the instrument belongs to ('' for the default one).
    /// </summary>
    Meter: string;

    /// <summary>
    ///   Instrument name, unique within its meter.
    /// </summary>
    Name: string;

    /// <summary>
    ///   Metric Kind: Counter, Gauge, Histogram.
    /// </summary>
    Kind: TMetricKind;

    /// <summary>
    ///   Free text, as passed when the instrument was created.
    /// </summary>
    Description: string;

    /// <summary>
    ///   Unit of measure, as passed when the instrument was created ('' when
    ///   none).
    /// </summary>
    [NeonProperty('unit')] UnitName: string;

    /// <summary>
    ///   Labels of this series, sorted by key. Empty when unlabeled.
    /// </summary>
    Labels: TArray<TMetricLabel>;

    /// <summary>
    ///   Number of recordings folded into this point.
    /// </summary>
    Count: Int64;

    /// <summary>
    ///   Sum of the recorded values (Counter: running total).
    /// </summary>
    Sum: Double;

    /// <summary>
    ///   Smallest recorded value.
    /// </summary>
    Min: Double;

    /// <summary>
    ///   Largest recorded value.
    /// </summary>
    Max: Double;

    /// <summary>
    ///   Most recent recorded value (Gauge: current value).
    /// </summary>
    Last: Double;

    /// <summary>
    ///   When the first recording of this series happened.
    /// </summary>
    FirstSeen: TDateTime;

    /// <summary>
    ///   When the most recent recording of this series happened.
    /// </summary>
    LastSeen: TDateTime;

    /// <summary>
    ///   One human readable line, e.g. "counter tool.calls sum=6 count=6
    ///   {tool=get_weather}".
    /// </summary>
    function ToString: string;
  end;

  /// <summary>
  ///   Receives the metrics collected by a Harvest call. Register instances on
  ///   an IMeterProvider; the provider calls Export once per Harvest with a
  ///   point array that must be treated as read only. Export runs on the
  ///   thread that called Harvest, outside the provider's lock.
  /// </summary>
  IMetricExporter = interface
    ['{8FC3461C-4F25-4B0C-BCC0-765A1B5F8BD1}']
    procedure Export(const APoints: TArray<TMetricPoint>);
  end;

  /// <summary>
  ///   A single instrument of any kind. Instruments are interfaces created by
  ///   an IMeter (or by the TMetrics facade) and cached by name, so the
  ///   same instrument is returned on every lookup. Record through the typed
  ///   sub-interfaces; IInstrument only exposes identity and state.
  /// </summary>
  IInstrument = interface
    ['{6E10C0EE-A370-49B2-A8FA-4BA2A39E4D12}']
    function GetName: string;
    function GetKind: TMetricKind;
    function GetDescription: string;
    function GetUnitName: string;
    function GetSeriesCount: Integer;
    function GetDroppedSeries: Int64;

    /// <summary>
    ///   Snapshot of the instrument's current series.
    /// </summary>
    function Collect: TArray<TMetricPoint>;

    /// <summary>
    ///   Drops every series; the instrument can keep being recorded into.
    /// </summary>
    procedure Clear;

    property Name: string read GetName;
    property Kind: TMetricKind read GetKind;
    property Description: string read GetDescription;
    property UnitName: string read GetUnitName;
    /// <summary>
    ///   Distinct label sets currently aggregated (1 when unlabeled).
    /// </summary>
    property SeriesCount: Integer read GetSeriesCount;
    /// <summary>
    ///   Series refused since the last Clear because the per-instrument label
    ///   set limit was reached. Anything above zero means snapshots are
    ///   incomplete; the recorded samples themselves were dropped.
    /// </summary>
    property DroppedSeries: Int64 read GetDroppedSeries;
  end;

  /// <summary>
  ///   Monotonic counter. Add is the only operation; negative values raise
  ///   EMetricsError. The total accumulated since creation (or since the
  ///   last Clear) is the point's Sum.
  /// </summary>
  ICounter = interface(IInstrument)
    ['{3D8E7B91-26A2-4DF5-86B7-1A6E889B27D4}']
    procedure Add(const AValue: Double = 1); overload;
    /// <summary>
    ///   Even number of strings: key, value, key, value...
    /// </summary>
    procedure Add(const AValue: Double; const ALabels: array of string); overload;
  end;

  /// <summary>
  ///   Current-value instrument. SetValue overwrites the series value; the
  ///   point's Last field is what a consumer should read.
  /// </summary>
  IGauge = interface(IInstrument)
    ['{E9C2C5B6-2B2A-4AF3-8B42-5583F0D47C29}']
    procedure SetValue(const AValue: Double); overload;
    /// <summary>
    ///   Even number of strings: key, value, key, value...
    /// </summary>
    procedure SetValue(const AValue: Double; const ALabels: array of string); overload;
  end;

  /// <summary>
  ///   Distribution instrument for latencies, sizes and other samples.
  ///   Record one value per observation; the point's Count/Sum/Min/Max are the
  ///   aggregates (no explicit buckets - averages and extremes only).
  /// </summary>
  IHistogram = interface(IInstrument)
    ['{6D938B7D-A9F1-45D9-9AE3-9E2D5A9434A5}']
    procedure Observe(const AValue: Double); overload;
    /// <summary>
    ///   Even number of strings: key, value, key, value...
    /// </summary>
    procedure Observe(const AValue: Double; const ALabels: array of string); overload;
  end;

  /// <summary>
  ///   A named group of instruments (the OpenTelemetry Meter). Meters are
  ///   obtained from a provider and cached by name; each meter keeps its
  ///   instruments, keyed by instrument name. Instruments of different meters
  ///   never mix, whatever their names.
  /// </summary>
  IMeter = interface
    ['{B8A87344-C1F5-4CB1-95FA-42795D23D578}']
    function GetName: string;
    /// <summary>
    ///   Snapshot of every instrument of this meter.
    /// </summary>
    function Collect: TArray<TMetricPoint>;
    /// <summary>
    ///   Clears every instrument of this meter.
    /// </summary>
    procedure Clear;
    property Name: string read GetName;
    function Counter(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): ICounter;
    function Gauge(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IGauge;
    function Histogram(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IHistogram;
  end;

  /// <summary>
  ///   The root of the subsystem (the OpenTelemetry MeterProvider). It owns
  ///   the meters, the registered exporters and the per-instrument label set
  ///   budget. Instances are reference counted: hold one, or use the process
  ///   wide default behind the TMetrics facade.
  /// </summary>
  IMeterProvider = interface
    ['{1C0D6F6E-5423-4EE7-9E30-9CE5B6E23102}']
    /// <summary>
    ///   Returns the meter with the given name, creating it on first use ('' is
    ///   the default meter).
    /// </summary>
    function GetMeter(const AName: string): IMeter;
    function GetMaxSeriesPerInstrument: Integer;
    procedure SetMaxSeriesPerInstrument(AValue: Integer);
    procedure AddExporter(const AExporter: IMetricExporter);
    procedure RemoveExporter(const AExporter: IMetricExporter);
    /// <summary>
    ///   Typed snapshot of every series of every meter, sorted by meter,
    ///   instrument and labels so consecutive harvests are comparable.
    /// </summary>
    function Collect: TArray<TMetricPoint>;
    /// <summary>
    ///   Snapshots everything and hands the copy to each registered exporter
    ///   in registration order. With AReset the collected state is cleared
    ///   afterwards, making the export a delta rather than a running total.
    /// </summary>
    procedure Harvest(const AReset: Boolean = False);
    /// <summary>
    ///   Clears every meter; instruments and exporters are kept.
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Upper bound on the distinct label sets one instrument may aggregate;
    ///   series beyond it are dropped and counted in DroppedSeries. Zero means
    ///   unbounded. Applied to meters created from now on; the default is
    ///   TMetrics.DEFAULT_MAX_SERIES.
    /// </summary>
    property MaxSeriesPerInstrument: Integer read GetMaxSeriesPerInstrument
      write SetMaxSeriesPerInstrument;
  end;

  /// <summary>
  ///   Static facade over the process wide default provider, for the one-line
  ///   usage shown in the unit comment. The default provider is created on
  ///   first use and released in the class destructor.
  /// </summary>
  TMetrics = class
  public const
    /// <summary>
    ///   Default label-set budget per instrument (see MaxSeriesPerInstrument).
    /// </summary>
    DEFAULT_MAX_SERIES = 128;
  strict private class var
    FDefault: IMeterProvider;
    FDefaultLock: TCriticalSection;
  private class var
    /// <summary>
    ///   Set at the beginning of the class destructor: from that moment on the
    ///   default provider is gone, so instruments still held by other units
    ///   must quietly stop recording instead of touching freed state. Not
    ///   strict: TMeterInstrument.RecordSample reads it on every recording.
    /// </summary>
    FShutdown: Boolean;
  public
    /// <summary>
    ///   Creates the lock guarding the default provider. Runs once, before the
    ///   initialization section of any unit that uses this one.
    /// </summary>
    class constructor Create;

    /// <summary>
    ///   Releases the default provider and then its lock, and marks the
    ///   subsystem as shut down. Runs once, after this unit's finalization.
    /// </summary>
    class destructor Destroy;

    /// <summary>
    ///   The process wide default provider.
    /// </summary>
    class function Default: IMeterProvider;

    /// <summary>
    ///   A brand new provider, independent from the default one.
    /// </summary>
    class function CreateProvider: IMeterProvider;

    /// <summary>
    ///   Shorthand for Default.GetMeter(AName) ('' is the default meter).
    /// </summary>
    class function Meter(const AName: string = ''): IMeter;

    class function Counter(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): ICounter;

    class function Gauge(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IGauge;

    class function Histogram(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IHistogram;

    /// <summary>
    ///   Shorthand for Default.Collect.
    /// </summary>
    class function Collect: TArray<TMetricPoint>;

    /// <summary>
    ///   Shorthand for Default.Harvest(AReset).
    /// </summary>
    class procedure Harvest(const AReset: Boolean = False);

    /// <summary>
    ///   Shorthand for Default.Clear.
    /// </summary>
    class procedure Clear;

    /// <summary>
    ///   Shorthand for Default.AddExporter.
    /// </summary>
    class procedure AddExporter(const AExporter: IMetricExporter);

    /// <summary>
    ///   Shorthand for Default.RemoveExporter.
    /// </summary>
    class procedure RemoveExporter(const AExporter: IMetricExporter);
  end;

implementation

uses
  System.DateUtils;

type
  /// <summary>The aggregated state of one label set, stored by an instrument.</summary>
  TMetricSeries = record
    Labels: TArray<TMetricLabel>;
    Count: Int64;
    Sum: Double;
    Min: Double;
    Max: Double;
    Last: Double;
    FirstSeen: TDateTime;
    LastSeen: TDateTime;
  end;

/// <summary>
///   Formats a number without locale surprises, up to 10 significant decimals.
/// </summary>
function FormatNumber(const AValue: Double): string;
begin
  Result := FormatFloat('0.##########', AValue, TFormatSettings.Invariant);
end;

/// <summary>
///   The recorder behind every instrument kind. One class implements all
///   three typed interfaces and checks the kind on entry, so calling Add on a
///   Gauge is a programming error that surfaces loudly instead of silently
///   corrupting the aggregates.
/// </summary>
type
  TMeterInstrument = class(TInterfacedObject, IInstrument, ICounter, IGauge, IHistogram)
  private
    FKind: TMetricKind;
    FMeterName: string;
    FName: string;
    FDescription: string;
    FUnitName: string;
    FMaxSeries: Integer; // 0 = unbounded
    FLock: TCriticalSection;
    FSeries: TList<TMetricSeries>;
    FDroppedSeries: Int64;
    function FindSeries(const ALabels: TArray<TMetricLabel>): Integer;
    procedure CheckKind(AKind: TMetricKind; const AOp: string);
  protected
    function GetName: string;
    function GetKind: TMetricKind;
    function GetDescription: string;
    function GetUnitName: string;
    function GetSeriesCount: Integer;
    function GetDroppedSeries: Int64;
  public
    constructor Create(AMeterName: string; AKind: TMetricKind;
      const AName, ADescription, AUnitName: string; AMaxSeries: Integer);
    destructor Destroy; override;

    procedure RecordSample(const ALabels: array of string; const AValue: Double);

    { IInstrument }
    function Collect: TArray<TMetricPoint>;
    procedure Clear;

    { ICounter }
    procedure Add(const AValue: Double); overload;
    procedure Add(const AValue: Double; const ALabels: array of string); overload;

    { IGauge }
    procedure SetValue(const AValue: Double); overload;
    procedure SetValue(const AValue: Double; const ALabels: array of string); overload;

    { IHistogram }
    procedure Observe(const AValue: Double); overload;
    procedure Observe(const AValue: Double; const ALabels: array of string); overload;
  end;

  /// <summary>
  ///   Implementation of IMeter: a named registry of instruments.
  /// </summary>
  TMeterImpl = class(TInterfacedObject, IMeter)
  private
    FName: string;
    FMaxSeries: Integer;
    FLock: TCriticalSection;
    FInstruments: TDictionary<string, IInstrument>;
    function CreateInstrument(const AName, ADescription, AUnitName: string;
      AKind: TMetricKind): IInstrument;
  protected
    function GetName: string;
  public
    constructor Create(const AName: string; AMaxSeries: Integer);
    destructor Destroy; override;

    function Collect: TArray<TMetricPoint>;
    procedure Clear;

    function Counter(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): ICounter;
    function Gauge(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IGauge;
    function Histogram(const AName: string; const ADescription: string = '';
      const AUnitName: string = ''): IHistogram;
  end;

  /// <summary>
  ///   Implementation of IMeterProvider.
  /// </summary>
  TMeterProviderImpl = class(TInterfacedObject, IMeterProvider)
  private
    FLock: TCriticalSection;
    FMeters: TDictionary<string, IMeter>;
    FExporters: TList<IMetricExporter>;
    FMaxSeries: Integer;
  protected
    function GetMaxSeriesPerInstrument: Integer;
    procedure SetMaxSeriesPerInstrument(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function GetMeter(const AName: string): IMeter;
    procedure AddExporter(const AExporter: IMetricExporter);
    procedure RemoveExporter(const AExporter: IMetricExporter);
    function Collect: TArray<TMetricPoint>;
    procedure Harvest(const AReset: Boolean = False);
    procedure Clear;
  end;

{ Local helpers ------------------------------------------------------------ }

/// <summary>
///   Turns a key/value string array into the sorted, duplicate free label
///   array a series is keyed by. Labels may come in any order: two recordings
///   carrying the same pairs in different orders hit the same series. When a
///   key appears twice the last occurrence wins.
/// </summary>
function NormalizeLabels(const ALabels: array of string): TArray<TMetricLabel>;
var
  LPairs: TArray<TMetricLabel>;
  LCount, I, J: Integer;
  LKeep: TArray<Boolean>;
  LLastKey: string;
begin
  Result := nil;
  LCount := Length(ALabels);
  if LCount = 0 then
    Exit;
  if Odd(LCount) then
    raise EMetricsError.Create(
      'Metrics labels must be an even number of strings (key, value, key, value...)');

  SetLength(LPairs, LCount div 2);
  for I := 0 to High(LPairs) do
  begin
    LPairs[I].Key := ALabels[I * 2];
    LPairs[I].Value := ALabels[I * 2 + 1];
  end;

  // Keep the LAST occurrence of each key...
  SetLength(LKeep, Length(LPairs));
  for I := 0 to High(LPairs) do
  begin
    LKeep[I] := True;
    for J := I + 1 to High(LPairs) do
      if LPairs[J].Key = LPairs[I].Key then
      begin
        LKeep[I] := False;
        Break;
      end;
  end;
  LCount := 0;
  for I := 0 to High(LPairs) do
    if LKeep[I] then
      Inc(LCount);
  if LCount = 0 then
    Exit;

  SetLength(Result, LCount);
  LCount := 0;
  for I := 0 to High(LPairs) do
    if LKeep[I] then
    begin
      Result[LCount] := LPairs[I];
      Inc(LCount);
    end;

  // ...then sort by key for a canonical order.
  TArray.Sort<TMetricLabel>(Result, TComparer<TMetricLabel>.Construct(
    function(const ALeft, ARight: TMetricLabel): Integer
    begin
      Result := CompareText(ALeft.Key, ARight.Key);
      if Result = 0 then
        Result := CompareText(ALeft.Value, ARight.Value);
    end));

  // Safety net for the canonical-key promise used elsewhere
  LLastKey := '';
  for I := 0 to High(Result) do
    if Result[I].Key = LLastKey then
      raise EMetricsError.Create('Metrics labels contain duplicate keys')
    else
      LLastKey := Result[I].Key;
end;

/// <summary>True when two label arrays hold the same pairs (order ignored).</summary>
function SameLabels(const ALeft, ARight: TArray<TMetricLabel>): Boolean;
var
  I, J: Integer;
  LFound: Boolean;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
  begin
    LFound := False;
    for J := 0 to High(ARight) do
      if (ARight[J].Key = ALeft[I].Key) and (ARight[J].Value = ALeft[I].Value) then
      begin
        LFound := True;
        Break;
      end;
    if not LFound then
      Exit(False);
  end;
  Result := True;
end;

/// <summary>"k=v,k2=v2" used to order points deterministically.</summary>
function LabelsKey(const ALabels: TArray<TMetricLabel>): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ALabels) do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + ALabels[I].Key + '=' + ALabels[I].Value;
  end;
end;

/// <summary>Sort key making a Collect snapshot deterministic between runs.</summary>
function PointKey(const APoint: TMetricPoint): string;
begin
  Result := APoint.Meter + #0 + APoint.Name + #0 +
    IntToStr(Ord(APoint.Kind)) + #0 + LabelsKey(APoint.Labels);
end;

procedure SortPoints(var APoints: TArray<TMetricPoint>);
begin
  TArray.Sort<TMetricPoint>(APoints, TComparer<TMetricPoint>.Construct(
    function(const ALeft, ARight: TMetricPoint): Integer
    begin
      Result := CompareText(PointKey(ALeft), PointKey(ARight));
    end));
end;

function MetricKindToStr(AKind: TMetricKind): string;
begin
  case AKind of
    TMetricKind.Counter:   Result := 'counter';
    TMetricKind.Gauge:     Result := 'gauge';
    TMetricKind.Histogram: Result := 'histogram';
  else
    Result := 'unknown';
  end;
end;

function BuildPointRow(const APoint: TMetricPoint): string;
var
  LWhere, LStats, LLabels: string;
  I: Integer;
begin
  LWhere := APoint.Name;
  if APoint.Meter <> '' then
    LWhere := APoint.Meter + '/' + LWhere;

  case APoint.Kind of
    TMetricKind.Counter:
      LStats := 'sum=' + FormatNumber(APoint.Sum) +
        ' count=' + IntToStr(APoint.Count);
    TMetricKind.Gauge:
      LStats := 'value=' + FormatNumber(APoint.Last);
    TMetricKind.Histogram:
      LStats := 'count=' + IntToStr(APoint.Count) +
        ' sum=' + FormatNumber(APoint.Sum) +
        ' avg=' + FormatNumber(APoint.Sum / APoint.Count) +
        ' min=' + FormatNumber(APoint.Min) +
        ' max=' + FormatNumber(APoint.Max);
  end;

  if Length(APoint.Labels) > 0 then
  begin
    LLabels := '';
    for I := 0 to High(APoint.Labels) do
    begin
      if LLabels <> '' then
        LLabels := LLabels + ' ';
      LLabels := LLabels + APoint.Labels[I].Key + '=' + APoint.Labels[I].Value;
    end;
    LStats := LStats + ' {' + LLabels + '}';
  end;

  Result := MetricKindToStr(APoint.Kind) + ' ' + LWhere + ': ' + LStats;
end;

{ TMetricPoint ---------------------------------------------------------- }

function TMetricPoint.ToString: string;
begin
  Result := BuildPointRow(Self);
end;

{ TMeterInstrument ------------------------------------------------------ }

constructor TMeterInstrument.Create(AMeterName: string; AKind: TMetricKind;
  const AName, ADescription, AUnitName: string; AMaxSeries: Integer);
begin
  inherited Create;
  FMeterName := AMeterName;
  FKind := AKind;
  FName := AName;
  FDescription := ADescription;
  FUnitName := AUnitName;
  FMaxSeries := AMaxSeries;
  FLock := TCriticalSection.Create;
  FSeries := TList<TMetricSeries>.Create;
end;

destructor TMeterInstrument.Destroy;
begin
  FSeries.Free;
  FLock.Free;
  inherited;
end;

procedure TMeterInstrument.CheckKind(AKind: TMetricKind; const AOp: string);
begin
  if FKind <> AKind then
    raise EMetricsError.CreateFmt(
      'Metrics: %s is a %s, not a %s - use the instrument returned by the meter',
      [FName, MetricKindToStr(FKind), MetricKindToStr(AKind)]);
end;

function TMeterInstrument.FindSeries(const ALabels: TArray<TMetricLabel>): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FSeries.Count - 1 do
    if SameLabels(FSeries[LIndex].Labels, ALabels) then
      Exit(LIndex);
  Result := -1;
end;

function TMeterInstrument.GetName: string;
begin
  Result := FName;
end;

function TMeterInstrument.GetKind: TMetricKind;
begin
  Result := FKind;
end;

function TMeterInstrument.GetDescription: string;
begin
  Result := FDescription;
end;

function TMeterInstrument.GetUnitName: string;
begin
  Result := FUnitName;
end;

function TMeterInstrument.GetSeriesCount: Integer;
begin
  FLock.Enter;
  try
    Result := FSeries.Count;
  finally
    FLock.Leave;
  end;
end;

function TMeterInstrument.GetDroppedSeries: Int64;
begin
  FLock.Enter;
  try
    Result := FDroppedSeries;
  finally
    FLock.Leave;
  end;
end;

procedure TMeterInstrument.RecordSample(const ALabels: array of string; const AValue: Double);
var
  LLabels: TArray<TMetricLabel>;
  LIndex: Integer;
  LSeries: TMetricSeries;
begin
  if TMetrics.FShutdown then
    Exit; // process is tearing the metrics subsystem down: drop quietly

  LLabels := NormalizeLabels(ALabels);

  FLock.Enter;
  try
    LIndex := FindSeries(LLabels);
    if LIndex < 0 then
    begin
      if (FMaxSeries > 0) and (FSeries.Count >= FMaxSeries) then
      begin
        Inc(FDroppedSeries);
        Exit; // label budget exhausted: this series is not tracked
      end;
      LSeries := Default(TMetricSeries);
      LSeries.Labels := LLabels;
      LSeries.Count := 1;
      LSeries.Sum := AValue;
      LSeries.Min := AValue;
      LSeries.Max := AValue;
      LSeries.Last := AValue;
      LSeries.FirstSeen := Now;
      LSeries.LastSeen := LSeries.FirstSeen;
      FSeries.Add(LSeries);
      Exit;
    end;

    LSeries := FSeries[LIndex];
    Inc(LSeries.Count);
    LSeries.Sum := LSeries.Sum + AValue;
    if AValue < LSeries.Min then
      LSeries.Min := AValue;
    if AValue > LSeries.Max then
      LSeries.Max := AValue;
    LSeries.Last := AValue;
    LSeries.LastSeen := Now;
    FSeries[LIndex] := LSeries;
  finally
    FLock.Leave;
  end;
end;

function TMeterInstrument.Collect: TArray<TMetricPoint>;
var
  LCount, I: Integer;
  LSeries: TMetricSeries;
begin
  FLock.Enter;
  try
    LCount := FSeries.Count;
    SetLength(Result, LCount);
    for I := 0 to LCount - 1 do
    begin
      LSeries := FSeries[I];
      Result[I].Meter := FMeterName;
      Result[I].Name := FName;
      Result[I].Kind := FKind;
      Result[I].Description := FDescription;
      Result[I].UnitName := FUnitName;
      Result[I].Labels := LSeries.Labels;
      Result[I].Count := LSeries.Count;
      Result[I].Sum := LSeries.Sum;
      Result[I].Min := LSeries.Min;
      Result[I].Max := LSeries.Max;
      Result[I].Last := LSeries.Last;
      Result[I].FirstSeen := LSeries.FirstSeen;
      Result[I].LastSeen := LSeries.LastSeen;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMeterInstrument.Clear;
begin
  FLock.Enter;
  try
    FSeries.Clear;
    FDroppedSeries := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TMeterInstrument.Add(const AValue: Double);
begin
  Add(AValue, []);
end;

procedure TMeterInstrument.Add(const AValue: Double;
  const ALabels: array of string);
begin
  CheckKind(TMetricKind.Counter, 'Add');
  if AValue < 0 then
    raise EMetricsError.CreateFmt(
      'Metrics: cannot decrement counter %s with Add(%s) - counters are monotonic; use a gauge to go down',
      [FName, FormatNumber(AValue)]);
  RecordSample(ALabels, AValue);
end;

procedure TMeterInstrument.SetValue(const AValue: Double);
begin
  SetValue(AValue, []);
end;

procedure TMeterInstrument.SetValue(const AValue: Double;
  const ALabels: array of string);
begin
  CheckKind(TMetricKind.Gauge, 'SetValue');
  RecordSample(ALabels, AValue);
end;

procedure TMeterInstrument.Observe(const AValue: Double);
begin
  Observe(AValue, []);
end;

procedure TMeterInstrument.Observe(const AValue: Double;
  const ALabels: array of string);
begin
  CheckKind(TMetricKind.Histogram, 'Observe');
  RecordSample(ALabels, AValue);
end;

{ TMeterImpl ------------------------------------------------------------ }

constructor TMeterImpl.Create(const AName: string; AMaxSeries: Integer);
begin
  inherited Create;
  FName := AName;
  FMaxSeries := AMaxSeries;
  FLock := TCriticalSection.Create;
  FInstruments := TDictionary<string, IInstrument>.Create;
end;

destructor TMeterImpl.Destroy;
begin
  FInstruments.Free;
  FLock.Free;
  inherited;
end;

function TMeterImpl.GetName: string;
begin
  Result := FName;
end;

function TMeterImpl.CreateInstrument(const AName, ADescription,
  AUnitName: string; AKind: TMetricKind): IInstrument;
var
  LExisting: IInstrument;
begin
  if AName.Trim.IsEmpty then
    raise EMetricsError.Create('Metrics: instrument name cannot be empty');

  FLock.Enter;
  try
    if FInstruments.TryGetValue(AName, LExisting) then
    begin
      if LExisting.Kind <> AKind then
        raise EMetricsError.CreateFmt(
          'Metrics: instrument name "%s" of meter "%s" is already a %s, not a %s',
          [AName, FName, MetricKindToStr(LExisting.Kind), MetricKindToStr(AKind)]);
      Result := LExisting; // cached: first creation wins for description/unit
      Exit;
    end;

    Result := TMeterInstrument.Create(FName, AKind, AName, ADescription,
      AUnitName, FMaxSeries);
    FInstruments.Add(AName, Result);
  finally
    FLock.Leave;
  end;
end;

function TMeterImpl.Counter(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): ICounter;
begin
  Result := CreateInstrument(AName, ADescription, AUnitName, TMetricKind.Counter) as ICounter;
end;

function TMeterImpl.Gauge(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): IGauge;
begin
  Result := CreateInstrument(AName, ADescription, AUnitName, TMetricKind.Gauge) as IGauge;
end;

function TMeterImpl.Histogram(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): IHistogram;
begin
  Result := CreateInstrument(AName, ADescription, AUnitName, TMetricKind.Histogram) as IHistogram;
end;

function TMeterImpl.Collect: TArray<TMetricPoint>;
var
  LInstruments: TArray<IInstrument>;
  LInstrument: IInstrument;
  LPoints: TArray<TMetricPoint>;
  LList: TList<TMetricPoint>;
begin
  FLock.Enter;
  try
    LInstruments := FInstruments.Values.ToArray;
  finally
    FLock.Leave;
  end;

  // Collected outside the meter lock: instruments lock themselves
  LList := TList<TMetricPoint>.Create;
  try
    for LInstrument in LInstruments do
    begin
      LPoints := LInstrument.Collect;
      LList.AddRange(LPoints);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TMeterImpl.Clear;
var
  LInstruments: TArray<IInstrument>;
  LInstrument: IInstrument;
begin
  FLock.Enter;
  try
    LInstruments := FInstruments.Values.ToArray;
  finally
    FLock.Leave;
  end;
  for LInstrument in LInstruments do
    LInstrument.Clear;
end;

{ TMeterProviderImpl ---------------------------------------------------- }

constructor TMeterProviderImpl.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FMeters := TDictionary<string, IMeter>.Create;
  FExporters := TList<IMetricExporter>.Create;
  FMaxSeries := TMetrics.DEFAULT_MAX_SERIES;
end;

destructor TMeterProviderImpl.Destroy;
begin
  FExporters.Free;
  FMeters.Free;
  FLock.Free;
  inherited;
end;

function TMeterProviderImpl.GetMeter(const AName: string): IMeter;
var
  LMeter: IMeter;
begin
  FLock.Enter;
  try
    if FMeters.TryGetValue(AName, LMeter) then
      Exit(LMeter);

    LMeter := TMeterImpl.Create(AName, FMaxSeries);
    FMeters.Add(AName, LMeter);
    Result := LMeter;
  finally
    FLock.Leave;
  end;
end;

function TMeterProviderImpl.GetMaxSeriesPerInstrument: Integer;
begin
  FLock.Enter;
  try
    Result := FMaxSeries;
  finally
    FLock.Leave;
  end;
end;

procedure TMeterProviderImpl.SetMaxSeriesPerInstrument(AValue: Integer);
begin
  if AValue < 0 then
    raise EMetricsError.Create('Metrics: MaxSeriesPerInstrument cannot be negative');
  FLock.Enter;
  try
    FMaxSeries := AValue;
  finally
    FLock.Leave;
  end;
end;

procedure TMeterProviderImpl.AddExporter(const AExporter: IMetricExporter);
var
  I: Integer;
begin
  if not Assigned(AExporter) then
    raise EMetricsError.Create('Metrics: cannot register a nil exporter');
  FLock.Enter;
  try
    for I := 0 to FExporters.Count - 1 do
      if Pointer(FExporters[I]) = Pointer(AExporter) then
        Exit; // already registered
    FExporters.Add(AExporter);
  finally
    FLock.Leave;
  end;
end;

procedure TMeterProviderImpl.RemoveExporter(const AExporter: IMetricExporter);
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := FExporters.Count - 1 downto 0 do
      if Pointer(FExporters[I]) = Pointer(AExporter) then
        FExporters.Delete(I);
  finally
    FLock.Leave;
  end;
end;

function TMeterProviderImpl.Collect: TArray<TMetricPoint>;
var
  LMeters: TArray<IMeter>;
  LMeter: IMeter;
  LPoints: TArray<TMetricPoint>;
  LList: TList<TMetricPoint>;
begin
  FLock.Enter;
  try
    LMeters := FMeters.Values.ToArray;
  finally
    FLock.Leave;
  end;

  LList := TList<TMetricPoint>.Create;
  try
    for LMeter in LMeters do
    begin
      LPoints := LMeter.Collect;
      LList.AddRange(LPoints);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;

  // Sorted outside the lock, on a copy nobody else can see: harvests taken at
  // different moments line up point by point.
  SortPoints(Result);
end;

procedure TMeterProviderImpl.Harvest(const AReset: Boolean = False);
var
  LPoints: TArray<TMetricPoint>;
  LExporters: TArray<IMetricExporter>;
  LExporter: IMetricExporter;
begin
  LPoints := Collect;

  FLock.Enter;
  try
    LExporters := FExporters.ToArray;
  finally
    FLock.Leave;
  end;

  // Outside the lock: an exporter runs user code that may record more metrics
  for LExporter in LExporters do
    LExporter.Export(LPoints);

  if AReset then
    Clear;
end;

procedure TMeterProviderImpl.Clear;
var
  LMeters: TArray<IMeter>;
  LMeter: IMeter;
begin
  FLock.Enter;
  try
    LMeters := FMeters.Values.ToArray;
  finally
    FLock.Leave;
  end;
  for LMeter in LMeters do
    LMeter.Clear;
end;

{ TMetrics -------------------------------------------------------------- }

class constructor TMetrics.Create;
begin
  FDefaultLock := TCriticalSection.Create;
end;

class destructor TMetrics.Destroy;
begin
  // The default provider must go away before the lock that guards it.
  FShutdown := True;
  FDefault := nil;
  FDefaultLock.Free;
  FDefaultLock := nil;
end;

class function TMetrics.Default: IMeterProvider;
begin
  if FShutdown then
    raise EMetricsError.Create(
      'Metrics: the MCPConnect.Metrics subsystem has been shut down');
  FDefaultLock.Enter;
  try
    if FDefault = nil then
      FDefault := CreateProvider;
    Result := FDefault;
  finally
    FDefaultLock.Leave;
  end;
end;

class function TMetrics.CreateProvider: IMeterProvider;
begin
  Result := TMeterProviderImpl.Create;
end;

class function TMetrics.Meter(const AName: string = ''): IMeter;
begin
  Result := Default.GetMeter(AName);
end;

class function TMetrics.Counter(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): ICounter;
begin
  Result := Meter.Counter(AName, ADescription, AUnitName);
end;

class function TMetrics.Gauge(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): IGauge;
begin
  Result := Meter.Gauge(AName, ADescription, AUnitName);
end;

class function TMetrics.Histogram(const AName: string;
  const ADescription: string = ''; const AUnitName: string = ''): IHistogram;
begin
  Result := Meter.Histogram(AName, ADescription, AUnitName);
end;

class function TMetrics.Collect: TArray<TMetricPoint>;
begin
  Result := Default.Collect;
end;

class procedure TMetrics.Harvest(const AReset: Boolean = False);
begin
  Default.Harvest(AReset);
end;

class procedure TMetrics.Clear;
begin
  Default.Clear;
end;

class procedure TMetrics.AddExporter(const AExporter: IMetricExporter);
begin
  Default.AddExporter(AExporter);
end;

class procedure TMetrics.RemoveExporter(const AExporter: IMetricExporter);
begin
  Default.RemoveExporter(AExporter);
end;

end.
