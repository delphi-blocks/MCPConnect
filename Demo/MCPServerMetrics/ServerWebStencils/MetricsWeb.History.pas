unit MetricsWeb.History;

{
  ==============================================================================
   The dashboard's own history

   The metrics subsystem keeps no time series: a report says what the numbers
   are now, not what they were. Because this dashboard runs in the same process
   as the metrics, it can keep its own - a bounded ring of samples taken on a
   timer - which is what the throughput and latency charts, the gauge
   sparklines and the per-instrument counter and histogram cards are drawn from.

   That is the one thing a server-rendered dashboard does better than a polling
   one: history is taken at a steady cadence whether or not anybody has a browser
   open, and it needs no store and no second process.

   Samples come from a report built by Server.Metrics.Document, so their counters
   are already the folded, monotonic ones: a reset the host performs cannot make
   a rate go negative.

   A sample is a plain value record - no pointers - so reading the history can
   never race with the trim that drops the oldest entry. Every instrument also
   gets a *track* per field it has - a gauge its last value, a counter its
   running total, a histogram its sum and its count - kept beside the samples
   as one list each, in lockstep with them: a rate is a difference between two
   totals, and a histogram's recent average a difference of sums over a
   difference of counts, so the totals are what has to be remembered.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,

  Server.Metrics.Document;

type
  /// <summary>One moment of the numbers the dashboard charts. A value: copy it freely.</summary>
  TMetricsSample = record
    At: TDateTime;

    /// <summary>Folded totals of the counters the KPI cards and charts are about.</summary>
    Requests: Double;
    Errors: Double;
    ToolCalls: Double;

    /// <summary>Requests being served at that moment (a gauge: last value).</summary>
    InFlight: Double;

    /// <summary>The request.duration_ms instrument, aggregated over its labels.</summary>
    LatencyAvg: Double;
    LatencyMax: Double;
    LatencyCount: Double;

    SeriesCount: Integer;
  end;

  /// <summary>
  ///   A bounded ring of samples, thread safe: the timer that fills it runs on
  ///   the main thread while a request thread may be reading it.
  /// </summary>
  TMetricsHistory = class
  private
    FLock: TCriticalSection;
    FSamples: TList<TMetricsSample>;
    /// <summary>
    ///   One list per instrument field, keyed "meter/name|field", always as long
    ///   as FSamples.
    /// </summary>
    FTracks: TObjectDictionary<string, TList<Double>>;
    FCapacity: Integer;
    procedure Trim;
    class function TrackKey(const AMeter, AName, AField: string): string; static;
  public
    constructor Create(ACapacity: Integer = 300);
    destructor Destroy; override;

    /// <summary>Appends the report's numbers as one sample, dropping the oldest.</summary>
    procedure Add(const AReport: TMetricsReport);

    /// <summary>The samples, oldest first.</summary>
    function Snapshot: TArray<TMetricsSample>;

    /// <summary>One gauge's values across the retained samples, oldest first, no gaps.</summary>
    function GaugeSeries(const AMeter, AName: string): TArray<Double>;

    /// <summary>
    ///   Fields of one instrument - TRACK_LAST, TRACK_SUM, TRACK_COUNT - one array
    ///   per field in the order asked, aligned with the sample times returned
    ///   beside them. Everything is read under one lock, so a sample added in
    ///   between cannot shift one array against another. A value is NaN for the
    ///   samples taken before the instrument existed; a field never recorded
    ///   answers an empty array.
    /// </summary>
    function Track(const AMeter, AName: string; const AFields: array of string;
      out ATimes: TArray<TDateTime>): TArray<TArray<Double>>;

    /// <summary>The request.duration_ms level across the retained samples.</summary>
    function LatencySeries(const AField: string): TArray<Double>;

    /// <summary>Seconds between the two latest samples, 0 when there are fewer than two.</summary>
    function SampleIntervalSeconds: Double;

    property Capacity: Integer read FCapacity;
  end;

const
  /// <summary>A gauge's value.</summary>
  TRACK_LAST = 'last';

  /// <summary>A counter's running total, or a histogram's sum of observations.</summary>
  TRACK_SUM = 'sum';

  /// <summary>A histogram's number of observations.</summary>
  TRACK_COUNT = 'count';

implementation

uses
  System.DateUtils,
  System.Math,

  MCPConnect.Metrics;

const
  METER_MCP = 'mcp.server';

{ TMetricsHistory }

constructor TMetricsHistory.Create(ACapacity: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSamples := TList<TMetricsSample>.Create;
  FTracks := TObjectDictionary<string, TList<Double>>.Create([doOwnsValues]);
  FCapacity := ACapacity;
  if FCapacity < 2 then
    FCapacity := 2;
end;

destructor TMetricsHistory.Destroy;
begin
  FTracks.Free;
  FSamples.Free;
  FLock.Free;
  inherited;
end;

class function TMetricsHistory.TrackKey(const AMeter, AName, AField: string): string;
begin
  Result := AMeter + '/' + AName + '|' + AField;
end;

procedure TMetricsHistory.Trim;
var
  LTrack: TList<Double>;
begin
  while FSamples.Count > FCapacity do
  begin
    FSamples.Delete(0);
    // Every track is kept exactly as long as the sample list, so the two stay
    // aligned by dropping the same index from each.
    for LTrack in FTracks.Values do
      if LTrack.Count > 0 then
        LTrack.Delete(0);
  end;
end;

procedure TMetricsHistory.Add(const AReport: TMetricsReport);
var
  LSample: TMetricsSample;
  LInstrument: TMetricInstrumentRollup;
  LPoint: TMetricPoint;
  LCurrent: TDictionary<string, Double>;
  LTrack: TList<Double>;
  LKey: string;
  LValue: Double;
begin
  LSample := Default(TMetricsSample);
  LSample.At := AReport.GeneratedAt;

  for LPoint in AReport.Cumulative do
  begin
    if LPoint.Name = 'request.count' then
      LSample.Requests := LSample.Requests + LPoint.Sum
    else if LPoint.Name = 'request.errors' then
      LSample.Errors := LSample.Errors + LPoint.Sum
    else if LPoint.Name = 'tool.count' then
      LSample.ToolCalls := LSample.ToolCalls + LPoint.Sum
    else if (LPoint.Name = 'request.in_flight') and (LPoint.Meter = METER_MCP) then
      LSample.InFlight := LPoint.Last;
  end;

  LInstrument := AReport.Instrument(METER_MCP, 'request.duration_ms');
  LSample.LatencyAvg := AverageOf2(LInstrument.Sum, LInstrument.Count);
  LSample.LatencyMax := LInstrument.Max;
  LSample.LatencyCount := LInstrument.Count;
  LSample.SeriesCount := AReport.SeriesCount;

  LCurrent := TDictionary<string, Double>.Create;
  try
    for LInstrument in AReport.Instruments do
      case LInstrument.Kind of
        TMetricKind.Gauge:
          LCurrent.AddOrSetValue(TrackKey(LInstrument.Meter, LInstrument.Name, TRACK_LAST),
            LInstrument.Last);
        TMetricKind.Counter:
          LCurrent.AddOrSetValue(TrackKey(LInstrument.Meter, LInstrument.Name, TRACK_SUM),
            LInstrument.Sum);
        TMetricKind.Histogram:
          begin
            LCurrent.AddOrSetValue(TrackKey(LInstrument.Meter, LInstrument.Name, TRACK_SUM),
              LInstrument.Sum);
            LCurrent.AddOrSetValue(TrackKey(LInstrument.Meter, LInstrument.Name, TRACK_COUNT),
              LInstrument.Count);
          end;
      end;

    FLock.Enter;
    try
      // A track that has appeared for the first time gets the rows it missed
      // filled in, so every list stays as long as the sample list.
      for LKey in LCurrent.Keys do
        if not FTracks.ContainsKey(LKey) then
        begin
          LTrack := TList<Double>.Create;
          while LTrack.Count < FSamples.Count do
            LTrack.Add(NaN);
          FTracks.Add(LKey, LTrack);
        end;

      // A track missing from this report (a series that was cleared) keeps its
      // previous value: the line is about the shape of the window, and a gap
      // would break it. For a total that is also the truth - nothing happened.
      for LKey in FTracks.Keys do
      begin
        LTrack := FTracks[LKey];
        if LCurrent.TryGetValue(LKey, LValue) then
          LTrack.Add(LValue)
        else if LTrack.Count > 0 then
          LTrack.Add(LTrack[LTrack.Count - 1])
        else
          LTrack.Add(NaN);
      end;

      FSamples.Add(LSample);
      Trim;
    finally
      FLock.Leave;
    end;
  finally
    LCurrent.Free;
  end;
end;

function TMetricsHistory.Snapshot: TArray<TMetricsSample>;
begin
  FLock.Enter;
  try
    Result := FSamples.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TMetricsHistory.GaugeSeries(const AMeter, AName: string): TArray<Double>;
var
  LGauge: TList<Double>;
  LValue: Double;
  LResult: TList<Double>;
begin
  LResult := TList<Double>.Create;
  try
    FLock.Enter;
    try
      if FTracks.TryGetValue(TrackKey(AMeter, AName, TRACK_LAST), LGauge) then
        for LValue in LGauge do
          if not IsNan(LValue) then
            LResult.Add(LValue);
    finally
      FLock.Leave;
    end;
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function TMetricsHistory.Track(const AMeter, AName: string; const AFields: array of string;
  out ATimes: TArray<TDateTime>): TArray<TArray<Double>>;
var
  LTrack: TList<Double>;
  LIndex: Integer;
begin
  SetLength(Result, Length(AFields));
  FLock.Enter;
  try
    SetLength(ATimes, FSamples.Count);
    for LIndex := 0 to FSamples.Count - 1 do
      ATimes[LIndex] := FSamples[LIndex].At;

    for LIndex := 0 to High(AFields) do
      if FTracks.TryGetValue(TrackKey(AMeter, AName, AFields[LIndex]), LTrack) then
        Result[LIndex] := LTrack.ToArray
      else
        Result[LIndex] := nil;
  finally
    FLock.Leave;
  end;
end;

function TMetricsHistory.LatencySeries(const AField: string): TArray<Double>;
var
  LSample: TMetricsSample;
  LResult: TList<Double>;
begin
  LResult := TList<Double>.Create;
  try
    FLock.Enter;
    try
      for LSample in FSamples do
      begin
        if SameText(AField, 'avg') then
          LResult.Add(LSample.LatencyAvg)
        else if SameText(AField, 'max') then
          LResult.Add(LSample.LatencyMax)
        else if SameText(AField, 'count') then
          LResult.Add(LSample.LatencyCount);
      end;
    finally
      FLock.Leave;
    end;
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function TMetricsHistory.SampleIntervalSeconds: Double;
var
  LCount: Integer;
begin
  FLock.Enter;
  try
    LCount := FSamples.Count;
    if LCount < 2 then
      Exit(0);
    Result := (FSamples[LCount - 1].At - FSamples[LCount - 2].At) * SecsPerDay;
  finally
    FLock.Leave;
  end;
end;

end.
