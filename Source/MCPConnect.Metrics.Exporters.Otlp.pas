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
///   Pushes MCPConnect.Metrics harvests to an OpenTelemetry collector (or any
///   OTLP backend) over OTLP/HTTP with the JSON encoding: every harvest becomes
///   one ExportMetricsServiceRequest POSTed to the /v1/metrics endpoint.
///   <code>
///   var LExporter: IMetricExporter;
///   begin
///     LExporter := TOtlpMetricExporter.Create('http://collector:4318/v1/metrics');
///     TMetrics.AddExporter(LExporter);
///     ...
///     TMetrics.Harvest(True);   // one POST per tick
///   end;
///   </code>
///   or, configured the OpenTelemetry way (OTEL_EXPORTER_OTLP_ENDPOINT,
///   OTEL_EXPORTER_OTLP_HEADERS, OTEL_SERVICE_NAME...):
///   <code>
///   TMetrics.AddExporter(TOtlpMetricExporter.FromEnvironment);
///   </code>
///
///   Temporality is always CUMULATIVE. The exporter keeps a running total per
///   series, so it does not matter whether the host harvests with or without
///   a reset (or mixes the two): what the collector receives is the total since
///   the series was first seen, with a start time that never moves. The same
///   running total is what makes delivery forgiving - a failed POST loses
///   nothing, the next harvest carries everything.
///
///   Mapping: a Counter is a monotonic "sum", a Gauge a "gauge" (last value,
///   sent only when the harvest carries it), a Histogram a "histogram" with
///   count/sum/min/max and a single bucket (the subsystem keeps no bounds).
///   Labels become string attributes, each meter an instrumentation scope.
///
///   Export never raises because of the network or the collector: a failure is
///   counted, kept in LastError and reported to OnError, since an exception
///   out of an exporter would stop Harvest from reaching the exporters after it
///   and from resetting. Harvests must not run concurrently on one exporter.
///
///   ## When the collector is not there
///
///   Two behaviours exist for the endpoint that is down, because a dead
///   collector is a normal thing for a metrics pipeline to live with:
///
///   * **Backoff** (on by default): after a failed export the next attempt waits
///     5 s, then 10, 20 ... up to 5 minutes, and a success clears it. A
///     connection that cannot be established comes with no Retry-After, so
///     without this the exporter would reconnect on every harvest and report the
///     same failure each time. Skips are counted in SkippedCount and are not
///     reported - the failure that started the wait already was. Tune it with
///     BackoffBaseMs / MaxBackoffMs, or turn it off with Backoff := False.
///   * **Async** (off by default): with Async set, the POST runs on a thread of
///     the exporter's own and Harvest returns as soon as the body is built, so a
///     collector that is slow - or one whose address blackholes the connection
///     for the whole timeout - cannot hold up the thread that harvests. That
///     thread is the UI thread in a VCL host and a request thread in a web host,
///     which is why a host that harvests on a timer, or from a request, wants
///     Async on. Only the newest body waits while a POST is in flight
///     (DroppedCount counts the ones replaced, and they carry nothing the newer
///     one does not), and Flush waits for the queue to drain when a caller wants
///     the result before reading the counters.
///
///   The synchronous default keeps the counting immediate: with Async off, the
///   counters already describe the attempt when Export returns.
/// </summary>
unit MCPConnect.Metrics.Exporters.Otlp;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,

  Neon.Core.Attributes,
  Neon.Core.Nullables,

  MCPConnect.Metrics;

const
  /// <summary>
  ///   Where a local collector listens for OTLP/HTTP metrics.
  /// </summary>
  OTLP_DEFAULT_ENDPOINT = 'http://localhost:4318/v1/metrics';

  /// <summary>
  ///   The signal path appended to OTEL_EXPORTER_OTLP_ENDPOINT.
  /// </summary>
  OTLP_METRICS_PATH = '/v1/metrics';

  /// <summary>
  ///   Scope name of the default meter, whose own name is empty.
  /// </summary>
  OTLP_DEFAULT_SCOPE = 'MCPConnect.Metrics';

  /// <summary>
  ///   Connection and response timeout, in milliseconds.
  /// </summary>
  OTLP_DEFAULT_TIMEOUT = 10000;

  /// <summary>
  ///   AggregationTemporality.AGGREGATION_TEMPORALITY_CUMULATIVE.
  /// </summary>
  OTLP_TEMPORALITY_CUMULATIVE = 2;

  /// <summary>
  ///   Longest Retry-After honoured, in seconds.
  /// </summary>
  OTLP_MAX_RETRY_AFTER = 3600;

  /// <summary>
  ///   First delay after a failed export, when the connect-failure backoff is on:
  ///   the next attempt is not made before it has elapsed. Each further failure
  ///   doubles it, up to OTLP_MAX_BACKOFF_MS. A success clears it.
  /// </summary>
  OTLP_DEFAULT_BACKOFF_MS = 5000;

  /// <summary>
  ///   Longest gap between attempts. A collector that is down tends to stay down
  ///   for a while, and retrying a dead endpoint on every harvest costs a connect
  ///   attempt, a warning line and - on a blackholed address rather than a
  ///   refused one - a full timeout each time.
  /// </summary>
  OTLP_MAX_BACKOFF_MS = 300000;

type
  /// <summary>
  ///   Raised on configuration errors (never on a failed export).
  /// </summary>
  EOtlpExporterError = class(EMetricsError);

  /// <summary>
  ///   One HTTP header sent with every export.
  /// </summary>
  TOtlpHeader = record
    Name: string;
    Value: string;
  end;

  /// <summary>What the transport hands back to the exporter.</summary>
  TOtlpHttpResponse = record
    StatusCode: Integer;
    Body: string;
    /// <summary>
    ///   The raw Retry-After header, '' when absent.
    /// </summary>
    RetryAfter: string;
  end;

  /// <summary>
  ///   The HTTP POST behind the exporter, an interface so tests (or a host
  ///   with its own HTTP stack) can replace it. Raises on a transport failure
  ///   (DNS, connection refused, timeout); any HTTP status is a response.
  /// </summary>
  IOtlpHttpSender = interface
    ['{C7A1F0E2-3B5D-4C8E-9F16-7D2A4B6E8C31}']
    function Post(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; ATimeoutMs: Integer): TOtlpHttpResponse;
  end;

  /// <summary>
  ///   IOtlpHttpSender over System.Net.HttpClient. Redirects are not followed:
  ///   the headers carry credentials, and a redirect would hand them to a URL
  ///   nobody configured.
  /// </summary>
  TOtlpHttpClientSender = class(TInterfacedObject, IOtlpHttpSender)
  public
    function Post(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; ATimeoutMs: Integer): TOtlpHttpResponse;
  end;

  /// <summary>
  ///   One body waiting to be POSTed, when the exporter runs its delivery on its
  ///   own thread (Async). Built by the harvest, sent by TOtlpExportWorker.
  /// </summary>
  /// <remarks>
  ///   Exposed only because a private field of the exporter has to name it; it is
  ///   not part of the exporter's interface and a host never touches one.
  /// </remarks>
  TOtlpPendingExport = class
  public
    Url: string;
    Headers: TArray<TOtlpHeader>;
    Body: TBytes;
    /// <summary>The harvest time this body was taken at, for the data point times.</summary>
    Time: TDateTime;
    TimeoutMs: Integer;
    constructor Create(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer);
  end;

  { OTLP entities -------------------------------------------------------------

    The subset of opentelemetry.proto.collector.metrics.v1 this exporter
    sends, as records Neon serializes with the camelCase field names of the
    OTLP JSON encoding. int64/fixed64 fields are strings, as that encoding
    writes them; enums are integers. }

  /// <summary>
  ///   A double that serializes NaN and the infinities as the proto3 JSON
  ///   strings ("NaN", "Infinity", "-Infinity") instead of invalid numbers.
  /// </summary>
  TOtlpDouble = type Double;

  /// <summary>
  ///   AnyValue, restricted to the string case (labels are strings).
  /// </summary>
  TOtlpAnyValue = record
    StringValue: string;
  end;

  /// <summary>
  ///   KeyValue: one attribute.
  /// </summary>
  TOtlpKeyValue = record
    Key: string;
    Value: TOtlpAnyValue;
    class function Create(const AKey, AValue: string): TOtlpKeyValue; static;
  end;

  TOtlpAttributes = TArray<TOtlpKeyValue>;

  /// <summary>
  ///   NumberDataPoint, as a double (counters and gauges).
  /// </summary>
  TOtlpNumberDataPoint = record

    [NeonInclude(IncludeIf.NotEmpty)] Attributes: TOtlpAttributes;

    /// <summary>
    ///   Absent on a gauge: an instantaneous reading has no window.
    /// </summary>
    [NeonInclude(IncludeIf.NotEmpty)] StartTimeUnixNano: string;

    TimeUnixNano: string;
    AsDouble: TOtlpDouble;
  end;

  /// <summary>
  ///   HistogramDataPoint.
  /// </summary>
  TOtlpHistogramDataPoint = record
    [NeonInclude(IncludeIf.NotEmpty)] Attributes: TOtlpAttributes;
    StartTimeUnixNano: string;
    TimeUnixNano: string;
    Count: string;
    Sum: TOtlpDouble;
    /// <summary>
    ///   One entry more than ExplicitBounds.
    /// </summary>
    BucketCounts: TArray<string>;
    ExplicitBounds: TArray<Double>;
    Min: TOtlpDouble;
    Max: TOtlpDouble;
  end;

  TOtlpSum = record
    AggregationTemporality: Integer;
    IsMonotonic: Boolean;
    DataPoints: TArray<TOtlpNumberDataPoint>;
  end;

  TOtlpGauge = record
    DataPoints: TArray<TOtlpNumberDataPoint>;
  end;

  TOtlpHistogram = record
    AggregationTemporality: Integer;
    DataPoints: TArray<TOtlpHistogramDataPoint>;
  end;

  /// <summary>
  ///   Metric. Exactly one of Sum, Gauge and Histogram has a value (the
  ///   protobuf oneof "data"); the others are left out of the JSON.
  /// </summary>
  TOtlpMetric = record
    Name: string;
    [NeonInclude(IncludeIf.NotEmpty)] Description: string;
    [NeonProperty('unit'), NeonInclude(IncludeIf.NotEmpty)] UnitName: string;
    [NeonInclude(IncludeIf.NotNull)] Sum: Nullable<TOtlpSum>;
    [NeonInclude(IncludeIf.NotNull)] Gauge: Nullable<TOtlpGauge>;
    [NeonInclude(IncludeIf.NotNull)] Histogram: Nullable<TOtlpHistogram>;
  end;

  TOtlpInstrumentationScope = record
    Name: string;
  end;

  TOtlpScopeMetrics = record
    Scope: TOtlpInstrumentationScope;
    Metrics: TArray<TOtlpMetric>;
  end;

  TOtlpResource = record
    Attributes: TOtlpAttributes;
  end;

  TOtlpResourceMetrics = record
    Resource: TOtlpResource;
    ScopeMetrics: TArray<TOtlpScopeMetrics>;
  end;

  /// <summary>
  ///   ExportMetricsServiceRequest: the body of POST /v1/metrics.
  /// </summary>
  TOtlpExportMetricsServiceRequest = record
    ResourceMetrics: TArray<TOtlpResourceMetrics>;
  end;

  /// <summary>
  ///   The OTLP/HTTP+JSON exporter. Configure it before registering it on a
  ///   provider; the statistics may be read from any thread.
  /// </summary>
  TOtlpMetricExporter = class(TInterfacedObject, IMetricExporter, IMetricHarvestExporter)
  private type
    /// <summary>
    ///   One counter or histogram series: Base is what was folded in by the
    ///   resets so far, Live the latest snapshot taken since the last reset.
    ///   The cumulative value is the two merged.
    /// </summary>
    TSeriesState = record
      Base: TMetricPoint;
      HasBase: Boolean;
      Live: TMetricPoint;
      HasLive: Boolean;
    end;
  private
    FLock: TCriticalSection;
    FSender: IOtlpHttpSender;
    FEndpoint: string;
    FHeaders: TList<TOtlpHeader>;
    FResource: TList<TMetricLabel>;
    FDefaultScope: string;
    FCompression: Boolean;
    FTimeout: Integer;
    FOnError: TProc<string>;
    FSeries: TDictionary<string, TSeriesState>;
    FRetryNotBefore: TDateTime;
    FSentCount: Int64;
    FFailedCount: Int64;
    FSkippedCount: Int64;
    FDroppedCount: Int64;
    FLastError: string;

    // Delivery: synchronous on the caller's thread, or handed to FWorker.
    FAsync: Boolean;
    FWorker: TThread;
    FWorkerSignal: TEvent;
    FPending: TOtlpPendingExport;
    FPosting: Boolean;

    // Connect-failure backoff.
    FBackoff: Boolean;
    FBackoffBaseMs: Integer;
    FMaxBackoffMs: Integer;
    FConsecutiveFailures: Integer;

    function Accumulate(const APoints: TArray<TMetricPoint>; AReset: Boolean): TArray<TMetricPoint>;
    function BuildHeaders: TArray<TOtlpHeader>;
    function HandleResponse(const AUrl: string; const AResponse: TOtlpHttpResponse; const ATime: TDateTime): string;
    /// <summary>
    ///   Records a failed export and, when the backoff is on, pushes the next
    ///   attempt out: 5 s, 10 s, 20 s ... capped. Returns AMessage, so a caller
    ///   can hand it straight to NotifyError.
    /// </summary>
    function ScheduleFailure(const AMessage: string; const ANow: TDateTime): string;
    /// <summary>
    ///   Sends one body on the calling thread and handles the answer. Never
    ///   raises: a transport failure comes back as the message.
    /// </summary>
    function SendNow(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer): string;
    /// <summary>
    ///   Hands a built body to the sender: on this thread, or to the delivery
    ///   thread when Async is set. Returns the error message in the first case,
    ///   '' in the second (the worker reports its own).
    /// </summary>
    function Deliver(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer): string;
    procedure EnsureWorker;
    procedure StopWorker;
    /// <summary>Nudges the delivery thread: a body is waiting.</summary>
    procedure WakeWorker;
    /// <summary>Takes the pending body, marking a POST as in flight. Called by the worker.</summary>
    function TakePending: TOtlpPendingExport;
    /// <summary>True while a body is queued or being POSTed.</summary>
    function HasWork: Boolean;
    procedure NotifyError(const AMessage: string);
    function GetSentCount: Int64;
    function GetFailedCount: Int64;
    function GetSkippedCount: Int64;
    function GetDroppedCount: Int64;
    function GetConsecutiveFailures: Integer;
    function GetLastError: string;
    function GetHeaders: TArray<TOtlpHeader>;
    function GetResourceAttributes: TArray<TMetricLabel>;
  public
    /// <summary>
    ///   AEndpoint is the full metrics URL, /v1/metrics included. Sets
    ///   service.name to unknown_service:&lt;executable&gt;, as OpenTelemetry does.
    /// </summary>
    constructor Create(const AEndpoint: string = OTLP_DEFAULT_ENDPOINT); overload;

    /// <summary>
    ///   Same, with the transport replaced.
    /// </summary>
    constructor Create(const AEndpoint: string; const ASender: IOtlpHttpSender); overload;
    destructor Destroy; override;

    /// <summary>
    ///   An exporter configured from the OTEL_* environment variables:
    ///   OTEL_EXPORTER_OTLP_[METRICS_]ENDPOINT, _HEADERS, _PROTOCOL (must be
    ///   http/json when set), _COMPRESSION, _TIMEOUT, plus OTEL_SERVICE_NAME and
    ///   OTEL_RESOURCE_ATTRIBUTES. Raises EOtlpExporterError on another protocol.
    /// </summary>
    class function FromEnvironment: TOtlpMetricExporter; overload;

    /// <summary>
    ///   Same, reading the variables through AGetVariable (which returns ''
    ///   for an unset one).
    /// </summary>
    class function FromEnvironment(const AGetVariable: TFunc<string, string>;
      const ASender: IOtlpHttpSender = nil): TOtlpMetricExporter; overload;

    /// <summary>
    ///   Adds a header, replacing one of the same name (case-insensitive).
    /// </summary>
    function AddHeader(const AName, AValue: string): TOtlpMetricExporter;

    /// <summary>
    ///   Adds a resource attribute, replacing one with the same key.
    /// </summary>
    function AddResourceAttribute(const AKey, AValue: string): TOtlpMetricExporter;

    { IMetricExporter: a harvest without info is taken as not resetting }
    procedure Export(const APoints: TArray<TMetricPoint>);

    { IMetricHarvestExporter }
    procedure ExportHarvest(const APoints: TArray<TMetricPoint>; const AInfo: TMetricHarvestInfo);

    property Endpoint: string read FEndpoint write FEndpoint;

    /// <summary>
    ///   Gzip the body (Content-Encoding: gzip). Off by default.
    /// </summary>
    property Compression: Boolean read FCompression write FCompression;

    /// <summary>
    ///   Connection and response timeout in milliseconds.
    /// </summary>
    property Timeout: Integer read FTimeout write FTimeout;

    /// <summary>
    ///   Scope name given to the default meter ('').
    /// </summary>
    property DefaultScopeName: string read FDefaultScope write FDefaultScope;

    /// <summary>
    ///   Called with the description of a failed or partially rejected export.
    ///   With Async off that is the harvest thread; with Async on it is the
    ///   exporter's delivery thread, so a handler must be thread safe (logging
    ///   is). An exception it raises is swallowed either way.
    /// </summary>
    property OnError: TProc<string> read FOnError write FOnError;
    property Headers: TArray<TOtlpHeader> read GetHeaders;
    property ResourceAttributes: TArray<TMetricLabel> read GetResourceAttributes;

    /// <summary>
    ///   When True the POST runs on a thread of the exporter's own and Export
    ///   returns as soon as the body is built: a collector that is slow, or
    ///   absent, cannot hold up the thread that harvests - which in a VCL host is
    ///   the UI thread, and in a web host may be a request thread.
    /// </summary>
    /// <remarks>
    ///   Off by default, which keeps the documented behaviour (the POST runs on
    ///   the harvest thread) and the counting synchronous: after Export the
    ///   counters already describe the attempt. With it on, only the most recent
    ///   body is kept while one is in flight - bodies are cumulative snapshots,
    ///   so the newest one carries everything the older one did - and
    ///   <see cref="Flush"/> waits for the queue to drain when a caller does want
    ///   the result (an on-demand "push now" button, a test, a shutdown).
    /// </remarks>
    property Async: Boolean read FAsync write FAsync;

    /// <summary>
    ///   After a failed export, wait before trying the collector again: 5 s at
    ///   first, doubling per consecutive failure up to 5 minutes, cleared by a
    ///   success. On by default.
    /// </summary>
    /// <remarks>
    ///   A connection that cannot be established is not answered with a
    ///   Retry-After, so without this the exporter retries on every harvest and
    ///   reports the same failure each time. Skips are counted in
    ///   <see cref="SkippedCount"/> and are not reported to OnError: the failure
    ///   that started the wait already was.
    /// </remarks>
    property Backoff: Boolean read FBackoff write FBackoff;

    /// <summary>First backoff delay in milliseconds. Default OTLP_DEFAULT_BACKOFF_MS.</summary>
    property BackoffBaseMs: Integer read FBackoffBaseMs write FBackoffBaseMs;

    /// <summary>Longest backoff delay in milliseconds. Default OTLP_MAX_BACKOFF_MS.</summary>
    property MaxBackoffMs: Integer read FMaxBackoffMs write FMaxBackoffMs;

    /// <summary>
    ///   Exports the collector accepted (partially or not).
    /// </summary>
    property SentCount: Int64 read GetSentCount;

    /// <summary>
    ///   Exports that failed: transport error or non-2xx status.
    /// </summary>
    property FailedCount: Int64 read GetFailedCount;

    /// <summary>
    ///   Harvests not sent because a Retry-After, or the connect-failure
    ///   backoff, had not elapsed yet.
    /// </summary>
    property SkippedCount: Int64 read GetSkippedCount;

    /// <summary>
    ///   Queued bodies replaced before they were sent, because a newer one
    ///   arrived while the previous POST was still in flight (Async only).
    ///   Nothing is lost: every body is a cumulative snapshot.
    /// </summary>
    property DroppedCount: Int64 read GetDroppedCount;

    /// <summary>Failures in a row, reset by an accepted export.</summary>
    property ConsecutiveFailures: Integer read GetConsecutiveFailures;

    property LastError: string read GetLastError;

    /// <summary>
    ///   Waits until nothing is queued or in flight, or the timeout elapses
    ///   (ATimeoutMs &lt;= 0 uses Timeout). True when the queue drained in time.
    ///   With Async off there is nothing to wait for and it returns True.
    /// </summary>
    function Flush(ATimeoutMs: Integer = 0): Boolean;
  end;

/// <summary>
///   Builds the OTLP ExportMetricsServiceRequest for the points, every point as
///   a cumulative data point. The points' FirstSeen (local time, as the
///   subsystem records it) is the start time; ATimeUtc is the time of every
///   data point. Meters become scopes, the default meter ('') ADefaultScope.
/// </summary>
function MetricsToOtlpRequest(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string = OTLP_DEFAULT_SCOPE): TOtlpExportMetricsServiceRequest;

/// <summary>
///   MetricsToOtlpRequest serialized by Neon: the JSON body the exporter POSTs.
/// </summary>
function MetricsToOtlpJson(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string = OTLP_DEFAULT_SCOPE): string;

/// <summary>
///   A UTC TDateTime as OTLP's fixed64 nanoseconds since the Unix epoch, as a
///   string.
/// </summary>
function OtlpUnixNano(const AUtcTime: TDateTime): string;

implementation

uses
  System.Math,
  System.JSON,
  System.Rtti,
  System.TypInfo,
  System.DateUtils,
  System.ZLib,
  System.TimeSpan,
  System.Generics.Defaults,
  System.Net.URLClient,
  System.Net.HttpClient,

  Neon.Core.Types,
  Neon.Core.Persistence,
  Neon.Core.Persistence.JSON;

{ Local helpers ------------------------------------------------------------ }

function OtlpUnixNano(const AUtcTime: TDateTime): string;
var
  LMilliseconds: Int64;
begin
  // TDateTime resolves milliseconds, so that is the precision on the wire
  LMilliseconds := Round((AUtcTime - UnixDateDelta) * MSecsPerDay);
  Result := IntToStr(LMilliseconds * Int64(1000000));
end;

function LocalToUtc(const ALocal: TDateTime): TDateTime;
begin
  try
    Result := TTimeZone.Local.ToUniversalTime(ALocal);
  except
    // A local time that does not exist (inside a DST gap) cannot be converted
    // exactly; the current offset is close enough for a start time.
    Result := IncMilliSecond(ALocal,
      -Round(TTimeZone.Local.UtcOffset.TotalMilliseconds));
  end;
end;

type
  /// <summary>
  ///   Writes a TOtlpDouble as a JSON number, or as the proto3 JSON string for
  ///   NaN and the infinities, which a JSON number cannot represent.
  /// </summary>
  TOtlpDoubleSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
      AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue;
      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

{ TOtlpDoubleSerializer }

class function TOtlpDoubleSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TOtlpDouble);
end;

class function TOtlpDoubleSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  // By identity: TOtlpDouble is a distinct type, a plain Double is not handled
  Result := AType = GetTargetInfo;
end;

function TOtlpDoubleSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LValue: Double;
begin
  LValue := AValue.AsExtended;
  if IsNan(LValue) then
    Result := TJSONString.Create('NaN')
  else if IsInfinite(LValue) then
  begin
    if LValue > 0 then
      Result := TJSONString.Create('Infinity')
    else
      Result := TJSONString.Create('-Infinity');
  end
  else
    Result := TJSONNumber.Create(LValue);
end;

function TOtlpDoubleSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LValue: TOtlpDouble;
begin
  if AValue is TJSONNumber then
    LValue := TJSONNumber(AValue).AsDouble
  else if SameText(AValue.Value, 'Infinity') then
    LValue := Infinity
  else if SameText(AValue.Value, '-Infinity') then
    LValue := NegInfinity
  else
    LValue := NaN;
  Result := TValue.From<TOtlpDouble>(LValue);
end;

/// <summary>
///   Neon settings of the OTLP entities: camelCase field names (the OTLP JSON
///   names), fields rather than properties, TOtlpDouble for the special floats.
/// </summary>
function OtlpNeonConfig: INeonConfiguration;
begin
  Result := TNeonConfiguration.Camel
    .SetMembers([TNeonMembers.Fields])
    .RegisterSerializer(TOtlpDoubleSerializer);
end;

{ TOtlpKeyValue }

class function TOtlpKeyValue.Create(const AKey, AValue: string): TOtlpKeyValue;
begin
  Result.Key := AKey;
  Result.Value.StringValue := AValue;
end;

function ToAttributes(const ALabels: TArray<TMetricLabel>): TOtlpAttributes;
var
  I: Integer;
begin
  SetLength(Result, Length(ALabels));
  for I := 0 to High(ALabels) do
    Result[I] := TOtlpKeyValue.Create(ALabels[I].Key, ALabels[I].Value);
end;

function LabelsText(const ALabels: TArray<TMetricLabel>): string;
var
  LLabel: TMetricLabel;
begin
  Result := '';
  for LLabel in ALabels do
    Result := Result + #0 + LLabel.Key + #1 + LLabel.Value;
end;

function SeriesKey(const APoint: TMetricPoint): string;
begin
  Result := APoint.Meter + #2 + APoint.Name + #2 + IntToStr(Ord(APoint.Kind)) +
    #2 + LabelsText(APoint.Labels);
end;

function ToNumberDataPoint(const APoint: TMetricPoint; const AValue: Double;
  const ATime: string): TOtlpNumberDataPoint;
begin
  Result := Default(TOtlpNumberDataPoint);
  Result.Attributes := ToAttributes(APoint.Labels);
  // A gauge is an instantaneous reading: no aggregation window to start
  if APoint.Kind <> TMetricKind.Gauge then
    Result.StartTimeUnixNano := OtlpUnixNano(LocalToUtc(APoint.FirstSeen));
  Result.TimeUnixNano := ATime;
  Result.AsDouble := AValue;
end;

function ToHistogramDataPoint(const APoint: TMetricPoint;
  const ATime: string): TOtlpHistogramDataPoint;
begin
  Result := Default(TOtlpHistogramDataPoint);
  Result.Attributes := ToAttributes(APoint.Labels);
  Result.StartTimeUnixNano := OtlpUnixNano(LocalToUtc(APoint.FirstSeen));
  Result.TimeUnixNano := ATime;
  Result.Count := IntToStr(APoint.Count);
  Result.Sum := APoint.Sum;
  // No explicit bounds: one bucket, (-inf, +inf), holding every sample
  Result.BucketCounts := [IntToStr(APoint.Count)];
  Result.ExplicitBounds := nil;
  Result.Min := APoint.Min;
  Result.Max := APoint.Max;
end;

/// <summary>
///   The metric of the adjacent points APoints[AFirst..ALast], which share
///   meter, name and kind.
/// </summary>
function ToMetric(const APoints: TArray<TMetricPoint>; AFirst, ALast: Integer;
  const ATime: string): TOtlpMetric;
var
  LFirst: TMetricPoint;
  LSum: TOtlpSum;
  LGauge: TOtlpGauge;
  LHistogram: TOtlpHistogram;
  I: Integer;
begin
  LFirst := APoints[AFirst];
  Result := Default(TOtlpMetric);
  Result.Name := LFirst.Name;
  Result.Description := LFirst.Description;
  Result.UnitName := LFirst.UnitName;

  case LFirst.Kind of
    TMetricKind.Counter:
    begin
      LSum.AggregationTemporality := OTLP_TEMPORALITY_CUMULATIVE;
      LSum.IsMonotonic := True;
      SetLength(LSum.DataPoints, ALast - AFirst + 1);
      for I := AFirst to ALast do
        LSum.DataPoints[I - AFirst] := ToNumberDataPoint(APoints[I], APoints[I].Sum, ATime);
      Result.Sum := LSum;
    end;
    TMetricKind.Gauge:
    begin
      SetLength(LGauge.DataPoints, ALast - AFirst + 1);
      for I := AFirst to ALast do
        LGauge.DataPoints[I - AFirst] := ToNumberDataPoint(APoints[I], APoints[I].Last, ATime);
      Result.Gauge := LGauge;
    end;
    TMetricKind.Histogram:
    begin
      LHistogram.AggregationTemporality := OTLP_TEMPORALITY_CUMULATIVE;
      SetLength(LHistogram.DataPoints, ALast - AFirst + 1);
      for I := AFirst to ALast do
        LHistogram.DataPoints[I - AFirst] := ToHistogramDataPoint(APoints[I], ATime);
      Result.Histogram := LHistogram;
    end;
  end;
end;

function MetricsToOtlpJson(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string): string;
begin
  Result := TNeon.ValueToJSONString(
    TValue.From<TOtlpExportMetricsServiceRequest>(
      MetricsToOtlpRequest(APoints, AResource, ATimeUtc, ADefaultScope)),
    OtlpNeonConfig);
end;

function MetricsToOtlpRequest(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string): TOtlpExportMetricsServiceRequest;
var
  LPoints: TArray<TMetricPoint>;
  LResourceMetrics: TOtlpResourceMetrics;
  LScope: TOtlpScopeMetrics;
  LTime: string;
  I, J, K: Integer;
begin
  // Ordinal ordering, so points of one meter and of one metric are adjacent
  LPoints := Copy(APoints);
  TArray.Sort<TMetricPoint>(LPoints, TComparer<TMetricPoint>.Construct(
    function(const ALeft, ARight: TMetricPoint): Integer
    begin
      Result := CompareStr(ALeft.Meter, ARight.Meter);
      if Result = 0 then
        Result := CompareStr(ALeft.Name, ARight.Name);
      if Result = 0 then
        Result := Ord(ALeft.Kind) - Ord(ARight.Kind);
      if Result = 0 then
        Result := CompareStr(LabelsText(ALeft.Labels), LabelsText(ARight.Labels));
    end));

  LTime := OtlpUnixNano(ATimeUtc);

  LResourceMetrics := Default(TOtlpResourceMetrics);
  LResourceMetrics.Resource.Attributes := ToAttributes(AResource);

  // One scope per run of points sharing a meter, one metric per run sharing
  // a name and kind within it
  I := 0;
  while I <= High(LPoints) do
  begin
    LScope := Default(TOtlpScopeMetrics);
    LScope.Scope.Name := LPoints[I].Meter;
    if LScope.Scope.Name = '' then
      LScope.Scope.Name := ADefaultScope;

    J := I;
    while (J <= High(LPoints)) and (LPoints[J].Meter = LPoints[I].Meter) do
    begin
      K := J + 1;
      while (K <= High(LPoints)) and (LPoints[K].Meter = LPoints[J].Meter) and
        (LPoints[K].Name = LPoints[J].Name) and (LPoints[K].Kind = LPoints[J].Kind) do
        Inc(K);
      LScope.Metrics := LScope.Metrics + [ToMetric(LPoints, J, K - 1, LTime)];
      J := K;
    end;

    LResourceMetrics.ScopeMetrics := LResourceMetrics.ScopeMetrics + [LScope];
    I := J;
  end;

  Result.ResourceMetrics := [LResourceMetrics];
end;

function GzipBytes(const AData: TBytes): TBytes;
var
  LOutput: TBytesStream;
  LZip: TZCompressionStream;
begin
  LOutput := TBytesStream.Create;
  try
    // 15 + 16 window bits: a gzip wrapper rather than a raw zlib stream
    LZip := TZCompressionStream.Create(LOutput, zcDefault, 15 + 16);
    try
      if Length(AData) > 0 then
        LZip.WriteBuffer(AData[0], Length(AData));
    finally
      LZip.Free; // writes the gzip trailer
    end;
    Result := Copy(LOutput.Bytes, 0, LOutput.Size);
  finally
    LOutput.Free;
  end;
end;

/// <summary>
///   Decodes %XX escapes as UTF-8, as the W3C Baggage format of the OTEL_*
///   lists requires. A '+' stays a '+': this is not form encoding, and API
///   keys routinely contain one.
/// </summary>
function PercentDecode(const AValue: string): string;
var
  LBytes: TBytes;
  LCount, I, LCode: Integer;
begin
  if AValue.IndexOf('%') < 0 then
    Exit(AValue);
  LBytes := TEncoding.UTF8.GetBytes(AValue);
  LCount := 0;
  I := 0;
  while I < Length(LBytes) do
  begin
    if (LBytes[I] = Ord('%')) and (I + 2 < Length(LBytes)) and
      TryStrToInt('$' + Chr(LBytes[I + 1]) + Chr(LBytes[I + 2]), LCode) then
    begin
      LBytes[LCount] := Byte(LCode);
      Inc(I, 3);
    end
    else
    begin
      LBytes[LCount] := LBytes[I];
      Inc(I);
    end;
    Inc(LCount);
  end;
  Result := TEncoding.UTF8.GetString(LBytes, 0, LCount);
end;

/// <summary>"k1=v1,k2=v2" into pairs; entries without a key are skipped.</summary>
function ParseKeyValueList(const AValue: string): TArray<TMetricLabel>;
var
  LEntry: string;
  LPos: Integer;
  LPair: TMetricLabel;
begin
  Result := nil;
  for LEntry in AValue.Split([',']) do
  begin
    LPos := LEntry.IndexOf('=');
    if LPos <= 0 then
      Continue;
    LPair.Key := PercentDecode(LEntry.Substring(0, LPos).Trim);
    LPair.Value := PercentDecode(LEntry.Substring(LPos + 1).Trim);
    if LPair.Key <> '' then
      Result := Result + [LPair];
  end;
end;

function MergePoints(const ABase, ALive: TMetricPoint): TMetricPoint;
begin
  Result := ALive;
  Result.Count := ABase.Count + ALive.Count;
  Result.Sum := ABase.Sum + ALive.Sum;
  Result.Min := System.Math.Min(ABase.Min, ALive.Min);
  Result.Max := System.Math.Max(ABase.Max, ALive.Max);
  Result.FirstSeen := ABase.FirstSeen;
end;

{ TOtlpHttpClientSender ---------------------------------------------------- }

function TOtlpHttpClientSender.Post(const AUrl: string;
  const AHeaders: TArray<TOtlpHeader>; const ABody: TBytes;
  ATimeoutMs: Integer): TOtlpHttpResponse;
var
  LClient: THTTPClient;
  LSource: TBytesStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  I: Integer;
begin
  LClient := THTTPClient.Create;
  try
    LClient.ConnectionTimeout := ATimeoutMs;
    LClient.ResponseTimeout := ATimeoutMs;
    LClient.HandleRedirects := False;

    SetLength(LHeaders, Length(AHeaders));
    for I := 0 to High(AHeaders) do
      LHeaders[I] := TNetHeader.Create(AHeaders[I].Name, AHeaders[I].Value);

    LSource := TBytesStream.Create(ABody);
    try
      LResponse := LClient.Post(AUrl, LSource, nil, LHeaders);
    finally
      LSource.Free;
    end;

    Result.StatusCode := LResponse.StatusCode;
    Result.Body := LResponse.ContentAsString(TEncoding.UTF8);
    Result.RetryAfter := LResponse.HeaderValue['Retry-After'];
  finally
    LClient.Free;
  end;
end;

{ TOtlpPendingExport ------------------------------------------------------- }

constructor TOtlpPendingExport.Create(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
  const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer);
begin
  inherited Create;
  Url := AUrl;
  Headers := AHeaders;
  Body := ABody;
  Time := ATime;
  TimeoutMs := ATimeoutMs;
end;

{ TOtlpExportWorker --------------------------------------------------------

  The thread an async exporter delivers on. One per exporter, created the first
  time a body is handed over, and stopped by the exporter's destructor: it takes
  a pending body, POSTs it, and handles the answer, so the harvest never waits on
  the network. It never lets an exception out - a thread nobody watches is the
  worst place for one, and SendNow already turns failures into messages. }

type
  TOtlpExportWorker = class(TThread)
  private
    FExporter: TOtlpMetricExporter;
    FSignal: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const AExporter: TOtlpMetricExporter; const ASignal: TEvent);
  end;

constructor TOtlpExportWorker.Create(const AExporter: TOtlpMetricExporter; const ASignal: TEvent);
begin
  // Created *running*, and deliberately not "Create(True) then Start": the RTL
  // starts a thread that was created running from TThread.AfterConstruction,
  // which runs after this constructor body - so the fields below are set before
  // the thread can use them - whereas calling Start here leaves FCreateSuspended
  // cleared, and AfterConstruction then tries to resume a thread that is already
  // running and raises EThread "Cannot call Start on a running or suspended
  // thread".
  inherited Create(False);
  FreeOnTerminate := False;
  FExporter := AExporter;
  FSignal := ASignal;
end;

procedure TOtlpExportWorker.Execute;
var
  LPending: TOtlpPendingExport;
  LError: string;
begin
  while not Terminated do
  begin
    // A timeout as well as the signal: a wake-up that arrives while the thread
    // is between two bodies is not lost, because the queue is checked again
    // after every wait.
    FSignal.WaitFor(1000);
    if Terminated then
      Break;

    LPending := FExporter.TakePending;
    while (LPending <> nil) and not Terminated do
    begin
      try
        LError := FExporter.SendNow(LPending.Url, LPending.Headers, LPending.Body,
          LPending.Time, LPending.TimeoutMs);
        if LError <> '' then
          FExporter.NotifyError(LError);
      except
        // Nothing may leave this thread: see the comment above.
      end;
      LPending.Free;
      LPending := FExporter.TakePending;
    end;
  end;
end;

{ TOtlpMetricExporter ------------------------------------------------------ }

constructor TOtlpMetricExporter.Create(const AEndpoint: string);
begin
  Create(AEndpoint, nil);
end;

constructor TOtlpMetricExporter.Create(const AEndpoint: string;
  const ASender: IOtlpHttpSender);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FHeaders := TList<TOtlpHeader>.Create;
  FResource := TList<TMetricLabel>.Create;
  FSeries := TDictionary<string, TSeriesState>.Create;
  FEndpoint := AEndpoint;
  FSender := ASender;
  if not Assigned(FSender) then
    FSender := TOtlpHttpClientSender.Create;
  FDefaultScope := OTLP_DEFAULT_SCOPE;
  FTimeout := OTLP_DEFAULT_TIMEOUT;
  FBackoff := True;
  FBackoffBaseMs := OTLP_DEFAULT_BACKOFF_MS;
  FMaxBackoffMs := OTLP_MAX_BACKOFF_MS;
  AddResourceAttribute('service.name',
    'unknown_service:' + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

destructor TOtlpMetricExporter.Destroy;
begin
  // The worker first: it reads FPending, FSender and the counters, so it has to
  // be gone before any of them are.
  StopWorker;

  if Assigned(FPending) then
    FreeAndNil(FPending);

  FSeries.Free;
  FResource.Free;
  FHeaders.Free;
  FLock.Free;
  inherited;
end;

procedure TOtlpMetricExporter.EnsureWorker;
begin
  FLock.Enter;
  try
    if not Assigned(FWorkerSignal) then
      FWorkerSignal := TEvent.Create(nil, False, False, '');
    if not Assigned(FWorker) then
      FWorker := TOtlpExportWorker.Create(Self, FWorkerSignal);
  finally
    FLock.Leave;
  end;
end;

procedure TOtlpMetricExporter.StopWorker;
begin
  FLock.Enter;
  try
    if Assigned(FWorker) then
    begin
      FWorker.Terminate;
      if Assigned(FWorkerSignal) then
        FWorkerSignal.SetEvent;
    end;
  finally
    FLock.Leave;
  end;

  if Assigned(FWorker) then
  begin
    // Bounded by the POST timeout: the worker may be inside one POST, and it
    // finishes that before it notices the termination.
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;

  if Assigned(FWorkerSignal) then
    FreeAndNil(FWorkerSignal);
end;

function TOtlpMetricExporter.TakePending: TOtlpPendingExport;
begin
  FLock.Enter;
  try
    Result := FPending;
    FPending := nil;
    FPosting := Result <> nil;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.HasWork: Boolean;
begin
  FLock.Enter;
  try
    Result := FPosting or (FPending <> nil);
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.Flush(ATimeoutMs: Integer): Boolean;
var
  LDeadline: UInt64;
begin
  if not FAsync then
    Exit(True);
  if ATimeoutMs <= 0 then
    ATimeoutMs := FTimeout;
  LDeadline := TThread.GetTickCount64 + UInt64(ATimeoutMs);

  while HasWork do
  begin
    if TThread.GetTickCount64 >= LDeadline then
      Exit(False);
    Sleep(5);
  end;

  Result := True;
end;

procedure TOtlpMetricExporter.WakeWorker;
begin
  if Assigned(FWorkerSignal) then
    FWorkerSignal.SetEvent;
end;

class function TOtlpMetricExporter.FromEnvironment: TOtlpMetricExporter;
begin
  Result := FromEnvironment(
    function(AName: string): string
    begin
      Result := GetEnvironmentVariable(AName);
    end);
end;

class function TOtlpMetricExporter.FromEnvironment(
  const AGetVariable: TFunc<string, string>;
  const ASender: IOtlpHttpSender): TOtlpMetricExporter;

  // The metrics-specific variable wins over the generic one
  function Setting(const ASuffix: string): string;
  begin
    Result := AGetVariable('OTEL_EXPORTER_OTLP_METRICS_' + ASuffix).Trim;
    if Result = '' then
      Result := AGetVariable('OTEL_EXPORTER_OTLP_' + ASuffix).Trim;
  end;

var
  LEndpoint, LProtocol, LCompression, LServiceName: string;
  LPair: TMetricLabel;
begin
  LProtocol := Setting('PROTOCOL');
  if (LProtocol <> '') and not SameText(LProtocol, 'http/json') then
    raise EOtlpExporterError.CreateFmt(
      'OTLP exporter: protocol "%s" is not supported, only http/json', [LProtocol]);

  // A signal-specific endpoint is used as is; the generic one is a base URL
  LEndpoint := AGetVariable('OTEL_EXPORTER_OTLP_METRICS_ENDPOINT').Trim;
  if LEndpoint = '' then
  begin
    LEndpoint := AGetVariable('OTEL_EXPORTER_OTLP_ENDPOINT').Trim;
    if LEndpoint <> '' then
      LEndpoint := LEndpoint.TrimRight(['/']) + OTLP_METRICS_PATH
    else
      LEndpoint := OTLP_DEFAULT_ENDPOINT;
  end;

  Result := TOtlpMetricExporter.Create(LEndpoint, ASender);
  try
    for LPair in ParseKeyValueList(Setting('HEADERS')) do
      Result.AddHeader(LPair.Key, LPair.Value);

    LCompression := Setting('COMPRESSION');
    Result.Compression := SameText(LCompression, 'gzip');
    Result.Timeout := StrToIntDef(Setting('TIMEOUT'), OTLP_DEFAULT_TIMEOUT);

    for LPair in ParseKeyValueList(AGetVariable('OTEL_RESOURCE_ATTRIBUTES')) do
      Result.AddResourceAttribute(LPair.Key, LPair.Value);

    // OTEL_SERVICE_NAME takes precedence over a service.name in the attributes
    LServiceName := AGetVariable('OTEL_SERVICE_NAME').Trim;
    if LServiceName <> '' then
      Result.AddResourceAttribute('service.name', LServiceName);
  except
    Result.Free;
    raise;
  end;
end;

function TOtlpMetricExporter.AddHeader(const AName, AValue: string): TOtlpMetricExporter;
var
  LHeader: TOtlpHeader;
  I: Integer;
begin
  LHeader.Name := AName;
  LHeader.Value := AValue;
  FLock.Enter;
  try
    for I := 0 to FHeaders.Count - 1 do
      if SameText(FHeaders[I].Name, AName) then
      begin
        FHeaders[I] := LHeader;
        Exit(Self);
      end;
    FHeaders.Add(LHeader);
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TOtlpMetricExporter.AddResourceAttribute(const AKey,
  AValue: string): TOtlpMetricExporter;
var
  LAttribute: TMetricLabel;
  I: Integer;
begin
  LAttribute.Key := AKey;
  LAttribute.Value := AValue;
  FLock.Enter;
  try
    for I := 0 to FResource.Count - 1 do
      if FResource[I].Key = AKey then
      begin
        FResource[I] := LAttribute;
        Exit(Self);
      end;
    FResource.Add(LAttribute);
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TOtlpMetricExporter.Accumulate(const APoints: TArray<TMetricPoint>;
  AReset: Boolean): TArray<TMetricPoint>;

  procedure FoldLive(var AState: TSeriesState);
  begin
    if not AState.HasLive then
      Exit;
    if AState.HasBase then
      AState.Base := MergePoints(AState.Base, AState.Live)
    else
      AState.Base := AState.Live;
    AState.HasBase := True;
    AState.HasLive := False;
  end;

var
  LResult: TList<TMetricPoint>;
  LPoint: TMetricPoint;
  LKey: string;
  LState: TSeriesState;
  LCleared: Boolean;
begin
  LResult := TList<TMetricPoint>.Create;
  try
    for LPoint in APoints do
    begin
      // A gauge is its last value: nothing to accumulate, nothing to remember
      if LPoint.Kind = TMetricKind.Gauge then
      begin
        LResult.Add(LPoint);
        Continue;
      end;

      LKey := SeriesKey(LPoint);
      if not FSeries.TryGetValue(LKey, LState) then
        LState := Default(TSeriesState);

      // A snapshot that went backwards means the provider was cleared outside
      // a Harvest: what Live held did happen, so it joins the base.
      if LState.HasLive then
      begin
        LCleared := (LPoint.FirstSeen <> LState.Live.FirstSeen) or
          (LPoint.Count < LState.Live.Count) or
          ((LPoint.Kind = TMetricKind.Counter) and (LPoint.Sum < LState.Live.Sum));
        if LCleared then
          FoldLive(LState);
      end;

      LState.Live := LPoint;
      LState.HasLive := True;
      FSeries.AddOrSetValue(LKey, LState);
    end;

    // Every series ever seen is reported, including those this harvest did not carry
    for LState in FSeries.Values do
      if LState.HasBase and LState.HasLive then
        LResult.Add(MergePoints(LState.Base, LState.Live))
      else if LState.HasBase then
        LResult.Add(LState.Base)
      else
        LResult.Add(LState.Live);

    // The provider starts from zero after this harvest: fold the snapshot in
    if AReset then
      for LKey in FSeries.Keys.ToArray do
      begin
        LState := FSeries[LKey];
        FoldLive(LState);
        FSeries[LKey] := LState;
      end;

    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function TOtlpMetricExporter.BuildHeaders: TArray<TOtlpHeader>;
var
  LHeader: TOtlpHeader;
begin
  LHeader.Name := 'Content-Type';
  LHeader.Value := 'application/json';
  Result := [LHeader];
  if FCompression then
  begin
    LHeader.Name := 'Content-Encoding';
    LHeader.Value := 'gzip';
    Result := Result + [LHeader];
  end;
  Result := Result + FHeaders.ToArray;
end;

procedure TOtlpMetricExporter.Export(const APoints: TArray<TMetricPoint>);
var
  LInfo: TMetricHarvestInfo;
begin
  LInfo.Reset := False;
  LInfo.Time := TTimeZone.Local.ToUniversalTime(Now);
  ExportHarvest(APoints, LInfo);
end;

procedure TOtlpMetricExporter.ExportHarvest(const APoints: TArray<TMetricPoint>;
  const AInfo: TMetricHarvestInfo);
var
  LPoints: TArray<TMetricPoint>;
  LHeaders: TArray<TOtlpHeader>;
  LBody: TBytes;
  LUrl: string;
  LTimeout: Integer;
  LSend: Boolean;
  LError: string;
begin
  LError := '';
  // LSend is assigned inside the try below before it is read - on the one path
  // that reads it - so it needs no initial value here.
  LTimeout := 0;
  try
    FLock.Enter;
    try
      // The running totals advance whether or not the POST succeeds
      LPoints := Accumulate(APoints, AInfo.Reset);
      LSend := Length(LPoints) > 0;
      if LSend and (FRetryNotBefore > 0) and (AInfo.Time < FRetryNotBefore) then
      begin
        // Either a Retry-After from the collector or the wait this exporter put
        // between attempts after a failure. Counted, and deliberately not
        // reported: whoever asked for the wait already heard about the failure.
        Inc(FSkippedCount);
        LSend := False;
      end;
      if LSend then
      begin
        LBody := TEncoding.UTF8.GetBytes(
          MetricsToOtlpJson(LPoints, FResource.ToArray, AInfo.Time, FDefaultScope));
        if FCompression then
          LBody := GzipBytes(LBody);
        LHeaders := BuildHeaders;
        LUrl := FEndpoint;
        LTimeout := FTimeout;
      end;
    finally
      FLock.Leave;
    end;

    // Outside the lock - and, with Async, on the exporter's own thread - so the
    // statistics stay readable and the caller never waits on the network.
    if LSend then
      LError := Deliver(LUrl, LHeaders, LBody, AInfo.Time, LTimeout);
  except
    on E: Exception do
      LError := ScheduleFailure(Format('OTLP export to %s failed: %s', [FEndpoint, E.Message]),
        TTimeZone.Local.ToUniversalTime(Now));
  end;

  if not FAsync then
    NotifyError(LError);
end;

function TOtlpMetricExporter.SendNow(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
  const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer): string;
begin
  try
    Result := HandleResponse(AUrl, FSender.Post(AUrl, AHeaders, ABody, ATimeoutMs), ATime);
  except
    on E: Exception do
      Result := ScheduleFailure(Format('OTLP export to %s failed: %s', [AUrl, E.Message]),
        TTimeZone.Local.ToUniversalTime(Now));
  end;
end;

function TOtlpMetricExporter.Deliver(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
  const ABody: TBytes; const ATime: TDateTime; ATimeoutMs: Integer): string;
var
  LPending: TOtlpPendingExport;
begin
  if not FAsync then
    Exit(SendNow(AUrl, AHeaders, ABody, ATime, ATimeoutMs));

  EnsureWorker;
  LPending := TOtlpPendingExport.Create(AUrl, AHeaders, ABody, ATime, ATimeoutMs);

  FLock.Enter;
  try
    // One slot: a body built while the previous POST is still in flight replaces
    // it. The bodies are cumulative snapshots, so the newer one carries
    // everything the older one did and nothing is lost by dropping it.
    if Assigned(FPending) then
    begin
      FreeAndNil(FPending);
      Inc(FDroppedCount);
    end;
    FPending := LPending;
  finally
    FLock.Leave;
  end;

  WakeWorker;
  Result := '';
end;

function TOtlpMetricExporter.HandleResponse(const AUrl: string;
  const AResponse: TOtlpHttpResponse; const ATime: TDateTime): string;
var
  LJson, LPartial, LValue: TJSONValue;
  LRejected, LMessage, LBody: string;
  LSeconds: Integer;
  LRetryAt: TDateTime;
begin
  Result := '';

  if (AResponse.StatusCode >= 200) and (AResponse.StatusCode < 300) then
  begin
    // A partial success is still a success: resending would duplicate what
    // was accepted, so it is only reported.
    LJson := TJSONValue.ParseJSONValue(AResponse.Body);
    try
      if (LJson is TJSONObject) then
      begin
        LPartial := TJSONObject(LJson).FindValue('partialSuccess');
        if LPartial is TJSONObject then
        begin
          // .Value rather than GetValue<string>: an int64 may arrive as a
          // string or as a number, and Value reads both
          LRejected := '0';
          LValue := TJSONObject(LPartial).FindValue('rejectedDataPoints');
          if Assigned(LValue) then
            LRejected := LValue.Value;
          LMessage := '';
          LValue := TJSONObject(LPartial).FindValue('errorMessage');
          if Assigned(LValue) then
            LMessage := LValue.Value;
          if ((LRejected <> '') and (LRejected <> '0')) or (LMessage <> '') then
            Result := Format('OTLP export to %s partially rejected: %s data point(s): %s',
              [AUrl, LRejected, LMessage]);
        end;
      end;
    finally
      LJson.Free;
    end;

    FLock.Enter;
    try
      Inc(FSentCount);
      // The collector answered, so whatever went wrong before is over: the wait
      // between attempts starts from scratch if it goes wrong again.
      FRetryNotBefore := 0;
      FConsecutiveFailures := 0;
      if Result <> '' then
        FLastError := Result;
    finally
      FLock.Leave;
    end;
    Exit;
  end;

  LBody := AResponse.Body;
  if Length(LBody) > 200 then
    LBody := LBody.Substring(0, 200) + '...';
  Result := ScheduleFailure(Format('OTLP export to %s answered HTTP %d: %s',
    [AUrl, AResponse.StatusCode, LBody]), ATime);

  // Only the delta-seconds form; an HTTP-date is rare enough to ignore. Taken
  // together with the backoff the failure already scheduled: whichever asks for
  // the longer wait wins.
  if TryStrToInt(AResponse.RetryAfter.Trim, LSeconds) and (LSeconds > 0) then
  begin
    FLock.Enter;
    try
      LRetryAt := IncSecond(ATime, System.Math.Min(LSeconds, OTLP_MAX_RETRY_AFTER));
      if LRetryAt > FRetryNotBefore then
        FRetryNotBefore := LRetryAt;
    finally
      FLock.Leave;
    end;
  end;
end;

function TOtlpMetricExporter.ScheduleFailure(const AMessage: string; const ANow: TDateTime): string;
var
  LDelayMs: Integer;
  LSteps: Integer;
begin
  FLock.Enter;
  try
    Inc(FFailedCount);
    FLastError := AMessage;

    if FBackoff then
    begin
      Inc(FConsecutiveFailures);

      // 5 s, 10 s, 20 s ... capped. The exponent is walked rather than shifted
      // so that a long outage cannot overflow its way back to a short wait.
      LDelayMs := FBackoffBaseMs;
      LSteps := 0;
      while (LSteps < FConsecutiveFailures - 1) and (LDelayMs < FMaxBackoffMs) do
      begin
        LDelayMs := LDelayMs * 2;
        Inc(LSteps);
      end;
      if (LDelayMs > FMaxBackoffMs) or (LDelayMs <= 0) then
        LDelayMs := FMaxBackoffMs;

      FRetryNotBefore := IncMilliSecond(ANow, LDelayMs);
    end;
  finally
    FLock.Leave;
  end;
  Result := AMessage;
end;

procedure TOtlpMetricExporter.NotifyError(const AMessage: string);
begin
  if (AMessage = '') or not Assigned(FOnError) then
    Exit;
  try
    FOnError(AMessage);
  except
    // Raising out of an exporter would stop Harvest; the failure is in LastError
  end;
end;

function TOtlpMetricExporter.GetSentCount: Int64;
begin
  FLock.Enter;
  try
    Result := FSentCount;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetFailedCount: Int64;
begin
  FLock.Enter;
  try
    Result := FFailedCount;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetSkippedCount: Int64;
begin
  FLock.Enter;
  try
    Result := FSkippedCount;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetDroppedCount: Int64;
begin
  FLock.Enter;
  try
    Result := FDroppedCount;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetConsecutiveFailures: Integer;
begin
  FLock.Enter;
  try
    Result := FConsecutiveFailures;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetLastError: string;
begin
  FLock.Enter;
  try
    Result := FLastError;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetHeaders: TArray<TOtlpHeader>;
begin
  FLock.Enter;
  try
    Result := FHeaders.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TOtlpMetricExporter.GetResourceAttributes: TArray<TMetricLabel>;
begin
  FLock.Enter;
  try
    Result := FResource.ToArray;
  finally
    FLock.Leave;
  end;
end;

end.
