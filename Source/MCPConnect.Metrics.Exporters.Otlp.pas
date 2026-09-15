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
///   The POST runs on the harvest thread and blocks it for up to Timeout.
/// </summary>
unit MCPConnect.Metrics.Exporters.Otlp;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,

  MCPConnect.Metrics;

const
  /// <summary>Where a local collector listens for OTLP/HTTP metrics.</summary>
  OTLP_DEFAULT_ENDPOINT = 'http://localhost:4318/v1/metrics';
  /// <summary>The signal path appended to OTEL_EXPORTER_OTLP_ENDPOINT.</summary>
  OTLP_METRICS_PATH = '/v1/metrics';
  /// <summary>Scope name of the default meter, whose own name is empty.</summary>
  OTLP_DEFAULT_SCOPE = 'MCPConnect.Metrics';
  /// <summary>Connection and response timeout, in milliseconds.</summary>
  OTLP_DEFAULT_TIMEOUT = 10000;
  /// <summary>AggregationTemporality.AGGREGATION_TEMPORALITY_CUMULATIVE.</summary>
  OTLP_TEMPORALITY_CUMULATIVE = 2;
  /// <summary>Longest Retry-After honoured, in seconds.</summary>
  OTLP_MAX_RETRY_AFTER = 3600;

type
  /// <summary>Raised on configuration errors (never on a failed export).</summary>
  EOtlpExporterError = class(EMetricsError);

  /// <summary>One HTTP header sent with every export.</summary>
  TOtlpHeader = record
    Name: string;
    Value: string;
  end;

  /// <summary>What the transport hands back to the exporter.</summary>
  TOtlpHttpResponse = record
    StatusCode: Integer;
    Body: string;
    /// <summary>The raw Retry-After header, '' when absent.</summary>
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
    FLastError: string;

    function Accumulate(const APoints: TArray<TMetricPoint>; AReset: Boolean): TArray<TMetricPoint>;
    function BuildHeaders: TArray<TOtlpHeader>;
    function HandleResponse(const AUrl: string; const AResponse: TOtlpHttpResponse;
      const ATime: TDateTime): string;
    function RecordFailure(const AMessage: string): string;
    procedure NotifyError(const AMessage: string);
    function GetSentCount: Int64;
    function GetFailedCount: Int64;
    function GetSkippedCount: Int64;
    function GetLastError: string;
    function GetHeaders: TArray<TOtlpHeader>;
    function GetResourceAttributes: TArray<TMetricLabel>;
  public
    /// <summary>
    ///   AEndpoint is the full metrics URL, /v1/metrics included. Sets
    ///   service.name to unknown_service:&lt;executable&gt;, as OpenTelemetry does.
    /// </summary>
    constructor Create(const AEndpoint: string = OTLP_DEFAULT_ENDPOINT); overload;
    /// <summary>Same, with the transport replaced.</summary>
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

    /// <summary>Adds a header, replacing one of the same name (case-insensitive).</summary>
    function AddHeader(const AName, AValue: string): TOtlpMetricExporter;
    /// <summary>Adds a resource attribute, replacing one with the same key.</summary>
    function AddResourceAttribute(const AKey, AValue: string): TOtlpMetricExporter;

    { IMetricExporter: a harvest without info is taken as not resetting }
    procedure Export(const APoints: TArray<TMetricPoint>);
    { IMetricHarvestExporter }
    procedure ExportHarvest(const APoints: TArray<TMetricPoint>;
      const AInfo: TMetricHarvestInfo);

    property Endpoint: string read FEndpoint write FEndpoint;
    /// <summary>Gzip the body (Content-Encoding: gzip). Off by default.</summary>
    property Compression: Boolean read FCompression write FCompression;
    /// <summary>Connection and response timeout in milliseconds.</summary>
    property Timeout: Integer read FTimeout write FTimeout;
    /// <summary>Scope name given to the default meter ('').</summary>
    property DefaultScopeName: string read FDefaultScope write FDefaultScope;
    /// <summary>
    ///   Called on the harvest thread with the description of a failed or
    ///   partially rejected export. An exception it raises is swallowed.
    /// </summary>
    property OnError: TProc<string> read FOnError write FOnError;
    property Headers: TArray<TOtlpHeader> read GetHeaders;
    property ResourceAttributes: TArray<TMetricLabel> read GetResourceAttributes;

    /// <summary>Exports the collector accepted (partially or not).</summary>
    property SentCount: Int64 read GetSentCount;
    /// <summary>Exports that failed: transport error or non-2xx status.</summary>
    property FailedCount: Int64 read GetFailedCount;
    /// <summary>Harvests not sent because a Retry-After had not elapsed.</summary>
    property SkippedCount: Int64 read GetSkippedCount;
    property LastError: string read GetLastError;
  end;

/// <summary>
///   Renders the points as an OTLP ExportMetricsServiceRequest in the JSON
///   encoding, every point as a cumulative data point. The points' FirstSeen
///   (local time, as the subsystem records it) is the start time; ATimeUtc is
///   the time of every data point.
/// </summary>
function MetricsToOtlpJson(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string = OTLP_DEFAULT_SCOPE): string;

/// <summary>A UTC TDateTime as OTLP's fixed64 nanoseconds since the Unix epoch, as a string.</summary>
function OtlpUnixNano(const AUtcTime: TDateTime): string;

implementation

uses
  System.Math,
  System.JSON,
  System.DateUtils,
  System.ZLib,
  System.Generics.Defaults,
  System.Net.URLClient,
  System.Net.HttpClient,

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

/// <summary>A double, or the proto3 JSON string for NaN and the infinities.</summary>
function JsonDouble(const AValue: Double): TJSONValue;
begin
  if IsNan(AValue) then
    Result := TJSONString.Create('NaN')
  else if IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Result := TJSONString.Create('Infinity')
    else
      Result := TJSONString.Create('-Infinity');
  end
  else
    Result := TJSONNumber.Create(AValue);
end;

function AttributesJson(const ALabels: TArray<TMetricLabel>): TJSONArray;
var
  LLabel: TMetricLabel;
begin
  Result := TJSONArray.Create;
  for LLabel in ALabels do
    Result.AddElement(TJSONObject.Create
      .AddPair('key', LLabel.Key)
      .AddPair('value', TJSONObject.Create.AddPair('stringValue', LLabel.Value)));
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

function DataPointJson(const APoint: TMetricPoint; const ATime: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  if Length(APoint.Labels) > 0 then
    Result.AddPair('attributes', AttributesJson(APoint.Labels));
  // A gauge is an instantaneous reading: no aggregation window to start
  if APoint.Kind <> TMetricKind.Gauge then
    Result.AddPair('startTimeUnixNano', OtlpUnixNano(LocalToUtc(APoint.FirstSeen)));
  Result.AddPair('timeUnixNano', ATime);

  case APoint.Kind of
    TMetricKind.Counter:
      Result.AddPair('asDouble', JsonDouble(APoint.Sum));
    TMetricKind.Gauge:
      Result.AddPair('asDouble', JsonDouble(APoint.Last));
    TMetricKind.Histogram:
    begin
      Result.AddPair('count', IntToStr(APoint.Count));
      Result.AddPair('sum', JsonDouble(APoint.Sum));
      // No explicit bounds: one bucket, (-inf, +inf), holding every sample
      Result.AddPair('bucketCounts', TJSONArray.Create(TJSONString.Create(IntToStr(APoint.Count))));
      Result.AddPair('explicitBounds', TJSONArray.Create);
      Result.AddPair('min', JsonDouble(APoint.Min));
      Result.AddPair('max', JsonDouble(APoint.Max));
    end;
  end;
end;

function MetricsToOtlpJson(const APoints: TArray<TMetricPoint>;
  const AResource: TArray<TMetricLabel>; const ATimeUtc: TDateTime;
  const ADefaultScope: string): string;
var
  LPoints: TArray<TMetricPoint>;
  LPoint: TMetricPoint;
  LRoot, LResourceMetrics, LScopeMetrics, LMetric, LData: TJSONObject;
  LScopes, LMetrics, LDataPoints: TJSONArray;
  LTime, LScopeName: string;
  LNewMetric: Boolean;
  I: Integer;
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
  LMetrics := nil;
  LDataPoints := nil;

  LRoot := TJSONObject.Create;
  try
    LResourceMetrics := TJSONObject.Create;
    LRoot.AddPair('resourceMetrics', TJSONArray.Create(LResourceMetrics));
    LResourceMetrics.AddPair('resource',
      TJSONObject.Create.AddPair('attributes', AttributesJson(AResource)));
    LScopes := TJSONArray.Create;
    LResourceMetrics.AddPair('scopeMetrics', LScopes);

    for I := 0 to High(LPoints) do
    begin
      LPoint := LPoints[I];

      if (I = 0) or (LPoint.Meter <> LPoints[I - 1].Meter) then
      begin
        LScopeName := LPoint.Meter;
        if LScopeName = '' then
          LScopeName := ADefaultScope;
        LScopeMetrics := TJSONObject.Create;
        LScopes.AddElement(LScopeMetrics);
        LScopeMetrics.AddPair('scope', TJSONObject.Create.AddPair('name', LScopeName));
        LMetrics := TJSONArray.Create;
        LScopeMetrics.AddPair('metrics', LMetrics);
        LNewMetric := True;
      end
      else
        LNewMetric := (LPoint.Name <> LPoints[I - 1].Name) or
          (LPoint.Kind <> LPoints[I - 1].Kind);

      if LNewMetric then
      begin
        LMetric := TJSONObject.Create;
        LMetrics.AddElement(LMetric);
        LMetric.AddPair('name', LPoint.Name);
        if LPoint.Description <> '' then
          LMetric.AddPair('description', LPoint.Description);
        if LPoint.UnitName <> '' then
          LMetric.AddPair('unit', LPoint.UnitName);

        LData := TJSONObject.Create;
        case LPoint.Kind of
          TMetricKind.Counter:
          begin
            LData.AddPair('aggregationTemporality', TJSONNumber.Create(OTLP_TEMPORALITY_CUMULATIVE));
            LData.AddPair('isMonotonic', TJSONBool.Create(True));
            LMetric.AddPair('sum', LData);
          end;
          TMetricKind.Gauge:
            LMetric.AddPair('gauge', LData);
          TMetricKind.Histogram:
          begin
            LData.AddPair('aggregationTemporality', TJSONNumber.Create(OTLP_TEMPORALITY_CUMULATIVE));
            LMetric.AddPair('histogram', LData);
          end;
        end;
        LDataPoints := TJSONArray.Create;
        LData.AddPair('dataPoints', LDataPoints);
      end;

      LDataPoints.AddElement(DataPointJson(LPoint, LTime));
    end;

    Result := TNeon.Print(LRoot, False);
  finally
    LRoot.Free;
  end;
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
  AddResourceAttribute('service.name',
    'unknown_service:' + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

destructor TOtlpMetricExporter.Destroy;
begin
  FSeries.Free;
  FResource.Free;
  FHeaders.Free;
  FLock.Free;
  inherited;
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
  LSender: IOtlpHttpSender;
  LSend: Boolean;
  LError: string;
begin
  LError := '';
  LSend := False;
  LTimeout := 0;
  try
    FLock.Enter;
    try
      // The running totals advance whether or not the POST succeeds
      LPoints := Accumulate(APoints, AInfo.Reset);
      LSend := Length(LPoints) > 0;
      if LSend and (FRetryNotBefore > 0) and (AInfo.Time < FRetryNotBefore) then
      begin
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
        LSender := FSender;
      end;
    finally
      FLock.Leave;
    end;

    // The network call runs outside the lock, so the statistics stay readable
    if LSend then
      LError := HandleResponse(LUrl, LSender.Post(LUrl, LHeaders, LBody, LTimeout), AInfo.Time);
  except
    on E: Exception do
      LError := RecordFailure(Format('OTLP export to %s failed: %s', [FEndpoint, E.Message]));
  end;
  NotifyError(LError);
end;

function TOtlpMetricExporter.HandleResponse(const AUrl: string;
  const AResponse: TOtlpHttpResponse; const ATime: TDateTime): string;
var
  LJson, LPartial, LValue: TJSONValue;
  LRejected, LMessage, LBody: string;
  LSeconds: Integer;
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
      FRetryNotBefore := 0;
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
  Result := RecordFailure(Format('OTLP export to %s answered HTTP %d: %s',
    [AUrl, AResponse.StatusCode, LBody]));

  // Only the delta-seconds form; an HTTP-date is rare enough to ignore
  if TryStrToInt(AResponse.RetryAfter.Trim, LSeconds) and (LSeconds > 0) then
  begin
    FLock.Enter;
    try
      FRetryNotBefore := IncSecond(ATime, System.Math.Min(LSeconds, OTLP_MAX_RETRY_AFTER));
    finally
      FLock.Leave;
    end;
  end;
end;

function TOtlpMetricExporter.RecordFailure(const AMessage: string): string;
begin
  FLock.Enter;
  try
    Inc(FFailedCount);
    FLastError := AMessage;
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
