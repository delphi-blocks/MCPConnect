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
unit MCPConnect.Tests.Metrics.Otlp;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils,
  System.Generics.Collections, System.SyncObjs,
  DUnitX.TestFramework,

  MCPConnect.Metrics,
  MCPConnect.Metrics.Exporters.Otlp;

type
  /// <summary>
  ///   IOtlpHttpSender that records every request and answers with a
  ///   configurable status, instead of touching the network.
  /// </summary>
  TFakeOtlpSender = class(TInterfacedObject, IOtlpHttpSender)
  public
    Calls: Integer;
    /// <summary>POSTs that have started, whether or not they have finished.</summary>
    Entered: Integer;
    LastUrl: string;
    LastHeaders: TArray<TOtlpHeader>;
    LastBody: TBytes;
    /// <summary>Every body sent, as JSON text (gunzipped when it was gzipped).</summary>
    Texts: TList<string>;
    Status: Integer;
    Body: string;
    RetryAfter: string;
    /// <summary>When not empty, Post raises with this message.</summary>
    RaiseMessage: string;
    /// <summary>When assigned, Post waits on it: an async test's handbrake.</summary>
    Gate: TEvent;
    /// <summary>Milliseconds each Post spends inside, for the drop test.</summary>
    DelayMs: Integer;
    constructor Create;
    destructor Destroy; override;
    function Post(const AUrl: string; const AHeaders: TArray<TOtlpHeader>;
      const ABody: TBytes; ATimeoutMs: Integer): TOtlpHttpResponse;
    function HeaderValue(const AName: string): string;
  end;

  /// <summary>MetricsToOtlpJson: the shape of the request.</summary>
  [TestFixture]
  TMetricsOtlpPayloadTest = class(TObject)
  public
    [Test]
    procedure TestCounter_IsMonotonicCumulativeSum();
    [Test]
    procedure TestGauge_HasValueAndNoStartTime();
    [Test]
    procedure TestHistogram_HasOneBucketHoldingEverySample();
    [Test]
    procedure TestMeters_BecomeScopes();
    [Test]
    procedure TestSeriesOfOneMetric_ShareTheMetric();
    [Test]
    procedure TestResource_IsRendered();
    [Test]
    procedure TestUnixNano_IsNanosecondsSinceEpoch();
    [Test]
    procedure TestInt64Fields_AreStrings();
    [Test]
    procedure TestNaN_IsTheProtoJsonString();
    [Test]
    procedure TestNoPoints_IsStillAValidRequest();
  end;

  /// <summary>TOtlpMetricExporter on a provider: running totals and delivery.</summary>
  [TestFixture]
  TMetricsOtlpExporterTest = class(TObject)
  private
    FProvider: IMeterProvider;
    FSender: TFakeOtlpSender;
    FSenderRef: IOtlpHttpSender;
    FExporter: TOtlpMetricExporter;
    FExporterRef: IMetricExporter;
    function Doc(AIndex: Integer): TJSONObject;
    function CounterSum(AIndex: Integer): Double;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestResetHarvests_AccumulateTotals();
    [Test]
    procedure TestPlainHarvests_DoNotDoubleCount();
    [Test]
    procedure TestMixedHarvests_StayCumulative();
    [Test]
    procedure TestStartTime_DoesNotMove();
    [Test]
    procedure TestClearOutsideHarvest_KeepsCountingUp();
    [Test]
    procedure TestIdleCounter_IsStillReportedAfterReset();
    [Test]
    procedure TestHistogram_MergesAcrossResets();
    [Test]
    procedure TestGauge_IsSentOnlyWhenHarvested();
    [Test]
    procedure TestNoSeries_SendsNothing();
    [Test]
    procedure TestRequest_CarriesUrlAndHeaders();
    [Test]
    procedure TestCompression_GzipsTheBody();
    [Test]
    procedure TestTransportFailure_DoesNotRaiseAndLosesNothing();
    [Test]
    procedure TestHttpError_IsCountedAndReported();
    [Test]
    procedure TestRetryAfter_SkipsHarvestsUntilElapsed();
    [Test]
    procedure TestPartialSuccess_IsReportedNotFailed();
    [Test]
    procedure TestPlainExport_SendsWithoutHarvestInfo();
  end;

  /// <summary>
  ///   Delivery when the collector is not there: the async worker (the POST off
  ///   the harvest thread) and the connect-failure backoff.
  /// </summary>
  [TestFixture]
  TMetricsOtlpDeliveryTest = class(TObject)
  private
    FProvider: IMeterProvider;
    FSender: TFakeOtlpSender;
    FSenderRef: IOtlpHttpSender;
    FExporter: TOtlpMetricExporter;
    FExporterRef: IMetricExporter;
    FGate: TEvent;
    function LastDoc: TJSONObject;
    function LastSum: Double;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAsync_ExportReturnsBeforeThePost();
    [Test]
    procedure TestAsync_KeepsOnlyTheNewestPendingBody();
    [Test]
    procedure TestBackoff_SkipsUntilItElapses();
    [Test]
    procedure TestBackoff_ResetsOnSuccess();
  end;

  /// <summary>TOtlpMetricExporter.FromEnvironment.</summary>
  [TestFixture]
  TMetricsOtlpEnvironmentTest = class(TObject)
  private
    function FromEnv(const APairs: array of string): TOtlpMetricExporter;
  public
    [Test]
    procedure TestDefaults();
    [Test]
    procedure TestSignalEndpoint_IsUsedAsIs();
    [Test]
    procedure TestBaseEndpoint_GetsTheMetricsPath();
    [Test]
    procedure TestHeaders_ArePercentDecoded();
    [Test]
    procedure TestServiceName_OverridesResourceAttributes();
    [Test]
    procedure TestCompressionAndTimeout();
    [Test]
    procedure TestProtocolOtherThanJson_Raises();
  end;

implementation

uses
  System.Math,
  System.ZLib;

const
  METRIC_PATH = 'resourceMetrics[0].scopeMetrics[0].metrics[0]';

  /// <summary>
  ///   How long a gated fake POST waits for its test to let it through: long
  ///   enough that a slow machine does not fail the test, short enough that a
  ///   forgotten gate does not hang the suite.
  /// </summary>
  GATE_TIMEOUT_MS = 15000;

function MakePoint(const AMeter, AName: string; AKind: TMetricKind;
  const ALabels: array of string; ACount: Int64; ASum, AMin, AMax, ALast: Double): TMetricPoint;
var
  I: Integer;
begin
  Result := Default(TMetricPoint);
  Result.Meter := AMeter;
  Result.Name := AName;
  Result.Kind := AKind;
  SetLength(Result.Labels, Length(ALabels) div 2);
  for I := 0 to High(Result.Labels) do
  begin
    Result.Labels[I].Key := ALabels[I * 2];
    Result.Labels[I].Value := ALabels[I * 2 + 1];
  end;
  Result.Count := ACount;
  Result.Sum := ASum;
  Result.Min := AMin;
  Result.Max := AMax;
  Result.Last := ALast;
  Result.FirstSeen := Now;
  Result.LastSeen := Result.FirstSeen;
end;

function NowUtc: TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(Now);
end;

function ParseObject(const AJson: string): TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  if not Assigned(Result) then
    raise Exception.Create('Not a JSON object: ' + AJson);
end;

function HeaderOf(const AHeaders: TArray<TOtlpHeader>; const AName: string): string;
var
  LHeader: TOtlpHeader;
begin
  Result := '';
  for LHeader in AHeaders do
    if SameText(LHeader.Name, AName) then
      Exit(LHeader.Value);
end;

function AttributeOf(const AAttributes: TArray<TMetricLabel>; const AKey: string): string;
var
  LAttribute: TMetricLabel;
begin
  Result := '';
  for LAttribute in AAttributes do
    if LAttribute.Key = AKey then
      Exit(LAttribute.Value);
end;

{ TFakeOtlpSender }

constructor TFakeOtlpSender.Create;
begin
  inherited Create;
  Texts := TList<string>.Create;
  Status := 200;
end;

destructor TFakeOtlpSender.Destroy;
begin
  Texts.Free;
  inherited;
end;

function TFakeOtlpSender.HeaderValue(const AName: string): string;
begin
  Result := HeaderOf(LastHeaders, AName);
end;

function TFakeOtlpSender.Post(const AUrl: string;
  const AHeaders: TArray<TOtlpHeader>; const ABody: TBytes;
  ATimeoutMs: Integer): TOtlpHttpResponse;
var
  LSource: TBytesStream;
  LOutput: TBytesStream;
  LUnzip: TZDecompressionStream;
begin
  // Entered counts arrivals, Calls completions: an async test can tell "the
  // worker has not got there yet" from "the worker is inside Post, waiting".
  Inc(Entered);
  LastUrl := AUrl;
  LastHeaders := AHeaders;
  LastBody := ABody;

  if Assigned(Gate) then
    Gate.WaitFor(GATE_TIMEOUT_MS);

  if DelayMs > 0 then
    Sleep(DelayMs);

  Inc(Calls);

  if RaiseMessage <> '' then
    raise Exception.Create(RaiseMessage);

  if SameText(HeaderOf(AHeaders, 'Content-Encoding'), 'gzip') then
  begin
    LSource := TBytesStream.Create(ABody);
    LOutput := TBytesStream.Create;
    try
      LUnzip := TZDecompressionStream.Create(LSource, 15 + 16);
      try
        LOutput.CopyFrom(LUnzip, 0);
      finally
        LUnzip.Free;
      end;
      Texts.Add(TEncoding.UTF8.GetString(LOutput.Bytes, 0, LOutput.Size));
    finally
      LOutput.Free;
      LSource.Free;
    end;
  end
  else
    Texts.Add(TEncoding.UTF8.GetString(ABody));

  Result.StatusCode := Status;
  Result.Body := Body;
  Result.RetryAfter := RetryAfter;
end;

{ TMetricsOtlpPayloadTest }

procedure TMetricsOtlpPayloadTest.TestCounter_IsMonotonicCumulativeSum();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson(
    [MakePoint('m', 'tool.calls', TMetricKind.Counter, [], 3, 5, 1, 2, 2)], [], NowUtc));
  try
    Assert.AreEqual('tool.calls', LDoc.GetValue<string>(METRIC_PATH + '.name'));
    Assert.AreEqual(2, LDoc.GetValue<Integer>(METRIC_PATH + '.sum.aggregationTemporality'));
    Assert.IsTrue(LDoc.GetValue<Boolean>(METRIC_PATH + '.sum.isMonotonic'));
    Assert.AreEqual(Double(5), LDoc.GetValue<Double>(METRIC_PATH + '.sum.dataPoints[0].asDouble'), 0.0001);
    Assert.IsNotEmpty(LDoc.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].startTimeUnixNano'));
    Assert.IsNotEmpty(LDoc.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].timeUnixNano'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestGauge_HasValueAndNoStartTime();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson(
    [MakePoint('m', 'queue.depth', TMetricKind.Gauge, [], 4, 30, 1, 12, 7)], [], NowUtc));
  try
    Assert.AreEqual(Double(7), LDoc.GetValue<Double>(METRIC_PATH + '.gauge.dataPoints[0].asDouble'), 0.0001);
    Assert.IsNull(LDoc.FindValue(METRIC_PATH + '.gauge.dataPoints[0].startTimeUnixNano'));
    Assert.IsNull(LDoc.FindValue(METRIC_PATH + '.sum'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestHistogram_HasOneBucketHoldingEverySample();
var
  LDoc: TJSONObject;
  LPath: string;
begin
  LDoc := ParseObject(MetricsToOtlpJson(
    [MakePoint('m', 'tool.duration_ms', TMetricKind.Histogram, [], 4, 100, 5, 60, 20)], [], NowUtc));
  try
    LPath := METRIC_PATH + '.histogram';
    Assert.AreEqual(2, LDoc.GetValue<Integer>(LPath + '.aggregationTemporality'));
    LPath := LPath + '.dataPoints[0]';
    Assert.AreEqual('4', LDoc.GetValue<string>(LPath + '.count'));
    Assert.AreEqual(Double(100), LDoc.GetValue<Double>(LPath + '.sum'), 0.0001);
    Assert.AreEqual(Double(5), LDoc.GetValue<Double>(LPath + '.min'), 0.0001);
    Assert.AreEqual(Double(60), LDoc.GetValue<Double>(LPath + '.max'), 0.0001);
    Assert.AreEqual(1, LDoc.GetValue<TJSONArray>(LPath + '.bucketCounts').Count);
    Assert.AreEqual('4', LDoc.GetValue<string>(LPath + '.bucketCounts[0]'));
    Assert.AreEqual(0, LDoc.GetValue<TJSONArray>(LPath + '.explicitBounds').Count);
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestMeters_BecomeScopes();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson([
    MakePoint('mcp.server', 'b', TMetricKind.Counter, [], 1, 1, 1, 1, 1),
    MakePoint('', 'a', TMetricKind.Counter, [], 1, 1, 1, 1, 1)], [], NowUtc, 'fallback'));
  try
    Assert.AreEqual(2, LDoc.GetValue<TJSONArray>('resourceMetrics[0].scopeMetrics').Count);
    Assert.AreEqual('fallback', LDoc.GetValue<string>('resourceMetrics[0].scopeMetrics[0].scope.name'));
    Assert.AreEqual('a', LDoc.GetValue<string>('resourceMetrics[0].scopeMetrics[0].metrics[0].name'));
    Assert.AreEqual('mcp.server', LDoc.GetValue<string>('resourceMetrics[0].scopeMetrics[1].scope.name'));
    Assert.AreEqual('b', LDoc.GetValue<string>('resourceMetrics[0].scopeMetrics[1].metrics[0].name'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestSeriesOfOneMetric_ShareTheMetric();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson([
    MakePoint('m', 'tool.calls', TMetricKind.Counter, ['tool', 'b'], 1, 1, 1, 1, 1),
    MakePoint('m', 'tool.calls', TMetricKind.Counter, ['tool', 'a'], 1, 2, 2, 2, 2)], [], NowUtc));
  try
    Assert.AreEqual(1, LDoc.GetValue<TJSONArray>('resourceMetrics[0].scopeMetrics[0].metrics').Count);
    Assert.AreEqual(2, LDoc.GetValue<TJSONArray>(METRIC_PATH + '.sum.dataPoints').Count);
    Assert.AreEqual('tool', LDoc.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].attributes[0].key'));
    Assert.AreEqual('a', LDoc.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].attributes[0].value.stringValue'));
    Assert.AreEqual('b', LDoc.GetValue<string>(METRIC_PATH + '.sum.dataPoints[1].attributes[0].value.stringValue'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestResource_IsRendered();
var
  LDoc: TJSONObject;
  LResource: TArray<TMetricLabel>;
begin
  SetLength(LResource, 1);
  LResource[0].Key := 'service.name';
  LResource[0].Value := 'mcp-server';
  LDoc := ParseObject(MetricsToOtlpJson([], LResource, NowUtc));
  try
    Assert.AreEqual('service.name', LDoc.GetValue<string>('resourceMetrics[0].resource.attributes[0].key'));
    Assert.AreEqual('mcp-server', LDoc.GetValue<string>('resourceMetrics[0].resource.attributes[0].value.stringValue'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestUnixNano_IsNanosecondsSinceEpoch();
begin
  Assert.AreEqual('1757894400000000000', OtlpUnixNano(EncodeDateTime(2025, 9, 15, 0, 0, 0, 0)));
  Assert.AreEqual('1757894400250000000', OtlpUnixNano(EncodeDateTime(2025, 9, 15, 0, 0, 0, 250)));
  Assert.AreEqual('0', OtlpUnixNano(UnixDateDelta));
end;

procedure TMetricsOtlpPayloadTest.TestInt64Fields_AreStrings();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson([
    MakePoint('m', 'h', TMetricKind.Histogram, [], 2, 3, 1, 2, 2)], [], NowUtc));
  try
    Assert.IsTrue(LDoc.FindValue(METRIC_PATH + '.histogram.dataPoints[0].count') is TJSONString);
    Assert.IsTrue(LDoc.FindValue(METRIC_PATH + '.histogram.dataPoints[0].startTimeUnixNano') is TJSONString);
    Assert.IsTrue(LDoc.FindValue(METRIC_PATH + '.histogram.dataPoints[0].timeUnixNano') is TJSONString);
    Assert.IsTrue(LDoc.FindValue(METRIC_PATH + '.histogram.aggregationTemporality') is TJSONNumber);
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestNaN_IsTheProtoJsonString();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson([
    MakePoint('m', 'g', TMetricKind.Gauge, [], 1, NaN, NaN, NaN, NaN)], [], NowUtc));
  try
    Assert.AreEqual('NaN', LDoc.GetValue<string>(METRIC_PATH + '.gauge.dataPoints[0].asDouble'));
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpPayloadTest.TestNoPoints_IsStillAValidRequest();
var
  LDoc: TJSONObject;
begin
  LDoc := ParseObject(MetricsToOtlpJson([], [], NowUtc));
  try
    Assert.AreEqual(1, LDoc.GetValue<TJSONArray>('resourceMetrics').Count);
    Assert.AreEqual(0, LDoc.GetValue<TJSONArray>('resourceMetrics[0].scopeMetrics').Count);
  finally
    LDoc.Free;
  end;
end;

{ TMetricsOtlpExporterTest }

procedure TMetricsOtlpExporterTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
  FSender := TFakeOtlpSender.Create;
  FSenderRef := FSender;
  FExporter := TOtlpMetricExporter.Create('http://collector:4318/v1/metrics', FSenderRef);
  FExporterRef := FExporter;
  // The delivery assertions in this fixture are deliberately synchronous: after
  // Harvest they read the counters, which only describe the attempt once it has
  // been made. The async path has its own fixture below, and the backoff its own
  // tests (the tests that fail an export twice need it off).
  FExporter.Async := False;
  FProvider.AddExporter(FExporterRef);
end;

procedure TMetricsOtlpExporterTest.TearDown();
begin
  FProvider := nil;
  FExporterRef := nil;
  FExporter := nil;
  FSenderRef := nil;
  FSender := nil;
end;

function TMetricsOtlpExporterTest.Doc(AIndex: Integer): TJSONObject;
begin
  Result := ParseObject(FSender.Texts[AIndex]);
end;

function TMetricsOtlpExporterTest.CounterSum(AIndex: Integer): Double;
var
  LDoc: TJSONObject;
begin
  LDoc := Doc(AIndex);
  try
    Result := LDoc.GetValue<Double>(METRIC_PATH + '.sum.dataPoints[0].asDouble');
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpExporterTest.TestResetHarvests_AccumulateTotals();
var
  LCounter: ICounter;
begin
  LCounter := FProvider.GetMeter('m').Counter('c');
  LCounter.Add(2);
  FProvider.Harvest(True);
  LCounter.Add(3);
  FProvider.Harvest(True);

  Assert.AreEqual(2, FSender.Calls);
  Assert.AreEqual(Double(2), CounterSum(0), 0.0001);
  Assert.AreEqual(Double(5), CounterSum(1), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestPlainHarvests_DoNotDoubleCount();
var
  LCounter: ICounter;
begin
  LCounter := FProvider.GetMeter('m').Counter('c');
  LCounter.Add(2);
  FProvider.Harvest(False);
  LCounter.Add(3);
  FProvider.Harvest(False);

  Assert.AreEqual(Double(2), CounterSum(0), 0.0001);
  Assert.AreEqual(Double(5), CounterSum(1), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestMixedHarvests_StayCumulative();
var
  LCounter: ICounter;
begin
  LCounter := FProvider.GetMeter('m').Counter('c');
  LCounter.Add(2);
  FProvider.Harvest(True);
  LCounter.Add(3);
  FProvider.Harvest(False);
  LCounter.Add(1);
  FProvider.Harvest(True);
  LCounter.Add(4);
  FProvider.Harvest(False);

  Assert.AreEqual(Double(2), CounterSum(0), 0.0001);
  Assert.AreEqual(Double(5), CounterSum(1), 0.0001);
  Assert.AreEqual(Double(6), CounterSum(2), 0.0001);
  Assert.AreEqual(Double(10), CounterSum(3), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestStartTime_DoesNotMove();
var
  LCounter: ICounter;
  LFirst, LSecond: TJSONObject;
begin
  LCounter := FProvider.GetMeter('m').Counter('c');
  LCounter.Add(1);
  FProvider.Harvest(True);
  Sleep(20); // a new series after the reset gets a later FirstSeen
  LCounter.Add(1);
  FProvider.Harvest(True);

  LFirst := Doc(0);
  LSecond := Doc(1);
  try
    Assert.AreEqual(
      LFirst.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].startTimeUnixNano'),
      LSecond.GetValue<string>(METRIC_PATH + '.sum.dataPoints[0].startTimeUnixNano'));
  finally
    LSecond.Free;
    LFirst.Free;
  end;
end;

procedure TMetricsOtlpExporterTest.TestClearOutsideHarvest_KeepsCountingUp();
var
  LCounter: ICounter;
begin
  LCounter := FProvider.GetMeter('m').Counter('c');
  LCounter.Add(2);
  LCounter.Add(2);
  LCounter.Add(2);
  FProvider.Harvest(False);
  FProvider.Clear;
  LCounter.Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual(Double(6), CounterSum(0), 0.0001);
  Assert.AreEqual(Double(7), CounterSum(1), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestIdleCounter_IsStillReportedAfterReset();
begin
  FProvider.GetMeter('m').Counter('c').Add(4);
  FProvider.Harvest(True);
  FProvider.Harvest(True);

  Assert.AreEqual(2, FSender.Calls);
  Assert.AreEqual(Double(4), CounterSum(1), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestHistogram_MergesAcrossResets();
var
  LHistogram: IHistogram;
  LDoc: TJSONObject;
  LPath: string;
begin
  LHistogram := FProvider.GetMeter('m').Histogram('h');
  LHistogram.Observe(10);
  LHistogram.Observe(20);
  FProvider.Harvest(True);
  LHistogram.Observe(5);
  FProvider.Harvest(True);

  LDoc := Doc(1);
  try
    LPath := METRIC_PATH + '.histogram.dataPoints[0]';
    Assert.AreEqual('3', LDoc.GetValue<string>(LPath + '.count'));
    Assert.AreEqual(Double(35), LDoc.GetValue<Double>(LPath + '.sum'), 0.0001);
    Assert.AreEqual(Double(5), LDoc.GetValue<Double>(LPath + '.min'), 0.0001);
    Assert.AreEqual(Double(20), LDoc.GetValue<Double>(LPath + '.max'), 0.0001);
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpExporterTest.TestGauge_IsSentOnlyWhenHarvested();
var
  LDoc: TJSONObject;
begin
  FProvider.GetMeter('m').Gauge('g').SetValue(7);
  FProvider.Harvest(True);
  FProvider.Harvest(True);

  Assert.AreEqual(1, FSender.Calls, 'a reset gauge with no new value is not re-sent');
  LDoc := Doc(0);
  try
    Assert.AreEqual(Double(7), LDoc.GetValue<Double>(METRIC_PATH + '.gauge.dataPoints[0].asDouble'), 0.0001);
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpExporterTest.TestNoSeries_SendsNothing();
begin
  FProvider.Harvest(False);
  Assert.AreEqual(0, FSender.Calls);
end;

procedure TMetricsOtlpExporterTest.TestRequest_CarriesUrlAndHeaders();
begin
  FExporter.AddHeader('api-key', 'secret');
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual('http://collector:4318/v1/metrics', FSender.LastUrl);
  Assert.AreEqual('application/json', FSender.HeaderValue('Content-Type'));
  Assert.AreEqual('secret', FSender.HeaderValue('api-key'));
  Assert.AreEqual('', FSender.HeaderValue('Content-Encoding'));
end;

procedure TMetricsOtlpExporterTest.TestCompression_GzipsTheBody();
begin
  FExporter.Compression := True;
  FProvider.GetMeter('m').Counter('c').Add(3);
  FProvider.Harvest(False);

  Assert.AreEqual('gzip', FSender.HeaderValue('Content-Encoding'));
  Assert.IsTrue(Length(FSender.LastBody) > 2);
  Assert.AreEqual(Byte($1F), FSender.LastBody[0]);
  Assert.AreEqual(Byte($8B), FSender.LastBody[1]);
  Assert.AreEqual(Double(3), CounterSum(0), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestTransportFailure_DoesNotRaiseAndLosesNothing();
var
  LReported: string;
begin
  LReported := '';
  FExporter.OnError :=
    procedure(AMessage: string)
    begin
      LReported := AMessage;
    end;
  // This test fails once and succeeds immediately after: the backoff, which
  // would hold the second attempt back, is the subject of its own tests.
  FExporter.Backoff := False;
  FSender.RaiseMessage := 'connection refused';
  FProvider.GetMeter('m').Counter('c').Add(2);

  Assert.WillNotRaise(
    procedure
    begin
      FProvider.Harvest(True);
    end);
  Assert.AreEqual(Int64(1), FExporter.FailedCount);
  Assert.Contains(FExporter.LastError, 'connection refused');
  Assert.Contains(LReported, 'connection refused');

  // The reset wiped the provider, not the exporter's running total
  FSender.RaiseMessage := '';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(True);
  Assert.AreEqual(Int64(1), FExporter.SentCount);
  Assert.AreEqual(Double(3), CounterSum(0), 0.0001);
end;

procedure TMetricsOtlpExporterTest.TestHttpError_IsCountedAndReported();
begin
  FSender.Status := 400;
  FSender.Body := 'bad request';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual(Int64(0), FExporter.SentCount);
  Assert.AreEqual(Int64(1), FExporter.FailedCount);
  Assert.Contains(FExporter.LastError, 'HTTP 400');
end;

procedure TMetricsOtlpExporterTest.TestRetryAfter_SkipsHarvestsUntilElapsed();
begin
  FSender.Status := 429;
  FSender.RetryAfter := '3600';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);
  Assert.AreEqual(1, FSender.Calls);

  FSender.Status := 200;
  FProvider.Harvest(False);
  Assert.AreEqual(1, FSender.Calls, 'no POST before Retry-After elapses');
  Assert.AreEqual(Int64(1), FExporter.SkippedCount);
end;

procedure TMetricsOtlpExporterTest.TestPartialSuccess_IsReportedNotFailed();
var
  LReported: string;
begin
  LReported := '';
  FExporter.OnError :=
    procedure(AMessage: string)
    begin
      LReported := AMessage;
    end;
  FSender.Body := '{"partialSuccess":{"rejectedDataPoints":"2","errorMessage":"bad unit"}}';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual(Int64(1), FExporter.SentCount);
  Assert.AreEqual(Int64(0), FExporter.FailedCount);
  Assert.Contains(LReported, 'bad unit');
end;

procedure TMetricsOtlpExporterTest.TestPlainExport_SendsWithoutHarvestInfo();
begin
  FProvider.GetMeter('m').Counter('c').Add(5);
  FExporterRef.Export(FProvider.Collect);

  Assert.AreEqual(1, FSender.Calls);
  Assert.AreEqual(Double(5), CounterSum(0), 0.0001);
end;

{ TMetricsOtlpDeliveryTest }

procedure TMetricsOtlpDeliveryTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
  FSender := TFakeOtlpSender.Create;
  FSenderRef := FSender;
  FExporter := TOtlpMetricExporter.Create('http://collector:4318/v1/metrics', FSenderRef);
  FExporterRef := FExporter;
  FProvider.AddExporter(FExporterRef);
end;

procedure TMetricsOtlpDeliveryTest.TearDown();
begin
  // Let a POST still in flight finish before the objects it touches go away.
  if Assigned(FExporter) then
    FExporter.Flush(5000);

  FGate.Free;
  FGate := nil;
  FSender.Gate := nil;
  FProvider := nil;
  FExporterRef := nil;
  FExporter := nil;
  FSenderRef := nil;
  FSender := nil;
end;

function TMetricsOtlpDeliveryTest.LastDoc: TJSONObject;
begin
  Result := ParseObject(FSender.Texts[FSender.Texts.Count - 1]);
end;

function TMetricsOtlpDeliveryTest.LastSum: Double;
var
  LDoc: TJSONObject;
begin
  LDoc := LastDoc;
  try
    Result := LDoc.GetValue<Double>(METRIC_PATH + '.sum.dataPoints[0].asDouble');
  finally
    LDoc.Free;
  end;
end;

procedure TMetricsOtlpDeliveryTest.TestAsync_ExportReturnsBeforeThePost();
begin
  // A gate the test opens by hand: until then the POST is inside the worker,
  // which is what makes "the harvest did not wait for it" observable rather
  // than a matter of timing.
  FGate := TEvent.Create(nil, True, False, '');
  FSender.Gate := FGate;
  FExporter.Async := True;

  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual(Int64(0), FExporter.SentCount, 'the harvest does not wait for the POST');
  Assert.IsFalse(FExporter.Flush(200), 'the queue is still busy while the POST is held');

  FGate.SetEvent;

  Assert.IsTrue(FExporter.Flush(5000), 'the queue drains once the POST may run');
  Assert.AreEqual(Int64(1), FExporter.SentCount);
  Assert.AreEqual(1, FSender.Calls);
  Assert.AreEqual(Double(1), LastSum, 0.0001);
end;

procedure TMetricsOtlpDeliveryTest.TestAsync_KeepsOnlyTheNewestPendingBody();
begin
  FExporter.Async := True;
  // The first POST takes its time, so the two harvests behind it queue up.
  FSender.DelayMs := 300;

  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);
  Sleep(50);

  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.IsTrue(FExporter.Flush(10000), 'the queue drains');

  // How many of the three bodies actually went is timing; what matters is that
  // dropping one loses nothing, because every body is a cumulative snapshot.
  Assert.IsTrue(FSender.Calls >= 1, 'at least the first body was sent');
  Assert.IsTrue(FExporter.DroppedCount >= 1, 'a queued body was replaced, not queued forever');
  Assert.AreEqual(Double(3), LastSum, 0.0001,
    'the body that went last carries all three recordings');
end;

procedure TMetricsOtlpDeliveryTest.TestBackoff_SkipsUntilItElapses();
begin
  FExporter.BackoffBaseMs := 60000;   // longer than this test can last

  FSender.RaiseMessage := 'connection refused';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);

  Assert.AreEqual(Int64(1), FExporter.FailedCount);
  Assert.AreEqual(1, FSender.Entered);
  Assert.AreEqual(1, FExporter.ConsecutiveFailures);

  // The collector is back, but the exporter is waiting: no attempt, and the
  // skip is counted rather than reported.
  FSender.RaiseMessage := '';
  FProvider.Harvest(False);

  Assert.AreEqual(1, FSender.Entered, 'no second attempt inside the backoff');
  Assert.AreEqual(Int64(1), FExporter.SkippedCount);
  Assert.AreEqual(Int64(0), FExporter.SentCount);
end;

procedure TMetricsOtlpDeliveryTest.TestBackoff_ResetsOnSuccess();
begin
  FExporter.BackoffBaseMs := 1;

  FSender.RaiseMessage := 'connection refused';
  FProvider.GetMeter('m').Counter('c').Add(1);
  FProvider.Harvest(False);
  Assert.AreEqual(1, FExporter.ConsecutiveFailures);

  FSender.RaiseMessage := '';
  Sleep(20);

  FProvider.Harvest(False);

  Assert.AreEqual(Int64(1), FExporter.SentCount, 'the attempt happens once the wait is over');
  Assert.AreEqual(0, FExporter.ConsecutiveFailures, 'and a success clears the run');
end;

{ TMetricsOtlpEnvironmentTest }

function TMetricsOtlpEnvironmentTest.FromEnv(const APairs: array of string): TOtlpMetricExporter;
var
  LVariables: TDictionary<string, string>;
  LSender: IOtlpHttpSender;
  I: Integer;
begin
  // Held in a variable: a constructor passed straight to a const interface
  // parameter is never released when the callee raises before keeping it
  LSender := TFakeOtlpSender.Create;
  LVariables := TDictionary<string, string>.Create;
  try
    I := 0;
    while I < High(APairs) do
    begin
      LVariables.AddOrSetValue(APairs[I], APairs[I + 1]);
      Inc(I, 2);
    end;
    Result := TOtlpMetricExporter.FromEnvironment(
      function(AName: string): string
      begin
        if not LVariables.TryGetValue(AName, Result) then
          Result := '';
      end,
      LSender);
  finally
    LVariables.Free;
  end;
end;

procedure TMetricsOtlpEnvironmentTest.TestDefaults();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv([]);
  LRef := LExporter;
  Assert.AreEqual(OTLP_DEFAULT_ENDPOINT, LExporter.Endpoint);
  Assert.IsFalse(LExporter.Compression);
  Assert.AreEqual(OTLP_DEFAULT_TIMEOUT, LExporter.Timeout);
  Assert.AreEqual(0, Length(LExporter.Headers));
  Assert.StartsWith('unknown_service:', AttributeOf(LExporter.ResourceAttributes, 'service.name'));
end;

procedure TMetricsOtlpEnvironmentTest.TestSignalEndpoint_IsUsedAsIs();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv([
    'OTEL_EXPORTER_OTLP_METRICS_ENDPOINT', 'http://a:4318/custom',
    'OTEL_EXPORTER_OTLP_ENDPOINT', 'http://b:4318']);
  LRef := LExporter;
  Assert.AreEqual('http://a:4318/custom', LExporter.Endpoint);
end;

procedure TMetricsOtlpEnvironmentTest.TestBaseEndpoint_GetsTheMetricsPath();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv(['OTEL_EXPORTER_OTLP_ENDPOINT', 'http://b:4318/']);
  LRef := LExporter;
  Assert.AreEqual('http://b:4318/v1/metrics', LExporter.Endpoint);
end;

procedure TMetricsOtlpEnvironmentTest.TestHeaders_ArePercentDecoded();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv(['OTEL_EXPORTER_OTLP_HEADERS', 'api-key=ab+c%3D%3D, x-tenant = acme']);
  LRef := LExporter;
  Assert.AreEqual('ab+c==', HeaderOf(LExporter.Headers, 'api-key'));
  Assert.AreEqual('acme', HeaderOf(LExporter.Headers, 'x-tenant'));
end;

procedure TMetricsOtlpEnvironmentTest.TestServiceName_OverridesResourceAttributes();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv([
    'OTEL_RESOURCE_ATTRIBUTES', 'service.name=from-attributes,deployment.environment=dev',
    'OTEL_SERVICE_NAME', 'svc']);
  LRef := LExporter;
  Assert.AreEqual('svc', AttributeOf(LExporter.ResourceAttributes, 'service.name'));
  Assert.AreEqual('dev', AttributeOf(LExporter.ResourceAttributes, 'deployment.environment'));
end;

procedure TMetricsOtlpEnvironmentTest.TestCompressionAndTimeout();
var
  LExporter: TOtlpMetricExporter;
  LRef: IMetricExporter;
begin
  LExporter := FromEnv([
    'OTEL_EXPORTER_OTLP_COMPRESSION', 'gzip',
    'OTEL_EXPORTER_OTLP_METRICS_TIMEOUT', '2500']);
  LRef := LExporter;
  Assert.IsTrue(LExporter.Compression);
  Assert.AreEqual(2500, LExporter.Timeout);
end;

procedure TMetricsOtlpEnvironmentTest.TestProtocolOtherThanJson_Raises();
begin
  Assert.WillRaise(
    procedure
    begin
      FromEnv(['OTEL_EXPORTER_OTLP_PROTOCOL', 'grpc']);
    end, EOtlpExporterError);
end;

initialization
  TDUnitX.RegisterTestFixture(TMetricsOtlpPayloadTest);
  TDUnitX.RegisterTestFixture(TMetricsOtlpExporterTest);
  TDUnitX.RegisterTestFixture(TMetricsOtlpEnvironmentTest);

end.
