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
unit MCPConnect.Tests.Metrics;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.IOUtils,
  DUnitX.TestFramework,

  Logify,
  MCPConnect.Logging.Memory,
  MCPConnect.Metrics,
  MCPConnect.Metrics.Exporters,
  MCPConnect.Metrics.Exporters.Files,
  MCPConnect.Metrics.Exporters.Logify;

type
  /// <summary>Exporter that just counts Harvest calls and keeps the last batch.</summary>
  TCountingExporter = class(TInterfacedObject, IMetricExporter)
  public
    Count: Integer;
    LastPoints: TArray<TMetricPoint>;
    procedure Export(const APoints: TArray<TMetricPoint>);
  end;

  /// <summary>Recording semantics of the three instrument kinds.</summary>
  [TestFixture]
  TMetricsInstrumentTest = class(TObject)
  private
    FProvider: IMeterProvider;
    FMeter: IMeter;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestCounter_AccumulatesTotalAndCount();
    [Test]
    procedure TestCounter_AddDefaultsToOne();
    [Test]
    procedure TestCounter_RejectsNegativeAdds();
    [Test]
    procedure TestGauge_KeepsTheLastValue();
    [Test]
    procedure TestHistogram_KeepsCountSumMinMax();
    [Test]
    procedure TestWrongKindOperation_Raises();
    [Test]
    procedure TestSameNameDifferentKind_Raises();
    [Test]
    procedure TestEmptyInstrumentName_Raises();
    [Test]
    procedure TestInstrument_IsCachedByName();
    [Test]
    procedure TestInstrument_ExposesMetadata();
    [Test]
    procedure TestInstrument_SnapshotIsIndependent();
  end;

  /// <summary>Labels: series splitting, canonical order, bounds.</summary>
  [TestFixture]
  TMetricsLabelsTest = class(TObject)
  private
    FProvider: IMeterProvider;
    FMeter: IMeter;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestLabels_SplitSeries();
    [Test]
    procedure TestLabels_OrderDoesNotMatter();
    [Test]
    procedure TestLabels_DuplicateKeyLastWins();
    [Test]
    procedure TestLabels_OddCountRaises();
    [Test]
    procedure TestLabels_AreSortedInThePoint();
    [Test]
    procedure TestMaxSeries_BoundsDistinctLabelSets();
    [Test]
    procedure TestMaxSeries_ZeroIsUnbounded();
    [Test]
    procedure TestClear_DropsSeriesAndCounters();
  end;

  /// <summary>Meters, collection and the exporters.</summary>
  [TestFixture]
  TMetricsHarvestTest = class(TObject)
  private
    FProvider: IMeterProvider;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestMeters_AreIsolated();
    [Test]
    procedure TestCollect_IsSortedAndStable();
    [Test]
    procedure TestProviderClear_KeepsInstrumentsWorking();
    [Test]
    procedure TestTextExporter_WritesOneBlockPerHarvest();
    [Test]
    procedure TestJsonExporter_WritesOneLinePerHarvest();
    [Test]
    procedure TestHarvest_ResetExportsTheDelta();
    [Test]
    procedure TestHarvest_RunsExportersInOrder();
    [Test]
    procedure TestAddExporter_IgnoresDuplicates();
    [Test]
    procedure TestRemoveExporter_StopsDeliveries();
  end;

  /// <summary>Concurrent recording keeps exact totals.</summary>
  [TestFixture]
  TMetricsConcurrencyTest = class(TObject)
  private
    FProvider: IMeterProvider;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestConcurrentAdds_NoLostUpdate();
  end;

  /// <summary>The TMetrics facade over the process wide default provider.</summary>
  [TestFixture]
  TMetricsFacadeTest = class(TObject)
  private
    FExporter: TCountingExporter;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestDefault_IsCached();
    [Test]
    procedure TestOneLiners_RecordOnTheDefaultMeter();
    [Test]
    procedure TestNamedMeter_KeepsItsOwnSeries();
    [Test]
    procedure TestClassHarvest_ReachesDefaultExporters();
  end;

  /// <summary>The TMetricFileExporter sink (JSON lines and text blocks).</summary>
  [TestFixture]
  TMetricsFileExporterTest = class(TObject)
  private
    FProvider: IMeterProvider;
    FFileName: string;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestJsonExport_AppendsOneLinePerHarvest();
    [Test]
    procedure TestTextExport_AppendsReadableBlocks();
  end;

  /// <summary>The TMetricLogifyExporter sink, captured through a memory adapter.</summary>
  [TestFixture]
  TMetricsLogifyExporterTest = class(TObject)
  private
    FStore: TMCPMemoryLog;
    FFactory: ILoggerAdapterFactory;
    FProvider: IMeterProvider;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestExport_LogsOneEntryPerHarvestAsJson();
    [Test]
    procedure TestExport_AsTextLogsTheBlock();
    [Test]
    procedure TestExport_EveryHarvestIsALogEntry();
  end;

implementation

uses
  System.Diagnostics;

{ TCountingExporter }

procedure TCountingExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  Inc(Count);
  LastPoints := APoints;
end;

{ TMetricsInstrumentTest }

procedure TMetricsInstrumentTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
  FMeter := FProvider.GetMeter('test');
end;

procedure TMetricsInstrumentTest.TearDown();
begin
  FProvider := nil;
  FMeter := nil;
end;

procedure TMetricsInstrumentTest.TestCounter_AccumulatesTotalAndCount();
var
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
begin
  LCounter := FMeter.Counter('calls', 'Tool calls', 'calls');
  LCounter.Add(1);
  LCounter.Add(2);
  LCounter.Add(2.5);

  LPoints := LCounter.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(Double(5.5), LPoints[0].Sum, 0.0001);
  Assert.AreEqual(Int64(3), LPoints[0].Count);
  Assert.AreEqual(TMetricKind.Counter, LPoints[0].Kind);
end;

procedure TMetricsInstrumentTest.TestCounter_AddDefaultsToOne();
var
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
begin
  LCounter := FMeter.Counter('hits');
  LCounter.Add();
  LCounter.Add();
  LCounter.Add(3);

  LPoints := LCounter.Collect;
  Assert.AreEqual(Double(5), LPoints[0].Sum, 0.0001);
  Assert.AreEqual(Int64(3), LPoints[0].Count);
end;

procedure TMetricsInstrumentTest.TestCounter_RejectsNegativeAdds();
var
  LCounter: ICounter;
begin
  LCounter := FMeter.Counter('guarded');
  Assert.WillRaise(
    procedure
    begin
      LCounter.Add(-1);
    end, EMetricsError);
  // Nothing was recorded
  Assert.AreEqual(0, Length(LCounter.Collect));
end;

procedure TMetricsInstrumentTest.TestGauge_KeepsTheLastValue();
var
  LGauge: IGauge;
  LPoints: TArray<TMetricPoint>;
begin
  LGauge := FMeter.Gauge('connections', 'Active connections');
  LGauge.SetValue(1);
  LGauge.SetValue(4);
  LGauge.SetValue(3);

  LPoints := LGauge.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(Double(3), LPoints[0].Last, 0.0001);
  Assert.AreEqual(TMetricKind.Gauge, LPoints[0].Kind);
end;

procedure TMetricsInstrumentTest.TestHistogram_KeepsCountSumMinMax();
var
  LHistogram: IHistogram;
  LPoints: TArray<TMetricPoint>;
begin
  LHistogram := FMeter.Histogram('latency_ms', 'Tool latency', 'ms');
  LHistogram.Observe(10);
  LHistogram.Observe(30);
  LHistogram.Observe(20);

  LPoints := LHistogram.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(Int64(3), LPoints[0].Count);
  Assert.AreEqual(Double(60), LPoints[0].Sum, 0.0001);
  Assert.AreEqual(Double(10), LPoints[0].Min, 0.0001);
  Assert.AreEqual(Double(30), LPoints[0].Max, 0.0001);
  Assert.AreEqual(Double(20), LPoints[0].Last, 0.0001);
  Assert.AreEqual(TMetricKind.Histogram, LPoints[0].Kind);
end;

procedure TMetricsInstrumentTest.TestWrongKindOperation_Raises();
var
  LCounter: ICounter;
begin
  // The concrete instrument implements every typed interface; the operation
  // still has to match the kind it was created with
  LCounter := FMeter.Counter('typed');
  Assert.WillRaise(
    procedure
    begin
      (LCounter as IGauge).SetValue(1);
    end, EMetricsError);
  Assert.WillRaise(
    procedure
    begin
      (LCounter as IHistogram).Observe(1);
    end, EMetricsError);
end;

procedure TMetricsInstrumentTest.TestSameNameDifferentKind_Raises();
begin
  FMeter.Counter('dup');
  Assert.WillRaise(
    procedure
    begin
      FMeter.Gauge('dup');
    end, EMetricsError);
end;

procedure TMetricsInstrumentTest.TestEmptyInstrumentName_Raises();
begin
  Assert.WillRaise(
    procedure
    begin
      FMeter.Counter('   ');
    end, EMetricsError);
end;

procedure TMetricsInstrumentTest.TestInstrument_IsCachedByName();
var
  LCounter: ICounter;
begin
  LCounter := FMeter.Counter('shared');
  FMeter.Counter('shared', 'A different description');
  FMeter.Counter('shared', '', 'unit');

  LCounter.Add(1);
  FMeter.Counter('shared').Add(1);

  Assert.AreEqual(1, Length(LCounter.Collect));
  Assert.AreEqual(Double(2), LCounter.Collect[0].Sum, 0.0001);
end;

procedure TMetricsInstrumentTest.TestInstrument_ExposesMetadata();
var
  LHistogram: IHistogram;
  LPoint: TMetricPoint;
begin
  LHistogram := FMeter.Histogram('meta', 'Description text', 'samples');
  LHistogram.Observe(1);

  Assert.AreEqual('meta', LHistogram.Name);
  Assert.AreEqual(TMetricKind.Histogram, LHistogram.Kind);
  Assert.AreEqual('Description text', LHistogram.Description);
  Assert.AreEqual('samples', LHistogram.UnitName);

  LPoint := LHistogram.Collect[0];
  Assert.AreEqual('meta', LPoint.Name);
  Assert.AreEqual('test', LPoint.Meter);
  Assert.AreEqual('Description text', LPoint.Description);
  Assert.AreEqual('samples', LPoint.UnitName);
  Assert.IsTrue(LPoint.FirstSeen > 0);
  Assert.IsTrue(LPoint.LastSeen >= LPoint.FirstSeen);
end;

procedure TMetricsInstrumentTest.TestInstrument_SnapshotIsIndependent();
var
  LCounter: ICounter;
  LFirst: TArray<TMetricPoint>;
begin
  LCounter := FMeter.Counter('snapshot');
  LCounter.Add(1);

  LFirst := LCounter.Collect;
  LCounter.Add(1);
  LCounter.Add(1);

  // The earlier snapshot is a copy: later recordings did not touch it
  Assert.AreEqual(Double(1), LFirst[0].Sum, 0.0001);
  Assert.AreEqual(Double(3), LCounter.Collect[0].Sum, 0.0001);
end;

{ TMetricsLabelsTest }

procedure TMetricsLabelsTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
end;

procedure TMetricsLabelsTest.TearDown();
begin
  FProvider := nil;
  FMeter := nil;
end;

procedure TMetricsLabelsTest.TestLabels_SplitSeries();
var
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
begin
  FMeter := FProvider.GetMeter('labels');
  LCounter := FMeter.Counter('calls');
  LCounter.Add(1, ['tool', 'add']);
  LCounter.Add(1, ['tool', 'add']);
  LCounter.Add(1, ['tool', 'delete']);

  LPoints := LCounter.Collect;
  Assert.AreEqual(2, Length(LPoints));
  Assert.AreEqual(Int64(2), LCounter.SeriesCount);
end;

procedure TMetricsLabelsTest.TestLabels_OrderDoesNotMatter();
var
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
begin
  FMeter := FProvider.GetMeter('labels');
  LCounter := FMeter.Counter('calls');
  LCounter.Add(1, ['tool', 'add', 'scope', 'a']);
  LCounter.Add(1, ['scope', 'a', 'tool', 'add']);

  LPoints := LCounter.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(Int64(2), LPoints[0].Count);
end;

procedure TMetricsLabelsTest.TestLabels_DuplicateKeyLastWins();
var
  LCounter: ICounter;
  LPoints: TArray<TMetricPoint>;
begin
  FMeter := FProvider.GetMeter('labels');
  LCounter := FMeter.Counter('calls');
  LCounter.Add(1, ['tool', 'first', 'tool', 'second']);

  LPoints := LCounter.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(1, Length(LPoints[0].Labels));
  Assert.AreEqual('tool', LPoints[0].Labels[0].Key);
  Assert.AreEqual('second', LPoints[0].Labels[0].Value);
end;

procedure TMetricsLabelsTest.TestLabels_OddCountRaises();
var
  LCounter: ICounter;
begin
  FMeter := FProvider.GetMeter('labels');
  LCounter := FMeter.Counter('calls');
  Assert.WillRaise(
    procedure
    begin
      LCounter.Add(1, ['tool']);
    end, EMetricsError);
end;

procedure TMetricsLabelsTest.TestLabels_AreSortedInThePoint();
var
  LCounter: ICounter;
  LPoint: TMetricPoint;
begin
  FMeter := FProvider.GetMeter('labels');
  LCounter := FMeter.Counter('calls');
  LCounter.Add(1, ['zeta', '1', 'alpha', '2', 'middle', '3']);

  LPoint := LCounter.Collect[0];
  Assert.AreEqual(3, Length(LPoint.Labels));
  Assert.AreEqual('alpha', LPoint.Labels[0].Key);
  Assert.AreEqual('middle', LPoint.Labels[1].Key);
  Assert.AreEqual('zeta', LPoint.Labels[2].Key);
end;

procedure TMetricsLabelsTest.TestMaxSeries_BoundsDistinctLabelSets();
var
  LCounter: ICounter;
begin
  FProvider.MaxSeriesPerInstrument := 2;
  FMeter := FProvider.GetMeter('bounded');
  LCounter := FMeter.Counter('calls');

  LCounter.Add(1, ['tool', 'a']);
  LCounter.Add(1, ['tool', 'b']);
  LCounter.Add(1, ['tool', 'c']);

  Assert.AreEqual(2, LCounter.SeriesCount);
  Assert.AreEqual(Int64(1), LCounter.DroppedSeries);
  Assert.AreEqual(2, Length(LCounter.Collect));
end;

procedure TMetricsLabelsTest.TestMaxSeries_ZeroIsUnbounded();
var
  LCounter: ICounter;
  I: Integer;
begin
  FProvider.MaxSeriesPerInstrument := 0;
  FMeter := FProvider.GetMeter('unbounded');
  LCounter := FMeter.Counter('calls');

  for I := 1 to 300 do
    LCounter.Add(1, ['tool', 'tool' + IntToStr(I)]);

  Assert.AreEqual(300, LCounter.SeriesCount);
  Assert.AreEqual(Int64(0), LCounter.DroppedSeries);
  Assert.AreEqual(300, Length(LCounter.Collect));
end;

procedure TMetricsLabelsTest.TestClear_DropsSeriesAndCounters();
var
  LCounter: ICounter;
begin
  FProvider.MaxSeriesPerInstrument := 1;
  FMeter := FProvider.GetMeter('bounded');
  LCounter := FMeter.Counter('calls');

  LCounter.Add(1, ['tool', 'a']);
  LCounter.Add(1, ['tool', 'b']);
  Assert.AreEqual(Int64(1), LCounter.DroppedSeries);

  LCounter.Clear;
  Assert.AreEqual(0, LCounter.SeriesCount);
  Assert.AreEqual(Int64(0), LCounter.DroppedSeries);
  Assert.AreEqual(0, Length(LCounter.Collect));

  // Still usable after the clear
  LCounter.Add(1, ['tool', 'a']);
  Assert.AreEqual(Double(1), LCounter.Collect[0].Sum, 0.0001);
end;

{ TMetricsHarvestTest }

procedure TMetricsHarvestTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
end;

procedure TMetricsHarvestTest.TearDown();
begin
  FProvider := nil;
end;

procedure TMetricsHarvestTest.TestMeters_AreIsolated();
var
  LPoints: TArray<TMetricPoint>;
begin
  FProvider.GetMeter('first').Counter('x').Add(1);
  FProvider.GetMeter('second').Counter('x').Add(1);

  LPoints := FProvider.Collect;
  Assert.AreEqual(2, Length(LPoints));
end;

procedure TMetricsHarvestTest.TestCollect_IsSortedAndStable();
var
  LFirst, LSecond: TArray<TMetricPoint>;
  LKeyOf: string;
  I: Integer;
begin
  // Creation order deliberately opposite to the sort order
  FProvider.GetMeter('zzz').Counter('aaa').Add(1);
  FProvider.GetMeter('aaa').Counter('zzz').Add(1, ['k', 'z']);
  FProvider.GetMeter('aaa').Counter('bbb').Add(1);

  LFirst := FProvider.Collect;
  LSecond := FProvider.Collect;

  Assert.AreEqual(3, Length(LFirst));
  // Meter first, then instrument name
  Assert.AreEqual('aaa', LFirst[0].Meter);
  Assert.AreEqual('bbb', LFirst[0].Name);
  Assert.AreEqual('zzz', LFirst[1].Name);
  Assert.AreEqual('zzz', LFirst[2].Meter);

  for I := 0 to High(LFirst) do
  begin
    LKeyOf := LFirst[I].Meter + '/' + LFirst[I].Name;
    Assert.AreEqual(LKeyOf, LSecond[I].Meter + '/' + LSecond[I].Name,
      'consecutive harvests must line up point by point');
  end;
end;

procedure TMetricsHarvestTest.TestProviderClear_KeepsInstrumentsWorking();
var
  LCounter: ICounter;
begin
  LCounter := FProvider.GetMeter('x').Counter('calls');
  LCounter.Add(1);

  FProvider.Clear;
  Assert.AreEqual(0, Length(FProvider.Collect));

  LCounter.Add(1);
  Assert.AreEqual(Double(1), FProvider.Collect[0].Sum, 0.0001);
end;

procedure TMetricsHarvestTest.TestTextExporter_WritesOneBlockPerHarvest();
var
  LTarget: TStringList;
  LExporter: IMetricExporter;
  LText: string;
begin
  LTarget := TStringList.Create;
  try
    LExporter := TMetricTextExporter.Create(LTarget);
    FProvider.AddExporter(LExporter);

    FProvider.GetMeter('app').Counter('calls', '', 'calls').Add(3, ['tool', 'add_task']);
    FProvider.Harvest;
    FProvider.Harvest;

    Assert.AreEqual(2, LTarget.Count, 'one block per Harvest call');
    LText := LTarget[0];
    Assert.Contains(LText, '1 point(s)');
    Assert.Contains(LText, 'counter');
    Assert.Contains(LText, 'app/calls');
    Assert.Contains(LText, 'sum=3');
    Assert.Contains(LText, 'tool=add_task');
  finally
    LTarget.Free;
  end;
end;

procedure TMetricsHarvestTest.TestJsonExporter_WritesOneLinePerHarvest();
var
  LTarget: TStringList;
  LExporter: IMetricExporter;
  LJson: TJSONValue;
  LArray: TJSONArray;
begin
  LTarget := TStringList.Create;
  try
    LExporter := TMetricJsonExporter.Create(LTarget);
    FProvider.AddExporter(LExporter);

    FProvider.GetMeter('app').Histogram('latency_ms', 'Tool latency', 'ms')
      .Observe(10, ['tool', 'add_task']);
    FProvider.GetMeter('app').Histogram('latency_ms', 'Tool latency', 'ms')
      .Observe(30, ['tool', 'add_task']);
    FProvider.GetMeter('app').Histogram('latency_ms', 'Tool latency', 'ms')
      .Observe(1.5, ['tool', 'add_task']);
    FProvider.GetMeter('app').Counter('plain', '', 'calls').Add(1, ['tool', 'add_task']);
    FProvider.Harvest;

    Assert.AreEqual(1, LTarget.Count);
    // The line is a well formed JSON array that parses back
    Assert.Contains(LTarget[0], '"sum":41.5', 'fractional values must stay parseable');
    Assert.Contains(LTarget[0], '"kind":"histogram"');
    Assert.Contains(LTarget[0], '"unit":"ms"', 'NeonProperty rename to "unit" must hold');
    LJson := TJSONObject.ParseJSONValue(LTarget[0]);
    Assert.IsNotNull(LJson, 'the exporter must emit parseable JSON');
    try
      LArray := LJson as TJSONArray;
      Assert.AreEqual(2, LArray.Count);
      Assert.AreEqual('histogram', (LArray.Items[0] as TJSONObject).GetValue<string>('kind'));
      Assert.AreEqual('latency_ms', (LArray.Items[0] as TJSONObject).GetValue<string>('name'));
      Assert.AreEqual('ms', (LArray.Items[0] as TJSONObject).GetValue<string>('unit'));
      Assert.AreEqual(Double(41.5), (LArray.Items[0] as TJSONObject).GetValue<double>('sum'), 0.0001);
      Assert.AreEqual('add_task',
        ((LArray.Items[0] as TJSONObject).GetValue('labels') as TJSONObject)
          .GetValue<string>('tool'));
    finally
      LJson.Free;
    end;
  finally
    LTarget.Free;
  end;
end;

procedure TMetricsHarvestTest.TestHarvest_ResetExportsTheDelta();
var
  LTarget: TStringList;
  LExporter: IMetricExporter;
begin
  LTarget := TStringList.Create;
  try
    LExporter := TMetricJsonExporter.Create(LTarget);
    FProvider.AddExporter(LExporter);

    FProvider.GetMeter('app').Counter('calls').Add(2);
    FProvider.Harvest(True);
    Assert.Contains(LTarget[0], '"sum":2.0');

    // Reset emptied the provider: the next harvest is an empty array...
    FProvider.Harvest(True);
    Assert.AreEqual('[]', LTarget[1]);

    // ...and recording starts over from zero
    FProvider.GetMeter('app').Counter('calls').Add(1);
    FProvider.Harvest(True);
    Assert.Contains(LTarget[2], '"sum":1.0');
  finally
    LTarget.Free;
  end;
end;

procedure TMetricsHarvestTest.TestHarvest_RunsExportersInOrder();
var
  LExporter1: TCountingExporter;
  LExporter2: TCountingExporter;
begin
  LExporter1 := TCountingExporter.Create;
  LExporter2 := TCountingExporter.Create;
  FProvider.AddExporter(LExporter1);
  FProvider.AddExporter(LExporter2);

  FProvider.GetMeter('app').Counter('calls').Add(1);
  FProvider.Harvest;

  Assert.AreEqual(1, LExporter1.Count);
  Assert.AreEqual(1, LExporter2.Count);
  Assert.AreEqual(1, Length(LExporter1.LastPoints));
  Assert.AreEqual(1, Length(LExporter2.LastPoints));
end;

procedure TMetricsHarvestTest.TestAddExporter_IgnoresDuplicates();
var
  LExporter: TCountingExporter;
begin
  LExporter := TCountingExporter.Create;
  FProvider.AddExporter(LExporter);
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls').Add(1);
  FProvider.Harvest;
  Assert.AreEqual(1, LExporter.Count, 'the same exporter instance is registered once');
end;

procedure TMetricsHarvestTest.TestRemoveExporter_StopsDeliveries();
var
  LExporter: TCountingExporter;
begin
  LExporter := TCountingExporter.Create;
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls').Add(1);
  FProvider.Harvest;
  Assert.AreEqual(1, LExporter.Count);

  FProvider.RemoveExporter(LExporter);
  FProvider.Harvest;
  Assert.AreEqual(1, LExporter.Count, 'no deliveries after removal');
end;

{ TMetricsConcurrencyTest }

procedure TMetricsConcurrencyTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
end;

procedure TMetricsConcurrencyTest.TearDown();
begin
  FProvider := nil;
end;

procedure TMetricsConcurrencyTest.TestConcurrentAdds_NoLostUpdate();
const
  THREAD_COUNT = 4;
  ADDS_PER_THREAD = 500;
var
  LCounter: ICounter;
  LThreads: array [0..THREAD_COUNT - 1] of TThread;
  LPoints: TArray<TMetricPoint>;
  I: Integer;
begin
  LCounter := FProvider.GetMeter('conc').Counter('calls');

  for I := 0 to THREAD_COUNT - 1 do
  begin
    LThreads[I] := TThread.CreateAnonymousThread(
      procedure
      var
        J: Integer;
      begin
        for J := 1 to ADDS_PER_THREAD do
          LCounter.Add(1);
      end);
    // Anonymous threads free themselves when they end unless told otherwise,
    // which would make the WaitFor below race their destruction.
    LThreads[I].FreeOnTerminate := False;
  end;

  for I := 0 to THREAD_COUNT - 1 do
    LThreads[I].Start;

  for I := 0 to THREAD_COUNT - 1 do
  begin
    LThreads[I].WaitFor;
    LThreads[I].Free;
  end;

  LPoints := LCounter.Collect;
  Assert.AreEqual(1, Length(LPoints));
  Assert.AreEqual(Double(THREAD_COUNT * ADDS_PER_THREAD), LPoints[0].Sum, 0.0001);
  Assert.AreEqual(Int64(THREAD_COUNT * ADDS_PER_THREAD), LPoints[0].Count);
end;

{ TMetricsFacadeTest }

procedure TMetricsFacadeTest.Setup();
begin
  FExporter := TCountingExporter.Create;
  // Leave the shared default provider clean for every test
  TMetrics.Clear;
end;

procedure TMetricsFacadeTest.TearDown();
var
  LExp: IMetricExporter;
begin
  // Hand the instance to an interface variable so its reference count is
  // actually balanced: passing a raw class instance to an interface method
  // (a const parameter) does not AddRef it, so an exporter that was never
  // registered would otherwise never be released.
  LExp := FExporter;
  TMetrics.RemoveExporter(LExp);
  LExp := nil;
  TMetrics.Clear;
  FExporter := nil;
end;

procedure TMetricsFacadeTest.TestDefault_IsCached();
begin
  Assert.IsTrue(Pointer(TMetrics.Default) = Pointer(TMetrics.Default));
end;

procedure TMetricsFacadeTest.TestOneLiners_RecordOnTheDefaultMeter();
var
  LPoints: TArray<TMetricPoint>;
begin
  TMetrics.Counter('facade.calls').Add(1, ['tool', 'a']);
  TMetrics.Counter('facade.calls').Add(1, ['tool', 'a']);
  TMetrics.Gauge('facade.workers').SetValue(3);
  TMetrics.Histogram('facade.latency').Observe(7);

  LPoints := TMetrics.Collect;
  Assert.AreEqual(3, Length(LPoints));
  // Points carry the default (empty) meter name
  Assert.AreEqual('', LPoints[0].Meter);
end;

procedure TMetricsFacadeTest.TestNamedMeter_KeepsItsOwnSeries();
var
  LPoints: TArray<TMetricPoint>;
begin
  TMetrics.Counter('same').Add(1);
  TMetrics.Meter('named').Counter('same').Add(1);

  LPoints := TMetrics.Collect;
  Assert.AreEqual(2, Length(LPoints), 'same instrument name, different meters');
  // Collect is sorted by meter name: the default (empty) one comes first
  Assert.AreEqual('', LPoints[0].Meter);
  Assert.AreEqual('named', LPoints[1].Meter);
end;

procedure TMetricsFacadeTest.TestClassHarvest_ReachesDefaultExporters();
begin
  TMetrics.AddExporter(FExporter);

  TMetrics.Counter('facade.calls').Add(2);
  TMetrics.Harvest;

  Assert.AreEqual(1, FExporter.Count);
  Assert.AreEqual(1, Length(FExporter.LastPoints));
  Assert.AreEqual(Double(2), FExporter.LastPoints[0].Sum, 0.0001);
end;

{ TMetricsFileExporterTest }

procedure TMetricsFileExporterTest.Setup();
begin
  FProvider := TMetrics.CreateProvider;
  FFileName := TPath.Combine(TPath.GetTempPath,
    'mcpmetrics-' + TGuid.NewGuid.ToString + '.jsonl');
end;

procedure TMetricsFileExporterTest.TearDown();
begin
  FProvider := nil;
  if (FFileName <> '') and TFile.Exists(FFileName) then
    TFile.Delete(FFileName);
end;

procedure TMetricsFileExporterTest.TestJsonExport_AppendsOneLinePerHarvest();
var
  LExporter: IMetricExporter;
  LLines: TArray<string>;
begin
  LExporter := TMetricFileExporter.Create(FFileName);
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls').Add(2);
  FProvider.Harvest;
  FProvider.Harvest;

  // Close the file (release the exporter) before reading it back
  FProvider.RemoveExporter(LExporter);
  LExporter := nil;
  LLines := TFile.ReadAllLines(FFileName);
  Assert.AreEqual(2, Length(LLines), 'one JSON line per harvest');
  Assert.Contains(LLines[0], '"name":"calls"');
  Assert.Contains(LLines[0], '"sum":2.0');
  Assert.Contains(LLines[1], '"sum":2.0');
end;

procedure TMetricsFileExporterTest.TestTextExport_AppendsReadableBlocks();
var
  LExporter: IMetricExporter;
  LContent: string;
begin
  LExporter := TMetricFileExporter.Create(FFileName, True);
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls', '', 'calls').Add(3, ['tool', 'add_task']);
  FProvider.Harvest;

  // Close the file (release the exporter) before reading it back
  FProvider.RemoveExporter(LExporter);
  LExporter := nil;
  LContent := TFile.ReadAllText(FFileName);
  Assert.Contains(LContent, '1 point(s)');
  Assert.Contains(LContent, 'counter app/calls');
  Assert.Contains(LContent, 'tool=add_task');
end;

{ TMetricsLogifyExporterTest }

procedure TMetricsLogifyExporterTest.Setup();
begin
  FStore := TMCPMemoryLog.Create(0, 32);
  FFactory := TLogifyAdapterMemoryFactory.CreateAdapterFactory(
    'MCPConnect.Tests.Metrics.Logify', TLogLevel.Trace, FStore);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FFactory);
  FProvider := TMetrics.CreateProvider;
end;

procedure TMetricsLogifyExporterTest.TearDown();
begin
  FProvider := nil;
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FFactory);
  FFactory := nil;
  FStore.Free;
end;

procedure TMetricsLogifyExporterTest.TestExport_LogsOneEntryPerHarvestAsJson();
var
  LExporter: IMetricExporter;
  LEntries: TArray<TMCPLogEntry>;
begin
  LExporter := TMetricLogifyExporter.Create();
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls').Add(2);
  FProvider.Harvest;

  LEntries := FStore.LogEntries;
  Assert.AreEqual(1, Length(LEntries));
  Assert.Contains(LEntries[0].Text, '"name":"calls"');
  Assert.Contains(LEntries[0].Text, '"sum":2.0');
end;

procedure TMetricsLogifyExporterTest.TestExport_AsTextLogsTheBlock();
var
  LExporter: IMetricExporter;
  LEntries: TArray<TMCPLogEntry>;
begin
  LExporter := TMetricLogifyExporter.Create(TLogLevel.Info, True);
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls', '', 'calls').Add(1, ['tool', 'add_task']);
  FProvider.Harvest;

  LEntries := FStore.LogEntries;
  Assert.AreEqual(1, Length(LEntries));
  Assert.Contains(LEntries[0].Text, '[metrics]');
  Assert.Contains(LEntries[0].Text, 'app/calls');
  Assert.Contains(LEntries[0].Text, 'tool=add_task');
end;

procedure TMetricsLogifyExporterTest.TestExport_EveryHarvestIsALogEntry();
var
  LExporter: IMetricExporter;
  LEntries: TArray<TMCPLogEntry>;
begin
  LExporter := TMetricLogifyExporter.Create(TLogLevel.Info);
  FProvider.AddExporter(LExporter);

  FProvider.GetMeter('app').Counter('calls').Add(1);
  FProvider.Harvest;
  FProvider.Harvest;

  LEntries := FStore.LogEntries;
  Assert.AreEqual(2, Length(LEntries), 'one log entry per harvest');
  Assert.Contains(LEntries[0].Text, 'calls');
  Assert.Contains(LEntries[1].Text, 'calls');
end;

initialization
  TDUnitX.RegisterTestFixture(TMetricsInstrumentTest);
  TDUnitX.RegisterTestFixture(TMetricsLabelsTest);
  TDUnitX.RegisterTestFixture(TMetricsHarvestTest);
  TDUnitX.RegisterTestFixture(TMetricsConcurrencyTest);
  TDUnitX.RegisterTestFixture(TMetricsFacadeTest);
  TDUnitX.RegisterTestFixture(TMetricsFileExporterTest);
  TDUnitX.RegisterTestFixture(TMetricsLogifyExporterTest);

end.
