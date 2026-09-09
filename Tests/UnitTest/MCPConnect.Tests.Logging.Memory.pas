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
unit MCPConnect.Tests.Logging.Memory;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,

  Logify,
  MCPConnect.Logging.Memory;

type
  /// <summary>
  ///   The [PERF] grammar, checked against the shapes the library really emits
  ///   (MCPConnect.MCP.Invoker, MCPConnect.MCP.Server.Api,
  ///   MCPConnect.Transport.Base, JRPC.Invoker, JRPC.Server).
  /// </summary>
  [TestFixture]
  TPerfSampleParseTest = class(TObject)
  public
    [Test]
    procedure TestParse_EveryShapeTheLibraryEmits();
    [Test]
    procedure TestParse_ToolPhase();
    [Test]
    procedure TestParse_KeyKeepsTheWholeLabel();
    [Test]
    procedure TestParse_WithoutTarget();
    [Test]
    procedure TestParse_LabelOfOneWord();
    [Test]
    procedure TestParse_TrailingDetailIsNotTheDuration();
    [Test]
    procedure TestParse_BracedDuration();
    [Test]
    procedure TestParse_ColonInsideTheTarget();
    [Test]
    procedure TestParse_TimestampIsLeftToTheCaller();
    [Test]
    procedure TestParse_IgnoresLeadingSpaces();
    [Test]
    procedure TestParse_RejectsNonPerfLine();
    [Test]
    procedure TestParse_RejectsLineWithoutDuration();
    [Test]
    procedure TestParse_RejectsOtherUnits();
    [Test]
    procedure TestParse_RejectsEmptyLabel();
    [Test]
    procedure TestIsPerfLine();
  end;

  /// <summary>
  ///   Capture, ring buffers and aggregation.
  /// </summary>
  [TestFixture]
  TMemoryLogStoreTest = class(TObject)
  private
    FStore: TMCPMemoryLog;
    FSeen: Integer;
    FLastKey: string;
    procedure CapturePerf(const AText: string);
    procedure CaptureText(const AText: string);
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestCapture_KeepsPerfSample();
    [Test]
    procedure TestCapture_NonPerfLineIsNotASample();
    [Test]
    procedure TestCapture_KeepsLogEntriesWhenAsked();
    [Test]
    procedure TestCapture_DropsLogEntriesWithoutCapacity();
    [Test]
    procedure TestStats_Aggregate();
    [Test]
    procedure TestStats_SurviveTheRingRollingOver();
    [Test]
    procedure TestPerfSamples_KeepTheNewestOnes();
    [Test]
    procedure TestPerfStats_SlowestFirst();
    [Test]
    procedure TestMaxKeys_BoundsTheStatistics();
    [Test]
    procedure TestClear_ResetsEverything();
    [Test]
    procedure TestOnPerfSample_FiresForPerfLinesOnly();
    [Test]
    procedure TestPerfReport_ListsTheKeys();
  end;

  /// <summary>
  ///   The adapter itself, driven through Logify the way an application does.
  /// </summary>
  [TestFixture]
  TMemoryAdapterTest = class(TObject)
  private const
    FACTORY_NAME = 'MCPConnect.Tests.Memory';
  private
    FStore: TMCPMemoryLog;
    FFactory: ILoggerAdapterFactory;
  public
    [Setup]
    procedure Setup();
    [TearDown]
    procedure TearDown();

    [Test]
    procedure TestAdapter_CapturesPerfLineLoggedAtDebug();
    [Test]
    procedure TestAdapter_CapturesFormattedPerfLine();
    [Test]
    procedure TestAdapter_KeepsTheClassNameAndTheLevel();
    [Test]
    procedure TestAdapter_SkipsLinesBelowItsLevel();
    [Test]
    procedure TestAdapter_CapturesRawLines();
    [Test]
    procedure TestAdapter_RecordsTheException();
    [Test]
    procedure TestInstall_IsIdempotentAndUninstallDetaches();
  end;

implementation

const
  /// <summary>
  ///   Every [PERF] shape emitted in the tree, as "line|scope|target|phase|ms"
  ///   ('-' where the line carries no target). Keep this list in step with the
  ///   Logger.LogDebug('[PERF] ...') calls: a new one that does not parse here
  ///   is a measurement TMCPMemoryLog would silently ignore.
  /// </summary>
  PERF_SHAPES: array[0..29] of string = (
    // MCPConnect.MCP.Invoker
    '[PERF] Tool [get_weather] ArgumentsToRttiParams: 1 ms|Tool|get_weather|ArgumentsToRttiParams|1',
    '[PERF] Tool [get_weather] Method.Invoke (business logic): 2 ms|Tool|get_weather|Method.Invoke (business logic)|2',
    '[PERF] Tool [get_weather] ResultToTool (result serialization): 3 ms|Tool|get_weather|ResultToTool (result serialization)|3',
    '[PERF] Resource [resource://report] Method.Invoke (business logic): 4 ms|Resource|resource://report|Method.Invoke (business logic)|4',
    '[PERF] Resource [resource://report] ResultToResource (result serialization): 5 ms|Resource|resource://report|ResultToResource (result serialization)|5',
    '[PERF] Template [resource://city/{name}] BuildTemplateParams: 6 ms|Template|resource://city/{name}|BuildTemplateParams|6',
    '[PERF] Template [resource://city/{name}] Method.Invoke (business logic): 7 ms|Template|resource://city/{name}|Method.Invoke (business logic)|7',
    '[PERF] Template [resource://city/{name}] ResultToResource (result serialization): 8 ms|Template|resource://city/{name}|ResultToResource (result serialization)|8',
    '[PERF] Prompt [summarize] ArgumentsToRttiParams: 9 ms|Prompt|summarize|ArgumentsToRttiParams|9',
    '[PERF] Prompt [summarize] Method.Invoke (business logic): 10 ms|Prompt|summarize|Method.Invoke (business logic)|10',
    '[PERF] Prompt [summarize] ResultToPrompt (result serialization): 11 ms|Prompt|summarize|ResultToPrompt (result serialization)|11',
    '[PERF] Completion [summarize/topic] Method.Invoke (business logic): 12 ms|Completion|summarize/topic|Method.Invoke (business logic)|12',
    // MCPConnect.MCP.Server.Api
    '[PERF] CallTool [add] total: 13 ms|CallTool|add|total|13',
    '[PERF] ToolsList total: 14 ms|ToolsList|-|total|14',
    '[PERF] ReadResource [resource://report] total: 15 ms|ReadResource|resource://report|total|15',
    '[PERF] ResourcesList total: 16 ms|ResourcesList|-|total|16',
    '[PERF] PromptList total: 17 ms|PromptList|-|total|17',
    '[PERF] ReadPrompt [summarize] total: 18 ms|ReadPrompt|summarize|total|18',
    '[PERF] Complete [summarize/topic] total: 19 ms|Complete|summarize/topic|total|19',
    // MCPConnect.Transport.Base
    '[PERF] Transport [POST] Dispatch: 20 ms|Transport|POST|Dispatch|20',
    '[PERF] Transport RequestConverter: 21 ms|Transport|-|RequestConverter|21',
    '[PERF] Transport [POST /mcp] total: 22 ms (HTTP: 200)|Transport|POST /mcp|total|22',
    '[PERF] Transport CreateFromJSON: 23 ms|Transport|-|CreateFromJSON|23',
    '[PERF] Transport CreateAsyncQueue: 24 ms|Transport|-|CreateAsyncQueue|24',
    // JRPC.Invoker / JRPC.Server
    '[PERF] JRPC [tools/call] FindMethod: 25 ms|JRPC|tools/call|FindMethod|25',
    '[PERF] JRPC [tools/call] RequestToRttiParams: 26 ms|JRPC|tools/call|RequestToRttiParams|26',
    '[PERF] JRPC [tools/call] Method.Invoke: 27 ms|JRPC|tools/call|Method.Invoke|27',
    '[PERF] JRPC [tools/call] ValueToJSON: 28 ms|JRPC|tools/call|ValueToJSON|28',
    '[PERF] JRPC CreateFromJson: 29 ms|JRPC|-|CreateFromJson|29',
    '[PERF] JRPC ProcessRequest total: 30 ms|JRPC|-|ProcessRequest total|30'
  );

{ TPerfSampleParseTest }

procedure TPerfSampleParseTest.TestParse_EveryShapeTheLibraryEmits();
var
  LCase: string;
  LParts: TArray<string>;
  LTarget: string;
  LSample: TMCPPerfSample;
begin
  for LCase in PERF_SHAPES do
  begin
    LParts := LCase.Split(['|']);
    Assert.AreEqual(5, Length(LParts), 'malformed test case: ' + LCase);

    LTarget := LParts[2];
    if LTarget = '-' then
      LTarget := '';

    Assert.IsTrue(TMCPPerfSample.TryParse(LParts[0], LSample), LParts[0]);
    Assert.AreEqual(LParts[1], LSample.Scope, LParts[0]);
    Assert.AreEqual(LTarget, LSample.Target, LParts[0]);
    Assert.AreEqual(LParts[3], LSample.Phase, LParts[0]);
    Assert.AreEqual(StrToInt64(LParts[4]), LSample.ElapsedMs, LParts[0]);
  end;
end;

procedure TPerfSampleParseTest.TestParse_ToolPhase();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsTrue(TMCPPerfSample.TryParse(
    '[PERF] Tool [get_weather] Method.Invoke (business logic): 42 ms', LSample));

  Assert.AreEqual('Tool', LSample.Scope);
  Assert.AreEqual('get_weather', LSample.Target);
  Assert.AreEqual('Method.Invoke (business logic)', LSample.Phase);
  Assert.AreEqual(Int64(42), LSample.ElapsedMs);
  Assert.AreEqual('', LSample.Detail);
end;

procedure TPerfSampleParseTest.TestParse_KeyKeepsTheWholeLabel();
var
  LFirst, LSecond: TMCPPerfSample;
begin
  // The target is part of the key: two tools must not share a statistic
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] Tool [add] total: 1 ms', LFirst));
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] Tool [sub] total: 1 ms', LSecond));

  Assert.AreEqual('Tool [add] total', LFirst.Key);
  Assert.AreNotEqual(LFirst.Key, LSecond.Key);
end;

procedure TPerfSampleParseTest.TestParse_WithoutTarget();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] JRPC ProcessRequest total: 51 ms', LSample));

  Assert.AreEqual('JRPC', LSample.Scope);
  Assert.AreEqual('', LSample.Target);
  Assert.AreEqual('ProcessRequest total', LSample.Phase);
  Assert.AreEqual(Int64(51), LSample.ElapsedMs);
end;

procedure TPerfSampleParseTest.TestParse_LabelOfOneWord();
var
  LSample: TMCPPerfSample;
begin
  // Not emitted any more, but the grammar still allows a label with no phase
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] HandleCOMMAND: 7 ms', LSample));

  Assert.AreEqual('HandleCOMMAND', LSample.Scope);
  Assert.AreEqual('', LSample.Phase);
  Assert.AreEqual(Int64(7), LSample.ElapsedMs);
end;

procedure TPerfSampleParseTest.TestParse_TrailingDetailIsNotTheDuration();
var
  LSample: TMCPPerfSample;
begin
  // "(HTTP: 200)" carries the only other colon on the line
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] Transport [POST /mcp] total: 55 ms (HTTP: 200)', LSample));

  Assert.AreEqual(Int64(55), LSample.ElapsedMs);
  Assert.AreEqual('Transport', LSample.Scope);
  Assert.AreEqual('POST /mcp', LSample.Target);
  Assert.AreEqual('total', LSample.Phase);
  Assert.AreEqual('(HTTP: 200)', LSample.Detail);
end;

procedure TPerfSampleParseTest.TestParse_BracedDuration();
var
  LSample: TMCPPerfSample;
begin
  // The braces were a typo in Transport.Base, fixed since; still tolerated
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] CreateAsyncQueue total: {12} ms', LSample));
  Assert.AreEqual(Int64(12), LSample.ElapsedMs);
end;

procedure TPerfSampleParseTest.TestParse_ColonInsideTheTarget();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsTrue(TMCPPerfSample.TryParse(
    '[PERF] Resource [resource://weather/today] ResultToResource (result serialization): 3 ms', LSample));

  Assert.AreEqual('Resource', LSample.Scope);
  Assert.AreEqual('resource://weather/today', LSample.Target);
  Assert.AreEqual(Int64(3), LSample.ElapsedMs);
end;

procedure TPerfSampleParseTest.TestParse_TimestampIsLeftToTheCaller();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsTrue(TMCPPerfSample.TryParse('[PERF] ToolsList total: 2 ms', LSample));

  Assert.AreEqual(Double(0), Double(LSample.Timestamp), 0.0);
  Assert.AreEqual(Int64(0), Int64(LSample.ThreadId));
end;

procedure TPerfSampleParseTest.TestParse_IgnoresLeadingSpaces();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsTrue(TMCPPerfSample.TryParse('   [PERF] ToolsList total: 2 ms', LSample));
  Assert.AreEqual('ToolsList total', LSample.Key);
end;

procedure TPerfSampleParseTest.TestParse_RejectsNonPerfLine();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsFalse(TMCPPerfSample.TryParse('Tool [add] answered in 42 ms', LSample));
  Assert.IsFalse(TMCPPerfSample.TryParse('', LSample));
end;

procedure TPerfSampleParseTest.TestParse_RejectsLineWithoutDuration();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF] Tool [add] starting', LSample));
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF] Tool [add] total: soon', LSample));
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF]', LSample));
end;

procedure TPerfSampleParseTest.TestParse_RejectsOtherUnits();
var
  LSample: TMCPPerfSample;
begin
  // Milliseconds are what the library logs; anything else would be misread
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF] Tool [add] total: 42 msec', LSample));
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF] Tool [add] total: 42 s', LSample));
end;

procedure TPerfSampleParseTest.TestParse_RejectsEmptyLabel();
var
  LSample: TMCPPerfSample;
begin
  Assert.IsFalse(TMCPPerfSample.TryParse('[PERF] : 42 ms', LSample));
end;

procedure TPerfSampleParseTest.TestIsPerfLine();
begin
  Assert.IsTrue(TMCPPerfSample.IsPerfLine('[PERF] anything'));
  Assert.IsTrue(TMCPPerfSample.IsPerfLine('  [perf] anything'));
  Assert.IsFalse(TMCPPerfSample.IsPerfLine('nothing [PERF] here'));
end;

{ TMemoryLogStoreTest }

procedure TMemoryLogStoreTest.Setup();
begin
  FStore := TMCPMemoryLog.Create(4, 3);
  FSeen := 0;
  FLastKey := '';
end;

procedure TMemoryLogStoreTest.TearDown();
begin
  FreeAndNil(FStore);
end;

procedure TMemoryLogStoreTest.CapturePerf(const AText: string);
begin
  CaptureText(AText);
end;

procedure TMemoryLogStoreTest.CaptureText(const AText: string);
begin
  FStore.Capture(TMCPLogEntry.New('TTestClass', AText, nil, TLogLevel.Debug));
end;

procedure TMemoryLogStoreTest.TestCapture_KeepsPerfSample();
var
  LSamples: TArray<TMCPPerfSample>;
begin
  CapturePerf('[PERF] Tool [add] total: 10 ms');

  LSamples := FStore.PerfSamples;
  Assert.AreEqual(1, Length(LSamples));
  Assert.AreEqual('Tool [add] total', LSamples[0].Key);
  Assert.AreEqual(Int64(10), LSamples[0].ElapsedMs);
  // The store, not the parser, dates the sample
  Assert.IsTrue(LSamples[0].Timestamp > 0);
  Assert.AreEqual(Int64(1), FStore.PerfCount);
end;

procedure TMemoryLogStoreTest.TestCapture_NonPerfLineIsNotASample();
begin
  CaptureText('Server started on port 8080');

  Assert.AreEqual(0, Length(FStore.PerfSamples));
  Assert.AreEqual(Int64(0), FStore.PerfCount);
  Assert.AreEqual(Int64(1), FStore.LogCount);
end;

procedure TMemoryLogStoreTest.TestCapture_KeepsLogEntriesWhenAsked();
var
  LEntries: TArray<TMCPLogEntry>;
begin
  CaptureText('first');
  CaptureText('second');

  LEntries := FStore.LogEntries;
  Assert.AreEqual(2, Length(LEntries));
  // Oldest first
  Assert.AreEqual('first', LEntries[0].Text);
  Assert.AreEqual('second', LEntries[1].Text);
  Assert.AreEqual('TTestClass', LEntries[0].LogClass);
end;

procedure TMemoryLogStoreTest.TestCapture_DropsLogEntriesWithoutCapacity();
var
  LStore: TMCPMemoryLog;
begin
  // The parameterless constructor is the perf-only one
  LStore := TMCPMemoryLog.Create;
  try
    LStore.Capture(TMCPLogEntry.New('', '[PERF] ToolsList total: 5 ms', nil, TLogLevel.Debug));
    LStore.Capture(TMCPLogEntry.New('', 'just a log line', nil, TLogLevel.Debug));

    Assert.AreEqual(0, Length(LStore.LogEntries));
    Assert.AreEqual(Int64(2), LStore.LogCount);
    // The measurement is still there: only the raw lines are dropped
    Assert.AreEqual(1, Length(LStore.PerfSamples));
  finally
    LStore.Free;
  end;
end;

procedure TMemoryLogStoreTest.TestStats_Aggregate();
var
  LStat: TMCPPerfStat;
begin
  CapturePerf('[PERF] Tool [add] total: 10 ms');
  CapturePerf('[PERF] Tool [add] total: 30 ms');
  CapturePerf('[PERF] Tool [add] total: 20 ms');

  Assert.IsTrue(FStore.TryGetStat('Tool [add] total', LStat));
  Assert.AreEqual(Int64(3), LStat.Count);
  Assert.AreEqual(Int64(60), LStat.TotalMs);
  Assert.AreEqual(Int64(10), LStat.MinMs);
  Assert.AreEqual(Int64(30), LStat.MaxMs);
  Assert.AreEqual(Int64(20), LStat.LastMs);
  Assert.AreEqual(Double(20), LStat.AverageMs, 0.001);
  Assert.AreEqual('add', LStat.Target);
end;

procedure TMemoryLogStoreTest.TestStats_SurviveTheRingRollingOver();
var
  LStat: TMCPPerfStat;
begin
  // Six samples into a four-slot ring
  for var I := 1 to 6 do
    CapturePerf(Format('[PERF] Tool [add] total: %d ms', [I]));

  Assert.AreEqual(4, Length(FStore.PerfSamples));
  Assert.IsTrue(FStore.TryGetStat('Tool [add] total', LStat));
  Assert.AreEqual(Int64(6), LStat.Count);
  Assert.AreEqual(Int64(21), LStat.TotalMs);
  Assert.AreEqual(Int64(1), LStat.MinMs);
  Assert.AreEqual(Int64(6), LStat.MaxMs);
  Assert.AreEqual(Int64(6), FStore.PerfCount);
end;

procedure TMemoryLogStoreTest.TestPerfSamples_KeepTheNewestOnes();
var
  LSamples: TArray<TMCPPerfSample>;
begin
  for var I := 1 to 6 do
    CapturePerf(Format('[PERF] Tool [add] total: %d ms', [I]));

  LSamples := FStore.PerfSamples;
  Assert.AreEqual(4, Length(LSamples));
  Assert.AreEqual(Int64(3), LSamples[0].ElapsedMs);
  Assert.AreEqual(Int64(6), LSamples[High(LSamples)].ElapsedMs);
end;

procedure TMemoryLogStoreTest.TestPerfStats_SlowestFirst();
var
  LStats: TArray<TMCPPerfStat>;
begin
  CapturePerf('[PERF] Tool [fast] total: 5 ms');
  CapturePerf('[PERF] Tool [slow] total: 50 ms');
  CapturePerf('[PERF] Tool [fast] total: 5 ms');

  LStats := FStore.PerfStats;
  Assert.AreEqual(2, Length(LStats));
  Assert.AreEqual('Tool [slow] total', LStats[0].Key);
  Assert.AreEqual('Tool [fast] total', LStats[1].Key);
end;

procedure TMemoryLogStoreTest.TestMaxKeys_BoundsTheStatistics();
var
  LStat: TMCPPerfStat;
begin
  FStore.MaxKeys := 1;

  CapturePerf('[PERF] Tool [first] total: 10 ms');
  CapturePerf('[PERF] Tool [second] total: 20 ms');
  CapturePerf('[PERF] Tool [first] total: 10 ms');

  Assert.AreEqual(1, Length(FStore.PerfStats));
  Assert.AreEqual(Int64(1), FStore.DroppedKeys);
  // The sample itself is still kept, it just has no statistic of its own
  Assert.AreEqual(3, Length(FStore.PerfSamples));
  Assert.IsTrue(FStore.TryGetStat('Tool [first] total', LStat));
  Assert.AreEqual(Int64(2), LStat.Count);
end;

procedure TMemoryLogStoreTest.TestClear_ResetsEverything();
begin
  CapturePerf('[PERF] Tool [add] total: 10 ms');
  CaptureText('a log line');

  FStore.Clear;

  Assert.AreEqual(0, Length(FStore.PerfSamples));
  Assert.AreEqual(0, Length(FStore.PerfStats));
  Assert.AreEqual(0, Length(FStore.LogEntries));
  Assert.AreEqual(Int64(0), FStore.PerfCount);
  Assert.AreEqual(Int64(0), FStore.LogCount);
  Assert.AreEqual(Int64(0), FStore.DroppedKeys);
end;

procedure TMemoryLogStoreTest.TestOnPerfSample_FiresForPerfLinesOnly();
begin
  FStore.OnPerfSample :=
    procedure(const ASample: TMCPPerfSample)
    begin
      Inc(FSeen);
      FLastKey := ASample.Key;
    end;

  CapturePerf('[PERF] Tool [add] total: 10 ms');
  CaptureText('not a measurement');

  Assert.AreEqual(1, FSeen);
  Assert.AreEqual('Tool [add] total', FLastKey);
end;

procedure TMemoryLogStoreTest.TestPerfReport_ListsTheKeys();
var
  LReport: string;
begin
  CapturePerf('[PERF] Tool [add] total: 10 ms');
  CapturePerf('[PERF] JRPC ProcessRequest total: 40 ms');

  LReport := FStore.PerfReport;

  Assert.Contains(LReport, 'Tool [add] total');
  Assert.Contains(LReport, 'JRPC ProcessRequest total');
  Assert.Contains(LReport, '2 samples, 2 keys');
end;

{ TMemoryAdapterTest }

procedure TMemoryAdapterTest.Setup();
begin
  FStore := TMCPMemoryLog.Create(16, 16);
  FFactory := TLogifyAdapterMemoryFactory.CreateAdapterFactory(
    FACTORY_NAME, TLogLevel.Trace, FStore);
  TLoggerAdapterRegistry.Instance.RegisterFactory(FFactory);
end;

procedure TMemoryAdapterTest.TearDown();
begin
  // The adapter has to go before the store it writes into
  TLoggerAdapterRegistry.Instance.UnregisterFactory(FFactory);
  FFactory := nil;
  FreeAndNil(FStore);
end;

procedure TMemoryAdapterTest.TestAdapter_CapturesPerfLineLoggedAtDebug();
var
  LSamples: TArray<TMCPPerfSample>;
begin
  Logger.LogDebug('[PERF] Tool [add] total: 12 ms');

  LSamples := FStore.PerfSamples;
  Assert.AreEqual(1, Length(LSamples));
  Assert.AreEqual('Tool [add] total', LSamples[0].Key);
  Assert.AreEqual(Int64(12), LSamples[0].ElapsedMs);
end;

procedure TMemoryAdapterTest.TestAdapter_CapturesFormattedPerfLine();
var
  LStat: TMCPPerfStat;
begin
  // The shape MCPConnect.MCP.Invoker actually logs
  Logger.LogDebug('[PERF] Tool [%s] Method.Invoke (business logic): %d ms', ['get_weather', 8]);

  Assert.IsTrue(FStore.TryGetStat('Tool [get_weather] Method.Invoke (business logic)', LStat));
  Assert.AreEqual(Int64(8), LStat.TotalMs);
end;

procedure TMemoryAdapterTest.TestAdapter_KeepsTheClassNameAndTheLevel();
var
  LEntries: TArray<TMCPLogEntry>;
begin
  TLoggerManager.GetLogger(TMemoryAdapterTest).LogWarning('careful');

  LEntries := FStore.LogEntries;
  Assert.AreEqual(1, Length(LEntries));
  Assert.AreEqual('careful', LEntries[0].Text);
  Assert.Contains(LEntries[0].LogClass, 'TMemoryAdapterTest');
  Assert.IsTrue(LEntries[0].Level = TLogLevel.Warning);
  Assert.Contains(LEntries[0].ToString, 'WARNING');
end;

procedure TMemoryAdapterTest.TestAdapter_SkipsLinesBelowItsLevel();
var
  LAdapter: ILoggerAdapter;
begin
  // Not registered: driven directly, so the level filter is the only thing
  // between the line and the store
  LAdapter := TLogifyAdapterMemory.Create(FStore, TLogLevel.Error);

  LAdapter.WriteLog('', '[PERF] Tool [add] total: 1 ms', nil, TLogLevel.Debug);
  Assert.AreEqual(Int64(0), FStore.PerfCount);

  LAdapter.WriteLog('', '[PERF] Tool [add] total: 2 ms', nil, TLogLevel.Error);
  Assert.AreEqual(Int64(1), FStore.PerfCount);
end;

procedure TMemoryAdapterTest.TestAdapter_CapturesRawLines();
begin
  Logger.LogRawLine('[PERF] ToolsList total: 4 ms', TLogLevel.Debug);

  Assert.AreEqual(Int64(1), FStore.PerfCount);
  Assert.AreEqual(1, Length(FStore.PerfSamples));
end;

procedure TMemoryAdapterTest.TestAdapter_RecordsTheException();
var
  LEntries: TArray<TMCPLogEntry>;
  LException: Exception;
begin
  LException := EProgrammerNotFound.Create('boom');
  try
    Logger.LogError(LException, 'tool call failed');
  finally
    LException.Free;
  end;

  LEntries := FStore.LogEntries;
  Assert.AreEqual(1, Length(LEntries));
  Assert.AreEqual('tool call failed', LEntries[0].Text);
  Assert.Contains(LEntries[0].ExceptionInfo, 'EProgrammerNotFound');
  Assert.Contains(LEntries[0].ExceptionInfo, 'boom');
end;

procedure TMemoryAdapterTest.TestInstall_IsIdempotentAndUninstallDetaches();
var
  LStore: TMCPMemoryLog;
begin
  LStore := TMCPMemoryLog.Install(TLogLevel.Debug);
  try
    Assert.AreSame(TMCPMemoryLog.Default, LStore);
    // Twice is a no-op, and what was already collected stays
    Assert.AreSame(LStore, TMCPMemoryLog.Install(TLogLevel.Debug));

    Logger.LogDebug('[PERF] CallTool [add] total: 9 ms');
    Assert.AreEqual(Int64(1), LStore.PerfCount);

    TMCPMemoryLog.Uninstall;
    Logger.LogDebug('[PERF] CallTool [add] total: 9 ms');
    Assert.AreEqual(Int64(1), LStore.PerfCount, 'the detached adapter kept capturing');
  finally
    TMCPMemoryLog.Uninstall;
    // The default store outlives the test: leave it empty for the next one
    LStore.Clear;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPerfSampleParseTest);
  TDUnitX.RegisterTestFixture(TMemoryLogStoreTest);
  TDUnitX.RegisterTestFixture(TMemoryAdapterTest);

end.
