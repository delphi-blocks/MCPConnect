# Metrics in MCPConnect

MCPConnect ships a small, interface based metrics subsystem - `MCPConnect.Metrics` - for
collecting measurements with one-liners and harvesting them later. It is deliberately modelled
on [OpenTelemetry](https://opentelemetry.io/docs/specs/otel/metrics/) and deliberately limited to
metrics: there is no OTLP, no traces, no wire protocol. Measurements accumulate in process,
thread safely, and are read back through a typed snapshot (`Collect`) or pushed to exporters
(`Harvest`).

This document explains:

1. What the subsystem does (and does not do).
2. The object model: providers, meters, instruments, points, exporters.
3. Recording measurements with one-liners.
4. Labels, cardinality and error handling.
5. Concurrency and lifetimes.
6. Harvesting: `Collect`, `Harvest`, and the `TMetricPoint` shape.
7. The sample exporters (`MCPConnect.Metrics.Exporters`) and how to write your own.
8. Using it inside an MCP server (the demo's `metrics_report` tool, and a resource variant).
9. Limitations and design notes.

The code lives in `Source/MCPConnect.Metrics.pas` (core: the RTL plus `Neon.Core.Attributes`,
for the attributes that pin the exported JSON shape - no serializer at runtime),
`Source/MCPConnect.Metrics.Exporters.pas` (text/JSON renderers) and
`Source/MCPConnect.Metrics.Exporters.Files.pas` (real sink, see 6.3).
Tests are in
`Tests/UnitTest/MCPConnect.Tests.Metrics.pas`. Requires Delphi 11 or newer.

## 1. Overview

Think of the API as a very small OpenTelemetry SDK for Delphi:

- a **MeterProvider** owns **Meters**, a Meter creates **instruments**, and every measurement may
  carry key/value **labels** (OpenTelemetry attributes);
- three instrument kinds are supported - **Counter**, **Gauge**, **Histogram** - and nothing else
  (no UpDownCounter: when a value can go down as well as up, use a Gauge);
- values accumulate in memory per instrument and per distinct label set, from any thread;
- "harvesting" means taking a typed snapshot of everything collected so far, or handing that
  snapshot to registered exporter implementations.

Recording is meant to stay a one-liner, and instruments are cached, so repeated lookups of the
same meter/instrument name return the same instance:

```delphi
uses
  MCPConnect.Metrics;

// count tool invocations, split by tool name
TMetrics
  .Counter('tool.calls', 'Tools invoked', 'calls')
  .Add(1, ['tool', 'get_weather']);

// a latency sample (milliseconds)
TMetrics
  .Histogram('tool.duration_ms', 'Tool latency', 'ms')
  .Observe(42.5, ['tool', 'get_weather']);

// a current-value snapshot
TMetrics.Gauge('active.requests').SetValue(FActive);
```

Harvesting later:

```delphi
var
  LPoints: TArray<TMetricPoint>;
begin
  LPoints := TMetrics.Collect;   // typed snapshot, sorted and stable

  // ... or push the snapshot to every registered exporter:
  TMetrics.Harvest;

  // ... with True, Harvest clears afterwards, so each export is a delta:
  TMetrics.Harvest(True);
end;
```

## 2. Object model

```text
                owns                     creates                aggregates
IMeterProvider -----> IMeter ----------> IInstrument ----------> series (per label set)
     |                     |                     |
     | GetMeter(name)      | Counter/Gauge/      | Collect/Clear        one TMetricPoint
     | Collect/Clear       | Histogram            |                      per series when read
     | Harvest             | Collect/Clear        |
     +-- IMetricExporter list (receives Collect snapshots on Harvest)
```

| Type | Role |
|---|---|
| `IMeterProvider` | Root object. Owns meters, the exporter list and the label budget (`MaxSeriesPerInstrument`). `GetMeter('')` returns the default (empty-named) meter. |
| `IMeter` | A named group of instruments, the OpenTelemetry Meter. Instruments of different meters never mix, whatever their names. |
| `IInstrument` | Identity (`Name`, `Kind`, `Description`, `UnitName`) plus `SeriesCount`, `DroppedSeries`, `Collect`, `Clear`. |
| `ICounter` / `IGauge` / `IHistogram` | The typed recorders: `Add`, `SetValue`, `Observe`. |
| `TMetricPoint` | One harvested series: identity + labels + `Count`/`Sum`/`Min`/`Max`/`Last` + `FirstSeen`/`LastSeen`. |
| `IMetricExporter` | Receives `Export(APoints)` once per `Harvest` call. |
| `TMetrics` | Static facade over a process-wide default provider, for the one-liner style. |

The facade and the interfaces are two views of the same thing - `TMetrics.Counter('x').Add(1)`
is exactly `TMetrics.Default.GetMeter('').Counter('x').Add(1)`.

## 3. Recording measurements

### 3.1 Instrument kinds

| Kind | Operation | Meaningful point fields | Notes |
|---|---|---|---|
| `Counter` | `Add(AValue = 1)` | `Sum` (running total), `Count` | Monotonic. Negative adds raise `EMetricsError`. Use it for totals: calls, errors, bytes. |
| `Gauge` | `SetValue(AValue)` | `Last` (current value) | Overwrites the series value. Use it for "how many right now": active requests, pool sizes. |
| `Histogram` | `Observe(AValue)` | `Count`, `Sum`, `Min`, `Max` (avg is `Sum / Count`) | One call per sample. No explicit buckets or percentiles - aggregates only. |

Every instrument records into the same shape, so `Count`/`Sum`/`Min`/`Max`/`Last` are always
filled; consumers interpret them by `Kind`. Values are stored as `Double`.

### 3.2 The one-liner facade

`TMetrics` fronts the process-wide default provider:

```delphi
TMetrics
  .Counter('todo.tool.calls', 'Todo tool invocations', 'calls')   // name, description, unit
  .Add(1, ['tool', 'add_task']);                                  // value, labels

TMetrics
  .Gauge('todo.tasks.total', 'Tasks currently in the list', 'tasks')
  .SetValue(TodoStore.CountTasks());

TMetrics
  .Histogram('todo.tool.duration_ms', 'Todo tool duration', 'ms')
  .Observe(LWatch.Elapsed.TotalMilliseconds, ['tool', 'add_task']);
```

`Add` defaults its value to 1, so a bare event is `...Counter('x').Add()` (or `Add(1)`). When an
instrument is recorded from several places, cache it in a field for a slightly cheaper call:

```delphi
type
  TMyService = class
  private
    FCalls: ICounter;   // created once, then FCalls.Add(1, [...]) anywhere
  end;

// ...
FCalls := TMetrics
  .Meter('myapp')
  .Counter('calls', 'Service calls', 'calls');
```

`Counter`, `Gauge` and `Histogram` lookups are cached per meter and instrument name: the first
call creates the instrument (its description/unit win), later calls return the same instance.

### 3.3 Meters

A meter is just a name that keeps instruments apart. It costs nothing to use one and it makes
harvests readable - the meter name lands in every point:

```delphi
TMetrics.Meter('mcp').Counter('tool.calls').Add(1, ['tool', 'add']);
TMetrics.Meter('app').Counter('tool.calls').Add(1, ['tool', 'add']);
// two independent counters: the same instrument name in two meters never mixes
```

`TMetrics.Meter()` with no argument (or `''`) is the default meter, which is what the bare
`TMetrics.Counter(...)` calls record into.

### 3.4 Labels

Labels are the OpenTelemetry attributes of the subsystem - optional key/value strings that split
one instrument into several series:

```delphi
TMetrics.Counter('tool.calls').Add(1, ['tool', 'add_task']);
TMetrics.Counter('tool.calls').Add(1, ['tool', 'delete_task']);
// two series under one instrument
```

Rules worth knowing:

- the array is an even number of strings - key, value, key, value - otherwise `EMetricsError`
  is raised;
- label order does not matter: `['tool','a','scope','b']` and `['scope','b','tool','a']` hit the
  same series;
- a repeated key keeps its last occurrence;
- series store labels sorted by key, so exported data is deterministic;
- a measurement without labels is one series under the instrument, exactly as if every recording
  carried the same empty label set.

Series are bounded per instrument to protect memory - label keys often carry names from the
outside world. The default budget is `TMetrics.DEFAULT_MAX_SERIES` (128); when it is reached
new label sets are dropped and counted in the instrument's `DroppedSeries` (a warning that
snapshots are incomplete). Configure the budget on the provider before asking for the meter,
since meters capture it when they are created:

```delphi
var
  LProvider: IMeterProvider;
begin
  LProvider := TMetrics.CreateProvider;      // a private provider
  LProvider.MaxSeriesPerInstrument := 1024;  // or 0 for unbounded
  LProvider
    .GetMeter('api')
    .Counter('calls')
    .Add(1, ['tenant', ATenantId]);
end;
```

### 3.5 Errors

Misuse raises `EMetricsError` instead of corrupting data:

- `Add` with a negative value (counters are monotonic; if a value can go down, use a `Gauge`);
- recording on an instrument through the wrong kind interface (e.g. `SetValue` on a counter);
- creating an instrument with a name already used by another kind in the same meter;
- an empty instrument name;
- an odd number of label strings;
- a negative `MaxSeriesPerInstrument`.

## 4. Concurrency and lifetimes

- Recording is thread safe: every instrument locks itself, so counters, gauges and histograms can
  be updated from any thread and never lose updates (covered by a test with four threads).
- `Collect` returns snapshots: values are copied under the lock, so a point array you already
  hold is not affected by later recordings or by `Clear`.
- Provider, meter and instrument registries are locked; exporter registration and harvesting can
  happen while other threads record.
- Exporters run on the thread that called `Harvest`, one after another, outside the provider's
  lock (an exporter may itself record metrics).
- Instruments are reference-counted interfaces. Hold them as `ICounter` etc. fields or locals.
- The default provider behind `TMetrics` is created on first use and released in the `TMetrics`
  class destructor, which runs after this unit's finalization. From that moment `TMetrics.Default`
  raises and recording on an instrument another unit still holds is dropped quietly, so a
  finalization section elsewhere cannot record into freed state.

Two lifetime details worth knowing:

1. Registering exporters is best done with interface-typed variables. Passing a *raw class
   instance* to a `const` interface parameter does not reference-count it, so an exporter that
   is never registered would never be released if you only ever passed the object around that
   way. The safe pattern is to keep the instance behind an interface:

```delphi
var
  LExporter: IMetricExporter;   // interface, not the class
begin
  LExporter := TMetricJsonExporter.Create(FTarget);
  TMetrics.AddExporter(LExporter);
  try
    ...
  finally
    TMetrics.RemoveExporter(LExporter);
  end;
end;
```

2. A provider holds its meters, a meter holds its instruments, and instruments hold no reference
   back - so releasing the provider releases everything, and an instrument interface kept by
   application code keeps working (and aggregating) on its own.

## 5. Harvesting

### 5.1 `Collect` - the typed snapshot

```delphi
var
  LPoints: TArray<TMetricPoint>;
begin
  LPoints := TMetrics.Collect;                // everything, every meter
  LPoints := TMetrics.Meter('mcp').Collect;   // one meter
  LPoints := TMetrics                         // one instrument
    .Counter('tool.calls')
    .Collect;                                 
end;
```

Provider snapshots are sorted by meter name, then instrument name, then label set, so two
harvests taken at different moments line up point by point - convenient for diffing and tests.

`TMetricPoint` fields:

| Field | Meaning |
|---|---|
| `Meter`, `Name`, `Kind`, `Description`, `UnitName` | Instrument identity. |
| `Labels` | The label set of this series (sorted, empty when unlabeled). |
| `Count` | Recordings folded into this point. |
| `Sum` | Counter: running total. Histogram: sum of samples. |
| `Min`, `Max` | Smallest/largest recorded value (meaningful for histograms). |
| `Last` | Most recent value; for a Gauge this is the current value. |
| `FirstSeen`, `LastSeen` | When this series was first and last recorded. |

Points must be treated as read only.

### 5.2 `Harvest` - push to exporters

```delphi
TMetrics.Harvest;             // snapshot + Export() to every exporter
TMetrics.Harvest(True);       // ...and clear afterwards (delta export)
```

`Clear` (on provider, meter or instrument) resets the recorded values while keeping the
instruments themselves, so code can keep recording into them. A delta pattern is therefore:

```delphi
TMetrics.Harvest(True);   // ships what happened since the previous Harvest(True)
```

## 6. Exporters

### 6.1 The exporter contract

```delphi
IMetricExporter = interface
  procedure Export(const APoints: TArray<TMetricPoint>);
end;
```

Register instances on a provider (`AddExporter`; duplicates are ignored) and drive them with
`Harvest`. Exports run synchronously on the caller's thread, in registration order. There is no
background machinery: if you want periodic harvesting, put `Harvest` on your own timer or
worker thread (see 6.5).

`Harvest` does not guard the exporters it calls. An exception from one propagates to the caller,
the exporters after it in the list do not run, and a `Harvest(True)` does not reach its `Clear` -
so nothing is lost, but nothing is reset either. An exporter that talks to something fallible (a
file, a socket) should swallow or log its own failures rather than let them out.

### 6.2 Sample exporters (`MCPConnect.Metrics.Exporters`)

Both samples append to a `TStrings` the caller owns (which must outlive the exporter).

`TMetricTextExporter` writes one human readable block per `Harvest`:

```text
[metrics] 08/09/2026 13:14:33 - 1 point(s)
  counter app/calls: sum=3 count=3 {tool=add_task}
```

`TMetricJsonExporter` writes one line per `Harvest` containing a complete JSON array - the
whole point set as a JSON-lines stream. The JSON is rendered by the Neon serializer
(`TNeonConfiguration.Camel`): member names are camelCased, `labels` is a JSON object whose keys
are sorted, and timestamps are ISO 8601 UTC:

```json
[{"meter":"app","name":"calls","kind":"counter","description":"","unit":"calls",
  "labels":{"tool":"add_task"},"count":3,"sum":3.0,"min":1.0,"max":1.0,"last":1.0,
  "firstSeen":"2026-11-20T10:12:33.000Z","lastSeen":"2026-11-20T10:13:01.500Z"}]
```

Numbers use an invariant decimal separator (whole values print with a trailing `.0`, exactly like
any Neon-serialized `Double` in the library).

Two details of that document are declared, not incidental, and a change to either shows up on the
wire: `"kind"` is lowercase because `TMetricKind` carries `[NeonEnumNames('counter,gauge,histogram')]`
(Neon reads the attribute off the *type*, never off a member), and `"labels"` is an object rather
than the array of `{key, value}` pairs `TMetricPoint.Labels` really is because `MetricsToJson`
registers `TMetricLabelsSerializer`. Keep the enum names in step with `MetricKindToStr`.

The same rendering is available without an exporter instance:

```delphi
uses
  MCPConnect.Metrics, MCPConnect.Metrics.Exporters;

S := MetricsToText(TMetrics.Collect);   // human readable block
S := MetricsToJson(TMetrics.Collect);   // JSON array text
```

### 6.3 A ready-made real sink (file)

One ready-made sink ships, `TMetricFileExporter` in `MCPConnect.Metrics.Exporters.Files`. It
appends every harvest to a UTF-8 text file that is opened (in append mode) when the
exporter is created and closed when it is released. Default is JSON-lines (one array per
harvest); `AsText = True` writes the readable blocks:

```delphi
uses
  MCPConnect.Metrics, MCPConnect.Metrics.Exporters.Files;

// A field, not a temporary: the file closes when the last reference goes, and
// RemoveExporter needs something to pass. This is what the Indy demo does.
FMetricsExporter := TMetricFileExporter.Create('metrics.jsonl');
TMetrics.AddExporter(FMetricsExporter);
...
TMetrics.Harvest(True);   // delta each tick -> one JSON line per tick
...
TMetrics.RemoveExporter(FMetricsExporter);
FMetricsExporter := nil;  // the file is closed here
```

Setup errors (an unwritable path, a missing folder) surface at creation, not per harvest.
Harvests reaching a given file exporter must be serialized - the usual shape is a single timer
or a shutdown flush, not several threads exporting at once.

### 6.4 A custom exporter

An exporter is just the interface - whatever sink you want beyond the shipped one (a remote
collector, a queue, a log). Implement `Export` and register an *interface-typed* instance:

```delphi
type
  TMetricCallbackExporter = class(TInterfacedObject, IMetricExporter)
  private
    FOnExport: TProc<TArray<TMetricPoint>>;
  public
    constructor Create(const AOnExport: TProc<TArray<TMetricPoint>>);
    procedure Export(const APoints: TArray<TMetricPoint>);
  end;

constructor TMetricCallbackExporter.Create(const AOnExport: TProc<TArray<TMetricPoint>>);
begin
  inherited Create;
  FOnExport := AOnExport;
end;

procedure TMetricCallbackExporter.Export(const APoints: TArray<TMetricPoint>);
begin
  if Assigned(FOnExport) then
    FOnExport(APoints);
end;
```

```delphi
var
  LExporter: IMetricExporter;
begin
  LExporter := TMetricCallbackExporter.Create(
    procedure (APoints: TArray<TMetricPoint>)
    begin
      try
        // MetricsToJson, from MCPConnect.Metrics.Exporters, renders the wire document
        FCollector.Post(MetricsToJson(APoints));
      except
        on E: Exception do
          Logger.LogError(E, '[metrics] collector push failed');
      end;
    end);
  TMetrics.AddExporter(LExporter);
end;
```

What an exporter must respect:

- **the points are read only** - the same array goes to every exporter in the list;
- **it runs on whichever thread called `Harvest`**, not a background one, so a slow export
  blocks that caller;
- **it should not let exceptions out** (see 6.1);
- **it may record metrics itself** - exporters run outside the provider's lock precisely so
  that this cannot deadlock.

### 6.5 Periodic harvesting

There is no built-in timer; a periodic harvester is a few lines wherever you already own a
thread or timer:

```delphi
// In a TTimer.OnTimer (or your worker loop):
procedure TTimer1Timer(Sender: TObject);
begin
  TMetrics.Harvest(True);   // delta each tick
end;
```

## 7. Inside an MCP server

The subsystem is transport- and protocol-agnostic, so there are two natural ways to expose the
harvest to an MCP client.

### 7.1 Instrument your tools, harvest with a tool (what the demo does)

The `Demo/MCPServer` Indy host shows the full cycle: it registers a
`TMetricFileExporter` over a `metrics.jsonl` file next to the executable and a 30 second
timer that runs `TMetrics.Harvest(True)` when there is anything to report - so every tool
call the LLM makes lands as one JSON-lines delta entry. On top of that,

`Demo/MCPServer/MCPServer.Tools.pas` records one-liners on every todo tool call
(`todo.tool.calls`, a duration histogram on `add_task`, a `todo.tasks.total` gauge) and adds a
`metrics_report` tool that renders the current snapshot with the sample text exporter - the LLM
can literally ask for the numbers "later":

```delphi
[McpTool('metrics_report', 'Harvests the metrics the demo collects on each todo tool call')]
function MetricsReport(): string;

function TTodoTool.MetricsReport(): string;
var
  LTarget: TStringList;
  LExporter: IMetricExporter;
begin
  LTarget := TStringList.Create();
  try
    LExporter := TMetricTextExporter.Create(LTarget);
    LExporter.Export(TMetrics.Collect);
    Result := LTarget.Text;
  finally
    LTarget.Free();
  end;
end;
```

### 7.2 Expose the harvest as an MCP resource

If you prefer data-over-tool (the client loads it, no side effect), expose the same snapshot as a
`[McpResource]`:

```delphi
[McpScope('metrics')]
TMetricsResource = class
  [McpResource('metrics', 'resource://metrics', 'application/json',
    'Current in-process metrics snapshot')]
  function ReadMetrics(): string;
end;

function TMetricsResource.ReadMetrics(): string;
begin
  // from MCPConnect.Metrics.Exporters
  Result := MetricsToJson(TMetrics.Collect);
end;
```

## 8. Limitations and design notes

- **Cumulative by default.** Instruments total up since creation; `Harvest(True)`/`Clear` is the
  only delta mechanism. There is no pull-based scrape protocol.
- **No histogram buckets or percentiles.** A histogram gives you count/sum/min/max; if you need
  explicit buckets, keep several counters or extend the exporter layer.
- **`Double` everywhere.** Totals beyond 2^53 lose integer precision - fine for request counts
  and timings, not for a byte counter past a few petabytes.
- **Cardinality is bounded per instrument**, not per label key: 128 distinct label sets by default
  (configurable, `0` = unbounded). Watch `DroppedSeries` when labels carry unbounded values
  such as user or tenant ids.
- **No OTLP/HTTP exporters ship.** `IMetricExporter` is the integration point; the JSON
  exporter gives you a wire-ready document to push anywhere.
- **Recording is cheap but not free** - a lock plus label normalization per call. For extremely
  hot loops, cache the instrument and keep label arrays short.

## 9. Quick API reference

| Member | Purpose |
|---|---|
| `TMetrics.Counter/Gauge/Histogram(Name[, Description[, Unit]])` | Create or fetch an instrument on the default meter. |
| `TMetrics.Meter(Name = '')` | Fetch (create) a named meter on the default provider. |
| `TMetrics.Default` | The process-wide `IMeterProvider`. |
| `TMetrics.CreateProvider` | A fresh, isolated `IMeterProvider`. |
| `TMetrics.Collect / Harvest(AReset) / Clear` | Harvest conveniences on the default provider. |
| `TMetrics.AddExporter / RemoveExporter` | Exporter conveniences on the default provider. |
| `IMeterProvider.MaxSeriesPerInstrument` | Label-set budget for meters created afterwards (default 128, `0` = unbounded). |
| `IInstrument.SeriesCount / DroppedSeries` | Series held by an instrument / series refused over the budget. |
| `IInstrument.Collect / Clear` | Snapshot or reset a single instrument. |
| `TMetricPoint.ToString` | One human readable line, e.g. `counter app/calls: sum=3 count=3 {tool=add_task}`. |
| `MetricsToText / MetricsToJson` | Render a point array without an exporter (from `MCPConnect.Metrics.Exporters`). |