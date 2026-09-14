# MCPServerMetrics — the MCPConnect metrics showcase

A self-contained MCP server that is also a guided tour of `MCPConnect.Metrics`.
Everything — the server definition, the tools, the middleware and the screen —
lives in this folder, so the demo builds on its own:

| Unit | What it holds |
|---|---|
| `Server.Config.pas` | the server definition: name, security, the todo resource/prompt/tools, the demo middleware |
| `Server.Tools.pas` | the todo store and its `[McpTool]` methods (the application metrics) |
| `Server.Middleware.pas` | the transport/message middleware of the shared demo |
| `Server.Metrics.pas` | the hub (`TServerMetrics`): instruments, the seven exporters, reports, the host sampler, the two API demos |
| `Server.Metrics.Middleware.pas` | protocol telemetry recorded from the chain (`TMetricsMCPMiddleware`) |
| `Server.Metrics.Features.pas` | the MCP tools, resource and prompt over the numbers |
| `Server.Dashboard.pas` + `.dfm` | the tabbed dashboard, a **design-time frame** |
| `Server.Form.Main.pas` + `.dfm` | the host form: the transport controls, and the frame docked under them |

The reference for the subsystem is [`Docs/metrics.md`](../../Docs/metrics.md);
this file is the tour.

## Running it

Build `MCPServerMetrics.dproj` (Delphi 13 by default), copy the shared
`Demo\data` folder next to the produced `.exe` if you want the resources/apps,
and run it. The server listens on `http://localhost:8080/` — point any MCP client
at it. The **Metrics** tab is live from the first second: the host gauges alone
already put rows in the grid.

> The JSON-lines and text files are opened for append when the hub starts and
> stay open, so a tool that insists on opening them for read+write may be
> refused while the demo runs. The `resource://metrics/live` resource is the
> live, lock-free way to get the same document.

## Editing the screen

`Server.Dashboard.pas` is an ordinary VCL frame: all of its controls are in
`Server.Dashboard.dfm`, so open the unit and press **F12** (or right-click it in
the Project Manager and pick **View as Form**) to see and rearrange the tabs, the
grid, the buttons and the timers in the designer. The host form only creates the
frame, docks it (`Align := alBottom`) and calls `RefreshAll` once the hub has
started; nothing about the layout is built in code.

## The screen

| Tab | Contents |
|---|---|
| **About** | what the demo is and where the code is |
| **Metrics** | the live grid — one row per series — plus the action buttons |
| **Use cases** | the narrative, use case by use case |
| **Slowest** | the slowest histogram series by average, refreshed per harvest |
| **Text** | the latest harvest rendered by `MetricsToText` |
| **JSON** | the latest harvest rendered by `MetricsToJson` |
| **Exporters** | the exporter list and the series-per-harvest history |
| **Log** | the application log (the frame owns the memo the buffer adapter writes into) |

The buttons on **Metrics** harvest a delta or the cumulative total, sample the
host gauges, clear everything, record a simulated workload, run the cardinality
and separate-provider demos, and toggle the one-second grid refresh.

## The instruments

Four meters keep unrelated measurements apart.

### `mcp.server` — protocol telemetry, recorded by the middleware

Recorded once per request by `TMetricsMCPMiddleware`, never by a business class:

| Instrument | Kind | Labels | Meaning |
|---|---|---|---|
| `request.count` | Counter | `method` | MCP requests handled |
| `request.duration_ms` | Histogram | `method` | request latency |
| `request.errors` | Counter | `method`, `error` | requests that raised (JSON-RPC code) |
| `request.in_flight` | Gauge | `scope` | requests being served right now |
| `tool.count` | Counter | `tool` | `tools/call` invocations |
| `tool.duration_ms` | Histogram | `tool` | tool latency |
| `tool.errors` | Counter | `tool` | tool calls that raised |
| `tool.params_bytes` | Histogram | `tool` | size of the JSON arguments |

### `demo.workload` — the application metrics a tool records

`metrics_simulate_workload` records: `orders.created` (Counter, `region` +
`status`), `order.value_usd` (Histogram, `region`), `cache.events` (Counter,
`cache` + `result`), `batch.size` (Histogram), `queue.depth` and
`worker.pool_size` (Gauges). `metrics_simulate_errors` fills `errors` by `kind`.

### `demo.system` — host gauges sampled on a timer

`process.uptime_s`, `process.cpu_percent`, `process.handles`,
`system.memory_load_percent`, `system.memory_available_mib`.

### `demo` — the subsystem observing itself

`metrics.harvests`, `metrics.harvest_series` (Histogram) and
`metrics.exported_series`. They are recorded just before a delta harvest so the
harvest carries them, and the reset then zeroes them like everything else.

## The exporters

Seven are attached to the default provider, and `Harvest` calls them in this
order:

1. `metrics.jsonl` — `TMetricFileExporter`, one JSON array per harvest;
2. `metrics.log` — `TMetricFileExporter`, readable blocks;
3. `snapshot` — a custom exporter keeping the last points, the harvest count and
   the series-per-harvest history (the Exporters tab);
4. `slowest` — a custom exporter rendering the slowest histograms by average;
5. `text view` / 6. `json view` — custom exporters that *replace* a memo with the
   latest harvest instead of appending, so a screen target stays bounded;
7. `logify` — a custom exporter writing one summary line per harvest through the
   application log.

`metrics_report` (from `Server.Tools.pas`) and the `MetricsToText` /
`MetricsToJson` free functions are used by the tools.

## The MCP surface

Tools (the `metrics` scope prefixes them):

| Tool | Purpose |
|---|---|
| `metrics_snapshot` | current metrics as a readable table |
| `metrics_snapshot_json` | the wire document plus a series count |
| `metrics_instrument` | every series of one instrument, with kind/unit/description |
| `metrics_slowest` | slowest histograms by average |
| `metrics_exporters` | the exporters and the harvest history |
| `metrics_use_cases` | what the demo demonstrates |
| `metrics_simulate_workload` | record counters/histograms/gauges with labels |
| `metrics_simulate_errors` | record error counters by kind |
| `metrics_reset` | `TMetrics.Clear` (instruments and exporters are kept) |
| `metrics_cardinality` | the series budget and `DroppedSeries` on a private provider |
| `metrics_separate_provider` | two providers never mix, even with the same names |

Resource: `resource://metrics/live` (`application/json`). Prompt:
`analyse-metrics`, which embeds the snapshot and asks the model to point out
anomalies.

## The use cases

1. **Protocol telemetry from middleware** — counters, a latency histogram, an
   error counter and an in-flight gauge for every request, labelled by method,
   plus a second set for tool calls labelled by tool name. Zero instrumentation
   in the business classes.
2. **Application metrics from the tools** — the todo tools and the workload
   simulator record domain metrics: orders by region and outcome, cache hits and
   misses, queue depth, batch and argument sizes.
3. **Host gauges on a timer** — uptime, CPU, handles, memory.
4. **The subsystem observing itself** — harvests, series per harvest.
5. **Seven exporters at once** — files, screen, memory, log, four of them custom.
6. **Exposing the numbers back over MCP** — tools, a resource and a prompt.
7. **Cardinality and isolation** — the label budget and independent providers.

## Files produced

Next to the `.exe`:

- `metrics.jsonl` — one JSON array per delta harvest (JSON-lines);
- `metrics.log` — the human readable blocks;
- `perf.log` — the `[PERF]` timing snapshot.
