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
| `Server.Metrics.Document.pas` | the numbers as one document: the fold, the rollups, the JSON shape — shared by both dashboards |
| `Server.Metrics.Middleware.pas` | protocol telemetry recorded from the chain (`TMetricsMCPMiddleware`) |
| `Server.Metrics.Features.pas` | the MCP tools, resource and prompt over the numbers |
| `Server.Metrics.Http.pas` | `GET /metrics`: the same numbers as plain JSON over HTTP, for a browser |
| `Server.Dashboard.pas` + `.dfm` | the tabbed dashboard, a **design-time frame** |
| `Server.Form.Main.pas` + `.dfm` | the host form: the transport controls, and the frame docked under them |
| `WebDashboard/` | the Vite + Vue web dashboard that polls `GET /metrics` |
| `WebStencils/` | the same dashboard rendered server-side with Embarcadero's WebStencils |

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

An **eighth, conditional** exporter is the OpenTelemetry one
(`MCPConnect.Metrics.Exporters.Otlp`): it attaches only when
`OTEL_EXPORTER_OTLP_ENDPOINT` or `OTEL_EXPORTER_OTLP_METRICS_ENDPOINT` is set,
and the **Export OTLP** button on the Metrics tab can also create one on demand.
Two things about it matter when it has nowhere to send:

- **It delivers on its own thread** (`Async`), so a collector that is down — or
  one whose address blackholes the connection for the whole timeout — cannot
  hold up the harvest, which here runs from a VCL timer, i.e. on the UI thread.
  Only the newest body waits while one is in flight, and they are cumulative
  snapshots, so nothing is lost.
- **It backs off after a failure**: 5 s, then 10, 20 … up to 5 minutes, cleared
  by the first success. A connection that cannot be established arrives with no
  `Retry-After`, so without this it would retry on every harvest and log the
  same warning each time. Skips are counted in `SkippedCount`.

Point it at the **collector**, not at Prometheus: `otel/docker-compose.yml`
listens for OTLP/HTTP on **4318** and Prometheus on 9090, and Prometheus does not
speak OTLP. Note also that the exporter appends `/v1/metrics` to
`OTEL_EXPORTER_OTLP_ENDPOINT`, so that variable is a base URL — pointing it at
`http://localhost:9090/api/v1/otlp` produces a POST to
`http://localhost:9090/api/v1/otlp/v1/metrics` and WinHTTP error 12029
("a connection with the server could not be established"), which is a
configuration mistake rather than a fault in the demo.

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

## The plain-HTTP endpoint

Not every consumer of a metric should have to speak MCP — a browser dashboard,
a script, a probe — so the demo also answers **`GET /metrics`** on the same port,
with an ordinary JSON document and no protocol on top of it:

```text
http://localhost:8080/metrics
```

`Server.Metrics.Http.pas` implements it as a *transport middleware*
(`TMetricsHttpMiddleware`), which is the only layer that can answer a request the
MCP protocol would refuse: the transport dispatches POST and OPTIONS and answers
405 to every other verb at the end of its chain, so a middleware that recognizes
its own request and does not call `Next` answers it first. Its priority puts it
inside the CORS middleware — a browser gets the `Access-Control-*` headers — and
short of the static-token gate, so the endpoint is public. Registering the same
class at `MW_PRIORITY_AUTHENTICATION` instead puts it behind the token.

The document is richer than the MCP resource, because nothing constrains it:
`service` (name, version, uptime), `poll`, `fold`, `summary`, an `instruments`
rollup, `groups` (each instrument by each of its label keys — "requests by
method" as a table), `slowest`, and then the raw data as **both** `live` (the
provider as it stands) and `cumulative` (monotonic since the endpoint started).

That last pair is the point of the `TMetricsDocumentState` fold. The demo harvests
with a reset every 30 s, so the provider's counters fall back to zero on each
tick; a dashboard reading that raw would draw a sawtooth. The fold closes each
window exactly when the host resets the provider — an exporter the endpoint
registers for itself — and reports the running total, so the counters a client
sees only ever grow. `fold.resetsDetected` says how many resets went into it.

## The web dashboard

`WebDashboard/` is a Vite + Vue app that polls that endpoint and draws it —
throughput, latency, the instrument table, label breakdowns, host gauges and the
raw document. In development it proxies `/metrics` to `http://localhost:8080`, so
there is nothing to configure for CORS:

```bash
cd WebDashboard
npm install
npm run dev        # http://localhost:5173
```

See [`WebDashboard/README.md`](WebDashboard/README.md) for the panels, the build,
and `tools/probe.ts` — a headless check of the whole loop that needs no browser.

## The WebStencils dashboard

[`WebStencils/`](WebStencils/README.md) is a second dashboard over the same
numbers, this one rendered **on the server** by Embarcadero's WebStencils
template engine. It is a WebBroker application that hosts the MCP endpoint, the
HTML pages and the JSON endpoint in one process:

```text
POST /mcp              the MCP endpoint
GET  /dashboard        the page, rendered from this process's metrics
GET  /dashboard/live   the fragment the page refreshes itself with
GET  /dashboard.css    the stylesheet
GET  /assets/*         the MCPConnect logo (templates\assets)
GET  /metrics          the same numbers as JSON
```

```bash
MCPServerMetricsWeb.exe        # from Demo\MCPServerMetrics\WebStencils
# http://localhost:8090/dashboard
```

Charts are inline SVG computed in Delphi and bars are CSS widths, so the page has
no dependencies at all; a small script swaps the live fragment, and the page is
complete without it. The two dashboards share
[`Server.Metrics.Document.pas`](Server.Metrics.Document.pas), so the JSON document
and the HTML cannot disagree about a number.

Requires Delphi 12+ (`Web.Stencils` is not in the Delphi 11 RTL); the library's
own Delphi 11 floor is unaffected.

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
