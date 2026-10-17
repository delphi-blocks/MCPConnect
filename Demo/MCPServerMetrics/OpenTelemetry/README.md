# OpenTelemetry — a local collector and Prometheus for the metrics demos

An optional observability stack: an OpenTelemetry Collector and Prometheus,
started with Docker, so the metrics the demos record can be watched outside the
application. Nothing here is needed to run the demos — the OTLP exporter only
attaches when `OTEL_EXPORTER_OTLP_ENDPOINT` (or
`OTEL_EXPORTER_OTLP_METRICS_ENDPOINT`) is set.

```
MCPServerMetrics.exe --OTLP/HTTP--> otel-collector :4318 --/metrics :8889--> Prometheus :9090
```

## What is in this folder

| File | What it is |
|---|---|
| `docker-compose.yml` | Two containers: the collector (`otel/opentelemetry-collector-contrib`) on **4318** for OTLP/HTTP and **8889** for Prometheus to read from, and Prometheus on **9090**. `docker compose up` in this folder starts both. |
| `otel-collector.yaml` | The collector's configuration. It receives OTLP/HTTP on 4318 and sends every batch to three places: the container's console log (`debug`), a `metrics.jsonl` file (one OTLP JSON document per line, rotated at 10 MB, three backups) and Prometheus on 8889. Instrument names keep their dots turned into underscores, with no unit and no `_total` suffix (`request.count` → `request_count`). A series silent for 10 minutes disappears from `/metrics`. |
| `prometheus.yml` | Prometheus reads from the collector every 15 s and keeps the labels the collector sets (`service.name` becomes the `job` label) instead of overwriting them. |
| `OtlpSmoke.dpr` | An end-to-end test of `TOtlpMetricExporter` against a running collector. It records counters, a histogram and a gauge on three meters, harvests with a reset, without one and gzipped, then points the exporter at a wrong path and at a closed port to show that a failed export is counted and reported, never raised. Its exit code is the number of failed checks. |
| `OtlpSmoke.Build.bat` | Builds `OtlpSmoke.exe` with Delphi 13 into `Bin\`. |
| `Bin\` | The built `OtlpSmoke.exe` and its compiled units. |

## Using it

Docker is the only requirement.

```powershell
cd Demo\MCPServerMetrics\OpenTelemetry
docker compose up

# then, in another shell: the collector, not Prometheus
$env:OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4318"
# and start MCPServerMetrics.exe from that same shell
```

Then open <http://localhost:9090> and paste a query from
[Querying the metrics with PromQL](#querying-the-metrics-with-promql) below.
Every harvest the demo runs also shows up in the collector's console.

To check the exporter on its own, without the demo:

```powershell
.\OtlpSmoke.Build.bat
.\Bin\OtlpSmoke.exe                  # default endpoint http://127.0.0.1:4318/v1/metrics
```

Optional settings, read by the exporter from the environment:

- `OTEL_SERVICE_NAME` renames the resource; it becomes the Prometheus `job`
  label (default `mcpconnect-metrics-demo`).
- `OTEL_EXPORTER_OTLP_COMPRESSION=gzip` sends compressed bodies.

The WebStencils dashboard (`..\WebStencils`) uses the same metrics hub, so it
exports the same way.

## Pitfalls

- **Point the exporter at the collector (4318), never at Prometheus (9090).**
  Prometheus does not speak OTLP. `OTEL_EXPORTER_OTLP_ENDPOINT` is a *base* URL —
  the exporter appends `/v1/metrics` — while
  `OTEL_EXPORTER_OTLP_METRICS_ENDPOINT` is used exactly as written. A wrong
  address shows up as `OTLP export to … failed: … (12029) A connection with the
  server could not be established`.
- **A demo started from the Delphi IDE does not see the shell's variables.** It
  gets the IDE's environment, plus the user overrides under *Tools → Options →
  IDE → Environment Variables*; set the variable there, and restart the IDE
  after changing it. An override left behind there keeps sending to the old
  address even after the Windows variable is removed.

## Querying the metrics with PromQL

What `Server.Metrics.pas` records, as Prometheus sees it once the stack above is
running: open <http://localhost:9090> and paste a query.

### How the names map

| Delphi side | Prometheus side |
|---|---|
| instrument name `request.duration_ms` | `request_duration_ms` (dots become underscores; no unit and no `_total` suffix, set by `translation_strategy: UnderscoreEscapingWithoutSuffixes` in `otel-collector.yaml`) |
| meter (`mcp.server`, `demo.system`, `demo.workload`, `demo`) | label `otel_scope_name` |
| `service.name` resource attribute (`mcpconnect-metrics-demo` unless `OTEL_SERVICE_NAME` is set) | label `job` |
| labels (`['method', 'tools/call']`) | labels (`method="tools/call"`) |
| Counter | counter: use `rate()` / `increase()` |
| Gauge | gauge: use it as-is, or `*_over_time()` |
| Histogram | `_bucket`, `_sum`, `_count` |

Keep these three points in mind:

- **Histograms have no real buckets.** The OTLP exporter sends a single
  `(-inf, +inf)` bucket, so `histogram_quantile()` returns nothing useful. The
  collector's Prometheus exporter also drops min and max. What you can chart is
  the **average**, `rate(_sum) / rate(_count)`, plus throughput. Percentiles need
  explicit bounds in `MCPConnect.Metrics`.
- **Values only change once per harvest**, every 30 s (`FHarvestTimer`). Use
  range windows of at least `[2m]`, or one `rate()` sees a single step and the
  chart gets spiky. Gauges likewise show the value from the last harvest. For
  example, `request_in_flight` is the count at harvest time, not a live count.
- **Counters are cumulative** even though the demo harvests with reset: the OTLP
  exporter keeps running totals. Restarting the demo is an ordinary counter
  reset, and `rate()` and `increase()` already handle that.

If `OTEL_SERVICE_NAME` is set, or several services feed the collector, add
`job="..."` to the selectors.

To see what is there, run:

```promql
count by (__name__, otel_scope_name) ({job="mcpconnect-metrics-demo"})
```

### 1. MCP protocol (`otel_scope_name="mcp.server"`)

Recorded by `TMetricsMCPMiddleware` for every request and every `tools/call`.

#### Requests

```promql
# Requests per second, by JSON-RPC method        (panel: time series, stacked)
sum by (method) (rate(request_count[2m]))

# Total request rate
sum(rate(request_count[2m]))

# Requests in the last hour, by method           (panel: bar gauge / table)
sum by (method) (increase(request_count[1h]))

# Errors per second, by method and error (JSON-RPC code or exception class)
sum by (method, error) (rate(request_errors[2m]))

# Error ratio per method, 0..1                   (panel: time series, unit percentunit)
  sum by (method) (rate(request_errors[5m]))
/
  sum by (method) (rate(request_count[5m]))

# Overall error ratio                            (panel: stat)
sum(rate(request_errors[5m])) / sum(rate(request_count[5m]))

# Error mix: share of each error code in the last hour  (panel: pie)
sum by (error) (increase(request_errors[1h]))

# Average latency per method, ms                 (panel: time series, unit ms)
  sum by (method) (rate(request_duration_ms_sum[5m]))
/
  sum by (method) (rate(request_duration_ms_count[5m]))

# Average latency over all requests, ms          (panel: stat)
sum(rate(request_duration_ms_sum[5m])) / sum(rate(request_duration_ms_count[5m]))

# Requests being served at the last harvest      (panel: stat / time series)
request_in_flight{scope="server"}

# ... and the peak seen in the last 15 minutes
max_over_time(request_in_flight{scope="server"}[15m])
```

#### Tools

```promql
# Calls per second, by tool
sum by (tool) (rate(tool_count[2m]))

# Top 5 most-called tools over the last hour     (panel: bar gauge)
topk(5, sum by (tool) (increase(tool_count[1h])))

# Average latency per tool, ms
  sum by (tool) (rate(tool_duration_ms_sum[5m]))
/
  sum by (tool) (rate(tool_duration_ms_count[5m]))

# Top 5 slowest tools by average latency
topk(5,
    sum by (tool) (rate(tool_duration_ms_sum[5m]))
  /
    sum by (tool) (rate(tool_duration_ms_count[5m]))
)

# Where the time goes: each tool's share of the total tool time, 0..1
  sum by (tool) (rate(tool_duration_ms_sum[5m]))
/ ignoring (tool) group_left
  sum(rate(tool_duration_ms_sum[5m]))

# Error ratio per tool, 0..1
  sum by (tool) (rate(tool_errors[5m]))
/
  sum by (tool) (rate(tool_count[5m]))

# Average size of the JSON arguments per tool, bytes   (panel: unit bytes)
  sum by (tool) (rate(tool_params_bytes_sum[5m]))
/
  sum by (tool) (rate(tool_params_bytes_count[5m]))

# How much of the tools/call time is spent inside the tool itself
# (the rest is dispatch, (de)serialization and middleware), 0..1
  sum(rate(tool_duration_ms_sum[5m]))
/
  sum(rate(request_duration_ms_sum{method="tools/call"}[5m]))
```

The two ratio queries only return series for tools that have had at least one
error. To show `0` for the others, append
`or sum by (tool) (rate(tool_count[5m])) * 0` (and the same with `method` for
requests).

### 2. Host (`otel_scope_name="demo.system"`)

Sampled by the dashboard timer (`SampleSystem`).

```promql
# Process CPU, %                                 (panel: time series, unit percent)
process_cpu_percent{scope="process"}

# Smoothed CPU over 5 minutes
avg_over_time(process_cpu_percent{scope="process"}[5m])

# System physical memory in use, %               (panel: gauge, 0..100)
system_memory_load_percent{scope="system"}

# Physical memory available, MiB
system_memory_available_mib{scope="system"}

# Handles held by the process
process_handles{scope="process"}

# Handle growth per hour: a steady positive value hints at a leak
deriv(process_handles{scope="process"}[15m]) * 3600

# Uptime, seconds                                (panel: stat, unit s)
process_uptime_s{scope="process"}

# Restarts in the last day (uptime going backwards)
resets(process_uptime_s{scope="process"}[1d])

# CPU cost per request: CPU% divided by requests/s
  avg_over_time(process_cpu_percent{scope="process"}[5m])
/ ignoring (scope)
  sum(rate(request_count[5m]))
```

### 3. Simulated workload (`otel_scope_name="demo.workload"`)

Generated by the "simulate workload" and "simulate errors" use cases.

```promql
# Orders per second by region and outcome        (panel: time series, stacked)
sum by (region, status) (rate(orders_created[2m]))

# Rejection ratio per region, 0..1 (the generator rejects 1 order in 11, ~0.09)
  sum by (region) (rate(orders_created{status="rejected"}[5m]))
/
  sum by (region) (rate(orders_created[5m]))

# Orders in the last hour per region             (panel: bar gauge)
sum by (region) (increase(orders_created[1h]))

# Average order value per region, USD
  sum by (region) (rate(order_value_usd_sum[5m]))
/
  sum by (region) (rate(order_value_usd_count[5m]))

# Revenue per minute, by region, USD
sum by (region) (rate(order_value_usd_sum[5m])) * 60

# Revenue in the last hour, all regions, USD     (panel: stat, unit currencyUSD)
sum(increase(order_value_usd_sum[1h]))

# Queue depth now, and its 15-minute peak
queue_depth{queue="orders"}
max_over_time(queue_depth{queue="orders"}[15m])

# Waiting orders per worker
sum(queue_depth{queue="orders"}) / sum(worker_pool_size{pool="default"})

# Cache hit ratio, 0..1 (the generator makes it 0.75)   (panel: gauge)
  sum(rate(cache_events{result="hit"}[5m]))
/
  sum(rate(cache_events[5m]))

# Cache lookups per second by outcome
sum by (result) (rate(cache_events{cache="orders"}[2m]))

# Average rows per batch
rate(batch_size_sum[5m]) / rate(batch_size_count[5m])

# Simulated errors per second by kind            (panel: time series, stacked)
# `errors` is a generic name, so pin the scope
sum by (kind) (rate(errors{otel_scope_name="demo.workload"}[2m]))
```

### 4. The subsystem observing itself (`otel_scope_name="demo"`)

Recorded by `HarvestDelta` just before each delta harvest.

```promql
# Harvests per minute (should be ~2 with the 30 s timer)
rate(metrics_harvests[5m]) * 60

# Average number of series per harvest: grows with every new label value,
# so it is the cardinality to watch
rate(metrics_harvest_series_sum[5m]) / rate(metrics_harvest_series_count[5m])

# Series exported per minute
rate(metrics_exported_series[5m]) * 60

# How many series Prometheus holds for the demo
count({job="mcpconnect-metrics-demo"})
```

The "cardinality" and "separate provider" use cases record into their own
`IMeterProvider`. That provider has no OTLP exporter, so those series never
reach Prometheus.

### 5. Alert expressions

These are ready to use as the `expr` of a Prometheus rule or a Grafana alert.

```promql
# More than 5% of MCP requests failing for 5 minutes
  sum(rate(request_errors[5m])) / sum(rate(request_count[5m])) > 0.05

# A tool averaging more than 500 ms
  sum by (tool) (rate(tool_duration_ms_sum[5m]))
/ sum by (tool) (rate(tool_duration_ms_count[5m])) > 500

# The demo stopped harvesting (no harvest in 2 minutes, or the series is gone)
increase(metrics_harvests[2m]) == 0 or absent(metrics_harvests)

# The machine is short of memory
system_memory_load_percent{scope="system"} > 90

# The queue keeps growing
deriv(queue_depth{queue="orders"}[10m]) > 0
  and queue_depth{queue="orders"} > 30
```

## Known issue

`otel-collector.yaml` writes the file to `./metrics.jsonl`, but its comment and
`docker-compose.yml` expect it in `/data` — the folder shared with the host as
`OpenTelemetry\data`. As written, the file probably stays inside the container. The fix
is likely `path: /data/metrics.jsonl`; it has not been verified by running the
containers.
