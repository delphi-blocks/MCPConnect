# MCPServerMetrics WebDashboard

A small Vite + Vue dashboard for the metrics `Demo/MCPServerMetrics` serves on
**`GET /metrics`** — plain HTTP and plain JSON, no MCP protocol involved. It
polls the endpoint and draws what it finds: throughput, latency, the instrument
table, label breakdowns, the host gauges and the raw document.

| Panel | What it shows |
|---|---|
| KPI cards | uptime, requests, errors, tool calls, in-flight, latency avg/max, series count |
| Throughput | requests, tool calls and errors per second, derived from consecutive samples |
| Latency | one histogram over time: average (solid) and observed max (dashed) |
| Host gauges | uptime, CPU, handles, memory — current value plus a sparkline of the window this tab has seen |
| Instruments | every instrument the server holds, filterable and sortable |
| Breakdown | one instrument split by one of its labels ("requests by method", "orders by region") |
| Slowest histograms | the top series ranked by average |
| Raw document | the endpoint's JSON exactly as it arrived |

## Running it

1. Start the server: `Demo\MCPServerMetrics.exe` (it listens on
   `http://localhost:8080/`).
2. Then, from this folder:

   ```bash
   npm install
   npm run dev
   ```

3. Open <http://localhost:5173>. The dashboard asks for `/metrics`, and the Vite
   dev server proxies that to `http://localhost:8080` (see `vite.config.ts`), so
   the browser stays on one origin and **nothing has to be configured for
   CORS**.

Point it at another server with `DASHBOARD_METRICS_URL`:

```bash
DASHBOARD_METRICS_URL=http://localhost:9090 npm run dev
```

## What it can and cannot show

The endpoint is a **snapshot**, and the subsystem behind it is cumulative with no
time series of its own — so:

- **history lives in this tab.** Charts are drawn from the samples the browser
  has seen since the page was opened (300 of them, about ten minutes at the
  default 2 s interval). Reloading starts over. If you need history that
  survives a reload, that is a store on the server side (or Prometheus through
  the OTLP exporter the demo already ships — see `../OpenTelemetry/`).
- **rates are differences.** Counters only grow, so a rate is the change between
  two samples over the time between them. A value that goes backwards (a server
  restart, or a reset the endpoint's fold could not see) is counted and skipped
  rather than drawn as a negative.
- **no percentiles.** A histogram keeps count, sum, min and max — not buckets —
  so average and max are what can be drawn; there is no p95 to compute.
- **gauges are instantaneous**, so they are shown as values, not rates.

The document's own `fold` section reports how the endpoint keeps counters
monotonic across the server's `Harvest(True)` resets; see
`../Server.Metrics.Http.pas` for the mechanism.

## Building it

```bash
npm run build      # vue-tsc type check, then a production bundle in dist/
npm run preview    # serve the built bundle
```

The build is a static site: any static host serves `dist/`. Two ways to point it
at a server:

- **same origin** — serve `dist/` from the server itself and leave
  `VITE_METRICS_URL` unset: a relative `/metrics` resolves on its own;
- **another origin** — set it at build time, and make sure the server allows the
  page's origin (`SetCORS(True)` in the server configuration, plus an allowed
  origin for anything that is not loopback):

  ```bash
  VITE_METRICS_URL=http://localhost:8080/metrics npm run build
  ```

## Checking the contract without a browser

`tools/probe.ts` drives the running server over MCP, reads `/metrics` twice and
pushes both documents through this app's own derivation code, printing what a
chart would draw. It is the quickest way to tell whether a change broke the
endpoint or the dashboard:

```bash
npx esbuild tools/probe.ts --bundle --format=esm --platform=node --outfile=.probe.mjs
node .probe.mjs
```

## Layout

```text
src/api/metrics.ts             the one fetch, and its shape check
src/types.ts                   the document, member for member
src/composables/useMetricsFeed.ts  polling, the retained history, the rates
src/lib/series.ts              samples -> chart series (pure, so the probe can use it)
src/lib/format.ts              number and time formatting
src/components/EChart.vue      ECharts wrapper (line + bar only, tree-shaken)
src/components/*.vue           the panels
tools/probe.ts                 the headless contract check
```
