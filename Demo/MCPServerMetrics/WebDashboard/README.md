# MCPServerMetrics WebDashboard

A small Vite + Vue dashboard for the metrics `Demo/MCPServerMetrics` serves on
**`GET /metrics`** — plain HTTP and plain JSON, no MCP protocol involved. It
polls the endpoint and draws what it finds.

It looks like the WebStencils dashboard (`../ServerWebStencils`) on purpose —
same header, same brand block, same panels in the same order, same stylesheet
rules (`src/style.css` follows `templates/dashboard.css`) and the same assets in
`public/assets` — so the page rendered on the server and the page drawn in the
browser can be compared side by side. What differs is where the work happens:
here the history behind every line is kept by the browser tab.

| Panel | What it shows |
|---|---|
| Header | logo, service, version and times; badges for the feed's status, the samples kept, instruments and polls, and the polling controls (interval, refresh, pause, clear history) |
| Brand block | the illustration, the logo and what the page is |
| KPI cards | uptime, requests, errors, tool calls, in-flight, latency avg/max, series count |
| Throughput | requests, tool calls and errors per second, derived from consecutive samples |
| Latency | one histogram over time: average (solid) and observed max (dashed) |
| Counters | one card per counter: total, rate line, and the total split by its first label key |
| Histograms | one card per histogram: average, the min..max band with the average marked, and the average of each interval |
| Gauges | one dial per gauge, with the range the window covered and a sparkline |
| Instruments | every instrument the server holds, filterable and sortable |
| Breakdown | every instrument rolled up by each of its label keys, one card each |
| Slowest histograms | the top series ranked by average |
| What the server says about itself | the service description, the fold note, and the raw document as it arrived |

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
