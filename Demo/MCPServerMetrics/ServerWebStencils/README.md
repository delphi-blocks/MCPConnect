# MCPServerMetricsWeb — the WebStencils dashboard

The same metrics the Indy showcase serves as JSON on `GET /metrics`, rendered as
**HTML on the server** by Embarcadero's [WebStencils](https://docwiki.embarcadero.com/RADStudio/en/WebStencils)
template engine. One Delphi process hosts the MCP endpoint *and* the dashboard,
so there is no HTTP hop between the page and the numbers, no CORS to configure,
no build step and no JavaScript framework.

The two serve different audiences, the way a real deployment would split them:

* **`/mcp` is for the model.** It exposes the application's tools and nothing
  else — here the todo list from [`../Server.Tools.pas`](../Server.Tools.pas):
  `add_task`, `list_tasks`, `complete_task`, `delete_task` and the MRTR tools
  `summarize_tasks`, `import_tasks`, `draft_day_plan`. There are no metrics tools
  on it: a model has no business reading the server's telemetry.
* **`/dashboard` and `/metrics` are for whoever runs the server.** They show the
  tool calls as they happen. The tools themselves know nothing about it —
  `TMetricsMCPMiddleware` records every call on the way through.

The `metrics_*` tools, resource and prompt, which expose the telemetry *to* a
model, belong to the developer-facing showcase one folder up and are not
registered here.

## Running it

Build `MCPServerMetricsWeb.dproj` (Delphi 12 or newer — see *Requirements*),
then run the executable **from this folder**, so that `templates\` sits next to
it:

```text
http://localhost:8080/dashboard
```

The form's **Open Dashboard** button starts the server if it is not running and
opens the browser. `-templates <path>` points the app at another template folder
(otherwise `<exe folder>\templates`).

The dashboard is empty until something calls the tools. Point any MCP client at
`http://localhost:8080/mcp` and work with the todo list — MCPJam
(`npx @mcpjam/inspector@latest`), or Claude Desktop through `mcp-remote`
(`npx -y mcp-remote http://localhost:8080/mcp`) — and watch the calls arrive.

## The routes

| Route | What answers |
|---|---|
| `POST /mcp` | the MCP endpoint — `TMCPDispatcher`, exactly as the WebBroker demo wires it, serving the todo tools |
| `GET /` and `/dashboard` | the page: layout + every panel |
| `GET /dashboard/live` | the same panels without the layout — the fragment the page's script swaps into `#live` |
| `GET /dashboard.css` | the stylesheet, served as a file (never through the template parser) |
| `GET /assets/*` | images and other binaries from `templates\assets` — the MCPConnect logo the page shows |
| `GET /metrics` | the numbers as JSON — the same document the Indy demo serves |
| anything else | a small 404 page that says where to look |

`/assets` is one wildcard action rather than a route per file, and it serves a
**name and nothing else**: a request whose remainder is not a plain file name in
that one directory (`..`, a path separator, raw or percent-encoded) is a 404
before anything is opened. Binaries go out through `Response.ContentStream`,
since `Response.Content` is a `UnicodeString` and would re-encode every byte above
127 — a `PNG` read into it arrives corrupted. `templates\assets` holds
the SVG logo shown at the left of the top bar and the brand block's two images:
`hero-mcp-metrics.svg`, a landscape illustration (an MCP client calling the
server, the counters, histograms and gauges it records, and the OTLP path to an
OpenTelemetry Collector and Prometheus, its dashed flows animated in CSS and
still under `prefers-reduced-motion`), and `mcpconnect-rising-emb.png`, the logo
between it and the text. `logo-mark.png` and `logo-black.png` are the earlier PNG
logos, no longer referenced by the page. The page's brand block is
`templates\_hero.html`, imported by the layout so it renders once per page load
and stays out of the refreshed fragment.

The page is complete and static without JavaScript; a fifteen-line script in the
layout asks for `/dashboard/live` every few seconds and replaces one `div`. With
scripting off it is a snapshot and the **Refresh now** link reloads it.

## What it shows

A brand block at the top — the MCPConnect logo, what the library is, and a
one-paragraph explanation of what this deployment is (one process answering
`/mcp`, `/metrics` and this page) with links to the source and the documentation —
then the server's own description and version in the header line.

Below that, the metrics: KPI cards (uptime, requests, errors, tool calls,
in-flight, latency average and max, series), two SVG charts (**throughput** in
requests/s and tool calls/s, and **latency** average and max), then one card per
instrument, by kind:

* **counters** — the total, a rate line (the change between two samples over the
  time between them) and a bar splitting the total by the counter's first label
  key;
* **histograms** — the average, a min..max band on a 0..max scale with the
  average marked on it (the subsystem keeps no buckets, so this is the whole of
  what is known about the distribution), and the average of each sample interval
  on its own;
* **gauges** — a dial for the current value, with a thin outer arc for the range
  the window covered (a percentage runs to 100 and turns amber at 75, red at 90;
  anything else is scaled to a round number above the largest value seen), and a
  sparkline.

The rates and interval averages come from `MetricsWeb.History`, which keeps a
*track* per instrument field — a gauge's value, a counter's total, a histogram's
sum and count — beside every sample. After that: the full instruments table, a breakdown card per instrument and label
key ("requests by method", "calls by tool"), the slowest histograms, and —
because the hub is in this process — the exporter list, what the dashboard is
watching, and an **About this server** text, in `<details>` blocks. The last two
are this demo's own (`AboutText`/`UseCasesText` in `MetricsWeb.WebModule.pas`),
not the hub's: the hub's describe the VCL showcase and its `metrics_*` tools.

`GET /metrics` sits outside every MCP gate, so it is public. That is fine on a
developer's machine; anywhere real, put it behind authentication or bind it to
loopback.

## Where the numbers come from

* [`../Server.Metrics.Document.pas`](../Server.Metrics.Document.pas) — the fold
  and the rollups, shared with the JSON endpoint so the two cannot disagree.
* [`../Server.Metrics.pas`](../Server.Metrics.pas) — the hub, recording the
  protocol telemetry, the application metrics and the host gauges.
* `MetricsWeb.History.pas` — a bounded ring of samples taken every two seconds by
  the host form: the charts' past. The subsystem keeps no time series, so a
  server-rendered dashboard keeps its own, at a steady cadence, whether or not
  anyone has a page open.
* `MetricsWeb.View.pas` — turns a report plus that history into the object the
  templates read, with the numbers already formatted.

## Requirements

**Delphi 12 or newer.** `Web.Stencils` is part of the RTL from Delphi 12 (it is
present in this machine's 12 and 13 installations and absent from 11), so this
project cannot be built with the library's Delphi 11 floor. Nothing in
`Source\` changes because of it.

## Notes for anyone writing templates here

Everything below is a trap this dashboard fell into, in the order it did.

* **Every member a template reads must be a *property*.** WebStencils resolves
  `@Metrics.Name` against properties by RTTI; a public *field* renders as the
  literal text `Metrics.Name`. The view model keeps its data in private fields
  and exposes properties, for that reason.
* **`@` belongs to output, not to expressions.** `@Metrics.ServiceName` prints a
  value; inside a directive the name stands alone:

  ```html
  @if (Metrics.HasGauges) { … }
  @foreach (var gauge in Metrics.Gauges) { @gauge.Name }
  ```

  Writing `@if (@Metrics.HasGauges)` produces *"Error evaluating expression …:
  Invalid character in source (#64)"* — 64 is `@`.
* **`@foreach` iterates `System.Classes.TObjectList`**, `TStrings` and
  `TDataSet` natively (anything else with a `GetEnumerator`/`MoveNext`/`Current`
  shape also works). Note that this non-generic `TObjectList` lives in
  `System.Contnrs`, not `System.Classes` — the view unit uses it deliberately.
* **`InputFileName` is used as written**; only imported files (`@Import`,
  `@LayoutPage`) are resolved against `TWebStencilsEngine.RootDirectory`. This
  web module therefore passes a fully qualified path, and sets the engine root
  for the imports.
* **The stylesheet is served as a file on purpose.** `@media` is a WebStencils
  directive; a stylesheet that went through the parser would need `@Processing
  False` around its at-rules, or `@@media`.
* **A template error is not an exception.** WebStencils writes the directive it
  could not process into the output and carries on, so the page is the only
  symptom. `MetricsWeb.WebModule.pas` logs every error to Logify *and* to
  `web-error.log` beside the executable — that file is what turned a page of
  unresolved variables into a list of three specific mistakes.
* **Non-ASCII belongs in templates, not in Delphi string literals.** Source files
  here carry no BOM, and the compiler reads a BOM-less file with the ANSI code
  page: a literal `·` (two bytes in UTF-8) compiles to *two* characters, `Â` and
  `·`, and the page — served as UTF-8, as it declares — then shows `Â·`. The
  templates are immune (WebStencils reads them as UTF-8 files), which is why the
  fault showed up in only one separator of a line like
  `demo.system · min 5.99 · max 16.0 · up`: the two from the template were right
  and the one the view model built was not. Write such characters as code points
  (`SEPARATOR = ' ' + #$00B7 + ' '` in `MetricsWeb.View.pas`, the way Logify
  writes `CAUSE_BRANCH`), or keep the literal ASCII.
* **`{$R *.dfm}` is not optional in a web module unit.** Without it
  `TWebModule1.Create` raises `EResNotFound` while streaming the module's own
  resource, WebBroker answers nothing, and Indy fills the response with its
  default `<HTML><BODY><B>200 OK</B></BODY></HTML>` page. The symptom looks like
  a routing problem and is not one. (The IDE says *"TWebModule1 not found —
  perhaps you need to register it?"* for the same missing resource.)
* **Build the MCP server before the socket opens.** `ConfigureMCPServer` is
  called from the form, not from `WebModuleCreate`: creating the server and
  registering the annotated classes is a one-off cost of a few seconds, and
  running it on the first request puts that cost on a client *and* beside the
  form's own sampling timers.
* **A request that raises must not open a dialog.** Delphi's default exception
  handler shows a message box, which for a web application is a hung request;
  the form installs `Application.OnException` and logs instead.
