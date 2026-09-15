# MCPServerMetricsClient — a command-line client for the metrics demo

This is the scratchpad that was used to exercise `Demo/MCPServerMetrics` while the
showcase was being built, kept as a repeatable demo. It talks MCP over
Streamable HTTP directly — no client library — so it doubles as a reference for
what a 2026-07-28 request looks like on the wire.

| File | What it is |
|---|---|
| `MCPClient.ps1` | the client: PowerShell, one file, runnable as-is |
| `README.md` | this |

## Running it

1. Build and start the server: run `Demo\MCPServerMetrics.exe` (it listens on
   `http://localhost:8080/` by default).
2. From this folder:

   ```powershell
   pwsh ./MCPClient.ps1
   ```

   or, against another port:

   ```powershell
   pwsh ./MCPClient.ps1 -Url http://localhost:9090/ -Truncate 4000
   ```

Options: `-Url` (endpoint, default `http://localhost:8080/`), `-ProtocolVersion`
(default `2026-07-28`), `-Truncate` (characters printed per answer, default 1200),
`-SkipMrtr` (skip the part that writes to the todo list).

The client is a normal PowerShell script, so it runs on Windows PowerShell 5.1
and PowerShell 7 alike; it needs nothing but `System.Net.Http`, which the script
loads itself.

## What it exercises

| Step | Request | Shows |
|---|---|---|
| 1 | `server/discover` | the server identity, the supported versions, the cache hints |
| 2 | `tools/list` | every tool the server registers |
| 3 | `tools/call metrics_snapshot` | the accumulated metrics, one line per series |
| 4 | `tools/call metrics_simulate_workload` | recording counters/histograms/gauges with labels |
| 5 | `tools/call metrics_instrument` | one instrument read back, series by series |
| 6 | `tools/call metrics_slowest` | the slowest histograms by average |
| 7 | `tools/call metrics_exporters` | the exporter list and the harvest history |
| 8 | `tools/call metrics_cardinality` | the per-instrument series budget and `DroppedSeries` |
| 9 | `tools/call metrics_separate_provider` | a second provider never mixing with the default |
| 10 | `resources/read resource://metrics/live` | the same snapshot as a JSON resource |
| 11 | `prompts/get analyse-metrics` | a prompt that embeds the snapshot for a model |
| 12 | `tools/call delete_task`, then the retry | MRTR: `input_required`, then the answer |
| 13 | `tools/call summarize_tasks`, then the retry | MRTR asking the *client*: `sampling/createMessage` |
| 14 | `tools/call import_tasks`, three rounds | MRTR: `roots/list`, then a form built from what was found there |
| 15 | `tools/call draft_day_plan`, then the retry | two requests — roots and sampling — in one `input_required` |

Steps 12–15 are the interesting ones, and they are one per kind of input
request. The first call of each carries no answer, so the server replies
`resultType: "input_required"` with the requests it needs fulfilled and, when it
has something to remember, a `requestState`; the client retries the same call
with that state and the answers under the keys the server chose.

- **12, `delete`** asks the *user*: an elicitation, answered `accept` + content.
- **13, `summary`** asks the *client's model*. The script has no model, so it
  fabricates the `TCreateMessageResult` (role, one text block, `model`,
  `stopReason`) — what the step demonstrates is the request: the system prompt,
  the token budget, and the advisory `modelPreferences` the client may ignore.
- **14** runs three rounds: `roots/list`, then an elicitation naming the files
  the server found in the root the client named, then the import. Its
  `requestState` is *signed* (`MCPRS1S.`), because it carries the folder the
  next round will read. The script creates that workspace under `%TEMP%\mcp-import-demo`
  and writes a `todo-import.md` for the server to find.
- **15** asks for roots and sampling at once: two keys in one interim result,
  two answers in one retry, and no `requestState` at all — nothing has to
  survive between the rounds.

## The request contract it demonstrates

Every POST sends three things beyond the JSON-RPC body:

- **Headers** `MCP-Protocol-Version: 2026-07-28` and `Mcp-Method: <method>`
  (plus `Mcp-Name` for `tools/call`, `resources/read` and `prompts/get`),
  which the request-headers middleware checks against the body.
- **The per-request `"_meta"`** with
  `io.modelcontextprotocol/protocolVersion` and
  `io.modelcontextprotocol/clientCapabilities` — both required, so a request
  without them is refused with `400`.
- **The client capabilities** — `elicitation`, `sampling` and `roots`, one per
  kind of request this script can answer — which is what lets an MRTR tool
  answer `input_required` at all. The server reads what an interim result asks
  for and refuses with `-32021 MissingRequiredClientCapability` (`400`) when the
  client never declared it, rather than asking for something it cannot do: drop
  `sampling` from the list and step 13 is refused, drop `roots` and steps 14
  and 15 are. Declaring `"sampling": {}` bare says nothing about its
  sub-capabilities — a request carrying `includeContext` or `tools` would need
  `context` / `tools` declared inside it, and none of these send either.

## Abridged output

```text
== server/discover ==========================================================
server    : MyMCPServer 1.0.0
versions  : 2026-07-28
cacheScope: public  ttlMs: 0

== tools/list ===============================================================
add_task, complete_task, delete_task, list_tasks, metrics_cardinality, ...

== tools/call metrics_snapshot ==============================================
[metrics] 11/09/2026 19:20:31 - 40 point(s)
  gauge demo.system/process.cpu_percent: value=6.25 {scope=process}
  histogram demo.workload/order.value_usd: count=13 sum=5913.4 avg=454.8 ...

== resources/read resource://metrics/live ===================================
[{"meter":"demo.system","name":"process.cpu_percent","kind":"gauge", ...

== prompts/get analyse-metrics ==============================================
Here is the current metrics snapshot of this MCP server:
...

== MRTR: delete_task asks, then the retry answers ===========================
Task #1 "Review the client demo" added successfully
resultType   : input_required
requestState : MCPRS1.eyJ0YXNrSWQiOjF9
keys         : delete
Task #1 "Review the client demo" deleted

== MRTR: summarize_tasks asks the client to sample a model ==================
resultType   : input_required
  key "summary" -> sampling/createMessage
systemPrompt : You summarize todo lists. Answer with the summary and nothing else.
maxTokens    : 400   temperature: 0.2
preferences  : hints=claude  speed=0.7  cost=0.5  intelligence=0.3
- One task is waiting on a review. - The rest are new.
-- summarized by demo-canned-model (stop reason: endTurn)

== MRTR: import_tasks - roots, then a form, then the import =================
resultType   : input_required
requestState : MCPRS1S.eyJzdGVwIjoiYXNrUm9vdHMiLCJyb290IjoiIn0=.QHgF3eV...
  key "where" -> roots/list
resultType   : input_required
  key "file" -> elicitation/create
question     : Import tasks from which file in ...\mcp-import-demo? (todo-import.md)
Imported 3 task(s) from ...\mcp-import-demo\todo-import.md

== MRTR: draft_day_plan asks for roots and sampling in one result ===========
resultType   : input_required
  key "where" -> roots/list
  key "draft" -> sampling/createMessage
=== Plan for today ===
Start with the review, then the release notes, then the README.
-- drafted by demo-canned-model
Workspace the plan is about:
  Demo workspace -> file:///C:/.../mcp-import-demo
```

## Notes

- For interactive exploration of the same endpoints, `Demo/api` holds the
  Bruno collection; this script is the non-interactive counterpart.
- The two file sinks the server writes (`metrics.jsonl`, `metrics.log`) stay
  open in append mode while it runs, so a tool that opens them for read+write may
  be refused. `resource://metrics/live` is the live, lock-free way to read the
  same document, and it is what step 10 uses.
