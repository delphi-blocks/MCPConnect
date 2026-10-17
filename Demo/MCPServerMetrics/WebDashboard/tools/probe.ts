/**
 * A headless check of the whole loop, with no browser in it: it drives the
 * running demo over MCP to make some traffic, reads GET /metrics twice, and
 * pushes the two documents through the dashboard's own derivation code
 * (src/lib/series.ts) to see what a chart would draw.
 *
 *   npx esbuild tools/probe.ts --bundle --format=esm --platform=node --outfile=.probe.mjs
 *   node .probe.mjs
 *
 * It exits non-zero when the document and the derivation disagree, which is the
 * check to run after touching either side of the contract.
 */
import { instrumentSeries, throughputSeries } from '../src/lib/series'
import { seriesKey, type MetricsDocument } from '../src/types'
import type { FeedSample } from '../src/composables/useMetricsFeed'

const METRICS_URL = 'http://127.0.0.1:8080/metrics'
const MCP_URL = 'http://127.0.0.1:8080/'

const meta = {
  'io.modelcontextprotocol/protocolVersion': '2026-07-28',
  'io.modelcontextprotocol/clientCapabilities': {},
}

async function grab(): Promise<MetricsDocument> {
  const response = await fetch(METRICS_URL, { cache: 'no-store', headers: { Accept: 'application/json' } })
  if (!response.ok) throw new Error(`GET /metrics answered HTTP ${response.status}`)
  return (await response.json()) as MetricsDocument
}

async function callTool(id: number, name: string, args: Record<string, unknown>): Promise<number> {
  const response = await fetch(MCP_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Accept: 'application/json',
      'MCP-Protocol-Version': '2026-07-28',
      'Mcp-Method': 'tools/call',
      'Mcp-Name': name,
    },
    body: JSON.stringify({
      jsonrpc: '2.0',
      id,
      method: 'tools/call',
      params: { name, arguments: args, _meta: meta },
    }),
  })
  return response.status
}

function counters(document: MetricsDocument): Map<string, number> {
  const sums = new Map<string, number>()
  for (const point of document.cumulative) {
    if (point.kind === 'counter') sums.set(seriesKey(point), point.sum)
  }
  return sums
}

function totals(document: MetricsDocument, name: string): number {
  return document.cumulative
    .filter((point) => point.name === name)
    .reduce((sum, point) => sum + point.sum, 0)
}

const first = await grab()
const firstAt = Date.now()
console.log(`schema            : ${first.schema}`)
console.log(`service           : ${first.service.name} ${first.service.version}`)
console.log(`meters            : ${first.summary.meters.join(', ')}`)

// Traffic the dashboard should see as a rate between the two samples.
const statuses = [
  await callTool(901, 'metrics_simulate_workload', { operations: 25, regions: 'eu,us' }),
  await callTool(902, 'metrics_simulate_workload', { operations: 25, regions: 'eu,us' }),
  await callTool(903, 'metrics_simulate_errors', { operations: 4 }),
]
console.log(`MCP tool calls    : HTTP ${statuses.join(', ')}`)

await new Promise((resolve) => setTimeout(resolve, 1200))
const second = await grab()
const secondAt = Date.now()

const samples: FeedSample[] = [
  { at: firstAt, document: first, counters: counters(first) },
  { at: secondAt, document: second, counters: counters(second) },
]

const throughput = throughputSeries(samples)
console.log(`\nthroughput samples: ${throughput.length}`)
for (const point of throughput) {
  console.log(
    `  requests/s=${point.requests.toFixed(2)} tools/s=${point.toolCalls.toFixed(2)} errors/s=${point.errors.toFixed(2)}`,
  )
}

console.log(`\nrequest.count     : ${totals(first, 'request.count')} -> ${totals(second, 'request.count')}`)
console.log(`orders.created    : ${totals(first, 'orders.created')} -> ${totals(second, 'orders.created')}`)

const history = samples.map((sample) => {
  const found = sample.document.instruments.find(
    (item) => item.meter === 'mcp.server' && item.name === 'request.duration_ms',
  )
  return {
    at: sample.at,
    count: found?.count ?? 0,
    sum: found?.sum ?? 0,
    min: found?.min ?? 0,
    max: found?.max ?? 0,
    avg: found?.avg ?? 0,
    last: found?.last ?? 0,
  }
})
console.log(`latency avg series: ${JSON.stringify(instrumentSeries(history, 'avg'))}`)

const group = second.groups.find((item) => item.name === 'orders.created' && item.label === 'region')
console.log(`group check       : ${group ? group.values.map((v) => `${v.value}=${v.sum}`).join(', ') : 'none'}`)
const slowest = second.slowest[0]
console.log(`slowest[0]        : ${slowest ? `${slowest.name} avg=${slowest.avg.toFixed(2)} (${slowest.labels})` : 'none'}`)
console.log(`fold              : resets=${second.fold.resetsDetected} poll=${second.poll.count}`)

const failures: string[] = []
if (throughput.length !== 1) failures.push('expected exactly one derived throughput sample')
if (!throughput[0] || throughput[0].requests <= 0) failures.push('no request rate derived')
if (totals(second, 'orders.created') <= totals(first, 'orders.created')) failures.push('orders.created did not grow')
if (!group) failures.push('no region group for orders.created')
if (second.instruments.length === 0) failures.push('no instruments in the document')
if (history[1].count <= history[0].count) failures.push('latency histogram did not grow')

console.log(failures.length === 0 ? '\nPROBE OK' : `\nPROBE FAILED: ${failures.join('; ')}`)
process.exit(failures.length === 0 ? 0 : 1)
