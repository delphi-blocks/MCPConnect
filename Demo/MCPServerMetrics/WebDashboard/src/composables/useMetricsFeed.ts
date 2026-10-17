/**
 * The feed: one GET /metrics every interval, plus what the dashboard derives
 * from a series of them.
 *
 * Two things are worth knowing about the data.
 *
 * The endpoint is *cumulative*: the values only grow, across the host's own
 * harvest resets (that is what its `fold` section is about), so a rate is the
 * difference between two samples divided by the time between them - never the
 * value itself. A delta that went backwards would mean the server restarted or
 * was reset outside a harvest; it is counted and skipped rather than drawn as a
 * negative rate.
 *
 * The history is kept in the browser, not on the server: see the README for why
 * (the endpoint is a snapshot, and a snapshot has no past). It is bounded, so a
 * tab left open overnight does not grow forever.
 */

import { computed, onBeforeUnmount, ref } from 'vue'

import { fetchMetricsDocument, MetricsFetchError } from '@/api/metrics'
import { seriesKey, type MetricInstrument, type MetricPoint, type MetricsDocument } from '@/types'

export type FeedStatus = 'idle' | 'ok' | 'error' | 'paused'

export interface FeedSample {
  /** When the browser received it, in epoch milliseconds. */
  at: number
  document: MetricsDocument
  /** Cumulative counter values of this sample, keyed by series. */
  counters: Map<string, number>
}

/** One instrument's numbers at one moment, as a chart wants them. */
export interface InstrumentSample {
  at: number
  count: number
  sum: number
  min: number
  max: number
  avg: number
  last: number
}

const HISTORY_LIMIT = 300
const ERROR_RETRY_MS = 5000

export function useMetricsFeed() {
  const document = ref<MetricsDocument | null>(null)
  const samples = ref<FeedSample[]>([])
  const status = ref<FeedStatus>('idle')
  const error = ref<string | null>(null)
  const intervalMs = ref(2000)
  const lastUpdated = ref<number | null>(null)
  const polls = ref(0)
  /** Counter values seen going backwards - a restart, or a reset the fold could not see. */
  const regressions = ref(0)

  let timer: ReturnType<typeof setTimeout> | null = null
  let controller: AbortController | null = null
  let running = false

  function counterSums(metrics: MetricsDocument): Map<string, number> {
    const sums = new Map<string, number>()
    for (const point of metrics.cumulative) {
      if (point.kind === 'counter') sums.set(seriesKey(point), point.sum)
    }
    return sums
  }

  function ingest(metrics: MetricsDocument, at: number): void {
    const counters = counterSums(metrics)
    const previous = samples.value.length > 0 ? samples.value[samples.value.length - 1] : null

    if (previous) {
      for (const [key, value] of counters) {
        const before = previous.counters.get(key)
        if (before !== undefined && value < before) regressions.value += 1
      }
    }

    samples.value = [...samples.value, { at, document: metrics, counters }].slice(-HISTORY_LIMIT)
    document.value = metrics
    lastUpdated.value = at
  }

  async function poll(): Promise<void> {
    controller?.abort()
    controller = new AbortController()

    try {
      const metrics = await fetchMetricsDocument(controller.signal)
      polls.value += 1
      error.value = null
      status.value = 'ok'
      ingest(metrics, Date.now())
    } catch (cause) {
      if (cause instanceof DOMException && cause.name === 'AbortError') return
      status.value = 'error'
      error.value = cause instanceof MetricsFetchError ? cause.message : String(cause)
    }
  }

  function schedule(): void {
    if (!running) return
    // A chain of timeouts rather than an interval: a slow answer delays the next
    // request instead of stacking another one behind it.
    const wait = status.value === 'error' ? Math.max(intervalMs.value, ERROR_RETRY_MS) : intervalMs.value
    timer = setTimeout(() => {
      void poll().then(schedule)
    }, wait)
  }

  function start(): void {
    if (running) return
    running = true
    status.value = 'ok'
    void poll().then(schedule)
  }

  function stop(): void {
    running = false
    if (timer !== null) {
      clearTimeout(timer)
      timer = null
    }
    controller?.abort()
    controller = null
  }

  function pause(): void {
    if (!running) return
    stop()
    status.value = 'paused'
  }

  function resume(): void {
    if (running) return
    start()
  }

  function togglePause(): void {
    if (running) pause()
    else resume()
  }

  function setIntervalMs(value: number): void {
    intervalMs.value = value
    if (running) {
      if (timer !== null) clearTimeout(timer)
      schedule()
    }
  }

  function clearHistory(): void {
    samples.value = []
  }

  onBeforeUnmount(stop)

  /** Per-second rate of every counter series, from the two latest samples. */
  const rates = computed<Map<string, number>>(() => {
    const result = new Map<string, number>()
    const list = samples.value
    if (list.length < 2) return result

    const previous = list[list.length - 2]
    const current = list[list.length - 1]
    const seconds = (current.at - previous.at) / 1000
    if (seconds <= 0) return result

    for (const [key, value] of current.counters) {
      // A series the previous sample did not have is a counter that was zero
      // then; one that went backwards is a restart, and is left out.
      const before = previous.counters.get(key) ?? 0
      const delta = value - before
      if (delta <= 0) continue
      result.set(key, delta / seconds)
    }
    return result
  })

  /** The rates the demo's instruments are about, summed over their label sets. */
  const eventRates = computed(() => {
    const totals = { requests: 0, errors: 0, toolCalls: 0 }
    const metrics = document.value
    if (!metrics) return totals

    for (const point of metrics.cumulative) {
      const rate = rates.value.get(seriesKey(point)) ?? 0
      if (rate === 0) continue
      if (point.name === 'request.count') totals.requests += rate
      else if (point.name === 'request.errors') totals.errors += rate
      else if (point.name === 'tool.count') totals.toolCalls += rate
    }
    return totals
  })

  const isLive = computed(() => status.value === 'ok')

  /** One instrument, summed over its label sets, from the current document. */
  function instrument(meter: string, name: string): MetricInstrument | undefined {
    return document.value?.instruments.find((item) => item.meter === meter && item.name === name)
  }

  /** Every series of one instrument, from the cumulative array. */
  function seriesOf(meter: string, name: string): MetricPoint[] {
    return (document.value?.cumulative ?? []).filter(
      (point) => point.meter === meter && point.name === name,
    )
  }

  /** The instruments of one kind, for the pickers. */
  function instrumentsOfKind(kind: MetricPoint['kind']): MetricInstrument[] {
    return (document.value?.instruments ?? []).filter((item) => item.kind === kind)
  }

  /** One instrument's numbers across the retained history, for a chart. */
  function instrumentHistory(meter: string, name: string): InstrumentSample[] {
    const result: InstrumentSample[] = []
    for (const sample of samples.value) {
      const found = sample.document.instruments.find(
        (item) => item.meter === meter && item.name === name,
      )
      if (found) {
        result.push({
          at: sample.at,
          count: found.count,
          sum: found.sum,
          min: found.min,
          max: found.max,
          avg: found.avg,
          last: found.last,
        })
      }
    }
    return result
  }

  /** The host gauges, which the demo samples on its own timer. */
  const gauges = computed<MetricInstrument[]>(
    () => (document.value?.instruments ?? []).filter((item) => item.kind === 'gauge'),
  )

  return {
    document,
    samples,
    status,
    error,
    intervalMs,
    lastUpdated,
    polls,
    regressions,
    rates,
    eventRates,
    isLive,
    gauges,
    instrument,
    seriesOf,
    instrumentsOfKind,
    instrumentHistory,
    start,
    stop,
    pause,
    resume,
    togglePause,
    setIntervalMs,
    clearHistory,
    pollNow: poll,
  }
}
