/**
 * Turning a list of samples into something a chart can draw.
 *
 * Every counter the endpoint reports is cumulative, so a rate is the difference
 * between two consecutive samples over the time between them; a delta that went
 * backwards is dropped (the server restarted, or was reset in a way the fold
 * could not see) rather than charted as a negative.
 */

import type { FeedSample, InstrumentSample } from '@/composables/useMetricsFeed'
import { seriesKey, type MetricPoint } from '@/types'

export interface ThroughputPoint {
  at: number
  requests: number
  errors: number
  toolCalls: number
}

export interface ValuePoint {
  at: number
  value: number
}

/** Requests, errors and tool calls per second, sample by sample. */
export function throughputSeries(samples: FeedSample[]): ThroughputPoint[] {
  const result: ThroughputPoint[] = []

  for (let index = 1; index < samples.length; index += 1) {
    const previous = samples[index - 1]
    const current = samples[index]
    const seconds = (current.at - previous.at) / 1000
    if (seconds <= 0) continue

    const point: ThroughputPoint = { at: current.at, requests: 0, errors: 0, toolCalls: 0 }

    for (const metric of current.document.cumulative) {
      // Absent in the previous sample means the series did not exist yet, and a
      // counter that did not exist was zero: taking it as such is what gives the
      // first interval after startup a rate at all. A value that went backwards
      // is a different thing - a restart, or a reset the fold could not see - and
      // is skipped rather than charted as a negative.
      const before = previous.counters.get(seriesKey(metric)) ?? 0
      const delta = metric.sum - before
      if (delta <= 0) continue
      const rate = delta / seconds

      if (metric.name === 'request.count') point.requests += rate
      else if (metric.name === 'request.errors') point.errors += rate
      else if (metric.name === 'tool.count') point.toolCalls += rate
    }

    result.push(point)
  }

  return result
}

/** One field of one instrument across the history. */
export function instrumentSeries(
  history: InstrumentSample[],
  field: 'avg' | 'max' | 'last' | 'sum' | 'count',
): ValuePoint[] {
  return history.map((sample) => ({ at: sample.at, value: sample[field] }))
}

/** The counter series the dashboard ranks by, for the KPI hints. */
export function counterTotals(points: MetricPoint[], instrumentName: string): number {
  return points
    .filter((point) => point.name === instrumentName)
    .reduce((total, point) => total + point.sum, 0)
}

/** The current value of a gauge series, summed over its label sets. */
export function gaugeValue(points: MetricPoint[], meter: string, name: string): number {
  const series = points.filter((point) => point.meter === meter && point.name === name)
  if (series.length === 0) return 0
  return series.reduce((total, point) => total + point.last, 0)
}
