/// <summary>
///   The document GET /metrics serves, mirroring Server.Metrics.Http.pas.
///   Everything the dashboard renders comes from here: the raw point arrays are
///   the data, the sections above them are conveniences the endpoint derived so
///   the client does not have to.
/// </summary>

export type MetricKind = 'counter' | 'gauge' | 'histogram' | 'unknown'

/** One series of one instrument, as MetricsToJson renders it. */
export interface MetricPoint {
  meter: string
  name: string
  kind: MetricKind
  description: string
  unit: string
  labels: Record<string, string>
  count: number
  sum: number
  min: number
  max: number
  last: number
  firstSeen: string
  lastSeen: string
}

/** One instrument summed over all of its label sets. */
export interface MetricInstrument {
  meter: string
  name: string
  kind: MetricKind
  unit: string
  description: string
  series: number
  count: number
  sum: number
  min: number
  max: number
  avg: number
  last: number
}

/** One label value, aggregated over the other labels of the same instrument. */
export interface MetricLabelRollup {
  value: string
  series: number
  count: number
  sum: number
  min: number
  max: number
  avg: number
  last: number
}

/** The values of one label key of one instrument - a "by method" table. */
export interface MetricGroup {
  meter: string
  name: string
  kind: MetricKind
  label: string
  values: MetricLabelRollup[]
}

/** One histogram series, ranked by average. */
export interface MetricSlowEntry {
  meter: string
  name: string
  labels: string
  count: number
  sum: number
  min: number
  max: number
  avg: number
}

export interface MetricsDocument {
  schema: string
  service: {
    name: string
    version: string
    description: string
    startedAt: string
    uptimeSeconds: number
  }
  generatedAt: string
  poll: {
    count: number
    suggestedIntervalMs: number
  }
  fold: {
    cumulative: boolean
    resetsDetected: number
    note: string
  }
  summary: {
    series: number
    instruments: number
    meters: string[]
    counters: { series: number; sum: number }
    histograms: { series: number; count: number; sum: number; max: number; avg: number }
    gauges: { series: number }
  }
  instruments: MetricInstrument[]
  groups: MetricGroup[]
  slowest: MetricSlowEntry[]
  /** Monotonic since the endpoint started: what a chart should be drawn from. */
  cumulative: MetricPoint[]
  /** The provider as it stands, i.e. since the host's last reset. */
  live: MetricPoint[]
}

/** A stable key for one series, used to line samples up across polls. */
export function seriesKey(point: Pick<MetricPoint, 'meter' | 'name' | 'labels'>): string {
  const labels = Object.keys(point.labels)
    .sort()
    .map((key) => `${key}=${point.labels[key]}`)
    .join(',')
  return `${point.meter}/${point.name}{${labels}}`
}
