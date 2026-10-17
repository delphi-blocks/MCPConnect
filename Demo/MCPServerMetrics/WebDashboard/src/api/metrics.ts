/**
 * The one call the dashboard makes: GET /metrics, plain JSON, no MCP.
 *
 * The URL is relative by default, which is what makes both deployments work: in
 * development Vite proxies /metrics to the server, and a build served from the
 * server's own origin resolves it directly. A dashboard hosted elsewhere points
 * at the endpoint with VITE_METRICS_URL at build time.
 */

import type { MetricsDocument } from '@/types'

export const METRICS_URL: string = import.meta.env.VITE_METRICS_URL || '/metrics'

export class MetricsFetchError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'MetricsFetchError'
  }
}

export async function fetchMetricsDocument(signal?: AbortSignal): Promise<MetricsDocument> {
  let response: Response
  try {
    response = await fetch(METRICS_URL, {
      method: 'GET',
      headers: { Accept: 'application/json' },
      cache: 'no-store',
      signal,
    })
  } catch (cause) {
    if (cause instanceof DOMException && cause.name === 'AbortError') throw cause
    throw new MetricsFetchError(
      `cannot reach ${METRICS_URL} - is the server running, and does it expose the endpoint?`,
    )
  }

  if (!response.ok) {
    throw new MetricsFetchError(`${METRICS_URL} answered HTTP ${response.status} ${response.statusText}`)
  }

  const text = await response.text()
  let parsed: unknown
  try {
    parsed = JSON.parse(text)
  } catch {
    throw new MetricsFetchError(`${METRICS_URL} did not answer with JSON (is it the MCP endpoint?)`)
  }

  if (!isMetricsDocument(parsed)) {
    throw new MetricsFetchError(`${METRICS_URL} answered a document this dashboard does not recognise`)
  }

  return parsed
}

/**
 * A light shape check: enough to fail with a sentence rather than with an
 * undefined property three panels later. The endpoint is versioned by its
 * "schema" member for exactly this reason.
 */
function isMetricsDocument(value: unknown): value is MetricsDocument {
  if (typeof value !== 'object' || value === null) return false
  const candidate = value as Partial<MetricsDocument>
  return (
    typeof candidate.schema === 'string' &&
    candidate.schema.startsWith('mcpconnect.metrics/') &&
    Array.isArray(candidate.instruments) &&
    Array.isArray(candidate.groups) &&
    Array.isArray(candidate.cumulative) &&
    Array.isArray(candidate.live) &&
    typeof candidate.summary === 'object' &&
    candidate.summary !== null
  )
}
