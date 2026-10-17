/** Presentation helpers: the endpoint sends numbers, the screen shows them. */

const integerFormatter = new Intl.NumberFormat('en-US', { maximumFractionDigits: 0 })
const oneDecimalFormatter = new Intl.NumberFormat('en-US', { maximumFractionDigits: 1 })
const twoDecimalsFormatter = new Intl.NumberFormat('en-US', { maximumFractionDigits: 2 })

/** Counts: thousands separators, no decimals. */
export function formatCount(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) return '-'
  return integerFormatter.format(value)
}

/** A measurement: enough precision to be useful, never a wall of digits. */
export function formatValue(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) return '-'
  if (value === 0) return '0'
  const magnitude = Math.abs(value)
  if (magnitude >= 1000) return integerFormatter.format(value)
  if (magnitude >= 10) return oneDecimalFormatter.format(value)
  if (magnitude >= 0.01) return twoDecimalsFormatter.format(value)
  return value.toExponential(1)
}

/** Per-second rates, which are almost always small. */
export function formatRate(value: number | null | undefined): string {
  if (value === null || value === undefined || Number.isNaN(value)) return '-'
  return twoDecimalsFormatter.format(value)
}

/** A value with its unit, the way the metric subsystem recorded it. */
export function formatWithUnit(value: number | null | undefined, unit: string): string {
  const text = formatValue(value)
  return unit ? `${text} ${unit}` : text
}

/** Uptime as "2h 04m 11s". */
export function formatUptime(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds < 0) return '-'
  const total = Math.floor(seconds)
  const days = Math.floor(total / 86400)
  const hours = Math.floor((total % 86400) / 3600)
  const minutes = Math.floor((total % 3600) / 60)
  const rest = total % 60
  const pad = (value: number) => String(value).padStart(2, '0')
  if (days > 0) return `${days}d ${pad(hours)}h ${pad(minutes)}m`
  if (hours > 0) return `${hours}h ${pad(minutes)}m ${pad(rest)}s`
  if (minutes > 0) return `${minutes}m ${pad(rest)}s`
  return `${rest}s`
}

/** A wall-clock time for a sample, in the browser's own timezone. */
export function formatClock(at: number): string {
  return new Date(at).toLocaleTimeString([], {
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
  })
}

/** An ISO timestamp from the document, as local wall-clock time. */
export function formatIsoClock(iso: string): string {
  const parsed = Date.parse(iso)
  if (Number.isNaN(parsed)) return iso
  return formatClock(parsed)
}

/** A short relative age, for "last update 3s ago". */
export function formatAge(at: number | null, now: number): string {
  if (at === null) return 'never'
  const seconds = Math.max(0, (now - at) / 1000)
  if (seconds < 1) return 'just now'
  if (seconds < 60) return `${seconds.toFixed(0)}s ago`
  return `${Math.floor(seconds / 60)}m ago`
}

/** The label set of a series, as "method=tools/call". */
export function formatLabels(labels: Record<string, string>): string {
  const keys = Object.keys(labels).sort()
  if (keys.length === 0) return '(no labels)'
  return keys.map((key) => `${key}=${labels[key]}`).join(' ')
}
