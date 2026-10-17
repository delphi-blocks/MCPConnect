/**
 * The small SVG geometry the instrument cards draw: sparklines, the gauge
 * dial's arcs and the scale a dial runs on. A port of the helpers in the
 * WebStencils dashboard (MetricsWeb.View.pas), so the two pages draw the same
 * shapes from the same numbers.
 */

/** The view box every sparkline is drawn in. */
export const SPARK_WIDTH = 100
export const SPARK_HEIGHT = 34

export interface Sparkline {
  /** The polyline's points, '' when there is nothing to draw. */
  points: string
  /** The same line closed down to the baseline, for the filled area. */
  area: string
  /** The largest value, which the line is scaled against. */
  max: number
}

/**
 * A polyline (and its filled area) over the sparkline view box, drawn from zero:
 * a chart that rescales to its own minimum is a chart that lies.
 */
export function sparkline(values: number[]): Sparkline {
  const max = values.reduce((top, value) => (Number.isFinite(value) && value > top ? value : top), 0)
  if (values.length < 2 || max <= 0) return { points: '', area: '', max }

  const points = values
    .map((value, index) => {
      const x = (index / (values.length - 1)) * SPARK_WIDTH
      const safe = Number.isFinite(value) && value > 0 ? value : 0
      const y = SPARK_HEIGHT - 2 - (safe / max) * (SPARK_HEIGHT - 6)
      return `${x.toFixed(1)},${y.toFixed(1)}`
    })
    .join(' ')

  return { points, area: `0,${SPARK_HEIGHT} ${points} ${SPARK_WIDTH},${SPARK_HEIGHT}`, max }
}

/** The smallest of 1, 2 or 5 times a power of ten that is not below the value. */
export function niceCeiling(value: number): number {
  if (!Number.isFinite(value) || value <= 0) return 1
  const power = Math.pow(10, Math.floor(Math.log10(value)))
  const mantissa = value / power
  const nice = mantissa <= 1 ? 1 : mantissa <= 2 ? 2 : mantissa <= 5 ? 5 : 10
  return nice * power
}

/** The centre of a dial's half circle, in its 0..120 by 0..72 view box. */
const DIAL_CX = 60
const DIAL_CY = 62

/** The radius of the value arc, and of the thinner window-range arc outside it. */
export const DIAL_RADIUS = 46
export const DIAL_RANGE_RADIUS = 55

/**
 * An SVG arc along a dial, from one fraction of the scale to another: 0 is the
 * left end of the half circle, 1 the right. '' when the two coincide.
 */
export function dialArc(from: number, to: number, radius: number): string {
  const clamp = (value: number) => Math.min(1, Math.max(0, Number.isFinite(value) ? value : 0))
  const start = clamp(from)
  const end = clamp(to)
  if (end - start < 0.002) return ''

  const pointAt = (fraction: number) => {
    const angle = Math.PI * (1 - fraction)
    return `${(DIAL_CX + radius * Math.cos(angle)).toFixed(1)},${(DIAL_CY - radius * Math.sin(angle)).toFixed(1)}`
  }

  // Never more than half a circle, so the large-arc flag is always 0; sweep 1
  // runs clockwise on screen, from the left end over the top.
  return `M${pointAt(start)} A${radius},${radius} 0 0 1 ${pointAt(end)}`
}

/** A 0..100 percentage for a CSS length. */
export function percent(value: number): string {
  const clamped = Math.min(100, Math.max(0, Number.isFinite(value) ? value : 0))
  return `${clamped.toFixed(1)}%`
}
