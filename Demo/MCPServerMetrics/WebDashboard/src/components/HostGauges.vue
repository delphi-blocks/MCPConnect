<script setup lang="ts">
/**
 * The host gauges (uptime, CPU, memory, handles...). A gauge is a current value,
 * so there is no rate to derive - the card shows the value and the shape of the
 * window retained in this tab, as a sparkline.
 */
import { computed } from 'vue'

import { formatValue } from '@/lib/format'
import type { ValuePoint } from '@/lib/series'
import type { MetricInstrument } from '@/types'

const props = defineProps<{
  gauges: MetricInstrument[]
  sparkFor: (meter: string, name: string) => ValuePoint[]
}>()

interface GaugeCard {
  instrument: MetricInstrument
  points: ValuePoint[]
  path: string
  min: number
  max: number
  trend: 'up' | 'down' | 'flat'
}

const cards = computed<GaugeCard[]>(() =>
  props.gauges
    .slice()
    .sort((left, right) => `${left.meter}/${left.name}`.localeCompare(`${right.meter}/${right.name}`))
    .map((instrument) => {
      const points = props.sparkFor(instrument.meter, instrument.name)
      const values = points.map((point) => point.value)
      const min = values.length > 0 ? Math.min(...values) : 0
      const max = values.length > 0 ? Math.max(...values) : 0
      const first = values[0]
      const last = values[values.length - 1]
      const trend: GaugeCard['trend'] =
        values.length < 2 || first === last ? 'flat' : last > first ? 'up' : 'down'
      return { instrument, points, path: sparkline(points, min, max), min, max, trend }
    }),
)

function sparkline(points: ValuePoint[], min: number, max: number): string {
  if (points.length < 2) return ''
  const span = max - min || 1
  const height = 30
  const width = 100
  return points
    .map((point, index) => {
      const x = (index / (points.length - 1)) * width
      const y = height - ((point.value - min) / span) * (height - 4) - 2
      return `${index === 0 ? 'M' : 'L'}${x.toFixed(1)},${y.toFixed(1)}`
    })
    .join(' ')
}
</script>

<template>
  <section class="panel">
    <h2>Host gauges</h2>
    <p v-if="cards.length === 0" class="empty">No gauge recorded yet.</p>
    <div v-else class="grid gauges">
      <div v-for="card in cards" :key="`${card.instrument.meter}/${card.instrument.name}`" class="kpi">
        <div class="label">{{ card.instrument.name }}</div>
        <div class="value">
          {{ formatValue(card.instrument.last) }}<small>{{ card.instrument.unit }}</small>
        </div>
        <div class="hint">
          {{ card.instrument.series }} series · min {{ formatValue(card.min) }} · max
          {{ formatValue(card.max) }}
        </div>
        <svg v-if="card.path" class="spark" viewBox="0 0 100 30" preserveAspectRatio="none">
          <path
            :d="card.path"
            fill="none"
            :stroke="card.trend === 'up' ? '#e0a13a' : card.trend === 'down' ? '#3fbf7f' : '#4aa3ff'"
            stroke-width="1.5"
            vector-effect="non-scaling-stroke"
          />
        </svg>
      </div>
    </div>
  </section>
</template>
