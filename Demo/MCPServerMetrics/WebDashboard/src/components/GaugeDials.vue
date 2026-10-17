<script setup lang="ts">
/**
 * The gauges, each on a dial: the arc is the value right now, the thin outer
 * arc the range this tab's window covered. A percentage runs to 100 and turns
 * amber at 75 and red at 90; any other gauge is scaled to a round number above
 * the largest value seen, so the arc moves with the window.
 */
import { computed } from 'vue'

import type { InstrumentSample } from '@/composables/useMetricsFeed'
import { formatValue } from '@/lib/format'
import { DIAL_RADIUS, DIAL_RANGE_RADIUS, dialArc, niceCeiling, sparkline } from '@/lib/geometry'
import type { MetricInstrument } from '@/types'

const props = defineProps<{
  gauges: MetricInstrument[]
  historyFor: (meter: string, name: string) => InstrumentSample[]
}>()

interface GaugeCard {
  instrument: MetricInstrument
  dialPath: string
  rangePath: string
  scaleMax: number
  tone: '' | 'warn' | 'bad'
  rangeText: string
  trend: 'up' | 'down' | 'flat'
  spark: string
}

const cards = computed<GaugeCard[]>(() =>
  props.gauges
    .slice()
    .sort((left, right) => `${left.meter}/${left.name}`.localeCompare(`${right.meter}/${right.name}`))
    .map((instrument) => {
      const values = props.historyFor(instrument.meter, instrument.name).map((sample) => sample.last)
      const min = values.length > 0 ? Math.min(...values) : 0
      const max = values.length > 0 ? Math.max(...values) : 0
      const first = values[0]
      const last = values[values.length - 1]

      const isPercent =
        instrument.unit.includes('%') ||
        instrument.unit.toLowerCase() === 'percent' ||
        instrument.name.toLowerCase().endsWith('percent')
      const scaleMax = isPercent ? 100 : niceCeiling(Math.max(max, instrument.last))

      let tone: GaugeCard['tone'] = ''
      if (isPercent && instrument.last >= 90) tone = 'bad'
      else if (isPercent && instrument.last >= 75) tone = 'warn'

      return {
        instrument,
        dialPath: dialArc(0, instrument.last / scaleMax, DIAL_RADIUS),
        rangePath: values.length > 0 ? dialArc(min / scaleMax, max / scaleMax, DIAL_RANGE_RADIUS) : '',
        scaleMax,
        tone,
        rangeText: values.length > 0 ? `min ${formatValue(min)} · max ${formatValue(max)}` : 'no history yet',
        trend: values.length < 2 || first === last ? 'flat' : last > first ? 'up' : 'down',
        spark: sparkline(values).points,
      }
    }),
)
</script>

<template>
  <section class="panel">
    <h2>Gauges</h2>
    <div class="note">
      The value right now on a dial; the thin outer arc is the range the window covered. A
      percentage runs to 100 and turns amber at 75 and red at 90; any other gauge is scaled to a
      round number above the largest value seen.
    </div>
    <div v-if="cards.length > 0" class="grid gauges">
      <div
        v-for="card in cards"
        :key="`${card.instrument.meter}/${card.instrument.name}`"
        class="kpi gauge"
        :class="card.tone"
      >
        <div class="label">{{ card.instrument.name }}</div>
        <svg
          class="dial"
          viewBox="0 0 120 72"
          role="img"
          :aria-label="`${card.instrument.name} ${formatValue(card.instrument.last)} ${card.instrument.unit}`"
        >
          <path class="track" d="M14,62 A46,46 0 0 1 106,62" />
          <path v-if="card.rangePath" class="range-arc" :d="card.rangePath" />
          <path v-if="card.dialPath" class="value-arc" :d="card.dialPath" />
          <text class="dial-value" x="60" y="52" text-anchor="middle">
            {{ formatValue(card.instrument.last) }}
          </text>
          <text class="dial-unit" x="60" y="62" text-anchor="middle">{{ card.instrument.unit }}</text>
          <text class="dial-scale" x="14" y="71" text-anchor="middle">0</text>
          <text class="dial-scale" x="106" y="71" text-anchor="middle">
            {{ formatValue(card.scaleMax) }}
          </text>
        </svg>
        <div class="hint">
          {{ card.instrument.meter || '(default)' }} · {{ card.rangeText }} · {{ card.trend }}
        </div>
        <svg
          v-if="card.spark"
          class="spark"
          viewBox="0 0 100 34"
          preserveAspectRatio="none"
          role="img"
          :aria-label="card.instrument.name"
        >
          <polyline
            :points="card.spark"
            fill="none"
            stroke="#4aa3ff"
            stroke-width="1.5"
            vector-effect="non-scaling-stroke"
          />
        </svg>
      </div>
    </div>
    <p v-else class="empty">No gauge recorded yet: the metrics hub samples the host on a timer.</p>
  </section>
</template>
