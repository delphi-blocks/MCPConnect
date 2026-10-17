<script setup lang="ts">
/**
 * One card per histogram: the average, the min..max band on a 0..max scale with
 * the average marked on it, and the average of each sample interval on its own.
 * A histogram here keeps count, sum, min and max - no buckets - so the band is
 * everything known about its distribution.
 */
import { computed } from 'vue'

import type { InstrumentSample } from '@/composables/useMetricsFeed'
import { formatCount, formatValue } from '@/lib/format'
import { percent, sparkline, type Sparkline } from '@/lib/geometry'
import type { MetricInstrument } from '@/types'

const props = defineProps<{
  histograms: MetricInstrument[]
  historyFor: (meter: string, name: string) => InstrumentSample[]
}>()

interface HistogramCard {
  instrument: MetricInstrument
  band: { left: string; width: string; avg: string } | null
  spark: Sparkline
  window: string
}

/**
 * The average of each interval alone: the change in the sum over the change in
 * the count. An interval with no observation repeats the previous value, so the
 * line keeps its shape instead of dropping to a zero nobody measured.
 */
function intervalAverages(history: InstrumentSample[]): number[] {
  const result: number[] = []
  for (let index = 1; index < history.length; index += 1) {
    const deltaCount = history[index].count - history[index - 1].count
    const deltaSum = history[index].sum - history[index - 1].sum
    if (deltaCount > 0) result.push(deltaSum / deltaCount)
    else if (result.length > 0) result.push(result[result.length - 1])
  }
  return result
}

/** The average and count of what was observed inside the retained window. */
function windowText(history: InstrumentSample[]): string {
  if (history.length < 2) return 'nothing observed in the window'
  const first = history[0]
  const last = history[history.length - 1]
  const count = last.count - first.count
  if (count <= 0) return 'nothing observed in the window'
  return `window avg ${formatValue((last.sum - first.sum) / count)} · ${formatCount(count)} observed`
}

const cards = computed<HistogramCard[]>(() =>
  props.histograms.map((instrument) => {
    const history = props.historyFor(instrument.meter, instrument.name)
    // The band runs on a scale from zero to the largest observation, so its left
    // edge is where the smallest one sits and the marker is the mean between.
    const band =
      instrument.count > 0 && instrument.max > 0
        ? {
            left: percent((instrument.min / instrument.max) * 100),
            width: percent(100 - (instrument.min / instrument.max) * 100),
            avg: percent((instrument.avg / instrument.max) * 100),
          }
        : null
    return { instrument, band, spark: sparkline(intervalAverages(history)), window: windowText(history) }
  }),
)
</script>

<template>
  <section class="panel">
    <h2>Histograms</h2>
    <div class="note">
      A histogram here keeps count, sum, min and max - no buckets - so the band is its
      distribution as far as it is known: from the smallest observation to the largest, with
      the average marked in amber. The line is the average of each sample interval on its own.
    </div>
    <div v-if="cards.length > 0" class="grid instruments">
      <article
        v-for="card in cards"
        :key="`${card.instrument.meter}/${card.instrument.name}`"
        class="inst histogram"
        :title="card.instrument.description"
      >
        <header>
          <span class="inst-name mono">{{ card.instrument.name }}</span>
          <span class="inst-meter">{{ card.instrument.meter || '(default)' }}</span>
        </header>
        <div class="inst-value">
          {{ formatValue(card.instrument.avg) }}<small>{{ card.instrument.unit }} avg</small>
        </div>
        <div class="inst-hint">
          {{ formatCount(card.instrument.count) }} observed · min
          {{ formatValue(card.instrument.min) }} · max {{ formatValue(card.instrument.max) }}
        </div>
        <div
          class="band"
          role="img"
          :aria-label="`${card.instrument.name} from ${formatValue(card.instrument.min)} to ${formatValue(card.instrument.max)}, average ${formatValue(card.instrument.avg)}`"
        >
          <template v-if="card.band">
            <div class="band-range" :style="{ left: card.band.left, width: card.band.width }" />
            <div class="band-avg" :style="{ left: card.band.avg }" />
          </template>
        </div>
        <div class="band-scale">
          <span>0</span><span>{{ formatValue(card.instrument.max) }} {{ card.instrument.unit }}</span>
        </div>
        <svg
          v-if="card.spark.points"
          class="spark"
          viewBox="0 0 100 34"
          preserveAspectRatio="none"
          role="img"
          :aria-label="`${card.instrument.name} average per interval`"
        >
          <polygon :points="card.spark.area" fill="#4aa3ff" opacity="0.15" />
          <polyline
            :points="card.spark.points"
            fill="none"
            stroke="#4aa3ff"
            stroke-width="1.5"
            vector-effect="non-scaling-stroke"
          />
        </svg>
        <div v-else class="spark-empty">the line appears after two samples with observations</div>
        <div class="inst-hint">{{ card.window }}</div>
      </article>
    </div>
    <p v-else class="empty">
      No histogram recorded yet. Call a tool over MCP and the request durations appear.
    </p>
  </section>
</template>
