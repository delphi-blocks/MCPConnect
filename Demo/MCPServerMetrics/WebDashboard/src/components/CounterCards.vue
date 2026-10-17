<script setup lang="ts">
/**
 * One card per counter: the total, a rate line (the change between two samples
 * over the time between them) and a bar splitting the total by the counter's
 * first label key - the same card the WebStencils dashboard renders.
 */
import { computed } from 'vue'

import type { InstrumentSample } from '@/composables/useMetricsFeed'
import { formatCount, formatRate } from '@/lib/format'
import { percent, sparkline, type Sparkline } from '@/lib/geometry'
import type { MetricGroup, MetricInstrument } from '@/types'

const props = defineProps<{
  counters: MetricInstrument[]
  groups: MetricGroup[]
  historyFor: (meter: string, name: string) => InstrumentSample[]
}>()

/** The colours of the split bar, in order; whatever is left over is "other". */
const SPLIT_COLOURS = ['#4aa3ff', '#3fbf7f', '#e0a13a', '#b58cff', '#5fd0d0']
const SPLIT_OTHER_COLOUR = '#5b6676'

interface Split {
  value: string
  total: number
  share: number
  colour: string
}

interface CounterCard {
  instrument: MetricInstrument
  rate: number | null
  peak: number | null
  spark: Sparkline
  splitKey: string
  splits: Split[]
}

/**
 * The per-second rate of the total across the retained samples. A total that
 * went backwards is a restart inside the window: that interval is unknown, not
 * negative, and is skipped.
 */
function rates(history: InstrumentSample[]): number[] {
  const result: number[] = []
  for (let index = 1; index < history.length; index += 1) {
    const seconds = (history[index].at - history[index - 1].at) / 1000
    const delta = history[index].sum - history[index - 1].sum
    if (seconds <= 0 || delta < 0) continue
    result.push(delta / seconds)
  }
  return result
}

/** The counter's first label key, ranked largest first. */
function splitsOf(instrument: MetricInstrument): { key: string; splits: Split[] } {
  const group = props.groups.find(
    (item) => item.meter === instrument.meter && item.name === instrument.name,
  )
  if (!group) return { key: '', splits: [] }

  const values = group.values.slice().sort((left, right) => right.sum - left.sum)
  const total = values.reduce((sum, item) => sum + item.sum, 0)
  if (total <= 0) return { key: '', splits: [] }

  const splits: Split[] = values.slice(0, SPLIT_COLOURS.length).map((item, index) => ({
    value: item.value,
    total: item.sum,
    share: (item.sum / total) * 100,
    colour: SPLIT_COLOURS[index],
  }))
  const other = values.slice(SPLIT_COLOURS.length).reduce((sum, item) => sum + item.sum, 0)
  if (other > 0) {
    splits.push({ value: 'other', total: other, share: (other / total) * 100, colour: SPLIT_OTHER_COLOUR })
  }
  return { key: group.label, splits }
}

const cards = computed<CounterCard[]>(() =>
  props.counters.map((instrument) => {
    const series = rates(props.historyFor(instrument.meter, instrument.name))
    const { key, splits } = splitsOf(instrument)
    return {
      instrument,
      rate: series.length > 0 ? series[series.length - 1] : null,
      peak: series.length > 0 ? Math.max(...series) : null,
      spark: sparkline(series),
      splitKey: key,
      splits,
    }
  }),
)
</script>

<template>
  <section class="panel">
    <h2>Counters</h2>
    <div class="note">
      Running totals that only go up. The line is the rate - the change between two samples
      over the time between them - and the bar splits the total by the counter's first label
      key.
    </div>
    <div v-if="cards.length > 0" class="grid instruments">
      <article
        v-for="card in cards"
        :key="`${card.instrument.meter}/${card.instrument.name}`"
        class="inst counter"
        :title="card.instrument.description"
      >
        <header>
          <span class="inst-name mono">{{ card.instrument.name }}</span>
          <span class="inst-meter">{{ card.instrument.meter || '(default)' }}</span>
        </header>
        <div class="inst-value">
          {{ formatCount(card.instrument.sum) }}<small>{{ card.instrument.unit }}</small>
        </div>
        <div class="inst-hint">
          {{ card.rate === null ? '-' : formatRate(card.rate) }}/s now · peak
          {{ card.peak === null ? '-' : formatRate(card.peak) }}/s ·
          {{ formatCount(card.instrument.series) }} series
        </div>
        <svg
          v-if="card.spark.points"
          class="spark"
          viewBox="0 0 100 34"
          preserveAspectRatio="none"
          role="img"
          :aria-label="`${card.instrument.name} rate`"
        >
          <polygon :points="card.spark.area" fill="#3fbf7f" opacity="0.15" />
          <polyline
            :points="card.spark.points"
            fill="none"
            stroke="#3fbf7f"
            stroke-width="1.5"
            vector-effect="non-scaling-stroke"
          />
        </svg>
        <div v-else class="spark-empty">the rate appears after two samples with traffic</div>
        <template v-if="card.splits.length > 0">
          <div class="split-title">by {{ card.splitKey }}</div>
          <div class="split" role="img" :aria-label="`${card.instrument.name} by ${card.splitKey}`">
            <span
              v-for="split in card.splits"
              :key="split.value"
              :style="{ width: percent(split.share), background: split.colour }"
              :title="`${split.value} - ${formatCount(split.total)} (${split.share.toFixed(0)}%)`"
            />
          </div>
          <ul class="split-legend">
            <li v-for="split in card.splits" :key="split.value">
              <i :style="{ background: split.colour }" /><span class="mono">{{ split.value }}</span>
              <em>{{ split.share.toFixed(0) }}%</em>
            </li>
          </ul>
        </template>
      </article>
    </div>
    <p v-else class="empty">
      No counter recorded yet. Call a tool over MCP and the request counters appear.
    </p>
  </section>
</template>
