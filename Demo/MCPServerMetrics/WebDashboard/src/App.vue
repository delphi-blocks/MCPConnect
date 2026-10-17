<script setup lang="ts">
/**
 * The dashboard: one polling feed, and the panels that read it.
 *
 * Everything here is derived in the browser from GET /metrics - the endpoint is
 * a snapshot with no past, so the history a chart draws is what this tab has
 * seen since it was opened. Reloading starts it over; that is the trade for not
 * running a time-series store next to the server.
 */
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'

import HostGauges from '@/components/HostGauges.vue'
import InstrumentTable from '@/components/InstrumentTable.vue'
import KpiCards from '@/components/KpiCards.vue'
import LabelBreakdown from '@/components/LabelBreakdown.vue'
import LatencyChart from '@/components/LatencyChart.vue'
import RawDocument from '@/components/RawDocument.vue'
import SlowestTable from '@/components/SlowestTable.vue'
import ThroughputChart from '@/components/ThroughputChart.vue'
import { METRICS_URL } from '@/api/metrics'
import { useMetricsFeed } from '@/composables/useMetricsFeed'
import { formatAge, formatClock } from '@/lib/format'
import { instrumentSeries, throughputSeries } from '@/lib/series'

const {
  document,
  samples,
  status,
  error,
  intervalMs,
  lastUpdated,
  polls,
  regressions,
  eventRates,
  gauges,
  instrumentsOfKind,
  instrumentHistory,
  togglePause,
  setIntervalMs,
  clearHistory,
  pollNow,
  start,
} = useMetricsFeed()

/** A ticking clock, so "last update" ages visibly while the feed is paused. */
const now = ref(Date.now())
const clock = setInterval(() => (now.value = Date.now()), 1000)

onMounted(start)
onBeforeUnmount(() => clearInterval(clock))

const intervals = [
  { label: '1 s', value: 1000 },
  { label: '2 s', value: 2000 },
  { label: '5 s', value: 5000 },
  { label: '10 s', value: 10000 },
  { label: '30 s', value: 30000 },
]

const throughput = computed(() => throughputSeries(samples.value))
const histograms = computed(() => instrumentsOfKind('histogram'))

const statusClass = computed(() => {
  if (status.value === 'ok') return 'badge ok'
  if (status.value === 'error') return 'badge bad'
  if (status.value === 'paused') return 'badge warn'
  return 'badge'
})

const statusLabel = computed(() => {
  if (status.value === 'ok') return 'live'
  if (status.value === 'error') return 'unreachable'
  if (status.value === 'paused') return 'paused'
  return 'connecting'
})

const sparkFor = (meter: string, name: string) =>
  instrumentSeries(instrumentHistory(meter, name), 'last')
</script>

<template>
  <header class="app-header">
    <div>
      <h1>MCPConnect metrics</h1>
      <div class="sub">
        <template v-if="document">
          {{ document.service.name || 'unnamed server' }} {{ document.service.version }} ·
          started {{ formatClock(Date.parse(document.service.startedAt)) }} · generated
          {{ formatClock(Date.parse(document.generatedAt)) }}
        </template>
        <template v-else>waiting for the first answer from {{ METRICS_URL }}</template>
      </div>
    </div>

    <div class="app-controls">
      <span :class="statusClass"><span class="dot" />{{ statusLabel }}</span>
      <span class="badge">{{ polls }} polls · {{ formatAge(lastUpdated, now) }}</span>
      <select
        :value="intervalMs"
        @change="setIntervalMs(Number(($event.target as HTMLSelectElement).value))"
      >
        <option v-for="option in intervals" :key="option.value" :value="option.value">
          every {{ option.label }}
        </option>
      </select>
      <button @click="pollNow">Refresh</button>
      <button @click="togglePause">{{ status === 'paused' ? 'Resume' : 'Pause' }}</button>
      <button @click="clearHistory">Clear history</button>
    </div>
  </header>

  <div v-if="status === 'error'" class="error-box">
    {{ error }}<br />
    <span class="empty">
      Start the demo (MCPServerMetrics.exe) and make sure the endpoint is reachable at
      {{ METRICS_URL }}.
    </span>
  </div>

  <template v-if="document">
    <KpiCards :document="document" :rates="eventRates" :regressions="regressions" />

    <div class="grid two">
      <ThroughputChart :points="throughput" />
      <LatencyChart :histograms="histograms" :history-for="instrumentHistory" />
    </div>

    <div class="grid">
      <HostGauges :gauges="gauges" :spark-for="sparkFor" />
    </div>

    <div class="grid two">
      <InstrumentTable :instruments="document.instruments" />
      <LabelBreakdown :groups="document.groups" />
    </div>

    <div class="grid">
      <SlowestTable :slowest="document.slowest" />
      <RawDocument :document="document" />
    </div>

    <p class="footnote">
      Polling <span class="mono">{{ METRICS_URL }}</span> every
      {{ intervalMs / 1000 }}s · counters are cumulative across the server's
      {{ document.fold.resetsDetected }} folded reset(s), so rates are derived from consecutive
      samples · history is kept in this tab only ({{ samples.length }} samples retained).
    </p>
  </template>

  <div v-else class="grid">
    <section class="panel">
      <h2>Connecting</h2>
      <p class="empty">
        Asking {{ METRICS_URL }} for the metrics document. In development, run
        <span class="mono">npm run dev</span> and the Vite proxy forwards this to
        <span class="mono">http://localhost:8080/metrics</span>.
      </p>
    </section>
  </div>
</template>
