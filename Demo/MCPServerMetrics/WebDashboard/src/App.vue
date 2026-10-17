<script setup lang="ts">
/**
 * The dashboard: one polling feed, and the panels that read it - laid out like
 * the WebStencils dashboard (ServerWebStencils), so the two pages read the same
 * whether the HTML is rendered on the server or in the browser.
 *
 * Everything here is derived in the browser from GET /metrics - the endpoint is
 * a snapshot with no past, so the history a chart draws is what this tab has
 * seen since it was opened. Reloading starts it over; that is the trade for not
 * running a time-series store next to the server.
 */
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'

import BreakdownCards from '@/components/BreakdownCards.vue'
import CounterCards from '@/components/CounterCards.vue'
import GaugeDials from '@/components/GaugeDials.vue'
import HeroBanner from '@/components/HeroBanner.vue'
import HistogramCards from '@/components/HistogramCards.vue'
import InstrumentTable from '@/components/InstrumentTable.vue'
import KpiCards from '@/components/KpiCards.vue'
import LatencyChart from '@/components/LatencyChart.vue'
import RawDocument from '@/components/RawDocument.vue'
import SlowestTable from '@/components/SlowestTable.vue'
import ThroughputChart from '@/components/ThroughputChart.vue'
import { METRICS_URL } from '@/api/metrics'
import { useMetricsFeed } from '@/composables/useMetricsFeed'
import { formatAge, formatClock, formatUptime } from '@/lib/format'
import { throughputSeries } from '@/lib/series'

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
const counters = computed(() => instrumentsOfKind('counter'))
const histograms = computed(() => instrumentsOfKind('histogram'))

const serviceName = computed(() => document.value?.service.name || 'unnamed server')

/** How much time the retained samples cover, as the WebStencils header says it. */
const historySpan = computed(() => {
  const list = samples.value
  if (list.length < 2) return 'no history yet'
  return formatUptime((list[list.length - 1].at - list[0].at) / 1000)
})

const statusClass = computed(() => {
  if (status.value === 'ok') return 'badge ok'
  if (status.value === 'error') return 'badge bad'
  if (status.value === 'paused') return 'badge warn'
  return 'badge'
})

const statusLabel = computed(() => {
  if (status.value === 'ok') return 'live in the browser'
  if (status.value === 'error') return 'unreachable'
  if (status.value === 'paused') return 'paused'
  return 'connecting'
})
</script>

<template>
  <header class="app-header">
    <div class="app-title">
      <a href="https://github.com/delphi-blocks/MCPConnect" class="app-logo">
        <img src="/assets/logo.svg" alt="MCPConnect" width="64" height="64" />
      </a>
    </div>
    <div class="app-side">
      <h1>{{ document ? `${serviceName} metrics` : 'MCPConnect metrics' }}</h1>
      <div class="sub">
        <template v-if="document">
          <template v-if="document.service.description">{{ document.service.description }} · </template>
          {{ document.service.version }} · started
          {{ formatClock(Date.parse(document.service.startedAt)) }} · generated
          {{ formatClock(Date.parse(document.generatedAt)) }} · {{ document.schema }}
        </template>
        <template v-else>waiting for the first answer from {{ METRICS_URL }}</template>
      </div>
      <div class="app-controls">
        <span :class="statusClass"><span class="dot" />{{ statusLabel }}</span>
        <span class="badge">{{ samples.length }} samples · {{ historySpan }}</span>
        <span v-if="document" class="badge">
          {{ document.summary.instruments }} instruments · {{ document.summary.meters.length }} meters
        </span>
        <span class="badge">{{ polls }} polls · {{ formatAge(lastUpdated, now) }}</span>
        <select
          class="badge"
          :value="intervalMs"
          @change="setIntervalMs(Number(($event.target as HTMLSelectElement).value))"
        >
          <option v-for="option in intervals" :key="option.value" :value="option.value">
            every {{ option.label }}
          </option>
        </select>
        <button class="badge" @click="pollNow">Refresh now</button>
        <button class="badge" @click="togglePause">{{ status === 'paused' ? 'Resume' : 'Pause' }}</button>
        <button class="badge" @click="clearHistory">Clear history</button>
      </div>
    </div>
  </header>

  <main>
    <HeroBanner />

    <div v-if="status === 'error'" class="error-box">
      {{ error }}<br />
      <span class="empty">
        Start the server (MCPServerMetrics.exe or MCPServerWebStencils.exe) and make sure the
        endpoint is reachable at {{ METRICS_URL }}.
      </span>
    </div>

    <template v-if="document">
      <KpiCards :document="document" :rates="eventRates" :regressions="regressions" />

      <section class="grid two">
        <ThroughputChart :points="throughput" />
        <LatencyChart :histograms="histograms" :history-for="instrumentHistory" />
      </section>

      <CounterCards :counters="counters" :groups="document.groups" :history-for="instrumentHistory" />
      <HistogramCards :histograms="histograms" :history-for="instrumentHistory" />
      <GaugeDials :gauges="gauges" :history-for="instrumentHistory" />
      <InstrumentTable :instruments="document.instruments" />
      <BreakdownCards :groups="document.groups" />
      <SlowestTable :slowest="document.slowest" />
      <RawDocument :document="document" />
    </template>

    <section v-else class="panel">
      <h2>Connecting</h2>
      <p class="empty">
        Asking {{ METRICS_URL }} for the metrics document. In development, run
        <span class="mono">npm run dev</span> and the Vite proxy forwards this to
        <span class="mono">http://localhost:8080/metrics</span>.
      </p>
    </section>
  </main>

  <footer v-if="document" class="footnote">
    This page is drawn in the browser from <span class="mono">{{ METRICS_URL }}</span>, polled
    every {{ intervalMs / 1000 }} s. Counters are cumulative across the server's
    {{ document.fold.resetsDetected }} folded reset(s), so rates are derived from consecutive
    samples, and the history is kept in this tab only ({{ samples.length }} samples retained).
  </footer>
</template>
