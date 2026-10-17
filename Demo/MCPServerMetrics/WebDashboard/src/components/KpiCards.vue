<script setup lang="ts">
/** The six numbers worth reading at a glance, plus what they are made of. */
import { computed } from 'vue'

import { formatCount, formatRate, formatUptime, formatValue } from '@/lib/format'
import { gaugeValue } from '@/lib/series'
import type { MetricsDocument } from '@/types'

const props = defineProps<{
  document: MetricsDocument
  rates: { requests: number; errors: number; toolCalls: number }
  regressions: number
}>()

const requests = computed(() => props.document.cumulative.filter((p) => p.name === 'request.count'))
const errors = computed(() => props.document.cumulative.filter((p) => p.name === 'request.errors'))
const tools = computed(() => props.document.cumulative.filter((p) => p.name === 'tool.count'))

const requestTotal = computed(() => requests.value.reduce((total, p) => total + p.sum, 0))
const errorTotal = computed(() => errors.value.reduce((total, p) => total + p.sum, 0))
const toolTotal = computed(() => tools.value.reduce((total, p) => total + p.sum, 0))
const inFlight = computed(() => gaugeValue(props.document.cumulative, 'mcp.server', 'request.in_flight'))

const latency = computed(() =>
  props.document.instruments.find((item) => item.meter === 'mcp.server' && item.name === 'request.duration_ms'),
)
</script>

<template>
  <div class="grid kpis">
    <div class="kpi">
      <div class="label">Uptime</div>
      <div class="value">{{ formatUptime(document.service.uptimeSeconds) }}</div>
      <div class="hint">
        {{ document.service.name || 'unnamed server' }} {{ document.service.version }}
      </div>
    </div>

    <div class="kpi">
      <div class="label">Requests</div>
      <div class="value">{{ formatCount(requestTotal) }}</div>
      <div class="hint">{{ formatRate(rates.requests) }}/s · {{ requests.length }} series</div>
    </div>

    <div class="kpi" :class="{ bad: errorTotal > 0 }">
      <div class="label">Errors</div>
      <div class="value">{{ formatCount(errorTotal) }}</div>
      <div class="hint">{{ formatRate(rates.errors) }}/s · {{ errors.length }} series</div>
    </div>

    <div class="kpi">
      <div class="label">Tool calls</div>
      <div class="value">{{ formatCount(toolTotal) }}</div>
      <div class="hint">{{ formatRate(rates.toolCalls) }}/s · {{ tools.length }} series</div>
    </div>

    <div class="kpi" :class="{ warn: inFlight > 0 }">
      <div class="label">In flight</div>
      <div class="value">{{ formatCount(inFlight) }}</div>
      <div class="hint">requests being served right now</div>
    </div>

    <div class="kpi">
      <div class="label">Latency avg / max</div>
      <div class="value">
        {{ latency ? formatValue(latency.avg) : '-' }}<small>ms</small>
      </div>
      <div class="hint">
        max {{ latency ? formatValue(latency.max) : '-' }} ms over {{ formatCount(latency?.count ?? 0) }} calls
      </div>
    </div>

    <div class="kpi">
      <div class="label">Series</div>
      <div class="value">{{ formatCount(document.summary.series) }}</div>
      <div class="hint">
        {{ document.summary.instruments }} instruments · {{ document.summary.meters.length }} meters ·
        {{ document.fold.resetsDetected }} host resets folded
      </div>
    </div>
  </div>

  <div v-if="regressions > 0" class="error-box">
    {{ regressions }} counter value(s) went backwards in the retained history - the server was
    restarted, or reset outside a harvest. The rate for those samples was skipped.
  </div>
</template>
