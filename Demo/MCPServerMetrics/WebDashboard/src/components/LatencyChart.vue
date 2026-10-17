<script setup lang="ts">
/**
 * One histogram over time. A histogram keeps count, sum, min and max - not
 * buckets - so the average (sum/count) and the observed maximum are what there
 * is to chart; there is no percentile to draw.
 */
import { computed, ref, watch } from 'vue'
import type { EChartsOption } from 'echarts'

import EChart from '@/components/EChart.vue'
import { formatClock, formatValue } from '@/lib/format'
import type { InstrumentSample } from '@/composables/useMetricsFeed'
import type { MetricInstrument } from '@/types'

const props = defineProps<{
  histograms: MetricInstrument[]
  historyFor: (meter: string, name: string) => InstrumentSample[]
}>()

const selected = ref<string>('')

watch(
  () => props.histograms,
  (list) => {
    if (selected.value === '' && list.length > 0) {
      const preferred = list.find((item) => item.name === 'request.duration_ms')
      selected.value = key(preferred ?? list[0])
    }
  },
  { immediate: true },
)

function key(instrument: MetricInstrument): string {
  return `${instrument.meter}/${instrument.name}`
}

const current = computed(() => {
  const [meter, name] = selected.value.split('/')
  const instrument = props.histograms.find((item) => item.meter === meter && item.name === name)
  return { meter, name, instrument }
})

const history = computed(() => {
  const { meter, name } = current.value
  if (!meter || !name) return []
  return props.historyFor(meter, name)
})

const option = computed<EChartsOption>(() => {
  const times = history.value.map((sample) => formatClock(sample.at))

  return {
    backgroundColor: 'transparent',
    grid: { left: 8, right: 12, top: 34, bottom: 8, containLabel: true },
    legend: { top: 0, textStyle: { color: '#93a1b3' }, itemHeight: 8, itemWidth: 14 },
    tooltip: { trigger: 'axis' },
    xAxis: { type: 'category', data: times, boundaryGap: false, axisLine: { lineStyle: { color: '#2a3341' } } },
    yAxis: {
      type: 'value',
      min: 0,
      name: current.value.instrument?.unit ?? 'ms',
      nameTextStyle: { color: '#93a1b3' },
      splitLine: { lineStyle: { color: '#222b38' } },
      axisLabel: { color: '#93a1b3' },
    },
    series: [
      {
        name: 'average',
        type: 'line',
        smooth: true,
        showSymbol: false,
        lineStyle: { width: 1.8, color: '#4aa3ff' },
        itemStyle: { color: '#4aa3ff' },
        areaStyle: { opacity: 0.1, color: '#4aa3ff' },
        data: history.value.map((sample) => Number(sample.avg.toFixed(3))),
      },
      {
        name: 'max',
        type: 'line',
        smooth: true,
        showSymbol: false,
        lineStyle: { width: 1.4, color: '#e0a13a', type: 'dashed' },
        itemStyle: { color: '#e0a13a' },
        data: history.value.map((sample) => Number(sample.max.toFixed(3))),
      },
    ],
  }
})

const latest = computed(() => history.value[history.value.length - 1])
</script>

<template>
  <section class="panel">
    <h2>Latency</h2>
    <div class="toolbar">
      <select v-model="selected" :disabled="histograms.length === 0">
        <option v-for="instrument in histograms" :key="key(instrument)" :value="key(instrument)">
          {{ instrument.meter }}/{{ instrument.name }}
        </option>
      </select>
      <span v-if="latest" class="empty">
        latest: avg {{ formatValue(latest.avg) }} · max {{ formatValue(latest.max) }} ·
        {{ latest.count }} sample(s)
      </span>
    </div>
    <p v-if="history.length < 2" class="empty">Not enough history yet.</p>
    <EChart v-else :option="option" height="240px" />
  </section>
</template>
