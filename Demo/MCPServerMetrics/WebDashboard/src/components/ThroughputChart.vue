<script setup lang="ts">
/** Requests, errors and tool calls per second, derived from two samples each. */
import { computed } from 'vue'
import type { EChartsOption } from 'echarts'

import EChart from '@/components/EChart.vue'
import type { ThroughputPoint } from '@/lib/series'
import { formatClock } from '@/lib/format'

const props = defineProps<{ points: ThroughputPoint[] }>()

const option = computed<EChartsOption>(() => {
  const times = props.points.map((point) => formatClock(point.at))

  const series = [
    { name: 'requests/s', color: '#4aa3ff', values: props.points.map((p) => p.requests) },
    { name: 'tool calls/s', color: '#3fbf7f', values: props.points.map((p) => p.toolCalls) },
    { name: 'errors/s', color: '#e05a5a', values: props.points.map((p) => p.errors) },
  ]

  return {
    backgroundColor: 'transparent',
    grid: { left: 8, right: 12, top: 34, bottom: 8, containLabel: true },
    legend: { top: 0, textStyle: { color: '#93a1b3' }, itemHeight: 8, itemWidth: 14 },
    tooltip: { trigger: 'axis' },
    xAxis: { type: 'category', data: times, boundaryGap: false, axisLine: { lineStyle: { color: '#2a3341' } } },
    yAxis: {
      type: 'value',
      min: 0,
      splitLine: { lineStyle: { color: '#222b38' } },
      axisLabel: { color: '#93a1b3' },
    },
    series: series.map((entry) => ({
      name: entry.name,
      type: 'line',
      smooth: true,
      showSymbol: false,
      lineStyle: { width: 1.6, color: entry.color },
      itemStyle: { color: entry.color },
      areaStyle: { opacity: 0.08, color: entry.color },
      data: entry.values,
    })),
  }
})
</script>

<template>
  <section class="panel">
    <h2>Throughput</h2>
    <p v-if="points.length < 2" class="empty">
      Waiting for a second sample - a rate needs two.
    </p>
    <EChart v-else :option="option" height="240px" />
  </section>
</template>
