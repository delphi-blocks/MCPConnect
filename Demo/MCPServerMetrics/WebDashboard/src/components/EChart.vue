<script setup lang="ts">
/**
 * A thin wrapper over ECharts: one instance per chart, kept in step with the
 * `option` prop and resized with its container. Everything the dashboard charts
 * goes through here, so no component has to know about init/dispose.
 *
 * ECharts is imported piece by piece rather than as one package: only the line
 * and bar charts this dashboard draws are registered, which is most of the
 * difference between a 1 MB bundle and a third of one.
 */
import { BarChart, LineChart } from 'echarts/charts'
import { GridComponent, LegendComponent, TooltipComponent } from 'echarts/components'
import * as echarts from 'echarts/core'
import { CanvasRenderer } from 'echarts/renderers'
import type { EChartsType } from 'echarts/core'
import type { EChartsOption } from 'echarts'
import { onBeforeUnmount, onMounted, ref, shallowRef, watch } from 'vue'

echarts.use([BarChart, LineChart, GridComponent, LegendComponent, TooltipComponent, CanvasRenderer])

/** The dashboard's theme, so no option has to repeat a colour. */
echarts.registerTheme('mcpconnect', {
  color: ['#4aa3ff', '#3fbf7f', '#e0a13a', '#e05a5a'],
  backgroundColor: 'transparent',
  textStyle: { color: '#e6ecf3' },
  legend: { textStyle: { color: '#93a1b3' } },
  tooltip: {
    backgroundColor: '#171d26',
    borderColor: '#2a3341',
    textStyle: { color: '#e6ecf3' },
  },
  categoryAxis: {
    axisLine: { lineStyle: { color: '#2a3341' } },
    axisLabel: { color: '#93a1b3' },
    splitLine: { lineStyle: { color: '#222b38' } },
  },
  valueAxis: {
    axisLine: { lineStyle: { color: '#2a3341' } },
    axisLabel: { color: '#93a1b3' },
    splitLine: { lineStyle: { color: '#222b38' } },
  },
})

const props = defineProps<{
  option: EChartsOption
  height?: string
}>()

const host = ref<HTMLDivElement | null>(null)
const chart = shallowRef<EChartsType | null>(null)
let observer: ResizeObserver | null = null

onMounted(() => {
  if (!host.value) return
  chart.value = echarts.init(host.value, 'mcpconnect', { renderer: 'canvas' })
  chart.value.setOption(props.option)

  observer = new ResizeObserver(() => chart.value?.resize())
  observer.observe(host.value)
})

watch(
  () => props.option,
  (option) => chart.value?.setOption(option, true),
  { deep: true },
)

onBeforeUnmount(() => {
  observer?.disconnect()
  observer = null
  chart.value?.dispose()
  chart.value = null
})
</script>

<template>
  <div ref="host" :style="{ width: '100%', height: height ?? '260px' }" />
</template>
