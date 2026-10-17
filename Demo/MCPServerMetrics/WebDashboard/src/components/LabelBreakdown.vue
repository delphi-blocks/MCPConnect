<script setup lang="ts">
/**
 * One instrument split by one of its labels - "requests by method", "tool calls
 * by tool", "orders by region". The endpoint rolls every label key up over the
 * other keys, so the picker is simply the list it sent.
 */
import { computed, ref, watch } from 'vue'
import type { EChartsOption } from 'echarts'

import EChart from '@/components/EChart.vue'
import { formatCount, formatValue } from '@/lib/format'
import type { MetricGroup } from '@/types'

const props = defineProps<{ groups: MetricGroup[] }>()

const selected = ref('')

function key(group: MetricGroup): string {
  return `${group.meter}/${group.name}#${group.label}`
}

watch(
  () => props.groups,
  (list) => {
    if (list.length === 0) return
    if (!list.some((group) => key(group) === selected.value)) {
      const preferred =
        list.find((group) => group.name === 'request.count' && group.label === 'method') ?? list[0]
      selected.value = key(preferred)
    }
  },
  { immediate: true },
)

const group = computed(() => props.groups.find((item) => key(item) === selected.value))

const values = computed(() =>
  (group.value?.values ?? []).slice().sort((left, right) => right.sum - left.sum),
)

const option = computed<EChartsOption>(() => {
  const histogram = group.value?.kind === 'histogram'
  return {
    backgroundColor: 'transparent',
    grid: { left: 8, right: 18, top: 10, bottom: 6, containLabel: true },
    tooltip: { trigger: 'axis', axisPointer: { type: 'shadow' } },
    xAxis: {
      type: 'value',
      splitLine: { lineStyle: { color: '#222b38' } },
      axisLabel: { color: '#93a1b3' },
    },
    yAxis: {
      type: 'category',
      inverse: true,
      data: values.value.map((item) => item.value),
      axisLabel: { color: '#e6ecf3' },
      axisLine: { lineStyle: { color: '#2a3341' } },
    },
    series: [
      {
        name: histogram ? 'average' : 'total',
        type: 'bar',
        barMaxWidth: 18,
        itemStyle: { color: '#4aa3ff', borderRadius: [0, 3, 3, 0] },
        data: values.value.map((item) => Number((histogram ? item.avg : item.sum).toFixed(3))),
      },
    ],
  }
})
</script>

<template>
  <section class="panel">
    <h2>Breakdown</h2>
    <p v-if="groups.length === 0" class="empty">
      Nothing labelled yet - the demo records labels as soon as a tool is called.
    </p>
    <template v-else>
      <div class="toolbar">
        <select v-model="selected">
          <option v-for="item in groups" :key="key(item)" :value="key(item)">
            {{ item.meter }}/{{ item.name }} by {{ item.label }}
          </option>
        </select>
      </div>

      <EChart :option="option" height="200px" />

      <div class="table-scroll">
        <table>
          <thead>
            <tr>
              <th class="static">{{ group?.label ?? 'value' }}</th>
              <th class="num static">series</th>
              <th class="num static">count</th>
              <th class="num static">sum</th>
              <th class="num static">avg</th>
              <th class="num static">max</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="item in values" :key="item.value">
              <td class="mono">{{ item.value }}</td>
              <td class="num">{{ formatCount(item.series) }}</td>
              <td class="num">{{ formatCount(item.count) }}</td>
              <td class="num">{{ formatValue(item.sum) }}</td>
              <td class="num">{{ formatValue(item.avg) }}</td>
              <td class="num">{{ formatValue(item.max) }}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>
  </section>
</template>
