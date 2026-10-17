<script setup lang="ts">
/** Every instrument the server holds, one row each, filterable and sortable. */
import { computed, ref } from 'vue'

import { formatCount, formatValue } from '@/lib/format'
import type { MetricInstrument } from '@/types'

type SortKey = 'meter' | 'name' | 'kind' | 'series' | 'count' | 'sum' | 'avg' | 'max' | 'last'

const props = defineProps<{ instruments: MetricInstrument[] }>()

const filter = ref('')
const sortKey = ref<SortKey>('name')
const descending = ref(false)

const columns: { key: SortKey; title: string; numeric: boolean }[] = [
  { key: 'meter', title: 'meter', numeric: false },
  { key: 'name', title: 'instrument', numeric: false },
  { key: 'kind', title: 'kind', numeric: false },
  { key: 'series', title: 'series', numeric: true },
  { key: 'count', title: 'count', numeric: true },
  { key: 'sum', title: 'sum', numeric: true },
  { key: 'avg', title: 'avg', numeric: true },
  { key: 'max', title: 'max', numeric: true },
  { key: 'last', title: 'last', numeric: true },
]

const rows = computed(() => {
  const needle = filter.value.trim().toLowerCase()
  const filtered = needle
    ? props.instruments.filter((item) =>
        `${item.meter}/${item.name} ${item.kind} ${item.unit} ${item.description}`
          .toLowerCase()
          .includes(needle),
      )
    : props.instruments.slice()

  const key = sortKey.value
  filtered.sort((left, right) => {
    const a = left[key]
    const b = right[key]
    const comparison =
      typeof a === 'number' && typeof b === 'number'
        ? a - b
        : String(a).localeCompare(String(b))
    return descending.value ? -comparison : comparison
  })

  return filtered
})

function sortBy(key: SortKey): void {
  if (sortKey.value === key) descending.value = !descending.value
  else {
    sortKey.value = key
    descending.value = false
  }
}
</script>

<template>
  <section class="panel">
    <h2>Instruments</h2>
    <div class="note">One row per instrument, the labels folded into the totals.</div>
    <div class="toolbar">
      <input v-model="filter" type="search" placeholder="filter by meter, name, unit..." />
      <span class="spacer" />
      <span class="empty">{{ rows.length }} of {{ instruments.length }}</span>
    </div>
    <p v-if="instruments.length === 0" class="empty">Nothing recorded yet.</p>
    <div v-else class="table-scroll">
      <table class="sortable">
        <thead>
          <tr>
            <th
              v-for="column in columns"
              :key="column.key"
              :class="{ num: column.numeric }"
              @click="sortBy(column.key)"
            >
              {{ column.title }}{{ sortKey === column.key ? (descending ? ' ↓' : ' ↑') : '' }}
            </th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in rows" :key="`${row.meter}/${row.name}`">
            <td class="mono">{{ row.meter || '(default)' }}</td>
            <td class="mono">{{ row.name }}</td>
            <td>{{ row.kind }}</td>
            <td class="num">{{ formatCount(row.series) }}</td>
            <td class="num">{{ formatCount(row.count) }}</td>
            <td class="num">{{ formatValue(row.sum) }}</td>
            <td class="num">{{ formatValue(row.avg) }}</td>
            <td class="num">{{ formatValue(row.max) }}</td>
            <td class="num">{{ formatValue(row.last) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>
