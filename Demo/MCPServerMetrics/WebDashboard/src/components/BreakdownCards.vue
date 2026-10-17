<script setup lang="ts">
/**
 * Every instrument rolled up by each of its label keys, one card each - "requests
 * by method", "calls by tool". A histogram is compared by its average (its total
 * grows with the number of samples it happened to take), a counter by its total.
 */
import { computed } from 'vue'

import { formatCount, formatValue } from '@/lib/format'
import { percent } from '@/lib/geometry'
import type { MetricGroup, MetricLabelRollup } from '@/types'

const props = defineProps<{ groups: MetricGroup[] }>()

interface GroupRow {
  item: MetricLabelRollup
  bar: string
}

interface GroupCard {
  group: MetricGroup
  measure: 'average' | 'total'
  rows: GroupRow[]
}

const cards = computed<GroupCard[]>(() =>
  props.groups.map((group) => {
    const histogram = group.kind === 'histogram'
    const measureOf = (item: MetricLabelRollup) => (histogram ? item.avg : item.sum)
    const reference = group.values.reduce((top, item) => Math.max(top, measureOf(item)), 0)
    return {
      group,
      measure: histogram ? 'average' : 'total',
      rows: group.values.map((item) => ({
        item,
        bar: percent(reference > 0 ? (measureOf(item) / reference) * 100 : 0),
      })),
    }
  }),
)
</script>

<template>
  <section class="panel">
    <h2>Breakdown</h2>
    <div class="note">
      Every instrument rolled up by each of its label keys, aggregated over the others. A
      histogram is compared by its average, a counter by its total.
    </div>
    <div v-if="cards.length > 0" class="grid two">
      <section
        v-for="card in cards"
        :key="`${card.group.meter}/${card.group.name}#${card.group.label}`"
        class="panel inner"
      >
        <h3>{{ card.group.meter || '(default)' }}/{{ card.group.name }} by {{ card.group.label }}</h3>
        <div class="note">{{ card.group.kind }} · bars by {{ card.measure }}</div>
        <table>
          <thead>
            <tr>
              <th>{{ card.group.label }}</th>
              <th class="num">count</th>
              <th class="num">sum</th>
              <th class="num">avg</th>
              <th class="num">max</th>
              <th class="bar-col" />
            </tr>
          </thead>
          <tbody>
            <tr v-for="row in card.rows" :key="row.item.value">
              <td class="mono">{{ row.item.value }}</td>
              <td class="num">{{ formatCount(row.item.count) }}</td>
              <td class="num">{{ formatValue(row.item.sum) }}</td>
              <td class="num">{{ formatValue(row.item.avg) }}</td>
              <td class="num">{{ formatValue(row.item.max) }}</td>
              <td class="bar-col"><div class="bar" :style="{ width: row.bar }" /></td>
            </tr>
          </tbody>
        </table>
      </section>
    </div>
    <p v-else class="empty">
      Nothing labelled yet: the demo records labels as soon as a tool is called.
    </p>
  </section>
</template>
