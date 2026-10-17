<script setup lang="ts">
/**
 * The slowest histogram series by average - the panel that answers "what is
 * slow right now". Averages only: the subsystem keeps no buckets, so there is no
 * p95 to show.
 */
import { formatCount, formatValue } from '@/lib/format'
import type { MetricSlowEntry } from '@/types'

defineProps<{ slowest: MetricSlowEntry[] }>()
</script>

<template>
  <section class="panel">
    <h2>Slowest histograms</h2>
    <p v-if="slowest.length === 0" class="empty">No histogram recorded yet.</p>
    <div v-else class="table-scroll">
      <table>
        <thead>
          <tr>
            <th class="static">#</th>
            <th class="static">series</th>
            <th class="num static">count</th>
            <th class="num static">avg</th>
            <th class="num static">max</th>
            <th class="num static">min</th>
            <th class="num static">sum</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="(entry, index) in slowest" :key="`${entry.meter}/${entry.name}/${entry.labels}`">
            <td>{{ index + 1 }}</td>
            <td>
              <span class="mono">{{ entry.name }}</span>
              <span class="empty"> {{ entry.labels }}</span>
            </td>
            <td class="num">{{ formatCount(entry.count) }}</td>
            <td class="num">{{ formatValue(entry.avg) }}</td>
            <td class="num">{{ formatValue(entry.max) }}</td>
            <td class="num">{{ formatValue(entry.min) }}</td>
            <td class="num">{{ formatValue(entry.sum) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </section>
</template>
