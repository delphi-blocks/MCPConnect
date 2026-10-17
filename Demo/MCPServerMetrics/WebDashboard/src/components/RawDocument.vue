<script setup lang="ts">
/**
 * What the server said, without the page: the document exactly as the endpoint
 * sent it, and the fold note that explains its counters. The panels above are
 * views of this; when a number looks wrong, this is where to see whether the
 * server said it or the dashboard computed it.
 */
import { computed, ref } from 'vue'

import { METRICS_URL } from '@/api/metrics'
import type { MetricsDocument } from '@/types'

const props = defineProps<{ document: MetricsDocument }>()

const copied = ref(false)

const text = computed(() => JSON.stringify(props.document, null, 2))

async function copy(): Promise<void> {
  try {
    await navigator.clipboard.writeText(text.value)
    copied.value = true
    setTimeout(() => (copied.value = false), 1500)
  } catch {
    copied.value = false
  }
}
</script>

<template>
  <section class="panel">
    <h2>What the server says about itself</h2>
    <div class="note">
      Everything above is drawn from one document. It is here as it arrived, for when a number
      needs checking against its source.
    </div>

    <details v-if="document.service.description">
      <summary>About this server</summary>
      <pre class="raw">{{ document.service.name }} {{ document.service.version }}
{{ document.service.description }}</pre>
    </details>

    <details>
      <summary>How the counters stay cumulative</summary>
      <pre class="raw">{{ document.fold.note }}

resets folded: {{ document.fold.resetsDetected }}</pre>
    </details>

    <details>
      <summary>
        The raw document ({{ document.schema }} · {{ document.cumulative.length }} cumulative ·
        {{ document.live.length }} live points)
      </summary>
      <div class="toolbar">
        <button class="badge" @click="copy">{{ copied ? 'Copied' : 'Copy JSON' }}</button>
        <a class="badge" :href="METRICS_URL" target="_blank" rel="noopener">Open {{ METRICS_URL }}</a>
      </div>
      <pre class="raw">{{ text }}</pre>
    </details>
  </section>
</template>
