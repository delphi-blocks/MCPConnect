<script setup lang="ts">
/**
 * The document exactly as the endpoint sent it. The panels above are views of
 * this; when a number looks wrong, this is where to see whether the server said
 * it or the dashboard computed it.
 */
import { computed, ref } from 'vue'

import type { MetricsDocument } from '@/types'

const props = defineProps<{ document: MetricsDocument }>()

const open = ref(false)
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
    <h2>Raw document</h2>
    <div class="toolbar">
      <button @click="open = !open">{{ open ? 'Hide' : 'Show' }} JSON</button>
      <button :disabled="!open" @click="copy">{{ copied ? 'Copied' : 'Copy' }}</button>
      <span class="spacer" />
      <span class="empty">
        {{ document.schema }} · {{ document.cumulative.length }} cumulative ·
        {{ document.live.length }} live points
      </span>
    </div>
    <pre v-if="open" class="raw">{{ text }}</pre>
  </section>
</template>
