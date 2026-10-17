import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// The dashboard is a plain HTTP client of the server's /metrics endpoint, so in
// development it proxies to the server and the browser stays on one origin -
// nothing has to be configured for CORS. Point it somewhere else with
// DASHBOARD_METRICS_URL (or serve the built app from the same origin as the
// server, where a relative /metrics resolves on its own).
const target = process.env.DASHBOARD_METRICS_URL ?? 'http://localhost:8080'

export default defineConfig({
  plugins: [vue()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  server: {
    port: 5173,
    proxy: {
      '/metrics': {
        target,
        changeOrigin: true,
      },
    },
  },
})
