<script setup lang="ts">
/**
 * The brand block under the header: the illustration, the logo and what the
 * page is - the same block the WebStencils dashboard opens with, its text
 * describing this deployment instead (a browser app polling the endpoint).
 */
import { METRICS_URL } from '@/api/metrics'
</script>

<template>
  <section class="panel hero">
    <img
      class="hero-art"
      src="/assets/hero-mcp-metrics.svg"
      width="960"
      height="480"
      alt="An MCP client calls MCPConnect over JSON-RPC; the server records counters, histograms and gauges and exports them over OTLP to an OpenTelemetry Collector and Prometheus"
    />
    <img class="hero-logo" src="/assets/mcpconnect-rising-emb.png" alt="MCPConnect" />
    <div class="hero-text">
      <h2>MCPConnect <span class="version">2.0.0</span></h2>
      <p class="tagline">
        A Delphi library for building Model Context Protocol servers: attribute-driven tools,
        resources and prompts over JSON-RPC 2.0, the same configuration on WebBroker, Indy and
        stdio, and the metrics on this page built in.
      </p>
      <p class="lede">
        This page is a Vue app in the browser. It asks the server for
        <a :href="METRICS_URL">{{ METRICS_URL }}</a> - plain JSON, no MCP - every few seconds and
        draws what it finds, so the history behind every line is what this tab has seen since it
        was opened. The server needs nothing but the endpoint: no template engine, no second
        process, and in development the Vite proxy keeps it on one origin, with nothing to
        configure for CORS.
      </p>
      <p class="links">
        <a href="https://github.com/delphi-blocks/MCPConnect">Source on GitHub</a>
        <span class="sep">·</span>
        <a href="https://mcpconnect.delphiblocks.dev/">Documentation</a>
        <span class="sep">·</span>
        <a :href="METRICS_URL">The numbers as JSON</a>
      </p>
    </div>
  </section>
</template>
