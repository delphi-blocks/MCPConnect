# Legacy MCP Compatibility

**Unit**: `Source/MCPConnect.Configuration.Legacy.pas`
**Configuration interface**: `IMCPLegacyConfig`
**Default**: disabled — the plugin costs nothing until it is asked for.

---

## 1. Objective

MCPConnect implements MCP revision **2026-07-28**. This plugin lets a server also answer clients written against the two revisions that precede it — **2025-06-18** and **2025-11-25** — without forking the server or downgrading the build.

It is deliberately *partial* compatibility: it fixes the two things that make a legacy client fail on the very first message, and nothing else.

---

## 2. What the revisions disagree on

| | 2025-06-18 / 2025-11-25 | 2026-07-28 |
|---|---|---|
| Session start | `initialize` request answered with the server identity and capabilities, then a `notifications/initialized` acknowledgement | no handshake at all; both were replaced by `server/discover`, which a client may call or skip |
| Per-request metadata | none | Streamable HTTP request-metadata headers (`MCP-Protocol-Version`, `Mcp-Method`, …) plus a per-request `_meta` naming the protocol version and the client capabilities |

A pre-2026 client therefore opens with an `initialize` that the server answers with *Method Not Found*, and every request it sends afterwards is refused by the default **Strict** header/`_meta` validation.

---

## 3. What enabling the plugin does

1. **Puts the handshake back.** `TMCPLegacyApi` is registered on that server's own API registry — never globally, because a server that does not enable the plugin must keep answering `initialize` with *Method Not Found*. It serves:
   - `initialize` → `TLegacyInitializeResult`
   - `notifications/initialized` → accepted and discarded; there is no session to open, and refusing it is the one thing that would break the client that sent it.
2. **Relaxes both validations to `Lenient`**, through `IMCPConfig.Security.SetHeaderValidation` / `.SetMetaValidation`.

`Lenient` is deliberately not `Off`. What is a property of the *request* rather than of the *revision* is still refused:

- a header that contradicts the body;
- a protocol version the request does state and this server does not speak.

---

## 4. What it does not do

Every result is still shaped as 2026-07-28. Enabling the plugin therefore logs a warning (`SMCPLegacyPartialSupportFmt`) by default; `SetLogWarning(False)` silences it.

These have no equivalent in the older revisions and are sent anyway:

- paginated lists,
- the `_meta` each result carries,
- the input requests of MRTR,
- the caching hints.

And these exist in the older revisions but are **not** brought back by echoing their version in the handshake: the tasks and the icons of 2025-11-25, and the default elicitation mode.

A legacy client ignores what it does not know, and that is the whole of the compatibility on offer.

---

## 5. Enabling it

```pascal
uses
  MCPConnect.Configuration.Legacy;

AServer
  .Plugin.Configure<IMCPLegacyConfig>
    .SetEnabled(True)
  .ApplyConfig
;
```

| Method | Default | Meaning |
|---|---|---|
| `SetEnabled(Boolean)` | `False` | whether the compatibility is applied when this configuration is |
| `SetLogWarning(Boolean)` | `True` | whether enabling it writes the "support is partial" warning |

Calling `ApplyConfig` more than once along a fluent chain is safe: `TMCPLegacyConfig` keeps an `FApplied` flag, because registering the same API class twice raises.

`ApplyToMCPConfig` reaches the MCP configuration through `FApplication.Plugin.Configure<IMCPConfig>` — not `BackToApp`, which *is* `ApplyConfig` and would recurse. That call creates the MCP configuration if the server has none yet, so the legacy plugin can be configured before or after the MCP section.

---

## 6. Why this matters for Claude Code over stdio

Claude Code is, today, a concrete reason to enable this plugin: on the **stdio** transport it speaks one of the legacy revisions unless it is told otherwise.

The behaviour is controlled by the `MCP_PROTOCOL_NEGOTIATION` environment variable (v2 MCP client runtime only, Claude Code v2.1.221 or later):

| Value | Which servers are probed for 2026-07-28 |
|---|---|
| unset | HTTP and claude.ai connector servers, on v2.1.232 or later — **stdio servers are not probed** |
| `auto` | HTTP, claude.ai connector **and stdio** servers |
| `legacy` | none; the probe is skipped for every server |

Any other value is ignored, with a warning in the debug log. A server that is probed but does not answer the probe connects on the earlier protocol anyway, as SSE and WebSocket servers always do.

So an MCPConnect server launched by Claude Code over `MCPConnect.Transport.Stdio` will, by default, be addressed with the pre-2026 handshake — an `initialize` request this build answers with *Method Not Found* unless the legacy plugin is enabled. Two ways out, and they are not exclusive:

- enable `IMCPLegacyConfig` on the server, as in section 5 — the server then answers both kinds of client, whatever the host is configured to do;
- set `MCP_PROTOCOL_NEGOTIATION=auto` in the environment Claude Code runs the server in, so that stdio is probed for 2026-07-28 too.

One caveat on `auto`: a server that declares the `claude/channel` capability and then negotiates 2026-07-28 cannot deliver channel messages, so Claude Code does not register it as a channel at all. A server that relies on channels to push messages into the session should therefore stay on the earlier handshake — leave the variable unset or set it to `legacy` — and enable this plugin to answer it.

References: [environment variables](https://code.claude.com/docs/en/env-vars), [MCP client runtimes](https://code.claude.com/docs/en/mcp#mcp-client-runtimes).
