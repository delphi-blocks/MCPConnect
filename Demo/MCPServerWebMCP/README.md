# Delphi Day 2027 - a WebMCP demo with WebStencils

A small conference site (speakers, sessions, a personal schedule, tickets, a cart) rendered
server-side with WebStencils. Each page offers the browser's own agent the MCP tools that make
sense *on that page*, through WebMCP (`MCPConnect.WebMCP`). The agent does what a visitor can do
with the mouse, by calling the same Delphi code.

## Run it

Build `DelphiDay.dproj` (Delphi 12 or newer; developed on 13), then:

```
DelphiDay.exe [-port 8080] [-templates <folder>]
```

Open <http://localhost:8080>. The MCP endpoint is `POST /mcp`; `data\delphiday.json` and
`templates\` are read from next to the executable.

WebMCP is a draft browser API and is off by default in Chrome: enable it (the WebMCP flag in
`chrome://flags`, name varies by version) or register for the origin trial, then reload the page.
Without it the site works as a normal site and the script does nothing.

## What it shows

| Page | Tools offered (`webmcp=<group>`) |
|---|---|
| every page | `view_schedule`, `view_cart` (bare `webmcp` tag) |
| `/speakers` | `search_speakers` |
| `/sessions` | `search_sessions`, `get_session`, `add_to_schedule` |
| `/schedule` | `remove_from_schedule` |
| `/tickets` | `list_tickets`, `check_availability`, `add_to_cart` |
| `/checkout` | `remove_from_cart`, `apply_discount`, `place_order` |

* **Per-page groups.** `TMCPWebMCP.View('<group>')` per page; the layout writes `@\WebMCP.Tag`.
* **One logic, two front ends.** The forms and the tools both call `DelphiDay.Service`, so the same
  rules apply: an overlap in the schedule is refused, seats are limited, discount codes are checked.
* **Who is calling.** The WebMCP script calls `/mcp` from the page with same-origin credentials, so
  the visitor's cookie (`dd_visitor`) tells the tools whose schedule and cart to use.
* **The agent cannot buy.** `place_order` only *prepares* the order. The visitor presses
  *Confirm purchase* on `/checkout`. The WebMCP script does not support MRTR input requests yet, so a
  confirmation the agent has to relay would not work in the browser anyway; a button on the page is
  the honest human-in-the-loop step.
* **Rule violations are answers, not protocol errors.** The tools return them as `isError` results
  with a message the agent can act on ("overlaps session 10 ...", "Combo: only 3 seats left").
* **Page follows the agent.** `/webmcp.js` dispatches `mcp:toolresult` on `document` after every tool
  call (and `mcp:toolerror` when a call fails). The layout listens for it and swaps in a fresh copy of the
  page, so the schedule and cart counters change as the agent works, with no polling. The page is
  complete without the script.

## Languages

Each speaker has a `language` in the data (`it` or `en`): Italian speakers get the Italian flag,
the others the UK flag. The flag shows on the speaker cards and next to the speaker of every session,
and the tools report it too. Titles and descriptions are always in English.

## Sample data

`data\delphiday.json` is **sample content** for an edition on 8-9 June 2027: **the whole 2027
program is invented**. It is modelled on the public listing of the 2026 Piacenza edition (9-10 June
2026), whose speaker names, keynote on KAI (Marco Cant&ugrave;), "Delphi & AI" talk (Paolo Rossi),
the two seminars "Delphi Bugs Meet AI" and Claude Code debugging, and talk topics were reused as
raw material. Which speaker gives which session, the times, rooms, levels, ticket types, prices,
seat numbers and the discount codes (`EARLYBIRD` 15%, `MCPCONNECT` 10%) are made up. Replace the
file when the real program is out. No payment is taken, and state is in memory.

## Files

| File | Role |
|---|---|
| `DelphiDay.dpr` | Console host (Indy WebBroker bridge). Lists the MCP units so their registrations are linked |
| `DelphiDay.WebModule.pas` | Routes, forms, cookie, MCP endpoint, WebMCP wiring |
| `DelphiDay.Service.pas` | Business rules and per-visitor state |
| `DelphiDay.Tools.pas` | The `[McpTool]` class |
| `DelphiDay.View.pas` | View models WebStencils reads |
| `DelphiDay.Data.pas` | Catalog loaded from the JSON file |
| `templates\` | Layout, pages, `site.css` (served verbatim, never parsed) |
| `assets\` | The PNG logos and icons, served at `/assets/<name>.png` (the `-white` variants in dark mode) |

This is a demo: forms carry no CSRF token (the cookie is `SameSite=Lax`).
