# Delphi Day 2027 - a WebMCP demo with WebStencils

> This is the **2025-protocol (main branch)** version. The visitor is an MCP *session* carried in a
> cookie. The `feature/mcp-2026-07-28` branch has a stateless version of the same demo.

A small conference site (speakers, sessions, a personal schedule, tickets, a cart) rendered
server-side with WebStencils. Each page offers the browser's own agent the MCP tools that make
sense *on that page*, through WebMCP (`MCPConnect.WebMCP`). The agent does what a visitor can do
with the mouse, by calling the same Delphi code.

## Run it

Build `DelphiDay.dproj` (Delphi 12 or newer, because of WebStencils; developed
on 13), then:

```
DelphiDay.exe [-port 8080] [-templates <folder>]
```

Open <http://localhost:8080>. The MCP endpoint is `POST /mcp`; `data\delphiday.json` and
`templates\` are read from next to the executable.

WebMCP is a draft browser API and is off by default in Chrome: enable it (the WebMCP flag in
`chrome://flags`, name varies by version) or register for the origin trial, then reload the page.
Without it the site works as a normal site and the script does nothing.

## What it shows

| Page | Tools offered |
|---|---|
| every page | `view_schedule`, `view_cart` |
| `/speakers` | `search_speakers` |
| `/sessions` | `search_sessions`, `get_session`, `add_to_schedule` |
| `/schedule` | `remove_from_schedule` |
| `/tickets` | `list_tickets`, `check_availability`, `add_to_cart` |
| `/checkout` | `remove_from_cart`, `apply_discount`, `place_order` (tagged `webmcp=checkout`) |

* **Pages choose their tools.** A bare `webmcp` tag allows a tool in the browser; each page names the
  ones it offers with `TMCPWebMCP.View(['search_sessions', ...], '<page>')`, so the tool declarations
  need not know the pages. The layout writes `@\WebMCP.Tag`.
* **A tool can still keep itself to one page.** `place_order` starts a purchase, so its tag names a
  group (`webmcp=checkout`): only the page rendering that group can offer it, even if another page
  lists it by mistake. The page chooses, the tool limits.
* **One logic, two front ends.** The forms and the tools both call `DelphiDay.Service`, so the same
  rules apply: an overlap in the schedule is refused, seats are limited, discount codes are checked.
* **Who is calling: the session, which also holds the state.** The visitor is an MCP session of the
  typed class `TDelphiDaySession` (`ISessionConfig.SetSessionClass`), which holds the schedule, cart,
  pending order and orders - so they go when the session expires and is freed, one lifetime instead of
  two. The session id travels in the `dd_visitor` cookie, which the server is configured to read
  (`SetLocation(Cookie)`); the WebMCP script calls `/mcp` with same-origin credentials, so the page and
  the tool calls made from it share one session, and a tool receives it through
  `[Context] FSession: TDelphiDaySession` (context data is found by its exact class).
* **A session only when the visitor changes something.** Browsing creates none: every session has a
  thread draining its inbound queue, and a crawler should not cost one per page. The first form POST
  creates it (and sets the cookie); if the agent acts first, the WebMCP script gets a 400 (no session) or
  404 (expired), sends `initialize` + `notifications/initialized`, and retries once - the server creates
  the session and sets the cookie, and the page picks it up on its next refresh. Expired sessions are
  swept by `TMCPSessionManager` (`SweepIntervalSeconds`, default 60 s), not just removed when asked for.
* **The agent cannot buy.** `place_order` only *prepares* the order. The visitor presses
  *Confirm purchase* on `/checkout`. The WebMCP script does not support MRTR input requests yet, so a
  confirmation the agent has to relay would not work in the browser anyway; a button on the page is
  the honest human-in-the-loop step.
* **Rule violations are answers, not protocol errors.** The tools return the reason as the text of the
  result ("overlaps session 10 ...", "Combo: only 3 seats left") with `isError: true`, as MCP prescribes for
  tool execution errors, so the agent can read it and correct itself.
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
| `..\..\Source\MCPConnect.WebMCP.pas` | The WebMCP unit itself (not part of the runtime package: add it to the host project) |
| `DelphiDay.WebModule.pas` | Routes, forms, session cookie, MCP endpoint, WebMCP wiring |
| `DelphiDay.Service.pas` | Business rules and per-visitor state |
| `DelphiDay.Tools.pas` | The `[McpTool]` class |
| `DelphiDay.View.pas` | View models WebStencils reads |
| `DelphiDay.Data.pas` | Catalog loaded from the JSON file |
| `templates\` | Layout, pages, `site.css` (served verbatim, never parsed) |
| `assets\` | The PNG logos and icons, served at `/assets/<name>.png` (the `-white` variants in dark mode) |

This is a demo: forms carry no CSRF token (the cookie is `SameSite=Lax`). The server runs with
`SetCookieSecure(False)` because the transport re-sends the session cookie on every response and marks it
`Secure` by default, which a browser does not return over plain `http`; behind https leave it on.
