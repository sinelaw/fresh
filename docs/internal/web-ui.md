# Web UI: Gaps to Desktop-Grade Polish

Purpose: the state of the non-terminal (web) frontend — the unified-scene architecture it is built on, what ships and works today, and a consolidated analysis of the **design gaps** (decisions still to be made) and **implementation gaps** (work items inside the current design) between the current prototype and the polish bar set by desktop-class web editors, VS Code in particular.

This doc replaces the retired `UNIFIED_SCENE_DESIGN.md` and `NON_TERMINAL_UI_RESEARCH.md` (in git history) as the referenced design home for the web UI. The evaluation evidence behind the "verified" claims is `web-ui-rendering-eval.md` at the repo root (2026-07: full build, the 50-assertion Playwright suite, the `scene_parity` test, and custom headless-Chromium probes).

---

## 1. What ships today — IMPLEMENTED

The architecture is described operationally in `web-ui/README.md` and, for the render-pipeline side, in [rendering-and-layout.md](rendering-and-layout.md). The short version:

- **One semantic model, two renderers.** Everything semantic — menu trees with enabled/checked state and accelerators, tabs, status segments, palette, popups, file explorer, trust dialog, plugin widget specs, context menus, the keybinding editor, the full Settings tree — is derived once in the core (`Scene` projections on `Editor`) and consumed by both the TUI cell renderer and the web bridge. A Rust parity test drives one editor through both and fails if they disagree on what the chrome *is*.
- **Buffer interiors are the real pipeline's cells.** The bridge runs the actual `Editor::render` into an in-memory buffer with chrome-cell drawing suppressed (`suppress_chrome_cells`), slices pane interiors (line-number gutter as its own block), and the frontend draws them as SVG text with every glyph pinned to its exact cell column. Layout, wrapping, highlighting, folding, scrollbars and split geometry are all the pipeline's own.
- **Input is real.** DOM key/mouse/wheel events are forwarded to the real `handle_key` / `handle_mouse` / shared hit-dispatch paths at cell coordinates; the browser re-renders from the editor's new state. The frontend holds **no** editor model.
- **The frame pump is server push.** One WebSocket (`/ws`) carries everything: the server's event loop ticks the editor (drains async LSP/plugin/file events, steps animations) at ~40 ms while the scene reports activity and ~250 ms when idle, and **pushes** a region-diff frame only when the scene actually changed (§3.1). All input rides the same socket; the HTTP routes stay alive for curl and the parity harness.

Verified working end-to-end (all 57 bundled Playwright assertions — now exercising the WebSocket input path and the push/diff/single-client behavior of §3.1 — plus custom probes): every chrome surface renders as native HTML with zero cell/SVG leakage; splits, drag selection, wide CJK/emoji glyphs, theme switching (native chrome restyles from the live `Theme`, light and dark), and a genuinely capable mobile touch shell (sticky one-shot modifiers, soft-keyboard summoning, breakpoint re-fit).

Strengths to preserve through any of the work below:

- The **parity discipline** — divergence between TUI and web is a test failure, not a bug class. Any new surface must land as a scene projection consumed by both renderers, never as web-only logic.
- **Theme fidelity with taste** — chrome colors seed from the editor theme, but surfaces/hairlines are derived (`color-mix`), so even high-contrast terminal palettes read as a designed UI.
- **DOM hygiene** — when nothing changed, no frame arrives at all (the server diffs before pushing, §3.1), so the DOM is never touched; scroll positions of natively-scrolled panels survive rebuilds; wheel events over natively-scrolled chrome are deliberately not forwarded.
- The **gutter/text split** in the bridge output, which exists precisely so a future native selection/copy layer can cover code without line numbers.

## 2. The benchmark — what "VS Code-level polish" decomposes into

"Polish" is not one feature. Measured against VS Code in a browser (vscode.dev / code-server), the bar decomposes into:

1. **Complete text input**: IME composition with inline preedit, dead keys, AltGr, autorepeat, and no keystrokes eaten by the browser.
2. **OS integration**: system clipboard both directions, drag-and-drop files, installable app shell, window title, file-association-ish deep links.
3. **Instant feel**: sub-frame perceived latency for typing and pointer interactions; no visible re-layout; 60 fps scrolling.
4. **Typographic control**: user font family/size, browser-zoom and HiDPI correctness, optional ligatures.
5. **Rich surfaces**: markdown hover cards with highlighted code blocks and working links, smooth-scrolling lists, pixel-granular scrollbars with hover/drag affordances.
6. **Accessibility**: screen-reader access to buffer and chrome, focus visibility, keyboard-only completeness.
7. **Deployment**: survives being more than one localhost tab — authentication, TLS, reconnect, more than one concurrent client.

Two things VS Code has that are **editor-core feature gaps, not web-frontend gaps**, and therefore out of scope here: the minimap and sticky scroll (neither exists in the TUI; the web renders what the pipeline renders). Pixel-smooth *sub-cell* scrolling is likewise bounded by the cell model itself — the web can only scroll in whole visual rows for as long as the pipeline thinks in cells. These belong to core rendering discussions, not this doc.

## 3. Design gaps — PLANNED, decisions needed before code

These are the places where reaching the bar requires choosing an architecture, not just writing more of the current kind of code. Each lists the tension and a recommended direction.

### 3.1 Transport and update model — IMPLEMENTED (WebSocket push with region diffs)

The recommended end state is now the shipped transport. The bridge (`crates/fresh-editor/src/webui/mod.rs`) runs one nonblocking event loop on the editor's thread (the editor is not `Send`; nothing moved off-thread):

- **`GET /ws`** upgrades to a hand-rolled RFC 6455 WebSocket (matching the bridge's hand-rolled HTTP; `sha1` + `base64` for the handshake, 7/16/64-bit frame lengths, masked client frames, ping/pong, close echo, defensive continuation handling).
- **On connect**: `{"type":"hello","seq":0,"scene":<full scene>}` — the same object `GET /state` returns, clipboard field included. A reconnect (or server restart) gets a fresh hello; `seq` restarts.
- **On change**: `{"type":"frame","seq":N,"changed":{<path>:<value>,…}}` where paths are `"w"`, `"h"`, `"theme"`, `"clipboard"`, and `"regions.<key>"` — except **panes**, which diff one level deeper (`"regions.panes.<i>"` plus `"regions.panes.len"` when the count changes), since panes carry the bulk of the bytes: typing resends only the changed pane. A changed value replaces the old one wholesale (null is legal — regions are frequently null). Change detection compares each unit's serialized JSON against the last-sent copy, cached per session; the cache is written only by the WS push path, so HTTP `/state` calls can't confuse it.
- **Input** (client→server) is JSON text frames with the exact field shapes of the old HTTP POST bodies, tagged `"type":"key"|"mouse"|"action"|"widget"|"settings"|"kbedit"|"paste"|"resize"`. Both transports run through one shared dispatch (`apply_message`), so they cannot drift. Inbound frames are drained as a batch and applied in order with **one** render per batch, not per message — the input batching §3.2 asked for fell out of the transport.
- **Pacing**: the editor ticks at ~40 ms while the scene's `poll.active` hint is true, ~250 ms when idle, client or no client (async LSP/plugin/file events never stall). The scene build+diff is additionally gated on `editor_tick`'s own needs-render signal, so a connected-but-idle session measures ~1.6 % CPU (debug build; ~0.7 % with no client).
- **Session model**: exactly one WebSocket client. A second upgrade gets a plain HTTP `409 Conflict` before any upgrade; an `Origin` whose host differs from the bind host gets `403 Forbidden` (non-browser tools send no Origin and pass). Multi-session and real auth remain §3.7.
- **HTTP stays**: every route above still answers exactly as before (full-scene responses, `Connection: close`) — curl-ability and the parity harness (`/step`, `/reset`) are untouched. A mutation made over HTTP is pushed to the connected browser as a diff in the same loop pass.
- **Frontend**: the poll loop is gone. The client applies hello/frames into its scene object, renders once per frame, and reconnects with backoff (500 ms doubling to 5 s, non-modal banner); input while disconnected is dropped, never queued — the hello resync is the recovery story.

Measured (debug build, 140×44, loopback): key→frame RTT over WS min 51.8 / avg ~60 / max 77.8 ms vs the same box's HTTP `/key` avg 64.1 / max 142 ms — the transport overhead over a bare scene build (`/state` ≈ 51–58 ms here) is now a few ms, and the tail is much tighter; the remaining latency is the debug-build render itself (release serving is still the §4 item). Idle: zero frames pushed when nothing changes.

Still PLANNED from the original option list: gzip/deflate (less urgent now that idle traffic is zero and typing sends per-pane diffs), and the multi-client / auth posture of §3.7.

### 3.2 Perceived latency vs. the no-mocks principle

The founding rule — the frontend re-implements nothing — means every echo of a keystroke waits on the server round-trip. VS Code (local) is in-process; VS Code remote solves the same problem with *speculative local echo* (type-ahead rendered locally, reconciled when the server catches up), which is a deliberate, bounded re-implementation.

Decision to make: how far to push honest latency before considering speculation. Recommended order: (1) release-build serving, (2) ~~WebSocket transport~~ **IMPLEMENTED** (§3.1 — removes per-request connection setup), (3) ~~input batching~~ **IMPLEMENTED** (the WS loop drains all pending input as one batch → one editor pass → one render). Only if measured p95 echo still exceeds ~50 ms should speculative echo be designed — and then as a clearly-labeled, reconciled overlay (the cursor cell + inserted glyphs only), never as a second text model. Getting this wrong quietly forfeits the architecture's core guarantee.

### 3.3 Typography and metrics model

Today the grid is hardcoded: `CW=8.2 px`, `CH=18 px`, `font-size 13`, one font stack. Click mapping divides `clientX` by `CW`, which silently assumes 1:1 CSS-pixel scale; browser zoom blurs the assumptions rather than breaking them loudly. There is no user font setting and no zoom affordance.

Decision to make: metrics become **negotiated, not constant**. The frontend measures the actual advance/line-height of the user's chosen monospace font (canvas `measureText` at boot and on font/zoom change), derives `CW/CH`, re-fits cols/rows via the existing `/resize`, and uses the measured values everywhere it currently uses the constants. Ligatures are a consequence of the same decision: per-glyph x-pinning (chosen so runs can't drift) is fundamentally ligature-hostile; supporting them means pinning per *run* and accepting measured cluster widths, or explicitly documenting ligatures as out of scope for cell-grid fidelity. Recommended: measured metrics + per-run pinning behind a setting, ligatures deferred.

### 3.4 Buffer rendering medium

Today: one `<svg>` per pane rebuilt on every changed frame, one `<tspan>` per style run with a per-character `x` list, vector rules replacing box-drawing glyphs. Visually excellent (verified crisp at 2× DPR, exact column alignment, no run-boundary drift) and fine at 140×44. It scales linearly with visible cells and rebuild frequency; a 4K fullscreen grid with several splits multiplies both.

Options: keep SVG but patch per row (keyed rows, only re-emit changed ones — the bridge already sends rows as arrays, so row-level dirty detection is a string compare); absolutely-positioned DOM line divs (what VS Code uses — cheap partial updates, native selection possible, but sub-pixel column alignment is harder); canvas/WebGL (xterm.js route — fastest, but forfeits DOM text, accessibility and crisp zoom for free).

Recommended: **SVG with per-row keyed patching** as the next step (smallest delta, keeps every current fidelity property), canvas only if profiling of realistic large-viewport use demands it. Whatever the medium, the switch must not leak into the bridge contract — cells in, pixels out.

### 3.5 Selection and clipboard model

Selection is entirely editor-side (drag → real selection cells, rendered as background runs — verified). `user-select:none` forecloses manual browser-native copy; the clipboard bridge below is the copy/paste story.

**Inbound (paste) — IMPLEMENTED.** A document `paste` listener reads `clipboardData` and delivers the whole text in one `POST /paste`; the bridge runs the editor's real bracketed-paste path (`paste_bracketed_into_focused_panel` → `paste_text`, the same routing the terminal's `Ev::Paste` takes), so focused panels, prompts, terminals and column-mode paste behave exactly as in the TUI. The bare Ctrl/Cmd+V keystroke is deliberately **not** forwarded (and not `preventDefault`-ed): the DOM paste event that follows is the one paste path, mirroring how the terminal frontend receives a bracketed paste rather than a Ctrl+V key. Long pastes never loop through per-char `/key` posts.

**Outbound (copy/cut) — IMPLEMENTED** via the scene-field approach (the editor stays the source of truth for what "copy" means, including block selections and multi-cursor): browser-facing scenes carry `"clipboard": {"seq": N, "text": "..."}`, where `seq` increments whenever the editor's internal clipboard text changed — the bridge tracks a hash per server (`ClipboardSync` in `webui/mod.rs`), so no new core-editor state; the core only gained the read-only `Editor::clipboard_text()` accessor. The frontend calls `navigator.clipboard.writeText` when it sees a new `seq`; the write happens in the async response to the very Ctrl+C / menu click that ran the copy, i.e. inside that gesture's transient-activation window. The exposed text is capped at 1 MiB and never logged; a rejected write (no permission, non-secure origin) degrades silently to editor-internal copy/paste. Still PLANNED: a dedicated fetch for over-cap copies, and rich-text (`copy_html`) pass-through.

### 3.6 Input model beyond keydown

**Hidden-input text sink — IMPLEMENTED as the universal text path.** The mobile shell's hidden input is now created and kept focused on desktop too (`focus({preventScroll:true})`; refocused on non-chrome clicks and window focus; visually hidden but never `display:none`, which would break IME). Desktop keeps `keydown` for plain printable keys, shortcuts and the named-key allowlist — that handler `preventDefault`s, which suppresses the matching `beforeinput`, so there is no double delivery and the mobile-only `kdHandledAt` de-dupe window never needs to engage on desktop. Text the keydown path can't express arrives through the sink: composition-less `insertText` via the existing `beforeinput` translation, and IME composition (CJK candidate commit, dead-key accents — `key` arrives as `"Process"`/`"Dead"`) via `compositionend`, with `isComposing` guards on keydown/beforeinput/input so the IME owns the sink until commit.

Still PLANNED — the **preedit story**: the uncommitted composition currently lives invisibly in the hidden input; real IME polish means rendering it inline at the caret, which needs a small new scene affordance (a transient "preedit overlay" the frontend paints at the cursor cell — deliberately *not* an edit to the buffer). Also still open: browser-reserved shortcuts (Ctrl+W/T/N are uninterceptable in a normal tab — the honest fixes are the Keyboard Lock API (fullscreen/PWA only), an installable PWA shell, and a documented alternate-binding fallback). AltGr and autorepeat fall out of the implemented paths for free.

### 3.7 Deployment and security posture

Today: plain HTTP, localhost, no auth, single thread, one implicit client, README warns accordingly. That is the correct scope for a dev prototype and the code says so honestly.

Decision to make *only if* the web UI graduates: session model first (one editor per client vs. shared editor with presence), then the boring hardening (token auth, TLS or bind-behind-reverse-proxy, reconnect/resync semantics beyond the implemented full-scene hello). The trap to avoid is accreting multi-client behavior onto a shared-mutable-editor accident. The §3.1 transport already takes the two defensive steps this section asked for while the decision is pending: concurrent WebSockets are actively rejected (`409 Conflict` — never interleaved), and upgrades from a foreign `Origin` host get `403 Forbidden`. Everything else here stays PLANNED, and the single-client stance stays loud in the docs.

### 3.8 Accessibility architecture

Today: effectively none. Buffer text lives in SVG `<tspan>`s (invisible to most screen readers as structured text), chrome divs carry no ARIA roles, and the stylesheet suppresses focus outlines globally.

The semantic-scene architecture is actually an unusual *advantage* here — the chrome is already a typed model (menus with enabled/checked state, rows, dialogs), so emitting `role="menu"/"menuitem"/"dialog"/"tree"` and `aria-*` from the projections is mechanical in a way it never is for pixel-first editors. The buffer needs the VS Code trick: a hidden accessible region mirroring the caret's line/viewport as plain text. Decision to make: whether the accessible-text mirror is a frontend derivation from cells (cheap, approximate) or a first-class scene projection (correct, one more surface under parity discipline). Recommended: scene projection — it's the same single-source principle, and the TUI could use it for its own screen-reader story later.

## 4. Implementation gaps — PLANNED, no design decision required

Work that is well-defined inside the current architecture. Effort: S (hours), M (days), L (week+).

**Input & OS interop**
- ~~`paste` event handler → editor~~ **IMPLEMENTED:** document `paste` listener → single `POST /paste` → the editor's bracketed-paste path (see §3.5; no per-char key loops, and the bare Ctrl+V key is not forwarded).
- ~~Desktop hidden-input sink~~ **IMPLEMENTED:** the mobile sink is the desktop text path too; composition *commit* works (`beforeinput` + `compositionend`), the preedit overlay of §3.6 remains PLANNED.
- ~~Forward the browser's click count~~ **IMPLEMENTED:** mouse downs carry `count` (`event.detail`); when `count ≥ 2` the bridge primes the editor's own click-tracking state (`previous_click_time/position`, `click_count`) so `detect_multi_click` resolves the browser's count deterministically — the editor's word/line-selection path itself is untouched, and no timing/cell-slop mismatch across the HTTP hop can drop a double-click.
- Touch pan/scroll on the mobile buffer: there are no `touch*`/`pointer*` handlers, so the buffer cannot be scrolled by swipe at all on phones — the most visible mobile-shell hole. Translate touch pan into the existing wheel forwarding. (S/M)
- File drag-and-drop onto the window → open buffer. (S)

**Rendering & visual polish**
- Keybinding-editor modal: footer hint row clips against the rounded bottom edge. (S)
- Boot-noise status ("Activating direnv environment…" persists until overwritten) — cosmetic, but it is the first thing every screenshot shows; clear transient plugin status after a TTL. (S — likely core-side, not web-side)
- Context-menu / dropdown viewport-edge clamping: verify and clamp native overlays that anchor to cell rects near the right/bottom edges. (S)
- Focus visibility: replace the global `:focus{outline:none}` with visible focus styling on interactive chrome (prerequisite for §3.8, independent of it). (S)
- Scrollbar affordances: hover highlight, drag at pixel granularity mapped back to cell scroll, click-to-page — currently the thumb is drawn but interactions are cell-granular editor hits. (M)
- Hover/popup markdown: LSP hover renders as plain popup lines; the TUI renders markdown with highlighting — port that projection so code blocks in hovers are highlighted runs, and make OSC-8-style links clickable. (M; scene projection may already carry enough structure to start)

**Performance (stepping stones toward §3.2; §3.1's transport is done)**
- Serve/measure release builds; all published numbers (≈55 ms scene build, ≈60 ms WS key→frame RTT) are debug-build, and the build now dominates the RTT. (S)
- ~~Server-side scene hash → tiny "unchanged" response for idle polls~~ **subsumed by §3.1:** the server diffs per region against the last-sent copy and pushes nothing when nothing changed — idle traffic is zero, typing sends only the changed pane.
- gzip/deflate (now: WS `permessage-deflate`) — less urgent post-§3.1, still worthwhile for big frames like theme switches. (S)
- Per-region DOM patching keyed by region identity, then per-row patching inside panes (§3.4): the frames already arrive as per-region diffs (`window.fresh.lastFrameKeys`), but `render()` still rebuilds the whole `#app` — the frontend half of this item is the remaining work. (M)
- ~~Input batching: coalesce same-tick keystrokes into one `/key` batch → one render.~~ **subsumed by §3.1:** the WS loop drains all pending input per pass and renders once per batch.

**Testing & tooling**
- ~~`web-ui/test/package.json` declaring `playwright` (the documented test command currently fails from a clean checkout with `ERR_MODULE_NOT_FOUND`) plus a one-command runner that builds the bridge, starts it, runs `drive.mjs`, and tears down.~~ **Done:** `web-ui/test/package.json` pins `playwright`, and `web-ui/test/run.sh` builds the bridge, installs deps, starts/polls the server, runs the suite, and tears down (CI: `.github/workflows/web-ui.yml`).
- CI job running the Playwright suite + `scene_parity` on PRs touching `web-ui/` or the view/webui crates. (M — see [testing.md](testing.md) for where it slots in)
- Screenshot-based visual regression on the captured surfaces (the suite already writes deterministic screenshots; a pixel-diff gate is the missing 10%). (M)
- Cross-browser matrix: the suite is Chromium-only; `color-mix` and per-glyph SVG need at least a Firefox and WebKit smoke pass. (S/M)

**Docs**
- The pointers to the retired design docs (`UNIFIED_SCENE_DESIGN.md`, `NON_TERMINAL_UI_RESEARCH.md`) in `web-ui/README.md` and source comments, and the wrong example path in `drive.mjs`'s header, are fixed alongside this doc's introduction; keep them pointing here.

## 5. Suggested phasing

- **Phase A — input correctness & interop** (all S/M, no design risk): clipboard paste, desktop text sink, click-count forwarding, touch pan, test packaging + CI. After this, the web UI is *usable* as a daily driver for ASCII-and-IME text alike.
- **Phase B — feel**: release serving, unchanged-scene short-circuit, gzip, then the WebSocket + region-diff transport (§3.1), per-region/row patching (§3.4), measured font metrics + zoom (§3.3). After this, it should be indistinguishable from native at local latencies.
- **Phase C — reach**: outbound clipboard design (§3.5), preedit overlay (§3.6), accessibility projection + ARIA (§3.8), PWA shell + Keyboard Lock, and — only with an explicit decision — the remote deployment posture (§3.7).

Each phase keeps the invariant that makes this frontend worth having: the editor remains the single source of truth, and anything semantic added for the web lands as a scene projection under parity test, never as web-only logic.
