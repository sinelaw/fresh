# Web UI: Gaps to Desktop-Grade Polish

Purpose: the state of the non-terminal (web) frontend — the unified-scene architecture it is built on, what ships and works today, and a consolidated analysis of the **design gaps** (decisions still to be made) and **implementation gaps** (work items inside the current design) between the current prototype and the polish bar set by desktop-class web editors, VS Code in particular.

This doc replaces the retired `UNIFIED_SCENE_DESIGN.md` and `NON_TERMINAL_UI_RESEARCH.md` (in git history) as the referenced design home for the web UI. The evaluation evidence behind the "verified" claims is `web-ui-rendering-eval.md` at the repo root (2026-07: full build, the 50-assertion Playwright suite, the `scene_parity` test, and custom headless-Chromium probes).

---

## 1. What ships today — IMPLEMENTED

The architecture is described operationally in `web-ui/README.md` and, for the render-pipeline side, in [rendering-and-layout.md](rendering-and-layout.md). The short version:

- **One semantic model, two renderers.** Everything semantic — menu trees with enabled/checked state and accelerators, tabs, status segments, palette, popups, file explorer, trust dialog, plugin widget specs, context menus, the keybinding editor, the full Settings tree — is derived once in the core (`Scene` projections on `Editor`) and consumed by both the TUI cell renderer and the web bridge. A Rust parity test drives one editor through both and fails if they disagree on what the chrome *is*.
- **Buffer interiors are the real pipeline's cells.** The bridge runs the actual `Editor::render` into an in-memory buffer with chrome-cell drawing suppressed (`suppress_chrome_cells`), slices pane interiors (line-number gutter as its own block), and the frontend draws them as SVG text with every glyph pinned to its exact cell column. Layout, wrapping, highlighting, folding, scrollbars and split geometry are all the pipeline's own.
- **Input is real.** DOM key/mouse/wheel events are forwarded to the real `handle_key` / `handle_mouse` / shared hit-dispatch paths at cell coordinates; the browser re-renders from the editor's new state. The frontend holds **no** editor model.
- **The frame pump.** Every request ticks the editor (drains async LSP/plugin/file events, steps animations), and the frontend polls fast (~40 ms) while the scene reports activity, slowly (500 ms) when idle.

Verified working end-to-end (all 50 bundled Playwright assertions plus custom probes): every chrome surface renders as native HTML with zero cell/SVG leakage; splits, drag selection, wide CJK/emoji glyphs, theme switching (native chrome restyles from the live `Theme`, light and dark), and a genuinely capable mobile touch shell (sticky one-shot modifiers, soft-keyboard summoning, breakpoint re-fit).

Strengths to preserve through any of the work below:

- The **parity discipline** — divergence between TUI and web is a test failure, not a bug class. Any new surface must land as a scene projection consumed by both renderers, never as web-only logic.
- **Theme fidelity with taste** — chrome colors seed from the editor theme, but surfaces/hairlines are derived (`color-mix`), so even high-contrast terminal palettes read as a designed UI.
- **DOM hygiene** — unchanged scenes short-circuit before touching the DOM; scroll positions of natively-scrolled panels survive rebuilds; wheel events over natively-scrolled chrome are deliberately not forwarded.
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

### 3.1 Transport and update model

Today: stateless HTTP request/response; every keystroke and every poll returns the **entire scene** (~41 KB at 140×44), every changed frame rebuilds the whole `#app` DOM, and each request is a fresh TCP connection (`Connection: close`). This was the right phase-1 choice — trivially debuggable, no session state, curl-able — and it is measurably fine at one local client (~31 ms/request, debug build).

The gap: idle async events (LSP diagnostics, file watchers) appear up to 500 ms late; typing costs a full serialize→transfer→parse→rebuild cycle per key; multiple clients interleave on one editor by accident rather than by design.

Decision to make: **push channel + incremental updates.** The natural landing point is a WebSocket carrying scene *regions* tagged with a frame sequence number, where the server sends only regions whose serialized form changed (the scene is already region-structured; the frontend already renders region-by-region). Options considered:

- Keep polling, add server-side "unchanged" short-circuit (hash the scene, return 304-equivalent): cheapest, keeps statelessness, fixes idle bandwidth but not latency or event push. Worth doing regardless as a stepping stone.
- SSE for push + POST for input: no bidirectional framing needed, but two channels to keep coherent.
- **WebSocket with region diffs (recommended end state)**: one ordered channel, natural place for input batching and server push; the bridge stays single-threaded per editor by design (the editor is not `Send`), so one socket per editor session is also the honest concurrency model.

### 3.2 Perceived latency vs. the no-mocks principle

The founding rule — the frontend re-implements nothing — means every echo of a keystroke waits on the server round-trip. VS Code (local) is in-process; VS Code remote solves the same problem with *speculative local echo* (type-ahead rendered locally, reconciled when the server catches up), which is a deliberate, bounded re-implementation.

Decision to make: how far to push honest latency before considering speculation. Recommended order: (1) release-build serving, (2) WebSocket transport (removes per-request connection setup), (3) input batching (coalesce burst keystrokes into one editor pass, which the bridge's step loop already supports conceptually). Only if measured p95 echo still exceeds ~50 ms should speculative echo be designed — and then as a clearly-labeled, reconciled overlay (the cursor cell + inserted glyphs only), never as a second text model. Getting this wrong quietly forfeits the architecture's core guarantee.

### 3.3 Typography and metrics model

Today the grid is hardcoded: `CW=8.2 px`, `CH=18 px`, `font-size 13`, one font stack. Click mapping divides `clientX` by `CW`, which silently assumes 1:1 CSS-pixel scale; browser zoom blurs the assumptions rather than breaking them loudly. There is no user font setting and no zoom affordance.

Decision to make: metrics become **negotiated, not constant**. The frontend measures the actual advance/line-height of the user's chosen monospace font (canvas `measureText` at boot and on font/zoom change), derives `CW/CH`, re-fits cols/rows via the existing `/resize`, and uses the measured values everywhere it currently uses the constants. Ligatures are a consequence of the same decision: per-glyph x-pinning (chosen so runs can't drift) is fundamentally ligature-hostile; supporting them means pinning per *run* and accepting measured cluster widths, or explicitly documenting ligatures as out of scope for cell-grid fidelity. Recommended: measured metrics + per-run pinning behind a setting, ligatures deferred.

### 3.4 Buffer rendering medium

Today: one `<svg>` per pane rebuilt on every changed frame, one `<tspan>` per style run with a per-character `x` list, vector rules replacing box-drawing glyphs. Visually excellent (verified crisp at 2× DPR, exact column alignment, no run-boundary drift) and fine at 140×44. It scales linearly with visible cells and rebuild frequency; a 4K fullscreen grid with several splits multiplies both.

Options: keep SVG but patch per row (keyed rows, only re-emit changed ones — the bridge already sends rows as arrays, so row-level dirty detection is a string compare); absolutely-positioned DOM line divs (what VS Code uses — cheap partial updates, native selection possible, but sub-pixel column alignment is harder); canvas/WebGL (xterm.js route — fastest, but forfeits DOM text, accessibility and crisp zoom for free).

Recommended: **SVG with per-row keyed patching** as the next step (smallest delta, keeps every current fidelity property), canvas only if profiling of realistic large-viewport use demands it. Whatever the medium, the switch must not leak into the bridge contract — cells in, pixels out.

### 3.5 Selection and clipboard model

Today selection is entirely editor-side (drag → real selection cells, rendered as background runs — verified). There are **no** `copy`/`paste` DOM handlers: Ctrl+C/Ctrl+V reach the editor as keys, so both operate on the editor-internal clipboard only. Text cannot enter from, or leave to, the OS clipboard; `user-select:none` also forecloses manual browser-native copy.

Decision to make: the clipboard bridge. Inbound is easy and decision-free (a `paste` listener feeding `clipboardData` into the editor — see §4). Outbound needs a design: the editor's copy actions execute server-side, so the browser must learn *what* was copied. Options: a `clipboardText` field on the scene when a copy action ran (frontend writes it to `navigator.clipboard` inside the user-gesture window); or intercepting copy keystrokes frontend-side, asking the bridge for the current selection text, then letting the key proceed. The scene-field approach fits the architecture better (the editor stays the source of truth for what "copy" means, including block selections and multi-cursor). Clipboard permissions and the user-gesture requirement are the constraint to design around, not an afterthought.

### 3.6 Input model beyond keydown

Today desktop input is a `document`-level `keydown` listener forwarding single characters and a named-key allowlist. That cannot express IME composition (CJK, dead keys, voice input — `key` arrives as `"Process"`/`"Unidentified"`), and it competes with browser-reserved shortcuts (Ctrl+W/T/N are uninterceptable in a normal tab). The mobile shell already contains the missing half: a hidden input whose `beforeinput` handler translates `insertText`/`insertLineBreak`/`deleteContentBackward` into editor keys.

Decision to make: promote the hidden-input sink to the universal text path (desktop keeps `keydown` for named/modifier keys, text arrives via `beforeinput`), and decide the **preedit story**: real IME polish means rendering the uncommitted composition inline at the caret, which needs a small new scene affordance (a transient "preedit overlay" the frontend paints at the cursor cell — deliberately *not* an edit to the buffer). Browser-reserved shortcuts: the honest fixes are the Keyboard Lock API (fullscreen/PWA only) and an installable PWA shell; a documented alternate-binding fallback covers the rest. AltGr and autorepeat mostly fall out of `beforeinput` for free.

### 3.7 Deployment and security posture

Today: plain HTTP, localhost, no auth, single thread, one implicit client, README warns accordingly. That is the correct scope for a dev prototype and the code says so honestly.

Decision to make *only if* the web UI graduates: session model first (one editor per client vs. shared editor with presence), then the boring hardening (token auth, TLS or bind-behind-reverse-proxy, origin checks on the future WebSocket, reconnect/resync semantics). The trap to avoid is accreting multi-client behavior onto the current shared-mutable-editor accident. Until that decision is made, the single-client stance should stay loud in the docs and the server should actively reject concurrent websockets rather than interleave them.

### 3.8 Accessibility architecture

Today: effectively none. Buffer text lives in SVG `<tspan>`s (invisible to most screen readers as structured text), chrome divs carry no ARIA roles, and the stylesheet suppresses focus outlines globally.

The semantic-scene architecture is actually an unusual *advantage* here — the chrome is already a typed model (menus with enabled/checked state, rows, dialogs), so emitting `role="menu"/"menuitem"/"dialog"/"tree"` and `aria-*` from the projections is mechanical in a way it never is for pixel-first editors. The buffer needs the VS Code trick: a hidden accessible region mirroring the caret's line/viewport as plain text. Decision to make: whether the accessible-text mirror is a frontend derivation from cells (cheap, approximate) or a first-class scene projection (correct, one more surface under parity discipline). Recommended: scene projection — it's the same single-source principle, and the TUI could use it for its own screen-reader story later.

## 4. Implementation gaps — PLANNED, no design decision required

Work that is well-defined inside the current architecture. Effort: S (hours), M (days), L (week+).

**Input & OS interop**
- `paste` event handler → editor (route each char through the existing key path, or add a bulk-insert action) — closes inbound clipboard. (S)
- Desktop hidden-input sink reusing the mobile `beforeinput` translation (composition *commit* works even before the full preedit design of §3.6; the preedit overlay can follow). (M)
- Forward the browser's click count (`event.detail`) with mouse downs. The editor detects double/triple clicks by its own timing today, which works at LAN latency but is jitter-sensitive; an explicit count is strictly more robust and the bridge already has an extensible mouse JSON. (S)
- Touch pan/scroll on the mobile buffer: there are no `touch*`/`pointer*` handlers, so the buffer cannot be scrolled by swipe at all on phones — the most visible mobile-shell hole. Translate touch pan into the existing wheel forwarding. (S/M)
- File drag-and-drop onto the window → open buffer. (S)

**Rendering & visual polish**
- Keybinding-editor modal: footer hint row clips against the rounded bottom edge. (S)
- Boot-noise status ("Activating direnv environment…" persists until overwritten) — cosmetic, but it is the first thing every screenshot shows; clear transient plugin status after a TTL. (S — likely core-side, not web-side)
- Context-menu / dropdown viewport-edge clamping: verify and clamp native overlays that anchor to cell rects near the right/bottom edges. (S)
- Focus visibility: replace the global `:focus{outline:none}` with visible focus styling on interactive chrome (prerequisite for §3.8, independent of it). (S)
- Scrollbar affordances: hover highlight, drag at pixel granularity mapped back to cell scroll, click-to-page — currently the thumb is drawn but interactions are cell-granular editor hits. (M)
- Hover/popup markdown: LSP hover renders as plain popup lines; the TUI renders markdown with highlighting — port that projection so code blocks in hovers are highlighted runs, and make OSC-8-style links clickable. (M; scene projection may already carry enough structure to start)

**Performance (ordered stepping stones toward §3.1/§3.2)**
- Serve/measure release builds; the published numbers (31 ms `/state`, 36 ms key RTT) are debug-build. (S)
- Server-side scene hash → tiny "unchanged" response for idle polls (client already string-compares; stop shipping 41 KB to learn "nothing changed" twice a second). (S)
- gzip/deflate on responses (scene JSON is highly compressible). (S)
- Per-region DOM patching keyed by region identity, then per-row patching inside panes (§3.4). (M)
- Input batching: coalesce same-tick keystrokes into one `/key` batch → one render. (M)

**Testing & tooling**
- `web-ui/test/package.json` declaring `playwright` (the documented test command currently fails from a clean checkout with `ERR_MODULE_NOT_FOUND`) plus a one-command runner that builds the bridge, starts it, runs `drive.mjs`, and tears down. (S)
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
