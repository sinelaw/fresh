# Fresh — Non-Terminal UI: Research & Design Options

**Status:** Research only. No implementation. This document surveys the options for giving
Fresh a modern, non-terminal UI while keeping a single shared core, and recommends a
phased direction with trade-offs.

**Date:** 2026-06 · **Branch:** `claude/non-terminal-ui-research-fir1y9`

> Method note: this is a synthesis of a large fan-out of source-cited research plus a
> read of Fresh's own code. Inline links point at primary sources (crate repos, editor
> design docs, retrospectives). Where a fact is version-sensitive (Rust GUI crates move
> fast and are nearly all pre-1.0), the version/date is given.

---

## 0. TL;DR / recommendation

1. **Fresh's entire view layer is a cell grid (ratatui), including its own widgets.** The
   existing "GUI" (`fresh-gui`) just renders that same grid in a GPU window via
   `ratatui-wgpu`. A genuinely *modern, non-terminal* look is therefore **not** a backend
   swap at the ratatui level — it requires deciding **where to cut a new UI seam**.

2. **Do not run the editor "in a browser" as the primary target, and do not use the
   Ratzilla path the WASM scaffold currently points at.** Ratzilla renders a terminal cell
   grid in the browser — it is "terminal-in-a-browser", which fails the modern-look goal.
   A pure browser sandbox also cripples Fresh's differentiators (multi-GB files, LSP
   subprocesses, QuickJS plugins, real local files). If a web/desktop-webview product is
   wanted, target a **webview shell (Tauri/wry) with a DOM editor view**, treating
   pure-browser as a feature-limited subset.

3. **For a native modern GUI, the realistic architecture across *every* toolkit surveyed
   is the same: use a toolkit/native chrome only for the shell (menus, dialogs, panels,
   tabs), and render the text editing surface yourself as a custom GPU widget on top of
   Fresh's existing piece-tree + visual-line virtualization.** No Rust GUI toolkit gives
   you a multi-GB editor for free; Zed (gpui) and Lapce (floem) both pair the toolkit with
   their own rope + virtualization + incremental highlighting — which Fresh already has.

4. **The keystone is the seam, not the toolkit.** The strongest precedent is Neovim's
   `ext_*` model: a **universal cell-grid fallback with opt-in *semantic* externalization**
   of well-known UI components (menus, command line, messages, tab line, per-window grids).
   xi-editor's retrospective is the cautionary counterpart: its serialized async
   front-end protocol was, by the author's own account, a mistake — **keep the seam
   in-process; never put serialization/async on the input-to-pixel hot path.**

5. **Recommended phasing (low-risk → modern):**
   - **Phase 0 (done):** `fresh-gui` GPU window of the cell grid.
   - **Phase 1:** Introduce a backend-agnostic **`RenderCommand` seam** *above* the cell
     grid but *below* today's `render.rs`, plus a native-chrome layer (`muda` menus +
     `rfd` dialogs — both already partly in use). The terminal frontend keeps working
     unchanged (it just lowers `RenderCommand`s back to cells).
   - **Phase 2:** Add a GPU frontend that interprets the richer commands — proportional
     fonts and real widgets for *chrome* first (menus/dialogs/palette/status), cell-grid
     text body second.
   - **Phase 3:** Replace the text-body renderer with a true GPU text surface
     (cosmic-text/glyphon **or** parley/Vello) — the big, optional payoff.
   - Browser/webview is an independent track that reuses the same seam.

6. **Budget for the two hidden costs up front:** **IME** and **accessibility**. A
   custom-rendered text surface inherits neither for free. Adopt **AccessKit** (via
   `accesskit_winit`) from the start and design the seam so the core can emit the
   per-`TextRun` data AccessKit needs; wire **winit IME** (`set_ime_allowed` +
   `set_ime_cursor_area` + `Ime::Preedit/Commit`) into the text widget deliberately.

---

## 1. High-level goals

Any solution must satisfy:

1. **One core, swappable UI.** `model/`, `primitives/`, editing, LSP, plugins (QuickJS),
   config/themes stay untouched. The UI implementation is swappable and experiment-able.
2. **Modern non-terminal look** — proportional fonts, smooth scrolling, real widgets,
   native-feeling dialogs — beyond the fixed cell grid.
3. **One coherent input/event story across backends** — keyboard, mouse, focus,
   event routing/"bubbling", IME/CJK, accessibility — defined once at the seam.
4. **Resolve the "WASM" question** — is WASM a *browser frontend* or just *core-in-WASM*?
   And if browser, how does the JS/DOM ↔ Rust-core bridge work?
5. **Preserve Fresh's performance** (multi-GB files, low input latency) and its
   plugin/theme ecosystem.

---

## 2. Current architecture (what we're starting from)

From a read of the codebase (`crates/fresh-editor`, `crates/fresh-gui`, `crates/fresh-core`):

- **Workspace:** `fresh-core` (shared types incl. `Menu`/`MenuContext`) → `fresh-editor`
  (model + primitives + **view**) → frontends. Plugins run in QuickJS (`rquickjs`).
- **The view layer is 100% ratatui cell-grid.** `Editor::render(&mut Frame)`
  (`app/render.rs`, ~4.8k lines) builds a `ratatui::layout::Layout`
  (`[menu_bar, main_content, status_bar, search_options, prompt_line]`), then renders the
  menu, splits/buffers (`view/ui/split_rendering/`), file explorer, status bar, prompt,
  and popups by writing styled chars into a `ratatui::Frame`. Rendering is **immediate-mode**
  (full redraw ~60fps); ratatui itself diffs the cell buffer.
- **Custom widgets are cell-grid too.** `view/controls/{button,dropdown,toggle,text_input,
  text_list,number_input,map_input,dual_list,keybinding_list}` each follow `State + Colors +
  render_*() -> hit Rect + input handler`. Hit areas are **computed fresh each frame**
  (no retained tree). Popups in `view/popup/`.
- **Input** is **crossterm** `KeyEvent`/`MouseEvent`. Dispatch
  (`app/input.rs`, `input_dispatch.rs`) walks modal overlays (Settings, KeybindingEditor,
  Menu, Prompt, Popup, Terminal) then falls through to keybinding resolution by context.
  Mouse coordinates are **(col,row) cells**; clicks are hit-tested against per-frame areas.
- **Focus** (`app/active_focus.rs`, `view/ui/focus.rs::FocusManager`) is a **global state
  machine with modal layering** (`overlay_layers()`, `blocks_input`), **not** event
  capture/bubbling.
- **Theme colors** are `ratatui::Color` (named 16 / `Indexed(u8)` / `Rgb(u8,u8,u8)`) — they
  map cleanly to true-color, so a GUI loses no fidelity.
- **Three frontends already exist or are scaffolded:**
  1. **Terminal** — `CrosstermBackend<Stdout>` (the real product).
  2. **`fresh-gui`** — native window via **winit + wgpu + `ratatui-wgpu`**, with native macOS
     menus via **`muda`**, defining a clean **`GuiApplication` trait**
     (`on_key`/`on_mouse`/`render(&mut Frame)`/`tick`/menu hooks/`take_color_update`) that
     translates winit events → crossterm. *It still renders the cell grid in a GPU window.*
  3. **`wasm/`** — a thin `WasmEditor` wrapper around `Buffer` plus feature-gated WASM-compatible
     view modules; comments point at **Ratzilla** rendering + an **IndexedDB** filesystem
     (both, per §4, are the *weaker* choices).
- **A client/server daemon** exists (`server/`) but it relays a **raw terminal byte
  stream** over IPC (Unix sockets / Windows named pipes) — a dumb cell relay, **not** a
  semantic protocol.

**The "waist" assessment.** All high-level logic is already decoupled. The cell-grid
assumption is **pervasive but localized to the render layer** (`app/render.rs`,
`view/ui/split_rendering/`, the controls' `render_*`). The cleanest existing seams are the
**`GuiApplication` trait** and ratatui's **`Backend` trait**; the `wasm` feature gate; the
reusable `FocusManager`; and the hit-area-returning widgets. The two things most coupled to
"cell grid": **line wrapping at `terminal.width`** and **mouse hit-testing in (col,row)**.

---

## 3. The three UI directions

### Direction A — Browser / WASM frontend

The disambiguation that matters most: **WASM is a compile target, not a runtime.** Three
distinct things get conflated:

- **Run in a browser** — sandboxed, URL-distributed, **no real local files** (only
  permission-gated File System Access API on Chromium; OPFS sandbox elsewhere), wasm32 4GB
  linear-memory ceiling, no LSP subprocesses, no real threads.
- **Run in a webview (Tauri/wry, Electron)** — HTML/CSS/JS front-end, but **native Rust
  backend** with full OS access (real files, LSP, threads, >4GB files). The core need not
  even be WASM here.
- **Use WASM only to execute the core logic** — an implementation detail, hostable by
  wasmtime/wasmer or a webview, orthogonal to the UI.

**Rendering sub-options:**
- **(a) Rust core (WASM) + JS/DOM view.** The VS Code/Monaco/CodeMirror model: Rust owns
  the piece tree; JS owns a virtualized DOM view, input, IME, a11y. *Best modern look and
  best "free" accessibility/IME* (real DOM), at the cost of a hand-written TS view +
  bridge. Monaco is literally a piece-tree `TextModel` + `ViewModel` + virtualized DOM
  `View` ([Monaco architecture](https://deepwiki.com/microsoft/vscode/3-monaco-editor)).
- **(b) Rust+WASM → `<canvas>` (WebGL/WebGPU).** egui/floem-web/eframe model. Max code
  reuse, self-styled look, **but you re-implement IME, a11y, text selection, and "find on
  page" yourself** — egui's own docs list these as canvas penalties. floem's web support is
  "experimental"; Zed's gpui-on-web is unproven.
- **(c) Ratzilla (ratatui → DOM/canvas/WebGL2).** Reuses ~100% of Fresh's TUI view with
  near-zero porting, **but renders a terminal cell grid** — "terminal-themed", not modern.
  This is the option the current scaffold points at and it **contradicts the stated goal**.

**The JS↔Rust bridge** (for option a): `wasm-bindgen`/`web-sys`/`js-sys`. The central design
rule — learned the hard way by **xi-editor** — is to keep the boundary **in-process** (WASM
linear memory) and **ship only visible-window line diffs** (a "line cache"), not whole-buffer
strings (`content() -> String` is O(file) per edit, fatal for GB files) and not full-model
JSON-RPC. Expose rendered bytes as zero-copy `Uint8Array` views over Rust `Vec<u8>`
(re-acquire after any allocation, since memory growth invalidates the view); reserve JSON
(`serde-wasm-bindgen`) for the cold control plane (completions, diagnostics).

**Text input in the browser** is the hard part: naive `keydown`→insert breaks IME,
autocorrect, dead keys, and mobile. The mature approaches are a **hidden `<textarea>`**
(ACE/CM5/Monaco) or **`contenteditable` + `beforeinput`/composition** (CodeMirror 6). The
universal IME rule: **do not mutate the DOM/selection during composition** or you abort the
IME. Strong recommendation: in option (a), **wrap CodeMirror 6 or Monaco's input layer** and
treat Fresh's core as the model rather than hand-rolling text input.

**Filesystem:** back the WASM `FileSystem` trait with **OPFS sync access handles in a Web
Worker** (≈90ms vs ≈850ms IndexedDB for a 100MB write), and use the **File System Access
API** as an optional Chromium-only "open/save real file" bridge with download/upload
fallback. **IndexedDB is the wrong default** for a GB-file editor.

**Verdict for A:** Treat a **Tauri/webview shell with a DOM view (wrapping CM6/Monaco)** as
the *primary* modern-UI-via-web target (same view code as a future pure-browser build, but
keeps native files/LSP/threads/>4GB files). Reject Ratzilla and IndexedDB for the primary
UI. Pure-browser is a sharable, capability-limited demo.

| Criterion (vs native Fresh) | (a) WASM core + JS/DOM | (b) Rust+WASM → canvas | (c) Ratzilla |
|---|---|---|---|
| Dev effort | High (new TS view + bridge) | Very high (view + GPU text + IME) | **Lowest** (reuse TUI view) |
| Look/feel modernity | **Best** (real DOM/CSS/IME) | Good, self-styled, canvas quirks | **Poor — still a grid** |
| Perf on GB files | Good (virtual DOM + zero-copy diffs) | **Best ceiling**, boundary still matters | OK throughput, weak UX |
| Filesystem | Chromium-only real files; OPFS else. **Webview removes the limit.** | same | same |
| Accessibility / IME | **Best** (real DOM / wrap CM6) | Must hand-roll; weak | Weak |
| Distribution | No-install URL, or installable (Tauri) | same | same |

#### A.1 — Reference design: Tauri + DOM/CSS chrome + SVG text (PoC built)

This is the concrete, recommended shape of Direction A, and a PoC renderer is in the
tree (`crates/fresh-editor/src/view/chrome_html.rs`, demo `docs/internal/chrome-web-demo.html`).

**Process model (Tauri-primary):**
- **Native Rust backend** owns the core — piece tree, LSP subprocesses, QuickJS plugins,
  **real local files, >4GB** — i.e. none of the pure-browser sandbox losses. The core
  already produces a serializable **`ChromeSnapshot`** (§ the chrome seam) plus the
  visible, styled text lines.
- **Webview frontend** (the OS webview via Tauri/`wry`) renders the UI from those two
  inputs. In production this is TypeScript; the Rust `chrome_html` renderer emits the
  *identical markup* so the structure is unit-testable and demoable without a browser.

**The bridge (heed the xi lesson — local but still a serialization boundary):**
- Frontend → backend: normalized input commands (key/mouse/IME, and chrome events like
  `SelectTab`/`CloseTab`/`OpenMenu`/`BeginDividerDrag`) via Tauri `invoke`.
- Backend → frontend: on change, push `{ chrome: ChromeSnapshot, lines: visible-window
  diff }` via a Tauri event. **Ship only the visible-window line diff (a line cache),
  never the whole buffer** — the same rule as the WASM bridge; Tauri IPC is local but
  serialized, so keep the big buffer in Rust and stream only on-screen rows.

**Rendering split (what the PoC proves):**
- **Chrome = DOM/CSS.** The browser does the *layout*, so the frontend consumes the
  snapshot's **semantic split tree** directly (no pixel math): each `Split` → a nested
  `<div class="split vertical|horizontal" style="grid-template-columns|rows:{a}fr 5px
  {b}fr">` with a `<div class="divider">` track; each `Leaf` → a `<section class="pane">`
  with a DOM tab bar (`role="tablist"`, native tabs + close affordance) and a content
  area. Menu bar = `<nav role="menubar">`; status bar = `<footer role="status">`;
  popups/overlays = `role="dialog"` scrim overlays. Because it's real DOM, **ARIA /
  screen-reader support comes largely for free** — the big a11y win over the GPU path.
  (Note: this path uses the *snapshot*, not `chrome_layout`'s pixel rects — those are for
  the GPU/canvas path where you must position everything yourself.)
- **Text body = SVG `<text>`/`<tspan>`.** Each visible line is one `<text>`; each
  syntax run is a `<tspan fill="#..">` at a monospace x-advance. SVG gives precise glyph
  positioning, crisp scaling/zoom (vector), and trivial per-run coloring; HTML in content
  is escaped. (A DOM-`<span>`-per-run body is the more-accessible alternative; SVG is what
  was requested and is better for exact positioning and transforms.)
- **Input/IME:** DOM `keydown`/`pointer`/`wheel`/composition events normalized and sent to
  the backend; IME via a hidden `contenteditable`/`<textarea>` overlay (or wrap CodeMirror
  6's input layer) — do not mutate the DOM during composition.

**What the PoC delivers (verifiable headlessly):** `render_document(&ChromeSnapshot,
&[WebLine], WebOptions) -> String` produces a self-contained HTML doc; 4 unit tests assert
the DOM/CSS chrome (menubar/tabs/close/status), the nested CSS-grid split tree with
dividers, native popups, the SVG `<text>`/`<tspan>` body, and HTML escaping; and
`examples/chrome_web_demo.rs` generates `docs/internal/chrome-web-demo.html` (3 panes from a
vertical+horizontal split, 6-item menu bar, a command-palette popup, syntax-colored SVG
text) — **openable in any browser** to see the chrome. Production work remaining: wrap it in
a Tauri shell, wire the `invoke`/event bridge with the line-cache diff, and the IME input
layer.

### Direction B — Native per-platform GUI / hybrid native chrome

The realistic shape here is a **hybrid**: a native window + **native menus** (`muda`,
already used) + **native dialogs** (`rfd`) + native notifications, with the **editor text
area and custom widgets app-rendered** on the GPU. Almost nobody ships a *fully* per-platform
native UI (separate AppKit/WinUI/GTK codebases over one core) for an editor — the maintenance
cost of N UI codebases is the reason Zed/Lapce/VS Code each chose a single rendering layer.

**Which parts genuinely benefit from being native** (delegate these): file/save pickers and
message boxes, the menu bar, system notifications, **IME/composition**, **accessibility tree**,
drag-and-drop, dark-mode/accent integration. **Which should stay custom:** the text editing
surface itself (you need full control of latency, virtualization, glyph rendering, multi-cursor).
This is exactly how Zed, WezTerm, Ghostty, and Alacritty split it.

**Rust native-integration crates (all current as of mid-2026, all viable):**

| Crate | Role | Notes |
|---|---|---|
| `winit` 0.30.x | windowing + event loop + IME | already used by `fresh-gui` |
| `muda` 0.19 | native menu bar / context menus | already used (macOS); GTK on Linux, HMENU on Win |
| `rfd` 0.17 | native open/save/folder + message dialogs | **recommended default**; Win/macOS/Linux + **WASM**; XDG-portal backend needs no GTK build dep |
| `accesskit` 0.2x + `accesskit_winit` 0.33 | cross-platform a11y (UIA / NSAccessibility / AT-SPI) | the standard; integrates via the winit adapter |
| `arboard` 3.6 | clipboard (text+image) | note X11/Wayland "clear on exit" needs a live process or `SetExtLinux::wait()` |
| `tray-icon` 0.24 | system tray | **no tray on stock GNOME Wayland** without an extension |
| `notify-rust` 4.x | desktop notifications | Linux-rich (XDG); macOS/Windows are a subset |
| `objc2` / `windows-rs` / `gtk4-rs` | direct platform calls | for anything muda/rfd don't cover; objc2 is the modern Cocoa path |

**Accessibility is the major hidden cost.** A native *toolkit widget* gets screen-reader
support essentially free (the OS widget *is* the AX object). A **custom-rendered text surface
must build and continuously maintain its own AccessKit tree by hand**: model the field as a
control node plus one `Role::TextRun` child **per line/format span**, and on every edit *and*
every caret move recompute `character_lengths` / `word_starts` / `character_positions` /
`character_widths` and re-push a `TreeUpdate`; map the cursor to a `TextSelection`
(`anchor`/`focus` as `(TextRun, character_index)`; a bare caret is a degenerate selection);
and service inbound `SetTextSelection`/`ReplaceSelectedText`. AccessKit's own author calls
text "the most notoriously difficult control type"; each platform adapter is ~2.4–4.2k LOC
(AccessKit absorbs that for you), rich text is still unsupported, and even Monaco/CodeMirror
**delegate to a hidden `<textarea>`** rather than reimplement it. Plan for it; don't discover
it.

**IME on a custom surface** is the symmetric hidden cost. winit gives only a thin API
(`set_ime_allowed`, `set_ime_cursor_area`, `Ime::{Enabled,Preedit,Commit,Disabled}`); you must
keep a separate preedit buffer, render preedit inline with distinct styling, map the
`Preedit` byte range to glyph pixels, continuously report the caret rectangle for candidate
placement, forward composition-navigation keys to the IME (not the document), and handle
dead-keys/AltGr yourself. Backend quality: Wayland/Windows solid, macOS good with edge cases,
**X11 fragile** (position-only, kills dead-key reporting when IME is on), mobile/web absent.

**Distribution/signing:** macOS `codesign` + `notarytool` + Gatekeeper (Fresh already has
Info.plist/entitlements/notarization scaffolding); Windows Authenticode + SmartScreen + a DPI/
Common-Controls manifest (already in the productization plan); Linux AppImage/Flatpak.

| Sub-variant | Dev effort | Modernity | Native feel | A11y for free | Maint. of N codebases | IME correctness |
|---|---|---|---|---|---|---|
| **Hybrid native chrome + custom GPU text** | Medium | High | High (chrome) | Partial (chrome yes, text no) | One UI codebase | You own it (winit) |
| **Fully native per-platform** | Very high | High | Highest | **Yes (everywhere)** | **N codebases** | Free (native widgets) |

**Three concrete gaps the research surfaced:** (1) **No cross-platform native font-picker
crate exists in Rust** — `rfd` covers file/message dialogs only, not font/color choosers; build
a small custom in-app font picker rather than wrapping `NSFontPanel`/`ChooseFont`/`GtkFontChooser`
by hand. (2) **AccessKit text a11y is a multi-month workstream**, not a drop-in (above).
(3) **Modern native macOS UI (SwiftUI) is unreachable from Rust** — `objc2` reaches AppKit/Obj-C
but SwiftUI is Swift-only, so a fully-native macOS frontend means *writing Swift* bridged over a
C ABI (the Ghostty model). This asymmetry — GTK4 fully Rust-native, Win32 well-covered by
`windows-rs`, but macOS needing a second language — is the decisive cost driver against
fully-native-per-platform.

**Precedents bracket the decision:** **Ghostty** validates fully-native (Zig core + Swift/AppKit
on macOS + GTK4 on Linux) *when native feel is the product*; **1Password 8** is the cautionary
inverse — it **abandoned** separate native AppKit/.NET apps for one Electron+Rust codebase because
N-codebase feature-parity was untenable (and ate the non-native backlash). Signing/notarization is
a per-OS tax **independent of UI approach**; fully-native only adds CI/toolchain + language cost
(Xcode/Swift, Windows App SDK, GTK dev libs), not signing cost.

**Verdict for B:** **Hybrid is almost certainly the answer.** Adopt `muda` (have it) + `rfd` +
`accesskit`/`accesskit_winit` + `arboard` (+ `notify-rust`, `tray-icon` if wanted) on the existing
winit+wgpu base; keep the text surface custom. Reject fully-native-per-platform unless native feel
becomes a top-three differentiator and platform specialists (esp. a Swift macOS layer) can be
funded — and note even then the piece-tree/LSP text area must stay custom, so 2b's "free a11y/IME"
benefit applies only to the chrome.

### Direction C — Cross-platform Rust GPU toolkit

The recurring, decisive finding across **all** toolkits: **none ships a multi-GB virtualized
editor; you use the toolkit for the shell and build the text surface as a custom GPU widget
with your own text engine** (which Fresh already has). The toolkits differ mainly in
*paradigm*, *text quality*, *winit+wgpu fit*, *a11y/IME*, and *maturity/license*.

| Toolkit | Paradigm | winit+wgpu fit | Built-in editor for huge files | a11y (AccessKit) | IME | License | Maturity / users |
|---|---|---|---|---|---|---|---|
| **egui** | immediate | **first-class** (`egui-winit`+`egui-wgpu`; custom `PaintCallback` → raw `wgpu::RenderPass`) | No (`TextEdit` re-lays-out whole buffer; >1s/keystroke @15M chars; ~2GB/10MB) | **Shipping** (Win/macOS, Linux via adapter; web=experimental reader); always-on in eframe | Solid on desktop since 0.34 (Mar 2026) | MIT/Apache-2.0 | Rerun; very active, pre-1.0 |
| **iced** | Elm/MVU | good (winit *fork* + wgpu; `shader::Primitive` → raw wgpu) | No (`text_editor` keeps whole doc resident; single cursor) | **Not upstream** (open #552; only in libcosmic fork) | shipped 0.14 (Dec 2025), works in both text widgets; WASM broken | MIT | System76 COSMIC; pre-1.0 |
| **floem** | fine-grained reactive (signals) | **best fit** (built on winit + wgpu; Vello/vger/Skia/tiny-skia) | **Closest** — ships `Editor`/`text_editor` + `floem-editor-core` (xi-rope) = Lapce's editor; visual-line virtualization | **None** (open #8 since 2023); Parley adds text a11y if adopted | TextInput solid; editor weaker (#1024); Linux rough | MIT | **Lapce** (only real user); pre-1.0 |
| **gpui** | hybrid immediate/retained | **poor** (own Cocoa/Win32 platform layer; Metal/DX11 native; Linux→wgpu Feb 2026 only) | Lowest-level but Zed-proven (`uniform_list` + custom `Element`) | **Infra just landed** (3 adapters), UI not yet wired → effectively none | **best** (NSTextInputClient etc., shipping in Zed), edge cases | Apache-2.0 (⚠ transitive GPL-3 via sum_tree/ztracing — verify) | Zed, Longbridge Pro; pre-1.0, macOS-first |
| **Slint** | declarative `.slint`, retained | yes (winit default; wgpu since 1.12, *experimental*, version-pinned) | No (`TextEdit` is flat `SharedString`; uniform-row `ListView` only) | **Shipping** (AccessKit on winit + Qt path); text-input a11y is the gap (#2895) | usable since 1.3; caret-positioning rough edges | tri-license (royalty-free desktop OK; GPLv3; paid for embedded) | embedded/automotive; mature 1.16 |
| **Xilem/Masonry + Vello/Parley** | reactive over retained | yes (winit + Vello/wgpu) | No (single-body `Prose`/`TextArea`; no large-file virtualization) | **a11y-native** (AccessKit author is a contributor); maturing per-widget | re-added 2024; candidate-window placement WIP | Apache-2.0 / MIT | **no production app yet**; alpha (Xilem 0.4) |
| **Dioxus + Blitz** | RSX/signals | `dioxus-desktop`=**webview** (collapses into Direction A); `dioxus-native`/Blitz = winit+Vello | No (Blitz pre-alpha, no editor engine; you'd embed Monaco/CM6 on the webview path) | webview path free; Blitz=experimental AccessKit | webview free; Blitz partial | MIT/Apache (Stylo MPL-2.0) | Dioxus mature; **Blitz pre-alpha** |
| **makepad** | shader DSL ("Live") | **no winit/wgpu** (own platform + GPU abstraction) | **has its own MSDF code editor** (powers Makepad Studio) | **none** (no-op scaffolding) | per-platform, **Linux broken** | MIT/Apache-2.0 | Robrix/Moly; thin docs, small ecosystem |
| **freya** | React-like (own core since 0.4) | winit + **Skia** (CPU default) | `freya-edit`: rope (ropey) + tree-sitter + `VirtualScrollView` + `use_editable` | **integrated** (incremental AccessKit tree) | partial, history of bugs | MIT | solo-maintained; 0.4 still RC after a year |

**Text-rendering deep-dive (the part you'll actually build).** The ecosystem has converged
on **HarfRust** for shaping (HarfBuzz port, used by both cosmic-text ≥0.15 and parley) and the
Fontations stack (`read-fonts`/`skrifa`, which ships in Chrome). Two end-to-end stacks for a
custom GPU text surface on your own wgpu:

- **cosmic-text + glyphon** (HarfRust shaping, **swash** rasterization with subpixel+hinting,
  **etagere** glyph atlas). The pragmatic, atlas-based, *crisp-at-fixed-zoom* choice; proven
  in Lapce (older) and Zed-on-Linux. Best for low-DPI Windows/Linux crispness; weak at
  continuous zoom (atlas thrash). This is the same stack Zed's `gpui_wgpu` uses.
- **parley + Vello** (HarfRust + skrifa + fontique + ICU4X for BiDi/segmentation; Vello GPU
  vector rendering). Best for arbitrary zoom and variable-font animation, and parley ships a
  ready `PlainEditor` with **AccessKit + IME** plus a `vello_editor` reference. Caveat: Vello
  is alpha and **lacks hinting/RGB-subpixel AA at low DPI** (glyph caching is mid-flight).

A universal gotcha for *any* shaping stack in a code editor: **ligatures that span
differently-colored syntax runs can't show two colors in one glyph** (open Zed/cosmic-text
issue) — split runs at color boundaries or disable ligatures across token boundaries.

**Top-2 candidates for C:** **egui** (most mature wgpu-callback escape hatch + shipping
AccessKit + IME, immediate-mode maps naturally onto "draw the visible viewport each frame";
bring your own text via cosmic-text/parley) and **floem** (best winit+wgpu fit and the only
toolkit with a real Lapce-derived editor module, MIT, but no a11y and a tiny ecosystem).
gpui is the most editor-proven but is the *worst* fit for an existing winit+wgpu codebase and
has the GPL-contamination flag.

---

## 4. Cross-cutting: the UI-abstraction seam (the keystone)

How real editors separated core from UI, and what it teaches us.

**The survey:**
- **xi-editor (Rust core + JSON-RPC frontend protocol; Cocoa/GTK/Electron frontends).**
  Raph Levien's [retrospective](https://raphlinus.github.io/xi/2020/06/27/xi-retrospective.html)
  is the central lesson: the **serialized, async, cross-process front-end protocol was a
  mistake** — it injected latency and enormous async complexity; the project concluded it
  should have **embedded the core in-process and communicated via FFI** ([decouple-from-JSON
  issue](https://github.com/xi-editor/xi-editor/issues/1235)). Levien's canonical example of
  the async pain was **word-wrap during a live window resize** (races between editing and wrap
  → tearing); his counterfactual is the whole lesson: *"if we just had the text available as
  an in-process data structure for the UI to query, it would have been quite straightforward."*
  The part that *was* right was the **line-cache `update` protocol**: the core sends ops
  applied old→new against an `old_ix` cursor — `copy`(reuse n) / `skip`(drop n) /
  `invalidate`(n placeholders) / `ins`(new lines) / `update`(restyle n) — and the frontend
  announces its viewport via `scroll`. This is the direct ancestor of Neovim's
  `grid_line`/`grid_scroll`; **keep this idea, drop the process/async boundary.**
- **Neovim — the positive model.** The `--embed` msgpack-RPC UI protocol externalizes a
  **cell grid by default** (`ext_linegrid`: `grid_line` with `[text, hl_id, repeat]` RLE runs,
  `grid_scroll`, `grid_cursor_goto`, `hl_attr_define`), and lets a UI **opt into semantic
  externalization** of specific components: `ext_multigrid` (per-window grids → native
  scrolling/tear-off), `ext_popupmenu`, `ext_cmdline`, `ext_messages`, `ext_tabline`. A
  single `redraw` notification batches events and a **`flush` event gives atomic frame
  semantics**. Input is one canonical vocabulary (`<C-a>` notation via `nvim_input`;
  `nvim_input_mouse` reports grid+cell so the UI does hit-testing). Many GUIs (Neovide,
  neovim-qt, VimR, goneovim) attach to one unmodified core, each picking which `ext_*` it
  wants. **Design insight: a universal low-level cell representation + opt-in semantic
  externalization of high-level components.**
- **Emacs — in-process backend swap.** One redisplay engine (`xdisp.c` builds a *desired*
  glyph matrix; `dispnew.c` diffs against the *current* matrix) drives many backends (tty, X,
  NS, w32, pgtk) through a **vtable of function pointers** (`struct terminal` hooks +
  `redisplay_interface`). This is conceptually *immediate-mode-built, diffed, retained
  matrix* — strikingly close to how ratatui already works. Proof an **in-process trait**, not
  an RPC, can drive radically different backends for decades.
- **Kakoune** — `kak -ui json`: a semantic-but-screen-oriented JSON protocol (styled
  `Atom`/`Line` spans, `draw`/`draw_status`/`menu_show`/`info_show`). Another clean
  semantic protocol example.
- **Zed (gpui)** and **VS Code** — the counter-examples. Zed is deliberately **monolithic**
  (the editor *is* a gpui View; a protocol seam would add latency it was built to eliminate).
  VS Code's much-touted multi-process model isolates **extensions**, not the **UI** — the
  renderer is one monolithic web app; "frontends" (desktop/web) are the same TS compiled
  against `common`/`browser`/`node` layers. **Helix** *wanted* a frontend-agnostic core
  (`helix-core`→`helix-view`→`helix-term`) but `helix-view` leaked terminal/crossterm types
  and the GUI never shipped — the cautionary "leaky seam" example.

**Three seam philosophies for Fresh:**

| Seam | What it is | Latency | "Modern look" reach | Cost | Fit with Fresh today |
|---|---|---|---|---|---|
| **(a) Semantic UI protocol** (Neovim `ext_*` / Kakoune) | core emits "show this menu / completion list / set cursor / draw this text grid"; frontend renders natively | low if in-process | High for chrome; text body still a grid unless multigrid-style | medium | natural extension of the `Menu` model + hit-area widgets |
| **(b) Retained widget/scene tree** the core builds, backend renders | a DOM the core owns | low | High | high (rewrite widgets as retained) | far from today's immediate-mode model |
| **(c) Shared immediate-mode draw trait** (core calls draw primitives each frame; backend implements) | closest to ratatui's `Backend` | lowest | **Cell-grid-bound → not "modern"** | **lowest** | Fresh already does exactly this |

**Recommendation for Fresh's seam:** a **layered hybrid**, mirroring Neovim's "grid +
semantic externalization":

1. **Keep the immediate-mode `Backend`-style draw model** (c) for the **text body** — it's
   what Fresh has, it's lowest-latency, and the terminal frontend needs it anyway.
2. **Introduce a richer, in-process `RenderCommand` enum** (a) *above* the cell grid for
   **chrome**: menus, command palette, completion popups, dialogs, status bar, tabs, file
   tree. The core emits semantic commands (`ShowMenu`, `ShowCompletions{items, anchor}`,
   `OpenDialog`, `SetCursor{shape, rect}`, `DrawTextRun{...}`), and:
   - the **terminal frontend** lowers them back to cells (so nothing breaks);
   - the **GPU/native frontend** renders them with proportional fonts / native widgets;
   - the **web frontend** renders them as DOM.
3. **Keep the seam strictly in-process** (a Rust trait the frontend implements; the core
   calls it directly), echoing the xi lesson. The existing byte-stream daemon stays as a
   *terminal-only* transport; do **not** generalize it into the UI seam.
4. **Define one input vocabulary** at the seam (Neovim-style): a backend-agnostic key/mouse
   event the terminal, GUI, and browser all map onto — Fresh already normalizes to crossterm
   types via `GuiApplication`; formalize that as *the* vocabulary, plus an IME event and a
   "focus/hit-test owner" decision (keep focus in the core's `FocusManager`; let the
   *backend* own pixel→logical hit-testing for proportional fonts and report logical targets,
   the way Neovim multigrid makes the UI report grid+cell).

This lets a modern frontend render chrome natively immediately (the high-visibility win),
while the text body can stay cell-grid until Phase 3 swaps in a real GPU text surface — all
without disturbing the terminal product and without serialization on the hot path.

### 4.1 Worked example: splits and tabs

Splits and tabs are the clearest concrete case of the grid-vs-semantic decision, and the
reason to prefer the Neovim-style hybrid over a flat `Backend`-level swap — a flat cell-grid
backend can *never* give native draggable tabs or per-pane fonts. Neovim already solved exactly
this with two dedicated extensions (`ext_tabline`, `ext_multigrid`); the design maps 1:1.

**Today in Fresh:** tabs are a 1-row cell tab bar per split (`view/ui/tabs.rs`; tab/buffer
state in `app/buffer_management.rs`, `buffer_groups.rs`). Splits are a tree of regions, each a
**sub-rectangle of the single grid** (`view/ui/split_rendering/`), with line-wrap at the
region's cell width and per-pane focus tracked in `app/active_focus.rs` (active buffer per
split, LRU). The terminal renders this fine and `fresh-gui` already shows it in a GPU
window — it just isn't modern.

**Tabs → `ext_tabline`.** Emit a semantic `Tabline { tabs: [{title, modified, icon,
buffer_id}], active }` (Neovim's `tabline_update`). The terminal lowers it to today's cell tab
bar unchanged; a GUI draws **native tabs** — close buttons, drag-to-reorder, overflow menu,
middle-click-close, file-type icons. The core stays the source of truth for order/active/buffer;
the backend owns pixel chrome and reports "clicked/closed/reordered/dragged tab X" via the
normalized input vocabulary. **A cheap Phase-2 win** — high visual payoff, zero change to the
text body.

**Splits → `ext_multigrid`.** Instead of one grid with sub-rects, each pane gets its **own
grid/surface** the backend positions (`win_pos`). The core keeps the *logical* split tree
(which buffer is where, focus); the backend owns pixel geometry. This is what unlocks the
things a single shared grid **cannot** do: pixel-precise **smooth draggable dividers** (not
cell-snapped), **per-pane independent font size/zoom**, native per-pane scrollbars + smooth
scroll, and **tear-off** a pane into a separate OS window (the daemon's existing `OpenWindow`
capability is the hook). This is a Phase-3 change, landing alongside the modern text body.

**Two complications splits/tabs force (both already in §5):**
1. **Hit-testing must become pane-relative.** Today mouse events are global `(col,row)`. With
   proportional fonts + multigrid, the *backend* must resolve pixel → `(pane/grid id, logical
   position)`, so the seam's mouse event carries a **pane/grid id + logical position**, not a
   global cell coordinate (exactly Neovim's `nvim_input_mouse(button, action, grid, row,
   col)`). Focus stays core-owned (`FocusManager`); the backend translates native pane/window
   focus into "focus pane X".
2. **Per-pane wrap is the xi resize-race in miniature.** Each pane's wrap width becomes a
   pixel measurement (pane width + font metrics). xi flagged word-wrap-during-resize across an
   async boundary as a tearing source — the concrete reason the seam stays **in-process and
   synchronous**: GUI resizes a pane → core re-wraps that pane synchronously → no race.

**Phasing:** Phase 2 — tabs go semantic (native tab bar); splits stay cell sub-rectangles (they
render fine, just with a GPU-drawn divider). Phase 3 — splits become per-window grids, enabling
per-pane fonts, pixel dividers, independent zoom, and tear-off.

---

## 5. Cross-cutting: input, focus, IME, accessibility

- **Input vocabulary:** formalize the crossterm-normalized key/mouse events already used by
  `GuiApplication` as the seam's input type, plus a dedicated **IME event**
  (`Enabled/Preedit/Commit/Disabled`) and a scroll/gesture event. Mouse events carry a
  *logical* target (the backend resolves pixels → logical position; with proportional fonts
  the backend must own hit-testing, unlike today's (col,row)).
- **Focus / "bubbling":** keep Fresh's **global modal `FocusManager`** as the source of
  truth — it's simple and already works. Backends do **not** invent their own focus/bubbling;
  they translate native focus/blur into the core's model (this is what `fresh-gui` already
  does). Modal dialogs/popups remain core-owned overlays.
- **IME:** wire winit IME into the text widget (`set_ime_allowed` on focus,
  `set_ime_cursor_area` on every caret move, render `Preedit` inline, commit on `Commit`,
  forward composition-nav keys to the IME). Expect X11 fragility; test Wayland/Windows/macOS.
- **Accessibility:** adopt **AccessKit via `accesskit_winit`** from Phase 1, even before the
  GPU text surface exists (chrome widgets get a11y cheaply). Design the `RenderCommand`/text
  model so the core can emit per-`TextRun` `character_lengths`/`word_starts`/positions/widths
  and a `TextSelection` — i.e., make a11y a first-class output of the seam, not a bolt-on.
  This is the single biggest hidden cost of a custom-rendered editor; starting early avoids a
  multi-year retrofit (cf. Quip).

---

## 6. Recommended phased plan

- **Phase 0 — done.** `fresh-gui` GPU window of the cell grid (`winit + wgpu + ratatui-wgpu`,
  `muda` menus). Ship/polish this as the "GPU terminal" baseline.
- **Phase 1 — the seam + native chrome.** Introduce the in-process `RenderCommand` seam above
  the cell grid; terminal frontend lowers it to cells (zero behavior change). Add `rfd`
  dialogs and extend `muda`; adopt `accesskit_winit`. Define the input/IME vocabulary.
  *Risk: low; entirely additive; terminal product unaffected.*
- **Phase 2 — modern chrome.** GPU frontend interprets the semantic commands: proportional
  fonts and real widgets for menus/palette/completions/dialogs/status/tabs. Text body still a
  cell grid. *This is the first visible "modern, non-terminal" payoff.*
- **Phase 3 — modern text body (optional, the big one).** Replace the text-body renderer with
  a custom GPU text surface on your own wgpu surface, using **cosmic-text + glyphon** (crisp,
  atlas, pragmatic) or **parley + Vello** (zoom/variable-font, ships editor+a11y+IME helpers).
  Reuse Fresh's piece tree + visual-line virtualization (the same pattern Zed/Lapce use).
  Wire full IME + AccessKit text here.
- **Web/webview track (parallel, optional).** A Tauri/wry shell with a DOM view (wrap CM6 or
  Monaco) consuming the same seam; OPFS + File System Access for storage. Pure-browser as a
  capability-limited subset.

**Toolkit choice if you adopt one for the shell rather than hand-rolling chrome:** lead with
**egui** (best wgpu-callback escape hatch, shipping AccessKit + IME, matches the existing
winit+wgpu stack) or **floem** (best winit+wgpu fit + a real editor module, but no a11y).
Avoid gpui for an existing winit+wgpu codebase. Either way, **build the text surface
yourself.**

---

## 7. Key risks & open questions

1. **Scope of "modern".** Is the goal native dialogs + proportional chrome (Phase 2, cheap,
   high-impact) or a full GPU text surface (Phase 3, large)? Phase 2 alone may satisfy "more
   modern, not terminal-based".
2. **Accessibility commitment.** A custom GPU text surface means owning AccessKit text a11y
   (the "most notoriously difficult control"). If a11y is a hard requirement and Phase 3 is
   wanted, budget it explicitly; otherwise the webview/DOM path gets a11y free.
3. **Performance proof.** Validate the seam adds no per-keystroke latency (keep it in-process;
   no serialization on the hot path — the xi lesson).
4. **Pre-1.0 churn.** Every Rust GUI/text crate surveyed is pre-1.0 with frequent breaking
   changes (egui, iced, floem, gpui, Slint, Xilem/Vello/Parley, Dioxus/Blitz, makepad, freya).
   Pin versions; prefer the most-used (egui, cosmic-text/glyphon, parley, winit, accesskit).
5. **Ligature/syntax-color interaction** in the text surface (split runs at color boundaries).
6. **gpui license** (transitive GPL-3 via `sum_tree`/`ztracing`) if gpui is ever considered.

---

## 8. Source index (representative)

- xi-editor retrospective — https://raphlinus.github.io/xi/2020/06/27/xi-retrospective.html ;
  decouple-from-JSON — https://github.com/xi-editor/xi-editor/issues/1235
- Neovim UI protocol (`ext_*`, redraw/flush, `nvim_input`) — https://neovim.io/doc/user/ui.html
- Emacs redisplay (`termhooks.h`, `dispextern.h`, `dispnew.c`) —
  https://github.com/emacs-mirror/emacs/blob/master/src/termhooks.h
- Kakoune JSON UI — https://github.com/mawww/kakoune/blob/master/doc/json_ui.asciidoc
- Helix architecture/GUI discussion — https://github.com/helix-editor/helix/blob/master/docs/architecture.md
- Monaco architecture — https://deepwiki.com/microsoft/vscode/3-monaco-editor ;
  CodeMirror 6 — https://marijnhaverbeke.nl/blog/codemirror-6-progress.html
- Zed gpui rendering — https://zed.dev/blog/videogame ; ownership — https://zed.dev/blog/gpui-ownership ;
  Linux→wgpu PR — https://github.com/zed-industries/zed/pull/46758
- Lapce on floem editor view — https://github.com/lapce/floem (src/views/editor)
- Text stacks: cosmic-text — https://github.com/pop-os/cosmic-text ; glyphon —
  https://github.com/grovesNL/glyphon ; parley — https://github.com/linebender/parley ;
  swash — https://github.com/dfrg/swash ; harfrust — https://github.com/harfbuzz/harfrust ;
  Vello — https://github.com/linebender/vello ; skrifa/fontations — https://github.com/googlefonts/fontations
- AccessKit — https://accesskit.dev/how-it-works/ , https://github.com/AccessKit/accesskit
- Native crates: muda — https://github.com/tauri-apps/muda ; rfd — https://github.com/PolyMeilex/rfd ;
  winit IME — https://docs.rs/winit/latest/winit/event/enum.Ime.html ;
  objc2 — https://github.com/madsmtm/objc2 ; windows-rs — https://github.com/microsoft/windows-rs ;
  gtk4-rs — https://github.com/gtk-rs/gtk4-rs
- Browser FS: OPFS — https://web.dev/articles/origin-private-file-system ; File System Access —
  https://developer.chrome.com/docs/capabilities/web-apis/file-system-access
- Toolkits: egui — https://github.com/emilk/egui ; iced — https://github.com/iced-rs/iced ;
  floem — https://github.com/lapce/floem ; gpui — https://github.com/zed-industries/zed/tree/main/crates/gpui ;
  Slint — https://github.com/slint-ui/slint ; Xilem — https://github.com/linebender/xilem ;
  Dioxus/Blitz — https://github.com/DioxusLabs/blitz ; makepad — https://github.com/makepad/makepad ;
  freya — https://github.com/marc2332/freya ; Ratzilla — https://github.com/ratatui/ratzilla
