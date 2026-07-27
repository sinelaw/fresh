# libghostty-vt vs `alacritty_terminal` for Fresh's embedded emulator

Status: **analysis / no change recommended yet** (July 2026)
Scope: the VT *output* emulation library behind `services/terminal/term.rs`.
Cross-ref: `docs/internal/terminal.md` §2, §5.

> This is a comparison of two *embeddable libraries*, not two terminal apps.
> Fresh does not run inside Alacritty or Ghostty; it embeds
> `alacritty_terminal` as a crate to emulate the PTY it spawns. The
> Ghostty-side equivalent is **libghostty-vt**, the C library extracted from
> Ghostty's core, reached from Rust through the community `libghostty-vt`
> crate.

Every claim below marked **[verified]** was checked against
`libghostty-vt` / `libghostty-vt-sys` 0.2.1 (pinned ghostty commit
`a887df42c`) with a real compiler and a real build, not from documentation.
Method is recorded in §9.

---

## 1. Verdict

**Don't switch now. Revisit when libghostty-vt tags a stable release and a
Zig-free build path exists.**

The API surface fits — a faithful port of `term.rs`'s core type-checks
cleanly against libghostty-vt **[verified]**, and several things we
hand-rolled (OSC 7 sniffing, OSC 133 activity, mouse/key encoding) are
first-class there. The problem is not capability. It is three things:

1. **`Terminal` is `!Send`/`!Sync`, and it blocks our exact construction.**
   `Arc<Mutex<TerminalState>>` fails to compile **[verified]**.
2. **The build needs Zig 0.15.x plus ~68 MB of network fetches** — and not
   just ghostty: the whole ghostty package graph, imgui and harfbuzz and
   fonts included, even for a vt-only build **[verified]**.
3. **Pre-1.0 on both sides of the FFI**, with the Rust binding pinned to a
   hardcoded ghostty commit that already lags `main` in ways that matter to
   us (§6.2).

Against ~4,900 lines of terminal e2e tests and 39 unit tests encoding
alacritty's exact grid/reflow semantics, that is a large bill.

**Separately and more urgently: §6.4 documents a scrollback-loss bug in
`flush_new_scrollback` that exists today, is backend-independent, and is a
hard blocker for the file-backed-scrollback offload plan.** Fix that first.

---

## 2. What we actually use

The coupling is narrow and lives in one file. `term.rs` is 2,177 lines; the
alacritty-facing part is maybe 80. Everything else in Fresh talks to our own
`TerminalState` / `TerminalCell`.

Seven capability groups, and nothing else:

| # | What we ask for | Call sites |
|---|---|---|
| A | **Construct + feed bytes.** `Term::new(TermConfig{scrolling_history: 200_000}, &TermSize, listener)`, `Processor::advance(&mut term, data)` | `term.rs:463-491`, `:541` |
| B | **Grid read for render.** `grid[Line(row - display_offset)][Column(c)]` → `.c`, `.fg`, `.bg`, `.flags` (BOLD/ITALIC/UNDERLINE/INVERSE) | `get_line` `:717-763` |
| C | **History read + wrap flag.** `grid().history_size()`, negative `Line(-n)` indexing, `Flags::WRAPLINE` on the last column | `row_wraps` `:1022`, `flush_new_scrollback` `:904`, `append_visible_screen` `:970`, `append_row_cells` `:1114` |
| D | **Resize with reflow.** `term.resize(TermSize)`, then re-anchor via a WRAPLINE-only scan | `:598-649` |
| E | **Mode flags (6).** `ALT_SCREEN`, `MOUSE_REPORT_CLICK`/`MOUSE_MOTION`/`MOUSE_DRAG`, `SGR_MOUSE`, `ALTERNATE_SCROLL`, `APP_CURSOR` | `:851-887` |
| F | **Events.** `Event::PtyWrite` (DSR replies), `Event::Title`, `Event::ResetTitle` | `PtyWriteListener` `:69-97` |
| G | **Viewport + cursor.** `scroll_display(Scroll::Bottom)`, `grid().cursor.point`, `grid().display_offset()` | `:840`, `:667`, `:722` |

And two things we build *ourselves* because alacritty drops them:
`Osc7Scanner` (`:109-200`) for OSC 7 cwd, and `OscActivityScanner` for
OSC 133 / OSC 9;4 activity.

We do **not** use alacritty for selection, search, key encoding, mouse
encoding, colors/palette (we hardcode a 16-colour table at `:1286-1301`),
damage tracking, or its `tty` module (we use `portable-pty`).

---

## 3. Capability mapping  **[verified — a port of all seven groups compiles]**

| | `alacritty_terminal` 0.25 | libghostty-vt 0.2.1 | Delta for Fresh |
|---|---|---|---|
| A | `Processor::advance` | `Terminal::vt_write(&[u8])` | 1:1. `TerminalOptions{cols, rows, max_scrollback}` replaces `TermConfig` |
| B | `grid[Line][Column]` direct index | `grid_ref(Point::Viewport{..})` → `GridRef::{row,cell,style}` — **but see §5.5, this is documented as not for a render loop** | Cell is opaque; `codepoint()`, `wide()`, `style()`. Viewport scroll is baked into `Point::Viewport`, so our `display_offset` arithmetic disappears |
| C | `history_size()` + `Line(-n)` + `WRAPLINE` on last col | `scrollback_rows()` + `Point::History{x,y}` (y=0 is oldest) + `Row::is_wrapped()` / `is_wrap_continuation()` | **Better.** Two explicit flags instead of inferring continuation from the previous row's last cell, and a non-negative index |
| D | `Term::resize`, reflow | `resize(cols, rows, cell_w_px, cell_h_px)`, reflow (alt screen does not reflow, same as alacritty) | Same model — and `TrackedGridRef` can replace our resync entirely (§6.1) |
| E | `TermMode` bitflags | `Mode::{ALT_SCREEN, DECCKM, NORMAL_MOUSE, BUTTON_MOUSE, ANY_MOUSE, SGR_MOUSE, ALT_SCROLL}` + `is_mouse_tracking()` | All six present |
| F | `EventListener::send_event` | `on_pty_write(\|term, data\|)`, `on_title_changed(\|term\|)` (read `term.title()` inside) | 1:1, typed rather than one `Event` enum |
| G | `scroll_display(Scroll::Bottom)`, `grid().cursor.point` | `scroll_viewport(ScrollViewport::{Bottom,Top,Delta,Row})`, `cursor_x()`/`cursor_y()`, `scrollbar()` | 1:1, plus `scrollbar()` |
| — | *(nothing — we sniff it)* | `pwd()` + `on_pwd_changed` (OSC 7 **and 9;9 and 1337**) | **Deletes `Osc7Scanner`** |
| — | *(nothing — we sniff it)* | `Row::semantic_prompt()`, `Cell::semantic_content()` | Replaces most of `OscActivityScanner` |
| — | `cursor_visible()` → hardcoded `true` (`:673`) | `is_cursor_visible()` (DEC 25) | Fixes a stub that is **ours**, not a library gap — alacritty has `TermMode::SHOW_CURSOR` |
| — | 4 bool flags | `Style { bold, italic, faint, blink, inverse, invisible, strikethrough, overline, underline: Underline, fg_color, bg_color, underline_color }` | Richer: `underline` is an enum (single/double/curly/dotted/dashed), colors are tagged (none/palette/rgb) |
| — | our hardcoded 16-colour table | `term.color_palette()` / `default_color_palette()` | Deletes `term.rs:1286-1301` |
| — | one `char` per cell | `GridRef::graphemes(&mut [char])`, `Cell::wide() -> CellWide` | Real ZWJ/emoji handling |
| — | none | `TrackedGridRef` (§6.1), scrollback compression, `KeyEncoder`, `MouseEncoder`, OSC 8 hyperlinks, Kitty graphics/keyboard, selection + HTML/VT formatters | |

**Everything we need is there.** That is the honest headline.

---

## 4. What we'd gain, ranked

1. **A reflow-surviving scrollback anchor** (`TrackedGridRef`) — see §6.1.
   For the file-backed-scrollback direction this is the single biggest item,
   and alacritty has no equivalent at all.
2. **OSC 7 for free, and more of it.** `on_pwd_changed` fires for OSC 7 **and
   OSC 9;9 (ConEmu) and OSC 1337 (iTerm2)**. OSC 9;9 is what PowerShell emits
   — a direct Windows cwd / Ctrl+Click improvement.
3. **Correct wide-char / grapheme handling.** `TerminalCell{c: char}` is a
   lossy model; this is a latent correctness bug, not a hypothetical.
4. **Shell integration as state, not as a sniffer** (`Row::semantic_prompt()`).
5. **Delete code**: `Osc7Scanner`, most of `OscActivityScanner`, the
   `display_offset` arithmetic, the hardcoded palette, possibly our key/mouse
   encoders.
6. **Scrollback memory**: byte *and* line caps, plus caller-driven incremental
   compression — though see §6.2, the byte cap is not reachable from the
   published Rust crate yet.
7. **Throughput.** SIMD parser, page-based storage. Plausibly faster — but
   **unmeasured for our workload**, and our hot path is dominated by
   `flush_new_scrollback`'s per-cell SGR reconstruction, not by parsing. Do
   not buy the migration on this.
8. **WASM.** `docs/wasm.md:276` calls `term.rs` an inherent blocker, correctly:
   alacritty drags in `libc`, `rustix-openpty`, `polling`, `signal-hook`,
   `miow`, `windows-sys` for a `tty` module we never use. libghostty-vt's
   *artifact* is zero-dependency and ships WASM utilities — but the Rust
   binding's `zig_target()` map has no `wasm32-*` entry and panics on unknown
   targets, so this win does not exist today.

---

## 5. What it would cost

### 5.1 `!Send` / `!Sync` — the architectural blocker  **[verified]**

Fresh shares the emulator between the PTY reader thread and the render path:

```rust
// manager.rs:55
pub state: Arc<Mutex<TerminalState>>,
```

`process_output` / `flush_new_scrollback` run on the reader thread;
`state.get_line(row)` is called synchronously per row from the render path
(`app/terminal.rs:1895`, `:2363`).

Porting `TerminalState` to hold a `Terminal<'static, 'static>` and asserting
`Arc<Mutex<TerminalState>>: Send` produces:

```
error[E0277]: `NonNull<TerminalImpl>` cannot be sent between threads safely
  = help: within `TerminalState`, the trait `Send` is not implemented for `NonNull<TerminalImpl>`
note: required because it appears within the type `Object<'static, TerminalImpl>`   (alloc.rs:48)
note: required because it appears within the type `Terminal<'static, 'static>`      (terminal.rs:230)
  = note: required for `Mutex<TerminalState>` to implement `Sync`
  = note: required for `Arc<Mutex<TerminalState>>` to implement `Send`

error[E0277]: `*mut c_void` cannot be shared between threads safely
  = help: within `libghostty_vt_sys::Allocator`, the trait `Sync` is not implemented for `*mut c_void`
```

Two independent causes: the `NonNull<TerminalImpl>` FFI handle, and the
`Box<dyn Fn…>` callbacks (not `+ Send`). Two ways out:

- **`unsafe impl Send` on a newtype.** The `Mutex` does serialize access and
  `!Send` here is structural, not thread-affine state — so this would very
  likely work. But it is outside what the library sanctions, on a pre-1.0 C
  API whose allocator behaviour we don't control.
- **Actor model.** Terminal lives solely on the reader thread; render pulls a
  snapshot over a channel. This is what the crate docs recommend, and it lines
  up with `RenderState`'s design (§5.5), which is explicitly built to be
  updated under a lock and then read lock-free.

**Correction to an earlier draft of this doc:** the `'alloc`/`'cb` lifetimes
are *not* an additional problem. `Terminal<'static, 'static>` type-checks
fine when callbacks capture owned data (which is what `PtyWriteListener`
already does with `Arc<Mutex<Vec<String>>>`) **[verified]**.

### 5.2 Build and distribution  **[verified]**

Without Zig on `PATH`, `cargo build` fails after cloning ghostty:

```
thread 'main' panicked at libghostty-vt-sys-0.2.1/build.rs:365:33:
failed to execute zig build: No such file or directory (os error 2)
```

With Zig 0.15.2 installed, the build then fetches **ghostty's entire package
graph** — 22 packages, ~68 MB — even though it passes
`-Demit-lib-vt=true -Dapp-runtime=none`:

> dcimgui, harfbuzz, freetype, fontconfig, oniguruma, glslang, spirv_cross,
> wuffs, highway, libpng, libxml2, zlib, pixels, uucode, libxev, vaxis, z2d,
> zf, zig_wayland, JetBrains Mono, Nerd Fonts Symbols, ghostty-themes

Root cause is in ghostty's `build.zig`: `SharedDeps.init`,
`GhosttyExe.init`, `GhosttyResources.init`, `GhosttyDocs.init`,
`GhosttyWebdata.init`, `GhosttyBench.init` and `GhosttyDist.init` all run
**unconditionally**, before any `emit_lib_vt` gating, so Zig must resolve
every `b.lazyDependency(...)` in the graph. They are fetched but not
compiled into the artifact. "Zero-dependency" is true of the *library*; it
is not true of *building the library through this crate*.

Fetches come from `github.com` (ghostty itself) and
`deps.files.ghostty.org`, plus at least one direct GitHub archive
(`ocornut/imgui`).

What that hits:

| Consumer | Impact |
|---|---|
| `cargo install fresh` (we publish to crates.io via `cargo-publish`) | **Every user needs Zig 0.15.x + git + ~68 MB of network.** The worst one — a direct adoption tax on our simplest install path |
| 6 dist targets in `dist-workspace.toml` | Cross-compile is supported — all six triples incl. `aarch64-pc-windows-msvc` are in `zig_target()`. CI runners need Zig |
| `musl-builds` | Both musl triples are mapped. OK |
| `min-size-build` | Static `libghostty-vt.a` adds size; `LIBGHOSTTY_VT_SYS_OPTIMIZE=ReleaseSmall` exists, delta unmeasured |
| Nix (`flake.nix`) | Pure builds forbid network → must pre-fetch ghostty *and its 22 Zig packages* as fixed-output derivations and pass `GHOSTTY_ZIG_SYSTEM_DIR` |
| Debian / AUR | Both want declared, offline, reproducible sources. A `build.rs` that clones from GitHub and then fetches 22 tarballs is a packaging problem |
| `wasm32-*` | `zig_target()` panics — unsupported |
| docs.rs | Handled: `build.rs` returns early when `DOCS_RS` is set |

Escape hatches exist (`GHOSTTY_SOURCE_DIR`, `GHOSTTY_ZIG_SYSTEM_DIR`,
a `pkg-config` feature), but they move the work to the packager rather than
removing it.

### 5.3 Maturity and maintenance

| | `alacritty_terminal` | libghostty-vt (Rust) |
|---|---|---|
| Latest | 0.26.0 (2026-04-06); we pin `"0.25"` → 0.25.1 | 0.2.1 (2026-07-18) |
| History | 0.24.0 → 0.26.0 over ~2 years, RC'd releases | first published 2026-03-28; 4 versions in 4 months |
| Downloads | ~899k | ~32k |
| Maintainer | Alacritty project | **Uzaaft (individual, community)** — not `ghostty-org` |
| Stability | de-facto stable | `vt.h`: "incomplete, work-in-progress API… definitely going to change". README: "pre-1.0 … do not guarantee compatibility with arbitrary installed C API revisions" |

Ghostty 1.3.0 (2026-03-09) separates libghostty's release cycle from the
GUI's, says the Zig module is "full featured" while the **C API is still in
progress** — the C API is the only one Rust can reach — and that they
"aren't sure yet when we'll tag the first libghostty releases". There is no
official ghostty-org Rust binding; Rust is one of ~15 community bindings.

The binding pins `GHOSTTY_COMMIT = "a887df42c…"` in `build.rs`. §6.2 shows
that pin already costs us something concrete.

### 5.4 Test surface

`tests/e2e/terminal.rs` is 4,891 lines / ~163 tests, plus 39 unit tests in
`term.rs`. Several encode alacritty's exact behaviour — the
cursor-reported-at-y=60 quirk (`terminal.rs:1149`), `ALTERNATE_SCROLL`-on-by-
default (`:3630`, `app/terminal.rs:1968`). Ghostty's mode defaults are not
guaranteed to match. The scrollback streaming invariants need re-proving
against a different reflow implementation.

### 5.5 The render path is not a drop-in  **[verified]**

The `grid_ref` API — the direct analogue of `get_line` — carries this in its
own doc comment (`screen.rs:41-43`):

> "This API is not meant to be used as the core of render loop. It isn't
> built to sustain the framerates needed for rendering large screens. Use
> the render state API for that."

And it means it: `Cell` and `Row` are opaque handles, and *every* attribute
is a separate FFI call (`ghostty_cell_get(tag, …)` / `ghostty_row_get(tag, …)`),
each returning `Result`. A naive per-cell read is `grid_ref` + `cell` +
`style` + `codepoint` + `wide` ≈ 5 fallible FFI calls per cell.

So `get_line` has to be rewritten onto `render::RenderState`, which is a
genuinely different model: `update(&terminal)` → `Snapshot` →
`RowIterator`/`CellIterator`, with two independent layers of dirty state
(global `Dirty` enum + per-row flags) that the caller must clear itself.
`CellIteration` does expose resolved `fg_color()`/`bg_color()`/`style()`/
`graphemes_utf8()`, so the per-cell work is better there — but it is a
rewrite of the render path, not a swap of the indexing expression.

Note this cuts the other way on §5.1: `RenderState` is explicitly designed so
the terminal is borrowed only during `update()`, "which allows the renderer
to be safely multi-threaded (as long as a lock is held during the update
call)". That is the actor split, and it is the sanctioned path.

---

## 6. Offloading scrollback to a file-backed buffer

This is the direction Fresh is heading: shrink the emulator's in-memory
history and let a lazily-loaded, file-backed Fresh buffer be the real
scrollback. Does that work with libghostty-vt as the backend?

**Short answer: yes, and ghostty is a *better* fit for it than alacritty —
but the thing that actually blocks the plan today is in our code, not in
either library (§6.4).**

### 6.1 `TrackedGridRef` is the primitive this plan wants

`Terminal::track_grid_ref(point)` returns an owned handle that "follows the
referenced cell as the terminal's page list is modified: **scrolling,
pruning, resize/reflow**, and other page-list operations update the tracked
reference automatically." `has_value()` goes false when the anchor is pruned
away; `point(PointSpace::History)` gives its current index; `snapshot()`
turns it back into a readable `GridRef`; `set()` moves it. It survives
primary/alternate screen switches (it stays bound to its owning page-list).

That is a direct replacement for four pieces of hand-rolled bookkeeping in
`term.rs`:

| `term.rs` today | With a tracked ref |
|---|---|
| `synced_history_lines` (physical rows, invalidated by reflow) | the anchor itself |
| `synced_logical_lines` (reflow-invariant proxy, `:421-425`) | unnecessary |
| `resync_after_reflow()` (`:631-649`) — walks history counting non-wrapped rows to re-derive the physical pointer | unnecessary |
| `pending_reflow_resync` (`:426-429`) — defers the above across alt-screen | unnecessary |

It also removes the documented trade-off in `terminal.md` §5 — "never *lose* a
line; bounded duplication (≤ screen height) is the accepted trade-off" — since
the anchor is exact rather than re-derived.

And critically for an offload design: `has_value() == false` is an explicit
"the emulator pruned rows we had not yet flushed" alarm. **Today Fresh has no
way to detect that at all** — its no-loss property is asserted, not checked.

Alacritty has no equivalent. This is the strongest single argument for the
switch, and it only becomes load-bearing once you shrink the window.

The other three groups the flush path needs all map cleanly:
`Point::History{x, y}` with y=0 = oldest (simpler than alacritty's negative
`Line(-n)`), `Row::is_wrapped()` / `is_wrap_continuation()` for logical-line
joining, and `scrollback_rows()` for the count **[verified]**.
`is_wrap_continuation()` in particular turns `append_visible_screen`'s
backward walk for an in-progress wrapped line (fresh#2649, `:982-987`) into a
direct query.

`grid_ref`'s "not for a render loop" caveat (§5.5) does **not** apply to the
flush path — flushing touches only rows that newly scrolled off, once per PTY
read, not the whole grid per frame.

### 6.2 But the scrollback knobs aren't reachable yet  **[verified]**

Ghostty `main`'s `vt.h` has exactly what an offload wants:

- `GHOSTTY_TERMINAL_OPT_SCROLLBACK_MAX_BYTES = 27` — cap by memory, the
  resource you're actually trying to reclaim
- `GHOSTTY_TERMINAL_OPT_SCROLLBACK_MAX_LINES = 28`
- both settable **at runtime** via `ghostty_terminal_set`, and "lowering the
  limit immediately removes eligible complete historical pages"

None of it is usable today:

- The pinned commit `a887df42c` stops at
  `GHOSTTY_TERMINAL_OPT_CLIPBOARD_WRITE = 26`. Neither scrollback option
  exists in the generated `libghostty-vt-sys` 0.2.1 bindings.
- Even once they land, `Terminal::get`/`set` are `pub(crate)` in the safe
  crate, so reaching them needs an upstream PR or a drop to `-sys`.

So as shipped: **construction-time `max_scrollback`, in lines, only** — the
same knob alacritty gives us. This is the concrete cost of the commit pin
from §5.3.

### 6.3 Page-granular pruning cuts both ways

Ghostty prunes at page granularity (~400 KB/page). The header is explicit:

> "the actual available scrollback lines will almost always be higher than
> configured… it ranges from dozens to a hundred or so lines."

- **Good**: you cannot accidentally under-provision below a page, so there is
  always a safety margin above whatever low-water mark you configure.
- **Bad**: you cannot tune the window tightly — a "just the screen plus a bit"
  target isn't achievable — and eviction is bursty (a whole page vanishes at
  once) rather than one row at a time as in alacritty.

Neither library offers an eviction callback, so "flush before pruning" stays a
cadence obligation either way. Ghostty just makes the failure *detectable*
(§6.1) instead of silent.

Scrollback compression composes well here: `compress(INCREMENTAL)` driven off
`compression_activity()` when idle, and "accessing compressed history
restores" it — so a large retained window costs less, and the rare deep read
still works. Both are present in the pinned commit **[verified]**.

### 6.4 The actual blocker is ours: flushing stops when history saturates

`flush_new_scrollback` (`term.rs:904-939`) indexes history rows as *offsets
from the oldest surviving row* and gates on:

```rust
let history = self.term.grid().history_size();
if history <= self.synced_history_lines {
    return Ok(0);
}
```

Once the emulator's history reaches its cap, `history` is pinned at the cap.
`synced_history_lines` also reaches the cap. The guard is then permanently
true and **streaming stops forever**. Worse, because both the counter and the
row index are relative to the oldest *surviving* row, they silently drift as
rows are pruned.

Reproduced against real `alacritty_terminal` 0.25.1 by replicating the
algorithm verbatim with `scrolling_history: 50`, `40x5`, flushing after
**every single line** (a far better cadence than Fresh's per-PTY-read):

```
emitted:        200 lines
history cap:    50
history_size(): 50
synced ptr:     50
flushed:        50 lines
first flushed:  Some("line-0000")
last flushed:   Some("line-0049")
MISSING from backing file: 150 lines
  first missing: line-0050, last missing: line-0199
```

Nothing mitigates it in production: `reset_sync_state()` (`:1218`) is only
called from a test, and `process_output`'s epoch reset (`:567-573`) only fires
when `history_after < history_before`, which never happens at a steady-state
cap.

Today this is masked — `SCROLLBACK_LINES = 200_000` (`:41`) means you need
200k lines of output in one terminal to hit it, so it reads as a long-session
edge case rather than a bug. **The offload plan inverts that**: the entire
point is to shrink the in-emulator window, which makes saturation the normal
operating state from the first screenful.

This must be fixed before any offload, and it is **backend-independent** —
switching to ghostty does not fix it by itself. The fix is the same shape
either way: stop indexing from "oldest surviving row" and anchor on something
stable. On alacritty that means a monotonically-increasing absolute row
counter maintained across prunes; on ghostty it is exactly what
`TrackedGridRef` provides for free.

---

## 7. If we did migrate: shape of the work

Behind a Cargo feature so both backends coexist during validation.

1. **Fix §6.4 first**, against alacritty. It is a live bug and a precondition
   for the offload regardless of backend.
2. **Extract a `VtBackend` trait** from the seven groups in §2. Pure refactor,
   no behaviour change. Worth doing on its own — it pins down exactly what we
   depend on and makes any future swap cheap.
3. **Resolve `!Send`** — actor-vs-`unsafe impl`. If actor, do it against
   alacritty so it is isolated from the backend swap. `RenderState`'s design
   (§5.5) means the actor split is the sanctioned shape anyway.
4. **Rewrite the render path onto a snapshot model** (§5.5) — also useful
   against alacritty, and required for ghostty.
5. **Implement the ghostty backend** behind `--features vt-ghostty`.
6. **Run the full e2e suite both ways**; triage mode defaults, reflow, cursor
   edges.
7. **Solve packaging** — Nix FODs for ghostty *and* its 22 Zig packages,
   Debian, AUR, and the `cargo install` story — before it can be the default.

Steps 1–4 are valuable on their own and carry no ghostty risk. Steps 5–7 are
where the cost is.

---

## 8. What to do now

- **Fix the saturation bug (§6.4).** Highest priority, live today,
  independent of everything else here.
- **Bump `alacritty_terminal` 0.25 → 0.26** (released 2026-04-06).
- **Extract the `VtBackend` trait** (step 2 above).
- **Handle OSC 9;9 in `Osc7Scanner`** — the Windows cwd win from §4.2, a few
  dozen lines, available today.
- **Fix `cursor_visible()`** (`:673`) — alacritty has `TermMode::SHOW_CURSOR`;
  the stub is ours.

### Triggers to revisit

Any two of:

- libghostty-vt tags a **stable C API**, or ghostty-org adopts an official
  Rust binding.
- A **Zig-free consumption path** — prebuilt static libs per target, or a
  vendored-C-source crate that builds with `cc`. (A vt-only `build.zig` that
  doesn't drag in the app's dependency graph would also help a lot.)
- We shrink the in-emulator scrollback for real, at which point
  `TrackedGridRef` (§6.1) stops being a nicety.
- We commit to the WASM/web build including a terminal.
- Wide-char/emoji corruption becomes a reported bug rather than a latent one.

---

## 9. How the verified claims were checked

- **API mapping (§3), `!Send` (§5.1), lifetimes:** a faithful port of
  `term.rs`'s core against `libghostty-vt` 0.2.1, type-checked with
  `DOCS_RS=1 cargo check` (which makes `build.rs` skip the native build, so
  the whole public API can be checked without Zig). Clean compile for the
  mapping; the two `E0277`s quoted in §5.1 for the `Arc<Mutex<…>>` shape.
- **Build requirements (§5.2):** `cargo build` without Zig, then with Zig
  0.15.2 on `PATH`, capturing the dependency fetches. Package list and byte
  count are from the actual fetch loop. The native link was **not** completed
  — `ocornut/imgui` is fetched directly from GitHub and this sandbox blocks
  non-scoped GitHub repos. That is an environment limit, not a crate defect;
  everything up to that point (Zig requirement, ghostty clone, 22 packages,
  ~68 MB) is observed.
- **Dependency-graph root cause (§5.2):** read from the pinned checkout's
  `build.zig` and `build.zig.zon` — only `uucode` is eager; the other 14 are
  `.lazy = true` but are still resolved because the unconditional
  `SharedDeps`/`GhosttyExe`/`GhosttyResources`/`GhosttyDocs`/`GhosttyWebdata`
  constructors call `b.lazyDependency` before the `emit_lib_vt` gates.
- **Missing scrollback options (§6.2):** `grep` of the published
  `libghostty-vt-sys` 0.2.1 `bindings.rs` and of the pinned checkout's
  `include/ghostty/vt/terminal.h` (`…OPT_CLIPBOARD_WRITE = 26` is the last
  entry) versus ghostty `main`'s header (which has 27 and 28).
- **Saturation bug (§6.4):** standalone crate replicating
  `flush_new_scrollback` verbatim against `alacritty_terminal` 0.25.1 with
  `scrolling_history: 50`; output quoted inline.
- **Crate metadata (§5.3):** crates.io API for `alacritty_terminal`,
  `libghostty-vt`, `libghostty-vt-sys`.

## Sources

- [libghostty-vt documentation](https://libghostty.tip.ghostty.org/)
- [ghostty `include/ghostty/vt.h`](https://github.com/ghostty-org/ghostty/blob/main/include/ghostty/vt.h) (and `vt/terminal.h`, `vt/osc.h`, `vt/grid_ref_tracked.h`)
- [Ghostty 1.3.0 release notes](https://ghostty.org/docs/install/release-notes/1-3-0)
- [`libghostty-vt` on docs.rs](https://docs.rs/libghostty-vt)
- [Uzaaft/libghostty-rs](https://github.com/Uzaaft/libghostty-rs)
- [Uzaaft/awesome-libghostty](https://github.com/Uzaaft/awesome-libghostty)
- [Libghostty Is Coming — Mitchell Hashimoto](https://mitchellh.com/writing/libghostty-is-coming)
