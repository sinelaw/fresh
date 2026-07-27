# libghostty-vt vs `alacritty_terminal` for Fresh's embedded emulator

Status: **analysis / no change recommended yet** (July 2026)
Scope: the VT *output* emulation library behind `services/terminal/term.rs`.
Cross-ref: `docs/internal/terminal.md` §2.

> This is a comparison of two *embeddable libraries*, not two terminal apps.
> Fresh does not run inside Alacritty or Ghostty; it embeds
> `alacritty_terminal` as a crate to emulate the PTY it spawns. The
> Ghostty-side equivalent is **libghostty-vt**, the C library extracted from
> Ghostty's core, reached from Rust through the community `libghostty-vt`
> crate.

---

## 1. Verdict

**Don't switch now. Revisit when libghostty-vt tags a stable release and a
Zig-free build path exists.**

The API surface fits — every single thing `term.rs` asks of
`alacritty_terminal` has a libghostty-vt counterpart, and several things we
hand-rolled (OSC 7 sniffing, OSC 133 activity, mouse/key encoding) are
first-class there. The problem is not capability. It is three things we
cannot absorb today:

1. **`Terminal` is `!Send`/`!Sync`.** We hold `Arc<Mutex<TerminalState>>`
   shared between the PTY reader thread and the render path
   (`manager.rs:55`, `app/terminal.rs:2363`). That construction does not
   compile against a `!Send` emulator without either an unsanctioned
   `unsafe impl Send` or restructuring the terminal into an
   actor-behind-a-channel.
2. **Zig 0.15.x becomes a hard build dependency**, plus a network `git clone`
   of ghostty at a pinned commit inside `build.rs`. That lands on
   `cargo install fresh`, six dist targets, musl, Debian, AUR, Nix, and the
   min-size build.
3. **Pre-1.0 on both sides of the FFI.** libghostty-vt's own header says
   "incomplete, work-in-progress API… definitely going to change"; the Rust
   binding is a single-maintainer community crate (4 releases in 4 months)
   pinned to a specific ghostty commit.

Against ~4,900 lines of terminal e2e tests and 39 unit tests encoding
alacritty's exact grid/reflow semantics, that is a large bill for benefits
we can mostly get incrementally.

---

## 2. What we actually use

The coupling is narrow and lives in exactly one file. `term.rs` is 2,177
lines; the alacritty-facing part is maybe 80. Everything else in Fresh talks
to our own `TerminalState` / `TerminalCell`.

Imports (`term.rs:30-35`):

```rust
alacritty_terminal::event::{Event, EventListener}
alacritty_terminal::grid::{Scroll, Dimensions}
alacritty_terminal::index::{Column, Line}
alacritty_terminal::term::test::TermSize
alacritty_terminal::term::{Config as TermConfig, Term, TermMode}
alacritty_terminal::term::cell::Flags
alacritty_terminal::vte::ansi::{Processor, Color, NamedColor}
```

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

- `Osc7Scanner` (`:109-200`) — a resumable byte-level state machine for
  `ESC ] 7 ; file://…`, because alacritty's OSC dispatcher discards OSC 7 and
  `Handler` has no cwd hook.
- `OscActivityScanner` — same treatment for OSC 133 command lifecycle and
  OSC 9;4 progress, driving the workspace working/idle dot.

Notably we do **not** use alacritty for: selection, search, key encoding,
mouse encoding, colors/palette (we hardcode a 16-colour table at
`:1286-1301`), damage tracking, or its `tty` module (we use `portable-pty`).

---

## 3. Capability mapping

Every group above maps. Column three is what changes for us.

| | `alacritty_terminal` 0.25 | libghostty-vt 0.2.1 | Delta for Fresh |
|---|---|---|---|
| A | `Processor::advance` | `Terminal::vt_write(&[u8])` | 1:1. `TerminalOptions{cols, rows, max_scrollback}` replaces `TermConfig` |
| B | `grid[Line][Column]` direct index | `Terminal::grid_ref(Point::Viewport{..})` → `GridRef::{row,cell,style}` | Cell is **opaque**; char via `codepoint()`, attributes via `style()` lookup. Viewport scroll is already baked into `Point::Viewport` — our `display_offset` arithmetic disappears |
| C | `history_size()` + `Line(-n)` + `WRAPLINE` on last col | `scrollback_rows()` + `Point::History{..}` + `Row::is_wrapped()` / `is_wrap_continuation()` | **Better.** Two explicit flags instead of inferring continuation from the previous row's last cell. `append_visible_screen`'s backward walk (`:982-987`) becomes a direct `is_wrap_continuation()` test |
| D | `Term::resize`, reflow | `Terminal::resize(cols, rows, cell_w_px, cell_h_px)`, reflow | Same model. Our `resync_after_reflow` (`:631`) still needed, but reads cleaner |
| E | `TermMode` bitflags | `Mode::{ALT_SCREEN, DECCKM, NORMAL_MOUSE, BUTTON_MOUSE, ANY_MOUSE, SGR_MOUSE, ALT_SCROLL, …}` + `is_mouse_tracking()` | All six present. `is_mouse_tracking()` replaces our 3-flag `intersects` |
| F | `EventListener::send_event` | `on_pty_write`, `on_title_changed` callbacks | 1:1, and typed rather than a match on one `Event` enum |
| G | `scroll_display(Scroll::Bottom)`, `grid().cursor.point` | `scroll_viewport(ScrollViewport)`, `cursor_x()`/`cursor_y()`, `scrollbar()` | 1:1, plus `scrollbar()` we don't have |
| — | *(nothing — we sniff it)* | `pwd()` + `on_pwd_changed` | **Deletes `Osc7Scanner`** (~100 lines + tests) |
| — | *(nothing — we sniff it)* | `Row::semantic_prompt()`, `Cell::semantic_content()`, `osc::CommandType::SEMANTIC_PROMPT` / `CONEMU_PROGRESS_REPORT` | Replaces most of `OscActivityScanner` |
| — | `cursor_visible()` → hardcoded `true` (`:673`) | `is_cursor_visible()` (DEC 25) | Fixes a known stub |
| — | one `char` per cell | grapheme clusters via `GridRef::graphemes(&mut [char])`, `Cell::wide()`, `CellWide` | Real ZWJ/emoji handling; our `TerminalCell{c: char}` currently can't represent them |
| — | none | `KeyEncoder`, `MouseEncoder`, `paste` validation, OSC 8 hyperlinks, Kitty graphics/keyboard, selection + HTML/VT formatters, scrollback compression | Could retire our hand-rolled key/mouse encoders |

**Everything we need is there.** That is the honest headline.

---

## 4. What we'd gain, ranked by how much we'd actually feel it

1. **OSC 7 for free, and more of it.** `GHOSTTY_TERMINAL_OPT_PWD_CHANGED`
   fires for OSC 7 **and OSC 9;9 (ConEmu) and OSC 1337 (iTerm2)**. Our
   scanner only handles OSC 7. OSC 9;9 is what PowerShell emits — this
   directly improves Ctrl+Click path resolution and terminal cwd tracking on
   Windows, which is exactly where we're weakest.
2. **Correct wide-char / grapheme handling.** `TerminalCell{c: char}` is a
   lossy model. Ghostty's `Cell::wide()`/`CellWide` + `graphemes()` is the
   right shape for emoji and combining marks in terminal output. This is a
   latent correctness bug for us, not a hypothetical.
3. **Shell integration as state, not as a sniffer.**
   `Row::semantic_prompt()` gives prompt/input/output per row from the
   emulator's own state. That is strictly more robust than
   `OscActivityScanner` guessing from the byte stream, and it's what
   click-to-move-cursor-in-prompt would need if we ever want it.
4. **Delete code.** `Osc7Scanner` + most of `OscActivityScanner` + the
   `display_offset` arithmetic + possibly our key/mouse encoders.
5. **Scrollback memory.** We set `SCROLLBACK_LINES = 200_000`. Ghostty offers
   *both* `SCROLLBACK_MAX_BYTES` and `SCROLLBACK_MAX_LINES`, plus caller-driven
   incremental scrollback compression (`compress(INCREMENTAL)` scheduled off an
   idle activity token). For a 200k-line budget that is a materially better
   memory story than alacritty's per-row `Vec<Cell>`.
6. **Throughput.** Ghostty's parser is SIMD-accelerated and its page-based
   storage is designed for exactly this. Plausibly faster on our
   `process_output` hot path — but **unmeasured for our workload**, and our
   hot path is currently dominated by `flush_new_scrollback`'s per-cell SGR
   reconstruction, not by parsing. Do not buy the migration on this.
7. **WASM.** `docs/wasm.md:276` lists `term.rs` as an *inherent* blocker
   because "alacritty_terminal is native-only" — which is true in practice:
   it drags in `libc`, `rustix-openpty`, `polling`, `signal-hook`, `miow`,
   `windows-sys` for a `tty` module we never use. libghostty-vt is
   zero-dependency (not even libc) and ships WASM utilities. **However** the
   Rust binding's `build.rs` `zig_target()` map has no `wasm32-*` entry and
   panics on unknown targets, so this win is theoretical today.

---

## 5. What it would cost

### 5.1 `!Send` / `!Sync` — the architectural blocker

`libghostty_vt::terminal::Terminal` documents `!Send + !Sync`, and the crate
guidance is to isolate emulation on one thread and talk to it over channels.

Fresh does the opposite:

```rust
// manager.rs:55
pub state: Arc<Mutex<TerminalState>>,
// manager.rs:382
let state = Arc::new(Mutex::new(TerminalState::new(cols, rows)));
```

The PTY reader thread calls `process_output` / `flush_new_scrollback`; the
render path calls `state.get_line(row)` synchronously per row
(`app/terminal.rs:1895`, `:2363`). `Arc<Mutex<T>>: Send` requires `T: Send`.

Two ways out, both real work:

- **`unsafe impl Send` on a newtype.** The `Mutex` does serialize access, and
  `!Send` here comes from raw FFI pointers rather than from thread-affine
  state — so this would very likely *work*. But it is explicitly outside what
  the library sanctions, on a pre-1.0 C API whose allocator behaviour we
  don't control. That's the kind of unsafe that is fine until it isn't.
- **Actor model.** Terminal lives solely on the reader thread; render pulls a
  snapshot over a channel. Correct, and arguably better regardless — but it
  changes the render path from "lock and read row N" to "own a frame
  snapshot", touching `manager.rs`, `app/terminal.rs`, and the split
  rendering path.

There is a second, smaller version of this: `Terminal<'alloc: 'cb, 'cb>`
carries lifetimes tied to its registered callbacks. Storing it in a
long-lived `Arc` means the callbacks must be `'static` (boxed/leaked), which
is doable but is more ceremony than alacritty's `EventListener` generic.

### 5.2 Build and distribution — the practical blocker

`libghostty-vt-sys`'s `build.rs` requires **Zig 0.15.x on `PATH`** and, by
default, `git clone --filter=blob:none` of ghostty at a pinned commit into
`OUT_DIR`. Escape hatches: `GHOSTTY_SOURCE_DIR`, `GHOSTTY_ZIG_SYSTEM_DIR`
(offline Zig package store), and a `pkg-config` feature. `DOCS_RS` is special-
cased to skip the native build.

What that hits, concretely:

| Consumer | Impact |
|---|---|
| `cargo install fresh` (we publish to crates.io via `cargo-publish`) | **Every user now needs Zig 0.15.x + git + network.** This is the worst one — it's a direct adoption tax on our simplest install path |
| 6 dist targets in `dist-workspace.toml` | Cross-compile is *supported* — all six triples are in `zig_target()`, incl. `aarch64-pc-windows-msvc`. CI runners need Zig installed |
| `musl-builds` job | `x86_64-unknown-linux-musl` / `aarch64-unknown-linux-musl` are mapped. OK |
| `min-size-build` job | Static `libghostty-vt.a` adds binary size; `LIBGHOSTTY_VT_SYS_OPTIMIZE=ReleaseSmall` exists but the delta is unmeasured |
| Nix (`flake.nix`) | Pure builds forbid network in the build phase → must pre-fetch ghostty as a fixed-output derivation and pass `GHOSTTY_ZIG_SYSTEM_DIR`. Doable, non-trivial |
| Debian (`debian/`) / AUR | Both want declared, offline, reproducible sources. A `build.rs` that clones from GitHub is a packaging problem, not a detail |
| `wasm32-*` | `zig_target()` panics — unsupported |

### 5.3 Maturity and maintenance

| | `alacritty_terminal` | libghostty-vt (Rust) |
|---|---|---|
| Latest | 0.26.0 (2026-04-06); we pin `"0.25"` → 0.25.1 | 0.2.1 (2026-07-18) |
| History | 0.24.0 → 0.26.0 over ~2 years, RC'd releases | first published 2026-03-28; 4 versions in 4 months |
| Downloads | ~899k total | ~32k total |
| Maintainer | Alacritty project | **Uzaaft (individual, community)** — not `ghostty-org` |
| Stability | de-facto stable, used by Zed, WezTerm-adjacent tooling, many TUIs | header says "incomplete, work-in-progress API… definitely going to change"; README: "pre-1.0 … do not guarantee compatibility with arbitrary installed C API revisions" |

Ghostty 1.3.0 (2026-03-09) states libghostty's release cycle is now separate
from the GUI's, that the Zig module is "full featured" while the **C API is
still in progress**, and that they "aren't sure yet when we'll tag the first
libghostty releases." The C API is the only one we can reach from Rust. There
is no official ghostty-org Rust binding — Rust is one of ~15 community
bindings in `awesome-libghostty`.

Mitchell has said libghostty may end up more influential than the Ghostty app
itself, and dozens of projects already use it. The direction is good. It is
just not a thing to bet a shipped editor's terminal on in July 2026.

### 5.4 Test surface

`tests/e2e/terminal.rs` is 4,891 lines / ~163 tests, plus 39 unit tests in
`term.rs`. A meaningful share encode alacritty's exact behaviour — e.g. the
cursor-reported-at-y=60 quirk (`terminal.rs:1149`) and
`ALTERNATE_SCROLL`-on-by-default (`:3630`, and `app/terminal.rs:1968`).
Ghostty's mode defaults are not guaranteed to match. Every one of those needs
re-validation, and the scrollback streaming invariants (never lose a line;
bounded duplication) need re-proving against a different reflow implementation.

### 5.5 Per-cell FFI in the render path

`get_line` currently indexes memory directly. Against libghostty-vt each cell
costs a `grid_ref` + `row`/`cell`/`style` lookups across the FFI boundary. For
a 200×50 pane that's tens of thousands of calls per frame. These are cheap
static-lib calls (no marshalling), so probably fine — and the intended fast
path is `render::RenderState` with `RowIterator`/`CellIterator` rather than
per-cell `grid_ref`. But it means `get_line`'s shape changes, and `GridRef`
is only valid until the next terminal update, so anything we read must be
copied out immediately.

---

## 6. If we did migrate: shape of the work

Phased, behind a Cargo feature so both backends can coexist during validation.

1. **Extract a `VtBackend` trait** from the 7 capability groups in §2. Pure
   refactor against alacritty, no behaviour change. This is worth doing
   *regardless* — it's the thing that makes any future swap cheap, and it
   pins down exactly what we depend on.
2. **Resolve `!Send` first, not last.** Decide actor-vs-`unsafe impl`. If
   actor, do it against alacritty so the change is isolated from the backend
   swap.
3. **Implement the ghostty backend** behind `--features vt-ghostty`.
4. **Run the full e2e suite both ways**, triage divergence (mode defaults,
   reflow, cursor edges).
5. **Solve packaging** — Nix FOD, Debian, AUR, and the crates.io install
   story — before it can become the default.

Steps 1 and 2 are the valuable ones and are useful on their own. Steps 3-5
are where the cost is.

---

## 7. What to do instead, now

- **Bump `alacritty_terminal` 0.25 → 0.26.** Released 2026-04-06, we're a
  minor behind. Cheap.
- **Do step 1 above** (the `VtBackend` trait). It costs little, documents our
  real dependency, and is the precondition for ever swapping.
- **Handle OSC 9;9 in `Osc7Scanner`.** That's the concrete Windows cwd win
  from §4.1, available today for a few dozen lines.
- **Fix `cursor_visible()`** (`term.rs:673`) — alacritty does track DEC 25;
  the stub is ours, not the library's.
- **Consider `libghostty-vt`'s `KeyEncoder`/`MouseEncoder` independently.**
  They're usable without adopting the whole emulator — though they carry the
  same Zig build cost, so probably not worth it alone.

## 8. Triggers to revisit

Any two of these:

- libghostty-vt tags a **1.0 / stable C API**, or ghostty-org adopts an
  official Rust binding.
- A **Zig-free consumption path** exists — prebuilt static libs per target,
  or a vendored-C-source crate that builds with `cc`.
- We commit to the **WASM/web build** including a terminal, where
  alacritty_terminal is a genuine blocker and libghostty-vt is not.
- Wide-char/emoji corruption in terminal output becomes a real reported bug
  rather than a latent one.

---

## Sources

- [libghostty-vt documentation](https://libghostty.tip.ghostty.org/)
- [ghostty `include/ghostty/vt.h`](https://github.com/ghostty-org/ghostty/blob/main/include/ghostty/vt.h) (and `vt/terminal.h`, `vt/osc.h`)
- [Ghostty 1.3.0 release notes](https://ghostty.org/docs/install/release-notes/1-3-0)
- [`libghostty-vt` on docs.rs](https://docs.rs/libghostty-vt)
- [Uzaaft/libghostty-rs](https://github.com/Uzaaft/libghostty-rs)
- [Uzaaft/awesome-libghostty](https://github.com/Uzaaft/awesome-libghostty)
- [Libghostty Is Coming — Mitchell Hashimoto](https://mitchellh.com/writing/libghostty-is-coming)
- crates.io API for `alacritty_terminal`, `libghostty-vt`, `libghostty-vt-sys`
