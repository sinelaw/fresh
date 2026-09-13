# Memory Profiling

Purpose: how to get a breakdown of where `fresh` spends memory at runtime —
per subsystem and per allocation site, not just a total — and how to read
the result.

Status: IMPLEMENTED. `scripts/memory-profile.py` and the `profiling` cargo
profile are in the tree; nothing here is planned work.

## 1. The problem

`fresh` is a TUI editor, so there is nothing to measure unless something is
driving it: a binary that starts, paints one frame and exits tells you about
startup, not about editing. And the number most tools hand back — one peak
RSS figure — does not say which subsystem to go look at. Highlighting,
grammars, the plugin runtime, the text model and the terminal emulator all
allocate, and they allocate very differently from each other.

So the harness does two things: it *drives* the editor through a fixed
workload over a real pseudo-terminal, and it attributes what was allocated
back to the code that asked for it.

## 2. Running it

```sh
cargo build --profile profiling --bin fresh
scripts/memory-profile.py                 # heap breakdown (Valgrind massif)
scripts/memory-profile.py --tool rss      # RSS timeline, no Valgrind, seconds
scripts/memory-profile.py --tool dhat     # allocation churn and lifetimes
scripts/memory-profile.py --files a.rs b.ts               # your own files
scripts/memory-profile.py --workload orchestrator --verbose   # the loaded editor
```

Two workloads, because they answer different questions:

| `--workload` | what it drives | what it tells you |
|---|---|---|
| `edit` (default) | three files opened on the command line, walked end to end, typed into, saved | what a buffer and its highlighting cost |
| `orchestrator` | several workspaces, each a git worktree with its own window, terminal, file explorer and buffers, over a throwaway clone of the repo | what the editor costs when it is actually loaded, and what each additional workspace adds |

`--verbose` logs each step as it runs, with its wall time and how many bytes
the editor painted — the first thing to look at when a workload stops
behaving.

Output lands in `target/memory-profile/`. The massif run also writes an
`ms_print` report next to the raw profile with the full allocation tree and
massif's ASCII graph of heap over time.

### The `profiling` profile

Build with `cargo build --profile profiling --bin fresh`, never a `dev` build:
a stripped or fat-LTO binary profiles as a handful of anonymous frames, and
`[profile.dev]` carries no line tables. [profiling.md](profiling.md) has the
profile's settings and the reasoning behind each one.

## 3. What the harness does

1. Allocates a pty and launches the binary on its slave end. The editor
   reopens `/dev/tty` and refuses to run without one, so a pipe will not do.
   This is the same shape as `tests/common/pty.rs` and
   `scripts/serial_lag_bench.py`.
2. Points `HOME` and every `XDG_*` variable at a throwaway directory, so the
   profile reflects the editor rather than the machine's config, plugins or
   restored session, and copies the workload files there — the workload types
   into them.
3. Feeds a fixed keystroke script: walk each buffer end to end, page through
   it, type, save, switch buffer. Same script every run, so two runs are
   comparable. Between steps it waits for the editor to stop writing to the
   terminal rather than sleeping a fixed amount — under Valgrind every step is
   tens of times slower, and a fixed sleep would race the repaint.
4. Quits with Ctrl+Q and waits for a *clean* exit. This matters: Valgrind
   writes its profile on exit, so a run that ends in SIGKILL produces nothing.
   The workload saves before quitting so no "buffer is modified" prompt can
   hold the shutdown up.

## 4. Reading a massif breakdown

massif's tree is rooted at `malloc` and grows *outwards*: a node's children
are its callers, and a node's bytes include everything its children account
for. The summary charges each stack to the innermost frame that names a real
subsystem — the code that actually asked for the memory — and then charges
nothing further out on that stack, which is what keeps the roll-up from
double counting. Frames that are pure allocator plumbing (`alloc::`,
`core::`, `Vec` growth) are never an answer, so they are skipped on the way
out and excluded from the "largest sites" list.

Two caveats worth knowing before drawing conclusions:

- **massif measures the heap, not RSS.** The binary's own mapped pages,
  thread stacks and anything obtained by `mmap` directly are not in it. Pass
  `--pages-as-heap` for a total-mapped-memory view, or cross-check with
  `--tool rss`, which reads `/proc/<pid>/smaps_rollup` on a native run.
- **Valgrind replaces the allocator.** Sizes are real; the fragmentation and
  arena behaviour around them are Valgrind's, not glibc's. `--tool rss` is
  the ground truth for "how much memory does this actually take".

`--tool dhat` answers what massif cannot: how many times a block was read or
written, and how long it lived. That is the tool for "this is allocated once
per keystroke" and for "this is parsed at startup, read once, and held for
the life of the process". Its output loads into
<https://nnethercote.github.io/dh_view/dh_view.html> (locally, in the
browser — the file is not uploaded).

## 5. A measured baseline

Run on 2026-09-13, `--profile profiling`, workload = `main.rs` (~7.3k lines
of Rust), `keybindings.rs`, `default.json`, terminal 120x40. Reproduce with
`scripts/memory-profile.py`; the raw profiles are kept in
`target/memory-profile/` so a run can be re-analyzed without re-running it.

### Resident memory, native (`--tool rss`)

| after | RSS | anon |
|---|---|---|
| process start | 9.7 MiB | 6.9 MiB |
| startup complete | 86.9 MiB | 56.8 MiB |
| editing the first buffer | 87.0 MiB | 56.9 MiB |
| second buffer opened + walked | 99.9 MiB | 69.7 MiB |
| third buffer opened | 100.3 MiB | 70.1 MiB |

Startup is where the memory goes: ~87 MiB before a key is pressed, and
editing a buffer costs nothing measurable after that. Each additional buffer
is ~10 MiB, most of it paid when it is first displayed rather than when it is
opened.

### Heap at the peak, by subsystem (`--tool massif`)

Peak heap 34.9 MiB (+3.0 MiB allocator overhead/fragmentation):

| subsystem | bytes | share |
|---|---|---|
| retained-mode UI (`fresh-ui`) | 12.5 MiB | 35.7% |
| QuickJS (plugin runtime) | 8.6 MiB | 24.6% |
| plugins (host side) | 3.0 MiB | 8.5% |
| editor state | 2.7 MiB | 7.6% |
| i18n / locales | 1.8 MiB | 5.1% |
| serde / json | 1.2 MiB | 3.5% |
| rendering (ratatui/crossterm) | 562 KiB | 1.6% |
| syntect | 294 KiB | 0.8% |
| unattributed | 4.4 MiB | 12.7% |

The largest single sites behind those rows:

| site | bytes |
|---|---|
| `js_malloc_rt` (quickjs.c) | 5.3 MiB |
| `Vec::with_capacity` of `fresh_ui::desc::Node<UiMsg>` | 5.2 MiB |
| `fresh_ui::element::Arena::alloc` (`Vec<Slot<UiMsg>>` growth) | 3.1 MiB |
| `fresh_i18n::store::register_locales` | 1.8 MiB |
| `fresh_plugin_runtime::thread::execute_prepared_plugin` | 1.7 MiB |
| `Vec` of `fresh_ui::element::Undo<UiMsg>` | 1.1 MiB |

### Allocation churn (`--tool dhat`)

Same workload, ~2 minutes of scripted editing: **1.7 GiB allocated across
5.1M blocks**, of which 35.2 MiB is live at the peak (an independent
confirmation of massif's 34.9 MiB, from a tool that counts differently) and
5.1 MiB is still live at exit.

Churn ranks the subsystems very differently from footprint:

| subsystem | allocated | share | blocks |
|---|---|---|---|
| syntect | 1.0 GiB | 59.1% | 1,738,881 |
| editor state | 311.4 MiB | 17.6% | 1,533,467 |
| oxc (TS transpile) | 121.2 MiB | 6.8% | 68,807 |
| retained-mode UI | 104.4 MiB | 5.9% | 336,083 |
| QuickJS | 99.4 MiB | 5.6% | 973,420 |

**Highlighting holds 3% of the heap and does 59% of the allocation.** Almost
all of it is Oniguruma, syntect's regex engine: `onig_search_with_param`
allocates ~5 KiB of match state per call (806 MiB over 166k calls) and
`onig_new_match_param` a 72-byte block per call (584,649 of them). Footprint
profiling would never have surfaced this; it is a CPU and allocator-pressure
problem wearing a memory profile.

dhat also records how much of each block was ever read or written, which
finds copies nobody asked for. The clearest one:

    site                                            allocated  blocks     read   written
    fresh_editor_core::model::buffer::TextBuffer::   22.0 MiB   5,901   5.8 KiB  22.0 MiB

`LineIterator::find_line_start_backward` calls `get_text_range_mut` to scan
backwards for a newline. That materializes a fresh `Vec<u8>` of the scan
chunk — ~3.8 KiB on average, fully written — and then `rposition` reads, on
this workload, a single byte of it before dropping it: 5,901 calls, 22 MiB
copied, 5,901 bytes actually read. It sits on the render path
(`line_token_stream` -> `build_base_tokens` -> `LineIterator::new`), so it
repeats per repaint. A backward scan that borrows from the piece tree
instead of copying out of it would delete the whole 22 MiB.

Of the 31.8 MiB allocated and never read at all, only 637 KiB was never even
written; the rest is write-only scratch (Oniguruma, QuickJS, and
`apply_theme_runs`, which writes 5.4 MiB nothing ever reads back).

### What this says

- **Footprint and churn have different owners.** The retained-mode UI is the
  largest thing live at this run's peak (the `desc::Node` tree is cloned
  wholesale, and the element arena and undo log grow alongside it — see
  [retained-mode-ui.md](retained-mode-ui.md)); highlighting holds under 4% of
  it and does 59% of the allocation. Ask which question you are asking before
  picking the tool. But see §6 before reading the UI's 12.5 MiB as a resting
  cost: on a workload whose peak lands elsewhere the same subsystem holds
  1.3 MiB, so this is a rebuild spike that the peak snapshot happened to
  catch.
- **The plugin runtime is a third of the heap** (QuickJS plus the host side),
  all of it established at startup. `--no-plugins` is therefore the single
  biggest lever on memory, and a useful A/B when profiling something else.
- **Heap is a minority of RSS**: 35 MiB of heap against 87-100 MiB resident.
  The rest is the mapped binary, thread stacks and allocator arenas, none of
  which massif sees. Do not quote the massif total as "how much memory fresh
  uses"; quote `--tool rss` for that.

## 5b. Comparing two runs

Run-to-run, the *totals* are not repeatable: an A/B of two binaries differing
only by two allocation fixes measured 1.8 GiB of churn against 2.4 GiB, with
Oniguruma — which neither change touches — accounting for the difference. The
workload is driven by wall-clock quiet windows, so a faster editor fits more
repaints (and so more parsing) into the same window. Faster code doing more
work per step is the profile behaving correctly; it is not a regression, and
it is not a result either.

So compare per-site, and normalize. The two fixes above, same workload, same
harness, one run each:

| | baseline | after | change |
|---|---|---|---|
| reconcile journal, bytes | 29.0 MiB | 2.5 MiB | **−91%** |
| reconcile journal, blocks | 1,138 | 140 | −88% |
| `get_text_range_mut`, bytes | 43.9 MiB | 14.7 MiB | **−67%** |
| `get_text_range_mut`, blocks | 69,289 | 68,365 | −1% |
| total churn | 1.8 GiB | 2.4 GiB | +38% |
| Oniguruma churn (parse volume) | 881 MiB | 1.5 GiB | +75% |

Both fixes read exactly as intended: the journal reallocates 88% less often,
and the line-start search performs the same number of reads (−1%) while
copying two thirds less. Normalized against Oniguruma churn as a proxy for how
much parsing a run did, they are −95% and −81%.

The general rule: a per-site figure at a comparable call count is evidence, a
total is not. If you want totals to mean something, hold the parse volume
fixed — same file, same scroll distance — and say so alongside the number.

## 6. A second baseline: the loaded editor

Run on 2026-09-13, same binary, `--workload orchestrator --workspaces 3`:
open `orchestrator-sessions.md` and the file explorer, then three times over
create a workspace (a git worktree, its own window and a bash terminal),
open its file explorer and three buffers, highlight one end to end, and run
`git --no-pager log` in its terminal -- all against a throwaway clone of this
repository, so the git plugins have a real repo to watch.

### Resident memory, native

| after | RSS | anon |
|---|---|---|
| process start | 10.3 MiB | 7.4 MiB |
| startup complete | 88.2 MiB | 58.2 MiB |
| file explorer open | 90.4 MiB | 59.9 MiB |
| workspace 1 fully up | 97.3 MiB | 66.6 MiB |
| workspace 2 fully up | 102.8 MiB | 72.1 MiB |
| workspace 3 fully up | 107.7 MiB | 77.1 MiB |

A whole workspace -- worktree, window, terminal, file explorer, three buffers
-- costs about **5.5 MiB**. Startup still dominates: 88 MiB before the user
does anything, 20 MiB for everything after.

### Heap at the peak: 47.0 MiB

| subsystem | bytes | share | the same subsystem in the `edit` workload |
|---|---|---|---|
| editor state | 13.2 MiB | 28.1% | 2.7 MiB |
| QuickJS (plugin runtime) | 9.0 MiB | 19.2% | 8.6 MiB |
| terminal emulation | 6.8 MiB | 14.4% | — (no terminals in it) |
| syntect | 3.6 MiB | 7.6% | 294 KiB |
| plugins (host side) | 3.0 MiB | 6.3% | 3.0 MiB |
| i18n / locales | 1.8 MiB | 3.8% | 1.8 MiB |
| retained-mode UI | 1.3 MiB | 2.8% | 12.5 MiB |
| serde / json | 1.2 MiB | 2.6% | 1.2 MiB |
| unattributed | 6.2 MiB | 13.1% | 4.4 MiB |

The last column is in bytes, not shares, on purpose: the two runs peak at
different totals (34.9 MiB and 47.0 MiB), so a share moving between them says
as much about the other subsystems as about this one. In bytes the split is
plain -- QuickJS, the plugin host and i18n are flat startup costs, editor
state and syntect grow with open buffers, and terminal emulation is new
because the editing workload has no terminals.

Three things this workload says that the editing one could not:

- **A terminal costs a flat 2 MiB before it emits a byte.** The largest
  single site in the whole profile is `vte::ansi::SyncState::default` at
  6.0 MiB: `Vec::with_capacity(SYNC_BUFFER_SIZE)` with `SYNC_BUFFER_SIZE =
  0x20_0000`, allocated eagerly per parser for DEC synchronized updates that
  most sessions never use. Three terminals, 6 MiB, 13% of the peak.
  Allocating it on first use is an upstream `vte` change. Alacritty's grid
  adds ~800 KiB per terminal on top, which is memory actually in use.
- **The plugin runtime does not scale with workspaces**: 9.0 MiB here against
  8.6 MiB with three files open. It is a fixed startup cost -- which is what
  makes it the biggest lever on footprint, and what makes it *not* a concern
  for heavy use.
- **The retained-mode UI's 35.7% in the editing profile was a spike, not a
  resting cost.** Here the same subsystem is 2.8%. `desc::Node` cloning is an
  allocation burst during rebuilds, so it belongs with the churn findings
  (§5) rather than the footprint ones -- and a peak-snapshot share is only
  ever "what was live at one instant", not "what this subsystem holds".

Editor state taking the top slot is mostly highlight spans (`spec_extend` of
`highlight_entries`, 2.4 MiB) and `ChromeLayout::reset_cell_*` (2.3 MiB),
across nine open buffers in three windows.

### Where the ~100 MiB actually is

massif only ever sees the heap, and the heap is a third of the process. The
rest is answered mapping by mapping out of `/proc/<pid>/smaps`, which
`--tool rss` now prints. Final state of the three-workspace run:

| mapping | RSS | share |
|---|---|---|
| anonymous (allocator arenas + thread stacks) | 48.2 MiB | 45.3% |
| heap (`brk`) | 25.3 MiB | 23.8% |
| editor binary: code | 16.5 MiB | 15.5% |
| editor binary: read-only data | 13.2 MiB | 12.4% |
| shared libraries | 2.8 MiB | 2.7% |
| main thread stack | 196 KiB | 0.2% |
| editor binary: data + relocations | 148 KiB | 0.1% |
| **total** | **106.4 MiB** | |

17 threads. Read it as three parts:

- **~30 MiB is the binary itself**, paged in as it runs: `.text` is 17.5 MiB
  and `.rodata` 11.7 MiB in this build, and nearly all of both ends up
  resident. Debug info is not in that -- `.debug_*` is mapped but never read,
  so the `profiling` profile's 199 MB on disk costs nothing at runtime. This
  is also the part a `release` build (fat LTO, `opt-level = "z"`) shrinks and
  this profile does not measure.
- **~74 MiB is the allocator** (`brk` plus anonymous), against the ~47 MiB
  massif reports as live allocations. The difference is the allocator's own
  bookkeeping, its free lists, and one stack per thread.
- **Everything else is noise**: stacks, relocations and kernel mappings
  together are under half a megabyte.

#### Inside the binary's ~30 MiB

The two binary rows are the parts of the file the kernel has actually paged
in. What is in them, from `nm` (code) and from the sections and the embedded
assets (read-only data):

**`.text`, 17.5 MiB — 16.5 MiB of it resident.** By crate:

| crate | size | share |
|---|---|---|
| `fresh` (the editor binary itself) | 3.29 MiB | 18.2% |
| `core` | 2.63 MiB | 14.5% |
| `fresh-editor-core` | 1.27 MiB | 7.0% |
| `alloc` | 1.24 MiB | 6.9% |
| hashbrown | 0.64 MiB | 3.5% |
| serde_json | 0.63 MiB | 3.5% |
| `std` | 0.61 MiB | 3.4% |
| `fresh-plugin-runtime` | 0.57 MiB | 3.1% |
| tokio | 0.54 MiB | 3.0% |
| serde | 0.49 MiB | 2.7% |
| QuickJS (C) | 0.34 MiB | 1.9% |
| rquickjs | 0.44 MiB | 2.5% |
| oxc (all crates) | ~0.5 MiB | 2.8% |
| rustls | 0.24 MiB | 1.3% |
| everything else + unnamed | ~4.7 MiB | 26% |

`core`, `alloc` and `std` are not the standard library sitting there being
large: they are *our* generic instantiations, monomorphized into the crate
that defines the generic. Read those rows as "the cost of how much we
instantiate", not as a dependency.

**The read-only mapping, ~21 MiB of sections — 13.2 MiB resident.** Three
things live there, and only the first is data:

| section | size | what it is |
|---|---|---|
| `.rodata` | 11.7 MiB | constants and embedded assets, below |
| `.eh_frame` + `.eh_frame_hdr` + `.gcc_except_table` | 7.0 MiB | unwinding and landing pads — what `panic = "abort"` drops |
| `.rela.dyn` | 2.6 MiB | load-time relocations |

And `.rodata` itself is mostly things we chose to embed:

| content | size |
|---|---|
| embedded plugins (`include_dir!` over `plugins/`) | 4.4 MiB (2.7 MiB of `.ts`, 1.7 MiB of per-plugin i18n JSON) |
| tree-sitter parse tables | 2.1 MiB |
| editor locale catalogs (15 JSON files) | 1.8 MiB |
| syntect dumps (`default_newlines` + `default_nonewlines` + themes) | 0.7 MiB |
| encoding_rs + chardetng tables | 0.15 MiB |
| string literals, panic messages, vtables, format strings | ~1.7 MiB |

Two of those rows are worth a second look. The locale catalogs are 1.8 MiB in
`.rodata` *and* 1.8 MiB on the heap, because `register_locales` copies each
one into a `Box<str>` (§5) — the same bytes twice. And syntect ships two
parse-table dumps, `newlines` and `nonewlines`, of which a given run uses
one.

One caveat that matters for reading the first two rows: this is a glibc,
dynamically linked build, and the split between `brk` and anonymous mappings
is glibc's allocator strategy, not Fresh's. The section below measures the
static musl release binary that actually ships, where the same session peaks
at 71.3 MiB rather than 106.4 -- most of the difference being allocator
overhead that only glibc has.

### The artifact that actually ships (static musl, release)

Everything above was measured on a `profiling` build: glibc, dynamically
linked, no LTO. Production is a static-PIE musl binary built with fat LTO and
`opt-level = "z"` (`.github/workflows/musl-builds.yml`). Same workload, same
harness, `--binary target/x86_64-unknown-linux-musl/release/fresh`:

| | profiling / glibc | release / musl |
|---|---|---|
| after startup | 88.2 MiB | **53.2 MiB** |
| peak, three workspaces | 106.4 MiB | **71.3 MiB** |
| per workspace | ~5.5 MiB | ~5.1 MiB |

| mapping | glibc | musl |
|---|---|---|
| allocator (`brk` + anonymous) | 73.5 MiB | **46.0 MiB** |
| binary: code | 16.5 MiB | 13.6 MiB |
| binary: read-only data | 13.2 MiB | 9.9 MiB |
| binary: data + relocations | 148 KiB | 1.0 MiB |
| shared libraries | 2.8 MiB | — (static) |

Three things to take from it:

- **The 26 MiB of "allocator overhead" was glibc's, and is not in production.**
  musl's mallocng holds 46.0 MiB against the ~47 MiB massif measures as live,
  so its overhead is small enough to disappear into the difference between two
  runs; glibc's per-thread arenas across 17 threads were the whole of it.
  (`brk` is 376 KiB here because musl mmaps nearly everything.)
- **LTO and `opt-level = "z"` take ~6 MiB off the binary's resident pages**
  (code 17.5 -> 14.2 MiB of section, unwind tables 6.3 -> 3.0 MiB, relocations
  2.6 -> 1.2 MiB), and static linking removes the 2.8 MiB of shared libraries.
  Static-PIE pays 1.0 MiB back in dirtied relocation pages.
- **The proportions shift, so the priorities do.** vte's eager sync buffers are
  8.4% of production RSS rather than 5.6%; `panic = "abort"` is worth 3.0 MiB
  here, not 7.0. Measure the shipped artifact before ranking anything.

### Not measured yet

No dhat profile for this workload. dhat instruments every memory access, and
on a workload that spawns worktrees and shells it runs several times slower
again than massif -- the run was still in its first workspace when it was
stopped. The open question it would answer: whether those 2 MiB sync buffers
are ever written to at all.

## 7. Reproducing these numbers yourself

Everything in this document comes out of two commands. Both take minutes, not
hours, and neither needs anything installed beyond what is listed here.

### What you need

Debian / Ubuntu:

```sh
# the profiler (massif, dhat and ms_print all ship with it)
sudo apt-get install -y valgrind

# only for the production figure: the musl target and its C toolchain,
# the same pair .github/workflows/musl-builds.yml installs
rustup target add x86_64-unknown-linux-musl
sudo apt-get install -y musl-tools
```

Arch:

```sh
sudo pacman -S --needed valgrind

rustup target add x86_64-unknown-linux-musl
sudo pacman -S --needed musl
export PATH="/usr/lib/musl/bin:$PATH"   # see below
```

Two Arch-specific things, both of which cost an afternoon if you meet them
without warning:

- **`musl-gcc` is not on `PATH`.** Arch's `musl` package puts it at
  `/usr/lib/musl/bin/musl-gcc`, where the `cc` crate — which builds
  Oniguruma and QuickJS for the musl target — will not find it. Either put
  that directory on `PATH` as above, or set
  `CC_x86_64_unknown_linux_musl=/usr/lib/musl/bin/musl-gcc`. Debian's
  `musl-tools` installs it as `/usr/bin/musl-gcc`, which is why this only
  bites here.
- **Valgrind breaks across a glibc bump**, with "a function redirection which
  is mandatory for this platform-tool combination cannot be set up" at
  startup. It means Valgrind is older than the glibc it is being pointed at;
  on a rolling release that is a normal Tuesday. Update `valgrind`, or side-step
  it entirely by profiling a *musl* build, which carries its own libc and needs
  no redirections: `cargo build --profile profiling --target x86_64-unknown-linux-musl`
  gives a binary with symbols that Valgrind will run whatever glibc is
  installed.

Building Fresh itself needs what the AUR package's `makedepends` name —
`cargo` (via `rustup`, since the workspace pins a toolchain) and `clang` —
plus `base-devel` for the C dependencies.

Python 3 with no third-party packages; Arch's `python` is already 3.x. Linux
only, either way: the harness reads `/proc/<pid>/smaps` and allocates a pty.

### The two runs

**Resident memory, as shipped.** The static musl release binary is the real
artifact, so it is the one to ask "how much memory does Fresh use":

```sh
cargo build --release --target x86_64-unknown-linux-musl --bin fresh
scripts/memory-profile.py --tool rss --workload orchestrator --workspaces 3 \
    --binary target/x86_64-unknown-linux-musl/release/fresh
```

~5 minutes, most of it the workload driving the editor at real speed. Prints
the RSS timeline step by step, the peak, and the `smaps` table that says what
each part of that memory *is*.

**Where the heap goes, by subsystem.** This one needs symbols, which the
release binary does not have, so it runs against the `profiling` build:

```sh
cargo build --profile profiling --bin fresh
scripts/memory-profile.py --tool massif --workload orchestrator --workspaces 3 --top 30
```

~10 minutes: Valgrind is 20-50x slower than native, and the workload spawns
three git worktrees inside that. Prints heap over time, then the peak snapshot
broken down by subsystem and by allocation site.

Add `--verbose` to either to watch the workload step by step — the first thing
to look at if a run stops behaving. Raw profiles are kept under
`target/memory-profile/`, so a run can be re-analyzed without re-running it,
and the massif run also writes an `ms_print` report beside its profile with
the full allocation tree.

### From a session you are driving yourself

The workloads exist so two runs are comparable. When the question is instead
"what is *this* session holding, right now, after the day I have just had",
there are two ways in and they answer different halves of it.

**Resident memory, live, costing nothing.** This only reads `/proc`, so the
editor does not notice it happened — and it works on the release binary,
since a mapping breakdown needs no symbols:

```sh
scripts/memory-profile.py --pid $(pgrep -x fresh)
```

Same table as the `--tool rss` run: RSS now, peak, thread count, and what each
part of the memory is. A one-file session a few seconds after startup reads
53.3 MiB against the three-workspace workload's 71.3, which is the shape to
expect: most of it is there before you open anything.

**Heap by subsystem.** Valgrind cannot attach to a running process, so the
session has to start under it. Use a build with symbols — the release binary
profiles as one anonymous frame:

```sh
cargo build --profile profiling --bin fresh
valgrind --tool=massif --massif-out-file=/tmp/fresh.massif \
    --detailed-freq=5 --max-snapshots=60 --threshold=0.2 --depth=20 \
    target/profiling/fresh
#   ... work in it as you normally would, then quit with Ctrl+Q
scripts/memory-profile.py --analyze /tmp/fresh.massif --top 30
```

Three things to know before you spend an afternoon in there. It will be 20-50x
slower — usable for a real session, but you will feel every keystroke. **The
exit has to be clean**: massif writes its profile when the process ends
normally, and a killed process leaves nothing at all, so quit the editor
rather than closing the terminal. And `--analyze` takes a dhat profile just as
happily, if `--tool=dhat --dhat-out-file=...` is what you ran.

### Where the clip numbers come from

`scripts/clips/memory-rss-breakdown.json` and `memory-live-data.json` are
transcriptions of those two outputs, and nothing else:

| clip slice | comes from |
|---|---|
| Working data 46 MB | rss table: `anonymous` 45.6 + `heap (brk)` 0.4 |
| Program code 13.6 MB | rss table: `editor binary: code` |
| Built-in data 9.9 MB | rss table: `editor binary: read-only data` |
| Startup fixups 1.0, Thread space 0.25 | rss table: the remaining two rows |
| 71 MB total | rss run: `Peak RSS (VmHWM)` |
| every slice of the live-data clip | massif: the `By subsystem` table |

The live-data clip merges massif's four smallest rows — the UI tree, serde
values, screen buffers and regexes — into one "Everything else" slice, and
rounds. Nothing else is editorial.

### If your numbers differ

They will, a little, and two differences are expected rather than
interesting. Parse volume varies between runs because the workload steps
forward on a quiet screen rather than a fixed schedule, so totals move by
tens of percent while per-site figures hold (see §5b). And the `orchestrator`
workload clones this repository into a throwaway directory and cuts branches
in the clone, so a different checkout means different files, different
highlighting, and a different number on every row.
