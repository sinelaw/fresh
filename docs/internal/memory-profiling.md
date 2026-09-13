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
scripts/memory-profile.py --files a.rs b.ts   # your own workload files
```

Output lands in `target/memory-profile/`. The massif run also writes an
`ms_print` report next to the raw profile with the full allocation tree and
massif's ASCII graph of heap over time.

### The `profiling` profile

`[profile.profiling]` in the root `Cargo.toml` inherits `release` — same
`opt-level = "z"`, so allocation behaviour matches a shipped binary — and
changes only what a profiler needs:

| | why |
|---|---|
| `debug = 1` | line tables, so a stack frame reads `syntect::parsing::…​ (parser.rs:412)` rather than a bare address |
| `strip = false` | `dist` strips; a stripped binary profiles as one anonymous frame |
| `lto = false`, `codegen-units = 16` | fat LTO inlines across crate boundaries until allocations can no longer be attributed to the crate that made them — and it is a ~20 minute single-threaded link |

Do not profile a `dev` build: `[profile.dev]` sets `debug = 0` (see
CONTRIBUTING), and unoptimized code allocates differently enough that the
breakdown misleads.

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

Three things worth knowing from this:

- **The retained-mode UI is the biggest heap consumer, not highlighting.**
  The `desc::Node` tree is cloned wholesale and the element arena and undo
  log grow alongside it — see [retained-mode-ui.md](retained-mode-ui.md) for
  what those are. Syntax highlighting, the usual suspect, is under 1%.
- **The plugin runtime is a third of the heap** (QuickJS plus the host side),
  all of it established at startup. `--no-plugins` is therefore the single
  biggest lever on memory, and a useful A/B when profiling something else.
- **Heap is a minority of RSS**: 35 MiB of heap against 87-100 MiB resident.
  The rest is the mapped binary, thread stacks and allocator arenas, none of
  which massif sees. Do not quote the massif total as "how much memory fresh
  uses"; quote `--tool rss` for that.
