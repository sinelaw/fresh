# Profiling

Purpose: an index of the measurement tools in this tree — what each one
answers, and which to reach for. Each tool documents its own usage; this page
exists so you do not have to know their names to find them.

Status: IMPLEMENTED. Everything listed here is in the tree and runs today.

## What to reach for

| Question | Tool |
|---|---|
| Where does the editor's memory go, per subsystem and per allocation site? | [`scripts/memory-profile.py`](../../scripts/memory-profile.py) — see [memory-profiling.md](memory-profiling.md) |
| How long does the editor take to show a usable screen? | `cargo run --release --bin measure_startup --features dev-bins,runtime` |
| How many bytes does a keystroke cost the terminal (the "lag over ssh/serial" question)? | [`scripts/serial_lag_bench.py`](../../scripts/serial_lag_bench.py), and [`serial_lag_diagnose.py`](../../scripts/serial_lag_diagnose.py) to attribute it to settings |
| How does the renderer behave on pathological input? | [`scripts/gen-single-line-bench.py`](../../scripts/gen-single-line-bench.py) generates a 500 KB single-line file |

They share a shape worth knowing about: each drives the **real binary** over a
real pseudo-terminal with a scripted workload, because an editor that nothing
is typing at only reports on startup. `tests/common/pty.rs` is the same idea
for tests.

## The `profiling` build profile

`[profile.profiling]` in the root `Cargo.toml` is what every profiler here
should run against. It inherits `release` — same `opt-level = "z"`, so
allocation and code behaviour match a shipped binary — and changes only what a
profiler needs:

| | why |
|---|---|
| `debug = 1` | line tables, so a stack frame reads `syntect::parsing::…​ (parser.rs:412)` rather than a bare address |
| `strip = false` | `dist` strips, and a stripped binary profiles as one anonymous frame |
| `lto = false`, `codegen-units = 16` | fat LTO inlines across crate boundaries until allocations can no longer be attributed to the crate that made them — and it is a ~20 minute single-threaded link |

```sh
cargo build --profile profiling --bin fresh
```

Do not profile a `dev` build: `[profile.dev]` sets `debug = 0` (see
CONTRIBUTING's *Dev Build Speed*), and unoptimized code allocates differently
enough that the results mislead.

## Reading any of them

Two habits that these tools are built around, and that their output only makes
sense under:

- **Say which workload.** Every number here is "on this workload", and the
  ranking changes with it — memory profiling found the retained-mode UI
  holding 36% of the heap on one workload and 3% on another, because the peak
  landed in a different place. A figure without its workload is not a result.
- **Separate footprint from churn.** What is live at the peak and what passes
  through the allocator are different questions with different owners; syntax
  highlighting holds ~3% of Fresh's heap and does ~59% of its allocation.
