#!/usr/bin/env bash
#
# Reproduce the intermittent e2e::plugins hang under CPU contention.
#
# Background: the macOS CI runners intermittently time out (180s) on a handful
# of e2e::plugins::* tests. They HANG (they don't assertion-fail) and pass on
# re-run. The root cause is a plugin whose synchronous UI action depends on an
# async `git` round-trip that isn't ready yet; the async window widens under CPU
# contention, so the bug is rare on a fast idle box and common on the slower,
# parallel CI runners. This script recreates that pressure locally.
#
# It runs each target test in its OWN process (mirroring `cargo nextest`, which
# is process-per-test), N copies in parallel, under a pile of CPU hogs, with a
# per-test wall-clock budget well above the <2s norm. On a timeout it captures a
# gdb backtrace of every thread and the child process tree before killing the
# hung process, so you can see WHERE it is stuck.
#
# Usage:
#   cargo test --no-run --all-features --test e2e_tests   # build first
#   scripts/repro_plugin_hang.sh                          # defaults below
#   TESTS="e2e::plugins::live_diff::test_live_diff_clears_after_commit" \
#     ITERS=15 PAR=8 STRESS=8 TIMEOUT=30 scripts/repro_plugin_hang.sh
#
# Env knobs: BIN, TESTS (space-separated), ITERS, PAR, STRESS, TIMEOUT, OUT.
set -u

# Auto-detect the most recent e2e_tests test binary unless BIN is given.
BIN="${BIN:-$(ls -t target/debug/deps/e2e_tests-* 2>/dev/null | grep -v '\.d$' | head -1)}"
if [ -z "${BIN:-}" ] || [ ! -x "$BIN" ]; then
  echo "e2e_tests binary not found; build it first:" >&2
  echo "  cargo test --no-run --all-features --test e2e_tests" >&2
  exit 1
fi

TIMEOUT="${TIMEOUT:-45}"   # per-test seconds; normal run is <2s, CI kill is 180s
ITERS="${ITERS:-40}"
PAR="${PAR:-6}"            # concurrent copies per test (oversubscribe the cores)
STRESS="${STRESS:-8}"      # background CPU-hog workers
OUT="${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/fresh-plugin-hang.XXXXXX")}"
mkdir -p "$OUT"
echo "Artifacts (screens, stacks, process trees) -> $OUT"

# Default to the three known offenders; override with $TESTS.
if [ -n "${TESTS:-}" ]; then
  read -r -a TESTS_ARR <<< "$TESTS"
else
  TESTS_ARR=(
    "e2e::plugins::live_diff::test_live_diff_clears_after_commit"
    "e2e::plugins::orchestrator_new_dialog::ctrl_enter_submits_from_a_text_field"
    "e2e::plugins::review_diff_line_staging::test_review_visual_discard_single_added_line"
  )
fi

echo "Starting $STRESS CPU-hog workers..."
HOGS=()
for _ in $(seq 1 "$STRESS"); do
  ( while :; do : ; done ) &
  HOGS+=($!)
done
cleanup() { kill "${HOGS[@]}" 2>/dev/null; }
trap cleanup EXIT

run_one() {
  local test="$1" tag="$2"
  local log="$OUT/${tag}.log"
  # --nocapture so wait_until's periodic screen dump reaches the log.
  "$BIN" --test-threads=1 --nocapture --exact "$test" >"$log" 2>&1 &
  local pid=$!
  local waited=0
  while kill -0 "$pid" 2>/dev/null; do
    if [ "$waited" -ge "$TIMEOUT" ]; then
      echo "!!! HANG: $test (pid $pid) after ${TIMEOUT}s -> capturing"
      ps -o pid,ppid,stat,wchan:24,comm,args --ppid "$pid" \
        >"$OUT/${tag}.children.txt" 2>&1
      if command -v gdb >/dev/null 2>&1; then
        gdb -p "$pid" -batch -ex "set pagination off" \
          -ex "thread apply all bt" >"$OUT/${tag}.gdb.txt" 2>&1
      fi
      kill -9 "$pid" 2>/dev/null
      echo "HANG:$test:$tag" >>"$OUT/hangs.txt"
      return 99
    fi
    sleep 1
    waited=$((waited + 1))
  done
  wait "$pid"
  return $?
}

fail=0
for iter in $(seq 1 "$ITERS"); do
  pids=()
  slot=0
  for t in "${TESTS_ARR[@]}"; do
    for _ in $(seq 1 "$PAR"); do
      run_one "$t" "iter${iter}_slot${slot}" &
      pids+=($!)
      slot=$((slot + 1))
    done
  done
  for p in "${pids[@]}"; do
    wait "$p" || { rc=$?; [ "$rc" = 99 ] && fail=1; }
  done
  echo "iter $iter done (hangs so far: $( [ -f "$OUT/hangs.txt" ] && wc -l < "$OUT/hangs.txt" || echo 0 ))"
  [ "$fail" = 1 ] && { echo "Stopping after first hang batch (see $OUT)."; break; }
done

echo "=== DONE. Hangs: ==="
[ -f "$OUT/hangs.txt" ] && cat "$OUT/hangs.txt" || echo "(none)"
