#!/usr/bin/env bash
# Stress-test the embedded plugin extraction race fix.
#
# What the race looked like (pre-fix): when many test processes start
# in parallel (cargo nextest), each independently calls `extract_plugins`.
# The pre-fix code's "is the cache_dir populated?" check returned true
# the moment process A wrote its first file — process B then started
# loading plugins from a half-extracted directory and saw failures
# like:
#
#   "Bundling failed for find_references: Cannot resolve import './lib/finder.ts'"
#   "TypeError: not a function" at clangd_support.ts:N
#
# This script wipes the embedded cache and spawns N parallel test
# processes that all race on extraction. With the fix, every process
# either publishes its own atomic extraction or falls back to a
# concurrent winner's, and every test passes.
#
# Usage:  scripts/stress-extract-race.sh [N]   (default N=16)

set -u

N="${1:-16}"

cargo test -p fresh-editor --test semantic_tests --no-run 2>&1 | tail -3
binary=$(find target/debug/deps -maxdepth 1 -name 'semantic_tests-*' -type f -executable -printf '%T@ %p\n' \
    | sort -nr | head -1 | awk '{print $2}')
if [[ -z "$binary" || ! -x "$binary" ]]; then
    echo "Could not locate semantic_tests binary" >&2
    exit 1
fi
echo "Using binary: $binary"

rm -rf "$HOME/.cache/fresh/embedded-plugins" "$HOME/.cache/fresh/plugin-prepare"
echo "Cache wiped. Spawning $N processes…"

logdir=$(mktemp -d)
pids=()
for i in $(seq 1 "$N"); do
    "$binary" theorem_sort_lines_basic_alphabetical \
        > "$logdir/run.$i.out" 2>&1 &
    pids+=("$!")
done

failures=0
for pid in "${pids[@]}"; do
    if ! wait "$pid"; then
        failures=$((failures + 1))
    fi
done

if [[ "$failures" -gt 0 ]]; then
    echo "FAIL: $failures of $N processes errored. Sample stderr:" >&2
    for f in "$logdir"/*.out; do
        if grep -q FAILED "$f"; then
            echo "── $f ──" >&2
            tail -20 "$f" >&2
            break
        fi
    done
    exit 1
fi
echo "OK: all $N processes passed."
