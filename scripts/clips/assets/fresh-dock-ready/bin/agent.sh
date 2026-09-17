#!/usr/bin/env bash
# One dock row's agent, parked in a chosen state at a chosen moment.
#
# The body is the repo's own fake coding agent
# (crates/fresh-editor/tests/fixtures/coding_agent.py, copied in by
# record.sh) -- a real scrolling agent transcript rather than a line
# printer, so the terminal the dock is reading looks like the thing it is
# meant to read. Nothing here tells the dock what to show.
#
# The dock derives a row's state from what its terminal said and when
# (orchestrator.ts: sessionState), so a state is staged by *when the output
# stops*:
#
#   working  never stops
#   done     a burst of at least WORK_MIN_MS (1.5s) while this window is NOT
#            the active one, then silence; the row flips at IDLE_AFTER_MS (5s)
#   blocked  the same burst, then a question -- matched before `done`, so it
#            wins
#   idle     one line: too short a burst to count as work, which is what
#            keeps it out of `done`
#
# Two graces have to be waited out first, and both are why `start` exists.
# Output in the 1.5s after a window becomes active is written off as the
# activation redraw, and output from a session that has not spoken yet is
# written off for 1.5s after any layout change -- and staging eleven
# workspaces is eleven layout changes in a row. So staging and working are
# two phases, and `start` is when the second one begins -- or, under a
# camera, the first shutter instead (see wait-for-start.py).
set -u
mode=${1:-working}; project=${2:-api}; start=${3:-0}; run=${4:-4}
CA="$(dirname "$0")/coding_agent.py"

"$(dirname "$0")/wait-for-start.py" "$start"

# `run_for` rather than a bare `timeout`: a foreground child killed by a
# signal makes bash print its own "Killed" job line onto the pane, which
# would be the last thing the dock's blocked-prompt heuristic reads and the
# last thing on screen. A backgrounded job killed the same way is silent.
run_for() { timeout -s KILL "$1" python3 "$CA" --as claude "$project" & wait $!; }

case "$mode" in
  working) exec python3 "$CA" --as claude "$project" ;;
  done)    run_for "$run" ;;
  blocked) run_for "$run"
           printf '\n\033[32m●\033[0m src/auth.rs rejects the expired token now.\n'
           printf '  Apply this change and re-run the suite? (y/n)\n' ;;
  idle)    sleep 2; printf '  worktree clean, nothing queued\n' ;;
esac
# Keep the PTY open: a row whose terminal exits reads `?`, not a state.
while :; do sleep 3600; done
