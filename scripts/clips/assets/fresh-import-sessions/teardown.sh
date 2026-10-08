#!/usr/bin/env bash
# Stop what start-live.sh started, and wait until it has stopped. The fixture
# itself stays.
#
# The wait is the point: a claude told to exit writes its last records on the
# way out, so restoring the store under one that is still going leaves a
# transcript of the previous take's live session in this one.
set -uo pipefail
DEMO_HOME="${DEMO_HOME:-/home/dev}"
export TMUX_TMPDIR="$DEMO_HOME/.tmux"
# Every pane's process and its children, taken before the kill: once the
# server is gone they are reparented and no longer findable from the pane.
pids=""
for pid in $(tmux list-panes -a -F '#{pane_pid}' 2>/dev/null); do
  pids="$pids $pid $(pgrep -P "$pid" 2>/dev/null | tr '\n' ' ')"
done
tmux kill-server 2>/dev/null
for pid in $pids; do
  for _ in $(seq 1 100); do kill -0 "$pid" 2>/dev/null || break; sleep 0.1; done
done
exit 0
