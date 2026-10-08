#!/usr/bin/env bash
# Start the work that is running right now on the demo machine: a tmux
# server with a live claude in web-dashboard, and a dev shell in
# payments-api. The Import sessions dialog finds these by asking tmux, so
# they have to be up while the clip films -- record.sh starts them before the
# capture and teardown.sh stops them after.
#
# The server lives under $DEMO_HOME/.tmux (TMUX_TMPDIR), and the clip's
# editor is given the same TMUX_TMPDIR, so the scan sees this server and
# never one of yours.
set -euo pipefail
DEMO_HOME="${DEMO_HOME:-/home/dev}"
export TMUX_TMPDIR="$DEMO_HOME/.tmux"
mkdir -p "$TMUX_TMPDIR"

# Same environment scrub as make-fixture.sh: a claude started from inside
# another Claude Code session would otherwise adopt the parent's session id.
CLEAN=(env -u CLAUDECODE -u CLAUDE_CODE_SESSION_ID -u CLAUDE_CODE_ENTRYPOINT
       -u CLAUDE_CODE_REMOTE_SESSION_ID -u CLAUDE_CODE_CHILD_SESSION
       -u TMUX -u TMUX_PANE HOME="$DEMO_HOME")

DEMO_HOME="$DEMO_HOME" "$(dirname "${BASH_SOURCE[0]}")/teardown.sh"

# Back to the store make-fixture.sh left: no transcript from a previous
# take's live claude, nothing appended by a previous take's import.
[ -d "$DEMO_HOME/.clip-pristine" ] || { echo "start-live: run make-fixture.sh first" >&2; exit 1; }
rm -rf "$DEMO_HOME/.claude/projects"
cp -a "$DEMO_HOME/.clip-pristine" "$DEMO_HOME/.claude/projects"

W="$DEMO_HOME/src/web-dashboard"
P="$DEMO_HOME/src/payments-api"

# A real interactive claude, asked something read-only so it answers and
# then sits at its prompt rather than stopping to ask for a permission.
"${CLEAN[@]}" tmux new-session -d -s dashboard -n review -x 160 -y 48 -c "$W" \
  "claude --model ${CLAUDE_MODEL:-sonnet} 'Walk me through how RevenueChart fills in days that have no revenue. Read-only, do not edit anything.'"
"${CLEAN[@]}" tmux new-session -d -s api -n server -c "$P" "bash --noprofile --norc -i"

# The tmux scanner reports a pane as an agent only once its foreground
# process is called `claude`; wait for that rather than racing it.
for _ in $(seq 1 60); do
  if "${CLEAN[@]}" tmux list-panes -a -F '#{pane_current_command}' | grep -qx claude; then
    echo "live: tmux sessions $("${CLEAN[@]}" tmux list-sessions -F '#S' | paste -sd' ')"
    exit 0
  fi
  sleep 0.5
done
echo "start-live: claude never became the dashboard pane's foreground command" >&2
"${CLEAN[@]}" tmux list-panes -a -F '#S #{pane_current_command}' >&2
exit 1
