#!/bin/bash
# Bring up the Workboard scenario in an isolated home, in tmux session `wb`.
#
#   scripts/workboard-scenario/run.sh [DIR]     (default: a fresh mktemp dir)
#   tmux attach -t wb
#
# Then, in the editor: run "Scenario: Build" from the palette (Ctrl+P), wait
# ~15 s for the agents to settle, and run "Workboard: Open". "Scenario:
# Churn" renames, archives, deletes and creates workspaces under an open
# board; "Scenario: Dump" writes the Orchestrator's view to DIR/dump.json.
#
# Uses the `fresh` on PATH (put target/debug first to test a build).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIR="${1:-$(mktemp -d)}"
mkdir -p "$DIR/home/.config/fresh" "$DIR/repos"
export HOME="$DIR/home" XDG_CONFIG_HOME="$DIR/home/.config" XDG_DATA_HOME="$DIR/home/.local/share"
export XDG_CACHE_HOME="$DIR/home/.cache" XDG_STATE_HOME="$DIR/home/.local/state"
"$HERE/repos.sh" "$DIR/repos"
sed "s#__REPOS__#$DIR/repos#" "$HERE/init.ts" > "$DIR/home/.config/fresh/init.ts"
cd "$DIR/repos/api"
tmux new-session -d -s wb -x "${W:-150}" -y "${H:-42}" \
  "env HOME=$HOME XDG_CONFIG_HOME=$XDG_CONFIG_HOME XDG_DATA_HOME=$XDG_DATA_HOME XDG_CACHE_HOME=$XDG_CACHE_HOME XDG_STATE_HOME=$XDG_STATE_HOME fresh"
echo "scenario in $DIR — tmux attach -t wb"
