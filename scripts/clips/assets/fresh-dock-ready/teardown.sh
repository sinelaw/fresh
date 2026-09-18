#!/usr/bin/env bash
# Undo everything the clip creates, in the order that actually works.
#
# Forgetting a workspace is two separate things: the window goes when the
# editor dies, but the *registry entry* under XDG_DATA_HOME/fresh/workspaces
# outlives it and the next launch rediscovers the worktree as a row. Both
# have to go, and the git worktrees have to be unregistered before their
# directories are removed or the source repo keeps refusing new worktrees.
set -uo pipefail
CLIP_HOME="${CLIP_HOME:-$HOME/.cache/fresh-dock-ready}"
SESSION="${CLIP_SESSION:-fresh-dock-ready}"
FRESH="${FRESH_BIN:-fresh}"

# 1. the daemon, if one is still up
if "$FRESH" --cmd daemon list 2>/dev/null | grep -q "$SESSION"; then
  "$FRESH" --cmd daemon kill "$SESSION" >/dev/null 2>&1 || true
fi

# 2. anything the clip left running in a workspace terminal
for p in $(ps -eo pid,args | awk '/fresh-dock-ready\/bin\/(agent\.sh|coding_agent\.py|wait-for-start\.py)/ && !/awk/ {print $1}'); do
  kill "$p" 2>/dev/null || true
done

# 3. unregister the worktrees before the tree goes
if [ -d "$CLIP_HOME/tree" ]; then
  for r in "$CLIP_HOME"/tree/*/; do
    [ -d "$r/.git" ] || continue
    git -C "$r" worktree prune >/dev/null 2>&1 || true
  done
fi

# 4. the staged tree and the per-clip XDG root -- which is what holds
#    workspaces/, so removing it is the "drop the on-disk registry" step.
#    There is no daemon left to ask.
rm -rf "$CLIP_HOME"

echo "torn down $CLIP_HOME"
