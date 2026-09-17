#!/usr/bin/env bash
# Build the throwaway project tree the dock-ready clip is staged against.
#
# Four small repos under $CLIP_HOME/tree (default ~/.cache/fresh-dock-ready),
# one per project in rows.json; the workspaces are git worktrees cut off
# them at stage time. Rebuilt from scratch every run: a worktree registered
# by a previous take and then deleted out from under git leaves the repo
# refusing new ones, so a half-cleaned tree is worse than no tree.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${CLIP_HOME:-$HOME/.cache/fresh-dock-ready}/tree"

rm -rf "$ROOT"; mkdir -p "$ROOT"
export GIT_AUTHOR_NAME="Clip Fixture" GIT_AUTHOR_EMAIL="clip@example.invalid"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME" GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
# A worktree add inherits the caller's identity; without these git can refuse
# outright on a machine with no global user.name.
export GIT_CONFIG_NOSYSTEM=1

for p in $(python3 -c "import json;print(' '.join(json.load(open('$HERE/rows.json'))['projects']))"); do
  d="$ROOT/$p"
  mkdir -p "$d/src"
  git -C "$d" init -q -b main
  git -C "$d" config user.name "$GIT_AUTHOR_NAME"
  git -C "$d" config user.email "$GIT_AUTHOR_EMAIL"
  for i in 1 2 3; do
    printf 'fn step_%s() {}\n' "$i" >> "$d/src/lib.rs"
    git -C "$d" add -A
    git -C "$d" commit -q -m "$p: step $i"
  done
done
echo "staged $ROOT"
