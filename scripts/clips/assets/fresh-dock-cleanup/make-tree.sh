#!/usr/bin/env bash
# Build the throwaway project tree the dock-cleanup clip is staged against.
#
# Every path lives under $CLIP_HOME/tree (default ~/.cache/fresh-dock-clip),
# which teardown.sh removes wholesale. Nothing here touches a real project.
# The tree is rebuilt from scratch on every run: a git worktree registered by
# a previous take and then deleted out from under git leaves the repo
# refusing new worktrees ("use 'add -f' to override"), so a half-cleaned tree
# is worse than no tree.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${CLIP_HOME:-$HOME/.cache/fresh-dock-clip}/tree"
ROWS="$HERE/rows.json"

rm -rf "$ROOT"; mkdir -p "$ROOT"
export GIT_AUTHOR_NAME="Clip Fixture" GIT_AUTHOR_EMAIL="clip@example.invalid"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME" GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
# A worktree add inherits the caller's identity; without these two git can
# refuse outright on a machine with no global user.name.
export GIT_CONFIG_NOSYSTEM=1

jqp() { python3 -c "import json,sys;d=json.load(open('$ROWS'));$1"; }

# ── repos: real history, a branch, a few uncommitted files ────────────────
while IFS=$'\t' read -r project branch commits dirty; do
  d="$ROOT/projects/$project"
  mkdir -p "$d/src"
  git -C "$d" init -q -b "$branch"
  git -C "$d" config user.name "$GIT_AUTHOR_NAME"
  git -C "$d" config user.email "$GIT_AUTHOR_EMAIL"
  for i in $(seq 1 "$commits"); do
    printf 'fn step_%s() {}\n' "$i" >> "$d/src/lib.rs"
    printf '# %s\n\nstep %s\n' "$project" "$i" > "$d/README.md"
    git -C "$d" add -A
    git -C "$d" commit -q -m "$project: step $i"
  done
  for i in $(seq 1 "$dirty"); do echo "wip $i" > "$d/wip_$i.txt"; done
done < <(jqp "
for r in d['repos']: print('\t'.join(str(r[k]) for k in ('project','branch','commits','dirty')))")

# ── plain directories: no git, so the dock's branch column stays empty ────
while read -r p; do
  mkdir -p "$ROOT/$p"; printf 'notes for %s\n' "$p" > "$ROOT/$p/NOTES.md"
done < <(jqp "print('\n'.join(d['plain']))")

# ── junk directories: the meaningless names a dock accumulates ────────────
while read -r p; do
  mkdir -p "$ROOT/junk/$p"; echo "$p" > "$ROOT/junk/$p/.keep"
done < <(jqp "print('\n'.join(d['junkdirs']))")

echo "staged $ROOT"
