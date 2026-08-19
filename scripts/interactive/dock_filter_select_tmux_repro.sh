#!/usr/bin/env bash
#
# Interactive tmux repro — Orchestrator dock: does selecting a workspace from
# a filtered list wipe the filter?
#
# Lays out a project with a handful of workspaces (created from an isolated
# init.ts so the dock has enough rows for a filter to matter), opens the dock,
# types a search needle, then activates a matching row two ways:
#
#   1. Enter on the highlighted row  ("dive in")
#   2. A real mouse click on a row    (SGR mouse report)
#
# After each, it re-focuses the dock and reports whether the needle survived.
# Expected after the fix: the filter is still applied and the search box still
# reads the needle — selecting a workspace is not "leaving the dock".
#
# Usage:   scripts/interactive/dock_filter_select_tmux_repro.sh
# Requires: tmux. Run from the repo root.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SESSION="fresh-dock-filter"
BIN="$REPO/target/debug/fresh"
OUT="$(mktemp -d /tmp/fresh-dock-filter-out.XXXX)"
WORK="$(mktemp -d /tmp/fresh-dock-filter-work.XXXX)"
PASS=0
FAIL=0

cleanup() { tmux kill-session -t "$SESSION" 2>/dev/null || true; }
trap cleanup EXIT

note() { printf '\033[36m• %s\033[0m\n' "$*"; }
pass() { printf '\033[32m  PASS\033[0m %s\n' "$*"; PASS=$((PASS + 1)); }
fail() { printf '\033[31m  FAIL\033[0m %s\n' "$*"; FAIL=$((FAIL + 1)); }

S() { tmux send-keys -t "$SESSION" "$@"; }
typ() { tmux send-keys -t "$SESSION" -l "$1"; }
cap() { tmux capture-pane -t "$SESSION" -p; }
shot() { cap > "$OUT/$1.txt"; }

wait_for() { # pattern timeout_iters
  local pat="$1" n="${2:-30}" i=0
  while [ "$i" -lt "$n" ]; do
    cap | grep -qE "$pat" && return 0
    sleep 0.5
    i=$((i + 1))
  done
  return 1
}

# Click cell (1-based col,row) with an SGR mouse press+release.
click() { # col row
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1"';'"$2"'M'
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1"';'"$2"'m'
}

# 1-based pane row of the first line matching $1 (empty if absent).
row_of() { cap | grep -nE "$1" | head -1 | cut -d: -f1; }

# --- 0. build + workspace ----------------------------------------------------
[ -x "$BIN" ] || { note "building fresh (debug)…"; (cd "$REPO" && cargo build --bin fresh) || exit 2; }

note "workspace: $WORK"
export HOME="$WORK/home"
mkdir -p "$HOME/.config/fresh"
mkdir -p "$WORK/proj" && (cd "$WORK/proj" && git init -q && git config user.email t@t && git config user.name t && printf 'root\n' > README.md && git add -A && git commit -qm init)
for w in alpha beta gamma delta epsilon zeta eta theta; do
  mkdir -p "$WORK/proj/$w" && printf '%s\n' "$w" > "$WORK/proj/$w/notes.md"
done

# Enough workspaces that the filter has something to hide.
cat > "$HOME/.config/fresh/init.ts" <<TS
for (const w of ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta"]) {
  editor.createWindow("$WORK/proj/" + w, w);
}
TS

# --- 1. launch ---------------------------------------------------------------
cleanup
tmux new-session -d -s "$SESSION" -x 200 -y 50
S "cd '$WORK/proj' && HOME='$HOME' TERM=xterm-256color '$BIN' --no-restore README.md" Enter
wait_for "Palette: Ctrl\+P" 60 || { note "editor did not start"; cap; exit 2; }
sleep 1
shot "00-boot"

# --- 2. open + focus the dock ------------------------------------------------
note "opening the orchestrator dock (Alt+O)"
S M-o
wait_for "gamma" 30 || { note "dock did not show the workspaces"; cap; exit 2; }
sleep 0.5
shot "01-dock-open"

if cap | grep -q "epsilon" && cap | grep -q "gamma"; then
  pass "dock lists the workspaces"
else
  fail "dock is missing workspaces"
fi

# --- 3. filter ---------------------------------------------------------------
filter_to() { # needle
  note "filtering to '$1'"
  S "/"
  sleep 0.4
  typ "$1"
  sleep 1
}

filter_to "gamma"
shot "02-filtered"
if cap | grep -q "gamma" && ! cap | grep -q "epsilon"; then
  pass "filter narrows the list to 'gamma'"
else
  fail "filter did not narrow the list"
fi

# Enter in the filter box returns to the list (filter still applied).
S Enter
sleep 0.6
shot "03-filter-enter-back-to-list"

# --- 4a. select with Enter ---------------------------------------------------
note "selecting the highlighted workspace with Enter (dive in)"
S Enter
sleep 1.2
shot "04-after-enter-dive"
note "re-focusing the dock (Alt+O)"
S M-o
sleep 1
shot "05-dock-refocused-after-enter"
if cap | grep -q "epsilon"; then
  fail "Enter-select wiped the filter (non-matching 'epsilon' is back)"
else
  pass "Enter-select kept the filter (non-matching rows still hidden)"
fi
if cap | grep -q "Search Tasks"; then
  fail "Enter-select cleared the search box (placeholder is showing)"
else
  pass "Enter-select kept the search box text"
fi

# --- 4b. select with a mouse click ------------------------------------------
# Reset: leave the dock (Esc) so we start from a clean, unfiltered list.
S Escape
sleep 0.8
S M-o
sleep 0.8
filter_to "delta"
S Enter
sleep 0.6
shot "06-filtered-delta"

DROW="$(row_of '  *delta')"
if [ -n "$DROW" ]; then
  note "clicking the 'delta' row at pane row $DROW"
  click 8 "$DROW"
  sleep 1.2
  shot "07-after-click"
  note "re-focusing the dock (Alt+O)"
  S M-o
  sleep 1
  shot "08-dock-refocused-after-click"
  if cap | grep -q "epsilon"; then
    fail "click-select wiped the filter (non-matching 'epsilon' is back)"
  else
    pass "click-select kept the filter (non-matching rows still hidden)"
  fi
  if cap | grep -q "Search Tasks"; then
    fail "click-select cleared the search box (placeholder is showing)"
  else
    pass "click-select kept the search box text"
  fi
else
  fail "could not locate the 'delta' row to click"
fi

# --- 4c. Esc still clears ----------------------------------------------------
note "leaving the dock with Esc must still clear the filter"
S Escape
sleep 0.8
S M-o
sleep 1
shot "09-dock-after-esc"
if cap | grep -q "epsilon"; then
  pass "Esc out of the dock clears the filter (full list is back)"
else
  fail "Esc out of the dock did not clear the filter"
fi

# --- 5. summary --------------------------------------------------------------
echo
note "pane snapshots saved under: $OUT"
printf '\033[1mRESULT: %d passed, %d failed\033[0m\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
