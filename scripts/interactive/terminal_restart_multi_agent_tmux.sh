#!/usr/bin/env bash
#
# Interactive tmux test — several agent terminals in one workspace, across an
# editor restart.
#
# The single-agent story is covered by terminal_restart_tmux_demo.sh. This one
# exists to catch the failures that only show up with more than one agent in
# flight, where a restore has to keep three separate conversations apart:
#
#   * three `claude` panes are launched into the *same* workspace via
#     `Run Agent…`, each holding a different number of turns (2 / 3 / 5)
#   * one is left running, one quits cleanly, one crashes with exit 3
#   * the editor is quit and started again on the same directory
#   * every pane must come back with its own transcript, its own tab name, and
#     — crucially — its own session id, so resuming pane #2 reports 3 turns and
#     pane #3 reports 5 rather than both landing on whichever session was
#     written last
#
# The turn counts are the whole point: they are per-session state on disk, so a
# resume that reports the wrong number proves the panes got cross-wired even
# though the screen would otherwise look perfectly restored.
#
# Usage:   scripts/interactive/terminal_restart_multi_agent_tmux.sh
# Requires: tmux. Run from the repo root. Exits non-zero if any check fails.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SESSION="fresh-multi-agent-demo"
BIN="$REPO/target/debug/fresh"
OUT="$(mktemp -d /tmp/fresh-multi-agent-out.XXXX)"
WORK="$(mktemp -d /tmp/fresh-multi-agent-work.XXXX)"
PASS=0
FAIL=0

cleanup() { tmux kill-session -t "$SESSION" 2>/dev/null || true; }
trap cleanup EXIT

note() { printf '\033[36m• %s\033[0m\n' "$*"; }
pass() { printf '\033[32m  PASS\033[0m %s\n' "$*"; PASS=$((PASS + 1)); }
fail() { printf '\033[31m  FAIL\033[0m %s\n' "$*"; FAIL=$((FAIL + 1)); }

S()   { tmux send-keys -t "$SESSION" "$@"; }
typ() { tmux send-keys -t "$SESSION" -l "$1"; }            # literal text
cap() { tmux capture-pane -t "$SESSION" -p; }
# Row 1 is the menu bar; the tab bar is row 2, sharing the line with the file
# explorer's top border (its own labels live on later rows, so counting tab
# names here is unambiguous).
tabbar() { cap | sed -n '2p'; }
# The status bar is the last row. Worth isolating: the restart indicator lives
# here, and the tab bar's own "(exited)" markers otherwise match the same
# substrings an indicator assertion looks for.
statusbar() { cap | tail -n 1; }
submit() { tmux send-keys -t "$SESSION" -l $'\033[13;5u'; }  # Ctrl+Enter

wait_for() { # pattern timeout_iters
  local pat="$1" n="${2:-30}" i=0
  while [ "$i" -lt "$n" ]; do
    cap | grep -qF "$pat" && return 0
    sleep 0.5; i=$((i + 1))
  done
  return 1
}
shot() { cap > "$OUT/$1.txt"; }

click() { # col row
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1;$2"'M'
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1;$2"'m'
}
click_text() { # text [col_offset]
  local text="$1" off="${2:-0}" pos
  pos="$(cap | awk -v t="$text" '{ i = index($0, t); if (i > 0) { print NR, i; exit } }')"
  [ -n "$pos" ] || return 1
  click "$(( ${pos#* } + off ))" "${pos%% *}"
}

palette() { # query
  S C-p; sleep 1.2
  local i ch
  for (( i = 0; i < ${#1}; i++ )); do ch="${1:i:1}"; typ "$ch"; sleep 0.2; done
  sleep 1.2
  S Enter; sleep 1.5
}

# Cycle to the terminal tab whose transcript contains `marker`. Bounded, so a
# missing pane fails the check instead of spinning forever.
focus_tab_with() { # marker
  local marker="$1" i
  for i in 1 2 3 4 5 6 7 8; do
    cap | grep -qF "$marker" && return 0
    S C-PageDown; sleep 1.2
  done
  return 1
}

# --- 0. fake agent + project -------------------------------------------------
[ -x "$BIN" ] || { note "building fresh (debug)…"; (cd "$REPO" && cargo build --bin fresh) || exit 2; }

mkdir -p "$WORK/bin" "$WORK/state" "$WORK/xdg-data" "$WORK/xdg-config" "$WORK/proj"
cat > "$WORK/bin/claude" <<'AGENT'
#!/usr/bin/env bash
# Fake `claude` CLI: enough of the surface for Fresh's agent registry.
set -u
STATE="${FAKE_CLAUDE_STATE:-/tmp/fake-claude-state}"; mkdir -p "$STATE"
mode=launch; id=""
while [ $# -gt 0 ]; do
  case "$1" in
    --session-id) id="$2"; mode=launch; shift 2 ;;
    --resume)     id="$2"; mode=resume; shift 2 ;;
    --continue)   mode=resume; shift ;;
    *)            shift ;;
  esac
done
[ -n "$id" ] || id=anonymous
turns_file="$STATE/$id.turns"; [ -f "$turns_file" ] || echo 0 > "$turns_file"
turns="$(cat "$turns_file")"
if [ "$mode" = resume ]; then
  printf 'FAKE-CLAUDE RESUMED session=%s turns=%s\n' "$id" "$turns"
else
  printf 'FAKE-CLAUDE LAUNCHED session=%s\n' "$id"
fi
while true; do
  printf '\n> '
  IFS= read -r line || { printf '\nFAKE-CLAUDE EOF\n'; exit 0; }
  case "$line" in
    bye)  printf 'FAKE-CLAUDE bye (exit 0)\n';   exit 0 ;;
    boom) printf 'FAKE-CLAUDE crash (exit 3)\n'; exit 3 ;;
  esac
  turns=$((turns + 1)); echo "$turns" > "$turns_file"
  printf 'assistant: ack #%s of %s\n' "$turns" "$line"
done
AGENT
chmod +x "$WORK/bin/claude"

( cd "$WORK/proj" && git init -q && git config user.email demo@example.com \
  && git config user.name demo && echo hello > README.md && git add -A \
  && git commit -qm init ) || exit 2

note "workspace: $WORK"

start_editor() {
  S "cd '$WORK/proj' && '$BIN' ." Enter
  wait_for "Palette: Ctrl+P" 60
}

# --- 1. launch fresh ---------------------------------------------------------
cleanup
tmux new-session -d -s "$SESSION" -x 200 -y 50
S "export PATH='$WORK/bin':\$PATH XDG_DATA_HOME='$WORK/xdg-data' XDG_CONFIG_HOME='$WORK/xdg-config' FAKE_CLAUDE_STATE='$WORK/state' TERM=xterm-256color" Enter
start_editor || { note "editor did not start"; cap; exit 2; }
sleep 1; shot "00-boot"

# --- 2. three agents in the current workspace --------------------------------
# `Run Agent…` opens the unified form already switched to "current workspace"
# and with focus already on the agent selector, so the only thing to drive is
# the selector itself.
#
# Its value is *remembered* between opens, so a blind `Right` is not a way to
# pick an agent — on the second open it would move claude → codex. Step right
# only until the label reads claude, and fail loudly rather than submit
# whatever happened to be selected. The list wraps, so this is bounded.
select_claude() {
  local i
  for i in 0 1 2 3 4 5 6; do
    cap | grep -qF "Agent: [claude" && return 0
    S Right; sleep 0.8
  done
  return 1
}

claude_tabs() { tabbar | grep -o "claude" | wc -l; }

run_agent() { # label turns
  local label="$1" turns="$2" i before
  before="$(claude_tabs)"
  palette "run agent"
  wait_for "Launch in" 40 || { fail "$label: Run Agent form did not open"; shot "err-$label-form"; return 1; }
  sleep 1
  select_claude || { fail "$label: could not select the claude agent"; shot "err-$label-agent"; return 1; }
  submit
  # Wait for a *new* tab, not for the launch banner: the banner from the
  # previous pane is still on screen if this launch fails, which would read as
  # a pass. A failed spawn leaves the tab count where it was.
  for i in $(seq 1 60); do
    [ "$(claude_tabs)" -gt "$before" ] && break
    sleep 0.5
  done
  if [ "$(claude_tabs)" -le "$before" ]; then
    fail "$label: no new agent tab appeared — $(cap | tail -1)"; shot "err-$label-launch"; return 1
  fi
  wait_for "FAKE-CLAUDE LAUNCHED" 40 || { fail "$label: agent did not launch"; shot "err-$label-launch"; return 1; }
  sleep 1
  for (( i = 1; i <= turns; i++ )); do typ "$label-$i"; S Enter; sleep 1.2; done
  wait_for "ack #$turns of $label-$turns" 20 || { fail "$label: agent did not reach $turns turns"; return 1; }
  shot "1x-$label-live"
  return 0
}

note "launching three claude panes into the same workspace"
run_agent alpha 2   && pass "pane 'alpha' launched and held 2 turns"
run_agent bravo 3   && pass "pane 'bravo' launched and held 3 turns"
run_agent charlie 5 && pass "pane 'charlie' launched and held 5 turns"

# --- 3. two of them stop, in different ways ----------------------------------
note "charlie crashes (exit 3), bravo quits cleanly, alpha stays running"
typ "boom"; S Enter; sleep 2.5          # charlie is focused
shot "20-charlie-crashed"
if cap | grep -qF "⟳ Resume claude (exit 3)"; then
  pass "charlie's crash shows '⟳ Resume claude (exit 3)'"
else
  fail "charlie's exit code missing from the indicator"
fi

focus_tab_with "ack #3 of bravo-3" || fail "could not focus the bravo pane"
typ "bye"; S Enter; sleep 2.5
shot "21-bravo-quit"
if statusbar | grep -qF "⟳ Resume claude" && ! statusbar | grep -qF "(exit"; then
  pass "bravo's clean exit shows '⟳ Resume claude' with no exit code"
else
  fail "bravo's indicator is wrong after a clean exit: $(statusbar)"
fi

# Two dead, one alive — the tab bar should say so.
shot "22-before-quit"
if [ "$(tabbar | grep -o "(exited)" | wc -l)" -eq 2 ]; then
  pass "the tab bar marks exactly the two dead panes '(exited)'"
else
  fail "expected 2 '(exited)' tabs, tab bar reads: $(tabbar)"
fi

# --- 4. quit the editor and start it again -----------------------------------
note "quitting the editor with one live and two dead agents"
palette "quit"
sleep 3
# A live terminal may raise a confirmation; take it.
if cap | grep -qiF "running"; then S Enter; sleep 2; fi
wait_for "$ " 30 || true
sleep 2; shot "30-quit"

note "starting the editor again on the same directory"
start_editor || { fail "editor did not restart"; shot "err-restart"; exit 2; }
sleep 3; shot "31-restored"

# --- 5. everything came back, and came back separate -------------------------
if [ "$(tabbar | grep -o "claude" | wc -l)" -eq 3 ]; then
  pass "all three agent tabs are back, still named 'claude'"
else
  fail "expected 3 'claude' tabs after restore, tab bar reads: $(tabbar)"
fi
if [ "$(tabbar | grep -o "(exited)" | wc -l)" -eq 2 ]; then
  pass "the two dead panes came back marked '(exited)'"
else
  fail "expected 2 '(exited)' tabs after restore, tab bar reads: $(tabbar)"
fi
if tabbar | grep -qF "Terminal 1"; then
  fail "a restored tab fell back to the '*Terminal N*' placeholder name"
else
  pass "no restored tab fell back to a placeholder name"
fi

# alpha was alive at quit — the restore path resumes it, as reopening a
# workspace always has. Its pane therefore shows the *new* process's output;
# the pre-quit transcript is above it, in scrollback rather than on the frame.
if focus_tab_with "turns=2"; then
  pass "alpha came back resumed, on its own session (2 turns)"
  if statusbar | grep -qF "⟳ Resume claude"; then
    fail "alpha is running but still advertises a restart"
  else
    pass "alpha is live again, so no restart is offered"
  fi
  # Drop into scrollback and go to the top to check the old transcript is
  # still underneath the resumed process's output.
  S C-Space; sleep 1.2; S C-Home; sleep 1.5; shot "40-alpha-scrollback"
  if cap | grep -qF "ack #2 of alpha-2"; then
    pass "alpha's pre-quit transcript is still in its scrollback"
  else
    fail "alpha's pre-quit transcript was lost"
  fi
  S C-Space; sleep 1
else
  fail "alpha's pane did not come back resumed"
fi

# bravo and charlie come back *dead*, on purpose: reopening a workspace must
# not silently re-run a process the user had finished with. Their offers are
# still on the table, and each must resume its own conversation.
check_dead_pane() { # label expected_turns
  local label="$1" turns="$2"
  if ! focus_tab_with "ack #$turns of $label-$turns"; then
    fail "$label's pane did not come back"; return
  fi
  pass "$label's transcript survived the restart"
  if cap | grep -qF "FAKE-CLAUDE RESUMED"; then
    fail "$label was silently resumed by the restore"
    return
  fi
  pass "$label came back stopped rather than silently resumed"
  if ! cap | grep -qF "⟳ Resume claude"; then
    fail "$label's restart offer did not survive the restart"; return
  fi
  pass "$label still offers '⟳ Resume claude' after the editor restart"
  click_text "⟳ Resume claude" 2 || { fail "$label: could not click the indicator"; return; }
  sleep 3; shot "4x-$label-resumed"
  if cap | grep -qF "turns=$turns"; then
    pass "$label resumed *its own* session ($turns turns)"
  else
    fail "$label resumed the wrong session (expected turns=$turns): $(cap | grep -F 'FAKE-CLAUDE RESUMED')"
  fi
}

check_dead_pane bravo 3
check_dead_pane charlie 5

# --- 6. summary --------------------------------------------------------------
echo
note "pane snapshots saved under: $OUT"
printf '\033[1mRESULT: %d passed, %d failed\033[0m\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
