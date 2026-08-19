#!/usr/bin/env bash
#
# Interactive tmux test — restarting a terminal whose process has quit.
#
# Builds `fresh`, puts a fake `claude` on PATH, and drives a real editor inside
# a tmux pane through the whole restart story:
#
#   1. plain shell      open a terminal, `exit` it   → status bar offers
#                                                      "⟳ Restart terminal";
#                                                      palette command restarts
#                                                      it in the same buffer with
#                                                      the old scrollback intact
#   2. agent            Orchestrator: New Workspace with agent = claude, hold a
#                       couple of turns, quit the agent → status bar offers
#                       "⟳ Resume claude"; CLICKING it runs `claude --resume
#                       <id>` and the agent reports the recovered turn count
#   3. crash            a non-zero exit shows "⟳ Resume claude (exit 3)"
#   4. menu             View → Terminal → Restart Terminal Process resumes too
#   5. guard            asking to restart a *live* terminal reports
#                       "Terminal process is still running" and leaves it alone
#
# The fake agent speaks just enough of the real CLI's surface for Fresh's agent
# registry (`--session-id` to launch, `--resume` to rejoin) and keeps a turn
# counter on disk, so a resume can *prove* it recovered prior context rather
# than silently starting over.
#
# Usage:   scripts/interactive/terminal_restart_tmux_demo.sh
# Requires: tmux. Run from the repo root. Exits non-zero if any check fails.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SESSION="fresh-restart-demo"
BIN="$REPO/target/debug/fresh"
OUT="$(mktemp -d /tmp/fresh-restart-demo-out.XXXX)"
WORK="$(mktemp -d /tmp/fresh-restart-demo-work.XXXX)"
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
# Ctrl+Enter as a CSI-u sequence — the Orchestrator form's "submit from
# anywhere" key. A plain tmux `C-Enter` can't encode it; the raw bytes can.
submit() { tmux send-keys -t "$SESSION" -l $'\033[13;5u'; }

# Poll the pane for a regex up to N*0.5s; succeeds as soon as it appears.
wait_for() { # pattern timeout_iters
  local pat="$1" n="${2:-30}" i=0
  while [ "$i" -lt "$n" ]; do
    cap | grep -qF "$pat" && return 0
    sleep 0.5; i=$((i + 1))
  done
  return 1
}
shot() { cap > "$OUT/$1.txt"; }   # save a labelled pane snapshot

# Click cell (col,row) — both 1-based — as an SGR mouse press+release.
click() { # col row
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1;$2"'M'
  tmux send-keys -t "$SESSION" -l $'\033[<0;'"$1;$2"'m'
}

# Click the first cell of `text` (plus an offset) wherever it currently sits.
click_text() { # text [col_offset]
  local text="$1" off="${2:-0}"
  local pos
  pos="$(cap | awk -v t="$text" '{ i = index($0, t); if (i > 0) { print NR, i; exit } }')"
  [ -n "$pos" ] || return 1
  click "$(( ${pos#* } + off ))" "${pos%% *}"
}

# Run a palette command by name (the unified prompt matches commands without a
# prefix). Typed a character at a time — the suggestion list is rebuilt per
# keystroke.
palette() { # query
  S C-p; sleep 1.2
  local i ch
  for (( i = 0; i < ${#1}; i++ )); do ch="${1:i:1}"; typ "$ch"; sleep 0.2; done
  sleep 1.2
  S Enter; sleep 1.5
}

# --- 0. build + fake agent + workspace ---------------------------------------
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

# --- 1. launch fresh ---------------------------------------------------------
cleanup
tmux new-session -d -s "$SESSION" -x 200 -y 50
S "export PATH='$WORK/bin':\$PATH XDG_DATA_HOME='$WORK/xdg-data' XDG_CONFIG_HOME='$WORK/xdg-config' FAKE_CLAUDE_STATE='$WORK/state' TERM=xterm-256color" Enter
S "cd '$WORK/proj' && '$BIN' ." Enter
wait_for "Palette: Ctrl+P" 60 || { note "editor did not start"; cap; exit 2; }
sleep 1; shot "00-boot"

# --- 2. plain terminal: exit, restart from the palette -----------------------
note "plain terminal — exit, then restart from the command palette"
palette "open terminal"
wait_for "$" 20 || true
sleep 1
typ "echo marker-before-exit"; S Enter; sleep 1
typ "exit"; S Enter; sleep 2
shot "10-plain-exited"

if cap | grep -qF "⟳ Restart terminal"; then
  pass "an exited plain shell offers '⟳ Restart terminal'"
else
  fail "no restart indicator after a plain shell exited"
fi

palette "restart terminal process"
sleep 2; shot "11-plain-restarted"
if cap | grep -qF "restarted" && ! cap | grep -qF "⟳ Restart terminal"; then
  pass "palette restart brings the shell back and clears the indicator"
else
  fail "palette restart did not revive the plain shell"
fi
typ "echo ALIVE-AFTER-RESTART"; S Enter; sleep 1.5
if cap | grep -qF "ALIVE-AFTER-RESTART"; then
  pass "the restarted shell is interactive"
else
  fail "the restarted shell does not accept input"
fi
# Scrollback continuity: the pre-exit transcript survives the restart.
S C-Space; sleep 1; S C-Home; sleep 1.5; shot "12-plain-scrollback"
if cap | grep -qF "marker-before-exit"; then
  pass "restart continues the same buffer's scrollback"
else
  fail "restart lost the pre-exit scrollback"
fi
S C-Space; sleep 1; S C-Space; sleep 1

# --- 3. agent: quit, resume by CLICKING the status-bar indicator -------------
note "agent workspace — quit the agent, then click '⟳ Resume claude'"
palette "orchestrator new"
wait_for "ORCHESTRATOR :: New Workspace" 40 || { fail "New Workspace form did not open"; shot "20-noform"; }
sleep 1
S Tab; sleep 0.6; S Tab; sleep 0.6      # Project Path → Workspace Name → Agent
S Right; sleep 1                         # terminal → claude
shot "20-form"
if cap | grep -qF "Agent: [claude"; then
  pass "New Workspace form selects the claude agent"
else
  fail "could not select the claude agent in the form"
fi
submit
wait_for "FAKE-CLAUDE LAUNCHED" 60 || { fail "agent did not launch"; shot "21-nolaunch"; }
shot "21-agent-launched"

typ "first question";  S Enter; sleep 1.5
typ "second question"; S Enter; sleep 1.5
typ "bye";             S Enter; sleep 2.5
shot "22-agent-exited"

if cap | grep -qF "⟳ Resume claude"; then
  pass "an exited agent offers '⟳ Resume claude' (resume wording, agent named)"
else
  fail "no resume indicator after the agent exited"
fi

click_text "⟳ Resume claude" 2 || fail "could not locate the indicator to click"
sleep 3; shot "23-agent-resumed"
if cap | grep -qF "FAKE-CLAUDE RESUMED"; then
  pass "clicking the indicator runs the agent's resume argv"
else
  fail "clicking the indicator did not resume the agent"
fi
if cap | grep -qF "turns=2"; then
  pass "the resumed agent recovered its prior conversation (2 turns)"
else
  fail "the resumed agent did not recover prior context"
fi

# --- 4. non-zero exit code rides along on the indicator ----------------------
note "crash exit — the indicator carries the exit code"
typ "third question"; S Enter; sleep 1.2
typ "boom";           S Enter; sleep 2.5
shot "30-agent-crashed"
if cap | grep -qF "⟳ Resume claude (exit 3)"; then
  pass "a non-zero exit shows '⟳ Resume claude (exit 3)'"
else
  fail "exit code missing from the indicator"
fi

# --- 5. the same action from the View → Terminal menu ------------------------
note "menu — View → Terminal → Restart Terminal Process"
click_text "View" 1 || fail "could not click the View menu"
sleep 1.2
click_text "Terminal " 1 || fail "could not open the Terminal submenu"
sleep 1.2; shot "40-menu"
if cap | grep -qF "Restart Terminal Process"; then
  pass "the Terminal menu lists Restart Terminal Process"
else
  fail "menu entry missing"
fi
click_text "Restart Terminal Process" 1 || fail "could not click the menu entry"
sleep 3; shot "41-menu-resumed"
if cap | grep -qF "turns=3"; then
  pass "the menu entry resumes the agent too"
else
  fail "the menu entry did not resume the agent"
fi

# --- 6. a live process is never restarted out from under the user ------------
note "guard — restarting a live terminal is refused"
S C-Space; sleep 1.2                     # live → scrollback (Normal context)
palette "restart terminal process"
sleep 1.5; shot "50-guard"
if cap | grep -qF "still running"; then
  pass "restarting a live terminal is refused, not silently obeyed"
else
  fail "the live-terminal guard did not report"
fi
if cap | grep -qF "turns=3"; then
  pass "the live agent was left untouched"
else
  fail "the live agent was disturbed by the refused restart"
fi

# --- 7. summary --------------------------------------------------------------
echo
note "pane snapshots saved under: $OUT"
printf '\033[1mRESULT: %d passed, %d failed\033[0m\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
