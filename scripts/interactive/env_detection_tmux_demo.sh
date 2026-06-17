#!/usr/bin/env bash
#
# Interactive tmux test — Orchestrator multi-session environment detection
# and the unified Workspace Trust prompt.
#
# Builds `fresh`, lays out a workspace with several projects — some containing
# detectable env files, some plain — then drives the Orchestrator to open one
# session per project inside a real tmux pane and asserts what each session
# shows:
#
#   project        marker         expected behaviour
#   ------------   ------------   ----------------------------------------------
#   direnv-proj    .envrc         single core trust modal, "Detected: .envrc"
#   mise-proj      mise.toml      single core trust modal, "Detected: mise.toml"
#   poetry-proj    poetry.lock    single core trust modal, "Detected: poetry.lock"
#   venv-proj      .venv (+python) NO modal — path-only env auto-trusts silently
#   plain-proj     (none)         NO modal — Trusted, nothing to gate
#
# It proves the end-to-end design: ONE trust prompt (core's), driven by ONE
# detection source (core's `env.detectors`, now incl. pipenv/poetry); the
# env-manager plugin never raises its own popup. Trusting a shell-env folder
# then activates the env via the `trust_changed` hook.
#
# Usage:   scripts/interactive/env_detection_tmux_demo.sh
# Requires: tmux. Run from the repo root. Exits non-zero if any check fails.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SESSION="fresh-env-demo"
BIN="$REPO/target/debug/fresh"
OUT="$(mktemp -d /tmp/fresh-env-demo-out.XXXX)"
WORK="$(mktemp -d /tmp/fresh-env-demo-work.XXXX)"
PASS=0
FAIL=0

cleanup() { tmux kill-session -t "$SESSION" 2>/dev/null || true; }
trap cleanup EXIT

note()  { printf '\033[36m• %s\033[0m\n' "$*"; }
pass()  { printf '\033[32m  PASS\033[0m %s\n' "$*"; PASS=$((PASS + 1)); }
fail()  { printf '\033[31m  FAIL\033[0m %s\n' "$*"; FAIL=$((FAIL + 1)); }

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
    cap | grep -qE "$pat" && return 0
    sleep 0.5; i=$((i + 1))
  done
  return 1
}
shot() { cap > "$OUT/$1.txt"; }   # save a labelled pane snapshot

# --- 0. build + workspace ----------------------------------------------------
[ -x "$BIN" ] || { note "building fresh (debug)…"; (cd "$REPO" && cargo build --bin fresh) || exit 2; }

note "workspace: $WORK"
mkdir -p "$WORK/direnv-proj"          && printf 'export DEMO=direnv\n'    > "$WORK/direnv-proj/.envrc"
mkdir -p "$WORK/mise-proj"            && printf '[tools]\nnode="20"\n'    > "$WORK/mise-proj/mise.toml"
mkdir -p "$WORK/poetry-proj"          && printf '# lock\n'                > "$WORK/poetry-proj/poetry.lock"
mkdir -p "$WORK/venv-proj/.venv/bin"  && : > "$WORK/venv-proj/.venv/bin/python" \
                                       && printf '#dummy\n'               > "$WORK/venv-proj/.venv/bin/activate"
mkdir -p "$WORK/plain-proj"           && printf '# just docs\n'           > "$WORK/plain-proj/README.md"

# --- 1. launch fresh on the plain project (boot session) ---------------------
cleanup
tmux new-session -d -s "$SESSION" -x 200 -y 50
S "TERM=xterm-256color '$BIN' '$WORK/plain-proj'" Enter
if wait_for "Palette: Ctrl\+P" 40; then :; else note "editor did not start"; cap; exit 2; fi
sleep 1; shot "00-boot-plain"

note "boot session = plain-proj (no env files)"
if cap | grep -qE "^ Trusted " && ! cap | grep -q "SECURITY WARNING"; then
  pass "plain folder boots Trusted with no trust modal"
else
  fail "plain folder should boot Trusted with no modal"
fi

# Open one Orchestrator session for $1, leaving the form submitted.
open_session() { # abs_path
  local path="$1"
  S C-p;                           sleep 1.2
  wait_for "command.*line.*buffer|>command" 20 || true
  typ "Orchestrator: New Session"; sleep 0.8
  S Enter
  wait_for "ORCHESTRATOR :: New Session" 30 || return 1
  wait_for "Project Path" 10 || true
  sleep 0.5
  typ "$path";                     sleep 1.2
  # Raw Ctrl+Enter ("submit from anywhere") creates the session even with the
  # path-completion popup open — so we never need to dismiss it (an Escape
  # here would close the whole form instead).
  submit
  return 0
}

# Assert a shell-env project raises the single core trust modal naming its
# marker, then Trust it and confirm the level flips to Trusted.
check_shell_env() { # label abs_path marker
  local label="$1" path="$2" marker="$3"
  note "session: $label  ($marker)"
  open_session "$path" || { fail "$label: could not open New Session form"; return; }
  if wait_for "SECURITY WARNING" 40; then
    shot "10-$label-modal"
    if cap | grep -qE "Detected: .*$marker"; then
      pass "$label: single core trust modal, names '$marker'"
    else
      fail "$label: trust modal did not name '$marker'"; cap | grep -A1 "Detected:" || true
    fi
    # The duplicate plugin popup must NOT appear.
    if cap | grep -qE "Environment detected|Trust & activate"; then
      fail "$label: env-manager popup appeared (duplicate!)"
    else
      pass "$label: no separate env-manager popup"
    fi
    S t; sleep 0.4; S Enter                     # Trust folder & Allow Tooling
    if wait_for "^ Trusted " 20; then
      pass "$label: trusting flips status to Trusted (env activates)"
    else
      fail "$label: status did not become Trusted after Trust"
    fi
    shot "11-$label-trusted"
  else
    fail "$label: expected a trust modal naming '$marker'"; shot "10-$label-NOMODAL"
  fi
}

# Assert a path-only venv auto-trusts with no modal.
check_path_only() { # label abs_path
  local label="$1" path="$2"
  note "session: $label  (.venv, path-only)"
  open_session "$path" || { fail "$label: could not open New Session form"; return; }
  # Wait for the new session's terminal tab to appear.
  wait_for "$(basename "$path")" 40 || true
  sleep 2; shot "20-$label"
  if cap | grep -q "SECURITY WARNING"; then
    fail "$label: path-only venv must NOT raise a trust modal"
    S k; sleep 0.3; S Enter
  else
    pass "$label: path-only venv auto-trusts, no modal"
  fi
}

# --- 2. drive a session per project -----------------------------------------
check_shell_env  "direnv" "$WORK/direnv-proj" ".envrc"
check_shell_env  "mise"   "$WORK/mise-proj"   "mise.toml"
check_shell_env  "poetry" "$WORK/poetry-proj" "poetry.lock"
check_path_only  "venv"   "$WORK/venv-proj"

# --- 3. summary --------------------------------------------------------------
echo
note "pane snapshots saved under: $OUT"
printf '\033[1mRESULT: %d passed, %d failed\033[0m\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
