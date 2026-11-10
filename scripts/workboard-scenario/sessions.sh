#!/bin/bash
# Seed agent transcripts under the scenario's home ($1) for repos under $2,
# so the session scan has something to find: Claude Code and Codex sessions
# in directories no workspace covers (a third checkout of web, a scratch
# folder), one in a workspace's directory (covered: the board hides it), and
# one a month old (too old: the board hides it too).
set -e
H=$1; R=$2
git clone -q -b main "$R/remotes/web.git" "$R/web-spike"
mkdir -p "$R/../scratch-ideas"
SCRATCH="$(cd "$R/../scratch-ideas" && pwd)"

# Claude Code: ~/.claude/projects/<encoded cwd>/<session id>.jsonl, each
# record carrying the cwd and branch.
claude() { # id cwd branch days-ago prompt
  local dir="$H/.claude/projects/$(printf '%s' "$2" | sed 's#[^A-Za-z0-9]#-#g')"
  mkdir -p "$dir"
  printf '{"type":"user","sessionId":"%s","cwd":"%s","gitBranch":"%s","message":{"role":"user","content":"%s"}}\n' \
    "$1" "$2" "$3" "$5" > "$dir/$1.jsonl"
  touch -d "$4 days ago" "$dir/$1.jsonl"
}
claude 0b9e6f1c-1111-4c6e-9a51-6c1c8f0e0001 "$R/web-spike" main 0 "Try a canvas renderer"
claude 0b9e6f1c-2222-4c6e-9a51-6c1c8f0e0002 "$SCRATCH" "" 2 "Brainstorm names"
claude 0b9e6f1c-3333-4c6e-9a51-6c1c8f0e0003 "$R/notes" "" 1 "Tidy the todo list"
claude 0b9e6f1c-4444-4c6e-9a51-6c1c8f0e0004 "$R/web-spike" main 30 "An old experiment"

# Codex: ~/.codex/sessions/YYYY/MM/DD/rollout-<timestamp>-<uuid>.jsonl,
# opened by a session_meta envelope.
codex() { # uuid cwd days-ago
  local day; day=$(date -d "$3 days ago" +%Y/%m/%d)
  local dir="$H/.codex/sessions/$day"; mkdir -p "$dir"
  local f="$dir/rollout-$(date -d "$3 days ago" +%Y-%m-%dT%H-%M-%S)-$1.jsonl"
  printf '{"type":"session_meta","payload":{"id":"%s","cwd":"%s","originator":"codex_cli_rs"}}\n' "$1" "$2" > "$f"
  touch -d "$3 days ago" "$f"
}
codex 5f3a1d2e-aaaa-4b7c-8d9e-0f1a2b3c0001 "$R/api-clone" 3
codex 5f3a1d2e-bbbb-4b7c-8d9e-0f1a2b3c0002 "$R/web-spike" 5
