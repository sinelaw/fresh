#!/usr/bin/env bash
# Build the demo machine, start its live sessions, and record the Import
# sessions clip.
#
#   scripts/clips/assets/fresh-import-sessions/record.sh [tui-clip args...]
#
# DEMO_HOME   the demo machine's home (default /home/dev; make-fixture.sh)
# FRESH_BIN   the binary to film (default: this tree's target/release/fresh)
# TUI_CLIPS   a tui-clips checkout (default ~/repos/tui-clips)
#
# The first run spends a few minutes of claude time building the fixture;
# later runs reuse it. To re-render without re-filming, run tui-clip directly
# with --skip-capture -- the live sessions are only needed while filming.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
TUI_CLIPS="${TUI_CLIPS:-$HOME/repos/tui-clips}"
SPEC="${CLIP_SPEC:-$REPO/scripts/clips/fresh-import-sessions.json}"

export DEMO_HOME="${DEMO_HOME:-/home/dev}"
export FRESH_BIN="${FRESH_BIN:-$REPO/target/release/fresh}"
# The spec copies the editor's config from here.
export CLIP_ASSETS="$HERE"

[ -x "$FRESH_BIN" ] || { echo "record: no binary at $FRESH_BIN; cargo build --release -p fresh-editor" >&2; exit 1; }
[ -x "$TUI_CLIPS/bin/tui-clip" ] || { echo "record: no tui-clips at $TUI_CLIPS; set TUI_CLIPS" >&2; exit 1; }

# Filmed from inside a Claude Code session, the editor -- and the claude it
# launches on Import -- would inherit that session's markers: the resumed
# claude would take the parent's id, or refuse to save its transcript.
unset CLAUDECODE CLAUDE_CODE_SESSION_ID CLAUDE_CODE_ENTRYPOINT \
  CLAUDE_CODE_REMOTE_SESSION_ID CLAUDE_CODE_CHILD_SESSION TMUX TMUX_PANE

"$HERE/make-fixture.sh"
"$HERE/start-live.sh"
trap '"$HERE/teardown.sh"' EXIT

# The live claude's own transcript is one of the rows the scan finds; it is
# written once claude has its first prompt, a few seconds after the pane is
# up. Wait for it rather than filming a scan that sometimes misses it.
bucket="$DEMO_HOME/.claude/projects/$(printf %s "$DEMO_HOME/src/web-dashboard" | sed 's/[^A-Za-z0-9]/-/g')"
for _ in $(seq 1 60); do
  [ "$(find "$bucket" -name '*.jsonl' | wc -l)" -ge 2 ] && break
  sleep 0.5
done

echo "recording $(basename "$SPEC") against $DEMO_HOME"
"$TUI_CLIPS/bin/tui-clip" "$SPEC" "$@"
