#!/usr/bin/env bash
# Stage the fixture and record the dock-cleanup clip, from a clean slate.
#
#   scripts/clips/assets/fresh-dock-cleanup/record.sh [tui-clip args...]
#
# Everything the clip touches lives under $CLIP_HOME (default
# ~/.cache/fresh-dock-clip) and under the capture's own scratch. The editor
# it drives gets its own XDG root, so `listWorkspaces()` can only ever see
# the 25 rows staged here — never a workspace of yours.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLIP_HOME="${CLIP_HOME:-$HOME/.cache/fresh-dock-clip}"
TUI_CLIPS="${TUI_CLIPS:-$HOME/repos/tui-clips}"
SPEC="${CLIP_SPEC:-$HERE/../../fresh-dock-cleanup.json}"

: "${FRESH_BIN:?set FRESH_BIN to the fresh binary the clip should drive}"

# A previous take's worktrees are still registered in the fixture repos, so
# teardown first — a half-cleaned tree makes git refuse the next worktree add.
CLIP_HOME="$CLIP_HOME" FRESH_BIN="$FRESH_BIN" "$HERE/teardown.sh" >/dev/null 2>&1 || true

mkdir -p "$CLIP_HOME/config"
CLIP_HOME="$CLIP_HOME" "$HERE/make-tree.sh"

# The staging script needs absolute paths, and the capture copies the config
# dir wholesale, so render the template here rather than guessing at runtime.
cp -r "$HERE/fresh" "$CLIP_HOME/config/fresh.tmp"
sed "s|@CLIP_HOME@|$CLIP_HOME|g" "$HERE/fresh/init.ts.in" > "$CLIP_HOME/config/fresh.tmp/init.ts"
rm -f "$CLIP_HOME/config/fresh.tmp/init.ts.in"
rm -rf "$CLIP_HOME/config/fresh"; mv "$CLIP_HOME/config/fresh.tmp" "$CLIP_HOME/config/fresh"

cp "$HERE/rows.json" "$CLIP_HOME/rows.json"
cp -r "$HERE/bin" "$CLIP_HOME/bin"
chmod +x "$CLIP_HOME"/bin/*

# The agent waits on the camera's raw dumps, which tui-clips writes into
# out/<name>/shots as it grabs. Hand it that path; the spec passes it through.
SPEC_NAME="$(python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["name"])' "$SPEC")"
export CLIP_SHOTS_DIR="$TUI_CLIPS/out/$SPEC_NAME/shots"

echo "staged $CLIP_HOME; recording $(basename "$SPEC")"
exec "$TUI_CLIPS/bin/tui-clip" "$SPEC" "$@"
