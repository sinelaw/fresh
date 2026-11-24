#!/usr/bin/env bash
# One-command runner for the web client's Playwright suite.
#
#   web-ui/test/run.sh
#
# Builds the webui_server host, installs the Playwright deps if missing,
# starts it on 127.0.0.1:$PORT (default 8141) with a sample file in a
# throwaway project, waits for the page, runs drive.mjs, and tears the server
# down again. Works from any CWD.
#
# Env:
#   PORT      port for the host (default 8141)
#   CHROMIUM  path to a Chromium binary; when set, no browser is downloaded
#   PROFILE   cargo profile for the host: "debug" (default) or "release"
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

PORT="${PORT:-8141}"
UI_URL="http://127.0.0.1:${PORT}"

PROFILE="${PROFILE:-debug}"
case "$PROFILE" in
  debug)   CARGO_PROFILE_FLAGS=() ;;
  release) CARGO_PROFILE_FLAGS=(--release) ;;
  *) echo "PROFILE must be 'debug' or 'release' (got '$PROFILE')" >&2; exit 2 ;;
esac

(cd "$REPO_ROOT" && cargo build ${CARGO_PROFILE_FLAGS[@]+"${CARGO_PROFILE_FLAGS[@]}"} --features web -p fresh-editor --example webui_server)

if [ -n "${CHROMIUM:-}" ]; then
  export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
fi
if [ ! -d "$SCRIPT_DIR/node_modules" ]; then
  (cd "$SCRIPT_DIR" && npm ci)
fi
if [ -z "${CHROMIUM:-}" ]; then
  chromium_ok=$(cd "$SCRIPT_DIR" && node -e \
    'const {chromium}=require("playwright");console.log(require("fs").existsSync(chromium.executablePath())?"yes":"no")')
  if [ "$chromium_ok" != "yes" ]; then
    (cd "$SCRIPT_DIR" && npx playwright install chromium --with-deps)
  fi
fi

# A throwaway project (the explorer's root) and config (settings the suite
# changes are saved there).
PROJECT="$(mktemp -d)"
CONFIG_HOME="$(mktemp -d)"
printf 'fn main() {\n    println!("hello");\n}\n' > "$PROJECT/main.rs"
printf '# Notes\n\nSome text.\n' > "$PROJECT/notes.md"
mkdir "$PROJECT/src"
printf 'pub fn lib() {}\n' > "$PROJECT/src/lib.rs"
# A skins folder with one skin, which the suite edits to see it re-applied.
SKINS="$(mktemp -d)"
mkdir "$SKINS/probe"
cat > "$SKINS/probe/skin.css" <<'EOF_SKIN'
/* ==FreshSkin==
@name          Probe
@frame         bezel
@furniture     furniture.html
@explorer-side right
==/FreshSkin== */
:root.theme-probe { --menu-bg: rgb(1, 2, 3); }
EOF_SKIN
cat > "$SKINS/probe/furniture.html" <<'EOF_SKIN'
<div class="probe-buffers" data-bind="skin_data:buffers">-</div>
<div class="probe-gauge" data-bind-var="--v:skin_data:buffers"></div>
<script>window.skinRan = true</script><a class="probe-link" href="https://example.com">x</a>
EOF_SKIN
printf '{"name": "probe-theme", "extends": "builtin://light"}\n' > "$SKINS/probe/theme.json"
(cd "$PROJECT" && XDG_CONFIG_HOME="$CONFIG_HOME" FRESH_WEB_SKINS="$SKINS" "$REPO_ROOT/target/${PROFILE}/examples/webui_server" "127.0.0.1:${PORT}" "$PROJECT/main.rs") &
SERVER_PID=$!
trap 'kill "$SERVER_PID" 2>/dev/null || true; rm -rf "$PROJECT" "$CONFIG_HOME" "$SKINS"' EXIT

echo "Waiting for $UI_URL ..."
ready=""
for _ in $(seq 1 60); do
  if curl -fsS "$UI_URL/" >/dev/null 2>&1; then ready=1; break; fi
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "webui_server exited before becoming ready" >&2
    exit 1
  fi
  sleep 1
done
if [ -z "$ready" ]; then
  echo "webui_server did not answer within 60s" >&2
  exit 1
fi

status=0
for _ in $(seq 1 "${REPEAT:-1}"); do (cd "$SCRIPT_DIR" && UI_URL="$UI_URL" PROJECT="$PROJECT" SKINS="$SKINS" node drive.mjs) || status=$?; done
exit "$status"
