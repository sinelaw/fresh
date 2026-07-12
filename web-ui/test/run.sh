#!/usr/bin/env bash
# One-command runner for the web UI Playwright suite.
#
#   web-ui/test/run.sh
#
# Builds the webui_server bridge, installs the Playwright deps if missing,
# starts the bridge on 127.0.0.1:$PORT (default 8141) with a sample file,
# waits for it to answer GET /state, runs drive.mjs, and tears the server
# down again. Works from any CWD.
#
# Env:
#   PORT      port for the bridge (default 8141)
#   CHROMIUM  path to a Chromium binary; when set, no browser is downloaded
#             and drive.mjs launches this binary instead
#   PROFILE   cargo profile for the bridge: "debug" (default, cheap for CI)
#             or "release" (what humans should serve; see web-ui/README.md)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

PORT="${PORT:-8141}"
UI_URL="http://127.0.0.1:${PORT}"
SAMPLE_FILE="$REPO_ROOT/crates/fresh-editor/src/view/scene.rs"

PROFILE="${PROFILE:-debug}"
case "$PROFILE" in
  debug)   CARGO_PROFILE_FLAGS=() ;;
  release) CARGO_PROFILE_FLAGS=(--release) ;;
  *) echo "PROFILE must be 'debug' or 'release' (got '$PROFILE')" >&2; exit 2 ;;
esac

# 1) Build the bridge. The example lives behind the `web` feature.
(cd "$REPO_ROOT" && cargo build ${CARGO_PROFILE_FLAGS[@]+"${CARGO_PROFILE_FLAGS[@]}"} --features web -p fresh-editor --example webui_server)

# 2) Install the JS deps if missing. With CHROMIUM provided, skip playwright's
#    browser download entirely; otherwise fetch Chromium only when it isn't
#    already in the playwright cache.
if [ -n "${CHROMIUM:-}" ]; then
  export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
fi
if [ ! -d "$SCRIPT_DIR/node_modules" ]; then
  if [ -f "$SCRIPT_DIR/package-lock.json" ]; then
    (cd "$SCRIPT_DIR" && npm ci)
  else
    (cd "$SCRIPT_DIR" && npm install)
  fi
fi
if [ -z "${CHROMIUM:-}" ]; then
  chromium_ok=$(cd "$SCRIPT_DIR" && node -e \
    'const {chromium}=require("playwright");console.log(require("fs").existsSync(chromium.executablePath())?"yes":"no")')
  if [ "$chromium_ok" != "yes" ]; then
    (cd "$SCRIPT_DIR" && npx playwright install chromium --with-deps)
  fi
fi

# 3) Start the bridge and always tear it down on exit.
(cd "$REPO_ROOT" && "./target/${PROFILE}/examples/webui_server" "127.0.0.1:${PORT}" "$SAMPLE_FILE") &
SERVER_PID=$!
trap 'kill "$SERVER_PID" 2>/dev/null || true' EXIT

echo "Waiting for $UI_URL/state ..."
ready=""
for _ in $(seq 1 60); do
  if curl -fsS "$UI_URL/state" >/dev/null 2>&1; then ready=1; break; fi
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "webui_server exited before becoming ready" >&2
    exit 1
  fi
  sleep 1
done
if [ -z "$ready" ]; then
  echo "webui_server did not answer GET /state within 60s" >&2
  exit 1
fi

# 4) Run the suite (CHROMIUM, if set, passes through via the environment).
status=0
(cd "$SCRIPT_DIR" && UI_URL="$UI_URL" node drive.mjs) || status=$?
exit "$status"
