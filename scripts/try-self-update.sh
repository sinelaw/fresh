#!/usr/bin/env bash
#
# Exercise `fresh --cmd update` by hand, without waiting for a release.
#
# Two modes, because they prove different halves of the path:
#
#   remote  Install a *real* published release from GitHub. The download, the
#           checksum sidecar and the cross-origin attestation check are all the
#           production ones. This is the only way to exercise attestation —
#           see "why not local" below. Works with a release build.
#
#   local   Fabricate a release and serve it from 127.0.0.1. Nothing touches
#           the network. Attestation is *skipped*, because a local server has
#           no attestations and never could. Needs a debug build.
#
# Neither mode touches your real install: both lay out a throwaway one under
# target/try-update/ and update that.
#
#   scripts/try-self-update.sh remote          # default
#   scripts/try-self-update.sh remote v0.4.5
#   scripts/try-self-update.sh local
#
# Why remote mode can downgrade at all: the engine takes the release feed URL
# from --releases-url, and endpoint.rs keeps an override *trusted* as long as
# it stays https on an allowlisted host. api.github.com is on that list, so
# pointing at one release's tag endpoint instead of /latest is not a downgrade
# in trust — attestation stays switched on. --allow-downgrade then permits
# installing a version older than the running one, which is what makes this
# work with no unreleased version to chase.
#
# Why local mode cannot test attestation: an out-of-policy endpoint (plain
# http://, 127.0.0.1) sets Endpoints::trusted = false, and the engine skips the
# attestation check for untrusted endpoints — deliberately, since a test server
# has no attestations. It also refuses to elevate with those bytes. That same
# policy is why local mode needs a debug build: a release build *refuses* an
# out-of-policy override outright rather than downgrading to untrusted.
set -euo pipefail

MODE="${1:-remote}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SANDBOX="$REPO_ROOT/target/try-update"
GH_REPO="sinelaw/fresh"

# Prefer a release build when there is one; local mode overrides this below.
FRESH_BIN="${FRESH_BIN:-}"
if [ -z "$FRESH_BIN" ]; then
  for candidate in "$REPO_ROOT/target/release/fresh" "$REPO_ROOT/target/debug/fresh"; do
    [ -x "$candidate" ] && { FRESH_BIN="$candidate"; break; }
  done
fi
[ -n "$FRESH_BIN" ] && [ -x "$FRESH_BIN" ] || {
  echo "no fresh binary found; build one first:" >&2
  echo "    cargo build -p fresh-editor --bin fresh            # debug (needed for local mode)" >&2
  echo "    cargo build -p fresh-editor --bin fresh --release" >&2
  exit 1
}

TRIPLE="${TRIPLE:-$(rustc -vV | sed -n 's/^host: //p')}"
case "$TRIPLE" in
  *-windows-*) EXT=zip ;;
  *-musl)      EXT=tar.gz ;;
  *)           EXT=tar.xz ;;
esac
ASSET="fresh-editor-${TRIPLE}.${EXT}"

# A throwaway install carrying the receipt that install.sh and the musl archive
# write. Without it provenance resolves to Unknown and the engine refuses to
# swap, which is its own worthwhile thing to watch — delete the receipt and
# re-run to see it.
setup_sandbox() {
  rm -rf "$SANDBOX"
  mkdir -p "$SANDBOX/install"
  ln "$FRESH_BIN" "$SANDBOX/install/fresh" 2>/dev/null \
    || cp "$FRESH_BIN" "$SANDBOX/install/fresh"
  chmod 755 "$SANDBOX/install/fresh"
  cat > "$SANDBOX/install/install-receipt.toml" <<EOF
schema = 1
channel = "tarball"
package_name = "fresh-editor"
managed = false
self_update = true
EOF
}

banner() { printf '\n=== %s ===\n' "$1"; }

case "$MODE" in
remote)
  TAG="${2:-}"
  if [ -z "$TAG" ]; then
    # Default to the newest release that is not the version we are running, so
    # there is always something to install.
    current="$("$FRESH_BIN" --version | awk '{print $2}')"
    TAG="$(curl -sSf "https://api.github.com/repos/$GH_REPO/releases?per_page=10" \
      | python3 -c "
import json,sys
cur='$current'
for r in json.load(sys.stdin):
    if r['tag_name'].lstrip('v') != cur:
        print(r['tag_name']); break
")"
  fi
  [ -n "$TAG" ] || { echo "could not pick a release tag" >&2; exit 1; }

  setup_sandbox
  echo "binary   : $FRESH_BIN"
  echo "target   : $TRIPLE  ->  $ASSET"
  echo "release  : $TAG (real, from GitHub)"
  echo "sandbox  : $SANDBOX/install/fresh"

  banner "before"
  "$SANDBOX/install/fresh" --version

  banner "fresh --cmd update --allow-downgrade --yes"
  # Only the feed URL is overridden, and only to another path on
  # api.github.com. The asset still comes from the default download base, and
  # the endpoint stays trusted, so the attestation check runs for real.
  set +e
  "$SANDBOX/install/fresh" --cmd update \
    --releases-url "https://api.github.com/repos/$GH_REPO/releases/tags/$TAG" \
    --allow-downgrade \
    --yes
  status=$?
  set -e
  echo "(exit $status)"

  banner "after"
  "$SANDBOX/install/fresh" --version
  ;;

local)
  # Debug only: a release build refuses an out-of-policy endpoint outright.
  FRESH_BIN="${FRESH_BIN_DEBUG:-$REPO_ROOT/target/debug/fresh}"
  [ -x "$FRESH_BIN" ] || {
    echo "local mode needs a debug build: cargo build -p fresh-editor --bin fresh" >&2
    exit 1
  }
  NEW_VERSION="${NEW_VERSION:-99.9.9}"

  setup_sandbox
  mkdir -p "$SANDBOX"/srv/releases "$SANDBOX/srv/dl/v$NEW_VERSION" "$SANDBOX/payload"

  # The payload stands in for the real binary so the swap is visible at a
  # glance; everything around it is the production path.
  mkdir -p "$SANDBOX/payload/fresh-editor-${TRIPLE}"
  cat > "$SANDBOX/payload/fresh-editor-${TRIPLE}/fresh" <<EOF
#!/bin/sh
echo "fresh ${NEW_VERSION}  <- swapped in by the update"
EOF
  chmod 755 "$SANDBOX/payload/fresh-editor-${TRIPLE}/fresh"
  ( cd "$SANDBOX/payload"
    case "$EXT" in
      tar.gz) tar -czf "$SANDBOX/srv/dl/v$NEW_VERSION/$ASSET" "fresh-editor-${TRIPLE}" ;;
      tar.xz) XZ_OPT=-0 tar -cJf "$SANDBOX/srv/dl/v$NEW_VERSION/$ASSET" "fresh-editor-${TRIPLE}" ;;
      zip)    zip -qr "$SANDBOX/srv/dl/v$NEW_VERSION/$ASSET" "fresh-editor-${TRIPLE}" ;;
    esac )
  ( cd "$SANDBOX/srv/dl/v$NEW_VERSION" && sha256sum "$ASSET" > "$ASSET.sha256" )

  PORT="$(python3 -c 'import socket;s=socket.socket();s.bind(("127.0.0.1",0));print(s.getsockname()[1]);s.close()')"
  BASE="http://127.0.0.1:$PORT"
  # http.server serves /releases/latest straight out of this file.
  cat > "$SANDBOX/srv/releases/latest" <<EOF
{"tag_name":"v${NEW_VERSION}","assets":[
  {"name":"${ASSET}","browser_download_url":"${BASE}/dl/v${NEW_VERSION}/${ASSET}"}]}
EOF
  ( cd "$SANDBOX/srv" && exec python3 -m http.server "$PORT" --bind 127.0.0.1 ) >/dev/null 2>&1 &
  server=$!
  trap 'kill $server 2>/dev/null || true' EXIT
  for _ in $(seq 50); do curl -sf "$BASE/releases/latest" >/dev/null && break; sleep 0.1; done

  echo "binary   : $FRESH_BIN (debug)"
  echo "target   : $TRIPLE  ->  $ASSET"
  echo "release  : v$NEW_VERSION (fabricated, served from $BASE)"

  banner "before"
  "$SANDBOX/install/fresh" --version

  banner "fresh --cmd update --yes"
  set +e
  env -u HTTP_PROXY -u HTTPS_PROXY -u http_proxy -u https_proxy NO_PROXY='*' \
    "$SANDBOX/install/fresh" --cmd update \
      --releases-url "$BASE/releases/latest" \
      --download-base "$BASE/dl" \
      --yes
  status=$?
  set -e
  echo "(exit $status)"

  banner "after"
  "$SANDBOX/install/fresh" --version
  ;;

*)
  echo "usage: $0 [remote|local] [tag]" >&2
  exit 2
  ;;
esac
