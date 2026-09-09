#!/usr/bin/env bash
#
# Can someone on the last release actually reach this one?
#
#   scripts/rehearse-self-update.sh v0.4.8
#
# Run against a release that is published but still flagged pre-release, so
# `/releases/latest` names the *previous* release and no stable user is offered
# this one yet. Promotion to latest is gated on this passing.
#
# What makes it worth running: it drives the binary that shipped last — not the
# one just built — through the real `fresh --cmd update`, against real GitHub.
# Only a genuine swap counts, so a broken updater in the *previous* release is
# caught here rather than by users who cannot update away from it.
#
# The attestation gate is live. Nothing is overridden in the common path, and
# the fallback points at an allowlisted host, which stays trusted — so a
# missing or mismatched attestation fails this script.
set -euo pipefail

TAG="${1:?usage: rehearse-self-update.sh <tag>}"
VERSION="${TAG#v}"
REPO="${GH_REPO:-sinelaw/fresh}"
API="https://api.github.com/repos/$REPO"

# The default Linux install, and the archive verify-release-assets.sh treats as
# required: if this one cannot self-update, the channel most users are pointed
# at cannot either.
TRIPLE="${TRIPLE:-x86_64-unknown-linux-musl}"
ASSET="fresh-editor-${TRIPLE}.tar.gz"

SANDBOX="${SANDBOX:-$(mktemp -d)}"

json_field() {
  python3 -c "
import json,sys
try:
    print(json.load(sys.stdin)['$1'])
except Exception:
    raise SystemExit('  FAIL: could not read $1 from release metadata')
"
}

version_of() { "$1" --version 2>/dev/null | awk '{print $2}'; }

echo "Rehearsing self-update into $TAG"

# The previous release is whatever /releases/latest still points at. If that is
# already this tag, the release was promoted before it was proven — the one
# ordering this whole flow depends on — and there is nothing left to rehearse.
#
# FROM_TAG overrides the starting point, which is how this script is exercised
# by hand between two releases that already exist:
#
#   FROM_TAG=v0.4.6 scripts/rehearse-self-update.sh v0.4.7
#
# The ordering guard is skipped then — it is about the release under test not
# having been promoted yet, which does not apply to a pair already published.
if [ -n "${FROM_TAG:-}" ]; then
  PREV_TAG="$FROM_TAG"
  echo "  (FROM_TAG set — rehearsing an already-published pair)"
else
  PREV_TAG="$(curl -sSf -m 60 "$API/releases/latest" | json_field tag_name)"
  if [ "$PREV_TAG" = "$TAG" ]; then
    echo "  FAIL: $TAG is already the latest release, so the update path into it"
    echo "        cannot be tested. It must stay a pre-release until this passes."
    exit 1
  fi
fi
PREV_VERSION="${PREV_TAG#v}"
echo "  updating from $PREV_TAG"

# Same version at both ends and the updater correctly does nothing, the version
# check at the end trivially passes, and the run reports success having proven
# nothing. The CI path cannot reach this — the tag guard above rules it out —
# but a hand-run FROM_TAG can, and a test that cannot fail is worse than none.
[ "$PREV_VERSION" != "$VERSION" ] || {
  echo "  FAIL: both ends are $VERSION; there is no upgrade to prove"
  exit 1
}

install_dir="$SANDBOX/fresh-editor-${TRIPLE}"
curl -sSfL -m 600 "https://github.com/$REPO/releases/download/$PREV_TAG/$ASSET" \
  -o "$SANDBOX/prev.tar.gz" \
  || { echo "  FAIL: could not download $ASSET from $PREV_TAG"; exit 1; }
tar -xzf "$SANDBOX/prev.tar.gz" -C "$SANDBOX"

BIN="$install_dir/fresh"
[ -x "$BIN" ] || { echo "  FAIL: $ASSET has no executable fresh at $BIN"; exit 1; }

# What is being simulated is an install.sh install, not a bare untar, and the
# receipt is the difference: without it provenance resolves to Unknown and the
# engine refuses to swap, so a pass would mean nothing.
#
# Release archives carry their own receipt, and that one wins. Archives from
# before that landed do not, and install.sh writes one itself rather than
# leaving the install to be guessed at — so this does too, through the same
# script that produces the real thing. FRESH_VERSION is pinned because the
# script would otherwise read the version out of the checkout, which is the
# release being tested rather than the one installed.
if [ -f "$install_dir/install-receipt.toml" ]; then
  echo "  archive carries its own receipt"
else
  echo "  archive predates build-time receipts; recording provenance as install.sh does"
  FRESH_VERSION="$PREV_VERSION" sh "$(dirname "$0")/write-install-receipt.sh" \
    tarball "$install_dir/install-receipt.toml" \
    target="$TRIPLE" asset="$ASSET"
fi

have="$(version_of "$BIN")"
[ "$have" = "$PREV_VERSION" ] || {
  echo "  FAIL: $PREV_TAG's binary reports $have"
  exit 1
}

# Phase A: the real path, nothing overridden. The updater discovers the release
# through the list endpoint exactly as a user running `--pre` would.
#
# A binary older than `--pre` ignores the flag and reads /releases/latest, which
# still names its own version — so it reports "already on the latest version"
# and exits 0 without doing anything. Exit codes cannot tell that apart from a
# real no-op, so the version is what is checked.
echo "  [A] $BIN --cmd update --pre --yes"
"$BIN" --cmd update --pre --yes 2>&1 | sed 's/^/      /' || true

if [ "$(version_of "$BIN")" != "$VERSION" ]; then
  # Phase B: for releases whose predecessor predates `--pre`. The tag endpoint
  # is on api.github.com, which is allowlisted, so the endpoint stays trusted
  # and the attestation check still runs. Once the shipped binary understands
  # `--pre`, phase A carries the test and this stops being reached.
  echo "  [A] no swap — $PREV_TAG predates --pre; falling back to the tag endpoint"
  echo "  [B] $BIN --cmd update --releases-url $API/releases/tags/$TAG --yes"
  "$BIN" --cmd update --pre --releases-url "$API/releases/tags/$TAG" --yes 2>&1 \
    | sed 's/^/      /' \
    || { echo "  FAIL: update exited non-zero"; exit 1; }
fi

got="$(version_of "$BIN")"
if [ "$got" != "$VERSION" ]; then
  echo "  FAIL: after updating, the binary reports ${got:-nothing} (wanted $VERSION)"
  exit 1
fi

# The swap wrote a file and set a mode; this is the cheapest proof it produced
# something that actually runs, rather than a truncated or non-executable one.
"$BIN" --version >/dev/null || { echo "  FAIL: the updated binary does not run"; exit 1; }

echo
echo "$PREV_TAG self-updated to $got, checksum and attestation verified."
