#!/usr/bin/env bash
#
# Does the updater we are about to ship work?
#
#   scripts/verify-shipped-updater.sh v0.4.8
#
# The companion to rehearse-self-update.sh, from the other end. That one asks
# whether the *last* release can reach this one, and is therefore limited to
# what the last release could already do — on the first run of this flow, a
# binary with no `--pre` and no attestation check. This one drives the archive
# just published, so the code under test is the code being shipped.
#
# It uses the published artifact rather than a fresh build of the same commit:
# same code, and it also proves the archive that users will download is sound.
#
# It cannot test updating *to* this release — the binary is already that
# version. What it can test is everything else, against the real feed at the one
# moment the release is public but not yet latest:
#
#   1. a stable client is not offered it
#   2. `--pre` is
#   3. the pre-release guard refuses when a feed does offer one
#   4. a real download verifies its checksum and its attestation, and swaps
#
set -euo pipefail

TAG="${1:?usage: verify-shipped-updater.sh <tag>}"
VERSION="${TAG#v}"
REPO="${GH_REPO:-sinelaw/fresh}"
API="https://api.github.com/repos/$REPO"
TRIPLE="${TRIPLE:-x86_64-unknown-linux-musl}"
ASSET="fresh-editor-${TRIPLE}.tar.gz"
SANDBOX="${SANDBOX:-$(mktemp -d)}"

version_of() { "$1" --version 2>/dev/null | awk '{print $2}'; }

fail() { echo "  FAIL: $*"; exit 1; }

echo "Verifying the updater shipped in $TAG"

PREV_TAG="$(curl -sSf -m 60 "$API/releases/latest" \
  | python3 -c "import json,sys; print(json.load(sys.stdin)['tag_name'])")"
[ "$PREV_TAG" != "$TAG" ] || fail "$TAG is already latest; run this while it is still a pre-release"
PREV_VERSION="${PREV_TAG#v}"

install_dir="$SANDBOX/fresh-editor-${TRIPLE}"
curl -sSfL -m 600 "https://github.com/$REPO/releases/download/$TAG/$ASSET" \
  -o "$SANDBOX/new.tar.gz" || fail "could not download $ASSET from $TAG"
tar -xzf "$SANDBOX/new.tar.gz" -C "$SANDBOX"

BIN="$install_dir/fresh"
[ -x "$BIN" ] || fail "$ASSET has no executable fresh"

# Unlike the previous release's archive, this one has no excuse: the receipt is
# written at build time by the pipeline in this repo. If it is missing, every
# tarball install from this release resolves Unknown and cannot self-update.
[ -f "$install_dir/install-receipt.toml" ] \
  || fail "$ASSET ships no install-receipt.toml — this release cannot self-update"

have="$(version_of "$BIN")"
[ "$have" = "$VERSION" ] || fail "$TAG's binary reports $have"

# 1 + 2. The same binary at the same moment, differing only in `--pre`. Together
# these are the property the whole publish-then-promote ordering rests on: the
# release is fetchable by anyone who asks for pre-releases and invisible to
# everyone who does not. Asserting only one of them would miss the case where
# the feed changed shape and both answers became the same.
echo "  [1] a stable client does not see $VERSION"
out="$("$BIN" --cmd update --check 2>&1 || true)"
grep -q "Latest version:  *$PREV_VERSION\$" <<<"$out" \
  || fail "expected a stable check to report $PREV_VERSION; got:"$'\n'"$out"

echo "  [2] --pre does"
out="$("$BIN" --cmd update --check --pre 2>&1 || true)"
grep -q "Latest version:  *$VERSION\$" <<<"$out" \
  || fail "expected --pre to report $VERSION; got:"$'\n'"$out"

# 3. The guard itself, against a feed that really does offer a pre-release —
# which /releases/latest never does, so this needs the tag endpoint. Without it,
# a build where the guard had been removed would still pass 1 and 2.
echo "  [3] the guard refuses a pre-release when one is offered"
if out="$("$BIN" --cmd update --check --releases-url "$API/releases/tags/$TAG" 2>&1)"; then
  fail "a pre-release was accepted without --pre:"$'\n'"$out"
fi
grep -q -- "--pre" <<<"$out" || fail "the refusal does not mention --pre:"$'\n'"$out"

# 4. A real swap, with the checks live. Downwards, because this binary is
# already the newest thing there is; the direction does not change what is
# exercised — feed, download, checksum, attestation, swap. The endpoint is on an
# allowlisted host, so it stays trusted (`endpoint::is_trusted`) and the
# attestation check still runs.
#
# The engine announces which way it went — "Verifying release attestation" when
# trusted, "skipping the attestation check" when not — so both are asserted.
# Requiring the positive line is what matters: were only the skip notice
# checked for, a build that stopped attesting silently would pass.
echo "  [4] a real download verifies and swaps ($VERSION -> $PREV_VERSION)"
out="$("$BIN" --cmd update --allow-downgrade --yes \
  --releases-url "$API/releases/tags/$PREV_TAG" 2>&1)" \
  || { echo "$out" | sed 's/^/      /'; fail "the update exited non-zero"; }
echo "$out" | sed 's/^/      /'

grep -qi "skipping the attestation" <<<"$out" \
  && fail "the attestation check was skipped, so this proved only the checksum"
grep -q "Verifying release attestation" <<<"$out" \
  || fail "the attestation check did not run"

got="$(version_of "$BIN")"
[ "$got" = "$PREV_VERSION" ] || fail "after the swap the binary reports ${got:-nothing}"

echo
echo "The updater in $TAG discovers, gates, verifies and swaps correctly."
