#!/usr/bin/env bash
#
# Does a published release actually support self-update?
#
#   scripts/verify-release-assets.sh v0.4.7
#
# The update path cannot be exercised until a release exists, and by then users
# depend on it. Three things have to hold for every archive the updater can
# install, and none of them is checked anywhere else:
#
#   1. The archive is present. `gh release create` in release.yml ends with
#      `2>/dev/null || true`, so a failed upload does not fail the release.
#   2. Its `.sha256` sidecar matches the bytes actually served.
#   3. GitHub holds a build attestation for that digest. Attestations are
#      produced by repository configuration, not by anything in this repo, so
#      nothing here would notice them going away.
#
# Each failure is silent until a user runs `fresh --cmd update`, where the
# checks are fail-closed: a missing attestation aborts the update. This turns
# that into a red release job instead.
#
# Bytes are hashed as they stream; nothing is written to disk.
set -euo pipefail

TAG="${1:?usage: verify-release-assets.sh <tag>}"
REPO="${GH_REPO:-sinelaw/fresh}"
API="https://api.github.com/repos/$REPO"

# The archives `fresh` can install itself from: exactly the names the engine
# builds, `fresh-editor-{TARGET_TRIPLE}.{archive_ext}`. Anchoring on the
# architecture keeps out release tarballs that merely look similar —
# `fresh-editor-npm-package.tar.gz` and `fresh-editor-<version>-source.tar.gz`
# are published too, are not update targets, and do not all carry sidecars.
#
# Packages (.deb/.rpm/...) are downloaded and verified by the updater as well,
# but never installed by it, so a problem there is a printed command that
# fails rather than a broken update.
ARCHIVE_RE='^fresh-editor-(x86_64|aarch64|i686|armv7|riscv64)-[a-z0-9_]+-[a-z0-9_]+(-[a-z0-9_]+)?\.(tar\.gz|tar\.xz|zip)$'

# The default Linux install. If this one is missing or unattested, the channel
# this project points most users at cannot update itself.
REQUIRED='fresh-editor-x86_64-unknown-linux-musl.tar.gz'

auth=()
[ -n "${GH_TOKEN:-}" ] && auth=(-H "Authorization: Bearer $GH_TOKEN")

echo "Verifying $REPO @ $TAG"

release_json="$(curl -sSf -m 60 "${auth[@]}" "$API/releases/tags/$TAG" 2>/dev/null)" || {
  echo "  FAIL: no release tagged $TAG in $REPO (or it is not readable)"
  exit 1
}

assets="$(printf '%s' "$release_json" | python3 -c "
import json,sys,re
try:
    rel=json.load(sys.stdin)
except Exception:
    sys.exit('  FAIL: release metadata for this tag was not valid JSON')
for a in rel.get('assets',[]):
    if re.match(r'''$ARCHIVE_RE''', a['name']):
        print(a['name'])
")"

[ -n "$assets" ] || { echo "  FAIL: no self-updatable archives in $TAG"; exit 1; }

if ! grep -qx "$REQUIRED" <<<"$assets"; then
  echo "  FAIL: $REQUIRED is missing — the default Linux install cannot update itself"
  exit 1
fi

dl="https://github.com/$REPO/releases/download/$TAG"
failed=0
count=0

while read -r asset; do
  [ -n "$asset" ] || continue
  count=$((count + 1))
  printf '  %-52s ' "$asset"

  # Hash the bytes as they arrive rather than storing a few hundred MB.
  actual="$(curl -sSfL -m 600 "$dl/$asset" | sha256sum | awk '{print $1}')" || {
    echo "FAIL (download)"; failed=1; continue
  }
  expected="$(curl -sSfL -m 60 "$dl/$asset.sha256" | awk '{print $1}')" || {
    echo "FAIL (no .sha256 sidecar)"; failed=1; continue
  }
  if [ "$actual" != "$expected" ]; then
    echo "FAIL (checksum: sidecar says $expected, bytes are $actual)"
    failed=1; continue
  fi

  # Attestations can lag the upload by a few seconds, so poll rather than
  # taking the first 404 as fact.
  found=0
  for attempt in 1 2 3 4 5; do
    n="$(curl -sS -m 60 "${auth[@]}" "$API/attestations/sha256:$actual" \
      | python3 -c "
import json,sys
try: d=json.load(sys.stdin)
except Exception: print(0); raise SystemExit
print(len(d.get('attestations') or []) if isinstance(d,dict) else 0)
")"
    [ "${n:-0}" -gt 0 ] && { found=1; break; }
    [ "$attempt" -lt 5 ] && sleep 15
  done
  if [ "$found" -eq 1 ]; then
    echo "ok (checksum + attestation)"
  else
    echo "FAIL (no attestation for $actual)"
    failed=1
  fi
done <<<"$assets"

echo
if [ "$failed" -ne 0 ]; then
  echo "$TAG has archives that cannot be self-updated to. See above."
  exit 1
fi
echo "All $count self-updatable archives are present, checksummed and attested."
