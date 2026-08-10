#!/usr/bin/env bash
#
# Everything that can be checked before anything exists on GitHub.
#
#   scripts/verify-staged-artifacts.sh artifacts/
#
# Publishing is irreversible here — immutable releases freeze the assets and
# lock the tag — so every check that does not need a published release belongs
# before one is created, not after. This is that set: the archives the updater
# can install are all present, and each one's `.sha256` sidecar describes the
# bytes beside it.
#
# What cannot move earlier, and is checked after publishing instead: the
# attestation (minted at publish, so a draft has none) and anything requiring a
# download (draft assets are not fetchable). See verify-release-assets.sh.
set -euo pipefail

DIR="${1:-artifacts}"
[ -d "$DIR" ] || { echo "  FAIL: no such directory: $DIR"; exit 1; }

# The archives `fresh` can install itself from — the same set
# verify-release-assets.sh checks after publishing, anchored on the
# architecture so release tarballs that merely look similar stay out.
ARCHIVE_RE='^fresh-editor-(x86_64|aarch64|i686|armv7|riscv64)-[a-z0-9_]+-[a-z0-9_]+(-[a-z0-9_]+)?\.(tar\.gz|tar\.xz|zip)$'

# Both musl names are required, and the xz is not redundant: binaries built
# before `archive_ext` existed ask for it unconditionally, so it is the only
# asset a pre-0.4.8 install can update through. Dropping it strands them.
REQUIRED=(
  fresh-editor-x86_64-unknown-linux-musl.tar.gz
  fresh-editor-x86_64-unknown-linux-musl.tar.xz
)

echo "Checking staged artifacts in $DIR"

mapfile -t archives < <(cd "$DIR" && ls -1 2>/dev/null | grep -E "$ARCHIVE_RE" || true)
[ "${#archives[@]}" -gt 0 ] || { echo "  FAIL: no self-updatable archives staged"; exit 1; }

failed=0
for want in "${REQUIRED[@]}"; do
  printf '%s\n' "${archives[@]}" | grep -qx "$want" || {
    echo "  FAIL: $want is missing"
    failed=1
  }
done

for asset in "${archives[@]}"; do
  printf '  %-52s ' "$asset"
  if [ ! -f "$DIR/$asset.sha256" ]; then
    echo "FAIL (no .sha256 sidecar)"; failed=1; continue
  fi
  # The sidecars are written next to the archive at build time, so the name in
  # them is bare; -c resolves it relative to the working directory.
  if (cd "$DIR" && sha256sum -c --status "$asset.sha256"); then
    echo "ok"
  else
    echo "FAIL (sidecar does not match the bytes)"; failed=1
  fi
done

echo
if [ "$failed" -ne 0 ]; then
  echo "Staged artifacts are not releasable. Nothing has been published."
  exit 1
fi
echo "All ${#archives[@]} self-updatable archives are present and match their sidecars."
