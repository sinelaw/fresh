#!/usr/bin/env bash
# Generate the vendored-dependency tarball referenced as Source1 in
# fresh-editor.spec. Run this from the repo root (where Cargo.lock lives),
# ideally on a Fedora host that has the packaging tools.
#
#   dnf install cargo cargo-vendor-filterer rust2rpm
#   ./fedora/gen-vendor.sh
#
# Produces:  fresh-editor-<version>-vendor.tar.zst
#
# %cargo_prep -v fresh-editor-<version>-vendor (in the spec) expects the
# tarball's top-level directory to be exactly that name, which --prefix sets.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

VERSION="$(awk -F'"' '
  /^\[workspace\.package\]/ { in_wp = 1; next }
  /^\[/                     { in_wp = 0 }
  in_wp && /^version[[:space:]]*=/ { print $2; exit }
' Cargo.toml)"
test -n "$VERSION"

PREFIX="fresh-editor-${VERSION}-vendor"
OUT="${PREFIX}.tar.zst"

echo ">> Vendoring dependencies for fresh-editor ${VERSION}"

# cargo-vendor-filterer is what rust2rpm uses: it strips Windows/macOS-only
# and other platform-specific crate sources that never build on Fedora,
# keeping the tarball smaller and the bundled-license list accurate.
if command -v cargo-vendor-filterer >/dev/null 2>&1; then
    cargo vendor-filterer \
        --platform '*-unknown-linux-gnu' \
        --prefix "${PREFIX}" \
        --format tar.zst \
        "${OUT}"
else
    echo "!! cargo-vendor-filterer not found; falling back to plain 'cargo vendor'."
    echo "!! Install it (dnf install cargo-vendor-filterer) for a filtered, smaller tarball."
    tmp="$(mktemp -d)"
    cargo vendor "${tmp}/${PREFIX}" >/dev/null
    tar --zstd -C "${tmp}" -cf "${OUT}" "${PREFIX}"
    rm -rf "${tmp}"
fi

echo ">> Wrote ${OUT} ($(du -h "${OUT}" | cut -f1))"
