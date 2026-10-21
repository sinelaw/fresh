#!/usr/bin/env bash
# Build Fresh's Debian source package (packaging/debian), sign it, and upload
# it to mentors.debian.net for a sponsor to review.
#
# The orig tarball is the upstream release: `git archive` of the tag
# v<version>, where <version> comes from packaging/debian/changelog. The
# debian/ directory is packaging/debian at --packaging-ref (default HEAD), so
# packaging fixes made after the release go into the upload.
#
# Usage: scripts/debian-mentors-upload.sh [options]
#   --packaging-ref REF  where packaging/debian comes from (default: HEAD)
#   --out DIR            where to build (default: ../fresh-editor-debian)
#   --no-upload          build (and sign, unless --no-sign) but don't upload
#   --no-sign            don't sign; implies --no-upload
#
# Needs: git, xz, dpkg-dev; devscripts (debsign) to sign; dput (or dput-ng)
# with a `mentors` host to upload; lintian, if installed, checks the result.
# Signing uses your OpenPGP key for the changelog's maintainer address (or
# DEBSIGN_KEYID); the key must be registered with your mentors.debian.net
# account.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
packaging_ref=HEAD
out="${REPO_ROOT}/../fresh-editor-debian"
upload=1
sign=1

while [ $# -gt 0 ]; do
  case "$1" in
    --packaging-ref) packaging_ref="$2"; shift 2 ;;
    --out) out="$2"; shift 2 ;;
    --no-upload) upload=0; shift ;;
    --no-sign) sign=0; upload=0; shift ;;
    -h|--help) sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 0 ;;
    *) echo "unknown option: $1 (see --help)" >&2; exit 2 ;;
  esac
done

need() {
  command -v "$1" >/dev/null || { echo "missing $1: $2" >&2; exit 1; }
}
need git "install git"
need xz "apt install xz-utils"
need dpkg-source "apt install dpkg-dev"
[ "$sign" = 1 ] && need debsign "apt install devscripts"
[ "$upload" = 1 ] && need dput "apt install dput (or dput-ng)"

cd "$REPO_ROOT"
git rev-parse --verify --quiet "${packaging_ref}^{commit}" >/dev/null \
  || { echo "unknown --packaging-ref: ${packaging_ref}" >&2; exit 1; }

# The version being uploaded, from the packaging's changelog.
changelog="$(mktemp)"
trap 'rm -f "$changelog"' EXIT
git show "${packaging_ref}:packaging/debian/changelog" > "$changelog"
version="$(dpkg-parsechangelog -l "$changelog" -S Version)"
distribution="$(dpkg-parsechangelog -l "$changelog" -S Distribution)"
upstream="${version%-*}"
tag="v${upstream}"
if [ "$distribution" = UNRELEASED ]; then
  echo "packaging/debian/changelog is UNRELEASED; set the distribution (unstable) first" >&2
  exit 1
fi
git fetch --quiet origin "refs/tags/${tag}:refs/tags/${tag}" 2>/dev/null || true
git rev-parse --verify --quiet "${tag}^{commit}" >/dev/null \
  || { echo "no upstream tag ${tag} for version ${version}" >&2; exit 1; }

echo "fresh-editor ${version} for ${distribution}: upstream ${tag}, packaging from ${packaging_ref}"

if [ -e "$out" ]; then
  echo "${out} already exists; remove it or pass --out" >&2
  exit 1
fi
mkdir -p "$out"
out="$(cd "$out" && pwd)"
src="${out}/fresh-editor-${upstream}"
orig="${out}/fresh-editor_${upstream}.orig.tar.xz"

git archive --prefix="fresh-editor-${upstream}/" "$tag" | xz -T0 -9 > "$orig"
tar -xf "$orig" -C "$out"
# The release's own debian/ (the crates.io build) gives way to this one.
rm -rf "${src}/debian"
git archive "$packaging_ref" packaging/debian | tar -x -C "$out"
mv "${out}/packaging/debian" "${src}/debian"
rmdir "${out}/packaging"

# A source-only upload, including the orig tarball (a new package's first
# upload must). No clean (-nc) and no build-dependency check (-d): nothing
# is built here.
(cd "$src" && dpkg-buildpackage -S -sa -d -nc -us -uc)

changes="${out}/fresh-editor_${version}_source.changes"
if command -v lintian >/dev/null; then
  lintian --info --display-info --fail-on error "$changes"
fi

if [ "$sign" = 1 ]; then
  debsign "$changes"
fi

if [ "$upload" = 1 ]; then
  if ! dput -H 2>/dev/null | grep -q '^mentors\b' && ! grep -qs '^\[mentors\]' ~/.dput.cf /etc/dput.cf; then
    cat >&2 <<'EOF'
dput has no `mentors` host. Add this to ~/.dput.cf and run again:

[mentors]
fqdn = mentors.debian.net
incoming = /upload
method = https
allow_unsigned_uploads = 0
progress_indicator = 2
allowed_distributions = .*
EOF
    exit 1
  fi
  dput mentors "$changes"
  echo
  echo "Uploaded. Once mentors has processed it:"
  echo "  dget -x https://mentors.debian.net/debian/pool/main/f/fresh-editor/fresh-editor_${version}.dsc"
else
  echo
  echo "Built in ${out} (not uploaded):"
  ls -1 "$out"
fi
