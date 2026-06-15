#!/usr/bin/env bash
# Build the fresh-editor RPM from source inside a Fedora container.
#
# This lets you exercise fresh-editor.spec on a non-Fedora host (the project is
# developed on Arch). It mirrors what Koji/mock do — build from a source
# tarball with vendored deps — using a plain `rpmbuild` in a Fedora image with
# all BuildRequires installed. (mock adds clean-chroot isolation; for a local
# smoke test rpmbuild-in-container is enough and far simpler.)
#
# The slow toolchain/BuildRequires install is baked into a builder image
# (fedora/Dockerfile) so it is paid once; re-running this script after a spec
# edit only re-does the fast vendor + rpmbuild steps.
#
# Usage:   ./fedora/docker-build.sh            # full build
#          FEDORA=rawhide ./fedora/docker-build.sh
#          REBUILD_IMAGE=1 ./fedora/docker-build.sh   # force builder rebuild
# Output RPMs are copied to ./fedora/out/ on the host.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# Fedora 43+ ships rustc 1.96, needed by the oxc crates (MSRV 1.93). Fedora 41
# (rustc 1.91) is too old.
FEDORA="${FEDORA:-43}"
TAG="fresh-fedora-builder:${FEDORA}"
OUT="${REPO_ROOT}/fedora/out"
mkdir -p "${OUT}"

# 1. Builder image (toolchain + BuildRequires). Built once, reused after.
if [[ "${REBUILD_IMAGE:-0}" == "1" ]] || ! docker image inspect "${TAG}" >/dev/null 2>&1; then
    echo ">> Building builder image ${TAG} (one-time, slow: installs the toolchain)"
    docker build --build-arg "FEDORA=${FEDORA}" -t "${TAG}" -f "${REPO_ROOT}/fedora/Dockerfile" "${REPO_ROOT}/fedora"
else
    echo ">> Reusing builder image ${TAG}"
fi

# 2. Packaging steps against the pre-baked image (no dnf at runtime).
echo ">> Building fresh-editor RPM in ${TAG} (repo: ${REPO_ROOT})"
docker run --rm \
    -v "${REPO_ROOT}:/src:ro" \
    -v "${OUT}:/out" \
    -e VERSION=0.4.0 \
    "${TAG}" \
    bash -euo pipefail -c '
set -x

echo "::: staging source tree (excluding target/.git/node_modules)"
DIR="fresh-${VERSION}"
mkdir -p "/build/${DIR}"
tar -C /src -cf - \
    --exclude=./target --exclude=./.git --exclude=./node_modules \
    --exclude=./.worktrees --exclude=./vendor --exclude=./fedora/out . \
  | tar -C "/build/${DIR}" -xf -

cd "/build/${DIR}"

echo "::: vendoring crate dependencies (offline build needs this)"
mkdir -p .cargo
# Plain cargo vendor (cargo-vendor-filterer is not in Fedora repos). Vendor to a
# sibling "vendor" dir; the spec extracts Source1 into a directory of that name.
cargo vendor "../vendor" > .cargo/vendor-config.toml
echo "vendored $(find ../vendor -maxdepth 1 -mindepth 1 -type d | wc -l) crates"

echo "::: building Source0 + Source1 tarballs"
cd /build
tar --owner=0 --group=0 -czf ~/rpmbuild/SOURCES/fresh-${VERSION}.tar.gz "fresh-${VERSION}"
# Source1 top-level dir is "vendor" (matches %cargo_prep -v vendor in the spec).
tar --owner=0 --group=0 -caf ~/rpmbuild/SOURCES/fresh-editor-${VERSION}-vendor.tar.zst "vendor"

cp "/build/fresh-${VERSION}/fedora/fresh-editor.spec" ~/rpmbuild/SPECS/

echo "::: rpmbuild -bs (SRPM — validates spec + sources)"
# Drop the GitHub Source0 URL so rpmbuild uses our local tarball.
sed -i "s|^Source0:.*|Source0:        fresh-${VERSION}.tar.gz|" ~/rpmbuild/SPECS/fresh-editor.spec
rpmbuild -bs ~/rpmbuild/SPECS/fresh-editor.spec

echo "::: rpmbuild -bb (binary RPM — builds fresh from source)"
rpmbuild -bb ~/rpmbuild/SPECS/fresh-editor.spec

echo "::: collecting artifacts"
find ~/rpmbuild/SRPMS ~/rpmbuild/RPMS -name "*.rpm" -exec cp -v {} /out/ \;
echo "::: installed-file list:"
rpm -qlp /out/fresh-editor-*.x86_64.rpm || true
'
echo ">> Done. RPMs in ${OUT}/"
ls -la "${OUT}/"
