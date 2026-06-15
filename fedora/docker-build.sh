#!/usr/bin/env bash
# Build the fresh-editor RPM from source inside a Fedora container.
#
# This lets you exercise fresh-editor.spec on a non-Fedora host (the project is
# developed on Arch). It mirrors what Koji/mock do — build from a source
# tarball with vendored deps — using a plain `rpmbuild` in a Fedora image with
# all BuildRequires dnf-installed. (mock adds clean-chroot isolation; for a
# local smoke test rpmbuild-in-container is enough and far simpler.)
#
# Usage:   ./fedora/docker-build.sh            # full build
#          FEDORA=rawhide ./fedora/docker-build.sh
# Output RPMs are copied to ./fedora/out/ on the host.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FEDORA="${FEDORA:-41}"
IMAGE="fedora:${FEDORA}"
OUT="${REPO_ROOT}/fedora/out"
mkdir -p "${OUT}"

echo ">> Building fresh-editor RPM in ${IMAGE} (repo: ${REPO_ROOT})"

# Mount the repo read-only; the container copies it to a writable build dir so
# the host tree is never modified (no vendor/, no target/ leakage).
docker run --rm \
    -v "${REPO_ROOT}:/src:ro" \
    -v "${OUT}:/out" \
    -e VERSION=0.4.0 \
    "${IMAGE}" \
    bash -euo pipefail -c '
set -x

echo "::: installing toolchain + BuildRequires"
dnf -y install \
    rust cargo rust-packaging rpm-build rpmdevtools \
    gcc clang pkgconf-pkg-config desktop-file-utils \
    zstd tar gzip findutils gawk git-core >/dev/null

rpmdev-setuptree

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
# Plain cargo vendor (cargo-vendor-filterer is not in Fedora repos); the spec
# unpacks this as the fresh-editor-${VERSION}-vendor directory.
cargo vendor "../fresh-editor-${VERSION}-vendor" > .cargo/vendor-config.toml
echo "vendored $(find ../fresh-editor-${VERSION}-vendor -maxdepth 1 -mindepth 1 -type d | wc -l) crates"

echo "::: building Source0 + Source1 tarballs"
cd /build
tar --owner=0 --group=0 -czf ~/rpmbuild/SOURCES/fresh-${VERSION}.tar.gz "fresh-${VERSION}"
tar --owner=0 --group=0 -caf ~/rpmbuild/SOURCES/fresh-editor-${VERSION}-vendor.tar.zst "fresh-editor-${VERSION}-vendor"

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
