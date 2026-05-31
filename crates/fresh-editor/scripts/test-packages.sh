#!/bin/bash
# Test script for .deb and .rpm packages using Docker
# This script builds the packages locally and tests them in Docker containers

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CRATE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$CRATE_ROOT/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "=== Building release binary (x86_64) ==="
cargo build --release --target x86_64-unknown-linux-gnu

echo "=== Stripping binary ==="
strip target/x86_64-unknown-linux-gnu/release/fresh
ls -la target/x86_64-unknown-linux-gnu/release/fresh

echo "=== Syncing debian/changelog with Cargo.toml ==="
"$PROJECT_ROOT/scripts/update-debian-changelog.sh"

echo "=== Building .deb package via dpkg-buildpackage ==="
# dpkg-buildpackage drops the .deb in the parent of the source tree.
dpkg-buildpackage -b -d -us -uc --host-arch amd64
DEB_FILE=$(ls "$PROJECT_ROOT"/../fresh-editor_*_amd64.deb | head -1)
# Move it into the repo so the Docker volume mount can see it.
mv "$DEB_FILE" "$PROJECT_ROOT/"
DEB_FILE="$(basename "$DEB_FILE")"
echo "Built: $DEB_FILE"

echo "=== Building .rpm package ==="
# RPM still uses cargo-generate-rpm; symlink so its asset paths resolve.
[ -L "$CRATE_ROOT/target" ] || ln -s ../../target "$CRATE_ROOT/target"
(cd "$CRATE_ROOT" && cargo generate-rpm --target x86_64-unknown-linux-gnu)
RPM_FILE=$(ls target/x86_64-unknown-linux-gnu/generate-rpm/*.rpm | head -1)
echo "Built: $RPM_FILE"

echo ""
echo "=== Testing .deb package in Ubuntu container ==="
docker run --rm -v "$PROJECT_ROOT:/workspace" ubuntu:22.04 bash -c "
    set -e
    echo 'Installing .deb package...'
    dpkg -i /workspace/$DEB_FILE || apt-get update && apt-get install -f -y && dpkg -i /workspace/$DEB_FILE

    echo ''
    echo 'Checking installed files...'
    dpkg -L fresh-editor

    echo ''
    echo 'Testing binary...'
    fresh --version

    echo ''
    echo 'Checking plugins directory...'
    ls -la /usr/share/fresh-editor/plugins/ || echo 'Plugins dir not found'

    echo ''
    echo '✓ .deb package test PASSED'
"

echo ""
echo "=== Testing .rpm package in Fedora container ==="
docker run --rm -v "$PROJECT_ROOT:/workspace" fedora:39 bash -c "
    set -e
    echo 'Installing .rpm package...'
    dnf install -y /workspace/$RPM_FILE

    echo ''
    echo 'Checking installed files...'
    rpm -ql fresh-editor

    echo ''
    echo 'Testing binary...'
    fresh --version

    echo ''
    echo 'Checking plugins directory...'
    ls -la /usr/share/fresh-editor/plugins/ || echo 'Plugins dir not found'

    echo ''
    echo '✓ .rpm package test PASSED'
"

echo ""
echo "=== All package tests PASSED ==="
