#!/bin/bash
# Test script for .deb and .rpm packages using Docker
# This script builds the packages locally and tests them in Docker containers

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

echo "=== Building release binary ==="
cargo build --release

echo "=== Stripping binary ==="
strip target/release/fresh
ls -la target/release/fresh

echo "=== Building .deb package ==="
cargo deb --no-build
DEB_FILE=$(ls target/debian/*.deb | head -1)
echo "Built: $DEB_FILE"

echo "=== Building .rpm package ==="
cargo generate-rpm
RPM_FILE=$(ls target/generate-rpm/*.rpm | head -1)
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
