#!/bin/bash
# Script to generate cargo-sources.json for Flatpak builds
# This downloads and runs the official flatpak-cargo-generator from flatpak-builder-tools

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Check for required tools
if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required"
    exit 1
fi

# Ensure we have the required Python modules
python3 -c "import tomlkit" 2>/dev/null || {
    echo "Installing required Python packages..."
    pip3 install tomlkit aiohttp
}

# Download the generator if not present
GENERATOR_URL="https://raw.githubusercontent.com/flatpak/flatpak-builder-tools/master/cargo/flatpak-cargo-generator.py"
GENERATOR_PATH="$SCRIPT_DIR/flatpak-cargo-generator.py"

if [ ! -f "$GENERATOR_PATH" ]; then
    echo "Downloading flatpak-cargo-generator.py..."
    curl -sL "$GENERATOR_URL" -o "$GENERATOR_PATH"
fi

# Generate cargo-sources.json from Cargo.lock
echo "Generating cargo-sources.json from Cargo.lock..."
cd "$PROJECT_ROOT"
python3 "$GENERATOR_PATH" "$PROJECT_ROOT/Cargo.lock" -o "$SCRIPT_DIR/cargo-sources.json"

echo "Generated: $SCRIPT_DIR/cargo-sources.json"
echo "You can now build the Flatpak with: flatpak-builder --force-clean build flatpak/io.github.sinelaw.fresh.yml"
