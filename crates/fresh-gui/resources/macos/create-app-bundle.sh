#!/bin/bash
# Create a macOS .app bundle for Fresh editor (GUI mode).
#
# Usage:
#   ./create-app-bundle.sh [path-to-fresh-binary]
#
# The binary should be built with: cargo build --release --features gui
# Output: Fresh.app/ in the current directory.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESOURCES_DIR="$SCRIPT_DIR"
BINARY="${1:-../../target/release/fresh}"

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found at $BINARY"
    echo "Build with: cargo build --release --features gui"
    exit 1
fi

APP_NAME="Fresh"
APP_DIR="${APP_NAME}.app"
CONTENTS_DIR="${APP_DIR}/Contents"
MACOS_DIR="${CONTENTS_DIR}/MacOS"
RESOURCES_OUT="${CONTENTS_DIR}/Resources"

# Clean previous build
rm -rf "$APP_DIR"

# Create directory structure
mkdir -p "$MACOS_DIR"
mkdir -p "$RESOURCES_OUT"

# Copy binary
cp "$BINARY" "${MACOS_DIR}/fresh"
chmod +x "${MACOS_DIR}/fresh"

# Create wrapper script that launches with --gui flag
cat > "${MACOS_DIR}/Fresh" << 'WRAPPER'
#!/bin/bash
DIR="$(cd "$(dirname "$0")" && pwd)"
exec "$DIR/fresh" --gui "$@"
WRAPPER
chmod +x "${MACOS_DIR}/Fresh"

# Copy Info.plist (update executable name to wrapper)
sed 's|<string>fresh</string>|<string>Fresh</string>|' \
    "${RESOURCES_DIR}/Info.plist" > "${CONTENTS_DIR}/Info.plist"

# Copy icon — prefer the pre-built ICNS from docs/icons/macos, fall back
# to generating one from PNGs if available.
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
ICONS_DIR="${REPO_ROOT}/docs/icons/macos"

if [ -f "${ICONS_DIR}/AppIcon.icns" ]; then
    # Use the pre-built ICNS from the icon set (best quality, all sizes).
    cp "${ICONS_DIR}/AppIcon.icns" "${RESOURCES_OUT}/Fresh.icns"
    echo "Copied pre-built AppIcon.icns"
elif [ -f "${RESOURCES_DIR}/../icon_1024x1024.png" ]; then
    # Fall back: generate ICNS from 1024px PNG using macOS tools.
    if command -v iconutil &> /dev/null; then
        ICONSET_DIR=$(mktemp -d)/Fresh.iconset
        mkdir -p "$ICONSET_DIR"
        sips -z 16 16     "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_16x16.png" 2>/dev/null
        sips -z 32 32     "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_16x16@2x.png" 2>/dev/null
        sips -z 32 32     "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_32x32.png" 2>/dev/null
        sips -z 64 64     "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_32x32@2x.png" 2>/dev/null
        sips -z 128 128   "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_128x128.png" 2>/dev/null
        sips -z 256 256   "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_128x128@2x.png" 2>/dev/null
        sips -z 256 256   "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_256x256.png" 2>/dev/null
        sips -z 512 512   "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_256x256@2x.png" 2>/dev/null
        sips -z 512 512   "${RESOURCES_DIR}/../icon_1024x1024.png" --out "${ICONSET_DIR}/icon_512x512.png" 2>/dev/null
        cp "${RESOURCES_DIR}/../icon_1024x1024.png" "${ICONSET_DIR}/icon_512x512@2x.png"
        iconutil -c icns "$ICONSET_DIR" -o "${RESOURCES_OUT}/Fresh.icns"
        rm -rf "$(dirname "$ICONSET_DIR")"
        echo "Created ICNS icon from PNG"
    else
        echo "Warning: iconutil not found (not on macOS?). Icon not converted to ICNS."
        echo "Copy Fresh.icns manually to ${RESOURCES_OUT}/Fresh.icns"
    fi
else
    echo "Warning: No icon found. Place AppIcon.icns in docs/icons/macos/ or"
    echo "         icon_1024x1024.png in ${RESOURCES_DIR}/../"
fi

echo "Created ${APP_DIR}"
echo ""
echo "To run: open ${APP_DIR}"
echo "To sign: codesign --force --deep --sign - ${APP_DIR}"
