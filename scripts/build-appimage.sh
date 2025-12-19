#!/bin/bash
# Build an AppImage from a pre-built fresh binary
#
# Usage: ./scripts/build-appimage.sh <binary-dir> <version> [arch]
#
# Arguments:
#   binary-dir  Directory containing 'fresh' binary and 'plugins/' directory
#   version     Version string (e.g., "0.1.0")
#   arch        Target architecture: x86_64 or aarch64 (default: host arch)
#
# Requirements:
#   - libfuse2 or fuse (for running appimagetool)
#   - wget (for downloading appimagetool if not present)
#
# Example:
#   cargo build --release
#   ./scripts/build-appimage.sh target/release 0.1.0

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
APP_ID="io.github.sinelaw.fresh"

# Parse arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <binary-dir> <version> [arch]"
    echo ""
    echo "Example:"
    echo "  cargo build --release"
    echo "  $0 target/release 0.1.0"
    exit 1
fi

BINARY_DIR="$1"
VERSION="$2"
ARCH="${3:-$(uname -m)}"

# Validate architecture
case "$ARCH" in
    x86_64|aarch64) ;;
    *)
        echo "Error: Unsupported architecture: $ARCH"
        echo "Supported: x86_64, aarch64"
        exit 1
        ;;
esac

# Validate binary exists
if [ ! -f "$BINARY_DIR/fresh" ]; then
    echo "Error: Binary not found at $BINARY_DIR/fresh"
    exit 1
fi

# Create working directory
WORK_DIR=$(mktemp -d)
trap "rm -rf $WORK_DIR" EXIT

echo "=== Building AppImage for fresh $VERSION ($ARCH) ==="
echo "Binary dir: $BINARY_DIR"
echo "Work dir: $WORK_DIR"
echo ""

# Download appimagetool if not present
APPIMAGETOOL="$WORK_DIR/appimagetool"
if [ ! -f "$APPIMAGETOOL" ]; then
    echo "=== Downloading appimagetool ==="
    # appimagetool is only available for x86_64, but can build for other archs
    wget -q "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage" -O "$APPIMAGETOOL"
    chmod +x "$APPIMAGETOOL"
fi

# For aarch64 cross-builds, download the runtime
if [ "$ARCH" = "aarch64" ]; then
    echo "=== Downloading aarch64 runtime ==="
    wget -q "https://github.com/AppImage/type2-runtime/releases/download/continuous/runtime-aarch64" -O "$WORK_DIR/runtime-aarch64"
fi

echo "=== Creating AppDir structure ==="
APPDIR="$WORK_DIR/Fresh.AppDir"

mkdir -p "$APPDIR/usr/bin"
mkdir -p "$APPDIR/usr/share/applications"
mkdir -p "$APPDIR/usr/share/icons/hicolor/scalable/apps"
mkdir -p "$APPDIR/usr/share/metainfo"
mkdir -p "$APPDIR/usr/share/fresh-editor/plugins"

# Copy binary to share directory (next to plugins)
# This allows fresh to find plugins via current_exe().parent().join("plugins")
cp "$BINARY_DIR/fresh" "$APPDIR/usr/share/fresh-editor/"

# Create symlink in bin pointing to the actual binary
ln -s ../share/fresh-editor/fresh "$APPDIR/usr/bin/fresh"

# Copy plugins next to binary
if [ -d "$BINARY_DIR/plugins" ]; then
    cp -r "$BINARY_DIR/plugins"/* "$APPDIR/usr/share/fresh-editor/plugins/" 2>/dev/null || true
    echo "Copied plugins from $BINARY_DIR/plugins"
else
    echo "Warning: No plugins directory found at $BINARY_DIR/plugins"
fi

# Create desktop file
cat > "$APPDIR/fresh.desktop" << EOF
[Desktop Entry]
Name=Fresh
GenericName=Text Editor
Comment=A lightweight, fast terminal-based text editor with LSP support
Exec=fresh %F
Icon=fresh
Terminal=true
Type=Application
Categories=Development;TextEditor;Utility;
Keywords=editor;terminal;text;code;programming;lsp;
MimeType=text/plain;text/x-csrc;text/x-chdr;text/x-c++src;text/x-c++hdr;text/x-java;text/x-python;text/x-script.python;application/x-python;text/x-rust;text/x-go;text/javascript;application/javascript;text/x-typescript;application/json;text/html;text/css;text/x-shellscript;text/x-lua;text/x-ruby;text/x-php;text/x-csharp;text/markdown;
StartupNotify=false
X-AppImage-Version=${VERSION}
EOF

# Also place in standard location
cp "$APPDIR/fresh.desktop" "$APPDIR/usr/share/applications/"

# Copy icon
ICON_SRC="$REPO_ROOT/flatpak/${APP_ID}.svg"
cp "$ICON_SRC" "$APPDIR/fresh.svg"
cp "$ICON_SRC" "$APPDIR/usr/share/icons/hicolor/scalable/apps/fresh.svg"

# Copy and update AppStream metadata (filename must match the app ID)
METAINFO="$APPDIR/usr/share/metainfo/${APP_ID}.metainfo.xml"
cp "$REPO_ROOT/flatpak/${APP_ID}.metainfo.xml" "$METAINFO"
sed -i "s/<release version=\"[^\"]*\"/<release version=\"${VERSION}\"/" "$METAINFO"
sed -i "s/date=\"[^\"]*\"/date=\"$(date +%Y-%m-%d)\"/" "$METAINFO"

# Create AppRun script
cat > "$APPDIR/AppRun" << 'EOF'
#!/bin/bash
SELF=$(readlink -f "$0")
HERE=${SELF%/*}

# Run fresh via the symlink - current_exe() resolves to the real binary
# in usr/share/fresh-editor/, where plugins/ is located next to it
exec "${HERE}/usr/bin/fresh" "$@"
EOF
chmod +x "$APPDIR/AppRun"

echo ""
echo "=== AppDir structure ==="
find "$APPDIR" \( -type f -o -type l \) | sed "s|$APPDIR|Fresh.AppDir|" | head -50

echo ""
echo "=== Building AppImage ==="
OUTPUT_NAME="fresh-editor-${VERSION}-${ARCH}.AppImage"
export ARCH

cd "$WORK_DIR"
if [ "$ARCH" = "aarch64" ]; then
    "$APPIMAGETOOL" --runtime-file runtime-aarch64 Fresh.AppDir "$OUTPUT_NAME"
else
    "$APPIMAGETOOL" Fresh.AppDir "$OUTPUT_NAME"
fi

chmod +x "$OUTPUT_NAME"

echo ""
echo "=== Result ==="
file "$OUTPUT_NAME"
ls -lh "$OUTPUT_NAME"

# Move to repo root
mv "$OUTPUT_NAME" "$REPO_ROOT/"

echo ""
echo "=== Generated ==="
echo "$REPO_ROOT/$OUTPUT_NAME"

# Generate checksum
cd "$REPO_ROOT"
sha256sum "$OUTPUT_NAME" > "${OUTPUT_NAME}.sha256"
echo "$REPO_ROOT/${OUTPUT_NAME}.sha256"
