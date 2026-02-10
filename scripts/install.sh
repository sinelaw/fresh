#!/bin/sh
# Fresh Editor Universal Installer
# Usage: curl -sL https://example.com/install.sh | sh

set -e

# ==============================================================================
#   PRIORITY & CONFIGURATION
#   (Modify these variables to change installation behavior)
# ==============================================================================

# 1. Fallback Priority Order
#    If the native OS method (apt, dnf, pacman, brew) fails or is unavailable,
#    the script will try these universal methods in the order listed below.
#    Valid options: "nix" "cargo" "npm" "appimage"
FALLBACK_PRIORITY="nix cargo npm appimage"

# 2. Arch Linux: AUR Helper Priority
#    The script will check for these helpers in order.
AUR_HELPER_PRIORITY="yay paru"

# 3. Cargo Configuration
#    Set to 1 to prefer 'cargo-binstall' (pre-compiled binary, faster).
#    Set to 0 to always force compilation from source.
PREFER_CARGO_BINSTALL=1

# 4. Repository Details (for scraping releases)
REPO_OWNER="sinelaw"
REPO_NAME="fresh"
BIN_NAME="fresh-editor"

# ==============================================================================
#   END CONFIGURATION
# ==============================================================================

# --- Colors and Helpers ---
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info()    { printf "${BLUE}[INFO]${NC} %s\n" "$1"; }
log_success() { printf "${GREEN}[SUCCESS]${NC} %s\n" "$1"; }
log_warn()    { printf "${YELLOW}[WARN]${NC} %s\n" "$1"; }
log_error()   { printf "${RED}[ERROR]${NC} %s\n" "$1"; exit 1; }

check_cmd() { command -v "$1" >/dev/null 2>&1; }

# Run a command with elevated privileges if needed
run_privileged() {
    if [ "$(id -u)" -eq 0 ]; then
        # Already root
        "$@"
    elif check_cmd sudo && sudo -n true 2>/dev/null; then
        # sudo available and works without password (or cached)
        sudo "$@"
    elif check_cmd sudo; then
        # sudo available, may prompt for password
        sudo "$@"
    else
        # No sudo, try direct execution (will fail if privileges needed)
        "$@"
    fi
}

# --- Specialized Installers ---

install_macos() {
    if check_cmd brew; then
        log_info "macOS detected. Installing via Homebrew..."
        brew tap "${REPO_OWNER}/${REPO_NAME}"
        brew install "${BIN_NAME}"
    else
        log_warn "Homebrew not found."
        run_fallbacks
    fi
}

install_arch() {
    log_info "Arch Linux detected."
    
    # Try configured AUR helpers in order
    for helper in $AUR_HELPER_PRIORITY; do
        if check_cmd "$helper"; then
            log_info "Found AUR helper '$helper'. Installing ${BIN_NAME}-bin..."
            "$helper" -S --noconfirm "${BIN_NAME}-bin"
            return
        fi
    done

    # Fallback to manual AUR build
    log_info "No AUR helper found. Building '${BIN_NAME}-bin' manually..."
    
    if ! check_cmd git || ! check_cmd makepkg; then
         log_error "git and makepkg are required for manual AUR installation."
    fi

    BUILD_DIR=$(mktemp -d)
    cur_dir=$(pwd)
    
    cd "$BUILD_DIR"
    git clone "https://aur.archlinux.org/${BIN_NAME}-bin.git"
    cd "${BIN_NAME}-bin"
    
    log_info "Running makepkg (you may be asked for sudo password)..."
    makepkg --syncdeps --install --noconfirm
    
    cd "$cur_dir"
    rm -rf "$BUILD_DIR"
}

get_release_url() {
    # $1 = pattern (e.g., .deb or .rpm)
    # $2 = arch
    url=$(curl -s "https://api.github.com/repos/${REPO_OWNER}/${REPO_NAME}/releases/latest" | grep "browser_download_url.*$2.*$1" | cut -d '"' -f 4 | head -n 1)
    echo "$url"
}

install_debian() {
    log_info "Debian/Ubuntu detected. Looking for .deb..."
    if ! check_cmd curl; then log_error "curl is required."; fi

    ARCH=$(dpkg --print-architecture)
    URL=$(get_release_url "\.deb" "$ARCH")

    if [ -z "$URL" ]; then
        log_warn "No .deb package found for $ARCH. Trying fallbacks..."
        run_fallbacks
        return
    fi

    TEMP_DEB=$(mktemp --suffix=.deb)
    log_info "Downloading $URL..."
    if ! curl -fSL "$URL" -o "$TEMP_DEB"; then
        rm -f "$TEMP_DEB"
        log_error "Failed to download .deb package."
    fi
    log_info "Installing via dpkg..."
    run_privileged dpkg -i "$TEMP_DEB"
    rm -f "$TEMP_DEB"
}

install_fedora() {
    log_info "Fedora/RHEL detected. Looking for .rpm..."
    if ! check_cmd curl; then log_error "curl is required."; fi

    ARCH=$(uname -m)
    URL=$(get_release_url "\.rpm" "$ARCH")

    if [ -z "$URL" ]; then
        log_warn "No .rpm package found for $ARCH. Trying fallbacks..."
        run_fallbacks
        return
    fi

    TEMP_RPM=$(mktemp --suffix=.rpm)
    log_info "Downloading $URL..."
    if ! curl -fSL "$URL" -o "$TEMP_RPM"; then
        rm -f "$TEMP_RPM"
        log_error "Failed to download .rpm package."
    fi
    log_info "Installing via rpm..."
    run_privileged rpm -U "$TEMP_RPM"
    rm -f "$TEMP_RPM"
}

# --- Universal Installers (Called by priority list) ---

do_install_appimage() {
    log_info "Attempting AppImage install..."
    if ! check_cmd curl; then log_error "curl is required."; fi

    ARCH=$(uname -m)
    # Map architecture to AppImage naming
    case "$ARCH" in
        x86_64)  APPIMAGE_ARCH="x86_64" ;;
        aarch64) APPIMAGE_ARCH="aarch64" ;;
        arm64)   APPIMAGE_ARCH="aarch64" ;;
        *)       log_warn "AppImage not available for architecture: $ARCH"; return 1 ;;
    esac

    URL=$(get_release_url "\.AppImage$" "$APPIMAGE_ARCH")

    if [ -z "$URL" ]; then
        log_warn "No AppImage found for $APPIMAGE_ARCH."
        return 1
    fi

    INSTALL_DIR="${HOME}/.local/share/fresh-editor"
    BIN_DIR="${HOME}/.local/bin"
    SYMLINK_PATH="${BIN_DIR}/fresh"

    # Download to temp file
    TEMP_APPIMAGE=$(mktemp)
    log_info "Downloading AppImage from $URL..."
    curl -sL "$URL" -o "$TEMP_APPIMAGE"
    chmod +x "$TEMP_APPIMAGE"

    # Extract AppImage (faster startup than running via FUSE)
    log_info "Extracting AppImage..."
    TEMP_EXTRACT=$(mktemp -d)
    (cd "$TEMP_EXTRACT" && "$TEMP_APPIMAGE" --appimage-extract > /dev/null 2>&1)
    rm -f "$TEMP_APPIMAGE"

    # Remove old installation and move new one in place
    rm -rf "$INSTALL_DIR"
    mkdir -p "$INSTALL_DIR" "$BIN_DIR"
    mv "$TEMP_EXTRACT/squashfs-root"/* "$INSTALL_DIR/"
    rm -rf "$TEMP_EXTRACT"

    # Create symlink to the binary
    ln -sf "$INSTALL_DIR/usr/bin/fresh" "$SYMLINK_PATH"

    # Check if ~/.local/bin is in PATH
    case ":$PATH:" in
        *":${BIN_DIR}:"*) ;;
        *)
            log_warn "${BIN_DIR} is not in your PATH."
            log_info "Add this to your shell profile:"
            log_info "  export PATH=\"\$HOME/.local/bin:\$PATH\""
            ;;
    esac

    log_success "Installed to $INSTALL_DIR"
    log_success "Symlink created at $SYMLINK_PATH"
}

do_install_nix() {
    log_info "Attempting Nix install..."
    nix profile install "github:${REPO_OWNER}/${REPO_NAME}"
}

do_install_cargo() {
    log_info "Attempting Cargo install..."
    if [ "$PREFER_CARGO_BINSTALL" -eq 1 ] && check_cmd cargo-binstall; then
        log_info "Using cargo-binstall (fast)..."
        cargo binstall -y "${BIN_NAME}"
    else
        log_info "Compiling from source (slow)..."
        cargo install --locked "${BIN_NAME}"
    fi
}

do_install_npm() {
    log_info "Attempting NPM install..."
    PKG_NAME="@${BIN_NAME}/${BIN_NAME}"
    if [ -w "$(npm root -g)" ]; then
        npm install -g "$PKG_NAME"
    else
        log_warn "NPM global install requires elevated privileges..."
        run_privileged npm install -g "$PKG_NAME"
    fi
}

# --- Fallback Manager ---

run_fallbacks() {
    log_info "Checking universal fallback methods in order: $FALLBACK_PRIORITY"

    for method in $FALLBACK_PRIORITY; do
        case "$method" in
            appimage)
                # AppImage works on most Linux distros (only requires FUSE)
                if [ "$(uname -s)" = "Linux" ]; then
                    if do_install_appimage; then return; fi
                fi
                ;;
            nix)
                if check_cmd nix; then do_install_nix; return; fi
                ;;
            cargo)
                if check_cmd cargo; then do_install_cargo; return; fi
                ;;
            npm)
                if check_cmd npm; then do_install_npm; return; fi
                ;;
        esac
    done

    log_error "Installation failed. No supported native package manager or fallback (appimage/nix/cargo/npm) found."
}

# --- Main Detection ---

OS="$(uname -s)"
case "${OS}" in
    Linux*)
        if [ -f /etc/os-release ]; then
            . /etc/os-release
            # Handle standard Distros
            case "$ID" in
                ubuntu|debian|linuxmint|pop|kali)
                    install_debian ;;
                fedora|rhel|centos|opensuse*|suse)
                    install_fedora ;;
                arch|manjaro|endeavouros)
                    install_arch ;;
                nixos)
                    do_install_nix ;;
                *)
                    # Handle derivatives
                    if echo "$ID_LIKE" | grep -q "arch"; then install_arch
                    elif echo "$ID_LIKE" | grep -q "debian"; then install_debian
                    elif echo "$ID_LIKE" | grep -q "fedora"; then install_fedora
                    else
                        log_warn "Unknown Linux distro: $ID"
                        run_fallbacks
                    fi
                    ;;
            esac
        else
            run_fallbacks
        fi
        ;;
    Darwin*)
        install_macos
        ;;
    *)
        log_warn "Unknown OS: $OS"
        run_fallbacks
        ;;
esac

log_success "Installation completed!"
