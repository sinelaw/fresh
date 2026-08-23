#!/bin/sh
# Fresh Editor Universal Installer
#
# Usage:
#   curl -fsSL .../install.sh | sh
#   curl -fsSL .../install.sh | sh -s -- --method=deb
#   FRESH_INSTALL_METHOD=rpm sh install.sh
#
# The default is the universal build: a statically linked (musl) binary
# unpacked under ~/.local, owned by you, needing no root and able to update
# itself in place via `fresh --cmd update`. Distro packages are still fully
# supported — they are just chosen deliberately with --method rather than
# picked for you, because a package-manager install hands the update lifecycle
# to that package manager and requires root to apply.
#
# What the default install does, in full:
#
#   downloads   github.com/sinelaw/fresh/releases, and nothing else
#   verifies    the .sha256 published beside each asset; a mismatch aborts
#   writes      ~/.local/share/fresh-editor and a `fresh` symlink in
#               ~/.local/bin, plus a .desktop entry and icons under
#               ~/.local/share (skip those with --no-desktop-integration)
#   records     every path written outside its own directory, in
#               installed-files.txt, so an uninstall needs no guesswork
#   root        never used, and never asked for
#
# No account, token, or credential is used or read, by any method. The deb, rpm
# and appimage methods also download only from the releases page above; deb and
# rpm then install system-wide, so those two need root. The brew, nix, cargo,
# npm and aur methods hand the install to that tool, which fetches from its own
# registry and applies its own privilege and signing rules -- npm and aur may
# ask for root.
#
# Read it before running it. That advice holds for anything piped into a shell,
# including this.

set -e

# ==============================================================================
#   PRIORITY & CONFIGURATION
#   (Modify these variables to change installation behavior)
# ==============================================================================

# 1. Fallback Priority Order
#    Tried in order when the selected method is unavailable (for example an
#    architecture we publish no prebuilt binary for).
#    Valid options: "tarball" "nix" "cargo" "npm" "appimage"
FALLBACK_PRIORITY="tarball nix cargo npm"

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

# 5. Where the universal build lands. Both are user-owned; no root involved.
INSTALL_DIR="${FRESH_INSTALL_DIR:-${HOME}/.local/share/fresh-editor}"
BIN_DIR="${FRESH_BIN_DIR:-${HOME}/.local/bin}"

# 6. Desktop integration for the universal build: the archive ships an icon
#    theme and a .desktop entry, and unpacking them under INSTALL_DIR puts them
#    somewhere no XDG lookup consults. Copying them into the XDG data dirs is
#    what the .deb and .rpm do; set FRESH_NO_DESKTOP=1 (or pass
#    --no-desktop-integration) to skip it, which is usually what you want on a
#    server or in a container.
NO_DESKTOP="${FRESH_NO_DESKTOP:-0}"

# The list of paths written outside INSTALL_DIR, recorded inside it. Without a
# manifest those files are unremovable without guesswork: everything else the
# installer creates is one directory plus one symlink.
MANIFEST_NAME="installed-files.txt"

# ==============================================================================
#   END CONFIGURATION
# ==============================================================================

# --- Colors and Helpers ---
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Diagnostics go to stderr. Helpers below return a value by echoing it, so a
# message on stdout would be captured as if it were that value.
log_info()    { printf "${BLUE}[INFO]${NC} %s\n" "$1" >&2; }
log_success() { printf "${GREEN}[SUCCESS]${NC} %s\n" "$1" >&2; }
log_warn()    { printf "${YELLOW}[WARN]${NC} %s\n" "$1" >&2; }

# `exit` inside $(...) ends only that subshell, so log_error is for top-level
# paths; helpers whose output is captured return a status instead.
log_error()   { printf "${RED}[ERROR]${NC} %s\n" "$1" >&2; exit 1; }

check_cmd() { command -v "$1" >/dev/null 2>&1; }

usage() {
    cat <<EOF
Fresh Editor installer

Usage: install.sh [--method=METHOD] [--no-desktop-integration]

Methods:
  auto      (default) universal build on Linux, Homebrew on macOS, Nix on NixOS
  tarball   universal static build under ~/.local — self-updating, no root
  deb       Debian/Ubuntu .deb from the latest release (needs root)
  rpm       Fedora/RHEL/openSUSE .rpm from the latest release (needs root)
  aur       Arch Linux, via the AUR
  brew      Homebrew
  nix       Nix profile
  cargo     cargo install / cargo binstall
  npm       npm install -g
  appimage  AppImage, extracted under ~/.local

Options:
  --no-desktop-integration
            do not install the .desktop entry and icon theme into the XDG data
            dirs (universal build only)

Environment:
  FRESH_INSTALL_METHOD   same as --method
  FRESH_INSTALL_DIR      where the universal build unpacks (default ~/.local/share/fresh-editor)
  FRESH_BIN_DIR          where the 'fresh' symlink goes (default ~/.local/bin)
  FRESH_NO_DESKTOP       set to 1 for --no-desktop-integration
  XDG_DATA_HOME          where the desktop entry and icons go (default ~/.local/share)

Notes:
  The universal build updates itself with 'fresh --cmd update'. Distro packages
  delegate updates to the package manager that installed them, which is why
  they are opt-in rather than the default.

  The universal build records every file it writes outside its install
  directory in <install-dir>/${MANIFEST_NAME}; re-running this script removes
  what the previous run put there before installing again.
EOF
}

# --- Argument parsing ---

METHOD="${FRESH_INSTALL_METHOD:-auto}"

for arg in "$@"; do
    case "$arg" in
        --method=*) METHOD="${arg#--method=}" ;;
        --method)   log_error "--method needs a value, e.g. --method=deb" ;;
        --no-desktop-integration) NO_DESKTOP=1 ;;
        -h|--help)  usage; exit 0 ;;
        *)          log_error "unknown argument: $arg (try --help)" ;;
    esac
done

# --- Privilege, staging, and verification ---

# Run a command with elevated privileges if needed
run_privileged() {
    if [ "$(id -u)" -eq 0 ]; then
        # Already root
        "$@"
    elif check_cmd sudo; then
        # sudo available; may or may not prompt depending on cached credentials
        sudo "$@"
    else
        # No sudo, try direct execution (will fail if privileges needed)
        "$@"
    fi
}

WORKDIR=""

# Methods already attempted this run. Dispatch falls back on failure, and the
# fallback list contains the universal build too, so without this an explicit
# --method=tarball that finds no asset would download the release metadata and
# fail a second time before moving on.
TRIED=""
already_tried() { case " $TRIED " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }
mark_tried()    { TRIED="$TRIED $1"; }

# A private staging directory. Downloads that are later handed to dpkg/rpm
# under sudo must not sit anywhere another user can write: between the checksum
# check and the install, a world-writable path lets an unprivileged process
# swap the payload and get its own package installed as root. mktemp -d creates
# the directory atomically with 0700.
make_workdir() {
    [ -n "$WORKDIR" ] && return 0
    WORKDIR=$(mktemp -d 2>/dev/null) || log_error "could not create a temporary directory."
    chmod 700 "$WORKDIR"
    trap 'rm -rf "$WORKDIR"' EXIT
    trap 'rm -rf "$WORKDIR"; exit 130' INT
    trap 'rm -rf "$WORKDIR"; exit 143' TERM
    return 0
}

sha256_of() {
    if check_cmd sha256sum; then
        sha256sum "$1" | awk '{print $1}'
    elif check_cmd shasum; then
        shasum -a 256 "$1" | awk '{print $1}'
    else
        return 1
    fi
}

# Fail closed: an artifact we cannot verify is not installed. Every release
# asset is published with a .sha256 sidecar, so a missing one means either the
# wrong URL or a response that did not come from the release.
download_verified() {
    # $1 = url, $2 = destination path

    # Never hand curl something that is not a URL.
    case "$1" in
        https://*|http://*) ;;
        *) log_error "refusing to download from a malformed location: $1" ;;
    esac

    log_info "Downloading $(basename "$1")..."

    # The status is read from -w rather than left to `-f`, which collapses every
    # HTTP error into one exit code: GitHub rate limits anonymous downloads too,
    # so a refusal is not evidence the asset is missing. Progress still shows; it
    # goes to stderr, only the status is captured.
    #
    # Failing to *get* bytes returns to the caller. Getting bytes that do not
    # *verify* stays fatal below -- retrying that elsewhere would only launder it.
    _dl=$(curl -SL -w '%{http_code}' "$1" -o "$2") || _dl=000
    case "$_dl" in
        2??) ;;
        403|429)
            log_warn "GitHub is rate limiting downloads from this network (HTTP $_dl); retry shortly."
            return 1 ;;
        404)
            log_warn "$(basename "$1") is not published in the latest release."
            return 1 ;;
        000)
            log_warn "Could not reach GitHub to download $(basename "$1")."
            return 1 ;;
        *)
            log_warn "Download of $(basename "$1") failed (HTTP $_dl)."
            return 1 ;;
    esac

    sums=$(curl -fsSL "$1.sha256" 2>/dev/null || true)
    [ -n "$sums" ] || log_error "no .sha256 published for $(basename "$1"); refusing to install unverified bytes."

    expected=$(printf '%s\n' "$sums" | awk 'NR==1 {print $1}')
    actual=$(sha256_of "$2") || log_error "need sha256sum or shasum to verify the download; install coreutils and retry."

    if [ "$expected" != "$actual" ]; then
        log_error "checksum mismatch for $(basename "$1") (expected $expected, got $actual)."
    fi
    log_info "Checksum verified."
}

# --- Release metadata ---
#
# Deliberately no GitHub API: unauthenticated it allows 60 requests per hour per
# source IP, so behind a NAT, a CI runner, or an office network it is often
# already spent and answers 403. Only the public download endpoints are used, so
# nothing here needs a credential.

# /releases/latest/download/NAME serves the current release's asset of that
# name, and its .sha256 sidecar the same way.
release_asset_url() {
    printf 'https://github.com/%s/%s/releases/latest/download/%s' \
        "$REPO_OWNER" "$REPO_NAME" "$1"
}

# The latest version, read from a redirect: /releases/latest answers 302 with
# Location .../releases/tag/vX.Y.Z. Only packages whose filenames embed a version
# need it, so the default install never calls this. As through the API, "latest"
# skips drafts and pre-releases.
latest_version() {
    _loc=$(curl -sSL -o /dev/null -w '%{url_effective}' \
        "https://github.com/${REPO_OWNER}/${REPO_NAME}/releases/latest" 2>/dev/null) || _loc=""
    case "$_loc" in
        */releases/tag/v*) printf '%s' "${_loc##*/releases/tag/v}" ;;
        */releases/tag/*)  printf '%s' "${_loc##*/releases/tag/}" ;;
        *)
            log_warn "Could not determine the latest version from GitHub."
            return 1 ;;
    esac
}

# --- The universal build ---

musl_target() {
    case "$(uname -m)" in
        x86_64|amd64)  echo "x86_64-unknown-linux-musl" ;;
        aarch64|arm64) echo "aarch64-unknown-linux-musl" ;;
        *)             return 1 ;;
    esac
}

xdg_data_home() {
    if [ -n "${XDG_DATA_HOME:-}" ]; then
        printf '%s\n' "$XDG_DATA_HOME"
    elif [ -n "${HOME:-}" ]; then
        printf '%s\n' "$HOME/.local/share"
    else
        return 1
    fi
}

# Remove what a previous run of this script installed outside INSTALL_DIR, so a
# reinstall does not leave an older layout's files orphaned in the XDG tree.
#
# Only the two shapes this script writes are honored. A manifest is a plain list
# of paths in a directory the user owns, so treating it as a list of things to
# delete unconditionally would turn one bad line — hand-edited, or written by
# something else that claimed the path — into an arbitrary `rm`. Anything that
# does not look like a file we placed is skipped and reported.
prune_recorded_files() {
    manifest="$INSTALL_DIR/$MANIFEST_NAME"
    [ -f "$manifest" ] || return 0

    while IFS= read -r recorded; do
        [ -n "$recorded" ] || continue
        case "$recorded" in
            */applications/fresh.desktop)
                rm -f "$recorded"
                ;;
            */icons/hicolor/*/apps/fresh.png)
                rm -f "$recorded"
                # Leave no empty size directories behind in a theme tree other
                # applications share. rmdir refuses a non-empty directory, which
                # is exactly the check we want.
                apps_dir=$(dirname "$recorded")
                if rmdir "$apps_dir" 2>/dev/null; then
                    rmdir "$(dirname "$apps_dir")" 2>/dev/null || true
                fi
                ;;
            *)
                log_warn "not removing unexpected entry in $MANIFEST_NAME: $recorded"
                ;;
        esac
    done < "$manifest"
    return 0
}

# Copy the archive's .desktop entry and icon theme into the XDG data dirs — the
# same places the .deb and .rpm install them. Unpacked under INSTALL_DIR they
# are inert: no desktop environment looks there.
#
# Best-effort throughout. A machine with no $HOME, no icons in the archive, or
# no cache tools installed still gets a working editor, and a failure here must
# not fail an install that has already succeeded.
integrate_desktop_files() {
    if [ "$NO_DESKTOP" = "1" ]; then
        log_info "Skipping desktop integration (requested)."
        return 0
    fi

    DATA_HOME=$(xdg_data_home) || {
        log_warn "Neither XDG_DATA_HOME nor HOME is set; skipping desktop integration."
        return 0
    }

    manifest="$INSTALL_DIR/$MANIFEST_NAME"
    : > "$manifest" || return 0

    icons_installed=0
    if [ -d "$INSTALL_DIR/icons" ]; then
        for size_dir in "$INSTALL_DIR"/icons/*/; do
            src="${size_dir}apps/fresh.png"
            [ -f "$src" ] || continue
            size=$(basename "$size_dir")
            dest_dir="$DATA_HOME/icons/hicolor/$size/apps"
            mkdir -p "$dest_dir" 2>/dev/null || continue
            cp "$src" "$dest_dir/fresh.png" 2>/dev/null || continue
            printf '%s\n' "$dest_dir/fresh.png" >> "$manifest"
            icons_installed=$((icons_installed + 1))
        done
    fi

    desktop_installed=0
    if [ -f "$INSTALL_DIR/fresh.desktop" ]; then
        apps_dir="$DATA_HOME/applications"
        if mkdir -p "$apps_dir" 2>/dev/null; then
            # The shipped entry says `Exec=fresh`, which is right for a package
            # that installs into /usr/bin. Here the binary is reached through a
            # symlink in ~/.local/bin, and a desktop environment launching the
            # entry very often does not have that on PATH — so the Exec line is
            # rewritten to the absolute path we just linked. Arguments after the
            # program name (the %F field code) are preserved.
            if FRESH_EXEC_PATH="$BIN_DIR/fresh" awk '
                /^Exec=/ && !done {
                    rest = $0
                    sub(/^Exec=[^ ]*/, "", rest)
                    print "Exec=" ENVIRON["FRESH_EXEC_PATH"] rest
                    done = 1
                    next
                }
                { print }
            ' "$INSTALL_DIR/fresh.desktop" > "$apps_dir/fresh.desktop" 2>/dev/null
            then
                printf '%s\n' "$apps_dir/fresh.desktop" >> "$manifest"
                desktop_installed=1
            else
                rm -f "$apps_dir/fresh.desktop"
                log_warn "Could not write $apps_dir/fresh.desktop."
            fi
        fi
    fi

    # Both caches are optional: a desktop environment reads the directories
    # directly when they are absent, and neither tool exists on a headless box.
    if [ "$desktop_installed" -eq 1 ] && check_cmd update-desktop-database; then
        update-desktop-database "$DATA_HOME/applications" >/dev/null 2>&1 || true
    fi

    if [ "$icons_installed" -gt 0 ] && check_cmd gtk-update-icon-cache; then
        gtk-update-icon-cache -q -t -f "$DATA_HOME/icons/hicolor" >/dev/null 2>&1 || true
    fi

    if [ "$desktop_installed" -eq 1 ] || [ "$icons_installed" -gt 0 ]; then
        log_success "Desktop entry and $icons_installed icon(s) installed under $DATA_HOME"
    else
        # Nothing outside INSTALL_DIR, so nothing to record and nothing for the
        # next run to clean up.
        rm -f "$manifest"
    fi
    return 0
}

do_install_tarball() {
    mark_tried tarball
    if [ "$(uname -s)" != "Linux" ]; then
        log_warn "The universal build is Linux-only."
        return 1
    fi

    TARGET=$(musl_target) || {
        log_warn "No prebuilt binary for architecture: $(uname -m)."
        return 1
    }

    # gzip, not xz: this script unpacks with the system `tar`, and `.tar.xz`
    # needs the xz binary, which minimal images often do not ship. Every system
    # can already read gzip. (The dist archives are xz; those are unpacked by
    # wrapper installers that bring their own tooling.)
    ASSET="${BIN_NAME}-${TARGET}.tar.gz"

    # This filename carries no version, so it can be asked for by name directly.
    URL="https://github.com/${REPO_OWNER}/${REPO_NAME}/releases/latest/download/${ASSET}"

    log_info "Installing the universal build ($TARGET)..."
    make_workdir

    ARCHIVE="$WORKDIR/$ASSET"

    # The download is its own availability check; download_verified reports why
    # if it fails.
    download_verified "$URL" "$ARCHIVE" || return 1

    EXTRACT="$WORKDIR/extract"
    mkdir -p "$EXTRACT"
    tar -xzf "$ARCHIVE" -C "$EXTRACT" 2>/dev/null \
        || log_error "could not unpack $ASSET."

    STAGED="$EXTRACT/${BIN_NAME}-${TARGET}"
    [ -x "$STAGED/fresh" ] || log_error "the archive did not contain a 'fresh' binary."

    # Provenance must be recorded, never inferred. The editor reads
    # install-receipt.toml next to the binary to resolve channel=tarball
    # authoritatively; with no receipt it falls back to the path heuristic,
    # which is deliberately not trusted enough to permit a self-update.
    #
    # Release archives carry their own receipt (written at build time, with the
    # exact version and target), so that one wins. Archives published before
    # that landed do not, and this script knows perfectly well what it just did
    # — so it writes the receipt itself rather than leaving the install to be
    # guessed at.
    if [ -f "$STAGED/install-receipt.toml" ]; then
        log_info "Archive carries an install receipt; keeping it."
    else
        log_info "Archive predates build-time receipts; recording provenance here."
        cat > "$STAGED/install-receipt.toml" <<EOF
schema = 1
channel = "tarball"
package_name = "fresh-editor"
managed = false
self_update = true

[hints]
target = "$TARGET"
asset = "$ASSET"
EOF
    fi

    case "$INSTALL_DIR" in
        ""|"/"|"$HOME") log_error "refusing to install into '$INSTALL_DIR'." ;;
    esac

    log_info "Finalizing installation..."
    mkdir -p "$(dirname "$INSTALL_DIR")" "$BIN_DIR"
    # Before the manifest goes away with the directory that holds it.
    prune_recorded_files
    rm -rf "$INSTALL_DIR"
    mv "$STAGED" "$INSTALL_DIR" || log_error "failed to move files to $INSTALL_DIR."

    ln -sf "$INSTALL_DIR/fresh" "$BIN_DIR/fresh"

    integrate_desktop_files

    case ":$PATH:" in
        *":${BIN_DIR}:"*) ;;
        *)
            log_warn "${BIN_DIR} is not in your PATH."
            log_info "Add this to your shell profile:"
            log_info "  export PATH=\"${BIN_DIR}:\$PATH\""
            ;;
    esac

    log_success "Installed to $INSTALL_DIR"
    log_success "Symlink created at $BIN_DIR/fresh"
    log_info "Update later with: fresh --cmd update"
    return 0
}

# --- Distro packages ---

install_macos() {
    if check_cmd brew; then
        log_info "Installing via Homebrew..."
        brew install "${BIN_NAME}"
    else
        log_warn "Homebrew not found."
        run_fallbacks
    fi
}

install_arch() {
    log_info "Installing from the AUR..."

    # Try configured AUR helpers in order
    for helper in $AUR_HELPER_PRIORITY; do
        if check_cmd "$helper"; then
            log_info "Found AUR helper '$helper'. Installing ${BIN_NAME}-bin..."
            "$helper" -S --noconfirm "${BIN_NAME}-bin"
            return 0
        fi
    done

    # Fallback to manual AUR build
    log_info "No AUR helper found. Building '${BIN_NAME}-bin' manually..."

    if ! check_cmd git || ! check_cmd makepkg; then
         log_error "git and makepkg are required for manual AUR installation."
    fi

    make_workdir
    cur_dir=$(pwd)

    cd "$WORKDIR"
    git clone "https://aur.archlinux.org/${BIN_NAME}-bin.git"
    cd "${BIN_NAME}-bin"

    log_info "Running makepkg (you may be asked for sudo password)..."
    makepkg --syncdeps --install --noconfirm

    cd "$cur_dir"
    return 0
}

install_debian() {
    log_info "Looking for a .deb..."
    check_cmd curl || log_error "curl is required."
    check_cmd dpkg || log_error "dpkg not found; this does not look like a Debian-based system."

    ARCH=$(dpkg --print-architecture)
    VERSION=$(latest_version) || return 1
    URL=$(release_asset_url "${BIN_NAME}_${VERSION}-1_${ARCH}.deb")

    make_workdir
    TEMP_DEB="$WORKDIR/${BIN_NAME}.deb"
    download_verified "$URL" "$TEMP_DEB" || log_error "download failed: $URL"

    log_info "Installing via dpkg (you may be asked for your password)..."
    run_privileged dpkg -i "$TEMP_DEB"
    log_info "Update later with your package manager, or: fresh --cmd update"
    return 0
}

install_fedora() {
    log_info "Looking for an .rpm..."
    check_cmd curl || log_error "curl is required."

    ARCH=$(uname -m)
    VERSION=$(latest_version) || return 1
    URL=$(release_asset_url "${BIN_NAME}-${VERSION}-1.${ARCH}.rpm")

    make_workdir
    TEMP_RPM="$WORKDIR/${BIN_NAME}.rpm"
    download_verified "$URL" "$TEMP_RPM" || log_error "download failed: $URL"

    log_info "Installing via rpm (you may be asked for your password)..."
    if check_cmd zypper; then
        run_privileged zypper --no-refresh install --allow-unsigned-rpm -y "$TEMP_RPM"
    else
        run_privileged rpm -U "$TEMP_RPM"
    fi
    log_info "Update later with your package manager, or: fresh --cmd update"
    return 0
}

# --- Other universal installers ---

do_install_appimage() {
    mark_tried appimage
    log_info "Attempting AppImage install..."
    check_cmd curl || log_error "curl is required."

    case "$(uname -m)" in
        x86_64)        APPIMAGE_ARCH="x86_64" ;;
        aarch64|arm64) APPIMAGE_ARCH="aarch64" ;;
        *) log_warn "AppImage not available for architecture: $(uname -m)"; return 1 ;;
    esac

    VERSION=$(latest_version) || return 1
    # Named exactly: the release also ships a gui AppImage for the same arch.
    URL=$(release_asset_url "${BIN_NAME}-${VERSION}-${APPIMAGE_ARCH}.AppImage")

    make_workdir
    TEMP_APPIMAGE="$WORKDIR/fresh.AppImage"
    TEMP_EXTRACT="$WORKDIR/appimage-extract"
    mkdir -p "$TEMP_EXTRACT"

    download_verified "$URL" "$TEMP_APPIMAGE" || log_error "download failed: $URL"
    chmod +x "$TEMP_APPIMAGE"

    log_info "Extracting AppImage..."
    if ! (cd "$TEMP_EXTRACT" && "$TEMP_APPIMAGE" --appimage-extract > /dev/null 2>&1); then
        log_error "Extraction failed (Check disk space or binary compatibility)."
    fi

    if [ ! -d "$TEMP_EXTRACT/squashfs-root" ]; then
        log_error "Extraction completed but source files are missing."
    fi

    case "$INSTALL_DIR" in
        ""|"/"|"$HOME") log_error "refusing to install into '$INSTALL_DIR'." ;;
    esac

    log_info "Finalizing installation..."
    mkdir -p "$(dirname "$INSTALL_DIR")" "$BIN_DIR"
    rm -rf "$INSTALL_DIR"
    mv "$TEMP_EXTRACT/squashfs-root" "$INSTALL_DIR" || log_error "Failed to move files to $INSTALL_DIR"

    ln -sf "$INSTALL_DIR/usr/bin/fresh" "$BIN_DIR/fresh"

    # Provenance receipt next to the extracted binary so the editor knows it
    # was installed as an AppImage by this script and can self-update the same
    # way. install_root records where to swap on update.
    cat > "$INSTALL_DIR/usr/bin/install-receipt.toml" <<EOF
schema = 1
channel = "appimage"
package_name = "fresh-editor"
managed = false
self_update = true

[hints]
install_root = "$INSTALL_DIR"
EOF

    case ":$PATH:" in
        *":${BIN_DIR}:"*) ;;
        *)
            log_warn "${BIN_DIR} is not in your PATH."
            log_info "Add this to your shell profile:"
            log_info "  export PATH=\"${BIN_DIR}:\$PATH\""
            ;;
    esac

    log_success "Installed to $INSTALL_DIR"
    log_success "Symlink created at $BIN_DIR/fresh"
    return 0
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
    log_info "Checking fallback methods in order: $FALLBACK_PRIORITY"

    for method in $FALLBACK_PRIORITY; do
        case "$method" in
            tarball)
                if [ "$(uname -s)" = "Linux" ] && ! already_tried tarball; then
                    if do_install_tarball; then return 0; fi
                fi
                ;;
            appimage)
                if [ "$(uname -s)" = "Linux" ] && ! already_tried appimage; then
                    if do_install_appimage; then return 0; fi
                fi
                ;;
            nix)
                if check_cmd nix; then do_install_nix; return 0; fi
                ;;
            cargo)
                if check_cmd cargo; then do_install_cargo; return 0; fi
                ;;
            npm)
                if check_cmd npm; then do_install_npm; return 0; fi
                ;;
        esac
    done

    log_error "Installation failed. No usable install method found (tried: $FALLBACK_PRIORITY)."
}

# --- Method selection ---

# The distro-native method a user on this system might prefer, if any. Used
# only to point it out — it is never selected automatically, because a package
# install needs root and moves updates to the distro's package manager.
native_method_for_distro() {
    [ -f /etc/os-release ] || return 1
    # shellcheck disable=SC1091
    . /etc/os-release
    case "$ID" in
        ubuntu|debian|linuxmint|pop|kali)  echo deb; return 0 ;;
        fedora|rhel|centos)                echo rpm; return 0 ;;
        opensuse*|suse)                    echo rpm; return 0 ;;
        arch|manjaro|endeavouros)          echo aur; return 0 ;;
    esac
    case "$ID_LIKE" in
        *debian*)        echo deb; return 0 ;;
        *fedora*|*suse*) echo rpm; return 0 ;;
        *arch*)          echo aur; return 0 ;;
    esac
    return 1
}

is_nixos() {
    [ -f /etc/os-release ] || return 1
    # shellcheck disable=SC1091
    . /etc/os-release
    [ "$ID" = "nixos" ]
}

resolve_auto_method() {
    case "$(uname -s)" in
        Darwin*) echo brew ;;
        Linux*)
            # NixOS has no writable ~/.local story worth fighting; its own
            # package manager is already the right answer there.
            if is_nixos; then echo nix; else echo tarball; fi
            ;;
        *) echo tarball ;;
    esac
}

if [ "$METHOD" = "auto" ]; then
    METHOD=$(resolve_auto_method)
    if [ "$METHOD" = "tarball" ]; then
        log_info "Installing the universal build: self-updating, user-owned, no root."
        if native=$(native_method_for_distro); then
            log_info "Prefer your distro's package manager? Re-run with --method=$native"
        fi
    fi
fi

# --- Dispatch ---

case "$METHOD" in
    tarball)  do_install_tarball || run_fallbacks ;;
    deb)      install_debian     || run_fallbacks ;;
    rpm)      install_fedora     || run_fallbacks ;;
    aur)      install_arch ;;
    brew)     install_macos ;;
    nix)      do_install_nix ;;
    cargo)    do_install_cargo ;;
    npm)      do_install_npm ;;
    appimage) do_install_appimage || run_fallbacks ;;
    *)        log_error "unknown method: $METHOD (try --help)" ;;
esac

log_success "Installation completed!"
