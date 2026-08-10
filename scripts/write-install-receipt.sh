#!/bin/sh
# Write an install-receipt.toml recording how `fresh` was installed.
#
# Usage: write-install-receipt.sh <channel> <output-file> [hint_key=value ...]
#
# The receipt is read at runtime by the `fresh-update` crate to resolve
# provenance authoritatively and update through the same mechanism. See
# docs/internal/packaging-self-update.md.
#
# `managed` and `self_update` are derived from the channel (mirroring the
# registry's UpdateKind) so callers only pass the channel id and any
# channel-specific hints (e.g. `formula=fresh-editor`, `target=...`).

set -eu

CHANNEL="${1:-}"
OUT="${2:-}"
if [ -z "$CHANNEL" ] || [ -z "$OUT" ]; then
    echo "usage: $0 <channel> <output-file> [key=value ...]" >&2
    exit 2
fi
shift 2

# Derive managed / self_update from the channel (keep in sync with
# fresh-update::registry::kind_for).
case "$CHANNEL" in
    homebrew|apt|dnf|zypper|pacman|aur|aur-bin|flatpak|snap|winget|scoop|chocolatey|nix|freebsd-pkg)
        MANAGED=true;  SELF_UPDATE=false ;;   # Delegated
    cargo|cargo-binstall|npm|mise)
        MANAGED=true;  SELF_UPDATE=false ;;   # Toolchain
    appimage|tarball|prebuilt)
        MANAGED=false; SELF_UPDATE=true  ;;   # SelfContained
    *)
        MANAGED=false; SELF_UPDATE=false ;;   # Manual / unknown
esac

# Version: explicit $FRESH_VERSION wins, else parse the workspace Cargo.toml if
# reachable, else omit.
VERSION="${FRESH_VERSION:-}"
if [ -z "$VERSION" ]; then
    for ct in Cargo.toml ../../Cargo.toml ../Cargo.toml; do
        if [ -f "$ct" ]; then
            VERSION=$(awk -F'"' '
                /^\[workspace\.package\]/ { in_wp=1; next }
                /^\[/                     { in_wp=0 }
                in_wp && /^version[[:space:]]*=/ { print $2; exit }' "$ct")
            [ -n "$VERSION" ] && break
        fi
    done
fi

# Timestamp (best-effort; -u for UTC). Omitted if `date` is unavailable.
INSTALLED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || true)

mkdir -p "$(dirname "$OUT")"

{
    echo "schema = 1"
    echo "channel = \"$CHANNEL\""
    [ -n "$VERSION" ]      && echo "version = \"$VERSION\""
    echo "package_name = \"fresh-editor\""
    [ -n "$INSTALLED_AT" ] && echo "installed_at = \"$INSTALLED_AT\""
    echo "managed = $MANAGED"
    echo "self_update = $SELF_UPDATE"
    if [ "$#" -gt 0 ]; then
        echo ""
        echo "[hints]"
        for kv in "$@"; do
            key=${kv%%=*}
            val=${kv#*=}
            echo "$key = \"$val\""
        done
    fi
} > "$OUT"

echo "wrote install receipt ($CHANNEL) -> $OUT" >&2
