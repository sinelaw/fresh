#!/bin/sh
# Reproduce the Debian QuickJS spike end to end (docs/internal/debian-quickjs-spike.md):
#
#   1. find Debian's libquickjs (installed, or fetched and unpacked from the
#      Debian archive when it is not — amd64 only),
#   2. run the wrapper's engine tests against it,
#   3. dump every bundled plugin to the JS the runtime executes,
#   4. run them on the system QuickJS and on quickjs-ng (rquickjs) and compare.
#
# Outputs land in $WORK (default: target/compat next to this script).
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
work=${WORK:-$here/target/compat}
mkdir -p "$work"

if [ -z "${QUICKJS_INCLUDE_DIR:-}" ] && [ ! -f /usr/include/quickjs/quickjs.h ]; then
    deb_url=${LIBQUICKJS_DEB_URL:-https://deb.debian.org/debian/pool/main/q/quickjs/libquickjs_2025.04.26-1+b2_amd64.deb}
    echo "libquickjs not installed; unpacking $deb_url" >&2
    curl -sSfLo "$work/libquickjs.deb" "$deb_url"
    rm -rf "$work/libquickjs"
    dpkg-deb -x "$work/libquickjs.deb" "$work/libquickjs"
    export QUICKJS_INCLUDE_DIR="$work/libquickjs/usr/include/quickjs"
    export QUICKJS_LIB_DIR="$work/libquickjs/usr/lib/x86_64-linux-gnu/quickjs"
fi

(cd "$here" && cargo test -q -p fresh-quickjs)

(cd "$root" && cargo run -q -p fresh-parser-js --example dump_plugins_js -- \
    crates/fresh-editor/plugins \
    crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs \
    "$work/plugins-js")

cd "$here"
cargo run -q --release -p fresh-quickjs --example compat_system -- "$work/plugins-js" >"$work/system.jsonl"
cargo run -q --release -p compat-ng -- "$work/plugins-js" >"$work/ng.jsonl"
cargo run -q --release -p fresh-quickjs --example compat_system -- --probe >"$work/probe-system.txt"
cargo run -q --release -p compat-ng -- --probe >"$work/probe-ng.txt"

python3 -I compat/compare.py "$work/system.jsonl" "$work/ng.jsonl" \
    "$work/probe-system.txt" "$work/probe-ng.txt" "$work/plugins-js"
