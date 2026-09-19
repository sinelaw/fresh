#!/usr/bin/env bash
# Compute the bundled-dependency License: expression for fresh-editor.spec.
#
# Because the spec bundles every crate, Fedora requires the `License:` tag to
# be the AND of Fresh's own license (GPL-2.0-or-later) and the license of
# every bundled crate that is actually compiled into the `fresh` binary.
#
# Run on a Fedora host:
#   dnf install cargo cargo-license
#   ./fedora/gen-bundled-license.sh
#
# Paste the resulting expression into the `License:` line of the spec
# (keep `GPL-2.0-or-later AND (...)`).
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

if ! command -v cargo-license >/dev/null 2>&1; then
    echo "cargo-license not found. Install with: dnf install cargo-license" >&2
    exit 1
fi

echo ">> Unique SPDX licenses across the dependency tree built for \`fresh\`:"
echo ">> (resolved for the default features of the fresh-editor crate)"
echo

# --avoid-dev-deps: dev-dependencies aren't shipped in the binary.
# Restrict to the fresh-editor package so GUI-only deps (wgpu/gtk) that the
# default `fresh` binary never links are not over-counted.
cargo license \
    --avoid-dev-deps \
    --current-dir crates/fresh-editor \
    2>/dev/null \
  | sed 's/:.*//' \
  | sort -u

echo
echo ">> Combine these into an SPDX AND expression with Fresh's own license, e.g.:"
echo "   License: GPL-2.0-or-later AND (MIT OR Apache-2.0) AND ISC AND BSD-3-Clause AND ..."
echo ">> Validate every identifier against the Fedora allowed-licenses list:"
echo "   https://docs.fedoraproject.org/en-US/legal/allowed-licenses/"
echo ">> NOTE: map any deprecated ids (e.g. 'Apache-2.0 OR MIT' ordering, 'Unicode-DFS-2016'"
echo ">>       -> 'Unicode-3.0') to current SPDX before submitting."
