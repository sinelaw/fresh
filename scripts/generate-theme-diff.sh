#!/usr/bin/env bash
set -euo pipefail

# generate-theme-diff.sh - Render before/after screenshots for theme changes.
#
# Renders the editor with each changed theme's *base* colors and its
# *working-tree* colors, then writes an HTML gallery pairing them side by side.
# See docs/theme-screenshot-diff.md for the full spec.
#
# Usage:
#   scripts/generate-theme-diff.sh                 # themes changed vs base ref
#   FRESH_THEME_DIFF_THEMES=dark,nord scripts/generate-theme-diff.sh
#   FRESH_THEME_DIFF_ALL=1 scripts/generate-theme-diff.sh   # every built-in
#
# Env:
#   FRESH_THEME_BASE_REF    base ref to diff against (default: auto-detect
#                           merge-base with origin/master, else origin/master)
#   FRESH_THEME_DIFF_THEMES comma-separated theme names to restrict to
#   FRESH_THEME_DIFF_ALL    set to render all built-in themes
#
# Output: docs/blog/theme-diff/index.html

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROOT_DIR"

echo "=== Generating theme diff gallery ==="
echo "base ref: ${FRESH_THEME_BASE_REF:-<auto>}"
echo ""

cargo nextest run \
    --package fresh-editor \
    --test e2e_tests \
    -E 'test(theme_diff_gallery)' \
    --run-ignored ignored-only \
    --no-capture

echo ""
if [[ -f docs/blog/theme-diff/index.html ]]; then
    echo "OK — open docs/blog/theme-diff/index.html"
else
    echo "No gallery produced (no theme changes detected?)."
fi
