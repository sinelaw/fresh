#!/usr/bin/env bash
set -euo pipefail

# update-showcases.sh - Run all blog showcase tests and generate animated GIFs
#
# Usage:
#   scripts/update-showcases.sh              # run all showcases
#   scripts/update-showcases.sh multi-cursor # run only matching showcases

FILTER="${1:-blog_showcase_}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BLOG_DIR="$ROOT_DIR/docs/blog"

cd "$ROOT_DIR"

echo "=== Running showcase tests (filter: $FILTER) ==="
cargo test --package fresh-editor --test all_tests "$FILTER" -- --ignored --nocapture

echo ""
echo "=== Generating GIFs ==="
fail=0
for json in $(find "$BLOG_DIR" -name showcase.json | sort); do
    dir="$(dirname "$json")"
    rel="${dir#"$ROOT_DIR/"}"
    if [[ ! -d "$dir/frames" ]]; then
        echo "SKIP $rel (no frames/)"
        continue
    fi
    if "$SCRIPT_DIR/frames-to-gif.sh" "$rel" 2>&1 | tail -1; then
        :
    else
        echo "FAIL $rel"
        fail=1
    fi
done

if [[ $fail -ne 0 ]]; then
    echo "Some GIFs failed to generate."
    exit 1
fi

# The landing page is not part of the VitePress build — it is copied verbatim
# with homepage/public/, so a GIF it embeds needs its own copy there. Keep the
# ones it uses in sync with the showcase they came from.
declare -A HOMEPAGE_GIFS=(
    ["orchestrator-worktrees"]="orchestrator.gif"
)
for src_dir in "${!HOMEPAGE_GIFS[@]}"; do
    src="$BLOG_DIR/$src_dir/showcase.gif"
    dest="$ROOT_DIR/homepage/public/assets/${HOMEPAGE_GIFS[$src_dir]}"
    if [[ -f "$src" ]]; then
        cp "$src" "$dest"
        echo "✓ Copied $src_dir/showcase.gif → homepage/public/assets/${HOMEPAGE_GIFS[$src_dir]}"
    fi
done

echo ""
echo "Done. All showcases updated."
