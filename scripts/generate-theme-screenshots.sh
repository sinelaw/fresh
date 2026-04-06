#!/usr/bin/env bash
set -euo pipefail

# generate-theme-screenshots.sh - Generate screenshot galleries for built-in themes
#
# Usage:
#   scripts/generate-theme-screenshots.sh              # all built-in themes
#   scripts/generate-theme-screenshots.sh dracula       # single theme
#   scripts/generate-theme-screenshots.sh dark light    # specific themes

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

BUILTIN_THEMES=(dark light high-contrast nostalgia dracula nord solarized-dark)

cd "$ROOT_DIR"

# Determine which themes to process
if [[ $# -gt 0 ]]; then
    THEMES=("$@")
else
    THEMES=("${BUILTIN_THEMES[@]}")
fi

echo "=== Generating theme screenshot galleries ==="
echo "Themes: ${THEMES[*]}"
echo ""

fail=0
for theme in "${THEMES[@]}"; do
    echo "--- Theme: $theme ---"

    FRESH_THEME="$theme" cargo nextest run \
        --package fresh-editor \
        --test e2e_tests \
        -E 'test(theme_screenshot_gallery)' \
        --run-ignored ignored-only \
        --no-capture \
        2>&1 | tail -5

    gallery_dir="docs/blog/theme-gallery/$theme"
    if [[ -d "$gallery_dir/frames" ]]; then
        frame_count=$(find "$gallery_dir/frames" -name '*.svg' | wc -l)
        echo "OK $theme ($frame_count frames in $gallery_dir/frames/)"

        # Generate GIF if frames-to-gif.sh exists and frames were produced
        if [[ $frame_count -gt 0 ]] && [[ -x "$SCRIPT_DIR/frames-to-gif.sh" ]]; then
            if "$SCRIPT_DIR/frames-to-gif.sh" "$gallery_dir" 2>&1 | tail -1; then
                :
            else
                echo "WARN $theme: GIF generation failed (frames are still available)"
            fi
        fi
    else
        echo "FAIL $theme (no frames generated)"
        fail=1
    fi
    echo ""
done

if [[ $fail -ne 0 ]]; then
    echo "Some themes failed to generate screenshots."
    exit 1
fi

echo "Done. Theme galleries are in docs/blog/theme-gallery/*/"
