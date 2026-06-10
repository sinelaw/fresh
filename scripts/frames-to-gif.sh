#!/usr/bin/env bash
set -euo pipefail

# frames-to-gif.sh - Convert blog showcase SVG frames to animated GIF
#
# Usage: scripts/frames-to-gif.sh <blog-dir> [--scale FACTOR] [--colors N] [--dither MODE]
# Example: scripts/frames-to-gif.sh docs/blog/multi-cursor
#          scripts/frames-to-gif.sh docs/blog/multi-cursor --scale 1.5
#          scripts/frames-to-gif.sh docs/blog/wave --colors 32 --dither none
#
# --colors / --dither tune the GIF palette (ffmpeg path). Terminal frames are
# flat-colour, so `--dither none` with a small palette is much smaller — and
# just as crisp — as the default dithered 128-colour palette; handy for dense,
# high-frame-rate animations like the wave screensaver. Defaults are unchanged.

BLOG_DIR="${1:?Usage: $0 <blog-dir> [--scale FACTOR] [--colors N] [--dither MODE]}"
SCALE="1.0"
COLORS="128"
DITHER="bayer:bayer_scale=3"

shift
while [[ $# -gt 0 ]]; do
    case "$1" in
        --scale) SCALE="$2"; shift 2 ;;
        --colors) COLORS="$2"; shift 2 ;;
        --dither) DITHER="$2"; shift 2 ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

SHOWCASE_JSON="$BLOG_DIR/showcase.json"
FRAMES_DIR="$BLOG_DIR/frames"
OUTPUT_GIF="$BLOG_DIR/showcase.gif"

# Validate inputs
if [[ ! -f "$SHOWCASE_JSON" ]]; then
    echo "Error: $SHOWCASE_JSON not found. Run the showcase test first." >&2
    exit 1
fi

if [[ ! -d "$FRAMES_DIR" ]]; then
    echo "Error: $FRAMES_DIR not found." >&2
    exit 1
fi

# Check dependencies
for cmd in resvg jq; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: '$cmd' is required but not found." >&2
        exit 1
    fi
done

if ! command -v ffmpeg &>/dev/null && ! command -v magick &>/dev/null; then
    echo "Error: 'ffmpeg' or 'magick' (ImageMagick) is required." >&2
    exit 1
fi

# Parse frame info from JSON
FRAME_COUNT=$(jq '.frames | length' "$SHOWCASE_JSON")
echo "Processing $FRAME_COUNT frames from $SHOWCASE_JSON"

# Create temp directory for PNGs
PNG_DIR=$(mktemp -d)
trap 'rm -rf "$PNG_DIR"' EXIT

# Step 1: Convert SVG frames to PNG
echo "Converting SVG → PNG (scale: ${SCALE}x)..."
for i in $(seq 0 $((FRAME_COUNT - 1))); do
    FILENAME=$(jq -r ".frames[$i].filename" "$SHOWCASE_JSON")
    SVG_PATH="$FRAMES_DIR/$FILENAME"
    PNG_PATH="$PNG_DIR/frame_$(printf '%04d' "$i").png"

    if [[ ! -f "$SVG_PATH" ]]; then
        echo "Warning: $SVG_PATH not found, skipping" >&2
        continue
    fi

    # resvg with optional scaling
    if [[ "$SCALE" != "1.0" && "$SCALE" != "1" ]]; then
        # Calculate scaled dimensions
        WIDTH=$(jq '.width' "$SHOWCASE_JSON")
        HEIGHT=$(jq '.height' "$SHOWCASE_JSON")
        # Terminal width/height are in cells; SVG uses 9px/cell wide, 18px/cell tall
        SVG_W=$((WIDTH * 9))
        SVG_H=$((HEIGHT * 18))
        SCALED_W=$(python3 -c "print(int($SVG_W * $SCALE))")
        SCALED_H=$(python3 -c "print(int($SVG_H * $SCALE))")
        resvg "$SVG_PATH" "$PNG_PATH" --width "$SCALED_W" --height "$SCALED_H" 2>/dev/null
    else
        resvg "$SVG_PATH" "$PNG_PATH" 2>/dev/null
    fi
done

echo "PNGs ready."

# Step 2: Create animated GIF
echo "Generating animated GIF..."

if command -v ffmpeg &>/dev/null; then
    # Build ffmpeg concat demuxer file with per-frame durations
    CONCAT_FILE="$PNG_DIR/concat.txt"
    for i in $(seq 0 $((FRAME_COUNT - 1))); do
        DURATION_MS=$(jq ".frames[$i].duration_ms" "$SHOWCASE_JSON")
        DURATION_S=$(python3 -c "print($DURATION_MS / 1000.0)")
        PNG_FILE="$PNG_DIR/frame_$(printf '%04d' "$i").png"
        if [[ -f "$PNG_FILE" ]]; then
            echo "file '$(realpath "$PNG_FILE")'" >> "$CONCAT_FILE"
            echo "duration $DURATION_S" >> "$CONCAT_FILE"
        fi
    done
    # Repeat last frame (ffmpeg concat needs it)
    LAST_PNG="$PNG_DIR/frame_$(printf '%04d' $((FRAME_COUNT - 1))).png"
    if [[ -f "$LAST_PNG" ]]; then
        echo "file '$(realpath "$LAST_PNG")'" >> "$CONCAT_FILE"
    fi

    # Generate palette for high-quality GIF
    PALETTE="$PNG_DIR/palette.png"
    ffmpeg -y -f concat -safe 0 -i "$CONCAT_FILE" \
        -vf "palettegen=max_colors=${COLORS}:stats_mode=diff" \
        "$PALETTE" 2>/dev/null

    # Generate final GIF using palette
    ffmpeg -y -f concat -safe 0 -i "$CONCAT_FILE" -i "$PALETTE" \
        -lavfi "paletteuse=dither=${DITHER}:diff_mode=rectangle" \
        -loop 0 \
        "$OUTPUT_GIF" 2>/dev/null

elif command -v magick &>/dev/null; then
    # ImageMagick fallback
    DELAY_ARGS=()
    for i in $(seq 0 $((FRAME_COUNT - 1))); do
        DURATION_MS=$(jq ".frames[$i].duration_ms" "$SHOWCASE_JSON")
        # ImageMagick delay is in 1/100ths of a second
        DELAY=$((DURATION_MS / 10))
        PNG_FILE="$PNG_DIR/frame_$(printf '%04d' "$i").png"
        if [[ -f "$PNG_FILE" ]]; then
            DELAY_ARGS+=(-delay "$DELAY" "$PNG_FILE")
        fi
    done
    magick "${DELAY_ARGS[@]}" -loop 0 "$OUTPUT_GIF"
fi

if [[ -f "$OUTPUT_GIF" ]]; then
    SIZE=$(du -h "$OUTPUT_GIF" | cut -f1)
    echo "✓ Created $OUTPUT_GIF ($SIZE)"
else
    echo "Error: Failed to create GIF" >&2
    exit 1
fi
