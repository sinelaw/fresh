#!/bin/bash
#
# Record a Fresh editor demo using asciinema
#
# Usage:
#   ./scripts/record-demo.sh [file]           # Record editing a file
#   ./scripts/record-demo.sh                  # Record with no file
#   ./scripts/record-demo.sh --render         # Convert last recording to GIF
#
# Requirements:
#   - asciinema: pip install asciinema (or via package manager)
#   - agg (for GIF): cargo install agg
#

set -e

RECORDING_PATH="/tmp/fresh-recording.cast"
OUTPUT_GIF="docs/demo.gif"

render_gif() {
    if [[ ! -f "$RECORDING_PATH" ]]; then
        echo "Error: No recording found at $RECORDING_PATH"
        echo "Run a recording first with: $0 [file]"
        exit 1
    fi

    if ! command -v agg &> /dev/null; then
        echo "Error: 'agg' not found. Install with: cargo install agg"
        exit 1
    fi

    echo "Rendering GIF..."
    agg --theme dracula \
        --font-size 14 \
        --speed 1.5 \
        "$RECORDING_PATH" \
        "$OUTPUT_GIF"

    echo "Done! GIF saved to: $OUTPUT_GIF"
}

record_demo() {
    if ! command -v asciinema &> /dev/null; then
        echo "Error: 'asciinema' not found."
        echo "Install with: pip install asciinema (or via your package manager)"
        exit 1
    fi

    # Build fresh first to avoid recording compilation
    echo "Building fresh..."
    cargo build --quiet

    local cmd="./target/release/fresh"
    if [[ -n "$1" ]]; then
        cmd="$cmd $1"
    fi

    echo "Starting recording..."
    echo "Press Ctrl-D or type 'exit' when done."
    echo ""

    asciinema rec \
        --overwrite \
        --title "Fresh Editor Demo" \
        --command "$cmd" \
        "$RECORDING_PATH"

    echo ""
    echo "Recording saved to: $RECORDING_PATH"
    echo "To convert to GIF: $0 --render"
}

# Main
case "${1:-}" in
    --render|-r)
        render_gif
        ;;
    --help|-h)
        echo "Usage: $0 [file]       Record editing a file"
        echo "       $0 --render     Convert recording to GIF"
        echo "       $0 --help       Show this help"
        ;;
    *)
        record_demo "$1"
        ;;
esac
