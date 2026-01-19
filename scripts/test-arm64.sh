#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
IMAGE_NAME="fresh-test-arm64"

# Build image if it doesn't exist
if ! podman image exists "$IMAGE_NAME"; then
    echo "Building $IMAGE_NAME image..."
    podman build --platform linux/arm64 -t "$IMAGE_NAME" -f "$SCRIPT_DIR/Containerfile.arm64-test" "$PROJECT_DIR"
fi

# Run container
exec podman run --rm -it --privileged --platform linux/arm64 \
    -v "$PROJECT_DIR:/work:Z" \
    "$IMAGE_NAME" \
    "${@:-bash}"
