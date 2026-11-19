#!/usr/bin/env bash
# Refresh the landing page's media:
#   homepage/public/assets/shots/*.webp            stills of Fresh in kitty (kitty_shots.py)
#   homepage/public/assets/orchestrator.{webp,mp4} the Orchestrator loop, and its still
#                                                  shots/orchestrator.webp (kitty_orchestrator.py)
# The hero slides between the stills, so they are all the page needs.
# Needs kitty, Xvfb, ImageMagick and JetBrainsMono Nerd Font (see README.md).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
export AD_WORK="${AD_WORK:-$REPO/target/ad}"
OUT="$REPO/homepage/public/assets"
export FFMPEG="${FFMPEG:-$(command -v ffmpeg || python3 -c 'import imageio_ffmpeg; print(imageio_ffmpeg.get_ffmpeg_exe())')}"

[[ -x "$REPO/target/debug/fresh" ]] || cargo build -p fresh-editor --manifest-path "$REPO/Cargo.toml"
"$HERE/setup.sh"
python3 "$HERE/kitty_shots.py"
python3 "$HERE/kitty_orchestrator.py"

echo "landing page media updated in $OUT"
