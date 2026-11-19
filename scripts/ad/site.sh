#!/usr/bin/env bash
# Refresh the landing page's media:
#   homepage/public/assets/shots/*.webp            stills of Fresh in kitty (kitty_shots.py)
#   homepage/public/assets/orchestrator.{webp,mp4} the Orchestrator loop, and its still
#                                                  shots/orchestrator.webp (kitty_orchestrator.py)
#   homepage/public/assets/fresh-film.mp4          the calm cut, 720x1280, plus its poster
# The stills need kitty, Xvfb, ImageMagick and JetBrainsMono Nerd Font (see README.md);
# the film needs a build.sh run first and is skipped without one.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
export AD_WORK="${AD_WORK:-$REPO/target/ad}"
OUT="$REPO/homepage/public/assets"
FFMPEG="${FFMPEG:-$(command -v ffmpeg || python3 -c 'import imageio_ffmpeg; print(imageio_ffmpeg.get_ffmpeg_exe())')}"
export FFMPEG

[[ -x "$REPO/target/debug/fresh" ]] || cargo build -p fresh-editor --manifest-path "$REPO/Cargo.toml"
"$HERE/setup.sh"
python3 "$HERE/kitty_shots.py"
python3 "$HERE/kitty_orchestrator.py"

cd "$AD_WORK"
if [[ -f fresh-ad-calm.mp4 ]]; then
  "$FFMPEG" -loglevel error -y -i fresh-ad-calm.mp4 -vf "scale=720:1280:flags=lanczos" \
    -c:v libx264 -preset slow -crf 25 -profile:v high -pix_fmt yuv420p -c:a aac -b:a 128k \
    -movflags +faststart "$OUT/fresh-film.mp4"
  "$FFMPEG" -loglevel error -y -ss 5.6 -i fresh-ad-calm.mp4 -frames:v 1 -vf "scale=720:1280:flags=lanczos" poster.png
  python3 -c "from PIL import Image; Image.open('poster.png').convert('RGB').save('$OUT/fresh-film-poster.webp', 'WEBP', quality=82, method=6)"
else
  echo "no fresh-ad-calm.mp4 in $AD_WORK (run build.sh): film left as it is"
fi
echo "landing page media updated in $OUT"
