#!/usr/bin/env bash
# Refresh the landing page's media from the ad recordings:
#   homepage/public/assets/shots/*.webp   stills cropped from the clips (shots.js)
#   homepage/public/assets/fresh-film.mp4 the calm cut, 720x1280, plus its poster
# Run build.sh first (it records the clips and renders fresh-ad-calm.mp4).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
export AD_WORK="${AD_WORK:-$REPO/target/ad}"
OUT="$REPO/homepage/public/assets"
FFMPEG="${FFMPEG:-$(command -v ffmpeg || python3 -c 'import imageio_ffmpeg; print(imageio_ffmpeg.get_ffmpeg_exe())')}"

cp "$HERE"/{engine.js,shots.html,shots.js,render.mjs} "$AD_WORK/"
cd "$AD_WORK"
PORT="${AD_PORT:-8765}"
python3 -m http.server "$PORT" --bind 127.0.0.1 >/dev/null 2>&1 &
SERVER=$!
trap 'kill $SERVER 2>/dev/null || true' EXIT
sleep 1

SHOTS=$(node -e "eval(require('fs').readFileSync('shots.js','utf8') + ';console.log(Object.keys(SHOTS).join(\" \"))')")
rm -rf shots   # only this run's stills, not leftovers from an older SHOTS list
AD_PORT="$PORT" AD_PAGE=shots.html node render.mjs shots shots $SHOTS
mkdir -p "$OUT/shots"
python3 - "$OUT/shots" <<'PY'
import glob, os, sys
from PIL import Image
out = sys.argv[1]
for p in sorted(glob.glob('shots/*.png')):
    im = Image.open(p).convert('RGB')
    name = os.path.basename(p)[:-4]
    # dense screens (the log) compress poorly; keep them a little smaller
    width, q = (1600, 80) if name == 'huge-file' else (1600, 84) if name.startswith('theme-') else (2000, 88)
    if im.width > width:
        im = im.resize((width, round(im.height * width / im.width)), Image.LANCZOS)
    im.save(f'{out}/{name}.webp', 'WEBP', quality=q, method=6)
    print(name, im.size)
PY

"$FFMPEG" -loglevel error -y -i fresh-ad-calm.mp4 -vf "scale=720:1280:flags=lanczos" \
  -c:v libx264 -preset slow -crf 25 -profile:v high -pix_fmt yuv420p -c:a aac -b:a 128k \
  -movflags +faststart "$OUT/fresh-film.mp4"
"$FFMPEG" -loglevel error -y -ss 5.6 -i fresh-ad-calm.mp4 -frames:v 1 -vf "scale=720:1280:flags=lanczos" poster.png
python3 -c "from PIL import Image; Image.open('poster.png').convert('RGB').save('$OUT/fresh-film-poster.webp', 'WEBP', quality=82, method=6)"
echo "landing page media updated in $OUT"
