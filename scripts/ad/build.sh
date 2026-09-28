#!/usr/bin/env bash
# Record every clip, synthesize the soundtrack and render the 30 s vertical ad.
#   scripts/ad/build.sh            # full run
#   SKIP_RECORD=1 scripts/ad/build.sh   # re-render from existing recordings
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
export AD_WORK="${AD_WORK:-$REPO/target/ad}"
CLIPS="code mouse palette themes multicursor grep huge review terminal blitz agents"

[[ -x "$REPO/target/debug/fresh" ]] || cargo build -p fresh-editor --manifest-path "$REPO/Cargo.toml"
"$HERE/setup.sh"
if [[ -z "${SKIP_RECORD:-}" ]]; then
  python3 "$HERE/rec.py" $CLIPS
fi
for c in $CLIPS; do python3 "$HERE/cast2frames.py" "$AD_WORK/casts/$c.cast" "$AD_WORK/frames/$c.json"; done
python3 "$HERE/music.py" "$AD_WORK/music.wav"

cp "$HERE/ad.html" "$HERE/scenes.js" "$HERE/render.mjs" "$AD_WORK/"
cd "$AD_WORK"
PORT="${AD_PORT:-8765}"
python3 -m http.server "$PORT" --bind 127.0.0.1 >/dev/null 2>&1 &
SERVER=$!
trap 'kill $SERVER 2>/dev/null || true' EXIT
sleep 1
kill -0 $SERVER 2>/dev/null || { echo "port $PORT is busy; set AD_PORT" >&2; exit 1; }
FFMPEG="${FFMPEG:-$(command -v ffmpeg || python3 -c 'import imageio_ffmpeg; print(imageio_ffmpeg.get_ffmpeg_exe())')}"
AD_PORT="$PORT" FFMPEG="$FFMPEG" node render.mjs video raw.mp4
# loudness to about -14 LUFS, where the social platforms normalise
"$FFMPEG" -loglevel error -y -i raw.mp4 -c:v copy -af volume=-4.5dB -c:a aac -b:a 192k -movflags +faststart fresh-ad.mp4
echo "wrote $AD_WORK/fresh-ad.mp4"
