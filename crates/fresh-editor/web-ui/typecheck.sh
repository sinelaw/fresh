#!/usr/bin/env bash
# Type-check the web UI's JavaScript with inty (https://github.com/sinelaw/inty).
#
#   web-ui/typecheck.sh            # inty on PATH, or INTY=/path/to/inty
#
# The page's scripts share one scope (build.rs concatenates js/*.js in
# filename order into a single <script>), so they are checked the same way:
# as one concatenated program. js/00-ffi.js is left out; its typed surface is
# types/ffi.d.js. The browser API and the server's scene protocol are
# declared in types/dom.d.js and types/scene.d.js.
#
# Diagnostics name the concatenated file; their `file:line:col` headers are
# rewritten to the source file and line they came from.
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INTY="${INTY:-inty}"
if ! command -v "$INTY" >/dev/null 2>&1; then
  echo "inty not found: install it (cargo install --git https://github.com/sinelaw/inty inty-cli)" >&2
  echo "or point INTY at the binary" >&2
  exit 2
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
ALL="$WORK/web-ui.js"
MAP="$WORK/map"

# Concatenate exactly as build.rs does, recording where each file starts.
: >"$ALL"
: >"$MAP"
line=1
for f in "$DIR"/js/*.js; do
  name="js/$(basename "$f")"
  [ "$name" = "js/00-ffi.js" ] && continue
  echo "$line $name" >>"$MAP"
  cat "$f" >>"$ALL"
  # A file without a trailing newline runs into the next one, as in the page.
  line=$(( $(wc -l <"$ALL") + 1 ))
done

status=0
"$INTY" --no-color \
  --lib "$DIR/types/dom.d.js" \
  --lib "$DIR/types/scene.d.js" \
  --lib "$DIR/types/ffi.d.js" \
  "$ALL" >"$WORK/out" 2>&1 || status=$?

# A clean run prints the whole program with its inferred types, then a
# summary line; keep just the summary (and any warnings).
if [ "$status" -eq 0 ]; then
  grep -E '^(Warning|All checks passed)' "$WORK/out" | sed "s|$ALL|js/*.js|"
  exit 0
fi

# `[ /tmp/…/web-ui.js:LINE:COL ]` → `[ js/NN-name.js:LINE:COL ]`
awk -v all="$ALL" '
  NR == FNR { start[NR] = $1; file[NR] = $2; n = NR; next }
  {
    while ((i = index($0, all ":")) > 0) {
      rest = substr($0, i + length(all) + 1)
      match(rest, /^[0-9]+/)
      ln = substr(rest, 1, RLENGTH) + 0
      k = 1
      for (j = 1; j <= n; j++) if (start[j] <= ln) k = j
      $0 = substr($0, 1, i - 1) file[k] ":" (ln - start[k] + 1) substr(rest, RLENGTH + 1)
    }
    print
  }' "$MAP" "$WORK/out"
exit "$status"
