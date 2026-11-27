#!/bin/sh
# Type-check the web client (every module under web/js and web/plugins; the
# vendored xterm.js is declared, not checked) with inty:
# https://github.com/sinelaw/inty. `INTY` names the binary (default: `inty`
# on the PATH). Extra arguments go to inty (e.g. `--timings`).
#
# First, the annotations must be laid out as `inty fmt` lays them out: it
# lists the files that would change and fails (fix with `inty fmt web/js
# web/plugins types`).
set -e
here=$(cd "$(dirname "$0")" && pwd)
inty=${INTY:-inty}
cd "$here/web"
"$inty" fmt --check js plugins ../types
exec "$inty" \
  --lib "$here/types/browser.d.js" \
  --lib "$here/types/fresh.d.js" \
  "$@" js/*.js plugins/*.js
