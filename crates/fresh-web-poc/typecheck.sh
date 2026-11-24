#!/bin/sh
# Type-check the web client (every module under web/js and web/plugins; the
# vendored xterm.js is declared, not checked) with inty:
# https://github.com/sinelaw/inty. `INTY` names the binary (default: `inty`
# on the PATH). Extra arguments go to inty (e.g. `--timings`).
set -e
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/web"
exec "${INTY:-inty}" \
  --lib "$here/types/browser.d.js" \
  --lib "$here/types/fresh.d.js" \
  "$@" js/*.js plugins/*.js
