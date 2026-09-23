#!/bin/bash
# Run the plugin unit tests. See tests/README.md.
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

NODE="${NODE:-node}"
command -v "$NODE" >/dev/null || { echo "node not found; set NODE=/path/to/node"; exit 127; }

FAILED=0
for test in *.test.ts; do
  [ -e "$test" ] || continue
  echo "── $test"
  "$NODE" --experimental-strip-types --no-warnings "$test" || FAILED=$((FAILED + 1))
done

if [ "$FAILED" -eq 0 ]; then
  echo "All plugin tests passed."
else
  echo "$FAILED test file(s) failed."
  exit 1
fi
