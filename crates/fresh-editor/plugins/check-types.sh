#!/bin/bash
# TypeScript type checker for Fresh plugins
# Usage: ./check-types.sh [files...]
# If no files specified, checks all plugin files
#
# Each file is checked individually because plugins run in separate
# global scopes at runtime, so variables like `editor` don't conflict.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Default to all plugin files if no arguments
if [ $# -eq 0 ]; then
  FILES=(*.ts lib/*.ts)
else
  FILES=("$@")
fi

echo "Checking TypeScript types for ${#FILES[@]} files..."

ERRORS=0
for file in "${FILES[@]}"; do
  if ! npx -p typescript tsc \
    --noEmit \
    --target esnext \
    --moduleResolution node \
    --lib esnext,dom \
    --skipLibCheck \
    --allowImportingTsExtensions \
    "$file" 2>&1; then
    ERRORS=$((ERRORS + 1))
  fi
done

if [ $ERRORS -eq 0 ]; then
  echo "All type checks passed!"
else
  echo ""
  echo "$ERRORS file(s) had type errors"
  exit 1
fi
