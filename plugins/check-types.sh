#!/bin/bash
# TypeScript type checker for Fresh plugins
# Usage: ./check-types.sh [files...]
# If no files specified, checks all plugin files

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Default to all plugin files if no arguments
if [ $# -eq 0 ]; then
  FILES=(*.ts lib/*.ts)
else
  FILES=("$@")
fi

echo "Checking TypeScript types for ${#FILES[@]} files..."

npx -p typescript tsc \
  --noEmit \
  --target esnext \
  --moduleResolution node \
  --lib esnext,dom \
  --skipLibCheck \
  --allowImportingTsExtensions \
  "${FILES[@]}"

echo "All type checks passed!"
