#!/usr/bin/env bash
# analyze-vscode-extensions.sh
#
# Clones the top 10 open-source VS Code extensions and analyzes their
# usage of the VS Code extension API (the `vscode` module).
#
# Output:
#   - Per-extension API usage reports in analysis/<ext>/api_usage.txt
#   - A combined summary in analysis/summary.txt
#   - A minimal API surface recommendation in analysis/minimal_api_surface.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CLONE_DIR="$SCRIPT_DIR/extensions"
ANALYSIS_DIR="$SCRIPT_DIR/analysis"

mkdir -p "$CLONE_DIR" "$ANALYSIS_DIR"

# ── Top 10 open-source VS Code extensions (by install count) ────────────
declare -A EXTENSIONS=(
  ["eslint"]="https://github.com/microsoft/vscode-eslint.git"
  ["prettier"]="https://github.com/prettier/prettier-vscode.git"
  ["python"]="https://github.com/microsoft/vscode-python.git"
  ["gitlens"]="https://github.com/gitkraken/vscode-gitlens.git"
  ["live-server"]="https://github.com/ritwickdey/vscode-live-server.git"
  ["docker"]="https://github.com/microsoft/vscode-docker.git"
  ["material-icon-theme"]="https://github.com/PKief/vscode-material-icon-theme.git"
  ["path-intellisense"]="https://github.com/ChristianKohler/PathIntellisense.git"
  ["auto-rename-tag"]="https://github.com/formulahendry/vscode-auto-rename-tag.git"
  ["rest-client"]="https://github.com/Huachao/vscode-restclient.git"
  ["cpptools"]="https://github.com/microsoft/vscode-cpptools.git"
  ["code-runner"]="https://github.com/formulahendry/vscode-code-runner.git"
)

# ── 1. Clone repositories (shallow, skip if already present) ────────────
echo "=== Cloning repositories ==="
for name in "${!EXTENSIONS[@]}"; do
  dest="$CLONE_DIR/$name"
  if [ -d "$dest" ]; then
    echo "  [skip] $name (already cloned)"
  else
    echo "  [clone] $name ..."
    git clone --depth 1 --quiet "${EXTENSIONS[$name]}" "$dest" 2>/dev/null || {
      echo "  [WARN] Failed to clone $name, skipping"
      continue
    }
  fi
done

# ── 2. Analyze each extension ──────────────────────────────────────────
echo ""
echo "=== Analyzing VS Code API usage ==="

analyze_extension() {
  local name="$1"
  local src="$CLONE_DIR/$name"
  local out="$ANALYSIS_DIR/$name"
  mkdir -p "$out"

  if [ ! -d "$src" ]; then
    echo "  [skip] $name (not cloned)"
    return
  fi

  echo "  [analyze] $name"

  # Find all TypeScript/JavaScript source files (exclude node_modules, .git, test fixtures)
  local src_files
  src_files=$(find "$src" -type f \( -name '*.ts' -o -name '*.js' -o -name '*.tsx' -o -name '*.jsx' \) \
    ! -path '*/node_modules/*' \
    ! -path '*/.git/*' \
    ! -path '*/out/*' \
    ! -path '*/dist/*' \
    ! -path '*/__mocks__/*' \
    2>/dev/null || true)

  if [ -z "$src_files" ]; then
    echo "    No source files found"
    echo "No source files found" > "$out/api_usage.txt"
    return
  fi

  local file_count
  file_count=$(echo "$src_files" | wc -l)
  echo "    Found $file_count source files"

  # ── Extract vscode API references ────────────────────────────────────
  # Pattern: vscode.XXX or vscode.XXX.YYY
  # This catches: vscode.commands.registerCommand, vscode.window.showInformationMessage, etc.
  local api_refs
  api_refs=$(echo "$src_files" | xargs grep -ohE '\bvscode\.[A-Za-z]+(\.[A-Za-z]+)*' 2>/dev/null | sort | uniq -c | sort -rn || true)

  # ── Extract destructured imports from 'vscode' ──────────────────────
  # Pattern: import { X, Y, Z } from 'vscode'
  local destructured_imports
  destructured_imports=$(echo "$src_files" | xargs grep -ohP '(?<=import\s{)\s*[^}]+(?=}\s*from\s+['\''"]vscode['\''"])' 2>/dev/null | \
    tr ',' '\n' | sed 's/^\s*//;s/\s*$//' | grep -v '^$' | sort | uniq -c | sort -rn || true)

  # ── Extract type references (used as type annotations) ──────────────
  local type_refs
  type_refs=$(echo "$src_files" | xargs grep -ohE '\bvscode\.[A-Z][A-Za-z]*\b' 2>/dev/null | sort | uniq -c | sort -rn || true)

  # ── Categorize API usage into namespaces ────────────────────────────
  local namespaces
  namespaces=$(echo "$src_files" | xargs grep -ohE '\bvscode\.[a-z][A-Za-z]*\b' 2>/dev/null | \
    sed 's/^vscode\.//' | sort | uniq -c | sort -rn || true)

  # ── Extract specific API method calls ───────────────────────────────
  local method_calls
  method_calls=$(echo "$src_files" | xargs grep -ohE '\bvscode\.[a-z][A-Za-z]*\.[a-zA-Z]+' 2>/dev/null | sort | uniq -c | sort -rn || true)

  # ── Extract event subscriptions (onDid*, onWill*) ───────────────────
  local events
  events=$(echo "$src_files" | xargs grep -ohE '\bvscode\.[a-z][A-Za-z]*\.on(Did|Will)[A-Za-z]+' 2>/dev/null | sort | uniq -c | sort -rn || true)

  # ── Check for activation events in package.json ─────────────────────
  local activation_events=""
  if [ -f "$src/package.json" ]; then
    activation_events=$(python3 -c "
import json, sys
try:
    with open('$src/package.json') as f:
        pkg = json.load(f)
    events = pkg.get('activationEvents', [])
    for e in events:
        print(e)
    # Also extract contributes keys
    contributes = pkg.get('contributes', {})
    if contributes:
        print('---contributes---')
        for key in sorted(contributes.keys()):
            items = contributes[key]
            count = len(items) if isinstance(items, list) else 1
            print(f'  {key}: {count} entries')
except Exception as e:
    print(f'Error: {e}', file=sys.stderr)
" 2>/dev/null || true)
  fi

  # ── Write per-extension report ──────────────────────────────────────
  {
    echo "═══════════════════════════════════════════════════════════════"
    echo "  VS Code API Usage Report: $name"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    echo "Source files analyzed: $file_count"
    echo ""

    echo "── Activation Events (from package.json) ──"
    if [ -n "$activation_events" ]; then
      echo "$activation_events"
    else
      echo "  (none or not found)"
    fi
    echo ""

    echo "── Top API Namespaces Used ──"
    if [ -n "$namespaces" ]; then
      echo "$namespaces" | head -20
    else
      echo "  (none found)"
    fi
    echo ""

    echo "── Top API Method Calls ──"
    if [ -n "$method_calls" ]; then
      echo "$method_calls" | head -40
    else
      echo "  (none found)"
    fi
    echo ""

    echo "── Type/Class References (vscode.XXX where XXX is capitalized) ──"
    if [ -n "$type_refs" ]; then
      echo "$type_refs" | head -30
    else
      echo "  (none found)"
    fi
    echo ""

    echo "── Destructured Imports from 'vscode' ──"
    if [ -n "$destructured_imports" ]; then
      echo "$destructured_imports" | head -30
    else
      echo "  (none found)"
    fi
    echo ""

    echo "── Event Subscriptions ──"
    if [ -n "$events" ]; then
      echo "$events"
    else
      echo "  (none found)"
    fi
    echo ""

    echo "── All API References (full list) ──"
    if [ -n "$api_refs" ]; then
      echo "$api_refs"
    else
      echo "  (none found)"
    fi
  } > "$out/api_usage.txt"
}

for name in "${!EXTENSIONS[@]}"; do
  analyze_extension "$name"
done

# ── 3. Build combined summary ─────────────────────────────────────────
echo ""
echo "=== Building combined summary ==="

{
  echo "╔═══════════════════════════════════════════════════════════════╗"
  echo "║        VS Code Extension API - Combined Usage Summary       ║"
  echo "╚═══════════════════════════════════════════════════════════════╝"
  echo ""
  echo "Extensions analyzed: ${#EXTENSIONS[@]}"
  echo "Date: $(date -u '+%Y-%m-%d %H:%M UTC')"
  echo ""

  # Aggregate all method calls across extensions
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  CROSS-EXTENSION API METHOD USAGE (by # of extensions using it)"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""

  # For each extension, get unique method calls, then count how many extensions use each
  tmp_methods=$(mktemp)
  for name in "${!EXTENSIONS[@]}"; do
    src="$CLONE_DIR/$name"
    [ -d "$src" ] || continue
    find "$src" -type f \( -name '*.ts' -o -name '*.js' \) \
      ! -path '*/node_modules/*' ! -path '*/.git/*' ! -path '*/out/*' ! -path '*/dist/*' \
      -exec grep -ohE '\bvscode\.[a-z][A-Za-z]*\.[a-zA-Z]+' {} + 2>/dev/null | \
      sort -u | while read -r method; do
        echo "$method"
      done
  done | sort | uniq -c | sort -rn > "$tmp_methods"

  echo "Methods used by 3+ extensions (most universal API surface):"
  awk '$1 >= 3' "$tmp_methods"
  echo ""
  echo "Methods used by 2 extensions:"
  awk '$1 == 2' "$tmp_methods"
  echo ""
  echo "Methods used by 1 extension only:"
  awk '$1 == 1' "$tmp_methods" | head -30
  echo "  ... (see individual reports for full lists)"
  rm -f "$tmp_methods"

  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  CROSS-EXTENSION TYPE/CLASS USAGE"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""

  tmp_types=$(mktemp)
  for name in "${!EXTENSIONS[@]}"; do
    src="$CLONE_DIR/$name"
    [ -d "$src" ] || continue
    find "$src" -type f \( -name '*.ts' -o -name '*.js' \) \
      ! -path '*/node_modules/*' ! -path '*/.git/*' ! -path '*/out/*' ! -path '*/dist/*' \
      -exec grep -ohE '\bvscode\.[A-Z][A-Za-z]*\b' {} + 2>/dev/null | \
      sort -u | while read -r typ; do
        echo "$typ"
      done
  done | sort | uniq -c | sort -rn > "$tmp_types"

  echo "Types used by 3+ extensions:"
  awk '$1 >= 3' "$tmp_types"
  echo ""
  echo "Types used by 2 extensions:"
  awk '$1 == 2' "$tmp_types"
  echo ""
  echo "Types used by 1 extension only:"
  awk '$1 == 1' "$tmp_types" | head -20
  rm -f "$tmp_types"

  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  NAMESPACE USAGE ACROSS EXTENSIONS"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""

  tmp_ns=$(mktemp)
  for name in "${!EXTENSIONS[@]}"; do
    src="$CLONE_DIR/$name"
    [ -d "$src" ] || continue
    find "$src" -type f \( -name '*.ts' -o -name '*.js' \) \
      ! -path '*/node_modules/*' ! -path '*/.git/*' ! -path '*/out/*' ! -path '*/dist/*' \
      -exec grep -ohE '\bvscode\.[a-z][A-Za-z]*\b' {} + 2>/dev/null | \
      sed 's/^vscode\.//' | sort -u | while read -r ns; do
        echo "$ns"
      done
  done | sort | uniq -c | sort -rn > "$tmp_ns"

  cat "$tmp_ns"
  rm -f "$tmp_ns"

  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  CONTRIBUTES (package.json) ACROSS EXTENSIONS"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""

  for name in "${!EXTENSIONS[@]}"; do
    src="$CLONE_DIR/$name"
    if [ -f "$src/package.json" ]; then
      contributes=$(python3 -c "
import json
with open('$src/package.json') as f:
    pkg = json.load(f)
contributes = pkg.get('contributes', {})
for key in sorted(contributes.keys()):
    print(key)
" 2>/dev/null || true)
      if [ -n "$contributes" ]; then
        echo "  $name: $(echo $contributes | tr '\n' ', ')"
      fi
    fi
  done

} > "$ANALYSIS_DIR/summary.txt"

# ── 4. Generate minimal API surface recommendation ────────────────────
echo "=== Generating minimal API surface recommendation ==="

python3 "$SCRIPT_DIR/generate_minimal_api.py" "$ANALYSIS_DIR" "$CLONE_DIR"

echo ""
echo "=== Done! ==="
echo "Reports are in: $ANALYSIS_DIR/"
echo "  - Per-extension reports: $ANALYSIS_DIR/<ext>/api_usage.txt"
echo "  - Combined summary: $ANALYSIS_DIR/summary.txt"
echo "  - Minimal API surface: $ANALYSIS_DIR/minimal_api_surface.txt"
