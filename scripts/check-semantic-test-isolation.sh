#!/usr/bin/env bash
# Enforce the test_api contract for semantic theorem tests.
#
# Files under crates/fresh-editor/tests/semantic/ may import only from
# fresh::test_api and the common test harness. They must NOT reach into
# fresh::app, fresh::model, fresh::view, fresh::input, or fresh::services
# — those are production internals, and the whole point of the test_api
# module is to keep the test/production contract one-directional.
#
# See docs/internal/e2e-test-migration-design.md §2.1 for rationale.

set -euo pipefail

SEMANTIC_DIR="crates/fresh-editor/tests/semantic"

if [[ ! -d "$SEMANTIC_DIR" ]]; then
    # No semantic tests yet — nothing to lint. (Phase 2 will populate.)
    exit 0
fi

# Forbidden import prefixes. Rust's `use` syntax allows several spellings,
# so we match conservatively on the path stem.
forbidden=(
    'use fresh::app'
    'use fresh::model'
    'use fresh::view'
    'use fresh::input'
    'use fresh::services'
    'use fresh::config_io'
    'use fresh::state'
    'use fresh::workspace'
    'use crossterm::'
)

violations=0
for pattern in "${forbidden[@]}"; do
    # -F: literal string, -r: recursive, -n: line numbers
    # Exclude generated/compiled files just in case.
    matches=$(grep -rFn --include='*.rs' "$pattern" "$SEMANTIC_DIR" || true)
    if [[ -n "$matches" ]]; then
        echo "Forbidden import in $SEMANTIC_DIR:"
        echo "$matches" | sed 's/^/  /'
        echo "  -> '$pattern' is a production internal; semantic tests must"
        echo "     observe the editor only through fresh::test_api."
        echo
        violations=$((violations + 1))
    fi
done

if [[ "$violations" -gt 0 ]]; then
    echo "FAIL: $violations forbidden-import pattern(s) found in $SEMANTIC_DIR"
    echo "See docs/internal/e2e-test-migration-design.md §2.1"
    exit 1
fi

echo "OK: $SEMANTIC_DIR observes only fresh::test_api"
