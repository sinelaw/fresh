#!/usr/bin/env bash
# Enforce the test_api contract for semantic theorem tests.
#
# Files under crates/fresh-editor/tests/semantic/ may import only from
# fresh::test_api and the common test harness. They must NOT reach into
# fresh::app, fresh::input, fresh::services, fresh::config_io,
# fresh::state, fresh::workspace, or fresh::config — those are
# production internals, and the whole point of the test_api module is to
# keep the test/production contract one-directional.
#
# Two exceptions, both documented in
# docs/internal/scenario-migration-status.md as the "Direct-harness for
# cross-state claims" pattern:
#   1. `use crossterm::event::{KeyCode, KeyModifiers}` — needed for
#      send_key flows that route through the production key handler
#      (the path that exercises normalize_key, prompt routing, etc.).
#   2. `use fresh::model::*` / `use fresh::view::*` for projection types
#      that have no EditorTestApi counterpart (MarkerId, LineIndicator).
#
# These exceptions are only allowed in files that explicitly opt in by
# importing the EditorTestHarness:
#
#     use crate::common::harness::EditorTestHarness;
#
# Any file importing the harness directly is taking the harness-direct
# pattern and is exempted from the crossterm / fresh::model / fresh::view
# rules. Files using only test_api projections (the BufferScenario /
# LayoutScenario / ModalScenario runners) are NOT exempt — they should
# never need those imports.
#
# See docs/internal/e2e-test-migration-design.md §2.1 for rationale.

set -euo pipefail

SEMANTIC_DIR="crates/fresh-editor/tests/semantic"

if [[ ! -d "$SEMANTIC_DIR" ]]; then
    # No semantic tests yet — nothing to lint.
    exit 0
fi

# Always-forbidden imports — production internals that even the
# harness-direct pattern doesn't need.
always_forbidden=(
    'use fresh::app'
    'use fresh::input'
    'use fresh::services'
    'use fresh::config_io'
    'use fresh::state'
    'use fresh::workspace'
)

# Conditionally-forbidden imports — allowed only in harness-direct
# files (files that import EditorTestHarness).
harness_only=(
    'use crossterm::'
    'use fresh::model'
    'use fresh::view'
    'use fresh::config::'
)

violations=0

for pattern in "${always_forbidden[@]}"; do
    matches=$(grep -rFn --include='*.rs' "$pattern" "$SEMANTIC_DIR" || true)
    if [[ -n "$matches" ]]; then
        echo "Forbidden import in $SEMANTIC_DIR:"
        echo "$matches" | sed 's/^/  /'
        echo "  -> '$pattern' is a production internal; semantic tests"
        echo "     must observe the editor only through fresh::test_api."
        echo
        violations=$((violations + 1))
    fi
done

# For the harness-only patterns, allow imports in files that themselves
# import EditorTestHarness. List violators file-by-file.
for pattern in "${harness_only[@]}"; do
    # Find all matching files.
    matches=$(grep -rFl --include='*.rs' "$pattern" "$SEMANTIC_DIR" || true)
    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        if grep -qF 'use crate::common::harness::EditorTestHarness' "$file"; then
            continue
        fi
        # Find the line(s) for the violation in this file.
        lines=$(grep -nF "$pattern" "$file" || true)
        echo "Forbidden import in non-harness-direct file:"
        echo "$lines" | sed "s|^|  $file:|"
        echo "  -> '$pattern' is only allowed in harness-direct files"
        echo "     (files that import EditorTestHarness). This file"
        echo "     uses test_api projections only — drop the import or"
        echo "     adopt the harness-direct pattern (and import"
        echo "     EditorTestHarness from crate::common::harness)."
        echo
        violations=$((violations + 1))
    done <<< "$matches"
done

if [[ "$violations" -gt 0 ]]; then
    echo "FAIL: $violations forbidden-import pattern(s) found in $SEMANTIC_DIR"
    echo "See docs/internal/e2e-test-migration-design.md §2.1"
    exit 1
fi

echo "OK: $SEMANTIC_DIR observes only fresh::test_api (+ harness-direct opt-in)"
