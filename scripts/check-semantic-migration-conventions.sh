#!/usr/bin/env bash
# Enforce migration conventions for semantic test files.
#
# Per docs/internal/scenario-migration-status.md and issue #2058,
# every file matching `tests/semantic/migrated_*.rs` must:
#
#   1. Name the exact `tests/e2e/<file>.rs` source(s) it migrates,
#      in the file's `//!` docstring. The convention is to mention
#      `tests/e2e/<name>.rs` so a grep can find the cite.
#
#   2. Include at least one anti-test — a `#[test]` function whose
#      name starts with `anti_` that exercises the inverse claim
#      (dropping the load-bearing action or precondition). The
#      anti-test must fail when the runner can detect a vacuous
#      assertion; it's the permanent guard against silently-inert
#      migrations.
#
# These rules don't apply to `tests/semantic/<domain>.rs` files
# that aren't migrations (e.g. `tests/semantic/properties.rs`,
# `tests/semantic/corpus.rs`), only to `migrated_*` files.
#
# Each violation is reported with the file path and the rule it
# breaks. Exit code is non-zero if any violations are found.

set -euo pipefail

SEMANTIC_DIR="crates/fresh-editor/tests/semantic"

if [[ ! -d "$SEMANTIC_DIR" ]]; then
    exit 0
fi

violations=0

for file in "$SEMANTIC_DIR"/migrated_*.rs; do
    [[ -e "$file" ]] || continue

    # Rule 1: docstring must cite an e2e source.
    if ! grep -qE '^//!.*tests/e2e/' "$file"; then
        echo "$file: missing e2e source cite in docstring"
        echo "  Add '//! ... tests/e2e/<name>.rs ...' so the source is grep-able."
        violations=$((violations + 1))
    fi

    # Rule 2: must include at least one anti-test.
    if ! grep -qE '^fn anti_|^\s*fn anti_' "$file"; then
        echo "$file: missing anti-test"
        echo "  Add a #[test] fn anti_<topic>_<scenario>() that drops the"
        echo "  load-bearing action and asserts the runner detects the"
        echo "  vacuous result. See docs/internal/scenario-migration-status.md"
        echo "  'Anti-test per file' for the convention."
        violations=$((violations + 1))
    fi
done

if [[ "$violations" -gt 0 ]]; then
    echo
    echo "FAIL: $violations migration-convention violation(s) in $SEMANTIC_DIR"
    echo "See docs/internal/scenario-migration-status.md and issue #2058"
    exit 1
fi

echo "OK: $SEMANTIC_DIR migration files cite an e2e source and ship an anti-test"
