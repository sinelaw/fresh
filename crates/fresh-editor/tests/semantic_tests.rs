//! Semantic theorem tests.
//!
//! These tests bind only to `fresh::test_api::EditorTestApi`. They do
//! not drive keys, do not render, and do not reach into editor
//! internals. The lint script `scripts/check-semantic-test-isolation.sh`
//! enforces this contract.
//!
//! See `docs/internal/e2e-test-migration-design.md` for the migration
//! design. New theorems live under `tests/semantic/<domain>.rs`.

mod common;
mod semantic;
