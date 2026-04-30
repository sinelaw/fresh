//! Theorem framework for semantic (state-only) tests.
//!
//! See `docs/internal/e2e-test-migration-design.md` §4 for design.
//!
//! Tests express claims as data: `(initial state, action sequence,
//! expected final state)`. A runner instantiates a headless editor,
//! applies the actions through `fresh::test_api::EditorTestApi`, and
//! asserts on the resulting state — no `terminal.draw`, no
//! `crossterm::KeyCode`, no screen scraping.

pub mod buffer_theorem;
pub mod failure;
pub mod layout_theorem;
pub mod trace_theorem;
