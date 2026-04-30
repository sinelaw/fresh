//! Structured theorem failures.
//!
//! Every assertion in every runner produces one of these variants on
//! mismatch. The runners' fallible (`check_*`) entry points return
//! `Result<(), TheoremFailure>`, so external drivers (fuzzers,
//! generators, proof-search loops) can call them in a tight loop
//! without `catch_unwind` or string-parsing panic messages.
//!
//! `Display` reproduces the legacy panic messages so the panicking
//! `assert_*` wrappers and `#[should_panic(expected = "…")]`
//! meta-tests work without changes.

use crate::common::theorem::buffer_theorem::CursorExpect;
use fresh::test_api::Caret;

#[derive(Debug, Clone)]
pub enum TheoremFailure {
    BufferTextMismatch {
        description: String,
        expected: String,
        actual: String,
    },
    PrimaryCursorMismatch {
        description: String,
        expected: CursorExpect,
        actual: Caret,
    },
    CursorCountMismatch {
        description: String,
        expected: usize,
        actual: usize,
    },
    SecondaryCursorMismatch {
        description: String,
        index: usize,
        expected: CursorExpect,
        actual: Caret,
    },
    SelectionTextMismatch {
        description: String,
        expected: String,
        actual: String,
    },
    ForwardTraceFailed {
        description: String,
        expected: String,
        actual: String,
    },
    ReverseTraceFailed {
        description: String,
        undo_count: usize,
        expected: String,
        actual: String,
    },
    ViewportTopByteMismatch {
        description: String,
        expected: usize,
        actual: usize,
    },
}

impl std::fmt::Display for TheoremFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TheoremFailure::BufferTextMismatch {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] buffer text mismatch\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::PrimaryCursorMismatch {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] primary cursor mismatch:\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::CursorCountMismatch {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] cursor count mismatch (got {actual} cursors, expected {expected})",
            ),
            TheoremFailure::SecondaryCursorMismatch {
                description,
                index,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] secondary cursor mismatch (index {index}):\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::SelectionTextMismatch {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] selection text mismatch\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::ForwardTraceFailed {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] forward trace failed: buffer text after actions\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::ReverseTraceFailed {
                description,
                undo_count,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] reverse trace failed: {undo_count} undos should yield the initial buffer\n   expected = {expected:?}\n   actual   = {actual:?}",
            ),
            TheoremFailure::ViewportTopByteMismatch {
                description,
                expected,
                actual,
            } => write!(
                f,
                "[{description}] viewport_top_byte mismatch: expected {expected}, got {actual}",
            ),
        }
    }
}

impl std::error::Error for TheoremFailure {}
