//! Track B migration of `tests/e2e/toggle_comment.rs`: skipped, with
//! a documented reason.
//!
//! The original tests assert language-specific comment prefixes (`//`
//! for `.rs`, `#` for `.py`, `#` for `.sh`). Language detection is
//! driven by file extension, but the semantic test framework's
//! `load_buffer_from_text` always uses `test_buffer.txt`. Migrating
//! these tests would require extending the test API with a
//! `load_buffer_from_text_named(filename, content)` entry point and
//! enabling the full grammar registry per-test.
//!
//! That's a Track C concern (observable extension) and the right
//! shape there is its own design discussion — toggle_comment is the
//! first migration to demand a specific filename, but won't be the
//! last (e.g. `bash_profile_editing`, `csharp_language_coherence`,
//! `glob_language_detection`, ~30 of the ~80 Class A tests). When
//! that pattern accumulates, the API should grow once with a single
//! good shape, not piecemeal.
//!
//! For now, `tests/e2e/toggle_comment.rs` continues to drive the
//! palette + language detection through the full E2E harness.
