//! Reproduction for issue #1718: Panic when searching "hidden" in Settings —
//! byte-based slicing splits a multi-byte UTF-8 character (`→`).
//!
//! ## Panic site
//!
//! `crates/fresh-editor/src/view/settings/render.rs:2689`:
//!
//! ```ignore
//! let truncated_desc = if desc.len() > area.width as usize - 2 {
//!     format!("  {}...", &desc[..area.width as usize - 5])  // <-- byte slice
//! ```
//!
//! ## Why this panics
//!
//! The `whitespace_show` setting has description:
//!   "Master toggle for whitespace indicator visibility.\n\
//!    When disabled, no whitespace indicators (·, →) are shown regardless\n\
//!    of the per-position settings below.\nDefault: true"
//!
//! `·` (U+00B7) is 2 bytes at offsets 92–93, and `→` (U+2192) is 3 bytes at
//! offsets 96–98.  Any terminal width that maps the item-area width to a value
//! where `area.width - 5` falls inside one of those multi-byte sequences panics.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Searching "hidden" in the Settings dialog must not panic at any terminal width.
///
/// Before the fix this panics (example) with:
///   byte index 97 is not a char boundary; it is inside '→' (bytes 96..99) …
///
/// The sweep covers widths 40–250 to exercise every possible truncation
/// cut-point through the description, including those that land inside `·`
/// (bytes 92–93) and `→` (bytes 96–98).
#[test]
fn settings_search_hidden_does_not_panic_across_widths() {
    for width in 40u16..=250 {
        let mut harness = EditorTestHarness::new(width, 40).unwrap();

        harness.open_settings().unwrap();

        // Enter search mode.
        harness
            .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // Type the query from the issue report.  The `whitespace_show` setting
        // description fuzzy-matches "hidden" and is rendered in the results; the
        // render code then byte-slices the description at an unsafe offset.
        for ch in "hidden".chars() {
            harness
                .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
                .unwrap();
        }

        // render() is where the panic occurs.
        harness
            .render()
            .unwrap_or_else(|e| panic!("width={width}: render failed: {e}"));
    }
}
