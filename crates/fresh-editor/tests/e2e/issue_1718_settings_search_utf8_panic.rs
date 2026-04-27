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
//! offsets 96–98.  When the settings modal's search-result item area is 103
//! columns wide, the code computes `area.width - 5 = 98`, which is the third
//! byte of `→` — not a char boundary — and `&desc[..98]` panics.
//!
//! ## Layout arithmetic for the chosen terminal size (146 × 40)
//!
//! modal_width  = (146 × 90 / 100).min(160) = 131
//! inner_width  = 131 − 2  = 129   (border)
//! content_width = 129              (content_area == inner_area)
//! settings_area.width = 129 − 24  = 105   (24 cols for the category list)
//! settings_inner.width = 105 − 2  = 103   (horizontal_padding = 2)
//! item_area.width = 103 (no scrollbar) / 102 (with scrollbar)
//!
//! Both values (102 and 103) land inside `→` (bytes 96 and 97 respectively),
//! so the test panics regardless of whether search results need a scrollbar.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Searching "hidden" in the Settings dialog must not panic.
///
/// Before the fix this panics with:
///   byte index 98 is not a char boundary; it is inside '→' (bytes 96..99) …
///
/// The terminal is sized so that the settings search-result rendering area is
/// 103 columns wide, placing the truncation cut-point inside the `→` character
/// of the `whitespace_show` setting description.
#[test]
fn settings_search_hidden_does_not_panic() {
    // Width 146 → modal 131 → item area 103 cols → truncation at byte 98 (inside →).
    let mut harness = EditorTestHarness::new(146, 40).unwrap();

    harness.open_settings().unwrap();

    // Enter search mode.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type the query from the issue report.  The `whitespace_show` setting
    // description fuzzy-matches "hidden" and is rendered in the results; the
    // render code then byte-slices the description at an unsafe offset.
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();

    // render() is where the panic occurs.
    harness.render().unwrap();
}
