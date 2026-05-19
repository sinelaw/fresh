//! Migration of `tests/e2e/popup_wrap_indent.rs` — popup line wrapping
//! must preserve a hanging indent on continuation lines.
//!
//! Load-bearing claim:
//!
//!   * When a long line in a `Centered` popup wraps because the popup
//!     `width` is narrower than the line, the continuation visual row
//!     must start with at least as many leading spaces as the source
//!     line. Without this, wrapped signature-help parameter
//!     descriptions visually merge into the next entry.
//!
//! ## Harness-direct pattern
//!
//! Popup injection goes through `Event::ShowPopup` via
//! `harness.apply_event(...)`. The popup data types
//! (`PopupData`, `PopupContentData`, `PopupKindHint`,
//! `PopupPositionData`) are model projections used by the e2e
//! original; there is no `Action::*` to dispatch a freeform popup.
//! Migrated as harness-direct so the model imports are permitted
//! under `scripts/check-semantic-test-isolation.sh`.
//!
//! Screen-text inspection uses
//! `RenderSnapshot::extract_with_rendered_rows` (vt100 round-trip),
//! matching the e2e's `screen_to_string` semantics.
//!
//! Source: `tests/e2e/popup_wrap_indent.rs` (1 test migrated; no
//! tests deferred).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::RenderSnapshot;
use fresh::model::event::{
    Event, PopupContentData, PopupData, PopupKindHint, PopupPositionData,
};

/// The signature-help-style fixture: a brief header, a separator,
/// and two indented parameter descriptions long enough that a
/// 40-col popup must wrap them.
fn popup_lines() -> Vec<String> {
    vec![
        "print(*values, sep, end, file, flush)".to_string(),
        "---".to_string(),
        "    sep  string inserted between values, default a space, used to join all output values together".to_string(),
        "    end  string appended after the last value, default a newline character sequence".to_string(),
    ]
}

fn show_popup(harness: &mut EditorTestHarness, width: u16) -> anyhow::Result<()> {
    harness.apply_event(Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Text,
            title: Some("Signature Help".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::Text(popup_lines()),
            position: PopupPositionData::Centered,
            width,
            max_height: 20,
            bordered: true,
        },
    })
}

#[test]
fn migrated_popup_wrapped_lines_have_hanging_indent() {
    // Narrow terminal so the 40-wide popup forces wrapping of the
    // long indented parameter descriptions.
    let mut harness = EditorTestHarness::new(60, 24).unwrap();
    show_popup(&mut harness, 40).unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    // Locate the original "sep" row and its wrapped continuation.
    let sep_line = snap
        .rendered_rows
        .iter()
        .find(|line| line.contains("sep  string inserted"))
        .unwrap_or_else(|| {
            panic!(
                "Should find the 'sep' parameter line. Rows:\n{:#?}",
                snap.rendered_rows
            )
        });

    let continuation = snap
        .rendered_rows
        .iter()
        .find(|line| {
            (line.contains("used to join") || line.contains("output values"))
                && !line.contains("sep  string")
        })
        .unwrap_or_else(|| {
            panic!(
                "Should find a wrapped continuation line for 'sep'. \
                 Rows:\n{:#?}",
                snap.rendered_rows
            )
        });

    // The continuation should sit inside the popup border characters.
    // Splitting on '│' isolates the cells inside the popup.
    let content_after_border = continuation
        .split('│')
        .nth(1)
        .unwrap_or_else(|| {
            panic!(
                "Continuation should be inside popup border. \
                 Continuation: {continuation:?}\nsep_line: {sep_line:?}"
            )
        });

    let leading_spaces = content_after_border
        .chars()
        .take_while(|ch| *ch == ' ')
        .count();

    // The original line has 4 spaces of indent. The continuation
    // must also have at least 4 spaces — that's the hanging-indent
    // property the renderer guarantees.
    assert!(
        leading_spaces >= 4,
        "Wrapped continuation line should have hanging indent (>= 4 \
         spaces), but only has {leading_spaces} leading spaces.\n\
         Continuation line: {continuation:?}\nsep_line: {sep_line:?}",
    );
}

/// Anti-test: drop the `apply_event(Event::ShowPopup ...)` dispatch.
/// Without the popup, no row may contain the "sep  string inserted"
/// fixture content — proves the visibility checked in the positive
/// test depends on actually showing the popup, not on incidental
/// editor state.
#[test]
fn anti_popup_wrap_indent_without_show_popup_renders_no_popup_content() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();
    // No Event::ShowPopup dispatch here — that's the load-bearing
    // step we're dropping.

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let has_sep_text = snap
        .rendered_rows
        .iter()
        .any(|line| line.contains("sep  string inserted"));
    assert!(
        !has_sep_text,
        "anti: without Event::ShowPopup the popup's 'sep  string \
         inserted' fixture text must NOT appear in any rendered row. \
         Rows:\n{:#?}",
        snap.rendered_rows
    );
}
