//! Reproducer for issue #2876: prompt-line inputs (search prompt, open-file
//! prompt, palette input) had no horizontal scrolling. With text longer than
//! the line, the paragraph was clipped at the right edge and
//! `set_cursor_position` was skipped entirely once the cursor's logical
//! column passed the width — the cursor vanished and the tail of the input
//! was unviewable, even though the internal cursor kept moving.
//!
//! With the fix, the input scrolls horizontally so the cursor is always
//! visible (pinned to the last column while past the right edge), and the
//! terminal cursor is always placed.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_search_prompt_long_input_scrolls_and_keeps_cursor_visible() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the search prompt and type 100 characters — far wider than the
    // 80-column line. The last 10 form a unique marker so the visible
    // window is observable on screen.
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let input = format!("{}TAILMARKER", "x".repeat(90));
    harness.type_text(&input).unwrap();

    // The tail of the input must be scrolled into view (the old renderer
    // showed only the first ~72 chars and never the tail) …
    let cursor = harness.render_observing_cursor().unwrap();
    harness.assert_screen_contains("TAILMARKER");
    // … and the hardware cursor must be visible, riding the last column of
    // the prompt line (the old renderer skipped `set_cursor_position`, so
    // the frame ended with the cursor hidden).
    assert_eq!(
        cursor,
        Some((79, 23)),
        "cursor must be visible at the right edge of the prompt line"
    );

    // Press Left 15 times: the cursor is still inside the overflowing
    // region, so it must stay visible while the window scrolls back with
    // it (the old renderer showed no cursor until ~28 presses).
    for _ in 0..15 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    let cursor = harness.render_observing_cursor().unwrap();
    assert_eq!(
        cursor,
        Some((79, 23)),
        "cursor must stay visible while moving left through clipped text"
    );
    // The window now ends just after the cursor (char 85 of 100), so the
    // tail marker (chars 90..100) must have scrolled off the right edge —
    // proof the window follows the cursor rather than staying pinned.
    harness.assert_screen_not_contains("TAILMARKER");
}
