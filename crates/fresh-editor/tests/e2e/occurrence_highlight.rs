use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Issue #3011: while text is selected the word-under-cursor highlight has to
/// step aside for the selection, and the selected text's other occurrences
/// have to be highlighted instead (as in VSCode/Zed). Before the fix the
/// occurrence highlight stayed on the unselected remainder of the word under
/// the cursor, so the extent of the selection could not be read off the
/// screen, and no other instance of the selected text was marked at all.
#[test]
fn test_selection_replaces_word_highlight_with_selection_matches() {
    // Two lines, both containing "beta"; "bet" also occurs inside every one
    // of them, so a selection of "bet" has matches to highlight.
    //   line 1: alpha beta gamma alpha
    //   line 2: beta alpha delta beta
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .load_buffer_from_text("alpha beta gamma alpha\nbeta alpha delta beta\n")
        .unwrap();
    harness.render().unwrap();

    let (first_row, _) = harness.content_area_rows();
    let line1 = first_row as u16;
    let line2 = line1 + 1;

    // Screen columns: the gutter occupies the first 6 columns, so text column
    // N of a line is screen column 6 + N.
    let col = |text_col: u16| 6 + text_col;
    // line 1: "beta" at text columns 6..10, "gamma" at 11..16
    let (l1_b, l1_e, l1_t, l1_a) = (col(6), col(7), col(8), col(9));
    let l1_neutral = col(11); // 'g' of "gamma" — never highlighted
                              // line 2: "beta" at text columns 0..4, "delta" at 11..16
    let (l2_b, l2_e, l2_t, l2_a) = (col(0), col(1), col(2), col(3));
    let l2_neutral = col(11); // 'd' of "delta"

    let bg = |h: &EditorTestHarness, x: u16, y: u16| h.get_cell_style(x, y).and_then(|s| s.bg);

    // Put the cursor on the "b" of "beta" on line 1 and let the occurrence
    // highlight settle: both "beta"s get a background of their own.
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .wait_until(|h| {
            bg(h, l1_e, line1) != bg(h, l1_neutral, line1)
                && bg(h, l2_e, line2) != bg(h, l2_neutral, line2)
        })
        .expect("the word under the cursor should be highlighted");

    // Precondition for the assertions below: the whole word is highlighted,
    // including the "a" that the selection will not cover.
    assert_ne!(
        bg(&harness, l1_a, line1),
        bg(&harness, l1_neutral, line1),
        "the trailing 'a' of \"beta\" starts out highlighted"
    );

    // Select "bet" out of that "beta".
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }

    // The word highlight must go: the unselected "a" of the selected word
    // falls back to the same background as the rest of the line.
    harness
        .wait_until(|h| bg(h, l1_a, line1) == bg(h, l1_neutral, line1))
        .expect("a selection must suppress the word-under-cursor highlight");

    let selection_bg = bg(&harness, l1_b, line1);
    assert_eq!(
        (bg(&harness, l1_e, line1), bg(&harness, l1_t, line1)),
        (selection_bg, selection_bg),
        "the three selected cells share the selection background"
    );
    assert_ne!(
        selection_bg,
        bg(&harness, l1_neutral, line1),
        "the selection has to be visible against the rest of its line"
    );

    // ... and the other occurrences of the *selected* text are highlighted.
    harness
        .wait_until(|h| bg(h, l2_b, line2) != bg(h, l2_neutral, line2))
        .expect("matches of the selected text should be highlighted");

    let match_bg = bg(&harness, l2_b, line2);
    assert_eq!(
        (bg(&harness, l2_e, line2), bg(&harness, l2_t, line2)),
        (match_bg, match_bg),
        "the whole \"bet\" match is highlighted"
    );
    assert_eq!(
        bg(&harness, l2_a, line2),
        bg(&harness, l2_neutral, line2),
        "the match stops at \"bet\" — the following 'a' is not part of it"
    );
    assert_ne!(
        match_bg, selection_bg,
        "the match highlight and the selection must not share a background"
    );

    // Dropping the selection brings the word highlight back.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            bg(h, l1_a, line1) != bg(h, l1_neutral, line1)
                && bg(h, l1_a, line1) == bg(h, l1_b, line1)
        })
        .expect("clearing the selection restores the word highlight");
}

#[test]
fn test_occurrence_highlight_toggle() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .type_text("apple banana apple cherry apple")
        .unwrap();
    harness.render().unwrap();

    let (content_first, _) = harness.content_area_rows();

    for _ in 0..14 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let first_apple_col = 6;
    let third_apple_col = 32;
    let default_bg = harness.get_cell_style(1, content_first as u16).unwrap().bg;

    // Use wait_until for initial highlight
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                != default_bg
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    != default_bg
        })
        .expect("Background should become highlighted initially");

    let initial_bg_first = harness
        .get_cell_style(first_apple_col, content_first as u16)
        .unwrap()
        .bg;
    let initial_bg_third = harness
        .get_cell_style(third_apple_col, content_first as u16)
        .unwrap()
        .bg;

    // Toggle off
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Occurrence Highlight").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait until it returns to default
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                == default_bg
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    == default_bg
        })
        .expect("Background should be restored to default when toggled off");

    // Toggle back on
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Occurrence Highlight").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait until it returns to highlighted
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                == initial_bg_first
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    == initial_bg_third
        })
        .expect("Background should be highlighted again");
}
