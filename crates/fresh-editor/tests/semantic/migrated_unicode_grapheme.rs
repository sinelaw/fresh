//! Faithful migration of the buffer-state claims from
//! `tests/e2e/issue_1577_unicode_width.rs` — the parts that don't
//! require screen-cell inspection (those need StyleScenario,
//! which is still a skeleton).
//!
//! Issue #1577: pre-fix, `Buffer::next_grapheme_boundary` /
//! `prev_grapheme_boundary` only fetched a 32-byte window when
//! locating the next grapheme. Long ZWJ sequences and Zalgo
//! strings exceed that window, so Right/Left arrow walked
//! one codepoint at a time through a multi-byte cluster, and End
//! could land inside the cluster.
//!
//! These migrations exercise the production grapheme code path
//! directly through `Action::MoveRight` / `MoveLineEnd`. No mocks.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

/// U+FF37 FULLWIDTH LATIN CAPITAL LETTER W — 3 bytes UTF-8, screen
/// width 2. Encoded literally to keep the test focused on byte
/// counts rather than escape syntax.
const FULLWIDTH_W: &str = "Ｗ";

/// Family ZWJ sequence: 👨‍👩‍👧‍👦. 25 UTF-8 bytes (pinned by the
/// e2e's own assertion). Single grapheme cluster.
const ZWJ_FAMILY: &str = "👨\u{200D}👩\u{200D}👧\u{200D}👦";

/// Build a Zalgo base char ('a' + 20 combining marks). 41 bytes
/// in a single grapheme — well past the old 32-byte boundary
/// lookahead, which is exactly the regression target.
fn zalgo_char() -> String {
    let mut s = String::from("a");
    for cp in 0x0300u32..0x0314u32 {
        if let Some(c) = char::from_u32(cp) {
            s.push(c);
        }
    }
    s
}

#[test]
fn migrated_move_line_end_lands_past_fullwidth_w() {
    // Original: first half of `test_issue_1577_fullwidth_w_cursor_and_row`.
    // "pre Ｗ post" is 4 + 3 + 5 = 12 bytes; End must land at the
    // byte position immediately past the last grapheme, not at
    // some interior boundary.
    let line = format!("pre {FULLWIDTH_W} post");
    let line_bytes = line.len();
    assert_eq!(line_bytes, 12, "sanity: 'pre Ｗ post' is 12 UTF-8 bytes");

    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd on 'pre Ｗ post' lands at byte 12 (past last grapheme)".into(),
        initial_text: format!("{line}\n"),
        actions: vec![Action::MoveLineEnd],
        expected_text: format!("{line}\n"),
        expected_primary: CursorExpect::at(line_bytes),
        ..Default::default()
    });
}

#[test]
fn migrated_move_right_traverses_each_grapheme_once() {
    // Original: second half of `test_issue_1577_fullwidth_w_cursor_and_row`.
    // 10 graphemes on the line: "p", "r", "e", " ", "Ｗ", " ",
    // "p", "o", "s", "t". 10 MoveRight presses must reach the line
    // end (byte 12).
    let line = format!("pre {FULLWIDTH_W} post");
    assert_buffer_scenario(BufferScenario {
        description: "10 MoveRight presses traverse 10 graphemes (incl. fullwidth W) to byte 12"
            .into(),
        initial_text: format!("{line}\n"),
        actions: std::iter::repeat_n(Action::MoveRight, 10).collect(),
        expected_text: format!("{line}\n"),
        expected_primary: CursorExpect::at(line.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_move_right_skips_zwj_family_in_one_step() {
    // Original: claims from `test_issue_1577_zwj_family_single_grapheme`
    // and `test_issue_1577_cursor_screen_column_advances_by_grapheme_width`,
    // restricted to the byte-position observable.
    //
    // ZWJ family is 25 bytes / 1 grapheme. One MoveRight from byte
    // 0 must land at byte 25 (past the whole cluster), and a second
    // MoveRight onto the trailing 'a' lands at byte 26.
    let line = format!("{ZWJ_FAMILY}abc");
    assert_eq!(ZWJ_FAMILY.len(), 25, "sanity: ZWJ family is 25 UTF-8 bytes");
    assert_buffer_scenario(BufferScenario {
        description: "MoveRight crosses the whole ZWJ family cluster in one step".into(),
        initial_text: format!("{line}\n"),
        actions: vec![Action::MoveRight, Action::MoveRight],
        expected_text: format!("{line}\n"),
        // After 1st Right: byte 25 (just past family).
        // After 2nd Right: byte 26 ('a' is 1 byte).
        expected_primary: CursorExpect::at(26),
        ..Default::default()
    });
}

#[test]
fn migrated_move_right_skips_zalgo_cluster_past_window() {
    // Original: `test_issue_1577_zalgo_grapheme_navigation`. The
    // cluster is 41 bytes — past the old 32-byte lookahead. One
    // MoveRight from byte 0 must skip the whole cluster.
    let zalgo = zalgo_char();
    assert!(
        zalgo.len() > 32,
        "sanity: zalgo cluster must exceed the old 32-byte lookahead (got {} bytes)",
        zalgo.len()
    );
    let cluster_len = zalgo.len();
    let line = format!("{zalgo}Z");

    assert_buffer_scenario(BufferScenario {
        description: "MoveRight from byte 0 skips a 41-byte Zalgo grapheme in one step".into(),
        initial_text: format!("{line}\n"),
        actions: vec![Action::MoveRight],
        expected_text: format!("{line}\n"),
        expected_primary: CursorExpect::at(cluster_len),
        ..Default::default()
    });
}

#[test]
fn migrated_move_line_end_lands_past_zwj_family() {
    // Original: assertion from `test_issue_1577_zwj_family_single_grapheme`
    // — End on a line containing just the family emoji must land
    // at byte 25, not mid-sequence.
    assert_buffer_scenario(BufferScenario {
        description: "MoveLineEnd on a line that's just the ZWJ family lands at byte 25".into(),
        initial_text: format!("{ZWJ_FAMILY}\n"),
        actions: vec![Action::MoveLineEnd],
        expected_text: format!("{ZWJ_FAMILY}\n"),
        expected_primary: CursorExpect::at(ZWJ_FAMILY.len()),
        ..Default::default()
    });
}

/// Anti-test: drops the `MoveRight` from the Zalgo scenario.
/// Without it the cursor stays at byte 0 and the
/// `cluster_len`-byte expectation cannot match — this proves the
/// runner is genuinely sensitive to the grapheme step rather than
/// reading some unrelated buffer state.
#[test]
fn anti_zalgo_move_right_dropping_action_yields_check_err() {
    let zalgo = zalgo_char();
    let cluster_len = zalgo.len();
    let scenario = BufferScenario {
        description: "anti: MoveRight dropped — cursor must not skip the Zalgo cluster".into(),
        initial_text: format!("{zalgo}Z\n"),
        actions: vec![],
        expected_text: format!("{zalgo}Z\n"),
        expected_primary: CursorExpect::at(cluster_len),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with no MoveRight, the cursor stays at byte 0; \
         the cluster-skip expectation must NOT match"
    );
}
