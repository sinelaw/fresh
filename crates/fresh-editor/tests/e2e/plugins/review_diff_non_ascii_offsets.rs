//! Regression: the review stream's inline highlights are placed by **UTF-8
//! byte offset**, computed in the plugin, and every one of them is derived by
//! summing the byte lengths of the pieces to its left.
//!
//! A word-level highlight is the strictest case: its start is the byte length
//! of the row's line-number prefix plus the diff marker plus every unchanged
//! word before it. Get any of those lengths wrong for non-ASCII text — count
//! UTF-16 units, or take an ASCII shortcut that doesn't hold — and the
//! highlight slides left by one column per multi-byte character above and to
//! the left of it, landing on the wrong word.
//!
//! So the file under review puts accented text *before* the changed word on
//! the same line, and the test asserts the highlight covers that word and
//! nothing to its left.
//!
//! The second test covers the other way byte offsets go wrong: the word
//! diff walks the two lines in UTF-16 units, so a pair of lines differing
//! inside an astral character — two emoji sharing a high surrogate — can
//! split that character down the middle. Each half then reads as a
//! four-byte character of its own, and the highlight lands a whole
//! character past where it belongs.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use std::fs;

/// The word that changes on the accented line. Distinctive enough to locate
/// on screen, all-ASCII so the columns it occupies are its character count.
const CHANGED_WORD: &str = "REPLACEMENT";
/// What it replaced.
const ORIGINAL_WORD: &str = "PLACEHOLDER";
/// The accented run that sits to the left of the changed word. Six multi-byte
/// characters, so a byte/char confusion shifts the highlight six columns.
const ACCENTED: &str = "añadido ñ·ñ·ñ";

/// Text of screen row `y`.
fn row_text(harness: &EditorTestHarness, y: u16) -> String {
    let buf = harness.buffer();
    (0..buf.area.width)
        .map(|x| buf[(x, y)].symbol())
        .collect::<String>()
}

/// Row index of the first screen row containing `needle`.
fn row_containing(harness: &EditorTestHarness, needle: &str) -> Option<u16> {
    let height = harness.buffer().area.height;
    (0..height).find(|&y| row_text(harness, y).contains(needle))
}

/// The columns of row `y` whose styling marks them as word-diff emphasis.
/// The stream paints a changed word bold on top of the row's add/remove
/// background, and paints nothing else on that row bold, so "bold" is the
/// discriminator that needs no theme colour hard-coded.
fn emphasised_columns(harness: &EditorTestHarness, y: u16) -> Vec<u16> {
    let buf = harness.buffer();
    (0..buf.area.width)
        .filter(|&x| {
            buf[(x, y)]
                .style()
                .add_modifier
                .contains(ratatui::style::Modifier::BOLD)
        })
        .collect()
}

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// A repo whose single unstaged change rewrites one word of a line that
/// carries accented text ahead of it.
fn repo_with_accented_modification() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    let body = |word: &str| {
        format!(
            "fn head() {{}}\n\
             fn ctx_one() {{}}\n\
             fn ctx_two() {{}}\n\
             // {ACCENTED} {word} tail\n\
             fn ctx_three() {{}}\n\
             fn tail() {{}}\n"
        )
    };
    repo.create_file("src/lib.rs", &body(ORIGINAL_WORD));
    repo.git_add_all();
    repo.git_commit("baseline");
    repo.create_file("src/lib.rs", &body(CHANGED_WORD));
    repo
}

/// The word-level highlight on an added line must cover the word that
/// actually changed, even when multi-byte characters precede it.
// TODO: git command output differs on Windows; the other review tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn review_word_highlight_lands_on_the_changed_word_after_non_ascii() {
    let repo = repo_with_accented_modification();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        140,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk") && !s.contains("Generating Review")
        })
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(CHANGED_WORD))
        .unwrap();
    // Overlays can land a frame after the text they decorate.
    harness.wait_for_async_quiescence(3).unwrap();

    let screen = harness.screen_to_string();
    let added_y = row_containing(&harness, CHANGED_WORD)
        .unwrap_or_else(|| panic!("`{CHANGED_WORD}` never rendered.\nScreen:\n{screen}"));
    let text = row_text(&harness, added_y);
    let word_start = text
        .find(CHANGED_WORD)
        .unwrap_or_else(|| panic!("row {added_y} lost `{CHANGED_WORD}`: {text:?}"));
    // `find` gives a byte offset into the row's text; the highlight is checked
    // in screen columns, and every character on this row is one column wide.
    let word_col = text[..word_start].chars().count() as u16;
    let word_cols: Vec<u16> = (0..CHANGED_WORD.chars().count() as u16)
        .map(|i| word_col + i)
        .collect();

    let bold = emphasised_columns(&harness, added_y);
    assert!(
        !bold.is_empty(),
        "the added row ({added_y}) carries no word-diff emphasis at all, so \
         this test cannot tell a misplaced highlight from a missing one\n\
         row: {text:?}\nScreen:\n{screen}",
    );
    assert_eq!(
        bold, word_cols,
        "the word-diff highlight on row {added_y} should cover exactly \
         `{CHANGED_WORD}` at columns {word_cols:?}; a highlight shifted left \
         means the accented text before it was measured in characters rather \
         than UTF-8 bytes\nrow: {text:?}\nScreen:\n{screen}",
    );

    // The accented run is unchanged text and must stay unpainted, which is
    // the same defect seen from the other side.
    let accented_start = text
        .find(ACCENTED)
        .unwrap_or_else(|| panic!("row {added_y} lost `{ACCENTED}`: {text:?}"));
    let accented_col = text[..accented_start].chars().count() as u16;
    let accented_end = accented_col + ACCENTED.chars().count() as u16;
    let leaked: Vec<u16> = bold
        .iter()
        .copied()
        .filter(|c| (accented_col..accented_end).contains(c))
        .collect();
    assert!(
        leaked.is_empty(),
        "columns {leaked:?} of the unchanged accented run on row {added_y} \
         are painted as changed\nrow: {text:?}\nScreen:\n{screen}",
    );
}

/// A line whose emoji changes. `U+1F600` and `U+1F601` share a high
/// surrogate, so a prefix walk in UTF-16 units stops between the halves
/// of the pair rather than before it.
const EMOJI_BEFORE: &str = "\u{1F600}";
const EMOJI_AFTER: &str = "\u{1F601}";
/// Plain text after the emoji, so a highlight that runs long is visible
/// as emphasis on characters that did not change.
const EMOJI_TAIL: &str = " tail";

fn repo_with_emoji_modification() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    let body = |emoji: &str| {
        format!(
            "fn head() {{}}\n\
             fn ctx_one() {{}}\n\
             fn ctx_two() {{}}\n\
             const S: &str = \"{emoji}{EMOJI_TAIL}\";\n\
             fn ctx_three() {{}}\n\
             fn tail() {{}}\n"
        )
    };
    repo.create_file("src/lib.rs", &body(EMOJI_BEFORE));
    repo.git_add_all();
    repo.git_commit("baseline");
    repo.create_file("src/lib.rs", &body(EMOJI_AFTER));
    repo
}

/// The columns an emoji occupies on screen: its own cell, plus the blank
/// continuation cell a double-width character takes.
fn emoji_columns(text: &str, emoji: &str) -> Vec<u16> {
    let at = text
        .find(emoji)
        .unwrap_or_else(|| panic!("`{emoji}` not in `{text}`"));
    // Everything before the emoji on these rows is ASCII, so the prefix's
    // character count is its column count.
    let col = text[..at].chars().count() as u16;
    vec![col, col + 1]
}

/// A word-level highlight must not split an astral character. The two
/// lines here differ only in the low half of an emoji's surrogate pair,
/// which is the case a UTF-16 prefix walk gets wrong: it charges the
/// shared high surrogate to the unchanged run and the low half to the
/// change, four bytes each, and the highlight lands past the emoji — on
/// the quote and the semicolon, and off the end of the row entirely.
// TODO: git command output differs on Windows; the other review tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn review_word_highlight_does_not_split_a_surrogate_pair() {
    let repo = repo_with_emoji_modification();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        140,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk") && !s.contains("Generating Review")
        })
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(EMOJI_AFTER))
        .unwrap();
    // Overlays can land a frame after the text they decorate.
    harness.wait_for_async_quiescence(3).unwrap();

    let screen = harness.screen_to_string();
    let added_y = row_containing(&harness, EMOJI_AFTER)
        .unwrap_or_else(|| panic!("the added row never rendered.\nScreen:\n{screen}"));
    let removed_y = row_containing(&harness, EMOJI_BEFORE)
        .unwrap_or_else(|| panic!("the removed row never rendered.\nScreen:\n{screen}"));

    for (y, emoji, side) in [
        (added_y, EMOJI_AFTER, "added"),
        (removed_y, EMOJI_BEFORE, "removed"),
    ] {
        let text = row_text(&harness, y);
        let allowed = emoji_columns(&text, emoji);
        let bold = emphasised_columns(&harness, y);
        assert!(
            !bold.is_empty(),
            "the {side} row ({y}) carries no word-diff emphasis at all, so \
             this test cannot tell a misplaced highlight from a missing \
             one\nrow: {text:?}\nScreen:\n{screen}",
        );
        let stray: Vec<u16> = bold
            .iter()
            .copied()
            .filter(|c| !allowed.contains(c))
            .collect();
        assert!(
            stray.is_empty(),
            "the word-diff highlight on the {side} row ({y}) should cover \
             only the emoji at columns {allowed:?}, but columns {stray:?} \
             are painted too — a highlight that starts inside the \
             surrogate pair is four bytes long and runs past it\n\
             row: {text:?}\nScreen:\n{screen}",
        );
    }

    // A highlight measured four bytes long runs off the end of its row and
    // paints the row below, which is unchanged context here.
    let below = row_text(&harness, added_y + 1);
    assert!(
        below.contains("ctx_three"),
        "expected the context row under the change, got {below:?}\nScreen:\n{screen}",
    );
    assert!(
        emphasised_columns(&harness, added_y + 1).is_empty(),
        "the unchanged context row below the change is painted as \
         changed\nrow: {below:?}\nScreen:\n{screen}",
    );
}
