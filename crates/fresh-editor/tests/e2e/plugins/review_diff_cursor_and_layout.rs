//! Review Diff: what a keystroke costs, and what a layout flip redoes.
//!
//! Four complaints from reading a hundred-commit range by hand, all of
//! them about the same thing — work happening on the wrong side of the
//! frame that shows it:
//!
//!   1. flipping back to the unified stream showed the old scroll
//!      position, then jumped, because the panel swapped to the stream
//!      buffer at once and its (identical) content landed a beat later;
//!   2. flipping to side-by-side and back rebuilt both views every time;
//!   3. the cursor-line bar trailed a held arrow key by one row, because
//!      the plugin repainted it from `cursor_moved` — always answering
//!      the frame that had already drawn;
//!   4. moving the cursor at all went out to the plugin thread and back
//!      before anything moved.
//!
//! The first two are asserted on the panel-relayout counter (a rebuild is
//! not visible on screen — its *absence* is the fix), the last two on
//! rendered output after a single frame.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// A repo whose single file has a long, unbroken run of changed lines, so
/// the stream has plenty of rows to walk with the cursor and the
/// side-by-side view has a whole file to render.
fn repo_with_long_diff() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    let before: String = (0..60)
        .map(|i| format!("fn original_{i}() {{ let x = {i}; }}\n"))
        .collect();
    repo.create_file("src/main.rs", &before);
    repo.git_add_all();
    repo.git_commit("Initial commit");
    let after: String = (0..60)
        .map(|i| format!("fn changed_{i}() {{ let y = {i}; }}\n"))
        .collect();
    repo.create_file("src/main.rs", &after);
    repo
}

fn harness_for(repo: &GitTestRepo) -> EditorTestHarness {
    EditorTestHarness::with_config_and_working_dir(160, 44, Config::default(), repo.path.clone())
        .unwrap()
}

fn open_review_diff(harness: &mut EditorTestHarness) {
    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            if s.contains("TypeError") || s.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", s);
            }
            s.contains("next hunk") && !s.contains("Generating Review")
        })
        .unwrap();
    // The toolbar lands before the stream does; wait for real diff rows.
    // A rewritten file diffs as 60 removals and then 60 additions, so the
    // rows on screen are the `original_` side.
    harness
        .wait_until(|h| h.screen_to_string().contains("original_0()"))
        .unwrap();
}

/// Screen row carrying the cursor-line bar, found by its background: the
/// bar washes its whole row in the selection colour, and no other row in
/// the diff carries more than a cell or two of it.
fn cursor_bar_row(harness: &EditorTestHarness) -> Option<u16> {
    let selection_bg = harness.editor().theme().selection_bg;
    let area = harness.buffer().area;
    (0..area.height)
        .map(|y| {
            let washed = (0..area.width)
                .filter(|&x| {
                    harness
                        .get_cell_style(x, y)
                        .is_some_and(|style| style.bg == Some(selection_bg))
                })
                .count();
            (y, washed)
        })
        .max_by_key(|&(_, washed)| washed)
        .filter(|&(_, washed)| washed > 3)
        .map(|(y, _)| y)
}

/// Every cell's background, so a test can tell "the frame changed" from
/// "the text changed" — a cursor move repaints highlights without moving
/// a single character.
fn background_fingerprint(harness: &EditorTestHarness) -> Vec<Option<ratatui::style::Color>> {
    let area = harness.buffer().area;
    (0..area.height)
        .flat_map(|y| (0..area.width).map(move |x| (x, y)))
        .map(|(x, y)| harness.get_cell_style(x, y).and_then(|style| style.bg))
        .collect()
}

/// The diff row the bar is sitting on, as text.
fn cursor_bar_text(harness: &EditorTestHarness) -> String {
    let row = cursor_bar_row(harness).unwrap_or_else(|| {
        panic!(
            "no cursor-line bar on screen:\n{}",
            harness.screen_to_string()
        )
    });
    harness
        .screen_to_string()
        .lines()
        .nth(row as usize)
        .unwrap_or_default()
        .trim_end()
        .to_string()
}

/// `2` then `1`, waiting for each layout to be the one on screen.
fn flip_to_split_and_back(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Side-by-side view"))
        .unwrap();
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Unified view"))
        .unwrap();
}

/// Press a key and draw exactly one frame — no draining of plugin work in
/// between, which is what `send_key` does and what hides a round trip.
fn key_then_one_frame(harness: &mut EditorTestHarness, code: KeyCode) {
    harness
        .editor_mut()
        .handle_key(code, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Issues 3 and 4: one keypress, one frame — the caret moves and the bar
/// moves with it.
///
/// Both halves used to need a round trip to the plugin thread: the key was
/// bound to a plugin handler that asked for `move_down`, and the bar was
/// repainted from the `cursor_moved` hook that fired afterwards. Neither
/// could land in the frame the key arrived in, so a held arrow key drew a
/// bar one row behind a caret that was itself a frame behind the key.
#[test]
fn test_cursor_and_its_bar_move_in_the_frame_the_key_arrives_in() {
    init_tracing_from_env();
    let repo = repo_with_long_diff();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // Land inside the hunk body, where every row is a diff line.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let before = cursor_bar_text(&harness);
    assert!(
        !before.is_empty(),
        "expected the bar to sit on a diff row, got an empty one:\n{}",
        harness.screen_to_string()
    );

    key_then_one_frame(&mut harness, KeyCode::Down);
    let after = cursor_bar_text(&harness);
    assert_ne!(
        before,
        after,
        "one Down, one frame: the bar should already be on the next row.\n\
         Screen:\n{}",
        harness.screen_to_string()
    );

    // ...and it is the row *below*, not any row: the bar tracks the caret
    // rather than lagging it.
    let screen = harness.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let bar = cursor_bar_row(&harness).expect("bar on screen") as usize;
    assert_eq!(
        rows[bar - 1].trim_end(),
        before,
        "the bar moved somewhere other than one row down.\nScreen:\n{}",
        screen
    );
}

/// Issue 4, side-by-side half: the composite takes the same native
/// motions, so a keypress moves its cursor in the frame it arrives in.
#[test]
fn test_side_by_side_cursor_moves_in_the_frame_the_key_arrives_in() {
    init_tracing_from_env();
    let repo = repo_with_long_diff();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Side-by-side view"))
        .unwrap();

    let before = background_fingerprint(&harness);
    key_then_one_frame(&mut harness, KeyCode::Down);
    assert_ne!(
        before,
        background_fingerprint(&harness),
        "Down should move the composite's cursor — and its row highlight — \
         within the frame the key arrived in.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Issues 1 and 2: flipping between the layouts re-uses both views instead
/// of rebuilding them.
///
/// The stream costs about a second to lay out on a large review, and the
/// panel swaps to it before that lands — so a needless rebuild is not
/// invisible waste, it is the reader watching a stale scroll position and
/// then a jump. Nothing about the review changes when the layout flips, so
/// nothing has to be laid out again.
#[test]
fn test_layout_flips_do_not_relayout_the_stream() {
    init_tracing_from_env();
    let repo = repo_with_long_diff();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // One warm-up pair first. The panel reports its real width to the
    // plugin only after the stream has been laid out once, and the inline
    // comment boxes are wrapped to that width — so the first flip legitimately
    // carries one relayout the reader is owed. What must not repeat is the
    // one after it.
    flip_to_split_and_back(&mut harness);

    let before = harness.editor().perf_counters().panel_content_rows;
    flip_to_split_and_back(&mut harness);
    flip_to_split_and_back(&mut harness);
    let after = harness.editor().perf_counters().panel_content_rows;

    // The sticky header is a panel too and it *does* change on a flip (it
    // names the current file), so the budget is not zero — but it is a row
    // or two per flip against the stream's ~125, which is what a rebuild
    // would put through here twice over.
    assert!(
        after - before < 60,
        "two layout flips re-laid-out {} panel rows; the stream should have \
         been reused",
        after - before
    );

    // The stream is still the real thing afterwards, not a stale husk.
    assert!(
        harness.screen_to_string().contains("original_"),
        "the reused stream should still show the diff:\n{}",
        harness.screen_to_string()
    );
}

/// The reuse is not blind: a change to what the stream says still lays it
/// out again. Toggling inline notes (`a`) is a pure view change over the
/// same git data, so it exercises the invalidation without touching the
/// repo.
#[test]
fn test_a_content_change_still_relayouts_the_stream() {
    init_tracing_from_env();
    let repo = repo_with_long_diff();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    let before = harness.editor().perf_counters().panel_content_rows;
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string().to_lowercase();
            s.contains("notes shown") || s.contains("notes hidden")
        })
        .unwrap();
    assert!(
        harness.editor().perf_counters().panel_content_rows - before > 60,
        "a view toggle changes what the stream says, so the whole stream \
         has to be laid out again"
    );
}

/// Issue 1, the visible half: the line the reader was on in side-by-side
/// is the line the stream comes back to.
#[test]
fn test_flip_back_to_unified_lands_on_the_line_the_reader_left() {
    init_tracing_from_env();
    let repo = repo_with_long_diff();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Side-by-side view"))
        .unwrap();
    // Walk well down the file, past what the stream had on screen.
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Unified view"))
        .unwrap();

    // The composite's cursor was ~30 lines into the file, which is past
    // the bottom of the stream's viewport: the stream should have followed
    // it there, leaving the file's first row behind.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("original_0()"),
        "the stream should come back around the line the reader left, not \
         the top of the file:\n{}",
        screen
    );
}
