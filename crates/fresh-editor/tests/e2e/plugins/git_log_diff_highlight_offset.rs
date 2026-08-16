//! Regression: the Git Log detail panel's diff colouring drifted away from the
//! lines it belongs to as soon as the commit contained non-ASCII text.
//!
//! `git_log` used to paint the streamed `git show` output itself, walking the
//! diff line by line and adding one background overlay per run of `+` / `-` /
//! `@@` rows. Overlay positions are **UTF-8 byte offsets**, but the walk
//! advanced by `line.length` — the number of UTF-16 code units. Every
//! multi-byte character above a row therefore pulled that row's stripe a few
//! bytes earlier, so the green and red blocks bled onto the neighbouring
//! context lines: the symptom is a diff whose colours are offset by a few
//! characters from the text they describe.
//!
//! That pass is gone — the buffer is a `.diff` file the host highlights on its
//! own — so these tests now guard the rendered result rather than any one
//! layer producing it.
//!
//! The commit below puts an accented added line above a plain added line so a
//! single frame shows both sides of the drift: the plain `+` row must be
//! coloured, and the context row between them must carry none of that colour.
//!
//! The second test covers the other walk over the same diff text — the one
//! `Enter` uses to turn the cursor's byte offset into a file and line — which
//! counted UTF-16 units the same way and so opened the file at the wrong line.

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

/// A harness whose grammar registry can actually highlight the `.diff` buffer
/// the detail panel shows. Tests default to an empty registry for startup
/// speed, which leaves the panel's diff uncoloured — and this file's whole
/// subject is which cells that colouring lands on.
fn harness_with_highlighting(
    width: u16,
    height: u16,
    working_dir: std::path::PathBuf,
) -> EditorTestHarness {
    EditorTestHarness::create(
        width,
        height,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_working_dir(working_dir)
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// The context line that sits between the accented addition and the plain one.
const CTX_ROW: &str = "CTX KEEP LINE";
/// The plain (all-ASCII) added line below it.
const ADDED_ROW: &str = "PLAIN ADD ROW";
/// A context line below the whole hunk, used as the "no diff colour here"
/// reference — the drift only ever pulls stripes *up*, so this row stays clean
/// with and without the fix.
const CLEAN_ROW: &str = "gamma three";

/// Text of screen row `y`.
fn row_text(harness: &EditorTestHarness, y: u16) -> String {
    let buf = harness.buffer();
    let mut row = String::new();
    for x in 0..buf.area.width {
        row.push_str(buf[(x, y)].symbol());
    }
    row
}

/// Row index of the first screen row containing `needle`.
fn row_containing(harness: &EditorTestHarness, needle: &str) -> Option<u16> {
    let height = harness.buffer().area.height;
    (0..height).find(|&y| row_text(harness, y).contains(needle))
}

/// The distinct background colours painted across the *detail panel* part of
/// row `y` — everything right of the split's `│` divider, so the commit list on
/// the left can't colour the sample.
fn detail_backgrounds(harness: &EditorTestHarness, y: u16) -> Vec<Color> {
    let divider = row_text(harness, y).chars().position(|c| c == '│');
    let first_x = divider.map(|d| d as u16 + 1).unwrap_or(0);
    let buf = harness.buffer();
    let mut seen: Vec<Color> = Vec::new();
    for x in first_x..buf.area.width {
        let bg = buf[(x, y)].style().bg.unwrap_or(Color::Reset);
        if !seen.contains(&bg) {
            seen.push(bg);
        }
    }
    seen
}

// TODO: git command output differs on Windows; the other git_log tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_diff_colours_stay_on_their_lines_after_non_ascii() {
    let repo = GitTestRepo::new();

    repo.create_file(
        "notes.txt",
        &format!("alpha one\nbeta two\n{CTX_ROW}\n{CLEAN_ROW}\ndelta four\n"),
    );
    repo.git_add(&["notes.txt"]);
    repo.git_commit("Add notes");

    // The added accented line carries six multi-byte characters, so every
    // overlay below it used to land six bytes too early — far enough to spill
    // onto the tail of `CTX KEEP LINE`, but not so far that the run starts
    // above it (which would move the defect off that row entirely).
    repo.create_file(
        "notes.txt",
        &format!(
            "alpha one\nbeta two\nañadido ñ·ñ·ñ\n{CTX_ROW}\n{ADDED_ROW}\n{CLEAN_ROW}\ndelta four\n"
        ),
    );
    repo.git_add(&["notes.txt"]);
    repo.git_commit("Add accented and plain lines");

    repo.setup_git_log_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = harness_with_highlighting(120, 40, repo.path.clone());

    harness.open_file(&repo.path.join("notes.txt")).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Every diff row under test must be on screen before a cell is sampled.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane")
                && s.contains(CTX_ROW)
                && s.contains(ADDED_ROW)
                && s.contains(CLEAN_ROW)
        })
        .unwrap();
    // The text can land a frame before its colouring does: `git show` streams
    // into the buffer and the highlighter runs over what has arrived. Let the
    // pipeline go quiet before reading cells.
    harness.wait_for_async_quiescence(3).unwrap();

    let screen = harness.screen_to_string();
    let find = |needle: &str| {
        row_containing(&harness, needle)
            .unwrap_or_else(|| panic!("`{needle}` never rendered.\nScreen:\n{screen}"))
    };
    let added_y = find(ADDED_ROW);
    let ctx_y = find(CTX_ROW);
    let clean_y = find(CLEAN_ROW);

    // Whatever the theme paints an addition with is "every background on the
    // `+` row that a plain context row doesn't have" — no theme colour is
    // hard-coded here.
    let context_bgs = detail_backgrounds(&harness, clean_y);
    let added_bgs = detail_backgrounds(&harness, added_y);
    let addition_only: Vec<Color> = added_bgs
        .iter()
        .copied()
        .filter(|bg| !context_bgs.contains(bg))
        .collect();
    assert!(
        !addition_only.is_empty(),
        "the `+{ADDED_ROW}` row ({added_y}) should be painted with a background \
         the context row ({clean_y}) doesn't have; saw {added_bgs:?} vs \
         {context_bgs:?}\nScreen:\n{screen}",
    );

    // The context line between the two additions must carry none of it.
    let ctx_bgs = detail_backgrounds(&harness, ctx_y);
    let leaked: Vec<Color> = ctx_bgs
        .iter()
        .copied()
        .filter(|bg| addition_only.contains(bg))
        .collect();
    assert!(
        leaked.is_empty(),
        "the context row `{CTX_ROW}` ({ctx_y}) must carry no addition \
         background — the stripe below it drifted up onto it; leaked {leaked:?} \
         out of {ctx_bgs:?}\nScreen:\n{screen}",
    );
}

/// The line the cursor is parked on in the second test. It sits below four
/// accented lines, so the byte/UTF-16 drift accumulated above it is what
/// decides whether `Enter` opens the file at the right place.
const ENTER_TARGET: &str = "TARGET LINE";

/// Pressing `Enter` on a diff row opens that file at *that* row's line. The
/// cursor's byte offset is mapped to a diff line by the same kind of walk the
/// colouring used, so with UTF-16 lengths the mapping slid several lines down
/// the file once the diff contained non-ASCII text.
// TODO: git command output differs on Windows; the other git_log tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_enter_opens_the_line_under_the_cursor_after_non_ascii() {
    let repo = GitTestRepo::new();

    // One new-file commit: every source line shows up as a `+` row, so the
    // diff row the cursor lands on maps 1:1 onto a line number. The four
    // accented lines contribute 60 bytes the UTF-16 walk never saw, which is
    // enough to slide the mapping ~6 rows past `TARGET LINE` (line 6).
    let accented = "ñññññ üüüüü ·····";
    let mut content = String::new();
    for _ in 0..4 {
        content.push_str(accented);
        content.push('\n');
    }
    content.push_str("aa\n");
    content.push_str(ENTER_TARGET);
    content.push('\n');
    for i in 1..=10 {
        content.push_str(&format!("fill {i:02}\n"));
    }
    repo.create_file("notes.txt", &content);
    repo.git_add(&["notes.txt"]);
    repo.git_commit("Add notes");

    repo.setup_git_log_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Wide enough that no diff row wraps in the 40%-width detail panel — the
    // cursor is walked down by display rows below, so a wrapped row would
    // desync the count from the buffer's lines.
    let mut harness = harness_with_highlighting(160, 45, repo.path.clone());

    harness.open_file(&repo.path.join("notes.txt")).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane") && s.contains("Author:") && s.contains(ENTER_TARGET)
        })
        .unwrap();

    // The detail panel opens scrolled to the top with its cursor on the first
    // line (`commit <sha>`, the row above `Author:`), so the number of `Down`
    // presses that lands on the target row is the on-screen distance between
    // them.
    let screen = harness.screen_to_string();
    let author_y = row_containing(&harness, "Author:")
        .unwrap_or_else(|| panic!("no `Author:` line in the diff.\nScreen:\n{screen}"));
    let target_y = row_containing(&harness, ENTER_TARGET)
        .unwrap_or_else(|| panic!("`{ENTER_TARGET}` never rendered.\nScreen:\n{screen}"));
    let first_y = author_y - 1;
    assert!(
        target_y > first_y,
        "the diff should render `{ENTER_TARGET}` below its `commit` header; \
         saw rows {target_y} and {first_y}\nScreen:\n{screen}",
    );

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..(target_y - first_y) {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // git_log titles the file-at-commit view `*<hash>:notes.txt*`, so
    // `:notes.txt` is unique to it (the diff pane only ever shows `b/notes.txt`).
    harness
        .wait_until(|h| h.screen_to_string().contains(":notes.txt"))
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();

    // `TARGET LINE` is line 6 of notes.txt; the status bar reports where the
    // cursor actually landed.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 6, Col 1"),
        "Enter on the `+{ENTER_TARGET}` row should open notes.txt at line 6 \
         (`Ln 6, Col 1`), but the cursor landed elsewhere.\nScreen:\n{screen}",
    );
}
