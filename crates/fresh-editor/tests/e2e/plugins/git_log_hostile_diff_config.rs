//! The Git Log detail panel is `git show --stat --patch` streamed into a
//! `.diff` file. Three consumers read that file's patch headers and rows —
//! the host's `.diff` highlighting, the plugin's fold ranges, and `Enter`,
//! which walks back from the cursor to a `+++ b/<path>` header and a `@@`
//! hunk header to open the file at the cursor's line. User git config can
//! reshape every one of those lines: `color.diff=always` wraps them in
//! escapes, `diff.noprefix` strips the `b/` the header walk keys on, and
//! `core.quotePath` quotes and octal-escapes a non-ASCII path so neither the
//! header walk nor the follow-up `git show <hash>:<path>` can use it.
//!
//! The fixture switches all of those on and asserts the rendered result the
//! way `git_log_diff_highlight_offset.rs` does — no status-bar text.

use crate::common::git_test_helper::{git_command, GitTestRepo};
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

/// A non-ASCII path, in a subdirectory so `diff.relative` has something to
/// strip.
const FILE: &str = "notas/añadido.txt";
/// The context line between the accented addition and the plain one.
const CTX_ROW: &str = "CTX KEEP LINE";
/// The plain (all-ASCII) added line; line 5 of the new file.
const ADDED_ROW: &str = "PLAIN ADD ROW";
/// A context line below the hunk, the "no diff colour here" reference.
const CLEAN_ROW: &str = "gamma three";

const BEFORE: &str = "alpha one\nbeta two\nCTX KEEP LINE\ngamma three\ndelta four\n";
const AFTER: &str =
    "alpha one\nbeta two\nañadido ñ·ñ·ñ\nCTX KEEP LINE\nPLAIN ADD ROW\ngamma three\ndelta four\n";

/// A harness whose grammar registry can highlight the `.diff` buffer.
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

/// Text of screen row `y`.
fn row_text(harness: &EditorTestHarness, y: u16) -> String {
    let buf = harness.buffer();
    (0..buf.area.width)
        .map(|x| buf[(x, y)].symbol().to_string())
        .collect()
}

/// Row index of the first screen row containing `needle`.
fn row_containing(harness: &EditorTestHarness, needle: &str) -> Option<u16> {
    let height = harness.buffer().area.height;
    (0..height).find(|&y| row_text(harness, y).contains(needle))
}

/// The distinct background colours painted across the detail-panel part of
/// row `y` — everything right of the split's `│` divider.
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

/// Two commits on `FILE`, then every setting that reshapes `git show`'s patch.
fn hostile_repo() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.create_file(FILE, BEFORE);
    repo.git_add(&[FILE]);
    repo.git_commit("Add notes");
    repo.create_file(FILE, AFTER);
    repo.git_add(&[FILE]);
    repo.git_commit("Add accented and plain lines");

    for (key, value) in [
        ("color.diff", "always"),
        ("color.ui", "always"),
        ("diff.noprefix", "true"),
        ("diff.mnemonicPrefix", "true"),
        ("core.quotePath", "true"),
        ("diff.suppressBlankEmpty", "true"),
        ("diff.relative", "true"),
    ] {
        let output = git_command(&repo.path)
            .args(["config", "--local", key, value])
            .output()
            .expect("run git config");
        assert!(
            output.status.success(),
            "git config {key} failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    repo.setup_git_log_plugin();
    repo
}

// TODO: git command output differs on Windows; the other git_log tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_detail_parses_diff_under_hostile_diff_config() {
    let repo = hostile_repo();
    let _guard = repo.change_to_repo_dir();

    // Wide enough that no diff row wraps in the detail panel — the cursor is
    // walked down by display rows below.
    let mut harness = harness_with_highlighting(160, 45, repo.path.clone());
    harness.open_file(&repo.path.join(FILE)).unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Git Log").unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane")
                && s.contains("Author:")
                && s.contains(CTX_ROW)
                && s.contains(ADDED_ROW)
                && s.contains(CLEAN_ROW)
        })
        .unwrap();
    // `git show` streams into the buffer and the highlighter runs over what
    // has arrived; let the pipeline go quiet before reading cells.
    harness.wait_for_async_quiescence(3).unwrap();

    let screen = harness.screen_to_string();

    // The per-file header must carry the `b/` prefix and the unquoted path:
    // that exact form is what `Enter`'s header walk matches.
    let header = format!("+++ b/{FILE}");
    assert!(
        screen.contains(&header),
        "the detail panel should show `{header}` whatever the prefix and \
         quoting settings say.\nScreen:\n{screen}",
    );

    // Colour escapes in the file would defeat the `.diff` grammar: the `+`
    // row must still be painted with a background the context row lacks.
    let find = |needle: &str| {
        row_containing(&harness, needle)
            .unwrap_or_else(|| panic!("`{needle}` never rendered.\nScreen:\n{screen}"))
    };
    let added_y = find(ADDED_ROW);
    let clean_y = find(CLEAN_ROW);
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

    // `Enter` on the `+` row opens the file at that row's line. The detail
    // panel opens with its cursor on the `commit <sha>` line, the row above
    // `Author:`, so the on-screen distance is the number of `Down` presses.
    let author_y = find("Author:");
    let first_y = author_y - 1;
    assert!(
        added_y > first_y,
        "the diff should render `{ADDED_ROW}` below its `commit` header; \
         saw rows {added_y} and {first_y}\nScreen:\n{screen}",
    );
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..(added_y - first_y) {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // git_log titles the file-at-commit view `*<hash>:<path>*`, so `:notas/`
    // is unique to it. Fail fast on the two ways the walk gives up — a header
    // it could not match, or a path `git show` could not resolve — rather than
    // waiting forever for a view that will never open.
    // The view is created with its cursor already on the target line, so once
    // its title is up and the frame has settled the cursor is where it will
    // stay.
    let view_title = format!(":{FILE}");
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            if s.contains("Move cursor to a diff line") || s.contains("not found at commit") {
                panic!("Enter on the `+{ADDED_ROW}` row did not open the file.\nScreen:\n{s}");
            }
            s.contains(&view_title)
        })
        .unwrap();

    // `PLAIN ADD ROW` is line 5 of the new file: the cursor must sit at the
    // byte where that line starts.
    let expected: usize = AFTER.split_inclusive('\n').take(4).map(str::len).sum();
    let screen = harness.screen_to_string();
    assert_eq!(
        harness.cursor_position(),
        expected,
        "Enter on the `+{ADDED_ROW}` row should open {FILE} at line 5.\nScreen:\n{screen}",
    );
}
