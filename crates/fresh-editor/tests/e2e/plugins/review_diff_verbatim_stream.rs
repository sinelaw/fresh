//! Review Diff: the unified stream is git's own output.
//!
//! The stream used to be laid out row by row in the plugin — a line-number
//! prefix baked into every row's text, a style and a property record per
//! row, a syntax region per hunk side — and every one of those was paid for
//! per row of a hundred-thousand-row review before the first row was on
//! screen. Now the stream carries `git diff`'s bytes as written, and the
//! host does the per-row work at paint time: its diff grammar colours the
//! rows, each file in its own language; its diff gutter numbers them from
//! the hunk headers; the plugin's own rows — section and file headers, note
//! boxes — are spliced in between.
//!
//! These tests pin down what the reader sees of that arrangement: numbers
//! that follow the hunk header rather than the buffer, the file's label
//! over git's `diff --git` row, code colour and change wash, a note box
//! under its line, and a collapsed hunk's glyph on git's own `@@` row.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
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

/// Tests get an empty grammar registry by default (fast startup); these
/// are about what the diff grammar draws, so they opt into the real thing.
fn harness_for(repo: &GitTestRepo) -> EditorTestHarness {
    EditorTestHarness::create(
        160,
        44,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_working_dir(repo.path.clone())
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// Open the review and wait until `first_row` — a string from the diff
/// rows the stream puts on screen — is actually rendered. The toolbar
/// lands well before the stream does.
fn open_review_diff_for(harness: &mut EditorTestHarness, first_row: &str) {
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
    harness
        .wait_until(|h| h.screen_to_string().contains(first_row))
        .unwrap();
}

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

/// Screen column at which `needle` starts in `row`.
fn column_of(row: &str, needle: &str) -> u16 {
    let at = row
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` not in `{row}`"));
    row[..at].chars().count() as u16
}

/// A file long enough for five-digit line numbers, changed once near its
/// end. The stream shows one hunk whose numbers start at 9999.
fn repo_with_five_digit_lines() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    let before: String = (1..=10004).map(|i| format!("line {i}\n")).collect();
    repo.create_file("src/long.txt", &before);
    repo.git_add_all();
    repo.git_commit("Initial commit");
    repo.create_file(
        "src/long.txt",
        &before.replace("line 10002\n", "changed 10002\n"),
    );
    repo
}

/// A Python file that gains a function: two context rows, then three
/// added rows, the second of which starts with a keyword.
fn repo_with_python_change() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    let before = "def add(a, b):\n    return a + b\n";
    repo.create_file("src/tool.py", before);
    repo.git_add_all();
    repo.git_commit("Initial commit");
    repo.create_file(
        "src/tool.py",
        &format!("{before}\ndef mul(a, b):\n    return a * b\n"),
    );
    repo
}

#[test]
fn review_stream_numbers_rows_from_the_hunk_header() {
    init_tracing_from_env();
    let repo = repo_with_five_digit_lines();
    let mut harness = harness_for(&repo);
    open_review_diff_for(&mut harness, "changed 10002");
    let screen = harness.screen_to_string();

    // A context row carries both numbers, a removal the old one alone, an
    // addition the new one alone — five digits wide, counted from the
    // hunk header, nothing to do with the row's position in the buffer.
    let context = format!("{:>5} {:>5} │  line 10001", 10001, 10001);
    let removed = format!("{:>5} {:>5} │ -line 10002", 10002, "");
    let added = format!("{:>5} {:>5} │ +changed 10002", "", 10002);
    for expected in [&context, &removed, &added] {
        assert!(
            screen.contains(expected.as_str()),
            "expected the gutter row `{expected}` on screen:\n{screen}"
        );
    }
    // The hunk header is git's own row, counts and all.
    assert!(
        screen.contains("@@ -9999,6 +9999,6 @@"),
        "expected git's hunk header verbatim:\n{screen}"
    );
    // git's `diff --git` row is in the buffer — it is what tells the
    // highlighter the file's language — and the reader sees the file's
    // label over it; the `index` / `---` / `+++` rows are not carried.
    for hidden in ["diff --git", "index ", "+++ b/"] {
        assert!(
            !screen.contains(hidden),
            "`{hidden}` should not be on screen:\n{screen}"
        );
    }
    assert!(
        screen.contains("▾ src/long.txt   +1 / -1"),
        "expected the file's label over its `diff --git` row:\n{screen}"
    );
}

#[test]
fn review_stream_colours_code_in_the_files_language() {
    init_tracing_from_env();
    let repo = repo_with_python_change();
    let mut harness = harness_for(&repo);
    open_review_diff_for(&mut harness, "+def mul");

    let added_y = row_containing(&harness, "+def mul(a, b):").unwrap();
    let added = row_text(&harness, added_y);
    let marker = column_of(&added, "+def mul");
    let context_y = row_containing(&harness, "return a + b").unwrap();
    let context = row_text(&harness, context_y);
    let context_marker = column_of(&context, "    return a + b") - 1;

    let buf = harness.buffer();
    // `def` is a keyword of the file's language and `a` a parameter: the
    // row was handed to the Python grammar, not painted one colour.
    let keyword = buf[(marker + 1, added_y)].style().fg;
    let param = buf[(marker + 9, added_y)].style().fg;
    assert_ne!(
        keyword,
        param,
        "`def` and `a` should differ in colour on `{added}`:\n{}",
        harness.screen_to_string()
    );
    // And the addition carries its wash: a background the context row
    // next to it does not have.
    let added_bg = buf[(marker + 1, added_y)].style().bg;
    let context_bg = buf[(context_marker + 1, context_y)].style().bg;
    assert_ne!(
        added_bg,
        context_bg,
        "an added row should be washed, a context row not:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn review_stream_note_box_follows_its_line() {
    init_tracing_from_env();
    let repo = repo_with_python_change();
    let mut harness = harness_for(&repo);
    open_review_diff_for(&mut harness, "+def mul");

    // The hunk's rows, after its header: two context rows, a blank
    // addition, then `+def mul`. `n` lands on the header.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Needs a docstring").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Needs a docstring"))
        .unwrap();

    // The box sits right under the line it annotates, titled with that
    // line's number, and its rows have no line numbers of their own.
    let line_y = row_containing(&harness, "+def mul(a, b):").unwrap();
    let top = row_text(&harness, line_y + 1);
    let body = row_text(&harness, line_y + 2);
    assert!(
        top.trim_start().starts_with("│ ╭─ +4 "),
        "the box's top border should follow the line, unnumbered: `{top}`\n{}",
        harness.screen_to_string()
    );
    assert!(
        body.trim_start().starts_with("│ │ Needs a docstring"),
        "the note text should be inside the box: `{body}`\n{}",
        harness.screen_to_string()
    );

    // `a` hides the notes and shows them again.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("Needs a docstring"))
        .unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Needs a docstring"))
        .unwrap();
}

#[test]
fn review_stream_collapsed_hunk_closes_its_own_header() {
    init_tracing_from_env();
    let repo = repo_with_python_change();
    let mut harness = harness_for(&repo);
    open_review_diff_for(&mut harness, "+def mul");

    let header = "@@ -1,2 +1,5 @@";
    let header_y = row_containing(&harness, header).unwrap();
    let header_x = column_of(&row_text(&harness, header_y), header);

    // A click on git's `@@` row folds the hunk; the row itself stays and
    // its `@@` reads as `▸ @@` while the body is hidden.
    harness.mouse_click(header_x + 3, header_y).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(&format!("▸ {header}")) && !s.contains("+def mul")
        })
        .unwrap();

    // `z r` expands everything: the body is back and the glyph is gone.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("▸ @@") && s.contains("+def mul")
        })
        .unwrap();
}
