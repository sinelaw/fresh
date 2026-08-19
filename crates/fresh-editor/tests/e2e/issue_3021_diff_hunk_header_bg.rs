//! Regression test for <https://github.com/sinelaw/fresh/issues/3021>:
//!
//! In a `.diff` buffer the hunk header's `diff_modify_bg` bar stopped at
//! the closing `@@`, so the enclosing-section name `git diff` appends
//! after it (`@@ -2,6 +2,8 @@ fn keep_one(…)`) sat in a bg-less hole and
//! the bar resumed only past end-of-line. `git diff` puts that section
//! name on nearly every real hunk header, so most hunk bars showed the
//! gap in practice.
//!
//! Asserted purely from the rendered screen: the background of every
//! cell of the hunk-header row's text, compared against the background
//! of the `@@` marker itself and against an ordinary context row.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use tempfile::TempDir;

/// The hunk header carries a section name after the closing `@@` — the
/// span that had no background. It is deliberately not the first line of
/// the file, so the cursor line (line 1) never overlaps the row under
/// test and can't tint its background.
const HUNK_HEADER: &str = "@@ -2,6 +2,8 @@ fn keep_one(&self) -> usize {";
const CONTEXT_LINE: &str = " context stays plain";

fn diff_content() -> String {
    format!(
        "diff --git a/file.rs b/file.rs\n\
         index 1111111..2222222 100644\n\
         --- a/file.rs\n\
         +++ b/file.rs\n\
         {HUNK_HEADER}\n\
         {CONTEXT_LINE}\n\
         +added line\n"
    )
}

/// Row index of the first screen row whose text contains `needle`, plus
/// the column the needle starts at.
fn find_row(harness: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let buf = harness.buffer();
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if let Some(byte_idx) = row.find(needle) {
            // The screen rows are built one cell per column, and the
            // fixture is ASCII, so the byte index is the column.
            return (y, byte_idx as u16);
        }
    }
    panic!(
        "never found `{needle}` on screen:\n{}",
        harness_dump(harness)
    );
}

fn harness_dump(harness: &EditorTestHarness) -> String {
    let buf = harness.buffer();
    let mut out = String::new();
    for y in 0..buf.area.height {
        for x in 0..buf.area.width {
            out.push_str(buf[(x, y)].symbol());
        }
        out.push('\n');
    }
    out
}

#[test]
fn test_diff_hunk_header_row_has_no_background_gap() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("hunk.diff");
    std::fs::write(&file_path, diff_content()).unwrap();

    let mut harness = EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains("keep_one").unwrap();

    let (header_row, header_col) = find_row(&harness, HUNK_HEADER);
    let (context_row, context_col) = find_row(&harness, CONTEXT_LINE.trim_end());

    let marker_bg = harness
        .get_cell_style(header_col, header_row)
        .and_then(|s| s.bg);
    let context_bg = harness
        .get_cell_style(context_col, context_row)
        .and_then(|s| s.bg);

    // Guard against a vacuous pass: if the hunk header carried no wash at
    // all, "every cell matches the marker cell" would hold trivially.
    assert_ne!(
        marker_bg, context_bg,
        "the `@@` marker at ({header_col}, {header_row}) should carry the \
         diff_modify_bg wash, distinct from a context row's background \
         {context_bg:?}",
    );

    for offset in 0..HUNK_HEADER.chars().count() as u16 {
        let x = header_col + offset;
        let bg = harness.get_cell_style(x, header_row).and_then(|s| s.bg);
        let ch = harness.buffer()[(x, header_row)].symbol().to_string();
        assert_eq!(
            bg,
            marker_bg,
            "column {x} (`{ch}`) of the hunk header row should carry the \
             same diff background as the `@@` marker ({marker_bg:?}); saw \
             {bg:?}\n{}",
            harness_dump(&harness),
        );
    }
}
