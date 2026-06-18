// Regression probe for "stray pipes when scrolling": a wrapping table renders
// continuation virtual lines, and scrolling could leave isolated `│` border
// glyphs hanging on otherwise-blank lines. This guards the *composed buffer*
// against logical stray-border regressions (e.g. from table cell-splitting or
// virtual-line changes). The live-only variant of this artifact was a
// tmux/synchronized-update terminal-output bug (fixed in main.rs) that the
// headless ratatui backend cannot exercise. Detector: see `stray_rows`.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

const FIXTURE: &str = "\
# Stray Pipe Probe

Intro paragraph one.

## Code

```rust
fn main() {
    let total: i32 = (1..=10).sum();
    println!(\"sum = {total}\");
}
```

## Tables

### Basic

| Feature | Supported | Notes |
| ------- | :-------: | ----- |
| Headings | ✅ | h1-h6 |
| Tables | ✅ | alignment |
| Mermaid | ❓ | the point of this test |

### Wrapping

| Property | Value / Description | Status |
| -------- | ------------------- | :----: |
| Short | OK | ✅ |
| Long prose | This is an intentionally long sentence that should wrap across multiple lines within its cell so we can confirm soft wrapping behaves the way we expect. | ✅ |
| Long URL | https://example.com/some/really/long/path/that/keeps/going?param=one&param2=two&token=abcdefghijklmnopqrstuvwxyz0123456789 | ⚠️ |
| Mixed | Call renderTableCell(content, wrap true, maxWidth 320) and then re-measure to verify the layout still fits the viewport at all widths. | ❓ |

## After

Trailing paragraph one.

Trailing paragraph two.

Trailing paragraph three.

Trailing paragraph four.

Trailing paragraph five.
";

const BOX_VERTICALS: [char; 4] = ['│', '┼', '├', '┤'];

/// A "stray" row is one whose editor *content* region (everything to the right
/// of the optional File Explorer sidebar's two `│` borders) consists only of
/// whitespace plus one or more box-vertical glyphs — i.e. a border pipe left
/// hanging on an otherwise blank line. Splitting on `│` makes this robust
/// whether or not the sidebar is open:
///   - no sidebar, blank line:        "" -> 1 part, not flagged
///   - no sidebar, real table row:    has non-blank cell text, not flagged
///   - sidebar + blank content:       ["", " tree ", "      ", ""] -> flagged
///   - sidebar + real table row:      a content part has cell text, not flagged
fn stray_rows(screen: &str) -> Vec<(usize, String)> {
    screen
        .lines()
        .enumerate()
        .filter(|(_, l)| {
            // This test always runs with the File Explorer open, so each row's
            // first two `│` are the sidebar's left and right borders. The
            // editor content region is everything after the 2nd `│`.
            let mut pipes = l.match_indices('│');
            let _left = pipes.next();
            let after_sidebar = match pipes.next() {
                Some((idx, _)) => &l[idx + '│'.len_utf8()..],
                None => return false,
            };
            // A stray = a box-vertical in the content region whose only company
            // is whitespace (no real cell text), i.e. a border pipe hanging on
            // an otherwise blank line. A legit table row has cell text here.
            if !after_sidebar.chars().any(|c| BOX_VERTICALS.contains(&c)) {
                return false;
            }
            after_sidebar
                .chars()
                .all(|c| c.is_whitespace() || BOX_VERTICALS.contains(&c))
        })
        .map(|(i, l)| (i, l.to_string()))
        .collect()
}

#[test]
fn test_compose_mode_no_stray_pipes_on_scroll() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("probe.md");
    std::fs::write(&md_path, FIXTURE).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("│") && s.contains("─")
        })
        .unwrap();

    // Open the File Explorer sidebar — this offsets and narrows the compose
    // content area, matching the live editor's restored layout where the
    // stray-pipe artifact appears.
    harness.editor_mut().toggle_file_explorer();
    harness
        .wait_until_stable(|h| h.editor().file_explorer_visible())
        .unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    let (content_start, content_end) = harness.content_area_rows();
    let mid = ((content_start + content_end) / 2) as u16;

    let mut all_stray: Vec<String> = Vec::new();

    // Scroll down through the document a few rows at a time, then back up,
    // dumping any stray rows seen at each step.
    for step in 0..30 {
        harness.mouse_scroll_down(40, mid).unwrap();
        let _ = harness.render();
        let screen = harness.screen_to_string();
        for (row, text) in stray_rows(&screen) {
            all_stray.push(format!("down step {step} row {row}: {:?}", text));
        }
    }
    for step in 0..30 {
        harness.mouse_scroll_up(40, mid).unwrap();
        let _ = harness.render();
        let screen = harness.screen_to_string();
        for (row, text) in stray_rows(&screen) {
            all_stray.push(format!("up step {step} row {row}: {:?}", text));
        }
    }

    if !all_stray.is_empty() {
        eprintln!("=== STRAY PIPE ROWS DETECTED ({}) ===", all_stray.len());
        for s in &all_stray {
            eprintln!("{s}");
        }
        eprintln!("=== final screen ===");
        for (i, l) in harness.screen_to_string().lines().enumerate() {
            eprintln!("{i:3}: {l}");
        }
    }

    assert!(
        all_stray.is_empty(),
        "Found {} stray box-vertical glyph(s) on non-table rows while scrolling.",
        all_stray.len()
    );
}
