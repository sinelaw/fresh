//! One placement, one format, one row build per visible text pane per frame.
//!
//! The retained-mode migration's Stage 1 moves every write the text
//! formatter made during paint — viewport placement, margins, the wrap
//! index, the scroll-to-end sync — into a pre-frame reconcile, so the
//! formatter is a read of `(state, viewport, rect)` and `build_view_data`
//! runs exactly once per pane per frame. The harness asserts the three
//! counters agree around every frame the corpus renders; these tests pin
//! the count itself to the number of panes on screen, across the paths
//! that used to build twice (a scroll-to-end sync) and the paths that
//! bypass the wrap index (a file beyond its ceilings).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::{frame_counters, FrameCounters};
use tempfile::TempDir;

/// Render one frame and return what it cost.
fn frame(harness: &mut EditorTestHarness) -> FrameCounters {
    let before = frame_counters();
    harness.render().unwrap();
    frame_counters().since(before)
}

/// Assert one frame placed, formatted and built exactly `panes` panes.
fn assert_frame_panes(harness: &mut EditorTestHarness, panes: u64, what: &str) {
    let cost = frame(harness);
    assert_eq!(
        cost,
        FrameCounters {
            pane_placements: panes,
            buffer_layouts: panes,
            view_data_builds: panes,
            composite_builds: 0,
        },
        "{what}: expected {panes} pane(s) placed, formatted and built once each, got {cost:?}"
    );
}

/// Run a command palette entry — waiting for the palette to list it, so a
/// plugin-registered command that lands late is not raced — and let the
/// frame after it settle.
fn palette(harness: &mut EditorTestHarness, command: &str) {
    harness.run_palette_command(command).unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

fn numbered_lines(count: usize, width: usize) -> String {
    (1..=count)
        .map(|i| format!("{:<width$}\n", format!("line {i:05}"), width = width))
        .collect()
}

fn open_text(harness: &mut EditorTestHarness, dir: &TempDir, name: &str, text: &str) {
    let path = dir.path().join(name);
    std::fs::write(&path, text).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
}

#[test]
fn a_single_pane_is_built_once_per_frame() {
    let dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    open_text(&mut harness, &dir, "plain.txt", &numbered_lines(200, 20));

    assert_frame_panes(&mut harness, 1, "idle frame");

    harness.type_text("x").unwrap();
    assert_frame_panes(&mut harness, 1, "after an edit");

    // Motion that scrolls: the cursor leaves the first page and the frame
    // that follows it is still one build.
    for _ in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_frame_panes(&mut harness, 1, "after scrolling motion");
    harness.assert_screen_contains("line 00041");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    assert_frame_panes(&mut harness, 1, "at the end of the document");
    harness.assert_screen_contains("line 00200");
}

#[test]
fn every_split_is_built_once_per_frame() {
    let dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_text(&mut harness, &dir, "split.txt", &numbered_lines(300, 20));
    assert_frame_panes(&mut harness, 1, "one pane");

    palette(&mut harness, "Split Vertical");
    assert_frame_panes(&mut harness, 2, "two panes on one buffer");

    palette(&mut harness, "Split Horizontal");
    assert_frame_panes(&mut harness, 3, "three panes on one buffer");

    for _ in 0..60 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_frame_panes(&mut harness, 3, "three panes after motion in one");
}

/// A same-buffer scroll-sync frame flags the other split to show the end
/// of the document. That used to be the second build — the rows were built
/// to count them, then built again from the answer. It is decided in row
/// space now, and the frame costs one build per pane.
#[test]
fn a_scroll_to_end_sync_frame_builds_each_pane_once() {
    let dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    open_text(&mut harness, &dir, "sync.txt", &numbered_lines(120, 20));

    palette(&mut harness, "Split Vertical");
    palette(&mut harness, "Toggle Scroll Sync");
    assert_frame_panes(&mut harness, 2, "two synced panes");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    assert_frame_panes(&mut harness, 2, "the frame after a jump to the end");
    assert_frame_panes(&mut harness, 2, "the frame after that");
    // Both panes show the document's tail.
    let screen = harness.screen_to_string();
    assert!(
        screen.matches("line 00120").count() >= 2,
        "both synced panes should show the last line:\n{screen}"
    );
}

#[test]
fn wrapped_long_lines_are_built_once_per_frame() {
    let dir = TempDir::new().unwrap();
    // Wrap is on by default; every line wraps into several rows.
    let mut harness = EditorTestHarness::new(60, 20).unwrap();
    open_text(&mut harness, &dir, "wrapped.txt", &numbered_lines(80, 200));
    assert_frame_panes(&mut harness, 1, "wrapped file");

    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_frame_panes(&mut harness, 1, "after motion through wrapped rows");
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    assert_frame_panes(&mut harness, 1, "cursor at the end of a wrapped line");
}

/// Beyond the wrap index's line ceiling there is no row space to place in:
/// the byte-oriented pass owns the viewport, and the pane still builds once.
#[test]
fn a_file_beyond_the_index_ceiling_is_built_once_per_frame() {
    let dir = TempDir::new().unwrap();
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();
    // 6,000 lines: past MAX_WRAP_SCROLLBAR_LINES (5,000), under the byte ceiling.
    open_text(&mut harness, &dir, "big.txt", &numbered_lines(6_000, 20));
    assert_frame_panes(&mut harness, 1, "unindexed file");

    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_frame_panes(&mut harness, 1, "after scrolling motion, unindexed");
    harness.assert_screen_contains("line 00051");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    assert_frame_panes(&mut harness, 1, "at the end, unindexed");
    harness.assert_screen_contains("line 06000");
}

/// A compose-mode split beside a source split of the same buffer: two
/// gutters, two wrap widths, one build each. The compose plugin has to be
/// in the project's plugins directory for its command to exist.
#[test]
fn a_compose_split_and_its_source_are_built_once_per_frame() {
    let dir = TempDir::new().unwrap();
    let project_root = dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let text: String = (1..=40)
        .map(|i| format!("# Heading {i}\n\nA paragraph of prose for section {i}, long enough to wrap when composed at a narrow page width.\n\n"))
        .collect();
    let md_path = project_root.join("doc.md");
    std::fs::write(&md_path, &text).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 40, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    assert_frame_panes(&mut harness, 1, "markdown source");

    palette(&mut harness, "Split Vertical");
    // Compose in the active (right) split, the way the compose split tests
    // do: the plugin registers its command asynchronously.
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
    // Compose conceals the heading markers in its pane (except on the cursor
    // line, where the markup stays revealed); the source pane keeps them.
    // Wait until only one pane still shows the second heading's marker.
    harness
        .wait_until_stable(|h| h.screen_to_string().matches("# Heading 2").count() == 1)
        .unwrap();
    assert_frame_panes(&mut harness, 2, "compose beside source");

    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_frame_panes(&mut harness, 2, "compose beside source, after motion");
}
