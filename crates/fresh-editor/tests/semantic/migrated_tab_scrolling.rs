//! Migration of `tests/e2e/tab_scrolling.rs` — tab-bar overflow
//! behaviour at a narrow width: cycling buffers always keeps the
//! active tab visible, and clicking the `<` / `>` scroll buttons
//! routes through the mouse path.
//!
//! ## DECLARATIVE-REWRITE DEFERRAL
//!
//! A purely declarative rewrite (scenarios-as-data, zero
//! `EditorTestHarness::` usage) was attempted and DEFERRED. The
//! load-bearing claims here need DSL extensions that don't yet exist:
//!
//!   * Multi-file workspace on a real `TempDir` — `open_file(path)`
//!     loads buffers from disk so each tab's display name is the
//!     resolved filename. `WorkspaceScenario`'s `NamedBuffer` carries
//!     in-memory content keyed by `filename`, but does not currently
//!     drive `Action::Open` against a real on-disk path. Extension
//!     needed: add `WorkspaceContext.tempfs_files: Vec<(PathBuf,
//!     String)>` and a runner step that writes them to disk before
//!     dispatching `Action::Open(path)` for each one.
//!
//!   * Mouse-click on the `<` / `>` tab-bar scroll buttons —
//!     `dispatch_mouse_click(col, row)` is on `EditorTestApi`, but
//!     `LayoutScenario` only accepts `actions: Vec<Action>` and
//!     `WorkspaceScenario` accepts no input events at all. Extension
//!     needed: thread `Vec<InputEvent>` through either runner so a
//!     scenario can interleave `InputEvent::Mouse(Click{...})` with
//!     buffer cycling.
//!
//!   * Per-step assertion (active filename on screen at every cycle
//!     step) — `LayoutScenario` evaluates expectations once at the
//!     end. Extension needed: either a per-step assertion shape, or
//!     a folded representation where each `Action::NextBuffer` is
//!     followed by an `InputEvent::Wait(WaitCondition::ScreenContains(
//!     filename))` that the runner asserts on the fly.
//!
//!   * Tab-bar `<` / `>` indicator visibility — requires inspecting
//!     specific characters at the tab-bar row. Expressible with
//!     `RowMatch::AnyRowContains(">")` but the per-step edge
//!     invariants ("first tab → no `<`, last tab → no `>`") need a
//!     parameterized matcher tied to the cycle step.
//!
//! Keeping the current harness-direct implementation until the
//! WorkspaceScenario / LayoutScenario extensions land.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Active tab visibility on open / cycle.** Opening many
//!      files into a narrow (NARROW_WIDTH=40) terminal must keep
//!      the most-recently-active tab's filename rendered on screen.
//!      Cycling forward (Ctrl+PageDown → NextBuffer) and backward
//!      (Ctrl+PageUp → PrevBuffer) must keep that invariant at
//!      every step. Edge invariants from the e2e: the leftmost
//!      tab must not draw a `<` indicator (nothing to scroll left
//!      to); the rightmost tab must not draw a `>` indicator
//!      (nothing to scroll right to).
//!
//!   2. **Manual scroll round-trip via Alt+PageDown/Up.** Manual
//!      tab-bar scrolling (Alt+PageDown=ScrollTabsRight,
//!      Alt+PageUp=ScrollTabsLeft) may move the active tab off
//!      screen, but any subsequent NextBuffer (Ctrl+PageDown) must
//!      bring the newly-active tab's filename back on screen — the
//!      "switch tab snaps view to active" contract.
//!
//!   3. **Mouse-click scroll buttons.** When the `>` indicator is
//!      visible (we're on the first tab of an overflowing bar),
//!      clicking the rightmost column of the tab-bar row routes a
//!      mouse-click through to the scroll-tabs-right action. The
//!      `<` indicator path mirrors it on the leftmost column. The
//!      e2e gates these blocks behind `if screen.contains(">")` /
//!      `if screen.contains("<")` — that conditional is preserved
//!      verbatim (overflow may not always render an indicator on
//!      every harness configuration; the test asserts that *if*
//!      the indicator is shown, the click is accepted).
//!
//! ## Harness-direct pattern
//!
//! All three claims need `EditorTestHarness` surfaces with no
//! `EditorTestApi` projection: `open_file` (multi-file workspace
//! setup against a real `TempDir`), `assert_screen_contains` /
//! `screen_to_string` (full rendered-screen substring search the
//! e2e uses), and `mouse_click` (low-level event routing). The
//! migration uses the harness-direct pattern.
//!
//! Source: `tests/e2e/tab_scrolling.rs` (2 tests migrated; no
//! tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

const NARROW_WIDTH: u16 = 40;
const TEST_HEIGHT: u16 = 20;
const NUM_FILES: usize = 15;

/// Helper to create dummy files with long names (mirrors the e2e
/// `create_dummy_files`).
fn create_dummy_files(temp_dir: &TempDir) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    for i in 0..NUM_FILES {
        let file_name = format!("long_file_name_number_{:02}.txt", i);
        let file_path = temp_dir.path().join(&file_name);
        std::fs::write(&file_path, format!("Content for file {}", i)).unwrap();
        files.push(file_path);
    }
    files
}

/// Display-width cap the tab bar applies to a tab's *name* portion
/// (mirrors `view::ui::tabs::TAB_NAME_MAX_COLS`, which is private).
const TAB_NAME_MAX_COLS: usize = 25;

/// The on-screen label the tab bar actually paints for a file.
///
/// Since issue #2650, a name wider than [`TAB_NAME_MAX_COLS`] columns is
/// elided to its leading `TAB_NAME_MAX_COLS - 1` columns plus a single `…`
/// so one long filename can't consume the whole strip and hide every other
/// tab. Every dummy filename here (`long_file_name_number_NN.txt`, 28 chars)
/// exceeds the cap, so each renders as its 24-char prefix + `…`. That prefix
/// still carries the two-digit index, so the elided labels stay unique per
/// file and the "active tab is visible" claim holds — we just assert on the
/// label the editor actually draws instead of the full, now-elided filename.
fn expected_tab_label(file_path: &std::path::Path) -> String {
    let name = file_path.file_name().unwrap().to_str().unwrap();
    if name.chars().count() <= TAB_NAME_MAX_COLS {
        name.to_string()
    } else {
        let prefix: String = name.chars().take(TAB_NAME_MAX_COLS - 1).collect();
        format!("{prefix}…")
    }
}

#[test]
fn migrated_active_tab_visibility_with_scrolling() {
    // Original: `test_active_tab_visibility_with_scrolling`. The
    // claim chain is opening-many-files + Ctrl+PageDown cycle
    // forward + Ctrl+PageUp cycle backward + Alt+PageDown/Up
    // manual scroll + final Ctrl+PageDown snaps active back on
    // screen. Each step asserts the active tab's filename is on
    // screen, plus the leftmost/rightmost edge invariants on the
    // `<` / `>` indicators.
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(NARROW_WIDTH, TEST_HEIGHT).unwrap();

    // Open all dummy files
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains(&expected_tab_label(file_path));
    }

    // Initial check: Last opened file is active.
    let mut active_idx = NUM_FILES - 1;
    harness.render().unwrap();
    harness.assert_screen_contains(&expected_tab_label(&files[active_idx]));
    if active_idx < NUM_FILES - 1 {
        assert!(
            harness.screen_to_string().contains(">"),
            "Expected right scroll indicator after opening many files. Screen:\n{}",
            harness.screen_to_string()
        );
    }

    // --- Cycle Forward (Next Buffer) ---
    for _i in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + 1) % NUM_FILES;

        harness.render().unwrap();
        harness.assert_screen_contains(&expected_tab_label(&files[active_idx]));

        let screen = harness.screen_to_string();
        // The e2e only enforces the no-left-indicator-on-first edge.
        if active_idx == 0 {
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                expected_tab_label(&files[active_idx])
            );
        }
    }

    // --- Cycle Backward (Prev Buffer) ---
    for _i in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + NUM_FILES - 1) % NUM_FILES;

        harness.render().unwrap();
        harness.assert_screen_contains(&expected_tab_label(&files[active_idx]));

        let screen = harness.screen_to_string();
        if active_idx == 0 {
            assert!(
                !screen.contains("<"),
                "Expected no left scroll indicator for file: {}",
                expected_tab_label(&files[active_idx])
            );
        }
        if active_idx == NUM_FILES - 1 {
            assert!(
                !screen.contains(">"),
                "Expected no right scroll indicator for file: {}",
                expected_tab_label(&files[active_idx])
            );
        }
    }

    // --- Test manual scrolling ---
    // Activate a middle tab so manual scroll can move it off-screen.
    let middle_idx = NUM_FILES / 2;
    let steps_to_middle = (middle_idx + NUM_FILES - active_idx) % NUM_FILES;
    for _ in 0..steps_to_middle {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        active_idx = (active_idx + 1) % NUM_FILES;
        harness.render().unwrap();
    }
    assert_eq!(active_idx, middle_idx, "Failed to activate middle tab");
    harness.assert_screen_contains(&expected_tab_label(&files[active_idx]));

    // Scroll right manually — active tab may scroll out of view.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }

    // Scroll left manually
    for _ in 0..10 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::ALT)
            .unwrap();
        harness.render().unwrap();
    }

    // After manual scrolling, switching tabs should bring active tab back into view.
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    active_idx = (active_idx + 1) % NUM_FILES;
    harness.assert_screen_contains(&expected_tab_label(&files[active_idx]));
}

#[test]
fn migrated_tab_scroll_button_click() {
    // Original: `test_tab_scroll_button_click`. Wider terminal
    // (80) so filenames are fully visible; focus is on the click
    // routing through the mouse path to the scroll-tabs-right /
    // scroll-tabs-left actions. The `if screen.contains(">")` /
    // `if screen.contains("<")` guards are preserved as the e2e
    // had them.
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(80, TEST_HEIGHT).unwrap();

    // Open all dummy files to ensure tab overflow.
    for file_path in &files {
        harness.open_file(file_path).unwrap();
        harness.render().unwrap();
    }

    // Go to first tab to ensure we can scroll right.
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now we're on the first file - should see ">" indicator for right scroll.
    let screen = harness.screen_to_string();
    if screen.contains(">") {
        let tab_row = 1; // Tab bar is usually at row 1.
        let right_scroll_col = NARROW_WIDTH - 1;

        harness.mouse_click(right_scroll_col, tab_row).unwrap();
        harness.render().unwrap();
    }

    // Go to last tab to ensure we can scroll left.
    for _ in 0..NUM_FILES {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
    }

    // Now on the last file - should see "<" indicator for left scroll.
    let screen = harness.screen_to_string();
    if screen.contains("<") {
        let tab_row = 1;
        let left_scroll_col = 0;

        harness.mouse_click(left_scroll_col, tab_row).unwrap();
        harness.render().unwrap();
    }
}

/// Anti-test: drop the `open_file` loop. Without any files opened
/// into the harness, none of the long dummy filenames may appear
/// on screen — proves the positive test's "active tab visibility"
/// claim depends on the actual `open_file` calls registering tabs
/// in the buffer-group, not on the filenames being spuriously
/// rendered (e.g. from a status message or workspace tree).
#[test]
fn anti_no_open_file_means_no_long_filenames_on_screen() {
    let temp_dir = TempDir::new().unwrap();
    let files = create_dummy_files(&temp_dir);

    let mut harness = EditorTestHarness::new(NARROW_WIDTH, TEST_HEIGHT).unwrap();
    // No open_file calls — that's the load-bearing step we drop.
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    for file_path in &files {
        let name = file_path.file_name().unwrap().to_str().unwrap();
        assert!(
            !screen.contains(name),
            "anti: without open_file, the dummy filename {name:?} \
             must NOT appear on screen. Screen:\n{screen}"
        );
    }
}
