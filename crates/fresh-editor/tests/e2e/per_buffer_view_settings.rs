//! E2E tests for per-buffer view-setting command-palette variants.
//!
//! Covers "Toggle Line Numbers (Current Buffer)", "Toggle Line Wrap (Current
//! Buffer)", "Toggle Indentation Guides (Current Buffer)" and "Toggle Folding
//! Indicators (Current Buffer)": they must affect only the current buffer (not
//! others) and persist across a session restart. All assertions observe
//! rendered screen output.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// A guide-bearing first indent level, using the default guide glyph.
const GUIDED_LINE: &str = "▏   let child = 1;";
/// Gutter indicator for an expanded, foldable line.
const EXPANDED_FOLD: &str = "▾";

const FOLDABLE_SOURCE: &str = "fn main() {\n    let child = 1;\n}\n";

/// Content rows whose gutter shows the "expanded, foldable" arrow.
fn expanded_fold_rows(harness: &EditorTestHarness) -> Vec<usize> {
    let (start, end) = harness.content_area_rows();
    (start..=end)
        .filter(|row| harness.get_cell(0, *row as u16).as_deref() == Some(EXPANDED_FOLD))
        .collect()
}

/// Run a command-palette entry by fuzzy-typing its full name and pressing Enter.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Toggling line numbers for the current buffer via the command palette must
/// not change line-number visibility in other buffers.
#[test]
fn test_line_numbers_current_buffer_scopes_to_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), "alpha\nbeta\n").unwrap();
    std::fs::write(dir.join("b.txt"), "delta\nepsilon\n").unwrap();

    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // b.txt is active and shows the line-number gutter.
    harness.assert_screen_contains("│ delta");

    run_command(&mut harness, "Toggle Line Numbers (Current Buffer)");

    // b.txt now renders without a gutter, but its content is still shown.
    harness.assert_screen_not_contains("│ delta");
    harness.assert_screen_contains("delta");

    // a.txt is untouched: switching to it still shows the gutter.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("│ alpha");

    // Returning to b.txt keeps its per-buffer override.
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("│ delta");
    harness.assert_screen_contains("delta");
}

/// A per-buffer line-number override must survive a session restart.
#[test]
fn test_line_numbers_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, "alpha\nbeta\n").unwrap();

    // Session 1: turn line numbers off for this buffer, then save the workspace.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("│ alpha");

        harness.editor_mut().toggle_line_numbers_current_buffer();
        harness.render().unwrap();
        harness.assert_screen_not_contains("│ alpha");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore the workspace; line numbers stay off for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        harness.assert_screen_contains("alpha");
        harness.assert_screen_not_contains("│ alpha");
    }
}

/// Toggling line wrap for the current buffer must not change wrapping in other
/// buffers. With wrap on, the overflow tail of a long line is visible on a
/// wrapped row; with wrap off it is truncated off-screen.
#[test]
fn test_line_wrap_current_buffer_scopes_to_buffer() {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    let mut harness = EditorTestHarness::with_temp_project_and_config(60, 24, config).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), format!("{}TAILAAA\n", "A".repeat(80))).unwrap();
    std::fs::write(dir.join("b.txt"), format!("{}TAILBBB\n", "B".repeat(80))).unwrap();

    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // b.txt active: wrap on, so the overflow tail wraps onto a visible row.
    harness.assert_screen_contains("TAILBBB");

    run_command(&mut harness, "Toggle Line Wrap (Current Buffer)");

    // b.txt now has wrap off: the tail is truncated off-screen.
    harness.assert_screen_not_contains("TAILBBB");

    // a.txt is untouched: it still wraps and shows its tail.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("TAILAAA");

    // b.txt keeps its per-buffer override.
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("TAILBBB");
}

/// A per-buffer line-wrap override must survive a session restart.
#[test]
fn test_line_wrap_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, format!("{}TAILAAA\n", "A".repeat(80))).unwrap();

    let mk_config = || {
        let mut c = Config::default();
        c.editor.line_wrap = true;
        c
    };

    // Session 1: wrap is on (tail visible); turn it off for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            60,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("TAILAAA");

        harness.editor_mut().toggle_line_wrap_current_buffer();
        harness.render().unwrap();
        harness.assert_screen_not_contains("TAILAAA");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; wrap stays off for this buffer (tail truncated).
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            60,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        harness.assert_screen_not_contains("TAILAAA");
    }
}

/// Toggling indentation guides for the current buffer must draw them even when
/// the global `editor.indentation_guide` mode is `none`, and must not leak into
/// other buffers or rewrite the global setting.
#[test]
fn test_indentation_guide_current_buffer_scopes_to_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.rs"), FOLDABLE_SOURCE).unwrap();
    std::fs::write(dir.join("b.rs"), FOLDABLE_SOURCE).unwrap();

    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.open_file(&dir.join("b.rs")).unwrap();
    harness.render().unwrap();

    // Guides are off by default (global mode `none`).
    harness.assert_screen_not_contains(GUIDED_LINE);

    run_command(&mut harness, "Toggle Indentation Guides (Current Buffer)");

    // b.rs now draws guides.
    harness.assert_screen_contains(GUIDED_LINE);

    // a.rs is untouched: it still follows the global `none`. This is also how
    // we observe that the toggle didn't rewrite the global setting — a rewrite
    // would light up every buffer.
    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains(GUIDED_LINE);

    // Returning to b.rs keeps its per-buffer override, and toggling again
    // takes the guides back off.
    harness.open_file(&dir.join("b.rs")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(GUIDED_LINE);
    run_command(&mut harness, "Toggle Indentation Guides (Current Buffer)");
    harness.assert_screen_not_contains(GUIDED_LINE);
}

/// A per-buffer indentation-guide override must survive a session restart.
#[test]
fn test_indentation_guide_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.rs");
    std::fs::write(&file, FOLDABLE_SOURCE).unwrap();

    // Session 1: guides are off globally; turn them on for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains(GUIDED_LINE);

        harness
            .editor_mut()
            .toggle_indentation_guide_current_buffer();
        harness.render().unwrap();
        harness.assert_screen_contains(GUIDED_LINE);

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; the guides come back even though the global mode is
    // still `none`.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        harness.assert_screen_contains(GUIDED_LINE);
    }
}

/// Toggling the folding indicators for the current buffer must hide the gutter
/// arrows there and nowhere else.
#[test]
fn test_fold_indicators_current_buffer_scopes_to_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.rs"), FOLDABLE_SOURCE).unwrap();
    std::fs::write(dir.join("b.rs"), FOLDABLE_SOURCE).unwrap();

    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.open_file(&dir.join("b.rs")).unwrap();
    harness.render().unwrap();

    assert!(
        !expanded_fold_rows(&harness).is_empty(),
        "precondition: `fn main()` is foldable, so the gutter shows an arrow\n{}",
        harness.screen_to_string()
    );

    run_command(&mut harness, "Toggle Folding Indicators (Current Buffer)");
    assert!(
        expanded_fold_rows(&harness).is_empty(),
        "the arrows should be hidden in b.rs\n{}",
        harness.screen_to_string()
    );

    // a.rs is untouched.
    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.render().unwrap();
    assert!(
        !expanded_fold_rows(&harness).is_empty(),
        "the toggle must not leak into other buffers\n{}",
        harness.screen_to_string()
    );

    // b.rs keeps its override, and toggling again brings the arrows back.
    harness.open_file(&dir.join("b.rs")).unwrap();
    harness.render().unwrap();
    assert!(expanded_fold_rows(&harness).is_empty());
    run_command(&mut harness, "Toggle Folding Indicators (Current Buffer)");
    assert!(
        !expanded_fold_rows(&harness).is_empty(),
        "toggling a second time should restore the arrows\n{}",
        harness.screen_to_string()
    );
}

/// A per-buffer folding-indicator override must survive a session restart.
#[test]
fn test_fold_indicators_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.rs");
    std::fs::write(&file, FOLDABLE_SOURCE).unwrap();

    // Session 1: hide the fold arrows for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        assert!(!expanded_fold_rows(&harness).is_empty());

        harness.editor_mut().toggle_fold_indicators_current_buffer();
        harness.render().unwrap();
        assert!(expanded_fold_rows(&harness).is_empty());

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; the arrows stay hidden for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        assert!(
            expanded_fold_rows(&harness).is_empty(),
            "the per-buffer folding-indicator choice should survive a restart\n{}",
            harness.screen_to_string()
        );
    }
}
