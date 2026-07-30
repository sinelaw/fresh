//! E2E tests for the command-palette settings-toggle scope convention.
//!
//! The convention (documented on `input::commands`) is that a toggle's scope is
//! readable from its name: `Toggle X (Current Buffer)` changes only the active
//! buffer and is persisted per file, while an unsuffixed `Toggle X` changes the
//! editor-wide default and is saved to the user config layer. Both halves have
//! to hold or the palette lies about what a command does.
//!
//! The per-buffer tests below cover line numbers, line wrap, indentation
//! guides, folding indicators, whitespace indicators, indentation style,
//! the current-line highlight and occurrence highlighting:
//! each must affect only the current buffer and survive a session restart, and
//! all of them assert on rendered screen output. The final test covers the
//! global half, where the durable artifact is a config file rather than
//! anything on screen.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// A guide-bearing first indent level, using the default guide glyph.
const GUIDED_LINE: &str = "▏   let child = 1;";
/// Gutter indicator for an expanded, foldable line.
const EXPANDED_FOLD: &str = "▾";

const FOLDABLE_SOURCE: &str = "fn main() {\n    let child = 1;\n}\n";

/// Whether the row holding `needle` is painted with the current-line
/// highlight, judged by comparing its background to another content row's.
/// Reading the styled cell is the only way this is visible on screen.
fn cursor_line_is_highlighted(harness: &EditorTestHarness, needle: &str) -> bool {
    let (_, cursor_row) = harness
        .find_text_on_screen(needle)
        .unwrap_or_else(|| panic!("expected {needle:?} on screen"));
    let (start, end) = harness.content_area_rows();
    let other_row = (start..=end)
        .map(|r| r as u16)
        .find(|r| *r != cursor_row && !harness.get_row_text(*r).trim().is_empty())
        .expect("expected a second content row to compare against");
    harness.get_cell_style(0, cursor_row).map(|s| s.bg)
        != harness.get_cell_style(0, other_row).map(|s| s.bg)
}

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

/// A per-buffer whitespace-indicator override must survive a session restart.
///
/// Go hides tab indicators by default, so the toggle turns them *on* here —
/// which is the direction that actually exercises the restore path: "shown" has
/// to be re-derived rather than read back from config, or a language that hides
/// them would quietly swallow the choice.
#[test]
fn test_whitespace_indicators_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.go");
    std::fs::write(&file, "func main() {\n\tprintln(\"x\")\n}\n").unwrap();

    // Session 1: Go hides tab indicators; turn them on for this buffer.
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
        harness.assert_screen_not_contains("→");

        run_command(&mut harness, "Toggle Tab Indicators (Current Buffer)");
        harness.assert_screen_contains("→");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; the indicators are still shown for this buffer.
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

        harness.assert_screen_contains("→");
    }
}

/// A per-buffer indentation-style override must survive a session restart.
///
/// Go defaults to tabs, so after switching this buffer to spaces a fresh Tab
/// keypress must not produce the tab indicator — with indicators explicitly
/// turned on, so the assertion can't pass vacuously.
#[test]
fn test_indentation_style_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.go");
    std::fs::write(&file, "\n").unwrap();

    // Session 1: show tab indicators, then switch this buffer to spaces.
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

        run_command(&mut harness, "Toggle Tab Indicators (Current Buffer)");
        run_command(&mut harness, "Toggle Indentation: Spaces");
        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore, then press Tab — spaces, so no indicator appears.
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

        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();

        assert!(
            !harness.screen_to_string().contains('→'),
            "Tab must still insert spaces after a restart — a restored \
             `use_tabs` override that `apply_config` re-stamped would put a real \
             tab here, and indicators are on so it would show. Screen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// The other half of the naming convention: an *unsuffixed* toggle changes the
/// editor-wide default and saves it, so the choice is still there next launch.
///
/// Persistence lands in the user config layer on disk, which is by definition
/// not on screen, so this asserts on the written file — the durable artifact
/// the convention promises. The on-screen half of each toggle is covered by the
/// scoping tests above and by each feature's own suite.
///
/// Before this convention was enforced, several of these mutated only the
/// in-memory `Arc<Config>` (or a `Window` flag) and were silently forgotten.
#[test]
fn test_global_view_toggles_save_to_the_user_config_layer() {
    let cases: &[(&str, &str)] = &[
        ("Toggle Line Wrap", "line_wrap"),
        ("Toggle Current Line Highlight", "highlight_current_line"),
        ("Toggle Occurrence Highlight", "highlight_occurrences"),
        ("Toggle Inlay Hints", "enable_inlay_hints"),
        ("Toggle Mouse Hover", "mouse_hover_enabled"),
        ("Toggle Tab Bar", "show_tab_bar"),
        ("Toggle Status Bar", "show_status_bar"),
        ("Toggle Prompt Line", "show_prompt_line"),
        ("Toggle Line Numbers", "line_numbers"),
        ("Toggle Menu Bar", "show_menu_bar"),
    ];

    for (command, setting) in cases {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project");
        std::fs::create_dir(&project_dir).unwrap();
        let file = project_dir.join("a.txt");
        std::fs::write(&file, "alpha\n").unwrap();

        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();

        run_command(&mut harness, command);

        let config_path = dir_context.config_path();
        let written = std::fs::read_to_string(&config_path).unwrap_or_else(|e| {
            panic!(
                "{command:?} should have written {}: {e}",
                config_path.display()
            )
        });
        let json: serde_json::Value = serde_json::from_str(&written)
            .unwrap_or_else(|e| panic!("{command:?} wrote invalid JSON: {e}\n{written}"));
        assert!(
            json.pointer(&format!("/editor/{setting}")).is_some(),
            "{command:?} must save `editor.{setting}` to the user config layer — an \
             unsuffixed toggle changes the editor-wide default and has to survive a \
             restart. Written config:\n{written}"
        );
    }
}

/// The current-line highlight is a per-(split, buffer) flag, so its per-buffer
/// toggle must pin one buffer without disturbing the rest — and must survive a
/// restart. Observed as the cursor row's background differing from a
/// non-cursor row's.
#[test]
fn test_current_line_highlight_current_buffer_scopes_and_persists() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let a = project_dir.join("a.txt");
    let b = project_dir.join("b.txt");
    std::fs::write(&a, "alpha\nbeta\n").unwrap();
    std::fs::write(&b, "delta\nepsilon\n").unwrap();

    // Session 1: pin the highlight off for a.txt only.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&a).unwrap();
        harness.render().unwrap();
        assert!(
            cursor_line_is_highlighted(&harness, "alpha"),
            "precondition: the highlight is on by default\n{}",
            harness.screen_to_string()
        );

        run_command(
            &mut harness,
            "Toggle Current Line Highlight (Current Buffer)",
        );
        assert!(
            !cursor_line_is_highlighted(&harness, "alpha"),
            "the pin should drop the highlight in a.txt\n{}",
            harness.screen_to_string()
        );

        // b.txt is untouched and still follows the global default.
        harness.open_file(&b).unwrap();
        harness.render().unwrap();
        assert!(
            cursor_line_is_highlighted(&harness, "delta"),
            "the pin must not leak into other buffers\n{}",
            harness.screen_to_string()
        );

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; a.txt is still unhighlighted, b.txt still is.
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
        harness.open_file(&a).unwrap();
        harness.render().unwrap();

        assert!(
            !cursor_line_is_highlighted(&harness, "alpha"),
            "the per-buffer current-line-highlight pin should survive a restart\n{}",
            harness.screen_to_string()
        );
    }
}

/// The per-buffer occurrence-highlight pin must round-trip through the
/// workspace file.
///
/// Unlike the other toggles here this one asserts on the saved artifact rather
/// than the screen: occurrence highlighting paints on a debounce after cursor
/// movement, so a rendered assertion would be a timing race, and CONTRIBUTING
/// rules out time-sensitive tests. Re-saving in session 2 is what makes this a
/// real round-trip — the field is only written back if restore actually applied
/// it, and every file-open path stamps `reference_highlight_overlay.enabled`
/// from config, so a pin the restore path dropped would vanish here.
#[test]
fn test_occurrence_highlight_current_buffer_round_trips_through_the_workspace() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.rs");
    std::fs::write(&file, "let alpha = 1;\nlet beta = alpha;\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let saved_override = |label: &str| -> serde_json::Value {
        let dir = dir_context.data_dir.join("workspaces");
        let mut found = serde_json::Value::Null;
        for entry in std::fs::read_dir(&dir)
            .unwrap_or_else(|e| panic!("{label}: no workspaces dir at {}: {e}", dir.display()))
        {
            let path = entry.unwrap().path();
            if path.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }
            let json: serde_json::Value =
                serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
            // The file states are nested under the split tree; walk for the key
            // rather than hard-coding the layout.
            let mut stack = vec![json];
            while let Some(node) = stack.pop() {
                match node {
                    serde_json::Value::Object(map) => {
                        for (key, value) in map {
                            if key == "highlight_occurrences" {
                                found = value.clone();
                            }
                            stack.push(value);
                        }
                    }
                    serde_json::Value::Array(items) => stack.extend(items),
                    _ => {}
                }
            }
        }
        found
    };

    let open_session = |dir_context: DirectoryContext| {
        EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context)
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // Session 1: pin occurrence highlighting off for this buffer and save.
    {
        let mut harness = open_session(dir_context.clone());
        harness.open_file(&file).unwrap();
        harness.render().unwrap();

        run_command(&mut harness, "Toggle Occurrence Highlight (Current Buffer)");
        harness.editor_mut().save_workspace().unwrap();
    }
    assert_eq!(
        saved_override("after session 1"),
        serde_json::Value::Bool(false),
        "the toggle should record `highlight_occurrences: false` for this file"
    );

    // Session 2: restore, re-open, and save again. The pin has to come back,
    // or this second save drops the field.
    {
        let mut harness = open_session(dir_context.clone());
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.editor_mut().save_workspace().unwrap();
    }
    assert_eq!(
        saved_override("after session 2"),
        serde_json::Value::Bool(false),
        "the pin must survive restore — re-opening the file re-stamps \
         `reference_highlight_overlay.enabled` from config, so this is where a \
         dropped override shows up"
    );
}
