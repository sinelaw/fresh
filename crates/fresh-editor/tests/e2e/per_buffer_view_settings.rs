//! E2E tests for the command-palette settings-toggle scope convention.
//!
//! The convention (documented on `input::commands`) is that a toggle's scope is
//! readable from its name: `Toggle X (Current Buffer)` changes only the active
//! buffer and is persisted per file, while an unsuffixed `Toggle X` changes the
//! editor-wide default and is saved to the user config layer. Both halves have
//! to hold or the palette lies about what a command does.
//!
//! The per-buffer tests below cover line numbers, line wrap, indentation
//! guides, folding indicators, whitespace indicators, indentation style, the
//! current-line highlight and occurrence highlighting: each must affect only
//! the current buffer and survive a session restart, and all of them assert on
//! rendered screen output. The final test covers the global half, where the
//! durable artifact is a config file rather than anything on screen.

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

/// The per-buffer occurrence-highlight pin must survive a session restart.
///
/// Occurrence highlighting paints on a debounce after cursor movement, so
/// asserting on the highlighted words would be a timing race. The toggle's own
/// status message is set synchronously, and it reports the state it moved *to*
/// — so a second toggle after restore says "enabled" only if the restored state
/// was off. If restore had dropped the pin the buffer would be on the global
/// default (on) and this would read "disabled" instead.
///
/// The terminal is deliberately wide: the status bar truncates its message slot
/// on a narrower screen and the informative word is what gets cut.
#[test]
fn test_occurrence_highlight_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.rs");
    std::fs::write(&file, "let alpha = 1;\nlet beta = alpha;\n").unwrap();

    // Session 1: pin occurrence highlighting off for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            200,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();

        run_command(&mut harness, "Toggle Occurrence Highlight (Current Buffer)");
        harness.assert_screen_contains("Occurrence highlight disabled");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore, re-open the file (every open path re-stamps
    // `reference_highlight_overlay.enabled` from config, so this is where a
    // dropped override would show up), and toggle again.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            200,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.open_file(&file).unwrap();
        harness.render().unwrap();

        run_command(&mut harness, "Toggle Occurrence Highlight (Current Buffer)");
        harness.assert_screen_contains("Occurrence highlight enabled");
    }
}

/// "Reset Buffer Settings" must un-pin *every* per-buffer setting and bring
/// the rendered state back to the config default — including the two whose
/// state lives outside `BufferSettings`: the occurrence highlight (rendered
/// from `reference_highlight_overlay.enabled`, which nothing re-derives
/// unless reset does it explicitly) and the current-line highlight (pinned on
/// the split's `BufferViewState`, which `clear_user_overrides` can't reach).
///
/// The discriminator is running the "(Current Buffer)" toggle again after the
/// reset: both settings default to on, so a correct reset brings them back on
/// and the toggle reports "disabled". With either regression the setting is
/// still off after reset, and the toggle would report "enabled".
#[test]
fn test_reset_buffer_settings_unpins_highlights() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(200, 24, Config::default()).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.rs"), "let alpha = 1;\nlet beta = alpha;\n").unwrap();
    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.render().unwrap();

    // Pin both highlights off for this buffer.
    run_command(&mut harness, "Toggle Occurrence Highlight (Current Buffer)");
    harness.assert_screen_contains("Occurrence highlight disabled (this buffer)");
    run_command(
        &mut harness,
        "Toggle Current Line Highlight (Current Buffer)",
    );
    harness.assert_screen_contains("Current line highlight disabled (this buffer)");

    run_command(&mut harness, "Reset Buffer Settings");

    // After the reset both settings follow config again (= on), so pinning
    // again turns them *off*.
    run_command(&mut harness, "Toggle Occurrence Highlight (Current Buffer)");
    harness.assert_screen_contains("Occurrence highlight disabled (this buffer)");
    run_command(
        &mut harness,
        "Toggle Current Line Highlight (Current Buffer)",
    );
    harness.assert_screen_contains("Current line highlight disabled (this buffer)");
}

/// The unsuffixed "Toggle Line Wrap" expresses global intent: it drops the
/// *active* split's pin, but a pin the user set in another split must survive
/// — same rule the current-line/occurrence global toggles follow, and what
/// the configuration docs promise ("buffers you have pinned keep their
/// choice").
#[test]
fn test_global_line_wrap_preserves_other_splits_pins() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_temp_project_and_config(60, 24, config).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), format!("{}TAILAAA\n", "A".repeat(80))).unwrap();
    std::fs::write(dir.join("b.txt"), format!("{}TAILBBB\n", "B".repeat(80))).unwrap();

    // In the narrow splits below, a wrapped line occupies several visual rows,
    // so "how many rows show this file's letter" distinguishes wrapped (>1)
    // from truncated (=1) without depending on where the wrap points fall.
    fn rows_with(harness: &mut EditorTestHarness, needle: &str) -> usize {
        harness
            .screen_to_string()
            .lines()
            .filter(|l| l.contains(needle))
            .count()
    }

    // Pin wrap ON for a.txt while the global default is off.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.render().unwrap();
    run_command(&mut harness, "Toggle Line Wrap (Current Buffer)");
    harness.assert_screen_contains("TAILAAA");

    // Second split with b.txt; it becomes the active split.
    run_command(&mut harness, "Split Vertical");
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // Global toggle #1 (wrap on) then #2 (wrap off), both run from b's split.
    // b follows the global both times; a's pin must survive both.
    run_command(&mut harness, "Toggle Line Wrap");
    harness.render().unwrap();
    assert!(
        rows_with(&mut harness, "BBB") > 1,
        "b follows the global: wrapped"
    );
    assert!(
        rows_with(&mut harness, "AAA") > 1,
        "a's pin keeps it wrapped"
    );

    run_command(&mut harness, "Toggle Line Wrap");
    harness.render().unwrap();
    assert_eq!(
        rows_with(&mut harness, "BBB"),
        1,
        "b follows the global: truncated"
    );
    // The old behavior cleared every split's pin on the first global toggle,
    // so the second one would unwrap a.txt too and collapse it to one row.
    assert!(
        rows_with(&mut harness, "AAA") > 1,
        "a's pin must survive global toggles"
    );
}

/// VS Code-style target derivation: a toggle writes the most specific config
/// layer that already defines the key. With a project `.fresh/config.json`
/// setting `editor.line_wrap`, "Toggle Line Wrap" must update the *project*
/// file — writing the user layer instead leaves the project value winning on
/// the next launch (the toggle appears dead in that project) while the
/// stranded user-layer entry leaks the change into every other project.
#[test]
fn test_global_toggle_writes_the_layer_that_defines_the_key() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    std::fs::write(project_dir.join("a.txt"), "alpha\n").unwrap();
    let project_config = project_dir.join(".fresh").join("config.json");
    std::fs::create_dir_all(project_config.parent().unwrap()).unwrap();
    std::fs::write(&project_config, r#"{ "editor": { "line_wrap": false } }"#).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    // The harness injects the config rather than resolving layers from disk,
    // so start it the way a real launch would resolve: with the project's
    // line_wrap=false already in effect.
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir.clone())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&project_dir.join("a.txt")).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "Toggle Line Wrap");

    let project_written = std::fs::read_to_string(&project_config).unwrap();
    let project_json: serde_json::Value = serde_json::from_str(&project_written).unwrap();
    assert_eq!(
        project_json.pointer("/editor/line_wrap"),
        Some(&serde_json::json!(true)),
        "the project layer defines line_wrap, so the toggle must update it there:\n{project_written}"
    );

    let user_config = std::fs::read_to_string(dir_context.config_path()).unwrap_or_default();
    let user_json: serde_json::Value =
        serde_json::from_str(&user_config).unwrap_or(serde_json::json!({}));
    assert_eq!(
        user_json.pointer("/editor/line_wrap"),
        None,
        "the user layer must not get a stranded shadowed entry:\n{user_config}"
    );

    // A key no layer defines still lands in the user layer.
    run_command(&mut harness, "Toggle Inlay Hints");
    let user_config = std::fs::read_to_string(dir_context.config_path()).unwrap();
    let user_json: serde_json::Value = serde_json::from_str(&user_config).unwrap();
    assert!(
        user_json.pointer("/editor/enable_inlay_hints").is_some(),
        "an undefined key defaults to the user layer:\n{user_config}"
    );
}

/// The workspace file must not shadow settings whose global toggles persist
/// to the config file. A workspace saved when line wrap was off must not
/// stamp "off" over a config that has since been set to wrapped — the config
/// file is the single source of truth for these.
#[test]
fn test_workspace_does_not_shadow_config_persisted_settings() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, format!("{}TAILAAA\n", "A".repeat(80))).unwrap();

    // Session 1: line wrap off; save the workspace in that state.
    {
        let mut config = Config::default();
        config.editor.line_wrap = false;
        let mut harness =
            EditorTestHarness::with_config_and_working_dir(60, 24, config, project_dir.clone())
                .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_not_contains("TAILAAA");
        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: the user's config now says wrapped. The restored workspace
    // must not stamp session 1's "off" over it.
    {
        let mut config = Config::default();
        config.editor.line_wrap = true;
        let mut harness =
            EditorTestHarness::with_config_and_working_dir(60, 24, config, project_dir.clone())
                .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();
        harness.assert_screen_contains("TAILAAA");
    }
}

/// "Toggle Tab Indicators (Current Buffer)" must flip only the tab arrows —
/// it used to share the master toggle-all with Whitespace Indicators, so
/// "hide the arrows" also killed the space dots despite the command's
/// description promising tab arrows only. The master toggle subsumes a tab
/// pin (all means all), and the tab pin persists across a restart.
#[test]
fn test_tab_indicators_toggle_independent_of_space_dots() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, "\thello\n  world\n").unwrap();

    // Leading-space dots on, so tabs and spaces are independently observable:
    // '→' for the tab, '·' for the leading spaces.
    let mk_config = || {
        let mut c = Config::default();
        c.editor.whitespace_spaces_leading = true;
        c
    };

    // Session 1
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("→");
        harness.assert_screen_contains("·");

        // Tab toggle hides the arrows and leaves the dots alone (the old
        // shared toggle-all would have hidden both).
        run_command(&mut harness, "Toggle Tab Indicators (Current Buffer)");
        harness.assert_screen_not_contains("→");
        harness.assert_screen_contains("·");

        // The master toggle subsumes the tab pin: off hides everything, on
        // brings everything back — including the arrows the pin had hidden.
        run_command(
            &mut harness,
            "Toggle Whitespace Indicators (Current Buffer)",
        );
        harness.assert_screen_not_contains("·");
        run_command(
            &mut harness,
            "Toggle Whitespace Indicators (Current Buffer)",
        );
        harness.assert_screen_contains("→");
        harness.assert_screen_contains("·");

        // Pin the arrows off again and save for the restart check.
        run_command(&mut harness, "Toggle Tab Indicators (Current Buffer)");
        harness.assert_screen_not_contains("→");
        harness.assert_screen_contains("·");
        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: the tab pin survives the restart; the dots still show.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            100,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();
        harness.assert_screen_not_contains("→");
        harness.assert_screen_contains("·");
    }
}

/// The indentation-guide and fold-indicator pins are per (split, buffer),
/// like line numbers and the current-line highlight: with the SAME buffer in
/// two splits, pinning in one split must leave the other split alone. Under
/// the old buffer-scoped pins, both splits changed together.
///
/// Same-glyph-in-both-splits means substring assertions can't tell the
/// splits apart, so the discriminator is the glyph count across the screen.
#[test]
fn test_guide_and_fold_pins_scope_to_the_split() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.rs"), FOLDABLE_SOURCE).unwrap();

    fn glyph_count(harness: &mut EditorTestHarness, glyph: &str) -> usize {
        harness.screen_to_string().matches(glyph).count()
    }

    harness.open_file(&dir.join("a.rs")).unwrap();
    harness.render().unwrap();

    // Same buffer in two splits; the new (right) split is active.
    run_command(&mut harness, "Split Vertical");
    harness.render().unwrap();
    let arrows_both_splits = glyph_count(&mut harness, EXPANDED_FOLD);
    assert!(
        arrows_both_splits >= 2,
        "both splits should show a fold arrow for fn main"
    );

    // Hide fold indicators in the active split only: the count drops but must
    // not reach zero — the other split keeps its arrows. The old buffer-wide
    // pin zeroed it.
    run_command(&mut harness, "Toggle Folding Indicators (Current Buffer)");
    harness.render().unwrap();
    let arrows_after = glyph_count(&mut harness, EXPANDED_FOLD);
    assert!(
        arrows_after < arrows_both_splits && arrows_after > 0,
        "hiding fold arrows in one split must leave the other split's arrows \
         ({arrows_both_splits} before, {arrows_after} after)"
    );

    // Guides on in the active split only, then also in the other split: the
    // second pin must add more guide glyphs. The old buffer-wide pin lit both
    // splits on the first toggle, so the second changed nothing.
    run_command(&mut harness, "Toggle Indentation Guides (Current Buffer)");
    harness.render().unwrap();
    let guides_one_split = glyph_count(&mut harness, "▏");
    assert!(guides_one_split > 0, "active split should now draw guides");

    run_command(&mut harness, "Next Split");
    harness.render().unwrap();
    run_command(&mut harness, "Toggle Indentation Guides (Current Buffer)");
    harness.render().unwrap();
    let guides_both_splits = glyph_count(&mut harness, "▏");
    assert!(
        guides_both_splits > guides_one_split,
        "pinning guides in the second split must add glyphs \
         ({guides_one_split} then {guides_both_splits})"
    );
}
