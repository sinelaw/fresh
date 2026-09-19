//! The sidebar focus cycle on its default keys (sinelaw/fresh#3326, A).
//!
//! `focus_next_sidebar_section` was the only keyboard route into a plugin
//! section and no shipped keymap bound it. It is Alt+Shift+N now, with
//! Alt+Shift+P running the cycle backwards, bound in every context the cycle
//! passes through so a press inside the explorer or a section continues the
//! cycle instead of restarting it from the editor.
//!
//! Drives keys only and asserts on the rendered column. The explorer's header
//! carries the focus hint (`File Explorer (Ctrl+E)`) only while something
//! *else* has the keyboard. The section is a tree, whose selected row wears
//! the `▌` mark whether or not it is focused, so "the section has the
//! keyboard" is probed: Down moves the mark only when it does.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::sidebar::{explorer_focused, explorer_hinted, screen_has, wait_for};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::Path;

const PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_sidebar_tree.ts"
));

fn install_plugin(project: &Path) {
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_sidebar_tree.ts"), PLUGIN).unwrap();
}

/// The section's row `text` carries the selection mark.
fn marked(h: &EditorTestHarness, text: &str) -> bool {
    h.screen_to_string()
        .lines()
        .any(|l| l.contains(text) && l.contains('▌'))
}

/// Whether the section has the keyboard: Down moves its mark from `alpha`
/// to `beta` only then. Leaves the mark where it found it.
fn section_takes_keys(h: &mut EditorTestHarness) -> bool {
    h.wait_until(|h| marked(h, "alpha")).unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.render().unwrap();
    let moved = marked(h, "beta");
    if moved {
        h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| marked(h, "alpha")).unwrap();
    }
    moved
}

fn next(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('N'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    h.render().unwrap();
    h.render().unwrap();
}

fn prev(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('P'), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    h.render().unwrap();
    h.render().unwrap();
}

#[test]
fn alt_shift_n_and_p_cycle_the_sidebar_from_any_context() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.txt"), "one\ntwo\nthree\n").unwrap();
    install_plugin(&project);

    let mut h = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    // A bare directory shows the explorer by default.
    h.editor_mut()
        .restore_active_window_on_launch(false)
        .unwrap();
    wait_for(&mut h, "the explorer", |h| screen_has(h, "File Explorer"));

    h.run_palette_command("SidebarTree: Mount").unwrap();
    wait_for(&mut h, "the section", |h| screen_has(h, "alpha"));
    // The editor has the keyboard to begin with.
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    wait_for(&mut h, "the explorer focused", explorer_focused);
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    wait_for(&mut h, "the explorer hinted", explorer_hinted);
    assert!(
        !section_takes_keys(&mut h),
        "the editor has the keys to start"
    );

    // Forward: editor → explorer → section → editor.
    next(&mut h);
    wait_for(&mut h, "explorer after one Alt+Shift+N", explorer_focused);
    next(&mut h);
    wait_for(&mut h, "the explorer hinted", explorer_hinted);
    assert!(
        section_takes_keys(&mut h),
        "section after two Alt+Shift+N\n{}",
        h.screen_to_string()
    );
    next(&mut h);
    wait_for(&mut h, "the explorer hinted", explorer_hinted);
    assert!(
        !section_takes_keys(&mut h),
        "editor after three Alt+Shift+N\n{}",
        h.screen_to_string()
    );

    // Backward: editor → section → explorer → editor.
    prev(&mut h);
    wait_for(&mut h, "the explorer hinted", explorer_hinted);
    assert!(
        section_takes_keys(&mut h),
        "section after one Alt+Shift+P\n{}",
        h.screen_to_string()
    );
    prev(&mut h);
    wait_for(&mut h, "explorer after two Alt+Shift+P", explorer_focused);
    prev(&mut h);
    wait_for(&mut h, "the explorer hinted", explorer_hinted);
    assert!(
        !section_takes_keys(&mut h),
        "editor after three Alt+Shift+P\n{}",
        h.screen_to_string()
    );

    // From a hidden sidebar the first step shows it and focuses the explorer.
    h.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    wait_for(&mut h, "the column hidden", |h| {
        !screen_has(h, "File Explorer")
    });
    next(&mut h);
    wait_for(&mut h, "explorer from hidden", explorer_focused);
}
