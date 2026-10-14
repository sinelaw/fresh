//! A window's own scratch buffer must follow the `editor.*` display settings
//! (issue #3426).
//!
//! `Editor::build_fresh_layout_if_needed` and `Window::seed_initial_layout`
//! build a window's initial `SplitViewState` with
//! `SplitViewState::with_buffer`, which goes through `BufferViewState::new` —
//! whose display flags are hard-coded (`highlight_current_line: true`,
//! `show_line_numbers: true`, no rulers) rather than read from the config.
//! Nothing stamped them, so the `[No Name]` buffer of any window seeded that
//! way ignored the user's settings: the same defect #3426 reported for a
//! buffer restored by hot exit (covered in `hot_exit_flows`), one path over.
//!
//! The boot-time seed in `Editor::new` always stamped, so this only ever
//! showed up in a window created later — which is what this drives.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

#[test]
fn new_window_scratch_buffer_follows_display_config() {
    // Each default flipped away from the value `BufferViewState::new`
    // hard-codes, so a view state that missed the stamp is unmistakable.
    let mut config = Config::default();
    config.editor.highlight_current_line = false;
    config.editor.line_numbers = false;
    config.editor.rulers = vec![80];

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();

    // A window at a root with no saved workspace: the restore never runs, so
    // the fresh layout's seed is what the user sees.
    let extra_root = tempfile::tempdir().unwrap();
    let second = harness
        .editor_mut()
        .create_window_at(extra_root.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second);
    harness.render().unwrap();

    let window = harness.editor().active_window();
    let (mgr, view_states) = window.buffers.splits().unwrap();
    let view = view_states
        .get(&mgr.active_split())
        .unwrap()
        .buffer_tab_state();
    assert!(
        !view.highlight_current_line,
        "a new window's scratch buffer must honour highlight_current_line: false"
    );
    assert!(
        !view.show_line_numbers,
        "a new window's scratch buffer must honour line_numbers: false"
    );
    assert_eq!(
        view.rulers,
        vec![80],
        "a new window's scratch buffer must carry the configured rulers"
    );
}
