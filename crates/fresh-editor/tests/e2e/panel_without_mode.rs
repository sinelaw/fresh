//! A floating panel mounted without a `mode` has no keymap of its own, and
//! does not borrow the window's editor mode (sinelaw/fresh#3386).
//!
//! The window's editor mode belongs to the buffer — vi_mode keeps
//! "vi-normal" there — so a panel that resolved the keys its focused control
//! passes against it handed them to whichever plugin held that slot. The
//! `test_panel_without_mode.ts` plugin sets a window mode that binds Esc and
//! mounts a mode-less panel over it: Esc must reach the panel's own default
//! (cancel), not the window mode's binding.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

#[test]
fn a_panel_without_a_mode_ignores_the_windows_editor_mode() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(
        plugins_dir.join("test_panel_without_mode.ts"),
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/plugins/test_panel_without_mode.ts"
        )),
    )
    .unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(100, 32, Default::default(), project_root)
            .unwrap();
    h.render().unwrap();

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("TestNoMode: Mount").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("NoModeButton"))
        .unwrap();

    // The button passes Esc. Either the panel's default cancels it, or the
    // window mode's binding takes the key and the panel stays up.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("PANEL-CANCELLED") || s.contains("WINDOW-MODE-TOOK-ESC")
    })
    .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("PANEL-CANCELLED") && !screen.contains("NoModeButton"),
        "Esc on a mode-less panel is the panel's cancel, not the window \
         mode's binding.\nScreen:\n{screen}"
    );
}
