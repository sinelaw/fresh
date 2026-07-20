//! Feature test for the **native modal-frame (dialog shell) chrome** wrapped
//! around a declarative `WidgetSpec` floating panel.
//!
//! ## What the feature is
//! The popup CONTENT stays a `WidgetSpec` tree (existing system), but the
//! popup SHELL — its title bar and `[×]` close button — is native chrome the
//! host draws AROUND the content (`MountFloatingWidget { title, closable }`),
//! not something a plugin fakes with a `labeledSection` border inside the
//! spec. See `FloatingWidgetState::{title, closable, close_button_rect}`.
//!
//! ## What this test asserts (on rendered output only — no model inspection)
//!   1. A centered floating panel mounted with `title` + `closable` renders
//!      its title bar text AND a `[×]` close button into the frame.
//!   2. Clicking the `[×]` dismisses the panel (title + button vanish) — the
//!      same dismiss path as Cancel.
//!   3. Re-mounting and pressing Esc also dismisses it.
//!
//! Without the render change, no title bar / `[×]` is drawn, so step 1 fails —
//! the assertion catches the feature's absence.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh_core::api::{PluginCommand, WidgetSpec};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// A distinctive title so `assert_screen_contains` can't collide with other
/// chrome text on screen.
const FRAME_TITLE: &str = "ZZFrameTitleZZ";

/// Minimal content spec — only the native shell (title + `[×]`) is under test,
/// and that chrome is independent of the spec body.
fn content_spec() -> WidgetSpec {
    WidgetSpec::Spacer {
        cols: 1,
        flex: false,
        key: None,
    }
}

/// Mount a centered floating panel carrying the native modal-frame chrome.
fn mount_titled_closable(harness: &mut EditorTestHarness) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
            spec: content_spec(),
            width_pct: 60,
            height_pct: 40,
            as_dock: false,
            focus_marker: false,
            title: Some(FRAME_TITLE.to_string()),
            closable: true,
        })
        .unwrap();
}

#[test]
fn floating_modal_renders_native_title_bar_and_close_button() {
    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    // Keep clipboard interaction internal so the test stays host-isolated.
    harness.editor_mut().set_clipboard_for_test(String::new());
    harness.tick_and_render().unwrap();

    // The title bar and `[×]` are host chrome, not part of the WidgetSpec.
    mount_titled_closable(&mut harness);
    harness.tick_and_render().unwrap();

    // 1. The native title bar renders its title text, and the native close
    //    button `[×]` renders in the frame. (The bracketed form is specific to
    //    the modal-frame chrome, so it won't collide with the file explorer's
    //    bare `×`.)
    harness.assert_screen_contains(FRAME_TITLE);
    harness.assert_screen_contains("[×]");

    // 2. Clicking the `[×]` dismisses the panel via the same cancel/close path
    //    as Esc. Locate the button on screen and click it — driving the real
    //    mouse hit-test against the recorded `close_button_rect`.
    let (bx, by) = harness
        .find_text_on_screen("[×]")
        .expect("close button `[×]` must be on screen");
    harness.mouse_click(bx, by).unwrap();
    harness.tick_and_render().unwrap();

    // The whole panel is gone: neither its title nor its close button remain.
    harness.assert_screen_not_contains(FRAME_TITLE);
    harness.assert_screen_not_contains("[×]");
}

#[test]
fn floating_modal_close_button_esc_also_dismisses() {
    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    harness.editor_mut().set_clipboard_for_test(String::new());
    harness.tick_and_render().unwrap();

    mount_titled_closable(&mut harness);
    harness.tick_and_render().unwrap();
    harness.assert_screen_contains(FRAME_TITLE);
    harness.assert_screen_contains("[×]");

    // 3. Esc dismisses the modal exactly like the `[×]` click.
    harness
        .send_key(KeyCode::Esc, KeyModifiers::empty())
        .unwrap();
    harness.tick_and_render().unwrap();
    harness.assert_screen_not_contains(FRAME_TITLE);
    harness.assert_screen_not_contains("[×]");
}
