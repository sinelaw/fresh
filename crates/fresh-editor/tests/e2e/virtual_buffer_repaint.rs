//! Two things a widget-panel repaint exposed about the host, neither of
//! which is about widget panels.
//!
//! Both were found on the welcome screen: a page opened behind a file,
//! brought forward with `Ctrl+PageDown` after the terminal was resized,
//! catching up to its new width ~100ms into the tab-switch slide. The slide
//! kept showing the first frame's cells for its whole duration, and then
//! the screen kept showing the slide's last frame until the next keystroke.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use fresh_core::api::PluginCommand;
use fresh_core::text_property::TextPropertyEntry;
use std::collections::HashMap;

fn entry(text: &str) -> TextPropertyEntry {
    TextPropertyEntry {
        text: text.to_string(),
        properties: HashMap::new(),
        style: None,
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

fn create(harness: &mut EditorTestHarness, text: &str, highlight_current_line: Option<bool>) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CreateVirtualBufferWithContent {
            name: "*Panel*".to_string(),
            mode: "panel-test".to_string(),
            read_only: true,
            entries: vec![entry(text)],
            show_line_numbers: false,
            show_cursors: true,
            editing_disabled: true,
            hidden_from_tabs: false,
            background: false,
            highlight_current_line,
            initial_cursor_line: None,
            indentation_guide: None,
            request_id: None,
        })
        .unwrap();
    harness.render().unwrap();
}

/// **A tab-switch slide froze the incoming pane at its first frame.**
/// `SlideIn` took its "after" snapshot once, on its first apply, and shifted
/// that for the slide's 260ms. Content that changed during the slide — here
/// a panel repainted right after the switch — was invisible until the slide
/// ended, and (see the design note's finding 34) beyond. The snapshot is now
/// retaken from the freshly painted frame on every apply, so a slide shows
/// the pane as it is, shifted.
///
/// The animation runs on the wall clock, so this drives frames for a bounded
/// stretch and asks whether the new content was ever visible *while the
/// slide was still running*. With the old capture it never is: every frame
/// of the slide is the pre-repaint cells.
#[test]
fn a_pane_repainted_mid_slide_shows_its_new_content_during_the_slide() {
    let mut config = Config::default();
    config.editor.animations = true;
    let mut harness = EditorTestHarness::with_config(60, 12, config).unwrap();

    create(&mut harness, "OLD CONTENT\n", None);
    let panel = harness.editor().active_buffer_id();
    create(&mut harness, "OTHER TAB\n", None);
    assert_ne!(harness.editor().active_buffer_id(), panel);

    // Switch back: this is what starts the slide. The harness seeds a
    // `[No Name]` tab first, so the order is seed, panel, other — from
    // `other`, *previous* is the panel.
    harness.editor_mut().prev_buffer();
    assert_eq!(harness.editor().active_buffer_id(), panel);
    // The slide's first frame. This is the frame the old capture took its
    // one and only snapshot from — the pane as it was *before* the repaint
    // below. That is the order the real page saw too: a plugin's catch-up
    // lands a few frames after the switch, not before it.
    harness.render().unwrap();
    assert!(
        harness.editor().active_window().animations.is_active(),
        "precondition: the tab switch started a slide"
    );
    // ...and then the pane's content changes under the slide.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetVirtualBufferContent {
            buffer_id: panel,
            entries: vec![entry("NEW CONTENT\n")],
        })
        .unwrap();

    let mut seen_new_while_sliding = false;
    for _ in 0..60 {
        harness.render().unwrap();
        if !harness.editor().active_window().animations.is_active() {
            break;
        }
        if harness.screen_to_string().contains("NEW CONTENT") {
            seen_new_while_sliding = true;
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    assert!(
        seen_new_while_sliding,
        "the slide kept showing the pane's first-frame cells; the repaint \
         made during it never reached the screen while it ran"
    );
}

/// `highlightCurrentLine: false` on `createVirtualBuffer` leaves the
/// caret's row unlit. The caret's row means nothing on a page laid out by
/// widgets, and a lit band across a centred wordmark reads as a selection.
#[test]
fn a_virtual_buffer_can_opt_out_of_the_current_line_highlight() {
    let mut config = Config::default();
    config.editor.highlight_current_line = true;

    // Control: with no opinion the buffer follows the setting, so the
    // caret's row (the first) is painted differently from the row below.
    let mut harness = EditorTestHarness::with_config(60, 12, config.clone()).unwrap();
    create(&mut harness, "first\nsecond\n", None);
    let lit = harness.get_cell_style(20, 2).map(|s| s.bg);
    let plain = harness.get_cell_style(20, 3).map(|s| s.bg);
    assert_ne!(
        lit, plain,
        "control: with the setting on and no override, the cursor row should \
         carry the highlight background"
    );

    // Opted out: the two rows paint alike.
    let mut harness = EditorTestHarness::with_config(60, 12, config).unwrap();
    create(&mut harness, "first\nsecond\n", Some(false));
    let cursor_row = harness.get_cell_style(20, 2).map(|s| s.bg);
    let other_row = harness.get_cell_style(20, 3).map(|s| s.bg);
    assert_eq!(
        cursor_row, other_row,
        "highlightCurrentLine: false still lit the cursor row"
    );
}
