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
            scrollable: None,
            request_id: None,
        })
        .unwrap();
    harness.render().unwrap();
}

/// **A tab-switch slide froze the incoming pane at its first frame, and the
/// screen kept that frame after the slide.** `SlideIn` took its snapshot once
/// and shifted it for 260ms; when it finished, the frame it finished on was
/// its own composite and nothing asked for another. Content that changed
/// under the slide — a panel repainting right after the switch — was hidden
/// until the reader's next keystroke.
///
/// The slide runs on the wall clock, so the mid-slide half of this is pinned
/// in `view::animation::tests::slide_in_retakes_the_incoming_snapshot_every_apply`,
/// where `elapsed` is a parameter. What this end-to-end test asserts is the
/// half a harness can observe without racing a 260ms window on a slow
/// runner: once the slide is over, the pane shows the content it has, not
/// the content it had when the slide began. `harness.render()` after the
/// slide is the settle frame the runner now owes (see finding 34).
#[test]
fn a_pane_repainted_mid_slide_settles_on_its_new_content() {
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
    // The slide's first frame, with the pane still holding OLD CONTENT.
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

    // Drive frames until the slide retires (bounded: 260ms of slide against
    // up to ~6s of budget, so a slow runner cannot time this out).
    let mut ran_out = true;
    for _ in 0..600 {
        harness.render().unwrap();
        if !harness.editor().active_window().animations.is_active() {
            ran_out = false;
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    assert!(!ran_out, "the slide never finished");

    // The settle frame.
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("NEW CONTENT") && !screen.contains("OLD CONTENT"),
        "after the slide the pane still shows the content it had when the \
         slide began:\n{screen}"
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
