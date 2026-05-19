//! Issue #2035 reproducer: the Orchestrator-style preview pane (any
//! `windowEmbed` widget mounted inside a floating panel) used to skip
//! virtual buffer GROUPS. Plugins like `git_log` build their UI as a
//! `createBufferGroup` of virtual panels — so previewing a session
//! whose active tab was `*Git Log*` rendered the previous file
//! buffer instead of the log/detail panels.
//!
//! Root cause was a single line in
//! `crates/fresh-editor/src/app/render.rs::render_session_preview_into_rect`:
//! it passed a freshly-allocated empty `grouped_subtrees` map to
//! `SplitRenderer::render_content`. When the split renderer asked
//! "what's behind the `active_group_tab` of this split?", the empty
//! map answered "nothing", and the renderer fell back to painting
//! the split's underlying (pre-group) buffer.
//!
//! This test exercises the same code path the Orchestrator's
//! `windowEmbed({windowId: s.id})` hits: a floating widget panel
//! whose contents is a single `windowEmbed` aimed at the current
//! window. The float is mounted at 100%×100% so the embed *is* the
//! entire visible interior — no leakage from the underlying editor
//! render. If the marker text from the buffer group's panels lands
//! inside the float, the preview path resolved the group correctly.
//!
//! Per CONTRIBUTING.md §2 the assertion is on rendered output only.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const LEFT_MARKER: &str = "ISSUE2035-LEFT-MARKER";
const RIGHT_MARKER: &str = "ISSUE2035-RIGHT-MARKER";

/// Install the issue-2035 test plugin into the project's plugin
/// directory. Mirrors the pattern in `e2e::buffer_groups`.
fn install_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);

    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_issue_2035_preview_embed.ts"
    ));
    let dst = plugins_dir.join("test_issue_2035_preview_embed.ts");
    fs::write(&dst, SRC).unwrap_or_else(|e| {
        panic!(
            "Failed to write test_issue_2035_preview_embed.ts to {:?}: {}",
            dst, e
        )
    });
}

/// Drive a registered command via the command palette and wait for
/// the plugin's status-message acknowledgement to appear.
fn run_command_and_wait(harness: &mut EditorTestHarness, name: &str, ack: &str) {
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
    harness
        .wait_until(|h| h.screen_to_string().contains(ack))
        .unwrap();
}

#[test]
fn floating_window_embed_renders_buffer_group_panels() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    install_plugin(&project_root);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Step 1: open a 2-panel buffer group in the active (and only)
    // window. After this, both markers must be visible somewhere on
    // screen — the panels render in the active split.
    run_command_and_wait(&mut harness, "TestPrev: Setup", "TestPrev: SETUP_DONE");
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(LEFT_MARKER) && s.contains(RIGHT_MARKER)
        })
        .unwrap();

    // Step 2: mount a 100%×100% floating widget whose content is a
    // single `windowEmbed` pointing at the current window. The
    // floating panel clears the entire visible area inside its
    // border before painting, so any marker visible AFTER mount is
    // a marker drawn by the embed — not leakage from the editor
    // underneath.
    run_command_and_wait(&mut harness, "TestPrev: Mount", "TestPrev: MOUNTED");

    // Allow a render frame for the floating panel + embed to paint.
    harness.render().unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // The float draws a box border (`┌─…─┐` top, `└─…─┘` bottom). The
    // rows strictly between those two borders are the float's
    // interior — any text on them was painted by the `windowEmbed`,
    // not by the editor underneath (the underlying render at those
    // rows was cleared before the embed paint).
    //
    // We must NOT use the full-screen text for the marker check:
    // the active window still renders its buffer group in the
    // narrow strips above/below the float, so the markers always
    // appear *somewhere* on screen. The bug is that they don't
    // appear *inside the float*.
    let lines: Vec<&str> = screen.lines().collect();
    let top_border = lines
        .iter()
        .position(|l| l.contains('┌') && l.contains('─'))
        .expect("float top border must be on screen after mount");
    let bottom_border = lines
        .iter()
        .rposition(|l| l.contains('└') && l.contains('─'))
        .expect("float bottom border must be on screen after mount");
    assert!(
        bottom_border > top_border + 1,
        "float must have an interior between its borders \
         (top={top_border}, bottom={bottom_border})"
    );
    let interior: String = lines[top_border + 1..bottom_border].join("\n");

    assert!(
        interior.contains(LEFT_MARKER),
        "issue #2035: LEFT panel marker must render INSIDE the \
         floating `windowEmbed` (the embed should resolve the active \
         window's buffer group). Without the fix, the embed renders \
         the underlying pre-group buffer and the marker is absent \
         from the float's interior.\nfloat interior:\n{interior}\n\
         full screen:\n{screen}"
    );
    assert!(
        interior.contains(RIGHT_MARKER),
        "issue #2035: RIGHT panel marker must render INSIDE the \
         floating `windowEmbed`.\nfloat interior:\n{interior}\n\
         full screen:\n{screen}"
    );
}
