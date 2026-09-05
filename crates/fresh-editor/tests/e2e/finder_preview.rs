//! End-to-end tests for previewing a *result* — the browse a finder does
//! as its selection moves down a list of locations (issue #3196).
//!
//! The finders used to compose a `*Preview*` buffer: eleven sliced lines in
//! a virtual buffer, with no language, no gutter and nothing to scroll. They
//! now show the file itself, as the editor's single preview tab — the File
//! Explorer's single-click behaviour, addressed at a split the caller names
//! (`Editor::preview_file_in_split`) and dropped again when the browse ends
//! without a choice (`Editor::dismiss_preview`).
//!
//! `preview_tabs.rs` covers the explorer's own gestures and the preview
//! invariants they share. What is specific here — and what these tests pin —
//! is that the browse names its split, does not move focus, does not
//! accumulate tabs, is not gated by the *explorer's* setting, and never
//! raises a dialog over a live result list.

use crate::common::harness::EditorTestHarness;
use fresh::model::event::LeafId;
use std::fs;
use std::path::{Path, PathBuf};

/// The tab bar sits on screen row 1 in the default harness layout.
const TAB_BAR_ROW: u16 = 1;

/// A harness over a temp project holding `files` (each file's content is
/// its own name), with `first` opened as a normal, permanent tab — the
/// buffer the user is reading when they start searching.
fn setup(files: &[&str], first: &str) -> (EditorTestHarness, PathBuf) {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    for name in files {
        fs::write(project.join(name), format!("{name} content\n")).unwrap();
    }
    harness.open_file(&project.join(first)).unwrap();
    harness.render().unwrap();
    (harness, project)
}

fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(TAB_BAR_ROW)
}

fn active_split(harness: &EditorTestHarness) -> LeafId {
    harness.editor().get_active_split()
}

/// Preview `name` in the split the "search" was started from — here, the
/// one that is active — exactly as a finder does on each selection change.
fn preview(harness: &mut EditorTestHarness, project: &Path, name: &str) {
    let split = active_split(harness);
    let _ =
        harness
            .editor_mut()
            .preview_file_in_split(&project.join(name), split, Some(1), Some(1));
    harness.render().unwrap();
}

/// Walking a result list must leave one ephemeral tab, not one tab per
/// file walked past. This is the whole complaint behind the old
/// `openFileInSplit`-per-item panel navigation.
#[test]
fn browsing_results_leaves_a_single_preview_tab() {
    let (mut harness, project) = setup(&["start.txt", "a.txt", "b.txt", "c.txt"], "start.txt");

    preview(&mut harness, &project, "a.txt");
    preview(&mut harness, &project, "b.txt");
    preview(&mut harness, &project, "c.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("c.txt") && row.contains("(preview)"),
        "the last result browsed should be the preview tab; got:\n{row}"
    );
    assert!(
        !row.contains("a.txt") && !row.contains("b.txt"),
        "results browsed past must not accumulate as tabs; got:\n{row}"
    );
    assert!(
        row.contains("start.txt"),
        "the buffer the user was reading stays open; got:\n{row}"
    );
    assert_eq!(
        row.matches("(preview)").count(),
        1,
        "at most one preview exists editor-wide; got:\n{row}"
    );
}

/// A browse is not a focus change. The finder's own surface — a prompt, or
/// a panel in another split — must keep the keys while the preview lands
/// somewhere else, which is also what keeps the preview from being
/// promoted on every step ("walking away is commitment").
#[test]
fn previewing_does_not_move_the_active_split() {
    let (mut harness, project) = setup(&["start.txt", "a.txt"], "start.txt");
    let before = active_split(&harness);

    preview(&mut harness, &project, "a.txt");

    assert_eq!(
        active_split(&harness),
        before,
        "previewing must leave the active split where the user is"
    );
    assert!(
        harness.editor().active_window().current_preview().is_some(),
        "the preview should still be a preview, not promoted by a focus move"
    );
}

/// Cancelling the search puts the user back where they were: the preview
/// tab goes, and the split shows what it showed before.
#[test]
fn dismissing_a_preview_restores_the_previous_buffer() {
    let (mut harness, project) = setup(&["start.txt", "a.txt"], "start.txt");

    preview(&mut harness, &project, "a.txt");
    assert!(tab_bar(&harness).contains("a.txt"));

    harness.editor_mut().dismiss_preview();
    harness.render().unwrap();

    let row = tab_bar(&harness);
    assert!(
        !row.contains("a.txt") && !row.contains("(preview)"),
        "a dismissed preview leaves no tab behind; got:\n{row}"
    );
    assert!(
        row.contains("start.txt"),
        "the split goes back to the buffer the search started from; got:\n{row}"
    );
    assert!(
        harness.editor().active_window().current_preview().is_none(),
        "no preview remains after dismissal"
    );
}

/// A result in a file the user already had open must not turn their tab
/// into an ephemeral one — and cancelling the search must not close it.
#[test]
fn a_file_the_user_already_had_open_is_never_demoted() {
    let (mut harness, project) = setup(&["start.txt", "a.txt"], "start.txt");
    harness.open_file(&project.join("a.txt")).unwrap();
    harness.render().unwrap();

    preview(&mut harness, &project, "a.txt");

    assert!(
        harness.editor().active_window().current_preview().is_none(),
        "switching to an already-open file must not make it the preview"
    );

    harness.editor_mut().dismiss_preview();
    harness.render().unwrap();

    let row = tab_bar(&harness);
    assert!(
        row.contains("a.txt"),
        "cancelling a search must not close a tab the user opened; got:\n{row}"
    );
}

/// `file_explorer.preview_tabs` is the File Explorer's setting. A finder's
/// result preview shares the explorer's *mechanism*, not its policy — with
/// the setting off, the explorer stops previewing and result browsing is
/// unaffected.
#[test]
fn the_explorer_setting_does_not_reach_the_result_preview() {
    let (mut harness, project) = setup(&["start.txt", "a.txt"], "start.txt");
    harness.editor_mut().config_mut().file_explorer.preview_tabs = false;

    preview(&mut harness, &project, "a.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("a.txt") && row.contains("(preview)"),
        "result previews are not gated by the explorer's setting; got:\n{row}"
    );
}

/// A browse walks over whatever the search matched, including files that
/// cannot be loaded without asking the user something. Opening a large
/// non-UTF-8 file deliberately raises a confirmation dialog — and a dialog
/// over a live result list is exactly what must not happen. The preview
/// reports the failure to its caller (which skips it) and paints nothing.
#[test]
fn a_file_that_would_prompt_is_skipped_not_asked_about() {
    let mut harness = EditorTestHarness::with_temp_project_and_config(
        120,
        40,
        fresh::config::Config {
            editor: fresh::config::EditorConfig {
                // Anything past this is "large", so the GBK file below
                // takes the path that asks before loading.
                large_file_threshold_bytes: 500,
                ..Default::default()
            },
            ..Default::default()
        },
    )
    .unwrap();
    let project = harness.project_dir().unwrap();
    fs::write(project.join("start.txt"), "start\n").unwrap();
    // ~540 bytes of GBK — an encoding that cannot be resynchronized, so
    // loading it needs the whole file and therefore the user's say-so.
    let mut gbk = Vec::new();
    for _ in 0..60 {
        gbk.extend_from_slice(&[0xC4, 0xE3, 0xBA, 0xC3, 0xCA, 0xC0, 0xBD, 0xE7, 0x0A]);
    }
    fs::write(project.join("big_gbk.txt"), &gbk).unwrap();
    harness.open_file(&project.join("start.txt")).unwrap();
    harness.render().unwrap();

    let split = active_split(&harness);
    let outcome = harness.editor_mut().preview_file_in_split(
        &project.join("big_gbk.txt"),
        split,
        Some(1),
        None,
    );
    harness.render().unwrap();

    assert!(
        outcome.is_err(),
        "the preview must report that it could not show this file"
    );
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("requires full load"),
        "a browse must never raise the encoding dialog; screen:\n{screen}"
    );
    assert!(
        harness.editor().active_window().current_preview().is_none(),
        "a skipped preview leaves no preview tab"
    );
    assert!(
        tab_bar(&harness).contains("start.txt"),
        "the user stays where they were"
    );
}
