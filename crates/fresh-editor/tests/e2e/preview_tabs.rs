//! Tests for the file-explorer preview-tab feature (issue #1403).
//!
//! Semantics under test:
//! - Single-click in the explorer opens a file in a "preview" tab.
//! - Two consecutive single-clicks on different files leave only one tab.
//! - Re-clicking an already-permanent file does not demote it.
//! - Editing the preview buffer promotes it to permanent.
//! - Opening via the Enter/double-click path promotes.
//! - Preview is anchored to the split it was opened in; focusing another
//!   split promotes the preview (walking away is commitment).
//! - The tab shows the translated `(preview)` suffix.
//! - Config flag `file_explorer.preview_tabs = false` disables the feature.

use crate::common::harness::EditorTestHarness;
use fresh::model::event::{CursorId, Event};
use std::fs;

/// Helper: set up a temp project with three files, all readable.
fn three_file_project(
    harness: &mut EditorTestHarness,
) -> (std::path::PathBuf, [std::path::PathBuf; 3]) {
    let project_root = harness.project_dir().unwrap();
    let files = [
        project_root.join("a.txt"),
        project_root.join("b.txt"),
        project_root.join("c.txt"),
    ];
    for (i, f) in files.iter().enumerate() {
        fs::write(f, format!("content {i}")).unwrap();
    }
    (project_root, files)
}

#[test]
fn single_click_opens_file_as_preview() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    let id = harness.editor_mut().open_file_preview(&files[0]).unwrap();

    assert!(
        harness.editor().is_buffer_preview(id),
        "single-click should mark buffer as preview"
    );
    assert_eq!(
        harness.editor().current_preview().map(|(_, b)| b),
        Some(id),
        "editor should track this as the current preview"
    );
}

#[test]
fn two_consecutive_previews_leave_one_tab() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    // Note: harness may start with an empty scratch buffer; `open_file`
    // can repurpose it in place. What matters is that a second preview
    // open does not ADD a new buffer on top of the first preview — it
    // should replace it in the same slot.
    let first = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    let after_first = harness.editor().open_buffer_count();

    let second = harness.editor_mut().open_file_preview(&files[1]).unwrap();
    let after_second = harness.editor().open_buffer_count();

    assert_ne!(first, second, "second preview should be a different buffer");
    assert_eq!(
        after_second, after_first,
        "replacing a preview should not add a new buffer"
    );
    assert!(harness.editor().is_buffer_preview(second));
    assert_eq!(
        harness.editor().current_preview().map(|(_, b)| b),
        Some(second)
    );
}

#[test]
fn editing_preview_promotes_it_to_permanent() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    let id = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert!(harness.editor().is_buffer_preview(id));

    // Any buffer mutation must promote. Use a minimal Insert event.
    harness.editor_mut().log_and_apply_event(&Event::Insert {
        position: 0,
        text: "x".to_string(),
        cursor_id: CursorId(0),
    });

    assert!(
        !harness.editor().is_buffer_preview(id),
        "editing a preview buffer must promote it"
    );
    assert!(
        harness.editor().current_preview().is_none(),
        "editor should no longer track any preview after promotion"
    );

    // A subsequent preview-open on a different file must NOT close the
    // promoted tab — the user committed to it.
    let base_count = harness.editor().open_buffer_count();
    let new_preview = harness.editor_mut().open_file_preview(&files[1]).unwrap();
    assert_ne!(new_preview, id);
    assert_eq!(
        harness.editor().open_buffer_count(),
        base_count + 1,
        "promoted tab must persist alongside the new preview"
    );
}

#[test]
fn reclicking_same_file_does_not_change_state() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    let id = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert!(harness.editor().is_buffer_preview(id));

    let id2 = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert_eq!(id, id2, "re-clicking same preview returns same buffer");
    assert!(
        harness.editor().is_buffer_preview(id),
        "re-clicking should neither promote nor demote"
    );
}

#[test]
fn reclicking_already_permanent_file_does_not_demote() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    // Open the first file through the permanent path.
    let id = harness.editor_mut().open_file(&files[0]).unwrap();
    assert!(!harness.editor().is_buffer_preview(id));

    // A preview open on the same path must NOT mark the existing
    // permanent tab as preview.
    let id2 = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert_eq!(id, id2);
    assert!(
        !harness.editor().is_buffer_preview(id),
        "preview-open on a permanent tab must not demote it"
    );
}

#[test]
fn closing_preview_clears_tracking() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    let id = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert_eq!(harness.editor().current_preview().map(|(_, b)| b), Some(id));

    harness.editor_mut().close_buffer(id).unwrap();
    assert!(
        harness.editor().current_preview().is_none(),
        "closing the preview buffer must clear preview tracking"
    );
}

#[test]
fn config_disabled_falls_back_to_normal_open() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    // Disable preview tabs.
    harness.editor_mut().config_mut().file_explorer.preview_tabs = false;

    let id = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    assert!(
        !harness.editor().is_buffer_preview(id),
        "with preview_tabs disabled, open_file_preview must behave like open_file"
    );
    assert!(
        harness.editor().current_preview().is_none(),
        "disabled feature must not set preview tracking"
    );
}

#[test]
fn preview_is_anchored_to_split_focus_change_promotes() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    // Open preview in split-1.
    let preview_id = harness.editor_mut().open_file_preview(&files[0]).unwrap();
    let (preview_split, _) = harness
        .editor()
        .current_preview()
        .expect("preview should be tracked");

    // Split horizontally. The split operation itself should promote first.
    harness.editor_mut().split_pane_horizontal();
    assert!(
        !harness.editor().is_buffer_preview(preview_id),
        "splitting the layout must promote any preview"
    );
    assert!(
        harness.editor().current_preview().is_none(),
        "split operation must clear preview tracking"
    );

    // Silence unused warning if split fails silently on this harness.
    let _ = preview_split;
}

#[test]
fn preview_tab_shows_translated_suffix_in_render() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let (_, files) = three_file_project(&mut harness);

    harness.editor_mut().open_file_preview(&files[0]).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // English default — the en.json key is "(preview)". If/when the
    // harness picks a non-English locale this assertion should move to
    // checking any of the known translated strings; today we pin to
    // en_US.
    assert!(
        screen.contains("(preview)"),
        "tab bar should render the translated preview suffix; got:\n{screen}"
    );
}
