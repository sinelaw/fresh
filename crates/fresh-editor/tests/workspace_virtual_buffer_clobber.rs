//! Regression for issue #2027: quitting with only a virtual buffer
//! visible (Dashboard, plugin scratch buffers) used to clobber the
//! on-disk workspace to empty — silently wiping the user's saved
//! open-file list. The fix lives in
//! `Editor::save_workspace`: refuse to overwrite a workspace that
//! has real content with an all-virtual snapshot.
//!
//! These tests drive the Editor directly rather than through the
//! TUI: we open a real file, save the workspace, then mutate the
//! buffer set and call save_workspace a second time. The on-disk
//! file is what we assert against.

mod common;

use fresh::config::Config;
use fresh::workspace::{get_workspace_path, Workspace};
use std::path::Path;
use tempfile::TempDir;

use common::harness::EditorTestHarness;

fn read_workspace(working_dir: &Path) -> Option<Workspace> {
    let path = get_workspace_path(working_dir).ok()?;
    let bytes = std::fs::read(path).ok()?;
    serde_json::from_slice(&bytes).ok()
}

/// Close the harness's initial seed buffer (the unnamed scratch
/// the editor opens with) so the visible buffer set matches the
/// production scenarios we're emulating: the Dashboard's
/// openDashboard() closes leftover untitled scratch buffers
/// before showing itself, and the "truly empty" scenario by
/// definition has no buffers at all.
fn close_unnamed_buffers(harness: &mut EditorTestHarness) {
    let ids: Vec<_> = harness
        .editor()
        .active_window()
        .buffer_metadata
        .iter()
        .filter_map(|(id, m)| {
            let path_empty = m
                .file_path()
                .map(|p| p.as_os_str().is_empty())
                .unwrap_or(true);
            let is_file_kind = m.file_path().is_some();
            if is_file_kind && path_empty {
                Some(*id)
            } else {
                None
            }
        })
        .collect();
    for id in ids {
        let _ = harness.editor_mut().force_close_buffer(id);
    }
}

#[test]
fn save_with_only_virtual_buffer_does_not_clobber_real_workspace() {
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let project_dir = project_dir.canonicalize().unwrap();

    let real_file = project_dir.join("kept.txt");
    std::fs::write(&real_file, "important user content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_dir.clone(),
    )
    .unwrap();

    // First save: a real file is open. Should land on disk with content.
    harness.open_file(&real_file).unwrap();
    harness.editor_mut().save_workspace().unwrap();

    let initial = read_workspace(&project_dir).expect("first save should write the workspace");
    assert!(
        !initial.has_no_real_content(),
        "sanity: first save must record the open file"
    );

    // Now simulate the bug's trigger: open a virtual buffer
    // (Dashboard), close any leftover unnamed scratch buffers (the
    // dashboard's openDashboard() does this in production), and
    // close the real file. The remaining live state is just the
    // virtual Dashboard buffer — which the serializer strips.
    let _virtual_id = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("Dashboard".to_string(), "dashboard".to_string(), true);
    close_unnamed_buffers(&mut harness);
    let real_id = harness
        .editor()
        .active_window()
        .buffer_metadata
        .iter()
        .find(|(_, m)| {
            m.file_path()
                .map(|p| p.ends_with("kept.txt"))
                .unwrap_or(false)
        })
        .map(|(id, _)| *id)
        .expect("real file buffer must exist after open_file");
    harness.editor_mut().force_close_buffer(real_id).unwrap();

    // Now only the virtual Dashboard buffer is in the active
    // window. Without the guard, save_workspace would write an
    // empty workspace and lose `kept.txt`.
    harness.editor_mut().save_workspace().unwrap();

    let after = read_workspace(&project_dir)
        .expect("workspace file must still exist; the guard skips the write, not delete it");
    assert!(
        !after.has_no_real_content(),
        "all-virtual save must NOT clobber the real workspace (issue #2027); got empty workspace"
    );
    let after_files: Vec<_> = after
        .split_states
        .values()
        .flat_map(|s| s.open_tabs.iter().cloned())
        .collect();
    assert!(
        !after_files.is_empty(),
        "previous open_tabs must be preserved after the no-op save"
    );
}

#[test]
fn closing_real_files_without_virtual_buffer_overwrites_workspace() {
    // Complement of the test above: closing every real file when
    // NO virtual buffer is present must NOT be blocked by the
    // guard — otherwise the user can never legitimately drop a
    // file from their saved workspace. Fresh's invariant forces
    // at least one buffer to exist at all times (the editor
    // synthesises an unnamed placeholder when the last one
    // closes), so the post-close snapshot is "unnamed-only", not
    // literally empty. The contract we check here is just:
    // `once.txt` must NOT be in the post-save workspace.
    let temp = TempDir::new().unwrap();
    let project_dir = temp.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let project_dir = project_dir.canonicalize().unwrap();

    let real_file = project_dir.join("once.txt");
    std::fs::write(&real_file, "first session").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_dir.clone(),
    )
    .unwrap();

    harness.open_file(&real_file).unwrap();
    harness.editor_mut().save_workspace().unwrap();
    let before = read_workspace(&project_dir).unwrap();
    let before_has_once = before.split_states.values().any(|s| {
        s.open_tabs.iter().any(|t| {
            use fresh::workspace::SerializedTabRef;
            matches!(t, SerializedTabRef::File(p) if p.ends_with("once.txt"))
        })
    });
    assert!(before_has_once, "sanity: first save must include once.txt");

    let real_id = harness
        .editor()
        .active_window()
        .buffer_metadata
        .iter()
        .find(|(_, m)| {
            m.file_path()
                .map(|p| p.ends_with("once.txt"))
                .unwrap_or(false)
        })
        .map(|(id, _)| *id)
        .expect("real file buffer must exist after open_file");
    harness.editor_mut().force_close_buffer(real_id).unwrap();
    harness.editor_mut().save_workspace().unwrap();

    let after = read_workspace(&project_dir).expect("workspace file must still exist");
    let after_has_once = after.split_states.values().any(|s| {
        s.open_tabs.iter().any(|t| {
            use fresh::workspace::SerializedTabRef;
            matches!(t, SerializedTabRef::File(p) if p.ends_with("once.txt"))
        })
    });
    assert!(
        !after_has_once,
        "closing the real file (no virtual buffers present) must remove it from \
         the saved workspace, but once.txt is still listed: {:#?}",
        after.split_states
    );
}
