//! Wiring tests for the editor `Session` abstraction.
//!
//! Step 1 of the Conductor migration adds a single forced session to
//! `Editor` without changing any user-visible behaviour. These tests
//! pin the boot invariants so subsequent migration steps don't break
//! them silently:
//!
//! - The editor boots with exactly one session.
//! - That session's id is `WindowId(1)` (the "base").
//! - Its `root` matches `editor.working_dir()` — call sites can swap
//!   from `working_dir()` to `active_window().root` and read the
//!   same value.
//! - Its `label` is non-empty.
//!
//! These are model invariants, not user-visible behaviour, so they
//! observe via `harness.editor()` rather than the rendered screen.
//! That follows the existing pattern in `workspace.rs` for
//! invariants that have no screen surface yet.

use crate::common::harness::EditorTestHarness;
use fresh_core::{BufferId, WindowId};

#[test]
fn editor_boots_with_one_base_session() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let editor = harness.editor();
    assert_eq!(editor.session_count(), 1, "expected exactly one session");
    assert_eq!(editor.active_session_id(), WindowId(1));
}

#[test]
fn active_session_root_matches_working_dir() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let editor = harness.editor();
    let session = editor.active_window();
    assert_eq!(
        session.root,
        editor.working_dir(),
        "session root must mirror working_dir for the migration to be \
         a behaviour-preserving refactor"
    );
}

#[test]
fn active_session_has_non_empty_label() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let session = harness.editor().active_window();
    assert!(
        !session.label.is_empty(),
        "session label fell through to empty string; \
         basename fallback in Window::new is broken"
    );
}

// ---------------------------------------------------------------------------
// Step 2: createSession / setActiveSession / closeSession lifecycle.
//
// These tests drive the editor methods directly rather than going through
// the JS plugin runtime. The plugin command dispatch is identical to
// existing fire-and-forget commands (registerCommand, focusSplit) — wiring
// is trusted and exercised by other e2e tests; here we focus on the
// session-state semantics.
// ---------------------------------------------------------------------------

use std::path::PathBuf;

/// `openFileInBackground({sessionId})` for an inactive session loads
/// the buffer and routes it into the target session's membership
/// + stashed split tree without disturbing the active session.
#[test]
fn open_file_in_background_targets_inactive_session() {
    use fresh_core::api::PluginCommand;

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let f = project_dir.join("alpha-file.txt");
    std::fs::write(&f, "alpha contents").unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(project_dir.join("wt-alpha-open"), "alpha".into());

    let active_buffer_before = harness.editor().active_buffer();

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::OpenFileInBackground {
            path: f.clone(),
            window_id: Some(alpha),
        })
        .unwrap();

    // Active buffer is unchanged — the user's view stays put.
    assert_eq!(
        harness.editor().active_buffer(),
        active_buffer_before,
        "openFileInBackground with inactive sessionId must not move active focus"
    );

    // Alpha's membership has gained the file's buffer.
    let alpha_count = harness.editor().session(alpha).unwrap().buffers.len();
    assert_eq!(
        alpha_count, 1,
        "alpha should own exactly the file we opened into it"
    );

    // Alpha's splits stash now has the buffer ready to render
    // on next dive.
    assert!(
        harness
            .editor()
            .session(alpha)
            .unwrap()
            .splits_stash
            .is_some(),
        "alpha's splits_stash should be populated"
    );
}

/// `createTerminal({ sessionId })` for an inactive session attaches
/// the buffer to that session's membership and seeds (or extends)
/// its stashed split tree — without disturbing the active session.
/// Conductor uses this to spawn agents in worktrees without diving.
#[test]
fn create_terminal_targets_inactive_session_via_session_id() {
    use fresh_core::api::PluginCommand;

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(project_dir.join("wt-alpha-term"), "alpha".into());

    let active_before = harness.editor().active_session_id();
    let active_buffer_before = harness.editor().active_buffer();

    // Drive the dispatch path directly — going through the JS
    // runtime would require a full plugin load with TS compile.
    // The wiring on the JS side is just `window_id: opts.session_id`.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CreateTerminal {
            cwd: None,
            direction: None,
            ratio: None,
            focus: None,
            persistent: false,
            window_id: Some(alpha),
            request_id: 9999,
        })
        .unwrap();

    // Active session is unchanged — the user's editor view is
    // not pulled away from base.
    assert_eq!(
        harness.editor().active_session_id(),
        active_before,
        "createTerminal with inactive sessionId must not switch active"
    );
    assert_eq!(
        harness.editor().active_buffer(),
        active_buffer_before,
        "active buffer must not change when terminal targets an inactive session"
    );

    // Alpha's membership set has gained a new buffer (the terminal).
    let alpha_buffers_count = harness.editor().session(alpha).unwrap().buffers.len();
    assert_eq!(
        alpha_buffers_count, 1,
        "alpha should have exactly the new terminal buffer attached"
    );

    // Alpha's stashed splits now have a leaf for the terminal,
    // ready to render on next dive. (Was None before — alpha had
    // never been activated.)
    assert!(
        harness
            .editor()
            .session(alpha)
            .unwrap()
            .splits_stash
            .is_some(),
        "alpha's split stash should be seeded with the terminal's leaf"
    );
}

#[test]
fn create_session_inserts_with_monotonic_id() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let editor = harness.editor_mut();

    let id_a = editor.create_window_at(PathBuf::from("/tmp/wt-a"), "alpha".into());
    let id_b = editor.create_window_at(PathBuf::from("/tmp/wt-b"), "beta".into());

    assert_eq!(
        id_a,
        WindowId(2),
        "first new session should take id 2 (after base)"
    );
    assert_eq!(id_b, WindowId(3), "ids must be monotonic");
    assert_eq!(editor.session_count(), 3);

    // Active session is unchanged by createSession alone.
    assert_eq!(editor.active_session_id(), WindowId(1));
}

#[test]
fn create_session_falls_back_to_basename_label() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/feat-auth"), String::new());

    let session = harness.editor().session(id).unwrap();
    assert_eq!(
        session.label, "feat-auth",
        "empty label must fall back to the root basename"
    );
}

#[test]
fn set_active_session_swaps_pointer_and_working_dir() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let new_root = PathBuf::from("/tmp/wt-feat-auth");
    let new_id = harness
        .editor_mut()
        .create_window_at(new_root.clone(), "feat-auth".into());

    harness.editor_mut().set_active_window(new_id);

    assert_eq!(harness.editor().active_session_id(), new_id);
    assert_eq!(
        harness.editor().active_window().root,
        new_root,
        "active session's root must be the new path"
    );
    assert_eq!(
        harness.editor().working_dir(),
        new_root,
        "working_dir must follow active_window().root for the migration \
         to be a behaviour-preserving refactor"
    );
}

#[test]
fn set_active_session_unknown_id_is_noop() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let original_root = harness.editor().working_dir().to_path_buf();
    let original_active = harness.editor().active_session_id();

    harness.editor_mut().set_active_window(WindowId(99));

    assert_eq!(harness.editor().active_session_id(), original_active);
    assert_eq!(harness.editor().working_dir(), original_root);
}

#[test]
fn close_session_drops_inactive_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-feat"), "feat".into());

    let removed = harness.editor_mut().close_window(id);

    assert!(
        removed,
        "close_window should succeed for an inactive session"
    );
    assert_eq!(harness.editor().session_count(), 1);
    assert_eq!(harness.editor().active_session_id(), WindowId(1));
}

#[test]
fn close_session_refuses_active_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let id = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-feat"), "feat".into());
    harness.editor_mut().set_active_window(id);

    let removed = harness.editor_mut().close_window(id);

    assert!(!removed, "close_window must refuse the active session");
    assert_eq!(harness.editor().session_count(), 2);
}

/// `setActiveSession` warm-swaps the split-tree layout (split
/// tree + per-leaf view state). Each session has its own splits
/// — the active buffer in the base session does not appear as
/// active in alpha, and the buffer set visible in tabs differs.
///
/// Concretely: the base session has its initial scratch buffer
/// active. Diving into a never-activated alpha brings up alpha's
/// fresh empty unnamed buffer (different id). Diving back brings
/// the base's active buffer back.
#[test]
fn dive_swaps_split_tree_and_active_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let base_active = harness.editor().active_buffer();

    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-splits"), "alpha".into());

    harness.editor_mut().set_active_window(alpha);
    let alpha_active = harness.editor().active_buffer();
    assert_ne!(
        alpha_active, base_active,
        "alpha's fresh dive must allocate a new seed buffer, \
         not inherit the base session's active buffer"
    );

    harness.editor_mut().set_active_window(WindowId(1));
    assert_eq!(
        harness.editor().active_buffer(),
        base_active,
        "diving back must restore the base session's split tree \
         and active buffer"
    );

    harness.editor_mut().set_active_window(alpha);
    assert_eq!(
        harness.editor().active_buffer(),
        alpha_active,
        "diving back into alpha (now warm) must restore alpha's seed buffer"
    );
}

/// `setActiveSession` warm-swaps the per-session mtime cache for
/// auto-revert. Each session tracks its own files; when the user
/// dives, the active mtime map switches with the rest of the
/// session state. This matches the user's mental model that a
/// dormant session "is paused" — its files don't auto-revert
/// until the user dives back in.
#[test]
fn dive_stashes_and_restores_file_mod_times() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-mt"), "alpha".into());

    let p = PathBuf::from("/tmp/some-file.txt");
    let t = std::time::SystemTime::now();
    harness.editor_mut().insert_mtime_for_test(p.clone(), t);
    assert!(harness.editor().has_mtime_for_test(&p));

    harness.editor_mut().set_active_window(alpha);
    assert!(
        !harness.editor().has_mtime_for_test(&p),
        "alpha's mtime cache starts empty — base's entry must be stashed"
    );

    harness.editor_mut().set_active_window(WindowId(1));
    assert!(
        harness.editor().has_mtime_for_test(&p),
        "diving back must restore base's mtime entry from its stash"
    );
}

/// `setActiveSession` warm-swaps the LSP manager. The outgoing
/// session's running LSPs are stashed (still alive in memory)
/// and the incoming session's stash — empty for a fresh session
/// — becomes the active slot. Future LSP requests in the new
/// session spawn fresh servers rooted at its path; future dive-
/// back finds the stashed LSPs warm.
#[test]
fn dive_stashes_and_restores_lsp_manager() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-lsp"), "alpha".into());

    // Pretend the base session has LSPs running.
    harness.editor_mut().install_dummy_lsp_for_test();
    assert!(harness.editor().has_lsp_for_test());

    harness.editor_mut().set_active_window(alpha);
    assert!(
        !harness.editor().has_lsp_for_test(),
        "alpha has no stashed LSP; active slot must be empty so a \
         fresh LspManager spawns rooted at alpha's path on demand"
    );

    harness.editor_mut().set_active_window(WindowId(1));
    assert!(
        harness.editor().has_lsp_for_test(),
        "diving back must restore the base's stashed LspManager"
    );
}

/// `setActiveSession` warm-swaps utility-dock panel-id occupancy.
/// Each session has its own dock — a search panel claimed in the
/// base session disappears when diving to alpha and reappears on
/// dive-back, even though Editor.panel_ids is the only live map.
#[test]
fn dive_stashes_and_restores_panel_ids() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-panel"), "alpha".into());

    // Pretend the base session has a search panel claimed in the dock.
    harness
        .editor_mut()
        .insert_panel_id_for_test("search".to_string(), BufferId(42));
    assert!(harness.editor().panel_ids_for_test().contains_key("search"));

    harness.editor_mut().set_active_window(alpha);
    assert!(
        !harness.editor().panel_ids_for_test().contains_key("search"),
        "alpha's dock starts empty — base's claim must have been stashed away"
    );

    harness.editor_mut().set_active_window(WindowId(1));
    assert_eq!(
        harness.editor().panel_ids_for_test().get("search").copied(),
        Some(BufferId(42)),
        "diving back must restore the base session's panel claim"
    );
}

/// `setActiveSession` warm-swaps file explorer state: each session
/// sees its own view (or rebuilds at its root on first toggle),
/// rather than every dive losing the outgoing session's expansion.
///
/// Concretely: open the file explorer in the base session (so it
/// has a `Some` view), dive away, dive back — the base's view
/// returns instead of being rebuilt from scratch. The "rebuild
/// from scratch" path was the MVP behaviour pre-warm-swap.
#[test]
fn dive_stashes_and_restores_file_explorer_view() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Spawn a side session to dive into.
    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-warm"), "alpha".into());

    // Force the base session's explorer into a `Some` state by
    // toggling, then pumping async until the lazy build settles.
    harness.editor_mut().toggle_file_explorer();
    for _ in 0..40 {
        harness.process_async_and_render().unwrap();
        if harness.editor().file_explorer().is_some() {
            break;
        }
        harness.sleep(std::time::Duration::from_millis(25));
    }
    assert!(
        harness.editor().file_explorer().is_some(),
        "file explorer should be built after toggle + async pump"
    );

    // Dive into alpha. Base session's view is stashed; the active
    // explorer slot is None (alpha has never opened one).
    harness.editor_mut().set_active_window(alpha);
    assert!(
        harness.editor().file_explorer().is_none(),
        "alpha session has no stashed explorer; active slot \
         must be None until alpha first toggles"
    );

    // Dive back. Base's stashed view returns.
    harness.editor_mut().set_active_window(WindowId(1));
    assert!(
        harness.editor().file_explorer().is_some(),
        "base session's file explorer should be restored from its stash"
    );
}

/// Buffer membership is attached on open. A file opened while
/// the active session is alpha shows up in alpha.buffers, not in
/// the base session's buffers.
#[test]
fn opening_a_file_attaches_buffer_to_active_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();

    let alpha = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-alpha-bufs"), "alpha".into());
    harness.editor_mut().set_active_window(alpha);

    let file_path = project_dir.join("attaches.txt");
    std::fs::write(&file_path, "hello").unwrap();
    harness.open_file(&file_path).unwrap();
    let buffer_id = harness.editor().active_buffer();

    // The buffer is in alpha's set, not the base's.
    let alpha_set = &harness.editor().session(alpha).unwrap().buffers;
    assert!(
        alpha_set.contains(&buffer_id),
        "buffer must be attached to active session at open time"
    );
    let base_set = &harness.editor().session(WindowId(1)).unwrap().buffers;
    assert!(
        !base_set.contains(&buffer_id),
        "buffer must NOT be attached to non-active sessions"
    );
}

#[test]
fn close_session_refuses_base_session() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    // Add an inactive session and switch to it so the base is no
    // longer active — only the "is base" rule should keep it
    // alive.
    let id = harness
        .editor_mut()
        .create_window_at(PathBuf::from("/tmp/wt-feat"), "feat".into());
    harness.editor_mut().set_active_window(id);

    let removed = harness.editor_mut().close_window(WindowId(1));

    assert!(
        !removed,
        "close_window must refuse the base session even when not active"
    );
    assert_eq!(harness.editor().session_count(), 2);
}
