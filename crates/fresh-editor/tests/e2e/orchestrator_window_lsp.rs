//! Regression test: a window created/dived into through the
//! orchestrator must get its own LSP manager.
//!
//! Only the base window (id 1) is born with an `LspManager` (built in
//! `editor_init::with_options`). Windows the orchestrator spawns later
//! — `create_window_at` + `set_active_window` (dive), or
//! `create_window_with_terminal` (new session) — start with
//! `Window.lsp == None`. Before this fix, diving into such a window and
//! opening a code buffer left every LSP action dead: the status popup's
//! "Start <server>" reported *"No LSP manager available"* and nothing
//! ever spawned.
//!
//! The fix attaches a manager lazily on the dive path
//! (`Editor::ensure_window_lsp`), so each window gets the same
//! configured server set the base window has. `has_lsp_for_test()`
//! reports whether the *active* window's `lsp` slot is populated, which
//! is exactly the condition that was false before.

use crate::common::harness::EditorTestHarness;

/// Diving into an orchestrator-created window attaches an LSP manager
/// to it, so it reaches parity with the base window instead of being
/// stuck on "No LSP manager available".
#[test]
fn dived_orchestrator_window_gets_its_own_lsp_manager() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();

    // Sanity: the base window is born with a manager.
    assert!(
        harness.editor().has_lsp_for_test(),
        "base window should have an LSP manager from editor_init"
    );

    // Spawn a second window the way the orchestrator does and dive in.
    let second_root = tempfile::tempdir().unwrap();
    let second_id = harness
        .editor_mut()
        .create_window_at(second_root.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second_id);

    // The regression guard: the dived-into window must now have its own
    // LSP manager. Pre-fix this was `None` → "No LSP manager available".
    assert!(
        harness.editor().has_lsp_for_test(),
        "orchestrator-created window must get its own LSP manager on dive"
    );

    // Open a code buffer in the new window and prove the LSP action no
    // longer dead-ends: with a manager + the default rust server config
    // present, Start/Restart LSP must not report the absent-manager or
    // unconfigured-language errors.
    std::fs::write(second_root.path().join("main.rs"), "fn main() {}\n").unwrap();
    harness
        .editor_mut()
        .open_file(&second_root.path().join("main.rs"))
        .unwrap();
    harness.editor_mut().handle_lsp_restart();

    let status = harness
        .editor()
        .active_window()
        .status_message
        .clone()
        .unwrap_or_default();
    assert!(
        !status.contains("No LSP manager available"),
        "Start/Restart LSP in an orchestrator window must not report a missing manager, got: {status:?}"
    );
    assert!(
        !status.contains("No LSP server configured"),
        "the window's manager must carry the configured rust server, got: {status:?}"
    );

    // Keep the second window's root alive until the assertions are done.
    drop(second_root);
}
