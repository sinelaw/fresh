//! Every `fresh-editor` integration test, in one binary.
//!
//! Each test root used to be its own `tests/*.rs`, and cargo builds one
//! binary per root -- each statically linking the whole editor and its
//! dependency graph. Compiling and linking those separately dominated the
//! cost of `cargo test`, so they are pulled in here as modules instead.
//! The files stay where they are; `#[path]` keeps `file!()`, `include_str!`
//! and insta snapshot paths pointing at the same places as before.
//!
//! Adding a test file under `tests/` means adding a line here -- cargo no
//! longer auto-discovers roots (`autotests = false` in Cargo.toml).

mod common;
mod e2e;
mod semantic;

#[path = "daemon_workspace_restore_parity.rs"]
mod daemon_workspace_restore_parity;
#[path = "editorconfig_tests.rs"]
mod editorconfig_tests;
#[path = "focused_bug_test.rs"]
mod focused_bug_test;
#[path = "format_on_save_undo_view_test.rs"]
mod format_on_save_undo_view_test;
#[path = "geometry_pass.rs"]
mod geometry_pass;
#[path = "harness_test.rs"]
mod harness_test;
#[path = "init_script_dashboard_suppress.rs"]
mod init_script_dashboard_suppress;
#[path = "integration_tests.rs"]
mod integration_tests;
#[path = "kube_fake_kubectl.rs"]
mod kube_fake_kubectl;
#[path = "mouse_capture_default.rs"]
mod mouse_capture_default;
#[path = "orchestrator_bringup_characterization.rs"]
mod orchestrator_bringup_characterization;
#[path = "orchestrator_bringup_plugin_verify.rs"]
mod orchestrator_bringup_plugin_verify;
#[path = "orchestrator_bringup_render_verify.rs"]
mod orchestrator_bringup_render_verify;
#[path = "orchestrator_co_tenant_restore.rs"]
mod orchestrator_co_tenant_restore;
#[path = "orchestrator_dock_connecting_commit.rs"]
mod orchestrator_dock_connecting_commit;
#[path = "orchestrator_dock_dormant_ssh_badge.rs"]
mod orchestrator_dock_dormant_ssh_badge;
#[path = "orchestrator_dock_failed_reconnect.rs"]
mod orchestrator_dock_failed_reconnect;
#[path = "orchestrator_dock_ssh_navigation_nonblocking.rs"]
mod orchestrator_dock_ssh_navigation_nonblocking;
#[path = "orchestrator_dock_ssh_navigation_slow_nonblocking.rs"]
mod orchestrator_dock_ssh_navigation_slow_nonblocking;
#[path = "orchestrator_eager_persistence.rs"]
mod orchestrator_eager_persistence;
#[path = "orchestrator_pending_local.rs"]
mod orchestrator_pending_local;
#[path = "orchestrator_pending_recovery.rs"]
mod orchestrator_pending_recovery;
#[path = "orchestrator_pending_ssh.rs"]
mod orchestrator_pending_ssh;
#[path = "orchestrator_persistence_paths.rs"]
mod orchestrator_persistence_paths;
#[path = "orchestrator_preparing_workspace.rs"]
mod orchestrator_preparing_workspace;
#[path = "orchestrator_workspace_roundtrip.rs"]
mod orchestrator_workspace_roundtrip;
#[path = "property_agent_tests.rs"]
mod property_agent_tests;
#[path = "property_persistence_tests.rs"]
mod property_persistence_tests;
#[path = "property_tests.rs"]
mod property_tests;
#[path = "regression_hidden_cursor_panic.rs"]
mod regression_hidden_cursor_panic;
#[path = "remote_channel_timeout_tests.rs"]
mod remote_channel_timeout_tests;
#[path = "remote_filesystem_tests.rs"]
mod remote_filesystem_tests;
#[path = "remote_orchestrator_persistence_local.rs"]
mod remote_orchestrator_persistence_local;
#[path = "remote_poll_hang_tests.rs"]
mod remote_poll_hang_tests;
#[path = "remote_restore_terminal_e2e.rs"]
mod remote_restore_terminal_e2e;
#[path = "remote_ssh_terminal.rs"]
mod remote_ssh_terminal;
#[path = "remote_terminal_backing_local.rs"]
mod remote_terminal_backing_local;
#[path = "screensaver_test.rs"]
mod screensaver_test;
#[path = "self_update_spine.rs"]
mod self_update_spine;
#[path = "shadow_model_editor_state_tests.rs"]
mod shadow_model_editor_state_tests;
#[path = "shadow_model_multi_cursor_tests.rs"]
mod shadow_model_multi_cursor_tests;
#[path = "shadow_model_tests.rs"]
mod shadow_model_tests;
#[path = "ssh_attach_error.rs"]
mod ssh_attach_error;
#[path = "streaming_and_grapheme_regression_tests.rs"]
mod streaming_and_grapheme_regression_tests;
#[path = "terminal_restore_live.rs"]
mod terminal_restore_live;
#[path = "terminal_restore_script_token.rs"]
mod terminal_restore_script_token;
#[path = "terminal_scrollback_isolation.rs"]
mod terminal_scrollback_isolation;
#[path = "test_auto_save.rs"]
mod test_auto_save;
#[path = "test_line_iterator_comprehensive.rs"]
mod test_line_iterator_comprehensive;
#[path = "test_overlay_colors.rs"]
mod test_overlay_colors;
#[path = "test_plugin_i18n.rs"]
mod test_plugin_i18n;
#[path = "test_plugin_i18n_completeness.rs"]
mod test_plugin_i18n_completeness;
#[path = "test_save_all.rs"]
mod test_save_all;
#[path = "test_theme_schema_i18n.rs"]
mod test_theme_schema_i18n;
#[path = "ui_shell_frame_parity.rs"]
mod ui_shell_frame_parity;
#[path = "ui_tree_dump.rs"]
mod ui_tree_dump;
#[path = "undo_redo_marker_roundtrip_tests.rs"]
mod undo_redo_marker_roundtrip_tests;
#[path = "workspace_persistence_gates.rs"]
mod workspace_persistence_gates;
#[path = "workspace_virtual_buffer_clobber.rs"]
mod workspace_virtual_buffer_clobber;

/// `autotests = false` means cargo no longer discovers `tests/*.rs` on its own:
/// a root missing from the list above is not an error, it simply never runs.
/// That is a silent hole, and it caught a real file (`ui_shell_frame_parity`)
/// the first time this branch was rebased onto other work. So the list checks
/// itself.
#[test]
fn every_test_root_is_listed_in_this_file() {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let me = std::fs::read_to_string(dir.join("all_tests.rs")).expect("read all_tests.rs");

    // Declared in Cargo.toml as its own target instead (gated on a feature).
    const SEPARATE_TARGETS: &[&str] = &["scene_parity"];

    let mut unlisted: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_file())
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            let stem = name.strip_suffix(".rs")?.to_string();
            if stem == "all_tests" || SEPARATE_TARGETS.contains(&stem.as_str()) {
                return None;
            }
            let listed = me.contains(&format!("#[path = \"{name}\"]"));
            (!listed).then_some(stem)
        })
        .collect();
    unlisted.sort();

    assert!(
        unlisted.is_empty(),
        "these test roots exist under tests/ but are not declared in all_tests.rs, \
         so none of their tests run:\n  {}\n\
         Add `#[path = \"<name>.rs\"] mod <name>;` to all_tests.rs (or declare a \
         [[test]] target in Cargo.toml and list it in SEPARATE_TARGETS).",
        unlisted.join("\n  ")
    );
}
