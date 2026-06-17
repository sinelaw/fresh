//! Plugin-related E2E tests
//! These tests are only compiled when the "plugins" feature is enabled.

pub mod after_file_open_preview;
pub mod asm_lsp_config;
pub mod audit_mode;
pub mod authority_snapshot;
pub mod buffer_info_splits;
pub mod command_keybinding_editor;
pub mod dashboard;
// The three modules below drive the in-tree fake-devcontainer
// CLI (a bash script under `scripts/fake-devcontainer/bin/`).
// Native Windows can't execute `#!/usr/bin/env bash` shebangs
// and the harness helper (`HarnessOptions::with_fake_devcontainer`)
// uses POSIX PATH separators. The other devcontainer test files
// stay on every OS — they exercise plugin wiring through the
// editor's command channel and don't shell out.
#[cfg(unix)]
pub mod devcontainer_attach_e2e;
pub mod devcontainer_failed_attach_popup;
#[cfg(unix)]
pub mod devcontainer_lsp_definition;
pub mod devcontainer_ports_panel;
pub mod devcontainer_run_lifecycle;
#[cfg(unix)]
pub mod devcontainer_spec_conformance;
#[cfg(unix)]
pub mod devcontainer_spec_repros;
pub mod devcontainer_usability_repros;
pub mod diagnostics_panel_bugs;
pub mod diagnostics_panel_jump;
pub mod diff_cursor;
pub mod env_manager;
pub mod file_explorer_slots;
pub mod find_file;
pub mod git;
pub mod git_log_current_file;
pub mod git_log_split_tab_focus;
pub mod git_statusbar;
pub mod goto_with_selection;
pub mod gutter;
pub mod init_script;
pub mod language_pack;
pub mod live_diff;
pub mod load_from_buffer;
pub mod lsp_find_references;
pub mod lsp_navigation;
pub mod markdown_source;
pub mod orchestrator_attach_worktree;
pub mod orchestrator_new_dialog;
pub mod orchestrator_new_session_renders;
pub mod orchestrator_open_cross_project;
pub mod package_manager;
pub mod plugin;
pub mod plugin_config_registration;
pub mod plugin_keybinding_execution;
pub mod plugins_dir_in_working_dir;
pub mod review_diff_hunk_parity;
pub mod review_diff_ux_bugs;
pub mod tab_actions;
pub mod terminal_hooks;
pub mod theme_editor;
pub mod trust_lockdown;
pub mod unified_keybindings;
pub mod vi_mode_autostart;
pub mod watch_path;
