//! E2E tests for plugin action popups (`editor.showActionPopup`).
//!
//! Action popups carry buffer-independent decisions (e.g. the
//! devcontainer plugin's "attach now?" prompt). They must remain visible
//! and actionable while the user is on _any_ buffer — including virtual
//! buffers like the Dashboard that own the whole split.
//!
//! Regression: the popup used to be attached to the active buffer's popup
//! stack at the moment showActionPopup ran, and would vanish as soon as a
//! plugin (e.g. the dashboard) made a different buffer active.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::plugins::api::{ActionPopupAction, PluginCommand};

fn show_devcontainer_attach_popup(harness: &mut EditorTestHarness) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowActionPopup {
            popup_id: "devcontainer-attach".to_string(),
            title: "Dev Container detected".to_string(),
            message: "Attach to dev container 'test-container'?".to_string(),
            actions: vec![
                ActionPopupAction {
                    id: "attach".to_string(),
                    label: "Attach".to_string(),
                },
                ActionPopupAction {
                    id: "dismiss".to_string(),
                    label: "Not now".to_string(),
                },
            ],
        })
        .unwrap();
}

/// The popup should render over a virtual buffer that owns the whole
/// split (Dashboard pattern), not just over file buffers. This is the
/// regression: previously the popup was scoped to the buffer that was
/// active at show-time, so a buffer switch hid it.
#[test]
fn action_popup_renders_over_virtual_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a virtual buffer mimicking the Dashboard plugin: a tab that
    // a plugin opens to fill the whole split before the popup appears.
    let dashboard_buffer = harness.editor_mut().create_virtual_buffer(
        "Dashboard".to_string(),
        "dashboard".to_string(),
        true,
    );
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            dashboard_buffer,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                "── Dashboard ──\n  weather: sunny\n  git: clean\n",
            )],
        )
        .unwrap();
    harness.editor_mut().switch_buffer(dashboard_buffer);
    harness.render().unwrap();

    // The dashboard text should be on screen before the popup is shown,
    // confirming the virtual buffer is the active split's content.
    let before = harness.screen_to_string();
    assert!(
        before.contains("Dashboard"),
        "Pre-popup screen should show the dashboard buffer. Screen:\n{}",
        before
    );

    // Now a plugin (e.g. devcontainer) shows its action popup. The
    // dashboard buffer is still active.
    show_devcontainer_attach_popup(&mut harness);
    harness.render().unwrap();

    // The popup body must be visible on screen even though the active
    // buffer is the virtual dashboard.
    let after = harness.screen_to_string();
    assert!(
        after.contains("Attach"),
        "Action popup should render over the dashboard. Screen:\n{}",
        after
    );
    assert!(
        after.contains("Not now"),
        "Action popup's dismiss action should be visible. Screen:\n{}",
        after
    );
    assert!(
        after.contains("Dev Container detected") || after.contains("Dev Container"),
        "Action popup title should be visible. Screen:\n{}",
        after
    );
}

/// Esc on a global action popup must dismiss it without falling through
/// to the buffer below.
#[test]
fn action_popup_dismisses_on_escape() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let dashboard_buffer = harness.editor_mut().create_virtual_buffer(
        "Dashboard".to_string(),
        "dashboard".to_string(),
        true,
    );
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            dashboard_buffer,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                "── Dashboard ──\n",
            )],
        )
        .unwrap();
    harness.editor_mut().switch_buffer(dashboard_buffer);
    harness.render().unwrap();

    show_devcontainer_attach_popup(&mut harness);
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Attach"),
        "Sanity: popup is up before Esc."
    );

    // Esc should route to the global popup, not the buffer.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    let after_esc = harness.screen_to_string();
    assert!(
        !after_esc.contains("Attach") && !after_esc.contains("Not now"),
        "Esc should dismiss the global action popup. Screen:\n{}",
        after_esc
    );
}

/// Switching to a different buffer after the popup is shown must NOT
/// hide it — the popup is editor-level, not buffer-local.
#[test]
fn action_popup_persists_across_buffer_switch() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Start on a file-style buffer.
    let scratch = harness.editor_mut().create_virtual_buffer(
        "scratch".to_string(),
        "text".to_string(),
        false,
    );
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            scratch,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                "scratch buffer\n",
            )],
        )
        .unwrap();
    harness.editor_mut().switch_buffer(scratch);
    harness.render().unwrap();

    // Show the popup while `scratch` is active.
    show_devcontainer_attach_popup(&mut harness);
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Attach"),
        "Sanity: popup visible on the scratch buffer."
    );

    // Open a Dashboard-style virtual buffer and switch to it. With the old
    // buffer-scoped popup the popup would be lost here.
    let dashboard = harness.editor_mut().create_virtual_buffer(
        "Dashboard".to_string(),
        "dashboard".to_string(),
        true,
    );
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            dashboard,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                "── Dashboard ──\n",
            )],
        )
        .unwrap();
    harness.editor_mut().switch_buffer(dashboard);
    harness.render().unwrap();

    let after_switch = harness.screen_to_string();
    assert!(
        after_switch.contains("Attach"),
        "Action popup must survive a buffer switch. Screen:\n{}",
        after_switch
    );
}

fn show_devcontainer_attach_popup_named(
    harness: &mut EditorTestHarness,
    popup_id: &str,
    container_name: &str,
) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowActionPopup {
            popup_id: popup_id.to_string(),
            title: "Dev Container detected".to_string(),
            message: format!("Attach to '{}'?", container_name),
            actions: vec![
                ActionPopupAction {
                    id: "attach".to_string(),
                    label: "Attach".to_string(),
                },
                ActionPopupAction {
                    id: "dismiss".to_string(),
                    label: "Not now".to_string(),
                },
            ],
        })
        .unwrap();
}

fn show_generic_popup(
    harness: &mut EditorTestHarness,
    popup_id: &str,
    title: &str,
    body: &str,
    actions: Vec<(&str, &str)>,
) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowActionPopup {
            popup_id: popup_id.to_string(),
            title: title.to_string(),
            message: body.to_string(),
            actions: actions
                .into_iter()
                .map(|(id, label)| ActionPopupAction {
                    id: id.to_string(),
                    label: label.to_string(),
                })
                .collect(),
        })
        .unwrap();
}

/// Two popups pushed concurrently (e.g. two plugins both deciding the
/// session needs attention) must queue LIFO: only the top is interactive,
/// dismissing it surfaces the next, and each one fires its own
/// `action_popup_result` hook instead of the second clobbering the first's
/// tracking.
#[test]
fn action_popups_queue_and_each_resolves_independently() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // First plugin pops up an attach prompt.
    show_generic_popup(
        &mut harness,
        "devcontainer-attach",
        "Attach?",
        "Attach to 'alpha'?",
        vec![("attach", "Attach alpha"), ("dismiss", "Not now alpha")],
    );
    // Second plugin pops up right after — e.g. a different plugin's warning.
    show_generic_popup(
        &mut harness,
        "pkg-install",
        "Install?",
        "Install bravo package?",
        vec![("yes", "Install bravo"), ("no", "Skip bravo")],
    );
    harness.render().unwrap();

    // Only the second (top-of-stack) popup is visible. The first is queued
    // underneath and re-surfaces when the top is dismissed.
    let frame1 = harness.screen_to_string();
    assert!(
        frame1.contains("Install bravo"),
        "Top-of-stack popup must render. Screen:\n{}",
        frame1
    );
    assert!(
        !frame1.contains("Attach alpha"),
        "Queued popup must not leak through underneath. Screen:\n{}",
        frame1
    );

    // Esc dismisses the top (fires action_popup_result for 'pkg-install').
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now the first popup (`devcontainer-attach`) takes its place.
    let frame2 = harness.screen_to_string();
    assert!(
        frame2.contains("Attach alpha"),
        "Queued popup must surface after the top is dismissed. Screen:\n{}",
        frame2
    );
    assert!(
        !frame2.contains("Install bravo"),
        "Dismissed popup must not be re-drawn. Screen:\n{}",
        frame2
    );

    // Dismiss the remaining popup too.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let frame3 = harness.screen_to_string();
    assert!(
        !frame3.contains("Attach alpha") && !frame3.contains("Install bravo"),
        "Both popups must be gone after two Esc presses. Screen:\n{}",
        frame3
    );
}

/// Same as above, but each popup is confirmed via Enter rather than Esc,
/// exercising the confirm path's parallel-stack pop.
#[test]
fn action_popups_queue_confirms_preserve_per_popup_identity() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    show_generic_popup(
        &mut harness,
        "first",
        "First?",
        "First popup body",
        vec![("ok", "OK first"), ("cancel", "Cancel first")],
    );
    show_generic_popup(
        &mut harness,
        "second",
        "Second?",
        "Second popup body",
        vec![("ok", "OK second"), ("cancel", "Cancel second")],
    );
    harness.render().unwrap();

    // Enter on the top popup ('second') — default selection is index 0 'OK'.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let after_first_confirm = harness.screen_to_string();
    assert!(
        !after_first_confirm.contains("Second popup body"),
        "Top popup must be gone after Enter. Screen:\n{}",
        after_first_confirm
    );
    assert!(
        after_first_confirm.contains("First popup body"),
        "Queued popup must surface after top is confirmed. Screen:\n{}",
        after_first_confirm
    );

    // Enter again — this time on the first popup.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_second_confirm = harness.screen_to_string();
    assert!(
        !after_second_confirm.contains("First popup body"),
        "Both popups must be resolved. Screen:\n{}",
        after_second_confirm
    );
}

/// Regression for "user reports Attach stopped attaching when an LSP
/// status popup is also on screen":
///
///   1. Open a file in a project that has devcontainer.json AND LSP
///      configured-but-dormant for the file's language. The user opens
///      the LSP status popup from the indicator (buffer stack); the
///      devcontainer plugin shows its Attach popup (global stack)
///      concurrently.
///   2. User presses Enter on the devcontainer popup to pick "Attach".
///   3. Pre-fix: `handle_popup_confirm`'s cascade checked
///      `pending_lsp_status_popup.is_some()` first, read the
///      *selected LSP popup row's* data, dismissed the devcontainer
///      popup without firing `action_popup_result`, and surfaced the
///      LSP popup underneath — so "Attach" never ran.
///   4. Post-fix: the confirm path inspects the focused popup's
///      `PopupResolver`; global popup's `PluginAction` variant is
///      matched first, fires the hook with the right action id, and
///      leaves the LSP popup untouched underneath.
///
/// The test installs a small plugin that captures every
/// `action_popup_result` call via the status bar so we can assert the
/// hook ran with the right arguments.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn action_popup_attach_fires_hook_even_when_lsp_status_popup_is_open() {
    use crate::common::harness::{copy_plugin_lib, HarnessOptions};
    use std::fs;

    // Plugin: registers a showActionPopup caller (the "devcontainer
    // mock") + listens for action_popup_result and writes the outcome
    // to the status bar.
    let temp = tempfile::tempdir().unwrap();
    let project_root = temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    // Plugin just needs to observe the action_popup_result hook — we
    // show the popup itself by handing Rust a `PluginCommand`, which
    // exercises the same dispatch code path as a JS-side
    // `editor.showActionPopup(...)`.
    let plugin = r#"
const editor = getEditor();

globalThis.devmock_on_result = function(data: { popup_id: string; action_id: string }): void {
    editor.setStatus("result: " + data.popup_id + "/" + data.action_id);
};
editor.on("action_popup_result", "devmock_on_result");
"#;
    fs::write(plugins_dir.join("devmock.ts"), plugin).unwrap();

    // Source file — .rs so the LSP auto-prompt's language match fires.
    let file = project_root.join("src.rs");
    fs::write(&file, "fn main() {}\n").unwrap();

    // Config with a dormant rust LSP (enabled, auto_start = false) so
    // opening src.rs queues the auto-prompt.
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("rust-analyzer".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root.clone()),
    )
    .unwrap();

    // Open the file, then explicitly open the LSP status popup so the
    // collision scenario below has both popups on screen.
    harness.open_file(&file).unwrap();
    harness.editor_mut().show_lsp_status_popup();
    harness.render().unwrap();
    // Sanity: the LSP popup surfaced.
    assert!(
        harness.screen_to_string().contains("rust-analyzer"),
        "LSP status popup should be visible after explicit open. Screen:\n{}",
        harness.screen_to_string()
    );

    // Trigger the devcontainer-mock action popup on top of it. From
    // here on both popups coexist: LSP on buffer stack, plugin action
    // on global stack.
    show_devcontainer_attach_popup_named(&mut harness, "devcontainer-attach", "mock-container");
    harness.render().unwrap();
    let with_both = harness.screen_to_string();
    assert!(
        with_both.contains("Attach") && with_both.contains("mock-container"),
        "Devcontainer-mock popup should be on screen. Screen:\n{}",
        with_both
    );

    // Enter on the global (devcontainer) popup. Default selection is
    // "Attach" (index 0). The hook must fire with action_id=attach —
    // that's the signal the plugin's attach routine ran.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Bounded wait: the plugin thread processes the hook
    // asynchronously, so poll a fixed number of tick+render cycles.
    // When the cascade bug is in effect, the hook never fires and the
    // assertion below trips with a concrete failure instead of
    // hanging the test runner.
    for _ in 0..50 {
        harness.tick_and_render().unwrap();
        if harness
            .screen_to_string()
            .contains("result: devcontainer-attach/")
        {
            break;
        }
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("result: devcontainer-attach/attach"),
        "Pressing Enter on the devcontainer popup must fire \
         action_popup_result with action_id=attach, even while an LSP \
         auto-prompt is also on screen. Got status line: looking for \
         'result: devcontainer-attach/attach' in:\n{}",
        screen
    );
    assert!(
        !screen.contains("result: devcontainer-attach/dismissed"),
        "Enter must not fire a 'dismissed' result — that would mean the \
         popup was swallowed by an unrelated confirm branch. Screen:\n{}",
        screen
    );
}
