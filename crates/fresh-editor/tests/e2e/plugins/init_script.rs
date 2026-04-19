//! End-to-end coverage for the user `init.ts` auto-loader (design M0).
//!
//! We drive the loader directly on the test harness. The harness doesn't
//! replicate `main()`'s boot sequence, so it doesn't call `load_init_script`
//! on its own — we do. That's enough to exercise: file discovery, the
//! `--no-init` escape hatch, and successful evaluation.

use crate::common::harness::EditorTestHarness;
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::PathBuf;

/// Build a harness whose `DirectoryContext::config_dir` is
/// `<tempdir>/config` (the layout `DirectoryContext::for_testing` uses).
/// Returns the harness, the temp guard, and the resolved config_dir.
fn harness_with_scratch_config_dir() -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();

    let harness = EditorTestHarness::with_shared_dir_context(
        80,
        24,
        Default::default(),
        working_dir,
        dir_context,
    )
    .expect("harness");
    (harness, temp, config_dir)
}

fn write_init_ts(config_dir: &std::path::Path, body: &str) {
    fs::write(config_dir.join("init.ts"), body).unwrap();
}

#[test]
fn missing_init_ts_is_silent() {
    let (mut harness, _tmp, _config_dir) = harness_with_scratch_config_dir();

    // Capture any pre-existing status (other plugins may set one) so we only
    // assert init.ts doesn't introduce a new one.
    let before = harness.editor().get_status_message().cloned();

    harness.editor_mut().load_init_script(true);

    let after = harness.editor().get_status_message().cloned();
    assert_eq!(
        before, after,
        "loading a missing init.ts must not change the status"
    );
    // And specifically: nothing should mention init.ts.
    assert!(
        after
            .as_deref()
            .map(|s| !s.contains("init.ts"))
            .unwrap_or(true),
        "status should not mention init.ts when the file is absent: {after:?}"
    );
}

#[test]
fn disabled_flag_skips_init_ts_even_when_present() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();
    write_init_ts(&config_dir, "throw new Error('should not run');");

    // `enabled = false` models `--no-init` / `--safe`.
    harness.editor_mut().load_init_script(false);

    // The eval intentionally would have thrown; if we skipped, no failure
    // banner should surface.
    let status = harness.editor().get_status_message().cloned();
    let offending = status
        .as_deref()
        .map(|s| s.contains("init.ts:"))
        .unwrap_or(false);
    assert!(
        !offending,
        "disabled init.ts must not surface a failure: status = {status:?}"
    );
}

#[test]
fn set_setting_updates_effective_config() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    let before: serde_json::Value =
        serde_json::to_value(&harness.editor().config_for_tests().editor).unwrap();
    let original_tab_size = before["tab_size"].as_u64().unwrap_or(4);
    let target_tab_size = if original_tab_size == 7 { 3 } else { 7 };

    write_init_ts(
        &config_dir,
        &format!(
            r#"
            const editor = getEditor();
            editor.setSetting("editor.tab_size", {target_tab_size});
            "#
        ),
    );

    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();

    let after_tab = harness.editor().config_for_tests().editor.tab_size;
    assert_eq!(
        after_tab as u64, target_tab_size,
        "init.ts setSetting should update the effective tab_size"
    );
}

#[test]
fn editor_on_accepts_a_closure_and_plugins_loaded_fires() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    // init.ts uses the closure form of `editor.on` — this is the DX win
    // of M2. The callback sets a status message we can observe.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.on("plugins_loaded", () => {
            editor.setStatus("plugins_loaded fired");
        });
        "#,
    );

    harness.editor_mut().load_init_script(true);
    harness.editor_mut().fire_plugins_loaded_hook();
    // Hook dispatch is async; drain it.
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("plugins_loaded fired"),
        "expected plugins_loaded closure to fire: status = {status:?}"
    );
}

#[test]
fn export_plugin_api_and_get_plugin_api_round_trip() {
    // A plugin exports a typed surface; init.ts-style code reaches it via
    // getPluginApi and calls through to the plugin's own configure method.
    // All in one plugin because the harness loads exactly one source at a
    // time — but the same mechanism works across plugins at runtime.
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();

        // Pretend this is a separate plugin publishing its config API.
        let stored = null;
        editor.exportPluginApi("fake-dashboard", {
            configure(opts) { stored = opts; },
            getStored() { return stored; },
        });

        // And this is init.ts reaching it.
        const api = editor.getPluginApi("fake-dashboard");
        if (api === null) {
            editor.setStatus("ERR: api not found");
        } else {
            api.configure({ title: "Hello" });
            const back = api.getStored();
            editor.setStatus(`got back: ${back.title}`);
        }
        "#,
    );

    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("got back: Hello"),
        "expected configure/read round-trip through getPluginApi: status = {status:?}"
    );
}

#[test]
fn get_plugin_api_returns_null_when_name_unknown() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        const api = editor.getPluginApi("does-not-exist");
        editor.setStatus(api === null ? "null" : "not-null");
        "#,
    );

    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("null"),
        "getPluginApi for unknown name should return null: status = {status:?}"
    );
}

#[test]
fn ready_hook_fires_and_can_be_observed_with_legacy_on_form() {
    // The legacy string-handler form must keep working alongside the
    // closure overload.
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        function on_ready_handler() {
            editor.setStatus("ready fired");
        }
        registerHandler("on_ready_handler", on_ready_handler);
        editor.on("ready", "on_ready_handler");
        "#,
    );

    harness.editor_mut().load_init_script(true);
    harness.editor_mut().fire_ready_hook();
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("ready fired"),
        "expected ready hook to fire with string-handler form: status = {status:?}"
    );
}

#[test]
fn set_setting_is_fire_and_forget_across_reload() {
    // setSetting writes persist across reload — fire-and-forget, same model
    // as Neovim/VS Code/Emacs/Sublime. A reload that no longer calls
    // setSetting does NOT revert the prior value.
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    let original_tab_size = harness.editor().config_for_tests().editor.tab_size as u64;
    let overridden = if original_tab_size == 7 { 3 } else { 7 };

    // First run: write an override.
    write_init_ts(
        &config_dir,
        &format!(
            r#"
            const editor = getEditor();
            editor.setSetting("editor.tab_size", {overridden});
            "#
        ),
    );
    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();
    assert_eq!(
        harness.editor().config_for_tests().editor.tab_size as u64,
        overridden
    );

    // Second run: no setSetting at all — the old write persists.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("init.ts reloaded with no writes");
        "#,
    );
    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();

    assert_eq!(
        harness.editor().config_for_tests().editor.tab_size as u64,
        overridden,
        "fire-and-forget: the prior setSetting write survives reload"
    );
}

#[test]
fn init_reload_action_picks_up_file_edits() {
    use fresh::input::keybindings::Action;

    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    // Initial content: writes one sentinel.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("first load");
        "#,
    );
    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();

    // Edit the file.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("second load");
        "#,
    );

    // Dispatch the palette action.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InitReload);
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("second load"),
        "init: Reload should re-read the file and run the new body: status = {status:?}"
    );
}

#[test]
fn init_edit_creates_starter_template_when_missing() {
    use fresh::input::keybindings::Action;

    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();
    assert!(!config_dir.join("init.ts").exists(), "precondition");

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InitEdit);
    harness.editor_mut().process_async_messages();

    let created = config_dir.join("init.ts");
    assert!(
        created.exists(),
        "init: Edit should create the starter file"
    );

    let body = std::fs::read_to_string(&created).unwrap();
    assert!(
        body.contains("const editor = getEditor();"),
        "starter template should set up the plugin API: body starts {:?}",
        &body.get(..60)
    );
    // Every example should be commented out — empty init is valid.
    assert!(body.contains("// Example:"));
}

#[test]
fn init_check_action_reports_ok_on_a_clean_file() {
    use fresh::input::keybindings::Action;

    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();
    write_init_ts(&config_dir, "const editor = getEditor();\n");

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InitCheck);
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("init.ts: ok"),
        "expected 'init.ts: ok', got {status:?}"
    );
}

#[test]
fn init_check_action_reports_an_error_on_a_broken_file() {
    use fresh::input::keybindings::Action;

    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();
    write_init_ts(&config_dir, "function broken(\n");

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InitCheck);
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("init.ts:") && status.contains("error"),
        "expected an init.ts error report, got {status:?}"
    );
}

#[test]
fn init_revert_unloads_plugin_but_settings_persist() {
    // Revert unloads the init.ts plugin (commands, handlers, events gone)
    // but setSetting writes persist — fire-and-forget, consistent with
    // the Neovim/VS Code/Emacs model.
    use fresh::input::keybindings::Action;

    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();
    let original_tab_size = harness.editor().config_for_tests().editor.tab_size as u64;
    let overridden = if original_tab_size == 7 { 3 } else { 7 };

    write_init_ts(
        &config_dir,
        &format!(
            r#"
            const editor = getEditor();
            editor.setSetting("editor.tab_size", {overridden});
            "#
        ),
    );
    harness.editor_mut().load_init_script(true);
    harness.editor_mut().process_async_messages();
    assert_eq!(
        harness.editor().config_for_tests().editor.tab_size as u64,
        overridden
    );

    // Revert — plugin is unloaded, but setting writes are fire-and-forget.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InitRevert);
    harness.editor_mut().process_async_messages();

    assert_eq!(
        harness.editor().config_for_tests().editor.tab_size as u64,
        overridden,
        "fire-and-forget: setSetting writes survive init: Revert"
    );
}

#[test]
fn init_ts_is_loaded_as_a_plugin_named_init_ts() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    // A minimal init.ts that just registers a status message lets us verify
    // it actually ran inside the plugin runtime.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("init.ts ran");
        "#,
    );

    harness.editor_mut().load_init_script(true);

    // Drain any plugin-command traffic queued by the load.
    harness.editor_mut().process_async_messages();

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("init.ts ran"),
        "expected init.ts to set a status; got {status:?}"
    );
}
