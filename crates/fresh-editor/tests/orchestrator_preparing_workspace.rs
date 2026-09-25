//! Reproducers for the "new workspace steals focus when its worktree lands"
//! bug, and for the empty-shell page that replaces the wait.
//!
//! Creating an Orchestrator workspace runs `git worktree add`, which on a
//! large repo or a slow disk runs long enough that the user goes off and does
//! something else. The old flow left them on the *previous* workspace for the
//! whole wait and then yanked focus into the new one at whatever moment the
//! worktree happened to land.
//!
//! A workspace is now a real `Window` from the moment it is asked for — it
//! just has nothing to show yet, so it paints a progress page (the same shape
//! a not-yet-connected remote session shows). Focus is settled *then*, at the
//! click, and the dive that seeding the terminal performs is undone against
//! wherever the user actually is by the time the build finishes.
//!
//! Both tests drive the host the way the Orchestrator plugin does and assert
//! on what is on screen. The one state assertion — that adopting a
//! placeholder keeps its durable `stable_id` — is the invariant the plugin's
//! mid-build rename and folder assignment are keyed on, and it is not
//! visible on screen from the host side, so it is checked directly.

#![cfg(feature = "plugins")]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh_core::api::PluginCommand;
use portable_pty::{native_pty_system, PtySize};

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

fn harness_at(project: &std::path::Path) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        36,
        HarnessOptions::new()
            .with_working_dir(project.to_path_buf())
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

/// "Create & Visit" commits the switch **up front**: the new workspace is on
/// screen — saying what it is doing — before any of the work behind it has
/// run, and the workspace the user came from is gone from the screen.
#[test]
fn creating_a_workspace_commits_the_switch_before_the_worktree_exists() {
    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let project = base.path().join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();

    let mut h = harness_at(&project);
    // A visible buffer in the workspace the user is leaving — the thing that
    // must leave the screen the moment the create commits.
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();

    h.editor_mut()
        .handle_plugin_command(PluginCommand::CreatePreparingWindow {
            root: project.clone(),
            label: "wip-workspace".into(),
            message: "Adding worktree…".into(),
            activate: true,
            request_id: 1,
        })
        .unwrap();

    h.wait_until(|h| !h.screen_to_string().contains("local_marker.txt"))
        .unwrap();
    // The workspace names itself and says what it is waiting on, rather than
    // presenting the empty scratch buffer it technically holds.
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("wip-workspace")
            && scr.contains("Adding worktree")
            && scr.contains("will open as soon as it has been created")
            && !scr.contains("[No Name]")
    })
    .unwrap();

    // Nothing here is editable before the workspace exists: typing must be
    // swallowed by the read-only placeholder, not land in a hidden buffer.
    h.type_text("XYZ").unwrap();
    h.process_async_and_render().unwrap();
    assert!(
        !h.get_buffer_content().unwrap_or_default().contains("XYZ"),
        "typing into the placeholder page must not edit anything"
    );
    h.assert_screen_not_contains("XYZ");
}

/// The build finishing must not move anyone. A workspace created in the
/// background — or one the user has since navigated away from — seeds its
/// terminal without dragging the keyboard along with it, which is the whole
/// point of settling focus at the click instead of at completion.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell as the agent command.
fn finishing_a_background_create_leaves_the_user_where_they_are() {
    if !pty_available() {
        eprintln!("Skipping background-create focus test: PTY not available");
        return;
    }
    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let project = base.path().join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();

    let mut h = harness_at(&project);
    h.open_file(&project.join("local_marker.txt")).unwrap();
    h.wait_for_screen_contains("local_marker.txt").unwrap();

    // "Create in Background": the placeholder opens without taking focus.
    let placeholder = h.editor_mut().open_preparing_window(
        project.clone(),
        "wip-workspace".into(),
        "Adding worktree…".into(),
    );
    h.process_async_and_render().unwrap();
    h.assert_screen_contains("local_marker.txt");

    let stable_id = h
        .editor()
        .session(placeholder)
        .expect("the placeholder is a real window")
        .stable_id
        .clone();
    assert!(
        !stable_id.is_empty(),
        "a placeholder is minted with a durable workspace id, so a rename or a \
         folder filed against it mid-build has something to hold onto"
    );

    // The build finishes and seeds the workspace's terminal.
    let authority = h.editor().local_session_authority(&project);
    let adopted = h
        .editor_mut()
        .create_window_with_terminal(
            project.clone(),
            "wip-workspace".into(),
            Some(project.clone()),
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            Some("agent".into()),
            std::sync::Arc::new(fresh::services::authority::Connection::plain(authority)),
            None,
            None,
            false,
            Some(placeholder),
        )
        .expect("adopting the placeholder should succeed")
        .0;
    h.process_async_and_render().unwrap();

    // The user never left the workspace they were working in.
    h.assert_screen_contains("local_marker.txt");
    h.assert_screen_not_contains("Adding worktree");

    assert_eq!(
        adopted, placeholder,
        "the live session takes over the placeholder's window id rather than \
         opening a second workspace beside it"
    );
    assert_eq!(
        h.editor()
            .session(adopted)
            .expect("the adopted window is live")
            .stable_id,
        stable_id,
        "the durable workspace id survives the adopt — a rename or folder filed \
         against the workspace while it was building must still point at it"
    );
    assert!(
        !h.editor().is_window_preparing(adopted),
        "a workspace with its terminal seeded is no longer 'being created'"
    );
}

/// Counts WARN events from `fresh::app::pane_mirror` on the thread it is
/// installed on — the harness renders on the test thread, so a scoped
/// subscriber sees exactly this test's layouts.
struct PaneMirrorWarnings(std::sync::Arc<std::sync::atomic::AtomicUsize>);

impl<S: tracing::Subscriber> tracing_subscriber::Layer<S> for PaneMirrorWarnings {
    fn on_event(
        &self,
        event: &tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        let meta = event.metadata();
        if *meta.level() == tracing::Level::WARN && meta.target() == "fresh::app::pane_mirror" {
            self.0.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        }
    }
}

/// The Orchestrator describes a building workspace's page itself, by mounting
/// a widget panel on the placeholder window's seed buffer. That buffer is the
/// window's hidden scratch buffer, not a plugin-composed one, so the pane
/// mirror has nothing it may write there — and used to try on every layout,
/// logging a warning each time for as long as the page was up (#3406).
#[test]
fn a_placeholder_page_panel_is_not_mirrored_into_the_seed_buffer() {
    use tracing_subscriber::layer::SubscriberExt;

    fresh::i18n::set_locale("en");
    let base = tempfile::tempdir().unwrap();
    let project = base.path().join("project");
    std::fs::create_dir_all(&project).unwrap();
    let project = project.canonicalize().unwrap();

    let warnings = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let subscriber = tracing_subscriber::registry().with(PaneMirrorWarnings(warnings.clone()));
    let _guard = tracing::subscriber::set_default(subscriber);

    let mut h = harness_at(&project);
    let placeholder = h.editor_mut().open_preparing_window(
        project.clone(),
        "wip-workspace".into(),
        "Adding worktree…".into(),
    );
    h.editor_mut().set_active_window(placeholder);
    h.process_async_and_render().unwrap();
    let seed = h
        .editor()
        .session(placeholder)
        .expect("the placeholder is a real window")
        .active_buffer();

    let spec: fresh_core::api::WidgetSpec = serde_json::from_value(serde_json::json!({
        "kind": "label",
        "text": "Plugin-described placeholder page",
        "labelWidth": 0,
    }))
    .unwrap();
    h.editor_mut()
        .handle_plugin_command(PluginCommand::MountWidgetPanel {
            plugin: "orchestrator".into(),
            panel_id: 900_002,
            buffer_id: seed,
            spec,
            options: Default::default(),
        })
        .unwrap();
    h.wait_for_screen_contains("Plugin-described placeholder page")
        .unwrap();
    // A few more layouts, as the page would get while it is up.
    for _ in 0..3 {
        h.process_async_and_render().unwrap();
    }

    assert_eq!(
        warnings.load(std::sync::atomic::Ordering::SeqCst),
        0,
        "laying out a panel mounted on a placeholder's seed buffer must not \
         try to mirror it into that buffer"
    );
}
