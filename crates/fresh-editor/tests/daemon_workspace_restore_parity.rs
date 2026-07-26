//! Every invocation sees and opens the same workspaces (issues #2808, #2811).
//!
//! Direct mode, an unnamed working-directory daemon (`fresh -a`) and a named
//! daemon (`fresh -a NAME`) are all hosts for one shared set of workspaces —
//! none of them owns a private copy.
//!
//! Reported symptom: build a set of Orchestrator workspaces in a direct-mode
//! run (`fresh`), quit, then come back through `fresh -a`. The dock listed every
//! workspace — boot discovery scans the per-directory store in both modes — but
//! each came up holding a single empty `[No Name]` buffer, and switching between
//! them never changed the buffer view.
//!
//! Daemons used to persist into `session-workspaces/<name>.json`, a store keyed
//! on the daemon's name that could hold only one window and that discovery never
//! scanned. An unnamed daemon was pushed into it by a basename fallback (#2808);
//! a named one lived there by design (#2811). Either way the workspaces
//! discovery had just enumerated were looked up somewhere nothing had written.
//!
//! Everything is asserted on rendered output, driving the dock with the same
//! keys a user presses (CONTRIBUTING: "E2E Tests Observe, Not Inspect").
//!
//! Lives in its own integration binary because it sets the process-global
//! `XDG_DATA_HOME` to isolate persistence: workspace save/load key off
//! `$XDG_DATA_HOME/fresh` while boot discovery reads
//! `DirectoryContext::data_dir`, so both must name one isolated tree. That
//! global is also why this is a single `#[test]` — two tests in this binary
//! would race each other's `set_var`. See `orchestrator_co_tenant_restore.rs`
//! for the same pattern. Linux-gated: `dirs::data_dir()` ignores
//! `XDG_DATA_HOME` elsewhere.
#![cfg(target_os = "linux")]

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::path::{Path, PathBuf};

/// Isolate ALL editor persistence into `base`: `$XDG_DATA_HOME/fresh` is where
/// `Workspace::save`/`load` live, and the returned `DirectoryContext`'s
/// `data_dir` is the SAME path — so the direct-mode run's saves and the
/// daemon-mode run's boot discovery agree, inside the test's temp tree.
fn isolated_dir_context(base: &Path) -> DirectoryContext {
    let xdg_data = base.join("xdg-data");
    std::fs::create_dir_all(&xdg_data).unwrap();
    std::env::set_var("XDG_DATA_HOME", &xdg_data);
    DirectoryContext {
        data_dir: xdg_data.join("fresh"),
        config_dir: base.join("config"),
        home_dir: Some(base.join("home")),
        documents_dir: None,
        downloads_dir: None,
    }
}

fn harness_in(project: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .with_config(config)
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

fn json_files_in(dir: &Path) -> Vec<PathBuf> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let mut out: Vec<PathBuf> = entries
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "json"))
        .collect();
    out.sort();
    out
}

#[test]
fn every_invocation_sees_and_opens_the_same_workspaces() {
    fresh::i18n::set_locale("en");
    let sandbox = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(sandbox.path());
    let workspaces_dir = dir_context.data_dir.join("workspaces");

    let mk = |n: &str| {
        let p = sandbox.path().join(n);
        std::fs::create_dir_all(&p).unwrap();
        p.canonicalize().unwrap()
    };
    let alpha_root = mk("alpha");
    let beta_root = mk("beta");
    let alpha_file = alpha_root.join("alpha_one.rs");
    let beta_file = beta_root.join("beta_one.md");
    std::fs::write(&alpha_file, "ALPHA_MARKER\n").unwrap();
    std::fs::write(&beta_file, "BETA_MARKER\n").unwrap();

    // Direct mode: one workspace per project root, each with its own file open.
    // This is the state `fresh` persists on quit.
    {
        let mut h = harness_in(&alpha_root, &dir_context);
        h.startup(true, &[]).unwrap();
        h.open_file(&alpha_file).unwrap();

        let beta_win = h
            .editor_mut()
            .create_window_at(beta_root.clone(), "beta".to_string());
        h.editor_mut().set_active_window(beta_win);
        h.open_file(&beta_file).unwrap();

        h.editor_mut().save_all_windows_workspaces().unwrap();
    }

    // Two workspaces are in the per-directory store — the only store boot
    // discovery enumerates.
    assert_eq!(
        json_files_in(&workspaces_dir).len(),
        2,
        "direct mode must persist one workspace file per window, got: {:?}",
        json_files_in(&workspaces_dir)
    );

    // Every way of launching must now see and open that same set: a
    // direct-mode run, an unnamed working-directory daemon, and a *named*
    // daemon, which used to keep a private store of its own.
    for daemon in [
        DaemonKind::Direct,
        DaemonKind::WorkingDirectory,
        DaemonKind::Named("build-01"),
    ] {
        let mut h = harness_in(&alpha_root, &dir_context);
        daemon.apply(&mut h);

        let restored = h.startup(true, &[]).unwrap();
        assert!(
            restored,
            "{daemon:?} must restore the workspace saved for this root"
        );
        h.render().unwrap();

        // The active workspace came back: its file's content is on screen, not
        // the empty unnamed buffer a daemon used to show.
        h.assert_screen_contains("ALPHA_MARKER");
        h.assert_screen_not_contains("[No Name]");

        // Switch workspaces the way a user does — run "Next Window" from the
        // command palette — and the buffer view must follow to the other
        // workspace's own file. This is the part that never changed before:
        // every workspace rendered the same empty buffer. (The Orchestrator
        // dock drives the same window switch; it lives in a plugin, and this
        // binary runs without plugins to keep the core restore path isolated.)
        h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        h.wait_for_prompt().unwrap();
        h.type_text("Next Window").unwrap();
        h.wait_until(|h| h.screen_to_string().contains("Next Window"))
            .unwrap();
        h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| h.screen_to_string().contains("BETA_MARKER"))
            .unwrap();

        // The other workspace opened its OWN file, not a second copy of the
        // active one.
        h.assert_screen_not_contains("ALPHA_MARKER");
    }

    // A layout a named daemon wrote *before* the stores were unified must not
    // be orphaned by the change: boot migration folds it into the shared set,
    // where it opens like any other workspace.
    let legacy_root = mk("legacy");
    let legacy_file = legacy_root.join("legacy_one.txt");
    std::fs::write(&legacy_file, "LEGACY_MARKER\n").unwrap();
    {
        // Build a real snapshot of that project, then move it into the retired
        // daemon-scoped store the way a pre-unification daemon would have left
        // it behind.
        let before: Vec<PathBuf> = json_files_in(&workspaces_dir);
        let mut h = harness_in(&legacy_root, &dir_context);
        h.startup(true, &[]).unwrap();
        h.open_file(&legacy_file).unwrap();
        h.editor_mut().save_all_windows_workspaces().unwrap();

        let written: Vec<PathBuf> = json_files_in(&workspaces_dir)
            .into_iter()
            .filter(|p| !before.contains(p))
            .collect();
        assert_eq!(
            written.len(),
            1,
            "expected one new workspace file to convert"
        );
        let legacy_dir = dir_context.data_dir.join("session-workspaces");
        std::fs::create_dir_all(&legacy_dir).unwrap();
        std::fs::copy(&written[0], legacy_dir.join("build-01.json")).unwrap();
        std::fs::remove_file(&written[0]).unwrap();
    }

    // Any invocation now finds it — here the plainest one, direct mode.
    let mut h = harness_in(&legacy_root, &dir_context);
    h.startup(true, &[]).unwrap();
    h.render().unwrap();
    h.assert_screen_contains("LEGACY_MARKER");

    assert!(
        json_files_in(&dir_context.data_dir.join("session-workspaces")).is_empty(),
        "the daemon-scoped store should be retired once its contents are folded in"
    );
}

/// How the editor under test was launched. Only the daemon *name* ever scoped
/// persistence; the rest is display and cursor rendering.
#[derive(Debug, Clone, Copy)]
enum DaemonKind {
    /// `fresh` — no daemon at all.
    Direct,
    /// `fresh -a` — a daemon addressed by its working directory, so it has a
    /// status-bar label but no name.
    WorkingDirectory,
    /// `fresh -a NAME` / `daemon new NAME` — addressed by name.
    Named(&'static str),
}

impl DaemonKind {
    fn apply(self, h: &mut EditorTestHarness) {
        match self {
            DaemonKind::Direct => {}
            DaemonKind::WorkingDirectory => {
                h.editor_mut().set_session_mode(true);
                h.editor_mut()
                    .set_session_display_name(Some("alpha".into()));
            }
            DaemonKind::Named(name) => {
                h.editor_mut().set_session_mode(true);
                h.editor_mut().set_session_name(Some(name.into()));
                h.editor_mut().set_session_display_name(Some(name.into()));
            }
        }
    }
}
