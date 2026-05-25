//! E2E coverage for attaching Orchestrator sessions to *existing*
//! git worktrees — both ways the feature surfaces:
//!
//! 1. **Discovery**: opening the Orchestrator Open dialog scans the
//!    worktrees of every known project (`git worktree list`, run
//!    per repo) and lists the ones that aren't open yet as `[○]`
//!    rows. The user can dive one to open a session there without
//!    creating it by hand.
//!
//! 2. **Form attach hint**: pointing the New Session form's Project
//!    Path at an existing linked worktree surfaces an "existing
//!    worktree" hint, signalling that submitting will attach to it
//!    (managed) rather than fork a fresh worktree.
//!
//! Both behaviours are new: on the pre-change plugin the dialog only
//! ever listed live windows, and the form had no notion of an
//! existing worktree, so these screens never appeared.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::path::{Path, PathBuf};
use std::process::Command;

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

/// Run a git subcommand in `cwd`, panicking with stderr on failure.
fn git(cwd: &Path, args: &[&str]) {
    let out = Command::new("git")
        .args(args)
        .current_dir(cwd)
        .output()
        .unwrap_or_else(|e| panic!("failed to spawn git {:?}: {}", args, e));
    assert!(
        out.status.success(),
        "git {:?} failed: {}",
        args,
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Build a git repo with one extra linked worktree on branch
/// `feature-x`, plus the orchestrator plugin installed in the repo's
/// `plugins/` dir. Returns (tempdir guard, repo path, worktree path).
/// The worktree is a sibling of the repo so it sits outside the
/// editor's working dir (discovery finds it via git, not the tree).
fn set_up_repo_with_worktree() -> (tempfile::TempDir, PathBuf, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    let repo = root.join("mainrepo");
    std::fs::create_dir(&repo).unwrap();

    git(&repo, &["init", "-q"]);
    git(&repo, &["config", "user.name", "Test User"]);
    git(&repo, &["config", "user.email", "test@example.com"]);
    git(&repo, &["config", "commit.gpgsign", "false"]);
    std::fs::write(repo.join("file.txt"), "hello\n").unwrap();
    git(&repo, &["add", "file.txt"]);
    git(&repo, &["commit", "-qm", "init"]);
    git(&repo, &["branch", "feature-x"]);

    let worktree = root.join("existing-wt");
    git(
        &repo,
        &["worktree", "add", worktree.to_str().unwrap(), "feature-x"],
    );

    let plugins_dir = repo.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    (temp, repo, worktree)
}

fn wait_for_command(harness: &mut EditorTestHarness, name: &str) {
    let owned = name.to_string();
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            reg.get_all()
                .iter()
                .any(|c| c.get_localized_name() == owned)
        })
        .unwrap();
}

fn open_orchestrator_dialog(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Orchestrator: Open").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Orchestrator: Open"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR :: Sessions"))
        .unwrap();
}

fn open_new_session_form(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Orchestrator: New Session").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Orchestrator: New Session"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR :: New Session"))
        .unwrap();
}

/// Opening the dialog discovers the on-disk `feature-x` worktree and
/// lists it as an `[○]` row labelled with its branch — even though no
/// session was ever opened there.
#[test]
fn open_dialog_discovers_existing_worktree() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);

    // The discovered worktree row carries the `[○]` on-disk glyph and
    // its branch name. The async per-project `git worktree list` scan
    // lands a beat after the dialog opens, so wait for it.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("[○]")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Open dialog should discover the on-disk `feature-x` worktree as an \
                 `[○]` row.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Selecting the discovered worktree row shows the on-disk preview
/// panel — the "On-disk worktree" header and the "Press Enter to
/// open" affordance — rather than a live window embed.
#[test]
fn discovered_worktree_preview_offers_open() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("[○]"))
        .unwrap();

    // The discovered row is the only non-base session; navigate to it
    // and confirm its preview pane describes the open-by-attach flow.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("On-disk worktree") && s.contains("Press Enter to open")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Discovered-worktree preview pane should describe the open-by-attach \
                 flow.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Diving a discovered worktree opens a real session there: the
/// `[○]` placeholder is replaced by a live, numeric-id row at the
/// worktree (no `⇄` shared badge — it's managed as the worktree it
/// is). Reproduces the headline "attach to existing worktree" flow.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // attach spawns a Unix shell terminal.
fn diving_discovered_worktree_attaches_managed_session() {
    if !pty_available() {
        eprintln!("skipping: no PTY available in this environment");
        return;
    }
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    // The discovered row sorts to the top (synthetic negative id);
    // wait for it, then dive it.
    harness
        .wait_until(|h| h.screen_to_string().contains("[○]"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Attach is async (`createWindowWithTerminal`). Synchronize on the
    // new window existing before reopening so the dialog's one-shot
    // discovery scan sees the worktree as live, not on-disk.
    harness
        .wait_until(|h| h.editor().session_count() >= 2)
        .unwrap();

    // Reopen the dialog. The worktree is now a live session, so it
    // lists with a numeric id and the discovery scan no longer
    // surfaces it as `[○]`.
    open_orchestrator_dialog(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && !s.contains("[○]")
        })
        .unwrap_or_else(|_| {
            panic!(
                "After diving the discovered worktree it should appear as a live \
                 (non-`[○]`) session.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });

    // The attached worktree is managed, not shared: no `⇄` badge.
    let screen = harness.screen_to_string();
    let feature_line = screen
        .lines()
        .find(|l| l.contains("feature-x") && l.contains('['))
        .unwrap_or("");
    assert!(
        !feature_line.contains('⇄'),
        "attached worktree session must not be flagged shared (`⇄`).\nRow: {}\nScreen:\n{}",
        feature_line,
        screen,
    );
}

/// Pointing the New Session form's Project Path at an existing linked
/// worktree surfaces the "existing worktree" attach hint.
#[test]
fn new_session_form_hints_existing_worktree() {
    let (_temp, repo, wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: New Session");

    open_new_session_form(&mut harness);

    // Type the worktree path into the focused Project Path field. The
    // debounced probe classifies it as a linked worktree and renders
    // the attach hint.
    harness.type_text(wt.to_str().unwrap()).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("existing worktree"))
        .unwrap_or_else(|_| {
            panic!(
                "New Session form should hint that Project Path is an existing \
                 worktree.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}
