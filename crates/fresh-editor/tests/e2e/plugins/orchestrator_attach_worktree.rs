//! E2E coverage for attaching Orchestrator sessions to *existing*
//! git worktrees — both ways the feature surfaces:
//!
//! 1. **Discovery**: opening the Orchestrator Open dialog scans the
//!    worktrees of every known project (`git worktree list`, run
//!    per repo) and lists the ones that aren't open yet as `· on-disk`
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

/// Canonicalize a path to the form git and the editor both accept.
///
/// `std::fs::canonicalize` returns a `\\?\C:\…` *verbatim* path on
/// Windows; git refuses it (`git worktree add` can't create leading
/// directories under `//?/C:/…`). Strip the verbatim prefix so we get
/// a plain `C:\…` path. On Unix this is just `canonicalize` (which
/// also resolves the macOS `/var → /private/var` symlink so the test
/// path matches what `git rev-parse` reports).
fn canonical_dir(p: &Path) -> PathBuf {
    let c = std::fs::canonicalize(p).expect("canonicalize tempdir");
    #[cfg(windows)]
    {
        // Tempdirs are always local-disk, so the verbatim form is
        // `\\?\C:\…`; dropping the 4-char prefix yields `C:\…`.
        if let Some(rest) = c.to_str().and_then(|s| s.strip_prefix(r"\\?\")) {
            return PathBuf::from(rest);
        }
    }
    c
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
    let root = canonical_dir(temp.path());
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

/// Like `set_up_repo_with_worktree` but adds two linked worktrees
/// (`feature-x`, `feature-y`) so multi-select / bulk flows have more
/// than one discovered row to work with. Returns (guard, repo, wt1,
/// wt2).
fn set_up_repo_with_two_worktrees() -> (tempfile::TempDir, PathBuf, PathBuf, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let root = canonical_dir(temp.path());
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
    git(&repo, &["branch", "feature-y"]);

    let wt1 = root.join("existing-wt-x");
    git(
        &repo,
        &["worktree", "add", wt1.to_str().unwrap(), "feature-x"],
    );
    let wt2 = root.join("existing-wt-y");
    git(
        &repo,
        &["worktree", "add", wt2.to_str().unwrap(), "feature-y"],
    );

    let plugins_dir = repo.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    (temp, repo, wt1, wt2)
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

/// Discovered on-disk worktrees are hidden by default; flip the "Show
/// all worktrees" toggle on via its Alt+T chord so the worktree-
/// dependent assertions have rows to find. Idempotent — a no-op when
/// the toggle already reads checked (`[v]`).
fn ensure_worktrees_shown(harness: &mut EditorTestHarness) {
    if harness
        .screen_to_string()
        .contains("[ ] Show all worktrees")
    {
        harness
            .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
            .unwrap();
        harness.tick_and_render().ok();
    }
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

/// Move the list highlight down onto the discovered on-disk worktree
/// row (which now sorts after the live sessions). Down routes to the
/// list via the host's smart-key dispatch even though focus sits on a
/// button. Stops once the on-disk preview pane is showing.
fn navigate_to_discovered_row(harness: &mut EditorTestHarness) {
    for _ in 0..12 {
        if harness.screen_to_string().contains("On-disk worktree") {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.tick_and_render().ok();
    }
    harness
        .wait_until(|h| h.screen_to_string().contains("On-disk worktree"))
        .unwrap_or_else(|_| {
            panic!(
                "could not navigate to the discovered on-disk row.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Opening the dialog discovers the on-disk `feature-x` worktree and
/// lists it as an on-disk row labelled with its branch — even though no
/// session was ever opened there.
#[test]
fn open_dialog_discovers_existing_worktree() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);

    // The discovered worktree row carries the `· on-disk` tag and its
    // branch name. The async per-project `git worktree list` scan
    // lands a beat after the dialog opens, so wait for it.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Open dialog should discover the on-disk `feature-x` worktree as a \
                 `· on-disk` row.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Selecting the discovered worktree row shows the on-disk preview
/// panel — the "On-disk worktree" header and the "click / Enter to
/// open" affordance — rather than a live window embed.
#[test]
fn discovered_worktree_preview_offers_open() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("· on-disk"))
        .unwrap();

    // The discovered row sorts after the live sessions now; navigate
    // onto it and confirm its preview pane describes the open-by-attach
    // flow.
    navigate_to_discovered_row(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("On-disk worktree") && s.contains("to open this worktree as a session")
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
/// `· on-disk` row is replaced by a live session row at the worktree
/// (no `⇄` shared badge — it's managed as the worktree it is).
/// Reproduces the headline "attach to existing worktree" flow.
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
    ensure_worktrees_shown(&mut harness);
    // Discovered worktrees now sort *after* the live sessions, so the
    // `· on-disk` row isn't the default selection. Wait for it, then
    // move the highlight down onto it (Down routes to the list even
    // though focus sits on a button) until its on-disk preview shows.
    harness
        .wait_until(|h| h.screen_to_string().contains("· on-disk"))
        .unwrap();
    navigate_to_discovered_row(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Attach is async (`createWindowWithTerminal`). Synchronize on the
    // new window existing before reopening so the dialog's one-shot
    // discovery scan sees the worktree as live, not on-disk.
    harness
        .wait_until(|h| h.editor().session_count() >= 2)
        .unwrap();

    // Reopen the dialog. The worktree is now a live session, so the
    // discovery scan no longer surfaces it as a `· on-disk` row.
    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && !s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "After diving the discovered worktree it should appear as a live \
                 (non-`· on-disk`) session.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });

    // The attached worktree is managed, not shared: no `⇄` badge.
    let screen = harness.screen_to_string();
    let feature_line = screen
        .lines()
        .find(|l| l.contains("feature-x") && l.contains("[ ]"))
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

/// The session list sorts by a *stable* key: the project's own checkout
/// (the `mainrepo` base) sits above its linked worktrees, then rows are
/// alphabetical. This grouping is by `root == projectPath`, NOT by
/// live/discovered state — so a worktree keeps its place when opened (see
/// `dock_opening_worktree_keeps_its_row_position`) instead of jumping into
/// a "live" group. Here the `mainrepo` base row stays above the discovered
/// `feature-x` worktree. The base row is identified by its `[ ]` checkbox
/// and the absence of the `· on-disk` tag (which marks the worktree).
#[test]
fn rows_sort_stably_main_checkout_above_worktrees() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("· on-disk"))
        .unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let base_idx = lines
        .iter()
        .position(|l| l.contains("[ ]") && l.contains("mainrepo") && !l.contains("· on-disk"));
    let disc_idx = lines.iter().position(|l| l.contains("· on-disk"));
    assert!(
        base_idx.is_some() && disc_idx.is_some() && base_idx < disc_idx,
        "main checkout (mainrepo) must sort above its worktrees, independent \
         of live/discovered state.\nbase_idx={:?} disc_idx={:?}\nScreen:\n{}",
        base_idx,
        disc_idx,
        screen
    );
}

/// Space-selecting two rows shows the dedicated bulk selection bar
/// (Layout B) with per-action counts. Uses the two discovered
/// worktree rows (selectable, no PTY needed). Space is the rebindable
/// `orchestrator_toggle_select` mode chord, so it fires regardless of
/// which control holds focus.
#[test]
fn space_selects_rows_and_shows_bulk_bar() {
    let (_temp, repo, _wt1, _wt2) = set_up_repo_with_two_worktrees();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    // Wait for both discovered worktrees to land.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("feature-y") && s.contains("· on-disk")
        })
        .unwrap();

    // Highlight the first discovered row and check it; move down and
    // check the second.
    navigate_to_discovered_row(&mut harness);
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness.tick_and_render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Bulk actions") && s.contains("2 selected") && s.contains("Delete (2)")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Selecting two rows should show the bulk bar with `Delete (2)`.\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Bulk-deleting two checked discovered worktrees runs `git worktree
/// remove` on both, so their directories disappear from disk. Drives
/// the selection → Delete (2) → Confirm Delete flow entirely from the
/// keyboard.
#[test]
fn bulk_delete_removes_selected_worktrees() {
    let (_temp, repo, wt1, wt2) = set_up_repo_with_two_worktrees();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("feature-y") && s.contains("· on-disk")
        })
        .unwrap();

    // Check both discovered rows.
    navigate_to_discovered_row(&mut harness);
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness.tick_and_render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Delete (2)"))
        .unwrap();

    // Entering bulk mode lands focus on `Archive`; Tab to `Delete`
    // (Stop is disabled for discovered rows, so it's out of the Tab
    // cycle), Enter to open the confirm panel.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Confirm Delete"))
        .unwrap_or_else(|_| {
            panic!(
                "Delete (2) should open the bulk Confirm Delete panel.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });

    // Confirm panel defaults focus to Cancel; Tab to `Confirm Delete`
    // and activate.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Both worktree directories should be removed from disk.
    harness
        .wait_until(|_| !wt1.exists() && !wt2.exists())
        .unwrap_or_else(|_| {
            panic!(
                "bulk delete should `git worktree remove` both worktrees.\n\
                 wt1.exists()={} wt2.exists()={}\nScreen:\n{}",
                wt1.exists(),
                wt2.exists(),
                harness.screen_to_string()
            )
        });
}

/// Build a repo with `n` linked worktrees `feat-1..feat-n`, plus the
/// orchestrator plugin installed. Used to overflow the session list so
/// it grows a scrollbar.
fn set_up_repo_with_many_worktrees(n: usize) -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let root = canonical_dir(temp.path());
    let repo = root.join("mainrepo");
    std::fs::create_dir(&repo).unwrap();
    git(&repo, &["init", "-q"]);
    git(&repo, &["config", "user.name", "Test User"]);
    git(&repo, &["config", "user.email", "test@example.com"]);
    git(&repo, &["config", "commit.gpgsign", "false"]);
    std::fs::write(repo.join("file.txt"), "hello\n").unwrap();
    git(&repo, &["add", "file.txt"]);
    git(&repo, &["commit", "-qm", "init"]);
    for i in 1..=n {
        let branch = format!("feat-{i}");
        git(&repo, &["branch", &branch]);
        let wt = root.join(format!("wt-{i}"));
        git(&repo, &["worktree", "add", wt.to_str().unwrap(), &branch]);
    }
    let plugins_dir = repo.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    (temp, repo)
}

/// The set of `feat-N` labels currently rendered in the Sessions
/// list column (the left pane). At open, nothing is selected so the
/// preview pane shows the base session — `feat-` only appears in the
/// list rows.
fn visible_feats(screen: &str) -> std::collections::BTreeSet<String> {
    let mut out = std::collections::BTreeSet::new();
    for line in screen.lines() {
        // Each "feat-<n>" token followed by a space/non-digit.
        let mut rest = line;
        while let Some(pos) = rest.find("feat-") {
            let after = &rest[pos + "feat-".len()..];
            let digits: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() {
                out.insert(format!("feat-{digits}"));
            }
            rest = &after[digits.len().max(1).min(after.len())..];
        }
    }
    out
}

/// A long session list grows a draggable scrollbar; clicking near the
/// bottom of its track scrolls the list (the canonical `ScrollbarMouse`
/// press path), surfacing rows that were hidden at the top. Runs in a
/// short terminal so the ~16 rows overflow the visible height.
#[test]
fn scrollbar_click_scrolls_the_session_list() {
    let (_temp, repo) = set_up_repo_with_many_worktrees(15);
    // 16 sessions rendered as multi-row cards far exceed the visible
    // height, so the list overflows and a scrollbar is drawn. The
    // terminal is tall enough that a few whole cards (each carrying a
    // `· on-disk` line) are visible to confirm discovery landed.
    let mut harness = EditorTestHarness::with_working_dir(160, 40, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    ensure_worktrees_shown(&mut harness);
    // Wait for the discovery scan to fold in the on-disk worktrees,
    // and for the screen to settle, so the full set is present before
    // we snapshot.
    harness
        .wait_until_stable(|h| h.screen_to_string().matches("· on-disk").count() >= 3)
        .unwrap();

    let before = visible_feats(&harness.screen_to_string());
    assert!(
        !before.is_empty(),
        "expected discovered feat- rows.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Locate the Sessions box: its top-border `╮` marks the right
    // edge; the scrollbar sits in the column just inside it. Click one
    // row above the box's bottom border — the bottom of the track.
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let top_border_row = lines
        .iter()
        .position(|l| l.contains("╭─ Sessions"))
        .expect("Sessions box top border");
    let border_col = lines[top_border_row]
        .chars()
        .position(|c| c == '╮')
        .expect("Sessions box right corner");
    let bottom_border_row = lines
        .iter()
        .skip(top_border_row + 1)
        .position(|l| l.contains('╰'))
        .map(|p| p + top_border_row + 1)
        .expect("Sessions box bottom border");
    // Scrollbar column = the last content column. The section wraps
    // its child as `│ <content> │` — a 2-column ` │` suffix — so the
    // scrollbar sits at `border_col - 2`, not directly under the
    // border. Click row = last track row (just above the bottom
    // border). (Verified interactively: border `╮` at col N ⇒ thumb at
    // col N-2.)
    let sb_col = (border_col.saturating_sub(2)) as u16;
    let click_row = (bottom_border_row.saturating_sub(1)) as u16;

    harness.mouse_click(sb_col, click_row).unwrap();
    harness
        .wait_until(|h| visible_feats(&h.screen_to_string()) != before)
        .unwrap_or_else(|_| {
            panic!(
                "clicking the scrollbar track (col {sb_col}, row {click_row}) should \
                 scroll the session list to a different set of rows.\n\
                 before={:?}\nScreen:\n{}",
                before,
                harness.screen_to_string()
            )
        });
}

/// On-disk worktrees are hidden by default; the "Show all worktrees"
/// checkbox (unchecked at open) reveals them, and its Alt+T chord
/// toggles it. Verifies the inverted default + rebindable keybinding.
#[test]
fn worktrees_hidden_by_default_until_show_toggled() {
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    // NB: open WITHOUT `ensure_worktrees_shown` — we're testing the
    // default-hidden state here.
    open_orchestrator_dialog(&mut harness);

    // The toggle renders unchecked (`[ ]`), and no on-disk row shows.
    assert!(
        harness
            .screen_to_string()
            .contains("[ ] Show all worktrees"),
        "the worktree toggle should default to unchecked.\nScreen:\n{}",
        harness.screen_to_string()
    );
    // Give the discovery scan time to run; it finds the worktree but
    // the filter keeps it hidden while the toggle is off.
    harness
        .wait_until(|h| h.editor().session_count() >= 1)
        .unwrap();
    harness.tick_and_render().unwrap();
    assert!(
        !harness.screen_to_string().contains("· on-disk"),
        "discovered worktrees must stay hidden while the toggle is off.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Alt+T (the rebindable `orchestrator_toggle_worktrees`) reveals
    // them, and the toggle flips to checked.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("· on-disk") && s.contains("[v] Show all worktrees")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Alt+T should reveal the on-disk worktree and check the toggle.\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });

    // Alt+T again hides them.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("· on-disk"))
        .unwrap_or_else(|_| {
            panic!(
                "Alt+T again should hide the on-disk worktree.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Toggle the global dock open via the command palette and wait for it
/// to render *and* take keyboard focus (focus is set asynchronously
/// through the plugin→host bridge, so poll `is_dock_focused`).
fn open_dock(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Orchestrator: Toggle Dock").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused()
        })
        .unwrap();
}

/// 0-based screen row containing `needle`, or panic with the screen.
fn dock_row_of(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

/// Clicking a discovered (on-disk) worktree row in the *dock* opens it
/// directly — it attaches a managed session at that worktree, the same
/// outcome the Open dialog produces. There's no live window to switch
/// to, so a click can't "live-switch" the way it does for a live row;
/// before the fix the click was therefore a silent no-op and visiting an
/// inactive session from the dock did nothing (you had to press Enter or
/// the Visit button). The dock's Enter (`dock_activate`) path still works
/// too — this just makes a click open the worktree without that step.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // attach spawns a Unix shell terminal.
fn dock_click_attaches_discovered_worktree() {
    if !pty_available() {
        eprintln!("skipping: no PTY available in this environment");
        return;
    }
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Toggle Dock");

    open_dock(&mut harness);

    // Alt+T reveals the discovered on-disk worktree in the dock.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "dock should reveal the on-disk `feature-x` worktree after Alt+T.\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });

    // A single click on the discovered row opens it — no Enter needed.
    let row = dock_row_of(&harness, "· on-disk") as u16;
    harness.mouse_click(3, row).unwrap();

    // Attach is async (`createWindowWithTerminal`). It opens a live
    // session rooted at the worktree, so the dock's discovered row turns
    // into a live `feature-x` row (no longer `· on-disk`).
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && !s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Clicking the dock's discovered worktree row should attach a live \
                 session (row loses `· on-disk`).\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Arrowing the dock's highlight onto a discovered (on-disk) worktree
/// opens it — keyboard parity with the click path. The dock's live-switch
/// treats the highlighted row as the active session, so for an inactive
/// worktree "becoming active" means attaching a session there. No click
/// and no Enter: just moving the selection onto it with the keyboard.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // attach spawns a Unix shell terminal.
fn dock_arrow_nav_opens_discovered_worktree() {
    if !pty_available() {
        eprintln!("skipping: no PTY available in this environment");
        return;
    }
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Toggle Dock");

    open_dock(&mut harness);

    // Alt+T reveals the discovered on-disk worktree below the base session.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "dock should reveal the on-disk `feature-x` worktree after Alt+T.\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });

    // Move the highlight down onto the on-disk row — no click, no Enter.
    // The debounced live-switch then opens it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && !s.contains("· on-disk")
        })
        .unwrap_or_else(|_| {
            panic!(
                "Arrowing onto the dock's discovered worktree row should open it \
                 (row loses `· on-disk`).\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}

/// Opening a discovered worktree from the dock keeps it in the *same row*
/// — the sort is stable. The order is the `mainrepo` base (main checkout)
/// first, then its worktrees alphabetically (`feature-x`, `feature-y`);
/// opening `feature-x` turns it live but it must stay above `feature-y`
/// rather than jumping into a "live" group (the old live-before-discovered
/// sort shuffled rows under you as you navigated).
#[test]
#[cfg_attr(target_os = "windows", ignore)] // attach spawns a Unix shell terminal.
fn dock_opening_worktree_keeps_its_row_position() {
    if !pty_available() {
        eprintln!("skipping: no PTY available in this environment");
        return;
    }
    let (_temp, repo, _wt1, _wt2) = set_up_repo_with_two_worktrees();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Toggle Dock");

    open_dock(&mut harness);
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("feature-x") && s.contains("feature-y") && s.contains("· on-disk")
        })
        .unwrap();

    // Stable lexicographic order before opening: feature-x above feature-y.
    let fx_before = dock_row_of(&harness, "feature-x");
    let fy_before = dock_row_of(&harness, "feature-y");
    assert!(
        fx_before < fy_before,
        "initial order should be lexicographic (feature-x above feature-y).\n\
         Screen:\n{}",
        harness.screen_to_string()
    );

    // Open feature-x by clicking it.
    harness.mouse_click(3, fx_before as u16).unwrap();
    harness
        .wait_until(|h| {
            // feature-x is now live (its on-disk tag is gone) while feature-y
            // is still discovered.
            let s = h.screen_to_string();
            s.matches("· on-disk").count() == 1 && s.contains("feature-y")
        })
        .unwrap_or_else(|_| {
            panic!(
                "clicking feature-x should open it (one on-disk row left).\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });

    // Stable: feature-x kept its place above feature-y after going live.
    let fx_after = dock_row_of(&harness, "feature-x");
    let fy_after = dock_row_of(&harness, "feature-y");
    assert!(
        fx_after < fy_after,
        "opening feature-x must not reorder it below feature-y — the sort is \
         stable.\nfx_after={fx_after} fy_after={fy_after}\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Archiving the *last* session — which is also the launch / in-place
/// session (no dedicated worktree) — must not be refused. Every session
/// is archivable now: the launch session is recorded at its own root and,
/// because it's the only live window, a replacement terminal session is
/// opened in its project first so the editor is never left empty.
///
/// Before the fix this was doubly blocked: `enterConfirm` refused both
/// "no worktree to archive" and "last window", so the confirm step never
/// even appeared.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // replacement spawns a Unix shell terminal.
fn archive_last_in_place_session_opens_replacement() {
    if !pty_available() {
        eprintln!("skipping: no PTY available in this environment");
        return;
    }
    // Only the launch session exists (the on-disk worktree is never
    // attached here) — an in-place session with no dedicated worktree.
    let (_temp, repo, _wt) = set_up_repo_with_worktree();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, repo.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_command(&mut harness, "Orchestrator: Open");

    open_orchestrator_dialog(&mut harness);
    // The launch session is a trivial (empty) session, hidden by default;
    // Alt+I reveals it so it can be selected.
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("launch session"))
        .unwrap_or_else(|_| {
            panic!(
                "the launch session should be listed after Alt+I.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });

    // Focus opens on Visit; Tab to the Archive button (Stop is disabled
    // for the terminal-less launch session, so the cycle skips it:
    // visit -> toggle-details -> archive). Activate it to open the
    // confirm. Without the fix Archive is disabled (dropped from the Tab
    // cycle) and the action is refused, so the confirm never shows.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Confirm Archive"))
        .unwrap_or_else(|_| {
            panic!(
                "archiving the launch session should reach the confirm step.\n\
                 Screen:\n{}",
                harness.screen_to_string()
            )
        });

    // Confirm (Cancel is focused first; Tab -> Confirm Archive).
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The launch session is archived; because it was the only window, a
    // replacement terminal session was opened in its project — so the
    // editor still hosts a live session, now a *Terminal*.
    harness
        .wait_until(|h| h.screen_to_string().contains("Terminal"))
        .unwrap_or_else(|_| {
            panic!(
                "archiving the last session should open a replacement terminal \
                 session.\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
}
