//! Orchestrator dock destructive-action confirmation + workspace-trust modal.
//!
//! Three reported defects, each guarded here by driving only keyboard/mouse and
//! asserting on rendered output (CONTRIBUTING §2):
//!
//! 1. The dock's right-click → Delete confirmation was **clipped** on a short
//!    terminal: it mounts at `heightPct: 44`, the centered placement treated
//!    that as a hard cap, and the tail of the spec — including the
//!    `[ Cancel ] [ Confirm Delete ]` row — was cut off. The modal read as
//!    "up but the keyboard isn't on it" while the (invisible) focused Cancel
//!    still answered Enter.
//!
//! 2. A workspace opened on a linked git worktree re-asked the trust question
//!    even though the user had already answered it for the repo, because trust
//!    was keyed purely on the workspace's own path.
//!
//! 3. Clicking a radio row in the trust modal recorded the decision and
//!    dismissed the dialog, rather than moving the selection and waiting for
//!    [ OK ] — so "Trust folder & Allow Tooling" was a one-click grant of full
//!    execution rights. The web UI forwards its radio clicks to the same
//!    hit-test, so both frontends are covered by the same fix.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;
/// Tall enough for the dock and a session card, short enough that a 44%-high
/// centered modal cannot hold the confirmation's ~13 content rows — the shape
/// that clipped the button row.
const SHORT_HEIGHT: u16 = 24;

/// 0-based screen (col, row) of the first occurrence of `needle`. Dock rows
/// carry multibyte box-drawing glyphs, so the byte offset `str::find` returns
/// is converted to a character column (= screen column for width-1 glyphs).
fn pos_of(h: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let screen = h.screen_to_string();
    screen
        .lines()
        .enumerate()
        .find_map(|(r, l)| {
            l.find(needle)
                .map(|b| (l[..b].chars().count() as u16, r as u16))
        })
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

fn row_of(h: &EditorTestHarness, needle: &str) -> usize {
    let screen = h.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

/// The status bar — the last painted row. Carries the `{trust}` pill, which is
/// how the trust level is *observable* rather than inspected.
fn status_bar(h: &EditorTestHarness) -> String {
    h.screen_to_string()
        .lines()
        .rev()
        .find(|l| !l.trim().is_empty())
        .unwrap_or_default()
        .to_string()
}

// ─────────────────────────────────────────────────────────────────────────────
// Issue 1 — the dock's delete confirmation must show its buttons
// ─────────────────────────────────────────────────────────────────────────────

/// A git project with the orchestrator plugin (+ shared lib) installed.
fn setup_project(name: &str) -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join(name);
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    assert!(std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success());
    (temp_dir, root)
}

fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused())
        .unwrap();
}

/// Open the dock, right-click the session card, and choose Delete. Leaves the
/// harness showing the confirmation.
fn open_delete_confirmation(height: u16) -> (tempfile::TempDir, EditorTestHarness) {
    let (tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(WIDTH, height, Default::default(), root)
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Guard the wait below against passing vacuously: the confirmation's
    // heading must not already be on screen before we ask for it.
    assert!(
        !h.screen_to_string().contains("Confirm Delete"),
        "the dock must not already be showing a delete confirmation"
    );

    let card_row = row_of(&h, "alphaproj") as u16;
    h.mouse_right_click(4, card_row).unwrap();
    // The menu's entries draw as plain uniform rows inside its box, not as
    // `[ bracketed ]` buttons, so match "Delete" *within the menu* — a bare
    // word match could pick up any other row that mentions it, and the box
    // border on the same line is what distinguishes one.
    let menu_row = |h: &EditorTestHarness| -> Option<(u16, u16)> {
        h.screen_to_string().lines().enumerate().find_map(|(r, l)| {
            if !l.contains('│') {
                return None;
            }
            l.find("Delete")
                .map(|b| (l[..b].chars().count() as u16, r as u16))
        })
    };
    h.wait_until(|h| menu_row(h).is_some()).unwrap();

    let (dcol, drow) = menu_row(&h).expect("the row menu lists Delete");
    h.mouse_click(dcol + 1, drow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Confirm Delete"))
        .unwrap();
    (tmp, h)
}

/// A destructive confirmation is useless if the user can't see how to answer
/// it. On a short terminal the centered panel must grow to fit its content
/// instead of clipping the tail: both the warning and the Cancel / Confirm
/// pair have to be on screen.
#[test]
fn dock_delete_confirmation_shows_its_buttons_on_a_short_terminal() {
    let (_tmp, h) = open_delete_confirmation(SHORT_HEIGHT);

    let screen = h.screen_to_string();
    assert!(
        screen.contains("Uncommitted changes will be lost"),
        "the delete warning was clipped off the confirmation.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("[ Cancel ]"),
        "the confirmation must show its Cancel button — a modal whose buttons \
         are clipped reads as 'the keyboard isn't on it'.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("[ Confirm Delete ]"),
        "the confirmation must show its Confirm button.\nScreen:\n{screen}"
    );
}

/// Cancel holds the keyboard by default, so a stray Enter on a destructive
/// prompt is recoverable: it returns to the context menu rather than wiping a
/// worktree.
#[test]
fn dock_delete_confirmation_focuses_cancel() {
    let (_tmp, mut h) = open_delete_confirmation(HEIGHT);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Visit") && !s.contains("Confirm Delete")
    })
    .unwrap();
    h.assert_screen_contains("Archive");
}

// ─────────────────────────────────────────────────────────────────────────────
// Issue 2 — a git worktree inherits its repo's trust decision
// ─────────────────────────────────────────────────────────────────────────────

fn git(args: &[&str], cwd: &Path) {
    let out = std::process::Command::new("git")
        .args(args)
        .current_dir(cwd)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Build a git repo with a manifest (so the folder has executable-content
/// markers and therefore trips the trust gate) plus one linked worktree.
/// Returns (tempdir guard, base repo root, worktree root).
fn repo_with_worktree() -> (tempfile::TempDir, PathBuf, PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    let base = temp.path().join("base");
    fs::create_dir(&base).unwrap();
    git(&["init", "-q", "-b", "main"], &base);
    git(&["config", "user.email", "t@example.com"], &base);
    git(&["config", "user.name", "T"], &base);
    fs::write(base.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
    git(&["add", "-A"], &base);
    git(&["commit", "-qm", "init"], &base);

    let wt = temp.path().join("wt-feature");
    git(
        &[
            "worktree",
            "add",
            "-q",
            "-b",
            "feature",
            wt.to_str().unwrap(),
        ],
        &base,
    );
    (temp, base, wt)
}

/// Write the on-disk decision the trust modal records for `root`. Spelled out
/// as literal JSON so the test's *setup* doesn't ride the same resolution the
/// assertion is checking.
fn record_trusted_on_disk(h: &EditorTestHarness, root: &Path) {
    let dir = h.editor().dir_context().project_state_dir(root);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("trust.json"), r#"{"level":"trusted"}"#).unwrap();
}

/// Opening a linked worktree of an already-trusted repo must not re-ask the
/// trust question: `git worktree add` makes a second checkout of the *same*
/// code, and an agent-per-worktree workflow would otherwise prompt on every
/// new session. The worktree opens under the repo's recorded level.
#[test]
fn worktree_inherits_base_repo_trust_decision() {
    let (_tmp, base, wt) = repo_with_worktree();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(WIDTH, HEIGHT, Default::default(), wt)
            .unwrap();
    // The user already answered the prompt for the base repo.
    record_trusted_on_disk(&h, &base);

    // Activating the worktree workspace runs the same gate the Orchestrator's
    // new-session path runs.
    h.editor_mut().maybe_prompt_workspace_trust(true);
    h.render().unwrap();

    let screen = h.screen_to_string();
    assert!(
        !screen.contains("SECURITY WARNING"),
        "a worktree of an already-trusted repo must not re-prompt for \
         trust.\nScreen:\n{screen}"
    );
    let status = status_bar(&h);
    assert!(
        status.contains("Trusted"),
        "the worktree must open under the repo's recorded level.\nStatus bar: {status}"
    );
}

/// The counterpart: an *undecided* repo still prompts through its worktree —
/// inheritance shares a real decision, it never invents one.
#[test]
fn worktree_of_undecided_repo_still_prompts() {
    let (_tmp, _base, wt) = repo_with_worktree();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(WIDTH, HEIGHT, Default::default(), wt)
            .unwrap();

    h.editor_mut().maybe_prompt_workspace_trust(true);
    h.render().unwrap();

    h.wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .unwrap();
    h.assert_screen_contains("Cargo.toml");
}

// ─────────────────────────────────────────────────────────────────────────────
// Issue 3 — clicking a trust radio selects, it does not accept
// ─────────────────────────────────────────────────────────────────────────────

/// Clicking a radio row in the workspace-trust modal must only move the
/// selection — the decision is committed by [ OK ], exactly as the keyboard
/// mnemonics (`T`/`K`/`B` select, Enter/`O` confirm) already behave. The web
/// UI forwards its clicks to this same hit-test, so the two share the fix.
#[test]
fn trust_dialog_radio_click_selects_without_accepting() {
    let mut h = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    let dir = h.editor().working_dir().to_path_buf();
    fs::write(dir.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();

    h.editor_mut().maybe_prompt_workspace_trust(true);
    h.render().unwrap();
    h.wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .unwrap();

    // The prompt opens on the safe default, so the "(*)" assertion below is a
    // real state change rather than a value that was already there.
    let trust_row = row_of(&h, "Trust folder & Allow Tooling");
    let before = h
        .screen_to_string()
        .lines()
        .nth(trust_row)
        .unwrap()
        .to_string();
    assert!(
        before.contains("( )"),
        "the Trust option should start unselected.\nRow: {before}"
    );

    // Click "Trust folder & Allow Tooling (T)".
    let (col, row) = pos_of(&h, "Trust folder & Allow Tooling");
    h.mouse_click(col + 2, row).unwrap();
    h.render().unwrap();

    let screen = h.screen_to_string();
    assert!(
        screen.contains("SECURITY WARNING"),
        "clicking a trust option must only move the radio — the dialog stays up \
         until [ OK ].\nScreen:\n{screen}"
    );
    let status = status_bar(&h);
    assert!(
        status.contains("Restricted"),
        "an un-committed selection must not change the live trust \
         level.\nStatus bar: {status}"
    );

    // The click did move the selection: the Trust row is now the marked radio.
    let line = h
        .screen_to_string()
        .lines()
        .nth(trust_row)
        .unwrap()
        .to_string();
    assert!(
        line.contains("(*)"),
        "the clicked row should become the selected radio.\nRow: {line}"
    );

    // [ OK ] commits it. Match the rendered button so the hit can't land on a
    // stray "OK" elsewhere on screen.
    let (ok_col, ok_row) = pos_of(&h, "[ OK ]");
    h.mouse_click(ok_col + 2, ok_row).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("SECURITY WARNING"))
        .unwrap();
    let status = status_bar(&h);
    assert!(
        status.contains("Trusted"),
        "[ OK ] must commit the highlighted option.\nStatus bar: {status}"
    );
}
