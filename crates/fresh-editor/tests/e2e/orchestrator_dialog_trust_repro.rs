//! Reproductions for three reported UX defects around the Orchestrator dock's
//! destructive-action confirmation and the workspace-trust modal.
//!
//! 1. **Dock context-menu confirm keyboard focus.** Right-click a dock session
//!    row → Delete must raise a centered confirmation whose Cancel button holds
//!    the keyboard. This one currently *passes*: focus lands on Cancel via the
//!    renderer's "first tabbable" fallback (`render_spec_inner`'s
//!    `auto_focus_first`), because the outgoing focus key (`ctx-delete`) is not
//!    tabbable in the confirm spec. It is guarded here because it holds only by
//!    accident — `dockMenuEnterConfirm` never pins `confirm-cancel` the way the
//!    modal picker's `enterConfirm` does, so reordering the button pair would
//!    silently move default focus onto **Confirm Delete**.
//!
//! 2. **A worktree re-asks the trust question.** Creating a workspace as a
//!    git worktree of an already-decided repo raises the trust modal again —
//!    trust is keyed purely on the worktree's own path, so the base repo's
//!    recorded decision is not inherited.
//!
//! 3. **Clicking a trust level accepts it immediately.** In the trust modal a
//!    click on a radio row calls `confirm_workspace_trust` — it records the
//!    decision and dismisses the dialog, instead of just moving the radio and
//!    waiting for OK (which is what the keyboard mnemonics `T`/`K`/`B` do).
//!    Shared by the TUI and the web UI (the web frontend forwards the click to
//!    the same `handle_workspace_trust_mouse`).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

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

// ─────────────────────────────────────────────────────────────────────────────
// Issue 1 — dock context-menu confirmation keyboard focus
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

/// Right-click a dock session card → Delete → the confirmation pane must open
/// with the **Cancel** button holding keyboard focus, so a stray Enter is a
/// no-op (it returns to the menu) rather than nothing at all.
#[test]
fn dock_context_menu_delete_confirm_focuses_cancel() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h = EditorTestHarness::with_config_and_working_dir(WIDTH, 32, Default::default(), root)
        .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    let card_row = row_of(&h, "alphaproj") as u16;
    h.mouse_right_click(4, card_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Delete"))
        .unwrap();

    let (dcol, drow) = pos_of(&h, "Delete");
    h.mouse_click(dcol, drow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Confirm Delete"))
        .unwrap();

    // Enter must activate the focused button. With Cancel focused (the safe
    // default for a destructive prompt) that returns to the three-action menu.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Visit"))
        .unwrap_or_else(|_| {
            let screen = h.screen_to_string();
            panic!(
                "Enter on the delete confirmation did nothing — no button holds \
                 keyboard focus. Expected Cancel to be focused and return to the \
                 context menu.\nScreen:\n{screen}"
            )
        });
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("Confirm Delete"),
        "Enter should have dismissed the confirmation via Cancel.\nScreen:\n{screen}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Issue 2 — a git worktree must inherit its base repo's trust decision
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

/// Record `level` for `root` in the harness's per-project trust store — the
/// same on-disk decision the trust modal writes when the user picks a row.
fn record_trust(
    h: &EditorTestHarness,
    root: &Path,
    level: fresh::services::workspace_trust::TrustLevel,
) {
    let dir = h.editor().dir_context().project_state_dir(root);
    fs::create_dir_all(&dir).unwrap();
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&dir);
    let trust =
        fresh::services::workspace_trust::WorkspaceTrust::new_persistent(None, level, store);
    trust.set_level(level);
}

/// Point the harness's live trust handle at the working dir's own (undecided)
/// project store, so the prompt gate sees a real per-project store.
fn arm_project_store(h: &mut EditorTestHarness) {
    let dir = h.editor().working_dir().to_path_buf();
    let store_path = h.editor().dir_context().project_state_dir(&dir);
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&store_path);
    h.editor()
        .authority()
        .workspace_trust
        .set_store(Some(store));
}

/// A workspace opened on a linked worktree of an already-trusted repo must NOT
/// re-ask the trust question — the decision belongs to the repo, not to each
/// checkout of it.
#[test]
#[ignore = "reproduces an open bug: trust is keyed on the worktree's own path, \
            so the base repo's recorded decision is never inherited"]
fn worktree_inherits_base_repo_trust_decision() {
    let (_tmp, base, wt) = repo_with_worktree();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(WIDTH, HEIGHT, Default::default(), wt)
            .unwrap();
    // The user already answered the trust prompt for the base repo.
    record_trust(
        &h,
        &base,
        fresh::services::workspace_trust::TrustLevel::Trusted,
    );
    arm_project_store(&mut h);

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
    assert_eq!(
        h.editor().authority().workspace_trust.level(),
        fresh::services::workspace_trust::TrustLevel::Trusted,
        "the worktree must adopt the base repo's recorded trust level"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Issue 3 — clicking a trust radio must select, not accept
// ─────────────────────────────────────────────────────────────────────────────

fn arm_undecided_project_with_markers(h: &mut EditorTestHarness) {
    let dir = h.editor().working_dir().to_path_buf();
    fs::write(dir.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
    arm_project_store(h);
}

/// Clicking a radio row in the workspace-trust modal must only move the
/// selection — the decision is committed by [ OK ], exactly as the keyboard
/// mnemonics (`T`/`K`/`B` select, Enter/`O` confirm) already behave. The web
/// UI forwards its clicks to this same hit-test, so the two share the fix.
#[test]
#[ignore = "reproduces an open bug: a radio click calls confirm_workspace_trust, \
            recording the decision and dismissing the dialog immediately"]
fn trust_dialog_radio_click_selects_without_accepting() {
    let mut h = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    arm_undecided_project_with_markers(&mut h);

    h.editor_mut().maybe_prompt_workspace_trust(true);
    h.render().unwrap();
    h.wait_until(|h| h.screen_to_string().contains("SECURITY WARNING"))
        .unwrap();

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

    let store_path = {
        let dir = h.editor().working_dir().to_path_buf();
        h.editor().dir_context().project_state_dir(&dir)
    };
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&store_path);
    assert!(
        !store.is_decided(),
        "clicking a trust option must not record the decision — only [ OK ] does"
    );

    // The click did move the selection: the Trust row is now the marked radio.
    let trust_row = row_of(&h, "Trust folder & Allow Tooling");
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

    // [ OK ] commits it.
    let (ok_col, ok_row) = pos_of(&h, "OK");
    h.mouse_click(ok_col + 1, ok_row).unwrap();
    h.render().unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("SECURITY WARNING"))
        .unwrap();
    assert_eq!(
        h.editor().authority().workspace_trust.level(),
        fresh::services::workspace_trust::TrustLevel::Trusted,
        "[ OK ] must commit the highlighted option"
    );
}
