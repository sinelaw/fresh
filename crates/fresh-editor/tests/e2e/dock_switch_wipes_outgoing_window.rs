//! Clicking between workspaces in the Orchestrator dock must never paint a
//! frame stitched out of two of them.
//!
//! Regression: plugin commands are drained twice per render — once before
//! layout, and once mid-render for the decorations that `lines_changed`
//! hooks produce. A dock click could land on the late drain, moving
//! `active_window` *after* the menu bar, the file-explorer sidebar and the
//! tab bar had been laid out and painted for the outgoing workspace, but
//! *before* the buffer content was. The frame that came out carried one
//! workspace's sidebar beside the other's buffer — the reported "the file
//! explorer shows up late and hangs over the wrong window for a moment"
//! when one workspace has the explorer open and the other does not. The
//! switch is now held back to the next frame's pre-layout drain, so every
//! frame is laid out for exactly one workspace.
//!
//! Per CONTRIBUTING §2 this drives only mouse/keyboard and asserts on
//! rendered output, and per §3 it waits on rendered state rather than a
//! clock: the check is a per-frame invariant, so it holds whether a frame
//! is caught mid-transition or after it settles.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

/// A git project carrying the orchestrator plugin, plus a second project
/// directory (`wt-betaws`) for the workspace the test switches to. The
/// second one holds a marker directory that shows up only in the file
/// explorer's tree.
fn setup_projects() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("alphaproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let beta = root.join("wt-betaws");
    fs::create_dir(&beta).unwrap();
    fs::create_dir(beta.join("zzbetaonly")).unwrap();
    let ok = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success();
    assert!(ok);
    (temp_dir, root)
}

/// Toggle the dock open via the command palette and wait for it to render
/// *and* take keyboard focus (the plugin sets focus asynchronously).
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

/// 0-based screen row containing `needle`, or panic with the screen.
fn row_of(h: &EditorTestHarness, needle: &str) -> u16 {
    let screen = h.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}")) as u16
}

/// 0-based screen column of `needle`'s first occurrence, if it is on
/// screen at all. Dock and explorer chrome are multibyte box-drawing
/// glyphs, so the byte offset `str::find` returns is converted to a
/// character count first.
fn col_of(screen: &str, needle: &str) -> Option<u16> {
    screen
        .lines()
        .find_map(|l| l.find(needle).map(|b| l[..b].chars().count() as u16))
}

/// Open the file explorer in the active window through the command
/// palette, and wait for its tree to carry the marker directory.
fn open_explorer(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Toggle File Explorer").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle File Explorer"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("zzbetaonly"))
        .unwrap();
}

/// Where each workspace's buffer text sits horizontally, captured from a
/// settled frame of that workspace. alphaproj's starts just right of the
/// dock; betaws' is pushed across by the width of its file-explorer
/// sidebar. A frame that lays the sidebar out for one workspace and the
/// buffer for the other puts a sentinel at the *other* workspace's column
/// — which is exactly the stitched frame this test rules out.
struct Columns {
    alpha: u16,
    beta: u16,
}

impl Columns {
    /// Panics unless every sentinel visible in `screen` sits at the column
    /// its own workspace's layout puts it at.
    fn assert_coherent(&self, screen: &str) {
        if let Some(col) = col_of(screen, "AAAA") {
            assert_eq!(
                col, self.alpha,
                "alphaproj's buffer is laid out at column {} in a frame that \
                 belongs to it, but this frame put it at {col} — the frame \
                 was assembled from two workspaces; screen:\n{screen}",
                self.alpha
            );
        }
        if let Some(col) = col_of(screen, "BBBB") {
            assert_eq!(
                col, self.beta,
                "betaws' buffer is laid out at column {} in a frame that \
                 belongs to it (its file-explorer sidebar takes the columns \
                 to its left), but this frame put it at {col} — the frame was \
                 assembled from two workspaces; screen:\n{screen}",
                self.beta
            );
        }
    }
}

#[test]
fn dock_switch_never_paints_two_workspaces_into_one_frame() {
    let (_tmp, root) = setup_projects();
    // An explicit Config keeps `editor.animations` at its user-facing
    // default (on) — the harness only forces animations off when a test
    // passes no config at all — so the switch plays its wipe here, exactly
    // as it does for a user.
    let config = Config::default();
    assert!(
        config.editor.animations,
        "precondition: this exercises the switch with its wipe animation on"
    );
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, config, root.clone()).unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-betaws"), "betaws".to_string());
    h.render().unwrap();

    // Type a per-workspace sentinel into each empty buffer — a screen
    // marker for "this text belongs to that workspace". `AAAA`/`BBBB`
    // avoid false matches against dock labels, menus and status text.
    h.type_text("AAAA").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("AAAA"))
        .unwrap();

    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("alphaproj") && s.contains("betaws")
    })
    .unwrap();

    // Dive into betaws, give it its own sentinel, and open its file
    // explorer. Only this workspace has a sidebar.
    let beta_row = row_of(&h, "betaws");
    h.mouse_click(3, beta_row).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("AAAA"))
        .unwrap();
    h.type_text("BBBB").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("BBBB"))
        .unwrap();
    open_explorer(&mut h);
    let beta_col = col_of(&h.screen_to_string(), "BBBB").unwrap();

    // Back to alphaproj to record its (sidebar-less) column with the dock
    // in place, then let the screen settle. Both baselines are now taken
    // from the layout the transitions below are measured against.
    let alpha_row = row_of(&h, "alphaproj");
    h.mouse_click(3, alpha_row).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("AAAA") && !s.contains("File Explorer")
    })
    .unwrap();
    let alpha_col = col_of(&h.screen_to_string(), "AAAA").unwrap();
    assert!(
        beta_col > alpha_col,
        "precondition: betaws' sidebar must push its buffer right of \
         alphaproj's (alpha {alpha_col}, beta {beta_col})"
    );
    let columns = Columns {
        alpha: alpha_col,
        beta: beta_col,
    };

    // ── Entering the workspace that owns the sidebar ───────────────────
    let beta_row = row_of(&h, "betaws");
    h.mouse_click(3, beta_row).unwrap();
    columns.assert_coherent(&h.screen_to_string());
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        columns.assert_coherent(&screen);
        // Settled on betaws: its buffer is up, with its sidebar beside it.
        screen.contains("BBBB") && screen.contains("zzbetaonly")
    })
    .unwrap();

    // ── Leaving it again ───────────────────────────────────────────────
    let alpha_row = row_of(&h, "alphaproj");
    h.mouse_click(3, alpha_row).unwrap();
    columns.assert_coherent(&h.screen_to_string());
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        columns.assert_coherent(&screen);
        // Settled on alphaproj: its buffer is up and no sidebar is left
        // over from the workspace we came from.
        screen.contains("AAAA") && !screen.contains("File Explorer")
    })
    .unwrap();
}
