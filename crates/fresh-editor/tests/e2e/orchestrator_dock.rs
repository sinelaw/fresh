//! E2E coverage for the global Orchestrator dock (the persistent,
//! non-modal left column toggled by "Orchestrator: Toggle Dock").
//!
//! Per CONTRIBUTING.md §2 these drive only keyboard/mouse and assert on
//! rendered output. Each guards a behaviour that regressed during dock
//! bring-up:
//!
//! * the dock renders as a left column beside the editor chrome;
//! * it is non-modal — Ctrl+P while the dock is focused opens the
//!   command palette (the key falls through to the editor) instead of
//!   being swallowed, and the dock stays visible;
//! * the session list order is stable as the active window changes
//!   (the picker's current-project-first sort must not reorder the
//!   persistent dock);
//! * mouse clicks land on dock widgets (the "+ New" button opens the
//!   new-session form).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;

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
/// *and* take keyboard focus.
///
/// `Toggle Dock` sets focus asynchronously through the plugin→host
/// bridge (the plugin issues `setFocusKey("sessions")` after the dock
/// mounts), so a key event dispatched after just `wait_until("ORCHESTRATOR")`
/// can land *before* `dock.focused = true` — falling through to the
/// editor base and leaving any follow-up `wait_until` to block forever
/// on a dock response that never comes. Polling `is_dock_focused()`
/// closes that race deterministically.
fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR") && h.editor().is_dock_focused())
        .unwrap();
}

/// 0-based screen row containing `needle`, or panic with the screen.
fn row_of(h: &EditorTestHarness, needle: &str) -> usize {
    let screen = h.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

#[test]
fn dock_renders_as_left_column_beside_chrome() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The dock and its controls render...
    h.assert_screen_contains("ORCHESTRATOR");
    h.assert_screen_contains("+ New");
    // ...and the editor chrome (menu bar) is still present to its right,
    // i.e. the dock is a column beside the window, not a replacement.
    h.assert_screen_contains("File");
    // The launch session is listed by its project basename.
    h.assert_screen_contains("alphaproj");
}

#[test]
fn ctrl_p_opens_palette_while_dock_focused_and_dock_stays() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The dock is focused on mount. Ctrl+P must NOT be swallowed: it
    // blurs the dock and falls through to the editor's global binding,
    // opening the command palette. Prove the palette is live by typing a
    // query and seeing a built-in command surface — and the dock must
    // stay visible (non-modal) throughout.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Open File").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Open File"))
        .unwrap();
    h.assert_screen_contains("Open File");
    h.assert_screen_contains("ORCHESTRATOR");
}

/// Alt+O toggles keyboard focus between the editor and the dock, and the
/// shift is *visible*: the dock's right-edge divider lights with the accent
/// colour while focused and dims when focus leaves. Drives only the keyboard
/// and asserts on rendered output (the divider cell's colour) per
/// CONTRIBUTING §2.
///
/// This single flow exercises both halves of the feature — without the
/// `toggle_dock_focus` binding Alt+O is inert and the divider never dims
/// (the `assert_ne!` fails); without the focus indicator the divider colour
/// is constant regardless of focus (the same `assert_ne!` fails).
#[test]
fn alt_o_toggles_dock_focus_with_visible_indicator() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h); // the dock mounts with keyboard focus

    // The dock's right border (`│`) is a full-height divider; sample its
    // colour on a content row. The default width is responsive, so scan for
    // the glyph rather than hard-coding a column.
    const ROW: u16 = 6;
    let border_col = |h: &EditorTestHarness| -> u16 {
        let cols = h.screen_row_text(0).chars().count() as u16;
        (0..cols)
            .find(|&c| h.get_cell(c, ROW).as_deref() == Some("│"))
            .expect("dock right border (│) should be present on a content row")
    };
    let divider_fg = |h: &EditorTestHarness| h.get_cell_style(border_col(h), ROW).unwrap().fg;

    // FOCUSED on mount: the divider wears its focused (accent) colour.
    let focused_fg = divider_fg(&h);

    // Alt+O → hand focus back to the editor. The dock stays visible
    // (non-modal), but its divider dims to the muted colour.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("ORCHESTRATOR");
    let blurred_fg = divider_fg(&h);
    assert_ne!(
        focused_fg, blurred_fg,
        "the dock divider must change colour when keyboard focus leaves it"
    );

    // Alt+O again → dive back into the dock: the divider relights with the
    // original focused colour.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("ORCHESTRATOR");
    assert_eq!(
        divider_fg(&h),
        focused_fg,
        "re-focusing the dock must restore the focused divider colour"
    );
}

#[test]
fn dock_list_order_is_stable_across_active_window_switch() {
    // Two sessions in *different* projects: switching the active window
    // changes the "current project", which the picker would float to the
    // top. The persistent dock must keep a stable order regardless.
    // Both projects are siblings under one parent so their project-key
    // (path) sort is deterministic (`aaa_project` < `zzz_project`),
    // making "stable order" testable without random-tempdir flakiness.
    let (_tmp_a, root_a) = setup_project("aaa_project");
    let parent = root_a.parent().unwrap().to_path_buf();
    let root_b = parent.join("zzz_project");
    fs::create_dir(&root_b).unwrap();
    assert!(std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root_b)
        .status()
        .unwrap()
        .success());

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root_a.clone())
            .unwrap();
    // Second session in the other project (launch session is aaa_project).
    h.editor_mut()
        .create_window_at(root_b.clone(), "zzz_project".to_string());
    h.render().unwrap();
    open_dock(&mut h);

    // Both sessions show; aaa sorts above zzz.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("aaa_project") && s.contains("zzz_project")
    })
    .unwrap();
    let aaa_before = row_of(&h, "aaa_project");
    let zzz_before = row_of(&h, "zzz_project");
    assert!(
        aaa_before < zzz_before,
        "expected aaa above zzz initially; got aaa at row {aaa_before}, \
         zzz at row {zzz_before}. Full screen for diagnosis:\n{}",
        h.screen_to_string(),
    );

    // Arrow down to the second row, which live-switches the active window
    // to the zzz project.
    //
    // Snapshot the pre-Down screen so we can wait on a *screen-observable*
    // post-switch signal — the dock's PROJECT column tag visibly swaps
    // when the active session changes. Before Down: aaa is current
    // (no project tag), zzz is not (tag = "zzz_project's basename"); after
    // the switch: zzz is current (no tag), aaa shows its tag. This lets us
    // detect the switch without an accessor wait (CONTRIBUTING §2) AND
    // without false matches on mid-render snapshots — the post-Down
    // highlight-move is a style-only change that doesn't enter
    // `screen_to_string`, so the first diff that does is the tag swap
    // after `scheduleDockSwitch`'s 30 ms debounce lands.
    let pre = h.screen_to_string();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string() != pre).unwrap();
    h.wait_until_stable(|_| true).unwrap();

    // Order must be unchanged — aaa still above zzz (the bug floated the
    // now-current zzz project to the top).
    let aaa_after = row_of(&h, "aaa_project");
    let zzz_after = row_of(&h, "zzz_project");
    assert!(
        aaa_after < zzz_after,
        "dock list reordered on switch: aaa now at {aaa_after}, zzz at {zzz_after}.\n\
         Full screen for diagnosis:\n{}",
        h.screen_to_string(),
    );
}

#[test]
fn mouse_click_on_dock_new_button_opens_form() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Click the "+ New" button inside the dock column. A click landing on
    // a dock widget proves mouse hit-testing routes into the panel.
    let new_row = row_of(&h, "+ New") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Session"))
        .unwrap();
    h.assert_screen_contains("New Session");
    // The dock and the centered form occupy disjoint slots, so opening
    // the form must NOT tear down the dock — its header stays painted in
    // the left column beside the modal.
    h.assert_screen_contains("ORCHESTRATOR");

    // Esc cancels the form; the dock regains focus and stays visible.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Session"))
        .unwrap();
    h.assert_screen_contains("ORCHESTRATOR");
}

#[test]
fn dock_alt_n_opens_form_keyboard_and_dock_stays() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Alt+N from the focused dock opens the new-session form (host fires a
    // `dock_new` widget_event since the dock has no editor mode). The dock
    // lives in its own slot, so the centered form coexists with it.
    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Session"))
        .unwrap();
    h.assert_screen_contains("New Session");
    h.assert_screen_contains("ORCHESTRATOR");

    // Esc returns to the dock, which is still mounted and re-focused.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Session"))
        .unwrap();
    h.assert_screen_contains("ORCHESTRATOR");
}

#[test]
fn dock_slash_filters_and_enter_returns_to_list() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    // Two extra sessions with distinct labels.
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.editor_mut()
        .create_window_at(root.join("wt-gamma"), "gamma".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("beta") && s.contains("gamma")
    })
    .unwrap();

    // "/" focuses the filter; typing narrows the list live (host-level
    // dock key, independent of editor modes).
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.type_text("gamma").unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("gamma") && !s.contains("] beta")
    })
    .unwrap();
    h.assert_screen_not_contains("] beta");

    // Enter in the filter returns to the list (does NOT dive) — the dock
    // stays visible and focused.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.assert_screen_contains("ORCHESTRATOR");
    h.assert_screen_contains("gamma");
}

#[test]
fn dock_space_toggles_multiselect_checkbox() {
    // Wire up the test-process tracing subscriber so the dock/host-side
    // `tracing::warn!` breadcrumbs added in
    // `dispatch_floating_widget_key` (Space branch),
    // `set_panel_focus_and_notify`, `refocus_floating_panel`,
    // `blur_floating_panel`, and the dock mouse-click router fire to
    // stderr when the test runs with `RUST_LOG=fresh::dock=warn` (the
    // default `RUST_LOG=warn` also picks them up). This is the
    // diagnostic hook for the Windows-CI timeout of this test —
    // `Space` doesn't produce `[x]` on Windows but does on Linux/CI,
    // and the breadcrumbs trace the dispatch path from key arrival
    // through plugin notification.
    crate::common::tracing::init_tracing_from_env();
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("beta"))
        .unwrap();

    // No row checked initially.
    h.assert_screen_not_contains("[x]");
    // Space toggles the highlighted row's checkbox (host fires dock_space,
    // the plugin owns the selection set).
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[x]"))
        .unwrap();
    h.assert_screen_contains("[x]");
    // Space again clears it.
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("[x]"))
        .unwrap();
}

#[test]
fn dock_mouse_click_row_then_space_selects_that_row() {
    // A click on a session row must focus the dock so the keyboard works
    // afterward (regression: clicking after a dive left the dock unable to
    // receive keys). Click the second row, then Space; that row's checkbox
    // must toggle — proving the click selected + re-focused it.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("beta"))
        .unwrap();

    let beta_row = row_of(&h, "beta") as u16;
    h.mouse_click(3, beta_row).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[x]"))
        .unwrap();
    // The checked row is the one we clicked (beta).
    let checked = row_of(&h, "[x]");
    let beta = row_of(&h, "beta");
    assert_eq!(
        checked, beta,
        "Space after click should check the clicked (beta) row"
    );
}

/// 0-based column of `needle` within screen row `row`.
fn col_in_row(h: &EditorTestHarness, row: u16, needle: &str) -> usize {
    let line = h.screen_row_text(row);
    line.find(needle)
        .unwrap_or_else(|| panic!("row {row} missing '{needle}': {line:?}"))
}

#[test]
fn dock_right_border_drag_resizes_and_persists() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR"))
        .unwrap();

    // The menu bar ("Edit") sits right of the dock on row 0; its index in
    // the row string shifts right as the dock widens. (We can't match the
    // box-drawing border char in `screen_row_text` — it collapses multi-
    // byte glyphs — but the menu word is ASCII and its delta tracks width.)
    let edit_before = col_in_row(&h, 0, "Edit");

    // Find the dock's right-border column by scanning row 0 for the `│`
    // glyph (`get_cell` returns the real cell symbol, unlike
    // `screen_row_text`). Don't hard-code a width: the default dock width
    // is responsive (scales with the terminal), so it isn't a fixed 32.
    // The press must land exactly on the border column for the host to
    // start a resize drag (see `handle_mouse_drag`).
    let row0_cols = h.screen_row_text(0).chars().count() as u16;
    let border_col = (0..row0_cols)
        .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
        .expect("dock right border (│) should be on row 0 when docked");
    h.mouse_drag(border_col, 6, border_col + 29, 6).unwrap();
    h.render().unwrap();
    let edit_after = col_in_row(&h, 0, "Edit");
    assert!(
        edit_after > edit_before + 15,
        "drag should widen the dock: Edit index {edit_before} -> {edit_after}"
    );

    // Width persists across a hide/show toggle.
    let widened = edit_after;
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("ORCHESTRATOR"))
        .unwrap();
    open_dock(&mut h);
    let edit_reopened = col_in_row(&h, 0, "Edit");
    assert!(
        (edit_reopened as i32 - widened as i32).abs() <= 3,
        "dock width should persist across toggle: {widened} -> {edit_reopened}"
    );
}

#[test]
fn dock_show_empty_toggle_flips_on_click() {
    // The "show empty" toggle defaults to off (hide trivial
    // sessions). Clicking it flips the checkbox `[ ]` → `[v]`, proving the
    // dock toggle is wired to the shared hide-trivial filter.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("show empty"))
        .unwrap();
    let trow = row_of(&h, "show empty") as u16;
    // Off by default: unchecked.
    assert!(
        h.screen_row_text(trow).contains("[ ] show empty"),
        "expected toggle off by default: {:?}",
        h.screen_row_text(trow)
    );
    // Click it → checked.
    h.mouse_click(3, trow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[v] show empty"))
        .unwrap();
}

#[test]
fn picker_space_toggles_focused_checkbox_not_list() {
    // OPEN_MODE binds Space to `orchestrator_toggle_select`
    // unconditionally — it has to, to keep Space out of the filter
    // text input (the host's `dispatch_floating_widget_key` defers any
    // explicitly-bound mode key, including bare chars, before the text-
    // input path). Without context-sensitivity, Space toggles the
    // sessions-list multi-select even while focus is on the
    // "Show all worktrees" / "Show empty/1-file" filter checkbox above
    // the list.
    //
    // With the fix, `toggleSelectCurrent` branches on the focused
    // widget (mirrored from the existing `focus` widget_event): Space
    // on `worktree-show` toggles that checkbox, not the list.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    // Open the centered picker via the command palette.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Open").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Open"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Wait until the picker is fully mounted: the header is painted,
    // the worktree filter row is visible, and the list shows alphaproj.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("ORCHESTRATOR :: Sessions")
            && s.contains("Show all worktrees")
            && s.contains("[ ] alphaproj")
    })
    .unwrap();

    // Sanity: focus opens on the sessions list, so Space toggles the
    // list multi-select. This guards against the test landing focus
    // elsewhere by accident on a future picker re-layout.
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[x] alphaproj"))
        .unwrap();
    // Reset before the focus walk.
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[ ] alphaproj"))
        .unwrap();

    // Tab cycle is spec-order: new-session → scope-toggle →
    // worktree-show → hide-trivial → filter → sessions. Three
    // Shift+Tabs from `sessions` land on `worktree-show`.
    h.send_key(KeyCode::BackTab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::BackTab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::BackTab, KeyModifiers::NONE).unwrap();

    // Space here must toggle `worktree-show`, NOT the list.
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[v] Show all worktrees"))
        .unwrap();
    assert!(
        h.screen_to_string().contains("[ ] alphaproj"),
        "Space while focus is on the worktree-show checkbox must not \
         toggle the list. Screen:\n{}",
        h.screen_to_string()
    );
}

/// Alt+T in the dock toggles "all worktrees" rather than blurring the
/// dock. The Open dialog handles Alt+T via its OPEN_MODE chord, but the
/// dock has no editor mode (it floats over the active buffer's mode), so
/// before the fix the host treated Alt+T as an unhandled Ctrl/Alt chord
/// and blurred the dock — the checkbox never flipped. The host now routes
/// it as a `dock_toggle_worktrees` widget_event the plugin maps to the
/// same toggle.
#[test]
fn dock_alt_t_toggles_worktrees_without_blurring() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The dock's worktree filter starts off.
    h.wait_until(|h| h.screen_to_string().contains("[ ] all worktrees"))
        .unwrap();

    // Alt+T flips it on. Without the fix the chord blurs the dock and the
    // checkbox stays unchecked, so this wait would time out.
    h.send_key(KeyCode::Char('t'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[v] all worktrees"))
        .unwrap();

    // Alt+T again flips it back off (proves it stays wired, not one-shot).
    h.send_key(KeyCode::Char('t'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[ ] all worktrees"))
        .unwrap();

    // And the dock kept keyboard focus throughout — it never blurred.
    assert!(
        h.editor().is_dock_focused(),
        "Alt+T must leave the dock focused, not blur it.\nScreen:\n{}",
        h.screen_to_string()
    );
}

/// Invoking `Orchestrator: Open` while the dock is visible must not be a
/// silent no-op. The dock and the modal picker share one panel + state
/// today, so the modal can't render on top; the command refocuses the
/// dock and surfaces a hint instead. Before the fix `openControlRoom`
/// bailed at `if (openPanel) return` and nothing happened at all.
#[test]
fn open_command_gives_feedback_when_dock_is_visible() {
    // Wide terminal so the dock (left column) still leaves the status bar
    // room to show the full hint text.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(200, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Ctrl+P falls through (blurs the dock) and opens the palette; run
    // "Orchestrator: Open" from it.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Open").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Open"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // With the fix the command refocuses the dock (Ctrl+P had blurred it)
    // and surfaces a status hint. Without the fix `openControlRoom` bails
    // at `if (openPanel) return` — nothing happens and the dock stays
    // blurred, so this wait times out.
    h.wait_until(|h| h.editor().is_dock_focused()).unwrap();

    let screen = h.screen_to_string();
    // The status bar carries the hint (feedback, not silence)...
    assert!(
        screen.contains("the dock already lists sessions"),
        "Open should surface a hint when the dock is visible.\nScreen:\n{screen}"
    );
    // ...the dock is still there...
    assert!(
        screen.contains("ORCHESTRATOR"),
        "the dock must stay visible.\nScreen:\n{screen}"
    );
    // ...and the centered modal picker did NOT open on top of it (the
    // dock header is "ORCHESTRATOR"; the modal title is the longer
    // "ORCHESTRATOR :: Sessions").
    assert!(
        !screen.contains("ORCHESTRATOR :: Sessions"),
        "the modal picker must not open over the dock.\nScreen:\n{screen}"
    );
}

#[test]
fn settings_dialog_does_not_overlap_dock() {
    // Open the dock, then open the Settings modal via the command
    // palette. The settings dialog must render fully inside
    // `chrome_area` (right of the dock) — the dialog's top-left
    // rounded corner glyph `╭` must be visible on the screen, NOT
    // clipped by the dock's right border. With the bug,
    // `render_settings` computes the modal x/y as *relative* offsets
    // (line 146-147 of view/settings/render.rs) and uses them as
    // *absolute* `Rect::new` coordinates — so the modal is placed
    // ~6 columns from the FRAME left edge (inside the dock), and the
    // dock then over-draws its left side, hiding the title bar.
    //
    // Observable signal: with the bug, the full "Settings" title
    // never paints in one piece — the leading characters are clipped
    // by the dock column. The full literal ` Settings [User] `
    // (with both spaces and brackets) only appears on the rendered
    // top border when the modal is positioned correctly.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(160, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Open Settings").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Open Settings"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Active Keybinding Map"))
        .unwrap();

    // The full title — including the leading space and the [User]
    // label — must appear in one contiguous run on the screen. With
    // the bug, the leading half is hidden behind the dock column.
    let screen = h.screen_to_string();
    assert!(
        screen.contains(" Settings [User] "),
        "settings dialog title `Settings [User]` should be visible \
         in full on the chrome side of the dock, but the screen \
         shows clipping:\n{screen}"
    );
}

#[test]
fn click_un_dive_switches_to_clicked_session() {
    init_tracing_from_env();
    // The Rust mouse handler sets `dock.focused = true` when a click
    // lands inside a blurred dock — the un-dive transition. The
    // existing `set_panel_focus_and_notify` it then calls only fires a
    // `focus` widget_event when the inner focus_key changes, which it
    // doesn't here (a dive leaves the inner widget alone, only toggles
    // overall dock focus). So the plugin's `dockBlurred` mirror stays
    // `true`, and when the click's `select` event then schedules a
    // dock-switch (`scheduleDockSwitch`), the +30 ms check
    // `if (... || dockBlurred) return` swallows the active-window
    // change. The fix is host-side: fire a `focus` widget_event on
    // un-blur, symmetric with `blur_floating_panel` (which has always
    // fired `blur` on dive).
    //
    // Reproduce by observing rendered output only (CONTRIBUTING §2):
    // type a sentinel into the dived-into session's buffer and watch
    // it disappear when the click switches the active window to a
    // different session whose buffer is empty.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("alphaproj") && s.contains("beta")
    })
    .unwrap();

    // Highlight beta then dive. The `activate` handler in
    // `orchestrator.ts` calls `setActiveWindow(beta)` and blurs the
    // dock synchronously, so the test doesn't depend on the
    // live-switch's 30 ms debounce landing first.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Wait for the dock's debounced live-switch (30 ms `editor.delay`
    // in `scheduleDockSwitch`) to actually flip active_window to beta.
    // Without this wait, Enter fires before the plugin event queue
    // processes Down's `select` event, so `openDialog.selectedIndex`
    // is still 0 and Enter activates alphaproj instead. Following the
    // `wait_for_prompt` (uses `is_prompting`) precedent — system-
    // readiness in test setup, asserted invariant is screen-only.
    let beta_root = root.join("wt-beta");
    h.wait_until(|h| h.editor().active_window().root == beta_root)
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Type a two-char sentinel into the dived-into buffer. With the
    // dock blurred and beta's `[No Name]` buffer active, the
    // keystrokes land in the buffer — proving the dive succeeded
    // AND giving a screen marker for "active session is beta". `ZZ`
    // avoids false matches with the chrome (no `Z` appears in any
    // dock label, menu, or status text by default).
    h.send_key(KeyCode::Char('Z'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('Z'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ZZ"))
        .unwrap();

    // Click alphaproj's row. With the fix:
    //   (a) `refocus_floating_panel` fires the `focus` widget_event,
    //       so the plugin's `dockBlurred` mirror clears, and
    //   (b) the click's `select` event then flips `active_window` to
    //       alphaproj — whose `[No Name]` buffer is empty, so `ZZ`
    //       leaves the chrome.
    let alpha_row = row_of(&h, "alphaproj") as u16;
    h.mouse_click(3, alpha_row).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("ZZ"))
        .unwrap();
}

#[test]
fn dock_initial_sort_is_lex_stable_not_current_first() {
    // Smoking-gun reproducer for the dock-reorder hypothesis behind the
    // Windows-only failure of `dock_list_order_is_stable_across_active_window_switch`.
    //
    // `openControlRoom` in `orchestrator.ts` runs the *first*
    // `filterSessions("")` at line 1757, BEFORE the `dockMode = true`
    // assignment at line 1765. So the dock's initial render uses
    // `pinCurrentFirst = !dockMode = true` — current-first ordering —
    // while every `refreshOpenDialog` afterward (active_window_changed,
    // window_created, …) uses `pinCurrentFirst = false` — the lex
    // ordering the dock comment explicitly mandates ("the dock is
    // persistent and switches the active session constantly, so it
    // must NOT reorder as the active project changes").
    //
    // Trigger: make the active window NOT the lex-first session, then
    // open the dock. The initial render puts the active session on top;
    // the stable order (which any subsequent active-change refresh
    // would have produced) is the lex order with aaa first.
    //
    // This bug is invisible to the existing
    // `dock_list_order_is_stable_across_active_window_switch` because
    // its launch session (aaa_project) is BOTH active and lex-first —
    // current-first and lex-first agree on the initial render. The
    // user-reported Windows failure of that test is consistent with
    // this bug surfacing through some environmental difference.
    let (_tmp_a, root_a) = setup_project("aaa_project");
    let parent = root_a.parent().unwrap().to_path_buf();
    let root_b = parent.join("zzz_project");
    fs::create_dir(&root_b).unwrap();
    assert!(std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root_b)
        .status()
        .unwrap()
        .success());

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root_a.clone())
            .unwrap();
    let zzz_id = h
        .editor_mut()
        .create_window_at(root_b.clone(), "zzz_project".to_string());
    // Make zzz active BEFORE opening the dock — that's the trigger.
    h.editor_mut().set_active_window(zzz_id);
    h.render().unwrap();
    open_dock(&mut h);

    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("aaa_project") && s.contains("zzz_project")
    })
    .unwrap();

    let aaa_row = row_of(&h, "aaa_project");
    let zzz_row = row_of(&h, "zzz_project");
    assert!(
        aaa_row < zzz_row,
        "dock initial order should be lex-stable (aaa above zzz); got \
         aaa at {aaa_row}, zzz at {zzz_row}.\n\
         Roots: aaa = {:?}, zzz = {:?}\n\
         Active root at assertion time: {:?}\n\
         Full screen for diagnosis:\n{}",
        root_a,
        root_b,
        h.editor().active_window().root,
        h.screen_to_string(),
    );
}

#[test]
fn dock_close_reflows_buffer_to_full_width() {
    // Open dock, then toggle it closed. The active window's buffer
    // must reflow to fill the freed columns on the LEFT — line 1's
    // gutter (`  1 │`) must move from inside the chrome (col ~32+)
    // back to column 0 immediately, without requiring any further
    // keypress / mouse-wheel. With the bug, the chrome stays at its
    // pre-close x-offset and the freed columns render as blank
    // whitespace until the user nudges the editor.
    let (_tmp, root) = setup_project("alphaproj");
    // A file with multiple lines so the gutter "  1 │" is observable.
    std::fs::write(root.join("readme.txt"), "alpha\nbeta\ngamma\n").unwrap();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut().open_file(&root.join("readme.txt")).unwrap();
    h.render().unwrap();
    h.wait_until(|h| h.screen_to_string().contains("alpha"))
        .unwrap();
    open_dock(&mut h);

    // Sanity: with the dock open, line 1's gutter "  1 │" lives in
    // the chrome (right of the dock column), so it sits beyond col 30.
    h.wait_until(|h| h.screen_to_string().contains("alpha"))
        .unwrap();
    let with_dock_col = h
        .screen_to_string()
        .lines()
        .find_map(|l| l.find("  1 │").map(|c| (c, l.to_string())))
        .expect("`  1 │` gutter on screen with dock open");
    assert!(
        with_dock_col.0 > 30,
        "with dock open, line-1 gutter should be in chrome (col > 30); got col {}: {:?}",
        with_dock_col.0,
        with_dock_col.1,
    );

    // Toggle the dock closed via the command palette — the same
    // path the user took in the interactive repro.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("ORCHESTRATOR"))
        .unwrap();

    // After the dock closes, the line-1 gutter must land at col 0
    // (or very near it) — the chrome filled the freed space.
    let after_close_col = h
        .screen_to_string()
        .lines()
        .find_map(|l| l.find("  1 │").map(|c| (c, l.to_string())))
        .expect("`  1 │` gutter still on screen after dock close");
    assert!(
        after_close_col.0 < 5,
        "after dock close, line-1 gutter should be at the left edge \
         (col < 5); got col {} — chrome did not reflow to fill the \
         freed dock columns. Row: {:?}",
        after_close_col.0,
        after_close_col.1,
    );
}
