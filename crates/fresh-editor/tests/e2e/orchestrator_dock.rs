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
    assert!(aaa_before < zzz_before, "expected aaa above zzz initially");

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
        "dock list reordered on switch: aaa now at {aaa_after}, zzz at {zzz_after}"
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
    // box-drawing border char — the harness renders multi-byte glyphs as
    // raw bytes — but the menu word is ASCII and its delta tracks width.)
    // Default dock width is 32 → right border at col 31.
    let edit_before = col_in_row(&h, 0, "Edit");

    // Drag the right border (col 31) out to col 60 to widen the dock.
    h.mouse_drag(31, 6, 60, 6).unwrap();
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
    // The "show empty/1-file" toggle defaults to off (hide trivial
    // sessions). Clicking it flips the checkbox `[ ]` → `[v]`, proving the
    // dock toggle is wired to the shared hide-trivial filter.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("show empty/1-file"))
        .unwrap();
    let trow = row_of(&h, "show empty/1-file") as u16;
    // Off by default: unchecked.
    assert!(
        h.screen_row_text(trow).contains("[ ] show empty/1-file"),
        "expected toggle off by default: {:?}",
        h.screen_row_text(trow)
    );
    // Click it → checked.
    h.mouse_click(3, trow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("[v] show empty/1-file"))
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

#[test]
fn click_un_dive_switches_to_clicked_session() {
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
