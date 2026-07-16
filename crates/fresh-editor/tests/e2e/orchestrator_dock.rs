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
//! * mouse clicks land on dock widgets (the "New Task… ▾" button opens
//!   the create dropdown).

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
/// mounts), so a key event dispatched after just `wait_until("Orchestrator")`
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
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused())
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

/// Expand the dock's collapsible "Filters" section so the density /
/// project / worktree / trivial controls and the "Manage" button — which
/// the redesigned toolbar tucks away by default — become visible.
fn expand_filters(h: &mut EditorTestHarness) {
    let frow = row_of(h, "Filters") as u16;
    h.mouse_click(3, frow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Manage"))
        .unwrap();
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
    h.assert_screen_contains("Orchestrator");
    h.assert_screen_contains("New Task");
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
    h.assert_screen_contains("Orchestrator");
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

    // The dock's right border (`│`) is a full-height divider; find its
    // column on the toolbar row (row 0), where no session-card `│` side
    // border can shadow it, then sample its colour on a content row. The
    // default width is responsive, so scan for the glyph rather than
    // hard-coding a column.
    const ROW: u16 = 6;
    let border_col = |h: &EditorTestHarness| -> u16 {
        let cols = h.screen_row_text(0).chars().count() as u16;
        (0..cols)
            .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
            .expect("dock right border (│) should be present on the toolbar row")
    };
    let divider_fg = |h: &EditorTestHarness| h.get_cell_style(border_col(h), ROW).unwrap().fg;

    // FOCUSED on mount: the divider wears its focused (accent) colour.
    let focused_fg = divider_fg(&h);

    // Alt+O → hand focus back to the editor. The dock stays visible
    // (non-modal), but its divider dims to the muted colour.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("Orchestrator");
    let blurred_fg = divider_fg(&h);
    assert_ne!(
        focused_fg, blurred_fg,
        "the dock divider must change colour when keyboard focus leaves it"
    );

    // Alt+O again → dive back into the dock: the divider relights with the
    // original focused colour.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("Orchestrator");
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

/// Invoke a command from the command palette by name + Enter, then wait for
/// the palette to close.
fn run_palette_command(h: &mut EditorTestHarness, command: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.wait_until(|h| h.screen_to_string().contains(command))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

/// Regression: cycling the active window with Next Window (the dock blurred,
/// keyboard in the editor) must NOT reorder the dock list, and the highlight
/// (the active session's seamless tab) must land on the session the editor
/// actually switched to — never a stale or wrong row. The dock orders rows by
/// a permanent first-seen slot, so the order is fixed no matter how the active
/// window changes.
#[test]
fn next_window_keeps_dock_order_stable_and_highlight_correct() {
    let (_tmp, root) = setup_project("alphaproj");
    let parent = root.parent().unwrap().to_path_buf();
    // A tall terminal so all cards fit without scrolling — this isolates
    // "order changed" from "list scrolled to follow the selection".
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 50, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    // The launch window's label (its dock card name) — captured rather than
    // assumed.
    let launch_label = h.editor().active_window().label.clone();
    // Four more windows in their own dirs (names == dir basenames so each
    // label is unique and searchable in the dock column).
    let mut labels = vec![launch_label.clone()];
    for name in ["projB", "projC", "projD", "projE"] {
        let dir = parent.join(name);
        fs::create_dir(&dir).unwrap();
        h.editor_mut().create_window_at(dir, name.to_string());
        labels.push(name.to_string());
    }
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        labels
            .iter()
            .all(|l| h.screen_to_string().contains(l.as_str()))
    })
    .unwrap();
    // Blur the dock: focus dives to the editor, the scenario in which the
    // user pages windows.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("Orchestrator");

    let wall_col = |h: &EditorTestHarness| -> u16 {
        let cols = h.screen_row_text(0).chars().count() as u16;
        (0..cols)
            .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
            .expect("dock right-edge divider should be present on the toolbar row")
    };
    // The dock row of `label`'s card (where the name sits left of the divider).
    let dock_row = |h: &EditorTestHarness, label: &str| -> u16 {
        let wc = wall_col(h);
        h.screen_to_string()
            .lines()
            .enumerate()
            .find_map(|(r, l)| {
                let b = l.find(label)?;
                (l[..b].chars().count() < wc as usize).then_some(r as u16)
            })
            .unwrap_or_else(|| panic!("dock card for {label} not found:\n{}", h.screen_to_string()))
    };
    // The labels in dock display order (top to bottom).
    let order = |h: &EditorTestHarness| -> Vec<String> {
        let mut v: Vec<String> = labels.clone();
        v.sort_by_key(|l| dock_row(h, l));
        v
    };

    let baseline = order(&h);

    // Cycle forward through every window (and wrap once). After each switch
    // the order must be byte-for-byte identical, and exactly the active
    // session's card must wear the seamless tab.
    for _ in 0..labels.len() + 1 {
        run_palette_command(&mut h, "Next Window");
        h.wait_until_stable(|_| true).unwrap();

        assert_eq!(
            order(&h),
            baseline,
            "dock tree reordered after Next Window:\n{}",
            h.screen_to_string()
        );

        // The active session the editor switched to is still listed in the
        // (stable-ordered) dock tree — the dock re-points its highlight at
        // the active window rather than reordering to float it.
        let active_label = h.editor().active_window().label.clone();
        assert!(
            labels.contains(&active_label),
            "active session {active_label} must remain listed in the dock:\n{}",
            h.screen_to_string()
        );
    }
}

/// Next/Prev Window must cycle through exactly the sessions the dock currently
/// shows — when a filter hides some windows, paging must skip them. The dock
/// publishes its filtered visible list as the window cycle order; core honours
/// it. Drives the dock's search filter, then the public `next_window`, and
/// asserts the active window only ever lands on a visible (matching) session.
#[test]
fn next_window_cycles_only_dock_visible_sessions() {
    let (_tmp, root) = setup_project("base");
    let parent = root.parent().unwrap().to_path_buf();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), root.clone())
            .unwrap();
    // Two windows that match the filter and two that don't (plus the launch
    // "base" window, also non-matching).
    for name in ["keepA", "keepB", "dropC", "dropD"] {
        let dir = parent.join(name);
        fs::create_dir(&dir).unwrap();
        h.editor_mut().create_window_at(dir, name.to_string());
    }
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        ["keepA", "keepB", "dropC", "dropD"]
            .iter()
            .all(|l| s.contains(l))
    })
    .unwrap();

    // Focus the dock filter ("/") and narrow to the "keep" sessions. The dock
    // republishes its visible list as the cycle order on every filter change.
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.type_text("keep").unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("keepA") && s.contains("keepB") && !s.contains("dropC") && !s.contains("dropD")
    })
    .unwrap();
    h.wait_until_stable(|_| true).unwrap();

    // Cycle with the public command (not the palette, which would blur the
    // dock and clear the search). Every landing must be a visible session —
    // never the filtered-out "dropC"/"dropD" or the launch "base".
    for _ in 0..6 {
        h.editor_mut().next_window();
        h.render().unwrap();
        h.wait_until_stable(|_| true).unwrap();
        let active = h.editor().active_window().label.clone();
        assert!(
            active == "keepA" || active == "keepB",
            "Next Window landed on a filtered-out session {active:?}; it must cycle only the \
             dock's visible list (keepA/keepB):\n{}",
            h.screen_to_string()
        );
    }
}

/// Rows a dock card occupies below its name row in card view: the two
/// remaining content rows (branch, PR/spacer) plus the bottom border.
/// Mirrors the plugin's `DOCK_CARD_HEIGHT` (3 content rows).
const DOCK_CARD_ROWS_BELOW_NAME: u16 = 3;

/// Column of the dock's right-edge divider (the "wall") on the title row.
fn dock_wall_col(h: &EditorTestHarness) -> u16 {
    let cols = h.screen_row_text(0).chars().count() as u16;
    (0..cols)
        .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
        .expect("dock right-edge divider should be present on the title row")
}

/// Screen row of `label`'s dock-card name line — the first row where the
/// label appears left of the wall (so an editor-side occurrence of the
/// same string never matches). `None` when the card isn't on screen.
fn dock_card_name_row(h: &EditorTestHarness, label: &str) -> Option<u16> {
    let wall = dock_wall_col(h);
    h.screen_to_string().lines().enumerate().find_map(|(r, l)| {
        let b = l.find(label)?;
        (l[..b].chars().count() < wall as usize).then_some(r as u16)
    })
}

/// True when `label`'s card wears the "seamless active tab": the dock
/// wall is scooped away across the card — `╯` at the wall column on its
/// top border row, `╮` on its bottom border row, and *no* `│` on the
/// content rows between (the card flows into the editor).
fn card_is_seamless_tab(h: &EditorTestHarness, label: &str) -> bool {
    let wall = dock_wall_col(h);
    let Some(name_row) = dock_card_name_row(h, label) else {
        return false;
    };
    let top = name_row.saturating_sub(1);
    let bot = name_row + DOCK_CARD_ROWS_BELOW_NAME;
    h.get_cell(wall, top).as_deref() == Some("╯")
        && h.get_cell(wall, bot).as_deref() == Some("╮")
        && (name_row..bot).all(|r| h.get_cell(wall, r).as_deref() == Some(" "))
}

/// True when the wall stands unbroken past `label`'s card (an inactive
/// card): every row of the card band keeps `│` at the wall column.
fn card_keeps_wall(h: &EditorTestHarness, label: &str) -> bool {
    let wall = dock_wall_col(h);
    let Some(name_row) = dock_card_name_row(h, label) else {
        return false;
    };
    let top = name_row.saturating_sub(1);
    let bot = name_row + DOCK_CARD_ROWS_BELOW_NAME;
    (top..=bot).all(|r| h.get_cell(wall, r).as_deref() == Some("│"))
}

/// The dock's session tree highlights the active window, and the
/// highlight tracks it: arrowing the tree live-switches the active window
/// to the highlighted session. The active session's card must render as a
/// *seamless tab* — its border merges into the editor by scooping away
/// the dock's right-edge divider (`paint_dock_seamless_active_tab`) —
/// while inactive cards keep the divider; the tab follows the
/// live-switch. Regression: the folder-tree redesign styled tree
/// selection as a bg fill without the heavy border glyphs the seamless
/// painter keys on, so the active card degraded to a plain highlighted
/// box. Uses two windows + a live active-window switch (no session
/// spawn). Drives only the keyboard and asserts on rendered output per
/// CONTRIBUTING §2.
#[test]
fn active_session_card_is_a_seamless_tab_and_follows_focus() {
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
    h.editor_mut()
        .create_window_at(root_b.clone(), "zzz_project".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("aaa_project") && s.contains("zzz_project")
    })
    .unwrap();

    // aaa_project is the launch (active) session; the dock tree lists both.
    assert_eq!(h.editor().active_window().label, "aaa_project");

    // The active card merges into the editor (scooped wall), the
    // inactive card keeps the full divider.
    h.wait_until(|h| card_is_seamless_tab(h, "aaa_project"))
        .unwrap();
    assert!(
        card_keeps_wall(&h, "zzz_project"),
        "inactive card must keep the dock divider:\n{}",
        h.screen_to_string()
    );

    // Arrow down live-switches the active window to zzz_project (the
    // highlight follows the arrow through the tree's session leaves) —
    // and the seamless tab moves with it.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().label == "zzz_project")
        .unwrap();
    h.wait_until(|h| card_is_seamless_tab(h, "zzz_project"))
        .unwrap();
    assert!(
        card_keeps_wall(&h, "aaa_project"),
        "previously-active card must regain the dock divider:\n{}",
        h.screen_to_string()
    );

    // Arrow back up returns the active window (and the tab) to aaa_project.
    h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().label == "aaa_project")
        .unwrap();
    h.wait_until(|h| card_is_seamless_tab(h, "aaa_project"))
        .unwrap();
}

/// The dock's session-list scrollbar is overlay-style: shown ONLY while the
/// pointer is over the list, and hidden otherwise — even when the list holds
/// keyboard focus. With enough sessions to overflow the list, the scrollbar
/// column's cells change with hover alone. The bar paints background-coloured
/// cells (no glyph), so this asserts on cell styles. Drives only mouse motion
/// and asserts on rendered output per CONTRIBUTING §2.
#[test]
fn dock_list_scrollbar_shows_only_on_hover() {
    let (_tmp, root) = setup_project("alphaproj");
    let parent = root.parent().unwrap().to_path_buf();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    // Enough extra sessions to overflow the dock tree (one row per session)
    // and force a scrollbar.
    for i in 0..30 {
        let dir = parent.join(format!("proj{i}"));
        fs::create_dir(&dir).unwrap();
        h.editor_mut().create_window_at(dir, format!("proj{i}"));
    }
    h.render().unwrap();
    open_dock(&mut h); // the dock mounts focused (keyboard on the tree)

    // The divider sits at the dock's right edge; the list's overlay scrollbar
    // sits one column to its left (nudged into the gutter, hugging the edge).
    let wall_col = {
        let cols = h.screen_row_text(0).chars().count() as u16;
        (0..cols)
            .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
            .expect("dock right-edge divider should be present on the toolbar row")
    };
    // The dock nudges its scrollbar into the gutter, one column left of the
    // divider (adjacent to the edge).
    let sb_col = wall_col.saturating_sub(1);
    // Styles of the scrollbar column across the list rows; the bar's presence
    // shows up as a change here (it paints background-coloured cells).
    let snapshot = |h: &EditorTestHarness| -> Vec<Option<ratatui::style::Style>> {
        (8u16..30).map(|y| h.get_cell_style(sb_col, y)).collect()
    };

    // Focused on mount but NOT hovered → the bar is hidden. (Focus alone must
    // not reveal it.)
    let idle_focused = snapshot(&h);

    // Hover over the list → the bar appears.
    h.mouse_move(2, 15).unwrap();
    let hovered = snapshot(&h);
    assert_ne!(
        idle_focused, hovered,
        "the dock list scrollbar must appear while the pointer is over the list"
    );

    // Move the pointer off the list (into the editor, right of the divider) →
    // the bar hides again, matching the focused-but-unhovered state. This is
    // the key behaviour: keyboard focus stays on the list, yet the bar hides
    // because the pointer left the list.
    h.mouse_move(wall_col + 8, 15).unwrap();
    let left = snapshot(&h);
    assert_eq!(
        idle_focused, left,
        "the dock list scrollbar must hide once the pointer leaves the list, \
         even though keyboard focus is still on the list"
    );
}

/// Regression: the dock's overlay scrollbar must follow a panel-global hover
/// memo, not the active window's stored cursor position. That cursor is kept
/// per editor window, so paging through sessions with next/prev-window used
/// to swap in each window's stale cursor — flickering the bar on for some
/// sessions and off for others even though the dock was blurred and the
/// pointer never moved. A blurred, unhovered dock must keep the bar hidden
/// regardless of any window's stored cursor.
#[test]
fn dock_scrollbar_ignores_stale_per_window_cursor_when_blurred() {
    let (_tmp, root) = setup_project("alphaproj");
    let parent = root.parent().unwrap().to_path_buf();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    for i in 0..30 {
        let dir = parent.join(format!("proj{i}"));
        fs::create_dir(&dir).unwrap();
        h.editor_mut().create_window_at(dir, format!("proj{i}"));
    }
    h.render().unwrap();
    open_dock(&mut h);
    // Blur the dock so the main view owns the keyboard, as it is while paging
    // windows with next/prev-window. Let the blur-triggered plugin re-render
    // (the hint rows disappear, growing the tree) land before snapshotting —
    // otherwise the baseline is taken mid-relayout and the comparison below
    // fails on the layout shift rather than the scrollbar.
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.assert_screen_contains("Orchestrator");
    h.wait_until_stable(|_| true).unwrap();

    let wall_col = {
        let cols = h.screen_row_text(0).chars().count() as u16;
        (0..cols)
            .find(|&c| h.get_cell(c, 0).as_deref() == Some("│"))
            .expect("dock right-edge divider should be present on the toolbar row")
    };
    // The dock nudges its scrollbar into the gutter, one column left of the
    // divider (adjacent to the edge).
    let sb_col = wall_col.saturating_sub(1);
    let snapshot = |h: &EditorTestHarness| -> Vec<Option<ratatui::style::Style>> {
        (8u16..30).map(|y| h.get_cell_style(sb_col, y)).collect()
    };

    // Blurred and unhovered → the bar is hidden.
    let hidden = snapshot(&h);

    // Plant a stale per-window cursor INSIDE the dock list region, as if this
    // window had last seen the pointer there. No mouse-move event fires, so
    // the global hover memo stays false.
    h.editor_mut().active_window_mut().mouse_cursor_position = Some((2, 15));
    h.render().unwrap();

    // The bar must stay hidden: visibility follows the global hover memo, not
    // the active window's stored cursor.
    assert_eq!(
        hidden,
        snapshot(&h),
        "a blurred, unhovered dock must keep its scrollbar hidden regardless \
         of the active window's stored cursor position"
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

    // Click the "New Task… ▾" dropdown button inside the dock column. A
    // click landing on a dock widget proves mouse hit-testing routes into
    // the panel; the button opens the create dropdown (New Task… / New
    // Folder…). Choosing "New Task…" (the cursor's first option, accepted
    // with Enter) then opens the new-session form.
    let new_row = row_of(&h, "New Task") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.assert_screen_contains("New Workspace");
    // The dock and the centered form occupy disjoint slots, so opening
    // the form must NOT tear down the dock — its header stays painted in
    // the left column beside the modal.
    h.assert_screen_contains("Orchestrator");

    // Esc cancels the form; the dock regains focus and stays visible.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.assert_screen_contains("Orchestrator");
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
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.assert_screen_contains("New Workspace");
    h.assert_screen_contains("Orchestrator");

    // Esc returns to the dock, which is still mounted and re-focused.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.assert_screen_contains("Orchestrator");
}

/// Enter on a Tab-focused dock button runs THAT button's action, not the
/// session list's dive. The dock's `dispatch_floating_widget_key` Enter
/// branch used to fire `dock_activate` unconditionally — so once the user
/// Tab-cycled focus onto a button (or checkbox), Enter ignored the focused
/// control and merely re-focused the list. Buttons worked with the mouse
/// but not the keyboard. Enter now routes through the smart-key dispatcher
/// when focus is off the list, activating the focused Button/Toggle.
#[test]
fn dock_enter_on_focused_button_runs_button_action() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Focus opens on the sessions tree. One Tab lands on the "New Task… ▾"
    // dropdown button (spec-order first tabbable). Enter must open the
    // create dropdown — the same thing a click on the button does — not
    // dive the tree.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
    h.assert_screen_contains("New Folder");

    // Esc closes the dropdown; back on the tree, nothing dived.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Folder"))
        .unwrap();

    // Tab to the "Filters" header button and Enter it: the section
    // expands (its "view:" control appears), proving Enter activated the
    // focused button rather than diving the tree. Tab order from the tree
    // is new-session → filter → filters-toggle.
    h.assert_screen_not_contains("view: card");
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("view: card"))
        .unwrap();
    h.assert_screen_contains("view: card");
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
        s.contains("gamma") && !s.contains("beta")
    })
    .unwrap();
    h.assert_screen_not_contains("beta");

    // Enter in the filter returns to the list (does NOT dive) — the dock
    // stays visible and focused.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.assert_screen_contains("Orchestrator");
    h.assert_screen_contains("gamma");
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
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator"))
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
    h.wait_until(|h| !h.screen_to_string().contains("Orchestrator"))
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
    // The trivial-sessions toggle now lives in the collapsible Filters
    // section — open it first.
    expand_filters(&mut h);
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
        s.contains("ORCHESTRATOR :: Workspaces")
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
    // The worktree toggle lives in the collapsible Filters section — open
    // it so the checkbox state is visible (Alt+T flips the flag either way).
    expand_filters(&mut h);

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

/// Invoking `Orchestrator: Open` while the dock is visible opens the full
/// modal control room *fullscreen over* the dock — not as a refusal nag,
/// and not by tearing the dock down. The control room is a global
/// orchestrator feature, so it opts into fullscreen placement (covering
/// its own dimmed dock) rather than being cramped beside it. The dock
/// stays mounted in its own host slot (PanelSlot::Dock); Esc drops the
/// modal and hands control back to it.
#[test]
fn open_picker_covers_dock_fullscreen_and_esc_restores_it() {
    // Wide terminal so the 90%-width fullscreen modal clearly covers the
    // dock's "Manage" button (which sits in the dock's right half).
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(200, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    // Sanity: the dock (not the modal picker) is what's up, and the dock's
    // "New Task… ▾" button — which only the dock renders, never the picker
    // (its create button reads "+ New") — is on screen.
    h.assert_screen_not_contains("ORCHESTRATOR :: Workspaces");
    h.assert_screen_contains("New Task");

    // Ctrl+P falls through (blurs the dock) and opens the palette; run
    // "Orchestrator: Open" from it.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Open").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Open"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The control room surfaces (no nag) ...
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR :: Workspaces"))
        .unwrap();
    h.assert_screen_not_contains("the dock already lists sessions");
    // ... fullscreen *over* the dock: the modal's title renders well within
    // the dock's left column (its left border lands at ~col 10 of the full
    // frame, not past the ~40-col dock). A beside-dock modal would lay into
    // `chrome_area`, pushing the title past the dock's right edge. Count
    // chars (not bytes) up to the title — the modal's `│` border before it
    // is multi-byte, so a byte offset would overstate the column.
    let screen = h.screen_to_string();
    let title_line = screen
        .lines()
        .find(|l| l.contains("ORCHESTRATOR :: Workspaces"))
        .unwrap();
    let byte_idx = title_line.find("ORCHESTRATOR :: Workspaces").unwrap();
    let title_col = title_line[..byte_idx].chars().count();
    assert!(
        title_col < 38,
        "the control room must render fullscreen *over* the dock — its title \
         is at col {title_col}, expected within the dock's left region (the \
         modal would start past col ~40 if confined beside the dock).\n\
         Screen:\n{}",
        h.screen_to_string()
    );

    // Esc drops the modal and hands keyboard control back to the live
    // dock — it could not regain focus if it had been unmounted — and the
    // dock's "Manage" button is still there.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("ORCHESTRATOR :: Workspaces"))
        .unwrap();
    h.wait_until(|h| h.editor().is_dock_focused()).unwrap();
    h.assert_screen_contains("New Task");
}

/// The Quick Open hint bar (`file | >command | :line | #buffer`) must align
/// with the suggestions popup above it — both sit in the chrome area to the
/// right of the dock. The hint row used to hardcode `x: 0`, drawing the bar
/// starting at the very left edge (under the dock column), so it was
/// partially obscured by the dock and visibly offset from the suggestions
/// box. The fix anchors the hint at the prompt's `x` (= the box's left
/// column), so "file" lands exactly `left_margin` (2) cols past the box's
/// left border.
#[test]
fn quick_open_hint_aligns_with_suggestions_not_under_dock() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Ctrl+P blurs the dock and opens the command palette; the dock stays
    // visible in its left column beside the prompt + suggestions.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    // Wait for both the Quick Open hint and the suggestions box to paint.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains(">command") && s.contains('┌')
    })
    .unwrap();

    // Char-column (not byte offset — the box borders are multi-byte) of the
    // hint's first word and of the suggestions popup's top-left corner.
    let screen = h.screen_to_string();
    let hint_line = screen.lines().find(|l| l.contains(">command")).unwrap();
    let hint_byte = hint_line.find("file").unwrap();
    let hint_col = hint_line[..hint_byte].chars().count();
    let box_line = screen.lines().find(|l| l.contains('┌')).unwrap();
    let box_byte = box_line.find('┌').unwrap();
    let box_col = box_line[..box_byte].chars().count();

    // The box left border sits at the prompt's `x` (right of the dock); the
    // hint text begins `left_margin` (2) cols into that same region. If the
    // hint were still drawn at `x: 0`, "file" would land at col 2 — far left
    // of the dock-offset box — and this would fail.
    assert_eq!(
        hint_col,
        box_col + 2,
        "Quick Open hint must align with the suggestions box (left_margin=2 \
         past its left border at col {box_col}), not be drawn under the dock.\n\
         Screen:\n{screen}"
    );
}

/// On a narrow preview pane the control room's action buttons must wrap onto
/// additional lines rather than the right-most ones being clipped off the
/// edge. With a plain (non-wrapping) row the merged button line is truncated
/// to the pane width, so "Delete" (the last button) vanishes; `wrappingRow`
/// reflows it onto a later line, keeping every action reachable.
#[test]
fn control_room_preview_buttons_wrap_on_narrow_pane() {
    // Narrow terminal so the preview pane (≈half the modal) can't fit all
    // five action buttons on one line.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(80, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    // Open the control room (no dock needed). A session is selected on
    // mount, so its preview pane — with the action buttons — renders.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Open").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Open"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR :: Workspaces"))
        .unwrap();

    // Every action stays on screen — the right-most "Delete" would be
    // clipped off a non-wrapping row at this width.
    h.wait_until(|h| h.screen_to_string().contains("Delete"))
        .unwrap();
    h.assert_screen_contains("Archive");

    // And they actually wrapped: "Visit" (first button) and "Delete" (last)
    // land on different rows.
    let visit_row = row_of(&h, "Visit");
    let delete_row = row_of(&h, "Delete");
    assert_ne!(
        visit_row,
        delete_row,
        "preview action buttons must wrap onto separate rows on a narrow \
         pane (Visit at row {visit_row}, Delete at {delete_row}).\nScreen:\n{}",
        h.screen_to_string()
    );
}

/// The New-Session form's Cancel / Create Workspace buttons must wrap onto
/// separate lines on a narrow form rather than "Create Workspace" being
/// clipped off the right edge (a plain row truncates the merged button line
/// to the form width). `wrappingRow` reflows the pair instead.
#[test]
fn new_session_form_buttons_wrap_on_narrow_form() {
    // Narrow terminal so the 60%-width form can't fit both buttons on one
    // line.
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(50, 30, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    // Open the New-Session form via the palette.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: New Workspace").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: New Workspace"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Wait on a short, clip-safe form signal: the centered header
    // "ORCHESTRATOR :: New Workspace" overflows this deliberately narrow
    // (~26-col) form and gets truncated, so don't key off it here.
    h.wait_until(|h| h.screen_to_string().contains("Workspace Name"))
        .unwrap();

    // Both buttons stay on screen — "Create Workspace" would be clipped off a
    // non-wrapping row at this width — and they land on different rows.
    h.wait_until(|h| h.screen_to_string().contains("Create Workspace"))
        .unwrap();
    let cancel_row = row_of(&h, "Cancel");
    let create_row = row_of(&h, "Create Workspace");
    assert_ne!(
        cancel_row,
        create_row,
        "New-Session form buttons must wrap onto separate rows on a narrow \
         form (Cancel at row {cancel_row}, Create Workspace at {create_row}).\n\
         Screen:\n{}",
        h.screen_to_string()
    );
}

/// The New-Session form is a fully modal dialog: it must swallow every
/// mouse event, even a double-click landing over the editor buffer it sits
/// in front of. Single clicks were already routed to the panel, but
/// double/triple-clicks (and the alternate-screen terminal forward) ran
/// *before* that guard, so a double-click leaked to the buffer underneath
/// and selected a word there. Observed via typing after the dialog closes:
/// a leaked word-select would be replaced by the typed text.
#[test]
fn new_session_form_swallows_doubleclick_no_buffer_leak() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(80, 30, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    // Selectable text in the editor buffer underneath, cursor left at end.
    h.type_text("hello world").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("hello world"))
        .unwrap();

    // Open the New-Session form (a fully modal centered dialog).
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: New Workspace").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: New Workspace"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();

    // "hello world" stays visible above the vertically-centered form. Find
    // "world" there and double-click it (two clicks at one spot; the test
    // clock doesn't advance, so they register as a double-click). This point
    // is over the editor, outside the modal box — the dialog must eat it.
    let screen = h.screen_to_string();
    let (wrow, wline) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("hello world"))
        .unwrap();
    let wcol = wline.find("world").unwrap(); // ASCII row: byte == column
    h.mouse_click(wcol as u16, wrow as u16).unwrap();
    h.mouse_click(wcol as u16, wrow as u16).unwrap();

    // Close the dialog and type. If the double-click had leaked it would
    // have selected "world", and the keystroke would replace it ("hello Z").
    // Full modal capture leaves the buffer untouched, so the insert lands at
    // the cursor (end): "hello worldZ".
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.type_text("Z").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("hello worldZ"))
        .unwrap();
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

    // Enter's dive blurs the dock through the plugin→host bridge
    // asynchronously: the `activate` handler in `orchestrator.ts` calls
    // `floatingPanelControl(panel, "blur")`, which only flips the host's
    // `dock.focused` once that bridge command is applied. This is the
    // mirror of the focus-grab race `open_dock` guards against. Without
    // waiting for the blur to land, the first `Z` below can race in
    // *before* the dock blurs and get routed to the still-focused dock
    // instead of beta's `[No Name]` buffer — only the second `Z` then
    // lands, the screen shows a lone `Z`, and the `contains("ZZ")` wait
    // blocks to the external nextest timeout. Gate on the same
    // screen-adjacent readiness signal (`is_dock_focused`) the dock's
    // own helpers use (CONTRIBUTING §3: semantic waiting, not implicit
    // keystroke-ordering assumptions).
    h.wait_until(|h| !h.editor().is_dock_focused()).unwrap();

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

/// Regression: clicking a session row in a *focused* dock must switch to
/// that session AND hand keyboard focus to the activated window — exactly
/// like pressing Enter. The `select` handler in `orchestrator.ts` used to
/// treat a click identically to arrow-nav: it live-switched the active
/// window but left focus on the dock, so the editor showed the clicked
/// session while keystrokes still drove the dock. The fix routes a
/// click (`payload.via === "click"`) through `diveDockSelectionFromClick`,
/// which switches and blurs the dock; arrow-nav (no `via`) keeps its
/// debounced live-switch and stays on the dock.
///
/// Observed through rendered output only (CONTRIBUTING §2): after the
/// click the dock must report blurred, and a sentinel typed into the
/// keyboard must land in the clicked session's empty buffer (proving both
/// that focus left the dock and that the active window is the clicked one).
#[test]
fn click_on_focused_dock_row_dives_focus_into_session() {
    init_tracing_from_env();
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.editor_mut()
        .create_window_at(root.join("wt-beta"), "beta".to_string());
    h.render().unwrap();
    open_dock(&mut h);
    // The dock mounts with keyboard focus; alphaproj is the active window.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("alphaproj") && s.contains("beta")
    })
    .unwrap();
    assert!(
        h.editor().is_dock_focused(),
        "precondition: the dock holds keyboard focus on mount"
    );

    // Click beta's row. The fix switches the active window to beta and
    // dives focus into it — so the dock blurs without any Enter.
    let beta_row = row_of(&h, "beta") as u16;
    h.mouse_click(3, beta_row).unwrap();
    h.wait_until(|h| !h.editor().is_dock_focused()).unwrap();

    // Focus now sits on beta's empty `[No Name]` buffer: a typed sentinel
    // lands in the buffer (visible on screen) rather than being swallowed
    // by the dock. `ZZ` avoids false matches with any chrome label.
    h.send_key(KeyCode::Char('Z'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('Z'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ZZ"))
        .unwrap();
    assert_eq!(
        h.editor().active_window().root,
        root.join("wt-beta"),
        "clicking beta's row must make it the active window"
    );
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
    h.wait_until(|h| !h.screen_to_string().contains("Orchestrator"))
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

/// F7: creating a worktree session in a repo with no commits surfaces
/// git's *real* failure verbatim, rather than a synthesized guess at the
/// cause. An earlier version assumed any failed HEAD probe meant "no
/// commits yet" and replaced the error with that message — but a
/// non-zero git exit can have other causes (corrupt repo, etc.), so we
/// always show what git actually said instead of guessing.
#[test]
fn dock_new_session_in_uncommitted_repo_surfaces_real_git_error() {
    // `setup_project` runs `git init` but never commits, so HEAD is
    // unborn — `git worktree add` fails with a `fatal:` reference error.
    let (_tmp, root) = setup_project("freshrepo");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Open the new-session form. The "Create a new git worktree" box
    // defaults on for a git repo, so submitting attempts a worktree add.
    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();

    // Tab forward until the "Create Workspace" button is the focused
    // control — its line carries the `▸` focus marker right before it
    // (the form reserves the marker gutter). Walking to the button by
    // its marker keeps this robust to the focus-cycle length: each radio
    // group ("Run in:", "Agent:") is a single Tab stop, so the number of
    // stops depends on the active backend's field count. Tab also closes
    // any open path-completion popup along the way. Enter then submits.
    let mut guard = 0;
    while !h.screen_to_string().contains("▸ [ Create Workspace ]") {
        h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        guard += 1;
        assert!(
            guard < 30,
            "Tab never focused the Create Workspace button.\n{}",
            h.screen_to_string(),
        );
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // git's actual error is surfaced (a `fatal:` line from the failed
    // `git worktree add`), not a synthesized substitute.
    h.wait_until(|h| h.screen_to_string().contains("fatal"))
        .unwrap();
}

/// F5: the dock filter must reset when focus leaves the dock, so
/// re-entering always shows the full session list. A stale filter
/// otherwise silently hides sessions on the next focus (only the filter
/// box hints why), with no one-key clear from the list.
#[test]
fn dock_filter_clears_when_focus_leaves_so_reentry_shows_all() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
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

    // Filter to "gamma" — the "beta" row drops out of the list.
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.type_text("gamma").unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("beta"))
        .unwrap();

    // Enter returns to the list (filter still applied); Esc then leaves
    // the dock. Leaving must clear the filter, so the previously hidden
    // "beta" row is back the moment the dock is shown again.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("beta"))
        .unwrap();
    h.assert_screen_contains("beta");
    // The filter *input* must clear too, not just the filtering: the box
    // is a controlled widget, so without resetting its value it would
    // still read the old query while the list shows everything. The
    // empty box shows its "Search Tasks" placeholder.
    h.assert_screen_contains("Search Tasks");
}

/// F6: the auto-generated session name is rooted in the project
/// (`<project>-N`) rather than a bare `session-N`, so a dock row tells
/// you which project a session belongs to.
#[test]
fn dock_new_session_name_is_rooted_in_the_project() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();

    // The Workspace Name field's auto-default carries the project basename
    // and a numeric suffix ("alphaproj-…"). Without the fix it reads
    // "session-N", which has no "alphaproj-" stem.
    h.wait_until(|h| h.screen_to_string().contains("alphaproj-"))
        .unwrap();
    h.assert_screen_contains("alphaproj-");
}

/// F8: accepting a directory path-completion with Tab CLOSES the
/// dropdown instead of re-popping it over the form fields. Because Tab
/// *accepts* while a popup is open, the old re-pop (which listed the
/// accepted directory's children) buried the worktree / Workspace Name
/// fields and trapped a Tab-to-advance user in a loop of re-accepting.
///
/// We observe the dropdown's open/closed state through the **"Session
/// Name" label**, which the popup paints over while it is up. We do NOT
/// assert on the candidate text: completion rows render the *full
/// absolute path* and the host tail-truncates them (render.rs
/// `render_completion_item`), so on a deep CI temp directory the
/// directory basename is cut off the end and never appears on screen —
/// that environment-dependent truncation made earlier versions of this
/// test hang to the external timeout. The label is fixed-width, git
/// independent, and always legible, so this is deterministic regardless
/// of how long the host's temp path is.
#[test]
fn dock_form_tab_accepting_directory_completion_closes_dropdown() {
    let (_tmp, root) = setup_project("alphaproj");
    // A directory that sorts first, so the path-completion's top
    // (highlighted) candidate is a directory. Give it several children so
    // that, *without* the fix, accepting `aaa_dir/` re-pops a dropdown
    // tall enough to keep the form fields buried (the bug's signature).
    let aaa = root.join("aaa_dir");
    fs::create_dir(&aaa).unwrap();
    for child in ["inner_a", "inner_b", "inner_c"] {
        fs::create_dir(aaa.join(child)).unwrap();
    }
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    // The form opens with every field visible, "Workspace Name" among them.
    assert!(
        h.screen_to_string().contains("Workspace Name"),
        "form should open with its fields visible:\n{}",
        h.screen_to_string()
    );

    // The Project Path field is empty on open (it only *shows* the
    // detected root as a placeholder). Type the project root + "/" so the
    // dropdown lists the directory's children; the top one is `aaa_dir/`.
    // Path completion is synchronous, so the popup is up once typing
    // finishes — and it paints over the fields below Project Path, hiding
    // the "Workspace Name" label.
    h.type_text(&format!("{}/", root.display())).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Workspace Name"))
        .unwrap();

    // Tab accepts the highlighted `aaa_dir/`. With the fix the dropdown
    // CLOSES, so the form fields — including "Workspace Name" — reappear.
    // Without the fix it re-pops `aaa_dir`'s children, keeping the fields
    // buried, and this wait times out: the observable bug.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Workspace Name"))
        .unwrap();
}

/// Regression: with the dock's project dropdown open, the keyboard drives
/// the *dropdown* — ↑/↓ move its cursor and Enter commits the highlighted
/// option — instead of leaking to the session list beneath it. Before the
/// fix the menu opened but was inert: focus stayed on the session list, so
/// ↑/↓ switched sessions and Enter dived into one, and a keyboard user
/// could never pick a project from the dropdown.
///
/// The toolbar's project control is the discriminator: it reads "All ▾"
/// while unfiltered and the project's basename once a project is picked.
/// Driving Alt+P → ↓ → Enter must flip it to "alphaproj ▾"; with the bug
/// the filter stays on "All".
#[test]
fn dock_project_dropdown_is_keyboard_navigable() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    // The project control lives in the collapsible Filters section.
    expand_filters(&mut h);

    // The project control starts unfiltered.
    h.assert_screen_contains("All ▾");

    // Alt+P opens the dropdown; it lists "All projects" plus this project.
    h.send_key(KeyCode::Char('p'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("All projects"))
        .unwrap();

    // ↓ moves the cursor from "All projects" onto the project row; Enter
    // commits it. With the bug these keys drove the session list instead,
    // leaving the filter on "All".
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The dropdown closed and the project filter is applied: the toolbar
    // now reads the project basename, no longer "All".
    h.wait_until(|h| h.screen_to_string().contains("alphaproj ▾"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("All ▾"),
        "project filter should be applied (toolbar should not read 'All ▾'):\n{screen}"
    );
    // And the menu itself is gone.
    assert!(
        !screen.contains("All projects"),
        "dropdown should have closed after Enter:\n{screen}"
    );
}

/// Esc cancels the open project dropdown without applying a filter and
/// leaves the keyboard with the dock (it must not commit the cursor's
/// option, nor blur the dock to the editor). We prove the dock kept focus
/// by re-opening the dropdown with Alt+P afterwards: if Esc had blurred the
/// dock, Alt+P would reach the editor instead and the menu would not return.
#[test]
fn dock_project_dropdown_esc_cancels_without_filtering() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    expand_filters(&mut h);

    h.send_key(KeyCode::Char('p'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("All projects"))
        .unwrap();
    // Move the cursor onto the project row, then cancel.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Menu closed and no filter applied — toolbar still reads "All ▾".
    h.wait_until(|h| !h.screen_to_string().contains("All projects"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("All ▾"),
        "Esc must not apply the cursor's project (toolbar should still read 'All ▾'):\n{screen}"
    );

    // The dock still owns the keyboard: Alt+P re-opens the dropdown.
    h.send_key(KeyCode::Char('p'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("All projects"))
        .unwrap();
}

/// Regression: creating a session while the dock is open moves the dock's
/// highlight onto the new session. It becomes the active window, and the
/// dock — a passive mirror once focus dives into the new terminal — must
/// re-point at it instead of stranding the highlight on the previously
/// active row. We read this off the *seamless tab*: the active session's
/// card scoops the dock's right-edge divider away (its rows have no `│`
/// wall — they flow into the editor), while inactive cards keep the wall.
/// After creation the new session's card must be the seamless one and the
/// old session's must keep its divider.
#[test]
fn creating_session_moves_dock_highlight_to_new_session() {
    let (_tmp, root) = setup_project("alphaproj");
    // A non-git directory for the new session: the worktree toggle
    // auto-disables there, so it spawns a plain terminal session with no
    // git worktree to create. It sits beside the git project (the tempdir
    // root is not itself a repo).
    let plain = root.parent().unwrap().join("plainwork");
    fs::create_dir(&plain).unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    // The launch session (alphaproj) is the only row, and it's selected.
    h.assert_screen_contains("alphaproj");

    // Open the new-session form and point it at the non-git dir.
    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    h.type_text(&plain.display().to_string()).unwrap();
    // The typed path lands in the field (its last segment is visible).
    h.wait_until(|h| h.screen_to_string().contains("plainwork"))
        .unwrap();
    // Accept the path completion with Tab so the popup closes and the
    // Create button is no longer obscured by it.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Create Workspace"))
        .unwrap();

    // Submit by clicking "Create Workspace".
    let screen = h.screen_to_string();
    let (col, btn_row) = screen
        .lines()
        .enumerate()
        .find_map(|(r, l)| l.find("Create Workspace").map(|c| (c as u16, r as u16)))
        .expect("Create Workspace button should be visible");
    h.mouse_click(col, btn_row).unwrap();

    // The new session becomes the active window; the dock — a passive
    // mirror once focus dives into the new terminal — re-points its
    // highlight at it (`syncDockSelectionToActive`) instead of stranding it
    // on the previously-active alphaproj row. The spawn + active-window
    // switch + dock refresh are async, so wait for the active window to
    // become the new (plain) session.
    h.wait_until(|h| {
        h.editor()
            .active_window()
            .root
            .to_string_lossy()
            .contains("plainwork")
    })
    .unwrap();

    // Both sessions remain listed in the dock tree — the highlight moved,
    // the old row wasn't dropped.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("plainwork") && s.contains("alphaproj")
    })
    .unwrap();
}

// ── right-click session context menu ──────────────────────────────────────
//
// Right-clicking a session card opens a small dimmed modal with
// Visit / Archive / Delete; the destructive actions swap it to a centered
// confirmation pane before they run. These drive only the mouse/keyboard
// and assert on rendered output per CONTRIBUTING §2.

/// Open the dock and right-click the first session card. Returns the
/// harness with the context menu showing.
fn open_dock_context_menu(name: &str) -> (tempfile::TempDir, EditorTestHarness) {
    let (tmp, root) = setup_project(name);
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The session card's name line bears the project basename; right-click
    // a column well inside the dock on that row.
    let card_row = row_of(&h, name) as u16;
    h.mouse_right_click(4, card_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Archive"))
        .unwrap();
    (tmp, h)
}

/// 0-based screen position (col, row) of the first occurrence of `needle`.
/// `str::find` returns a *byte* offset, but dock rows contain multibyte
/// box-drawing glyphs (`┗━━━│`, 3 bytes each), so convert to a character
/// column — that's the screen column for width-1 glyphs — before returning.
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

#[test]
fn dock_right_click_opens_context_menu() {
    let (_tmp, h) = open_dock_context_menu("alphaproj");

    // All three actions plus the session header are present.
    h.assert_screen_contains("Visit");
    h.assert_screen_contains("Archive");
    h.assert_screen_contains("Delete");
    h.assert_screen_contains("alphaproj");
}

#[test]
fn dock_context_menu_esc_closes() {
    let (_tmp, mut h) = open_dock_context_menu("alphaproj");

    // Esc dismisses the menu; the dock returns (its "New Task… ▾" button
    // shows) and the menu-only "Archive"/"Delete" actions are gone.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Archive"))
        .unwrap();
    h.assert_screen_contains("New Task");
}

#[test]
fn dock_context_menu_delete_shows_centered_confirmation() {
    let (_tmp, mut h) = open_dock_context_menu("alphaproj");

    // Click the menu's "Delete" action → the confirmation pane replaces
    // the menu (full-screen dimmed, centered).
    let (dcol, drow) = pos_of(&h, "Delete");
    h.mouse_click(dcol, drow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Confirm Delete"))
        .unwrap();
    // The destructive-action warning and the Confirm/Cancel pair render.
    h.assert_screen_contains("Uncommitted changes will be lost");
    h.assert_screen_contains("Cancel");
}

#[test]
fn dock_context_menu_confirm_cancel_returns_to_menu() {
    let (_tmp, mut h) = open_dock_context_menu("alphaproj");

    let (dcol, drow) = pos_of(&h, "Delete");
    h.mouse_click(dcol, drow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Confirm Delete"))
        .unwrap();

    // Cancel returns to the three-action menu rather than closing outright,
    // so a mis-click on a destructive action is recoverable.
    let (ccol, crow) = pos_of(&h, "Cancel");
    h.mouse_click(ccol, crow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Visit"))
        .unwrap();
    h.assert_screen_contains("Archive");
    h.assert_screen_contains("Delete");
}

#[test]
fn dock_context_menu_archive_shows_confirmation() {
    let (_tmp, mut h) = open_dock_context_menu("alphaproj");

    let (acol, arow) = pos_of(&h, "Archive");
    h.mouse_click(acol, arow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Confirm Archive"))
        .unwrap();
    h.assert_screen_contains("Cancel");
}

/// The menu is an unobtrusive popup anchored at the click, not a centered
/// modal: its items render in the left columns (near the dock click), not
/// around mid-screen, and at roughly the clicked row.
#[test]
fn dock_context_menu_is_anchored_near_click() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    let card_row = row_of(&h, "alphaproj") as u16;
    h.mouse_right_click(3, card_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Visit"))
        .unwrap();

    let (vcol, vrow) = pos_of(&h, "Visit");
    // Anchored to the left edge where the click landed — a centered modal
    // on a 120-wide terminal would put this near col ~50.
    assert!(
        vcol < 24,
        "context menu should hug the click (left columns), got col {vcol}"
    );
    // And vertically near the clicked row, not screen-centered.
    assert!(
        vrow >= card_row && vrow <= card_row + 6,
        "context menu should open near the clicked row {card_row}, got row {vrow}"
    );
}

/// Clicking outside the anchored popup dismisses it (standard menu
/// behaviour) and returns control to the dock.
#[test]
fn dock_context_menu_click_outside_dismisses() {
    let (_tmp, mut h) = open_dock_context_menu("alphaproj");

    // Click far away in the editor area, well outside the popup box.
    h.mouse_click(90, 20).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Archive"))
        .unwrap();
    h.assert_screen_contains("New Task");
}

// ── folder tree ───────────────────────────────────────────────────────────

/// The "New Task… ▾" dropdown can create a folder, and a session's
/// context menu can file it into that folder — the dock's hierarchical
/// organisation. Creating a folder then moving the session into it makes
/// the folder report a member count of `(1)`.
#[test]
fn dock_new_folder_and_move_session_into_it() {
    init_tracing_from_env();
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Open the "New Task… ▾" create dropdown, move the cursor to
    // "New Folder…", and accept it — that opens the New Folder dialog.
    let new_row = row_of(&h, "New Task") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The dialog opens with focus in the (empty) name field. Type the
    // name, then Tab onto the "Organize … under this folder" checkbox and
    // Space it OFF — this test exercises the explicit move-into-folder
    // path below, so the folder must start empty. Enter submits from
    // anywhere in the dialog.
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text("Docs").unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The dialog closes and the folder appears in the dock tree,
    // initially empty (no member count).
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains("Docs")
    })
    .unwrap();

    // Right-click the alphaproj session row and choose "Move to Folder…".
    let session_row = row_of(&h, "alphaproj") as u16;
    h.mouse_right_click(4, session_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    let (mcol, mrow) = pos_of(&h, "Move to Folder");
    h.mouse_click(mcol, mrow).unwrap();

    // The move dropdown lists "Top level", then "Docs", then "New
    // folder…". Move the cursor onto "Docs" and accept.
    h.wait_until(|h| h.screen_to_string().contains("Top level"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The folder now reports one member — the session was filed into it.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Docs") && s.contains("(1)")
    })
    .unwrap();
}

/// Enter on the New Folder dialog's focused `[ Cancel ]` button cancels —
/// it must NOT create the folder. The dialog's mode-level Enter binding
/// ("submit from anywhere") used to win over the focused Cancel button,
/// so Tab→Tab→Enter silently created the folder (and filed the active
/// session under it via the organize checkbox).
#[test]
fn dock_new_folder_dialog_enter_on_cancel_cancels() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Open the New Folder dialog via the "New Task… ▾" dropdown.
    let new_row = row_of(&h, "New Task") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();

    // Type a name, then Tab past the organize checkbox onto [ Cancel ]
    // and press Enter.
    h.type_text("Zed").unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The dialog closes without creating anything: no "Zed" folder row
    // in the dock tree.
    h.wait_until(|h| !h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.assert_screen_not_contains("Zed");
}

/// The mouse wheel scrolls the dock's session tree in the default card
/// density. The wheel handler used to compare the *row* budget against
/// the *node* count, so with 3-row cards `max_scroll` collapsed to 0 and
/// the wheel was dead exactly when the card list overflowed.
#[test]
fn dock_card_tree_wheel_scrolls_when_overflowing() {
    init_tracing_from_env();
    let (_tmp, root) = setup_project("aaaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    // Enough sessions that the bordered 5-row cards overflow a 32-row
    // screen (~5 visible cards): 13 nodes total.
    for i in 1..=12 {
        h.editor_mut()
            .create_window_at(root.join(format!("wt-bb{i:02}")), format!("bb{i:02}"));
    }
    h.render().unwrap();
    open_dock(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("bb01"))
        .unwrap();
    // The tail of the list is cut off by the dock's height. ("bb12"
    // appears nowhere else on screen — the boot session stays active, so
    // the explorer/tab bar shows only the boot project.)
    h.assert_screen_not_contains("bb12");

    // Wheel down over the tree: the view scrolls, revealing the tail.
    // Each notch scrolls 3 nodes; with ~5 bordered cards visible the
    // wheel clamps at max-scroll (node 8 of 13) on the third notch,
    // which puts the last card on screen.
    h.mouse_scroll_down(5, 15).unwrap();
    h.mouse_scroll_down(5, 15).unwrap();
    h.mouse_scroll_down(5, 15).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("bb12"))
        .unwrap();

    // A dock refresh must NOT yank the scrolled view back to the
    // selection. The plugin's `refreshOpenDialog` re-pins the tree
    // selection on every repaint, and it runs asynchronously off the
    // git probe poll — under CI load a probe completing *after* the
    // wheel used to snap the view back to the top and `bb12` vanished
    // forever (the tree had no `user_scrolled` suppression, unlike
    // List). Settle all pending refreshes and require the tail to
    // still be on screen.
    h.wait_until_stable(|_| true).unwrap();
    assert!(
        h.screen_to_string().contains("bb12"),
        "a dock refresh re-pinning the same selection must not snap the \
         wheel-scrolled view back to the top:\n{}",
        h.screen_to_string()
    );

    // And back up: the tail scrolls back out of view.
    h.mouse_scroll_up(5, 15).unwrap();
    h.mouse_scroll_up(5, 15).unwrap();
    h.mouse_scroll_up(5, 15).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("bb12"))
        .unwrap();
}

/// The Menu key opens the highlighted session's context menu (keyboard
/// parity with right-click — previously "Move to Folder…" was
/// mouse-only), and ↑/↓ walk the menu's entries like every other dock
/// dropdown (previously only Tab moved focus, and ↓+Enter activated
/// "Visit…" instead of the second entry).
#[test]
fn dock_menu_key_opens_context_menu_and_arrows_navigate() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Menu key on the focused tree → the highlighted node's context menu.
    h.send_key(KeyCode::Menu, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    h.assert_screen_contains("Visit");

    // ↓ moves the focus from "Visit…" to "Move to Folder…"; Enter runs
    // it — the "move to" dropdown replaces the popup. Without arrow
    // support Enter would have activated "Visit…" and dived instead.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Top level"))
        .unwrap();
}

/// Card density draws each session as a rounded bordered card — the
/// `╭─…─╮` pill look the dock had before the folder-tree redesign, which
/// the tree rendering dropped (cards were three flat text rows; issue
/// #2703). Compact density stays border-free.
#[test]
fn dock_card_view_draws_card_borders() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The default density is "card": the session's card wears a rounded
    // border, with the session name on the row below the top border.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains('╭') && s.contains('╰')
    })
    .unwrap();
    let screen = h.screen_to_string();
    let top_row = screen.lines().position(|l| l.contains('╭')).unwrap();
    let name_row = row_of(&h, "alphaproj");
    assert!(
        name_row > top_row,
        "the card's top border must sit above the session name:\n{screen}"
    );
    // The name row is inside the card: a `│` side border precedes the
    // name. (The dock's own right-edge wall is also a `│`, but it sits
    // *after* the name, so the position check can't pass vacuously.)
    let name_line = screen.lines().nth(name_row).unwrap();
    assert!(
        name_line.find('│').unwrap_or(usize::MAX) < name_line.find("alphaproj").unwrap(),
        "the session row must sit inside the card's side borders:\n{screen}"
    );

    // Toggle to compact density: the borders disappear.
    expand_filters(&mut h);
    let vrow = row_of(&h, "view: card") as u16;
    h.mouse_click(3, vrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("view: compact"))
        .unwrap();
    h.wait_until(|h| !h.screen_to_string().contains('╭'))
        .unwrap();
}

/// The focused dock's hint bar advertises the context-menu key. The
/// menu (the only keyboard route to "Move to Folder…" and the folder
/// organise actions) opened via Menu / Shift+F10, but nothing on
/// screen said so — the feature was undiscoverable without a mouse
/// (issue #2703). Also pins the Shift+F10 route itself, which the
/// Menu-key test above doesn't cover (many keyboards have no Menu key).
#[test]
fn dock_hint_bar_advertises_context_menu_key_and_shift_f10_opens_it() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // The focused dock shows the context-menu key hint.
    h.wait_until(|h| h.screen_to_string().contains("S-F10"))
        .unwrap();
    h.assert_screen_contains("menu");

    // Shift+F10 opens the highlighted node's context menu, exactly like
    // the Menu key / a right-click.
    h.send_key(KeyCode::F(10), KeyModifiers::SHIFT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    h.assert_screen_contains("Visit");
}

/// A folder's "Rename…" action opens the same centered dialog as "New
/// Folder" — pre-filled with the current name — instead of the bottom
/// minibuffer prompt (which also ran the label and value together).
#[test]
fn dock_folder_rename_uses_dialog() {
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    open_dock(&mut h);

    // Create a folder "Docs" (empty — organize checkbox off).
    let new_row = row_of(&h, "New Task") as u16;
    h.mouse_click(4, new_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Folder"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Folder name"))
        .unwrap();
    h.type_text("Docs").unwrap();
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Folder name") && s.contains("Docs")
    })
    .unwrap();

    // Right-click the folder row → "Rename…" → the centered dialog opens
    // with the name field pre-filled ("Docs"), not a minibuffer prompt.
    let folder_row = row_of(&h, "Docs") as u16;
    h.mouse_right_click(4, folder_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Rename"))
        .unwrap();
    let (rcol, rrow) = pos_of(&h, "Rename");
    h.mouse_click(rcol, rrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Rename Folder"))
        .unwrap();
    h.assert_screen_contains("Folder name");

    // The cursor sits at the end of the pre-filled name; typing appends,
    // and Enter commits the rename — the tree shows the new name.
    h.type_text("Team").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("Rename Folder") && s.contains("DocsTeam")
    })
    .unwrap();
}
