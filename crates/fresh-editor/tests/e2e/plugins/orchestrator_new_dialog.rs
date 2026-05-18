//! E2E coverage for the Orchestrator "New Session" form's path-
//! completion popup behaviour:
//!
//! 1. The dropdown renders inside a bordered box (it used to be bare
//!    overlay rows painted on top of the worktree toggle).
//! 2. Tab accepts the highlighted suggestion into the field.
//! 3. Enter does NOT accept the suggestion — it leaves the typed
//!    text intact and proceeds (matches bash / fish / readline
//!    path-completion conventions). Before the fix, the host's
//!    picker-style smart-key wiring fired the completion list's
//!    activate event on Enter and silently overwrote the field.
//!
//! Each test sets up a workspace with two predictable subdirs
//! (`alpha_dir/` and `alpha_two/`). The Project Path is driven via
//! an absolute path (`<workspace>/al`) so the plugin's
//! `fetchPathCompletions` reads the workspace directly — its
//! `parent = "."` branch for un-slashed inputs would resolve
//! against the cargo-test process cwd, not the harness workspace.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;

/// Build a workspace with two `alpha*` subdirs and the orchestrator
/// plugin installed. Returns (tempdir guard, canonicalized
/// workspace path). The path is canonicalized so screen matching
/// is stable on systems where `/tmp` is a symlink (e.g. macOS).
fn set_up_workspace() -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    fs::create_dir(workspace.join("alpha_dir")).unwrap();
    fs::create_dir(workspace.join("alpha_two")).unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    (temp, workspace)
}

/// Workspace variant for the "popup with > visible_rows
/// candidates" scenario. Creates 10 `alpha_NN` subdirs so the
/// default-5 popup needs to scroll. Returns workspace path +
/// the sorted candidate-name list so callers can spot-check
/// which entries are visible / hidden in any given scroll
/// position.
fn set_up_workspace_many_alphas() -> (tempfile::TempDir, PathBuf, Vec<String>) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let mut names: Vec<String> = (0..10).map(|i| format!("alpha_{:02}", i)).collect();
    for n in &names {
        fs::create_dir(workspace.join(n)).unwrap();
    }
    names.sort();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    (temp, workspace, names)
}

fn wait_for_new_session_command(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            reg.get_all()
                .iter()
                .any(|c| c.get_localized_name() == "Orchestrator: New Session")
        })
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

/// Read the bracketed text inside the Project Path field from the
/// rendered screen. The field renders as `│ [<value>...] │` on the
/// row after the `Project Path` label. Returns the trimmed value.
fn project_path_field_value(screen: &str) -> String {
    let lines: Vec<&str> = screen.lines().collect();
    let label_row = lines
        .iter()
        .position(|l| l.contains("Project Path"))
        .expect("Project Path label must appear on screen");
    for next in lines.iter().skip(label_row + 1).take(3) {
        if let Some(open) = next.find('[') {
            if let Some(close_rel) = next[open + 1..].find(']') {
                return next[open + 1..open + 1 + close_rel].trim().to_string();
            }
        }
    }
    panic!(
        "Could not find [...] field after Project Path label.\nScreen:\n{}",
        screen
    );
}

/// True when the rendered screen contains a dim `┄┄┄...┄┄┄`
/// separator row — the host-rendered popup's replacement for
/// the input field's normal `╰─...─╯` bottom border. Its
/// presence is the load-bearing visual cue that input + popup
/// are part of one unified box: above the separator is the
/// active input, below it (and inside the labeled section's
/// side borders) are the candidate rows.
fn screen_has_completion_dim_separator(screen: &str) -> bool {
    screen.lines().any(|l| {
        if let Some(start) = l.find('┄') {
            let rest = &l[start..];
            let run: String = rest.chars().take_while(|c| *c == '┄').collect();
            return run.chars().count() >= 8;
        }
        false
    })
}

/// Type `<workspace>/al` into the focused Project Path field and
/// wait for the completion dropdown to surface both `alpha_dir/`
/// and `alpha_two/` candidates. Returns the typed prefix so the
/// caller can compare against the field value.
fn type_alpha_prefix_and_wait(
    harness: &mut EditorTestHarness,
    workspace: &std::path::Path,
) -> String {
    let prefix = format!("{}/al", workspace.display());
    harness.type_text(&prefix).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("alpha_dir/") && s.contains("alpha_two/")
        })
        .unwrap();
    prefix
}

/// The host-rendered popup integrates with the wrapping
/// labeled-section chrome: the input field's normal bottom
/// border becomes a dim `┄┄┄...┄┄┄` separator (cueing that the
/// box has extended downward), and the side borders continue
/// past the input through the candidate rows.
#[test]
fn completion_popup_renders_with_dim_separator() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_alpha_prefix_and_wait(&mut harness, &workspace);

    let screen = harness.screen_to_string();
    assert!(
        screen_has_completion_dim_separator(&screen),
        "completion popup must render with a dim `┄┄┄...┄┄┄` separator \
         between input and candidates. Screen:\n{}",
        screen,
    );
}

/// Tab accepts the highlighted completion: the Project Path field
/// must contain the first suggestion (`<workspace>/alpha_dir/`)
/// after Tab is pressed with the dropdown open. Pins the
/// already-working behaviour as a regression guard.
#[test]
fn tab_accepts_highlighted_completion() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    let typed = type_alpha_prefix_and_wait(&mut harness, &workspace);

    // Precondition: typed text intact before Tab.
    assert_eq!(project_path_field_value(&harness.screen_to_string()), typed,);

    // First item (`alpha_dir/`, sorted before `alpha_two/`) is
    // highlighted by default — setCompletionItems resets
    // selectedIndex to 0.
    let expected = format!("{}/alpha_dir/", workspace.display());
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| project_path_field_value(&h.screen_to_string()) == expected)
        .unwrap();
}

/// Enter must NOT accept the highlighted completion. Before the
/// fix, Enter routed through the host's picker-style smart-key
/// wiring and overwrote the field with the highlighted suggestion.
/// After the fix, the form's explicit Enter binding closes the
/// dropdown without accepting and forwards Enter through to the
/// smart-key dispatcher's focus-advance branch — leaving the typed
/// text intact.
#[test]
fn enter_keeps_typed_text_when_completion_open() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    let typed = type_alpha_prefix_and_wait(&mut harness, &workspace);

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Dropdown should close — neither `alpha_dir/` nor `alpha_two/`
    // should remain on screen as suggestions. Wait for that so
    // we're reading a steady state, not the in-flight render.
    harness
        .wait_until(|h| !h.screen_to_string().contains("alpha_two/"))
        .unwrap();

    assert_eq!(
        project_path_field_value(&harness.screen_to_string()),
        typed,
        "Enter must leave the typed text intact (not accept the highlighted suggestion). \
         Screen:\n{}",
        harness.screen_to_string(),
    );
}

/// Type the `<workspace>/alpha_` prefix into Project Path and
/// wait until the popup has surfaced at least the first
/// candidate (`alpha_00/`). With 10 candidates the popup spans
/// `total - visible = 5` extra rows that the user must scroll
/// to reach.
fn type_many_alphas_prefix_and_wait(harness: &mut EditorTestHarness, workspace: &std::path::Path) {
    let prefix = format!("{}/alpha_", workspace.display());
    harness.type_text(&prefix).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("alpha_00/"))
        .unwrap();
}

/// With more candidates than the default visible-rows cap, the
/// popup paints exactly `5` rows + scrollbar — never the whole
/// list. The first batch of `alpha_NN/` directories sits in the
/// window; the tail ones (`alpha_07/` … `alpha_09/`) are
/// off-screen until the user scrolls. This pins the host's
/// fixed visible-rows behaviour against accidental "render all
/// candidates" regressions.
#[test]
fn completion_popup_caps_at_visible_rows() {
    let (_temp, workspace, names) = set_up_workspace_many_alphas();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_many_alphas_prefix_and_wait(&mut harness, &workspace);

    let screen = harness.screen_to_string();
    let visible: Vec<&String> = names
        .iter()
        .filter(|n| screen.contains(&format!("{}/", n)))
        .collect();
    assert_eq!(
        visible.len(),
        5,
        "default `completions_visible_rows = 5` should cap the painted candidates to 5; \
         saw {} on screen ({:?}).\nScreen:\n{}",
        visible.len(),
        visible,
        screen,
    );
    // The first five (`alpha_00` … `alpha_04`) should be the
    // ones in view since the host starts the scroll at 0.
    for n in names.iter().take(5) {
        assert!(
            screen.contains(&format!("{}/", n)),
            "candidate `{}/` should be in the initial window. Screen:\n{}",
            n,
            screen,
        );
    }
}

/// Pressing Down past the bottom of the visible window scrolls
/// the candidate list — earlier candidates fall out the top,
/// later ones (`alpha_07/`, `alpha_08/`, `alpha_09/`) come into
/// view. Verifies the host's auto-scroll-to-keep-selection-in-
/// view path.
#[test]
fn completion_popup_scrolls_with_down_arrow() {
    let (_temp, workspace, _names) = set_up_workspace_many_alphas();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_many_alphas_prefix_and_wait(&mut harness, &workspace);

    // Sanity: tail candidate is off-screen before any Down.
    assert!(
        !harness.screen_to_string().contains("alpha_09/"),
        "precondition: `alpha_09/` must be off-screen before scrolling",
    );

    // Press Down enough times to walk selection to the last
    // candidate. Auto-scroll should snap the window so
    // `alpha_09/` is visible at the bottom.
    for _ in 0..9 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .wait_until(|h| h.screen_to_string().contains("alpha_09/"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("alpha_00/"),
        "after scrolling to the bottom, the first candidate `alpha_00/` should fall \
         off the top of the window. Screen:\n{}",
        screen,
    );
}

/// When the candidate count exceeds the visible-rows cap, the
/// popup paints a scrollbar in the right edge — at minimum, a
/// solid block (`█`) glyph appears somewhere inside the popup
/// area. Pinning this prevents a future "the host stopped
/// drawing the scrollbar" regression that would silently make
/// the popup feel un-scrollable.
#[test]
fn completion_popup_renders_scrollbar_when_overflowing() {
    let (_temp, workspace, _names) = set_up_workspace_many_alphas();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_many_alphas_prefix_and_wait(&mut harness, &workspace);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains('█'),
        "scrollbar thumb glyph `█` should paint when the popup has more \
         candidates than fit visible rows. Screen:\n{}",
        screen,
    );
}

/// The selected candidate's row paints with `popup_selection_bg`
/// across the candidate text + trailing pad + scrollbar column,
/// but the popup's `│` side borders must stay outside the
/// highlight — the right `│` in particular must keep the
/// popup's base bg (`theme.suggestion_bg`), not the selection
/// blue. Regression guard for a bug where the row-level
/// selection style propagated onto the wrapping `│ ... │` entry
/// and the per-border fg-only inline overlay could not paint
/// the bg back, so the right border sat on selection blue.
#[test]
fn selection_highlight_does_not_overlap_right_border() {
    let (_temp, workspace, _names) = set_up_workspace_many_alphas();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_many_alphas_prefix_and_wait(&mut harness, &workspace);

    // `alpha_00/` is the first candidate, selected by default
    // (the host resets `selectedIndex` to 0 whenever the
    // candidate list updates).
    let (_text_col, row) = harness
        .find_text_on_screen("alpha_00/")
        .expect("`alpha_00/` should be visible as the first candidate row");

    // Scan the selected row right-to-left for the popup's
    // right `│` border. The dialog that wraps the form draws
    // its own `│` one column further out, so the rightmost
    // `│` is the dialog's border — the popup's right border
    // is the next `│` inward, identified as the rightmost `│`
    // whose left neighbor is NOT also `│` (the dialog border
    // would have the popup's `│` immediately to its left).
    let width = harness.buffer().area.width;
    let mut right_border_col: Option<u16> = None;
    for x in (1..width).rev() {
        if harness.get_cell(x, row).as_deref() == Some("│")
            && harness.get_cell(x - 1, row).as_deref() != Some("│")
        {
            right_border_col = Some(x);
            break;
        }
    }
    let right_border_col = right_border_col.unwrap_or_else(|| {
        panic!(
            "popup right `│` border should be visible on the selected candidate row.\nScreen:\n{}",
            harness.screen_to_string(),
        )
    });

    let (popup_selection_bg, suggestion_bg) = {
        let theme = harness.editor().theme();
        (theme.popup_selection_bg, theme.suggestion_bg)
    };
    let border_style = harness
        .get_cell_style(right_border_col, row)
        .expect("right border cell should have a style");

    assert_ne!(
        border_style.bg,
        Some(popup_selection_bg),
        "right `│` border on the selected candidate row must NOT paint with \
         `popup_selection_bg` ({:?}); the selection highlight should stop \
         inside the border, not overlap it. Border cell bg: {:?}.\nScreen:\n{}",
        popup_selection_bg,
        border_style.bg,
        harness.screen_to_string(),
    );
    assert_eq!(
        border_style.bg,
        Some(suggestion_bg),
        "right `│` border on the selected candidate row should paint on the \
         popup's base background (`suggestion_bg` = {:?}), not {:?}.\nScreen:\n{}",
        suggestion_bg,
        border_style.bg,
        harness.screen_to_string(),
    );

    // Also confirm the cell *inside* the popup's right border
    // (which is either the `█` scrollbar thumb or a pad
    // space, depending on whether the popup overflows) DOES
    // carry the selection bg. Pins the "highlight reads as a
    // single solid block, not truncated at end of text"
    // requirement: the selection should extend all the way to
    // the inside of the border, including the scrollbar
    // column on selected rows.
    let inside_col = right_border_col.saturating_sub(1);
    let inside_style = harness
        .get_cell_style(inside_col, row)
        .expect("inside cell should have a style");
    assert_eq!(
        inside_style.bg,
        Some(popup_selection_bg),
        "selection highlight should extend across the row's interior up to \
         the inside of the right border (including the scrollbar column \
         when present). Cell at col {} ({:?}) bg: {:?}.\nScreen:\n{}",
        inside_col,
        harness.get_cell(inside_col, row),
        inside_style.bg,
        harness.screen_to_string(),
    );
}

/// Completion candidates render left-aligned with the input
/// field's value. The input row's leading chrome is `│ [` —
/// dialog border + section padding + value's `[` bracket —
/// putting the value's first char three columns inside the
/// dialog border. The popup row's leading chrome should match,
/// so the candidate's first char sits directly under the
/// value's first char.
#[test]
fn completion_candidates_left_aligned_with_input_value() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    let prefix = type_alpha_prefix_and_wait(&mut harness, &workspace);
    let _ = prefix;

    // The typed value starts with the workspace path; its first
    // char on the input row is the leading `/` of `/tmp/...`.
    // The popup's first candidate row is `alpha_dir/` —
    // however, both rows contain the workspace path as a prefix
    // (we typed `<workspace>/al` and the popup echoes
    // `<workspace>/alpha_dir/`), so the leading `/` is the
    // common anchor we can locate on both rows.
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let input_row = lines
        .iter()
        .position(|l| l.contains('[') && l.contains(']') && l.contains("/al"))
        .expect("input row with [<typed path>] should be on screen");
    let popup_row = lines
        .iter()
        .position(|l| l.contains("alpha_dir/"))
        .expect("popup row with `alpha_dir/` should be on screen");

    let input_col = lines[input_row]
        .find('[')
        .map(|i| i + '['.len_utf8())
        .expect("input row should contain `[`");
    // First `/` on the popup row IS the candidate's first char
    // (the candidate starts with the workspace path, which
    // begins with `/`). Use that as the anchor.
    let popup_col = lines[popup_row]
        .find('/')
        .expect("popup row should contain the candidate's leading `/`");

    assert_eq!(
        input_col, popup_col,
        "popup candidate's first column ({}) should match the input value's first column ({}); \
         the candidate text must sit directly under the typed value, not one column to the \
         left of it.\nInput row {}:\n{}\nPopup row {}:\n{}",
        popup_col, input_col, input_row, lines[input_row], popup_row, lines[popup_row],
    );
}

/// After the popup dismisses on Enter and a second Enter advances
/// focus to the next tabbable widget, the plugin's local focus
/// mirror must advance in lockstep with the host. Without this,
/// the plugin still thinks the text input is focused and the
/// next Up/Down walks the *previous* text input's history,
/// silently rewriting its value. This was the user-reported
/// breakage: "after using the popup and pressing enter to visit
/// the checkbox/next field, up/down still modify the input box".
///
/// We verify by typing a marker character after the focus
/// advance and asserting the original input is unchanged. If
/// focus is correctly on the next tabbable, the marker goes
/// elsewhere; if focus is stuck on the original text input, the
/// marker extends its value.
#[test]
fn enter_advance_after_popup_dismiss_moves_plugin_focus_mirror() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    let typed = type_alpha_prefix_and_wait(&mut harness, &workspace);

    // Popup is open. Enter #1 dismisses it (stays on the
    // Project Path text input).
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("alpha_two/"))
        .unwrap();
    assert_eq!(
        project_path_field_value(&harness.screen_to_string()),
        typed,
        "Enter #1 should dismiss the popup without changing the typed text",
    );

    // Enter #2 advances focus to the next tabbable widget.
    // (The worktree checkbox is disabled because the typed
    // `<workspace>/al` path doesn't exist; the focus cycle
    // skips it.)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.tick_and_render().unwrap();

    // Now type a marker character. If the plugin's focus
    // mirror correctly advanced past Project Path, this char
    // lands somewhere else (Session Name's input replaces the
    // placeholder, etc.) and the Project Path value stays at
    // `typed`. If the plugin's mirror is stuck on
    // project_path, the marker extends it.
    harness.type_text("Z").unwrap();
    harness.tick_and_render().unwrap();

    assert_eq!(
        project_path_field_value(&harness.screen_to_string()),
        typed,
        "after Enter-Enter, focus must have advanced past the Project Path \
         text input; a subsequent keystroke must not extend the Project Path \
         value. Screen:\n{}",
        harness.screen_to_string(),
    );
}

/// Mouse wheel over the popup scrolls its candidate list — same
/// behaviour the user gets from Down arrow, except the selected
/// index stays put (it's a scroll, not a selection move). Goes
/// directly through `Editor::handle_mouse` since SGR mouse
/// escape sequences sent via `tmux send-keys` get filtered by
/// tmux's pane-input pipeline and never reach crossterm, so
/// interactive tmux verification isn't possible without a real
/// mouse device.
#[test]
fn completion_popup_scrolls_with_mouse_wheel() {
    let (_temp, workspace, _names) = set_up_workspace_many_alphas();
    let mut harness = EditorTestHarness::with_working_dir(160, 50, workspace.clone()).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_new_session_command(&mut harness);

    open_new_session_form(&mut harness);
    type_many_alphas_prefix_and_wait(&mut harness, &workspace);

    // Sanity: bottom candidate is off-screen before scrolling.
    assert!(
        !harness.screen_to_string().contains("alpha_09/"),
        "precondition: `alpha_09/` must be off-screen before scrolling",
    );

    // Locate a row owned by the popup so the wheel lands on its
    // hit-test target. `alpha_00/` is the top candidate row when
    // the popup just opened; find its on-screen row and scroll
    // there. Column is irrelevant for the host's wheel routing
    // (it only checks `last_inner_rect` containment), but pick
    // a column inside the panel for realism.
    let (col, row) = harness
        .find_text_on_screen("alpha_00/")
        .expect("`alpha_00/` should be visible before scrolling");
    let _ = col;

    // Each ScrollDown event ticks the popup's host-side scroll
    // by 3 (the editor's default wheel step). 5 events is enough
    // to reach the end of the 10-row list regardless of which
    // direction the step is clamped from.
    for _ in 0..5 {
        harness.mouse_scroll_down(80, row).unwrap();
    }
    harness
        .wait_until(|h| h.screen_to_string().contains("alpha_09/"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("alpha_00/"),
        "after scrolling down with the mouse wheel, `alpha_00/` should fall off \
         the top of the visible window. Screen:\n{}",
        screen,
    );
}
