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
