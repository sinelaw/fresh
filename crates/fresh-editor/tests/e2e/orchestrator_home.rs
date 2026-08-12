//! E2E coverage for Orchestrator **Home** — the full-screen panel that
//! answers "which of my agents needs me", with a chat carrying what they have
//! said and a live pane showing the selected workspace's terminal.
//!
//! Per CONTRIBUTING.md §2 these drive only the keyboard and assert on
//! rendered output. The pane's title line (`live · <name>`) is the visible
//! statement of which workspace Home is pointed at, so it is what the
//! selection assertions read.

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

/// Open Home through the command palette and wait for it to render.
fn open_home(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Home").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Home"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The hint bar is Home's own chrome and is present exactly when the panel
    // has painted. The header is not usable here: it reads "1 agent" or
    // "N agents" depending on how many there are.
    h.wait_until(|h| h.screen_to_string().contains("Enter send / go"))
        .unwrap();
    // Home opens with the keyboard in the chat line. Tab moves it to the list,
    // which is what every selection assertion below drives.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("\u{25b8} "))
        .unwrap();
}

/// The workspace named on the live pane's title line, i.e. the one Home says
/// it is showing. `None` before the pane has painted.
fn shown_workspace(h: &EditorTestHarness) -> Option<String> {
    h.screen_to_string()
        .lines()
        .find_map(|l| l.split("live · ").nth(1))
        .map(|rest| {
            rest.trim_start()
                .split([' ', '·', '─', '│'])
                .find(|s| !s.is_empty())
                .unwrap_or("")
                .to_string()
        })
        .filter(|s| !s.is_empty())
}

/// The live pane's rows, title line included.
///
/// The title reads `live · <name>` normally and `typing → <name>` while the
/// pane holds the keyboard, so both are accepted — a helper that knew only the
/// first silently returned nothing in typing mode.
fn pane_text(h: &EditorTestHarness) -> String {
    let screen = h.screen_to_string();
    let start = match screen
        .lines()
        .position(|l| l.contains("live · ") || l.contains("typing → "))
    {
        Some(i) => i,
        None => return String::new(),
    };
    screen
        .lines()
        .skip(start)
        .take(8)
        .collect::<Vec<_>>()
        .join("\n")
}

/// The column the agent list starts at.
///
/// Home is two columns: a chat on the left and the list over the live pane on
/// the right. Both halves are on the same screen rows, so a row parser that
/// reads a whole line picks up whatever the chat happens to be saying. The
/// list section's left border is at a fixed column, and the header legend is
/// drawn inside it, so the `\u{256d}` that precedes the header locates it.
fn list_column(h: &EditorTestHarness) -> usize {
    let screen = h.screen_to_string();
    let header = screen
        .lines()
        .find(|l| l.contains("needs you") || l.contains("none waiting"))
        .unwrap_or_default()
        .to_string();
    let legend = header
        .find(" agents \u{b7}")
        .or_else(|| header.find(" agent \u{b7}"))
        .unwrap_or(0);
    // Char index of the section corner that opens the list, counting in chars
    // because the borders are multi-byte.
    header[..legend]
        .rfind('\u{256d}')
        .map(|b| header[..b].chars().count())
        .unwrap_or(0)
}

/// Workspace names as Home lists them, top to bottom. Read off the panel so
/// the test sees the same order the user does.
fn row_labels(h: &EditorTestHarness) -> Vec<String> {
    let screen = h.screen_to_string();
    let col = list_column(h);
    screen
        .lines()
        .skip_while(|l| !l.contains("needs you") && !l.contains("none waiting"))
        .skip(1)
        .take_while(|l| !l.contains("live \u{b7} "))
        .map(|l| l.chars().skip(col).collect::<String>())
        .filter_map(|l| {
            let l = l.as_str();
            // `│ ! name   agent   branch   detail │` — the name is the token
            // after the state glyph.
            let body = l.trim_start_matches(|c: char| !c.is_alphanumeric() && c != '!');
            let mut it = body.split_whitespace();
            let first = it.next()?;
            let name = if first == "!" || first == "*" || first == "·" {
                it.next()?
            } else {
                first
            };
            (!name.is_empty()).then(|| name.to_string())
        })
        .collect()
}

/// The window id behind a Home row label.
fn window_id_for(h: &EditorTestHarness, label: &str) -> fresh_core::WindowId {
    h.editor()
        .describe_environment()
        .windows
        .into_iter()
        .find(|w| w.label == label)
        .unwrap_or_else(|| panic!("no window labelled {label}"))
        .window_id
}

/// Home's selection follows the *workspace*, not the row it happened to
/// be on.
///
/// Rows are sorted by urgency — waiting first, then working, then quiet — so
/// the order changes underneath the user as agents change state. With the
/// selection held as a row index, an agent going from quiet to working
/// reorders the list and silently re-points the selection at whatever slid
/// into that slot: observed by hand as selecting one workspace, typing, and
/// having the keystrokes land in a different workspace's buffer two rows
/// away.
///
/// Drives the reorder the way it happens in life — by making a workspace
/// produce output — and asserts the pane still names the workspace the user
/// chose.
#[test]
fn home_selection_survives_a_reorder() {
    let (_tmp, root) = setup_project("alphaproj");
    let parent = root.parent().unwrap().to_path_buf();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    // Three more workspaces, so there are rows to reorder.
    for name in ["projB", "projC", "projD"] {
        let dir = parent.join(name);
        fs::create_dir(&dir).unwrap();
        h.editor_mut().create_window_at(dir, name.to_string());
    }
    h.render().unwrap();
    open_home(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("projD"))
        .unwrap();

    // Step down twice so the selection sits below the top of the list, and
    // remember which workspace the pane names.
    for _ in 0..2 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| shown_workspace(h).is_some()).unwrap();
    }
    let chosen = shown_workspace(&h).expect("the live pane names a workspace");

    // Shorten the list above the selection, which shifts every row below it up
    // by one. Deliberately not driven by agent activity: Home only sees
    // activity for terminals the Orchestrator itself created, so a
    // host-created terminal never changes a row's rank and the reorder would
    // never happen.
    let rows = row_labels(&h);
    let idx = rows
        .iter()
        .position(|l| l == &chosen)
        .unwrap_or_else(|| panic!("the selected workspace is listed. rows={rows:?}"));
    assert!(
        idx > 0,
        "precondition: rows above the selection. rows={rows:?}"
    );
    // Anything but the active window, which cannot be closed out from under
    // itself.
    let active = h.editor().active_window().label.clone();
    let victim = rows[..idx]
        .iter()
        .find(|l| *l != &active)
        .unwrap_or_else(|| panic!("a closable row above the selection. rows={rows:?}"))
        .clone();
    let victim_id = window_id_for(&h, &victim);
    h.editor_mut().close_window(victim_id);
    h.render().unwrap();
    h.wait_until(|h| !row_labels(h).contains(&victim)).unwrap();

    // The pane must still name the workspace the user chose. Holding the
    // selection as a row index reported whichever workspace slid into that
    // slot instead.
    h.wait_until_stable(|h| shown_workspace(h).is_some())
        .unwrap();
    assert_eq!(
        shown_workspace(&h).as_deref(),
        Some(chosen.as_str()),
        "Home must keep showing the workspace the user selected after the \
         row above it went away. Screen:\n{}",
        h.screen_to_string()
    );
}

/// Home's live pane shows one *terminal*, not the whole session.
///
/// It renders through the `pane` widget rather than the whole-window embed,
/// so a workspace's tab bar and editor chrome must not appear inside it — the
/// pane is there to show what an agent is saying, and session furniture is
/// noise that also makes the pane ambiguous when a workspace holds two
/// terminals.
#[test]
fn home_pane_shows_the_terminal_without_session_chrome() {
    let (_tmp, root) = setup_project("betaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    // A terminal in this workspace, with a recognisable prompt on screen.
    h.editor_mut().open_terminal();
    h.render().unwrap();
    let buffer_id = h.editor().active_buffer_id();
    let terminal_id = h
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .expect("the active buffer should be the terminal just opened");
    let home = h.editor().active_window_id();
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| !s.text.is_empty())
    })
    .unwrap();
    h.editor_mut()
        .send_terminal_input_to(home, terminal_id, "echo FLEETPANE\n");

    open_home(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("FLEETPANE"))
        .unwrap();

    // The pane must settle into showing the terminal *alone*: its output
    // present, and none of the tab-bar chrome the whole-session embed paints.
    //
    // A wait rather than a bare assertion because the pane's buffer is
    // resolved by Home's probe a tick after it opens; without the fix it
    // never resolves and Home keeps embedding the whole session, so this
    // times out — which CONTRIBUTING.md §1 accepts as the failing half of a
    // regression test.
    h.wait_until(|h| {
        let pane = pane_text(h);
        pane.contains("FLEETPANE") && !pane.contains("[No Name]") && !pane.contains("*Terminal")
    })
    .unwrap();

    let pane = pane_text(&h);
    assert!(
        pane.contains("FLEETPANE"),
        "the pane shows the terminal's own output. Pane:\n{pane}"
    );
    assert!(
        !pane.contains("*Terminal") && !pane.contains("[No Name]"),
        "the pane renders one terminal, so the session's tab bar must not \
         appear inside it. Pane:\n{pane}"
    );
}

/// Typing in Home reaches the selected agent's terminal, Alt+` is the way
/// in and back out, and Tab reaches the agent rather than toggling.
///
/// This is the point of the pane being interactive rather than a picture: a
/// focused interactive pane takes every key the panel's own mode does not
/// claim and routes it to the PTY, so an agent's question can be answered
/// without leaving the view that reported it. Covers `focused_interactive_pane`
/// / `send_key_to_pane` and the `interactive` flag.
///
/// Tab is asserted explicitly because it is the reason the toggle is Alt+`:
/// Tab is a key agents genuinely want (completion, field navigation in their
/// own TUI), so a toggle that ate it would make the one view built for
/// answering an agent unable to send it.
#[test]
fn home_typing_reaches_the_selected_terminal() {
    let (_tmp, root) = setup_project("gammaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();

    h.editor_mut().open_terminal();
    h.render().unwrap();
    let buffer_id = h.editor().active_buffer_id();
    let terminal_id = h
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .expect("the active buffer should be the terminal just opened");
    let home = h.editor().active_window_id();
    // Wait for the shell rather than a fixed pause: it is what will echo.
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| !s.text.is_empty())
    })
    .unwrap();

    open_home(&mut h);
    // The pane must be a real pane before typing means anything.
    h.wait_until(|h| {
        let pane = pane_text(h);
        !pane.is_empty() && !pane.contains("[No Name]") && !pane.contains("*Terminal")
    })
    .unwrap();

    // Alt+` hands the keyboard to the pane, which the chrome states.
    h.send_key(KeyCode::Char('`'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("typing →"))
        .unwrap();

    // Type a command and run it. Both the characters and Enter have to reach
    // the PTY — a mode that only forwarded Enter would leave an empty line.
    h.type_text("echo FLEETTYPING").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| pane_text(h).contains("FLEETTYPING"))
        .unwrap();

    // Tab reaches the agent instead of toggling: the shell echoes a completion
    // or a literal tab, but either way Home stays in typing mode. Without
    // this the toggle would be eating a key the agent needs.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    assert!(
        h.screen_to_string().contains("typing →"),
        "Tab must reach the agent, not toggle Home out of typing mode:\n{}",
        h.screen_to_string()
    );

    // The same key that focused the pane un-focuses it.
    h.send_key(KeyCode::Char('`'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("type at agent"))
        .unwrap();
}

/// An agent parked on a question shows `!`, and the reason it shows is the
/// question rather than one of its options.
///
/// This is the state Home exists for: "working" and "idle" are both
/// derived from whether a PTY printed recently, and a blocked agent prints
/// nothing, so without this it is indistinguishable from one that finished.
/// Recognition is per agent kind and reads the terminal's own screen, so the
/// test runs a stand-in named `claude` — the name is what resolves the
/// registry entry whose pattern is matched.
///
/// Also covers `matchWaiting`'s preference for the line ending in `?`: the
/// stand-in prints the question *and* a numbered option, both of which match
/// the pattern, and "1. Yes" as the stated reason a workspace is blocked would
/// tell the user nothing they could act on.
#[test]
fn home_shows_why_an_agent_is_blocked() {
    let (_tmp, root) = setup_project("deltaproj");

    // A stand-in agent: asks the way Claude Code asks, then goes quiet. Quiet
    // is the whole difficulty — timing alone cannot tell it from finished.
    let agent = root.join("claude");
    fs::write(
        &agent,
        // Sets its terminal title with OSC 0, the way a real agent's TUI
        // does. The title is what resolves the registry entry whose prompt
        // pattern is matched, so a stand-in that never set one would be
        // testing nothing.
        "#!/usr/bin/env bash\n\
         printf '\\033]0;claude\\007'\n\
         echo 'Do you want to make this edit to tests/e2e.rs?'\n\
         echo '> 1. Yes'\n\
         sleep 3600\n",
    )
    .unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&agent, fs::Permissions::from_mode(0o755)).unwrap();
    }

    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    h.editor_mut().open_terminal();
    h.render().unwrap();

    let buffer_id = h.editor().active_buffer_id();
    let terminal_id = h
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .expect("the active buffer should be the terminal just opened");
    let home = h.editor().active_window_id();
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| !s.text.is_empty())
    })
    .unwrap();
    // Run it in the foreground so the terminal's title — which is what
    // resolves the agent registry entry — becomes `claude`.
    h.editor_mut()
        .send_terminal_input_to(home, terminal_id, "./claude\n");
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| s.text.iter().any(|l| l.contains("Do you want")))
    })
    .unwrap();

    open_home(&mut h);

    // The row must state that this workspace wants something, and say what.
    h.wait_until(|h| {
        h.screen_to_string()
            .lines()
            .any(|l| l.contains("needs you") || l.contains('!'))
    })
    .unwrap();
    h.wait_until(|h| {
        h.screen_to_string()
            .lines()
            .any(|l| l.contains("deltaproj") && l.contains("Do you want"))
    })
    .unwrap();

    // The reason is the question, not the option beneath it.
    let row = h
        .screen_to_string()
        .lines()
        .find(|l| l.contains("deltaproj") && l.contains("Do you want"))
        .expect("Home row for the blocked workspace")
        .to_string();
    assert!(
        !row.contains("1. Yes"),
        "the reason a workspace is blocked should be the question, not one of \
         its options. Row:\n{row}"
    );
}

/// An agent's own `status` line outranks anything inferred from its screen.
///
/// The outbox is the mailbox's read half (docs/internal/agent-control-plane.md
/// §8.1). Screen matching is a guess about someone else's TUI: it cannot tell
/// "the word Approve appeared in a diff the agent is showing me" from "I am
/// asking you to approve". This test puts the two in direct conflict — a
/// terminal printing a question that the `claude` registry pattern matches,
/// and a status file saying the agent is merely working — and asserts the
/// agent's own claim wins, both for the state glyph and for the reason column.
#[test]
fn an_agents_own_status_outranks_its_screen() {
    let (_tmp, root) = setup_project("epsilonproj");

    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 36, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    h.editor_mut().open_terminal();
    h.render().unwrap();

    let buffer_id = h.editor().active_buffer_id();
    let terminal_id = h
        .editor()
        .active_window()
        .get_terminal_id(buffer_id)
        .expect("the active buffer should be the terminal just opened");
    let home = h.editor().active_window_id();
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| !s.text.is_empty())
    })
    .unwrap();

    // Say, on screen, exactly what the screen-matching path looks for.
    h.editor_mut().send_terminal_input_to(
        home,
        terminal_id,
        "printf '\\033]0;claude\\007'; echo 'Do you want to make this edit to tests/e2e.rs?'\n",
    );
    h.wait_until(|h| {
        h.editor()
            .terminal_screen(home, terminal_id, None, true)
            .is_ok_and(|s| s.text.iter().any(|l| l.contains("Do you want")))
    })
    .unwrap();

    // …and contradict it in the outbox. Written the way an agent nobody
    // launched from the editor writes it — `.fresh/agents/<name>/status` in
    // its own checkout — which is also the path the plugin hands its own
    // agents as `$FRESH_AGENT_STATUS`.
    let status_dir = root.join(".fresh").join("agents").join("byhand");
    fs::create_dir_all(&status_dir).unwrap();
    fs::write(
        status_dir.join("status"),
        "working rebuilding the index, nothing needed from you\n",
    )
    .unwrap();

    open_home(&mut h);

    // The agent's summary is what the row shows…
    h.wait_until(|h| h.screen_to_string().contains("rebuilding the index"))
        .unwrap();
    // …and the question on screen must not have raised the needs-you state,
    // because the agent said it is working.
    let screen = h.screen_to_string();
    let header = screen
        .lines()
        .find(|l| l.contains("agents ·") || l.contains("agent ·"))
        .unwrap_or_default()
        .to_string();
    assert!(
        header.contains("none waiting"),
        "the agent said `working`, so nothing should be flagged as waiting:\n{screen}"
    );
}
