//! Universal Search's "All workspaces" toggle (Alt+A on the Scope row): with
//! it on, every workspace's matches are listed, one group per workspace under
//! a header row, the current workspace first; Enter on another workspace's hit
//! switches to that workspace and lands on the hit there, as it would in its
//! own. Off (the default) the search stays in the current workspace.

use crate::common::global_state::isolated_dir_context;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

const MARKER: &str = "MANGO_7731";

/// No workspace-switch wipe, so a frame after Enter is the frame that stays.
fn config() -> Config {
    let mut config = Config::default();
    config.editor.animations = false;
    config
}

/// `<tmp>/web` and `<tmp>/api`, each holding one saved file that mentions
/// the marker; Live Grep's plugin lives in `web`, where the editor starts.
fn two_projects(temp: &Path) -> (PathBuf, PathBuf) {
    let root = temp.canonicalize().unwrap();
    let web = root.join("web");
    let api = root.join("api");
    fs::create_dir_all(web.join("plugins")).unwrap();
    fs::create_dir_all(&api).unwrap();
    copy_plugin_lib(&web.join("plugins"));
    copy_plugin(&web.join("plugins"), "live_grep");
    fs::write(web.join("web_note.txt"), format!("{MARKER} in web\n")).unwrap();
    fs::write(api.join("api_note.txt"), format!("{MARKER} in api\n")).unwrap();
    fs::write(api.join("api_draft.txt"), "draft\n").unwrap();
    (web, api)
}

/// The editor in `web`, with `api` open as a second workspace whose
/// `api_draft.txt` has an unsaved edit mentioning the marker; `web` active.
fn harness_with_two_workspaces(web: &Path, api: &Path) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 36, config(), web.to_path_buf())
            .unwrap();
    let web_id = harness.editor().active_window_id();
    let api_id = harness
        .editor_mut()
        .create_window_at(api.to_path_buf(), "api".to_string());
    harness.editor_mut().set_active_window(api_id);
    harness.open_file(&api.join("api_draft.txt")).unwrap();
    harness.type_text(&format!("{MARKER} unsaved ")).unwrap();
    harness.editor_mut().set_active_window(web_id);
    harness.render().unwrap();
    harness
}

fn open_universal_search(harness: &mut EditorTestHarness, query: &str) {
    harness
        .run_palette_command("Live Grep (Find in Files)")
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Scope "))
        .unwrap();
    harness.type_text(query).unwrap();
}

/// The screen row `text` first appears on.
fn row_of(harness: &EditorTestHarness, text: &str) -> u16 {
    harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} not on screen:\n{}", harness.screen_to_string()))
        .1
}

/// Off by default: the other workspace's matches are not listed. Alt+A puts
/// them in, under one header per workspace, the current workspace's group
/// first, each row as it reads without the toggle.
#[test]
fn all_workspaces_lists_every_workspaces_matches_grouped_by_workspace() {
    let temp = tempfile::TempDir::new().unwrap();
    let (web, api) = two_projects(temp.path());
    let mut harness = harness_with_two_workspaces(&web, &api);

    open_universal_search(&mut harness, MARKER);
    harness
        .wait_until(|h| h.screen_to_string().contains("web_note.txt:1"))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("All workspaces"),
        "the toggle is on the Scope row:\n{screen}"
    );
    assert!(
        !screen.contains("api_note.txt") && !screen.contains("api_draft.txt"),
        "with the toggle off only this workspace is searched:\n{screen}"
    );

    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("── api · 2 matches") && s.contains("[buf] api_draft.txt:1")
        })
        .unwrap();
    let screen = harness.screen_to_string();
    // The file on disk and the unsaved buffer, read as in their own workspace
    assert!(screen.contains("api_note.txt:1"), "{screen}");
    // This workspace's group leads
    let web_row = row_of(&harness, "web_note.txt:1");
    let api_header = row_of(&harness, "── api · 2 matches");
    assert!(
        web_row < api_header,
        "the current workspace's matches come first:\n{screen}"
    );
    assert!(
        harness.screen_row_text(web_row - 1).contains("· 1 match"),
        "this workspace's group has its own header:\n{screen}"
    );
}

/// Enter on another workspace's unsaved buffer switches to that workspace
/// and shows the buffer, unsaved edit and all.
#[test]
fn enter_on_another_workspaces_unsaved_buffer_goes_there() {
    let temp = tempfile::TempDir::new().unwrap();
    let (web, api) = two_projects(temp.path());
    let mut harness = harness_with_two_workspaces(&web, &api);

    open_universal_search(&mut harness, MARKER);
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("[buf] api_draft.txt:1"))
        .unwrap();
    // The list opens on the first match (web's), not on its header:
    // web_note, api header, api_note, api_draft
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("api_draft.txt*") && s.contains(&format!("{MARKER} unsaved draft"))
        })
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("web_note.txt"),
        "web's tabs are no longer on screen:\n{screen}"
    );
}

/// Enter on another workspace's file on disk switches to that workspace and
/// opens the file there.
#[test]
fn enter_on_another_workspaces_file_opens_it_in_that_workspace() {
    let temp = tempfile::TempDir::new().unwrap();
    let (web, api) = two_projects(temp.path());
    let mut harness = harness_with_two_workspaces(&web, &api);

    open_universal_search(&mut harness, MARKER);
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("api_note.txt:1"))
        .unwrap();
    for _ in 0..2 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(&format!("{MARKER} in api")) && s.contains("api_draft.txt*")
        })
        .unwrap();
    harness.assert_screen_contains("api_note.txt");
}

/// A line on another workspace's terminal screen is listed under that
/// workspace, as `[term] <terminal>:<line>`; Enter switches there and shows
/// the terminal.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // `printf` in a Unix shell
fn enter_on_another_workspaces_terminal_line_shows_that_terminal() {
    use portable_pty::{native_pty_system, PtySize};
    let pty = native_pty_system().openpty(PtySize {
        rows: 1,
        cols: 1,
        pixel_width: 0,
        pixel_height: 0,
    });
    if pty.is_err() {
        eprintln!("Skipping test: PTY not available in this environment");
        return;
    }

    let temp = tempfile::TempDir::new().unwrap();
    let (web, api) = two_projects(temp.path());
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 36, config(), web.clone()).unwrap();
    let web_id = harness.editor().active_window_id();
    let api_id = harness
        .editor_mut()
        .create_window_at(api.clone(), "api".to_string());
    harness.editor_mut().set_active_window(api_id);
    harness.editor_mut().open_terminal();
    // `\x5a` is `Z`: the marker is only ever on screen as output, never as
    // the typed command
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"printf '\\x5aEBRA_XWS\\n'\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("ZEBRA_XWS"))
        .unwrap();
    harness.editor_mut().set_active_window(web_id);
    harness
        .wait_until(|h| !h.screen_to_string().contains("ZEBRA_XWS"))
        .unwrap();

    open_universal_search(&mut harness, "ZEBRA_XWS");
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("── api · ") && s.contains("[term] ")
        })
        .unwrap();
    // The list opens on the group's first match
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("Scope ") && s.contains("ZEBRA_XWS")
        })
        .unwrap();
}

/// A workspace listed but not opened yet this session is searched too — its
/// files on disk — and Enter on one of them opens that workspace at it.
#[test]
#[cfg(target_os = "linux")] // the pinned data dir; see `isolated_dir_context`
fn a_workspace_not_opened_yet_is_searched_and_opened_on_enter() {
    let temp = tempfile::TempDir::new().unwrap();
    let (web, api) = two_projects(temp.path());
    // Both sessions save to and discover from one store
    let (dir_context, _pin) = isolated_dir_context(&temp.path().join("state"));
    let harness_in = |dir: &Path| {
        EditorTestHarness::create(
            140,
            36,
            HarnessOptions::new()
                .with_config(config())
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // A session in api leaves its workspace saved
    {
        let mut harness = harness_in(&api);
        harness.startup(true, &[]).unwrap();
        harness.open_file(&api.join("api_draft.txt")).unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // A session in web, where api is only a saved workspace
    let mut harness = harness_in(&web);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    open_universal_search(&mut harness, MARKER);
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("── api · 1 match") && s.contains("api_note.txt:1")
        })
        .unwrap();

    // web_note, api header, api_note
    for _ in 0..2 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("Scope ") && s.contains(&format!("{MARKER} in api"))
        })
        .unwrap();
    // It opened in api's workspace, next to api's own tab
    harness.assert_screen_contains("api_draft.txt");
}
