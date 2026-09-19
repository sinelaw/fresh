//! A daemon repaints for the deadlines its editor declares, not only for
//! events.
//!
//! The daemon's loop forced a frame for exactly two time-driven things —
//! an active animation and a wheel gesture still walking its lines — copied
//! by hand from what the TUI loop waits on. Everything else that owes a frame
//! at a *time* simply did not happen there until some unrelated input caused
//! one: the LSP spinner, the async-paste fallback, and the occurrence
//! highlight, whose debounce is applied inside a render.
//!
//! The highlight is what made it visible. Move the cursor onto a word and sit
//! still: no frames, so the debounced update stayed armed and the word under
//! the cursor kept the *previous* word's highlight. The next keystroke was
//! the first thing to force a render, so the highlight jumped as you typed —
//! reported as markdown compose "briefly rendering the line differently and
//! then going back", though it has nothing to do with compose and happens in
//! any buffer.
//!
//! Asserted where it goes wrong: a real daemon, a real client on a pty, and a
//! frame that has to arrive with *nothing typed after the cursor move*. The
//! wait is bounded, because the failure mode here is silence — an unbounded
//! one would hang to the harness timeout instead of failing.
//!
//! Linux-gated like the other binary-driving tests: `XDG_*` isolation of the
//! config and socket trees works there, and `common::pty` is Linux-only.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin};
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// The word the cursor lands on. It occurs twice, so the highlight has real
/// occurrences to paint, and it is long enough to clear the highlighter's
/// minimum word length.
const WORD: &str = "Heading";

/// How long the frame may take to arrive. The debounce is 150 ms; this is
/// generous enough for a debug binary on a loaded CI runner and still far
/// short of "never", which is what the bug does.
const BUDGET: Duration = Duration::from_secs(10);

/// A `fresh` whose config, state and sockets all live under `home`, so the
/// daemon this test starts cannot meet one from another test.
fn isolated_fresh(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home.join("project"))
        .env("HOME", home)
        .env("TMPDIR", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("XDG_RUNTIME_DIR", home.join("run"))
        .env("TERM", "xterm-256color")
        .env("LANG", "C.UTF-8")
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN");
    cmd
}

fn setup(home: &Path) {
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let config_dir = home.join("config").join("fresh");
    std::fs::create_dir_all(&config_dir).unwrap();
    std::fs::write(
        config_dir.join("config.json"),
        "{\n  \"check_for_updates\": false\n}\n",
    )
    .unwrap();
    std::fs::write(
        home.join("project").join("note.md"),
        format!("# {WORD} one\n\n## {WORD} two\n"),
    )
    .unwrap();
}

/// Stop the daemon this test started, so it does not idle on in the
/// background holding a socket in the temp tree.
fn kill_daemon(home: &Path, session: &str) {
    let pid_file = home.join("run").join("fresh").join(format!("{session}.pid"));
    if let Ok(pid) = std::fs::read_to_string(pid_file) {
        if let Ok(pid) = pid.trim().parse::<i32>() {
            // SAFETY: a plain `kill(2)`; an already-dead pid just returns
            // ESRCH, which is ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
}

/// The highlight for the word under the cursor must appear on its own.
#[test]
fn a_daemon_paints_the_occurrence_highlight_without_further_input() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    setup(home);
    let session = "idle-redraw";

    // The daemon is started here rather than by the client, and without
    // plugins, because the property under test is "nothing else causes a
    // frame". A plugin that asks for a render on its own schedule hides the
    // bug: the stranded update then rides somebody else's frame, which is
    // precisely the accident the fix removes.
    let mut server = isolated_fresh(home);
    server
        .args(["--server", "--session-name", session, "--no-plugins"])
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    let mut server = server.spawn().expect("spawn the daemon");

    let mut cmd = isolated_fresh(home);
    cmd.args(["--no-plugins", "-a", session, "note.md"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh on a pty");

    client
        .wait_for_screen(|screen| screen.contains(&format!("# {WORD} one")))
        .unwrap_or_else(|e| panic!("the daemon never rendered the file: {e}"));

    // Onto the word, with the cursor starting at the head of line 1: past
    // "# " and into "Heading". Nothing is sent after this — the frame under
    // test is the one the editor owes itself.
    for _ in 0..4 {
        client.send(b"\x1b[C").unwrap();
    }

    // Watching the cells, not the text: the highlight *is* a background, and
    // no character on screen changes when it lands.
    let waited = client.wait_for_cells_within(BUDGET, |screen| highlighted(screen, WORD));
    let screen = client.screen();
    client.kill();
    kill_daemon(home, session);
    let _ = server.kill();
    let _ = server.wait();

    assert!(
        waited.is_ok(),
        "the word under the cursor never got its occurrence highlight: with the \
         editor idle the daemon rendered no frame, so the debounced update stayed \
         armed until something else forced one\nScreen:\n{screen}"
    );
}

/// Whether the run of cells spelling `word` on some row carries a background
/// of its own — i.e. the occurrence highlight is painted there.
///
/// Read off the vt100 grid the client parsed, so this is the colour a
/// terminal would actually show.
fn highlighted(screen: &vt100::Screen, word: &str) -> bool {
    let (rows, cols) = screen.size();
    let chars: Vec<char> = word.chars().collect();
    for row in 0..rows {
        for col in 0..cols.saturating_sub(chars.len() as u16) {
            let matches = chars.iter().enumerate().all(|(i, c)| {
                screen
                    .cell(row, col + i as u16)
                    .is_some_and(|cell| cell.contents() == c.to_string())
            });
            if !matches {
                continue;
            }
            // The whole run shares one background, and it differs from the
            // cell just past it — which is the page's own ground.
            let bg = screen.cell(row, col).map(|c| c.bgcolor());
            let after = screen
                .cell(row, col + chars.len() as u16)
                .map(|c| c.bgcolor());
            if bg.is_some() && bg != after {
                return true;
            }
        }
    }
    false
}
