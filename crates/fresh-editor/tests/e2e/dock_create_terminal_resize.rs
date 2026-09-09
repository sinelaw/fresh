//! E2E coverage for issue #2969 item 5: a plugin panel that *creates* the
//! bottom utility dock takes rows away from the panes above it, and an
//! already-running terminal in one of those panes has to be told about it.
//!
//! Before the fix the dock-creating path (`CreateVirtualBufferInSplit` with
//! `role = "utility_dock"`, which the code-tour plugin uses) never ran the
//! layout funnel, so the PTY kept the row count it had before the dock
//! appeared. The shell went on writing below the pane's new bottom edge and
//! everything it wrote there — including its prompt — landed in grid rows
//! that are never drawn.
//!
//! Per CONTRIBUTING.md §2 the assertion is on rendered output only: the shell
//! itself decides where "the last row of the screen" is (it asks the kernel
//! via `stty size`) and writes a marker there. Whether that marker is on
//! screen is exactly the user-visible consequence of the PTY size — at the
//! correct size it is the bottom row of the pane, at the stale size it is
//! well past it and invisible.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::{Path, PathBuf};

const MAIN_RS: &str = "fn main() {\n    let l = listen();\n}\n\nfn handle() {\n    todo!()\n}\n";

/// The tour step's file. Its contents are the test's evidence that the step
/// actually opened, and its name is deliberately absent from `main.rs`.
const HANDLER_RS: &str = "fn handler() {\n    // STEP_TARGET_CONTENT\n}\n";

/// A path as a JSON string literal — quotes and escapes included. A Windows
/// path interpolated raw is not valid JSON (`C:\Users\…` carries `\U`), which
/// makes the plugin's parse throw so no panel ever mounts.
fn json_path(path: &Path) -> String {
    serde_json::to_string(&path.display().to_string()).expect("path is UTF-8")
}

fn tour_json(root: &Path) -> String {
    let handler_rs = json_path(&root.join("src/handler.rs"));
    format!(
        r###"{{
  "title": "Pipeline Tour",
  "description": "How a request reaches the handler",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "The handler",
      "file_path": {handler_rs},
      "lines": [1, 3],
      "explanation": "## Where it lands\n\nEach connection is dispatched here."
    }}
  ]
}}"###
    )
}

/// Project with the code-tour plugin and a tour manifest at the root, under a
/// per-test temp dir (CONTRIBUTING.md §4).
fn setup_tour_project() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    // Canonicalize before it reaches the manifest: on macOS a tempdir is
    // `/var/folders/…`, a symlink to `/private/var/…`, and the editor stores
    // the resolved path on the buffer, so an unresolved manifest path never
    // matches and no step finds its file.
    let project_root = fs::canonicalize(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "code-tour");

    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), MAIN_RS).unwrap();
    fs::write(project_root.join("src/handler.rs"), HANDLER_RS).unwrap();
    fs::write(
        project_root.join(".fresh-tour.json"),
        tour_json(&project_root),
    )
    .unwrap();

    (temp_dir, project_root)
}

/// Shell input that asks the kernel how tall the terminal is and writes a
/// marker on that last row, then a second marker on the first row.
///
/// The row-1 marker is the test's sync point: it is written *after* the
/// bottom one and lands in a grid row that is on screen under either PTY
/// size, so seeing it proves the bottom marker has already been processed.
/// That is what makes the assertion below a real failure rather than a hang.
///
/// `B` and `T` are emitted as octal escapes so the marker text never appears
/// in the terminal's echo of the command line itself — finding `BOTMARK_…`
/// on screen can only mean the *output* row was rendered. Octal rather than
/// `\x` because hex escapes in `printf` are a GNU extension.
fn bottom_row_probe(tag: &str) -> Vec<u8> {
    format!(
        "printf '\\033[2J\\033[H'; \
         R=$(stty size | cut -d' ' -f1); \
         printf '\\033[%d;1H\\102OTMARK_{tag}' \"$R\"; \
         printf '\\033[1;1H\\124OPMARK_{tag}'\n"
    )
    .into_bytes()
}

/// Run the probe and block until its rendered end state is on screen.
fn run_bottom_row_probe(harness: &mut EditorTestHarness, tag: &str) {
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(&bottom_row_probe(tag));
    let top = format!("TOPMARK_{tag}");
    harness
        .wait_until(|h| h.screen_to_string().contains(&top))
        .unwrap();
}

/// A terminal open in a vertical split keeps writing where the user can see
/// it after the code tour opens the bottom dock beneath it.
///
/// Fails without the fix: the second probe's bottom marker is written well
/// below the pane's new bottom edge and never renders.
#[test]
#[cfg(not(windows))] // drives a Unix shell (`stty`, `cut`, POSIX `printf`)
fn tour_creating_the_dock_resizes_an_open_terminal() {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }

    let (_temp, project_root) = setup_tour_project();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness
        .open_file(&project_root.join("src/main.rs"))
        .unwrap();
    harness.render().unwrap();

    // A terminal in a vertical split to the right, full height for now. It
    // takes focus, so the probe below reaches its PTY.
    harness
        .run_palette_command("Open Terminal to the Right")
        .unwrap();

    // Baseline: the shell's idea of the bottom row is on screen. Without it
    // the post-tour assertion could pass vacuously against a probe that never
    // worked in the first place (CONTRIBUTING.md §16).
    run_bottom_row_probe(&mut harness, "PRE");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("BOTMARK_PRE"),
        "before the dock exists the terminal's last row must be on screen; \
         if this fails the probe itself is broken.\nScreen:\n{screen}"
    );

    // Focus the editor split, so the tour opens its step there and the
    // palette keystrokes are not swallowed by the PTY.
    harness.mouse_click(20, 10).unwrap();
    harness.render().unwrap();

    // Load the tour: this is the path that creates the bottom dock.
    harness
        .run_palette_command("Tour: Load Definition")
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    harness
        .type_text(&project_root.join(".fresh-tour.json").display().to_string())
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Opening a step is an async chain: mount the panel, open the step's file
    // (focus → editor split), then hand focus back to the panel. The panel is
    // focused *transiently* at the start too, so panel focus alone is not a
    // sound wait — a key sent then would race the rest of the chain. The
    // step's file is only open once `revealStep` has run, so "its tab is on
    // the tab bar" ∧ "the read-only panel holds focus" is first true when the
    // chain is finished.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("*Tour: Pipeline Tour*")
                && screen.contains("Jump to code")
                && screen.contains("handler.rs")
                && h.get_status_bar().contains("[RO]")
        })
        .unwrap();

    // Back into the terminal pane. Column 120 is inside the right split at
    // this width, row 5 inside the (now shorter) terminal pane. The status
    // bar drops the panel's `[RO]` once focus has actually landed there.
    harness.mouse_click(120, 5).unwrap();
    harness
        .wait_until(|h| !h.get_status_bar().contains("[RO]"))
        .unwrap();

    run_bottom_row_probe(&mut harness, "POST");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("BOTMARK_POST"),
        "after the tour created the bottom dock the terminal's own last row \
         must still be on screen — a stale PTY row count puts the shell's \
         output, and its prompt, below the pane's visible bottom edge.\n\
         Screen:\n{screen}"
    );
}
