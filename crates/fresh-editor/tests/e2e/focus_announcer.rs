//! Hook parity for focus changes (`app::focus_announcer`, sinelaw/fresh#3326).
//!
//! A plugin keys its state on `buffer_activated` / `buffer_deactivated` /
//! `buffer_closed`, and every path that changes what the user is looking at
//! owes it those hooks. The window switch used to fire only
//! `active_window_changed`, so twelve bundled plugins held the previous
//! window's buffer after a dive; closing a window fired nothing at all for the
//! buffers it dropped. Every buffer hook now also names the window the buffer
//! belongs to, and the composed `active_buffer_changed` says why.
//!
//! The fixture plugin (`tests/plugins/test_focus_log.ts`) appends every
//! focus hook to a file; the test drives window creation, switching and
//! closing through the editor and asserts on the sequence.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use std::fs;
use std::path::Path;

const PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_focus_log.ts"
));

fn install_plugin(project: &Path) {
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_focus_log.ts"), PLUGIN).unwrap();
}

/// The complete lines of the log. The plugin rewrites the file whole and
/// ends it with a newline, so a read that lands mid-write shows a last
/// line without one: that line is not counted.
fn log_lines(project: &Path) -> Vec<String> {
    let text = fs::read_to_string(project.join("focus_log.txt")).unwrap_or_default();
    let complete = match text.rfind('\n') {
        Some(i) => &text[..i],
        None => "",
    };
    complete.lines().map(str::to_string).collect()
}

/// Wait until the log has a line equal to `line`, then return the log.
fn wait_for_line(h: &mut EditorTestHarness, project: &Path, line: &str) -> Vec<String> {
    h.wait_until(|_| log_lines(project).iter().any(|l| l == line))
        .unwrap_or_else(|e| panic!("waiting for {line:?} in {:?}: {e}", log_lines(project)));
    log_lines(project)
}

/// Wait until the log has `line` and, after it, a line starting with
/// `then` — the end of the sequence the announcer fires for one change,
/// each hook of which reaches the file separately. Returns the log from
/// `line` on.
fn wait_for_sequence(
    h: &mut EditorTestHarness,
    project: &Path,
    line: &str,
    then: &str,
) -> Vec<String> {
    let tail = |lines: &[String]| -> Option<Vec<String>> {
        let at = lines.iter().position(|l| l == line)?;
        lines[at..]
            .iter()
            .any(|l| l.starts_with(then))
            .then(|| lines[at..].to_vec())
    };
    h.wait_until(|_| tail(&log_lines(project)).is_some())
        .unwrap_or_else(|e| {
            panic!(
                "waiting for {line:?} then {then:?} in {:?}: {e}",
                log_lines(project)
            )
        });
    tail(&log_lines(project)).unwrap()
}

/// `<buffer>@<window>` after the last `buffer_activated`.
fn last_activated(lines: &[String]) -> String {
    lines
        .iter()
        .rev()
        .find_map(|l| l.strip_prefix("buffer_activated "))
        .expect("a buffer_activated line")
        .to_string()
}

fn window_of(buffer_at_window: &str) -> &str {
    buffer_at_window.split('@').nth(1).unwrap()
}

#[test]
fn a_window_switch_and_close_fire_the_buffer_hooks() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.txt"), "hello\n").unwrap();
    let other = temp_dir.path().join("other");
    fs::create_dir(&other).unwrap();
    install_plugin(&project);

    let mut h = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Opening a file in window 1 activates its buffer — the baseline the
    // announcer diffs against. The hook names the window.
    h.editor_mut().open_file(&project.join("a.txt")).unwrap();
    h.wait_until(|_| {
        log_lines(&project)
            .iter()
            .any(|l| l.starts_with("buffer_activated "))
    })
    .unwrap();
    let a = last_activated(&log_lines(&project));
    assert_eq!(
        window_of(&a),
        "1",
        "window 1's buffer is announced as such: {a}"
    );

    // A second window, then a dive into it. `set_active_window` is the path
    // that fired only `active_window_changed` before.
    let b = h
        .editor_mut()
        .create_window_at(other.clone(), "other".to_string());
    h.editor_mut().set_active_window(b);
    // The composed hook is the last of the sequence: once it is in the
    // file, the whole switch has been recorded.
    let after_switch = wait_for_sequence(
        &mut h,
        &project,
        &format!("active_window_changed 1->{}", b.0),
        "active_buffer_changed ",
    );
    let after_switch = &after_switch[..];
    assert!(
        after_switch
            .iter()
            .any(|l| l == &format!("buffer_deactivated {a}")),
        "the dive deactivates window 1's buffer, in window 1: {after_switch:?}"
    );
    let bb = last_activated(after_switch);
    assert_ne!(
        bb, a,
        "the dive activates window {}'s own buffer: {after_switch:?}",
        b.0
    );
    assert_eq!(window_of(&bb), b.0.to_string(), "{after_switch:?}");
    assert!(
        after_switch
            .iter()
            .position(|l| l.starts_with("buffer_deactivated"))
            .unwrap()
            < after_switch
                .iter()
                .position(|l| l.starts_with("buffer_activated"))
                .unwrap(),
        "deactivated before activated: {after_switch:?}"
    );
    // The composed hook names both ends and the reason.
    assert!(
        after_switch
            .iter()
            .any(|l| l == &format!("active_buffer_changed {bb} from {a} window")),
        "{after_switch:?}"
    );

    // Back to window 1: the same hooks, the other way round.
    h.editor_mut().set_active_window(fresh_core::WindowId(1));
    let after_back = wait_for_sequence(
        &mut h,
        &project,
        &format!("active_window_changed {}->1", b.0),
        "active_buffer_changed ",
    );
    let after_back = &after_back[..];
    assert!(
        after_back
            .iter()
            .any(|l| l == &format!("buffer_deactivated {bb}")),
        "{after_back:?}"
    );
    assert_eq!(last_activated(after_back), a, "{after_back:?}");

    // Closing window B closes its buffers, naming the window, and says so
    // before the window.
    assert!(h.editor_mut().close_window(b));
    let lines = wait_for_line(&mut h, &project, &format!("window_closed {}", b.0));
    let closed_at = lines
        .iter()
        .position(|l| l == &format!("buffer_closed {bb}"))
        .unwrap_or_else(|| panic!("buffer_closed {bb} in {lines:?}"));
    let window_closed_at = lines
        .iter()
        .position(|l| l == &format!("window_closed {}", b.0))
        .unwrap();
    assert!(closed_at < window_closed_at, "{lines:?}");

    // A switch that changes nothing announces nothing: the last line is
    // still the window close.
    h.editor_mut().set_active_window(fresh_core::WindowId(1));
    h.render().unwrap();
    h.render().unwrap();
    assert_eq!(
        log_lines(&project).last().map(String::as_str),
        Some(format!("window_closed {}", b.0).as_str())
    );
}
