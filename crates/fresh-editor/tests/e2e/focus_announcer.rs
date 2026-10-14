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

/// Wait until the log has a line equal to `line`, and return the log that
/// had it. Re-reading the file afterwards would return a different one:
/// the announcer writes each hook separately.
fn wait_for_line(h: &mut EditorTestHarness, project: &Path, line: &str) -> Vec<String> {
    let mut matched: Option<Vec<String>> = None;
    h.wait_until(|_| {
        let lines = log_lines(project);
        matched = lines.iter().any(|l| l == line).then_some(lines);
        matched.is_some()
    })
    .unwrap_or_else(|e| panic!("waiting for {line:?} in {:?}: {e}", log_lines(project)));
    matched.expect("wait_until returned only once the log matched")
}

/// Wait until the log has `line` and, after it, a line starting with
/// `then` — the end of the sequence the announcer fires for one change.
/// Returns `line` through the first `then` after it, out of the log that
/// had it. Cut at the terminator so the caller sees one switch's hooks and
/// not whatever the editor announces next.
fn wait_for_sequence(
    h: &mut EditorTestHarness,
    project: &Path,
    line: &str,
    then: &str,
) -> Vec<String> {
    let sequence = |lines: &[String]| -> Option<Vec<String>> {
        let at = lines.iter().position(|l| l == line)?;
        let end = lines[at..].iter().position(|l| l.starts_with(then))?;
        Some(lines[at..=at + end].to_vec())
    };
    let mut matched: Option<Vec<String>> = None;
    h.wait_until(|_| {
        matched = sequence(&log_lines(project));
        matched.is_some()
    })
    .unwrap_or_else(|e| {
        panic!(
            "waiting for {line:?} then {then:?} in {:?}: {e}",
            log_lines(project)
        )
    });
    matched.expect("wait_until returned only once the sequence was complete")
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

    // A switch that changes nothing announces nothing. "Nothing was
    // written" cannot be established by looking — the hooks arrive
    // asynchronously, so an empty tail only means "not yet" — so the no-op
    // is followed by a change that must announce, and the gap between them
    // is what gets asserted on.
    h.editor_mut().set_active_window(fresh_core::WindowId(1));
    fs::write(project.join("c.txt"), "world\n").unwrap();
    h.editor_mut().open_file(&project.join("c.txt")).unwrap();
    let window_closed = format!("window_closed {}", b.0);
    // The open activates a buffer neither window held before.
    let seen = &[
        format!("buffer_activated {a}"),
        format!("buffer_activated {bb}"),
    ];
    let is_new_activation = |l: &String| l.starts_with("buffer_activated ") && !seen.contains(l);
    let mut matched: Option<Vec<String>> = None;
    h.wait_until(|_| {
        let lines = log_lines(&project);
        matched = lines
            .iter()
            .position(|l| l == &window_closed)
            .filter(|&at| lines[at..].iter().any(is_new_activation))
            .map(|at| lines[at..].to_vec());
        matched.is_some()
    })
    .unwrap_or_else(|e| panic!("waiting for the reopen in {:?}: {e}", log_lines(&project)));
    let after_noop = matched.unwrap();
    // The open announces its own buffer change, so the assertion is that
    // nothing in the tail was announced as a *window* change — the no-op is
    // the only window switch in it.
    assert!(
        !after_noop
            .iter()
            .any(|l| l.starts_with("active_window_changed")
                || l.starts_with("active_buffer_changed") && l.ends_with(" window")),
        "the no-op window switch announced a window change: {after_noop:?}"
    );
}
