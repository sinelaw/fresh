//! Regression test for issue #2373: the `#` buffer quick switcher
//! (Ctrl+P → `#`) omitted virtual buffers.
//!
//! Virtual buffers — plugin panels like `*blame:lib.rs*` or `*Git Log: …*`
//! — are shown as tabs but have no backing file path. The switcher built
//! its candidate list only from file-backed buffers, so these panels could
//! be reached only by clicking their tab, never by name.
//!
//! These tests drive the keyboard and assert on rendered output. Because a
//! virtual buffer's name is *always* present in the tab bar, "listed in the
//! switcher" is observed as the name appearing a second time — once in the
//! tab bar and once in the suggestion dropdown.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Count non-overlapping occurrences of `needle` across the rendered screen.
fn occurrences(screen: &str, needle: &str) -> usize {
    screen.matches(needle).count()
}

/// Open the Quick Open prompt and switch it into `#` (buffer) mode.
///
/// Ctrl+P opens Quick Open pre-seeded with the `>` command prefix; deleting
/// it and typing `#` is exactly the "Ctrl+P → #" flow from the issue.
fn open_buffer_switcher(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    // Remove the default ">" command prefix, then enter "#" buffer mode.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#").unwrap();
    harness.render().unwrap();
}

#[test]
fn buffer_switcher_lists_virtual_buffers() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // A plugin panel: a virtual buffer shown as a tab, with no file path.
    let panel = harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("*Git Log: main*".to_string(), "git-log".to_string(), true);
    harness
        .editor_mut()
        .set_virtual_buffer_content(
            panel,
            vec![fresh::primitives::text_property::TextPropertyEntry::text(
                "commit abc123\n  initial commit\n",
            )],
        )
        .unwrap();
    harness.render().unwrap();

    // Sanity: before opening the switcher, the name is present exactly once
    // (the tab bar).
    let before = harness.screen_to_string();
    assert_eq!(
        occurrences(&before, "Git Log: main"),
        1,
        "virtual buffer should appear once (its tab) before opening the switcher. Screen:\n{}",
        before
    );

    open_buffer_switcher(&mut harness);

    // After opening the `#` switcher the panel must ALSO be in the suggestion
    // dropdown — i.e. the name now appears twice (tab + suggestion).
    let after = harness.screen_to_string();
    assert!(
        occurrences(&after, "Git Log: main") >= 2,
        "buffer switcher should list the virtual buffer by name. Screen:\n{}",
        after
    );
}

#[test]
fn buffer_switcher_filters_virtual_buffer_by_name() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Two virtual panels with distinct names.
    harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("*blame:lib.rs*".to_string(), "blame".to_string(), true);
    harness
        .editor_mut()
        .active_window_mut()
        .create_virtual_buffer("*Diagnostics*".to_string(), "diagnostics".to_string(), true);
    harness.render().unwrap();

    // Switch into `#` mode and type a query matching only the blame panel.
    open_buffer_switcher(&mut harness);
    harness.type_text("blame").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // blame:lib.rs appears in the tab bar AND, with the query matching it, in
    // the suggestion dropdown → at least twice.
    assert!(
        occurrences(&screen, "blame:lib.rs") >= 2,
        "fuzzy query 'blame' should surface the blame virtual buffer. Screen:\n{}",
        screen
    );
    // Diagnostics does not match the query, so it stays tab-only (once).
    assert_eq!(
        occurrences(&screen, "Diagnostics"),
        1,
        "query 'blame' should not list the unrelated Diagnostics panel. Screen:\n{}",
        screen
    );
}
