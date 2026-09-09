//! Regression tests for issue #2969 (defect 1): a mouse wheel over UI chrome
//! — the menu bar, a tab bar, the status bar — was routed to whatever pane
//! held *focus*, because the wheel router fell back to the active split
//! whenever the pointer hit-tested to no split at all.
//!
//! The user-visible damage was worst with a focused terminal: its scrollback
//! scrolled while the pointer sat on the status bar, nowhere near the terminal
//! pane. The same fall-through moved a focused *editor* the same way, which is
//! what the first test drives (no PTY required, so it covers the fix
//! everywhere).
//!
//! The rule the fix enforces is that the wheel moves what the pointer is over,
//! so the third test covers the other half of it: the tab strip *does* own
//! scrollable content, and a wheel over it pans the tabs rather than doing
//! nothing.
//!
//! All three assert only on rendered output (CONTRIBUTING.md Testing §2): the
//! window of `LINE n` markers on screen *is* the scroll position, so "the pane
//! did not move" and "the pane did move" are both read off the screen. Each
//! also wheels over a surface that should respond, so a fix that simply
//! swallowed every wheel event could not pass.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Wheel notches sent per target, matching the issue's measurements.
const NOTCHES: usize = 5;

/// Every `LINE n` marker visible on screen, in reading order.
///
/// This is the rendered scroll position of whichever pane owns the markers:
/// the list shifts as soon as the pane moves by a single line, and is
/// identical frame-to-frame while it stays put. The shell's echoed command
/// (`... echo "LINE $i" ...`) carries no digits after the marker, so it never
/// contributes an entry.
fn line_markers(screen: &str) -> Vec<u32> {
    screen
        .lines()
        .flat_map(|line| line.split("LINE ").skip(1))
        .filter_map(|rest| {
            let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
            digits.parse::<u32>().ok()
        })
        .collect()
}

/// The chrome rows of a rendered screen: the menu bar, the tab bar, and the
/// status bar — named by what is actually drawn on them, so the coordinates
/// the wheel is aimed at are verified against the frame rather than assumed.
fn chrome_rows(screen: &str) -> Vec<(&'static str, u16)> {
    let row_with = |label: &str, needle: &str| -> u16 {
        screen
            .lines()
            .position(|line| line.contains(needle))
            .unwrap_or_else(|| panic!("no {label} row ({needle:?}) on screen:\n{screen}"))
            as u16
    };
    vec![
        // The menu bar's own labels, the tab bar's close button, and the
        // status bar's palette hint — each row located by what it draws, so
        // the wheel targets follow the frame instead of hard-coded rows.
        ("menu bar", row_with("menu bar", "File   Edit")),
        ("tab bar", row_with("tab bar", "×")),
        ("status bar", row_with("status bar", "Palette: Ctrl+P")),
    ]
}

/// The text of the tab row (the row carrying a tab's close button).
fn tab_bar_text(screen: &str) -> String {
    screen
        .lines()
        .find(|line| line.contains('×'))
        .unwrap_or_else(|| panic!("no tab row on screen:\n{screen}"))
        .to_string()
}

/// A wheel over the menu bar, the tab bar or the status bar must leave the
/// focused editor where it is — while a wheel over the editor pane itself
/// still scrolls it.
///
/// Before the fix the chrome wheel fell through to the focused split, so all
/// three chrome targets scrolled the editor exactly as if the pointer had been
/// over its text (issue #2969: "with the editor focused, wheel over the status
/// bar scrolls the editor, top line 9 → 1").
#[test]
fn wheel_over_chrome_does_not_scroll_the_focused_editor() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    let content: Vec<String> = (1..=200).map(|i| format!("LINE {i}")).collect();
    let _fixture = harness.load_buffer_from_text(&content.join("\n")).unwrap();

    // Park the viewport in the middle of the file so a wheel-up has somewhere
    // to go in either direction.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let baseline = line_markers(&harness.screen_to_string());
    assert!(
        !baseline.is_empty(),
        "the editor should be showing numbered lines. Screen:\n{}",
        harness.screen_to_string()
    );

    for (label, row) in chrome_rows(&harness.screen_to_string()) {
        for _ in 0..NOTCHES {
            harness.mouse_scroll_up(30, row).unwrap();
        }
        assert_eq!(
            line_markers(&harness.screen_to_string()),
            baseline,
            "a wheel over the {label} (row {row}) must not scroll the focused editor. Screen:\n{}",
            harness.screen_to_string()
        );
    }

    // Guard against over-fixing: the wheel still works where a real pane is
    // under the pointer.
    for _ in 0..NOTCHES {
        harness.mouse_scroll_up(30, 10).unwrap();
    }
    let scrolled = line_markers(&harness.screen_to_string());
    assert!(
        scrolled.first() < baseline.first(),
        "a wheel over the editor pane should still scroll it up (was {:?}, now {:?}). Screen:\n{}",
        baseline.first(),
        scrolled.first(),
        harness.screen_to_string()
    );
}

/// The issue's own repro: an editor pane on the left, a focused terminal on
/// the right, and the wheel aimed at chrome above/below the *editor*. The
/// terminal's scrollback must not move — before the fix it scrolled on every
/// one of the three chrome targets (top line 264 → 249 → 234 → 219).
///
/// Requires a PTY; skipped where one cannot be opened, like the other terminal
/// e2e tests.
#[test]
#[cfg(not(windows))] // Uses a Unix shell to produce scrollback
fn wheel_over_chrome_does_not_scroll_the_focused_terminal() {
    // Imported here rather than at module scope so the Windows build, which
    // cfg's this test out, doesn't carry an unused import.
    use portable_pty::{native_pty_system, PtySize};

    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }
    let mut harness = EditorTestHarness::new(120, 30).unwrap();

    // Editor on the left with locatable content, terminal on the right. The
    // terminal takes focus, so it is what the old fall-through would scroll.
    harness.type_text("EDITORPANE").unwrap();
    harness.render().unwrap();
    harness
        .run_palette_command("Open Terminal to the Right")
        .unwrap();
    harness.render().unwrap();

    // Fill the terminal with numbered lines so its scrollback has somewhere to
    // move, then let the shell go quiet: `wait_until_stable` returns only once
    // the prompt has been drawn and the frame stops changing, so nothing the
    // shell emits can be mistaken for a wheel-driven scroll below.
    harness
        .editor_mut()
        .active_window_mut()
        .send_terminal_input(b"for i in $(seq 1 200); do echo \"LINE $i\"; done\n");
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("LINE 200"))
        .unwrap();

    let baseline = line_markers(&harness.screen_to_string());
    assert!(
        !baseline.is_empty(),
        "the terminal should be showing numbered lines. Screen:\n{}",
        harness.screen_to_string()
    );

    // Column 30 is over the *editor* half of the 120-column window, exactly as
    // in the issue: these rows belong to no pane at all.
    for (label, row) in chrome_rows(&harness.screen_to_string()) {
        for _ in 0..NOTCHES {
            harness.mouse_scroll_up(30, row).unwrap();
        }
        assert_eq!(
            line_markers(&harness.screen_to_string()),
            baseline,
            "a wheel over the {label} (row {row}) must not scroll the focused terminal's \
             scrollback. Screen:\n{}",
            harness.screen_to_string()
        );
    }

    // Guard against over-fixing: over the terminal pane the wheel still walks
    // back into the scrollback.
    for _ in 0..NOTCHES {
        harness.mouse_scroll_up(100, 10).unwrap();
    }
    let scrolled = line_markers(&harness.screen_to_string());
    assert!(
        scrolled.first() < baseline.first(),
        "a wheel over the terminal pane should still scroll its scrollback up (was {:?}, now \
         {:?}). Screen:\n{}",
        baseline.first(),
        scrolled.first(),
        harness.screen_to_string()
    );
}

/// The tab strip is chrome, but chrome that owns content: when the tabs
/// overflow their bar it scrolls, so a wheel over it pans the strip rather
/// than doing nothing. That is the same rule as everywhere else — the wheel
/// moves what the pointer is over — and it moves the *view*, not the
/// selection: the active tab and the editor beneath both stay put.
#[test]
fn wheel_over_the_tab_bar_pans_the_tab_strip() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Enough long-named tabs to overflow a 100-column bar. Each fixture has to
    // outlive the test, so they are all bound.
    let _fixtures: Vec<_> = [
        "tab_alpha",
        "tab_bravo",
        "tab_charlie",
        "tab_delta",
        "tab_foxtrot",
        "tab_golf",
        "tab_hotel",
    ]
    .iter()
    .map(|name| {
        harness
            .load_buffer_from_text_named(&format!("{name}.txt"), "PAD")
            .unwrap()
    })
    .collect();
    // The last-opened file is the active tab and carries the numbered content,
    // so the `LINE n` window doubles as proof the editor never moved while the
    // strip did.
    let content: Vec<String> = (1..=200).map(|i| format!("LINE {i}")).collect();
    let _active = harness
        .load_buffer_from_text_named("tab_echo.txt", &content.join("\n"))
        .unwrap();
    harness.render().unwrap();

    let tab_row = chrome_rows(&harness.screen_to_string())
        .into_iter()
        .find(|(label, _)| *label == "tab bar")
        .expect("the tab bar is one of the chrome rows")
        .1;
    let baseline_tabs = tab_bar_text(&harness.screen_to_string());
    let baseline_lines = line_markers(&harness.screen_to_string());
    assert!(
        baseline_tabs.contains("tab_echo") && !baseline_tabs.contains("tab_alpha"),
        "the strip should be scrolled to the active tab with earlier tabs off the left edge — \
         otherwise there is nothing to pan. Tab row: {baseline_tabs:?}"
    );

    // Wheel up over the bar walks the strip back toward the first tab.
    for _ in 0..NOTCHES {
        harness.mouse_scroll_up(30, tab_row).unwrap();
    }
    let panned = tab_bar_text(&harness.screen_to_string());
    assert!(
        panned.contains("tab_alpha"),
        "a wheel over the tab bar should pan the strip toward the first tab (was {baseline_tabs:?}, \
         now {panned:?})"
    );
    assert_eq!(
        line_markers(&harness.screen_to_string()),
        baseline_lines,
        "panning the tab strip must not scroll the editor beneath it. Screen:\n{}",
        harness.screen_to_string()
    );

    // ...and wheel down walks it forward again, stopping at the last tab
    // rather than running off into empty space.
    for _ in 0..NOTCHES * 2 {
        harness.mouse_scroll_down(30, tab_row).unwrap();
    }
    let returned = tab_bar_text(&harness.screen_to_string());
    assert!(
        returned.contains("tab_echo"),
        "a wheel down over the tab bar should pan back to the last tab and stop there, \
         got {returned:?}"
    );
    assert_eq!(
        line_markers(&harness.screen_to_string()),
        baseline_lines,
        "panning the tab strip must not scroll the editor beneath it. Screen:\n{}",
        harness.screen_to_string()
    );
}
