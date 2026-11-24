//! Issue #2877: characters from neighbouring lines stranded between emoji
//! after scrolling.
//!
//! ratatui writes a VS16 emoji (`❤️`) and then, as a separate cell, its
//! trailing column — the cluster's right-hand neighbour, so without a cursor
//! move. A terminal that draws the cluster two columns wide (tmux, kitty,
//! WezTerm, iTerm2) has already advanced past that column, so the write lands
//! one column to the right and the rest of the row's changed cells drift with
//! it. Cells the diff thinks are unchanged are never rewritten, which strands
//! the previous row's characters between the hearts (`❤️=  ❤️ ❤️`).
//!
//! These tests replay the real frame-to-frame output through a terminal
//! model under both width conventions and assert that every column shows the
//! cluster the frame put there.

use crate::common::harness::EditorTestHarness;
use crate::common::terminal_model::WidthPolicy;

const WIDTH: u16 = 46;
const HEIGHT: u16 = 24;

/// The recipe from the issue: emoji rows alternating with ASCII rows long
/// enough to wrap, so scrolling slides one over the other.
fn alternating(emoji: &str, pairs: usize) -> String {
    let mut text = String::new();
    for _ in 0..pairs {
        for _ in 0..6 {
            text.push_str(emoji);
            text.push(' ');
        }
        text.push('\n');
        text.push_str("a=1; b=2; c=3; d=4; e=5; f=6; g=7; h=8;\n");
    }
    text
}

fn scroll_and_check(content: &str, policy: WidthPolicy) {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("a=1; b=2;");
    harness.enable_terminal_model(policy);
    harness.assert_terminal_model_matches_frame();

    for _ in 0..3 {
        harness.mouse_scroll_down(20, 12).unwrap();
        harness.assert_terminal_model_matches_frame();
    }
    // The scroll really moved the view: the first line is gone.
    let first_row = harness.terminal_model().row_text(2);
    assert!(
        !first_row.trim_start().starts_with("1 "),
        "the view did not scroll: {first_row:?}"
    );
}

#[test]
fn vs16_heart_scroll_leaves_no_strays_on_wide_terminal() {
    scroll_and_check(
        &alternating("\u{2764}\u{FE0F}", 60),
        WidthPolicy::ClusterWidth,
    );
}

#[test]
fn vs16_heart_scroll_leaves_no_strays_on_narrow_terminal() {
    scroll_and_check(
        &alternating("\u{2764}\u{FE0F}", 60),
        WidthPolicy::BaseCodepoint,
    );
}

#[test]
fn keycap_scroll_leaves_no_strays_on_wide_terminal() {
    scroll_and_check(
        &alternating("1\u{FE0F}\u{20E3}", 60),
        WidthPolicy::ClusterWidth,
    );
}

#[test]
fn mixed_clusters_scroll_leave_no_strays_on_wide_terminal() {
    let mut content = String::new();
    for emoji in [
        "\u{2764}\u{FE0F}",
        "1\u{FE0F}\u{20E3}",
        "\u{1F1EF}\u{1F1F5}",
        "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}",
        "\u{1F600}",
        "\u{6F22}",
    ] {
        content.push_str(&alternating(emoji, 10));
    }
    scroll_and_check(&content, WidthPolicy::ClusterWidth);
}
