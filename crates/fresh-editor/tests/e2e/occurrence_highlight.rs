use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_occurrence_highlight_toggle() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .type_text("apple banana apple cherry apple")
        .unwrap();
    harness.render().unwrap();

    let (content_first, _) = harness.content_area_rows();

    for _ in 0..14 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let first_apple_col = 6;
    let third_apple_col = 32;
    let default_bg = harness.get_cell_style(1, content_first as u16).unwrap().bg;

    // Use wait_until for initial highlight
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                != default_bg
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    != default_bg
        })
        .expect("Background should become highlighted initially");

    let initial_bg_first = harness
        .get_cell_style(first_apple_col, content_first as u16)
        .unwrap()
        .bg;
    let initial_bg_third = harness
        .get_cell_style(third_apple_col, content_first as u16)
        .unwrap()
        .bg;

    // Toggle off
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Occurrence Highlight").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait until it returns to default
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                == default_bg
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    == default_bg
        })
        .expect("Background should be restored to default when toggled off");

    // Toggle back on
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Occurrence Highlight").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait until it returns to highlighted
    harness
        .wait_until(|h| {
            h.get_cell_style(first_apple_col, content_first as u16)
                .unwrap()
                .bg
                == initial_bg_first
                && h.get_cell_style(third_apple_col, content_first as u16)
                    .unwrap()
                    .bg
                    == initial_bg_third
        })
        .expect("Background should be highlighted again");
}
