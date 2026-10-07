//! Issue #3466: a wrapped row's hanging indent must not take the highlight of
//! a match that starts the row. A row-wide overlay must still cover it.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, EditorConfig};
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use ratatui::style::Color;

const INDENT: usize = 8;
/// The gutter takes the first 6 screen columns.
const GUTTER: u16 = 6;
const WORD: &str = "banana";

fn bg(harness: &EditorTestHarness, x: u16, y: u16) -> Option<Color> {
    harness.get_cell_style(x, y).and_then(|s| s.bg)
}

fn col(content_col: usize) -> u16 {
    GUTTER + content_col as u16
}

/// Line 1 holds the occurrence the cursor sits on. Line 2 is indented and
/// made only of `WORD`s, so it wraps and every continuation row starts with a
/// match whatever the content width is, and each row also has a match in its
/// middle to compare against.
fn open_wrapped() -> EditorTestHarness {
    let config = Config {
        editor: EditorConfig {
            line_wrap: true,
            wrap_indent: true,
            highlight_occurrences: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let wrapped = format!("{}{}", " ".repeat(INDENT), [WORD; 40].join(" "));
    harness
        .load_buffer_from_text(&format!("{WORD} is the control\n{wrapped}\n"))
        .unwrap();
    harness.render().unwrap();
    harness
}

/// Line 2's first continuation row, checked against what was drawn so a
/// change in wrap geometry fails here instead of making a test vacuous.
fn continuation_row(harness: &EditorTestHarness, control_row: u16) -> u16 {
    let row = control_row + 2;
    let text = harness.screen_row_text(row);
    let at = |x: u16| text.chars().nth(x as usize);
    assert_eq!(
        (
            at(col(INDENT - 1)),
            at(col(INDENT)),
            at(col(INDENT + WORD.len()))
        ),
        (Some(' '), Some('b'), Some(' ')),
        "expected indent + {WORD:?} + gap, got {text:?}"
    );
    row
}

#[test]
fn test_wrapped_row_hanging_indent_is_not_occurrence_highlighted() {
    let mut harness = open_wrapped();
    let (first_row, _) = harness.content_area_rows();
    let control_row = first_row as u16;
    let cont_row = continuation_row(&harness, control_row);

    let head_match = col(INDENT);
    let gap = col(INDENT + WORD.len());
    let mid_match = col(INDENT + WORD.len() + 1);

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .wait_until(|h| bg(h, col(1), control_row) != bg(h, col(WORD.len() + 1), control_row))
        .expect("the word under the cursor should be highlighted");

    let row_bg = bg(&harness, gap, cont_row);

    // A match in the middle of the row, so the test cannot pass by nothing
    // being highlighted.
    assert_ne!(
        bg(&harness, mid_match, cont_row),
        row_bg,
        "a match mid-row is highlighted"
    );
    assert_eq!(
        bg(&harness, head_match, cont_row),
        bg(&harness, mid_match, cont_row),
        "a match starting the row is highlighted the same way"
    );
    for content_col in 0..INDENT {
        assert_eq!(
            bg(&harness, col(content_col), cont_row),
            row_bg,
            "indent column {content_col} must not take the match highlight"
        );
    }
}

/// The band's range starts at the row's first glyph, so only the sweep being
/// advanced onto that byte can admit it while the loop is in the indent. That
/// is the same path a band spanning the wrap takes when the viewport is
/// anchored on a continuation row.
#[test]
fn test_row_wide_band_still_covers_a_wrapped_row_hanging_indent() {
    let mut harness = open_wrapped();
    let (first_row, _) = harness.content_area_rows();
    let control_row = first_row as u16;
    let cont_row = continuation_row(&harness, control_row);

    let words_drawn = harness
        .screen_row_text(control_row + 1)
        .matches(WORD)
        .count();
    let line2_start = format!("{WORD} is the control\n").len();
    let band_start = line2_start + INDENT + words_drawn * (WORD.len() + 1);

    harness
        .apply_event(Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("band".into())),
            range: band_start..band_start + WORD.len(),
            face: OverlayFace::Background { color: (0, 80, 0) },
            priority: 90,
            message: None,
            extend_to_line_end: true,
            url: None,
        })
        .unwrap();
    harness.render().unwrap();

    let band_bg = bg(&harness, col(INDENT), cont_row);
    assert_eq!(
        band_bg,
        Some(Color::Rgb(0, 80, 0)),
        "the band should paint the glyph its range starts on"
    );
    for content_col in 0..INDENT {
        assert_eq!(
            bg(&harness, col(content_col), cont_row),
            band_bg,
            "the band must still cover indent column {content_col}"
        );
    }
}
