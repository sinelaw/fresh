//! Reproducer: an overlay with `extend_to_line_end: true` should fill the
//! trailing cells of a non-wrapping line with the overlay's background,
//! even when the editor's `line_wrap` setting is `true` (the default).
//!
//! Today the renderer's fill code is gated on `!line_wrap`, so the
//! trailing cells stay default-bg. The test below documents that
//! behaviour as a failing assertion until the renderer is fixed.

use crate::common::harness::EditorTestHarness;
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use ratatui::style::Color;

/// With the default config (`line_wrap = true`) and a short line that
/// never visually wraps, an overlay covering the line content with
/// `extend_to_line_end: true` should paint its bg out to the right
/// edge of the content area.
#[test]
fn overlay_extend_to_line_end_fills_under_default_line_wrap() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(
        harness.config().editor.line_wrap,
        "this test asserts behaviour under the default line_wrap=true config",
    );

    // Short content that won't wrap at width=80.
    harness.type_text("hi").unwrap();
    harness.render().unwrap();

    // Add an overlay covering "hi" with bright-green bg and extendToLineEnd.
    let event = Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("repro".into())),
        range: 0..2,
        face: OverlayFace::Background { color: (0, 80, 0) },
        priority: 50,
        message: None,
        extend_to_line_end: true,
        url: None,
    };
    harness.apply_event(event).unwrap();
    harness.render().unwrap();

    // Find the row that holds "hi".
    let buf = harness.buffer();
    let mut hit_row: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("hi") {
            hit_row = Some(y);
            break;
        }
    }
    let row = hit_row.expect("could not find 'hi' on screen");

    // Pick a cell well past 'hi' but inside the content area. Width=80;
    // gutter is small; col 40 is comfortably past the content.
    let trailing_cell = &buf[(40, row)];
    let bg = trailing_cell.style().bg;

    // The fix: trailing cell should have the overlay's green bg.
    assert_eq!(
        bg,
        Some(Color::Rgb(0, 80, 0)),
        "expected trailing cell on the overlay row to have the overlay bg \
         (Rgb(0,80,0)) under default line_wrap=true; saw {:?}",
        bg,
    );
}

/// Empty source lines — the line user pressed Enter on but hasn't typed
/// anything in yet — should also fill with the overlay's bg when there
/// is an `extend_to_line_end` overlay covering that line. Live-diff
/// users see "skipped" rows in the middle of an added block (one
/// blank line is left default-bg while the lines around it are green).
///
/// The plugin emits a zero-width overlay for empty lines, but the
/// renderer's overlay sweep is driven by `byte_pos` from visible chars
/// — there are none on an empty line — so the trailing fill never
/// fires for empty source lines.
#[test]
fn overlay_extend_to_line_end_fills_empty_source_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Two non-empty lines surrounding one empty line.
    harness.type_text("aa\n\nbb").unwrap();
    harness.render().unwrap();

    // Buffer is "aa\n\nbb" (6 bytes). Line 0 = "aa" (bytes 0..2).
    // Line 1 = "" (just the \n at byte 3). Line 2 = "bb" (bytes 4..6).
    // Add three overlays, one per line, mirroring how live_diff per-line
    // overlays look. Use `start..start+1` for the empty line so the
    // range is non-zero (covers the newline byte).
    for (start, end) in [(0_usize, 2_usize), (3, 4), (4, 6)] {
        harness
            .apply_event(Event::AddOverlay {
                namespace: Some(OverlayNamespace::from_string("repro".into())),
                range: start..end,
                face: OverlayFace::Background { color: (0, 80, 0) },
                priority: 50,
                message: None,
                extend_to_line_end: true,
                url: None,
            })
            .unwrap();
    }
    harness.render().unwrap();

    // Find the rows for "aa" and "bb".
    let buf = harness.buffer();
    let mut row_aa: Option<u16> = None;
    let mut row_bb: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("aa") && row_aa.is_none() {
            row_aa = Some(y);
        }
        if row.contains("bb") && row_bb.is_none() {
            row_bb = Some(y);
        }
    }
    let row_aa = row_aa.expect("could not find 'aa' on screen");
    let row_bb = row_bb.expect("could not find 'bb' on screen");
    let empty_row = row_aa + 1;
    assert!(
        empty_row < row_bb,
        "expected empty line between aa ({row_aa}) and bb ({row_bb})",
    );

    // The trailing cell on each row — including the empty middle row —
    // should have the green bg.
    for (label, y) in [("aa", row_aa), ("empty", empty_row), ("bb", row_bb)] {
        let bg = buf[(40, y)].style().bg;
        assert_eq!(
            bg,
            Some(Color::Rgb(0, 80, 0)),
            "row '{label}' (y={y}): trailing cell should have the overlay's \
             green bg; saw {bg:?}",
        );
    }
}

/// The band's trailing cells must name a foreground the terminal can invert.
///
/// A block cursor is painted by the terminal, which inverts the two colours it
/// finds in the cell it sits on. Past the end of a line there is no text, so
/// the `extend_to_line_end` fill *is* what the cursor inverts — and the fill
/// used to state `fg = bg`, which inverts to itself: a caret that cannot be
/// seen anywhere inside a code-tour step's band. The fill is ground, so it
/// states the ground's foreground, exactly as the cells past a plain line's
/// end do.
#[test]
fn overlay_extend_to_line_end_fill_keeps_a_visible_cursor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.type_text("hi").unwrap();
    harness.render().unwrap();

    harness
        .apply_event(Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("repro".into())),
            range: 0..2,
            face: OverlayFace::Background { color: (0, 80, 0) },
            priority: 50,
            message: None,
            extend_to_line_end: true,
            url: None,
        })
        .unwrap();
    harness.render().unwrap();

    let editor_fg = harness.editor().theme().editor_fg;
    let buf = harness.buffer();
    let row = (0..buf.area.height)
        .find(|y| {
            (0..buf.area.width)
                .map(|x| buf[(x, *y)].symbol())
                .collect::<String>()
                .contains("hi")
        })
        .expect("could not find 'hi' on screen");

    // The cursor sits just past "hi" — the first cell of the fill — and the
    // rest of the band is the same ground it would move onto.
    let hi_end = (0..buf.area.width)
        .find(|x| buf[(*x, row)].symbol() == "h")
        .expect("could not locate 'h' on the row")
        + 2;
    for x in [hi_end, hi_end + 1, 40] {
        let style = buf[(x, row)].style();
        assert_eq!(
            style.bg,
            Some(Color::Rgb(0, 80, 0)),
            "col {x}: expected the band's bg",
        );
        assert_ne!(
            style.fg, style.bg,
            "col {x}: the band's fill inverts to itself — a block cursor \
             there is invisible",
        );
        assert_eq!(
            style.fg,
            Some(editor_fg),
            "col {x}: the fill is ground, so it states the ground's fg",
        );
    }
}

/// An inlay hint inside a band wears the band.
///
/// LSP inlay hints are spliced into the row as cells with no source byte, and
/// the overlay sweep only ran for cells that had one — so the hint kept the
/// plain editor background and punched a hole through a code-tour step's
/// highlight.
#[test]
fn overlay_extend_to_line_end_covers_inlay_hints() {
    use fresh::view::virtual_text::VirtualTextPosition;
    use ratatui::style::Style;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.type_text("let x = 5;").unwrap();
    harness.render().unwrap();

    // A type hint after `x` (byte 5) and a parameter hint before `5` (byte 8),
    // the two shapes an LSP produces: `AfterChar` and `BeforeChar`.
    {
        let state = harness.editor_mut().active_state_mut();
        let hint_style = Style::default().fg(Color::Rgb(128, 128, 128));
        state.virtual_texts.add(
            &mut state.marker_list,
            5,
            ": i32".to_string(),
            hint_style,
            VirtualTextPosition::AfterChar,
            0,
        );
        state.virtual_texts.add(
            &mut state.marker_list,
            8,
            "n:".to_string(),
            hint_style,
            VirtualTextPosition::BeforeChar,
            0,
        );
    }
    harness.render().unwrap();

    harness
        .apply_event(Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("repro".into())),
            range: 0..10,
            face: OverlayFace::Background { color: (0, 80, 0) },
            priority: 50,
            message: None,
            extend_to_line_end: true,
            url: None,
        })
        .unwrap();
    harness.render().unwrap();

    let buf = harness.buffer();
    let row = (0..buf.area.height)
        .find(|y| {
            (0..buf.area.width)
                .map(|x| buf[(x, *y)].symbol())
                .collect::<String>()
                .contains("let x")
        })
        .expect("could not find the code line on screen");
    let text: String = (0..buf.area.width)
        .map(|x| buf[(x, row)].symbol())
        .collect();

    for hint in [": i32", "n:"] {
        let start = text
            .find(hint)
            .unwrap_or_else(|| panic!("hint {hint:?} is not on screen: {text:?}"));
        for (i, _) in hint.char_indices() {
            let x = (start + i) as u16;
            assert_eq!(
                buf[(x, row)].style().bg,
                Some(Color::Rgb(0, 80, 0)),
                "hint {hint:?} col {x}: an inlay hint inside the band must \
                 wear the band's bg, not punch a hole in it. Row: {text:?}",
            );
        }
    }
}

/// …and a hint on the line *after* a band stays outside it.
///
/// The hint borrows its anchor's styling, not the styling of whatever cell
/// the renderer happened to paint last: a `BeforeChar` hint opening the line
/// below a band is spliced in before any of that line's own bytes, and the
/// band's last byte — the newline it ends on — is the one the overlay sweep
/// still stood on.
#[test]
fn overlay_extend_to_line_end_leaves_the_next_line_s_hints_alone() {
    use fresh::view::virtual_text::VirtualTextPosition;
    use ratatui::style::Style;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.type_text("let x = 5;\nfoo();").unwrap();
    harness.render().unwrap();

    // A hint in front of `foo` — the first byte of the second line (11).
    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add(
            &mut state.marker_list,
            11,
            "call:".to_string(),
            Style::default().fg(Color::Rgb(128, 128, 128)),
            VirtualTextPosition::BeforeChar,
            0,
        );
    }
    harness.render().unwrap();

    // The band covers the first line and the newline that ends it, the way a
    // code tour's step range does.
    harness
        .apply_event(Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("repro".into())),
            range: 0..11,
            face: OverlayFace::Background { color: (0, 80, 0) },
            priority: 50,
            message: None,
            extend_to_line_end: true,
            url: None,
        })
        .unwrap();
    harness.render().unwrap();

    let buf = harness.buffer();
    let row_of = |needle: &str| {
        (0..buf.area.height)
            .find(|y| {
                (0..buf.area.width)
                    .map(|x| buf[(x, *y)].symbol())
                    .collect::<String>()
                    .contains(needle)
            })
            .unwrap_or_else(|| panic!("could not find {needle:?} on screen"))
    };
    let banded = row_of("let x");
    let below = row_of("call:");
    assert_eq!(
        buf[(40, banded)].style().bg,
        Some(Color::Rgb(0, 80, 0)),
        "the first line should carry the band",
    );

    let text: String = (0..buf.area.width)
        .map(|x| buf[(x, below)].symbol())
        .collect();
    let start = text.find("call:").expect("hint is not on screen");
    for i in 0.."call:".len() {
        let x = (start + i) as u16;
        assert_ne!(
            buf[(x, below)].style().bg,
            Some(Color::Rgb(0, 80, 0)),
            "hint col {x}: the line below the band is not in it. Row: {text:?}",
        );
    }
}
