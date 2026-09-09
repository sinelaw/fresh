//! Display-width conformance (plan §2.1, Stage 0.1).
//!
//! Layout measures text with `unicode-width`; the reference backend must
//! paint each row into exactly the columns that measure reserved. These
//! tests pin the policy `fresh_ui::glyph` states, end to end: a description
//! goes in, a screen comes out, and the cells are where layout said.

mod support;

use fresh_ui::{row, text, text_runs, Draw, Run, Size, Sizing, Ui};
use support::screen::{render, Screen};

const FRAME: Size = Size { w: 12, h: 1 };

fn paint(root: fresh_ui::Node<()>) -> (fresh_ui::LayoutSpec, Screen) {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(root, FRAME).clone();
    let screen = render(&spec);
    (spec, screen)
}

/// The row as a list of cells, so an assertion reads like the screen: a wide
/// glyph, then the empty cell it spills into.
fn cells(s: &Screen, y: u16) -> Vec<&str> {
    (0..s.w).map(|x| s.symbol(x, y)).collect()
}

/// `text("你好")` reserves four columns and paints all four: the glyphs in
/// the first and third, their continuations in the second and fourth. The
/// exit criterion of Stage 0.1.
#[test]
fn a_wide_glyph_occupies_the_two_columns_layout_measured() {
    let (spec, s) = paint(row().children([text("你好"), text("!")]));
    let rects: Vec<_> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| (i.rect.x, i.rect.w))
        .collect();
    assert_eq!(
        rects,
        vec![(0, 4), (4, 1)],
        "layout reserves by display width"
    );
    assert_eq!(
        cells(&s, 0),
        ["你", "", "好", "", "!", " ", " ", " ", " ", " ", " ", " "]
    );
    assert!(s.is_continuation(1, 0) && s.is_continuation(3, 0));
    assert_eq!(s.line(0).trim_end(), "你好!");
}

/// A styled run is several items, one per fragment, and each starts where the
/// last one's glyphs end — so a CJK identifier followed by a keyword paints the
/// keyword at column four, not at column two where a per-char walk would put
/// it (and not two cells past its own hole, either).
#[test]
fn mixed_runs_paint_end_to_end_with_no_hole() {
    let (spec, s) = paint(text_runs([
        Run::themed("你好", "identifier"),
        Run::themed("fn", "keyword"),
    ]));
    let rects: Vec<_> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| (i.rect.x, i.rect.w, i.theme.as_str().to_string()))
        .collect();
    assert_eq!(
        rects,
        vec![(0, 4, "identifier".into()), (4, 2, "keyword".into())]
    );
    assert_eq!(
        cells(&s, 0),
        ["你", "", "好", "", "f", "n", " ", " ", " ", " ", " ", " "]
    );
}

/// A base letter and its combining mark are one cell, and the mark rides in
/// it: what follows starts one column on, not two.
#[test]
fn a_combining_mark_consumes_no_column() {
    let (spec, s) = paint(row().children([text("e\u{301}x"), text("|")]));
    let rects: Vec<_> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| (i.rect.x, i.rect.w))
        .collect();
    assert_eq!(rects, vec![(0, 2), (2, 1)]);
    assert_eq!(s.symbol(0, 0), "e\u{301}", "the mark is in its base's cell");
    assert_eq!(s.symbol(1, 0), "x");
    assert_eq!(s.symbol(2, 0), "|");
}

/// An emoji ZWJ sequence is one glyph two cells wide — not one cell per
/// scalar, of which it has five.
#[test]
fn an_emoji_sequence_is_one_two_column_glyph() {
    let family = "👨\u{200d}👩\u{200d}👧";
    let (spec, s) = paint(row().children([text(family), text("!")]));
    let rects: Vec<_> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| (i.rect.x, i.rect.w))
        .collect();
    assert_eq!(rects, vec![(0, 2), (2, 1)]);
    assert_eq!(s.symbol(0, 0), family);
    assert!(s.is_continuation(1, 0));
    assert_eq!(s.symbol(2, 0), "!");
}

/// A wide glyph the clip cuts in half is not painted at all; the column of it
/// that is visible is a blank, and nothing of it leaks past the clip.
#[test]
fn a_wide_glyph_straddling_the_clip_edge_paints_as_a_blank() {
    // Three cells for four columns of text: `好` straddles the edge.
    let (spec, s) = paint(row().children([text("你好").w(Sizing::Cells(3)), text("|")]));
    let rects: Vec<_> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| (i.rect.x, i.rect.w))
        .collect();
    assert_eq!(rects, vec![(0, 3), (3, 1)]);
    assert_eq!(
        cells(&s, 0)[..5],
        ["你", "", " ", "|", " "],
        "half of 好 is a blank, and the neighbour is untouched"
    );
}
