//! A pane's text content, as rows the tree lays out.
//!
//! **This is the seam Blocker A is about**, and it is deliberately only the
//! seam. The editor's style stack — the highlighter, the overlay resolver, the
//! selection sweep — is not ported here and does not need to be: it already
//! produces a row as `Vec<Span>`, and [`runs_of`] turns that into the `Run`s a
//! description carries. What moves is where a row *is*; what a row *says*
//! stays where it is.
//!
//! # Why a supply rather than a builder
//!
//! A description is a layout input; a pane's height is a layout output. Every
//! builder the library offers below layout is `'static` — `layout_reader`'s
//! closure and `HostSpec::Leaf`'s factory both — so none of them can reach the
//! `Editor` to ask for a row. The way out is not a new builder: it is to hand
//! the tree a supply it can read without borrowing anything, prepared while
//! the editor is still in hand.
//!
//! The circularity that seems to make that impossible is already solved in
//! this codebase, twice. `Ui::layout_only` exists precisely so a host can lay
//! out a description it has just built and read rectangles back off it, and
//! `SplitManager::get_leaves_with_rects` already lays the *same* grid
//! description out to answer where the panes are. So the editor can know a
//! pane's exact content height before the frame that shows it, with no second
//! geometry and nothing stale: lay out, prepare the rows that height needs,
//! then build the real description around them.
//!
//! # What the reader still decides
//!
//! [`content`] does not simply place every row it was given. It reads the
//! height it is *actually* granted and emits `min(rows, max_h)` of them, so a
//! disagreement between the editor's idea of the pane's height and layout's
//! degrades to a short pane rather than to rows painted outside their box.
//! The supply is a superset the tree clips; it is not an instruction.
//!
//! # The invariant
//!
//! The number of display-list items a pane produces is a function of its
//! on-screen rows and not of its document length. `a_pane_costs_its_rows_not_its_document`
//! is that, asserted. It is the property the buffer model depends on: the
//! piece tree, `WrapIndex`'s damage-based repair and the highlighter are what
//! make a large file editable, and a description that walked the document
//! would hand all three back.

use std::rc::Rc;

use fresh_ui::{col, layout_reader, text_runs, LayoutInfo, Node, Run, Sizing};
use ratatui::style::Style;
use ratatui::text::Span;

use crate::app::shell_host::shell_theme::{Attrs, Ink, Paint};

/// One visual row of a pane, as the pieces it is styled in.
///
/// A row, not a line: a soft-wrapped source line is several of these, which is
/// what makes the row count uniform at one cell and so what lets a viewport
/// index them. `WrapIndex` already counts in this unit.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Row {
    pub runs: Vec<Run>,
}

impl Row {
    pub fn new(runs: impl IntoIterator<Item = Run>) -> Row {
        Row {
            runs: runs.into_iter().collect(),
        }
    }
}

/// The rows a pane has prepared for the frame being built.
///
/// `Rc` because the builder that reads it is `'static` and may run more than
/// once in a single layout pass — see [`content`].
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Content {
    pub rows: Rc<[Row]>,
}

impl Content {
    pub fn new(rows: impl IntoIterator<Item = Row>) -> Content {
        Content {
            rows: rows.into_iter().collect::<Vec<_>>().into(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }
}

/// A pane's content as a description: one node per visual row on screen.
///
/// The builder runs during layout and closes over nothing but the `Rc`. It may
/// run more than once per pass — the library says so — which is why it is pure
/// and why the supply is immutable.
pub fn content<M: 'static>(c: Content) -> Node<M> {
    layout_reader(move |info: LayoutInfo| {
        // The height layout actually granted, which is the authority. A supply
        // longer than the box is clipped here rather than painted through it.
        let rows = (info.constraints.max_h as usize).min(c.rows.len());
        col().children(
            c.rows[..rows]
                .iter()
                .map(|r| text_runs(r.runs.iter().cloned()).h(Sizing::Cells(1))),
        )
    })
}

/// A ratatui `Style` as an [`Ink`], against the ground the pane paints on.
///
/// **A style that names no background keeps the one behind it.** That is what
/// a `Span` with only `fg` set means to the painter, and an `Item` carries one
/// theme name for both halves, so "unchanged" has to be said rather than left
/// out — which is exactly what `Ink::with_fg` is for.
pub fn ink_of(style: Style, ground: &Ink) -> Ink {
    let mut ink = ground.clone();
    if let Some(fg) = style.fg {
        ink = ink.with_fg(Paint::Lit(fg));
    }
    if let Some(bg) = style.bg {
        ink = ink.with_bg(Paint::Lit(bg));
    }
    ink.plus(Attrs::from_modifier(style.add_modifier))
}

/// A painted row as runs, against the ground the pane paints on.
///
/// The one conversion, so a row that crosses into the tree keeps the colours
/// the painter gave it rather than being re-derived from a theme lookup that
/// would have to reproduce the whole overlay stack to agree.
pub fn runs_of<'a>(spans: impl IntoIterator<Item = &'a Span<'a>>, ground: &Ink) -> Vec<Run> {
    spans
        .into_iter()
        .filter(|s| !s.content.is_empty())
        .map(|s| Run::themed(s.content.as_ref(), ink_of(s.style, ground).to_string()))
        .collect()
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{Size, Ui};
    use ratatui::style::Color;

    const W: u16 = 20;

    fn ground() -> Ink {
        Ink::keys("editor.fg", "editor.bg")
    }

    /// A document of `n` rows, each naming its own index.
    fn document(n: usize) -> Content {
        Content::new((0..n).map(|i| Row::new([Run::plain(format!("line {i}"))])))
    }

    /// Lay a pane's content out in a box `h` cells tall and report the display
    /// list. The pane is the whole box, which is what a maximised pane is.
    fn frame(c: Content, h: u16) -> (usize, String) {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(
            col().h(Sizing::Cells(h)).child(content::<()>(c)),
            Size::new(W, h),
        );
        let mut buf = ratatui::buffer::Buffer::empty(ratatui::layout::Rect::new(0, 0, W, h));
        super::super::fold::fold_native(
            spec,
            &mut buf,
            &super::super::fold::test_palette::palette,
            super::super::fold::Band::Background,
        );
        let text = (0..h)
            .map(|y| {
                (0..W)
                    .map(|x| buf[(x, y)].symbol().to_string())
                    .collect::<String>()
                    .trim_end()
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join("\n");
        (spec.items.len(), text)
    }

    /// **The invariant the buffer model depends on.**
    ///
    /// A pane costs what it shows. The whole reason the piece tree,
    /// `WrapIndex`'s damage-based repair and the highlighter exist is that a
    /// large file must not be walked to be shown, and a description that
    /// walked one would hand all three back — silently, because it would still
    /// be correct on screen.
    ///
    /// Ten rows of a ten-row document and ten rows of a hundred-thousand-row
    /// document are the same frame, so they must be the same cost. The supply
    /// differs in length; the display list must not.
    #[test]
    fn a_pane_costs_its_rows_not_its_document() {
        let (small, _) = frame(document(10), 10);
        let (large, _) = frame(document(100_000), 10);
        assert_eq!(
            small, large,
            "a pane showing ten rows costs ten rows, whatever is behind them"
        );
    }

    /// The rows the box has room for, in order, and no more.
    #[test]
    fn the_window_is_the_rows_the_box_has_room_for() {
        let (_, text) = frame(document(100), 3);
        assert_eq!(text, "line 0\nline 1\nline 2");
    }

    /// **A supply longer than the box is clipped, not painted through it.**
    ///
    /// The editor prepares rows from a height it worked out before the frame;
    /// layout decides the height the frame actually has. Where the two
    /// disagree the tree's answer wins, because it is the one the cells are
    /// laid against — a supply is a superset to draw from, not an instruction.
    #[test]
    fn a_supply_longer_than_the_box_is_clipped_by_it() {
        let (items, text) = frame(document(50), 2);
        assert_eq!(text, "line 0\nline 1");
        let (exact, _) = frame(document(2), 2);
        assert_eq!(items, exact, "the extra rows cost nothing at all");
    }

    /// A pane shorter than its supply is a short pane, not an empty one.
    #[test]
    fn a_supply_shorter_than_the_box_leaves_the_rest_blank() {
        let (_, text) = frame(document(2), 4);
        assert_eq!(text, "line 0\nline 1\n\n");
    }

    /// **A span that names only a foreground keeps the ground behind it.**
    ///
    /// This is what a `Span` means to the painter, and an `Item` carries one
    /// theme name covering both halves — so "the background is unchanged"
    /// cannot be left unsaid on the way across. Said wrongly, every
    /// syntax-coloured token would punch a hole in the selection band it sits
    /// in.
    #[test]
    fn a_foreground_only_span_keeps_the_grounds_background() {
        let g = ground();
        let ink = ink_of(Style::default().fg(Color::Rgb(1, 2, 3)), &g);
        assert_eq!(ink.bg, g.bg, "the ground's background survives");
        assert_eq!(ink.fg, Paint::Lit(Color::Rgb(1, 2, 3)));
    }

    /// The painter's colours cross unchanged. They are arbitrary runtime
    /// values — a plugin's overlay, a markdown span — so there is no theme key
    /// to name them with, and re-deriving them here would mean reproducing the
    /// whole overlay stack to agree with it.
    #[test]
    fn a_painted_row_keeps_the_colours_the_painter_gave_it() {
        let g = ground();
        let spans = [
            Span::styled("fn ", Style::default().fg(Color::Rgb(9, 9, 9))),
            Span::styled("main", Style::default().fg(Color::Rgb(7, 7, 7))),
            Span::raw(""),
        ];
        let runs = runs_of(spans.iter(), &g);
        assert_eq!(runs.len(), 2, "an empty span is not a run");
        assert_eq!(&*runs[0].text, "fn ");
        let named = runs[0].theme.as_ref().expect("a run names its ink");
        assert_eq!(
            Ink::parse(named.as_str())
                .expect("a written ink parses back")
                .fg,
            Paint::Lit(Color::Rgb(9, 9, 9)),
        );
    }
}
