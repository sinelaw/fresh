//! The status bar as a native `fresh-ui` description — the first real surface
//! (stage S2 of the migration).
//!
//! The status bar is the smallest surface worth migrating first, and it is
//! already the best prepared: its geometry is derived live rather than recorded
//! during paint, and it is projected as a flat list of segments, each with its
//! own name and side. Nothing about it needs the frame to have inverted, so it
//! can be built and tested before the shell drives rendering.
//!
//! # It answers the inline-styling question, for this surface
//!
//! A display-list item carries **one** theme key, and `TextRun` holds one
//! unstyled string — so a surface needing several colours inside one line has
//! to be composed of several nodes. That is an open decision for the surfaces
//! that style *within* a run (palette match highlights, menu mnemonics,
//! markdown).
//!
//! The status bar is not one of them. Its colour boundaries are exactly its
//! segment boundaries, so one `text()` per segment — each with its own theme —
//! is a faithful rendering, not a workaround. Segment-level theming is the
//! whole requirement here.

use fresh_ui::{row, text, Node, Sizing};

/// Which end of the bar a segment tiles from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

impl Side {
    /// The projection spells this as a string; keep the mapping in one place.
    pub fn from_str(s: &str) -> Side {
        match s {
            "right" => Side::Right,
            _ => Side::Left,
        }
    }
}

/// One status-bar segment: its text, which end it tiles from, and the theme
/// name that decides its colour.
///
/// Deliberately *without* the `x`/`w` the `StatusSegment` projection carries:
/// those are the old layout's output. Here layout computes them.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Segment {
    pub name: &'static str,
    pub text: String,
    pub side: Side,
}

impl Segment {
    pub fn new(name: &'static str, text: impl Into<String>, side: Side) -> Segment {
        Segment {
            name,
            text: text.into(),
            side,
        }
    }
}

/// The status bar: left segments packed from the left, right segments packed
/// against the right, and whatever is left over between them.
pub fn status_bar<M: 'static>(segments: &[Segment]) -> Node<M> {
    // A `TextRun` measures itself (`unicode-width`), so a segment takes exactly
    // the width its text needs and no sizing has to be stated here.
    let seg = |s: &Segment| text(s.text.clone()).theme(s.name);
    let left = segments.iter().filter(|s| s.side == Side::Left).map(seg);
    let right = segments.iter().filter(|s| s.side == Side::Right).map(seg);

    row().theme("status").h(Sizing::Cells(1)).children(
        left
            // The gap between the two groups: it takes whatever is left, so
            // the right group ends flush with the frame's edge at any width.
            .chain(std::iter::once(row().theme("status").flex(1)))
            .chain(right)
            .collect::<Vec<_>>(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold, Caret, HostPainter, Palette};
    use crate::view::shell::frame::HostRegion;
    use fresh_ui::{Draw, Size, ThemeKey, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::style::Style;

    struct NoHost;
    impl HostPainter for NoHost {
        fn paint_host(&mut self, _: HostRegion, _: Rect, _: &mut Buffer, _: &mut Caret) {}
    }

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn render(segments: &[Segment], w: u16) -> (Buffer, Vec<(String, Rect)>) {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(status_bar(segments), Size::new(w, 1)).clone();
        let themed: Vec<(String, Rect)> = spec
            .items
            .iter()
            .filter(|it| matches!(it.draw, Draw::Lines(_)))
            .map(|it| {
                (
                    it.theme.as_str().to_string(),
                    Rect {
                        x: it.rect.x as u16,
                        y: it.rect.y as u16,
                        width: it.rect.w,
                        height: it.rect.h,
                    },
                )
            })
            .collect();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, 1));
        let palette: &dyn Palette = &plain;
        fold(&spec, &mut buf, palette, &mut NoHost);
        (buf, themed)
    }

    fn line(buf: &Buffer) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, 0)].symbol().to_string())
            .collect()
    }

    fn segs() -> Vec<Segment> {
        vec![
            Segment::new("mode", "NORMAL", Side::Left),
            Segment::new("file", "main.rs", Side::Left),
            Segment::new("position", "12:4", Side::Right),
        ]
    }

    #[test]
    fn left_segments_pack_from_the_left_and_right_ones_against_the_edge() {
        let (buf, _) = render(&segs(), 30);
        let row = line(&buf);
        assert!(row.starts_with("NORMALmain.rs"), "got {row:?}");
        assert!(row.ends_with("12:4"), "got {row:?}");
        assert_eq!(row.len(), 30);
    }

    /// The right group stays flush with the edge as the frame changes width —
    /// which is the whole reason the gap between the groups is a flex child
    /// rather than a computed number of spaces.
    #[test]
    fn the_right_group_stays_flush_at_any_width() {
        for w in [20u16, 30, 40, 80, 120] {
            let (buf, _) = render(&segs(), w);
            let row = line(&buf);
            assert!(
                row.ends_with("12:4"),
                "width {w}: right group not flush, got {row:?}"
            );
        }
    }

    /// **The inline-styling finding.** Each segment is its own display-list item
    /// carrying its own theme, so the backend can colour them independently
    /// without the library needing styled spans inside one run. The status bar's
    /// colour boundaries are exactly its segment boundaries.
    #[test]
    fn every_segment_carries_its_own_theme() {
        let (_, themed) = render(&segs(), 30);
        let names: Vec<&str> = themed.iter().map(|(t, _)| t.as_str()).collect();
        for want in ["mode", "file", "position"] {
            assert!(names.contains(&want), "{want:?} missing from {names:?}");
        }
    }

    /// A segment's rectangle is exactly its text's width — the measurement the
    /// old layout computed by hand is now layout's own output.
    #[test]
    fn a_segment_is_as_wide_as_its_text() {
        let (_, themed) = render(&segs(), 30);
        let by = |n: &str| {
            themed
                .iter()
                .find(|(t, _)| t == n)
                .map(|(_, r)| *r)
                .unwrap()
        };
        assert_eq!(by("mode").width, "NORMAL".len() as u16);
        assert_eq!(by("file").width, "main.rs".len() as u16);
        assert_eq!(by("position").width, "12:4".len() as u16);
        // Left group packs with no gap between its segments.
        assert_eq!(by("mode").x, 0);
        assert_eq!(by("file").x, "NORMAL".len() as u16);
    }

    /// An empty bar is a blank row, not a panic or a stray item.
    #[test]
    fn no_segments_is_a_blank_row() {
        let (buf, themed) = render(&[], 12);
        assert_eq!(line(&buf), " ".repeat(12));
        assert!(themed.is_empty());
    }
}
