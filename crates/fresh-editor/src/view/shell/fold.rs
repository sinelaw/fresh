//! The backend: a display list folded into terminal cells.
//!
//! `fresh-ui` paints into a [`LayoutSpec`] — a flat, ordered, absolute list of
//! items — and never touches cells itself. This module is the other half of
//! that seam: it walks the list in paint order and writes into a
//! `ratatui::Buffer`, exactly as `examples/interactive.rs` does for the demo.
//!
//! The part that does not exist in any `fresh-ui` backend today is
//! [`Draw::Host`]: content the host owns and draws itself. Both of the
//! library's own backends stub it as a fill. Here it is a callback —
//! [`HostPainter`] — invoked *in paint order*, so a chrome item painted after a
//! host lands on top of it, which is what a popup over a buffer requires.
//!
//! # The borrow shape
//!
//! The callback takes `&mut self` on the host, because painting a buffer split
//! needs the whole `WindowBuffers::with_all_mut` disjoint borrow. That is only
//! possible if the `Ui` does **not** live on the `Editor`: `ui.spec()` borrows
//! the `Ui`, and the callback borrows the editor, so the two must be separate
//! objects. This is the shape the library's own tutorial uses (`app` and `ui`
//! side by side in `main`), and it is a real constraint on where the `Ui` is
//! stored when this lands on the render path.

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;

use fresh_ui::{Draw, LayoutSpec, Scrim, ThemeKey};

use super::frame::HostRegion;

/// Where the terminal caret ends up for a frame.
pub type Caret = Option<(u16, u16)>;

/// Content the host paints itself — a buffer split, a terminal grid, or (during
/// the migration) any region not yet moved onto `fresh-ui`.
pub trait HostPainter {
    /// Paint `region` into `rect`.
    ///
    /// `caret` is an out-parameter with the same shape as `render_content`'s
    /// `pending_hardware_cursor`: a region that owns the caret writes its
    /// screen position, and [`fold`] resolves the winner (see [`fold`]'s doc).
    fn paint_host(&mut self, region: HostRegion, rect: Rect, buf: &mut Buffer, caret: &mut Caret);
}

/// Resolve a theme key to a concrete style. `fresh-ui` says only *where*
/// appearance comes from; the mapping is the backend's.
pub trait Palette {
    fn style(&self, theme: &ThemeKey) -> Style;
}

impl<F: Fn(&ThemeKey) -> Style> Palette for F {
    fn style(&self, theme: &ThemeKey) -> Style {
        self(theme)
    }
}

/// Paint only what the tree owns outright, leaving host regions to their own
/// painters.
///
/// The migration's working state: the frame is a `fresh-ui` tree, but most of
/// its regions are still `Host` leaves painted by the code that always painted
/// them. This folds the native items and skips the hosts, so a region can move
/// into the tree on its own without the ones around it having to move first.
///
/// When the last region is native this collapses into [`fold`], whose
/// `HostPainter` is the general form.
pub fn fold_native(spec: &LayoutSpec, buf: &mut Buffer, palette: &dyn Palette) -> Caret {
    struct Skip;
    impl HostPainter for Skip {
        fn paint_host(&mut self, _: HostRegion, _: Rect, _: &mut Buffer, _: &mut Caret) {}
    }
    fold(spec, buf, palette, &mut Skip)
}

/// Fold a display list into `buf`, returning the caret position for the frame.
///
/// **Caret rule.** A native `fresh-ui` widget that placed a cursor
/// (`LayoutSpec::cursor` — a focused `TextField`) wins over a caret a host
/// region wrote. That reproduces today's behaviour, where a prompt or overlay
/// field takes the caret away from the buffer, without needing the
/// `cursor_suppressed_by_late_overlay` list: if a native field has focus, it
/// set the cursor, and it wins by construction.
pub fn fold(
    spec: &LayoutSpec,
    buf: &mut Buffer,
    palette: &dyn Palette,
    host: &mut dyn HostPainter,
) -> Caret {
    let frame = buf.area;
    let mut host_caret: Caret = None;

    for item in &spec.items {
        let style = palette.style(&item.theme);
        let rect = to_rect(item.rect);
        let clip = intersect(to_rect(item.clip), frame);

        match &item.draw {
            Draw::Fill => fill(buf, rect, ' ', style, clip),
            Draw::Border => border(buf, rect, style, clip),
            Draw::Scrim(Scrim::Opaque) => fill(buf, frame, ' ', style, frame),
            // Dimming is a backend decision; the library only says "everything
            // behind this is receding".
            Draw::Scrim(Scrim::Dim) => restyle(buf, frame, style, frame),
            Draw::Lines(lines) => {
                for (i, line) in lines.iter().enumerate() {
                    let y = rect.y.saturating_add(i as u16);
                    let mut x = rect.x;
                    for ch in line.chars() {
                        put(buf, x, y, ch, style, clip);
                        x = x.saturating_add(1);
                    }
                }
            }
            Draw::Scrollbar {
                offset,
                content,
                window: _,
            } => {
                let track = rect.height.max(1);
                let (top, len) = Draw::scrollbar_thumb(*offset, *content, track);
                for i in 0..track {
                    let ch = if i >= top && i < top + len {
                        '█'
                    } else {
                        '│'
                    };
                    put(buf, rect.x, rect.y.saturating_add(i), ch, style, clip);
                }
            }
            // A hint about where selecting text is meaningful; nothing to draw.
            Draw::Selectable => {}
            Draw::Host(id) => {
                if let Some(region) = HostRegion::from_host_id(*id) {
                    // Clipped to the frame, so a host never paints outside it.
                    let area = intersect(rect, clip);
                    if area.width > 0 && area.height > 0 {
                        host.paint_host(region, area, buf, &mut host_caret);
                    }
                }
            }
        }
    }

    spec.cursor
        .filter(|c| c.visible)
        .map(|c| (c.pos.x.max(0) as u16, c.pos.y.max(0) as u16))
        .or(host_caret)
}

// ---------------------------------------------------------------------------
// cell writing
// ---------------------------------------------------------------------------

fn to_rect(r: fresh_ui::Rect) -> Rect {
    Rect {
        x: r.x.max(0) as u16,
        y: r.y.max(0) as u16,
        width: r.w,
        height: r.h,
    }
}

fn intersect(a: Rect, b: Rect) -> Rect {
    let x = a.x.max(b.x);
    let y = a.y.max(b.y);
    let right = (a.x + a.width).min(b.x + b.width);
    let bottom = (a.y + a.height).min(b.y + b.height);
    Rect {
        x,
        y,
        width: right.saturating_sub(x),
        height: bottom.saturating_sub(y),
    }
}

fn contains(r: Rect, x: u16, y: u16) -> bool {
    x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height
}

fn put(buf: &mut Buffer, x: u16, y: u16, ch: char, style: Style, clip: Rect) {
    if !contains(clip, x, y) || !contains(buf.area, x, y) {
        return;
    }
    let cell = &mut buf[(x, y)];
    cell.set_char(ch);
    cell.set_style(style);
}

fn fill(buf: &mut Buffer, r: Rect, ch: char, style: Style, clip: Rect) {
    for y in r.y..r.y.saturating_add(r.height) {
        for x in r.x..r.x.saturating_add(r.width) {
            put(buf, x, y, ch, style, clip);
        }
    }
}

/// Restyle without touching glyphs — what a dimming scrim does.
fn restyle(buf: &mut Buffer, r: Rect, style: Style, clip: Rect) {
    for y in r.y..r.y.saturating_add(r.height) {
        for x in r.x..r.x.saturating_add(r.width) {
            if contains(clip, x, y) && contains(buf.area, x, y) {
                buf[(x, y)].set_style(style);
            }
        }
    }
}

fn border(buf: &mut Buffer, r: Rect, style: Style, clip: Rect) {
    if r.width < 2 || r.height < 2 {
        return;
    }
    let (l, t) = (r.x, r.y);
    let right = r.x + r.width - 1;
    let bottom = r.y + r.height - 1;
    for x in l..=right {
        put(buf, x, t, '─', style, clip);
        put(buf, x, bottom, '─', style, clip);
    }
    for y in t..=bottom {
        put(buf, l, y, '│', style, clip);
        put(buf, right, y, '│', style, clip);
    }
    put(buf, l, t, '╭', style, clip);
    put(buf, right, t, '╮', style, clip);
    put(buf, l, bottom, '╰', style, clip);
    put(buf, right, bottom, '╯', style, clip);
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{
        col, host, layer, text, Align, Anchor, ComponentExt, Modality, Node, Size, Sizing, Ui,
    };

    /// Records what it was asked to paint, and fills its rect with a letter so
    /// paint order is visible in the buffer.
    #[derive(Default)]
    struct Recorder {
        calls: Vec<(HostRegion, Rect)>,
        caret_at: Option<(HostRegion, u16, u16)>,
    }

    impl HostPainter for Recorder {
        fn paint_host(
            &mut self,
            region: HostRegion,
            rect: Rect,
            buf: &mut Buffer,
            caret: &mut Caret,
        ) {
            self.calls.push((region, rect));
            let ch = match region {
                HostRegion::Body => 'B',
                HostRegion::MenuBar => 'M',
                HostRegion::StatusBar => 'S',
                HostRegion::Dock => 'D',
                HostRegion::Explorer => 'E',
                HostRegion::SearchOptions => 'O',
                HostRegion::PromptLine => 'P',
            };
            fill(buf, rect, ch, Style::default(), rect);
            if let Some((r, x, y)) = self.caret_at {
                if r == region {
                    *caret = Some((x, y));
                }
            }
        }
    }

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn run(root: Node<()>, w: u16, h: u16, rec: &mut Recorder) -> (Buffer, Caret) {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(root, Size::new(w, h)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let caret = fold(&spec, &mut buf, &plain, rec);
        (buf, caret)
    }

    fn row_text(buf: &Buffer, y: u16) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, y)].symbol().to_string())
            .collect()
    }

    /// Every visible region reaches its painter, with the rect layout computed.
    #[test]
    fn each_region_is_painted_into_its_own_rect() {
        let f = Frame {
            menu_bar: true,
            status_bar: true,
            ..Frame::default()
        };
        let mut rec = Recorder::default();
        let (buf, _) = run(frame_tree(f), 10, 4, &mut rec);

        let mut got: Vec<_> = rec.calls.iter().map(|(r, _)| *r).collect();
        got.sort();
        assert_eq!(
            got,
            vec![HostRegion::MenuBar, HostRegion::Body, HostRegion::StatusBar]
                .into_iter()
                .collect::<std::collections::BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
        );
        assert_eq!(row_text(&buf, 0), "MMMMMMMMMM");
        assert_eq!(row_text(&buf, 1), "BBBBBBBBBB");
        assert_eq!(row_text(&buf, 2), "BBBBBBBBBB");
        assert_eq!(row_text(&buf, 3), "SSSSSSSSSS");
    }

    /// **The ordering guarantee.** A chrome item that paints after a host lands
    /// on top of it — the popup-over-a-buffer case. If the fold collected hosts
    /// and painted them in a separate pass, this would come out inverted.
    #[test]
    fn chrome_painted_after_a_host_lands_on_top_of_it() {
        // A layer over the body: it resolves after the frame, so its items come
        // later in the display list.
        let root: Node<()> = col().children([
            host(HostRegion::Body.id()).flex(1),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .modality(Modality::None)
                .child(text("xx").h(Sizing::Cells(1))),
        ]);
        let mut rec = Recorder::default();
        let (buf, _) = run(root, 6, 3, &mut rec);

        let painted: String = (0..3).map(|y| row_text(&buf, y)).collect();
        assert!(
            painted.contains("xx"),
            "the layer's text must survive the host fill beneath it, got {painted:?}"
        );
        // And the host really did paint underneath.
        assert!(painted.contains('B'), "host content missing: {painted:?}");
    }

    /// A host is never handed a rect reaching outside the frame.
    #[test]
    fn a_host_rect_is_clipped_to_the_frame() {
        let f = Frame {
            menu_bar: true,
            status_bar: true,
            search_options: true,
            prompt_line: true,
            ..Frame::default()
        };
        let mut rec = Recorder::default();
        let (buf, _) = run(frame_tree(f), 8, 2, &mut rec);
        for (region, rect) in &rec.calls {
            assert!(
                rect.x + rect.width <= buf.area.width && rect.y + rect.height <= buf.area.height,
                "{region:?} got {rect:?}, outside {:?}",
                buf.area
            );
        }
    }

    /// A host region owning the caret has it reported when nothing native
    /// placed one.
    #[test]
    fn a_host_region_can_own_the_caret() {
        let mut rec = Recorder::default();
        rec.caret_at = Some((HostRegion::Body, 3, 1));
        let (_, caret) = run(frame_tree(Frame::default()), 10, 4, &mut rec);
        assert_eq!(caret, Some((3, 1)));
    }

    /// **The caret rule.** A focused native field placed a cursor, so it wins
    /// over the buffer's — today's "an overlay takes the caret" behaviour,
    /// derived rather than listed.
    #[test]
    fn a_native_cursor_beats_a_host_caret() {
        let root: Node<()> = col().children([
            host(HostRegion::Body.id()).flex(1),
            fresh_ui::widgets::TextField::new("hi")
                .autofocus()
                .node()
                .h(Sizing::Cells(1)),
        ]);
        let mut rec = Recorder::default();
        rec.caret_at = Some((HostRegion::Body, 9, 9));
        let (_, caret) = run(root, 10, 4, &mut rec);
        assert!(
            caret.is_some() && caret != Some((9, 9)),
            "the focused field's cursor must win over the host's, got {caret:?}"
        );
    }

    /// **The borrow shape**, as a compile-time proof: the fold reads the spec
    /// off the `Ui` while the callback holds `&mut` on a separate host object.
    /// This only type-checks because the `Ui` is *not* stored on the host —
    /// the constraint documented at the top of this module.
    #[test]
    fn the_ui_and_the_host_are_borrowed_disjointly() {
        struct Host {
            painted: u32,
        }
        impl HostPainter for Host {
            fn paint_host(&mut self, _: HostRegion, _: Rect, _: &mut Buffer, _: &mut Caret) {
                self.painted += 1; // real &mut access to host state
            }
        }

        let mut ui: Ui<()> = Ui::new();
        let mut host_state = Host { painted: 0 };
        let mut buf = Buffer::empty(Rect::new(0, 0, 10, 4));

        // Two separate objects, borrowed at the same time.
        let spec = ui.frame(frame_tree(Frame::default()), Size::new(10, 4));
        let _ = fold(spec, &mut buf, &plain, &mut host_state);

        assert!(host_state.painted > 0);
    }
}

#[cfg(test)]
mod native_tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, Ui};

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    /// **The property the migration's working state depends on.** While every
    /// region is a `Host` leaf, folding the native items paints nothing — so
    /// the shell can own the frame's layout without touching a single cell,
    /// and each region's existing painter keeps producing exactly what it did.
    #[test]
    fn a_frame_of_host_regions_paints_nothing() {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui
            .frame(frame_tree(Frame::default()), Size::new(20, 6))
            .clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 6));
        let before = buf.clone();
        fold_native(&spec, &mut buf, &plain);
        assert_eq!(buf, before, "host regions must be left to their painters");
    }

    /// And a native region *is* painted, so a surface starts drawing through
    /// the fold the moment it stops being a host.
    #[test]
    fn a_native_region_is_painted() {
        use crate::view::shell::status_bar::{status_bar, Segment, Side};
        let mut ui: Ui<()> = Ui::new();
        let segs = [Segment::new("mode", "NORMAL", Side::Left)];
        let spec = ui.frame(status_bar(&segs), Size::new(20, 1)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 1));
        fold_native(&spec, &mut buf, &plain);
        let row: String = (0..20).map(|x| buf[(x, 0)].symbol().to_string()).collect();
        assert!(row.starts_with("NORMAL"), "got {row:?}");
    }
}
