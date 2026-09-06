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

use fresh_ui::glyph::glyphs_in;
use fresh_ui::{BorderStyle, Draw, LayoutSpec, Scrim, ThemeKey};

use super::frame::HostTarget;

/// Where the fold reports what it painted, for the theme inspector.
///
/// **The fold is the only party that sees every described cell**, which is why
/// F.6 belongs here: every other writer of the per-cell theme map is a painter,
/// and a described surface has no painter, so Ctrl+Right-click went blank over
/// the menu bar, the status bar, the explorer, settings, the popups and the
/// dock as each one crossed — silently, because no test asks the inspector
/// about chrome.
///
/// It reports the *rectangle and the theme name*, not a resolved key pair: the
/// grammar those names are written in belongs to the host (`shell_theme`), and
/// teaching this module to read it would drag the editor's theme vocabulary
/// into the backend that is meant to be ignorant of it.
pub trait ProvenanceSink {
    /// One painted item: the cells `rect ∩ clip` covers, wearing `theme`.
    fn item(&mut self, rect: Rect, clip: Rect, theme: &ThemeKey);
}

/// Content the host paints itself — a buffer split, a terminal grid, or (during
/// the migration) any region not yet moved onto `fresh-ui`.
///
/// **A host paints cells and nothing else.** Where the frame's caret is, is
/// the display list's to say (`LayoutSpec::cursor`): a pane's leaf places it
/// from the caret its text pass settled, a field's run from its byte. The
/// out-parameter a host used to write its caret into is gone with the last
/// host that had one to write.
pub trait HostPainter {
    /// Paint `region` into `rect`.
    fn paint_host(&mut self, target: HostTarget, rect: Rect, buf: &mut Buffer);
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

/// Which half of the display list a fold pass writes.
///
/// The migration needs two passes because there is one display list and *many*
/// legacy painters, and the legacy painters are not in the list. A native
/// region that sits under the legacy ones (the menu bar row, the sidebar, the
/// status bar) has to be written *before* them; a native overlay (a context
/// menu, a dropdown) has to be written *after*. One pass can only serve one of
/// those, which is why migration was previously confined to surfaces that
/// paint over everything.
///
/// The two bands are the same list, cut once at `LayoutSpec::layers_from`.
/// Legacy painters run in between, so each band lands where its surface
/// belongs, and the rule that migration must proceed top-down through the old
/// paint order retires with it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Band {
    /// In-flow content: everything before the first overlay.
    Background,
    /// Out-of-flow content: the `Layer`s, which `fresh-ui` paints after the
    /// tree they were declared in.
    Overlay,
}

/// Paint one band of what the tree owns outright, leaving host regions to
/// their own painters.
///
/// The migration's working state: the frame is a `fresh-ui` tree, but most of
/// its regions are still `Host` leaves painted by the code that always painted
/// them. This folds the native items of one band and skips the hosts.
///
/// # Two passes, one list
///
/// `render` calls this twice. [`Band::Background`] runs before any legacy
/// painter, so the legacy ones land on top of it; [`Band::Overlay`] runs after
/// all of them, so it lands on top of *them*. Within each band the display
/// list already interleaves correctly by paint order; the cut is what lets the
/// legacy painters slot in between, since they are not in the list at all.
///
/// Get the band wrong and the failure is silent — a status bar painted in the
/// overlay band sits on top of the popup that is supposed to cover it — so the
/// rule is the surface's own nature: a `Layer` is an overlay, anything in flow
/// is background.
///
/// When the last region is native this collapses into [`fold`], whose
/// `HostPainter` is the general form, and both bands become one pass again.
pub fn fold_native(spec: &LayoutSpec, buf: &mut Buffer, palette: &dyn Palette, band: Band) {
    fold_band(spec, buf, palette, &mut SkipHosts, band, Paints::All, None)
}

/// A [`HostPainter`] that paints no host region.
///
/// For a band whose hosts were painted by someone else — the overlay band,
/// whose `Host` leaves belong to the legacy painters that ran between the two
/// folds — or for a fold that only wants the described cells.
pub struct SkipHosts;

impl HostPainter for SkipHosts {
    fn paint_host(&mut self, _: HostTarget, _: Rect, _: &mut Buffer) {}
}

/// Fold a display list into `buf`.
///
/// The frame's caret is not this fold's to return: it is `spec.cursor`, which
/// the library resolved when it painted — the last surface to place one wins,
/// and a layer painted over a cursor ends it — so the caller reads it off the
/// display list it folded.
pub fn fold(
    spec: &LayoutSpec,
    buf: &mut Buffer,
    palette: &dyn Palette,
    host: &mut dyn HostPainter,
) {
    fold_all(spec, buf, palette, host, Paints::All, None);
}

/// [`fold`], with which items a pass writes and one provenance sink over
/// both bands: **the frame's one paint** (design §3.3). Every cell the
/// frame shows is written here — the tree's own surfaces in display-list
/// order, the panes and embeds through `host` where their leaves are — so
/// nothing paints between the in-flow content and the layers, and what
/// runs after it is an effect over the finished cells (the software cursor,
/// an animation), never a surface.
pub fn fold_all(
    spec: &LayoutSpec,
    buf: &mut Buffer,
    palette: &dyn Palette,
    host: &mut dyn HostPainter,
    paints: Paints,
    provenance: Option<&mut dyn ProvenanceSink>,
) {
    match provenance {
        Some(p) => {
            fold_band(
                spec,
                buf,
                palette,
                host,
                Band::Background,
                paints,
                Some(&mut *p),
            );
            fold_band(spec, buf, palette, host, Band::Overlay, paints, Some(p));
        }
        None => {
            fold_band(spec, buf, palette, host, Band::Background, paints, None);
            fold_band(spec, buf, palette, host, Band::Overlay, paints, None);
        }
    }
}

/// Which of the display list's items a pass writes.
///
/// A frontend that draws the tree's own surfaces itself — the web projects the
/// menu bar, the status bar and the explorer as DOM — still needs the host
/// regions painted into cells, because the panes *are* cells there. That is
/// one distinction, and it is this one: `HostsOnly` runs every `Draw::Host`
/// callback and writes nothing else.
///
/// It replaces skipping the fold outright, which skipped the body with it —
/// so the pass that fed the web its pane cells had to be a second, separate
/// call to the split renderer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Paints {
    All,
    HostsOnly,
}

/// [`fold`], restricted to one band. See [`Band`].
pub fn fold_band(
    spec: &LayoutSpec,
    buf: &mut Buffer,
    palette: &dyn Palette,
    host: &mut dyn HostPainter,
    band: Band,
    paints: Paints,
    mut provenance: Option<&mut dyn ProvenanceSink>,
) {
    let frame = buf.area;

    // The library says where the split is. It used to be derived here, by
    // matching keys against a hand-kept list of the frame's layer families —
    // which was wrong for a scrim (unkeyed, and pushed *before* its layer's
    // items) and for an unkeyed layer (`widgets::Dropdown` has none), and
    // would have put both on the background side, silently.
    let items = match band {
        Band::Background => spec.in_flow(),
        Band::Overlay => spec.layers(),
    };
    for item in items {
        if paints == Paints::HostsOnly && !matches!(item.draw, Draw::Host(_)) {
            continue;
        }
        // From a reset, not a patch. An item's theme says what its cells look
        // like *outright* — a display list is not a diff over whatever was
        // there before. `Cell::set_style` patches, so without the reset an
        // item painted over legacy cells inherits their modifiers: a dropdown
        // drawn over the active tab came out bold, because the tab's BOLD
        // survived a fill that only named a foreground and a background. The
        // ratatui `Block` this replaced said `Style::reset()` for the same
        // reason.
        let style = Style::reset().patch(palette.style(&item.theme));
        let rect = to_rect(item.rect);
        let clip = intersect(to_rect(item.clip), frame);

        // Recorded before the draw, and only for the kinds that put a theme on
        // cells. A `Host` is excluded because the painter behind it records its
        // own — the split grid's line numbers and syntax runs are far finer
        // than "this rectangle wore this name". A scrim is excluded because it
        // is a statement about everything behind it rather than about its own
        // cells.
        if let Some(sink) = provenance.as_deref_mut() {
            if matches!(
                item.draw,
                Draw::Fill | Draw::Wash | Draw::Border(_) | Draw::Lines(_) | Draw::Scrollbar { .. }
            ) {
                sink.item(rect, clip, &item.theme);
            }
        }

        match &item.draw {
            Draw::Fill => fill(buf, rect, ' ', style, clip),
            // A wash keeps the text it covers: the theme's ground over the
            // cells, their symbols and foregrounds as they were.
            Draw::Wash => wash(buf, rect, style, clip),
            Draw::Border(bs) => border(buf, item.rect, style, clip, *bs),
            Draw::Scrim(Scrim::Opaque) => fill(buf, frame, ' ', style, frame),
            // Dimming is a backend decision; the library only says "everything
            // behind this is receding" — so this one *is* a patch over what is
            // already there. **The same patch the painters made**:
            // `apply_dimming` darkened every colour behind a modal by two
            // thirds and marked a `Reset` cell `DIM`, and a described modal's
            // scrim must read the same way, or the frame behind a settings
            // dialog and the frame behind a keybinding dialog would dim
            // differently. Restyling with the palette's *ground* was what
            // this did before, which repainted every cell behind the layer in
            // the editor's plain colours rather than receding them.
            Draw::Scrim(Scrim::Dim) => crate::view::dimming::dim_buffer(buf, frame, None),
            Draw::Lines(lines) => {
                // Clipped to the item's own rect as well as its inherited one:
                // an item declares how much room it has. Layout hands a
                // constrained node the width it was *allowed*, not the width
                // its content wants, so a run can be longer than the rect
                // carrying it — and one that is would paint through whatever
                // encloses it, a menu row straight through its own border.
                // The library's own backends make the same guarantee.
                let clip = intersect(clip, rect);
                for (i, line) in lines.iter().enumerate() {
                    // The row is the item's own, so a block whose first rows
                    // have scrolled off the top loses those rows rather than
                    // sliding the rest down. Same rule as the outline above.
                    let y = item.rect.y + i as i32;
                    if !(0..=u16::MAX as i32).contains(&y) {
                        continue;
                    }
                    let y = y as u16;
                    // By display width, not by char — the library's policy
                    // (`fresh_ui::glyph`): a wide glyph keeps both cells
                    // layout measured for it, a mark rides in its base's
                    // cell, and a glyph the clip would halve is a blank.
                    for g in glyphs_in(
                        line,
                        item.rect.x,
                        i32::from(clip.x),
                        i32::from(clip.x.saturating_add(clip.width)),
                    ) {
                        put_symbol(buf, g.x as u16, y, g.text, g.width, style, clip);
                    }
                }
            }
            // **A bar is two background colours, not two glyphs.** The
            // editor's own scrollbar (`view::ui::scrollbar::render_scrollbar`)
            // fills each track cell with a space and a background, because
            // box-drawing glyphs leave gaps between rows in some terminals —
            // and every test that finds a scrollbar on screen finds it by that
            // background. So the pair the item names reads the way it does
            // everywhere else, with the thumb in front of the track it sits
            // on: the foreground is the thumb, the background is the track,
            // and both are written as the cell's background.
            Draw::Scrollbar {
                offset,
                content,
                window,
                axis,
                marks,
            } => {
                let track = match axis {
                    fresh_ui::Axis::Vertical => rect.height.max(1),
                    fresh_ui::Axis::Horizontal => rect.width.max(1),
                };
                let (top, len) =
                    Draw::scrollbar_thumb(*offset, *content, u32::from(*window), track);
                for i in 0..track {
                    let colour = if i >= top && i < top + len {
                        style.fg
                    } else {
                        style.bg
                    };
                    let mut cell = Style::default();
                    if let Some(c) = colour {
                        // Both halves, not just the ground. The glyph is a
                        // space, so the foreground is invisible either way —
                        // what naming it buys is that the cell is the *bar's*
                        // whatever it covers. An overlay bar floats over rows
                        // that light on hover, and a cell that took its
                        // foreground from the row underneath would change
                        // colour when the row did.
                        cell = cell.fg(c).bg(c);
                    }
                    // A mark shares the cell with the bar: a half block in
                    // the mark's colour over the bar's own ground shows both
                    // the mark and the scroll position; a full mark takes
                    // the cell (a track cell lit under the pointer).
                    let mut glyph = ' ';
                    if let Some(m) = marks.iter().find(|m| m.at == i) {
                        let ms = palette.style(&m.theme);
                        if m.full {
                            if let Some(c) = ms.bg.or(ms.fg) {
                                cell = cell.fg(c).bg(c);
                            }
                        } else if let Some(c) = ms.fg {
                            cell = cell.fg(c);
                            glyph = '▌';
                        }
                    }
                    let (x, y) = match axis {
                        fresh_ui::Axis::Vertical => (rect.x, rect.y.saturating_add(i)),
                        fresh_ui::Axis::Horizontal => (rect.x.saturating_add(i), rect.y),
                    };
                    put(buf, x, y, glyph, cell, clip);
                }
            }
            // A hint about where selecting text is meaningful; nothing to draw.
            Draw::Selectable => {}
            Draw::Host(id) => {
                let target = HostTarget::from_host_id(*id);
                debug_assert!(
                    target.is_some(),
                    "a host id that names neither a region nor a pane: {id:?} \
                     would paint nothing, in silence"
                );
                if let Some(target) = target {
                    // Clipped to the frame, so a host never paints outside it.
                    let area = intersect(rect, clip);
                    if area.width > 0 && area.height > 0 {
                        host.paint_host(target, area, buf);
                    }
                }
            }
        }
    }
}

/// A palette whose styles are *distinguishable*, for tests.
///
/// The shell's tests all used `Style::default()`, so every cell came out
/// looking the same and no test could tell a highlighted row from an ordinary
/// one, a bold label from a plain one, or a fill that reset the cell beneath
/// it from one that inherited its modifiers. Four cell-level bugs reached CI
/// through that gap in a single wave. A test that renders should be able to
/// assert *how* a cell looks, not only what it says.
///
/// Each theme name gets its own colour, and the two modifiers the real
/// [`ShellPalette`](crate::app::ShellPalette) applies — bold for an active
/// menu-bar label, underline for its mnemonic — are reproduced from the name,
/// so a test asserting them is asserting the same structure the editor uses.
#[cfg(test)]
pub(crate) mod test_palette {
    use ratatui::style::Style;

    use fresh_ui::ThemeKey;

    /// The style this palette gives a theme name.
    ///
    /// The **real** resolution — `shell_theme::resolve` against a default
    /// `Theme` — not a synthetic stand-in. A shell name is a pair of theme
    /// keys, so a test that asserts a style is asserting the mapping the
    /// editor actually performs: that a highlighted row reads
    /// `ui.menu_highlight_*`, that a mnemonic adds an underline and nothing
    /// else. A hash-derived palette could only ever say "these two names
    /// differ".
    pub(crate) fn of(name: &str) -> Style {
        thread_local! {
            static THEME: crate::view::theme::Theme =
                crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
                    .expect("a theme of nothing but defaults");
        }
        THEME.with(|t| crate::app::shell_host::shell_theme::resolve(name, t))
    }

    /// The palette itself, for handing to [`super::fold_native`].
    pub(crate) fn palette(theme: &ThemeKey) -> Style {
        of(theme.as_str())
    }

    /// What a cell painted under this palette actually carries.
    ///
    /// Computed by painting one, rather than by reproducing the arithmetic:
    /// the fold applies `Style::reset()` before the theme's own style and
    /// `Cell::set_style` patches, so the modifier bookkeeping is easy to
    /// restate wrongly and impossible to get wrong this way.
    pub(crate) fn painted(name: &str) -> Style {
        let mut cell = ratatui::buffer::Cell::default();
        cell.set_style(Style::reset().patch(of(name)));
        cell.style()
    }
}

// ---------------------------------------------------------------------------
// cell writing
// ---------------------------------------------------------------------------

/// A library rectangle as a buffer one: the part of it the screen has.
///
/// **What is off the top is cut, not moved.** The library places a scrolled
/// item at its true position, which for one whose top has left the window is a
/// negative `y`; clamping that to zero and keeping the height slid the whole
/// box down the screen by however far it had scrolled. A fill only looked
/// taller for it, but a bordered card carried its bottom edge along — the
/// frame of a section that was half off the top closed several rows into the
/// section after it.
fn to_rect(r: fresh_ui::Rect) -> Rect {
    let (x, y) = (r.x.max(0), r.y.max(0));
    let cut = |lost: i32, extent: u16| extent.saturating_sub(lost.min(u16::MAX as i32) as u16);
    Rect {
        x: x as u16,
        y: y as u16,
        width: cut(x - r.x, r.w),
        height: cut(y - r.y, r.h),
    }
}

/// One cell of a draw whose geometry is the library's, in the library's
/// coordinates: off the top or the left of the screen is nothing to paint.
fn put_at(buf: &mut Buffer, x: i32, y: i32, ch: char, style: Style, clip: Rect) {
    if x < 0 || y < 0 || x > u16::MAX as i32 || y > u16::MAX as i32 {
        return;
    }
    put(buf, x as u16, y as u16, ch, style, clip);
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
    let mut b = [0u8; 4];
    put_symbol(buf, x, y, ch.encode_utf8(&mut b), 1, style, clip);
}

/// Paint one grapheme cluster at `(x, y)`, and blank the `w - 1` cells after
/// it that a wide glyph spills into.
///
/// `Cell::set_symbol` writes one cell and leaves the next alone, and ratatui
/// then skips that next cell when it draws, because the wide glyph before it
/// covers it — so whatever was in it stays *in the buffer*, and the moment
/// something narrow is painted over the glyph, the stale cell is back on
/// screen. Blanking it here is what `Buffer::set_stringn` does for the same
/// reason.
fn put_symbol(buf: &mut Buffer, x: u16, y: u16, sym: &str, w: u16, style: Style, clip: Rect) {
    if !contains(clip, x, y) || !contains(buf.area, x, y) {
        return;
    }
    let cell = &mut buf[(x, y)];
    cell.set_symbol(sym);
    cell.set_style(style);
    for k in 1..w {
        let cx = x.saturating_add(k);
        if !contains(clip, cx, y) || !contains(buf.area, cx, y) {
            break;
        }
        let cell = &mut buf[(cx, y)];
        cell.set_symbol(" ");
        cell.set_style(style);
    }
}

fn fill(buf: &mut Buffer, r: Rect, ch: char, style: Style, clip: Rect) {
    for y in r.y..r.y.saturating_add(r.height) {
        for x in r.x..r.x.saturating_add(r.width) {
            put(buf, x, y, ch, style, clip);
        }
    }
}

/// Lay `style`'s background over `r`, keeping each cell's symbol, foreground
/// and modifiers — the region form of a run that inherits its background.
fn wash(buf: &mut Buffer, r: Rect, style: Style, clip: Rect) {
    let Some(bg) = style.bg else {
        return;
    };
    let area = intersect(intersect(r, clip), buf.area);
    for y in area.y..area.y.saturating_add(area.height) {
        for x in area.x..area.x.saturating_add(area.width) {
            buf[(x, y)].set_bg(bg);
        }
    }
}

/// **The outline is drawn at the box's own corners, not the window's.** It
/// takes the library's rectangle rather than the screen one so an edge that
/// has scrolled out of view is simply not drawn; a box clamped into the window
/// first would grow a top edge it does not have and lose the bottom one it
/// does.
fn border(buf: &mut Buffer, r: fresh_ui::Rect, style: Style, clip: Rect, bs: BorderStyle) {
    if r.w < 2 || r.h < 2 {
        return;
    }
    let (l, t) = (r.x, r.y);
    let right = r.x + r.w as i32 - 1;
    let bottom = r.y + r.h as i32 - 1;
    let (h, v, tl, tr, br, bl) = bs.glyphs();
    for x in l..=right {
        put_at(buf, x, t, h, style, clip);
        put_at(buf, x, bottom, h, style, clip);
    }
    for y in t..=bottom {
        put_at(buf, l, y, v, style, clip);
        put_at(buf, right, y, v, style, clip);
    }
    // **The corners are the description's, not this backend's.** This used to
    // be an unconditional `┌┐└┘`, matching ratatui's `BorderType::Plain` and
    // every bordered surface in the editor's chrome — which was right until a
    // *plugin panel* was described. A `WidgetSpec` card and a labelled section
    // have always been drawn `╭╮╰╯` by `widgets::render`, so describing one
    // silently squared it off: the code-tour panel's `╭─ Steps`, and the dock's
    // card density, are what noticed.
    put_at(buf, l, t, tl, style, clip);
    put_at(buf, right, t, tr, style, clip);
    put_at(buf, l, bottom, bl, style, clip);
    put_at(buf, right, bottom, br, style, clip);
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{col, host, layer, text, Align, Anchor, Modality, Node, Size, Sizing, Ui};

    /// Records what it was asked to paint, and fills its rect with a letter so
    /// paint order is visible in the buffer.
    #[derive(Default)]
    struct Recorder {
        /// Panes, addressed by leaf: there is no fixed set of them.
        panes: Vec<(crate::model::event::LeafId, Rect)>,
    }

    impl HostPainter for Recorder {
        fn paint_host(&mut self, target: HostTarget, rect: Rect, buf: &mut Buffer) {
            match target {
                HostTarget::Pane(leaf) => {
                    self.panes.push((leaf, rect));
                    fill(buf, rect, '#', Style::default(), rect);
                }
                // An embedded editor window is a leaf in a plugin panel. These
                // tests build frames with no panel in them, so nothing reaches
                // here — but filling it distinguishably means a frame that
                // grows one is a visible change rather than a silent no-op.
                HostTarget::Embed(_) => {
                    fill(buf, rect, '@', Style::default(), rect);
                }
                // The card's preview band, for the same reason.
                HostTarget::Card(_) => {
                    fill(buf, rect, '%', Style::default(), rect);
                }
            }
        }
    }

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn run_msg(
        root: Node<crate::view::shell::msg::UiMsg>,
        w: u16,
        h: u16,
        rec: &mut Recorder,
    ) -> Buffer {
        let mut ui: Ui<crate::view::shell::msg::UiMsg> = Ui::new();
        let spec = ui.frame(root, Size::new(w, h)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        fold(&spec, &mut buf, &plain, rec);
        buf
    }

    fn run(root: Node<()>, w: u16, h: u16, rec: &mut Recorder) -> Buffer {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(root, Size::new(w, h)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        fold(&spec, &mut buf, &plain, rec);
        buf
    }

    fn row_text(buf: &Buffer, y: u16) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, y)].symbol().to_string())
            .collect()
    }

    /// A frame with one pane and no grid: what is left for the painter.
    fn one_pane() -> crate::view::shell::splits::Splits {
        use crate::model::event::BufferId;
        crate::view::shell::splits::Splits {
            root: crate::view::split::SplitNode::leaf(BufferId(1), fresh_core::SplitId(1)),
            maximized: None,
            active: None,
            chrome: Default::default(),
            controls: crate::view::shell::splits::PaneControls {
                maximize: false,
                close: false,
            },
            groups: Default::default(),
            interiors: Default::default(),
            strips: Default::default(),
            hover: None,
            drop_zone: None,
            hosts: Default::default(),
        }
    }

    /// The only cells a painter is handed are a pane's: every region of the
    /// frame is native, so the menu bar and the status bar are drawn from
    /// the tree, and the grid's pane reaches its painter in the rect layout
    /// computed for it — the body, between the two.
    #[test]
    fn only_the_panes_reach_the_painter() {
        let f = Frame {
            menu_bar: true,
            status_bar: true,
            splits: Some(one_pane()),
            ..Frame::default()
        };
        let mut rec = Recorder::default();
        let buf = run_msg(frame_tree(f), 10, 4, &mut rec);

        assert_eq!(
            rec.panes,
            vec![(
                crate::model::event::LeafId(fresh_core::SplitId(1)),
                Rect::new(0, 1, 10, 2)
            )],
            "one pane, at the body"
        );
        assert_eq!(row_text(&buf, 0), "          ", "the native bar, unthemed");
        assert_eq!(row_text(&buf, 1), "##########");
        assert_eq!(row_text(&buf, 2), "##########");
        assert_eq!(row_text(&buf, 3), "          ", "the native status bar");
    }

    /// **The ordering guarantee.** A chrome item that paints after a host lands
    /// on top of it — the popup-over-a-buffer case. If the fold collected hosts
    /// and painted them in a separate pass, this would come out inverted.
    #[test]
    fn chrome_painted_after_a_host_lands_on_top_of_it() {
        // A layer over the body: it resolves after the frame, so its items come
        // later in the display list.
        let pane = crate::view::shell::frame::pane_host_id(crate::model::event::LeafId(
            fresh_core::SplitId(1),
        ));
        let root: Node<()> = col().children([
            host(pane.0).flex(1),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .modality(Modality::None)
                .child(text("xx").h(Sizing::Cells(1))),
        ]);
        let mut rec = Recorder::default();
        let buf = run(root, 6, 3, &mut rec);

        let painted: String = (0..3).map(|y| row_text(&buf, y)).collect();
        assert!(
            painted.contains("xx"),
            "the layer's text must survive the host fill beneath it, got {painted:?}"
        );
        // And the host really did paint underneath.
        assert!(painted.contains('#'), "host content missing: {painted:?}");
    }

    /// A host is never handed a rect reaching outside the frame.
    #[test]
    fn a_host_rect_is_clipped_to_the_frame() {
        let f = Frame {
            menu_bar: true,
            status_bar: true,
            search_options: Some(Default::default()),
            prompt_line: true,
            splits: Some(one_pane()),
            ..Frame::default()
        };
        let mut rec = Recorder::default();
        let buf = run_msg(frame_tree(f), 8, 2, &mut rec);
        for (pane, rect) in &rec.panes {
            assert!(
                rect.x + rect.width <= buf.area.width && rect.y + rect.height <= buf.area.height,
                "{pane:?} got {rect:?}, outside {:?}",
                buf.area
            );
        }
    }

    /// `fold`'s signature must keep admitting a **mutable** host while the
    /// display list is borrowed from the `Ui` — that is what lets a host
    /// region take the `with_all_mut` split it needs to paint a buffer. A
    /// `fold` that took `&mut Ui`, or a `HostPainter` taking `&self`, would
    /// fail to compile here.
    ///
    /// It does not prove where the `Ui` is stored; that is enforced at the
    /// call site by the `expect` in `render`.
    #[test]
    fn fold_admits_a_mutable_host_while_the_spec_is_borrowed() {
        struct Host {
            painted: u32,
        }
        impl HostPainter for Host {
            fn paint_host(&mut self, _: HostTarget, _: Rect, _: &mut Buffer) {
                self.painted += 1; // real &mut access to host state
            }
        }

        let mut ui: Ui<crate::view::shell::msg::UiMsg> = Ui::new();
        let mut host_state = Host { painted: 0 };
        let mut buf = Buffer::empty(Rect::new(0, 0, 10, 4));

        // Two separate objects, borrowed at the same time. A pane is what
        // reaches a painter; no region does.
        let f = Frame {
            splits: Some(one_pane()),
            ..Frame::default()
        };
        let spec = ui.frame(frame_tree(f), Size::new(10, 4));
        fold(spec, &mut buf, &plain, &mut host_state);

        assert!(host_state.painted > 0);
    }
}

#[cfg(test)]
mod width_tests {
    //! Display-width painting (plan §2.1, Stage 0.1): the fold advances by
    //! what layout measured, and a wide glyph's continuation cell is blank.

    use super::*;
    use fresh_ui::{row, text, text_runs, Node, Run, Size, Sizing, Ui};

    fn fold_into(root: Node<()>, w: u16) -> Buffer {
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(root, Size::new(w, 1)).clone();
        // Pre-filled with a marker, so a cell the fold did not write is
        // visible as one — a continuation cell that was merely skipped
        // would still read `~`.
        let mut buf = Buffer::filled(Rect::new(0, 0, w, 1), ratatui::buffer::Cell::new("~"));
        struct NoHosts;
        impl HostPainter for NoHosts {
            fn paint_host(&mut self, _: HostTarget, _: Rect, _: &mut Buffer) {
                unreachable!("no hosts in these trees")
            }
        }
        fn plain(_: &ThemeKey) -> Style {
            Style::default()
        }
        fold(&spec, &mut buf, &plain, &mut NoHosts);
        buf
    }

    fn symbols(buf: &Buffer) -> Vec<&str> {
        (0..buf.area.width).map(|x| buf[(x, 0)].symbol()).collect()
    }

    /// `text("你好")` occupies exactly its four columns: the glyphs in the
    /// first and third, blanks — written, not skipped — in the second and
    /// fourth, and the sibling in the fifth.
    #[test]
    fn a_wide_glyph_takes_two_cells_and_blanks_its_continuation() {
        let buf = fold_into(row().children([text("你好"), text("!")]), 6);
        assert_eq!(symbols(&buf), ["你", " ", "好", " ", "!", "~"]);
    }

    /// The next run starts where the last one's glyphs end: a CJK identifier
    /// then a keyword, and the keyword is at column four.
    #[test]
    fn the_next_run_starts_after_the_wide_glyphs() {
        let buf = fold_into(
            text_runs([
                Run::themed("你好", "identifier"),
                Run::themed("fn", "keyword"),
            ]),
            8,
        );
        assert_eq!(symbols(&buf), ["你", " ", "好", " ", "f", "n", "~", "~"]);
    }

    /// A combining mark is painted into its base's cell and takes no column
    /// of its own; an emoji sequence is one two-cell symbol.
    #[test]
    fn marks_ride_in_their_base_cell_and_emoji_sequences_are_one_symbol() {
        let buf = fold_into(row().children([text("e\u{301}x"), text("|")]), 4);
        assert_eq!(symbols(&buf), ["e\u{301}", "x", "|", "~"]);

        let family = "👨\u{200d}👩\u{200d}👧";
        let buf = fold_into(row().children([text(family), text("|")]), 4);
        assert_eq!(symbols(&buf), [family, " ", "|", "~"]);
    }

    /// A wide glyph the clip would halve is a blank in its visible column,
    /// and its hidden column is never touched.
    #[test]
    fn a_wide_glyph_at_the_clip_edge_is_a_blank() {
        let buf = fold_into(
            row().children([text("你好").w(Sizing::Cells(3)), text("|")]),
            5,
        );
        assert_eq!(symbols(&buf), ["你", " ", " ", "|", "~"]);
    }
}

#[cfg(test)]
mod band_tests {
    use super::*;
    use crate::view::shell::context_menu::Menu;
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::menu::{DropdownLevel, DropdownRow};
    use crate::view::shell::msg::UiMsg;
    use fresh_ui::{Size, Ui};
    use ratatui::layout::Rect;
    use ratatui::style::Style;

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn spec_of(f: Frame, w: u16, h: u16) -> fresh_ui::LayoutSpec {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(frame_tree(f), Size::new(w, h)).clone()
    }

    fn a_menu() -> Menu {
        Menu {
            x: 2,
            y: 1,
            width: 10,
            highlighted: 0,
            items: vec!["Copy".into()],
        }
    }

    fn a_dropdown() -> DropdownLevel {
        DropdownLevel {
            from: 0,
            rows: vec![DropdownRow::plain(" New")],
        }
    }

    fn painted(spec: &fresh_ui::LayoutSpec, band: Band, w: u16, h: u16) -> Vec<String> {
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        fold_native(spec, &mut buf, &plain, band);
        (0..h)
            .map(|y| (0..w).map(|x| buf[(x, y)].symbol().to_string()).collect())
            .collect()
    }

    /// **The two bands partition the list.** Nothing is dropped and nothing is
    /// painted twice: every item belongs to exactly one side of the cut.
    #[test]
    fn the_cut_partitions_the_display_list() {
        let spec = spec_of(
            Frame {
                menu: Some(a_menu()),
                dropdowns: vec![a_dropdown()],
                ..Frame::default()
            },
            30,
            10,
        );
        assert!(
            !spec.in_flow().is_empty(),
            "the frame's regions are background"
        );
        assert!(!spec.layers().is_empty(), "its layers are overlay");
        assert_eq!(
            spec.in_flow().len() + spec.layers().len(),
            spec.items.len(),
            "and nothing is dropped or counted twice"
        );
    }

    /// Every item in the background band belongs to the frame itself, and the
    /// overlay band carries the menus' boxes — which is what makes
    /// "background" and "overlay" mean what they say.
    #[test]
    fn the_background_band_is_the_regions_and_the_overlay_band_is_the_layers() {
        let spec = spec_of(
            Frame {
                menu: Some(a_menu()),
                dropdowns: vec![a_dropdown()],
                ..Frame::default()
            },
            30,
            10,
        );
        for item in spec.in_flow() {
            assert!(
                !matches!(item.draw, Draw::Border(_) | Draw::Scrim(_)),
                "a layer's item landed in the background band: {:?}",
                item.draw
            );
        }
        assert!(
            spec.layers()
                .iter()
                .any(|i| matches!(i.draw, Draw::Border(_))),
            "the overlay band must carry the menus' boxes"
        );
    }

    /// With no overlay open the whole list is background, so the late pass has
    /// nothing to do — a frame that opens no menu must not pay for a second
    /// walk that finds items.
    #[test]
    fn a_frame_with_no_layers_is_all_background() {
        let spec = spec_of(Frame::default(), 30, 10);
        assert!(spec.layers().is_empty());
        assert!(painted(&spec, Band::Overlay, 30, 10)
            .iter()
            .all(|r| r.chars().all(|c| c == ' ')));
    }

    /// **An item declares how much room it has.** A run longer than the rect
    /// carrying it must not paint through whatever encloses it — a menu row
    /// through its own border, which is what a `Paragraph`'s silent
    /// truncation used to hide.
    #[test]
    fn a_run_longer_than_its_item_is_clipped_to_it() {
        use crate::view::shell::menu::{BarItem, DropdownLevel, DropdownRow, MenuBar};
        // A frame eight cells wide: the box is as wide as its row wants, and
        // the room it is placed in cuts it to six cells of content.
        let spec = spec_of(
            Frame {
                menu_bar_items: MenuBar {
                    items: vec![BarItem {
                        runs: vec![(" F ".into(), "ui.menu_fg/ui.menu_bg".into())],
                        index: 0,
                    }],
                },
                dropdowns: vec![DropdownLevel {
                    from: 0,
                    rows: vec![DropdownRow::plain("0123456789")],
                }],
                ..Frame::default()
            },
            8,
            6,
        );
        let rows = painted(&spec, Band::Overlay, 8, 6);
        let painted_box: String = rows[2].chars().take(8).collect();
        assert_eq!(
            painted_box, "\u{2502}012345\u{2502}",
            "the border survives the row: {rows:?}"
        );
    }

    /// **The point of the split.** A layer paints in the overlay band and
    /// nothing else does, so a legacy painter running between the two passes
    /// lands under it — which is what lets a background region migrate while
    /// the popups over it have not.
    #[test]
    fn a_layer_paints_only_in_the_overlay_band() {
        let spec = spec_of(
            Frame {
                menu: Some(a_menu()),
                ..Frame::default()
            },
            30,
            10,
        );
        let bg = painted(&spec, Band::Background, 30, 10);
        assert!(
            bg.iter().all(|r| r.chars().all(|c| c == ' ')),
            "regions are hosts, so the background band paints nothing yet: {bg:?}"
        );
        let over = painted(&spec, Band::Overlay, 30, 10);
        assert!(
            over.iter().any(|r| r.contains('\u{250c}')),
            "the menu's box belongs to the overlay band: {over:?}"
        );
    }

    /// **Every layer the frame declares reaches the overlay band**, whether or
    /// not it is keyed.
    ///
    /// This used to be a check that each layer named a family in a
    /// hand-maintained list, which could only ever confirm what the list
    /// already said. The split is the library's now, so what is worth pinning
    /// is the property itself: three layers, three boxes above the fold.
    #[test]
    fn every_declared_layer_reaches_the_overlay_band() {
        let spec = spec_of(
            Frame {
                menu: Some(a_menu()),
                dropdowns: vec![a_dropdown(), a_dropdown()],
                ..Frame::default()
            },
            30,
            10,
        );
        // Two dropdown levels and the context menu, each a bordered box.
        let boxes = spec
            .layers()
            .iter()
            .filter(|i| matches!(i.draw, Draw::Border(_)))
            .count();
        assert_eq!(boxes, 3, "every declared layer paints above the fold");
        assert!(
            spec.in_flow()
                .iter()
                .all(|i| !matches!(i.draw, Draw::Border(_))),
            "and none of them below it"
        );
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
        let mut ui: Ui<crate::view::shell::msg::UiMsg> = Ui::new();
        let spec = ui
            .frame(frame_tree(Frame::default()), Size::new(20, 6))
            .clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 6));
        let before = buf.clone();
        fold_native(&spec, &mut buf, &plain, Band::Background);
        fold_native(&spec, &mut buf, &plain, Band::Overlay);
        assert_eq!(buf, before, "host regions must be left to their painters");
    }
}
