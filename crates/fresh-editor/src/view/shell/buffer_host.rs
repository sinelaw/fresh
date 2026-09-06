//! A pane's content as a host leaf: the text buffer's place in the tree
//! (design §3.7.1–§3.7.4).
//!
//! The buffer is the one thing on screen the tree must not try to own — a
//! document is a piece tree with a wrap index that repairs on edit and a
//! highlighter that converges, none of it tree-shaped — so the pane's
//! content is a [`HostLeaf`]: an ordinary render object with exactly the
//! capabilities a built-in primitive has. It takes its rectangle from
//! layout, paints one `Draw::Host` item the fold resolves to the text
//! pipeline, claims the pointer over its whole rectangle, and answers the
//! byte under a cell.
//!
//! # One object per pane, for the life of the pane
//!
//! A `HostSpec::Leaf` is equal to another when it holds the same factory, so
//! the factory is kept by the window in a [`PaneHandle`] and handed to the
//! description every frame; the object is created once per mount and never
//! replaced while the pane exists. What it needs to answer the pointer — the
//! rows the last text pass drew, and where — is the [`PaneView`] the handle
//! shares with it, settled by the painter after each frame. The map used to
//! be `WindowLayoutCache::view_line_mappings`, a side table the mouse path
//! read after finding the pane by scanning rectangles; it lives in the
//! pane's own view now, read by the leaf's `text_byte_at` and by the
//! editor's keyboard motion alike.

use std::cell::{Ref, RefCell};
use std::rc::Rc;

use fresh_ui::{
    Axis, Constraints, Draw, DrawList, Geom, Hit, HostLeaf, HostObject, LayoutCx, Mark, Node,
    Point, RenderObject, Size, ThemeKey,
};
use ratatui::layout::Rect;
use ratatui::style::Color;

use crate::app::click_geometry::{screen_to_buffer_position_with_overshoot, ClickTarget};
use crate::app::shell_host::shell_theme::{literal, pair};
use crate::app::types::ViewLineMapping;
use crate::model::event::LeafId;
use crate::view::scrollbar_marker::{bucket, RowMark};

/// What a pane's last text pass settled: where its content was drawn and
/// which bytes each visual row shows.
///
/// `rows` is one entry per visual row on screen — a soft-wrapped source line
/// is several — which is what makes a cell index them. The rest is what the
/// projection from a cell to a byte needs beside the rows: the gutter the
/// rows sit right of, the compose-mode centering, and the byte a cell maps
/// to when there are no rows to ask.
#[derive(Debug, Clone, Default)]
pub struct PaneView {
    /// The content rectangle the rows were drawn into, in screen cells.
    pub rect: Rect,
    /// Columns of gutter (line numbers, folds, diagnostics) left of the text.
    pub gutter_width: u16,
    /// The compose-mode paper width, when the buffer is centered in its pane.
    pub compose_width: Option<u16>,
    /// The viewport's top byte: where a cell resolves when there are no rows.
    pub top_byte: usize,
    /// The visual rows, top to bottom.
    pub rows: Vec<ViewLineMapping>,
    /// The caret's cell, local to `rect`, when this pane has one: the pane
    /// is the active one and its buffer shows cursors. The one derivation of
    /// it is the text pass's (`orchestration::caret_cell`); the popup
    /// anchored to the caret reads it here whether or not it is shown.
    pub caret: Option<(u16, u16)>,
    /// Whether the leaf places the display list's cursor at `caret`: false
    /// while another surface owns the keyboard or a modal is up, when the
    /// caret still has a cell (a popup anchors to it) but is not on screen.
    pub caret_shown: bool,
}

impl PaneView {
    /// The buffer position under a cell of the pane's content, with how far
    /// past the drawn content the cell is.
    ///
    /// **Local cells, not screen cells.** The rows were drawn into `rect`, and
    /// a cell is named relative to that rectangle's origin — which is what
    /// the leaf is asked in (`RenderObject::text_byte_at`), and what a reader
    /// that has a screen cell converts to through the rectangle the tree
    /// gives the pane *now*. Between a layout and the paint that follows it
    /// the two rectangles can differ (a pane just split), and resolving
    /// against the settled origin would answer a different byte from the
    /// leaf's for the same press.
    ///
    /// `allow_gutter` says whether a cell in the gutter resolves to its row's
    /// start (a press does) or to nothing (a hover does); `fallback` is the
    /// position a pane with no rows answers.
    pub(crate) fn resolve(
        &self,
        col: u16,
        row: u16,
        allow_gutter: bool,
        fallback: usize,
    ) -> Option<ClickTarget> {
        let rows = Some(&self.rows[..]);
        screen_to_buffer_position_with_overshoot(
            col,
            row,
            Rect::new(0, 0, self.rect.width, self.rect.height),
            self.gutter_width,
            rows,
            fallback,
            allow_gutter,
            self.compose_width,
        )
    }

    /// Where a press at a content cell lands: a gutter cell is its row's
    /// start.
    pub(crate) fn click_target(&self, col: u16, row: u16) -> Option<ClickTarget> {
        self.resolve(col, row, true, self.top_byte)
    }

    /// The byte a press at a content cell places the caret at.
    pub fn byte_at(&self, col: u16, row: u16) -> Option<usize> {
        self.click_target(col, row).map(|t| t.position)
    }

    /// Which visual row shows `byte_pos`.
    pub fn find_visual_row(&self, byte_pos: usize) -> Option<usize> {
        if let Some(idx) = self.rows.iter().position(|m| m.contains_byte(byte_pos)) {
            return Some(idx);
        }
        // No row drew this byte. It can still be the position just past some
        // row's last character — a compose-mode soft break consumes the space
        // it fell on, so that position is carried by no cell even though the
        // row owns it (see `ViewLineMapping::end_exclusive`). Asked only after
        // the rows that do draw the byte have had their say, so a row never
        // takes a byte the row below actually starts with: that is the
        // ordinary wrapped row, where the next row draws the byte and `Down`
        // steps onto it.
        self.rows
            .iter()
            .position(|m| m.end_exclusive == Some(byte_pos))
    }

    /// The visual column of `byte_pos` within its row.
    pub fn byte_to_visual_column(&self, byte_pos: usize) -> Option<usize> {
        let row = self.rows.get(self.find_visual_row(byte_pos)?)?;
        for (visual_col, &char_idx) in row.visual_to_char.iter().enumerate() {
            if let Some(source_byte) = row.char_source_bytes.get(char_idx).and_then(|b| *b) {
                if source_byte == byte_pos {
                    return Some(visual_col);
                }
                // Past the byte: the previous column is the one.
                if source_byte > byte_pos {
                    return Some(visual_col.saturating_sub(1));
                }
            }
        }
        // At or past the end of the row: the column just after the last
        // *source-backed* cell. Trailing cells that map to no source byte are
        // purely visual (indentation guides synthesised on a blank line
        // inside an indented block); counting them would push the column
        // right by one per guide, so a Down onto the next line would land one
        // column too far (issue #2564).
        let last_real_col = row
            .visual_to_char
            .iter()
            .enumerate()
            .rev()
            .find(|(_, &char_idx)| {
                row.char_source_bytes
                    .get(char_idx)
                    .is_some_and(|b| b.is_some())
            })
            .map(|(visual_col, _)| visual_col + 1)
            .unwrap_or(0);
        Some(last_real_col)
    }

    /// The position one visual row up (`-1`) or down (`1`) from
    /// `current_pos`, keeping `goal_visual_col`, and the goal column kept.
    /// `None` at the edge of the rows on screen.
    pub fn move_visual_line(
        &self,
        current_pos: usize,
        goal_visual_col: usize,
        direction: i8,
    ) -> Option<(usize, usize)> {
        let rows = &self.rows;
        let current_row = self.find_visual_row(current_pos)?;

        // Walk past purely-virtual rows (markdown_compose table borders and
        // separators, live-diff deletion lines). They are plugin-injected and
        // their `line_end_byte` is inherited from the adjacent content row,
        // so stopping on one would land the cursor on a byte that is already
        // the row above's end — and a Down after a table would teleport back.
        //
        // A row is navigable iff at least one of its visual columns maps to
        // a real source byte.
        let mut target_row = current_row;
        let navigable = |idx: usize| -> bool {
            rows.get(idx)
                .map(|m| m.char_source_bytes.iter().any(|b| b.is_some()))
                .unwrap_or(false)
        };
        loop {
            target_row = if direction < 0 {
                target_row.checked_sub(1)?
            } else {
                let next = target_row + 1;
                if next >= rows.len() {
                    return None;
                }
                next
            };
            // Either the next row has real source content, or it is a
            // non-source row the rest of the editor already treats as a
            // cursor stop (the trailing empty line at EOF, an empty source
            // line between paragraphs).
            if navigable(target_row) {
                break;
            }
            if rows.get(target_row)?.is_plugin_virtual {
                continue;
            }
            break;
        }

        let target = rows.get(target_row)?;
        // The byte at the goal column; past the end of the row's content,
        // the row's end; a column with no source byte (padding on a wrapped
        // continuation), the nearest column that has one.
        let new_pos = if goal_visual_col >= target.visual_to_char.len() {
            target.line_end_byte
        } else {
            target
                .source_byte_at_visual_col(goal_visual_col)
                .or_else(|| target.nearest_source_byte(goal_visual_col))
                .unwrap_or(target.line_end_byte)
        };
        Some((new_pos, goal_visual_col))
    }

    /// The start of the visual row showing `byte_pos`; with `allow_advance`,
    /// a cursor already at that start moves to the previous row's start.
    pub fn visual_line_start(&self, byte_pos: usize, allow_advance: bool) -> Option<usize> {
        let row_idx = self.find_visual_row(byte_pos)?;
        let row = self.rows.get(row_idx)?;
        let row_start = row.first_source_byte()?;
        if allow_advance && byte_pos == row_start && row_idx > 0 {
            self.rows.get(row_idx - 1)?.first_source_byte()
        } else {
            Some(row_start)
        }
    }

    /// The end of the visual row showing `byte_pos`; with `allow_advance`, a
    /// cursor already at that end moves to the next row's end.
    pub fn visual_line_end(&self, byte_pos: usize, allow_advance: bool) -> Option<usize> {
        let row_idx = self.find_visual_row(byte_pos)?;
        let row = self.rows.get(row_idx)?;
        if allow_advance && byte_pos == row.line_end_byte && row_idx + 1 < self.rows.len() {
            Some(self.rows.get(row_idx + 1)?.line_end_byte)
        } else {
            Some(row.line_end_byte)
        }
    }
}

/// The leaf: the pane's content in the render tree.
///
/// **Layout touches no document byte.** A pane has no intrinsic size — a
/// document has no natural height — so the leaf is as big as the grid gives
/// it, and nothing below it is measured. `paint` is one item; the fold hands
/// its rectangle to the text pipeline, which materialises exactly the window
/// that rectangle shows. The pointer is answered from the view the last pass
/// settled.
pub struct BufferHost {
    leaf: LeafId,
    view: Rc<RefCell<PaneView>>,
    /// Whether the pane's buffer is a live terminal taking the keyboard
    /// raw — settled by the editor per frame (`PaneHandle::set_raw_input`),
    /// read by the library when it links the leaf.
    raw: Rc<std::cell::Cell<bool>>,
}

impl RenderObject for BufferHost {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        c.constrain(c.max())
    }

    /// **The PTY is this leaf.** A live terminal's pane takes raw input, and
    /// the tree derives whether it is reachable this frame — no exclusive
    /// layer above it — as `Ui::raw_input`, which is the editor's PTY gate
    /// (design §3.7.8).
    fn takes_raw_input(&self) -> bool {
        self.raw.get()
    }

    fn relayout_boundary(&self) -> bool {
        true
    }

    /// One `Draw::Host` for the fold to hand the text pass, and the display
    /// list's cursor at the caret the pass settled — so the frame's caret has
    /// one source and reaches the terminal the way every native field's
    /// does, through `LayoutSpec::cursor` (design §3.7.3).
    fn paint(&self, g: Geom, out: &mut DrawList) {
        out.push(Draw::Host(super::frame::pane_host_id(self.leaf)), g);
        let view = self.view.borrow();
        if let Some((x, y)) = view.caret.filter(|_| view.caret_shown) {
            let at = Point::new(g.rect.x + i32::from(x), g.rect.y + i32::from(y));
            if g.clip.contains(at) {
                out.set_cursor(at);
            }
        }
    }

    fn hit(&self, _local: Point) -> Hit {
        Hit::Opaque
    }

    /// The byte under a cell of the leaf's own rectangle.
    ///
    /// Answered from the rows the last text pass drew, in the leaf's own
    /// cells. A cell in the gutter answers its row's start, as a press there
    /// places the caret.
    fn text_byte_at(&self, local: Point) -> Option<usize> {
        let col = u16::try_from(local.x).ok()?;
        let row = u16::try_from(local.y).ok()?;
        self.view.borrow().byte_at(col, row)
    }

    fn render_name(&self) -> &'static str {
        "BufferHost"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl HostLeaf for BufferHost {}

/// A pane's handle on its leaf: the factory the description mounts it by,
/// and the view the leaf and the editor share.
///
/// Kept by the window for as long as the pane exists, so every frame's
/// description names the same factory and the leaf is never replaced.
#[derive(Clone)]
pub struct PaneHandle {
    leaf: LeafId,
    view: Rc<RefCell<PaneView>>,
    factory: HostObject,
    /// See `BufferHost::raw`.
    raw: Rc<std::cell::Cell<bool>>,
    /// The pane's keyboard context — `Terminal` while its live terminal takes
    /// the keyboard, `CompositeBuffer` for a composite, else the editor's
    /// plain one — settled by the editor per frame (`PaneHandle::set_context`)
    /// and read off the focus chain by `Editor::get_key_context` when the
    /// chain names this pane's content. A settled fact of the leaf, like its
    /// raw input, rather than a node above it: the leaf's element is the same
    /// whatever mode it is in, so a capture or a focus it holds survives the
    /// mode changing under a gesture (a drag that parks a live terminal in
    /// scroll-back).
    context: Rc<RefCell<crate::input::keybindings::KeyContext>>,
    /// The vertical bar's facts and its leaf's factory, and the horizontal
    /// bar's. Settled by the editor before the frame is described
    /// (`Editor::settle_pane_bars`); painted by the bar leaves beside the
    /// content leaf (design §3.7.6).
    vbar: Rc<RefCell<Option<BarFacts>>>,
    hbar: Rc<RefCell<Option<BarFacts>>>,
    vbar_factory: HostObject,
    hbar_factory: HostObject,
}

impl std::fmt::Debug for PaneHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PaneHandle")
            .field("leaf", &self.leaf)
            .field("view", &self.view.borrow())
            .finish()
    }
}

impl PaneHandle {
    pub fn new(leaf: LeafId) -> PaneHandle {
        let view = Rc::new(RefCell::new(PaneView::default()));
        let raw: Rc<std::cell::Cell<bool>> = Rc::default();
        let (shared, shared_raw) = (view.clone(), raw.clone());
        let factory: HostObject = Rc::new(move || {
            Box::new(BufferHost {
                leaf,
                view: shared.clone(),
                raw: shared_raw.clone(),
            })
        });
        let vbar: Rc<RefCell<Option<BarFacts>>> = Rc::default();
        let hbar: Rc<RefCell<Option<BarFacts>>> = Rc::default();
        let (v, h) = (vbar.clone(), hbar.clone());
        let vbar_factory: HostObject =
            Rc::new(move || Box::new(BarHost::new(Axis::Vertical, v.clone())));
        let hbar_factory: HostObject =
            Rc::new(move || Box::new(BarHost::new(Axis::Horizontal, h.clone())));
        PaneHandle {
            leaf,
            view,
            factory,
            raw,
            context: Rc::new(RefCell::new(crate::input::keybindings::KeyContext::Normal)),
            vbar,
            hbar,
            vbar_factory,
            hbar_factory,
        }
    }

    /// One of the pane's scrollbars, for the interior's bar slot.
    pub fn bar_node<M>(&self, axis: Axis) -> Node<M> {
        let f = match axis {
            Axis::Vertical => &self.vbar_factory,
            Axis::Horizontal => &self.hbar_factory,
        };
        fresh_ui::host_object(f.clone())
    }

    /// What one of the bars shows this frame, as settled.
    pub fn bar(&self, axis: Axis) -> Option<BarFacts> {
        match axis {
            Axis::Vertical => self.vbar.borrow().clone(),
            Axis::Horizontal => self.hbar.borrow().clone(),
        }
    }

    /// Record what the pane's bars show this frame. `None` for a bar the
    /// pane does not have — its slot has no width, and the leaf paints
    /// nothing.
    pub fn settle_bars(&self, vertical: Option<BarFacts>, horizontal: Option<BarFacts>) {
        *self.vbar.borrow_mut() = vertical;
        *self.hbar.borrow_mut() = horizontal;
    }

    pub fn leaf(&self) -> LeafId {
        self.leaf
    }

    /// The leaf, for the pane's content slot.
    pub fn node<M>(&self) -> Node<M> {
        fresh_ui::host_object(self.factory.clone())
    }

    /// What the last text pass settled.
    pub fn view(&self) -> Ref<'_, PaneView> {
        self.view.borrow()
    }

    /// Record what a text pass drew: the rows, at the content rectangle they
    /// were drawn into, with the projection's other inputs as they stood.
    #[allow(clippy::too_many_arguments)]
    pub fn settle(
        &self,
        rect: Rect,
        rows: Vec<ViewLineMapping>,
        gutter_width: u16,
        compose_width: Option<u16>,
        top_byte: usize,
        caret: Option<(u16, u16)>,
        caret_shown: bool,
    ) {
        *self.view.borrow_mut() = PaneView {
            rect,
            gutter_width,
            compose_width,
            top_byte,
            rows,
            caret,
            caret_shown,
        };
    }

    /// Forget the rows: what they showed is no longer on screen (a wrap
    /// setting changed under them) and the next frame draws new ones.
    pub fn clear_rows(&self) {
        self.view.borrow_mut().rows.clear();
    }

    /// Say whether the pane's buffer is a live terminal taking the keyboard
    /// raw — the leaf answers `takes_raw_input` from it.
    pub fn set_raw_input(&self, on: bool) {
        self.raw.set(on);
    }

    /// Say which keyboard context the pane's content resolves keys in.
    pub fn set_context(&self, context: crate::input::keybindings::KeyContext) {
        *self.context.borrow_mut() = context;
    }

    /// The keyboard context the pane's content resolves keys in.
    pub fn context(&self) -> crate::input::keybindings::KeyContext {
        self.context.borrow().clone()
    }

    /// Forget the caret: the pass that would have placed it did not run for
    /// this pane this frame (a panel took its content, a maximized sibling
    /// hides it), so it has none — not the cell it had when it last did.
    pub fn clear_caret(&self) {
        let mut v = self.view.borrow_mut();
        v.caret = None;
        v.caret_shown = false;
    }
}

/// The window a bar's thumb is sized by, in the bar's own unit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BarWindow {
    /// The track's own length: one row per line, which is a pane's vertical
    /// bar.
    Track,
    /// The track less some cells — a composite view keeps a header row out
    /// of its window.
    TrackLess(u16),
    /// A stated extent: the horizontal bar's visible columns.
    Cells(u32),
    /// A file too large to count lines on: the thumb is one cell, placed by
    /// the offset's fraction of the content.
    OneCell,
}

/// What a pane's scrollbar shows: the facts the editor settles before the
/// frame, in the unit the pane scrolls in (design §3.7.6).
///
/// The editor owns the pane's scroll, so the bar publishes rather than
/// scrolls: offset and content in rows (or bytes, on a large file), the
/// window as a rule the leaf applies to the track layout gives it, and the
/// marks resolved to rows for the leaf to bucket onto that track (L8).
#[derive(Clone, Debug, PartialEq)]
pub struct BarFacts {
    pub offset: u32,
    pub content: u32,
    pub window: BarWindow,
    /// The marks, resolved to rows of `total`; bucketed onto the track by
    /// the leaf, so a resize never shows a mark projected for another
    /// height.
    pub marks: Rc<[RowMark]>,
    pub total: u64,
    /// Marker colours the theme resolved, by the mark's index — a literal
    /// colour is what a theme name cannot say, so it is named as one.
    pub colors: Rc<[Color]>,
    /// A track cell lit under the pointer: painted whole in the track's
    /// hover colour.
    pub hover_cell: Option<u16>,
}

impl BarFacts {
    /// The bar as `Draw::Scrollbar` states it on a `track`-long bar:
    /// `(offset, content, window)`, the window rule applied. What the leaf
    /// paints and what a press reads the thumb from, so the two are one
    /// arithmetic (`Draw::scrollbar_thumb` over these three).
    pub fn on_track(&self, track: u16) -> (u32, u32, u32) {
        let track = track.max(1);
        match self.window {
            BarWindow::Track => (self.offset, self.content, u32::from(track)),
            BarWindow::TrackLess(n) => (
                self.offset,
                self.content,
                u32::from(track.saturating_sub(n).max(1)),
            ),
            BarWindow::Cells(n) => (self.offset, self.content, n),
            // The thumb is one cell at the offset's fraction of the content:
            // said in track cells, so the numbers stay small.
            BarWindow::OneCell => {
                let row = (u64::from(self.offset) * u64::from(track)
                    / u64::from(self.content.max(1)))
                .min(u64::from(track.saturating_sub(1))) as u32;
                (row, u32::from(track), 1)
            }
        }
    }

    /// The thumb on a `track`-long bar, as `(start, end)` cells.
    pub fn thumb(&self, track: u16) -> (u16, u16) {
        let (offset, content, window) = self.on_track(track);
        let (top, len) = Draw::scrollbar_thumb(
            offset,
            content,
            window.min(u32::from(u16::MAX)),
            track.max(1),
        );
        (top, top.saturating_add(len))
    }

    /// A bar with nothing to mark.
    pub fn plain(offset: u32, content: u32, window: BarWindow) -> BarFacts {
        BarFacts {
            offset,
            content,
            window,
            marks: Rc::from(Vec::new()),
            total: 1,
            colors: Rc::from(Vec::new()),
            hover_cell: None,
        }
    }
}

/// The theme name a track cell under the pointer is lit in.
pub fn track_hover_theme() -> String {
    pair("ui.scrollbar_track_hover_fg", "ui.scrollbar_track_hover_fg")
}

/// The leaf: one of a pane's scrollbars, an ordinary `Draw::Scrollbar` item
/// beside the content leaf, drawn from the facts the pane settled.
///
/// **The track is layout's.** The leaf is as long as its slot and one cell
/// thick; the window it sizes the thumb by, and the cells it buckets the
/// marks onto, are read off the rectangle paint hands it — so a bar is never
/// drawn for a height the pane no longer has.
pub struct BarHost {
    axis: Axis,
    facts: Rc<RefCell<Option<BarFacts>>>,
}

impl BarHost {
    fn new(axis: Axis, facts: Rc<RefCell<Option<BarFacts>>>) -> BarHost {
        BarHost { axis, facts }
    }

    /// The marks on a `track`-long bar, bucketed from the facts' rows, plus
    /// the hovered cell.
    ///
    /// **Bucketed on every paint, not memoized.** The half of a projection
    /// that costs anything — resolving marks to rows against the buffer —
    /// is done once per change on the editor's side (`resolve_scrollbar_marks`);
    /// what is left here is arithmetic over the rows and a walk of the
    /// track, cheaper than the paint that asks for it. A memo of it keyed
    /// on the rows' address, the track and the hover was one more thing to
    /// invalidate — and it missed the theme's colours, which the facts carry
    /// fresh every frame, and could serve a freed allocation's bucketing to
    /// the mark set that reused its address.
    fn marks(&self, f: &BarFacts, track: u16) -> Rc<[Mark]> {
        let cells = bucket(&f.marks, f.total, usize::from(track));
        let mut out: Vec<Mark> = cells
            .iter()
            .enumerate()
            .filter_map(|(at, cell)| {
                let cell = cell.as_ref()?;
                // Which mark won the cell is known by its colour and
                // priority; the resolved colour is looked up by the mark
                // that carries that spec.
                let color = f
                    .marks
                    .iter()
                    .position(|m| m.color == cell.color && m.priority == cell.priority)
                    .and_then(|i| f.colors.get(i).copied())?;
                Some(Mark {
                    at: at as u16,
                    theme: ThemeKey(Some(pair(&literal(color), &literal(color)).into())),
                    full: false,
                })
            })
            .collect();
        if let Some(h) = f.hover_cell.filter(|h| *h < track) {
            out.retain(|m| m.at != h);
            out.push(Mark {
                at: h,
                theme: ThemeKey(Some(track_hover_theme().into())),
                full: true,
            });
        }
        Rc::from(out)
    }
}

impl RenderObject for BarHost {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        c.constrain(c.max())
    }

    fn relayout_boundary(&self) -> bool {
        true
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        let facts = self.facts.borrow();
        let Some(f) = facts.as_ref() else {
            return;
        };
        let track = match self.axis {
            Axis::Vertical => g.rect.h,
            Axis::Horizontal => g.rect.w,
        }
        .max(1);
        let (offset, content, window) = f.on_track(track);
        out.push(
            Draw::Scrollbar {
                offset,
                content,
                window: window.min(u32::from(u16::MAX)) as u16,
                axis: self.axis,
                marks: self.marks(f, track),
            },
            g,
        );
    }

    fn hit(&self, _local: Point) -> Hit {
        Hit::Opaque
    }

    fn render_name(&self) -> &'static str {
        "BarHost"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl HostLeaf for BarHost {}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::SplitId;
    use fresh_ui::{col, Ui};

    fn leaf() -> LeafId {
        LeafId(SplitId(3))
    }

    /// A row of `text` starting at `start`, every character a source byte.
    fn row(start: usize, text: &str) -> ViewLineMapping {
        let n = text.len();
        ViewLineMapping {
            char_source_bytes: (0..n).map(|i| Some(start + i)).collect(),
            visual_to_char: (0..n).collect(),
            line_end_byte: start + n,
            is_plugin_virtual: false,
            end_exclusive: None,
        }
    }

    fn settled(h: &PaneHandle) {
        h.settle(
            Rect::new(10, 2, 40, 5),
            vec![row(0, "alpha"), row(6, "bravo"), row(12, "charlie")],
            3,
            None,
            0,
            None,
            false,
        );
    }

    /// **The object is made once per mount and never replaced.** The same
    /// handle described on every frame is the same factory, so the render
    /// tree keeps the object it has — the equality the library defines for a
    /// leaf spec is the factory's identity.
    #[test]
    fn one_handle_is_one_leaf_across_frames() {
        let h = PaneHandle::new(leaf());
        let mut ui: Ui<()> = Ui::new();
        let describe = |h: &PaneHandle| col().child(h.node::<()>().h(fresh_ui::Sizing::Cells(5)));
        ui.frame(describe(&h), Size::new(40, 10));
        let first = ui.dump();
        ui.frame(describe(&h), Size::new(40, 10));
        let second = ui.dump();
        // The dump names each element and why it was last touched: the
        // second frame's leaf is the first frame's element, and nothing on
        // it says `mount`.
        let elements = |d: &str| -> Vec<String> {
            d.lines()
                .map(|l| l.split("cause=").next().unwrap_or(l).trim_end().to_string())
                .collect()
        };
        assert_eq!(
            elements(&first),
            elements(&second),
            "the same elements, at the same places"
        );
        let host = second
            .lines()
            .find(|l| l.contains("Host"))
            .expect("the leaf is a host");
        assert!(
            !host.contains("cause=mount"),
            "the second frame does not mount the leaf again:\n{second}"
        );
    }

    /// One item per pane, whatever is behind it: the leaf paints a `Host`
    /// and nothing else.
    #[test]
    fn a_pane_is_one_item() {
        let h = PaneHandle::new(leaf());
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(h.node::<()>(), Size::new(40, 10));
        let hosts: Vec<_> = spec
            .items
            .iter()
            .filter(|i| matches!(i.draw, Draw::Host(id) if id == super::super::frame::pane_host_id(leaf())))
            .collect();
        assert_eq!(hosts.len(), 1, "one host item, keyed by the pane");
        assert_eq!(spec.items.len(), 1, "and nothing else");
    }

    /// The byte under a cell is the settled row's, in the leaf's own cells:
    /// a cell in the gutter is its row's start, a cell past a row's text is
    /// the row's end.
    #[test]
    fn the_leaf_answers_the_byte_under_a_cell() {
        let h = PaneHandle::new(leaf());
        settled(&h);
        let obj = (h.factory)();
        // Row 1 ("bravo", bytes 6..11), column 2 of the text: the gutter is
        // three wide, so text column 2 is local x 5. Where the rows were
        // drawn on screen does not enter into it.
        assert_eq!(obj.text_byte_at(Point::new(5, 1)), Some(8));
        // In the gutter: the row's start.
        assert_eq!(obj.text_byte_at(Point::new(1, 1)), Some(6));
        // Past the row's text: its end.
        assert_eq!(obj.text_byte_at(Point::new(30, 0)), Some(5));
        // Below the last row: the last row, at the column.
        assert_eq!(obj.text_byte_at(Point::new(4, 4)), Some(13));
    }

    /// **The caret has one source.** The leaf places the display list's
    /// cursor at the caret its text pass settled, in the cells layout gave
    /// it; a pane whose pass settled none — an inactive pane, a hidden
    /// caret — places nothing, and the display list carries no cursor.
    #[test]
    fn the_leaf_places_the_cursor_its_text_pass_settled() {
        let h = PaneHandle::new(leaf());
        let cursor_of = |h: &PaneHandle| {
            let mut ui: Ui<()> = Ui::new();
            ui.frame(
                col().children([
                    fresh_ui::text("strip").h(fresh_ui::Sizing::Cells(1)),
                    h.node(),
                ]),
                fresh_ui::Size::new(40, 10),
            );
            ui.spec().cursor.map(|c| (c.pos.x, c.pos.y))
        };
        assert_eq!(cursor_of(&h), None, "nothing settled, nothing placed");

        h.settle(
            Rect::new(0, 1, 40, 9),
            vec![row(0, "alpha")],
            0,
            None,
            0,
            Some((3, 2)),
            true,
        );
        assert_eq!(
            cursor_of(&h),
            Some((3, 3)),
            "the settled cell, from the leaf's own origin"
        );

        h.settle(
            Rect::new(0, 1, 40, 9),
            vec![row(0, "alpha")],
            0,
            None,
            0,
            Some((3, 2)),
            false,
        );
        assert_eq!(
            cursor_of(&h),
            None,
            "a caret the pane does not show — the keyboard is elsewhere — is a \
             cell a popup anchors to, not a cursor"
        );
    }

    /// **A pane the pass did not settle shows no caret.** Its rows stay —
    /// a hidden sibling of a maximized pane comes back with them — but the
    /// caret is this frame's answer or nobody's.
    #[test]
    fn a_cleared_caret_is_neither_placed_nor_anchored_to() {
        let h = PaneHandle::new(leaf());
        h.settle(
            Rect::new(0, 1, 40, 9),
            vec![row(0, "alpha")],
            0,
            None,
            0,
            Some((3, 2)),
            true,
        );
        h.clear_caret();
        let v = h.view();
        assert_eq!((v.caret, v.caret_shown), (None, false));
        assert_eq!(v.rows.len(), 1, "the rows are kept");
    }

    /// With nothing settled the leaf answers the viewport's top, which is
    /// where a press on an empty pane puts the caret.
    #[test]
    fn an_unsettled_pane_answers_its_top() {
        let h = PaneHandle::new(leaf());
        h.settle(Rect::new(0, 0, 10, 3), Vec::new(), 0, None, 42, None, false);
        let obj = (h.factory)();
        assert_eq!(obj.text_byte_at(Point::new(4, 2)), Some(42));
    }

    /// **A bar is one `Draw::Scrollbar`, sized to the track it is given.**
    /// The facts say offset and content in rows and the window as a rule;
    /// the leaf applies the rule to its own height, so the thumb a press
    /// reads (`BarFacts::thumb`) is the thumb the item states.
    #[test]
    fn a_bar_states_its_thumb_on_the_track_it_is_given() {
        use crate::view::scrollbar_marker::RowMark;
        let h = PaneHandle::new(leaf());
        // 100 rows, scrolled 40 down, with a mark at row 50 and a lit track
        // cell at 2.
        let marks: Rc<[RowMark]> = Rc::from(vec![RowMark::test(50, None, 1)]);
        h.settle_bars(
            Some(BarFacts {
                offset: 40,
                content: 100,
                window: BarWindow::Track,
                marks: marks.clone(),
                total: 100,
                colors: Rc::from(vec![Color::Rgb(1, 2, 3)]),
                hover_cell: Some(2),
            }),
            None,
        );
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(
            h.bar_node::<()>(Axis::Vertical)
                .w(fresh_ui::Sizing::Cells(1))
                .h(fresh_ui::Sizing::Cells(10)),
            Size::new(1, 10),
        );
        let bar = spec
            .items
            .iter()
            .find_map(|i| match &i.draw {
                Draw::Scrollbar {
                    offset,
                    content,
                    window,
                    axis,
                    marks,
                } => Some((*offset, *content, *window, *axis, marks.clone())),
                _ => None,
            })
            .expect("the bar's item");
        assert_eq!((bar.0, bar.1, bar.2, bar.3), (40, 100, 10, Axis::Vertical));
        // The thumb the item states is the one the facts answer for a
        // ten-cell track.
        let (top, len) = Draw::scrollbar_thumb(bar.0, bar.1, u32::from(bar.2), 10);
        let facts = h.bar(Axis::Vertical).expect("settled");
        assert_eq!(facts.thumb(10), (top, top + len));
        // Row 50 of 100 lands at cell 5 as a half block in the mark's
        // literal colour; the hovered cell is a full mark.
        let at: Vec<(u16, bool)> = bar.4.iter().map(|m| (m.at, m.full)).collect();
        assert_eq!(at, vec![(5, false), (2, true)]);
        assert!(
            bar.4[0].theme.as_str().contains("#010203"),
            "{}",
            bar.4[0].theme.as_str()
        );
    }

    /// A pane with no bar settled paints no bar: the slot has no width, and
    /// the leaf says nothing.
    #[test]
    fn a_bar_with_no_facts_paints_nothing() {
        let h = PaneHandle::new(leaf());
        let mut ui: Ui<()> = Ui::new();
        let spec = ui.frame(h.bar_node::<()>(Axis::Vertical), Size::new(1, 10));
        assert!(spec.items.is_empty());
    }

    /// The keyboard's visual motion reads the same rows: a column is kept
    /// across rows, and the rows' ends are where `End` goes.
    #[test]
    fn visual_motion_reads_the_settled_rows() {
        let h = PaneHandle::new(leaf());
        settled(&h);
        let v = h.view();
        assert_eq!(v.find_visual_row(7), Some(1));
        assert_eq!(v.byte_to_visual_column(7), Some(1));
        assert_eq!(v.move_visual_line(7, 1, 1), Some((13, 1)));
        assert_eq!(v.move_visual_line(7, 1, -1), Some((1, 1)));
        assert_eq!(v.move_visual_line(13, 1, 1), None, "off the rows on screen");
        assert_eq!(v.visual_line_end(7, false), Some(11));
        assert_eq!(v.visual_line_start(7, false), Some(6));
        assert_eq!(
            v.visual_line_start(6, true),
            Some(0),
            "already at the start: the row above"
        );
    }
}
