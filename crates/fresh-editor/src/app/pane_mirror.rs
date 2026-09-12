//! A pane-mounted panel's buffer, derived from the tree.
//!
//! **The mirror follows the tree; nothing else produces it.** A widget panel
//! mounted in a pane is a virtual buffer with a tree drawn over it: the pane
//! shows the tree, and the buffer is where the panel's text is *reported*
//! from — the status bar's `Ln`/`Col`, a plugin's `cursor_moved` and
//! `getBufferText`, and the page reader's row and column
//! (`Editor::page_follows_caret`). The text projection used to write those
//! rows on every plugin update, as a second rendering of the same spec; this
//! reads them off the rows layout settled, so the buffer's line `n` is the
//! tree's row `n` by construction rather than by parity.
//!
//! **Every row the tree laid out, not every row the screen shows.** A page
//! is one viewport over thousands of rows and the reader's row is counted in
//! the page's own space, so the subtree is painted unclipped
//! ([`fresh_ui::Ui::paint_subtree`]) and folded through the same fold that
//! paints the frame — borders, rules and glyphs come out as the screen would
//! show them — into a cell grid the size of the content, whose rows are the
//! buffer's lines. A panel that is not a page has no window of its own, so
//! its grid is its box.
//!
//! **Once per layout, and only when the rows changed.** Runs at the end of
//! `Editor::lay_out_shell`; a mirror that comes out equal to the last one
//! written leaves the buffer alone, so a frame that laid the tree out for
//! some other reason costs one fold and no buffer write.

use fresh_core::BufferId;

use crate::app::Editor;
use crate::view::shell::msg::UiMsg;
use crate::widgets::PanelKey;

/// What one layout of a pane panel says its buffer holds.
struct Mirror {
    /// The rows, trailing blanks trimmed.
    rows: Vec<String>,
    /// Where the tree put the panel's caret, as a content row and display
    /// column — `None` when the panel places no caret this frame.
    caret: Option<(u32, u16)>,
}

impl Editor {
    /// Bring every pane-mounted panel's buffer up to date with the rows the
    /// tree just laid out, and seat its cursor where the tree put the caret.
    pub(crate) fn mirror_pane_panels(&mut self) {
        let Some(mut ui) = self.shell_ui.take() else {
            return;
        };
        let mut mirrors: Vec<(PanelKey, BufferId, Mirror)> = Vec::new();
        for key in self.widget_registry.panel_keys() {
            // Only a pane-mounted panel has a buffer to mirror into; the
            // dock, the floating panel and a sidebar section are the tree's
            // alone.
            if self.slot_of_panel(&key).is_some() {
                continue;
            }
            let Some(buffer) = self.widget_registry.get(&key).and_then(|p| p.buffer_id) else {
                continue;
            };
            // The panel's own subtree, resolved the way every other by-key
            // read into a panel is — a buffer group's inner panes share the
            // outer leaf, so the leaf alone does not name the panel.
            let Some(root) = self.panel_subtree_root(&ui, &key) else {
                continue;
            };
            let page = self.page_viewport(&key);
            mirrors.push((key, buffer, mirror_of(&mut ui, root, page)));
        }
        self.shell_ui = Some(ui);
        for (key, buffer, mirror) in mirrors {
            self.apply_pane_mirror(&key, buffer, mirror);
        }
    }

    /// Write `mirror` into the panel's buffer when it differs from what the
    /// buffer holds, and seat the buffer's cursor on the tree's caret.
    fn apply_pane_mirror(&mut self, key: &PanelKey, buffer: BufferId, mirror: Mirror) {
        if self.pane_mirrors.get(key) != Some(&mirror.rows) {
            // One entry per row, each ending in the newline that makes it a
            // line of the buffer: the buffer's line model is byte-driven, so
            // without it the rows would run together into one line and a
            // reading row would be a column count.
            let entries = mirror
                .rows
                .iter()
                .map(|r| {
                    crate::primitives::text_property::TextPropertyEntry::text(format!("{r}\n"))
                })
                .collect();
            if let Err(e) = self
                .active_window_mut()
                .set_virtual_buffer_content(buffer, entries)
            {
                tracing::warn!("mirroring widget panel {} into {:?}: {}", key, buffer, e);
                return;
            }
            self.pane_mirrors.insert(key.clone(), mirror.rows.clone());
        }
        // A widget panel is laid out to the pane's exact width and clipped
        // there, so its view has nothing to scroll sideways to; the caret
        // below can sit at the end of a full row, and cursor-following would
        // otherwise drag the whole panel left by a column.
        self.pin_widget_panel_horizontal_scroll(buffer);
        // **A page's cursor is the reader's**, seated by `move_page_reader`
        // from the reading position the caret marker is drawn *from*; seating
        // it here again would only round-trip that position through the row
        // text. Every other panel's caret is a focused field's, and the buffer
        // cursor follows it — `Ln`/`Col` and `cursor_moved` read it there.
        if self.page_anchors.contains_key(key) {
            return;
        }
        // A plugin that took the cursor for itself (`setBufferShowCursors`)
        // owns its visibility and its position — the git log's commit list is
        // cursor-driven, and a repaint must not clear it.
        let locked = self
            .active_window()
            .buffers
            .get(&buffer)
            .map(|s| s.cursor_visibility_locked)
            .unwrap_or(false);
        if locked {
            return;
        }
        let byte = mirror.caret.and_then(|(row, col)| {
            let line = mirror.rows.get(row as usize)?;
            let start = self
                .active_window()
                .buffers
                .get(&buffer)?
                .buffer
                .line_start_offset(row as usize)?;
            Some(start + super::widget_runtime::byte_at_display_col(line, col))
        });
        let window = self.active_window_mut();
        if let Some(state) = window.buffers.get_mut(&buffer) {
            state.show_cursors = byte.is_some();
        }
        let Some(byte) = byte else {
            return;
        };
        for vs in window
            .split_view_states_mut()
            .expect("active window must have a populated split layout")
            .values_mut()
        {
            if vs.buffer_state(buffer).is_some() {
                vs.cursors.primary_mut().position = byte;
            }
        }
    }
}

/// The rows of the subtree under `root`, as the fold would paint them, in the
/// content space of `page`'s viewport when there is one and of the subtree's
/// own box otherwise.
fn mirror_of(
    ui: &mut fresh_ui::Ui<UiMsg>,
    root: fresh_ui::ElementId,
    page: Option<fresh_ui::ElementId>,
) -> Mirror {
    use crate::view::shell::fold::{fold_band, Band, Paints, SkipHosts};
    use unicode_width::UnicodeWidthStr;

    // Content space: a page's rows are counted from the top of its content,
    // which the window's offset has scrolled off the top of its box.
    let (origin, scroll, extent) = match page {
        Some(vp) => {
            let r = ui.rect_of(vp);
            let (scroll, content) = ui.scroll(vp);
            (
                r.origin(),
                scroll,
                fresh_ui::Size::new(r.w, content.h.max(r.h)),
            )
        }
        None => {
            let r = ui.rect_of(root);
            (r.origin(), fresh_ui::Point::ZERO, r.size())
        }
    };
    let (dx, dy) = (-origin.x, scroll.y - origin.y);
    let mut spec = ui.paint_subtree(root);
    for item in &mut spec.items {
        item.rect = item.rect.translate(dx, dy);
        item.clip = item.rect;
    }
    // A page's content is every row that was painted, however the viewport
    // counts its content; a box's is its own height, so a list's rows
    // scrolled out of their window stay out.
    let extent = match page {
        Some(_) => {
            let bottom = spec
                .items
                .iter()
                .map(|i| i.rect.bottom())
                .max()
                .unwrap_or(0)
                .clamp(0, u16::MAX as i32) as u16;
            fresh_ui::Size::new(extent.w, extent.h.max(bottom))
        }
        None => extent,
    };
    let caret = spec
        .cursor
        .filter(|c| c.visible)
        .map(|c| ((c.pos.y + dy).max(0) as u32, (c.pos.x + dx).max(0) as u16));
    spec.frame = extent;

    // The same fold that paints the frame, into a grid the size of the
    // content; colour is not the mirror's, so the palette says nothing.
    let mut buf =
        ratatui::buffer::Buffer::empty(ratatui::layout::Rect::new(0, 0, extent.w, extent.h));
    let palette = |_: &fresh_ui::ThemeKey| ratatui::style::Style::default();
    fold_band(
        &spec,
        &mut buf,
        &palette,
        &mut SkipHosts,
        Band::Background,
        Paints::All,
        None,
    );
    let rows = (0..extent.h)
        .map(|y| {
            let mut row = String::new();
            let mut x = 0u16;
            while x < extent.w {
                let sym = buf[(x, y)].symbol();
                row.push_str(sym);
                // A wide glyph keeps both cells layout measured for it; the
                // fold blanks the second, and the row is the glyph once.
                x += (sym.width() as u16).max(1);
            }
            row.trim_end().to_string()
        })
        .collect();
    Mirror { rows, caret }
}
