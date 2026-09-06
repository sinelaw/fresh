//! What a pane's scrollbars show, settled before the frame is described
//! (design §3.7.6).
//!
//! The editor owns the pane's scroll, so the bar is not a viewport of the
//! tree's: it is a leaf beside the content that paints an ordinary
//! `Draw::Scrollbar` from the facts settled here — the offset and the content
//! in the unit the pane scrolls in, the window as a rule over the track the
//! leaf is laid out to, and the marks resolved to rows for the leaf to bucket
//! onto that track (`view::shell::buffer_host::BarFacts`). Settled from the
//! viewport as the frame's pre-sync left it, which is the viewport the frame
//! paints; a reconcile that moves it under a new rectangle asks for the
//! frame that shows the move (`Editor::request_frame`).

use std::collections::HashMap;
use std::rc::Rc;

use crate::app::types::HoverTarget;
use crate::app::Editor;
use crate::model::event::LeafId;
use crate::view::scrollbar_marker::MarkerBasis;
use crate::view::shell::buffer_host::{BarFacts, BarWindow};
use crate::view::shell::splits::PaneChrome;
use crate::view::ui::split_rendering::scrollbar::{
    compute_max_line_length, resolve_marker_color, resolve_scrollbar_marks, scrollbar_line_counts,
};

impl Editor {
    /// Settle every visible pane's bars for the frame about to be described.
    /// `chrome` says which bars each pane has; a pane without one settles
    /// `None` and its bar's slot has no width.
    pub(crate) fn settle_pane_bars(&mut self, chrome: &HashMap<LeafId, PaneChrome>) {
        let hover = self.shell_hover.clone();
        let threshold = self.config.editor.large_file_threshold_bytes;
        let theme = self.theme.read().unwrap().clone();
        let panes = self.window_panes();
        let win = self.active_window_mut();
        for (pane, buffer_id) in panes {
            let c = chrome.get(&pane).copied().unwrap_or_default();
            let hover_cell = match hover {
                Some(HoverTarget::ScrollbarTrack(p, row)) if p == pane => Some(row),
                _ => None,
            };
            let composite = win
                .composite_buffers
                .get(&buffer_id)
                .map(|comp| comp.row_count());
            let composite_scroll = win
                .composite_view_states
                .get(&(pane, buffer_id))
                .map(|vs| vs.scroll_row);
            let facts = win.buffers.with_all_mut(|buffers, _mgr, vs_map| {
                let (Some(state), Some(vs)) = (buffers.get_mut(&buffer_id), vs_map.get_mut(&pane))
                else {
                    return (None, None);
                };
                if state.is_composite_buffer {
                    // A composite view scrolls by row under a header row it
                    // keeps out of its window; it does not scroll sideways.
                    let v = c.vscroll.then(|| {
                        let mut f = BarFacts::plain(
                            composite_scroll.unwrap_or(0).min(u32::MAX as usize) as u32,
                            composite.unwrap_or(0).min(u32::MAX as usize) as u32,
                            BarWindow::TrackLess(1),
                        );
                        f.hover_cell = hover_cell;
                        f
                    });
                    let h = c
                        .hscroll
                        .then(|| BarFacts::plain(0, 0, BarWindow::Cells(1)));
                    return (v, h);
                }
                let bvs = vs.active_state_mut();
                let buffer_len = state.buffer.len();
                let v = c.vscroll.then(|| {
                    let fold_ranges = state.fold_ranges(&bvs.folds);
                    let (total_lines, top_line, basis) = scrollbar_line_counts(
                        state,
                        &bvs.viewport,
                        threshold,
                        buffer_len,
                        fold_ranges,
                    );
                    let (offset, content, window) = match basis {
                        // No line coordinate exists on a file this size; the
                        // thumb is one cell placed by the byte fraction.
                        MarkerBasis::Bytes { .. } => (
                            bvs.viewport.top_byte().min(u32::MAX as usize) as u32,
                            buffer_len.clamp(1, u32::MAX as usize) as u32,
                            BarWindow::OneCell,
                        ),
                        _ => (
                            top_line.min(u32::MAX as usize) as u32,
                            total_lines.clamp(1, u32::MAX as usize) as u32,
                            BarWindow::Track,
                        ),
                    };
                    let marks = resolve_scrollbar_marks(state, basis);
                    let colors: Rc<[ratatui::style::Color]> = marks
                        .iter()
                        .map(|m| resolve_marker_color(&m.color, &theme))
                        .collect::<Vec<_>>()
                        .into();
                    BarFacts {
                        offset,
                        content,
                        window,
                        marks,
                        total: basis.total().max(1),
                        colors,
                        hover_cell,
                    }
                });
                let h = c.hscroll.then(|| {
                    if bvs.viewport.line_wrap_enabled {
                        // Wrapped rows never scroll sideways: everything is
                        // in the window.
                        return BarFacts::plain(0, 0, BarWindow::Cells(1));
                    }
                    let visible = bvs.viewport.width as usize;
                    let widest = compute_max_line_length(state, &mut bvs.viewport).max(visible);
                    BarFacts::plain(
                        bvs.viewport.left_column.min(u32::MAX as usize) as u32,
                        widest.clamp(1, u32::MAX as usize) as u32,
                        BarWindow::Cells(visible.clamp(1, u32::MAX as usize) as u32),
                    )
                });
                (v, h)
            });
            let (v, h) = facts.unwrap_or((None, None));
            win.pane_handle_for(pane).settle_bars(v, h);
        }
    }
}
