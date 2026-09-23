use super::drag::TabDragState;
use crate::config::ExplorerWidth;
use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection};

/// Mouse state tracking
#[derive(Debug, Clone, Default)]
pub struct MouseState {
    /// The press the pointer is holding, from the press to its release.
    ///
    /// **One value, because one press is held at a time.** The shell's own
    /// surfaces — the dock's width, the sidebar's dividers, a markdown
    /// document's selection — keep theirs on the editor, beside the surface
    /// they belong to. Every drag is
    /// routed by a node's pointer capture, so the moves and the release come
    /// back to the node that took the press; this is what that node's
    /// gesture has to remember in between. It used to be seventeen loose
    /// fields — `dragging_scrollbar`, `drag_start_row`, `drag_selection_*`,
    /// … — that each gesture set a few of and a blanket sweep cleared, so a
    /// gesture could read a field another one had left behind, and some
    /// release arms cleared three of a gesture's five. See
    /// `docs/internal/retained-mode-ui.md`, *A drag's state is the
    /// gesture's*.
    pub drag: Option<PointerDrag>,
    /// Mouse hover for LSP: byte position being hovered, timer start, screen
    /// position, and the buffer the mouse is over.
    /// Format: (byte_position, hover_start_instant, screen_x, screen_y, buffer_id)
    ///
    /// `buffer_id` records which split's buffer the pointer is over so the
    /// hover request targets *that* buffer rather than the active one. Without
    /// it, hovering a non-active split (or a UI panel such as the
    /// Search/Replace dock) fired a hover for the active code buffer at a byte
    /// offset taken from the hovered split's geometry — the popup "leaked
    /// through" the panel (#2572).
    pub lsp_hover_state: Option<(usize, std::time::Instant, u16, u16, BufferId)>,
    /// Whether we've already sent a hover request for the current position
    pub lsp_hover_request_sent: bool,
}

impl MouseState {
    /// The tab being dragged, if that is the held press.
    pub fn tab_drag(&self) -> Option<&TabDragState> {
        match &self.drag {
            Some(PointerDrag::Tab(t)) => Some(t),
            _ => None,
        }
    }

    pub fn tab_drag_mut(&mut self) -> Option<&mut TabDragState> {
        match &mut self.drag {
            Some(PointerDrag::Tab(t)) => Some(t),
            _ => None,
        }
    }

    /// End the tab drag, if that is the held press, and hand it back.
    pub fn take_tab_drag(&mut self) -> Option<TabDragState> {
        match self.drag.take() {
            Some(PointerDrag::Tab(t)) => Some(t),
            other => {
                self.drag = other;
                None
            }
        }
    }

    /// The text selection being dragged in `pane`, if that is the held press.
    pub fn selection_in(&self, pane: LeafId) -> Option<&SelectionDrag> {
        match &self.drag {
            Some(PointerDrag::Selection(s)) if s.pane == pane => Some(s),
            _ => None,
        }
    }
}

/// One held press: what each gesture needs between its press and its
/// release. The press builds the value whole, the captured moves read it,
/// and the release takes it.
#[derive(Debug, Clone)]
pub enum PointerDrag {
    /// A pane's vertical scrollbar. `grab` is set when the press landed on
    /// the thumb, which then moves by how far the pointer has travelled; a
    /// press on the track jumps, and so does every move after it.
    VerticalScrollbar {
        pane: LeafId,
        grab: Option<VerticalGrab>,
    },
    /// A pane's horizontal scrollbar, on the same terms.
    HorizontalScrollbar {
        pane: LeafId,
        grab: Option<HorizontalGrab>,
    },
    /// The file explorer's border. Width is measured from the press, and a
    /// drag keeps the variant the width started in (`Percent` or `Columns`).
    ExplorerBorder {
        press_x: u16,
        start_width: ExplorerWidth,
    },
    /// A split separator.
    Separator(SeparatorDrag),
    /// A text selection being swept across a pane.
    Selection(SelectionDrag),
    /// A press on a live terminal grid, not yet a selection. A bare click
    /// keeps the terminal live (click to focus and type); the first move
    /// drops the split into read-only scrollback and becomes a
    /// [`PointerDrag::Selection`] anchored here
    /// (`Editor::begin_terminal_grid_selection`).
    TerminalPress {
        pane: LeafId,
        buffer: BufferId,
        col: u16,
        row: u16,
    },
    /// A tab being dragged to another split or position.
    Tab(TabDragState),
}

/// Where a vertical scrollbar's thumb was taken.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VerticalGrab {
    /// The screen row the press landed on.
    pub row: u16,
    /// Where the pane was scrolled to at the press.
    pub from: VerticalScroll,
}

/// A pane's vertical scroll position, in whichever unit the pane scrolls in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerticalScroll {
    /// A buffer: its top byte, and the wrapped row within that line.
    Buffer {
        top_byte: usize,
        view_line_offset: usize,
    },
    /// A composite view, which scrolls by row.
    Composite { scroll_row: usize },
}

/// Where a horizontal scrollbar's thumb was taken.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HorizontalGrab {
    /// The screen column the press landed on.
    pub col: u16,
    /// The pane's first visible column at the press.
    pub left_column: usize,
}

/// A split separator drag.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SeparatorDrag {
    /// The container whose divider was pressed.
    pub container: ContainerId,
    pub direction: SplitDirection,
    /// Where the press landed. A move's delta is measured from here.
    pub press: (u16, u16),
    /// The container's ratio at the press.
    pub start_ratio: f32,
}

/// A text selection drag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SelectionDrag {
    pub pane: LeafId,
    /// The byte the selection is anchored at. `None` for a page, whose
    /// selection follows its reader rather than an anchor in the buffer.
    pub anchor: Option<usize>,
    /// Set after a double click: the drag extends by whole words, and this
    /// is the end of the word that was clicked, which is the anchor when
    /// the drag goes backwards.
    pub word_end: Option<usize>,
}
