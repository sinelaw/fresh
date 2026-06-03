use super::drag::TabDragState;
use super::hover::HoverTarget;
use crate::config::ExplorerWidth;
use crate::model::event::{ContainerId, LeafId, SplitDirection};

/// Mouse state tracking
#[derive(Debug, Clone, Default)]
pub struct MouseState {
    /// Whether we're currently dragging a vertical scrollbar
    pub dragging_scrollbar: Option<LeafId>,
    /// Whether we're currently dragging a horizontal scrollbar
    pub dragging_horizontal_scrollbar: Option<LeafId>,
    /// Initial mouse column when starting horizontal scrollbar drag
    pub drag_start_hcol: Option<u16>,
    /// Initial left_column when starting horizontal scrollbar drag
    pub drag_start_left_column: Option<usize>,
    /// Last mouse position
    pub last_position: Option<(u16, u16)>,
    /// Mouse hover for LSP: byte position being hovered, timer start, and screen position
    /// Format: (byte_position, hover_start_instant, screen_x, screen_y)
    pub lsp_hover_state: Option<(usize, std::time::Instant, u16, u16)>,
    /// Whether we've already sent a hover request for the current position
    pub lsp_hover_request_sent: bool,
    /// Initial mouse row when starting to drag the scrollbar thumb
    /// Used to calculate relative movement rather than jumping
    pub drag_start_row: Option<u16>,
    /// Initial viewport top_byte when starting to drag the scrollbar thumb
    pub drag_start_top_byte: Option<usize>,
    /// Initial viewport top_view_line_offset when starting to drag the scrollbar thumb
    /// This is needed for proper visual row calculation when scrolled into a wrapped line
    pub drag_start_view_line_offset: Option<usize>,
    /// Whether we're currently dragging a split separator
    /// Stores (split_id, direction) for the separator being dragged
    pub dragging_separator: Option<(ContainerId, SplitDirection)>,
    /// Initial mouse position when starting to drag a separator
    pub drag_start_position: Option<(u16, u16)>,
    /// Initial split ratio when starting to drag a separator
    pub drag_start_ratio: Option<f32>,
    /// Whether we're currently dragging the file explorer border
    pub dragging_file_explorer: bool,
    /// File explorer width at the moment the drag started. Drag
    /// preserves the active variant: a drag that begins in `Percent`
    /// stays in `Percent`, and likewise for `Columns`.
    pub drag_start_explorer_width: Option<ExplorerWidth>,
    /// Current hover target (if any)
    pub hover_target: Option<HoverTarget>,
    /// Whether we're currently doing a text selection drag
    pub dragging_text_selection: bool,
    /// The split where text selection started
    pub drag_selection_split: Option<LeafId>,
    /// The buffer byte position where the selection anchor is
    pub drag_selection_anchor: Option<usize>,
    /// When true, dragging extends selection by whole words (set by double-click)
    pub drag_selection_by_words: bool,
    /// The end of the initially double-clicked word (used as anchor when dragging backward)
    pub drag_selection_word_end: Option<usize>,
    /// Tab drag state (for drag-to-split functionality)
    pub dragging_tab: Option<TabDragState>,
    /// Whether we're currently dragging a popup scrollbar (popup index)
    pub dragging_popup_scrollbar: Option<usize>,
    /// Initial scroll offset when starting to drag popup scrollbar
    pub drag_start_popup_scroll: Option<usize>,
    /// Whether we're currently dragging the prompt's suggestion-list
    /// scrollbar (Live Grep floating overlay, issue #1796). The
    /// rect is held in `ChromeLayout::suggestions_scrollbar_rect`
    /// and the math is shared with the buffer-popup scrollbar via
    /// `view::ui::scrollbar::ScrollbarState::click_to_offset`.
    pub dragging_prompt_scrollbar: bool,
    /// Whether we're currently selecting text in a popup (popup index)
    pub selecting_in_popup: Option<usize>,
    /// Initial composite scroll_row when starting to drag the scrollbar thumb
    /// Used for composite buffer scrollbar drag
    pub drag_start_composite_scroll_row: Option<usize>,
}
