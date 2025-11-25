use crate::async_bridge::LspMessageType;
use crate::event::{BufferId, SplitDirection, SplitId};
use crate::keybindings::Action;
use ratatui::layout::Rect;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub const DEFAULT_BACKGROUND_FILE: &str = "scripts/landscape-wide.txt";

/// Search state for find/replace functionality
#[derive(Debug, Clone)]
pub(super) struct SearchState {
    /// The search query
    pub query: String,
    /// All match positions in the buffer (byte offsets)
    pub matches: Vec<usize>,
    /// Index of the currently selected match
    pub current_match_index: Option<usize>,
    /// Whether search wraps around at document boundaries
    pub wrap_search: bool,
    /// Optional search range (for search in selection)
    pub search_range: Option<Range<usize>>,
    /// Whether search is case-sensitive (default: true)
    pub case_sensitive: bool,
    /// Whether to match whole words only (default: false)
    pub whole_word: bool,
}

/// A bookmark in the editor (position in a specific buffer)
#[derive(Debug, Clone)]
pub(super) struct Bookmark {
    /// Buffer ID where the bookmark is set
    pub buffer_id: BufferId,
    /// Byte offset position in the buffer
    pub position: usize,
}

/// State for interactive replace (query-replace)
#[derive(Debug, Clone)]
pub(super) struct InteractiveReplaceState {
    /// The search pattern
    pub search: String,
    /// The replacement text
    pub replacement: String,
    /// Current match position (byte offset of the match we're at)
    pub current_match_pos: usize,
    /// Starting position (to detect when we've wrapped around full circle)
    pub start_pos: usize,
    /// Whether we've wrapped around to the beginning
    pub has_wrapped: bool,
    /// Number of replacements made so far
    pub replacements_made: usize,
}

/// The kind of buffer (file-backed or virtual)
#[derive(Debug, Clone, PartialEq)]
pub enum BufferKind {
    /// A buffer backed by a file on disk
    File {
        /// Path to the file
        path: PathBuf,
        /// LSP URI for the file
        uri: Option<lsp_types::Uri>,
    },
    /// A virtual buffer (not backed by a file)
    /// Used for special buffers like *Diagnostics*, *Grep*, etc.
    Virtual {
        /// The buffer's mode (e.g., "diagnostics-list", "grep-results")
        mode: String,
    },
}

/// Metadata associated with a buffer
#[derive(Debug, Clone)]
pub struct BufferMetadata {
    /// The kind of buffer (file or virtual)
    pub kind: BufferKind,

    /// Display name for the buffer (project-relative path or filename or *BufferName*)
    pub display_name: String,

    /// Whether LSP is enabled for this buffer (always false for virtual buffers)
    pub lsp_enabled: bool,

    /// Reason LSP is disabled (if applicable)
    pub lsp_disabled_reason: Option<String>,

    /// Whether the buffer is read-only (typically true for virtual buffers)
    pub read_only: bool,
}

impl BufferMetadata {
    /// Get the file path if this is a file-backed buffer
    pub fn file_path(&self) -> Option<&PathBuf> {
        match &self.kind {
            BufferKind::File { path, .. } => Some(path),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Get the file URI if this is a file-backed buffer
    pub fn file_uri(&self) -> Option<&lsp_types::Uri> {
        match &self.kind {
            BufferKind::File { uri, .. } => uri.as_ref(),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Check if this is a virtual buffer
    pub fn is_virtual(&self) -> bool {
        matches!(self.kind, BufferKind::Virtual { .. })
    }

    /// Get the mode name for virtual buffers
    pub fn virtual_mode(&self) -> Option<&str> {
        match &self.kind {
            BufferKind::Virtual { mode } => Some(mode),
            BufferKind::File { .. } => None,
        }
    }
}

impl BufferMetadata {
    /// Create new metadata for a buffer (unnamed, file-backed)
    pub fn new() -> Self {
        Self {
            kind: BufferKind::File {
                path: PathBuf::new(),
                uri: None,
            },
            display_name: "[No Name]".to_string(),
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
        }
    }

    /// Create metadata for a file-backed buffer
    ///
    /// # Arguments
    /// * `path` - The canonical absolute path to the file
    /// * `working_dir` - The canonical working directory for computing relative display name
    pub fn with_file(path: PathBuf, working_dir: &Path) -> Self {
        // Compute URI from the absolute path
        let file_uri = url::Url::from_file_path(&path)
            .ok()
            .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok());

        // Compute display name (project-relative if possible, otherwise just filename)
        let display_name = path
            .strip_prefix(working_dir)
            .ok()
            .and_then(|rel_path| rel_path.to_str())
            .or_else(|| path.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "[Unknown]".to_string());

        Self {
            kind: BufferKind::File {
                path,
                uri: file_uri,
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
        }
    }

    /// Create metadata for a virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    pub fn virtual_buffer(name: String, mode: String, read_only: bool) -> Self {
        Self {
            kind: BufferKind::Virtual { mode },
            display_name: name,
            lsp_enabled: false, // Virtual buffers don't use LSP
            lsp_disabled_reason: Some("Virtual buffer".to_string()),
            read_only,
        }
    }

    /// Disable LSP for this buffer with a reason
    pub fn disable_lsp(&mut self, reason: String) {
        self.lsp_enabled = false;
        self.lsp_disabled_reason = Some(reason);
    }
}

/// State for macro recording
#[derive(Debug, Clone)]
pub(super) struct MacroRecordingState {
    /// The register key for this macro
    pub key: char,
    /// Actions recorded so far
    pub actions: Vec<Action>,
}

/// LSP progress information
#[derive(Debug, Clone)]
pub(super) struct LspProgressInfo {
    pub language: String,
    pub title: String,
    pub message: Option<String>,
    pub percentage: Option<u32>,
}

/// LSP message entry (for window messages and logs)
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(super) struct LspMessageEntry {
    pub language: String,
    pub message_type: LspMessageType,
    pub message: String,
    pub timestamp: std::time::Instant,
}

/// Types of UI elements that can be hovered over
#[derive(Debug, Clone, PartialEq)]
pub enum HoverTarget {
    /// Hovering over a split separator (split_id, direction)
    SplitSeparator(SplitId, SplitDirection),
    /// Hovering over a scrollbar thumb (split_id)
    ScrollbarThumb(SplitId),
    /// Hovering over a scrollbar track (split_id)
    ScrollbarTrack(SplitId),
    /// Hovering over a menu bar item (menu_index)
    MenuBarItem(usize),
    /// Hovering over a menu dropdown item (menu_index, item_index)
    MenuDropdownItem(usize, usize),
    /// Hovering over a popup list item (popup_index in stack, item_index)
    PopupListItem(usize, usize),
    /// Hovering over a suggestion item (item_index)
    SuggestionItem(usize),
    /// Hovering over the file explorer border (for resize)
    FileExplorerBorder,
}

/// Mouse state tracking
#[derive(Debug, Clone, Default)]
pub(super) struct MouseState {
    /// Whether we're currently dragging a scrollbar
    pub dragging_scrollbar: Option<SplitId>,
    /// Last mouse position
    pub last_position: Option<(u16, u16)>,
    /// Initial mouse row when starting to drag the scrollbar thumb
    /// Used to calculate relative movement rather than jumping
    pub drag_start_row: Option<u16>,
    /// Initial viewport top_byte when starting to drag the scrollbar thumb
    pub drag_start_top_byte: Option<usize>,
    /// Whether we're currently dragging a split separator
    /// Stores (split_id, direction) for the separator being dragged
    pub dragging_separator: Option<(SplitId, SplitDirection)>,
    /// Initial mouse position when starting to drag a separator
    pub drag_start_position: Option<(u16, u16)>,
    /// Initial split ratio when starting to drag a separator
    pub drag_start_ratio: Option<f32>,
    /// Whether we're currently dragging the file explorer border
    pub dragging_file_explorer: bool,
    /// Initial file explorer width percentage when starting to drag
    pub drag_start_explorer_width: Option<f32>,
    /// Current hover target (if any)
    pub hover_target: Option<HoverTarget>,
}

/// Cached layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
pub(super) struct CachedLayout {
    /// File explorer area (if visible)
    pub file_explorer_area: Option<Rect>,
    /// Editor content area (excluding file explorer)
    pub editor_content_area: Option<Rect>,
    /// Individual split areas with their scrollbar areas and thumb positions
    /// (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end)
    pub split_areas: Vec<(SplitId, BufferId, Rect, Rect, usize, usize)>,
    /// Split separator positions for drag resize
    /// (split_id, direction, x, y, length)
    pub separator_areas: Vec<(SplitId, SplitDirection, u16, u16, u16)>,
    /// Popup areas for mouse hit testing
    /// (popup_index, rect, inner_rect, scroll_offset, num_items)
    pub popup_areas: Vec<(usize, Rect, Rect, usize, usize)>,
    /// Suggestions area for mouse hit testing
    /// (inner_rect, scroll_start_idx, visible_count, total_count)
    pub suggestions_area: Option<(Rect, usize, usize, usize)>,
}
