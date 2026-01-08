use crate::app::file_open::SortMode;
use crate::input::keybindings::Action;
use crate::model::event::{BufferId, SplitDirection, SplitId};
use crate::services::async_bridge::LspMessageType;
use ratatui::layout::Rect;
use rust_i18n::t;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};

pub const DEFAULT_BACKGROUND_FILE: &str = "scripts/landscape-wide.txt";

/// Pre-calculated line information for an event
/// Calculated BEFORE buffer modification so line numbers are accurate
#[derive(Debug, Clone, Default)]
pub(super) struct EventLineInfo {
    /// Start line (0-indexed) where the change begins
    pub start_line: usize,
    /// End line (0-indexed) where the change ends (in original buffer for deletes)
    pub end_line: usize,
    /// Number of lines added (for inserts) or removed (for deletes)
    pub line_delta: i32,
}

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

    /// Whether the buffer contains binary content
    /// Binary buffers are automatically read-only and render unprintable chars as code points
    pub binary: bool,

    /// LSP server instance IDs that have received didOpen for this buffer.
    /// Used to ensure didOpen is sent before any requests to a new/restarted server.
    /// When a server restarts, it gets a new ID, so didOpen is automatically resent.
    /// Old IDs are harmless - they just remain in the set but don't match any active server.
    pub lsp_opened_with: HashSet<u64>,

    /// Whether this buffer should be hidden from tabs (used for composite source buffers)
    pub hidden_from_tabs: bool,
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
            display_name: t!("buffer.no_name").to_string(),
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
        }
    }

    /// Create new metadata for an unnamed buffer with a custom display name
    /// Used for buffers created from stdin or other non-file sources
    pub fn new_unnamed(display_name: String) -> Self {
        Self {
            kind: BufferKind::File {
                path: PathBuf::new(),
                uri: None,
            },
            display_name,
            lsp_enabled: false, // No file path, so no LSP
            lsp_disabled_reason: Some(t!("lsp.disabled.unnamed").to_string()),
            read_only: false,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
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

        // Compute display name (project-relative when under working_dir, else absolute path).
        // Use canonicalized forms first to handle macOS /var -> /private/var differences.
        let display_name = Self::display_name_for_path(&path, working_dir);

        Self {
            kind: BufferKind::File {
                path,
                uri: file_uri,
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
        }
    }

    /// Compute display name relative to working_dir when possible, otherwise absolute
    pub fn display_name_for_path(path: &Path, working_dir: &Path) -> String {
        // Canonicalize working_dir to normalize platform-specific prefixes
        let canonical_working_dir = working_dir
            .canonicalize()
            .unwrap_or_else(|_| working_dir.to_path_buf());

        // Try to canonicalize the file path; if it fails (e.g., new file), fall back to absolute
        let absolute_path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            // If we were given a relative path, anchor it to working_dir
            canonical_working_dir.join(path)
        };
        let canonical_path = absolute_path
            .canonicalize()
            .unwrap_or_else(|_| absolute_path.clone());

        // Prefer canonical comparison first, then raw prefix as a fallback
        let relative = canonical_path
            .strip_prefix(&canonical_working_dir)
            .or_else(|_| path.strip_prefix(working_dir))
            .ok()
            .and_then(|rel| rel.to_str().map(|s| s.to_string()));

        relative
            .or_else(|| canonical_path.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| t!("buffer.unknown").to_string())
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
            lsp_disabled_reason: Some(t!("lsp.disabled.virtual").to_string()),
            read_only,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
        }
    }

    /// Create metadata for a hidden virtual buffer (for composite source buffers)
    /// These buffers are not shown in tabs and are managed by their parent composite buffer.
    /// Hidden buffers are always read-only to prevent accidental edits.
    pub fn hidden_virtual_buffer(name: String, mode: String) -> Self {
        Self {
            kind: BufferKind::Virtual { mode },
            display_name: name,
            lsp_enabled: false,
            lsp_disabled_reason: Some(t!("lsp.disabled.virtual").to_string()),
            read_only: true, // Hidden buffers are always read-only
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: true,
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
    /// Hovering over a submenu item (depth, item_index) - depth 1+ for nested submenus
    SubmenuItem(usize, usize),
    /// Hovering over a popup list item (popup_index in stack, item_index)
    PopupListItem(usize, usize),
    /// Hovering over a suggestion item (item_index)
    SuggestionItem(usize),
    /// Hovering over the file explorer border (for resize)
    FileExplorerBorder,
    /// Hovering over a file browser navigation shortcut
    FileBrowserNavShortcut(usize),
    /// Hovering over a file browser file/directory entry
    FileBrowserEntry(usize),
    /// Hovering over a file browser column header
    FileBrowserHeader(SortMode),
    /// Hovering over the file browser scrollbar
    FileBrowserScrollbar,
    /// Hovering over the file browser "Show Hidden" checkbox
    FileBrowserShowHiddenCheckbox,
    /// Hovering over a tab name (buffer_id, split_id) - for non-active tabs
    TabName(BufferId, SplitId),
    /// Hovering over a tab close button (buffer_id, split_id)
    TabCloseButton(BufferId, SplitId),
    /// Hovering over a close split button (split_id)
    CloseSplitButton(SplitId),
    /// Hovering over a maximize/unmaximize split button (split_id)
    MaximizeSplitButton(SplitId),
    /// Hovering over the file explorer close button
    FileExplorerCloseButton,
    /// Hovering over the status bar LSP indicator
    StatusBarLspIndicator,
    /// Hovering over the status bar warning badge
    StatusBarWarningBadge,
    /// Hovering over the status bar line ending indicator
    StatusBarLineEndingIndicator,
    /// Hovering over the search options "Case Sensitive" checkbox
    SearchOptionCaseSensitive,
    /// Hovering over the search options "Whole Word" checkbox
    SearchOptionWholeWord,
    /// Hovering over the search options "Regex" checkbox
    SearchOptionRegex,
    /// Hovering over the search options "Confirm Each" checkbox
    SearchOptionConfirmEach,
    /// Hovering over a tab context menu item (item_index)
    TabContextMenuItem(usize),
}

/// Tab context menu items
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabContextMenuItem {
    /// Close this tab
    Close,
    /// Close all other tabs
    CloseOthers,
    /// Close tabs to the right
    CloseToRight,
    /// Close tabs to the left
    CloseToLeft,
    /// Close all tabs
    CloseAll,
}

impl TabContextMenuItem {
    /// Get all menu items in order
    pub fn all() -> &'static [Self] {
        &[
            Self::Close,
            Self::CloseOthers,
            Self::CloseToRight,
            Self::CloseToLeft,
            Self::CloseAll,
        ]
    }

    /// Get the display label for this menu item
    pub fn label(&self) -> String {
        match self {
            Self::Close => t!("tab.close").to_string(),
            Self::CloseOthers => t!("tab.close_others").to_string(),
            Self::CloseToRight => t!("tab.close_to_right").to_string(),
            Self::CloseToLeft => t!("tab.close_to_left").to_string(),
            Self::CloseAll => t!("tab.close_all").to_string(),
        }
    }
}

/// State for tab context menu (right-click popup on tabs)
#[derive(Debug, Clone)]
pub struct TabContextMenu {
    /// The buffer ID this context menu is for
    pub buffer_id: BufferId,
    /// The split ID where the tab is located
    pub split_id: SplitId,
    /// Screen position where the menu should appear (x, y)
    pub position: (u16, u16),
    /// Currently highlighted menu item index
    pub highlighted: usize,
}

impl TabContextMenu {
    /// Create a new tab context menu
    pub fn new(buffer_id: BufferId, split_id: SplitId, x: u16, y: u16) -> Self {
        Self {
            buffer_id,
            split_id,
            position: (x, y),
            highlighted: 0,
        }
    }

    /// Get the currently highlighted item
    pub fn highlighted_item(&self) -> TabContextMenuItem {
        TabContextMenuItem::all()[self.highlighted]
    }

    /// Move highlight down
    pub fn next_item(&mut self) {
        let items = TabContextMenuItem::all();
        self.highlighted = (self.highlighted + 1) % items.len();
    }

    /// Move highlight up
    pub fn prev_item(&mut self) {
        let items = TabContextMenuItem::all();
        self.highlighted = if self.highlighted == 0 {
            items.len() - 1
        } else {
            self.highlighted - 1
        };
    }
}

/// Drop zone for tab drag-and-drop
/// Indicates where a dragged tab will be placed when released
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabDropZone {
    /// Drop into an existing split's tab bar (before tab at index, or at end if None)
    /// (target_split_id, insert_index)
    TabBar(SplitId, Option<usize>),
    /// Create a new split on the left edge of the target split
    SplitLeft(SplitId),
    /// Create a new split on the right edge of the target split
    SplitRight(SplitId),
    /// Create a new split on the top edge of the target split
    SplitTop(SplitId),
    /// Create a new split on the bottom edge of the target split
    SplitBottom(SplitId),
    /// Drop into the center of a split (switch to that split's tab bar)
    SplitCenter(SplitId),
}

impl TabDropZone {
    /// Get the split ID this drop zone is associated with
    pub fn split_id(&self) -> SplitId {
        match self {
            Self::TabBar(id, _)
            | Self::SplitLeft(id)
            | Self::SplitRight(id)
            | Self::SplitTop(id)
            | Self::SplitBottom(id)
            | Self::SplitCenter(id) => *id,
        }
    }
}

/// State for a tab being dragged
#[derive(Debug, Clone)]
pub struct TabDragState {
    /// The buffer being dragged
    pub buffer_id: BufferId,
    /// The split the tab was dragged from
    pub source_split_id: SplitId,
    /// Starting mouse position when drag began
    pub start_position: (u16, u16),
    /// Current mouse position
    pub current_position: (u16, u16),
    /// Currently detected drop zone (if any)
    pub drop_zone: Option<TabDropZone>,
}

impl TabDragState {
    /// Create a new tab drag state
    pub fn new(buffer_id: BufferId, source_split_id: SplitId, start_position: (u16, u16)) -> Self {
        Self {
            buffer_id,
            source_split_id,
            start_position,
            current_position: start_position,
            drop_zone: None,
        }
    }

    /// Check if the drag has moved enough to be considered a real drag (not just a click)
    pub fn is_dragging(&self) -> bool {
        let dx = (self.current_position.0 as i32 - self.start_position.0 as i32).abs();
        let dy = (self.current_position.1 as i32 - self.start_position.1 as i32).abs();
        dx > 3 || dy > 3 // Threshold of 3 pixels before drag activates
    }
}

/// Mouse state tracking
#[derive(Debug, Clone, Default)]
pub(super) struct MouseState {
    /// Whether we're currently dragging a scrollbar
    pub dragging_scrollbar: Option<SplitId>,
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
    /// Whether we're currently doing a text selection drag
    pub dragging_text_selection: bool,
    /// The split where text selection started
    pub drag_selection_split: Option<SplitId>,
    /// The buffer byte position where the selection anchor is
    pub drag_selection_anchor: Option<usize>,
    /// Tab drag state (for drag-to-split functionality)
    pub dragging_tab: Option<TabDragState>,
    /// Whether we're currently dragging a popup scrollbar (popup index)
    pub dragging_popup_scrollbar: Option<usize>,
    /// Initial scroll offset when starting to drag popup scrollbar
    pub drag_start_popup_scroll: Option<usize>,
}

/// Mapping from visual row to buffer positions for mouse click handling
/// Each entry represents one visual row with byte position info for click handling
#[derive(Debug, Clone, Default)]
pub struct ViewLineMapping {
    /// Source byte offset for each character (None for injected/virtual content)
    pub char_source_bytes: Vec<Option<usize>>,
    /// Character index at each visual column (for O(1) mouse clicks)
    pub visual_to_char: Vec<usize>,
    /// Last valid byte position in this visual row (newline for real lines, last char for wrapped)
    /// Clicks past end of visible text position cursor here
    pub line_end_byte: usize,
}

impl ViewLineMapping {
    /// Get source byte at a given visual column (O(1) for mouse clicks)
    #[inline]
    pub fn source_byte_at_visual_col(&self, visual_col: usize) -> Option<usize> {
        let char_idx = self.visual_to_char.get(visual_col).copied()?;
        self.char_source_bytes.get(char_idx).copied().flatten()
    }
}

/// Cached layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
pub(crate) struct CachedLayout {
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
    /// (popup_index, rect, inner_rect, scroll_offset, num_items, scrollbar_rect, total_lines)
    /// scrollbar_rect is Some if popup has a scrollbar
    pub popup_areas: Vec<(usize, Rect, Rect, usize, usize, Option<Rect>, usize)>,
    /// Suggestions area for mouse hit testing
    /// (inner_rect, scroll_start_idx, visible_count, total_count)
    pub suggestions_area: Option<(Rect, usize, usize, usize)>,
    /// Tab hit areas for mouse interaction
    /// (split_id, buffer_id, tab_row, tab_start_col, tab_end_col, close_button_start_col)
    /// The close button spans from close_button_start_col to tab_end_col
    pub tab_areas: Vec<(SplitId, BufferId, u16, u16, u16, u16)>,
    /// Close split button hit areas
    /// (split_id, row, start_col, end_col)
    pub close_split_areas: Vec<(SplitId, u16, u16, u16)>,
    /// Maximize split button hit areas
    /// (split_id, row, start_col, end_col)
    pub maximize_split_areas: Vec<(SplitId, u16, u16, u16)>,
    /// View line mappings for accurate mouse click positioning per split
    /// Maps visual row index to character position mappings
    /// Used to translate screen coordinates to buffer byte positions
    pub view_line_mappings: HashMap<SplitId, Vec<ViewLineMapping>>,
    /// Settings modal layout for hit testing
    pub settings_layout: Option<crate::view::settings::SettingsLayout>,
    /// Status bar area (row, x, width)
    pub status_bar_area: Option<(u16, u16, u16)>,
    /// Status bar LSP indicator area (row, start_col, end_col)
    pub status_bar_lsp_area: Option<(u16, u16, u16)>,
    /// Status bar warning badge area (row, start_col, end_col)
    pub status_bar_warning_area: Option<(u16, u16, u16)>,
    /// Status bar line ending indicator area (row, start_col, end_col)
    pub status_bar_line_ending_area: Option<(u16, u16, u16)>,
    /// Search options layout for checkbox hit testing
    pub search_options_layout: Option<crate::view::ui::status_bar::SearchOptionsLayout>,
}
