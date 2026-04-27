use crate::app::file_open::SortMode;
use crate::model::event::{BufferId, ContainerId, LeafId, SplitDirection};
use crate::services::async_bridge::LspMessageType;
use ratatui::layout::Rect;
use rust_i18n::t;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};

pub const DEFAULT_BACKGROUND_FILE: &str = "scripts/landscape-wide.txt";

pub const FILE_EXPLORER_CONTEXT_MENU_WIDTH: u16 = 18;

/// Unique identifier for a buffer group
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BufferGroupId(pub usize);

/// Layout node for a buffer group
#[derive(Debug, Clone)]
pub enum GroupLayoutNode {
    /// A scrollable panel backed by a real buffer
    Scrollable {
        /// Panel name (e.g., "tree", "picker")
        id: String,
        /// Buffer ID for this panel (set during creation)
        buffer_id: Option<BufferId>,
        /// Split leaf ID (set during creation)
        split_id: Option<LeafId>,
    },
    /// A fixed-height panel (header, footer, toolbar)
    Fixed {
        /// Panel name
        id: String,
        /// Height in rows
        height: u16,
        /// Buffer ID (set during creation)
        buffer_id: Option<BufferId>,
        /// Split leaf ID (set during creation)
        split_id: Option<LeafId>,
    },
    /// A horizontal or vertical split containing two children
    Split {
        direction: SplitDirection,
        /// Ratio for the first child (0.0 to 1.0)
        ratio: f32,
        first: Box<GroupLayoutNode>,
        second: Box<GroupLayoutNode>,
    },
}

/// A buffer group: multiple splits/buffers appearing as one tab.
///
/// Each panel is a real buffer with its own viewport, scrollbar,
/// and cursor. The group presents them as a single logical entity
/// in the tab bar and buffer list.
#[derive(Debug)]
pub struct BufferGroup {
    /// Unique ID
    pub id: BufferGroupId,
    /// Display name (shown in tab bar)
    pub name: String,
    /// Mode for keybindings
    pub mode: String,
    /// Layout tree
    pub layout: GroupLayoutNode,
    /// All buffer IDs in this group (panel name → buffer ID)
    pub panel_buffers: HashMap<String, BufferId>,
    /// All split leaf IDs in this group
    pub panel_splits: HashMap<String, LeafId>,
    /// The "representative" split that owns the tab entry.
    /// This is typically the first scrollable panel.
    pub representative_split: Option<LeafId>,
}

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
    /// Match lengths parallel to `matches` (needed for viewport overlay creation)
    pub match_lengths: Vec<usize>,
    /// Index of the currently selected match
    pub current_match_index: Option<usize>,
    /// Whether search wraps around at document boundaries
    pub wrap_search: bool,
    /// Optional search range (for search in selection)
    pub search_range: Option<Range<usize>>,
    /// True if the match count was capped at MAX_MATCHES
    #[allow(dead_code)]
    pub capped: bool,
}

impl SearchState {
    /// Maximum number of search matches to collect before stopping.
    /// Prevents unbounded memory usage when searching for common patterns
    /// in large files.
    pub const MAX_MATCHES: usize = 100_000;
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
    /// Length of the current match in bytes (may differ from search.len() for regex)
    pub current_match_len: usize,
    /// Starting position (to detect when we've wrapped around full circle)
    pub start_pos: usize,
    /// Whether we've wrapped around to the beginning
    pub has_wrapped: bool,
    /// Number of replacements made so far
    pub replacements_made: usize,
    /// Compiled regex for regex-mode replace (None when regex mode is off)
    pub regex: Option<regex::bytes::Regex>,
}

/// The kind of buffer (file-backed or virtual)
#[derive(Debug, Clone, PartialEq)]
pub enum BufferKind {
    /// A buffer backed by a file on disk
    File {
        /// Host-side path to the file. Filesystem APIs and the
        /// editor's own buffer state always speak in host paths.
        path: PathBuf,
        /// LSP-facing URI for the file. Already translated for the
        /// active authority, so handing this to the LSP server is
        /// always correct. See [`LspUri`] for the why.
        uri: Option<LspUri>,
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

    /// Whether this buffer is opened in "preview" mode (ephemeral).
    /// A preview buffer is one opened by a single-click in the file explorer
    /// (or a similar soft-open gesture). Its tab is rendered in italic and
    /// it is replaced the next time another file is opened the same way.
    /// The flag is cleared ("promoted") when the user edits the buffer,
    /// double-clicks the file, or otherwise signals commitment to the file.
    ///
    /// Intentionally ephemeral — never serialized into workspace or
    /// recovery state. Restarting the editor always brings buffers back
    /// as permanent tabs; preview status belongs to the current session's
    /// exploration flow only.
    pub is_preview: bool,

    /// Stable recovery ID for unnamed buffers.
    /// For file-backed buffers, recovery ID is computed from the path hash.
    /// For unnamed buffers, this is generated once and reused across auto-saves.
    pub recovery_id: Option<String>,
}

impl BufferMetadata {
    /// Get the file path if this is a file-backed buffer
    pub fn file_path(&self) -> Option<&PathBuf> {
        match &self.kind {
            BufferKind::File { path, .. } => Some(path),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Get the LSP-facing URI if this is a file-backed buffer.
    ///
    /// The URI is already translated for the active authority — i.e.
    /// it carries the in-container path on a devcontainer authority
    /// and the host path elsewhere. Hand it to the LSP server
    /// directly; do NOT pass it to filesystem APIs (use
    /// [`Self::file_path`] for that).
    pub fn file_uri(&self) -> Option<&LspUri> {
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

impl Default for BufferMetadata {
    fn default() -> Self {
        Self::new()
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
            is_preview: false,
            recovery_id: None,
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
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create metadata for a file-backed buffer
    ///
    /// # Arguments
    /// * `canonical_path` - The canonical (symlink-resolved) absolute path to the file
    /// * `display_path` - The user-visible path before canonicalization (for library detection)
    /// * `working_dir` - The canonical working directory for computing relative display name
    /// * `path_translation` - Active authority's host↔remote workspace mapping;
    ///   used to build the LSP-facing `file_uri` so an in-container LSP sees
    ///   in-container paths. `None` for local/SSH authorities.
    pub fn with_file(
        canonical_path: PathBuf,
        display_path: &Path,
        working_dir: &Path,
        path_translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Self {
        // Compute URI from the absolute path. When the active authority
        // has a host↔remote mapping (devcontainer attach), this is
        // where the host path gets rewritten into the container path
        // the LSP server actually understands.
        let file_uri = LspUri::from_host_path(&canonical_path, path_translation);

        // Compute display name (project-relative when under working_dir, else absolute path).
        // Use canonicalized forms first to handle macOS /var -> /private/var differences.
        let display_name = Self::display_name_for_path(&canonical_path, working_dir);

        // Check if this is a library file (in vendor directories or standard libraries).
        // Library files are read-only (to prevent accidental edits) but LSP stays
        // enabled so that Goto Definition, Hover, Find References, etc. still work
        // when the user navigates into library source code (issue #1344).
        //
        // A file is only considered a library file if BOTH the canonical path and the
        // user-visible path are in a library directory. This prevents symlinked dotfiles
        // (e.g., ~/.bash_profile -> /nix/store/...) from being marked read-only when
        // the user explicitly opened a non-library path (issue #1469).
        let is_library = Self::is_library_path(&canonical_path, working_dir)
            && Self::is_library_path(display_path, working_dir);

        Self {
            kind: BufferKind::File {
                path: canonical_path,
                uri: file_uri,
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: is_library,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create metadata for a buffer fetched from inside a container.
    ///
    /// Used by `Editor::open_lsp_uri_target` when a Goto-Definition
    /// (or similar) URI lands on a path that exists only inside the
    /// container — typically a stdlib / site-packages entry that
    /// isn't bind-mounted onto the host. The buffer is read-only
    /// because there's no host-side writeback path; LSP stays enabled
    /// so further navigation from the fetched buffer (hover, more
    /// goto-defs) keeps working.
    ///
    /// The supplied `uri` is the wire URI the LSP returned (already
    /// in container-side coordinates) and is cached verbatim — no
    /// host→remote translation, because the path *is* the remote
    /// path. The display name is the file name, since the container
    /// path has nothing to relativize against the host working dir.
    pub fn with_container_file(container_path: PathBuf, uri: LspUri) -> Self {
        let display_name = container_path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.to_string())
            .unwrap_or_else(|| container_path.to_string_lossy().to_string());
        Self {
            kind: BufferKind::File {
                path: container_path,
                uri: Some(uri),
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: true,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Check if a path is a library file (in vendor directories or standard libraries)
    ///
    /// Library files include:
    /// - Files in common vendor/dependency directories (.cargo, node_modules, etc.)
    /// - Standard library / toolchain files (rustup toolchains, system includes, etc.)
    pub fn is_library_path(path: &Path, _working_dir: &Path) -> bool {
        // Check for common library paths
        let path_str = path.to_string_lossy();

        // Rust: .cargo directory (can be within project for vendor'd crates)
        if path_str.contains("/.cargo/") || path_str.contains("\\.cargo\\") {
            return true;
        }

        // Rust: rustup toolchains (standard library source files)
        if path_str.contains("/rustup/toolchains/") || path_str.contains("\\rustup\\toolchains\\") {
            return true;
        }

        // Node.js: node_modules
        if path_str.contains("/node_modules/") || path_str.contains("\\node_modules\\") {
            return true;
        }

        // Python: site-packages, dist-packages
        if path_str.contains("/site-packages/")
            || path_str.contains("\\site-packages\\")
            || path_str.contains("/dist-packages/")
            || path_str.contains("\\dist-packages\\")
        {
            return true;
        }

        // Go: pkg/mod
        if path_str.contains("/pkg/mod/") || path_str.contains("\\pkg\\mod\\") {
            return true;
        }

        // Ruby: gems
        if path_str.contains("/gems/") || path_str.contains("\\gems\\") {
            return true;
        }

        // Java/Gradle: .gradle
        if path_str.contains("/.gradle/") || path_str.contains("\\.gradle\\") {
            return true;
        }

        // Maven: .m2
        if path_str.contains("/.m2/") || path_str.contains("\\.m2\\") {
            return true;
        }

        // C/C++: system include directories
        if path_str.starts_with("/usr/include/") || path_str.starts_with("/usr/local/include/") {
            return true;
        }

        // Nix store (system-managed packages)
        if path_str.starts_with("/nix/store/") {
            return true;
        }

        // Homebrew (macOS system-managed packages)
        if path_str.starts_with("/opt/homebrew/Cellar/")
            || path_str.starts_with("/usr/local/Cellar/")
        {
            return true;
        }

        // .NET / C#: NuGet packages
        if path_str.contains("/.nuget/") || path_str.contains("\\.nuget\\") {
            return true;
        }

        // Swift / Xcode toolchains
        if path_str.contains("/Xcode.app/Contents/Developer/")
            || path_str.contains("/CommandLineTools/SDKs/")
        {
            return true;
        }

        false
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
            is_preview: false,
            recovery_id: None,
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
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Disable LSP for this buffer with a reason
    pub fn disable_lsp(&mut self, reason: String) {
        self.lsp_enabled = false;
        self.lsp_disabled_reason = Some(reason);
    }
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
    /// Hovering over a split separator (container_id, direction)
    SplitSeparator(ContainerId, SplitDirection),
    /// Hovering over a scrollbar thumb (split_id)
    ScrollbarThumb(LeafId),
    /// Hovering over a scrollbar track (split_id, relative_row)
    ScrollbarTrack(LeafId, u16),
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
    /// Hovering over the file browser "Detect Encoding" checkbox
    FileBrowserDetectEncodingCheckbox,
    /// Hovering over a tab name (target, split_id) - for non-active tabs
    TabName(crate::view::split::TabTarget, LeafId),
    /// Hovering over a tab close button (target, split_id)
    TabCloseButton(crate::view::split::TabTarget, LeafId),
    /// Hovering over a close split button (split_id)
    CloseSplitButton(LeafId),
    /// Hovering over a maximize/unmaximize split button (split_id)
    MaximizeSplitButton(LeafId),
    /// Hovering over the file explorer close button
    FileExplorerCloseButton,
    /// Hovering over a file explorer item's status indicator (path)
    FileExplorerStatusIndicator(std::path::PathBuf),
    /// Hovering over the status bar LSP indicator
    StatusBarLspIndicator,
    /// Hovering over the status bar remote-authority indicator
    StatusBarRemoteIndicator,
    /// Hovering over the status bar warning badge
    StatusBarWarningBadge,
    /// Hovering over the status bar line ending indicator
    StatusBarLineEndingIndicator,
    /// Hovering over the status bar encoding indicator
    StatusBarEncodingIndicator,
    /// Hovering over the status bar language indicator
    StatusBarLanguageIndicator,
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
    /// Hovering over a file explorer context menu item (item_index)
    FileExplorerContextMenuItem(usize),
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
    pub split_id: LeafId,
    /// Screen position where the menu should appear (x, y)
    pub position: (u16, u16),
    /// Currently highlighted menu item index
    pub highlighted: usize,
}

impl TabContextMenu {
    /// Create a new tab context menu
    pub fn new(buffer_id: BufferId, split_id: LeafId, x: u16, y: u16) -> Self {
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

/// File explorer context menu items
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileExplorerContextMenuItem {
    NewFile,
    NewDirectory,
    Rename,
    Cut,
    Copy,
    Paste,
    Delete,
}

impl FileExplorerContextMenuItem {
    pub fn all() -> &'static [Self] {
        &[
            Self::NewFile,
            Self::NewDirectory,
            Self::Rename,
            Self::Cut,
            Self::Copy,
            Self::Paste,
            Self::Delete,
        ]
    }

    pub fn multi_selection() -> &'static [Self] {
        &[Self::Cut, Self::Copy, Self::Paste, Self::Delete]
    }

    pub fn root_single_selection() -> &'static [Self] {
        &[Self::NewFile, Self::NewDirectory, Self::Paste]
    }

    pub fn label(&self) -> String {
        match self {
            Self::NewFile => t!("explorer.context.new_file").to_string(),
            Self::NewDirectory => t!("explorer.context.new_directory").to_string(),
            Self::Rename => t!("explorer.context.rename").to_string(),
            Self::Cut => t!("explorer.context.cut").to_string(),
            Self::Copy => t!("explorer.context.copy").to_string(),
            Self::Paste => t!("explorer.context.paste").to_string(),
            Self::Delete => t!("explorer.context.delete").to_string(),
        }
    }
}

/// State for file explorer context menu (right-click popup in the file explorer)
#[derive(Debug, Clone)]
pub struct FileExplorerContextMenu {
    /// Screen position where the menu should appear (x, y)
    pub position: (u16, u16),
    /// Currently highlighted menu item index
    pub highlighted: usize,
    /// Whether the menu was opened with multiple items selected
    pub is_multi_selection: bool,
    /// Whether the sole selected node is the project root
    pub is_root_selected: bool,
}

impl FileExplorerContextMenu {
    pub fn new(x: u16, y: u16, is_multi_selection: bool, is_root_selected: bool) -> Self {
        Self {
            position: (x, y),
            highlighted: 0,
            is_multi_selection,
            is_root_selected,
        }
    }

    pub fn items(&self) -> &'static [FileExplorerContextMenuItem] {
        if self.is_multi_selection {
            FileExplorerContextMenuItem::multi_selection()
        } else if self.is_root_selected {
            FileExplorerContextMenuItem::root_single_selection()
        } else {
            FileExplorerContextMenuItem::all()
        }
    }

    pub fn height(&self) -> u16 {
        self.items().len() as u16 + 2
    }

    pub fn clamped_position(&self, screen_width: u16, screen_height: u16) -> (u16, u16) {
        let x = if self.position.0 + FILE_EXPLORER_CONTEXT_MENU_WIDTH > screen_width {
            screen_width.saturating_sub(FILE_EXPLORER_CONTEXT_MENU_WIDTH)
        } else {
            self.position.0
        };
        let h = self.height();
        let y = if self.position.1 + h > screen_height {
            screen_height.saturating_sub(h)
        } else {
            self.position.1
        };
        (x, y)
    }

    pub fn next_item(&mut self) {
        let len = self.items().len();
        self.highlighted = (self.highlighted + 1) % len;
    }

    pub fn prev_item(&mut self) {
        let len = self.items().len();
        self.highlighted = if self.highlighted == 0 {
            len - 1
        } else {
            self.highlighted - 1
        };
    }
}

/// Lightweight per-cell theme key provenance recorded during rendering.
/// Stored in `CachedLayout::cell_theme_map` so the theme inspector popup
/// can look up the exact keys used for any screen position.
#[derive(Debug, Clone, Default)]
pub struct CellThemeInfo {
    /// Foreground theme key (e.g. "syntax.keyword", "editor.fg")
    pub fg_key: Option<&'static str>,
    /// Background theme key (e.g. "editor.bg", "diagnostic.warning_bg")
    pub bg_key: Option<&'static str>,
    /// Short region label (e.g. "Line Numbers", "Editor Content")
    pub region: &'static str,
    /// Dynamic region suffix (e.g. syntax category display name appended to "Syntax: ")
    pub syntax_category: Option<&'static str>,
}

/// Information about which theme key(s) style a specific screen position.
/// Used by the Ctrl+Right-Click theme inspector popup.
#[derive(Debug, Clone)]
pub struct ThemeKeyInfo {
    /// The foreground theme key path (e.g., "syntax.keyword", "editor.fg")
    pub fg_key: Option<String>,
    /// The background theme key path (e.g., "editor.bg", "editor.selection_bg")
    pub bg_key: Option<String>,
    /// Human-readable description of the UI region
    pub region: String,
    /// The actual foreground color value currently applied
    pub fg_color: Option<ratatui::style::Color>,
    /// The actual background color value currently applied
    pub bg_color: Option<ratatui::style::Color>,
    /// For syntax highlights: the HighlightCategory display name
    pub syntax_category: Option<String>,
}

/// State for the theme inspector popup (Ctrl+Right-Click)
#[derive(Debug, Clone)]
pub struct ThemeInfoPopup {
    /// Screen position where popup appears (x, y)
    pub position: (u16, u16),
    /// Resolved theme key information
    pub info: ThemeKeyInfo,
    /// Whether the "Open in Theme Editor" button is highlighted (mouse hover)
    pub button_highlighted: bool,
}

/// Drop zone for tab drag-and-drop
/// Indicates where a dragged tab will be placed when released
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabDropZone {
    /// Drop into an existing split's tab bar (before tab at index, or at end if None)
    /// (target_split_id, insert_index)
    TabBar(LeafId, Option<usize>),
    /// Create a new split on the left edge of the target split
    SplitLeft(LeafId),
    /// Create a new split on the right edge of the target split
    SplitRight(LeafId),
    /// Create a new split on the top edge of the target split
    SplitTop(LeafId),
    /// Create a new split on the bottom edge of the target split
    SplitBottom(LeafId),
    /// Drop into the center of a split (switch to that split's tab bar)
    SplitCenter(LeafId),
}

impl TabDropZone {
    /// Get the split ID this drop zone is associated with
    pub fn split_id(&self) -> LeafId {
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
    pub source_split_id: LeafId,
    /// Starting mouse position when drag began
    pub start_position: (u16, u16),
    /// Current mouse position
    pub current_position: (u16, u16),
    /// Currently detected drop zone (if any)
    pub drop_zone: Option<TabDropZone>,
}

impl TabDragState {
    /// Create a new tab drag state
    pub fn new(buffer_id: BufferId, source_split_id: LeafId, start_position: (u16, u16)) -> Self {
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
    pub drag_start_explorer_width: Option<crate::config::ExplorerWidth>,
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
    /// Whether we're currently selecting text in a popup (popup index)
    pub selecting_in_popup: Option<usize>,
    /// Initial composite scroll_row when starting to drag the scrollbar thumb
    /// Used for composite buffer scrollbar drag
    pub drag_start_composite_scroll_row: Option<usize>,
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

    /// Find the nearest source byte to a given visual column, searching outward.
    /// Returns the source byte at the closest valid visual column.
    pub fn nearest_source_byte(&self, goal_col: usize) -> Option<usize> {
        let width = self.visual_to_char.len();
        if width == 0 {
            return None;
        }
        // Search outward from goal_col: try +1, -1, +2, -2, ...
        for delta in 1..width {
            if goal_col + delta < width {
                if let Some(byte) = self.source_byte_at_visual_col(goal_col + delta) {
                    return Some(byte);
                }
            }
            if delta <= goal_col {
                if let Some(byte) = self.source_byte_at_visual_col(goal_col - delta) {
                    return Some(byte);
                }
            }
        }
        None
    }

    /// Check if this visual row contains the given byte position
    #[inline]
    pub fn contains_byte(&self, byte_pos: usize) -> bool {
        // A row contains a byte if it's in the char_source_bytes range
        // The first valid source byte marks the start, line_end_byte marks the end
        if let Some(first_byte) = self.char_source_bytes.iter().find_map(|b| *b) {
            byte_pos >= first_byte && byte_pos <= self.line_end_byte
        } else {
            // Empty/virtual row - only matches if byte_pos equals line_end_byte
            byte_pos == self.line_end_byte
        }
    }

    /// Get the first source byte position in this row (if any)
    #[inline]
    pub fn first_source_byte(&self) -> Option<usize> {
        self.char_source_bytes.iter().find_map(|b| *b)
    }
}

/// Type alias for popup area layout information used in mouse hit testing.
/// Fields: (popup_index, rect, inner_rect, scroll_offset, num_items, scrollbar_rect, total_lines)
pub(crate) type PopupAreaLayout = (usize, Rect, Rect, usize, usize, Option<Rect>, usize);

/// Cached layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
pub(crate) struct CachedLayout {
    /// File explorer area (if visible)
    pub file_explorer_area: Option<Rect>,
    /// Editor content area (excluding file explorer)
    pub editor_content_area: Option<Rect>,
    /// Individual split areas with their scrollbar areas and thumb positions
    /// (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end)
    pub split_areas: Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
    /// Horizontal scrollbar areas per split
    /// (split_id, buffer_id, horizontal_scrollbar_rect, max_content_width, thumb_start_col, thumb_end_col)
    pub horizontal_scrollbar_areas: Vec<(LeafId, BufferId, Rect, usize, usize, usize)>,
    /// Split separator positions for drag resize
    /// (container_id, direction, x, y, length)
    pub separator_areas: Vec<(ContainerId, SplitDirection, u16, u16, u16)>,
    /// Popup areas for mouse hit testing
    /// scrollbar_rect is Some if popup has a scrollbar
    pub popup_areas: Vec<PopupAreaLayout>,
    /// Editor-level popup areas (e.g. plugin action popups) for mouse hit
    /// testing. Stored separately from buffer popups because they're owned by
    /// `Editor.global_popups` rather than the active buffer's state.
    /// Fields: (popup_index, rect, inner_rect, scroll_offset, num_items)
    pub global_popup_areas: Vec<(usize, Rect, Rect, usize, usize)>,
    /// Suggestions area for mouse hit testing
    /// (inner_rect, scroll_start_idx, visible_count, total_count)
    pub suggestions_area: Option<(Rect, usize, usize, usize)>,
    /// Tab layouts per split for mouse interaction
    pub tab_layouts: HashMap<LeafId, crate::view::ui::tabs::TabLayout>,
    /// Close split button hit areas
    /// (split_id, row, start_col, end_col)
    pub close_split_areas: Vec<(LeafId, u16, u16, u16)>,
    /// Maximize split button hit areas
    /// (split_id, row, start_col, end_col)
    pub maximize_split_areas: Vec<(LeafId, u16, u16, u16)>,
    /// View line mappings for accurate mouse click positioning per split
    /// Maps visual row index to character position mappings
    /// Used to translate screen coordinates to buffer byte positions
    pub view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>>,
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
    /// Status bar encoding indicator area (row, start_col, end_col)
    pub status_bar_encoding_area: Option<(u16, u16, u16)>,
    /// Status bar language indicator area (row, start_col, end_col)
    pub status_bar_language_area: Option<(u16, u16, u16)>,
    /// Status bar message area (row, start_col, end_col) - clickable to show status log
    pub status_bar_message_area: Option<(u16, u16, u16)>,
    /// Status bar remote-authority indicator area (row, start_col, end_col)
    /// — clickable to open the remote-authority context menu.
    pub status_bar_remote_area: Option<(u16, u16, u16)>,
    /// Search options layout for checkbox hit testing
    pub search_options_layout: Option<crate::view::ui::status_bar::SearchOptionsLayout>,
    /// Menu bar layout for hit testing
    pub menu_layout: Option<crate::view::ui::menu::MenuLayout>,
    /// Last frame dimensions — used by recompute_layout for macro replay
    pub last_frame_width: u16,
    pub last_frame_height: u16,
    /// Per-cell theme key provenance recorded during rendering.
    /// Flat vec indexed as `row * width + col` where `width = last_frame_width`.
    pub cell_theme_map: Vec<CellThemeInfo>,
}

impl CachedLayout {
    /// Reset the cell theme map for a new frame
    pub fn reset_cell_theme_map(&mut self) {
        let total = self.last_frame_width as usize * self.last_frame_height as usize;
        self.cell_theme_map.clear();
        self.cell_theme_map.resize(total, CellThemeInfo::default());
    }

    /// Look up the theme info for a screen position
    pub fn cell_theme_at(&self, col: u16, row: u16) -> Option<&CellThemeInfo> {
        let idx = row as usize * self.last_frame_width as usize + col as usize;
        self.cell_theme_map.get(idx)
    }

    /// Find which visual row contains the given byte position for a split
    pub fn find_visual_row(&self, split_id: LeafId, byte_pos: usize) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        mappings.iter().position(|m| m.contains_byte(byte_pos))
    }

    /// Get the visual column of a byte position within its visual row
    pub fn byte_to_visual_column(&self, split_id: LeafId, byte_pos: usize) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;

        // Find the visual column that maps to this byte position
        for (visual_col, &char_idx) in row.visual_to_char.iter().enumerate() {
            if let Some(source_byte) = row.char_source_bytes.get(char_idx).and_then(|b| *b) {
                if source_byte == byte_pos {
                    return Some(visual_col);
                }
                // If we've passed the byte position, return previous column
                if source_byte > byte_pos {
                    return Some(visual_col.saturating_sub(1));
                }
            }
        }
        // Byte is at or past end of row - return column after last character
        // This handles cursor positions at end of line (e.g., after last char before newline)
        Some(row.visual_to_char.len())
    }

    /// Move by visual line using the cached mappings
    /// Returns (new_position, new_visual_column) or None if at boundary
    pub fn move_visual_line(
        &self,
        split_id: LeafId,
        current_pos: usize,
        goal_visual_col: usize,
        direction: i8, // -1 = up, 1 = down
    ) -> Option<(usize, usize)> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let current_row = self.find_visual_row(split_id, current_pos)?;

        // Walk past purely-virtual rows (e.g. markdown_compose table top/
        // bottom borders and inter-row separators).  Those rows have no
        // source mapping at all — their `char_source_bytes` are all `None`
        // and their `line_end_byte` is inherited from the adjacent content
        // row.  If MoveDown/MoveUp stopped on them the cursor would land on
        // a byte that's already at the row above's end, which in turn
        // causes Down-after-table to teleport back to an earlier position
        // (regression exposed by markdown_compose's table border feature).
        //
        // A row is "navigable" iff at least one of its visual columns maps
        // to a real source byte.  Skip entirely-virtual rows in the move
        // direction until we hit a navigable one or run off the edge.
        let mut target_row = current_row;
        let navigable = |idx: usize| -> bool {
            mappings
                .get(idx)
                .map(|m| m.char_source_bytes.iter().any(|b| b.is_some()))
                .unwrap_or(false)
        };
        loop {
            target_row = if direction < 0 {
                target_row.checked_sub(1)?
            } else {
                let next = target_row + 1;
                if next >= mappings.len() {
                    return None;
                }
                next
            };
            // Either the next row has real source content, or we've reached
            // a legitimate non-source row that the rest of the editor
            // already treats as a cursor stop (trailing empty line at EOF,
            // implicit blank final line).  In either case stop walking.
            if navigable(target_row) {
                break;
            }
            let mapping = mappings.get(target_row)?;
            let is_plugin_virtual =
                mapping.visual_to_char.is_empty() || mapping.char_source_bytes.is_empty();
            if !is_plugin_virtual {
                // The row has columns but none carry a source byte — most
                // likely a plugin-injected decoration with padding.  Keep
                // looking.
                continue;
            }
            // Empty mapping (no visual columns) is how EOF-related virtual
            // rows are represented; those are legitimate cursor stops so we
            // accept them and fall out of the loop.
            break;
        }

        let target_mapping = mappings.get(target_row)?;

        // Try to get byte at goal visual column.  If the goal column is past
        // the end of visible content, land at line_end_byte (the newline or
        // end of buffer).  If the column exists but has no source byte (e.g.
        // padding on a wrapped continuation line), search outward for the
        // nearest valid source byte at minimal visual distance.
        let new_pos = if goal_visual_col >= target_mapping.visual_to_char.len() {
            target_mapping.line_end_byte
        } else {
            target_mapping
                .source_byte_at_visual_col(goal_visual_col)
                .or_else(|| target_mapping.nearest_source_byte(goal_visual_col))
                .unwrap_or(target_mapping.line_end_byte)
        };

        Some((new_pos, goal_visual_col))
    }

    /// Get the start byte position of the visual row containing the given byte position.
    /// If the cursor is already at the visual row start and this is a wrapped continuation,
    /// moves to the previous visual row's start (within the same logical line).
    /// Get the start byte position of the visual row containing the given byte position.
    /// When `allow_advance` is true and the cursor is already at the row start,
    /// moves to the previous visual row's start.
    pub fn visual_line_start(
        &self,
        split_id: LeafId,
        byte_pos: usize,
        allow_advance: bool,
    ) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;
        let row_start = row.first_source_byte()?;

        if allow_advance && byte_pos == row_start && row_idx > 0 {
            let prev_row = mappings.get(row_idx - 1)?;
            prev_row.first_source_byte()
        } else {
            Some(row_start)
        }
    }

    /// Get the end byte position of the visual row containing the given byte position.
    /// If the cursor is already at the visual row end and the next row is a wrapped continuation,
    /// moves to the next visual row's end (within the same logical line).
    /// Get the end byte position of the visual row containing the given byte position.
    /// When `allow_advance` is true and the cursor is already at the row end,
    /// advances to the next visual row's end.
    pub fn visual_line_end(
        &self,
        split_id: LeafId,
        byte_pos: usize,
        allow_advance: bool,
    ) -> Option<usize> {
        let mappings = self.view_line_mappings.get(&split_id)?;
        let row_idx = self.find_visual_row(split_id, byte_pos)?;
        let row = mappings.get(row_idx)?;

        if allow_advance && byte_pos == row.line_end_byte && row_idx + 1 < mappings.len() {
            let next_row = mappings.get(row_idx + 1)?;
            Some(next_row.line_end_byte)
        } else {
            Some(row.line_end_byte)
        }
    }
}

/// Convert a file path to an `lsp_types::Uri`.
pub fn file_path_to_lsp_uri(path: &Path) -> Option<lsp_types::Uri> {
    fresh_core::file_uri::path_to_lsp_uri(path)
}

/// LSP-facing URI: a URI as it appears on the wire to or from a
/// language server. This is a newtype around `lsp_types::Uri`. The
/// type-system point is to force every URI that crosses the
/// editor↔LSP boundary through one of the two checked constructors:
///
///   * [`LspUri::from_host_path`] — given a host path and the active
///     authority's host↔remote translation, produces an `LspUri` that
///     carries the in-container path on container authorities (and
///     the host path everywhere else).
///   * [`LspUri::from_wire`] — wraps a raw `lsp_types::Uri` that was
///     received from the LSP server. The wrapped URI is "remote-side"
///     under a container authority and must be passed back through
///     [`LspUri::to_host_path`] before any filesystem-facing code
///     sees it.
///
/// Conversely, the only ways to extract a path are:
///
///   * [`LspUri::to_host_path`] — applies remote→host translation
///     symmetrically with `from_host_path`. This is the host-side
///     `PathBuf` filesystem APIs accept. Untranslated extraction
///     (`as_uri().path()`) is intentionally not exposed as a method —
///     callers that genuinely want the wire-side path string read
///     `as_str()` and document why a host-path interpretation isn't
///     wanted.
///
/// Storing buffer URIs in [`BufferMetadata`] as `LspUri` (not
/// `lsp_types::Uri`) keeps the cached form already translated for the
/// active authority, so the dozens of `metadata.file_uri()` call
/// sites can't accidentally ship a host URI to a container LSP.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LspUri(lsp_types::Uri);

impl LspUri {
    /// Build an LSP-facing URI from a host path, applying the
    /// authority's host→remote translation when one is set. Returns
    /// `None` for relative paths (matches the pre-newtype helper).
    pub fn from_host_path(
        path: &Path,
        translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Option<Self> {
        let mapped = translation
            .and_then(|t| t.host_to_remote(path))
            .unwrap_or_else(|| path.to_path_buf());
        fresh_core::file_uri::path_to_lsp_uri(&mapped).map(Self)
    }

    /// Wrap a raw URI received from the LSP wire. The caller must
    /// subsequently translate via [`Self::to_host_path`] before
    /// opening the file or comparing with host paths — that's the
    /// whole point of having the newtype.
    pub fn from_wire(uri: lsp_types::Uri) -> Self {
        Self(uri)
    }

    /// Borrow the underlying raw URI for serialization to the LSP
    /// wire (e.g. into JSON-RPC params). Only the LSP transport layer
    /// should call this; editor-level code never sees a bare
    /// `lsp_types::Uri`.
    pub fn as_uri(&self) -> &lsp_types::Uri {
        &self.0
    }

    /// String form, for log messages and equality comparisons against
    /// other URI strings (e.g. when matching a buffer against an
    /// incoming notification's URI). Does not strip the
    /// host-vs-container ambiguity — comparisons must be between two
    /// `LspUri`s, not between a wire URI and a host URI.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Decode this URI to a host path, applying the authority's
    /// remote→host translation when one is set. Returns `None` for
    /// non-`file://` URIs.
    pub fn to_host_path(
        &self,
        translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Option<PathBuf> {
        let raw = fresh_core::file_uri::lsp_uri_to_path(&self.0)?;
        Some(
            translation
                .and_then(|t| t.remote_to_host(&raw))
                .unwrap_or(raw),
        )
    }
}

/// Build the LSP-facing URI for a host-side `path`, applying the
/// authority's host→remote translation when one is set.
///
/// Thin shim around [`LspUri::from_host_path`] that returns the
/// inner [`lsp_types::Uri`] for the few callers (root_uri building
/// inside `LspManager`, code-action workspace folder hand-off) that
/// have to feed a raw `Uri` into a third-party API. New code should
/// prefer `LspUri::from_host_path` directly so the host-vs-LSP side
/// stays type-checked.
pub fn file_path_to_lsp_uri_with_translation(
    path: &Path,
    translation: Option<&crate::services::authority::PathTranslation>,
) -> Option<lsp_types::Uri> {
    LspUri::from_host_path(path, translation).map(|u| u.into_inner())
}

impl LspUri {
    /// Consume `self` and return the raw `lsp_types::Uri`. Reserved
    /// for the wire layer (LSP transport, lsp_types interop). Editor
    /// code uses [`Self::as_uri`] when it just needs to borrow.
    pub fn into_inner(self) -> lsp_types::Uri {
        self.0
    }
}

#[cfg(test)]
mod lsp_uri_tests {
    use super::*;
    use crate::services::authority::PathTranslation;

    fn translation() -> PathTranslation {
        PathTranslation {
            host_root: PathBuf::from("/tmp/.tmpA1B2"),
            remote_root: PathBuf::from("/workspaces/proj"),
        }
    }

    #[test]
    fn from_host_path_under_workspace_translates_to_remote_uri() {
        let host = PathBuf::from("/tmp/.tmpA1B2/src/util.py");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).expect("absolute path");
        assert_eq!(lsp_uri.as_str(), "file:///workspaces/proj/src/util.py");
    }

    #[test]
    fn from_host_path_outside_workspace_passes_through() {
        // System headers / library sources sit outside the mounted
        // workspace; translation returns `None` and the host URI is
        // shipped to the LSP unchanged. The point of the newtype is
        // just to make the decision explicit.
        let host = PathBuf::from("/usr/include/stdio.h");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).expect("absolute path");
        assert_eq!(lsp_uri.as_str(), "file:///usr/include/stdio.h");
    }

    #[test]
    fn to_host_path_under_remote_root_translates_back() {
        let wire: lsp_types::Uri = "file:///workspaces/proj/src/util.py".parse().unwrap();
        let host = LspUri::from_wire(wire)
            .to_host_path(Some(&translation()))
            .expect("file:// URI");
        assert_eq!(host, PathBuf::from("/tmp/.tmpA1B2/src/util.py"));
    }

    #[test]
    fn to_host_path_outside_remote_root_passes_through() {
        let wire: lsp_types::Uri = "file:///usr/include/stdio.h".parse().unwrap();
        let host = LspUri::from_wire(wire)
            .to_host_path(Some(&translation()))
            .expect("file:// URI");
        assert_eq!(host, PathBuf::from("/usr/include/stdio.h"));
    }

    #[test]
    fn round_trip_host_to_wire_to_host_under_workspace() {
        // The whole point of the symmetry: anything that goes out
        // through `from_host_path` must come back through
        // `to_host_path` byte-identical. This is the property the
        // editor relies on so a buffer's host file_path matches the
        // path resolved from a server-returned `Location`.
        let host = PathBuf::from("/tmp/.tmpA1B2/main.py");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).unwrap();
        let back = lsp_uri.to_host_path(Some(&translation())).unwrap();
        assert_eq!(back, host);
    }

    #[test]
    fn no_translation_is_identity() {
        let host = PathBuf::from("/some/host/path/file.rs");
        let lsp_uri = LspUri::from_host_path(&host, None).unwrap();
        assert_eq!(lsp_uri.as_str(), "file:///some/host/path/file.rs");
        let back = lsp_uri.to_host_path(None).unwrap();
        assert_eq!(back, host);
    }
}

#[cfg(test)]
mod uri_encoding_tests {
    use super::*;

    /// Helper to get a platform-appropriate absolute path for testing.
    fn abs_path(suffix: &str) -> PathBuf {
        std::env::temp_dir().join(suffix)
    }

    #[test]
    fn test_brackets_in_path() {
        let path = abs_path("MY_PROJECTS [temp]/gogame/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(
            uri.is_some(),
            "URI should be computed for path with brackets"
        );
        let uri = uri.unwrap();
        assert!(
            uri.as_str().contains("%5Btemp%5D"),
            "Brackets should be percent-encoded: {}",
            uri.as_str()
        );
    }

    #[test]
    fn test_spaces_in_path() {
        let path = abs_path("My Projects/src/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "URI should be computed for path with spaces");
    }

    #[test]
    fn test_normal_path() {
        let path = abs_path("project/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "URI should be computed for normal path");
        let s = uri.unwrap().as_str().to_string();
        assert!(s.starts_with("file:///"), "Should be a file URI: {}", s);
        assert!(
            s.ends_with("project/main.go"),
            "Should end with the path: {}",
            s
        );
    }

    #[test]
    fn test_relative_path_returns_none() {
        let path = PathBuf::from("main.go");
        assert!(file_path_to_lsp_uri(&path).is_none());
    }

    #[test]
    fn test_all_special_chars() {
        let path = abs_path("a[b]c{d}e^g`h/file.rs");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "Should handle all special characters");
        let s = uri.unwrap().as_str().to_string();
        assert!(!s.contains('['), "[ should be encoded in {}", s);
        assert!(!s.contains(']'), "] should be encoded in {}", s);
        assert!(!s.contains('{'), "{{ should be encoded in {}", s);
        assert!(!s.contains('}'), "}} should be encoded in {}", s);
        assert!(!s.contains('^'), "^ should be encoded in {}", s);
        assert!(!s.contains('`'), "` should be encoded in {}", s);
    }
}
