//! Session persistence for per-project editor state
//!
//! Saves and restores:
//! - Split layout and open files
//! - Cursor and scroll positions per split per file
//! - File explorer state
//! - Search/replace history and options
//! - Bookmarks
//!
//! ## Storage
//!
//! Sessions are stored in `$XDG_DATA_HOME/fresh/sessions/{encoded_path}.json`
//! where `{encoded_path}` is the working directory path with:
//! - Path separators (`/`) replaced with underscores (`_`)
//! - Special characters percent-encoded as `%XX`
//!
//! Example: `/home/user/my project` becomes `home_user_my%20project.json`
//!
//! The encoding is fully reversible using `decode_filename_to_path()`.
//!
//! ## Crash Resistance
//!
//! Uses atomic writes: write to temp file, then rename.
//! This ensures the session file is never left in a corrupted state.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::input::input_history::get_data_dir;

/// Current session file format version
pub const SESSION_VERSION: u32 = 1;

/// Current per-file session version
pub const FILE_SESSION_VERSION: u32 = 1;

/// Persisted session state for a working directory
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    /// Schema version for future migrations
    pub version: u32,

    /// Working directory this session belongs to (for validation)
    pub working_dir: PathBuf,

    /// Split layout tree
    pub split_layout: SerializedSplitNode,

    /// Active split ID
    pub active_split_id: usize,

    /// Per-split view states (keyed by split_id)
    pub split_states: HashMap<usize, SerializedSplitViewState>,

    /// Editor config overrides (toggles that differ from defaults)
    #[serde(default)]
    pub config_overrides: SessionConfigOverrides,

    /// File explorer state
    pub file_explorer: FileExplorerState,

    /// Input histories (search, replace, command palette, etc.)
    #[serde(default)]
    pub histories: SessionHistories,

    /// Search options (persist across searches within session)
    #[serde(default)]
    pub search_options: SearchOptions,

    /// Bookmarks (character key -> file position)
    #[serde(default)]
    pub bookmarks: HashMap<char, SerializedBookmark>,

    /// Open terminal sessions (for restoration)
    #[serde(default)]
    pub terminals: Vec<SerializedTerminalSession>,

    /// External files open in the session (files outside working_dir)
    /// These are stored as absolute paths since they can't be made relative
    #[serde(default)]
    pub external_files: Vec<PathBuf>,

    /// Timestamp when session was saved (Unix epoch seconds)
    pub saved_at: u64,
}

/// Serializable split layout (mirrors SplitNode but with file paths instead of buffer IDs)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SerializedSplitNode {
    Leaf {
        /// File path relative to working_dir (None for scratch buffers)
        file_path: Option<PathBuf>,
        split_id: usize,
    },
    Terminal {
        terminal_index: usize,
        split_id: usize,
    },
    Split {
        direction: SerializedSplitDirection,
        first: Box<Self>,
        second: Box<Self>,
        ratio: f32,
        split_id: usize,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SerializedSplitDirection {
    Horizontal,
    Vertical,
}

/// Per-split view state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedSplitViewState {
    /// Open tabs in tab order (files or terminals)
    #[serde(default)]
    pub open_tabs: Vec<SerializedTabRef>,

    /// Active tab index in open_tabs (if present)
    #[serde(default)]
    pub active_tab_index: Option<usize>,

    /// Open files in tab order (paths relative to working_dir)
    /// Deprecated; retained for backward compatibility.
    #[serde(default)]
    pub open_files: Vec<PathBuf>,

    /// Active file index in open_files
    #[serde(default)]
    pub active_file_index: usize,

    /// Per-file cursor and scroll state
    #[serde(default)]
    pub file_states: HashMap<PathBuf, SerializedFileState>,

    /// Tab scroll offset
    #[serde(default)]
    pub tab_scroll_offset: usize,

    /// View mode
    #[serde(default)]
    pub view_mode: SerializedViewMode,

    /// Compose width if in compose mode
    #[serde(default)]
    pub compose_width: Option<u16>,
}

/// Per-file state within a split
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedFileState {
    /// Primary cursor position (byte offset)
    pub cursor: SerializedCursor,

    /// Additional cursors for multi-cursor
    #[serde(default)]
    pub additional_cursors: Vec<SerializedCursor>,

    /// Scroll position (byte offset)
    pub scroll: SerializedScroll,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedCursor {
    /// Cursor position as byte offset from start of file
    pub position: usize,
    /// Selection anchor as byte offset (if selection active)
    #[serde(default)]
    pub anchor: Option<usize>,
    /// Sticky column for vertical movement (character column)
    #[serde(default)]
    pub sticky_column: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedScroll {
    /// Top visible position as byte offset
    pub top_byte: usize,
    /// Virtual line offset within the top line (for wrapped lines)
    #[serde(default)]
    pub top_view_line_offset: usize,
    /// Left column offset (for horizontal scroll)
    #[serde(default)]
    pub left_column: usize,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub enum SerializedViewMode {
    #[default]
    Source,
    Compose,
}

/// Config overrides that differ from base config
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SessionConfigOverrides {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line_numbers: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub relative_line_numbers: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line_wrap: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub syntax_highlighting: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enable_inlay_hints: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mouse_enabled: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub menu_bar_hidden: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileExplorerState {
    pub visible: bool,
    #[serde(default)]
    pub width_percent: f32,
    /// Expanded directories (relative paths)
    #[serde(default)]
    pub expanded_dirs: Vec<PathBuf>,
    /// Scroll offset
    #[serde(default)]
    pub scroll_offset: usize,
    /// Show hidden files (fixes #569)
    #[serde(default)]
    pub show_hidden: bool,
    /// Show gitignored files (fixes #569)
    #[serde(default)]
    pub show_gitignored: bool,
}

impl Default for FileExplorerState {
    fn default() -> Self {
        Self {
            visible: false,
            width_percent: 0.3,
            expanded_dirs: Vec::new(),
            scroll_offset: 0,
            show_hidden: false,
            show_gitignored: false,
        }
    }
}

/// Per-session input histories
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SessionHistories {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub search: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub replace: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub command_palette: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub goto_line: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub open_file: Vec<String>,
}

/// Search options that persist across searches within a session
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchOptions {
    #[serde(default)]
    pub case_sensitive: bool,
    #[serde(default)]
    pub whole_word: bool,
    #[serde(default)]
    pub use_regex: bool,
    #[serde(default)]
    pub confirm_each: bool,
}

/// Serialized bookmark (file path + byte offset)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedBookmark {
    /// File path (relative to working_dir)
    pub file_path: PathBuf,
    /// Byte offset position in the file
    pub position: usize,
}

/// Reference to an open tab (file path or terminal index)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SerializedTabRef {
    File(PathBuf),
    Terminal(usize),
}

/// Persisted metadata for a terminal session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedTerminalSession {
    pub terminal_index: usize,
    pub cwd: Option<PathBuf>,
    pub shell: String,
    pub cols: u16,
    pub rows: u16,
    pub log_path: PathBuf,
    pub backing_path: PathBuf,
}

// ============================================================================
// Global file state persistence (per-file, not per-project)
// ============================================================================

/// Individual file state stored in its own file
///
/// Each source file's scroll/cursor state is stored in a separate JSON file
/// at `$XDG_DATA_HOME/fresh/file_states/{encoded_path}.json`.
/// This allows concurrent editors to safely update different files without
/// conflicts.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistedFileState {
    /// Schema version for future migrations
    pub version: u32,

    /// The file state (cursor, scroll, etc.)
    pub state: SerializedFileState,

    /// Timestamp when last saved (Unix epoch seconds)
    pub saved_at: u64,
}

impl PersistedFileState {
    fn new(state: SerializedFileState) -> Self {
        Self {
            version: FILE_SESSION_VERSION,
            state,
            saved_at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }
}

/// Per-file session storage for scroll/cursor positions
///
/// Unlike project sessions which store file states relative to a working directory,
/// this stores file states by absolute path so they persist across projects.
/// This means opening the same file from different projects (or without a project)
/// will restore the same scroll/cursor position.
///
/// Each file's state is stored in a separate JSON file at
/// `$XDG_DATA_HOME/fresh/file_states/{encoded_path}.json` to avoid conflicts
/// between concurrent editors. States are loaded lazily when opening files
/// and saved immediately when closing files or saving the session.
pub struct PersistedFileSession;

impl PersistedFileSession {
    /// Get the directory for file state files
    fn states_dir() -> io::Result<PathBuf> {
        Ok(get_data_dir()?.join("file_states"))
    }

    /// Get the state file path for a source file
    fn state_file_path(source_path: &Path) -> io::Result<PathBuf> {
        let canonical = source_path
            .canonicalize()
            .unwrap_or_else(|_| source_path.to_path_buf());
        let filename = format!("{}.json", encode_path_for_filename(&canonical));
        Ok(Self::states_dir()?.join(filename))
    }

    /// Load the state for a file by its absolute path (from disk)
    pub fn load(path: &Path) -> Option<SerializedFileState> {
        let state_path = match Self::state_file_path(path) {
            Ok(p) => p,
            Err(_) => return None,
        };

        if !state_path.exists() {
            return None;
        }

        let content = match std::fs::read_to_string(&state_path) {
            Ok(c) => c,
            Err(_) => return None,
        };

        let persisted: PersistedFileState = match serde_json::from_str(&content) {
            Ok(p) => p,
            Err(_) => return None,
        };

        // Check version compatibility
        if persisted.version > FILE_SESSION_VERSION {
            return None;
        }

        Some(persisted.state)
    }

    /// Save the state for a file by its absolute path (to disk, atomic write)
    pub fn save(path: &Path, state: SerializedFileState) {
        let state_path = match Self::state_file_path(path) {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!("Failed to get state path for {:?}: {}", path, e);
                return;
            }
        };

        // Ensure directory exists
        if let Some(parent) = state_path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                tracing::warn!("Failed to create state dir: {}", e);
                return;
            }
        }

        let persisted = PersistedFileState::new(state);
        let content = match serde_json::to_string_pretty(&persisted) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!("Failed to serialize file state: {}", e);
                return;
            }
        };

        // Write atomically: temp file + rename
        let temp_path = state_path.with_extension("json.tmp");

        let write_result = (|| -> io::Result<()> {
            let mut file = std::fs::File::create(&temp_path)?;
            file.write_all(content.as_bytes())?;
            file.sync_all()?;
            std::fs::rename(&temp_path, &state_path)?;
            Ok(())
        })();

        if let Err(e) = write_result {
            tracing::warn!("Failed to save file state for {:?}: {}", path, e);
        } else {
            tracing::trace!("File state saved for {:?}", path);
        }
    }
}

// ============================================================================
// Session file management
// ============================================================================

/// Get the sessions directory
pub fn get_sessions_dir() -> io::Result<PathBuf> {
    Ok(get_data_dir()?.join("sessions"))
}

/// Encode a path into a filesystem-safe filename using percent encoding
///
/// Keeps alphanumeric chars, `-`, `.`, `_` as-is.
/// Replaces `/` with `_` for readability.
/// Percent-encodes other special characters as %XX.
///
/// Example: `/home/user/my project` -> `home_user_my%20project`
pub fn encode_path_for_filename(path: &Path) -> String {
    let path_str = path.to_string_lossy();
    let mut result = String::with_capacity(path_str.len() * 2);

    for c in path_str.chars() {
        match c {
            // Path separators become underscores for readability
            '/' | '\\' => result.push('_'),
            // Safe chars pass through
            c if c.is_ascii_alphanumeric() => result.push(c),
            '-' | '.' => result.push(c),
            // Underscore needs special handling to avoid collision with /
            '_' => result.push_str("%5F"),
            // Everything else gets percent-encoded
            c => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("%{:02X}", byte));
                }
            }
        }
    }

    // Remove leading underscores (from leading /)
    let result = result.trim_start_matches('_').to_string();

    // Collapse multiple underscores
    let mut final_result = String::with_capacity(result.len());
    let mut last_was_underscore = false;
    for c in result.chars() {
        if c == '_' {
            if !last_was_underscore {
                final_result.push(c);
            }
            last_was_underscore = true;
        } else {
            final_result.push(c);
            last_was_underscore = false;
        }
    }

    if final_result.is_empty() {
        final_result = "root".to_string();
    }

    final_result
}

/// Decode a filename back to the original path (for debugging/tooling)
#[allow(dead_code)]
pub fn decode_filename_to_path(encoded: &str) -> Option<PathBuf> {
    if encoded == "root" {
        return Some(PathBuf::from("/"));
    }

    let mut result = String::with_capacity(encoded.len() + 1);
    // Re-add leading slash that was stripped during encoding
    result.push('/');

    let mut chars = encoded.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            // Read two hex digits
            let hex: String = chars.by_ref().take(2).collect();
            if hex.len() == 2 {
                if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                    result.push(byte as char);
                }
            }
        } else if c == '_' {
            result.push('/');
        } else {
            result.push(c);
        }
    }

    Some(PathBuf::from(result))
}

/// Get the session file path for a working directory
pub fn get_session_path(working_dir: &Path) -> io::Result<PathBuf> {
    let canonical = working_dir
        .canonicalize()
        .unwrap_or_else(|_| working_dir.to_path_buf());
    let filename = format!("{}.json", encode_path_for_filename(&canonical));
    Ok(get_sessions_dir()?.join(filename))
}

/// Session error types
#[derive(Debug)]
pub enum SessionError {
    Io(anyhow::Error),
    Json(serde_json::Error),
    WorkdirMismatch { expected: PathBuf, found: PathBuf },
    VersionTooNew { version: u32, max_supported: u32 },
}

impl std::fmt::Display for SessionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "Session error: {}", e),
            Self::Json(e) => write!(f, "JSON error: {}", e),
            Self::WorkdirMismatch { expected, found } => {
                write!(
                    f,
                    "Working directory mismatch: expected {:?}, found {:?}",
                    expected, found
                )
            }
            SessionError::VersionTooNew {
                version,
                max_supported,
            } => {
                write!(
                    f,
                    "Session version {} is newer than supported (max: {})",
                    version, max_supported
                )
            }
        }
    }
}

impl std::error::Error for SessionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => e.source(),
            Self::Json(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for SessionError {
    fn from(e: io::Error) -> Self {
        SessionError::Io(e.into())
    }
}

impl From<anyhow::Error> for SessionError {
    fn from(e: anyhow::Error) -> Self {
        SessionError::Io(e)
    }
}

impl From<serde_json::Error> for SessionError {
    fn from(e: serde_json::Error) -> Self {
        SessionError::Json(e)
    }
}

impl Session {
    /// Load session for a working directory (if exists)
    pub fn load(working_dir: &Path) -> Result<Option<Session>, SessionError> {
        let path = get_session_path(working_dir)?;
        tracing::debug!("Looking for session at {:?}", path);

        if !path.exists() {
            tracing::debug!("Session file does not exist");
            return Ok(None);
        }

        tracing::debug!("Loading session from {:?}", path);
        let content = std::fs::read_to_string(&path)?;
        let session: Session = serde_json::from_str(&content)?;

        tracing::debug!(
            "Loaded session: version={}, split_states={}, active_split={}",
            session.version,
            session.split_states.len(),
            session.active_split_id
        );

        // Validate working_dir matches (canonicalize both for comparison)
        let expected = working_dir
            .canonicalize()
            .unwrap_or_else(|_| working_dir.to_path_buf());
        let found = session
            .working_dir
            .canonicalize()
            .unwrap_or_else(|_| session.working_dir.clone());

        if expected != found {
            tracing::warn!(
                "Session working_dir mismatch: expected {:?}, found {:?}",
                expected,
                found
            );
            return Err(SessionError::WorkdirMismatch { expected, found });
        }

        // Check version compatibility
        if session.version > SESSION_VERSION {
            tracing::warn!(
                "Session version {} is newer than supported {}",
                session.version,
                SESSION_VERSION
            );
            return Err(SessionError::VersionTooNew {
                version: session.version,
                max_supported: SESSION_VERSION,
            });
        }

        Ok(Some(session))
    }

    /// Save session to file using atomic write (temp file + rename)
    ///
    /// This ensures the session file is never left in a corrupted state:
    /// 1. Write to a temporary file in the same directory
    /// 2. Sync to disk (fsync)
    /// 3. Atomically rename to the final path
    pub fn save(&self) -> Result<(), SessionError> {
        let path = get_session_path(&self.working_dir)?;
        tracing::debug!("Saving session to {:?}", path);

        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Serialize to JSON
        let content = serde_json::to_string_pretty(self)?;
        tracing::trace!("Session JSON size: {} bytes", content.len());

        // Write atomically: temp file + rename
        let temp_path = path.with_extension("json.tmp");

        // Write to temp file
        {
            let mut file = std::fs::File::create(&temp_path)?;
            file.write_all(content.as_bytes())?;
            file.sync_all()?; // Ensure data is on disk before rename
        }

        // Atomic rename
        std::fs::rename(&temp_path, &path)?;
        tracing::info!("Session saved to {:?}", path);

        Ok(())
    }

    /// Delete session for a working directory
    pub fn delete(working_dir: &Path) -> Result<(), SessionError> {
        let path = get_session_path(working_dir)?;
        if path.exists() {
            std::fs::remove_file(path)?;
        }
        Ok(())
    }

    /// Create a new session with current timestamp
    pub fn new(working_dir: PathBuf) -> Self {
        Self {
            version: SESSION_VERSION,
            working_dir,
            split_layout: SerializedSplitNode::Leaf {
                file_path: None,
                split_id: 0,
            },
            active_split_id: 0,
            split_states: HashMap::new(),
            config_overrides: SessionConfigOverrides::default(),
            file_explorer: FileExplorerState::default(),
            histories: SessionHistories::default(),
            search_options: SearchOptions::default(),
            bookmarks: HashMap::new(),
            terminals: Vec::new(),
            external_files: Vec::new(),
            saved_at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    /// Update the saved_at timestamp to now
    pub fn touch(&mut self) {
        self.saved_at = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_path_percent_encoding() {
        // Test basic path encoding - readable with underscores for separators
        let encoded = encode_path_for_filename(Path::new("/home/user/project"));
        assert_eq!(encoded, "home_user_project");
        assert!(!encoded.contains('/')); // No slashes in encoded output

        // Round-trip: encode then decode should give original path
        let decoded = decode_filename_to_path(&encoded).unwrap();
        assert_eq!(decoded, PathBuf::from("/home/user/project"));

        // Different paths should give different encodings
        let path1 = get_session_path(Path::new("/home/user/project")).unwrap();
        let path2 = get_session_path(Path::new("/home/user/other")).unwrap();
        assert_ne!(path1, path2);

        // Same path should give same encoding
        let path1_again = get_session_path(Path::new("/home/user/project")).unwrap();
        assert_eq!(path1, path1_again);

        // Filename should end with .json and be readable
        let filename = path1.file_name().unwrap().to_str().unwrap();
        assert!(filename.ends_with(".json"));
        assert!(filename.starts_with("home_user_project"));
    }

    #[test]
    fn test_percent_encoding_edge_cases() {
        // Path with dashes (should pass through)
        let encoded = encode_path_for_filename(Path::new("/home/user/my-project"));
        assert_eq!(encoded, "home_user_my-project");

        // Path with spaces (percent-encoded)
        let encoded = encode_path_for_filename(Path::new("/home/user/my project"));
        assert_eq!(encoded, "home_user_my%20project");
        let decoded = decode_filename_to_path(&encoded).unwrap();
        assert_eq!(decoded, PathBuf::from("/home/user/my project"));

        // Path with underscores (percent-encoded to avoid collision with /)
        let encoded = encode_path_for_filename(Path::new("/home/user/my_project"));
        assert_eq!(encoded, "home_user_my%5Fproject");
        let decoded = decode_filename_to_path(&encoded).unwrap();
        assert_eq!(decoded, PathBuf::from("/home/user/my_project"));

        // Root path
        let encoded = encode_path_for_filename(Path::new("/"));
        assert_eq!(encoded, "root");
    }

    #[test]
    fn test_session_serialization() {
        let session = Session::new(PathBuf::from("/home/user/test"));
        let json = serde_json::to_string(&session).unwrap();
        let restored: Session = serde_json::from_str(&json).unwrap();

        assert_eq!(session.version, restored.version);
        assert_eq!(session.working_dir, restored.working_dir);
    }

    #[test]
    fn test_session_config_overrides_skip_none() {
        let overrides = SessionConfigOverrides::default();
        let json = serde_json::to_string(&overrides).unwrap();

        // Empty overrides should serialize to empty object
        assert_eq!(json, "{}");
    }

    #[test]
    fn test_session_config_overrides_with_values() {
        let overrides = SessionConfigOverrides {
            line_wrap: Some(false),
            ..Default::default()
        };
        let json = serde_json::to_string(&overrides).unwrap();

        assert!(json.contains("line_wrap"));
        assert!(!json.contains("line_numbers")); // None values skipped
    }

    #[test]
    fn test_split_layout_serialization() {
        // Create a nested split layout
        let layout = SerializedSplitNode::Split {
            direction: SerializedSplitDirection::Vertical,
            first: Box::new(SerializedSplitNode::Leaf {
                file_path: Some(PathBuf::from("src/main.rs")),
                split_id: 1,
            }),
            second: Box::new(SerializedSplitNode::Leaf {
                file_path: Some(PathBuf::from("src/lib.rs")),
                split_id: 2,
            }),
            ratio: 0.5,
            split_id: 0,
        };

        let json = serde_json::to_string(&layout).unwrap();
        let restored: SerializedSplitNode = serde_json::from_str(&json).unwrap();

        // Verify the restored layout matches
        match restored {
            SerializedSplitNode::Split {
                direction,
                ratio,
                split_id,
                ..
            } => {
                assert!(matches!(direction, SerializedSplitDirection::Vertical));
                assert_eq!(ratio, 0.5);
                assert_eq!(split_id, 0);
            }
            _ => panic!("Expected Split node"),
        }
    }

    #[test]
    fn test_file_state_serialization() {
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: 1234,
                anchor: Some(1000),
                sticky_column: 15,
            },
            additional_cursors: vec![SerializedCursor {
                position: 5000,
                anchor: None,
                sticky_column: 0,
            }],
            scroll: SerializedScroll {
                top_byte: 500,
                top_view_line_offset: 2,
                left_column: 10,
            },
        };

        let json = serde_json::to_string(&file_state).unwrap();
        let restored: SerializedFileState = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.cursor.position, 1234);
        assert_eq!(restored.cursor.anchor, Some(1000));
        assert_eq!(restored.cursor.sticky_column, 15);
        assert_eq!(restored.additional_cursors.len(), 1);
        assert_eq!(restored.scroll.top_byte, 500);
        assert_eq!(restored.scroll.left_column, 10);
    }

    #[test]
    fn test_bookmark_serialization() {
        let mut bookmarks = HashMap::new();
        bookmarks.insert(
            'a',
            SerializedBookmark {
                file_path: PathBuf::from("src/main.rs"),
                position: 1234,
            },
        );
        bookmarks.insert(
            'b',
            SerializedBookmark {
                file_path: PathBuf::from("src/lib.rs"),
                position: 5678,
            },
        );

        let json = serde_json::to_string(&bookmarks).unwrap();
        let restored: HashMap<char, SerializedBookmark> = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.len(), 2);
        assert_eq!(restored.get(&'a').unwrap().position, 1234);
        assert_eq!(
            restored.get(&'b').unwrap().file_path,
            PathBuf::from("src/lib.rs")
        );
    }

    #[test]
    fn test_search_options_serialization() {
        let options = SearchOptions {
            case_sensitive: true,
            whole_word: true,
            use_regex: false,
            confirm_each: true,
        };

        let json = serde_json::to_string(&options).unwrap();
        let restored: SearchOptions = serde_json::from_str(&json).unwrap();

        assert!(restored.case_sensitive);
        assert!(restored.whole_word);
        assert!(!restored.use_regex);
        assert!(restored.confirm_each);
    }

    #[test]
    fn test_full_session_round_trip() {
        let mut session = Session::new(PathBuf::from("/home/user/myproject"));

        // Configure split layout
        session.split_layout = SerializedSplitNode::Split {
            direction: SerializedSplitDirection::Horizontal,
            first: Box::new(SerializedSplitNode::Leaf {
                file_path: Some(PathBuf::from("README.md")),
                split_id: 1,
            }),
            second: Box::new(SerializedSplitNode::Leaf {
                file_path: Some(PathBuf::from("Cargo.toml")),
                split_id: 2,
            }),
            ratio: 0.6,
            split_id: 0,
        };
        session.active_split_id = 1;

        // Add split state
        session.split_states.insert(
            1,
            SerializedSplitViewState {
                open_tabs: vec![
                    SerializedTabRef::File(PathBuf::from("README.md")),
                    SerializedTabRef::File(PathBuf::from("src/lib.rs")),
                ],
                active_tab_index: Some(0),
                open_files: vec![PathBuf::from("README.md"), PathBuf::from("src/lib.rs")],
                active_file_index: 0,
                file_states: HashMap::new(),
                tab_scroll_offset: 0,
                view_mode: SerializedViewMode::Source,
                compose_width: None,
            },
        );

        // Add bookmarks
        session.bookmarks.insert(
            'm',
            SerializedBookmark {
                file_path: PathBuf::from("src/main.rs"),
                position: 100,
            },
        );

        // Set search options
        session.search_options.case_sensitive = true;
        session.search_options.use_regex = true;

        // Serialize and deserialize
        let json = serde_json::to_string_pretty(&session).unwrap();
        let restored: Session = serde_json::from_str(&json).unwrap();

        // Verify everything matches
        assert_eq!(restored.version, SESSION_VERSION);
        assert_eq!(restored.working_dir, PathBuf::from("/home/user/myproject"));
        assert_eq!(restored.active_split_id, 1);
        assert!(restored.bookmarks.contains_key(&'m'));
        assert!(restored.search_options.case_sensitive);
        assert!(restored.search_options.use_regex);

        // Verify split state
        let split_state = restored.split_states.get(&1).unwrap();
        assert_eq!(split_state.open_files.len(), 2);
        assert_eq!(split_state.open_files[0], PathBuf::from("README.md"));
    }

    #[test]
    fn test_session_file_save_load() {
        use std::fs;

        // Create a temporary directory for testing
        let temp_dir = std::env::temp_dir().join("fresh_session_test");
        let _ = fs::remove_dir_all(&temp_dir); // Clean up from previous runs
        fs::create_dir_all(&temp_dir).unwrap();

        let session_path = temp_dir.join("test_session.json");

        // Create a session
        let mut session = Session::new(temp_dir.clone());
        session.search_options.case_sensitive = true;
        session.bookmarks.insert(
            'x',
            SerializedBookmark {
                file_path: PathBuf::from("test.txt"),
                position: 42,
            },
        );

        // Save it directly to test path
        let content = serde_json::to_string_pretty(&session).unwrap();
        let temp_path = session_path.with_extension("json.tmp");
        let mut file = std::fs::File::create(&temp_path).unwrap();
        std::io::Write::write_all(&mut file, content.as_bytes()).unwrap();
        file.sync_all().unwrap();
        std::fs::rename(&temp_path, &session_path).unwrap();

        // Load it back
        let loaded_content = fs::read_to_string(&session_path).unwrap();
        let loaded: Session = serde_json::from_str(&loaded_content).unwrap();

        // Verify
        assert_eq!(loaded.working_dir, temp_dir);
        assert!(loaded.search_options.case_sensitive);
        assert_eq!(loaded.bookmarks.get(&'x').unwrap().position, 42);

        // Cleanup
        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_session_version_check() {
        let session = Session::new(PathBuf::from("/test"));
        assert_eq!(session.version, SESSION_VERSION);

        // Serialize with a future version number
        let mut json_value: serde_json::Value = serde_json::to_value(&session).unwrap();
        json_value["version"] = serde_json::json!(999);

        let json = serde_json::to_string(&json_value).unwrap();
        let restored: Session = serde_json::from_str(&json).unwrap();

        // Should still deserialize, but version is 999
        assert_eq!(restored.version, 999);
    }

    #[test]
    fn test_empty_session_histories() {
        let histories = SessionHistories::default();
        let json = serde_json::to_string(&histories).unwrap();

        // Empty histories should serialize to empty object (due to skip_serializing_if)
        assert_eq!(json, "{}");

        // But should deserialize back correctly
        let restored: SessionHistories = serde_json::from_str(&json).unwrap();
        assert!(restored.search.is_empty());
        assert!(restored.replace.is_empty());
    }

    #[test]
    fn test_file_explorer_state() {
        let state = FileExplorerState {
            visible: true,
            width_percent: 0.25,
            expanded_dirs: vec![
                PathBuf::from("src"),
                PathBuf::from("src/app"),
                PathBuf::from("tests"),
            ],
            scroll_offset: 5,
            show_hidden: true,
            show_gitignored: false,
        };

        let json = serde_json::to_string(&state).unwrap();
        let restored: FileExplorerState = serde_json::from_str(&json).unwrap();

        assert!(restored.visible);
        assert_eq!(restored.width_percent, 0.25);
        assert_eq!(restored.expanded_dirs.len(), 3);
        assert_eq!(restored.scroll_offset, 5);
        assert!(restored.show_hidden);
        assert!(!restored.show_gitignored);
    }
}
