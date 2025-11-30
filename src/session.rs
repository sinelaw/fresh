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
//! Sessions are stored in `$XDG_DATA_HOME/fresh/sessions/{hash}.json`
//! where `{hash}` is derived from the canonical working directory path.
//!
//! ## Crash Resistance
//!
//! Uses atomic writes: write to temp file, then rename.
//! This ensures the session file is never left in a corrupted state.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::input::input_history::get_data_dir;

/// Current session file format version
pub const SESSION_VERSION: u32 = 1;

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
    Split {
        direction: SerializedSplitDirection,
        first: Box<SerializedSplitNode>,
        second: Box<SerializedSplitNode>,
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
    /// Open files in tab order (paths relative to working_dir)
    pub open_files: Vec<PathBuf>,

    /// Active file index in open_files
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
}

impl Default for FileExplorerState {
    fn default() -> Self {
        Self {
            visible: false,
            width_percent: 0.3,
            expanded_dirs: Vec::new(),
            scroll_offset: 0,
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

// ============================================================================
// Session file management
// ============================================================================

/// Get the sessions directory
pub fn get_sessions_dir() -> io::Result<PathBuf> {
    Ok(get_data_dir()?.join("sessions"))
}

/// Get the session file path for a working directory
pub fn get_session_path(working_dir: &Path) -> io::Result<PathBuf> {
    let canonical = working_dir.canonicalize().unwrap_or_else(|_| working_dir.to_path_buf());
    let mut hasher = Sha256::new();
    hasher.update(canonical.to_string_lossy().as_bytes());
    let hash = format!("{:x}", hasher.finalize());
    let filename = format!("{}.json", &hash[..16]);
    Ok(get_sessions_dir()?.join(filename))
}

/// Session error types
#[derive(Debug)]
pub enum SessionError {
    Io(io::Error),
    Json(serde_json::Error),
    WorkdirMismatch { expected: PathBuf, found: PathBuf },
    VersionTooNew { version: u32, max_supported: u32 },
}

impl std::fmt::Display for SessionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SessionError::Io(e) => write!(f, "IO error: {}", e),
            SessionError::Json(e) => write!(f, "JSON error: {}", e),
            SessionError::WorkdirMismatch { expected, found } => {
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
            SessionError::Io(e) => Some(e),
            SessionError::Json(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for SessionError {
    fn from(e: io::Error) -> Self {
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
        if !path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(&path)?;
        let session: Session = serde_json::from_str(&content)?;

        // Validate working_dir matches (canonicalize both for comparison)
        let expected = working_dir
            .canonicalize()
            .unwrap_or_else(|_| working_dir.to_path_buf());
        let found = session
            .working_dir
            .canonicalize()
            .unwrap_or_else(|_| session.working_dir.clone());

        if expected != found {
            return Err(SessionError::WorkdirMismatch { expected, found });
        }

        // Check version compatibility
        if session.version > SESSION_VERSION {
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

        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Serialize to JSON
        let content = serde_json::to_string_pretty(self)?;

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
    fn test_session_path_hashing() {
        let path1 = get_session_path(Path::new("/home/user/project")).unwrap();
        let path2 = get_session_path(Path::new("/home/user/other")).unwrap();

        // Different paths should give different hashes
        assert_ne!(path1, path2);

        // Same path should give same hash
        let path1_again = get_session_path(Path::new("/home/user/project")).unwrap();
        assert_eq!(path1, path1_again);

        // Filename should be 16 hex chars + .json
        let filename = path1.file_name().unwrap().to_str().unwrap();
        assert!(filename.ends_with(".json"));
        assert_eq!(filename.len(), 16 + 5); // 16 hex + ".json"
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
}
