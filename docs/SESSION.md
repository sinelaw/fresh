# Session Persistence Feature Design

## Overview

This feature saves and restores editor state (open files, splits, cursors, scroll positions, toggles) per working directory, enabling seamless project resumption.

## 1. Session State Schema

**New file: `src/session.rs`**

```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

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
    pub config_overrides: SessionConfigOverrides,

    /// File explorer state
    pub file_explorer: FileExplorerState,

    /// Input histories (search, replace, command palette, etc.)
    pub histories: SessionHistories,

    /// Search options (persist across searches within session)
    pub search_options: SearchOptions,

    /// Bookmarks (character key -> file position)
    pub bookmarks: HashMap<char, SerializedBookmark>,

    /// Timestamp when session was saved
    pub saved_at: u64,
}

/// Serializable split layout (mirrors SplitNode)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SerializedSplitNode {
    Leaf {
        /// File path (not buffer_id - those are runtime)
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
    pub file_states: HashMap<PathBuf, SerializedFileState>,

    /// Tab scroll offset
    pub tab_scroll_offset: usize,

    /// View mode
    pub view_mode: SerializedViewMode,

    /// Compose width if in compose mode
    pub compose_width: Option<u16>,
}

/// Per-file state within a split
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedFileState {
    /// Primary cursor position (byte offset)
    pub cursor: SerializedCursor,

    /// Additional cursors for multi-cursor
    pub additional_cursors: Vec<SerializedCursor>,

    /// Scroll position (byte offset)
    pub scroll: SerializedScroll,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedCursor {
    /// Cursor position as byte offset from start of file
    pub position: usize,
    /// Selection anchor as byte offset (if selection active)
    pub anchor: Option<usize>,
    /// Sticky column for vertical movement (character column)
    pub sticky_column: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedScroll {
    /// Top visible position as byte offset
    pub top_byte: usize,
    /// Virtual line offset within the top line (for wrapped lines)
    pub top_view_line_offset: usize,
    /// Left column offset (for horizontal scroll)
    pub left_column: usize,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SerializedViewMode {
    Source,
    Compose,
}

/// Config overrides that differ from base config
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SessionConfigOverrides {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_numbers: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relative_line_numbers: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_wrap: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub syntax_highlighting: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_inlay_hints: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mouse_enabled: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileExplorerState {
    pub visible: bool,
    pub width: u16,
    /// Expanded directories (relative paths)
    pub expanded_dirs: Vec<PathBuf>,
    /// Scroll offset
    pub scroll_offset: usize,
}

/// Per-session input histories
/// These override the global histories when working in this project
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SessionHistories {
    /// Search history (find in file)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub search: Vec<String>,

    /// Replace history (find & replace)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub replace: Vec<String>,

    /// Command palette history (recently used commands)
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub command_palette: Vec<String>,

    /// Goto line history
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub goto_line: Vec<String>,

    /// Open file prompt history
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub open_file: Vec<String>,
}

/// Search options that persist across searches within a session
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchOptions {
    pub case_sensitive: bool,
    pub whole_word: bool,
    pub use_regex: bool,
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
```

## 2. File Storage Location

Session files are stored in the XDG data directory with hashed workdir names:

```
~/.local/share/fresh/sessions/
├── {hash1}.json    # Session for /home/user/project-a
├── {hash2}.json    # Session for /home/user/project-b
└── index.json      # Optional: maps hashes to paths for debugging
```

**Hash Strategy:**
- Use SHA-256 of canonicalized absolute path
- Truncate to 16 hex chars for filename
- Store full path inside JSON for validation

```rust
// In src/session.rs

use std::path::Path;
use sha2::{Sha256, Digest};

pub fn get_sessions_dir() -> std::io::Result<PathBuf> {
    let data_dir = dirs::data_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine data directory",
        )
    })?;
    Ok(data_dir.join("fresh").join("sessions"))
}

pub fn get_session_path(working_dir: &Path) -> std::io::Result<PathBuf> {
    let canonical = working_dir.canonicalize()?;
    let mut hasher = Sha256::new();
    hasher.update(canonical.to_string_lossy().as_bytes());
    let hash = format!("{:x}", hasher.finalize());
    let filename = format!("{}.json", &hash[..16]);
    Ok(get_sessions_dir()?.join(filename))
}
```

## 3. Session Manager API

```rust
// In src/session.rs

pub struct SessionManager;

impl SessionManager {
    /// Load session for a working directory (if exists)
    pub fn load(working_dir: &Path) -> Result<Option<Session>, SessionError> {
        let path = get_session_path(working_dir)?;
        if !path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(&path)?;
        let session: Session = serde_json::from_str(&content)?;

        // Validate working_dir matches
        if session.working_dir.canonicalize()? != working_dir.canonicalize()? {
            return Err(SessionError::WorkdirMismatch);
        }

        // Check version compatibility
        if session.version > CURRENT_SESSION_VERSION {
            return Err(SessionError::VersionTooNew(session.version));
        }

        Ok(Some(session))
    }

    /// Save session for current editor state
    pub fn save(editor: &Editor) -> Result<(), SessionError> {
        let session = Session::from_editor(editor)?;
        let path = get_session_path(&editor.working_dir)?;

        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let content = serde_json::to_string_pretty(&session)?;
        std::fs::write(&path, content)?;
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
}

#[derive(Debug, thiserror::Error)]
pub enum SessionError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    #[error("Working directory mismatch")]
    WorkdirMismatch,
    #[error("Session version {0} is newer than supported")]
    VersionTooNew(u32),
    #[error("File no longer exists: {0}")]
    FileNotFound(PathBuf),
}
```

## 4. Conversion Functions

```rust
// In src/session.rs

impl Session {
    pub const CURRENT_VERSION: u32 = 1;

    /// Capture current editor state into a session
    pub fn from_editor(editor: &Editor) -> Result<Self, SessionError> {
        let split_layout = serialize_split_node(
            editor.split_manager.root(),
            &editor.buffers,
            &editor.buffer_metadata,
        );

        let mut split_states = HashMap::new();
        for (split_id, view_state) in &editor.split_view_states {
            split_states.insert(
                *split_id,
                serialize_split_view_state(
                    view_state,
                    &editor.buffers,
                    &editor.buffer_metadata,
                    &editor.working_dir,
                ),
            );
        }

        Ok(Session {
            version: Self::CURRENT_VERSION,
            working_dir: editor.working_dir.clone(),
            split_layout,
            active_split_id: editor.split_manager.active_split_id(),
            split_states,
            config_overrides: SessionConfigOverrides::from_config(&editor.config),
            file_explorer: FileExplorerState {
                visible: editor.file_explorer_visible,
                width: editor.file_explorer_width,
                expanded_dirs: editor.file_explorer.expanded_dirs(),
                scroll_offset: editor.file_explorer.scroll_offset(),
            },
            histories: SessionHistories {
                search: editor.search_history.items().to_vec(),
                replace: editor.replace_history.items().to_vec(),
                command_palette: editor.command_registry.command_history().to_vec(),
                goto_line: Vec::new(), // TODO: add goto_line_history to Editor
                open_file: Vec::new(), // TODO: add open_file_history to Editor
            },
            search_options: SearchOptions {
                case_sensitive: editor.search_case_sensitive,
                whole_word: editor.search_whole_word,
                use_regex: editor.search_use_regex,
                confirm_each: editor.search_confirm_each,
            },
            bookmarks: serialize_bookmarks(&editor.bookmarks, &editor.buffer_metadata, &editor.working_dir),
            saved_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        })
    }

    /// Apply session state to editor during initialization
    pub fn apply_to_editor(&self, editor: &mut Editor) -> Result<(), SessionError> {
        // 1. Apply config overrides
        self.config_overrides.apply_to(&mut editor.config);

        // 2. Open files and build buffer mappings
        let buffer_mapping = self.open_session_files(editor)?;

        // 3. Reconstruct split layout
        editor.split_manager = self.reconstruct_splits(&buffer_mapping)?;

        // 4. Restore per-split view states
        self.restore_split_view_states(editor, &buffer_mapping)?;

        // 5. Restore file explorer state
        editor.file_explorer_visible = self.file_explorer.visible;
        editor.file_explorer_width = self.file_explorer.width;
        editor.file_explorer.restore_state(
            &self.file_explorer.expanded_dirs,
            self.file_explorer.scroll_offset,
        );

        // 6. Restore input histories (merge with global, session takes precedence)
        if !self.histories.search.is_empty() {
            editor.search_history = InputHistory::from_items(self.histories.search.clone());
        }
        if !self.histories.replace.is_empty() {
            editor.replace_history = InputHistory::from_items(self.histories.replace.clone());
        }
        if !self.histories.command_palette.is_empty() {
            editor.command_registry.restore_history(&self.histories.command_palette);
        }

        // 7. Restore search options
        editor.search_case_sensitive = self.search_options.case_sensitive;
        editor.search_whole_word = self.search_options.whole_word;
        editor.search_use_regex = self.search_options.use_regex;
        editor.search_confirm_each = self.search_options.confirm_each;

        // 8. Restore bookmarks
        editor.bookmarks = deserialize_bookmarks(&self.bookmarks, &buffer_mapping)?;

        Ok(())
    }
}
```

## 5. Integration Points

### A. Auto-save on Exit (`src/app/mod.rs`)

```rust
impl Editor {
    pub fn shutdown(&mut self) {
        // Existing cleanup...

        // Save session (best-effort, don't block on errors)
        if let Err(e) = SessionManager::save(self) {
            eprintln!("Warning: Failed to save session: {}", e);
        }
    }
}
```

### B. Auto-load on Startup (`src/app/mod.rs`)

```rust
impl Editor {
    pub fn with_working_dir(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
    ) -> io::Result<Self> {
        let working_dir = working_dir
            .or_else(|| std::env::current_dir().ok())
            .unwrap_or_else(|| PathBuf::from("."));

        let mut editor = Self::new_empty(config, width, height, working_dir.clone())?;

        // Try to restore session
        match SessionManager::load(&working_dir) {
            Ok(Some(session)) => {
                if let Err(e) = session.apply_to_editor(&mut editor) {
                    eprintln!("Warning: Failed to restore session: {}", e);
                    // Fall back to empty state
                }
            }
            Ok(None) => {
                // No session exists, start fresh
            }
            Err(e) => {
                eprintln!("Warning: Failed to load session: {}", e);
            }
        }

        Ok(editor)
    }
}
```

### C. Periodic Auto-save (optional)

```rust
// In event loop, save every N minutes
if last_save.elapsed() > Duration::from_secs(300) {
    let _ = SessionManager::save(editor);
    last_save = Instant::now();
}
```

### D. New Actions

```rust
pub enum Action {
    // ... existing actions ...
    SaveSession,
    ClearSession,
    // Optionally: LoadSession(PathBuf) for switching projects
}
```

## 6. File Structure

```
src/
├── session.rs          # NEW: Session types and SessionManager
├── session/            # NEW: Module directory (alternative)
│   ├── mod.rs
│   ├── types.rs        # Session, SerializedSplitNode, etc.
│   ├── manager.rs      # SessionManager impl
│   └── convert.rs      # Conversion to/from editor state
├── config.rs           # Existing (unchanged)
├── app/
│   └── mod.rs          # Add session load/save integration
└── main.rs             # Add --no-session flag option
```

## 7. CLI Flags

```rust
// In main.rs Args struct
#[derive(Parser)]
struct Args {
    // ... existing args ...

    /// Don't restore previous session
    #[arg(long)]
    no_session: bool,

    /// Don't save session on exit
    #[arg(long)]
    no_save_session: bool,
}
```

## 8. Edge Cases

| Scenario | Behavior |
|----------|----------|
| File deleted since session saved | Skip file, log warning, continue with remaining |
| File moved | Skip file (path-based lookup), user can reopen |
| Hash collision | Validated by `working_dir` field in JSON |
| Corrupt session file | Log error, start fresh |
| Future version session | Error with clear message |
| No write permission | Best-effort save, warn on failure |
| Buffer modified but not saved | Only persist cursor/scroll, not content |
| Virtual buffers (search results, etc.) | Skip - only persist file-backed buffers |

## 9. Example Session File

```json
{
  "version": 1,
  "working_dir": "/home/user/my-project",
  "split_layout": {
    "Split": {
      "direction": "Vertical",
      "first": {
        "Leaf": { "file_path": "src/main.rs", "split_id": 1 }
      },
      "second": {
        "Leaf": { "file_path": "src/lib.rs", "split_id": 2 }
      },
      "ratio": 0.5,
      "split_id": 0
    }
  },
  "active_split_id": 1,
  "split_states": {
    "1": {
      "open_files": ["src/main.rs", "src/config.rs"],
      "active_file_index": 0,
      "file_states": {
        "src/main.rs": {
          "cursor": { "position": 1842, "anchor": null, "sticky_column": 15 },
          "additional_cursors": [],
          "scroll": { "top_byte": 1200, "top_view_line_offset": 0, "left_column": 0 }
        }
      },
      "tab_scroll_offset": 0,
      "view_mode": "Source",
      "compose_width": null
    }
  },
  "config_overrides": {
    "line_wrap": true
  },
  "file_explorer": {
    "visible": true,
    "width": 30,
    "expanded_dirs": ["src", "src/app"],
    "scroll_offset": 0
  },
  "histories": {
    "search": ["handleAuth", "TODO", "unwrap()"],
    "replace": ["expect(\"failed\")"],
    "command_palette": ["save", "toggle_line_wrap", "split_vertical"]
  },
  "search_options": {
    "case_sensitive": false,
    "whole_word": false,
    "use_regex": false,
    "confirm_each": true
  },
  "bookmarks": {
    "a": { "file_path": "src/main.rs", "position": 1842 },
    "b": { "file_path": "src/lib.rs", "position": 500 }
  },
  "saved_at": 1700000000
}
```

## 10. Implementation Order

1. **Phase 1: Core Types** - Add `src/session.rs` with all serializable types
2. **Phase 2: Conversion** - Implement `Session::from_editor()` and serialization helpers
3. **Phase 3: Path Handling** - Implement `get_session_path()` with hashing
4. **Phase 4: Save** - Implement `SessionManager::save()` + shutdown hook
5. **Phase 5: Load** - Implement `SessionManager::load()` + startup integration
6. **Phase 6: Apply** - Implement `Session::apply_to_editor()` with file opening
7. **Phase 7: CLI Flags** - Add `--no-session` and `--no-save-session`
8. **Phase 8: Polish** - Add actions, error handling, edge cases

---

## 11. Comparison with Other Editors

### VS Code

**Approach**: VS Code automatically tracks workspace state without explicit user action. State is stored in a SQLite database per workspace, managed internally.

| Aspect | VS Code | This Design |
|--------|---------|-------------|
| Storage format | SQLite database | JSON files |
| Location | `.vscode/` folder or user data | XDG data dir (centralized) |
| Granularity | Per-workspace | Per-working-directory |
| User visibility | Opaque (internal DB) | Human-readable JSON |
| State scope | Full workspace state + extensions | Editor state only |

**Key insight**: VS Code's `ExtensionContext.workspaceState` API provides workspace-scoped storage for extensions. Our design focuses on core editor state only.

Sources:
- [VS Code Workspaces](https://code.visualstudio.com/docs/editor/workspaces)
- [VS Code Extension Storage API](https://code.visualstudio.com/api/extension-capabilities/common-capabilities)

### Neovim (ShaDa + Sessions)

**Approach**: Neovim separates concerns into two systems:
- **ShaDa** (Shared Data): Global state - marks, registers, command history, search patterns
- **Sessions** (`:mksession`): Project-specific state - open files, window layout, cursor positions

| Aspect | Neovim ShaDa | Neovim Sessions | This Design |
|--------|--------------|-----------------|-------------|
| Scope | Global | Per-project | Per-project |
| Format | MessagePack binary | Vim script | JSON |
| Trigger | Automatic | Manual (`:mksession`) | Automatic |
| Content | Marks, registers, history | Layout, buffers, cursors | Layout, buffers, cursors, toggles |

**Key insight**: Neovim's separation allows global state (command history) to persist across all projects while session state is project-specific. Our design combines these - we could consider separating global state (like search history, which Fresh already stores separately in `~/.local/share/fresh/`).

Sources:
- [Neovim ShaDa System](https://deepwiki.com/neovim/neovim/6.1-search-and-patterns)
- [persistence.nvim Plugin](https://github.com/folke/persistence.nvim)
- [Neovim Session Docs](https://neovim.io/doc/user/usr_21.html)

### Zed

**Approach**: Zed uses automatic session restoration with configurable behavior via `restore_on_startup` setting.

| Aspect | Zed | This Design |
|--------|-----|-------------|
| Storage | SQLite database | JSON files |
| Options | `last_session`, `last_workspace`, `none` | Restore or don't (CLI flags) |
| Scope | Per-window/workspace | Per-working-directory |
| Config | `restore_on_file_reopen` setting | Implicit (always restore if exists) |

**Key insight**: Zed's database approach is more robust for concurrent access but less transparent. Their `restore_on_file_reopen` setting (per-pane file state restoration) is something we could consider adding.

Sources:
- [Zed Workspace Persistence](https://zed.dev/docs/workspace-persistence)
- [Zed Configuration](https://zed.dev/docs/configuring-zed)

---

## 12. Integration with Existing Config System

### Current Config Architecture

Fresh's config system (`src/config.rs`) uses:
- **Serde JSON** for serialization
- **Default trait** with `#[serde(default)]` for missing fields
- **Single file** at `~/.config/fresh/config.json`
- **Runtime modification** via toggle actions (e.g., `ToggleLineWrap`)

### Session vs Config: Separation of Concerns

| Concern | Config (`config.json`) | Session (`sessions/*.json`) |
|---------|------------------------|------------------------------|
| **Purpose** | User preferences | Runtime state |
| **Lifetime** | Persistent across all projects | Per-project, ephemeral |
| **Scope** | Global defaults | Working directory specific |
| **Examples** | Tab size, theme, keybindings | Open files, cursor positions, split layout |
| **User edits** | Yes, intended | No, machine-generated |

### Design Decision: Config Overrides

The `SessionConfigOverrides` struct stores only **deviations** from the base config:

```rust
pub struct SessionConfigOverrides {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_numbers: Option<bool>,
    // ... only set if user toggled during session
}
```

**Rationale**:
- If user's global config sets `line_wrap: true`, but they disabled it for a specific project, only that override is saved
- When loading, we apply: `base_config.merge(session_overrides)`
- Changes to global config still take effect unless explicitly overridden per-project

### Alternative Considered: Full Config Copy

We could store a complete `EditorConfig` in each session file:

```rust
pub struct Session {
    pub editor_config: EditorConfig,  // Full copy
    // ...
}
```

**Rejected because**:
- Bloats session files with redundant data
- Global config changes wouldn't propagate to existing sessions
- Harder to distinguish "user intentionally set X" from "X was the default when session was saved"

### Shared Infrastructure

Both systems use:
- `serde::{Serialize, Deserialize}` - consistent serialization
- `serde_json` - same format
- `dirs` crate - XDG-compliant paths
- Similar error handling patterns (`ConfigError` / `SessionError`)

The session module can import and reuse:
- `EditorConfig` field types (for type safety in overrides)
- Path utilities from `dirs` crate
- JSON pretty-printing conventions

---

## 13. Tradeoffs and Design Decisions

### JSON vs SQLite

| | JSON Files | SQLite Database |
|-|------------|-----------------|
| **Pros** | Human-readable, easy debugging, no dependencies, trivial merge conflicts | ACID transactions, concurrent access, efficient queries |
| **Cons** | No atomicity (crash during write = corrupt), larger files, slower for many entries | Opaque to users, adds dependency, overkill for simple data |

**Decision**: JSON files. Fresh is a simple editor; we don't need concurrent access or complex queries. Human readability aids debugging.

### Hashed Filenames vs Readable Names

| | Hashed (`a1b2c3d4.json`) | Readable (`home_user_project.json`) |
|-|--------------------------|-------------------------------------|
| **Pros** | Fixed length, no special char issues, collision-resistant | User can identify sessions manually |
| **Cons** | Opaque - need index file to map | Path encoding issues, long filenames, potential collisions |

**Decision**: Hashed filenames with embedded `working_dir` field for validation. Add optional `index.json` for discoverability.

### Automatic vs Manual Save

| | Automatic (on exit) | Manual (`:mksession`) |
|-|---------------------|----------------------|
| **Pros** | Zero friction, always up-to-date | User control, explicit intent |
| **Cons** | May save unwanted state, can't have multiple named sessions | Easy to forget, stale sessions |

**Decision**: Automatic with opt-out (`--no-save-session`). Most users want "just work" behavior. Power users can disable.

### Per-Split vs Global Cursor State

The design stores cursor/scroll state **per-split-per-file**:

```rust
split_states: HashMap<SplitId, SerializedSplitViewState>
SerializedSplitViewState.file_states: HashMap<PathBuf, SerializedFileState>
```

**Why per-split**: The same file can be open in multiple splits with different scroll positions. Storing only global state would lose this.

**Tradeoff**: More complex data structure, larger session files for users with many splits.

### Line/Column vs Byte Offset

| | Line/Column | Byte Offset |
|-|-------------|-------------|
| **Pros** | Survives minor edits, human-readable | Exact position, O(1) lookup, works for huge files |
| **Cons** | O(n) to compute for large files, requires line counting | Invalid after any edit, opaque |

**Decision**: Byte offsets. Rationale:
- Fresh already uses byte offsets internally (`Cursor::position`, `Viewport::top_byte`)
- No conversion needed during save/restore - direct mapping to internal state
- O(1) position lookup regardless of file size (critical for multi-GB files)
- Line/column would require expensive line counting for large files
- If file changed externally, we clamp byte offset to file length (cursor goes to EOF)
- Users who edit files outside Fresh expect cursor reset anyway

### What NOT to Persist

Explicitly excluded from session state:
- **Undo history**: Too large, complex serialization, low value after restart
- **Buffer content**: Already on disk (for file-backed buffers)
- **LSP state**: Transient, reconstructed from language servers
- **Unsaved changes**: User should be prompted to save, not silently restored

### Global vs Per-Session History

| History Type | Storage | Rationale |
|--------------|---------|-----------|
| Search/Replace | Per-session | Project-specific terms (function names, patterns) |
| Command palette | Per-session | Different workflows per project |
| Goto line | Per-session | Line numbers only meaningful in context |
| Open file | Per-session | File paths relative to project |

**Alternative considered**: Keep histories global (current behavior).
**Rejected because**: Search terms like `handleUserAuth` or `DATABASE_URL` are project-specific. Users switching between projects don't want cross-contamination.

**Fallback behavior**: If session history is empty, fall back to global history. This gives new projects useful defaults while established projects get isolation.

---

## 14. Pros and Cons Summary

### Pros

1. **Zero-friction restoration** - Users get their workspace back automatically
2. **Per-project isolation** - Different projects have independent sessions
3. **Transparent format** - JSON files can be inspected, edited, backed up
4. **Minimal dependencies** - Reuses existing serde/dirs infrastructure
5. **Config integration** - Overrides layer cleanly on top of global config
6. **Graceful degradation** - Missing/corrupt sessions just start fresh
7. **XDG compliance** - Follows Linux desktop standards

### Cons

1. **No crash recovery** - Non-atomic JSON writes could corrupt on crash
2. **No multi-instance support** - Last-close-wins for session saves
3. **Path-based identification** - Moving a project breaks its session link
4. **Storage growth** - Sessions accumulate without cleanup mechanism
5. **No named sessions** - Can't have multiple sessions per project (e.g., "feature-branch")
6. **File-path coupling** - Renamed/moved files lose their cursor state

### Future Improvements (Out of Scope)

- **Session cleanup**: Prune sessions for non-existent directories
- **Named sessions**: Multiple sessions per project with user-chosen names
- **Atomic writes**: Write to temp file, then rename
- **Multi-instance coordination**: File locking or last-write-wins with merge
- **Workspace files**: VS Code-style `.fresh-workspace` files for shareable project configs
- **Branch-aware sessions**: Different sessions per git branch (like Zed's requested feature)
