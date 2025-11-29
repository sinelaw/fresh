# File Open Dialog Design

## Overview

This document describes a redesigned Open File command that:
1. Works without plugin dependencies (core implementation)
2. Provides a rich, interactive file browser popup above the prompt
3. Keeps the prompt in the standard location (status bar area)
4. Supports sorting, navigation shortcuts, and visual metadata

## Current Implementation

The current Open File command (`src/input/commands.rs:92-97`) delegates to plugins:
- `Action::Open` triggers `start_prompt("Open file: ", PromptType::OpenFile)`
- `update_prompt_suggestions()` fires the `prompt_changed` plugin hook
- Plugins respond with `SetPromptSuggestions` containing file paths
- Rendering uses `SuggestionsRenderer` (`src/view/ui/suggestions.rs`)

**Problems:**
- Non-functional when plugins are disabled (`--no-plugins`)
- No file metadata (size, modified date)
- No sorting options
- No quick navigation shortcuts

## Proposed Architecture

### New Components

```
src/
  app/
    file_open.rs          # FileOpenState, async directory loading, sorting
  view/
    ui/
      file_browser.rs     # FileBrowserRenderer - structured popup
      scrollbar.rs        # Extracted scrollbar widget (refactored from viewport)
```

### State Management

```rust
// src/app/file_open.rs

pub struct FileOpenState {
    /// Current directory being viewed
    pub current_dir: PathBuf,

    /// Loaded directory entries with metadata
    pub entries: Vec<FileOpenEntry>,

    /// Loading state for async operations
    pub loading: bool,

    /// Current sort mode
    pub sort_mode: SortMode,

    /// Sort direction
    pub sort_ascending: bool,

    /// Selected index in the main file list
    pub selected_index: usize,

    /// Scroll offset for the file list
    pub scroll_offset: usize,

    /// Section the selection is in
    pub active_section: FileOpenSection,

    /// Filter text (from prompt input)
    pub filter: String,
}

pub struct FileOpenEntry {
    pub fs_entry: FsEntry,
    pub matches_filter: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SortMode {
    Name,
    Size,
    Modified,
    Type,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FileOpenSection {
    Navigation,    // Parent dirs, shortcuts
    Files,         // Main file list
}
```

### Integration Points

1. **PromptType::OpenFile handling** (`src/app/mod.rs`):
   - Instead of firing plugin hook, populate `FileOpenState` directly
   - Use `FsManager::list_dir()` + `get_metadata_batch()` from `src/services/fs/`

2. **Input handling** (`src/app/input.rs`):
   - New keybindings for sort (click headers or keyboard shortcuts)
   - Navigation shortcuts for quick access
   - Scrollbar drag support

3. **Rendering** (`src/view/ui/mod.rs`):
   - Replace `SuggestionsRenderer::render()` call with `FileBrowserRenderer::render()`
   - Only when `prompt.prompt_type == PromptType::OpenFile`

## UI Layout

```
+----------------------------------------------------------------+
|                     File Open Popup                             |
+----------------------------------------------------------------+
| Navigation:                                                     |
|   ..             (parent directory)                             |
|   /              (root)                                        |
|   ~              (home directory)                               |
|   [Recent]       (recent directories - if available)            |
+----------------------------------------------------------------+
| [Name]          [Size]       [Modified]                    [^] |
+----------------------------------------------------------------+
| > file1.txt      1.2 KB      2024-01-15 10:30              |=| |
|   file2.rs       4.5 KB      2024-01-14 09:15              | | |
|   subdir/        --          2024-01-10 08:00              | | |
|   another.md     892 B       2024-01-13 14:22              |v| |
+----------------------------------------------------------------+
| [Filtered entries shown grayed if filter active]                |
+----------------------------------------------------------------+

Status bar area (unchanged):
+----------------------------------------------------------------+
| Open file: src/app/                                       | ▌  |
+----------------------------------------------------------------+
```

### Layout Breakdown

1. **Navigation Section** (top):
   - `..` - Parent directory navigation
   - `/` - Filesystem root
   - `~` - Home directory (cross-platform via `dirs` crate or env vars)
   - Platform-specific shortcuts (Windows drives, XDG dirs, etc.)

2. **Column Headers** (clickable for sorting):
   - Name (left-aligned)
   - Size (right-aligned, human-readable: KB, MB, GB)
   - Modified (right-aligned, relative or absolute date)
   - Sort indicator arrow on active column

3. **File List** (scrollable):
   - Icons/indicators for type (dir `/`, symlink `@`, file ` `)
   - Selection highlight on current row
   - Filtered-out entries shown grayed at bottom (when filter active)
   - Scrollbar on right side (reuse editor scroll logic)

4. **Prompt** (unchanged location):
   - Standard prompt rendering in status bar area
   - Input filters the main file list
   - Tab/Shift-Tab or Ctrl-N/Ctrl-P for list navigation

## Scrollbar Extraction

Refactor scrollbar logic from `src/view/viewport.rs` into reusable widget:

```rust
// src/view/ui/scrollbar.rs

pub struct ScrollbarState {
    pub total_items: usize,
    pub visible_items: usize,
    pub scroll_offset: usize,
}

impl ScrollbarState {
    /// Calculate thumb position and size
    pub fn thumb_geometry(&self, track_height: u16) -> (u16, u16) {
        // thumb_start, thumb_height
    }

    /// Convert click position to scroll offset
    pub fn click_to_offset(&self, track_height: u16, click_y: u16) -> usize {
        // ...
    }
}

pub struct ScrollbarRenderer;

impl ScrollbarRenderer {
    pub fn render(
        frame: &mut Frame,
        area: Rect,  // Vertical strip for scrollbar
        state: &ScrollbarState,
        theme: &Theme,
    ) {
        // Track background
        // Thumb (draggable)
        // Up/down arrows (optional)
    }
}
```

## Cross-Platform Shortcuts

```rust
// src/app/file_open.rs

pub fn get_platform_shortcuts() -> Vec<(String, PathBuf)> {
    let mut shortcuts = Vec::new();

    // Universal shortcuts
    if let Some(home) = dirs::home_dir() {
        shortcuts.push(("~".to_string(), home));
    }

    #[cfg(unix)]
    {
        shortcuts.push(("/".to_string(), PathBuf::from("/")));
        // XDG directories
        if let Some(docs) = dirs::document_dir() {
            shortcuts.push(("Documents".to_string(), docs));
        }
        if let Some(downloads) = dirs::download_dir() {
            shortcuts.push(("Downloads".to_string(), downloads));
        }
    }

    #[cfg(windows)]
    {
        // Windows drives
        for letter in b'A'..=b'Z' {
            let path = PathBuf::from(format!("{}:\\", letter as char));
            if path.exists() {
                shortcuts.push((format!("{}:", letter as char), path));
            }
        }
        // Special folders
        if let Some(docs) = dirs::document_dir() {
            shortcuts.push(("Documents".to_string(), docs));
        }
    }

    shortcuts
}
```

## Filtering Behavior

When the user types in the prompt:
1. Filter text is applied to current directory entries
2. **Matching entries** are shown in normal style, sorted according to current sort mode
3. **Non-matching entries** are moved to bottom and grayed out (dimmed)
4. Selection stays on matching entries by default
5. User can still select grayed entries (they're just visually de-emphasized)

```rust
impl FileOpenState {
    pub fn apply_filter(&mut self, filter: &str) {
        self.filter = filter.to_string();
        let filter_lower = filter.to_lowercase();

        for entry in &mut self.entries {
            entry.matches_filter = filter.is_empty()
                || entry.fs_entry.name.to_lowercase().contains(&filter_lower);
        }

        // Re-sort to put non-matching at bottom
        self.sort_entries();

        // Adjust selection if needed
        if !self.entries.is_empty() {
            // Try to keep selection on a matching entry
            if !self.entries[self.selected_index].matches_filter {
                if let Some(first_match) = self.entries.iter().position(|e| e.matches_filter) {
                    self.selected_index = first_match;
                }
            }
        }
    }

    fn sort_entries(&mut self) {
        // Primary: matching vs non-matching
        // Secondary: directories before files
        // Tertiary: current sort mode
        self.entries.sort_by(|a, b| {
            // Matching entries first
            match (a.matches_filter, b.matches_filter) {
                (true, false) => return std::cmp::Ordering::Less,
                (false, true) => return std::cmp::Ordering::Greater,
                _ => {}
            }

            // Directories before files
            match (a.fs_entry.is_dir(), b.fs_entry.is_dir()) {
                (true, false) => return std::cmp::Ordering::Less,
                (false, true) => return std::cmp::Ordering::Greater,
                _ => {}
            }

            // Apply sort mode
            let ord = match self.sort_mode {
                SortMode::Name => a.fs_entry.name.cmp(&b.fs_entry.name),
                SortMode::Size => {
                    let a_size = a.fs_entry.metadata.as_ref().and_then(|m| m.size).unwrap_or(0);
                    let b_size = b.fs_entry.metadata.as_ref().and_then(|m| m.size).unwrap_or(0);
                    a_size.cmp(&b_size)
                }
                SortMode::Modified => {
                    let a_mod = a.fs_entry.metadata.as_ref().and_then(|m| m.modified);
                    let b_mod = b.fs_entry.metadata.as_ref().and_then(|m| m.modified);
                    a_mod.cmp(&b_mod)
                }
                SortMode::Type => {
                    // Sort by extension
                    let a_ext = std::path::Path::new(&a.fs_entry.name)
                        .extension()
                        .and_then(|e| e.to_str())
                        .unwrap_or("");
                    let b_ext = std::path::Path::new(&b.fs_entry.name)
                        .extension()
                        .and_then(|e| e.to_str())
                        .unwrap_or("");
                    a_ext.cmp(b_ext)
                }
            };

            if self.sort_ascending { ord } else { ord.reverse() }
        });
    }
}
```

## Keybindings

### Navigation (in file browser popup)
| Key | Action |
|-----|--------|
| `Up` / `Ctrl-P` | Move selection up |
| `Down` / `Ctrl-N` | Move selection down |
| `PageUp` | Page up |
| `PageDown` | Page down |
| `Home` / `Ctrl-Home` | Jump to first entry |
| `End` / `Ctrl-End` | Jump to last entry |
| `Enter` | Open selected (file) or navigate into (directory) |
| `Backspace` (when filter empty) | Navigate to parent directory |
| `Escape` | Cancel and close |

### Sorting
| Key | Action |
|-----|--------|
| `Ctrl-1` or click Name header | Sort by name |
| `Ctrl-2` or click Size header | Sort by size |
| `Ctrl-3` or click Modified header | Sort by modified date |
| `Ctrl-R` | Reverse sort direction |

### Quick Navigation
| Key | Action |
|-----|--------|
| `~` (when filter empty) | Jump to home directory |
| `/` (when filter empty) | Jump to root (Unix) |
| `Ctrl-H` | Toggle hidden files |

## Mouse Interaction

1. **Column headers**: Click to sort, click again to reverse
2. **File list rows**: Click to select, double-click to open/navigate
3. **Scrollbar**:
   - Click track to page up/down
   - Drag thumb to scroll
   - Click arrows (if present) for single-line scroll
4. **Navigation shortcuts**: Click to navigate

## Async Loading

Directory contents are loaded asynchronously using the existing `FsManager`:

```rust
impl App {
    async fn load_file_open_directory(&mut self, path: PathBuf) {
        // Set loading state
        if let Some(state) = &mut self.file_open_state {
            state.loading = true;
            state.current_dir = path.clone();
        }

        // Async load via FsManager
        let entries = self.fs_manager.list_dir(&path).await;

        match entries {
            Ok(mut entries) => {
                // Fetch metadata in batch
                let paths: Vec<PathBuf> = entries.iter().map(|e| e.path.clone()).collect();
                let metadata_results = self.fs_manager.get_metadata_batch(&paths).await;

                // Merge metadata
                for (entry, meta_result) in entries.iter_mut().zip(metadata_results) {
                    if let Ok(meta) = meta_result {
                        entry.metadata = Some(meta);
                    }
                }

                // Update state
                if let Some(state) = &mut self.file_open_state {
                    state.entries = entries.into_iter()
                        .map(|e| FileOpenEntry { fs_entry: e, matches_filter: true })
                        .collect();
                    state.loading = false;
                    state.sort_entries();
                    state.scroll_offset = 0;
                    state.selected_index = 0;
                }
            }
            Err(e) => {
                // Show error in status message
                self.set_status_message(format!("Failed to read directory: {}", e));
                if let Some(state) = &mut self.file_open_state {
                    state.loading = false;
                }
            }
        }
    }
}
```

## Rendering Integration

The file browser popup is rendered above the prompt when `PromptType::OpenFile` is active:

```rust
// In src/view/ui/mod.rs or rendering code

fn render_prompt_area(
    frame: &mut Frame,
    prompt: &Prompt,
    file_open_state: Option<&FileOpenState>,
    // ...
) {
    // Calculate areas
    let terminal_height = frame.size().height;
    let prompt_y = terminal_height - 1;  // Bottom line

    // Render prompt at bottom (unchanged)
    StatusBarRenderer::render_prompt(frame, prompt_area, prompt, theme);

    // Render file browser popup above if OpenFile prompt
    if prompt.prompt_type == PromptType::OpenFile {
        if let Some(state) = file_open_state {
            let popup_height = (terminal_height - 2).min(20);  // Max 20 lines, leave room for prompt
            let popup_area = Rect {
                x: 0,
                y: prompt_y.saturating_sub(popup_height + 1),
                width: frame.size().width,
                height: popup_height,
            };
            FileBrowserRenderer::render(frame, popup_area, state, theme);
        }
    }
}
```

## Theme Colors

Add to `Theme` struct (`src/view/theme.rs`):

```rust
// File browser specific colors
pub file_browser_header_bg: Color,
pub file_browser_header_fg: Color,
pub file_browser_sort_indicator: Color,
pub file_browser_dir_fg: Color,
pub file_browser_file_fg: Color,
pub file_browser_filtered_fg: Color,  // Grayed out non-matching
pub file_browser_size_fg: Color,
pub file_browser_date_fg: Color,
pub file_browser_scrollbar_track: Color,
pub file_browser_scrollbar_thumb: Color,
```

## Migration Path

1. **Phase 1**: Add `FileOpenState` and basic rendering
   - Core file listing without plugins
   - Simple list view (no columns yet)

2. **Phase 2**: Add column layout and sorting
   - Headers with click-to-sort
   - Size/date display

3. **Phase 3**: Add scrollbar
   - Extract from viewport
   - Add to file browser

4. **Phase 4**: Polish
   - Keyboard shortcuts
   - Filter behavior refinement
   - Theme integration
   - Platform shortcuts

## Testing Strategy

1. **Unit tests**:
   - `FileOpenState` sorting logic
   - `ScrollbarState` geometry calculations
   - Filter matching logic

2. **Integration tests**:
   - Directory loading with `SlowFsBackend`
   - Navigation (parent, root, home)
   - Keyboard navigation

3. **Manual testing**:
   - Various terminal sizes
   - Long file names
   - Large directories
   - Unicode filenames
   - Platform-specific shortcuts

## Backwards Compatibility

- The existing plugin hook `prompt_changed` with `prompt_type: "open-file"` will **not** be fired
- Plugins that relied on providing file suggestions will no longer work for Open File
- Other prompt types (SaveFileAs, Plugin prompts) continue to work unchanged
- The `--no-plugins` flag will now have a functional Open File command
