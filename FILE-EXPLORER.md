# File Explorer Implementation Plan

## Overview

This document outlines the design and implementation plan for a VS Code-style file explorer in the Rust terminal text editor. The implementation follows Emacs design philosophy: creating reusable, composable components that integrate cleanly with existing infrastructure.

## Design Principles

1. **Separation of Concerns**: Separate filesystem access, tree model, and presentation layers
2. **Async-First**: All filesystem operations must be non-blocking for slow network filesystems
3. **Lazy Loading**: Only load directory contents when explicitly requested
4. **Pluggable Backends**: Abstract filesystem interface supporting local, NFS, SSHFS, etc.
5. **Reusable Components**: Build general-purpose infrastructure that can be used elsewhere
6. **Emacs Philosophy**: Keep core clean, use existing buffer/split infrastructure

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     File Explorer UI                         │
│  (keyboard navigation, rendering, event handling)            │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────┴────────────────────────────────────────┐
│                   File Tree Model                            │
│  (tree structure, expansion state, selection)                │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────┴────────────────────────────────────────┐
│              Filesystem Abstraction Layer                    │
│  (async operations, caching, pluggable backends)             │
└────────────────────┬────────────────────────────────────────┘
                     │
         ┌───────────┴──────────┬──────────────┐
         │                      │              │
┌────────┴────────┐  ┌─────────┴────────┐  ┌──┴────────┐
│  Local FS       │  │   Network FS     │  │  Future   │
│  Backend        │  │   (NFS/SSHFS)    │  │  Backends │
└─────────────────┘  └──────────────────┘  └───────────┘
```

## Module Structure

### 1. Filesystem Abstraction Layer (`src/fs/`)

**Purpose**: Provide async, pluggable filesystem access optimized for slow/network filesystems.

#### 1.1 Core Trait (`src/fs/backend.rs`)

```rust
/// Represents a file or directory entry
#[derive(Debug, Clone)]
pub struct FsEntry {
    pub path: PathBuf,
    pub name: String,
    pub entry_type: FsEntryType,
    pub metadata: Option<FsMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FsEntryType {
    File,
    Directory,
    Symlink,
}

#[derive(Debug, Clone)]
pub struct FsMetadata {
    pub size: Option<u64>,
    pub modified: Option<SystemTime>,
    pub permissions: Option<Permissions>,
    pub is_hidden: bool,
}

/// Async filesystem backend trait
#[async_trait]
pub trait FsBackend: Send + Sync {
    /// List entries in a directory (non-recursive)
    /// Returns entries without metadata for speed
    async fn read_dir(&self, path: &Path) -> io::Result<Vec<FsEntry>>;

    /// Get metadata for multiple paths in parallel
    async fn get_metadata_batch(&self, paths: &[PathBuf]) -> Vec<io::Result<FsMetadata>>;

    /// Check if path exists
    async fn exists(&self, path: &Path) -> bool;

    /// Check if path is a directory
    async fn is_dir(&self, path: &Path) -> io::Result<bool>;

    /// Get single entry with metadata
    async fn get_entry(&self, path: &Path) -> io::Result<FsEntry>;

    /// Watch directory for changes (optional)
    async fn watch(&self, path: &Path) -> io::Result<FsWatcher>;
}
```

#### 1.2 Local Filesystem Backend (`src/fs/local.rs`)

```rust
pub struct LocalFsBackend {
    /// Cache for metadata to reduce syscalls
    metadata_cache: Arc<RwLock<LruCache<PathBuf, (FsMetadata, Instant)>>>,
    cache_duration: Duration,
}

impl LocalFsBackend {
    pub fn new() -> Self;
    pub fn with_cache_duration(duration: Duration) -> Self;
}

#[async_trait]
impl FsBackend for LocalFsBackend {
    // Implementation using tokio::fs for async I/O
    // Parallelize metadata operations using tokio::spawn
}
```

#### 1.3 Filesystem Manager (`src/fs/manager.rs`)

```rust
/// Manages filesystem operations with caching and batching
pub struct FsManager {
    backend: Arc<dyn FsBackend>,
    pending_requests: Arc<Mutex<HashMap<PathBuf, Vec<oneshot::Sender<io::Result<Vec<FsEntry>>>>>>>,
}

impl FsManager {
    pub fn new(backend: Arc<dyn FsBackend>) -> Self;

    /// Request directory listing (batches duplicate requests)
    pub async fn list_dir(&self, path: PathBuf) -> io::Result<Vec<FsEntry>>;

    /// Get metadata for multiple paths efficiently
    pub async fn get_metadata(&self, paths: Vec<PathBuf>) -> Vec<io::Result<FsMetadata>>;
}
```

### 2. File Tree Model (`src/file_tree/`)

**Purpose**: Maintain the hierarchical tree structure with lazy loading and state management.

#### 2.1 Tree Node (`src/file_tree/node.rs`)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

pub struct TreeNode {
    pub id: NodeId,
    pub entry: FsEntry,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
    pub state: NodeState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeState {
    /// Directory not yet expanded
    Collapsed,
    /// Directory expanded, loading children
    Loading,
    /// Directory expanded, children loaded
    Expanded,
    /// Failed to load (with error)
    Error,
    /// File (leaf node)
    Leaf,
}
```

#### 2.2 File Tree (`src/file_tree/tree.rs`)

```rust
pub struct FileTree {
    root_path: PathBuf,
    nodes: HashMap<NodeId, TreeNode>,
    path_to_node: HashMap<PathBuf, NodeId>,
    root_id: NodeId,
    next_id: usize,
    fs_manager: Arc<FsManager>,
}

impl FileTree {
    pub fn new(root_path: PathBuf, fs_manager: Arc<FsManager>) -> Self;

    /// Get node by ID
    pub fn get_node(&self, id: NodeId) -> Option<&TreeNode>;

    /// Get node by path
    pub fn get_node_by_path(&self, path: &Path) -> Option<&TreeNode>;

    /// Expand a directory node (triggers async load)
    pub async fn expand_node(&mut self, id: NodeId) -> io::Result<()>;

    /// Collapse a directory node
    pub fn collapse_node(&mut self, id: NodeId);

    /// Toggle node expansion
    pub async fn toggle_node(&mut self, id: NodeId) -> io::Result<()>;

    /// Get visible nodes (flattened tree respecting expansion state)
    pub fn get_visible_nodes(&self) -> Vec<NodeId>;

    /// Refresh a node (re-read directory)
    pub async fn refresh_node(&mut self, id: NodeId) -> io::Result<()>;

    /// Get parent chain for a node
    pub fn get_ancestors(&self, id: NodeId) -> Vec<NodeId>;
}
```

#### 2.3 Ignore Patterns (`src/file_tree/ignore.rs`)

```rust
pub struct IgnorePattern {
    patterns: Vec<glob::Pattern>,
    gitignore_patterns: Vec<(PathBuf, gitignore::File)>,
}

impl IgnorePattern {
    pub fn new() -> Self;

    /// Add glob pattern (e.g., "*.o", "target/")
    pub fn add_pattern(&mut self, pattern: &str) -> Result<(), glob::PatternError>;

    /// Load .gitignore file
    pub fn load_gitignore(&mut self, path: &Path) -> io::Result<()>;

    /// Check if path should be ignored
    pub fn is_ignored(&self, path: &Path, is_dir: bool) -> bool;

    /// Get ignore status (for rendering grayed out files)
    pub fn get_status(&self, path: &Path, is_dir: bool) -> IgnoreStatus;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IgnoreStatus {
    Visible,
    Ignored,
    GitIgnored,
}
```

### 3. File Tree View (`src/file_tree/view.rs`)

**Purpose**: Manage the visual representation and navigation state.

```rust
pub struct FileTreeView {
    tree: FileTree,
    selected_node: Option<NodeId>,
    scroll_offset: usize,
    ignore_patterns: IgnorePattern,
    show_ignored: bool,
    sort_mode: SortMode,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SortMode {
    Name,
    Type,  // Directories first, then files
    Modified,
}

impl FileTreeView {
    pub fn new(tree: FileTree) -> Self;

    /// Get currently visible and filtered nodes
    pub fn get_display_nodes(&self) -> Vec<(NodeId, usize)>; // (node_id, indent_level)

    /// Navigation
    pub fn select_next(&mut self);
    pub fn select_prev(&mut self);
    pub fn select_first(&mut self);
    pub fn select_last(&mut self);
    pub fn select_parent(&mut self);

    /// Get selected node
    pub fn get_selected(&self) -> Option<NodeId>;

    /// Scroll management
    pub fn ensure_visible(&mut self, viewport_height: usize);
    pub fn get_scroll_offset(&self) -> usize;

    /// Settings
    pub fn set_show_ignored(&mut self, show: bool);
    pub fn set_sort_mode(&mut self, mode: SortMode);
}
```

### 4. File Explorer UI (`src/ui/file_explorer.rs`)

**Purpose**: Render the file tree and handle user interaction.

```rust
pub struct FileExplorerRenderer;

impl FileExplorerRenderer {
    /// Render file tree in a ratatui frame area
    pub fn render(
        view: &FileTreeView,
        frame: &mut Frame,
        area: Rect,
        is_focused: bool,
    );

    /// Render a single tree node line
    fn render_node(
        node: &TreeNode,
        indent: usize,
        is_selected: bool,
        ignore_status: IgnoreStatus,
    ) -> Line;

    /// Get icon for file/directory
    fn get_icon(node: &TreeNode) -> &'static str;

    /// Get color for node based on state and ignore status
    fn get_style(node: &TreeNode, is_selected: bool, ignore_status: IgnoreStatus) -> Style;
}
```

### 5. File Explorer Buffer Integration (`src/file_explorer_buffer.rs`)

**Purpose**: Integrate file explorer into the existing buffer system as a special buffer type.

```rust
/// Special buffer type for file explorer
pub struct FileExplorerBuffer {
    buffer_id: BufferId,
    view: FileTreeView,
    root_path: PathBuf,
}

impl FileExplorerBuffer {
    pub fn new(buffer_id: BufferId, root_path: PathBuf, fs_manager: Arc<FsManager>) -> Self;

    /// Handle key events specific to file explorer
    pub async fn handle_action(&mut self, action: FileExplorerAction) -> Option<EditorAction>;
}

pub enum FileExplorerAction {
    NavigateUp,
    NavigateDown,
    NavigateToParent,
    ExpandCollapse,
    OpenFile,
    CreateFile,
    CreateDirectory,
    DeleteFile,
    RenameFile,
    Refresh,
    ToggleShowIgnored,
    ChangeSortMode,
}

/// Return type for actions that affect the editor
pub enum EditorAction {
    OpenFile(PathBuf),
    CloseExplorer,
    None,
}
```

### 6. Integration with Editor (`src/editor.rs` modifications)

```rust
// Add to Editor struct
pub struct Editor {
    // ... existing fields ...
    file_explorer: Option<FileExplorerBuffer>,
    file_explorer_split: Option<SplitId>,
    fs_manager: Arc<FsManager>,
}

// Add actions
pub enum Action {
    // ... existing actions ...
    ToggleFileExplorer,
    FileExplorerNavigateUp,
    FileExplorerNavigateDown,
    FileExplorerExpand,
    FileExplorerOpenFile,
    // ... more file explorer actions ...
}

impl Editor {
    /// Toggle file explorer visibility
    pub fn toggle_file_explorer(&mut self);

    /// Open file explorer at specific path
    pub fn open_file_explorer(&mut self, path: PathBuf);

    /// Close file explorer
    pub fn close_file_explorer(&mut self);

    /// Route actions to file explorer if focused
    pub async fn handle_file_explorer_action(&mut self, action: Action);
}
```

## Implementation Phases

### Phase 1: Filesystem Abstraction Layer
**Goal**: Build and test async filesystem backend infrastructure.

**Tasks**:
1. Create `src/fs/` directory structure
2. Implement `FsBackend` trait in `backend.rs`
3. Implement `LocalFsBackend` with async I/O
4. Implement `FsManager` with request batching
5. Write unit tests for each component
6. Write integration tests for concurrent operations
7. Benchmark metadata batch operations

**Files Created**:
- `src/fs/mod.rs`
- `src/fs/backend.rs`
- `src/fs/local.rs`
- `src/fs/manager.rs`
- `tests/fs_backend_tests.rs`

**Success Criteria**:
- All tests pass
- Can list directory with 10,000+ entries
- Parallel metadata fetching is faster than sequential
- No blocking operations on main thread

### Phase 2: File Tree Model
**Goal**: Build tree structure with lazy loading and state management.

**Tasks**:
1. Create `src/file_tree/` directory structure
2. Implement `TreeNode` and `NodeId` in `node.rs`
3. Implement `FileTree` with lazy loading
4. Implement ignore pattern matching
5. Implement tree traversal and visibility calculation
6. Write unit tests for tree operations
7. Write tests for ignore patterns

**Files Created**:
- `src/file_tree/mod.rs`
- `src/file_tree/node.rs`
- `src/file_tree/tree.rs`
- `src/file_tree/ignore.rs`
- `tests/file_tree_tests.rs`

**Success Criteria**:
- Tree correctly represents directory hierarchy
- Lazy loading only loads expanded directories
- Ignore patterns correctly filter files
- Tree operations are efficient (O(log n) where possible)

### Phase 3: File Tree View
**Goal**: Implement navigation and filtering logic.

**Tasks**:
1. Implement `FileTreeView` in `view.rs`
2. Implement navigation (up/down/parent)
3. Implement scroll management
4. Implement filtering and sorting
5. Write unit tests for navigation
6. Write tests for edge cases (empty dirs, single file, etc.)

**Files Created**:
- `src/file_tree/view.rs`
- `tests/file_tree_view_tests.rs`

**Success Criteria**:
- Navigation works correctly in all scenarios
- Scrolling keeps selected item visible
- Filtering correctly hides/shows items

### Phase 4: File Explorer UI Renderer
**Goal**: Create ratatui-based rendering for file tree.

**Tasks**:
1. Create `src/ui/file_explorer.rs`
2. Implement node rendering with icons and colors
3. Implement tree line drawing (├──, └──, etc.)
4. Implement selection highlighting
5. Implement loading/error state rendering
6. Test rendering with various tree states

**Files Created**:
- `src/ui/file_explorer.rs`

**Success Criteria**:
- File tree renders correctly in terminal
- Icons and colors distinguish file types
- Tree structure is visually clear
- Performance is good for large trees (100+ visible items)

### Phase 5: File Explorer Buffer Integration
**Goal**: Integrate file explorer as a special buffer type.

**Tasks**:
1. Create `src/file_explorer_buffer.rs`
2. Implement `FileExplorerBuffer`
3. Implement action handling
4. Integrate with existing event system
5. Add file operation handlers (open, create, delete, rename)
6. Add keyboard shortcuts
7. Test integration with buffer system

**Files Created**:
- `src/file_explorer_buffer.rs`

**Success Criteria**:
- File explorer works as a buffer
- Actions correctly trigger operations
- File operations work (open, create, etc.)

### Phase 6: Editor Integration
**Goal**: Integrate file explorer with main editor and split view system.

**Tasks**:
1. Add `FileExplorer` support to `Editor`
2. Add `ToggleFileExplorer` action
3. Implement split view management for explorer
4. Add keybindings for file explorer
5. Update action routing to handle explorer actions
6. Implement focus management between explorer and editor
7. Add commands to command palette

**Files Modified**:
- `src/editor.rs`
- `src/actions.rs`
- `src/keybindings.rs`
- `src/commands.rs`

**Success Criteria**:
- File explorer toggles on/off correctly
- Split view shows explorer + editor
- Focus switches between panes
- Opening files from explorer works
- All keyboard shortcuts work

### Phase 7: Async Bridge and Message Passing
**Goal**: Ensure filesystem operations don't block the UI.

**Tasks**:
1. Create async message types for filesystem operations
2. Implement message handling in main event loop
3. Add loading indicators during async operations
4. Implement error handling and user feedback
5. Test with slow filesystem scenarios

**Files Modified**:
- `src/editor.rs`
- `src/file_explorer_buffer.rs`

**Success Criteria**:
- UI remains responsive during directory loading
- Loading indicators show progress
- Errors are displayed to user
- No blocking operations on main thread

### Phase 8: Polish and Features
**Goal**: Add nice-to-have features and polish.

**Tasks**:
1. Implement file watching for automatic refresh
2. Add file preview on selection (optional)
3. Add search/filter in file explorer
4. Improve icons based on file extensions
5. Add context menu or command mode
6. Optimize rendering for very large trees
7. Add configuration options

**Success Criteria**:
- All polish features work correctly
- Performance is excellent even with large directories
- User experience is smooth and intuitive

## Testing Strategy

### Unit Tests
- Each module has comprehensive unit tests
- Test edge cases (empty dirs, single files, errors)
- Test async operations with tokio test runtime
- Mock filesystem backend for deterministic tests

### Integration Tests
- Test full flow from filesystem to UI
- Test with real directory structures
- Test with slow filesystem (add delays to backend)
- Test concurrent operations

### Performance Tests
- Benchmark directory listing with 10,000+ files
- Benchmark tree traversal
- Benchmark rendering
- Profile memory usage

### Manual Testing
- Test with various directory structures
- Test with network filesystems (NFS, SSHFS)
- Test edge cases (symlinks, permissions, hidden files)
- Test keyboard navigation thoroughly

## Dependencies

### New Crates to Add

```toml
[dependencies]
# Existing dependencies remain...

# For glob patterns
glob = "0.3"

# For gitignore parsing
ignore = "0.4"  # From ripgrep, robust gitignore implementation

# For LRU cache in filesystem backend
lru = "0.12"

# For async oneshot channels (batching)
# Already have tokio, use tokio::sync::oneshot
```

## Configuration

Add to `Config` struct:

```rust
pub struct FileExplorerConfig {
    /// Show hidden files (starting with .)
    pub show_hidden: bool,

    /// Show ignored files (gitignore, patterns)
    pub show_ignored: bool,

    /// Default sort mode
    pub sort_mode: SortMode,

    /// Ignore patterns
    pub ignore_patterns: Vec<String>,

    /// Max depth to auto-expand
    pub auto_expand_depth: usize,

    /// Cache duration for metadata
    pub metadata_cache_duration_secs: u64,

    /// Initial width of file explorer (percentage)
    pub explorer_width: f32,
}
```

## Key Design Decisions

### 1. Buffer vs Special Widget
**Decision**: Implement as a special buffer type, not a standalone widget.

**Rationale**:
- Reuses existing buffer infrastructure
- Works naturally with split view system
- Can leverage viewport and scrolling logic
- Fits Emacs philosophy (everything is a buffer)

### 2. Async Architecture
**Decision**: All filesystem operations are async with message passing.

**Rationale**:
- Essential for network filesystems
- Keeps UI responsive
- Enables batching and optimization
- Fits existing async bridge architecture

### 3. Lazy Loading
**Decision**: Only load directory contents when explicitly expanded.

**Rationale**:
- Critical for performance with huge directories
- Reduces memory usage
- Faster startup time
- Better UX (no freezing on deep trees)

### 4. Pluggable Backend
**Decision**: Abstract filesystem behind trait with multiple implementations.

**Rationale**:
- Enables future network filesystem support
- Easier to test (mock backend)
- Can optimize per filesystem type
- Follows SOLID principles

### 5. Separate Model and View
**Decision**: Split tree model (`FileTree`) and view state (`FileTreeView`).

**Rationale**:
- MVC architecture
- Model can be reused elsewhere
- View state (selection, scroll) separate from data
- Easier to test independently

## Future Enhancements

1. **Network Filesystem Backends**
   - SSH/SFTP backend
   - S3 backend
   - Custom protocol support

2. **Advanced Features**
   - File search within explorer
   - Quick file creation templates
   - Drag and drop (if terminal supports)
   - Bulk operations

3. **Performance**
   - Virtual scrolling for huge trees
   - Incremental rendering
   - Background preloading

4. **Integration**
   - LSP workspace symbols in tree
   - Git status indicators
   - File watchers for auto-refresh

## Risk Mitigation

### Risk 1: Performance with Huge Directories
**Mitigation**:
- Lazy loading (only load visible)
- Virtual scrolling
- Async operations
- Benchmark early and often

### Risk 2: Network Filesystem Slowness
**Mitigation**:
- Aggressive caching
- Parallel metadata fetching
- Request batching
- Loading indicators

### Risk 3: Integration Complexity
**Mitigation**:
- Build modules independently first
- Comprehensive tests before integration
- Use existing patterns (buffers, splits)
- Incremental integration

### Risk 4: Merge Conflicts
**Mitigation**:
- Build new modules in separate files
- Minimize changes to existing code
- Integrate last
- Keep changes in dedicated branch

## Success Metrics

1. **Performance**
   - List 10,000 file directory in < 100ms
   - UI stays responsive during all operations
   - Memory usage < 10MB for typical project

2. **Functionality**
   - All navigation works smoothly
   - File operations (open, create, delete) work
   - Ignore patterns work correctly
   - Works with network filesystems

3. **Code Quality**
   - > 80% test coverage
   - All tests pass
   - No clippy warnings
   - Clean separation of concerns

4. **User Experience**
   - Intuitive keyboard navigation
   - Clear visual feedback
   - Helpful error messages
   - Fast and responsive

## Timeline Estimate

- **Phase 1** (Filesystem Layer): 2-3 days
- **Phase 2** (Tree Model): 2-3 days
- **Phase 3** (Tree View): 1-2 days
- **Phase 4** (UI Renderer): 1-2 days
- **Phase 5** (Buffer Integration): 1-2 days
- **Phase 6** (Editor Integration): 1-2 days
- **Phase 7** (Async Bridge): 1 day
- **Phase 8** (Polish): 1-2 days

**Total**: 10-17 days of focused development

## Conclusion

This plan provides a comprehensive approach to building a robust, performant file explorer that:
- Works well with slow network filesystems
- Integrates cleanly with existing architecture
- Follows Emacs design philosophy
- Provides excellent user experience
- Is thoroughly tested and maintainable

The implementation follows a bottom-up approach, building reusable infrastructure first and integrating last to minimize merge conflicts and ensure stability.
