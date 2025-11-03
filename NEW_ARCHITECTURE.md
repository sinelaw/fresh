# Editor: Clean Architecture Design

## Core Principles

1. **Arbitrary large files** - GB+ files with constant memory usage
2. **Ultra-low latency** - Every operation <1ms, rendering at 60fps
3. **Multiple cursors** - First-class, efficient, scalable to 1000+ cursors
4. **Best-effort highlighting** - Fast > correct, tree-sitter based
5. **Radical simplicity** - Minimal abstractions, obvious code

## Architecture Overview

```
┌─────────────────────────────────────────┐
│           Editor (main.rs)              │
│  - Event loop                           │
│  - Multiple cursor state                │
│  - Rendering                            │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│        Buffer (buffer.rs)               │
│  - Text storage (ChunkTree)             │
│  - Line index (cached)                  │
│  - Edit operations                      │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│      Rope/ChunkTree (chunk_tree.rs)     │
│  - Persistent tree structure            │
│  - Byte-level operations                │
│  - Gap support for sparse files         │
└─────────────────────────────────────────┘
```

## Key Data Structures

### 1. Buffer - The Core Abstraction

```rust
pub struct Buffer {
    /// Persistent text storage - the source of truth
    content: Rope,

    /// Cached line boundaries for fast line<->byte conversion
    /// Vec of byte offsets where each line starts
    /// Rebuilt lazily when content changes
    line_cache: LineCache,

    /// Optional: File backing for persistence
    file_path: Option<PathBuf>,

    /// Dirty flag for save detection
    modified: bool,
}

/// Rope is just a type alias for our existing ChunkTree
type Rope = ChunkTree<'static>;

struct LineCache {
    /// Byte offset of each line start. line_starts[0] = 0, line_starts[1] = offset of line 1, etc.
    line_starts: Vec<usize>,

    /// Is cache valid? Invalidated on edits
    valid: bool,

    /// Cached total line count
    line_count: usize,
}
```

**Key Operations** (all O(log n) or better):
```rust
impl Buffer {
    // Core editing - positions are byte offsets
    fn insert(&mut self, pos: usize, text: &str);
    fn delete(&mut self, range: Range<usize>);
    fn slice(&self, range: Range<usize>) -> String;

    // Line-based convenience (uses cache)
    fn line_to_byte(&self, line: usize) -> usize;
    fn byte_to_line(&self, byte: usize) -> usize;
    fn line_content(&self, line: usize) -> &str;
    fn line_count(&self) -> usize;

    // Efficient range queries for rendering
    fn lines_in_range(&self, start: usize, count: usize) -> Vec<&str>;

    // File operations
    fn load_from_file(path: &Path) -> Result<Buffer>;
    fn save(&mut self) -> Result<()>;
}
```

**Why this works**:
- ChunkTree handles arbitrary size (only loads what's needed)
- Line cache is rebuilt incrementally (only affected ranges)
- All positions are byte offsets (simple, no conversion overhead)

### 2. Cursor - Lightweight and Independent

```rust
#[derive(Clone, Copy, Debug)]
pub struct Cursor {
    /// Primary position (where edits happen)
    position: usize,  // byte offset in buffer

    /// Selection anchor (if any) for visual selection
    anchor: Option<usize>,

    /// Desired column (for up/down navigation)
    /// When moving up/down, try to stay in this column
    sticky_column: usize,
}

impl Cursor {
    fn new(position: usize) -> Self;
    fn with_selection(start: usize, end: usize) -> Self;
    fn collapsed(&self) -> bool;
    fn selection_range(&self) -> Option<Range<usize>>;
}
```

**Multi-cursor state**:
```rust
pub struct Cursors {
    /// All cursors, kept sorted by position
    cursors: Vec<Cursor>,
}

impl Cursors {
    fn new() -> Self {
        Cursors { cursors: vec![Cursor::new(0)] }
    }

    /// Add cursor, auto-merge if overlapping
    fn add(&mut self, cursor: Cursor);

    /// Remove overlapping/duplicate cursors
    fn normalize(&mut self);

    /// Get primary cursor (last one, typically)
    fn primary(&self) -> &Cursor;

    /// Apply transformation to all cursors
    fn map<F>(&mut self, f: F) where F: Fn(&mut Cursor);

    /// Adjust all cursor positions after edit at `pos` with `delta` change
    fn adjust_for_edit(&mut self, pos: usize, delta: isize);
}
```

**Why this works**:
- Cursors are just byte positions - trivial to adjust after edits
- No complex tracking or invalidation
- Sorting + merging gives O(n log n) for n cursors (fast for n < 1000)

### 3. Viewport - What's Visible

```rust
pub struct Viewport {
    /// Top-left corner (byte offset of first visible line)
    scroll_pos: usize,

    /// Terminal dimensions
    width: u16,
    height: u16,

    /// Computed visible range (cached)
    visible_lines: Range<usize>,
}

impl Viewport {
    /// Ensure cursor is visible, scroll if needed
    fn scroll_to_cursor(&mut self, buffer: &Buffer, cursor: &Cursor);

    /// Get lines to render (with line numbers)
    fn visible_content(&self, buffer: &Buffer) -> Vec<(usize, &str)>;
}
```

### 4. Highlighter - Best Effort Syntax

```rust
pub struct Highlighter {
    /// Tree-sitter parser
    parser: tree_sitter::Parser,

    /// Highlight configuration
    config: HighlightConfiguration,

    /// Cache: byte range -> highlighted spans
    /// Only cache what's visible
    cache: HighlightCache,
}

struct HighlightCache {
    /// Cached range
    range: Range<usize>,

    /// Highlighted spans: (start, end, style_id)
    spans: Vec<(usize, usize, u8)>,

    /// Is cache valid?
    valid: bool,
}

impl Highlighter {
    /// Highlight visible range, return styled spans
    /// Best effort: if tree-sitter fails, return unstyled
    fn highlight(&mut self, buffer: &Buffer, range: Range<usize>)
        -> Vec<(usize, usize, Style)>;

    /// Invalidate cache on edit
    fn invalidate(&mut self, edited_range: Range<usize>);
}
```

**Strategy**:
- Only highlight visible window (~50 lines)
- Cache aggressively, invalidate on nearby edits
- If parsing takes >5ms, abort and show unstyled
- Parse incrementally in background if time allows

## Editor State

```rust
pub struct Editor {
    /// The text content
    buffer: Buffer,

    /// All cursors
    cursors: Cursors,

    /// What's visible
    viewport: Viewport,

    /// Syntax highlighting
    highlighter: Highlighter,

    /// Terminal handle
    terminal: Terminal,

    /// Mode (insert/normal if vim-like, or always insert)
    mode: Mode,
}

impl Editor {
    pub fn new(file: Option<PathBuf>) -> Result<Self>;

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.render()?;

            let event = crossterm::event::read()?;

            if !self.handle_event(event)? {
                break;
            }
        }
        Ok(())
    }

    fn render(&mut self) -> Result<()>;
    fn handle_event(&mut self, event: Event) -> Result<bool>;
}
```

## Core Operations - Multi-Cursor Aware

### Typing a Character
```rust
fn insert_char(&mut self, c: char) {
    // Collect all edits (sorted by position, reversed for back-to-front)
    let edits: Vec<_> = self.cursors.cursors
        .iter()
        .map(|cursor| (cursor.position, c.to_string()))
        .collect();

    // Apply edits back-to-front (doesn't invalidate positions)
    for (pos, text) in edits.iter().rev() {
        self.buffer.insert(*pos, text);
    }

    // Adjust all cursors forward by 1
    for cursor in &mut self.cursors.cursors {
        cursor.position += c.len_utf8();
    }

    self.cursors.normalize();  // Merge if any overlapped
}
```

### Moving Left
```rust
fn move_left(&mut self) {
    for cursor in &mut self.cursors.cursors {
        if cursor.position > 0 {
            // Move to previous UTF-8 char boundary
            cursor.position = self.buffer.prev_char_boundary(cursor.position);
        }
        cursor.anchor = None;  // Clear selection
    }
    self.cursors.normalize();
}
```

### Delete Selection(s)
```rust
fn delete_selection(&mut self) {
    let deletions: Vec<_> = self.cursors.cursors
        .iter()
        .filter_map(|c| c.selection_range())
        .collect();

    // Apply deletions back-to-front
    let mut total_deleted = 0;
    for range in deletions.iter().rev() {
        self.buffer.delete(range.clone());
        total_deleted += range.len();
    }

    // Adjust cursors
    self.cursors.adjust_for_deletions(&deletions);
    self.cursors.normalize();
}
```

## Performance Characteristics

### Memory Usage
- **ChunkTree**: O(log n) overhead per edit, structural sharing via Arc
- **Line cache**: O(lines) = ~8 bytes per line for 1M lines = ~8MB
- **Highlight cache**: O(visible) = ~50 lines * ~100 bytes = ~5KB
- **Total for 1GB file**: ~20-30MB in steady state

### Latency (target)
- **Keystroke -> render**: <1ms (includes edit + cache update + render)
- **Cursor navigation**: <0.1ms (cache lookup)
- **Scroll**: <1ms (cache hit) or <10ms (cache miss, reparse)
- **Multi-cursor edit (100 cursors)**: <2ms

### Rendering Pipeline (60fps = 16ms budget)
```
Handle input          <1ms
Apply edits           <1ms
Update line cache     <1ms
Update cursors        <1ms
Syntax highlight      <5ms (best effort, can skip)
Draw to terminal      <5ms
-----------------
Total:                <14ms ✓
```

## Simplifications vs. Traditional Editors

### What We DON'T Do
1. **No undo/redo** (phase 1) - add later using ChunkTree persistence
2. **No clipboard** (phase 1) - just selections
3. **No search** (phase 1) - add later
4. **No config files** - hardcoded keybindings
5. **No plugins** - monolithic binary
6. **No LSP** - just syntax highlighting
7. **No line wrapping** (phase 1) - horizontal scroll only
8. **No Unicode bidi** - left-to-right only
9. **No fancy rendering** - monospace terminal only

### What We DO Well
1. ✓ Open any file size instantly
2. ✓ Scroll anywhere with zero lag
3. ✓ Edit with ultra-low latency
4. ✓ Multiple cursors everywhere
5. ✓ Pretty syntax colors
6. ✓ Stable, no crashes

## File Structure

```
src/
├── main.rs           # Entry point, CLI arg parsing
├── editor.rs         # Editor struct, event loop, rendering
├── buffer.rs         # Buffer struct, line cache, file I/O
├── cursor.rs         # Cursor, Cursors multi-cursor logic
├── viewport.rs       # Viewport, scrolling logic
├── highlighter.rs    # Syntax highlighting with tree-sitter
├── chunk_tree.rs     # (keep existing) Rope implementation
├── keybindings.rs    # Key event -> action mapping
└── render.rs         # Terminal rendering helpers

# Delete these:
- lines.rs          # (obsolete - buffer handles this)
- logs.rs           # (use proper logging crate if needed)
- memstore.rs       # (obsolete - buffer handles chunk loading)
- virtual_file.rs   # (replaced by buffer.rs)
```

## Implementation Strategy

### Phase 1: Core Editor (2-3 days)
- [ ] Implement Buffer with ChunkTree + line cache
- [ ] Implement single Cursor + basic navigation
- [ ] Implement Viewport + scrolling
- [ ] Implement Editor event loop
- [ ] Implement basic rendering (no highlighting)
- [ ] Basic keybindings: arrows, type, backspace, save, quit

**Milestone**: Can edit text files, no highlighting

### Phase 2: Multi-Cursor (1 day)
- [ ] Implement Cursors collection
- [ ] Implement cursor adjustments after edits
- [ ] Add cursor keybindings: Ctrl+Click, Ctrl+D (select next occurrence)
- [ ] Visual selection rendering

**Milestone**: Full multi-cursor editing

### Phase 3: Syntax Highlighting (1 day)
- [ ] Implement Highlighter with tree-sitter
- [ ] Implement highlight cache
- [ ] Add language detection (by extension)
- [ ] Render with colors

**Milestone**: Pretty colored code

### Phase 4: Large File Optimization (1 day)
- [ ] Lazy line cache building (only compute visible range)
- [ ] Chunk loading on-demand in ChunkTree
- [ ] Profile and optimize hot paths
- [ ] Test with 1GB+ files

**Milestone**: Production ready for massive files

### Phase 5: Polish (ongoing)
- [ ] Undo/redo
- [ ] Search/replace
- [ ] Clipboard
- [ ] Status bar improvements
- [ ] More languages
- [ ] Configuration file

## Key Design Decisions

### Why byte offsets everywhere?
- **Simplicity**: No line/column conversion overhead
- **Performance**: Direct indexing into ChunkTree
- **Correctness**: No UTF-8 boundary bugs
- **Trade-off**: Must use helper functions for line-based ops

### Why lazy line cache?
- **Memory**: Don't pay for what you don't use
- **Speed**: Only compute visible range initially
- **Incremental**: Rebuild affected ranges on edit
- **Trade-off**: First scroll to new area might be slower

### Why limited highlight cache?
- **Latency**: Only parse what's visible
- **Memory**: Don't cache entire file's parse tree
- **Best-effort**: Skip if too slow, user doesn't notice
- **Trade-off**: Scrolling might briefly show unstyled text

### Why sorted cursor list?
- **Simplicity**: Easy to merge/normalize
- **Correctness**: Applying edits back-to-front is trivial
- **Performance**: O(n log n) for n cursors, fine for n < 1000
- **Trade-off**: Could use tree for 10,000+ cursors (overkill)

## Testing Strategy

### Unit Tests
- ChunkTree operations (already exists)
- Buffer line cache correctness
- Cursor adjustment after edits
- Multi-cursor normalization

### Integration Tests
- Load 1GB file
- Edit with 100 cursors
- Scroll through entire file
- Syntax highlighting doesn't crash

### Performance Tests
- Keystroke latency benchmark
- Multi-cursor edit benchmark
- Syntax highlighting benchmark
- Memory usage with large files

## Next Steps

Ready to start implementing? I propose:

1. **Delete old code**: Remove virtual_file.rs, lines.rs, memstore.rs, logs.rs, old main.rs
2. **Start with buffer.rs**: Implement Buffer + line cache
3. **Simple main.rs**: Load file, show content, quit on 'q'
4. **Iterate**: Add features one by one, test constantly

Thoughts?
