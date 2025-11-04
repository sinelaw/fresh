# Scorched Earth Rewrite Plan

## Goal

Replace current ChunkTree-based Buffer with VirtualBuffer architecture from DESIGN.md in as few steps as possible.

## Key Principles

1. **No migration period** - Rip and replace in one go
2. **Drop all heuristics** - No line_to_byte, line_end_byte, LineCache
3. **Iterator-only API** - No "find line start" methods exposed
4. **Keep ChunkTree** - Wrap it as a persistence backend, don't rewrite it

## What Gets Deleted (800+ lines)

### From buffer.rs:
- ❌ `struct LineCache` (entire implementation)
- ❌ `line_to_byte()` - replaced by iterators
- ❌ `line_end_byte()` - replaced by iterators
- ❌ `line_end_byte_with_newline()` - replaced by iterators
- ❌ `byte_to_line()` - replaced by iterators
- ❌ `byte_to_line_lazy()` - replaced by iterators
- ❌ `display_line_number()` - replaced by iterators
- ❌ `find_line_start_at_byte()` - made internal to LineIterator
- ❌ `find_line_end_at_byte()` - unused
- ❌ `register_line_in_cache()` - no more cache
- ❌ `register_line_in_cache_efficient()` - no more cache
- ❌ `ensure_line_cache()` - no more cache
- ❌ `count_newlines_in_range()` - unused
- ❌ `is_last_line()` - replaced by iterator check
- ❌ `LineNumber` enum - no more absolute/relative distinction

### From chunk_tree.rs:
- ❌ Old `ByteIterator` struct (with lifetime `'a`) - replaced with Arc-based version
- ❌ `bytes_range()` method - replaced by `bytes_at()`

### From editor.rs:
- ❌ All direct calls to `line_to_byte()`, `line_end_byte()`
- ❌ All direct calls to `find_line_start_at_byte()`

**Total deletion: ~800-1000 lines**

## What Gets Added (600+ lines)

### New files:
1. **src/virtual_buffer.rs** (~250 lines)
   - `VirtualBuffer` - Arc-based handle
   - `InnerBuffer` - shared state
   - `ByteIterator` - with edit tracking
   - Edit log management

2. **src/persistence.rs** (~150 lines)
   - `PersistenceLayer` trait
   - `ChunkTreePersistence` - wraps existing ChunkTree
   - `MemoryPersistence` - for testing

3. **src/cache.rs** (~100 lines)
   - `Cache` struct - BTreeMap-based
   - Simple LRU eviction
   - Dirty region tracking

4. **src/edit.rs** (~50 lines)
   - `Edit` struct
   - `EditKind` enum
   - Helper methods

### Modified files:
5. **src/buffer.rs** - Gutted and replaced (~150 lines)
   - New `Buffer` wraps `VirtualBuffer`
   - Minimal public API
   - `LineIterator` rewrite (Arc-based, no lifetimes)

6. **src/editor.rs** - Fix call sites (~50 lines of changes)
   - Remove all line_to_byte calls
   - Use iterators everywhere

**Total addition: ~600-750 lines**

## Step-by-Step Implementation

### Step 1: Create New Core (New files only, nothing breaks)

**Files to create:**
- `src/virtual_buffer.rs`
- `src/persistence.rs`
- `src/cache.rs`
- `src/edit.rs`

**What to implement:**

```rust
// src/edit.rs
#[derive(Clone, Debug)]
pub struct Edit {
    pub version: u64,
    pub kind: EditKind,
}

#[derive(Clone, Debug)]
pub enum EditKind {
    Insert { offset: usize, len: usize },
    Delete { offset: usize, len: usize },
}

// src/persistence.rs
pub trait PersistenceLayer: Send {
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>>;
    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()>;
    fn delete(&mut self, range: Range<usize>) -> io::Result<()>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool { self.len() == 0 }
}

pub struct ChunkTreePersistence {
    tree: ChunkTree<'static>,
}

impl PersistenceLayer for ChunkTreePersistence {
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>> {
        Ok(self.tree.bytes_at(offset)
            .take(len)
            .collect())
    }

    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()> {
        self.tree.insert(offset, data);
        Ok(())
    }

    fn delete(&mut self, range: Range<usize>) -> io::Result<()> {
        self.tree.remove(range);
        Ok(())
    }

    fn len(&self) -> usize {
        self.tree.len()
    }
}

// src/cache.rs
pub struct Cache {
    regions: BTreeMap<usize, Vec<u8>>,
    max_size: usize,
}

impl Cache {
    pub fn new(max_size: usize) -> Self {
        Self {
            regions: BTreeMap::new(),
            max_size,
        }
    }

    pub fn read(&self, offset: usize, len: usize) -> Option<&[u8]> {
        // Find region containing offset
        // Return slice
    }

    pub fn write(&mut self, offset: usize, data: Vec<u8>) {
        // Store region
        // Evict if needed
    }

    pub fn ensure_cached<P: PersistenceLayer>(
        &mut self,
        persistence: &P,
        offset: usize,
        len: usize,
    ) -> io::Result<()> {
        // Check if already cached
        // If not, load from persistence
    }
}

// src/virtual_buffer.rs
pub struct VirtualBuffer {
    inner: Arc<InnerBuffer>,
}

struct InnerBuffer {
    persistence: Mutex<Box<dyn PersistenceLayer>>,
    cache: Mutex<Cache>,
    edit_log: RwLock<Vec<Edit>>,
    edit_version: AtomicU64,
    active_iterator_versions: Mutex<BTreeSet<u64>>,
}

pub struct ByteIterator {
    buffer: Arc<InnerBuffer>,
    position: usize,
    version_at_creation: u64,
}

impl VirtualBuffer {
    pub fn new(persistence: Box<dyn PersistenceLayer>) -> Self { ... }
    pub fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>> { ... }
    pub fn insert(&self, offset: usize, data: &[u8]) -> io::Result<()> { ... }
    pub fn delete(&self, range: Range<usize>) -> io::Result<()> { ... }
    pub fn len(&self) -> usize { ... }
    pub fn iter_at(&self, position: usize) -> ByteIterator { ... }
}

impl ByteIterator {
    pub fn next(&mut self) -> Option<u8> { ... }
    pub fn prev(&mut self) -> Option<u8> { ... }
    pub fn seek(&mut self, position: usize) { ... }
    pub fn position(&self) -> usize { ... }
    pub fn peek(&self) -> Option<u8> { ... }
    fn adjust_for_edits(&mut self) { ... }
}

impl Drop for ByteIterator {
    fn drop(&mut self) {
        // Unregister from active_iterator_versions
    }
}
```

**Add to src/lib.rs:**
```rust
mod virtual_buffer;
mod persistence;
mod cache;
mod edit;
```

**Test it:**
```rust
#[test]
fn test_virtual_buffer_basic() {
    let tree = ChunkTree::from_slice(b"hello world");
    let persistence = Box::new(ChunkTreePersistence::new(tree));
    let vbuf = VirtualBuffer::new(persistence);

    // Test read
    assert_eq!(vbuf.read(0, 5).unwrap(), b"hello");

    // Test insert
    vbuf.insert(5, b" beautiful").unwrap();
    assert_eq!(vbuf.read(0, 15).unwrap(), b"hello beautiful");

    // Test iterator
    let mut iter = vbuf.iter_at(0);
    assert_eq!(iter.next(), Some(b'h'));
}
```

### Step 2: Gut and Replace buffer.rs

**Delete these sections:**
```rust
// DELETE:
struct LineCache { ... }
impl LineCache { ... }
enum LineNumber { ... }
impl LineNumber { ... }

// DELETE these methods from Buffer:
fn line_to_byte(&self, line: usize) -> usize
fn line_end_byte(&self, line: usize) -> usize
fn line_end_byte_with_newline(&self, line: usize) -> usize
fn byte_to_line_lazy(&self, byte: usize) -> LineNumber
fn display_line_number(&self, byte: usize) -> LineNumber
fn find_line_start_at_byte(&self, byte: usize) -> usize
fn register_line_in_cache(...)
fn ensure_line_cache(...)
fn count_newlines_in_range(...)
fn is_last_line(...)
```

**Replace Buffer struct:**
```rust
pub struct Buffer {
    // NEW:
    virtual_buffer: VirtualBuffer,

    // KEEP:
    file_path: Option<PathBuf>,
    modified: bool,
}

impl Buffer {
    pub fn new() -> Self {
        let tree = ChunkTree::new(DEFAULT_CONFIG);
        let persistence = Box::new(ChunkTreePersistence::new(tree));
        Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: None,
            modified: false,
        }
    }

    pub fn from_str(text: &str) -> Self {
        let tree = ChunkTree::from_slice(text.as_bytes());
        let persistence = Box::new(ChunkTreePersistence::new(tree));
        Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: None,
            modified: false,
        }
    }

    pub fn load_from_file(path: &Path) -> io::Result<Self> {
        let contents = std::fs::read(path)?;
        let tree = ChunkTree::from_slice(&contents);
        let persistence = Box::new(ChunkTreePersistence::new(tree));
        Ok(Self {
            virtual_buffer: VirtualBuffer::new(persistence),
            file_path: Some(path.to_path_buf()),
            modified: false,
        })
    }

    pub fn save(&mut self) -> io::Result<()> {
        if let Some(path) = &self.file_path {
            let contents = self.virtual_buffer.read(0, self.virtual_buffer.len())?;
            std::fs::write(path, contents)?;
            self.modified = false;
        }
        Ok(())
    }

    pub fn insert(&mut self, pos: usize, text: &str) -> io::Result<()> {
        self.virtual_buffer.insert(pos, text.as_bytes())?;
        self.modified = true;
        Ok(())
    }

    pub fn delete(&mut self, range: Range<usize>) -> io::Result<()> {
        self.virtual_buffer.delete(range)?;
        self.modified = true;
        Ok(())
    }

    pub fn slice(&self, range: Range<usize>) -> String {
        let bytes = self.virtual_buffer.read(range.start, range.len())
            .unwrap_or_default();
        String::from_utf8_lossy(&bytes).to_string()
    }

    pub fn len(&self) -> usize {
        self.virtual_buffer.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // ONLY iterator API exposed:
    pub fn line_iterator(&self, byte_pos: usize) -> LineIterator {
        LineIterator::new(&self.virtual_buffer, byte_pos)
    }
}
```

**Replace LineIterator:**
```rust
pub struct LineIterator {
    byte_iter: crate::virtual_buffer::ByteIterator,
}

impl LineIterator {
    /// Create line iterator at any byte position
    /// Automatically finds the start of the line containing byte_pos
    pub fn new(vbuf: &VirtualBuffer, byte_pos: usize) -> Self {
        let mut byte_iter = vbuf.iter_at(byte_pos);

        // Scan backward to find line start (newline or position 0)
        while byte_iter.position() > 0 {
            byte_iter.prev();
            if byte_iter.peek() == Some(b'\n') {
                byte_iter.next(); // Move past newline to line start
                break;
            }
        }

        Self { byte_iter }
    }

    /// Get next line: (line_start_byte, line_content)
    /// line_content includes newline if present
    pub fn next(&mut self) -> Option<(usize, String)> {
        let line_start = self.byte_iter.position();

        if line_start >= self.byte_iter.buffer_len() {
            return None;
        }

        let mut content = String::new();

        // Read until newline or EOF
        loop {
            match self.byte_iter.next() {
                Some(b'\n') => {
                    content.push('\n');
                    break;
                }
                Some(byte) => {
                    content.push(byte as char);
                }
                None => break,
            }
        }

        Some((line_start, content))
    }

    /// Get previous line
    pub fn prev(&mut self) -> Option<(usize, String)> {
        let current_pos = self.byte_iter.position();

        if current_pos == 0 {
            return None;
        }

        // Step 1: Move back past newlines at current position
        self.byte_iter.seek(current_pos.saturating_sub(1));
        while self.byte_iter.position() > 0 && self.byte_iter.peek() == Some(b'\n') {
            self.byte_iter.prev();
        }

        // Step 2: Scan backward to find start of this line
        while self.byte_iter.position() > 0 {
            self.byte_iter.prev();
            if self.byte_iter.peek() == Some(b'\n') {
                self.byte_iter.next(); // Move past newline
                break;
            }
        }

        // Step 3: Read forward to get line content
        let line_start = self.byte_iter.position();
        let mut content = String::new();

        loop {
            match self.byte_iter.next() {
                Some(b'\n') => {
                    content.push('\n');
                    break;
                }
                Some(byte) => {
                    content.push(byte as char);
                }
                None => break,
            }
        }

        // Reset to line start for next operation
        self.byte_iter.seek(line_start);

        Some((line_start, content))
    }

    pub fn current_position(&self) -> usize {
        self.byte_iter.position()
    }
}
```

### Step 3: Fix editor.rs Call Sites

**Pattern to replace everywhere:**

```rust
// OLD:
let line = state.buffer.byte_to_line_lazy(cursor.position).value();
let line_start = state.buffer.line_to_byte(line);
let line_end = state.buffer.line_end_byte(line);

// NEW:
let mut iter = state.buffer.line_iterator(cursor.position);
if let Some((line_start, line_content)) = iter.next() {
    let line_end = line_start + line_content.len();
    // use line_start, line_end
}
```

**Specific places to fix:**

1. `add_cursor_above()` - Already fixed, verify it works
2. `add_cursor_below()` - Already fixed, verify it works
3. `Action::DeleteLine` - Already fixed, verify it works
4. `Action::SelectLineStart` - Replace with iterator
5. `Action::SelectLineEnd` - Replace with iterator
6. `render_content()` - Already uses iterator, should work
7. Any other `line_to_byte` / `line_end_byte` calls

**Search for remaining calls:**
```bash
grep -n "line_to_byte\|line_end_byte\|byte_to_line" src/editor.rs
```

Fix each one using the iterator pattern.

### Step 4: Fix viewport.rs

**Current viewport uses:**
- `byte_to_line_lazy()` - Replace with iterator-based counting
- `line_to_byte()` - Replace with iterator seeking
- `top_line` field of type `LineNumber` - Change to simple `usize` or remove

**Options:**
1. **Simple:** Remove `top_line` entirely, only use `top_byte` (already the source of truth)
2. **Keep display:** Calculate line number on-demand using iterator

**Recommended: Option 1 (simpler)**
```rust
pub struct Viewport {
    pub top_byte: usize,  // KEEP - source of truth
    // REMOVE: pub top_line: LineNumber
    pub left_column: usize,
    pub width: u16,
    pub height: u16,
    pub scroll_offset: usize,
    pub horizontal_scroll_offset: usize,
}

impl Viewport {
    // Gutter width can be calculated differently or made fixed
    pub fn gutter_width(&self) -> usize {
        // Option A: Fixed width
        7  // "9999 │ "

        // Option B: Dynamic based on buffer size (requires passing buffer)
        // Calculate digits needed for buffer.len()
    }
}
```

### Step 5: Fix Tests

**Tests that need updates:**
- Any test calling `line_to_byte()`, `line_end_byte()`, etc.
- Any test checking `LineCache` internals
- Any test using `LineNumber` enum

**Pattern:**
```rust
// OLD:
let line_start = buffer.line_to_byte(5);
assert_eq!(line_start, 42);

// NEW:
let mut iter = buffer.line_iterator(0);
for _ in 0..5 {
    iter.next();
}
let (line_start, _) = iter.next().unwrap();
assert_eq!(line_start, 42);
```

### Step 6: Delete Dead Code

**After everything compiles and tests pass:**

1. Remove unused imports
2. Remove `#[allow(dead_code)]` attributes
3. Run `cargo clippy` and fix warnings
4. Remove old `ByteIterator` from chunk_tree.rs if not used by `ChunkTreePersistence`

**Final cleanup:**
```bash
# Remove dead code warnings
cargo clippy --fix

# Format
cargo fmt

# Verify tests
cargo test
```

## Migration Checklist

- [ ] Step 1: Create new core files (virtual_buffer, persistence, cache, edit)
- [ ] Step 1: Test VirtualBuffer independently
- [ ] Step 2: Gut buffer.rs, remove LineCache and heuristics
- [ ] Step 2: Replace Buffer implementation with VirtualBuffer wrapper
- [ ] Step 2: Rewrite LineIterator (Arc-based, finds line start automatically)
- [ ] Step 2: Test Buffer API still works
- [ ] Step 3: Fix all editor.rs call sites (remove line_to_byte calls)
- [ ] Step 4: Fix viewport.rs (remove LineNumber, simplify)
- [ ] Step 5: Fix failing tests
- [ ] Step 6: Delete dead code, run clippy
- [ ] Step 6: Final test run (cargo test)
- [ ] Step 6: Run e2e tests (cargo test --test e2e_tests)

## Expected Breakage

**Will definitely break:**
- All tests calling `line_to_byte()` directly
- Tests checking `LineCache` state
- Any code using `LineNumber` enum
- Viewport line number display (needs rework)

**Should keep working:**
- Basic insert/delete/slice operations
- File load/save
- Most editor actions (once we fix call sites)
- Rendering (already uses iterator)

## Rollback Plan

If this goes badly:
1. The old code is in git history
2. Revert to last commit before Step 2
3. Step 1 (new files) can stay - they don't break anything

## Success Criteria

- [ ] All unit tests pass (cargo test)
- [ ] All e2e tests pass (cargo test --test e2e_tests)
- [ ] No `line_to_byte()` or `line_end_byte()` calls remain
- [ ] No `LineCache` code remains
- [ ] Buffer API is iterator-only
- [ ] Code compiles with no warnings

## Timeline Estimate

- **Step 1:** 4-6 hours (new implementation)
- **Step 2:** 2-3 hours (buffer.rs rewrite)
- **Step 3:** 1-2 hours (fix editor.rs)
- **Step 4:** 1 hour (fix viewport.rs)
- **Step 5:** 2-3 hours (fix tests)
- **Step 6:** 1 hour (cleanup)

**Total: 11-16 hours of focused work**

Can be split into:
- Day 1: Step 1 (implement core, test in isolation)
- Day 2: Steps 2-3 (replace buffer, fix editor)
- Day 3: Steps 4-6 (fix viewport/tests, cleanup)
