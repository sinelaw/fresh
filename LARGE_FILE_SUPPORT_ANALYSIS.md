# Large File Support Analysis

## Executive Summary

This document analyzes how to extend the `piece_tree` and `text_buffer` implementation to support huge files (multi-GB) through:
1. **Optional line indexing** - Skip line metadata computation for large files
2. **Lazy loading** - Load file content on-demand rather than eagerly
3. **Constant approximation** - Use estimated line lengths for position access
4. **Unloaded buffer state** - Support buffers that reference file regions without loading data

---

## Current Architecture Overview

### Data Flow

```
File on Disk
    ↓ (eager load - read entire file)
Vec<u8> in memory
    ↓ (compute all line starts)
StringBuffer { data: Vec<u8>, line_starts: Vec<usize> }
    ↓ (single piece spanning entire buffer)
PieceTree { single leaf node }
    ↓ (edits create new pieces)
TextBuffer { piece_tree, buffers }
```

### Key Components

**StringBuffer** (piece_tree.rs:13-66)
- Stores actual text data: `data: Vec<u8>`
- Precomputed line metadata: `line_starts: Vec<usize>`
- Line starts computed eagerly in `new()` by scanning for `\n` bytes
- Memory: O(file_size) for data + O(line_count) for line_starts

**PieceTreeNode** (piece_tree.rs:86-454)
- Tree structure with line tracking at every node
- Internal nodes: aggregate metadata (`lf_left` - line feeds in left subtree)
- Leaf nodes: reference to `(buffer_id, offset, bytes, line_feed_cnt)`
- Does NOT store text, only references

**PieceTree** (piece_tree.rs:456-1037)
- Immutable tree of Arc-wrapped nodes
- Provides O(log n) position ↔ offset conversions
- Line counting integrated into tree structure

**TextBuffer** (text_buffer.rs:54-982)
- Owns `Vec<StringBuffer>` - the actual storage
- Wraps PieceTree for high-level operations
- Loading: `load_from_file()` reads entire file into memory (line 123-137)

### Current Limitations for Large Files

1. **Memory overhead**: 10 GB file = 10 GB for data + ~100 MB for line_starts (if avg 100 bytes/line)
2. **Startup time**: Must read entire file + compute all line starts before use
3. **Mandatory line indexing**: No way to skip line metadata computation
4. **No lazy loading**: All data loaded even if only viewing small portion

---

## Proposed Architecture for Large Files

### Design Goals

1. **Fast startup**: Don't load entire file content
2. **Low memory**: Only load viewed/edited regions
3. **Optional line indexing**: Skip line metadata for large files
4. **Graceful degradation**: Approximate line access when no index
5. **Transparent edits**: Modifications work same as small files

### Core Concept: Lazy-Loaded Buffers

```
File on Disk
    ↓ (stat file, create metadata only)
UnloadedBuffer { file_path, file_offset, bytes, state: Unloaded }
    ↓ (create piece tree WITHOUT loading data)
PieceTree { single leaf referencing unloaded buffer }
    ↓ (access triggers lazy load)
Load chunk around access point
    ↓ (split piece, create loaded buffer for chunk)
PieceTree { ... UnloadedBuffer ... LoadedBuffer(chunk) ... UnloadedBuffer ... }
```

---

## Detailed Design

### 1. Buffer State Management

**Modify StringBuffer to support unloaded state:**

```rust
pub enum BufferData {
    /// Loaded in memory
    Loaded {
        data: Vec<u8>,
        line_starts: Option<Vec<usize>>,  // Optional for large files
    },
    /// Not yet loaded from file
    Unloaded {
        file_path: PathBuf,
        file_offset: usize,  // Where in file this buffer starts
        bytes: usize,        // Length of this region
    },
}

pub struct StringBuffer {
    pub id: usize,
    pub data: BufferData,
}

impl StringBuffer {
    /// Create buffer for file region (not yet loaded)
    pub fn new_unloaded(
        id: usize,
        file_path: PathBuf,
        file_offset: usize,
        bytes: usize
    ) -> Self {
        StringBuffer {
            id,
            data: BufferData::Unloaded {
                file_path,
                file_offset,
                bytes,
            },
        }
    }

    /// Create loaded buffer with optional line indexing
    pub fn new_loaded(
        id: usize,
        data: Vec<u8>,
        compute_lines: bool
    ) -> Self {
        let line_starts = if compute_lines {
            Some(Self::compute_line_starts(&data))
        } else {
            None
        };

        StringBuffer {
            id,
            data: BufferData::Loaded { data, line_starts },
        }
    }

    /// Check if buffer is loaded
    pub fn is_loaded(&self) -> bool {
        matches!(self.data, BufferData::Loaded { .. })
    }

    /// Get data, loading if necessary
    pub fn ensure_loaded(&mut self) -> io::Result<&[u8]> {
        match &self.data {
            BufferData::Loaded { data, .. } => Ok(data),
            BufferData::Unloaded { file_path, file_offset, bytes } => {
                // Load this region from file
                let mut file = File::open(file_path)?;
                file.seek(SeekFrom::Start(*file_offset as u64))?;

                let mut buffer = vec![0u8; *bytes];
                file.read_exact(&mut buffer)?;

                // Replace with loaded data (no line indexing for lazy-loaded chunks)
                self.data = BufferData::Loaded {
                    data: buffer,
                    line_starts: None,
                };

                // Get reference to newly loaded data
                match &self.data {
                    BufferData::Loaded { data, .. } => Ok(data),
                    _ => unreachable!(),
                }
            }
        }
    }
}
```

### 2. Large File Detection and Initialization

**Modify TextBuffer::load_from_file():**

```rust
// Configuration
const LARGE_FILE_THRESHOLD: usize = 100 * 1024 * 1024; // 100 MB
const LOAD_CHUNK_SIZE: usize = 1024 * 1024; // 1 MB chunks
const CHUNK_ALIGNMENT: usize = 64 * 1024; // 64 KB alignment

pub struct TextBuffer {
    piece_tree: PieceTree,
    buffers: Vec<StringBuffer>,
    next_buffer_id: usize,
    file_path: Option<PathBuf>,
    modified: bool,
    large_file: bool,  // NEW: track if this is a large file
}

impl TextBuffer {
    pub fn load_from_file<P: AsRef<Path>>(
        path: P,
        large_file_threshold: usize,
    ) -> io::Result<Self> {
        let path = path.as_ref();
        let metadata = std::fs::metadata(path)?;
        let file_size = metadata.len() as usize;

        let large_file = file_size >= large_file_threshold;

        if large_file {
            // Large file: create unloaded buffer
            Self::load_large_file(path, file_size)
        } else {
            // Small file: eager load with line indexing
            Self::load_small_file(path)
        }
    }

    fn load_small_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        // Current implementation - eager load with line indexing
        let path = path.as_ref();
        let mut file = std::fs::File::open(path)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let mut buffer = Self::from_bytes_with_lines(contents, true);
        buffer.file_path = Some(path.to_path_buf());
        buffer.modified = false;
        buffer.large_file = false;
        Ok(buffer)
    }

    fn load_large_file<P: AsRef<Path>>(path: P, file_size: usize) -> io::Result<Self> {
        let path = path.as_ref();

        // Create unloaded buffer spanning entire file
        let buffer = StringBuffer::new_unloaded(
            0,
            path.to_path_buf(),
            0,
            file_size,
        );

        // Create piece tree with single leaf (no line feed count - we don't know yet)
        let piece_tree = PieceTree::new(
            BufferLocation::Stored(0),
            0,
            file_size,
            0,  // line_feed_cnt = 0 (unknown for unloaded buffers)
        );

        Ok(TextBuffer {
            piece_tree,
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: Some(path.to_path_buf()),
            modified: false,
            large_file: true,
        })
    }

    fn from_bytes_with_lines(content: Vec<u8>, compute_lines: bool) -> Self {
        let bytes = content.len();
        let buffer = StringBuffer::new_loaded(0, content, compute_lines);
        let line_feed_cnt = if compute_lines {
            buffer.line_feed_count()
        } else {
            0
        };

        TextBuffer {
            piece_tree: if bytes > 0 {
                PieceTree::new(BufferLocation::Stored(0), 0, bytes, line_feed_cnt)
            } else {
                PieceTree::empty()
            },
            buffers: vec![buffer],
            next_buffer_id: 1,
            file_path: None,
            modified: false,
            large_file: !compute_lines,
        }
    }
}
```

### 3. Lazy Loading on Access

**Trigger loading when accessing data:**

```rust
impl TextBuffer {
    /// Get text in a byte range, loading chunks as needed
    pub fn get_text_range(&mut self, offset: usize, length: usize) -> Vec<u8> {
        let mut result = Vec::with_capacity(length);
        let mut current_offset = offset;
        let mut remaining = length;

        while remaining > 0 {
            // Find piece containing current_offset
            let piece_info = match self.piece_tree.find_by_offset(current_offset) {
                Some(info) => info,
                None => break,
            };

            let buffer_id = piece_info.location.buffer_id();
            let buffer = &mut self.buffers[buffer_id];

            // LAZY LOAD: Check if buffer is unloaded
            if !buffer.is_loaded() {
                // Load chunk around requested offset
                self.load_chunk_for_piece(buffer_id, current_offset)?;
                // Note: load_chunk_for_piece will split the piece tree
                // Continue loop to get new piece info
                continue;
            }

            // Buffer is loaded, extract data
            let data = match &buffer.data {
                BufferData::Loaded { data, .. } => data,
                _ => unreachable!(),
            };

            let offset_in_piece = piece_info.offset_in_piece.unwrap_or(0);
            let piece_offset = piece_info.offset + offset_in_piece;
            let available = piece_info.bytes - offset_in_piece;
            let to_copy = remaining.min(available);

            result.extend_from_slice(
                &data[piece_offset..piece_offset + to_copy]
            );

            current_offset += to_copy;
            remaining -= to_copy;
        }

        result
    }

    /// Load a chunk of file around the requested offset
    /// Splits the piece tree to create loaded region
    fn load_chunk_for_piece(
        &mut self,
        buffer_id: usize,
        doc_offset: usize
    ) -> io::Result<()> {
        let buffer = &self.buffers[buffer_id];

        // Get file info from unloaded buffer
        let (file_path, file_offset, total_bytes) = match &buffer.data {
            BufferData::Unloaded { file_path, file_offset, bytes } => {
                (file_path.clone(), *file_offset, *bytes)
            }
            _ => return Ok(()), // Already loaded
        };

        // Find piece containing doc_offset
        let piece_info = self.piece_tree.find_by_offset(doc_offset)
            .ok_or_else(|| io::Error::new(
                io::ErrorKind::InvalidInput,
                "Offset not found in piece tree"
            ))?;

        let offset_in_piece = piece_info.offset_in_piece.unwrap_or(0);
        let piece_offset = piece_info.offset;

        // Calculate aligned chunk to load
        let target_offset = piece_offset + offset_in_piece;
        let chunk_start = (target_offset / CHUNK_ALIGNMENT) * CHUNK_ALIGNMENT;
        let chunk_end = ((target_offset + LOAD_CHUNK_SIZE + CHUNK_ALIGNMENT - 1)
            / CHUNK_ALIGNMENT) * CHUNK_ALIGNMENT;
        let chunk_end = chunk_end.min(total_bytes);

        // Load chunk from file
        let mut file = File::open(&file_path)?;
        file.seek(SeekFrom::Start((file_offset + chunk_start) as u64))?;

        let chunk_size = chunk_end - chunk_start;
        let mut chunk_data = vec![0u8; chunk_size];
        file.read_exact(&mut chunk_data)?;

        // Create new loaded buffer for this chunk
        let new_buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;

        let loaded_buffer = StringBuffer::new_loaded(
            new_buffer_id,
            chunk_data,
            false  // Don't compute line starts for lazy-loaded chunks
        );
        self.buffers.push(loaded_buffer);

        // Split piece tree to insert loaded region
        // Before: [...] [UNLOADED 0-1000] [...]
        // After:  [...] [UNLOADED 0-100] [LOADED 100-200] [UNLOADED 200-1000] [...]

        if chunk_start > 0 {
            // Keep unloaded region before chunk
            self.piece_tree = self.piece_tree.insert(
                file_offset + chunk_start,
                BufferLocation::Stored(buffer_id),
                0,
                chunk_start,
                0,
            );
        }

        // Insert loaded chunk
        self.piece_tree = self.piece_tree.insert(
            file_offset + chunk_start,
            BufferLocation::Stored(new_buffer_id),
            0,
            chunk_size,
            0,  // line_feed_cnt unknown
        );

        if chunk_end < total_bytes {
            // Keep unloaded region after chunk
            self.piece_tree = self.piece_tree.insert(
                file_offset + chunk_end,
                BufferLocation::Stored(buffer_id),
                chunk_end,
                total_bytes - chunk_end,
                0,
            );
        }

        Ok(())
    }
}
```

### 4. Position Access with Constant Approximation

**For large files without line indexing:**

```rust
// Configuration
const ASSUMED_LINE_LENGTH: usize = 80;  // Bytes per line estimate

impl TextBuffer {
    /// Convert position to offset
    /// For large files, uses approximation then binary search for exact position
    pub fn position_to_offset(&mut self, position: Position) -> usize {
        if !self.large_file {
            // Small file: use exact line indexing
            return self.piece_tree.position_to_offset(
                position.line,
                position.column,
                &self.buffers
            );
        }

        // Large file: use approximation
        self.position_to_offset_approximate(position)
    }

    fn position_to_offset_approximate(&mut self, position: Position) -> usize {
        // Initial approximation
        let approx_offset = position.line * ASSUMED_LINE_LENGTH + position.column;
        let approx_offset = approx_offset.min(self.total_bytes());

        // Load chunk around approximation
        if approx_offset > 0 {
            // Ensure data is loaded (will trigger lazy load if needed)
            let _ = self.get_text_range(approx_offset, 1);
        }

        // Binary search for actual line start
        // This requires loading chunks but not full line index
        self.find_line_start_binary_search(position.line, approx_offset)
    }

    fn find_line_start_binary_search(&mut self, target_line: usize, hint: usize) -> usize {
        let total_bytes = self.total_bytes();
        if total_bytes == 0 || target_line == 0 {
            return 0;
        }

        let mut left = 0;
        let mut right = total_bytes;
        let mut best = hint;

        // Binary search for line start
        // At each step, count lines from 0 to mid and adjust range
        while left < right {
            let mid = (left + right) / 2;

            // Count lines from 0 to mid (may trigger lazy loads)
            let lines_up_to_mid = self.count_lines_up_to(mid);

            if lines_up_to_mid < target_line {
                left = mid + 1;
            } else if lines_up_to_mid > target_line {
                right = mid;
            } else {
                // Found exact line start
                best = mid;
                break;
            }

            // Limit iterations to prevent too many loads
            if right - left < ASSUMED_LINE_LENGTH {
                // Close enough, do linear search
                return self.find_line_start_linear(target_line, left);
            }
        }

        best
    }

    fn count_lines_up_to(&mut self, offset: usize) -> usize {
        // Count newlines from 0 to offset
        // This will trigger lazy loads for accessed regions
        let data = self.get_text_range(0, offset);
        data.iter().filter(|&&b| b == b'\n').count()
    }

    fn find_line_start_linear(&mut self, target_line: usize, start: usize) -> usize {
        let chunk = self.get_text_range(start, LOAD_CHUNK_SIZE);
        let mut current_line = self.count_lines_up_to(start);

        for (i, &byte) in chunk.iter().enumerate() {
            if byte == b'\n' {
                current_line += 1;
                if current_line == target_line {
                    return start + i + 1;
                }
            }
        }

        start
    }

    /// Line count for large files (expensive - scans entire file if needed)
    pub fn line_count(&mut self) -> usize {
        if !self.large_file {
            // Small file: use piece tree
            return self.piece_tree.line_count();
        }

        // Large file: this is expensive, avoid if possible
        // Could cache result after first computation
        self.count_lines_up_to(self.total_bytes()) + 1
    }
}
```

### 5. Handling Edits in Large Files

**Edits work normally but create loaded buffers:**

```rust
impl TextBuffer {
    pub fn insert_bytes(&mut self, offset: usize, text: Vec<u8>) -> Cursor {
        if text.is_empty() {
            return self.piece_tree.cursor_at_offset(offset);
        }

        self.modified = true;

        // For large files, we don't compute line feeds for inserted text either
        let line_feed_cnt = if !self.large_file {
            text.iter().filter(|&&b| b == b'\n').count()
        } else {
            0
        };

        // Create new buffer for inserted text (always loaded)
        let buffer_id = self.next_buffer_id;
        self.next_buffer_id += 1;

        let buffer = StringBuffer::new_loaded(
            buffer_id,
            text.clone(),
            !self.large_file,  // Compute lines only for small files
        );
        self.buffers.push(buffer);

        // Insert into piece tree
        self.piece_tree = self.piece_tree.insert(
            offset,
            BufferLocation::Added(buffer_id),
            0,
            text.len(),
            line_feed_cnt,
        );

        self.piece_tree.cursor_at_offset(offset + text.len())
    }
}
```

---

## Implementation Strategy

### Phase 1: Add Unloaded Buffer Support

1. **Modify StringBuffer** to use `BufferData` enum
2. Add `new_unloaded()` and `new_loaded()` constructors
3. Add `ensure_loaded()` method for lazy loading
4. Update all buffer access sites to handle loading

**Files to modify:**
- `src/piece_tree.rs`: StringBuffer struct and impl
- All sites that access `buffer.data` directly

**Testing:**
- Create unloaded buffer, verify it loads on access
- Test with small file converted to unloaded format

### Phase 2: Implement Large File Initialization

1. **Add large file detection** in `load_from_file()`
2. Implement `load_large_file()` - create unloaded buffer for entire file
3. Add `large_file: bool` field to TextBuffer
4. Modify `from_bytes()` to accept `compute_lines` parameter

**Files to modify:**
- `src/text_buffer.rs`: load_from_file, new fields

**Testing:**
- Load file just below threshold (eager load)
- Load file just above threshold (lazy load)
- Verify memory usage is O(1) for large files initially

### Phase 3: Implement Lazy Loading on Access

1. **Modify `get_text_range()`** to check for unloaded buffers
2. Implement `load_chunk_for_piece()` with alignment
3. **Piece tree splitting** when inserting loaded chunks
4. Handle piece tree updates after loading

**Files to modify:**
- `src/text_buffer.rs`: get_text_range, new helper methods
- `src/piece_tree.rs`: Ensure insert/delete work with zero line_feed_cnt

**Testing:**
- Access different offsets, verify correct chunks loaded
- Verify piece tree correctly splits unloaded/loaded regions
- Test overlapping accesses, avoid redundant loads

### Phase 4: Position Approximation

1. **Implement `position_to_offset_approximate()`**
2. Add binary search for line starts
3. Add linear search fallback for nearby offsets
4. Cache line count if computed

**Files to modify:**
- `src/text_buffer.rs`: position conversion methods

**Testing:**
- Test position access with various line lengths
- Verify approximation converges to correct position
- Test edge cases (empty lines, very long lines)

### Phase 5: Optimize and Tune

1. **Chunk size tuning** based on access patterns
2. **Caching** frequently accessed chunks
3. **Eviction policy** for loaded chunks (LRU)
4. **Prefetching** nearby chunks

**Files to modify:**
- All files, add configuration constants

**Testing:**
- Benchmark with real large files
- Profile memory usage
- Test scrolling performance

---

## Challenges and Trade-offs

### Challenges

**1. Piece Tree Splitting Complexity**
- Inserting loaded chunks into tree with unloaded pieces
- Maintaining tree balance after frequent splits
- Handling overlapping loads

**Solution:** Careful piece tree insertion logic, track loaded ranges

**2. Position Access Performance**
- Without line index, line→offset is expensive
- Binary search requires multiple chunk loads
- Cannot do O(log n) traversal without line counts

**Solution:**
- Cache approximation parameters (avg line length)
- Prefer byte offsets for large file operations
- Warn users that line-based access is slow

**3. Line Count for Large Files**
- Getting total line count requires scanning entire file
- Progress dialogs, status bars need line count
- Caching helps but initial computation still expensive

**Solution:**
- Make line_count() async with progress callback
- Cache result after first computation
- Show "~N lines (estimated)" before exact count available

**4. Edit Performance**
- Edits may be at unloaded offset, triggering load
- Frequent small edits cause many small loads
- Need to balance chunk size vs memory

**Solution:**
- Load larger chunks (1 MB) on first edit in region
- Keep edit buffers in memory even if file buffer evicted
- Consider marking "hot" regions to keep loaded

**5. Memory Management**
- Loaded chunks accumulate over time
- Need eviction policy but can't evict dirty buffers
- LRU requires tracking access patterns

**Solution:**
- Implement chunk eviction (drop Vec<u8>, keep metadata)
- Never evict Added buffers (user edits)
- Can reload Stored buffers from file
- Add `evict()` method to StringBuffer

### Trade-offs

**With Line Indexing (Small Files)**
- ✅ Fast line→offset conversion: O(log n)
- ✅ Fast line count: O(1)
- ✅ Fast offset→line conversion: O(log n)
- ❌ Memory overhead: ~1% of file size for line_starts
- ❌ Startup time: scan entire file

**Without Line Indexing (Large Files)**
- ✅ Low memory: O(viewed_regions)
- ✅ Fast startup: O(1) to stat file
- ✅ Supports huge files: multi-GB files feasible
- ❌ Slow line→offset: O(n) worst case, requires chunk loads
- ❌ Expensive line count: O(n) file scan
- ❌ Unpredictable performance: depends on access pattern

**Recommendation:**
- Use line indexing for files < 100 MB
- Disable line indexing for files ≥ 100 MB
- Provide user override for threshold
- Display warning when opening large file: "Line numbers are approximate"

---

## Alternative Approaches Considered

### A. Sparse Line Index

**Idea:** Index every Nth line (e.g., every 1000 lines)

**Pros:**
- Reduces memory by 99.9% for line index
- Still allows O(log n + k) line access where k=lines_between_index_points

**Cons:**
- More complex implementation
- Still requires scanning 1000 lines for exact position
- Doesn't solve startup time (must scan file to build sparse index)

### B. Memory-Mapped Files

**Idea:** Use mmap() to map file into address space

**Pros:**
- OS handles paging automatically
- No explicit chunk loading code
- Works with existing buffer.data access patterns

**Cons:**
- Line indexing still requires scanning (startup problem remains)
- mmap limit on 32-bit systems
- Platform-specific code
- Complicates modification handling (need separate changed regions)

### C. External Line Index File

**Idea:** Compute and cache line index in separate .idx file

**Pros:**
- First open is slow, subsequent opens fast
- Line index can be loaded lazily
- Shareable across editor instances

**Cons:**
- Cache invalidation complexity
- Extra files to manage
- Stale index risk
- Storage overhead

**Verdict:** Could be added later as enhancement, but lazy loading is more fundamental

---

## Expected Performance Characteristics

### Startup Time

| File Size | Current (eager) | Proposed (lazy) |
|-----------|----------------|-----------------|
| 10 MB     | 20 ms          | 20 ms (indexed) |
| 100 MB    | 200 ms         | 2 ms            |
| 1 GB      | 2000 ms        | 2 ms            |
| 10 GB     | 20 sec         | 2 ms            |

### Memory Usage

| File Size | Current (eager)     | Proposed (lazy)        |
|-----------|---------------------|------------------------|
| 10 MB     | 10 MB + 100 KB idx  | 10 MB + 100 KB idx     |
| 100 MB    | 100 MB + 1 MB idx   | ~5 MB (chunks viewed)  |
| 1 GB      | 1 GB + 10 MB idx    | ~10 MB (chunks viewed) |
| 10 GB     | OOM                 | ~10-50 MB              |

### Access Patterns

**Sequential read (viewing file top to bottom):**
- Current: Fast after initial load
- Proposed: Slightly slower (chunk loads), but similar overall

**Random access (jumping to different lines):**
- Current: Fast with line index
- Proposed: Slower (must approximate + search), each jump may load chunk

**Editing:**
- Current: Fast (loaded in memory)
- Proposed: Similar (edited regions are loaded)

**Line count:**
- Current: O(1)
- Proposed: O(n) first time, cached thereafter

---

## Recommendations

### Implementation Priority

**Must Have (MVP):**
1. ✅ Unloaded buffer state
2. ✅ Lazy loading on access
3. ✅ Large file detection and initialization
4. ✅ Basic chunk loading (aligned, 1 MB chunks)

**Should Have:**
5. Position approximation for line access
6. Chunk eviction (LRU)
7. Configuration for thresholds

**Nice to Have:**
8. Prefetching heuristics
9. Sparse line index option
10. Background line count computation
11. Memory usage monitoring

### Configuration

```rust
pub struct LargeFileConfig {
    /// Files larger than this use lazy loading
    pub threshold: usize,  // Default: 100 MB

    /// Chunk size to load at once
    pub chunk_size: usize,  // Default: 1 MB

    /// Chunk alignment boundary
    pub chunk_alignment: usize,  // Default: 64 KB

    /// Assumed average line length for approximation
    pub assumed_line_length: usize,  // Default: 80 bytes

    /// Maximum loaded chunks to keep in memory
    pub max_cached_chunks: usize,  // Default: 100 (100 MB if 1 MB chunks)

    /// Force line indexing even for large files
    pub force_line_index: bool,  // Default: false
}
```

### Testing Strategy

**Unit Tests:**
- StringBuffer loading/unloading
- Chunk alignment calculations
- Piece tree splitting with unloaded buffers
- Position approximation edge cases

**Integration Tests:**
- Load 200 MB file, verify only metadata loaded
- Access offset 1 GB into file, verify correct chunk loaded
- Edit at various offsets, verify piece tree updates
- Sequential reads vs random access patterns

**Performance Tests:**
- Benchmark startup time for various file sizes
- Memory profiling during large file editing
- Measure line→offset latency with/without index

**Real-World Tests:**
- Large log files (GBs)
- Large JSON/XML files
- Large source code files
- Binary files (verify UTF-8 handling)

---

## Conclusion

The proposed architecture enables supporting files of arbitrary size by:
1. Deferring file loading until access
2. Loading data in aligned chunks on-demand
3. Making line indexing optional for memory savings
4. Using approximation for position access when no index

**Key insight:** Piece tree already separates logical structure from physical storage, making it natural to support unloaded buffers.

**Trade-off:** Fast startup and low memory vs slower line-based access for large files.

**Next steps:**
1. Implement Phase 1 (unloaded buffer support)
2. Test with progressively larger files
3. Measure performance and tune chunk sizes
4. Add configuration and user controls
