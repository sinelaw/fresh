# Virtual Buffer Design

## Overview

This document outlines the design for a virtual memory system for text editing with the following goals:

1. **Very large (infinite) underlying storage support** - Handle files larger than memory, or conceptually infinite buffers
2. **Caching of accessed regions** - Only load what's needed into memory
3. **Efficient byte-level iteration** - Seek to any position and iterate in either direction efficiently
4. **Support modifications during iteration** - Iterators automatically adjust when edits occur
5. **Pluggable persistence layer** - Backend can use deltas, separate insertion files, or any other strategy

## Architecture

### 1. Persistence Layer (Pluggable Backend)

The persistence layer is a trait that abstracts the underlying storage mechanism:

```rust
pub trait PersistenceLayer {
    /// Read bytes from backing store
    fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>>;

    /// Write bytes (could be delta-based, append-only, etc)
    fn write(&mut self, offset: usize, data: &[u8]) -> io::Result<()>;

    /// Insert bytes (backend decides how to handle - deltas, separate file, etc)
    fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()>;

    /// Delete bytes
    fn delete(&mut self, range: Range<usize>) -> io::Result<()>;

    /// Total logical size
    fn len(&self) -> usize;
}
```

**Example implementations:**

```rust
// Simple file-based persistence
pub struct FilePersistence {
    file: File,
    len: usize,
}

// Delta-based persistence (like git)
pub struct DeltaPersistence {
    base_file: File,
    deltas: Vec<Delta>,  // append-only delta log
}

// Sparse file with separate insertions
pub struct SparsePersistence {
    original: File,
    insertions: BTreeMap<usize, Vec<u8>>,  // separate storage for insertions
}

// Network-backed persistence
pub struct NetworkPersistence {
    url: String,
    cache: LocalCache,
}
```

### 2. Cache Layer

Manages loaded regions from the persistence layer:

```rust
pub struct Cache {
    // Map of loaded regions: (start_offset, data)
    regions: BTreeMap<usize, Vec<u8>>,

    // Track which regions are dirty (modified but not persisted)
    dirty: HashSet<usize>,

    // Cache eviction policy parameters
    max_cache_size: usize,
    eviction_policy: EvictionPolicy,  // LRU, LFU, etc.
}

impl Cache {
    /// Ensure a byte range is loaded into cache
    fn ensure_cached(&mut self, offset: usize, len: usize) -> io::Result<()>;

    /// Read from cache (assumes already loaded)
    fn read(&self, offset: usize, len: usize) -> Option<&[u8]>;

    /// Write to cache
    fn write(&mut self, offset: usize, data: &[u8]);

    /// Evict regions to stay under max_cache_size
    fn evict_if_needed(&mut self);
}
```

### 3. Virtual Buffer

The main buffer type that ties persistence and caching together:

```rust
pub struct VirtualBuffer {
    // Pluggable persistence backend
    persistence: Box<dyn PersistenceLayer>,

    // Cache for loaded regions
    cache: Cache,

    // Track modifications for iterator adjustment
    edit_log: Vec<Edit>,

    // Version counter - increments on each edit
    edit_version: u64,
}

impl VirtualBuffer {
    /// Create a new virtual buffer with a persistence backend
    pub fn new(persistence: Box<dyn PersistenceLayer>) -> Self;

    /// Read bytes (loads from cache, falls back to persistence)
    pub fn read(&self, offset: usize, len: usize) -> io::Result<Vec<u8>>;

    /// Insert bytes at offset
    pub fn insert(&mut self, offset: usize, data: &[u8]) -> io::Result<()>;

    /// Delete bytes in range
    pub fn delete(&mut self, range: Range<usize>) -> io::Result<()>;

    /// Create an iterator at a position
    pub fn iter_at(&self, position: usize) -> ByteIterator;

    /// Get edits since a version
    pub fn edits_since(&self, version: u64) -> &[Edit];
}
```

### 4. Edit Tracking

Track modifications for iterator adjustment:

```rust
#[derive(Clone, Debug)]
pub struct Edit {
    version: u64,
    kind: EditKind,
}

#[derive(Clone, Debug)]
pub enum EditKind {
    Insert { offset: usize, len: usize },
    Delete { offset: usize, len: usize },
}
```

### 5. Iterator with Edit Awareness

Iterators automatically adjust their position based on edits:

```rust
pub struct ByteIterator {
    // Shared reference to buffer
    buffer: Arc<VirtualBuffer>,

    // Current position
    position: usize,

    // Track what version this iterator was created at
    version_at_creation: u64,
}

impl ByteIterator {
    pub fn next(&mut self) -> Option<u8> {
        // 1. Adjust position based on edits since creation
        self.adjust_for_edits();

        // 2. Ensure region is cached
        self.buffer.ensure_cached(self.position, 1)?;

        // 3. Read from cache
        let byte = self.buffer.read_cached(self.position)?;
        self.position += 1;
        Some(byte)
    }

    pub fn prev(&mut self) -> Option<u8> {
        if self.position == 0 {
            return None;
        }

        // Adjust for edits
        self.adjust_for_edits();

        self.position -= 1;
        self.buffer.ensure_cached(self.position, 1)?;
        let byte = self.buffer.read_cached(self.position)?;
        Some(byte)
    }

    pub fn seek(&mut self, position: usize) {
        self.position = position;
        self.adjust_for_edits();
    }

    fn adjust_for_edits(&mut self) {
        // Apply all edits since version_at_creation to adjust position
        for edit in self.buffer.edits_since(self.version_at_creation) {
            match edit.kind {
                EditKind::Insert { offset, len } if offset <= self.position => {
                    self.position += len;
                }
                EditKind::Delete { offset, len } if offset <= self.position => {
                    self.position = self.position.saturating_sub(len);
                }
                _ => {}
            }
        }
        self.version_at_creation = self.buffer.edit_version;
    }
}
```

## Key Features

### Infinite Storage Support

The persistence layer can return `usize::MAX` for `len()` to represent a conceptually infinite buffer. The cache only loads regions that are actually accessed.

### Efficient Caching

- **On-demand loading:** Regions are only loaded when accessed by read operations or iterators
- **Eviction policy:** When cache exceeds `max_cache_size`, least recently/frequently used regions are evicted
- **Dirty tracking:** Modified regions are marked dirty and can be flushed to persistence asynchronously

### Efficient Iteration

- **O(1) within cached regions:** Once a region is cached, iteration through it is constant time
- **O(log n) cache lookup:** Finding which cached region contains a position uses BTreeMap lookup
- **Bidirectional:** Can iterate forward with `next()` or backward with `prev()`
- **Seekable:** Can jump to any position with `seek()`

### Edit Tracking

Iterators maintain their logical position even as the buffer is modified:

- **Insert before iterator:** Position shifts forward by insertion length
- **Delete before iterator:** Position shifts backward by deletion length
- **Edit after iterator:** No adjustment needed
- **Version tracking:** Each iterator tracks which version it was created at and adjusts incrementally

### Pluggable Persistence

Different backends can optimize for different use cases:

- **Simple file:** Direct read/write to a single file (rewrites on insert)
- **Delta log:** Append-only log of changes, compact periodically
- **Sparse storage:** Keep original immutable, store insertions separately
- **Network:** Fetch chunks from remote server, cache locally
- **Database:** Store chunks in key-value store
- **Memory-mapped:** Use mmap for very large local files

## Design Questions

### 1. Iterator Lifetime

With `Arc<VirtualBuffer>`, iterators can outlive borrows and be stored in structs. This solves the self-referential struct problem but introduces shared ownership.

**Question:** Is `Arc` acceptable, or should we explore other ownership models? What are the implications for thread safety?

### 2. Edit Log Management

The edit log grows with every modification. Old edits are only needed by iterators created before those edits.

**Question:** Should we:
- Compact/garbage collect old edits when no iterators reference them?
- Use weak references to track living iterators?
- Cap the edit log size and invalidate old iterators?
- Keep edit log indefinitely for undo/redo?

### 3. Cache Granularity

Caching strategy affects performance:

**Question:** Should we:
- Use fixed-size blocks (e.g., 4KB pages like OS virtual memory)?
- Use variable-sized regions based on access patterns?
- Align cache regions with persistence layer chunks?
- Cache entire lines for line-oriented operations?

### 4. External Modifications

The persistence layer could be modified externally (e.g., file changed on disk).

**Question:** How do we handle this?
- Detect changes and invalidate cache?
- Reload automatically with dirty region warnings?
- Lock the backing store exclusively?
- Support collaborative editing with conflict resolution?

### 5. Line-Level Operations

Most text editors work with lines, not just bytes.

**Question:** Should we:
- Build `LineIterator` on top of `ByteIterator` (current approach)?
- Integrate line tracking into the cache (cache line boundaries)?
- Have a separate line cache that maps line numbers to byte offsets?
- Use the edit log to track line number changes?

### 6. Persistence Strategy for Edits

Different operations have different costs depending on the persistence layer.

**Question:** Should we:
- Let the persistence layer decide entirely (current design)?
- Provide hints about operation patterns (e.g., "batch mode")?
- Buffer edits and flush periodically vs immediately?
- Support transactions/snapshots?

### 7. Memory Pressure

The cache could grow large with heavy editing.

**Question:** How do we handle memory pressure?
- Strict max cache size with immediate eviction?
- Soft limit with background eviction?
- Integration with OS memory pressure signals?
- Let users configure cache size?

### 8. Iterator Invalidation

Some edits might make iterator adjustment impossible or ambiguous.

**Question:** Should we:
- Always adjust iterator position (current design)?
- Mark iterators as invalid after certain edits?
- Return `Result` from iterator methods to indicate invalidation?
- Clone a snapshot of the buffer for each iterator?

## Migration Path

To migrate from the current `ChunkTree` implementation to this design:

1. **Phase 1:** Implement `PersistenceLayer` trait with a `ChunkTreePersistence` adapter that wraps the existing `ChunkTree`
2. **Phase 2:** Implement `Cache` and `VirtualBuffer` layers on top
3. **Phase 3:** Update `ByteIterator` to use edit tracking instead of direct tree references
4. **Phase 4:** Update `Buffer` to use `VirtualBuffer` internally
5. **Phase 5:** Replace `ChunkTree` entirely, or keep it as one persistence option

This allows incremental migration while keeping the editor functional.

## Performance Considerations

### Current Implementation (ChunkTree)

- Tree seek: O(log n)
- Iteration within chunk: O(1)
- Iteration across chunks: O(log n) per chunk boundary
- Memory: O(n) where n is file size

### New Implementation (VirtualBuffer)

- Cache lookup: O(log n) where n is number of cached regions
- Iteration within cache: O(1)
- Iteration across cache boundaries: O(log n) per boundary
- Memory: O(cache_size) where cache_size << file_size
- Edit adjustment: O(e) where e is number of edits since iterator creation

### Trade-offs

- **Memory:** New design uses much less memory for large files
- **Complexity:** More layers and indirection
- **Latency:** Potential cache misses require persistence reads
- **Throughput:** Can be higher for large files since less memory pressure

## Conclusion

This design provides a flexible, scalable foundation for text editing that can handle very large files efficiently while supporting modern editing patterns like multiple cursors and live iteration. The pluggable persistence layer enables experimentation with different storage strategies without changing the core editing logic.
