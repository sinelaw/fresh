# **Virtual Buffer Design**

## **Overview**

This document outlines the design for a virtual memory system for text editing with the following goals:

1. **Very large (infinite) underlying storage support** - Handle files larger than memory.
1. **Caching of accessed regions** - Only load what's needed into memory.
1. **Efficient byte-level iteration** - Seek to any position and iterate in either direction efficiently.
1. **Support modifications during iteration** - Iterators automatically adjust when edits occur.
1. **Pluggable persistence layer** - Backend can use deltas, separate insertion files, or any other strategy.

This updated design focuses on a **thread-safe implementation in 100% safe Rust** by using standard concurrency primitives to manage shared, mutable state.

## **Architecture**

The core architecture is split into a public-facing VirtualBuffer handle and an InnerBuffer struct that holds the shared state using interior mutability.

### **1. Persistence Layer (Pluggable Backend)**

Unchanged from the original design. This trait perfectly abstracts the storage.

A trait that defines the interface for the pluggable persistence layer, specifying methods for reading, writing, inserting, deleting, and getting the length of the underlying storage.

### **2. Cache Layer**

Unchanged from the original design. The Cache struct manages loaded regions. It will be wrapped in a Mutex by the InnerBuffer.

A struct that manages caching of loaded data regions. It uses a BTreeMap to store regions mapped by their starting offset and includes logic for tracking dirty regions and implementing an eviction policy.

### **3. Virtual Buffer (Updated)**

The VirtualBuffer is now a lightweight, cloneable handle that points to the shared inner state. This allows iterators and the buffer handle to coexist safely.

The core data structures for the virtual buffer.
-   `InnerBuffer`: A struct holding the shared, mutable state, including the persistence layer, cache, edit log, edit version, and active iterator versions. It uses concurrency primitives like `Mutex`, `RwLock`, and `AtomicU64` to ensure thread-safe interior mutability.
-   `VirtualBuffer`: A lightweight, cloneable handle (`Arc<InnerBuffer>`) that provides public access to the shared inner state.
-   The implementation of `VirtualBuffer` provides methods for creating a new buffer, reading, inserting, and deleting bytes. These methods handle the necessary locking of internal state. It also includes a method to create an iterator (`iter_at`) which registers itself for edit tracking, and a private method (`prune_edit_log`) for garbage collecting old entries from the edit log based on the versions of active iterators.

### **4. Edit Tracking**

Unchanged from the original design.

Data structures for tracking edits. `Edit` associates a version number with an `EditKind`, which can be either an `Insert` or a `Delete` operation, each storing the offset and length of the change.

### **5. Iterator with Edit Awareness (Updated)**

The ByteIterator now holds an Arc\<InnerBuffer> and automatically registers/unregisters its version for garbage collection.

**Performance Optimization:** To avoid excessive locking (4 locks per byte), the iterator uses a two-level caching strategy:

1. **ChunkTree Snapshot:** Gets a cheap clone of the underlying ChunkTree via `PersistenceLayer::get_chunk_tree_snapshot()`
2. **Internal 4KB Buffer:** Reads chunks from the snapshot, reducing iterator creation from O(n) to O(n/4096)
3. **Lazy Invalidation:** When `adjust_for_edits()` detects version changes, it invalidates both snapshot and buffer

The `ByteIterator` struct provides a way to iterate over the buffer's bytes. It holds a reference to the shared `InnerBuffer`, its current position, and the version at which it was created. For performance, it uses a two-level caching strategy: a snapshot of the underlying `ChunkTree` and a small internal chunk buffer. The `next` method advances the iterator, using the cache for speed. The `adjust_for_edits` method is called to update the iterator's state and invalidate its caches when the underlying buffer is modified.

**Performance:** ~4096x fewer locks and iterator creations compared to per-byte access.

______________________________________________________________________

## **Answers to Design Questions (Updated)**

### **1. Iterator Lifetime**

**Answer:** The Arc\<InnerBuffer> pattern is the standard, safe Rust solution. It fully supports thread-safety and allows iterators to outlive the original VirtualBuffer handle, as they just hold a shared reference to the inner state.

### **2. Edit Log Management**

**Answer:** This is now solved.

1. VirtualBuffer tracks all active iterator versions in a Mutex\<BTreeSet\<u64>>.
1. VirtualBuffer::iter_at() **registers** a new iterator's version.
1. ByteIterator::Drop **unregisters** its version.
1. ByteIterator::adjust_for_edits() (called on next(), prev(), etc.) **updates** its version in the set, effectively "bumping" it forward.
1. After an edit, VirtualBuffer::prune_edit_log() finds the minimum version in the set (the "low-water mark") and truncates the edit_log of all older edits.

### **3. Cache Granularity**

**Answer:** This remains a key tuning decision, independent of the concurrency model. The design still supports fixed blocks, variable regions, or line-based caching.

### **4. External Modifications**

**Answer:** This remains a high-level problem. The Mutex-protected persistence layer makes it possible to add a check_for_external_changes() method that could lock, check file mtime, and invalidate the cache, but this design doesn't solve the core file-locking or conflict-resolution problem.

### **5. Line-Level Operations**

**Answer:** The original recommendation stands: build a LineIterator or LineCache *on top* of this VirtualBuffer layer. Do not complicate the byte-level logic.

### **6. Persistence Strategy for Edits**

**Answer:** This design still gives the PersistenceLayer full control. The VirtualBuffer's insert/delete methods now provide a natural "flush point" that could be made asynchronous (e.g., pushing the edit to a work queue) if the PersistenceLayer implementation supports it.

### **7. Memory Pressure**

**Answer:** This is handled by the Cache's eviction policy. The interior-mutability pattern makes it easier to run eviction in a separate background thread (which would lock the Cache) without blocking the main editing thread.

### **8. Iterator Invalidation**

## **Answer:** The current design *always* adjusts the iterator's position. This is simple and predictable. Returning a Result would be more explicit but adds cognitive load for all iterator consumers. This remains a valid trade-off to consider.

## **Performance Considerations (Updated)**

### **New Implementation (VirtualBuffer)**

- **Memory:** O(cache_size) + O(edit_log_size) + O(active_iterators). Memory use is now bounded and well-managed.
- **Latency (Iteration):**
  - Cached: O(1).
  - Cache miss: O(persistence_read).
  - Edit adjustment: O(E * log E) or O(E) to find and apply edits (where E = edits since last use), plus O(log N) to update version set (where N = active iterators). This is a fast "catch-up" operation.
- **Latency (Edit):**
  - O(persistence_write) + O(log E) to prune edit log (where E = total edits).
  - **NEW:** Risk of **lock contention**. If one thread is performing a large insert, all iterators (even in other threads) will block when they try to call next() (as they'll wait for the cache/edit log locks). This is the correct and safe trade-off.

### **Conclusion**

This updated design is **thread-safe, implementable in 100% safe Rust, and robust against memory leaks** from the edit log. It pays a small, necessary cost in lock-based concurrency but gains correctness and a solid foundation for advanced features.
