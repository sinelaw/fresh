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

```rust
pub trait PersistenceLayer {
    // ... (methods: read, write, insert, delete, len)
}
```

### **2. Cache Layer**

Unchanged from the original design. The Cache struct manages loaded regions. It will be wrapped in a Mutex by the InnerBuffer.

```rust
pub struct Cache {
    // Map of loaded regions: (start_offset, data)
    regions: BTreeMap<usize, Vec<u8>>,
    // ... (dirty tracking, eviction policy)
}
```

### **3. Virtual Buffer (Updated)**

The VirtualBuffer is now a lightweight, cloneable handle that points to the shared inner state. This allows iterators and the buffer handle to coexist safely.

```rust

use std::sync::{Arc, Mutex, RwLock};  
use std::sync::atomic::{AtomicU64, Ordering};  
use std::collections::{BTreeSet, BTreeMap};

// The shared, internally-mutable state  
pub struct InnerBuffer {  
    // Pluggable persistence backend, mutex-protected for writes  
    persistence: Mutex\<Box\<dyn PersistenceLayer\>\>,

    // Cache for loaded regions, mutex-protected for reads/writes  
    cache: Mutex\<Cache\>,

    // Log of edits, read-write locked  
    // (Many iterators read, one edit operation writes)  
    edit\_log: RwLock\<Vec\<Edit\>\>,

    // Version counter, atomic for lock-free increments  
    edit\_version: AtomicU64,

    // NEW: Tracks all active iterators' versions for GC  
    // BTreeSet makes finding the minimum (oldest) version fast  
    active\_iterator\_versions: Mutex\<BTreeSet\<u64\>\>,  
}

// The public-facing handle  
\#\[derive(Clone)\]  
pub struct VirtualBuffer {  
    inner: Arc\<InnerBuffer\>,  
}

impl VirtualBuffer {  
    pub fn new(persistence: Box\<dyn PersistenceLayer\>) \-\> Self {  
        Self {  
            inner: Arc::new(InnerBuffer {  
                persistence: Mutex::new(persistence),  
                cache: Mutex::new(Cache::new()), // Assuming a Cache::new()  
                edit\_log: RwLock::new(Vec::new()),  
                edit\_version: AtomicU64::new(0),  
                active\_iterator\_versions: Mutex::new(BTreeSet::new()),  
            }),  
        }  
    }

    /// Read bytes (takes \&self, locks internally)  
    pub fn read(\&self, offset: usize, len: usize) \-\> io::Result\<Vec\<u8\>\> {  
        // 1\. Lock cache  
        // 2\. If not present, lock persistence and read  
        // 3\. Update cache  
        // ... (implementation)  
        unimplemented\!()  
    }

    /// Insert bytes (takes \&self, locks internally)  
    pub fn insert(\&self, offset: usize, data: &\[u8\]) \-\> io::Result\<()\> {  
        // 1\. Lock persistence and cache  
        self.inner.persistence.lock().unwrap().insert(offset, data)?;  
        self.inner.cache.lock().unwrap().write(offset, data); //

        // 2\. Get new version and create edit  
        let new\_version \= self.inner.edit\_version.fetch\_add(1, Ordering::SeqCst) \+ 1;  
        let edit \= Edit {  
            version: new\_version,  
            kind: EditKind::Insert { offset, len: data.len() }, //  
        };

        // 3\. Lock edit log and push  
        self.inner.edit\_log.write().unwrap().push(edit);

        // 4\. Prune old edits  
        self.prune\_edit\_log();  
        Ok(())  
    }

    /// Delete bytes (takes \&self, locks internally)  
    pub fn delete(\&self, range: Range\<usize\>) \-\> io::Result\<()\> {  
        // ... (Similar to insert: update persistence/cache, add edit, prune log)  
        unimplemented\!()  
    }

    /// Create an iterator (registers its version)  
    pub fn iter\_at(\&self, position: usize) \-\> ByteIterator {  
        let current\_version \= self.inner.edit\_version.load(Ordering::Relaxed);

        // Register this new iterator's version  
        self.inner  
            .active\_iterator\_versions  
            .lock()  
            .unwrap()  
            .insert(current\_version);

        ByteIterator {  
            buffer: self.inner.clone(),  
            position: position,  
            version\_at\_creation: current\_version, //  
        }  
    }

    // ... (other methods) ...

    /// NEW: Edit Log Garbage Collection  
    fn prune\_edit\_log(\&self) {  
        let versions \= self.inner.active\_iterator\_versions.lock().unwrap();  
          
        // Find the oldest iterator version still in use  
        let low\_water\_mark \= versions.iter().next().cloned();

        if let Some(oldest\_version) \= low\_water\_mark {  
            let mut edit\_log \= self.inner.edit\_log.write().unwrap();  
              
            // Find index of first edit to \*keep\*  
            let first\_index\_to\_keep \= edit\_log  
                .binary\_search\_by\_key(\&oldest\_version, |e| e.version)  
                .unwrap\_or\_else(|e| e);

            // Drain all edits \*before\* that version  
            edit\_log.drain(..first\_index\_to\_keep);  
        }  
        // If no iterators exist, we could drain all, but it's  
        // safer to keep them for a bit (e.g., for undo)  
    }  
}

```

### **4. Edit Tracking**

Unchanged from the original design.

```rust

\#\[derive(Clone, Debug)\]  
pub struct Edit {  
    version: u64,  
    kind: EditKind,  
} //

\#\[derive(Clone, Debug)\]  
pub enum EditKind {  
    Insert { offset: usize, len: usize },  
    Delete { offset: usize, len: usize },  
} //
```

### **5. Iterator with Edit Awareness (Updated)**

The ByteIterator now holds an Arc\<InnerBuffer> and automatically registers/unregisters its version for garbage collection.

```rust

pub struct ByteIterator {  
    // Shared reference to \*inner\* buffer  
    buffer: Arc\<InnerBuffer\>,

    // Current position  
    position: usize,

    // Track what version this iterator has "caught up" to  
    version\_at\_creation: u64, //  
}

impl ByteIterator {  
    pub fn next(\&mut self) \-\> Option\<u8\> {  
        self.adjust\_for\_edits(); //

        // Ensure region is cached (locks cache internally)  
        self.buffer.cache.lock().unwrap().ensure\_cached(self.position, 1).ok()?; //

        // Read from cache  
        let byte \= self.buffer.cache.lock().unwrap().read(self.position, 1)?\[0\];  
        self.position \+= 1;  
        Some(byte)  
    }

    pub fn prev(\&mut self) \-\> Option\<u8\> {  
        // ... (Similar logic)  
        unimplemented\!()  
    }

    fn adjust\_for\_edits(\&mut self) {  
        let current\_version \= self.buffer.edit\_version.load(Ordering::Relaxed);  
        if self.version\_at\_creation \== current\_version {  
            return; // Already up-to-date  
        }

        // Get read lock on edit log  
        let edit\_log \= self.buffer.edit\_log.read().unwrap();  
          
        // Find first edit \*after\* our version  
        let first\_edit\_index \= edit\_log  
            .binary\_search\_by\_key(\&self.version\_at\_creation, |e| e.version)  
            .map(|i| i \+ 1\) // We want edits \*after\* our version  
            .unwrap\_or\_else(|e| e); // \`e\` is insertion point for our version

        // Apply all edits since version\_at\_creation to adjust position  
        for edit in \&edit\_log\[first\_edit\_index..\] {  
            match edit.kind {  
                EditKind::Insert { offset, len } if offset \<= self.position \=\> {  
                    self.position \+= len;  
                } //  
                EditKind::Delete { offset, len } if offset \<= self.position \=\> {  
                    self.position \= self.position.saturating\_sub(len);  
                } //  
                \_ \=\> {} //  
            }  
        }  
          
        // \*\*\* NEW: Update version tracking for GC \*\*\*  
        let mut versions \= self.buffer.active\_iterator\_versions.lock().unwrap();  
        versions.remove(\&self.version\_at\_creation);  
        self.version\_at\_creation \= current\_version; // Bump our version  
        versions.insert(self.version\_at\_creation);  
    }  
}

/// NEW: Implement Drop to unregister the iterator  
impl Drop for ByteIterator {  
    fn drop(\&mut self) {  
        // Remove this iterator's version from the active set  
        self.buffer  
            .active\_iterator\_versions  
            .lock()  
            .unwrap()  
            .remove(\&self.version\_at\_creation);  
    }  
}
```

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
