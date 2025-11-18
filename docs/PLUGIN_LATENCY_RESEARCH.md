# Plugin Latency Research

## Overview

This document presents research on the plugin-to-editor communication latency in Fresh, including empirical measurement tools, identified bottlenecks, and improvement recommendations.

## Architecture Summary

Fresh uses a **message-passing architecture** between the main UI thread and a dedicated plugin thread:

```
Plugin Thread (Tokio async)          Main UI Thread (Sync event loop)
├─ TypeScript Runtime (Deno Core)    ├─ Event processing
├─ Plugin command sender             ├─ Command receiver polling
└─ State snapshot reader             └─ State snapshot writer
```

### Communication Flow

1. **Plugin → Editor (Commands)**: Fire-and-forget via mpsc channel
2. **Editor → Plugin (State)**: Shared RwLock-protected snapshot
3. **Editor → Plugin (Responses)**: Oneshot channels for async operations

## Empirical Measurement Tools

### TypeScript Benchmark Plugin

Location: `plugins/latency_benchmark.ts`

Usage:
```
:benchmark_run    # Run latency measurements
:benchmark_show   # Display results in split buffer
```

The benchmark measures:
- **State queries**: Operations reading from the EditorStateSnapshot
- **Command sends**: Fire-and-forget operations to editor
- **Async operations**: Full round-trip operations (spawn, file I/O, buffer creation)

### Rust-Side Metrics Collection

The plugin metrics system (enabled by the benchmark) measures:
- Command processing time by type
- Hook execution time by hook name
- State snapshot update time

## Latency Components

### 1. Plugin Thread Event Loop (1ms polling)

**Location**: `plugin_thread.rs:441`

```rust
tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
```

**Impact**: Max 1ms delay when plugin thread has no work
**Status**: Acceptable for interactive use

### 2. Main Thread Polling Timeout (50ms when idle)

**Location**: `main.rs:233`

```rust
Duration::from_millis(50)  // When idle, poll plugins every 50ms
```

**Impact**: Up to 50ms delay for plugin command processing when user is idle
**Status**: Previously was 1000ms - fixed to 50ms in recent commit

### 3. Command Queue Transfer

**Mechanism**: Unbounded mpsc channel
**Expected Latency**: < 1μs
**Status**: Optimal - no blocking

### 4. State Snapshot Updates

**Location**: `editor.rs:3029-3088`

```rust
fn update_plugin_state_snapshot(&mut self) {
    // Clears and rebuilds entire snapshot
    snapshot.buffers.clear();
    for (buffer_id, state) in &self.buffers {
        snapshot.buffers.insert(...);
    }
}
```

**Complexity**: O(n) where n = number of buffers
**Expected Latency**: < 100μs for typical use (< 50 buffers)
**Status**: May become bottleneck with many buffers

### 5. Hook Serialization

**Location**: `plugin_thread.rs:643-781`

Each hook invocation serializes arguments to JSON:
```rust
serde_json::to_string(&json_value)
```

**Expected Latency**: 1-10μs per hook
**Status**: Acceptable but could be optimized

### 6. RwLock Contention

The state snapshot uses `Arc<RwLock<EditorStateSnapshot>>`:
- Main thread: Writes (exclusive lock)
- Plugin thread: Reads (shared lock)

**Expected Latency**: < 1μs with low contention
**Status**: No contention issues observed

## Expected Latency by Operation Type

| Operation Type | Expected Latency | Notes |
|----------------|------------------|-------|
| State queries (getActiveBufferId, etc.) | 0.01 - 0.1 ms | Direct snapshot read |
| Buffer text reads (< 1KB) | 0.1 - 1 ms | Snapshot access |
| Command sends (setStatus, insertText) | 0.01 - 0.1 ms | Channel send only |
| Overlay operations | 0.05 - 0.5 ms | Channel send |
| File system ops (sync) | 0.1 - 1 ms | OS calls |
| Process spawn | 5 - 50 ms | OS dependent |
| File read async | 1 - 10 ms | I/O dependent |
| Virtual buffer creation | 10 - 100 ms | Full round-trip |

## Identified Bottlenecks

### 1. 50ms Idle Poll Timeout

**Problem**: When user is idle, plugin commands may wait up to 50ms to be processed.

**Impact**: Noticeable delay for background plugin operations

**Solution Options**:
- Reduce to 10-20ms (minor CPU impact)
- Use adaptive timeout based on plugin activity
- Add a "wake" mechanism when commands are queued

### 2. Full Snapshot Rebuild on Each Update

**Problem**: `update_plugin_state_snapshot()` clears and rebuilds entire buffer map.

**Impact**: O(n) with buffer count, could reach 1-5ms with 100+ buffers

**Solution Options**:
- Incremental updates (only changed fields)
- Dirty tracking for buffers
- Copy-on-write for buffer info

### 3. Hook JSON Serialization Allocation

**Problem**: Each hook call allocates a new JSON string.

**Impact**: Adds ~1-10μs per hook invocation

**Solution Options**:
- Pre-serialize common hook arguments
- Use a binary protocol (MessagePack)
- Pool string allocations

### 4. Pending Responses Mutex

**Problem**: `Arc<Mutex<HashMap>>` for pending responses.

**Impact**: Currently negligible (single-threaded), but would block under high concurrency

**Solution Options**:
- Use lock-free DashMap
- Sharded locks
- Channel-based response routing

## Improvement Recommendations

### High Priority (User-Visible Impact)

#### 1. Reduce Idle Poll Timeout
```rust
// main.rs:233
Duration::from_millis(16)  // Match frame rate for responsiveness
```
**Benefit**: Reduces max latency from 50ms to 16ms
**Cost**: Minimal CPU overhead

#### 2. Adaptive Poll Timeout
```rust
let poll_timeout = if has_pending_plugin_commands {
    Duration::ZERO  // Process immediately
} else {
    Duration::from_millis(50)
};
```
**Benefit**: Zero latency when plugins are active
**Cost**: Need to track pending command state

### Medium Priority (Scalability)

#### 3. Incremental Snapshot Updates
```rust
fn update_plugin_state_snapshot(&mut self) {
    let mut snapshot = snapshot_handle.write().unwrap();

    // Only update active buffer info
    snapshot.active_buffer_id = self.active_buffer;
    snapshot.active_split_id = self.split_manager.active_split().0;

    // Update only changed buffers
    for (buffer_id, state) in &self.dirty_buffers {
        snapshot.buffers.insert(*buffer_id, ...);
    }
    self.dirty_buffers.clear();
}
```
**Benefit**: O(1) for most operations
**Cost**: Additional tracking complexity

#### 4. Cache Hook Arguments
```rust
struct CachedHookArgs {
    editor_initialized: String,  // "{}"
    // Pre-serialized common patterns
}
```
**Benefit**: Eliminate allocation for common hooks
**Cost**: Minor code complexity

### Low Priority (Future Optimization)

#### 5. Lock-Free Pending Responses
Replace mutex with DashMap or crossbeam queue for better concurrent access.

#### 6. Binary Hook Protocol
Use MessagePack instead of JSON for hook arguments to reduce serialization overhead.

#### 7. Command Batching
Batch multiple commands into single queue operations when possible.

## Monitoring and Profiling

### Using the Benchmark Plugin

1. Open Fresh with a buffer
2. Run `:benchmark_run`
3. Run `:benchmark_show` to view results

The results include:
- TypeScript-side latency for each operation type
- Rust-side metrics for command processing
- Percentile distributions (p50, p95, p99)

### Interpreting Results

**Good performance indicators**:
- State queries < 0.1ms avg
- Command sends < 0.1ms avg
- Snapshot updates < 100μs

**Warning signs**:
- State queries > 1ms (RwLock contention)
- Snapshot updates > 1ms (too many buffers)
- p99 >> p50 (sporadic delays)

## Conclusion

The current plugin system has acceptable latency for interactive use, with recent improvements (50ms idle timeout) addressing the major previous issue. The main opportunities for improvement are:

1. **Immediate**: Reduce idle poll timeout to 16ms
2. **Short-term**: Implement adaptive polling based on activity
3. **Medium-term**: Incremental snapshot updates for large buffer counts

The provided benchmark tools allow empirical measurement to validate any optimizations.

## Files Modified

- `src/plugin_metrics.rs` - New metrics collection module
- `src/lib.rs` - Added plugin_metrics module
- `src/editor.rs` - Added timing instrumentation for command processing
- `src/plugin_thread.rs` - Added timing instrumentation for hook execution
- `src/ts_runtime.rs` - Added metrics ops (metricsStart, metricsStop, metricsIsActive)
- `plugins/latency_benchmark.ts` - New benchmark plugin for measurement
