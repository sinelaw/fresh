# Slow Backend Testing Strategy

## Overview

This document describes the strategy for testing the Fresh editor's performance and UX with slow filesystem and persistence backends. The goal is to ensure the editor remains responsive and provides good user experience even when underlying I/O operations are slow (e.g., network filesystems, slow disks, remote storage).

## Architecture Analysis

### Current Architecture

The Fresh editor has excellent abstraction layers that make it ideal for performance testing:

1. **Filesystem Abstraction** (`src/fs/backend.rs`)
   - `FsBackend` trait defines all filesystem operations
   - `LocalFsBackend` provides the production implementation
   - `FsManager` wraps the backend with request deduplication and batching
   - Operations are async, suitable for slow I/O

2. **Persistence Abstraction** (`src/persistence.rs`)
   - `PersistenceLayer` trait defines text storage operations
   - `ChunkTreePersistence` provides the production implementation
   - Operations: `read`, `insert`, `delete`, `len`

3. **Editor Construction** (`src/editor.rs:346`)
   - Currently hardcodes `LocalFsBackend::new()`
   - Needs modification to accept custom backends for testing

### Key Integration Points

```
User Action
    ↓
Editor::handle_key() / handle_mouse()
    ↓
Editor State Modification
    ↓
    ├→ FsManager::list_dir() → FsBackend::read_dir()
    ├→ FsManager::get_metadata() → FsBackend::get_metadata_batch()
    └→ Buffer operations → PersistenceLayer::read/insert/delete()
```

## Testing Strategy

### 1. Slow Backend Wrappers

Create decorator/wrapper implementations that add configurable delays:

#### SlowFsBackend
- Wraps any `FsBackend` implementation
- Adds configurable delays to each operation
- Tracks metrics (call counts, total time)
- Supports operation-specific delays (e.g., read_dir slow, metadata fast)

#### SlowPersistenceLayer
- Wraps any `PersistenceLayer` implementation
- Adds configurable delays to read/write operations
- Simulates various slow scenarios (constant delay, random delay, increasing delay)

### 2. Test Scenarios

#### Filesystem Scenarios
1. **Slow Directory Listing** - Simulate network filesystem with slow readdir
2. **Slow Metadata Fetching** - Test when stat() operations are slow
3. **Mixed Performance** - Some directories fast, others slow
4. **Increasing Latency** - Performance degrades over time
5. **Timeout Scenarios** - Operations that take extremely long

#### Persistence Scenarios
1. **Slow Reads** - Simulate slow disk reads
2. **Slow Writes** - Test when saving is slow
3. **Batch Write Delays** - Multiple inserts with cumulative delay
4. **Large File Delays** - Delays proportional to data size

### 3. UX Quality Metrics

Tests should verify:

1. **Responsiveness**
   - UI remains responsive during slow operations
   - Cursor movement not blocked by I/O
   - Typing latency stays low

2. **No Unnecessary Operations**
   - Directory not re-scanned on every render
   - Metadata fetched only when needed
   - Request deduplication working correctly

3. **Progress Indication**
   - User aware of ongoing operations
   - Loading states visible
   - No silent hangs

4. **Timeout Handling**
   - Operations don't block indefinitely
   - Graceful degradation
   - Error messages helpful

### 4. Performance Measurement

Create utilities to measure:
- Time to first render after action
- Number of backend calls
- Total time spent in I/O
- UI frame rate during operations

## Implementation Plan

### Phase 1: Infrastructure (Core Testing Support)

1. **SlowFsBackend Implementation** (`src/fs/slow.rs`)
   ```rust
   pub struct SlowFsBackend {
       inner: Arc<dyn FsBackend>,
       config: SlowFsConfig,
       metrics: Arc<Mutex<BackendMetrics>>,
   }

   pub struct SlowFsConfig {
       read_dir_delay: Duration,
       metadata_delay: Duration,
       exists_delay: Duration,
       // ... per-operation delays
   }

   pub struct BackendMetrics {
       read_dir_calls: usize,
       metadata_calls: usize,
       total_delay: Duration,
       // ... tracking
   }
   ```

2. **SlowPersistenceLayer Implementation** (`src/persistence/slow.rs` or in `src/persistence.rs`)
   ```rust
   pub struct SlowPersistenceLayer<T: PersistenceLayer> {
       inner: T,
       config: SlowPersistenceConfig,
       metrics: BackendMetrics,
   }

   pub struct SlowPersistenceConfig {
       read_delay: Duration,
       insert_delay: Duration,
       delete_delay: Duration,
   }
   ```

3. **Editor Backend Injection** (`src/editor.rs`)
   - Add `with_custom_backend()` constructor
   - Keep existing constructors unchanged for production use
   - Example:
   ```rust
   pub fn with_custom_backend(
       config: Config,
       width: u16,
       height: u16,
       fs_backend: Arc<dyn FsBackend>,
       working_dir: Option<PathBuf>,
   ) -> io::Result<Self>
   ```

4. **Test Harness Enhancement** (`tests/common/harness.rs`)
   - Add method to create harness with custom backend
   ```rust
   pub fn with_slow_fs(
       width: u16,
       height: u16,
       fs_config: SlowFsConfig,
   ) -> io::Result<Self>
   ```

### Phase 2: Test Utilities

5. **Performance Measurement Tools** (`tests/common/performance.rs`)
   ```rust
   pub struct PerformanceMonitor {
       start: Instant,
       operation_times: Vec<Duration>,
   }

   impl PerformanceMonitor {
       pub fn measure_operation<F>(&mut self, f: F) where F: FnOnce()
       pub fn assert_responsive(&self, max_latency: Duration)
       pub fn assert_max_calls(&self, metrics: &BackendMetrics, max: usize)
   }
   ```

6. **Assertion Helpers**
   ```rust
   // Assert operation completed within timeout
   pub fn assert_completes_within<F>(timeout: Duration, f: F)

   // Assert no more than N filesystem calls
   pub fn assert_fs_call_count(metrics: &BackendMetrics, expected: usize)

   // Assert UI stays responsive
   pub fn assert_no_ui_freeze(harness: &EditorTestHarness)
   ```

### Phase 3: Example Tests

7. **File Explorer Tests** (`tests/e2e/slow_filesystem.rs`)
   ```rust
   #[test]
   fn test_slow_directory_listing_stays_responsive()
   #[test]
   fn test_no_redundant_dir_scans()
   #[test]
   fn test_file_explorer_incremental_loading()
   #[test]
   fn test_timeout_on_extremely_slow_fs()
   ```

8. **Buffer Operations Tests** (`tests/e2e/slow_persistence.rs`)
   ```rust
   #[test]
   fn test_typing_responsive_during_slow_save()
   #[test]
   fn test_large_file_slow_load()
   #[test]
   fn test_undo_redo_with_slow_storage()
   ```

9. **Integration Tests** (`tests/e2e/slow_integration.rs`)
   ```rust
   #[test]
   fn test_switch_buffers_with_slow_fs()
   #[test]
   fn test_search_with_slow_file_access()
   #[test]
   fn test_git_operations_slow_fs()
   ```

### Phase 4: Documentation

10. **Update Testing Documentation** (`docs/TESTING.md`)
    - Add section on slow backend testing
    - Document how to use SlowFsBackend
    - Example test patterns

11. **Performance Guidelines** (`docs/PERFORMANCE.md` - new)
    - Expected performance characteristics
    - When to use slow backend tests
    - Interpreting test results

## Example Test Code

### Test: File Explorer Doesn't Rescan on Every Render

```rust
#[test]
fn test_file_explorer_no_redundant_scans() {
    // Create slow filesystem with 500ms directory listing delay
    let slow_config = SlowFsConfig {
        read_dir_delay: Duration::from_millis(500),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Open file explorer
    harness.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL).unwrap();
    harness.wait_for_async(|h| h.screen_to_string().contains("Explorer"), 2000).unwrap();

    let metrics_before = harness.get_fs_metrics().clone();

    // Navigate within the same directory (should not rescan)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

    let metrics_after = harness.get_fs_metrics();

    // Assert: Navigation should not trigger additional directory scans
    assert_eq!(
        metrics_before.read_dir_calls,
        metrics_after.read_dir_calls,
        "File explorer should not rescan directory during navigation"
    );
}
```

### Test: Typing Remains Responsive During Slow Save

```rust
#[test]
fn test_typing_responsive_during_slow_save() {
    let slow_persistence = SlowPersistenceConfig {
        insert_delay: Duration::from_millis(100),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_slow_persistence(80, 24, slow_persistence).unwrap();

    let mut monitor = PerformanceMonitor::new();

    // Type a long string - each character insert is slow
    let text = "The quick brown fox jumps over the lazy dog";
    monitor.measure_operation(|| {
        harness.type_text(text).unwrap();
    });

    // Despite 100ms per character insert, the UI batching should keep it responsive
    // Total time should be reasonable (not 100ms * 44 characters = 4.4 seconds)
    assert!(monitor.total_time() < Duration::from_secs(1),
        "Text insertion should be batched, not per-character");
}
```

### Test: Request Deduplication Works

```rust
#[test]
fn test_fs_manager_deduplicates_requests() {
    let slow_config = SlowFsConfig {
        read_dir_delay: Duration::from_millis(1000),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Simulate scenario where multiple components request the same directory
    // (This might happen with file explorer + git status + search all running)

    let path = harness.project_dir().unwrap();

    // Make multiple concurrent requests for the same directory
    let (results, metrics) = harness.concurrent_list_dir(&path, 5);

    // All requests should succeed
    assert_eq!(results.len(), 5);
    assert!(results.iter().all(|r| r.is_ok()));

    // But only ONE filesystem operation should have been performed
    assert_eq!(metrics.read_dir_calls, 1,
        "FsManager should deduplicate concurrent requests for same directory");
}
```

## Success Criteria

The implementation is successful when:

1. ✅ Can simulate various slow I/O scenarios
2. ✅ Can measure and assert on performance characteristics
3. ✅ Tests verify no unnecessary I/O operations
4. ✅ Tests verify UI responsiveness is maintained
5. ✅ Slow backend overhead is minimal (when delay is 0, should match real backend)
6. ✅ Easy to write new slow backend tests
7. ✅ Clear documentation and examples

## Future Enhancements

- **Network Simulation**: Add packet loss, jitter, bandwidth limits
- **Failure Injection**: Simulate I/O errors, timeouts, partial failures
- **Stress Testing**: Combine slow I/O with large files, many files
- **Profiling Integration**: Connect slow backend metrics to profiling tools
- **Visual Indicators**: Generate reports showing I/O patterns during tests

## Related Documents

- `docs/ARCHITECTURE.md` - Overall architecture
- `docs/FILE_EXPLORER.md` - File explorer implementation
- `docs/TESTING.md` - Testing infrastructure
- `src/fs/backend.rs` - Filesystem backend trait
- `src/persistence.rs` - Persistence layer trait
