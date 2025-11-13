# Document Model Migration Guide

This guide shows how to migrate existing code to use the DocumentModel API and helper methods.

## Overview

The DocumentModel provides a clean abstraction layer between editor operations and the underlying text buffer. It supports:
- **Dual coordinate systems**: Line/column for small files, byte offsets for large files
- **Explicit error handling**: `Result<T>` instead of silent failures
- **Lazy loading**: Transparent support for huge files
- **Type safety**: Clear APIs that make intent explicit

## When to Use DocumentModel

### ✅ Use DocumentModel when:
1. You need explicit error handling for text operations
2. You're implementing new features that should work with huge files
3. You want type-safe position handling
4. You're writing code that might be used with remote/virtual documents in the future

### ⏸️ Keep existing code when:
1. The code is working well and not causing issues
2. The operation is performance-critical and uses optimized buffer methods
3. The code is in a hot path and the abstraction overhead matters

## Helper Methods

The `EditorState` provides convenient helper methods that use DocumentModel internally:

### 1. `get_text_range_safe(start, end) -> Result<String>`

Use instead of `buffer.slice()` when you need explicit error handling.

**Before:**
```rust
let text = state.buffer.slice(0..100);
// Returns empty string on error - can't distinguish failure from empty range
```

**After:**
```rust
let text = state.get_text_range_safe(0, 100)?;
// Explicit error handling - caller knows if operation failed
```

### 2. `get_line_at_offset(offset) -> Option<(usize, String)>`

Get the line containing a byte offset, with its start position.

**Before:**
```rust
// Complex: need to find line start, then get content
let mut iter = state.buffer.line_iterator(cursor_pos);
if let Some((line_start, content)) = iter.next() {
    // ...
}
```

**After:**
```rust
if let Some((line_start, content)) = state.get_line_at_offset(cursor_pos) {
    // Simple and clear
}
```

### 3. `get_text_to_end_of_line(cursor_pos) -> Result<String>`

Common pattern for editing operations.

**Before:**
```rust
// Manual calculation of line boundaries
let mut iter = state.buffer.line_iterator(cursor_pos);
if let Some((line_start, line_content)) = iter.next() {
    let offset_in_line = cursor_pos - line_start;
    let text = &line_content[offset_in_line..];
}
```

**After:**
```rust
let text = state.get_text_to_end_of_line(cursor_pos)?;
// Clean and explicit
```

## DocumentModel Trait Methods

For more complex operations, use the DocumentModel trait directly:

### Capabilities

```rust
use fresh::document_model::DocumentModel;

let caps = state.capabilities();
if caps.has_line_index {
    // Can use line/column positions
    let pos = DocumentPosition::line_col(10, 5);
} else {
    // Use byte offsets for large files
    let pos = DocumentPosition::byte(10000);
}
```

### Viewport Content

```rust
use fresh::document_model::{DocumentModel, DocumentPosition};

// Get viewport for rendering
let viewport = state.get_viewport_content(
    DocumentPosition::byte(start_offset),
    line_count,
)?;

for line in viewport.lines {
    println!("Line at {}: {}", line.byte_offset, line.content);
}
```

### Editing Operations

```rust
use fresh::document_model::{DocumentModel, DocumentPosition};

// Insert text
let bytes_inserted = state.insert(
    DocumentPosition::byte(100),
    "hello",
)?;

// Delete range
state.delete(
    DocumentPosition::byte(100),
    DocumentPosition::byte(105),
)?;

// Replace range
state.replace(
    DocumentPosition::byte(100),
    DocumentPosition::byte(105),
    "world",
)?;
```

### Search Operations

```rust
use fresh::document_model::{DocumentModel, DocumentPosition};

// Find all matches
let matches = state.find_matches("pattern", None)?;

// Find in range
let range_matches = state.find_matches(
    "pattern",
    Some((
        DocumentPosition::byte(0),
        DocumentPosition::byte(1000),
    )),
)?;
```

## Migration Strategy

### Incremental Approach

1. **Start with new code**: Use DocumentModel for all new features
2. **Migrate on touch**: When modifying existing code, consider migration if beneficial
3. **Focus on value**: Migrate code that clearly benefits from the abstraction
4. **Keep what works**: Don't migrate code that's working well unless there's a reason

### Example Migration: Error Message Generation

**Before:**
```rust
fn get_error_context(state: &EditorState, pos: usize) -> String {
    let start = pos.saturating_sub(20);
    let end = (pos + 20).min(state.buffer.len());
    let context = state.buffer.slice(start..end);
    format!("Error near: {}", context)
}
```

**After:**
```rust
fn get_error_context(state: &EditorState, pos: usize) -> Result<String> {
    let start = pos.saturating_sub(20);
    let end = (pos + 20).min(state.buffer.len());
    let context = state.get_text_range_safe(start, end)?;
    Ok(format!("Error near: {}", context))
}
```

**Benefits:**
- Explicit error handling
- Works with lazy loading
- Clear failure vs. empty distinction

### Example Migration: Line Information

**Before:**
```rust
fn get_current_line_info(state: &EditorState) -> (usize, String) {
    let pos = state.cursors.primary().position;
    let mut iter = state.buffer.line_iterator(pos);
    if let Some((start, content)) = iter.next() {
        (start, content)
    } else {
        (0, String::new())
    }
}
```

**After:**
```rust
fn get_current_line_info(state: &EditorState) -> Option<(usize, String)> {
    let pos = state.cursors.primary().position;
    state.get_line_at_offset(pos)
}
```

**Benefits:**
- More concise
- Uses DocumentModel abstraction
- Returns `Option` to indicate failure clearly

## Testing with DocumentModel

The e2e tests in `tests/e2e/document_model.rs` demonstrate comprehensive usage:

- `test_document_model_small_file` - Line-based operations
- `test_document_model_large_file` - Byte-based operations
- `test_document_model_editing` - Insert/delete/replace
- `test_document_model_search` - Pattern matching

Use these as reference for implementing DocumentModel features.

## Common Patterns

### Pattern 1: Safe Range Access

```rust
// Instead of slice() which silently fails:
match state.get_text_range_safe(start, end) {
    Ok(text) => process(text),
    Err(e) => handle_error(e),
}
```

### Pattern 2: Line-Aware Operations

```rust
// Get line content with position:
if let Some((line_start, line_content)) = state.get_line_at_offset(cursor) {
    // Work with both position and content
    let col = cursor - line_start;
    // ...
}
```

### Pattern 3: Position Conversion

```rust
use fresh::document_model::{DocumentModel, DocumentPosition};

// Convert between position types:
let line_pos = DocumentPosition::line_col(10, 5);
let byte_offset = state.position_to_offset(line_pos)?;

let byte_pos = DocumentPosition::byte(1000);
let converted = state.offset_to_position(byte_offset);
```

## Performance Considerations

### When to Optimize

The DocumentModel helpers are optimized for correctness and clarity. For hot paths:

1. **Rendering**: Use `prepare_for_render()` + direct buffer access (already optimized)
2. **Bulk operations**: Consider batching DocumentModel operations
3. **Tight loops**: Profile first - premature optimization is the root of all evil

### Benchmarking

If migrating performance-critical code:

```rust
#[bench]
fn bench_old_approach(b: &mut Bencher) {
    let state = setup();
    b.iter(|| {
        // Old code
    });
}

#[bench]
fn bench_document_model(b: &mut Bencher) {
    let state = setup();
    b.iter(|| {
        // New DocumentModel code
    });
}
```

## Future Compatibility

Code using DocumentModel will automatically support:
- **Remote files**: Network-based document sources
- **Virtual documents**: Computed/generated content
- **Collaborative editing**: Position transformation for OT/CRDT
- **Advanced features**: Any feature built on the abstraction

## Summary

- ✅ **Use helper methods** for common patterns (`get_text_range_safe`, `get_line_at_offset`, etc.)
- ✅ **Use DocumentModel trait** for advanced operations (viewport, editing, search)
- ✅ **Migrate incrementally** - new code first, existing code when touched
- ✅ **Keep what works** - don't rewrite code that doesn't benefit
- ✅ **Test thoroughly** - use existing e2e tests as examples

The DocumentModel is production-ready and tested. Migration is optional but recommended for new code and code that benefits from explicit error handling or large file support.
