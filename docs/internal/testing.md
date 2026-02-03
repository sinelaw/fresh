# Testing Guide

This document covers testing practices for Fresh core development.

## Test Types

### E2E Tests (`tests/e2e/`)

End-to-end tests simulate real user interactions by sending keyboard/mouse events and examining rendered output. They use the `EditorTestHarness` which provides a virtual terminal environment.

```rust
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_basic_editing() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type text
    harness.type_text("Hello").unwrap();

    // Send key combinations
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Assert on rendered output
    harness.render().unwrap();
    harness.assert_screen_contains("Hello");

    // Assert on buffer content
    harness.assert_buffer_content("Hello\n");
}
```

Key harness methods:
- `type_text(text)` - Type characters
- `send_key(code, modifiers)` - Send key events
- `render()` - Render to virtual terminal
- `assert_screen_contains(text)` - Check rendered output
- `assert_buffer_content(text)` - Check buffer content
- `open_file(path)` - Open a file
- `with_temp_project(w, h)` - Create harness with temp project directory

### Shadow Model Validation

For tests focused on text editing operations, enable shadow validation to catch bugs in the piece tree implementation:

```rust
#[test]
fn test_editing_operations() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.enable_shadow_validation();  // Enable shadow tracking

    harness.type_text("Hello").unwrap();
    harness.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();

    // assert_buffer_content will also verify shadow model matches
    harness.assert_buffer_content("Hell");
}
```

The shadow model maintains a simple `String` that mirrors editing operations. When enabled, `assert_buffer_content` verifies both the actual buffer and shadow model match.

### Property-Based Tests (`tests/shadow_model_tests.rs`)

The shadow model tests use proptest to generate random sequences of editing operations and verify the `TextBuffer` (using PieceTree) always matches a simple `Vec<u8>` oracle:

```rust
proptest! {
    #[test]
    fn test_random_edits(ops in vec(edit_operation(), 1..100)) {
        let mut buffer = TextBuffer::new();
        let mut model = Vec::new();

        for op in ops {
            match op {
                Insert(offset, text) => {
                    buffer.insert_bytes(offset, &text);
                    model.splice(offset..offset, text.iter().copied());
                }
                Delete(offset, len) => {
                    buffer.delete_bytes(offset, len);
                    model.drain(offset..offset+len);
                }
            }
            assert_eq!(buffer.to_string(), String::from_utf8_lossy(&model));
        }
    }
}
```

## Test Guidelines

### 1. No Timeouts

Use semantic waiting instead of fixed timers:

```rust
// BAD - flaky
std::thread::sleep(Duration::from_millis(100));

// GOOD - wait for specific state
harness.wait_for_screen_contains("Expected text");
```

### 2. Test Isolation

Tests run in parallel. Use isolated resources:

```rust
#[test]
fn test_file_operations() {
    // Use temp directories
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let file_path = harness.project_dir().unwrap().join("test.txt");

    // Internal clipboard mode is enabled by default in harness
}
```

### 3. E2E Over Unit Tests

Prefer e2e tests that verify rendered output over unit tests that examine internal state. This catches integration issues and allows refactoring internals freely.

### 4. Reproduce Before Fixing

Always include a failing test case that reproduces the bug:

```rust
#[test]
fn test_issue_123_cursor_bug() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Steps that reproduce the bug
    harness.type_text("trigger").unwrap();
    // This assertion should fail without the fix
    harness.assert_cursor_position(0, 7);
}
```

## Running Tests

```bash
# Run all tests
cargo test --package fresh-editor

# Run specific e2e test
cargo test --package fresh-editor test_basic_editing

# Run with output
cargo test --package fresh-editor -- --nocapture

# Run property tests with more cases
PROPTEST_CASES=1000 cargo test shadow_model
```

## Test Organization

```
tests/
├── common/
│   ├── harness.rs       # EditorTestHarness
│   └── fixtures.rs      # Test file helpers
├── e2e/                 # End-to-end tests
│   ├── basic.rs
│   ├── encoding.rs
│   └── ...
├── shadow_model_tests.rs    # Property-based buffer tests
├── property_tests.rs        # Other property tests
└── integration_tests.rs     # Integration tests
```
