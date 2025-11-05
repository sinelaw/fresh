# Testing Strategy

## Core Principles

1. **Sanity Tests First** - Test the most obvious, critical behaviors
2. **Property-Based Testing** - Test invariants and properties that should always hold
3. **No Mocks** - Test real implementations, not mocks
4. **End-to-End TUI Testing** - Test the complete editor as a black box

## Testing Levels

### Level 1: Unit Tests (Sanity + Properties)

Each module has its own test suite focusing on:

#### Sanity Tests
- Does the most basic operation work?
- Does it handle empty input?
- Does it handle the simplest non-trivial case?

#### Property Tests
- Invariants that must always hold
- Round-trip properties (serialize → deserialize)
- Commutative operations
- Idempotent operations

### Level 2: Integration Tests (Module Interactions)

Test how modules work together:
- Buffer + LineCache consistency
- Cursors + Buffer edit adjustments
- EditorState + EventLog (undo/redo properties)
- Viewport + Buffer scrolling

### Level 3: End-to-End TUI Tests

Test the complete editor through a virtual terminal:
- Simulate keyboard input
- Capture terminal output
- Verify rendered state
- No rendering to actual terminal

## Module-Specific Test Plans

### buffer.rs

#### Sanity Tests
- [ ] Empty buffer has 0 length
- [ ] Insert at 0 works
- [ ] Delete empty range is no-op
- [ ] Line 0 starts at byte 0
- [ ] Single-line file has 1 line

#### Property Tests
- [ ] **Insert-Delete Property**: Insert(pos, text) then Delete(pos..pos+len) restores original
- [ ] **Line Cache Consistency**: byte_to_line(line_to_byte(n)) == n
- [ ] **Content Length**: After insert(pos, text), len increases by text.len()
- [ ] **Edit Bounds**: insert(pos, text) where pos > len should fail/clamp
- [ ] **Line Count Monotonic**: Deleting text never increases line count
- [ ] **Round-trip**: save() then load() gives identical content

### cursor.rs

#### Sanity Tests
- [ ] New cursor at position 0
- [ ] Cursor with selection has anchor
- [ ] Empty Cursors has 1 primary cursor

#### Property Tests
- [ ] **Normalization Idempotent**: normalize() twice == normalize() once
- [ ] **Adjustment Consistency**: After insert at pos, all cursors after pos shift by len
- [ ] **Cursor Merge**: Overlapping cursors merge into one
- [ ] **Position Bounds**: All cursor positions <= buffer.len()
- [ ] **Primary Invariant**: primary() always exists and is valid

### event.rs

#### Sanity Tests
- [ ] Empty log has no events
- [ ] Append adds event
- [ ] Undo on empty log does nothing
- [ ] Redo without undo does nothing

#### Property Tests
- [ ] **Undo-Redo Inverse**: redo(undo(state)) == state
- [ ] **Inverse Correctness**: event then inverse(event) restores state
- [ ] **Log Truncation**: After undo then new event, can't redo past truncation
- [ ] **Serialization Round-trip**: save() then load() gives identical log
- [ ] **Event Ordering**: Timestamps are monotonically increasing

### state.rs

#### Sanity Tests
- [ ] New state has empty buffer
- [ ] Apply insert event updates buffer
- [ ] Apply move event updates cursor position

#### Property Tests
- [ ] **Event Application Determinism**: apply(events) twice gives same result
- [ ] **Cursor Visibility**: After any event, primary cursor is in viewport bounds
- [ ] **Buffer-Cursor Consistency**: All cursors are within buffer bounds
- [ ] **Selection Validity**: If cursor has selection, anchor is valid position
- [ ] **Undo-Redo State**: apply(events) then undo(len) restores original state

### viewport.rs

#### Sanity Tests
- [ ] New viewport starts at line 0
- [ ] Scroll down increases top_line
- [ ] ensure_visible doesn't scroll if cursor already visible

#### Property Tests
- [ ] **Scroll Bounds**: top_line + height <= total_lines (when possible)
- [ ] **Visibility**: After ensure_visible, cursor line is in [top_line, top_line+height)
- [ ] **Screen Position**: cursor_screen_position returns coords in [0, width) × [0, height)
- [ ] **Multiple Cursors**: ensure_cursors_visible makes all cursors visible (or as many as fit)

### config.rs

#### Sanity Tests
- [ ] Default config is valid
- [ ] Load missing file returns default
- [ ] Save then load gives identical config

#### Property Tests
- [ ] **Validation**: validate() never panics
- [ ] **Serialization Round-trip**: save then load preserves all fields
- [ ] **Keybinding Uniqueness**: No duplicate key combinations
- [ ] **Default Completeness**: Default config has all required keybindings

### keybindings.rs

#### Sanity Tests
- [ ] Empty resolver has no bindings
- [ ] Add binding then resolve returns action
- [ ] Unknown key returns None

#### Property Tests
- [ ] **Action Parsing**: action_from_str round-trips with action_to_str
- [ ] **Modifier Combinations**: All modifier combos are distinguishable
- [ ] **Resolution Determinism**: resolve(key) always returns same action

### chunk_tree.rs (Existing)

Already has 79 tests. Additional property tests:

#### Property Tests
- [ ] **Insert-Delete Property**: insert then delete restores original
- [ ] **Slice Consistency**: slice(0..len) == entire content
- [ ] **Concatenation**: concat(a, b).len() == a.len() + b.len()
- [ ] **Structural Sharing**: Persistent operations don't copy entire tree

## Integration Tests

### tests/integration_tests.rs

#### Buffer + Cursor Integration
- [ ] **Edit Adjustment**: Insert at pos adjusts all cursors after pos
- [ ] **Multi-cursor Insert**: Insert at multiple positions maintains cursor order
- [ ] **Selection Delete**: Delete selection updates cursor to start of deletion

#### State + EventLog Integration
- [ ] **Undo/Redo Full Cycle**: Complex editing session can be fully undone/redone
- [ ] **Event Replay**: Replaying event log reconstructs exact state
- [ ] **Snapshot Recovery**: Load snapshot + replay events gives correct state

#### Viewport + Buffer Integration
- [ ] **Scroll Tracking**: Viewport tracks cursor through large file
- [ ] **Resize Handling**: Resize maintains cursor visibility
- [ ] **Line Wrapping** (future): Virtual lines match physical lines

## End-to-End TUI Tests

### Strategy: Virtual Terminal Testing

Use `ratatui::backend::TestBackend` to create a virtual terminal that captures all rendering output without displaying to screen.

### Test Harness Design

```rust
/// Virtual editor environment for testing
pub struct EditorTestHarness {
    /// The editor instance
    editor: Editor,

    /// Virtual terminal backend
    backend: TestBackend,

    /// Simulated terminal size
    size: (u16, u16),

    /// Captured terminal output
    frames: Vec<Buffer>,
}

impl EditorTestHarness {
    /// Create new test harness with virtual terminal
    pub fn new(width: u16, height: u16) -> Self;

    /// Open a file in the editor
    pub fn open_file(&mut self, path: &Path) -> Result<()>;

    /// Create new empty buffer
    pub fn new_buffer(&mut self) -> Result<()>;

    /// Simulate keyboard input
    pub fn send_key(&mut self, key: KeyCode, modifiers: KeyModifiers);

    /// Simulate typing a string
    pub fn type_text(&mut self, text: &str);

    /// Force a render cycle
    pub fn render(&mut self) -> Result<()>;

    /// Get the current terminal buffer (what would be displayed)
    pub fn get_buffer(&self) -> &Buffer;

    /// Get text at specific screen position
    pub fn get_cell(&self, x: u16, y: u16) -> Option<Cell>;

    /// Get entire screen as string (for debugging)
    pub fn screen_to_string(&self) -> String;

    /// Verify text appears on screen
    pub fn assert_screen_contains(&self, text: &str);

    /// Verify cursor at screen position
    pub fn assert_cursor_at(&self, x: u16, y: u16);

    /// Get buffer content (not screen, actual buffer)
    pub fn get_buffer_content(&self) -> String;

    /// Verify buffer content matches expected
    pub fn assert_buffer_content(&self, expected: &str);
}
```

### End-to-End Test Cases

#### Basic Editing (tests/e2e/basic_editing.rs)
- [ ] **Open and View**: Open file, verify content rendered
- [ ] **Type Character**: Type 'a', verify it appears on screen and in buffer
- [ ] **Newline**: Press Enter, verify cursor moves to next line
- [ ] **Backspace**: Type then backspace, verify deletion
- [ ] **Arrow Navigation**: Move cursor with arrows, verify screen position
- [ ] **Save File**: Edit and save, verify file written to disk

#### Multi-Cursor (tests/e2e/multi_cursor.rs)
- [ ] **Add Cursor**: Ctrl+Click adds cursor, verify multiple cursors rendered
- [ ] **Type at Multiple**: Type with multiple cursors, verify all insertions
- [ ] **Delete at Multiple**: Backspace with multiple cursors
- [ ] **Navigate Multiple**: Arrow keys move all cursors

#### Undo/Redo (tests/e2e/undo_redo.rs)
- [ ] **Undo Single Edit**: Type then undo, verify restored
- [ ] **Redo**: Undo then redo, verify re-applied
- [ ] **Undo Chain**: Multiple edits, undo all, verify original state
- [ ] **Undo Branch**: Undo then edit creates new branch

#### File Operations (tests/e2e/file_ops.rs)
- [ ] **Save New File**: Create buffer, type, save-as with filename
- [ ] **Save Existing**: Open, edit, save (no prompt)
- [ ] **Multiple Buffers**: Open multiple files, switch between them
- [ ] **Close Buffer**: Close buffer, verify removed from tab bar

#### Clipboard (tests/e2e/clipboard.rs)
- [ ] **Copy**: Select text, copy, verify clipboard
- [ ] **Paste**: Copy then paste, verify duplication
- [ ] **Cut**: Select, cut, verify deletion and clipboard
- [ ] **Cross-Buffer Paste**: Copy in one buffer, paste in another

#### Viewport (tests/e2e/viewport.rs)
- [ ] **Scroll Down**: PageDown scrolls viewport
- [ ] **Scroll to End**: Ctrl+End goes to file end
- [ ] **Auto-Scroll**: Type at bottom of screen, viewport follows
- [ ] **Large File**: Open 10,000 line file, scroll through it

#### Error Handling (tests/e2e/errors.rs)
- [ ] **Missing File**: Try to open non-existent file, verify error message
- [ ] **Read-Only File**: Try to save read-only file, verify error
- [ ] **Invalid UTF-8**: Open binary file, handle gracefully

### Test File Structure

```
tests/
├── common/
│   ├── mod.rs              # Shared test utilities
│   ├── harness.rs          # EditorTestHarness implementation
│   └── fixtures.rs         # Test file fixtures
│
├── integration/
│   ├── buffer_cursor.rs    # Buffer + Cursor integration
│   ├── state_eventlog.rs   # State + EventLog integration
│   └── viewport_buffer.rs  # Viewport + Buffer integration
│
└── e2e/
    ├── basic_editing.rs    # Basic edit operations
    ├── multi_cursor.rs     # Multi-cursor scenarios
    ├── undo_redo.rs        # Undo/redo workflows
    ├── file_ops.rs         # File open/save/close
    ├── clipboard.rs        # Copy/paste operations
    └── viewport.rs         # Scrolling and navigation
```

## Property Test Implementation

Use `proptest` or `quickcheck` for property-based tests:

```toml
[dev-dependencies]
proptest = "1.0"
```

Example property test:

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn insert_delete_inverse(
        pos in 0usize..1000,
        text in "\\PC{0,100}"  // Any Unicode string, 0-100 chars
    ) {
        let mut buffer = Buffer::new();
        let original = buffer.to_string();

        // Insert then delete should restore original
        buffer.insert(pos.min(buffer.len()), &text);
        let len = text.len();
        buffer.delete(pos.min(buffer.len())..pos.min(buffer.len()).saturating_add(len));

        assert_eq!(buffer.to_string(), original);
    }
}
```

## Running Tests

### Unit Tests
```bash
cargo test --lib                    # All unit tests
cargo test --lib buffer::tests      # Specific module
cargo test --lib -- --nocapture     # With output
```

### Integration Tests
```bash
cargo test --test integration_tests
```

### End-to-End Tests
```bash
cargo test --test e2e               # All E2E tests
cargo test --test e2e basic_editing # Specific suite
```

### Property Tests
```bash
cargo test --lib -- --ignored       # Run property tests (can be slow)
PROPTEST_CASES=10000 cargo test     # More test cases
```

### All Tests
```bash
cargo test                          # Everything
cargo test --release               # Optimized (faster property tests)
```

## CI/CD Testing

### GitHub Actions Workflow

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Run unit tests
        run: cargo test --lib
      - name: Run integration tests
        run: cargo test --test integration_tests
      - name: Run E2E tests
        run: cargo test --test e2e
      - name: Run property tests (quick)
        run: PROPTEST_CASES=100 cargo test --lib -- --ignored
```

## Coverage Goals

- **Unit Tests**: 80%+ coverage of each module
- **Integration Tests**: All module interaction paths
- **E2E Tests**: All user-facing features
- **Property Tests**: All critical invariants

## Test Development Workflow

1. **Write Sanity Test First**: Before implementing feature, write simplest test
2. **Implement Feature**: Make test pass
3. **Add Property Tests**: Identify invariants, write property tests
4. **Add E2E Test**: Verify feature works end-to-end
5. **Run Full Suite**: Ensure no regressions

## Debugging Failed Tests

### Unit Test Failure
1. Run with `--nocapture` to see output
2. Add `dbg!()` statements
3. Use `cargo test -- --test-threads=1` to avoid interleaving

### Integration Test Failure
1. Isolate which modules are interacting incorrectly
2. Add unit tests for edge cases
3. Check module boundaries

### E2E Test Failure
1. Use `harness.screen_to_string()` to see rendered output
2. Check `harness.get_buffer_content()` vs screen
3. Add logging to Editor event loop
4. Use `RUST_LOG=debug cargo test`

### Property Test Failure
1. Property test framework will minimize failing case
2. Copy minimal case to regular test for debugging
3. Check if invariant is actually always true

## Performance Testing

### Benchmarks (benches/benchmarks.rs)

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_insert(c: &mut Criterion) {
    c.bench_function("buffer insert 1000 chars", |b| {
        b.iter(|| {
            let mut buffer = Buffer::new();
            for i in 0..1000 {
                buffer.insert(i, "a");
            }
        });
    });
}

criterion_group!(benches, bench_insert);
criterion_main!(benches);
```

Run with:
```bash
cargo bench
```

### Large File Tests

Create fixtures:
```bash
# 1MB file
python3 -c "print('x' * 1000000)" > tests/fixtures/1mb.txt

# 100MB file
python3 -c "print('x' * 100000000)" > tests/fixtures/100mb.txt

# 1GB file (not checked in)
python3 -c "print('x' * 1000000000)" > /tmp/1gb.txt
```

Test:
```rust
#[test]
#[ignore] // Slow test
fn test_large_file_1gb() {
    let mut harness = EditorTestHarness::new(80, 24);
    harness.open_file(Path::new("/tmp/1gb.txt")).unwrap();

    // Should load instantly (memory-mapped)
    assert!(harness.get_buffer_content().len() > 0);

    // Should scroll instantly
    harness.send_key(KeyCode::End, KeyModifiers::CONTROL);
    harness.render().unwrap();

    // Should edit instantly
    harness.type_text("hello");
    assert!(harness.get_buffer_content().ends_with("hello"));
}
```

## Test Maintenance

- **Keep tests fast**: Unit tests should run in <1s total
- **Keep tests isolated**: No shared state between tests
- **Keep tests readable**: Test name describes what's tested
- **Keep tests reliable**: No flaky tests, no timing dependencies
- **Update tests with features**: When feature changes, update tests

## Success Criteria

Before merging any PR:
- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] All E2E tests pass
- [ ] No new warnings
- [ ] Property tests pass (with reasonable case count)
- [ ] Code coverage doesn't decrease

Before releasing:
- [ ] All tests pass including ignored slow tests
- [ ] Benchmarks show acceptable performance
- [ ] Large file tests (1GB+) pass
- [ ] Manual testing on real terminal
