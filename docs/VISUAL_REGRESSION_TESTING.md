# Visual Regression Testing

This project uses a hybrid visual regression testing system that combines:
- **Text snapshots** (via `insta`) for reliable test assertions
- **SVG screenshots** for visual review in GitHub PRs
- **Auto-generated documentation** that's always up-to-date

## Overview

Visual regression tests capture the terminal UI at different steps in user workflows. These screenshots serve two purposes:

1. **Regression Testing**: Detect unintended UI changes automatically
2. **Living Documentation**: Provide visual examples of all features

## How It Works

### 1. Test Execution

When you run tests, the system:
1. Captures terminal output as text (via `insta` snapshots)
2. Generates SVG screenshots from the terminal buffer
3. Collects metadata about each workflow step
4. Generates markdown documentation with embedded screenshots

### 2. Smart Updates

Screenshots are only regenerated when:
- The screenshot doesn't exist yet, OR
- The text snapshot has changed (actual UI change detected)

This prevents unnecessary git churn from identical-looking screenshots.

### 3. Automatic Documentation

Each test automatically generates its own documentation file:
- Test docs: `docs/visual-regression/tests/{TestName}.md` (written on test completion)
- Screenshots: `docs/visual-regression/screenshots/{TestName}_{Step}.svg`
- All files are committed and viewable in GitHub with inline SVG rendering
- No index needed - each test is self-contained and can run in parallel

## Writing Visual Regression Tests

### Basic Pattern

```rust
use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;

#[test]
fn visual_my_feature() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a flow with category and description
    let mut flow = VisualFlow::new(
        "My Feature",           // Flow name
        "Core Features",        // Category (for grouping in docs)
        "Description of feature" // Description
    );

    // Step 1: Initial state
    harness.capture_visual_step(&mut flow, "initial", "Starting state").unwrap();

    // Step 2: Perform action
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.render().unwrap();
    harness.capture_visual_step(&mut flow, "after_action", "After pressing Ctrl+P").unwrap();

    // Flow is automatically registered when dropped
}
```

### Categories

Organize tests into logical categories:
- **Core Features**: Basic editing, command palette, help system
- **File Management**: File explorer, file operations
- **Advanced Editing**: Multiple cursors, selections, find/replace
- **Layout**: Split views, tabs, panels
- **Appearance**: Themes, syntax highlighting, colors

## Running Tests

### Normal Test Run (Auto-Update)

```bash
cargo test
```

This will:
- Run all tests including visual regression tests (in parallel by default)
- Update snapshots if UI changed (first time only - will fail)
- Generate SVG screenshots in `docs/visual-regression/screenshots/`
- Generate test documentation in `docs/visual-regression/tests/`
- Each test writes its own files independently (parallel-safe)

### First Time Setup (Accept New Snapshots)

```bash
INSTA_UPDATE=always cargo test
```

Use this when:
- Running visual tests for the first time
- Intentionally changing the UI
- Adding new visual tests

### Review Snapshots

```bash
cargo insta review
```

Interactive tool to review and accept/reject snapshot changes.

### Run Only Visual Tests

```bash
cargo test --test e2e_tests visual_
```

### Skip Visual Documentation Generation

```bash
SKIP_VISUAL_DOCS=1 cargo test
```

## Viewing Results

### In GitHub PRs

When you create a PR with UI changes:
1. GitHub shows diffs in individual test files: `docs/visual-regression/tests/*.md`
2. SVG screenshots are rendered inline (click to expand)
3. Each test file is self-contained with all its steps
4. Reviewers can see exactly what changed visually

### Locally

1. **Test Files**: Browse `docs/visual-regression/tests/` for all test documentation
2. **Individual Test**: Open `docs/visual-regression/tests/{TestName}.md` for detailed steps
3. **SVG Files**: View `docs/visual-regression/screenshots/*.svg` in a browser
4. **Text Snapshots**: Check `tests/common/snapshots/*.snap`

## CI Integration

### Recommended Workflow

```yaml
# .github/workflows/visual-regression.yml
- name: Run tests with visual regression
  run: INSTA_UPDATE=always cargo test

- name: Check for uncommitted changes
  run: |
    if [[ -n $(git status --porcelain docs/visual-regression/) ]]; then
      echo "📸 Visual changes detected"
      git diff docs/visual-regression/
      git add docs/
      # Optionally: Create commit or fail to require manual review
    fi
```

### Options

**Option 1: Auto-commit** (easier for developers)
```yaml
- name: Commit visual changes
  run: |
    git add docs/
    git commit -m "Update visual regression screenshots" || true
    git push
```

**Option 2: Fail on changes** (require manual review)
```yaml
- name: Fail if visuals changed
  run: |
    if [[ -n $(git status --porcelain docs/) ]]; then
      echo "❌ Visual regression tests changed - please review locally"
      exit 1
    fi
```

## File Structure

```
fresh/
├── docs/
│   └── visual-regression/
│       ├── tests/                      # Individual test docs (auto-generated, committed)
│       │   ├── Basic_Editing.md
│       │   ├── Command_Palette.md
│       │   ├── File_Explorer.md
│       │   ├── Help_System.md
│       │   ├── LSP_Diagnostics.md
│       │   ├── Multiple_Cursors.md
│       │   ├── Split_View.md
│       │   └── Theme_Colors.md
│       └── screenshots/                # SVG screenshots (auto-generated, committed)
│           ├── Basic_Editing_01_initial.svg
│           ├── Basic_Editing_02_typed_text.svg
│           └── ...
├── tests/
│   ├── common/
│   │   ├── snapshots/                  # Text snapshots (committed)
│   │   │   └── *.snap
│   │   └── visual_testing.rs          # Visual testing infrastructure
│   └── e2e/
│       └── visual_regression.rs       # Visual regression tests
└── target/
    └── visual-tests-temp/              # Temp screenshots (ignored)
```

## Best Practices

### 1. Keep Terminal Size Consistent

Use consistent terminal dimensions across tests:
```rust
EditorTestHarness::new(80, 24)  // Standard size
EditorTestHarness::new(120, 30) // Wide layouts
```

### 2. Meaningful Step Names

Use descriptive step names and descriptions:
```rust
// Good
harness.capture_visual_step(&mut flow, "file_selected", "User selected README.md in explorer")

// Bad
harness.capture_visual_step(&mut flow, "step2", "after click")
```

### 3. Test Complete Workflows

Each test should capture a complete user workflow:
- Initial state
- User actions
- Expected results

### 4. Use Categories Wisely

Group related features together using categories to organize the documentation.

## Troubleshooting

### Snapshots Keep Failing

```bash
# Review changes interactively
cargo insta review

# Or accept all changes
INSTA_UPDATE=always cargo test
```

### SVG Files Not Generated

Check that:
1. Tests are passing (SVGs only generated on success)
2. `SKIP_VISUAL_DOCS` is not set
3. `docs/visual-regression/` directory exists

### Markdown Not Updated

The markdown is generated in the test teardown hook. Make sure:
1. All tests complete (don't exit early)
2. `SKIP_VISUAL_DOCS` is not set

### Large File Sizes

SVG files can be large (100-300KB each). This is normal because they contain:
- Full terminal grid (every cell)
- Color information
- Text content

To reduce size:
- Use smaller terminal dimensions
- Limit number of visual tests

## Advanced Usage

### Custom Snapshot Settings

```rust
// In your test file
#[cfg(test)]
mod settings {
    use insta;

    #[ctor::ctor]
    fn setup_insta() {
        insta::set_snapshot_path("../snapshots/custom");
    }
}
```

### Programmatic Access

```rust
use crate::common::visual_testing::generate_visual_documentation;

// Manually trigger documentation generation
generate_visual_documentation().unwrap();
```

## Limitations

1. **No pixel-perfect comparison**: Text snapshots detect content changes, not visual rendering differences
2. **Terminal-only**: Only works for terminal UI, not GUI applications
3. **No video**: Only static screenshots, no animations
4. **File size**: SVG files can be large (100-300KB per screenshot)

## Future Enhancements

Potential improvements:
- [ ] PNG rendering option (smaller files, better compatibility)
- [ ] Animated GIF support for multi-step workflows
- [ ] Pixel-level diff images (highlight what changed)
- [ ] Perceptual image comparison (detect visual changes even if text identical)
- [ ] HTML report with side-by-side comparisons

## See Also

- [TESTING.md](TESTING.md) - General testing strategy
- [ARCHITECTURE.md](ARCHITECTURE.md) - How fresh works
- [insta documentation](https://insta.rs/) - Snapshot testing library
