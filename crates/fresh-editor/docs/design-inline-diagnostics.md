# Design: Inline Diagnostics

## Overview

Display LSP diagnostic messages inline at the end of the affected line, rendered
after the source code with a severity-colored background. This is similar to
Neovim's `virtual_text` diagnostics or VS Code's "Error Lens" extension.

```
  4 │ fn main() {
  5 │     let x: i32 = "hello";  ◀ expected `i32`, found `&str`
  6 │     unused_var();           ◀ unused variable: `unused_var`
  7 │ }
```

The diagnostic text appears to the right of the source code, separated by a
gap, with background/foreground colors matching the diagnostic severity (red
for errors, yellow for warnings, blue for info, gray for hints).

## Goals

- Show the highest-severity diagnostic message inline at the end of each line
- Color-code by severity using existing `DiagnosticColors` from the theme
- Truncate messages that would exceed the viewport width
- Toggle-able via a configuration setting (off by default initially)
- No interference with existing diagnostic overlays (underlines/backgrounds)
- Maintain performance: viewport-localized, no full-buffer scans
- Work correctly with line wrapping, code folding, and horizontal scrolling

## Non-Goals (for initial implementation)

- Showing multiple diagnostics per line (show only highest severity)
- Clickable diagnostic text
- Multi-line diagnostic messages (truncate to single line)
- Plugin API for custom inline annotations (future extension)

---

## Design Alternatives

### Alternative A: Extend VirtualText with `EndOfLine` position (Recommended)

Add a new `VirtualTextPosition::EndOfLine` variant to the existing virtual text
system. Diagnostic messages are added as virtual texts anchored to the start of
each diagnostic line, with `EndOfLine` positioning.

**Data flow:**
```
LSP publishDiagnostics
  → apply_diagnostics_to_state_cached()  [existing]
    → create overlay (underline/bg)       [existing, unchanged]
    → create EndOfLine virtual text       [NEW]
  → render loop
    → after rendering line content, before fill-to-end
    → check for EndOfLine virtual texts on this line
    → render: gap + severity_icon + truncated_message
```

**Changes required:**

1. `virtual_text.rs`: Add `EndOfLine` variant to `VirtualTextPosition`
2. `virtual_text.rs`: Update `is_inline()` / `is_line()` (EndOfLine is inline)
3. `split_rendering.rs`: After rendering all characters in a line, before the
   end-of-line fill, check for EndOfLine virtual texts and render them
4. `diagnostics.rs`: In `apply_diagnostics_to_state()`, also create EndOfLine
   virtual texts for each diagnostic (deduped per line, highest severity wins)
5. `theme/types.rs`: No changes needed (reuse existing `DiagnosticColors`)
6. Config: Add `inline_diagnostics: bool` setting

**Rendering logic (in split_rendering.rs):**
```
after last char of line:
  remaining_width = viewport_width - current_column
  if remaining_width > MIN_DIAGNOSTIC_WIDTH (e.g., 10):
    render gap (2 spaces)
    render severity icon (e.g., "● " for error)
    render message text, truncated to fit remaining_width
    apply severity background + foreground style
```

**Pros:**
- Leverages all existing VirtualText infrastructure (markers, namespaces,
  priority, bulk clear, viewport queries)
- Marker-based: positions auto-adjust on buffer edits
- Namespace-based: easy bulk removal on diagnostic update
- Plugin-accessible through existing VirtualText API (future)
- Consistent with how inlay hints are rendered

**Cons:**
- EndOfLine is semantically different from BeforeChar/AfterChar - it's
  anchored to a line, not a character position
- Rendering logic is different from other virtual text (right-aligned/
  truncated vs. inline-injected)
- Need to handle the "one per line" dedup at the data level or render level

### Alternative B: Dedicated InlineDiagnostic manager

Create a new `InlineDiagnosticManager` (similar to `OverlayManager`,
`VirtualTextManager`, etc.) specifically for end-of-line annotations.

**Data model:**
```rust
pub struct InlineDiagnostic {
    marker_id: MarkerId,     // Anchored to line start byte
    message: String,
    severity: DiagnosticSeverity,
    source: Option<String>,  // e.g., "rustc", "clippy"
}

pub struct InlineDiagnosticManager {
    diagnostics: BTreeMap<u64, InlineDiagnostic>,  // marker_id -> diagnostic
    namespace: String,
}
```

**Pros:**
- Clean, purpose-built data model
- Can enforce "one diagnostic per line" at the data level
- Diagnostic-specific features are natural (severity, source, dedup strategy)
- No changes to the VirtualText system
- Clear separation of concerns

**Cons:**
- Duplicates marker/namespace infrastructure from VirtualText
- New manager to add to `EditorState`, wire through rendering
- Not reusable for non-diagnostic end-of-line annotations
- More boilerplate code

### Alternative C: Overlay extension with text content

Extend the existing `Overlay` system to support text content rendered at the
end of an overlay's range.

**Changes:**
```rust
pub struct Overlay {
    // ... existing fields ...
    pub end_of_line_text: Option<String>,  // NEW: text to render at EOL
    pub end_of_line_style: Option<Style>,  // NEW: style for EOL text
}
```

**Pros:**
- Minimal new infrastructure
- Overlays already track diagnostic ranges and messages
- Single data structure for both underline and inline text

**Cons:**
- Overlays are range-based decorations, not text injections - conceptual
  mismatch
- Overlay rendering is per-character (style application), not text injection
- Multiple overlays on the same line need dedup logic in the renderer
- Overloads the Overlay type with a different concern

### Alternative D: Right margin annotations

Use the existing `MarginManager` right margin infrastructure.

**Pros:**
- MarginManager already has left/right margin concept
- `MarginConfig::right_default()` exists (disabled by default)

**Cons:**
- Right margin is fixed-width for ALL lines, wasting horizontal space
- Fixed width can't adapt to variable-length diagnostic messages
- Not appropriate for variable-content annotations
- Poor UX: either too narrow (truncated messages) or too wide (wasted space)

### Alternative E: LineBelow virtual text

Use existing `VirtualTextPosition::LineBelow` to show diagnostics on a
dedicated line below the affected source line.

**Pros:**
- Already implemented, no rendering changes needed
- Full line width available for the message
- No truncation needed

**Cons:**
- Takes up vertical space (pushes content down)
- Disrupts code reading flow more than inline annotations
- Doesn't match the design spec (which shows inline, not below)
- Changes the apparent line count, confusing scrollbar/minimap

---

## Recommendation: Alternative A (Extend VirtualText)

Alternative A is recommended because it:

1. **Minimizes new code** - reuses the mature VirtualText infrastructure
2. **Follows existing patterns** - markers, namespaces, viewport queries
3. **Is extensible** - `EndOfLine` virtual text can later be used for other
   features (git blame inline, performance annotations, etc.)
4. **Maintains separation** - diagnostic data logic stays in `diagnostics.rs`,
   rendering logic stays in `split_rendering.rs`

The semantic difference between EndOfLine and BeforeChar/AfterChar is real but
manageable: EndOfLine anchors to a byte position but renders at the visual end
of the containing line, not at the character position. This is analogous to how
LineAbove/LineBelow anchor to a byte position but render as separate lines.

---

## Detailed Design (Alternative A)

### 1. VirtualTextPosition extension

```rust
pub enum VirtualTextPosition {
    BeforeChar,
    AfterChar,
    EndOfLine,   // NEW: render after all content on the line
    LineAbove,
    LineBelow,
}

impl VirtualTextPosition {
    pub fn is_line(&self) -> bool {
        matches!(self, Self::LineAbove | Self::LineBelow)
    }

    pub fn is_inline(&self) -> bool {
        matches!(self, Self::BeforeChar | Self::AfterChar | Self::EndOfLine)
    }
}
```

### 2. Diagnostic virtual text creation

In `diagnostics.rs`, extend `apply_diagnostics_to_state()`:

```rust
// After creating overlays (existing code), also create EndOfLine virtual texts
// Group diagnostics by line, keeping highest severity per line
let mut by_line: BTreeMap<usize, &Diagnostic> = BTreeMap::new();
for diag in diagnostics {
    let line = diag.range.start.line as usize;
    if let Some(existing) = by_line.get(&line) {
        if severity_priority(diag) > severity_priority(existing) {
            by_line.insert(line, diag);
        }
    } else {
        by_line.insert(line, diag);
    }
}

// Create EndOfLine virtual text for each line's highest-severity diagnostic
let ns = VirtualTextNamespace::from_string("lsp-inline-diagnostic".to_string());
state.virtual_texts.clear_namespace(&mut state.marker_list, &ns);

for (line, diag) in by_line {
    let byte_pos = buffer.line_start_byte(line);
    let style = diagnostic_inline_style(diag, theme);
    let prefix = diagnostic_severity_icon(diag);
    let text = format!("{} {}", prefix, first_line(&diag.message));

    state.virtual_texts.add_line(  // or a new add_eol() method
        &mut state.marker_list,
        byte_pos,
        text,
        style,
        VirtualTextPosition::EndOfLine,
        ns.clone(),
        severity_priority_value(diag),
    );
}
```

### 3. Rendering in split_rendering.rs

After the per-character loop for a line completes, before the end-of-line fill:

```rust
// Check for EndOfLine virtual texts on this line
if let Some(eol_vtexts) = eol_virtual_text_lookup.get(&line_start_byte) {
    // Take the highest priority one (last in sorted order)
    if let Some(vtext) = eol_vtexts.last() {
        let remaining = viewport_width.saturating_sub(current_visual_col);
        let gap = 2; // spaces between code and diagnostic
        let min_text_width = 8; // minimum chars to show

        if remaining > gap + min_text_width {
            // Render gap
            push_span("  ", Style::default());

            // Truncate message to fit
            let max_chars = remaining - gap;
            let display_text = truncate_to_width(&vtext.text, max_chars);

            // Render diagnostic text with severity style
            push_span(&display_text, vtext.style);
        }
    }
}
```

### 4. EndOfLine lookup construction

Build a separate lookup for EndOfLine virtual texts keyed by the line's start
byte (not the character byte position):

```rust
// In the decoration context setup, build EOL lookup
let eol_lookup: HashMap<usize, Vec<&VirtualText>> =
    state.virtual_texts.build_eol_lookup(
        &state.marker_list,
        viewport_start,
        viewport_end,
        |byte| buffer.line_start_byte(buffer.get_line_number(byte)),
    );
```

Alternatively, filter the existing `build_lookup` result for EndOfLine entries
and re-key by line start byte.

### 5. Configuration

Add to the editor configuration schema:

```json
{
    "diagnostics": {
        "inline_text": {
            "enabled": false,
            "min_spacing": 2,
            "min_message_width": 8,
            "show_source": false,
            "severity_filter": "all"
        }
    }
}
```

- `enabled`: Master toggle (default: false for initial release)
- `min_spacing`: Minimum gap between code and diagnostic text (default: 2)
- `min_message_width`: Don't show if less than N chars available (default: 8)
- `show_source`: Append source name e.g., "[rustc]" (default: false)
- `severity_filter`: "all", "error", "warning+error" (default: "all")

### 6. Style/theming

Reuse existing `DiagnosticColors` from the theme. The inline diagnostic style
combines the severity's foreground color with a dimmed/semi-transparent
background:

```rust
fn diagnostic_inline_style(severity: DiagnosticSeverity, theme: &Theme) -> Style {
    match severity {
        DiagnosticSeverity::ERROR => Style::default()
            .fg(theme.diagnostic_error_fg)
            .bg(theme.diagnostic_error_bg),
        DiagnosticSeverity::WARNING => Style::default()
            .fg(theme.diagnostic_warning_fg)
            .bg(theme.diagnostic_warning_bg),
        // ... etc
    }
}
```

Severity icons (Unicode, single-width):
- Error: `●` (red)
- Warning: `▲` (yellow)
- Info: `ℹ` (blue)
- Hint: `…` (gray)

### 7. Interaction with other features

**Line wrapping:** When line wrapping is enabled, the inline diagnostic should
appear at the end of the *last* visual line of a wrapped source line. If the
source line wraps such that no space remains on the last visual line, the
diagnostic is not shown (rather than wrapping it onto another line).

**Code folding:** Folded lines don't render, so their inline diagnostics are
naturally hidden. The fold header line may have its own diagnostic.

**Horizontal scrolling:** When scrolled right, the diagnostic moves with the
line content. If the line content extends past the viewport, the diagnostic is
not shown (no space).

**Multiple cursors / selections:** No interaction - inline diagnostics are
read-only decorations.

**Existing diagnostic overlays:** Unchanged. The underline/background overlays
on the diagnostic range continue to work. The inline text is additive.

### 8. Performance considerations

- **Viewport-localized:** Only create/query EndOfLine virtual texts for the
  visible viewport range
- **Hash-based caching:** Reuse existing `DIAGNOSTIC_CACHE` - only recreate
  virtual texts when diagnostics actually change
- **O(1) per-line lookup:** The EOL lookup is a HashMap keyed by line start
  byte, so checking each line during rendering is O(1)
- **No full-buffer scan:** Line start bytes are computed only for visible lines

### 9. Testing strategy

Following CONTRIBUTING.md's E2E testing requirements:

1. **E2E test: basic inline diagnostic display**
   - Open a file with a known LSP error
   - Assert the screen contains the diagnostic message text at the end of the
     affected line

2. **E2E test: severity priority**
   - File with error and warning on the same line
   - Assert only the error message is shown inline

3. **E2E test: truncation**
   - Long diagnostic message with narrow viewport
   - Assert message is truncated with ellipsis

4. **E2E test: toggle setting**
   - Enable/disable inline diagnostics via config
   - Assert messages appear/disappear

5. **Unit test: dedup logic**
   - Multiple diagnostics on same line
   - Assert highest severity wins

6. **Unit test: style computation**
   - Verify correct fg/bg colors for each severity level

---

## Implementation Plan

1. Add `EndOfLine` to `VirtualTextPosition` with appropriate classification
2. Add `build_eol_lookup()` to `VirtualTextManager`
3. Extend `apply_diagnostics_to_state()` to create EndOfLine virtual texts
4. Add rendering logic in `split_rendering.rs` for EndOfLine virtual texts
5. Add configuration setting and wire it through
6. Add E2E tests
7. Default to disabled; document in help/settings
