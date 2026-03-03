# Design: Inline Diagnostics

## Overview

Display LSP diagnostic messages inline at the end of the affected line, rendered
after the source code with a severity-colored background. This is similar to
Neovim's `virtual_text` diagnostics or VS Code's "Error Lens" extension.

```
  4 │ fn main() {
  5 │     let x: i32 = "hello";  ● expected `i32`, found `&str`
  6 │     unused_var();           ▲ unused variable: `unused_var`
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

## Design Alternatives Considered

### Alternative A: Extend VirtualText with `EndOfLine` position

Add a new `VirtualTextPosition::EndOfLine` variant. Diagnostic messages are
stored as virtual texts anchored to each diagnostic line.

**Rejected** because it creates one VirtualText + marker per diagnostic across
the entire buffer. In a huge file with thousands of diagnostics, this means:
- O(total_markers) adjustment on every keystroke
- O(total_vtexts) iteration in `build_lookup()` per frame
- O(n) bulk clear + recreate on every diagnostic update
This violates the "avoid full-buffer scans" guideline.

### Alternative B: Dedicated InlineDiagnosticManager

New purpose-built manager with markers per diagnostic.

**Rejected** for the same marker-bloat reasons as Alternative A, plus it
duplicates infrastructure already provided by the overlay system.

### Alternative C: Render-time overlay piggyback (Chosen)

Leverage the existing diagnostic overlay system. Overlays already carry
marker-tracked positions, viewport-efficient queries (O(log M + k)), severity
(encoded in priority), and the diagnostic message (`overlay.message`). The
renderer already filters overlays to the viewport and identifies diagnostic
lines. Inline text is derived from this data at render time with zero
additional persistent state or markers.

### Alternative D: Right margin annotations

Use MarginManager's right margin — fixed-width for ALL lines, wasting space.
**Rejected.**

### Alternative E: LineBelow virtual text

Full line below the affected source line. **Rejected** — takes vertical space,
disrupts reading flow, doesn't match the design spec.

---

## Chosen Design: Overlay Piggyback (Alternative C)

### Key Insight

The gutter diagnostic indicator (`●`) is already rendered by piggy-backing on
overlay data. The render pipeline:

1. Calls `overlay_manager.query_viewport()` — O(log M + k) efficient
2. Filters for `namespace == "lsp-diagnostic"`
3. Resolves each overlay's `range.start` to a `line_start_byte`
4. Collects into `diagnostic_lines: HashSet<usize>`
5. During per-line rendering, checks `diagnostic_lines.contains(&line_start_byte)`

Inline diagnostic text uses the same mechanism, but additionally reads the
overlay's `.message` field and `.priority` for dedup/severity.

### Data Flow

```
LSP publishDiagnostics
  → apply_diagnostics_to_state_cached()  [existing, unchanged]
    → creates Overlay per diagnostic with:
        - byte range (marker-tracked)
        - namespace "lsp-diagnostic"
        - priority (100=error, 50=warning, 30=info, 10=hint)
        - message (diagnostic text)         ← already set
  → render loop
    → query_viewport() filters overlays     [existing]
    → build diagnostic_inline_texts from viewport overlays  [NEW]
    → per-line: render inline text if present                [NEW]
```

**Zero new markers. Zero new data structures persisted on EditorState.
Zero changes to diagnostics.rs.**

### Render-Time Construction

In `DecorationContext::build()`, alongside the existing `diagnostic_lines`
HashSet, build a map of inline texts from the same viewport overlays:

```rust
// Existing: identifies which lines have diagnostics (for gutter ●)
let diagnostic_lines: HashSet<usize> = ...;  // unchanged

// NEW: per-line inline diagnostic text, deduped to highest severity
let diagnostic_inline_texts: HashMap<usize, (&str, Style)> =
    build_inline_diagnostic_texts(&viewport_overlays, &diagnostic_ns, ...);
```

The `build_inline_diagnostic_texts` function:
1. Iterates only viewport-filtered overlays (already computed)
2. Filters for `namespace == "lsp-diagnostic"`
3. For each, resolves `range.start` → `line_start_byte`
4. Keeps only the highest-priority (= highest severity) per line
5. Returns `HashMap<line_start_byte, (message, style)>`

This is O(k) where k = diagnostic overlays in the viewport — typically < 50.

### Per-Line Rendering

After the character loop for a line completes, before end-of-line fill:

```rust
if let Some((message, style)) = diagnostic_inline_texts.get(&line_start_byte) {
    let used_columns = current_visual_col;
    let available = viewport_width.saturating_sub(used_columns);
    let gap = 2;        // spaces between code and diagnostic
    let min_width = 10; // don't show if less than this available

    if available > gap + min_width {
        // Render gap
        push_span("  ", Style::default());
        // Truncate and render message
        let max_chars = available - gap;
        let display = truncate_to_width(message, max_chars);
        push_span(&display, *style);
    }
}
```

### Severity Style

Derived from the overlay's priority field (already set by `diagnostic_to_overlay`):

```rust
fn inline_style_from_priority(priority: i32, theme: &Theme) -> Style {
    match priority {
        100 => Style::default().fg(theme.diagnostic_error_fg).bg(theme.diagnostic_error_bg),
        50  => Style::default().fg(theme.diagnostic_warning_fg).bg(theme.diagnostic_warning_bg),
        30  => Style::default().fg(theme.diagnostic_info_fg).bg(theme.diagnostic_info_bg),
        _   => Style::default().fg(theme.diagnostic_hint_fg).bg(theme.diagnostic_hint_bg),
    }
}
```

### Configuration

Add to the editor configuration schema:

```json
{
    "diagnostics": {
        "inline_text": {
            "enabled": false
        }
    }
}
```

- `enabled`: Master toggle (default: false for initial release)

Keep configuration minimal for the initial implementation.

### Interaction with Other Features

**Line wrapping:** Inline diagnostic appears at the end of the *last* visual
line of a wrapped source line. If the line wraps such that no space remains on
the last visual line, the diagnostic is not shown.

**Code folding:** Folded lines don't render, so their inline diagnostics are
naturally hidden. The fold header line may have its own diagnostic.

**Horizontal scrolling:** If the line content extends past the viewport, the
diagnostic is not shown (no remaining space).

**Existing diagnostic overlays:** Unchanged. The underline/background overlays
on the diagnostic range continue to work. The inline text is additive.

### Performance

- **No new markers**: Zero per-keystroke cost beyond existing overlays
- **No new persistent state**: Everything derived at render time
- **O(k) construction**: k = diagnostic overlays in viewport (typically < 50)
- **O(1) per-line lookup**: HashMap keyed by line_start_byte
- **No full-buffer scan**: Piggybacks on overlay viewport query

### Testing Strategy

Following CONTRIBUTING.md E2E testing requirements:

1. **E2E test: inline diagnostic display**
   - Create a buffer with diagnostic overlays
   - Assert the screen contains the diagnostic message text at the end of
     the affected line

2. **E2E test: highest severity wins**
   - Two diagnostic overlays on the same line (error + warning)
   - Assert only the error message is shown inline

3. **E2E test: truncation**
   - Long diagnostic message with narrow viewport
   - Assert message is truncated (not wrapped)

4. **E2E test: toggle**
   - Enable/disable inline diagnostics via config
   - Assert messages appear/disappear

---

## Implementation Plan

1. Add `inline_diagnostic_texts` to `DecorationContext` (derived from
   viewport overlays at build time)
2. Add per-line rendering logic in the line render pass
3. Add configuration setting and wire through to renderer
4. Add E2E tests
5. Default to disabled
