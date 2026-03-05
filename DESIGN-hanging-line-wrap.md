# Design: Hanging Line Wrap Indentation

Issue: https://github.com/sinelaw/fresh/issues/1169

## Problem

When line wrapping is enabled and a long line wraps, the continuation segments
currently start at column 0 (the left edge of the text area). This makes it
hard to visually distinguish wrapped continuations from new logical lines,
especially in structured content like XML/HTML where indentation carries
semantic meaning.

**Current behavior:**
```
  1 │     <element attr1="value1" attr2="valu
e2" attr3="value3" />
  2 │     <next-element />
```

**Desired behavior (hanging indent):**
```
  1 │     <element attr1="value1" attr2="valu
    │     e2" attr3="value3" />
  2 │     <next-element />
```

Continuation lines should be indented to the same level as the leading
whitespace of their parent logical line.

## Current Architecture

The rendering pipeline is:

```
Buffer bytes
  → LineIterator (logical lines)
  → Syntax highlighting / tokenization → Vec<ViewTokenWire>
  → apply_wrapping_transform() → inserts Break tokens at wrap points
  → ViewLineIterator → Vec<ViewLine> (one per display row)
  → SplitRenderer (gutter + content → terminal)
```

Key components:
- **`line_wrapping.rs`**: `wrap_line()` + `WrapConfig` — standalone wrapping
  logic used for scroll calculations. First and continuation lines currently
  have **identical text width**.
- **`split_rendering.rs` :: `apply_wrapping_transform()`**: The real wrapping
  engine for rendering. Operates on the token stream, inserting `Break` tokens.
  Currently uses a single `available_width` for all segments.
- **`view_pipeline.rs` :: `ViewLineIterator`**: Converts tokens (including
  Breaks) into `ViewLine` structs. Tags continuation lines with
  `LineStart::AfterBreak`.
- **`split_rendering.rs` :: gutter rendering**: For `is_continuation` lines,
  renders blank space in the gutter. Content starts at column 0 of the text
  area.
- **`markdown.rs`**: Already implements hanging indent for popup/hover text
  via `hanging_indent_width()` and `wrap_text_line()` — a useful reference
  but operates on a completely separate code path (styled lines, not the
  token pipeline).

## Design Space

There are two fundamental questions:

1. **Where is the indent computed?** (What knows about the leading whitespace?)
2. **Where is the indent applied?** (What actually emits the padding?)

And a cross-cutting concern:

3. **How does the indent interact with cursor navigation, scrolling, and
   position mapping?**

---

## Approach A: Indent via Reduced Continuation Width (wrapping layer)

**Idea**: Detect leading whitespace of the logical line, reduce the
`available_width` for continuation segments by that amount. The continuation
text gets fewer columns, and the renderer pads the left side.

### Where indent is computed
- In `apply_wrapping_transform()`. Before processing tokens for a logical
  line, scan the initial tokens to count leading whitespace visual width.

### Where indent is applied
- Two sub-options:

  **A1: Inject space tokens after each Break**
  - After inserting a `Break`, also insert a `Text(" ".repeat(indent))` token
    with `source_offset: None`.
  - The `ViewLineIterator` naturally picks these up as part of the line text.

  **A2: Reduce continuation width, pad in renderer**
  - `apply_wrapping_transform()` uses a smaller width for continuation
    segments.
  - The gutter renderer (which already handles `is_continuation`) also emits
    indent padding before the text content.

### Pros
- Self-contained: wrapping + indentation are co-located.
- `wrap_line()` in `line_wrapping.rs` can also be updated with the same
  logic for consistency (scroll calculations).
- A1 is simpler: no renderer changes needed beyond what the pipeline already
  does.

### Cons
- A1 creates "fake" whitespace characters in `ViewLine.text` that don't map
  to any source bytes. This could confuse cursor positioning, selection,
  copy-paste, and `char_source_bytes` mappings. Need careful handling with
  `source_offset: None`.
- A2 requires coordinating width reduction in two places (wrapping + renderer).
- Both require `apply_wrapping_transform()` to understand "logical line
  boundaries" to detect leading whitespace. Currently it only sees a flat
  token stream — it resets `current_line_width` on `Newline` but doesn't
  track per-line indentation.

### Impact on cursor/scroll
- The `line_wrapping.rs` `WrapConfig` currently gives
  `continuation_line_width == first_line_width`. If we reduce continuation
  width, `char_position_to_segment()` and viewport scroll calculations
  automatically account for narrower continuation segments.
- Cursor left/right at the boundary between segments would need to skip over
  injected indent spaces (A1) or the renderer would need to offset the cursor
  position (A2).

---

## Approach B: Indent purely in the renderer (rendering layer)

**Idea**: Keep wrapping unchanged. In `SplitRenderer`, when rendering a
continuation line (`is_continuation == true`), detect the indentation of the
parent line and prepend visual padding.

### Where indent is computed
- In the render loop of `render_buffer_lines()`. When encountering a
  continuation `ViewLine`, look back to the parent line's content and measure
  its leading whitespace.

### Where indent is applied
- In the same gutter/content rendering code that already handles continuation
  lines. Instead of just blank gutter, also emit indent spaces before the
  content.

### Pros
- Wrapping logic is unchanged — fewer moving parts.
- No impact on `line_wrapping.rs` or scroll calculations.
- Clean separation: wrapping decides *where* to break, renderer decides *how*
  to display.

### Cons
- **Width mismatch**: Wrapping breaks at `available_width`, but the rendered
  continuation only has `available_width - indent` columns. This means the
  continuation text will overflow or be clipped on the right. To fix this,
  wrapping must also know about the reduced width, bringing us back to
  Approach A.
- Alternatively, the renderer could truncate the text — but this loses content
  visibility, which defeats the purpose of wrapping.
- The viewport/scroll layer uses `wrap_line()` to estimate how many visual
  rows a line occupies. If the renderer uses a different effective width, scroll
  position calculations will be wrong (cursor off-screen, scroll jumping).

### Verdict
**Not viable as a pure renderer-only approach.** The wrapping layer *must*
know the continuation width to produce correctly-sized segments. A hybrid
(wrapping computes the width, renderer applies the padding) is Approach A2.

---

## Approach C: Smart indent via content analysis (AST-aware)

**Idea**: Instead of using leading whitespace, use the syntax tree to determine
the "semantic" indent level. For XML, indent to the attribute position; for
code, indent to the expression nesting level.

### Pros
- More intelligent wrapping (e.g., for XML: `<tag attr1="..." \n      attr2=`)
  rather than just matching the leading spaces.
- Could handle cases where logical indentation doesn't match whitespace (e.g.,
  continuation of a function call argument list).

### Cons
- Dramatically more complex.
- Requires per-language configuration or heuristics.
- Tree-sitter integration for indentation already exists but for auto-indent
  on newlines, not for wrap-indent.
- Not what the issue requests — the issue shows VS Code behavior which uses
  leading whitespace, not AST-aware indentation.
- Can be added as a future enhancement on top of the simpler approach.

### Verdict
**Out of scope for initial implementation.** The simple leading-whitespace
approach matches VS Code and handles the common case well.

---

## Recommended Approach: A1 (inject indent tokens in wrapping layer)

### Why A1 over A2

- **A1** (inject space tokens): The `ViewLineIterator` and renderer need no
  changes. The indent appears naturally as part of `ViewLine.text`. The only
  coordination is between wrapping width reduction and space injection — both
  happen in `apply_wrapping_transform()`.
- **A2** (reduce width + pad in renderer): Requires changes in two layers
  that must agree on the indent width. More error-prone.

### Detailed Design

#### 1. Detect indentation in `apply_wrapping_transform()`

When processing tokens, track "indentation of the current logical line":
- On `Newline` (or at the very start), reset `current_line_indent = 0` and
  enter "measuring indent" mode.
- While in measuring mode, accumulate the visual width of leading `Space`
  tokens and leading whitespace in `Text` tokens.
- Stop measuring on the first non-whitespace content.
- Clamp: `indent = min(indent, available_width - MIN_CONTENT_WIDTH)` where
  `MIN_CONTENT_WIDTH` (e.g., 10) ensures continuation lines always have
  room for content.
- If indent would be 0 or 1, skip it (no benefit).

#### 2. Reduce effective continuation width

After measuring, the effective width for continuation segments of this logical
line becomes: `continuation_width = available_width - indent`.

When `current_line_width >= continuation_width` (instead of `available_width`),
insert a `Break`.

#### 3. Inject indent after Break

After inserting a `Break` token, immediately inject:
```rust
ViewTokenWire {
    source_offset: None,
    kind: ViewTokenWireKind::Text(" ".repeat(indent)),
    style: None,
}
```
And set `current_line_width = indent`.

#### 4. Update `line_wrapping.rs` for consistency

`WrapConfig` already has `continuation_line_width`. Change `WrapConfig::new()`
to accept an optional indent width, or add a `with_indent()` builder method.

But there's a subtlety: `wrap_line()` in `line_wrapping.rs` doesn't see the
actual text indentation — it's called with raw line text. Two options:

- **Option 1**: Have the caller detect indentation and pass it in.
- **Option 2**: Have `wrap_line()` detect it from the text.

Option 2 is cleaner since `wrap_line()` already has the text:
```rust
pub fn wrap_line(text: &str, config: &WrapConfig) -> Vec<WrappedSegment> {
    let indent = detect_indent(text, config.first_line_width);
    let continuation_width = config.first_line_width.saturating_sub(indent);
    // ... use continuation_width for is_continuation segments
}
```

#### 5. Cursor positioning

`char_position_to_segment()` should continue to work correctly because:
- The injected indent spaces have `source_offset: None`.
- `ViewLine.char_source_bytes` will have `None` entries for the indent.
- Cursor movement already skips `None`-mapped characters in some contexts.

However, need to verify:
- Home key on a continuation line should go to the first real character
  (after the indent), not to column 0.
- Mouse clicks on the indent area should map to the start of the real
  content on that line.

#### 6. Configuration

Add a setting `editor.wrap_indent` (boolean, default `true`) to allow users
to disable hanging indent if they prefer the current behavior.

Alternatively, an enum: `"none"`, `"same"`, `"deeper"` (where `"deeper"` adds
extra indent to make continuation visually distinct — like VS Code's
`wrappingIndent: "deepIndent"`). Start with just `"none"` and `"same"`.

### Files to modify

| File | Change |
|------|--------|
| `line_wrapping.rs` | Add indent detection; reduce continuation width |
| `split_rendering.rs` :: `apply_wrapping_transform()` | Detect indent, reduce width, inject indent tokens after Break |
| `viewport.rs` | Pass indent info when computing scroll (uses `wrap_line()`) |
| `config.rs` | Add `wrap_indent` setting |
| Tests: `line_wrapping.rs`, `e2e/line_wrapping.rs` | New test cases |

### Edge cases to handle

1. **Tab-indented lines**: Tabs expand to variable width. The indent detection
   should use visual width (via `tab_expansion_width()`), and the injected
   indent should use spaces (since tabs at non-zero column would have wrong
   width).
2. **Very deep indentation**: Clamp indent so at least `MIN_CONTENT_WIDTH`
   (e.g., 10) characters remain for content.
3. **Lines with no indentation**: `indent = 0`, continuation behaves as today.
4. **Mixed indent (tabs + spaces)**: Convert to visual column width; inject
   spaces.
5. **Binary content / ANSI codes**: Skip indent detection for binary buffers.
6. **Compose mode**: Compose mode centers content; indent should work relative
   to the compose area, not the terminal edge.
7. **Force-wrapped ultra-long lines** (`MAX_SAFE_LINE_WIDTH`): These bypass
   normal wrapping — indent should still apply.

### Performance considerations

- Indent detection is O(k) where k = number of leading whitespace characters.
  Negligible compared to the wrapping computation itself.
- Injecting indent tokens adds a small number of extra tokens (one per
  continuation line). No significant memory or rendering impact.
- `wrap_line()` changes don't affect its O(n) complexity.

## Summary of tradeoffs

| Approach | Complexity | Correctness | Maintainability |
|----------|-----------|-------------|-----------------|
| A1: Inject indent tokens in wrapping | Medium | High (single source of truth for width) | Good (changes localized to wrapping layer) |
| A2: Reduce width + pad in renderer | Medium-High | High but fragile (two layers must agree) | Moderate |
| B: Renderer-only | Low | **Broken** (width mismatch) | N/A |
| C: AST-aware | Very High | Highest for edge cases | Poor (per-language) |

**Recommendation: Approach A1** — inject indent tokens in the wrapping layer.
It provides correct behavior with changes localized primarily in
`apply_wrapping_transform()` and `wrap_line()`, and naturally integrates with
the existing pipeline.
