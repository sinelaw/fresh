# Plan: Typora-Style Seamless Canvas for Fresh

## Status

**Phase 2 COMPLETE**: Core features implemented, usability-tested, and working.

### What's implemented:
- `ConcealManager` (Rust) with marker-based position tracking
- `addConceal(bufferId, namespace, start, end, replacement?)` plugin API
- `clearConcealNamespace(bufferId, namespace)` plugin API
- Token pipeline integration: conceal ranges filter/split tokens during rendering
- Cursor-aware emphasis concealment in markdown compose plugin
  - `**bold**`, `*italic*`, `***bold-italic***`, `` `code` ``, `~~strikethrough~~`
  - Markers hidden when cursor is outside the span
  - Markers revealed when cursor enters the span (Typora "blur/focus" behavior)
- Link concealment: `[text](url)` → styled "text" with blue underline, cursor-aware reveal
- Table grid rendering: `|` → `│`, separator rows → `├──┼──┤`, cursor-aware per-row reveal
- Cursor positions passed in `ViewTransformRequest` hook (multi-cursor aware)
- Visual line cursor movement works with plugin view transforms (Up/Down through wrapped lines)
- Editing works inside concealed spans (emphasis, links, table cells, wrapped paragraphs)

### Usability Test Results (manual tmux testing)

| Test | Result | Notes |
|------|--------|-------|
| T1: Compose mode toggle | PASS | Line numbers hide, text reflows, status bar confirms |
| T2: Emphasis concealment | PASS | All 5 types (bold, italic, code, strikethrough, bold-italic) |
| T3: Emphasis cursor reveal | PASS | Markers appear when cursor enters span, hide when leaving |
| T4: Link concealment | PASS | `[text](url)` → "text" styled blue+underline; `![img]()` unaffected |
| T5: Link cursor reveal | PASS | Full syntax revealed when cursor enters link |
| T6: Table grid rendering | PASS | Box-drawing characters, proper corner pieces |
| T7: Table cursor reveal | PASS | Row with cursor shows raw pipes, others show grid |
| T8: Visual line movement | PASS | Down moves through visual lines (Col 1→79→160→236→next line) |
| T9: Multi-cursor conceal | SKIPPED | Multi-cursor add works via Ctrl+Alt+↑/↓ but not manually tested |
| T10: Wrapping/hanging indent | PASS | Lists get hanging indent, code blocks don't wrap |
| T11: Edit emphasis text | PASS | Typed inside bold span, markers revealed, edit preserved |
| T12: Edit link text | PASS | Edited link text, `[...]()` syntax preserved |
| T13: Edit table cell | PASS | Edited cell, raw pipes revealed on cursor row |
| T14: Edit wrapped paragraph | PASS | Text inserted, wrapping re-adjusted correctly |

### Known Issues / Remaining Tasks

#### Bugs (observed during usability testing)

1. **Render glitch on scroll / cursor flicker while typing**
2. **Mouse wheel scroll not working** in compose mode
3. **Table columns not aligned** — need auto-padding to equal column widths

---

### Root Cause Analysis: Render Flicker (Issues 1 & 2)

The flicker has two related causes stemming from the async plugin architecture.

#### The Rendering Pipeline (per frame)

There are **two separate command processing paths** in the event loop:

**Path A: Between frames** (`main.rs:2726` → `process_async_messages()`)
```
process_async_messages() → process_plugin_commands()
  → plugin_manager.process_commands()    // try_recv() from channel
  → handle_plugin_command(cmd)           // applies AddConceal, SubmitViewTransform, etc.
  → sets plugin_render_requested = true
  → returns needs_render = true
```

**Path B: During render** (`render.rs:251-325`)
```
render() {
  run_hook("view_transform_request", base_tokens)   // NON-BLOCKING send to plugin thread
  process_commands()                                 // try_recv() — race condition!
  render_content()                                   // uses whatever state exists
}
```

#### Race Condition in Path B

The `view_transform_request` hook and `process_commands()` happen in the same `render()` call, but there is a **race**:

1. `run_hook()` sends the hook request to the plugin thread via an mpsc channel (line 512 in `thread.rs`) — **non-blocking, returns immediately**
2. The plugin thread must receive the message, execute JS, and send back commands via a separate channel
3. `process_commands()` calls `try_recv()` — if the JS hasn't finished yet, the queue is **empty**
4. `render_content()` proceeds with stale state (old or missing view transform)

**Result:** The current frame renders with **stale transforms** from the previous frame. The new transforms arrive and are processed in the **next** `process_async_messages()` call, causing `plugin_render_requested = true` and triggering a second render — but there's a visible flash of the stale frame.

#### Why This Causes Two Distinct Symptoms

**Scroll glitch:** When scrolling changes the viewport, the current frame's base tokens are NEW (for the new viewport position), but the view transform is from the PREVIOUS viewport. The mismatch shows raw markdown briefly until the plugin re-transforms for the new viewport.

**Typing flicker:** When the user types, the buffer content changes. The current frame's base tokens reflect the edit, but the conceals/overlays are from BEFORE the edit (stale byte offsets). This causes momentary misalignment — markers appear at wrong positions for one frame.

#### Frame Sequence (Scroll Example)

```
Frame N:  viewport scrolls to new position
          → base_tokens = tokens for new viewport
          → run_hook("view_transform_request", new_tokens)  // sent to plugin thread
          → process_commands()  // EMPTY — plugin hasn't responded yet
          → render_content() uses STALE view_transform from Frame N-1
          → FLASH: wrong/raw content visible

Frame N+1: process_async_messages() picks up plugin's response
           → SubmitViewTransform, AddConceal, AddOverlay applied
           → plugin_render_requested = true → triggers render
           → render_content() uses CORRECT view_transform
           → Correct content now visible
```

The total flicker duration is ~16ms (1 frame at 60fps) plus plugin thread latency.

---

### Proposed Alternatives

#### Alternative A: Synchronous Hook Execution (simplest, highest latency risk)

Make `run_hook("view_transform_request")` blocking — wait for the JS handler to complete and its commands to arrive before proceeding with `render_content()`.

```rust
// In render.rs, replace non-blocking run_hook with:
self.plugin_manager.run_hook_sync("view_transform_request", args, timeout_ms: 50);
let commands = self.plugin_manager.process_commands();
// Now render_content() has up-to-date state
```

**Pros:** Eliminates race completely. Simple change.
**Cons:** Blocks the render thread. If JS handler takes >16ms, frame rate drops. Risk of deadlock if plugin calls back synchronously.

#### Alternative B: Reuse Previous Transform Until New One Arrives (mask the glitch)

Instead of rendering with base tokens when no transform is available for the current viewport, **re-use the previous frame's transform**. Only update the transform when a new one arrives.

The current code already does this partially — `view_state.view_transform` persists across frames. The problem is that `render_content()` rebuilds base tokens for the new viewport but uses the old transform (which has tokens for the old viewport).

**Enhancement:** When viewport changes and no new transform has arrived yet, **skip rendering the content area entirely** (keep the old frame's content) until the plugin responds. Or render a "loading" indicator.

```rust
// In build_view_data(), if viewport changed but transform is stale:
if transform_viewport_start != viewport.top_byte {
    return previous_frame_view_data.clone(); // reuse old frame
}
```

**Pros:** No visible glitch — old content stays until new content ready.
**Cons:** Slightly delayed viewport updates. Need to store viewport info with transforms.

#### Alternative C: Double-Buffer the View Transform (cleanest)

Store the viewport that each transform was built for. In `render_content()`, if the stored transform's viewport doesn't match the current viewport, hold the old visual output until a matching transform arrives.

```rust
struct SplitViewState {
    view_transform: Option<ViewTransformPayload>,
    transform_viewport_start: usize,  // NEW: which viewport this transform was built for
    // ...
}
```

In `build_view_data()`:
```rust
let transform = if let Some(vt) = view_transform {
    if vt.viewport_start == viewport.top_byte {
        Some(vt)  // Transform matches current viewport — use it
    } else {
        None  // Stale transform — fall through to base tokens + suppress output
    }
} else {
    None
};
```

**Pros:** Clean. Never shows stale content.
**Cons:** Brief content freeze during scroll (might feel laggy).

#### Alternative D: Eager Plugin Execution on Same Thread (best UX, most work)

Run the markdown compose JS handler on the **main thread** during render, rather than on the async plugin thread. This would require either:

1. A lightweight synchronous plugin execution mode for "critical path" hooks like `view_transform_request`
2. Or compiling the markdown compose logic to Rust (no JS overhead)

**Pros:** Zero latency, zero flicker.
**Cons:** Significant architectural change. Blocks render thread on JS execution.

#### Alternative E: Suppress First Frame After State Change (pragmatic)

When a state change occurs (scroll, edit, cursor move) that would invalidate the current transform/conceal state, set a flag that suppresses rendering for one frame. The next frame picks up the plugin's response and renders correctly.

```rust
// In event handling (scroll/edit/cursor):
self.view_transform_invalidated = true;

// In render():
if self.view_transform_invalidated {
    // Don't draw content area — keep previous frame
    // Just process the hook + commands
    self.view_transform_invalidated = false;
    return;
}
```

**Pros:** Simple. No visible glitch.
**Cons:** Adds 16ms latency to all user interactions in compose mode. May feel slightly less responsive.

#### Alternative F: Optimistic Conceal Retention (targeted fix for typing flicker)

The typing flicker specifically happens because `clearConcealNamespace` + `addConceal` creates a window where conceals are cleared but not yet re-added. Instead:

1. Don't clear conceals until the new ones are ready
2. Use an "atomic swap" pattern: build the new conceal set, then swap it in
3. Or: the editor core automatically adjusts conceal byte offsets after edits (marker-based positions already do this!)

The conceal ranges use `MarkerId` for start/end positions, which auto-adjust with buffer edits. The real problem is that the plugin calls `clearConcealNamespace` followed by `addConceal` — the clear removes all conceals, and the adds haven't arrived yet.

**Fix:** Make `clearConcealNamespace` lazy — don't actually clear until the namespace gets new entries, then atomically replace.

```rust
// In ConcealManager:
fn clear_namespace_deferred(&mut self, namespace: OverlayNamespace) {
    self.pending_clears.insert(namespace);
}

fn add(&mut self, range: ConcealRange) {
    // If this namespace has a pending clear, execute it now (just before adding)
    if self.pending_clears.remove(&range.namespace) {
        self.ranges.retain(|r| r.namespace != range.namespace);
    }
    self.ranges.push(range);
}
```

**Pros:** Targeted fix for the typing flicker. Minimal change. No latency added.
**Cons:** Only fixes the conceal flicker, not the scroll glitch.

---

### Recommendation

Combine **Alternative B/C** (hold old content during scroll) with **Alternative F** (atomic conceal swap for typing). This would:

1. Eliminate scroll glitch by never showing stale transforms for a wrong viewport
2. Eliminate typing flicker by making conceal clear+add atomic
3. Not add any latency to user interactions
4. Require moderate code changes (viewport tracking in transform, lazy clear in ConcealManager)

#### Feature Requests

5. **Clickable links via OSC codes**: Link text should use terminal OSC 8 hyperlink escape codes to make them natively clickable in the terminal, not just styled with blue+underline.

6. **Header `#` concealment**: Hide `#` prefix markers, show styled heading text.

7. **Task list checkbox interaction**: `- [ ]` → `☐`, `- [x]` → `☑`, click to toggle.

8. **Code block fence concealment**: Hide ``` fences, show language label.

9. **Image link rendering**: `![alt](url)` → styled placeholder with alt text.

---

## Architecture Summary (Current State)

The plugin system provides:
- **`transformViewTokens`**: Receives base tokens, returns modified tokens. Can inject virtual tokens (`source_offset=null`) and omit source tokens.
- **Overlays**: Style byte ranges (fg, bg, bold, italic, underline, strikethrough).
- **Virtual text**: Inject decorative text before/after byte positions (non-editable, cursor skips them).
- **Conceal ranges**: Hide or replace byte ranges during rendering. Applied in the token pipeline after plugin transforms.
- **Cursor APIs**: `getCursorPosition()`, `cursor_moved` hook.
- **Mouse events**: `mouse_click` hook with content coordinates.

Key constraint: tokens with `source_offset=null` are **not navigable** — cursor backtracks to the nearest `Some(byte_offset)`. This means you can hide source tokens by omitting them, but you cannot replace them with navigable alternatives. The conceal ranges API handles this at the editor level, removing bytes from the token stream.

---

## Core Problem

Typora's "blur" behavior requires **cursor-aware conditional rendering**: show raw syntax when the cursor is inside a block, hide syntax and show rich rendering when the cursor is elsewhere. This needs two primitives that don't exist yet:

1. **Efficient cursor context in transforms** — the transform must know where the cursor is
2. **Source-mapped replacement tokens** — display different characters while preserving cursor mapping to the original source bytes

---

## New API Primitives (Rust-side Changes)

### Primitive 1: Cursor Position in View Transform Request

**Change**: Add `cursor_positions: Vec<usize>` to the `ViewTransformRequest` hook args.

**Rationale**: Currently, plugins must call `getCursorPosition()` separately. Since the view transform is called on every render, the cursor position should be included in the hook data to avoid an extra round-trip and ensure consistency (cursor may move between the hook fire and the API call).

**Rust changes**:
- `hooks.rs`: Add `cursor_positions` field to `HookArgs::ViewTransformRequest`
- `split_rendering.rs`: Include primary cursor (and all multi-cursor positions) when building hook args
- Plugin TS type: Add `cursor_positions: number[]` to the hook data

**Effort**: Small. Purely additive, no breaking changes.

### Primitive 2: Source-Mapped Replacement Tokens

**Change**: Allow `ViewTokenWire` to carry both a `source_offset: Some(N)` AND a modified `kind` that differs from the actual buffer byte at offset N.

Current behavior:
- `{ source_offset: Some(42), kind: { Text: "|" } }` — displays the character at byte 42 (the `kind` is informational, rendering reads from source)
- `{ source_offset: null, kind: { Text: "│" } }` — displays `│` but cursor cannot land here

New behavior with replacement:
- `{ source_offset: Some(42), kind: { Text: "│" }, replacement: true }` — displays `│` but cursor maps to byte 42

**Implementation approach**: Add an optional `replacement: bool` flag to `ViewTokenWire`. When `replacement=true` and `source_offset=Some(N)`:
- `ViewLineIterator` uses the token's `kind` text for display (not the buffer byte)
- `char_source_bytes[i] = Some(N)` — cursor still maps to byte N
- Arrow keys, click, selection all work against source positions

**Rust changes**:
- `api.rs`: Add `pub replacement: bool` to `ViewTokenWire` (default false, `#[serde(default)]`)
- `view_pipeline.rs` (`ViewLineIterator::next`): When `replacement=true`, use token's text content instead of looking up source byte. Still record `source_offset` in `char_source_bytes`.
- The existing `add_char!` macro already takes the character and source offset separately — just need to pass the replacement char instead of the source char.

**Effort**: Medium. Targeted change in ViewLineIterator, no architectural overhaul.

### Primitive 3: Conceal Ranges (Higher-Level Alternative)

**Change**: `editor.addConcealRange(bufferId, namespace, start, end, options?)` where options include optional replacement text and cursor behavior.

```typescript
editor.addConcealRange(bufferId, "md-syntax", startByte, endByte, {
  replacement?: string,        // Text to show instead (null = hide completely)
  replacementStyle?: { fg?, bg?, bold?, italic? },
  cursorBehavior: "skip" | "land-at-start" | "expand",
  // "skip": cursor arrows past it
  // "land-at-start": cursor can land at start byte but doesn't enter
  // "expand": entering the range triggers a re-render showing raw content
});
```

**Rationale**: This is higher-level than replacement tokens. The editor manages the concealment lifecycle, cursor skipping, and expansion. Plugins declare *what* to hide rather than how.

**Rust changes**:
- New `ConcealRange` struct in `view/` module
- `ConcealManager` similar to `VirtualTextManager` with marker-based position tracking
- Integration in `build_base_tokens` or `ViewLineIterator` to filter/replace concealed ranges
- Cursor movement logic in `input.rs` to handle skip/land behavior
- Hook to notify plugin when cursor enters a concealed range (for "expand" behavior)

**Effort**: Large. New subsystem, but provides the cleanest UX for all Typora features.

**Recommendation**: Implement Primitive 1 + 2 first (small+medium effort, unlocks everything at plugin level). Primitive 3 can come later as a convenience API built on top.

---

## Plugin-Side Implementation Plan

### Phase 0: Foundation — Cursor-Aware Transform

With Primitive 1 (cursor position in hook args):

```typescript
globalThis.onMarkdownViewTransform = function(data) {
  const cursorPos = data.cursor_positions[0]; // primary cursor byte offset
  const blocks = parseMarkdownBlocks(sourceText);

  // Find which block the cursor is in
  const focusedBlock = blocks.find(b =>
    cursorPos >= b.startByte + viewportStart &&
    cursorPos <= b.endByte + viewportStart
  );

  // For each block: if focused, emit raw tokens. If blurred, emit concealed tokens.
};
```

No Rust changes needed for this phase — plugin can already call `getCursorPosition()`. Primitive 1 just makes it faster and race-free.

### Phase 1: Emphasis Concealment (Bold/Italic/Strikethrough/Code)

**Blurred state** (cursor outside span):
- Scan source for `**bold**` patterns
- Omit the `**` marker tokens from output (or use replacement tokens to map them to zero-width)
- Apply bold overlay on the content bytes
- Result: user sees **bold text** without asterisks

**Focused state** (cursor inside span):
- Emit all tokens normally (show `**bold**` syntax)
- Still apply bold overlay on the content

**Token splitting needed**: Base tokens may contain `**bold**` as one Text token. Plugin must split it:
```
Input:  { source_offset: 10, kind: { Text: "**bold**" } }
Output: [
  // Omit or conceal bytes 10-11 (the leading **)
  { source_offset: 12, kind: { Text: "bold" } },
  // Omit or conceal bytes 16-17 (the trailing **)
]
```

With Primitive 2 (replacement tokens), the markers can be replaced with empty strings or zero-width spaces while keeping cursor mapping.

### Phase 2: Header Concealment

**Blurred state**:
- Omit `# ` prefix tokens
- Apply heading overlay (bold, larger-looking via color/weight)
- Content renders without the hash marks

**Focused state**:
- Show `# ` prefix (normal editing)

Simpler than emphasis — the prefix is always at line start, easy to isolate in token stream.

### Phase 3: Link Concealment

**Blurred state**: `[Link Text](https://url)` → `Link Text` (styled as link)
- Omit `[` token (1 byte)
- Emit `Link Text` tokens normally
- Omit `](https://url)` tokens
- Apply link overlay (blue, underline) on the content

**Focused state**:
- Show full `[Link Text](https://url)` syntax
- Apply lighter link styling

**Interaction**: Ctrl+Click to follow link (already possible via `mouse_click` hook + checking modifiers).

### Phase 4: Image Rendering

**Blurred state**: `![alt](path)` → show a placeholder or the alt text styled distinctively
- Omit `![` and `](path)` tokens
- Emit alt text with image icon overlay
- Could use virtual text to add an icon: `addVirtualText(pos, "🖼", ...)`

**Focused state**: Show full `![alt](path)` syntax

**Advanced (future)**: Actual image rendering would require terminal image protocol support (iTerm2/Kitty image protocol) — out of scope for this plan.

### Phase 5: Table Grid Rendering

**Blurred state**: Replace pipe characters with box-drawing characters, pad cells for alignment.

This is the most complex feature. Requires:

1. **Full table parsing**: Detect all rows, parse cells, compute column widths
2. **Cell padding**: Inject virtual Space tokens to align columns
3. **Pipe replacement**: Use replacement tokens to show `│` instead of `|`
4. **Separator replacement**: Replace `|---|---|` with `─────┼─────┼─────`

```
Source:  | Name  | Age |
         |-------|-----|
         | Alice | 30  |

Blurred: │ Name  │ Age │
         ├───────┼─────┤
         │ Alice │  30 │
```

With Primitive 2:
```typescript
// Replace | at byte 0 with │
{ source_offset: 0, kind: { Text: "│" }, replacement: true }
// Content token "Name" at bytes 2-5
{ source_offset: 2, kind: { Text: "Name" } }
// Pad with virtual spaces
{ source_offset: null, kind: "Space" }
{ source_offset: null, kind: "Space" }
// Replace | at byte 8 with │
{ source_offset: 8, kind: { Text: "│" }, replacement: true }
```

**Focused state** (cursor in table): Show raw pipes and original spacing for editing.

**Tab navigation**: Plugin listens for Tab key (via `pre_command` hook or keybinding) when cursor is in a table, and moves cursor to the next cell.

### Phase 6: Task List Checkboxes

**Blurred state**: `- [ ] Task` → `☐ Task`, `- [x] Done` → `☑ Done`
- Replace `- [ ] ` with `☐ ` using replacement tokens
- Replace `- [x] ` with `☑ ` using replacement tokens

**Interaction**: Click on the checkbox region toggles `[ ]` ↔ `[x]` via buffer edit:
```typescript
editor.on("mouse_click", "onCheckboxClick");

globalThis.onCheckboxClick = function(data) {
  // Check if click lands on a checkbox byte range
  // If so, replace [ ] with [x] or vice versa
  editor.replaceRange(bufferId, checkboxStart, checkboxEnd, newText);
};
```

**Focused state**: Show raw `- [ ] ` syntax for manual editing.

### Phase 7: Code Block Enhancement

**Blurred state**:
- The `` ``` `` fence lines can be dimmed or concealed
- Language tag shown as a subtle label (virtual text in top-right of block)
- Content gets syntax highlighting (already works via TextMate grammar)
- Background overlay on the whole block region

**Focused state**: Show `` ``` `` fences normally.

---

## Implementation Order

| Step | What | Rust Change? | Plugin Change? | Effort | Status |
|------|------|-------------|----------------|--------|--------|
| 1 | Conceal ranges API (addConceal/clearConcealNamespace) | Yes | Yes | M | DONE |
| 2 | Cursor-aware emphasis concealment | No | Yes | M | DONE |
| 3 | Link concealment with blue underline styling | No | Yes | M | DONE |
| 4 | Cursor positions in ViewTransformRequest hook | Yes (small) | Yes (small) | S | DONE |
| 5 | Table grid rendering (box-drawing chars via conceal replacement) | No | Yes | L | DONE |
| 6 | Visual line cursor movement fix for plugin transforms | Yes (small) | No | S | DONE |
| 7 | **Fix: Render glitch on scroll** (frame-skip between base and transformed tokens) | Yes | No | M | TODO |
| 8 | **Fix: Cursor/overlay flicker while typing** (off-by-one frame lag) | Yes | No | M | TODO |
| 9 | **Fix: Mouse wheel scroll** not working in compose mode | Yes | No | S | TODO |
| 10 | **Table column alignment** (auto-pad cells to equal column widths) | No | Yes | M | TODO |
| 11 | **Clickable links via OSC 8** terminal hyperlink escape codes | Yes | Yes | M | TODO |
| 12 | Header `#` concealment | No | Yes | S | TODO |
| 13 | Task list checkbox interaction (click to toggle) | No | Yes | S | TODO |
| 14 | Code block fence concealment | No | Yes | S | TODO |

Steps 1-6 are implemented and usability tested. Steps 7-9 are bugs found during testing. Steps 10-14 are remaining features.

---

## Key Architectural Decisions

### Q: Should concealment be in the plugin or the editor core?

**Recommendation**: Start in the plugin (steps 1-6), then extract common patterns into core APIs (step 10).

Rationale: The plugin already has full control over token output. Omitting tokens effectively conceals them. The cursor backtrack behavior is imperfect but usable. Once the UX patterns stabilize, promote them to first-class editor APIs for better cursor handling.

### Q: How to handle the "expand on focus" transition?

When cursor enters a concealed block, the plugin must:
1. Detect cursor entry (via cursor_positions in transform hook)
2. Re-emit all tokens for that block (no concealment)
3. The view re-renders with raw syntax visible

**Problem**: The cursor byte position was set based on the concealed view. After expanding, the same byte position may visually shift. The cursor should remain at the same source byte — which it will, since source_offset mapping is stable.

**Smooth transition**: The transition is instant (single render frame). No animation needed in a terminal.

### Q: What about multi-cursor?

Each cursor independently determines which block is "focused." A block is focused if ANY cursor is inside it. Multiple blocks can be focused simultaneously.

### Q: Performance?

The view transform already runs on every render. Adding cursor awareness and token filtering adds O(n) work where n = tokens in viewport. For typical markdown documents (< 1000 visible tokens), this is negligible.

Table column width calculation requires scanning all rows — but only rows in the viewport (the transform only sees viewport tokens). For tables that extend beyond the viewport, column widths may shift as the user scrolls. This is acceptable for a first implementation; a future optimization could cache table structure.
