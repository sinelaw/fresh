# Syntax Highlighting: Parse State Checkpoint Design

## Problem

Fresh uses syntect (TextMate grammars) for syntax highlighting. Syntect's parser
is a sequential state machine — it must process text line-by-line from the start
of the file to correctly track embedded language transitions (e.g. CSS inside
HTML `<style>` tags, JS inside `<script>`, code inside markdown fences).

The original implementation parsed a small window around the viewport
(`viewport_start - 10KB .. viewport_end + 10KB`) with a fresh `ParseState`. This
broke for embedded languages when the embedding tag was more than 10KB before
the viewport — the parser didn't know it was in CSS mode and produced zero
highlight spans.

## Current Implementation (v1: byte-interval checkpoints)

**Commit:** `5f6a0b98` — "Fix embedded language highlighting via parse state checkpoints"

Stores `ParseState` + `ScopeStack` checkpoints in a `Vec<ParseCheckpoint>` at
~4KB byte intervals. On cache miss, resumes from the nearest checkpoint before
the viewport. For files > 1MB where no checkpoint exists, falls back to a fresh
`ParseState` (correct for single-language files).

### Known performance issue

Every edit calls `invalidate_range`, which discards all checkpoints after the
edit point. On the next render, the engine must re-parse from the nearest
surviving checkpoint through the viewport (~20KB for a typical view). This
happens on **every keystroke**, making typing sluggish in large files.

The root cause: checkpoints store raw byte offsets that become stale after
inserts/deletes, so all checkpoints after the edit must be discarded. Even
with checkpoints before the edit surviving, re-parsing 20KB per keystroke is
too slow in debug builds and noticeable in release for large files (200KB+).

## Planned Implementation (v2: marker-based checkpoints with convergence)

### Key ideas

1. **Use the marker system for checkpoint positions.** Fresh's `MarkerList` uses
   an AVL interval tree with lazy delta propagation. Markers automatically
   adjust their byte offsets on buffer edits in O(log n). This means checkpoint
   positions stay correct across edits without discarding them.

2. **Convergence-based invalidation (VSCode's approach).** After an edit, don't
   discard downstream checkpoints. Instead, defer validation to render time:
   re-parse from the checkpoint before the edit, and at each subsequent
   checkpoint, compare the new `ParseState` with the stored one. If they
   match, **stop** — everything downstream is still valid. Most single-character
   edits converge after 1-2 checkpoints because they don't change the parse
   state (you're still "inside CSS" or "inside a function").

3. **Same system for small and large files.** The only difference is when
   checkpoints get populated:
   - **Small files (< 1MB):** Parse from byte 0 on first render, creating
     checkpoints across the entire parsed region. Full coverage.
   - **Large files (> 1MB):** Parse only the viewport region on first render.
     Extend coverage lazily as the user scrolls. Gaps are filled by parsing
     from the nearest existing checkpoint forward.

### Design

```
TextMateEngine {
    marker_ids: Vec<MarkerId>,          // sorted; positions tracked by MarkerList
    states: HashMap<MarkerId, (ParseState, ScopeStack)>,
    dirty_from: Option<usize>,          // byte offset of earliest unvalidated edit
    span_cache: Option<TextMateCache>,  // cached spans for last viewport
}
```

**Checkpoint interval:** ~256 bytes. Since syntect parses line-by-line,
checkpoints land at the first line boundary after each 256-byte interval. This
is effectively every ~4-8 lines for typical code. A 200KB file gets ~800
markers — well within the marker system's O(log n) comfort zone.

**On edit at byte X:**
- Markers auto-adjust positions (handled by MarkerList).
- Set `dirty_from = min(dirty_from, X)`.
- Clear span cache if it overlaps the edit.
- Do NOT delete or re-parse anything yet.

**On render (highlight_viewport):**
1. Check span cache → return if valid.
2. If `dirty_from` is set and before viewport: run convergence walk.
   - Find the marker just before `dirty_from`.
   - `query_range(dirty_from, viewport_end)` → get markers to validate.
   - Re-parse from the pre-dirty marker forward, line by line.
   - At each marker position, compare new state with stored state.
   - If match → stop. Clear `dirty_from`. All downstream markers are valid.
   - If mismatch → update the marker's stored state. Continue.
3. If viewport has gaps (no markers): parse from nearest marker or byte 0,
   creating new markers as we go.
4. Collect spans for viewport from the parse, cache them.

**On scroll to new region:**
- If markers cover the region → collect spans directly (no parsing).
- If gap → find nearest marker before the gap, parse forward to fill.

### Performance characteristics

| Operation | Current (v1) | Planned (v2) |
|-----------|-------------|-------------|
| Keystroke (typical) | Re-parse ~20KB from checkpoint | Re-parse 1-2 checkpoints (~500 bytes) |
| Keystroke (opens `/*`) | Re-parse ~20KB | Re-parse until `*/` convergence |
| Scroll within cached region | Span cache hit | Span cache hit |
| Jump to uncached region | Parse from byte 0 or checkpoint | Parse from nearest marker |
| Memory (200KB file) | ~50 checkpoints (~10KB) | ~800 markers (~200KB with ParseState) |

### Alternatives considered

**Per-line state caching (pure VSCode model):** Store ParseState at every line.
Convergence at every `\n` instead of every ~256 bytes. Faster convergence but
requires line-number indexing and splicing the state array on newline
insert/delete. More complex and couples highlighting to buffer line tracking.
Rejected in favor of byte-interval + markers which is simpler and decoupled.

**Larger checkpoint intervals (4KB, current):** Fewer markers but slower
convergence — up to 4KB of re-parsing before checking if state matches.
Keystroke latency is proportional to interval size. 256 bytes is a better
tradeoff: 16x more markers but 16x faster convergence per edit.

**No convergence (current approach):** Discard all checkpoints after edit,
re-parse from surviving checkpoint. Simple but O(viewport_size) per keystroke.
Unacceptable for large files.

**Background thread parsing (Sublime Text model):** Parse the full file in a
background thread, render viewport immediately with partial results. More
complex (async coordination, partial rendering) for marginal benefit over
convergence-based approach.

## References

- [VSCode syntax highlighting optimizations](https://code.visualstudio.com/blogs/2017/02/08/syntax-highlighting-optimizations) — per-line state caching with convergence
- [syntect docs on caching](https://docs.rs/syntect) — recommends checkpointing ParseState every ~1000 lines
- [microsoft/vscode-textmate](https://github.com/microsoft/vscode-textmate) — StateStack cached per line, incremental retokenization
- Fresh marker system: `crates/fresh-editor/src/model/marker.rs`, `marker_tree.rs`
