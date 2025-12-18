# Paste Handling (Internal vs External)

Fresh currently has two distinct “paste” paths:

1. **Internal paste** (editor action `Paste`, e.g. `Ctrl+V` or menu `Edit -> Paste`)
   - Fresh can fetch clipboard contents and apply it as a single edit event.
   - Result: a single undo step reverses the whole paste.

2. **External terminal paste** (often `Ctrl+Shift+V`)
   - Fresh does **not** control this.
   - Many terminals send this as a rapid stream of key events (e.g. `KeyCode::Char(_)`), which currently becomes many independent edit events/undo steps.

Issue: #372 — external paste should behave more like internal paste in undo history.

## Goals

- Detect external paste when possible and apply it as **one undoable edit**.
- Avoid character-by-character paste behavior (auto-close, skip-over, auto-indent quirks) when pasting large text.
- Keep normal typing behavior (undo granularity, auto-close, etc.) unchanged.
- Provide a best-effort fallback for terminals that cannot reliably mark paste.

## Best detection: “bracketed paste” (reliable)

Many terminals support **bracketed paste mode**, where pasted content is wrapped in escape sequences. When the application enables bracketed paste mode, input libraries can surface paste as a single event.

Fresh already has evidence that `crossterm` can surface this as `Event::Paste(String)`:

- `src/bin/event_debug.rs` prints `Event::Paste(text)`

### Design

1. **Enable bracketed paste on startup**
   - When supported, this causes external paste (e.g. `Ctrl+Shift+V`) to arrive as a distinct “paste” event rather than a stream of keypresses.

2. **Handle `CrosstermEvent::Paste(text)` in the main loop**
   - The main event loop (`src/main.rs` `run_event_loop_common`) currently only handles `Key/Mouse/Resize` and ignores other `CrosstermEvent` variants.
   - Add handling for paste events and route them to the editor as a single atomic edit.

3. **Insert as one undo step**
   - Insert the entire `text` with one undoable entry:
     - Single cursor: one `Event::Insert { text, ... }`
     - Multi-cursor and/or selection deletion: one `Event::Batch { events, description }`
   - This reuses existing undo semantics: `Event::Batch` is undone/redone atomically (`src/model/event.rs`).

### Behavioral notes

- Paste insertion should avoid character-level auto-edit behaviors (auto-close delimiters, skip-over, etc.) that are desirable for typing but often undesirable for bulk paste.
- Newline/tab handling is naturally correct because the paste event includes the literal text.

## Fallback: burst coalescing heuristic (best-effort)

If bracketed paste is unavailable, external paste may still be indistinguishable from “very fast typing” because it arrives as a stream of `KeyCode::Char(_)` events.

In this case, the best practical option is a heuristic: coalesce a burst of fast character input into one edit.

### Heuristic proposal

- Maintain a short-lived buffer of “textual” keypresses in the event loop:
  - Typically `KeyCode::Char(c)` with no modifiers.
  - Optionally include `Enter` → `'\n'` and `Tab` → `'\t'` if desired.
- Treat input as a paste-burst when **both** are true:
  - Inter-key gaps are extremely small (e.g. `<= 2–5ms`)
  - Burst length crosses a minimum (e.g. `>= 8–16 chars`)
- Flush the buffer immediately when:
  - A non-text key arrives (navigation, delete, etc.)
  - Any mouse/resize event arrives
  - UI context changes (prompt/menu/settings/popup)
  - The time gap exceeds the threshold
- When flushing:
  - Apply the buffered text via the same “atomic insert” path used by bracketed paste (single undo step).

### Limitations

- This can never be perfect: extremely fast typists or keyboard macros may be misclassified as paste.
- Bracketed paste remains the preferred, robust solution.

## “Atomic insert” API (recommended internal abstraction)

To keep paste behavior consistent across internal paste, bracketed paste, and heuristic coalescing, define a single editor-level entrypoint such as:

- `insert_text_atomic(text, description, source)`

Where `source` might distinguish:

- `InternalPaste` (clipboard-driven)
- `ExternalPaste` (bracketed paste event)
- `CoalescedInput` (heuristic)

Responsibilities of the atomic insert path:

- Delete selections (if any) before inserting.
- Insert the entire string as one undoable entry (`Event::Insert` or `Event::Batch`).
- Ensure all cross-cutting concerns run through the normal event path:
  - Use `Editor::apply_event_to_active_buffer(...)` so LSP, plugin hooks, layout invalidation, etc. remain consistent (`src/app/mod.rs`).

## Suggested configuration knobs

To keep behavior tunable across terminals and preferences:

- `editor.enable_bracketed_paste` (default: true)
- `editor.external_paste_coalesce` (default: true)
- `editor.external_paste_min_chars` (default: 16)
- `editor.external_paste_max_inter_key_ms` (default: 3)
- Optional: `editor.external_paste_max_total_ms` (cap to avoid pathological grouping)

## Implementation touchpoints (for #372)

- `src/main.rs`
  - Handle `CrosstermEvent::Paste(text)` in `run_event_loop_common`.
  - (If needed) enable bracketed paste mode during terminal setup/teardown.
- `src/app/*`
  - Add an editor API to apply an “atomic insert” (single undo entry) for paste-like input sources.
- `src/model/event.rs`
  - No new primitives required; `Event::Batch` already provides atomic undo/redo.

