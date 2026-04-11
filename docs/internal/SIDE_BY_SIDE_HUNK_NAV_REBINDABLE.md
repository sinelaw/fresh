# Make side-by-side diff hunk navigation rebindable

## Goal

The side-by-side diff view (composite buffer) already supports `n`/`p`
(and `]`/`[`) for next/prev hunk navigation, but the keys are hardcoded
in the `CompositeInputRouter`. Make them rebindable through the standard
keybinding system so they appear in the keybinding editor.

Read CONTRIBUTING.md before starting.

## Current Architecture

### How it works today

1. **Composite router intercepts keys** —
   `crates/fresh-editor/src/input/composite_router.rs:112-143` has a
   `route_key_event` function that pattern-matches raw `KeyEvent`s:

   ```rust
   (KeyModifiers::NONE, KeyCode::Char('n')) => RoutedEvent::NavigateHunk(Direction::Next),
   (KeyModifiers::NONE, KeyCode::Char('p')) => RoutedEvent::NavigateHunk(Direction::Prev),
   (KeyModifiers::NONE, KeyCode::Char(']')) => RoutedEvent::NavigateHunk(Direction::Next),
   (KeyModifiers::NONE, KeyCode::Char('[')) => RoutedEvent::NavigateHunk(Direction::Prev),
   ```

2. **Input dispatch calls the router** —
   `crates/fresh-editor/src/app/input.rs:4035-4089`,
   `try_route_composite_key` calls `CompositeInputRouter::route_key_event`
   and dispatches `RoutedEvent::NavigateHunk` to
   `composite_next_hunk`/`composite_prev_hunk`.

3. **Hunk nav implementation** —
   `crates/fresh-editor/src/app/composite_buffer_actions.rs:299-348`,
   `composite_next_hunk` searches for the next `RowType::HunkHeader` row
   and scrolls to center it at 1/3 from the top.

4. **Plugin fallback** — `crates/fresh-editor/plugins/audit_mode.ts`
   `review_next_hunk` (line ~1871) also calls
   `editor.compositeNextHunk(bufferId)` when
   `activeCompositeDiffState` is set, as a plugin-side fallback.

### Why this is a problem

The keybinding editor (accessible from the command palette) cannot rebind
`n`/`p`/`]`/`[` in composite buffer views because these keys are
intercepted by the router before the Action-based keybinding system sees
them. Users who want different keys (e.g. `gn`/`gp`, or `Ctrl+Down`/`Ctrl+Up`)
cannot customize this.

## What to do

### Step 1: Add Action variants

In `crates/fresh-editor/src/input/keybindings.rs`:

- Add `CompositeNextHunk` and `CompositePrevHunk` to the `Action` enum.
- Add entries to the `define_action_str_mapping!` macro (maps string
  names ↔ enum variants). Use names `"composite_next_hunk"` and
  `"composite_prev_hunk"`.
- Add them to `all_action_names()` so they appear in the keybinding editor.
- Add translation keys in `format_action` — use
  `t!("action.composite_next_hunk")` / `t!("action.composite_prev_hunk")`.

### Step 2: Add translation strings

Add the English strings to the locale file (check
`crates/fresh-editor/src/i18n/` for the locale YAML/JSON files):

```
action.composite_next_hunk: "Next Hunk (Side-by-Side Diff)"
action.composite_prev_hunk: "Previous Hunk (Side-by-Side Diff)"
```

### Step 3: Add default keybindings

In `crates/fresh-editor/src/input/keybindings.rs`, in the default
keybinding definitions, add:

```
n → CompositeNextHunk   (context: composite buffer active)
p → CompositePrevHunk   (context: composite buffer active)
] → CompositeNextHunk   (context: composite buffer active)
[ → CompositePrevHunk   (context: composite buffer active)
```

These should only activate when a composite buffer is the active view.
Check how other context-dependent bindings work (e.g. terminal mode
bindings use `KeyContext::Terminal`). You may need a
`KeyContext::CompositeBuffer` or use a custom context.

### Step 4: Handle the actions

In `crates/fresh-editor/src/app/input.rs`, in `handle_action`, add
cases for `Action::CompositeNextHunk` and `Action::CompositePrevHunk`.
These should call the existing `composite_next_hunk_active` /
`composite_prev_hunk_active` methods (defined in
`composite_buffer_actions.rs`).

### Step 5: Remove hardcoded keys from router

In `crates/fresh-editor/src/input/composite_router.rs`:

- Remove the `n`, `p`, `]`, `[` → `NavigateHunk` mappings from
  `route_key_event`.
- You can also remove the `NavigateHunk` variant from `RoutedEvent` and
  the `Direction` enum if no other code uses them, OR keep them for
  potential plugin-API use.

In `crates/fresh-editor/src/app/input.rs`:

- Remove the `RoutedEvent::NavigateHunk` dispatch in
  `try_route_composite_key` (since it's now handled by the Action
  system).
- The router's `try_route_composite_key` should still run FIRST for
  non-rebindable composite-specific keys (scroll, pane switch, etc.).
  Only hunk navigation moves to the Action system.

### Step 6: Update the plugin

In `crates/fresh-editor/plugins/audit_mode.ts`, the `review_next_hunk`
/ `review_prev_hunk` handlers currently call
`editor.compositeNextHunk()` as a fallback. This path is now redundant
since the Action system handles it. Remove the composite-buffer fallback
from the plugin handlers — when the composite buffer view is active, the
review-mode `n`/`p` bindings won't fire because the composite buffer's
Action bindings take precedence (or the composite buffer isn't a
review-mode context).

Verify this: if pressing `n` in the side-by-side view still goes through
the review-mode plugin handler (because `review-mode` context is active),
make sure the plugin handler returns early / doesn't interfere with the
Action-based hunk nav. The cleanest approach is to ensure the Action
keybinding takes priority over the plugin mode binding when a composite
buffer is focused.

## Testing

Run existing tests:
```bash
cargo test --package fresh-editor --test e2e_tests side_by_side_diff_hunk_nav
cargo test --package fresh-editor --test e2e_tests review_diff_ux_bugs
cargo test --package fresh-editor --test e2e_tests audit_mode
```

All must pass. The existing `side_by_side_diff_hunk_nav` tests use
direct API calls (`composite_next_hunk_active`), not keybindings, so
they should still pass. Add at least one new e2e test that presses the
`n` key in a composite buffer view opened from Review Diff drill-down
and verifies the viewport scrolled to a different hunk.

## Files to modify

| File | Change |
|------|--------|
| `crates/fresh-editor/src/input/keybindings.rs` | Add Action variants, action-string mapping, defaults |
| `crates/fresh-editor/src/app/input.rs` | Handle new actions, remove NavigateHunk dispatch |
| `crates/fresh-editor/src/input/composite_router.rs` | Remove hardcoded n/p/]/[ |
| `crates/fresh-editor/plugins/audit_mode.ts` | Remove composite fallback from review_next_hunk |
| `crates/fresh-editor/src/i18n/...` | Add translation strings |
| `crates/fresh-editor/tests/e2e/...` | Add keybinding-driven e2e test |
