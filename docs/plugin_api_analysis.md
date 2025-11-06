# Plugin API Design Analysis: TODO Highlighter Bug

## Summary

The TODO Highlighter plugin exhibited a critical bug where highlights would "slide around" instead of updating when buffer content changed. This document analyzes the root causes and proposes API improvements to prevent similar issues.

## The Bug

**Symptom:** When text was inserted at the beginning of a buffer, TODO keyword highlights remained at their original byte positions instead of moving with the text or being recalculated.

**Example:**
1. Buffer contains: `// TODO: Fix this`  (TODO highlighted at bytes 3-7)
2. User inserts `// FIXME: New line\n` at the beginning
3. **Expected:** Both FIXME and TODO should be highlighted
4. **Actual:** Only FIXME was highlighted (at bytes 3-7 where TODO used to be), TODO on line 2 had no highlighting

## Root Causes

### 1. **Overlays Use Absolute Byte Positions**

**Problem:** The overlay API uses absolute byte positions (`start..end`) that don't automatically adjust when the buffer changes.

```lua
-- Plugin code
editor.add_overlay(buffer_id, overlay_id, 3, 7, 255, 165, 0, false)
-- This creates an overlay at bytes 3-7
-- If text is inserted before byte 3, the overlay doesn't move!
```

**Why it's bad:**
- Violates the principle of least surprise - developers expect highlights to "stick" to content, not byte positions
- Makes it easy to create bugs - plugin authors must manually track and update overlays
- No compile-time or runtime checks to catch this

### 2. **No Automatic Overlay Adjustment**

**Problem:** The editor doesn't provide any mechanism to automatically adjust overlay positions when the buffer changes.

**Comparison with other editors:**
- VSCode: Decorations use `Range` objects that track positions
- Emacs: Overlays have a `before-string` / `after-string` property and can be anchored
- Vim: Virtual text is anchored to line/column positions

### 3. **Hook System Was Incomplete**

**Problem:** The `editor.on()` API was exposed to plugins but not implemented - it was just a stub that didn't actually call Lua callbacks.

```rust
// Old code (non-functional)
let hook_callback = Box::new(move |_args: &HookArgs| -> bool {
    // In real implementation, we'd call the Lua function here
    // For now, just return true
    true
});
```

**Impact:**
- Plugins couldn't respond to buffer changes
- Documentation/examples would suggest using hooks, but they didn't work
- Silent failure - no error, hooks just never fired

### 4. **Stale State Snapshot**

**Problem:** The `EditorStateSnapshot` used by plugins was only updated during `process_async_messages()`, not after synchronous events like typing.

```rust
// Snapshot was only updated here:
pub fn process_async_messages(&mut self) {
    // ... handle async messages ...
    self.update_plugin_state_snapshot();  // Only called here!
}
```

**Impact:**
- Plugins reading buffer content via `editor.get_buffer_content()` would get stale data
- Even if hooks were called, the plugin couldn't see the updated buffer
- Very difficult to debug - the API appeared to work but returned wrong data

### 5. **No Documentation of Invariants**

**Problem:** The API didn't document critical invariants:
- Overlays are absolute byte positions
- Overlays don't auto-adjust
- Plugin must re-scan on every buffer change
- Buffer content queries might be stale

## Fixes Implemented

### 1. Implemented Hook System

- Store Lua callbacks in a global `_hook_callbacks` table
- Call callbacks from Rust when events occur
- Trigger hooks after Insert and Delete events

### 2. Update Snapshot Before Hooks

- Call `update_plugin_state_snapshot()` before invoking hooks
- Ensures plugins see current state when hooks fire

### 3. Add Hooks to Plugin

- Register `after-insert` and `after-delete` hooks
- Re-scan buffer on every change to update overlays

## Proposed API Improvements

### 1. **Content-Anchored Overlays** (Breaking Change, High Priority)

**Current API:**
```lua
editor.add_overlay(buffer_id, overlay_id, start_byte, end_byte, r, g, b, underline)
```

**Proposed API:**
```lua
-- Option A: Explicit anchoring
editor.add_overlay(buffer_id, overlay_id, {
    start = start_byte,
    end = end_byte,
    anchor = "content"  -- or "absolute" for old behavior
}, style)

-- Option B: Pattern-based (for use cases like TODO highlighting)
editor.add_overlay_pattern(buffer_id, overlay_id, {
    pattern = "TODO",
    in_comments = true,
    style = {r = 255, g = 165, b = 0}
})
```

**Benefits:**
- Overlays automatically move with content
- Reduces boilerplate in plugins
- Harder to create bugs

### 2. **Incremental Overlay API** (Medium Priority)

Provide helpers for common patterns:

```lua
-- Clear and rebuild overlays for a range
editor.update_overlays_in_range(buffer_id, start, end, function(text, offset)
    -- Plugin returns array of overlays
    return find_keywords_in_text(text, offset)
end)
```

### 3. **Buffer Change Events with Deltas** (High Priority)

**Current:**
```lua
editor.on("after-insert", function()
    -- No info about what changed!
    -- Must re-scan entire buffer
end)
```

**Proposed:**
```lua
editor.on("after-insert", function(event)
    -- event.buffer_id - which buffer changed
    -- event.position - where insertion happened
    -- event.text - what was inserted
    -- event.affected_range - what range needs re-scanning

    -- Can do incremental updates!
    update_overlays_in_range(event.affected_range)
end)
```

### 4. **Overlay Collections** (Medium Priority)

Group related overlays for easier management:

```lua
local todo_highlights = editor.create_overlay_collection("todo_highlights")

todo_highlights:clear()  -- Clear all overlays in this collection
todo_highlights:add(start, end, style)
todo_highlights:refresh()  -- Re-apply all overlays
```

### 5. **Better Error Messages** (Low Priority, High Impact)

```lua
-- Instead of silent failure:
editor.add_overlay(buffer_id, overlay_id, -1, 5, ...)
-- Error: "Invalid overlay range: start (-1) must be >= 0"

-- Instead of stale data:
local content = editor.get_buffer_content(buffer_id)
-- Warning: "Buffer content may be stale. Call from a hook or after process_async_messages()"
```

### 6. **Explicit Snapshot Refresh** (Low Priority)

Allow plugins to explicitly request fresh state:

```lua
editor.refresh_snapshot()  -- Force snapshot update
local content = editor.get_buffer_content(buffer_id)  -- Guaranteed fresh
```

### 7. **Overlay Validation Mode** (Development Tool)

```lua
-- In debug builds:
editor.enable_overlay_validation()
-- Warns when:
// - Overlays are outside buffer bounds
// - Multiple overlays have same ID
// - Overlays aren't updated after buffer changes
```

## Priority Ranking

### Must Have (Prevent Common Bugs)
1. ✅ Hook system implementation
2. ✅ Snapshot updates before hooks
3. Buffer change events with deltas
4. Better error messages

### Should Have (Improve Developer Experience)
5. Content-anchored overlays
6. Overlay collections
7. Incremental overlay API

### Nice to Have (Advanced Features)
8. Explicit snapshot refresh
9. Overlay validation mode
10. Pattern-based overlay API

## Lessons Learned

### 1. **Mutable State + Absolute Positions = Bugs**

When you have:
- Mutable buffer content
- Overlays tied to absolute positions
- No automatic adjustment

You get a "pit of failure" where the default path leads to bugs.

### 2. **Incomplete APIs Are Worse Than No APIs**

The `editor.on()` stub was worse than not having it because:
- It gave false confidence
- It was hard to debug (no error, just no effect)
- Documentation/examples would be wrong

**Better:** Either implement it fully or don't expose it.

### 3. **State Consistency is Critical**

The stale snapshot issue shows that for query APIs like `get_buffer_content()`, you need clear invariants:
- When is data fresh?
- How do I ensure freshness?
- What happens if I query at the wrong time?

### 4. **Test The Sad Path**

All existing tests checked that highlights appeared initially. None tested that they updated correctly. The bug was in the "change" path, not the "initial" path.

**Better:** Always test state changes, not just initial state.

### 5. **Developer Experience Matters**

Even after fixing the bugs, the plugin still requires:
- Manual re-scanning on every change
- Manual overlay clearing and re-creation
- Managing overlay IDs manually

This is tedious and error-prone. A better API would handle common patterns automatically.

## Conclusion

The root cause was a combination of:
1. Low-level API (absolute positions) without high-level helpers
2. Incomplete implementation (hooks didn't work)
3. State consistency issues (stale snapshots)
4. Lack of documentation

The fix made it *possible* to write correct plugins, but didn't make it *easy*. The proposed improvements aim to make correct code the default path.

### Immediate Actions

1. ✅ Implement hooks
2. ✅ Fix snapshot staleness
3. Document the current limitations
4. Add warnings/errors for common mistakes

### Future Work

1. Implement content-anchored overlays
2. Add buffer change deltas to hook events
3. Provide higher-level overlay management APIs
4. Add validation mode for development
