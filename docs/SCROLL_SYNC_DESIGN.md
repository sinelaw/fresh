# Scroll Sync Design for Side-by-Side Diff View

## Problem Statement

The current scroll synchronization for side-by-side diff views is problematic:
- Jittery and slow scrolling
- Feedback loops where panes chase each other
- Async plugin hooks (`on_viewport_changed` → `setSplitScroll`) create race conditions

## Proposed Solution: Marker-Based Sync Anchors

Use the existing `MarkerList` and `IntervalTree` infrastructure to track "sync anchors" - corresponding positions in both buffers that should align when scrolling.

### Core Concept

```
Left Buffer (old)                    Right Buffer (new)
┌────────────────────┐               ┌────────────────────┐
│ Line 1  (context)  │ ←──────────── │ Line 1  (context)  │  SyncAnchor #1
│ Line 2  (context)  │               │ Line 2  (context)  │
│ Line 3  (context)  │               │ Line 3  (context)  │
├────────────────────┤               ├────────────────────┤
│ Line 4  (deleted)  │ ←──────────── │ Line 4  (added)    │  SyncAnchor #2
│ Line 5  (deleted)  │               │ Line 5  (added)    │
│                    │               │ Line 6  (added)    │
├────────────────────┤               ├────────────────────┤
│ Line 6  (context)  │ ←──────────── │ Line 7  (context)  │  SyncAnchor #3
│ Line 7  (context)  │               │ Line 8  (context)  │
└────────────────────┘               └────────────────────┘
```

Sync anchors are placed at:
1. Start of file (line 0 in both)
2. Start/end of each diff hunk
3. End of file

### Why Markers?

The codebase already has a sophisticated marker system (`src/model/marker_tree.rs`, `src/model/marker.rs`):

| Feature | Benefit for Scroll Sync |
|---------|------------------------|
| O(log n) position lookups | Fast anchor queries |
| Automatic position adjustment | Survives buffer edits |
| Line anchors with confidence | Track line numbers accurately |
| Range queries O(log n + k) | Find anchors in viewport efficiently |

### Data Structures

```rust
/// A sync anchor linking corresponding positions in two buffers
pub struct SyncAnchor {
    /// Marker ID in the left (old) buffer
    pub left_marker: MarkerId,
    /// Marker ID in the right (new) buffer
    pub right_marker: MarkerId,
    /// Line number at anchor in left buffer
    pub left_line: usize,
    /// Line number at anchor in right buffer
    pub right_line: usize,
}

/// A group of splits that scroll together
pub struct ScrollSyncGroup {
    /// The left split (old file)
    pub left_split: SplitId,
    /// The right split (new file)
    pub right_split: SplitId,
    /// Single source of truth: scroll position in "logical line" space
    /// This is the line number in the LEFT buffer
    pub scroll_line: usize,
    /// Which split was last scrolled by the user
    pub last_scrolled: SplitId,
    /// Sync anchors ordered by left_line
    pub anchors: Vec<SyncAnchor>,
}
```

### Algorithm

#### Creating Anchors (when diff is computed)

```rust
fn create_sync_anchors(diff_hunks: &[DiffHunk]) -> Vec<SyncAnchor> {
    let mut anchors = Vec::new();

    // Anchor at start of file
    anchors.push(SyncAnchor {
        left_line: 0,
        right_line: 0,
        left_marker: marker_list.create_at_line(0),
        right_marker: marker_list.create_at_line(0),
    });

    let mut left_offset = 0i64;
    let mut right_offset = 0i64;

    for hunk in diff_hunks {
        // Anchor at start of hunk
        anchors.push(SyncAnchor {
            left_line: hunk.old_start,
            right_line: hunk.new_start,
            // ... create markers
        });

        // Anchor at end of hunk (start of next context)
        let left_end = hunk.old_start + hunk.old_lines;
        let right_end = hunk.new_start + hunk.new_lines;
        anchors.push(SyncAnchor {
            left_line: left_end,
            right_line: right_end,
            // ... create markers
        });
    }

    anchors
}
```

#### Scroll Synchronization (at start of render)

```rust
fn sync_scroll_groups(&mut self) {
    for group in &mut self.scroll_sync_groups {
        // Get the authoritative scroll position
        let scroll_line = group.scroll_line;

        // Find the anchor just above scroll_line
        let anchor = group.anchors.iter()
            .filter(|a| a.left_line <= scroll_line)
            .last()
            .unwrap_or(&group.anchors[0]);

        // Calculate offset from anchor in left buffer
        let offset_from_anchor = scroll_line.saturating_sub(anchor.left_line);

        // Compute corresponding line in right buffer
        let right_scroll_line = anchor.right_line + offset_from_anchor;

        // Set viewport positions (synchronously, no async commands)
        self.set_split_scroll_to_line(group.left_split, scroll_line);
        self.set_split_scroll_to_line(group.right_split, right_scroll_line);
    }
}
```

#### Handling User Scroll Events

```rust
fn handle_scroll_event(&mut self, split_id: SplitId, delta_lines: i32) {
    // Check if this split is in a sync group
    if let Some(group) = self.find_sync_group_mut(split_id) {
        // Update the single source of truth
        if split_id == group.left_split {
            // Scrolling in left pane: directly update scroll_line
            group.scroll_line = (group.scroll_line as i64 + delta_lines as i64)
                .max(0) as usize;
        } else {
            // Scrolling in right pane: convert to left-buffer line space
            let current_right_line = self.get_viewport_line(split_id);
            let new_right_line = (current_right_line as i64 + delta_lines as i64)
                .max(0) as usize;

            // Find corresponding left line using anchors
            group.scroll_line = self.right_to_left_line(new_right_line, &group.anchors);
        }

        group.last_scrolled = split_id;
        // Actual viewport sync happens in sync_scroll_groups() at render time
    }
}
```

### Key Design Principles

1. **Single Source of Truth**: Only `scroll_line` (in left buffer's line space) is authoritative. Both viewports derive their positions from it.

2. **Synchronous Sync**: Viewport synchronization happens at the start of `render()`, not via async plugin commands. This eliminates race conditions.

3. **No Feedback Loops**: Since there's only one `scroll_line`, there's no possibility of panes "chasing" each other.

4. **Hunk-Boundary Alignment**: Anchors at hunk boundaries provide semantically meaningful alignment. Within a hunk, lines may not align 1:1 (which is correct for diff viewing).

5. **Edit Survival**: Markers automatically track position through buffer edits, so sync remains valid even if user edits while viewing.

### Comparison to Alternatives

| Approach | Pros | Cons |
|----------|------|------|
| **Marker-Based (this design)** | Leverages existing infra, survives edits, O(hunks) memory | Alignment only at hunk boundaries |
| **Line Offset Tables** | Line-by-line precision | O(lines) memory, must rebuild on edit |
| **Async Plugin Hooks** | Simple plugin API | Race conditions, feedback loops, jitter |

### Implementation Plan

1. **Add `ScrollSyncGroup` to Editor** (`src/app/mod.rs`)
   - New field: `scroll_sync_groups: Vec<ScrollSyncGroup>`
   - Helper methods: `create_scroll_sync_group()`, `remove_scroll_sync_group()`

2. **Sync at Render Start** (`src/app/render.rs`)
   - Call `sync_scroll_groups()` before `render_content()`
   - Set viewport positions directly (no async commands)

3. **Handle Scroll Events** (`src/input/mod.rs` or `src/app/mod.rs`)
   - Intercept scroll events for synced splits
   - Update `scroll_line` instead of viewport directly

4. **Plugin API** (`src/plugins/commands/`)
   - `createScrollSyncGroup(leftSplit, rightSplit, anchors)`
   - `removeScrollSyncGroup(groupId)`
   - Plugin computes anchors from diff, sends to core

5. **Update audit_mode.ts**
   - When opening side-by-side diff, compute anchors
   - Call `createScrollSyncGroup()` instead of using `on_viewport_changed`

### Testing

The existing test `test_side_by_side_diff_scroll_sync` should pass with:
- G (go to end) - both panes show late content
- g (go to start) - both panes show early content
- Ctrl+Down, PageDown, mouse wheel - smooth synchronized scrolling
- No jitter, no feedback loops

---

## Implementation Status (2025-12-31)

### Completed Work

1. **Core Infrastructure Created** (`src/view/scroll_sync.rs`)
   - `SyncAnchor` struct for mapping left/right line positions
   - `ScrollSyncGroup` with single source of truth (`scroll_line`)
   - `ScrollSyncManager` for managing multiple sync groups
   - Line conversion functions: `left_to_right_line()`, `right_to_left_line()`

2. **Plugin API Implemented** (`src/services/plugins/`)
   - `CreateScrollSyncGroup` command (synchronous, plugin-provided ID)
   - `SetScrollSyncAnchors` command for setting anchor pairs
   - `RemoveScrollSyncGroup` command for cleanup
   - Exposed via `editor.createScrollSyncGroup()`, `editor.setScrollSyncAnchors()`, `editor.removeScrollSyncGroup()`

3. **Plugin Integration** (`plugins/audit_mode.ts`)
   - Updated `review_drill_down` to create scroll sync groups
   - Generates anchors from aligned diff lines
   - Cleans up groups on buffer close via `on_buffer_closed` handler
   - Fallback to old `on_viewport_changed` approach if core sync fails

4. **Async Blocking Fix**
   - Changed `CreateScrollSyncGroup` from async to synchronous operation
   - Plugin now provides its own group IDs (counter-based)
   - Eliminates deadlock where plugin awaited response that required render cycle

### Current Issue: Scroll Sync Not Working

The `sync_scroll_groups()` function in `src/app/render.rs` is being called but computing `active_line = 0` instead of the actual scroll position.

**Debug Logging Added:**
```rust
tracing::debug!(
    "sync_scroll_groups: active_split={:?}, buffer_id={:?}, top_byte={}, buffer_len={}, active_line={}",
    active_split, active_buffer_id, active_top_byte, buffer_len, active_line
);
```

**Suspected Causes:**
1. Wrong buffer being used (split shows virtual buffer, but `buffer_for_split` returns different ID)
2. `viewport.top_byte` not being updated when cursor moves with 'G'
3. `get_line_number()` returning 0 for some reason

### Files Modified

| File | Changes |
|------|---------|
| `src/view/scroll_sync.rs` | New file - core sync infrastructure |
| `src/view/mod.rs` | Added `scroll_sync` module |
| `src/app/mod.rs` | Added `scroll_sync_manager` field, command handlers |
| `src/app/render.rs` | Added `sync_scroll_groups()` call in render loop |
| `src/services/plugins/api.rs` | Added scroll sync commands |
| `src/services/plugins/runtime.rs` | Added sync JS ops |
| `src/services/plugins/thread.rs` | Removed async response handling |
| `plugins/audit_mode.ts` | Updated to use core scroll sync |

### Next Steps

1. Debug why `active_line` is computed as 0:
   - Check if `viewport.top_byte` is being set correctly
   - Verify correct buffer is being used for virtual diff buffers
   - Add more logging around viewport updates

2. Consider alternative approach:
   - Use cursor position instead of viewport top_byte
   - Or track scroll position directly in scroll sync group

3. Run full test suite to verify no regressions
