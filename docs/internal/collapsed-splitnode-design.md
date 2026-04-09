# Buffer Groups via Collapsed SplitNode Variant

Supersedes `buffer-groups-design.md` (which was implemented but produced
the wrong UX — multiple side-by-side splits instead of a single tab).

## Motivation

Three plugins (`pkg.ts`, `theme_editor.ts`, `audit_mode.ts`) render
side-by-side panels inside a single virtual buffer. This has known
problems: no per-panel scrollbar, inconsistent scroll behavior, no
mouse scroll support, ~200 lines of boilerplate per plugin.

Earlier attempts:

1. **Scroll regions** (implemented then removed): metadata-based
   per-region scrollbar rendering. Too much plugin-side state
   management, fragile bugs around initial creation, viewport
   mismatches, drag compounding.
2. **Buffer groups as split subtrees** (current implementation):
   each panel is a real split/buffer. Creates the correct rendering
   but the wrong UX — opening a group wraps the entire split tree,
   so the user sees multiple side-by-side splits instead of a single
   switchable tab.

The target UX: a buffer group appears as **one tab** in the existing
tab bar. When that tab is active, the split's content area shows the
nested multi-panel layout. When another tab is active, the split shows
that buffer normally. Tabs remain per-split (no global tab bar
refactor needed) because the root split's tab bar naturally acts as
the top-level tab bar.

## Core Insight

The current editor's root split IS the top-level tab bar. It's just
that the root usually has a single leaf, making the tab bar look
"per-split". Buffer groups should add their representative to the root
split's (or current split's) tab list, not create new splits
alongside it.

## Design: `SplitNode::Collapsed`

Add a new variant to `SplitNode`:

```rust
enum SplitNode {
    Leaf {
        buffer_id: BufferId,
        split_id: LeafId,
    },
    Split {
        direction: SplitDirection,
        first: Box<Self>,
        second: Box<Self>,
        ratio: f32,
        split_id: ContainerId,
        fixed_first: Option<u16>,
        fixed_second: Option<u16>,
    },
    /// A collapsed subtree that appears as a single tab entry in its
    /// parent split's tab bar. When active, the subtree is expanded
    /// and rendered inside the parent split's content area.
    Collapsed {
        /// Unique ID, used in tab targets
        split_id: LeafId,
        /// Display name shown in the tab bar
        name: String,
        /// The nested layout to render when this node is active
        layout: Box<SplitNode>,
        /// The initially active leaf within the layout (for focus)
        active_inner_leaf: LeafId,
    },
}
```

A `Collapsed` node behaves like a `Leaf` from the outside — it has a
`split_id` (acting as a `LeafId`), appears in tab lists, can be
activated. Internally it wraps a subtree that's rendered when active.

## Tab Targets

The current tab list (`SplitViewState.open_buffers: Vec<BufferId>`)
becomes:

```rust
pub enum TabTarget {
    Buffer(BufferId),
    Group(LeafId),  // points to a Collapsed node's split_id
}

pub open_buffers: Vec<TabTarget>,
```

A tab can point to either a regular buffer or a Collapsed node. The
tab bar rendering iterates `open_buffers` and draws each:

- `TabTarget::Buffer(id)` → look up `BufferMetadata[id].display_name`
- `TabTarget::Group(leaf_id)` → look up the Collapsed node in the
  split tree, use its `name`

## Rendering

The rendering code already walks the split tree recursively
(`get_leaves_with_rects`). Add a case for `Collapsed`:

```rust
match node {
    Leaf { buffer_id, split_id } => {
        vec![(*split_id, *buffer_id, rect)]
    }
    Split { first, second, direction, ratio, fixed_first, fixed_second, .. } => {
        let (r1, r2) = split_rect_ext(rect, ..);
        first.get_leaves_with_rects(r1)
            .chain(second.get_leaves_with_rects(r2))
            .collect()
    }
    Collapsed { split_id, layout, .. } => {
        // If this Collapsed node is the active target in its parent's
        // tab list, recurse into the layout. Otherwise, it's not
        // rendered — the parent split's active buffer is something
        // else.
        //
        // BUT: get_leaves_with_rects is called from the root down
        // and doesn't know about tab state. So we always recurse
        // into the layout. The tab state is applied elsewhere when
        // deciding what to render INTO each leaf.
        layout.get_leaves_with_rects(rect)
    }
}
```

Actually, the cleanest model: `Collapsed` ALWAYS recurses. The only
question is what rect it gets.

**Key decision:** the rendering walks the tree top-down. At each
Leaf or Collapsed node, it needs to determine "what to render in this
rect". For a Leaf that matches the active tab in the parent split,
render the buffer. For a Collapsed node that matches the active tab,
recurse into the layout and render each inner leaf.

This means the tab resolution happens PER-SPLIT when computing what to
render. A split (leaf or collapsed) chooses its rendering based on
which tab in its parent is active.

### Simpler model: flatten during layout

Instead of handling Collapsed during rendering, handle it during
layout:

```rust
fn compute_visible_layout(&self, rect: Rect, active_targets: &HashMap<SplitId, TabTarget>) -> Vec<(LeafId, BufferId, Rect)>
```

Walk the tree. For each Split node, recurse. For each Leaf, check if
it's the active target in its parent's tab list. For each Collapsed
node, check if it's the active target; if yes, recurse into its
layout; if no, skip.

This approach:
- Keeps the split tree structure clean
- Tab state is external (HashMap of split_id → active target)
- Rendering is driven by the flattened list of visible leaves
- Works with nested Collapsed (a Collapsed inside a Collapsed)

## Plugin API (Unchanged)

Plugins still call `createBufferGroup`, `setPanelContent`,
`closeBufferGroup`, `focusBufferGroupPanel`. The semantics change:

- `createBufferGroup`:
  1. Creates panel buffers (regular virtual buffers, hidden from tabs)
  2. Builds a `SplitNode` subtree using the layout
  3. Wraps it in a `Collapsed` node with the group name
  4. Adds the Collapsed node to the current split's `open_buffers`
     list as a `TabTarget::Group(collapsed_node.split_id)`
  5. Sets the current split's active tab to that Collapsed node
- `setPanelContent`: writes content to a specific panel buffer
- `closeBufferGroup`: removes the Collapsed node from the tab list,
  closes the nested panel buffers
- `focusBufferGroupPanel`: sets the focused leaf within the
  Collapsed subtree

## Hidden Panel Buffers

Panel buffers (tree, picker, diff, etc.) are NOT in any split's
`open_buffers` list. They're only accessed via the Collapsed node's
layout. `BufferMetadata.hidden_from_tabs = true` is set on each,
which also hides them from the buffer list (`#buffer`).

When the user closes the group, the panel buffers are closed along
with the Collapsed node.

## What About the Representative Buffer's Own Tab Bar?

A Collapsed node has no tab bar of its own. Its subtree's leaves
don't show tab bars either — they inherit `suppress_chrome = true`.
Only the parent split (the one holding the Collapsed node in its tab
list) has a visible tab bar.

If the user splits inside a collapsed group (e.g., presses `Ctrl+\`
while the theme tree panel is focused), the split happens within the
Collapsed's layout subtree. The new split inherits `suppress_chrome`.
The group's outer tab bar is unaffected.

## Mouse and Keyboard Routing

The existing per-split routing works because each leaf inside a
Collapsed node is a real leaf with its own rect. Mouse clicks on a
panel hit the panel's leaf. Keyboard focus goes to the focused leaf
inside the active Collapsed node.

The only new concept: tab bar clicks on a Collapsed node's tab entry
should activate that node. The existing tab click handling calls
"set active buffer" — extend it to "set active target" (buffer or
group).

## Split Operations Inside a Group

If the user presses a split command (`Ctrl+\`) while a panel is
active, the current behavior is to split the active leaf. Inside a
Collapsed node, this would split one of the group's panels. Options:

1. **Allow it**: the group's layout grows a new split. The new leaf
   shows another copy of the panel's buffer (or the user picks a
   buffer to show). The group's tab bar stays at one entry.
2. **Block it**: splitting within a Collapsed node is disallowed —
   show a status message "cannot split inside a buffer group".
3. **Lift it**: splitting a panel creates a new top-level tab
   alongside the group, with the panel's buffer.

Option 1 is the most consistent with the split tree model. Option 2
is safer for plugins that don't expect their layout to change.
**Recommendation: Option 2** — groups have fixed layouts declared by
the plugin. The plugin controls the structure; the user controls
content within panels.

## Lifecycle

Opening a buffer group:
1. Plugin calls `createBufferGroup(name, mode, layout_json)`
2. Core parses layout, creates panel buffers (hidden, virtual)
3. Core builds `Collapsed { name, layout, ... }` node
4. Core allocates a `LeafId` for the Collapsed node
5. Core adds `TabTarget::Group(leaf_id)` to the current split's
   `open_buffers`
6. Core sets the current split's active tab to that target
7. Core returns `{ groupId, panels: { name → bufferId } }` to plugin

Writing content:
1. Plugin calls `setPanelContent(groupId, panelName, entries)`
2. Core looks up the panel's buffer ID via the group metadata
3. Core writes content to that buffer (via `setVirtualBufferContent`)

Closing:
1. Plugin calls `closeBufferGroup(groupId)`
2. Core removes the `TabTarget::Group(leaf_id)` from any split's
   `open_buffers`
3. Core removes the Collapsed node from the split tree
4. Core closes all panel buffers
5. Core activates whatever tab the user previously had active (or
   the next tab in the list)

Switching to a group tab:
1. User clicks the group's tab or presses a tab-switch key
2. Core sets the split's `active_tab` to `TabTarget::Group(leaf_id)`
3. Rendering walks the split tree, finds the Collapsed node, recurses
   into its layout, and renders each panel buffer in its computed rect

Switching away from a group tab:
1. User clicks a different tab or presses tab-switch
2. Core sets the split's `active_tab` to that target
3. Rendering walks the tree, the Collapsed node is no longer the
   active target, so it's skipped during rect computation
4. The parent split's content area renders the newly-active target

## Nested Groups

A Collapsed node can contain a Split that contains another Collapsed
node. The inner Collapsed would have its own tab bar — no wait, it
wouldn't, because `suppress_chrome = true` is inherited through the
subtree. Inner Collapsed nodes would need a tab bar to be useful.

**Decision:** disallow nested groups for v1. A Collapsed node's layout
can only contain `Leaf` and `Split` nodes, not other `Collapsed`
nodes. Future work: allow nested groups if use cases emerge.

## Composability Check

The Collapsed variant is a natural extension of SplitNode:

- `Leaf`, `Split`, and `Collapsed` are all nodes in the same tree
- Layout computation recurses through all three uniformly
- Tab bars still work per-split; the root split's tab bar is the
  top-level tab bar
- No new concept of "global tab bar"
- No parallel rendering pipeline
- Existing split operations (resize, focus, close) work on regular
  splits; only split-inside-Collapsed is restricted

## Implementation Plan

All changes land together.

### 1. Model changes

- Add `SplitNode::Collapsed { split_id, name, layout, active_inner_leaf }`
- Add `TabTarget` enum: `Buffer(BufferId) | Group(LeafId)`
- Change `SplitViewState.open_buffers` from `Vec<BufferId>` to
  `Vec<TabTarget>`
- Change `SplitViewState.active_buffer: BufferId` to
  `active_target: TabTarget`

### 2. Tree traversal

- `get_leaves_with_rects` recurses through Collapsed nodes, using the
  active target map to decide what to render
- `find` / `find_mut` handle Collapsed nodes
- `parent_container_of` handles Collapsed nodes
- Workspace serialization handles Collapsed nodes (persist layout
  structure; panel content is rebuilt by the plugin on load)

### 3. Tab bar rendering

- `TabsRenderer::render_for_split` iterates `Vec<TabTarget>`
- For `Buffer(id)` → use `BufferMetadata.display_name`
- For `Group(leaf_id)` → look up Collapsed node by leaf_id, use its
  `name`
- Tab clicks dispatch to `set_active_target(split_id, target)`

### 4. Buffer group creation

- `create_buffer_group` builds the layout subtree (existing code)
- Wraps it in a Collapsed node (new)
- Adds `TabTarget::Group(collapsed_leaf_id)` to the current split's
  `open_buffers`
- Sets that as the active target

### 5. Rendering

- When a split's active target is `Group(leaf_id)`, find the
  Collapsed node and render its layout
- Each panel buffer is rendered as a normal leaf (gets all buffer
  features)
- No special rendering code for panels — they're real buffers

### 6. Plugin migration

- No changes needed. Plugins already use `createBufferGroup` etc.
- The only difference is the resulting split tree structure.

### 7. E2E test

- `test_theme_editor_tab_bar_persists`:
  1. Initial state: `[No Name]` visible
  2. Open theme editor → tab bar shows `[No Name]` and `*Theme Editor*`
  3. Close theme editor → tab bar shows just `[No Name]`
- `test_switch_between_file_and_group_tabs`:
  1. Open a file → file tab visible
  2. Open theme editor → both tabs visible
  3. Click file tab → split shows the file
  4. Click theme editor tab → split shows the group layout
  5. Close theme editor → only file tab visible

## Relationship to Existing Code

| Existing piece | Change needed |
|----------------|---------------|
| `SplitNode` enum | Add `Collapsed` variant |
| `SplitViewState.open_buffers` | Change type from `Vec<BufferId>` to `Vec<TabTarget>` |
| `SplitViewState.active_buffer` | Change to `active_target: TabTarget` |
| `get_leaves_with_rects` | Add Collapsed case; take active target map |
| `TabsRenderer::render_for_split` | Handle both TabTarget variants |
| Tab click handling | Dispatch by target type |
| `create_buffer_group` | Build Collapsed node, add to current split's tabs |
| `close_buffer_group` | Remove Collapsed node and close panel buffers |
| Buffer group plugin API | Unchanged |
| Individual plugins | Unchanged |

The existing scroll region removal, buffer group infrastructure,
fixed-height splits, chrome suppression, and plugin migrations all
remain. This design replaces only the "wrapping outer split" approach
with a `Collapsed` node in the existing tab list.
