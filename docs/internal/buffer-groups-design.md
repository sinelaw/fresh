# Buffer Groups Design

Supersedes the scroll regions approach in `panel-scroll-regions-design.md`.

## Motivation

The same problems described in the scroll regions design doc remain:
plugins rendering side-by-side panels in a single virtual buffer have
no per-panel scrollbar, inconsistent scroll behavior, no mouse scroll
support, and ~200 lines of boilerplate per plugin.

The scroll regions approach (implemented and tested) solved these
problems but introduced a new class of bugs, all stemming from the
plugin being responsible for things the core should own:

1. **Scroll regions not set on initial buffer creation** — plugins
   create buffers via `createVirtualBuffer()` which doesn't accept
   scroll regions. Regions only appear after the first `updateDisplay()`
   call, requiring an extra call after creation.

2. **Viewport dimension mismatch** — plugins cache viewport height at
   initialization with a default value. If they don't re-query on
   every render, content height doesn't match the actual viewport,
   producing tilde lines or clipped content.

3. **Mouse scroll suppression scope** — had to expand from "within
   region rect" to "entire buffer" because plugin chrome (headers,
   footers) wasn't covered by any scroll region.

4. **Drag delta compounding** — plugins applied `currentWidth + delta`
   instead of `startWidth + delta`, requiring each plugin to track
   drag start state.

5. **Content not adapting to resize** — plugins hardcoded field name
   truncation widths that didn't update when the panel was resized.

6. **Round-trip latency** — every mouse scroll event goes
   core → plugin → re-render → core, rather than the core scrolling
   the viewport directly.

All of these bugs exist because the scroll regions design asks plugins
to be rendering engines. Plugins pre-slice content to viewport height,
track scroll offsets, handle viewport math, pad/truncate content, and
re-render on every scroll event. The core provides only scrollbar
overlays and event routing.

## Key Insight

The editor already has everything needed for per-panel scrolling:
`Buffer` (piece table content storage), `Viewport` (scroll position
tracking), `SplitViewState` (per-buffer cursor, viewport, folds),
`SplitNode` (recursive layout with ratios), scrollbar rendering,
mouse handling, and split separator drag-to-resize.

The only reason plugins don't use splits is that each split creates
a separate tab entry and separate mode-line chrome. A plugin with
two panels would show two tabs and two status bars — wasteful and
visually broken.

The solution: group multiple splits into one logical entity that
appears as one tab and one buffer to the user.

## Design: Buffer Groups

A `BufferGroup` is a set of splits and buffers that appear as a
single tab in the tab bar and a single entry in the buffer list.
Internally, each panel is a real split with a real buffer, reusing
all existing editor infrastructure.

### Layout

```
BufferGroup "Theme Editor"
├── header (Buffer, fixed 2 rows): "Theme Editor: name" + separator
├── body (Split, horizontal)
│   ├── left (Buffer, ratio 0.35): theme tree - all lines
│   └── right (Buffer, ratio 0.65): color picker - all lines
└── footer (Buffer, fixed 1 row): hints
```

Each leaf is either:
- A **regular buffer** — full piece-table-backed text, scrollable,
  optionally editable, with its own viewport and scrollbar.
- A **composite buffer** — existing multi-pane aligned diff view,
  with synced scrolling across panes.
- A **fixed-height buffer** — for headers, footers, toolbars.
  Not scrollable, no scrollbar.

### Plugin API

```typescript
// Create a buffer group with a layout tree
const group = await editor.createBufferGroup({
  name: "*Theme Editor*",
  mode: "theme-editor",
  layout: {
    direction: "v",       // vertical stack
    children: [
      { id: "header", type: "fixed", height: 2 },
      {
        direction: "h",   // horizontal split for body
        children: [
          { id: "tree", type: "scrollable", width: { ratio: 0.35 } },
          { id: "picker", type: "scrollable", width: { flex: true } },
        ],
      },
      { id: "footer", type: "fixed", height: 1 },
    ],
  },
});

// Write content to each panel — no slicing, no scroll tracking
editor.setPanelContent(group.id, "tree", treeEntries);     // all 80 lines
editor.setPanelContent(group.id, "picker", pickerEntries); // all 30 lines
editor.setPanelContent(group.id, "header", headerEntries);
editor.setPanelContent(group.id, "footer", footerEntries);

// To update one panel without touching others:
editor.setPanelContent(group.id, "tree", newTreeEntries);

// Focus a specific panel (for keyboard routing):
editor.focusPanel(group.id, "tree");

// Read scroll position if needed:
const pos = editor.getPanelScrollPosition(group.id, "tree");
```

The plugin writes **all content** to each panel. The core handles:
- Viewport windowing (only renders visible lines)
- Scrollbar rendering (automatic, based on content vs viewport)
- Mouse scroll (direct viewport update, no plugin round-trip)
- Keyboard scroll (arrow keys, PageUp/PageDown in focused panel)
- Split separator drag-to-resize (existing split ratio system)
- Resize on terminal resize (existing split layout recomputation)
- Cursor management (if panels are editable)

### Use Cases

#### Current: Two-panel plugin (theme editor, pkg manager, review diff)

```
BufferGroup "*Packages*"
├── header (fixed, 4 rows): title + search + filters + separator
├── body (h-split)
│   ├── list (scrollable, ratio 0.4): all package rows
│   └── details (scrollable, flex): selected package info
└── footer (fixed, 2 rows): separator + help text
```

#### 3-way merge tool

```
BufferGroup "Merge: file.rs"
├── header (fixed, 1 row): "Merging: file.rs  [3 conflicts]"
├── diff (composite buffer, synced): BASE | OURS | THEIRS
├── result (scrollable, editable): merged result
└── footer (fixed, 1 row): "[n] next  [a] accept ours  [t] theirs"
```

The composite buffer handles hunk-aligned synced scrolling across
the three diff panes. The result buffer is a real editable buffer
with undo/redo, syntax highlighting, and all editor features. The
header and footer are fixed chrome.

#### Debugger

```
BufferGroup "*Debug*"
├── header (fixed, 1 row): toolbar
├── body (h-split)
│   ├── left (v-split)
│   │   ├── variables (scrollable): watch panel
│   │   └── callstack (scrollable): call stack
│   └── source (scrollable): source code at breakpoint
└── footer (fixed, 1 row): step/continue/break buttons
```

### What the Core Reuses (Zero New Code)

| Existing Component | Role in Buffer Groups |
|---|---|
| `Buffer` (piece table) | Content storage per panel |
| `Viewport` | Per-panel scroll position, `top_byte`, `left_column` |
| `SplitViewState` | Per-panel cursor, viewport, folds |
| `SplitNode::Split` | Recursive binary layout tree with ratios |
| `render_scrollbar()` | Per-panel scrollbar (already per-split) |
| `handle_mouse_scroll()` | Per-panel mouse wheel (already per-split) |
| Scrollbar click/drag | Already implemented per-split |
| Split separator drag | Already handles ratio-based resize |
| `handle_editor_click()` | Text selection, cursor positioning |
| Syntax highlighting | Per-buffer, if language is set |
| Overlay system | Per-buffer visual decorations |
| Text properties | Per-buffer metadata |

### What's New (~300-400 LoC)

1. **`BufferGroup` struct** — groups N buffer IDs + a layout tree
   under one name. Stored in a `HashMap<BufferGroupId, BufferGroup>`
   on the `Editor`.

2. **Tab bar rendering** — when a buffer belongs to a group, show
   one tab entry for the group instead of N entries. Closing the tab
   closes all buffers in the group.

3. **Buffer list filtering** — `#buffer` search and `Ctrl+Tab`
   switcher show the group name, not individual panel buffer names.

4. **Fixed-height splits** — a split variant that doesn't scroll
   and has a fixed row count. Used for headers and footers. This
   could be a flag on `SplitViewState` or a new `SplitNode` variant.

5. **`createBufferGroup` plugin command** — creates the group,
   creates the splits and buffers, returns the group ID and panel
   buffer IDs.

6. **`setPanelContent` plugin command** — sets content on a panel
   buffer by group ID + panel name. Same as `setVirtualBufferContent`
   but addressed by panel name.

7. **Suppressing split chrome** — no mode-line or tab-bar per split
   when splits are part of a group. Only the group's header/footer
   and the outer tab entry.

### What Becomes Unnecessary

The following code from the scroll regions implementation can be
removed once buffer groups are adopted:

- `ScrollRegion` struct and all scroll region metadata
- `BorderRegion` struct and all border region metadata
- `scroll_region_mouse.rs` (hit testing, drag, hover)
- `ScrollRegionHitArea` and `PanelBorderHitArea` in `CachedLayout`
- `MouseState` fields for scroll region and border dragging
- `HoverTarget` variants for scroll regions and borders
- Mouse scroll suppression logic in `input.rs`
- Global scrollbar hiding when scroll regions present
- `on_region_scroll`, `on_border_drag`, `on_border_drag_end` hooks
- Per-plugin scroll state management (ScrollState, offsets)
- Per-plugin viewport dimension tracking
- Per-plugin content slicing and padding
- Per-plugin mouse scroll handlers
- Per-plugin border drag handlers with start-width tracking

### Key Design Decisions

**Each panel is a real buffer, not a virtual content area.** This
means panels can be editable (for merge tools), have syntax
highlighting, support undo/redo, and use all existing buffer
features. Virtual buffers with text properties still work for
panels that need structured metadata.

**Layout uses the existing split tree.** The `SplitNode::Split`
binary tree with direction and ratio already computes nested
layouts. No new layout engine needed. The only addition is
fixed-height nodes for headers/footers.

**Composite buffers as leaves.** A buffer group leaf can be a
composite buffer (the existing aligned diff view). This means the
3-way merge case composes: composite for synced diff viewing,
regular buffer for editing, fixed buffers for chrome — all within
one tab.

**No per-panel mode-line.** Splits within a group suppress their
individual mode-line and tab-bar chrome. The group has a single
outer tab entry and optional header/footer panels for plugin
chrome.

**Focus cycling within the group.** Tab key (or plugin-defined key)
cycles focus between scrollable panels in the group. The focused
panel receives keyboard scroll events. This replaces the plugin's
manual `focusPanel` state tracking.

### Relationship to Scroll Regions

The scroll regions implementation serves as a working prototype
and can remain as a transitional mechanism. Plugins can be migrated
to buffer groups incrementally:

1. Build the buffer group infrastructure.
2. Migrate one plugin (pkg.ts, simplest) to validate the API.
3. Migrate remaining plugins.
4. Remove scroll region infrastructure.

The scroll regions code doesn't need to be deleted before buffer
groups are implemented — both can coexist during migration.

### Implementation Plan

All changes land together. Commits should be structured per
CONTRIBUTING.md: separate bug fixes from new functionality, each
commit must pass `cargo check --all-targets` and `cargo fmt`.

#### 1. BufferGroup Model (Rust, ~100 LoC)

- `BufferGroup` struct: `id`, `name`, `mode`, `panel_buffers: HashMap<String, BufferId>`,
  `layout: GroupLayout`, `header_buffer: Option<BufferId>`,
  `footer_buffer: Option<BufferId>`.
- `GroupLayout` enum: `Leaf(String)`, `Split { direction, ratio, first, second }`.
- Store `buffer_groups: HashMap<BufferGroupId, BufferGroup>` on `Editor`.
- Reverse index: `buffer_to_group: HashMap<BufferId, BufferGroupId>` for
  tab bar and buffer list lookups.

#### 2. Split Tree Integration (Rust, ~100 LoC)

- When creating a buffer group, construct the `SplitNode` tree from
  the `GroupLayout`, creating real splits and buffers.
- Mark splits as "grouped" so they suppress chrome.
- Fixed-height splits: add a `fixed_height: Option<u16>` to
  `SplitViewState` that prevents scrolling and forces the split
  to render at exactly that height.

#### 3. Tab Bar and Buffer List (Rust, ~50 LoC)

- Tab bar: skip individual buffer tabs for grouped buffers; show
  one tab for the group.
- Buffer list: filter out grouped buffers; show group name instead.
- Close tab: close all buffers in the group.

#### 4. Plugin API (Rust bridge, ~100 LoC)

- `createBufferGroup` command: creates group, splits, buffers.
- `setPanelContent` command: sets content on a panel buffer.
- `focusPanel` command: focus a specific panel.
- `closeBufferGroup` command: close the group.

#### 5. Migrate Plugins

- **pkg.ts**: Two scrollable panels + header + footer. Simplest.
- **theme_editor.ts**: Two scrollable panels + header + footer.
  Right panel has structured content (color picker) that may
  benefit from being a real buffer with overlays.
- **audit_mode.ts** (magit view): Two scrollable panels + toolbar.
  The drill-down diff view already uses composite buffers — it
  would become a composite buffer leaf in the group.

#### 6. E2E Tests

- Create a buffer group with two scrollable panels.
- Write content exceeding viewport to one panel.
- Verify scrollbar renders on that panel.
- Mouse scroll on one panel doesn't affect the other.
- Verify split separator drag resizes panels.
- Verify closing tab closes all group buffers.
- Verify buffer list shows group name, not individual panels.

Run `crates/fresh-editor/plugins/check-types.sh` to verify all
plugin TypeScript after migration. Use semantic waiting in tests.
Test isolation: internal clipboard, per-test temp dirs.
