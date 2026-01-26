# Unified UI Layout and Event Handling Framework

This document proposes a minimalistic, incremental solution to unify UI patterns across the editor core and plugins by **extracting and generalizing the existing controls library**.

## Existing Foundation

The codebase already has well-designed patterns that should become the unified framework:

### Controls Library (`src/view/controls/`)

A mature control library with consistent patterns:

```
controls/
├── mod.rs              # Exports, FocusState, ControlColors
├── button/             # ButtonState, ButtonLayout, ButtonColors, render_button
├── dropdown/           # DropdownState, DropdownLayout, DropdownColors, render_dropdown
├── number_input/       # NumberInputState, NumberInputLayout, ...
├── text_input/         # TextInputState, TextInputLayout, ...
├── text_list/          # TextListState, TextListLayout, ...
├── map_input/          # MapState, MapLayout, ...
├── toggle/             # ToggleState, ToggleLayout, ...
└── keybinding_list/    # KeybindingListState, KeybindingListLayout, ...
```

**Each control follows the pattern:**
- `*State` - Data and behavior (selection, open/closed, value)
- `*Colors` - Theme-derived colors with `from_theme()`
- `*Layout` - Hit testing info with `contains()`, `is_*()`, `*_at()` methods
- `render_*()` - Returns `*Layout` for hit testing

**Shared types:**
- `FocusState` enum: `Normal`, `Focused`, `Hovered`, `Disabled`
- `ControlColors` - Common color scheme

### Settings UI (`src/view/settings/`)

Uses the controls library with additional orchestration:

- `SettingsLayout` - Aggregates all control layouts + button areas
- `ControlLayoutInfo` - Maps control types to their specific layout structs
- `SettingsHit` enum - Unified hit test result type
- `ScrollablePanel` - Generic scrollable container with `ScrollItem` trait

### ScrollablePanel (`src/view/ui/scroll_panel.rs`)

Reusable scrolling abstraction:
- `ScrollItem` trait with `height()` and `focus_regions()`
- `FocusRegion` for sub-focus within items
- `ScrollState` for scroll position
- Returns `ScrollablePanelLayout<L>` with item layouts

## Problem Summary

| Component | Uses Controls? | Uses Generalized Layout? | Problem |
|-----------|---------------|--------------------------|---------|
| Settings UI | Yes | No - has its own `SettingsLayout`, `ControlLayoutInfo` | Good patterns but not reusable |
| Menu | No | No - manual hit calc | Duplicates width calculation |
| Tabs | No | Partial - returns Vec | Different return type |
| Status Bar | No | Partial | Ad-hoc layout |
| Plugin (pkg.ts) | No | No | Manual byte offset calculation |

**The goal:**
1. Extract patterns from Settings UI into a shared library
2. Settings UI, Menu, Tabs all use the shared library
3. Expose the same patterns to plugins via TypeScript bindings

## Design Principles

1. **Extract from Settings, then Settings uses the extraction** - One unified library
2. **All components use the same abstractions** - No special cases
3. **Incremental migration** - Extract → Settings adopts → Menu/Tabs adopt → plugins get bindings
4. **Minimal new abstractions** - Generalize existing `*Layout` pattern

## Architecture Overview

Extract patterns from Settings into shared modules, then all components use them:

```
┌─────────────────────────────────────────────────────────────────────┐
│                         src/view/                                    │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    ui/ (SHARED LIBRARY)                       │   │
│  │                                                               │   │
│  │  layout.rs (NEW)     - HitTest trait, CompositeLayout<H>     │   │
│  │  focus.rs (NEW)      - FocusManager<T>                       │   │
│  │  scroll_panel.rs     - ScrollItem, ScrollablePanel (EXISTS)  │   │
│  │  scrollbar.rs        - ScrollbarState (EXISTS)               │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                              │                                       │
│            ┌─────────────────┼─────────────────┐                    │
│            ▼                 ▼                 ▼                    │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    controls/ (EXISTING)                       │   │
│  │  Button, Dropdown, Toggle, TextInput, NumberInput, ...        │   │
│  │  Pattern: *State + *Layout + *Colors + render_*()             │   │
│  │  ADD: impl HitTest for *Layout                                │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                              │                                       │
│       ┌──────────────────────┼──────────────────────┐               │
│       ▼                      ▼                      ▼               │
│  ┌──────────┐  ┌──────────────────┐  ┌──────────────────────────┐  │
│  │ settings │  │ menu.rs, tabs.rs │  │ Plugin TypeScript API    │  │
│  │ (USES)   │  │ (MIGRATE TO)     │  │ (NEW BINDINGS)           │  │
│  └──────────┘  └──────────────────┘  └──────────────────────────┘  │
│                                                                      │
│  DELETE: settings/layout.rs (moves to ui/layout.rs)                 │
└─────────────────────────────────────────────────────────────────────┘
```

**Key change:** Settings UI is no longer special - it uses the same shared library as everyone else.

## Part 1: Generalizing Existing Patterns (Rust)

### 1.1 The `*Layout` Pattern (Already Exists)

Each control already returns a layout struct. Example from `DropdownLayout`:

```rust
// EXISTING in controls/dropdown/mod.rs
pub struct DropdownLayout {
    pub button_area: Rect,
    pub option_areas: Vec<Rect>,
    pub full_area: Rect,
    pub scroll_offset: usize,
}

impl DropdownLayout {
    pub fn is_button(&self, x: u16, y: u16) -> bool { ... }
    pub fn option_at(&self, x: u16, y: u16) -> Option<usize> { ... }
    pub fn contains(&self, x: u16, y: u16) -> bool { ... }
}
```

**This pattern is already correct. No changes needed to individual controls.**

### 1.2 Unified HitMap Trait (New)

Create a trait that all layout structs can implement, allowing uniform hit testing:

```rust
// crates/fresh-editor/src/view/ui/layout.rs (NEW FILE)

use ratatui::layout::Rect;

/// Common hit testing interface for all layout structs
///
/// This trait allows EditorState to perform hit testing uniformly
/// across different UI components (Menu, Tabs, Settings controls).
pub trait HitTest {
    /// The hit result type for this component
    type Hit;

    /// Test if point (x,y) hits any element, returning the hit info
    fn hit_test(&self, x: u16, y: u16) -> Option<Self::Hit>;

    /// Check if point is within the component's bounds at all
    fn contains(&self, x: u16, y: u16) -> bool;
}

/// Helper to check point in rect (used by all Layout impls)
#[inline]
pub fn point_in_rect(rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x && x < rect.x + rect.width &&
    y >= rect.y && y < rect.y + rect.height
}
```

### 1.3 Implement HitTest for Existing Layouts

Add trait implementations to existing layout structs:

```rust
// In controls/dropdown/mod.rs - add impl
impl HitTest for DropdownLayout {
    type Hit = DropdownHit;

    fn hit_test(&self, x: u16, y: u16) -> Option<DropdownHit> {
        if self.is_button(x, y) {
            return Some(DropdownHit::Button);
        }
        if let Some(idx) = self.option_at(x, y) {
            return Some(DropdownHit::Option(idx));
        }
        if self.contains(x, y) {
            return Some(DropdownHit::Background);
        }
        None
    }

    fn contains(&self, x: u16, y: u16) -> bool {
        point_in_rect(self.full_area, x, y)
    }
}

pub enum DropdownHit {
    Button,
    Option(usize),
    Background,
}
```

### 1.4 Composite Layout (Generalize SettingsLayout)

`SettingsLayout` already aggregates multiple control layouts. Generalize this pattern:

```rust
// crates/fresh-editor/src/view/ui/layout.rs

/// A layout that aggregates multiple component layouts
///
/// This is a generalization of SettingsLayout - any complex UI
/// can use this to track hit areas for its children.
#[derive(Debug, Default)]
pub struct CompositeLayout<H> {
    /// Bounds of the entire component
    pub bounds: Rect,
    /// Named regions with their hit result
    regions: Vec<(Rect, H)>,
}

impl<H: Clone> CompositeLayout<H> {
    pub fn new(bounds: Rect) -> Self {
        Self { bounds, regions: Vec::new() }
    }

    /// Add a hit region
    pub fn add(&mut self, area: Rect, hit: H) {
        self.regions.push((area, hit));
    }

    /// Hit test - returns first matching region (last added wins for overlaps)
    pub fn hit_test(&self, x: u16, y: u16) -> Option<H> {
        // Check in reverse order (last added = on top)
        for (rect, hit) in self.regions.iter().rev() {
            if point_in_rect(*rect, x, y) {
                return Some(hit.clone());
            }
        }
        None
    }
}
```

### 1.5 FocusManager (Extract from Settings)

Settings already has focus panel navigation. Extract it:

```rust
// crates/fresh-editor/src/view/ui/focus.rs (NEW FILE)

/// Manages Tab-order focus navigation
///
/// Extracted from SettingsState focus panel logic.
/// Can be used by Menu, plugin UIs, or any component with Tab navigation.
#[derive(Debug, Clone)]
pub struct FocusManager<T: Copy + Eq> {
    /// Ordered list of focusable elements
    elements: Vec<T>,
    /// Current focus index
    current: usize,
}

impl<T: Copy + Eq> FocusManager<T> {
    pub fn new(elements: Vec<T>) -> Self {
        Self { elements, current: 0 }
    }

    pub fn current(&self) -> Option<T> {
        self.elements.get(self.current).copied()
    }

    pub fn focus_next(&mut self) -> Option<T> {
        if self.elements.is_empty() { return None; }
        self.current = (self.current + 1) % self.elements.len();
        self.current()
    }

    pub fn focus_prev(&mut self) -> Option<T> {
        if self.elements.is_empty() { return None; }
        self.current = (self.current + self.elements.len() - 1) % self.elements.len();
        self.current()
    }

    pub fn focus(&mut self, element: T) -> bool {
        if let Some(idx) = self.elements.iter().position(|&e| e == element) {
            self.current = idx;
            true
        } else {
            false
        }
    }

    pub fn is_focused(&self, element: T) -> bool {
        self.current().map_or(false, |e| e == element)
    }
}

// SettingsState.focus_panel can become FocusManager<FocusPanel>
// MenuState can use FocusManager<usize> for menu items
// pkg.ts can use FocusManager equivalent in TypeScript
```

## Part 2: Plugin UI Framework (TypeScript)

The key insight: plugins like pkg.ts need the **same controls** that Settings UI uses, but accessible from TypeScript. Rather than build a parallel widget system, we expose the existing control patterns.

### 2.1 What pkg.ts Currently Does (The Problem)

```typescript
// Current pkg.ts - 400+ lines of manual UI construction
function buildListViewEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Manual button rendering with bracket logic
  const focused = isButtonFocused("filter", i);
  const leftBracket = focused ? "[" : " ";
  const rightBracket = focused ? "]" : " ";
  entries.push({ text: `${leftBracket} ${label} ${rightBracket}`, properties: {...} });

  // Manual list rendering with selection prefix
  const prefix = isSelected ? "▸" : " ";
  const line = `${prefix} ${name.padEnd(18)} ${ver.padEnd(7)} ${status}`;

  // Manual byte offset calculation for overlays
  const len = utf8ByteLength(entry.text);
  editor.addOverlay(bufferId, "pkg", byteOffset, byteOffset + len, options);
  byteOffset += len;
  // ... 400+ more lines
}
```

### 2.2 The Solution: TypeScript Mirrors of Rust Controls

Provide TypeScript classes that mirror the Rust control patterns:

```typescript
// crates/fresh-editor/plugins/lib/controls.ts

import { FocusState } from "./focus";

/**
 * Button control - mirrors controls/button in Rust
 *
 * Pattern: State + render() -> Layout with hit areas
 */
export class ButtonControl {
  constructor(
    public label: string,
    public focus: FocusState = FocusState.Normal
  ) {}

  /** Render button text with focus indicators */
  render(): { text: string; styles: StyleRange[] } {
    const focused = this.focus === FocusState.Focused;
    const left = focused ? "[" : " ";
    const right = focused ? "]" : " ";
    const text = `${left} ${this.label} ${right}`;

    return {
      text,
      styles: focused ? [{ start: 0, end: text.length, bg: "syntax.keyword" }] : [],
    };
  }
}

/**
 * Selectable list control - mirrors Settings item list behavior
 */
export class ListControl<T> {
  public selectedIndex: number = 0;
  public scrollOffset: number = 0;

  constructor(
    public items: T[],
    public renderItem: (item: T, selected: boolean) => string,
    public maxVisible: number = 10
  ) {}

  selectNext(): void {
    if (this.items.length === 0) return;
    this.selectedIndex = Math.min(this.selectedIndex + 1, this.items.length - 1);
    this.ensureVisible();
  }

  selectPrev(): void {
    if (this.items.length === 0) return;
    this.selectedIndex = Math.max(this.selectedIndex - 1, 0);
    this.ensureVisible();
  }

  private ensureVisible(): void {
    if (this.selectedIndex < this.scrollOffset) {
      this.scrollOffset = this.selectedIndex;
    } else if (this.selectedIndex >= this.scrollOffset + this.maxVisible) {
      this.scrollOffset = this.selectedIndex - this.maxVisible + 1;
    }
  }

  render(): { text: string; styles: StyleRange[]; selectedLine: number } {
    const lines: string[] = [];
    const styles: StyleRange[] = [];
    let charOffset = 0;

    const visibleItems = this.items.slice(
      this.scrollOffset,
      this.scrollOffset + this.maxVisible
    );

    for (let i = 0; i < visibleItems.length; i++) {
      const actualIndex = this.scrollOffset + i;
      const selected = actualIndex === this.selectedIndex;
      const prefix = selected ? "▸ " : "  ";
      const line = prefix + this.renderItem(visibleItems[i], selected);
      lines.push(line);

      if (selected) {
        styles.push({
          start: charOffset,
          end: charOffset + line.length,
          fg: "ui.menu_active_fg",
          bg: "ui.menu_active_bg",
        });
      }
      charOffset += line.length + 1; // +1 for \n
    }

    return {
      text: lines.join("\n"),
      styles,
      selectedLine: this.selectedIndex - this.scrollOffset
    };
  }
}

/**
 * Focus manager - mirrors FocusManager<T> from Rust
 */
export class FocusManager<T> {
  private currentIndex: number = 0;

  constructor(public elements: T[]) {}

  current(): T | undefined {
    return this.elements[this.currentIndex];
  }

  focusNext(): T | undefined {
    if (this.elements.length === 0) return undefined;
    this.currentIndex = (this.currentIndex + 1) % this.elements.length;
    return this.current();
  }

  focusPrev(): T | undefined {
    if (this.elements.length === 0) return undefined;
    this.currentIndex = (this.currentIndex + this.elements.length - 1) % this.elements.length;
    return this.current();
  }

  isFocused(element: T): boolean {
    return this.elements[this.currentIndex] === element;
  }

  focus(element: T): boolean {
    const idx = this.elements.indexOf(element);
    if (idx >= 0) {
      this.currentIndex = idx;
      return true;
    }
    return false;
  }
}

export enum FocusState {
  Normal = "normal",
  Focused = "focused",
  Hovered = "hovered",
  Disabled = "disabled",
}

export interface StyleRange {
  start: number;  // Character offset
  end: number;
  fg?: string | [number, number, number];
  bg?: string | [number, number, number];
  bold?: boolean;
}
```

### 2.3 Virtual Buffer Builder

A builder that handles byte offset calculation automatically:

```typescript
// crates/fresh-editor/plugins/lib/vbuffer.ts

/**
 * Builds virtual buffer content with automatic style offset tracking.
 *
 * Eliminates manual utf8ByteLength() calls and offset tracking.
 */
export class VirtualBufferBuilder {
  private entries: Array<{ text: string; styles: StyleRange[] }> = [];
  private namespace: string;

  constructor(
    private bufferId: number,
    namespace: string = "ui"
  ) {
    this.namespace = namespace;
  }

  /** Add text with optional styles */
  text(content: string, styles?: StyleRange[]): this {
    this.entries.push({ text: content, styles: styles ?? [] });
    return this;
  }

  /** Add a newline */
  newline(): this {
    return this.text("\n");
  }

  /** Add a horizontal separator */
  separator(width: number, char: string = "─"): this {
    return this.text(char.repeat(width) + "\n");
  }

  /** Add a control's output */
  control(output: { text: string; styles: StyleRange[] }): this {
    this.entries.push(output);
    return this;
  }

  /** Add a row of controls with spacing */
  row(...controls: Array<{ text: string; styles: StyleRange[] }>): this {
    let combined = "";
    const allStyles: StyleRange[] = [];
    let offset = 0;

    for (const ctrl of controls) {
      // Shift styles by current offset
      for (const style of ctrl.styles) {
        allStyles.push({
          ...style,
          start: style.start + offset,
          end: style.end + offset,
        });
      }
      combined += ctrl.text;
      offset += ctrl.text.length;
    }

    this.entries.push({ text: combined, styles: allStyles });
    return this;
  }

  /** Build and apply to the virtual buffer */
  build(): void {
    // Combine all text
    let fullText = "";
    const allStyles: StyleRange[] = [];
    let charOffset = 0;

    for (const entry of this.entries) {
      for (const style of entry.styles) {
        allStyles.push({
          ...style,
          start: style.start + charOffset,
          end: style.end + charOffset,
        });
      }
      fullText += entry.text;
      charOffset += entry.text.length;
    }

    // Convert to TextPropertyEntry format
    const textEntries = [{ text: fullText, properties: {} }];
    editor.setVirtualBufferContent(this.bufferId, textEntries);

    // Apply overlays (handles UTF-8 byte conversion internally)
    editor.clearNamespace(this.bufferId, this.namespace);
    for (const style of allStyles) {
      const byteStart = this.charToByteOffset(fullText, style.start);
      const byteEnd = this.charToByteOffset(fullText, style.end);
      editor.addOverlay(this.bufferId, this.namespace, byteStart, byteEnd, {
        fg: style.fg,
        bg: style.bg,
        bold: style.bold,
      });
    }
  }

  private charToByteOffset(text: string, charOffset: number): number {
    // Use TextEncoder for accurate UTF-8 byte counting
    const encoder = new TextEncoder();
    const prefix = text.slice(0, charOffset);
    return encoder.encode(prefix).length;
  }
}
```

### 2.4 Simplified pkg.ts Example

With the new library, pkg.ts becomes:

```typescript
// Simplified pkg.ts using controls library
import { ButtonControl, ListControl, FocusManager, FocusState } from "./lib/controls";
import { VirtualBufferBuilder } from "./lib/vbuffer";

// State uses FocusManager instead of manual focus tracking
const focusManager = new FocusManager([
  "search", "filter-all", "filter-installed", "filter-plugins", "filter-themes",
  "sync", "list", "action-0", "action-1"
]);

const packageList = new ListControl(
  getFilteredItems(),
  (item, selected) => formatPackageRow(item),
  20 // max visible
);

function updatePkgManagerView(): void {
  const builder = new VirtualBufferBuilder(pkgState.bufferId!, "pkg");

  // Header
  builder.text(" Packages\n", [{ start: 0, end: 10, fg: "syntax.keyword" }]);
  builder.newline();

  // Filter buttons row
  builder.row(
    new ButtonControl("All", focusManager.isFocused("filter-all") ? FocusState.Focused : FocusState.Normal).render(),
    { text: " ", styles: [] },
    new ButtonControl("Installed", focusManager.isFocused("filter-installed") ? FocusState.Focused : FocusState.Normal).render(),
    { text: " ", styles: [] },
    new ButtonControl("Sync", focusManager.isFocused("sync") ? FocusState.Focused : FocusState.Normal).render(),
  );
  builder.newline();

  // Separator
  builder.separator(80);

  // Package list
  builder.control(packageList.render());

  // Build and apply
  builder.build();
}

// Navigation now uses shared controls
globalThis.pkg_nav_down = () => {
  packageList.selectNext();
  updatePkgManagerView();
};

globalThis.pkg_next_button = () => {
  focusManager.focusNext();
  updatePkgManagerView();
};
```

**Reduction: ~400 lines of manual UI → ~50 lines using controls.**

## Part 3: Migration Strategy

### Phase 1: Foundation (Non-breaking)

1. **Create `ui/layout.rs`** with `HitTest` trait and `point_in_rect()` helper
2. **Create `ui/focus.rs`** with generic `FocusManager<T>`
3. **Create `plugins/lib/controls.ts`** with TypeScript control classes
4. **Create `plugins/lib/vbuffer.ts`** with `VirtualBufferBuilder`

No changes to existing code - all additive.

### Phase 2: pkg.ts Migration (Proof of Concept)

Refactor pkg.ts to use the new TypeScript controls:

```
Before: 2300 lines, ~400 for UI rendering
After:  1500 lines, ~50 for UI rendering (using controls library)
```

This validates the approach without touching Rust code.

### Phase 3: Menu Migration

Current problem in `menu.rs`:

```rust
// MenuState has manual hit calculation that duplicates rendering logic
impl MenuState {
    pub fn get_menu_at_position(&self, menus: &[Menu], x: u16) -> Option<usize> {
        let mut pos = 2;
        for (i, menu) in menus.iter().enumerate() {
            let label_width = str_width(&menu.label) + 4; // Duplicates render logic!
            // ...
        }
    }
}
```

Migration:

1. **Add `MenuLayout` struct** (similar to `DropdownLayout`):
```rust
pub struct MenuLayout {
    pub menu_areas: Vec<Rect>,     // Top-level menu labels
    pub item_areas: Vec<Rect>,     // Dropdown items
    pub full_area: Rect,
}

impl HitTest for MenuLayout {
    type Hit = MenuHit;
    fn hit_test(&self, x: u16, y: u16) -> Option<MenuHit> { ... }
}

pub enum MenuHit {
    MenuLabel(usize),
    MenuItem(usize),
    Background,
}
```

2. **`render_menu_bar()` returns `MenuLayout`** instead of nothing
3. **Delete `get_menu_at_position()`** - use `MenuLayout::hit_test()` instead
4. **MenuState uses `FocusManager<usize>`** for item navigation

### Phase 4: Tabs Migration

Current: `render_for_split()` returns `Vec<(BufferId, u16, u16, u16)>` - ad-hoc tuple.

Migration:

1. **Add `TabLayout` struct**:
```rust
pub struct TabLayout {
    pub tabs: Vec<TabHitArea>,
    pub full_area: Rect,
}

pub struct TabHitArea {
    pub buffer_id: BufferId,
    pub label_area: Rect,
    pub close_button_area: Rect,
}

impl HitTest for TabLayout {
    type Hit = TabHit;
    // ...
}

pub enum TabHit {
    Tab(BufferId),
    CloseButton(BufferId),
    Background,
}
```

2. **`TabsRenderer::render_for_split()` returns `TabLayout`**
3. Mouse handling uses `TabLayout::hit_test()`

### Phase 5: Settings UI Migration

Settings UI should use the generalized library so there's one unified system:

1. **Replace `SettingsLayout` with `CompositeLayout<SettingsHit>`**:
```rust
// Current: custom SettingsLayout struct
pub struct SettingsLayout {
    pub modal_area: Rect,
    pub categories: Vec<(usize, Rect)>,
    pub items: Vec<ItemLayout>,
    pub save_button: Option<Rect>,
    // ... many fields
}

// After: use CompositeLayout from ui/layout.rs
pub type SettingsLayoutMap = CompositeLayout<SettingsHit>;

// render_settings returns the shared type
pub fn render_settings(...) -> SettingsLayoutMap {
    let mut layout = CompositeLayout::new(modal_area);
    layout.add(save_button_rect, SettingsHit::SaveButton);
    layout.add(category_rect, SettingsHit::Category(idx));
    // ...
    layout
}
```

2. **Replace `ControlLayoutInfo` enum** - controls already return their own Layout types:
```rust
// Current: ControlLayoutInfo duplicates what *Layout already provides
pub enum ControlLayoutInfo {
    Toggle(Rect),
    Dropdown { button_area: Rect, option_areas: Vec<Rect>, ... },
    // ...
}

// After: store the actual control layout, use HitTest trait
pub struct ItemLayout {
    pub index: usize,
    pub path: String,
    pub area: Rect,
    // Store actual layout type, not a copy of its fields
    pub control_layout: ControlLayout,
}

pub enum ControlLayout {
    Toggle(ToggleLayout),
    Dropdown(DropdownLayout),
    Number(NumberInputLayout),
    // ...
}
```

3. **`SettingsState` uses `FocusManager<FocusPanel>`**:
```rust
// Current: manual focus cycling
pub focus_panel: FocusPanel,

pub fn next_panel(&mut self) {
    self.focus_panel = match self.focus_panel {
        FocusPanel::Categories => FocusPanel::Settings,
        FocusPanel::Settings => FocusPanel::Footer,
        FocusPanel::Footer => FocusPanel::Categories,
    };
}

// After: use FocusManager
pub focus: FocusManager<FocusPanel>,

pub fn next_panel(&mut self) {
    self.focus.focus_next();
}
```

4. **Delete `settings/layout.rs`** - functionality moves to `ui/layout.rs`

### Phase 6: Split Rendering Decomposition (Future)

`split_rendering.rs` is 5.5k lines mixing many concerns. Future work could:

1. Extract `BufferRenderer` - single buffer rendering
2. Extract `SplitLayout` - pane sizing and composition
3. Extract `InlineDiff` - character-level diff logic
4. Use `ScrollablePanel` for buffer content (it's already used in Settings)

This is the largest change and should happen after the control/layout unification proves successful.

## Part 4: Concrete Examples

### 4.1 How Menu Will Use the Patterns

**Before** (current `menu.rs`):

```rust
// MenuState has hit testing that duplicates render logic
impl MenuState {
    pub fn get_menu_at_position(&self, menus: &[Menu], x: u16) -> Option<usize> {
        let mut pos = 2;
        for (i, menu) in menus.iter().enumerate() {
            let label_width = str_width(&menu.label) + 4; // DUPLICATES RENDERING!
            if x >= pos && x < pos + label_width as u16 {
                return Some(i);
            }
            pos += label_width as u16;
        }
        None
    }
}
```

**After** (using `MenuLayout`):

```rust
// menu.rs - add MenuLayout following the controls pattern

/// Layout info for menu bar - mirrors DropdownLayout pattern
pub struct MenuLayout {
    pub menu_labels: Vec<(usize, Rect)>,  // (menu_index, area)
    pub dropdown_items: Vec<(usize, Rect)>, // (item_index, area)
    pub full_area: Rect,
}

impl MenuLayout {
    pub fn menu_at(&self, x: u16, y: u16) -> Option<usize> {
        for (idx, area) in &self.menu_labels {
            if point_in_rect(*area, x, y) {
                return Some(*idx);
            }
        }
        None
    }

    pub fn item_at(&self, x: u16, y: u16) -> Option<usize> {
        for (idx, area) in &self.dropdown_items {
            if point_in_rect(*area, x, y) {
                return Some(*idx);
            }
        }
        None
    }
}

// MenuRenderer now returns MenuLayout
pub fn render_menu_bar(
    frame: &mut Frame,
    area: Rect,
    menus: &[Menu],
    state: &MenuState,
    theme: &Theme,
) -> MenuLayout {
    let mut layout = MenuLayout { menu_labels: vec![], dropdown_items: vec![], full_area: area };

    let mut x = area.x + 2;
    for (i, menu) in menus.iter().enumerate() {
        let width = str_width(&menu.label) as u16 + 4;
        let label_area = Rect::new(x, area.y, width, 1);

        // Render the label...
        render_menu_label(frame, label_area, &menu.label, state.active_menu == Some(i), theme);

        // Record in layout (single source of truth!)
        layout.menu_labels.push((i, label_area));
        x += width;
    }

    layout
}

// Mouse handling uses layout, no duplicate calculation
fn handle_menu_mouse(layout: &MenuLayout, x: u16, y: u16) -> Option<MenuAction> {
    if let Some(menu_idx) = layout.menu_at(x, y) {
        return Some(MenuAction::OpenMenu(menu_idx));
    }
    if let Some(item_idx) = layout.item_at(x, y) {
        return Some(MenuAction::SelectItem(item_idx));
    }
    None
}

// DELETE get_menu_at_position - no longer needed!
```

### 4.2 How pkg.ts Simplifies

**Before** (current - manual everything):

```typescript
// 260+ lines in buildListViewEntries()
function buildListViewEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Manual button with bracket logic (repeated 6x for each button)
  const syncFocused = isButtonFocused("sync");
  const syncLeft = syncFocused ? "[" : " ";
  const syncRight = syncFocused ? "]" : " ";
  entries.push({ text: `${syncLeft} Sync ${syncRight}`, properties: { type: "sync-btn", focused: syncFocused } });

  // Manual list with selection indicator
  for (let i = 0; i < items.length; i++) {
    const isSelected = i === pkgState.selectedIndex;
    const prefix = isSelected ? "▸ " : "  ";
    // ... padding, formatting ...
  }

  return entries;
}

// Separate function to apply overlays with manual byte offset tracking
function applyPkgManagerHighlighting(): void {
  let byteOffset = 0;
  for (const entry of entries) {
    const len = utf8ByteLength(entry.text);  // Manual byte counting!
    // ... determine style based on entry type ...
    editor.addOverlay(pkgState.bufferId, "pkg", byteOffset, byteOffset + len, options);
    byteOffset += len;  // Manual offset tracking!
  }
}
```

**After** (using controls library):

```typescript
import { ButtonControl, ListControl, FocusState } from "./lib/controls";
import { VirtualBufferBuilder } from "./lib/vbuffer";

function updatePkgManagerView(): void {
  const builder = new VirtualBufferBuilder(pkgState.bufferId!, "pkg");

  // Buttons use ButtonControl (handles brackets, focus styling)
  const filterButtons = ["All", "Installed", "Plugins", "Themes"].map((label, i) =>
    new ButtonControl(label, pkgState.focus.type === "filter" && pkgState.focus.index === i
      ? FocusState.Focused : FocusState.Normal)
  );

  builder
    .text(" Packages\n", [{ start: 0, end: 10, fg: "syntax.keyword" }])
    .newline()
    .row(...filterButtons.map(b => b.render()), { text: "  ", styles: [] },
         new ButtonControl("Sync", pkgState.focus.type === "sync" ? FocusState.Focused : FocusState.Normal).render())
    .newline()
    .separator(80)
    .control(packageList.render())  // ListControl handles selection, scrolling
    .build();  // Handles byte offset conversion automatically
}
```

**Line count comparison:**
- `buildListViewEntries()`: 260 lines → ~20 lines
- `applyPkgManagerHighlighting()`: 130 lines → 0 lines (handled by builder)
- Manual focus tracking: 100 lines → uses `FocusManager`

### 4.3 Settings UI Migration to Shared Library

Settings UI has good patterns that become the shared library. Then Settings itself uses that library:

| Pattern | Current (Settings-specific) | After (Shared Library) |
|---------|----------------------------|------------------------|
| Hit Testing | `SettingsLayout` in `settings/layout.rs` | `CompositeLayout<H>` in `ui/layout.rs` |
| Focus Nav | Manual `focus_panel` cycling | `FocusManager<FocusPanel>` from `ui/focus.rs` |
| Control Layout | `ControlLayoutInfo` enum duplicates fields | Store actual `*Layout` types, use `HitTest` trait |
| Control State | `DropdownState`, etc. in `controls/` | Unchanged - already good |
| Scrolling | `ScrollablePanel` | Unchanged - already reusable |
| Colors | `*Colors::from_theme()` | Unchanged - already good |

**Before:** Settings has its own layout system that others can't use.
**After:** Everyone (Settings, Menu, Tabs, plugins) uses the same `ui/layout.rs` and `ui/focus.rs`.

## API Summary

### Existing Rust API (Unchanged)

These already exist and remain the foundation:

```rust
// controls/mod.rs - EXISTING, UNCHANGED
pub enum FocusState { Normal, Focused, Hovered, Disabled }
pub struct ControlColors { bg, fg, border, accent, disabled }

// controls/dropdown/mod.rs - EXISTING (example of the pattern)
pub struct DropdownState { selected, options, values, open, focus, ... }
pub struct DropdownLayout { button_area: Rect, option_areas: Vec<Rect>, ... }
pub struct DropdownColors { label, selected, border, ... }
pub fn render_dropdown(frame, area, state, colors) -> DropdownLayout;

// Same pattern for: Toggle, NumberInput, TextInput, TextList, MapInput, Button

// ui/scroll_panel.rs - EXISTING, UNCHANGED
pub trait ScrollItem { fn height(&self) -> u16; fn focus_regions(&self) -> Vec<FocusRegion>; }
pub struct ScrollState { offset, viewport, content_height }
pub struct ScrollablePanel { scroll: ScrollState }
```

### New Shared Library (Extracted/Generalized)

```rust
// ui/layout.rs - NEW (generalized from settings/layout.rs)
pub trait HitTest {
    type Hit;
    fn hit_test(&self, x: u16, y: u16) -> Option<Self::Hit>;
    fn contains(&self, x: u16, y: u16) -> bool;
}

/// Generic composite layout - replaces SettingsLayout
pub struct CompositeLayout<H> {
    pub bounds: Rect,
    regions: Vec<(Rect, H)>,
}
impl<H: Clone> CompositeLayout<H> {
    fn new(bounds: Rect) -> Self;
    fn add(&mut self, area: Rect, hit: H);
    fn hit_test(&self, x: u16, y: u16) -> Option<H>;
}

pub fn point_in_rect(rect: Rect, x: u16, y: u16) -> bool;

// ui/focus.rs - NEW (extracted from SettingsState)
pub struct FocusManager<T: Copy + Eq> {
    elements: Vec<T>,
    current: usize,
}
impl<T> FocusManager<T> {
    fn new(elements: Vec<T>) -> Self;
    fn current(&self) -> Option<T>;
    fn focus_next(&mut self) -> Option<T>;
    fn focus_prev(&mut self) -> Option<T>;
    fn focus(&mut self, element: T) -> bool;
    fn is_focused(&self, element: T) -> bool;
}
```

### Components Using the Shared Library

```rust
// settings/render.rs - MODIFIED to use shared library
pub fn render_settings(...) -> CompositeLayout<SettingsHit> { ... }

// settings/state.rs - MODIFIED to use FocusManager
pub struct SettingsState {
    pub focus: FocusManager<FocusPanel>,  // was: focus_panel: FocusPanel
    // ...
}

// ui/menu.rs - MODIFIED (add MenuLayout using HitTest)
pub struct MenuLayout { menu_labels: Vec<(usize, Rect)>, ... }
impl HitTest for MenuLayout { type Hit = MenuHit; ... }

// ui/tabs.rs - MODIFIED (add TabLayout using HitTest)
pub struct TabLayout { tabs: Vec<TabHitArea>, ... }
impl HitTest for TabLayout { type Hit = TabHit; ... }

// controls/dropdown/mod.rs - ADD HitTest impl
impl HitTest for DropdownLayout { type Hit = DropdownHit; ... }
```

### New TypeScript Plugin API

```typescript
// plugins/lib/controls.ts - NEW (mirrors Rust controls)
export enum FocusState { Normal, Focused, Hovered, Disabled }
export class ButtonControl { label, focus; render(): { text, styles } }
export class ListControl<T> { items, selectedIndex; selectNext(); selectPrev(); render() }
export class FocusManager<T> { elements; current(); focusNext(); focusPrev(); isFocused(t) }

// plugins/lib/vbuffer.ts - NEW (eliminates manual byte offset tracking)
export class VirtualBufferBuilder {
    constructor(bufferId: number, namespace?: string);
    text(content: string, styles?: StyleRange[]): this;
    newline(): this;
    separator(width: number): this;
    control(output: { text, styles }): this;
    row(...controls): this;
    build(): void;  // Handles UTF-8 conversion automatically
}

export interface StyleRange {
    start: number;  // Character offset (not bytes!)
    end: number;
    fg?: string | [number, number, number];
    bg?: string | [number, number, number];
    bold?: boolean;
}
```

## What This Plan Does NOT Include

The goal is minimal changes that leverage existing code:

1. **No new component trait** - The `render() → *Layout` pattern is sufficient
2. **No virtual DOM** - Full rebuild on each render is fine for TUI
3. **No flexbox/grid** - Manual Rect calculation works; `ratatui::Layout` handles splitting
4. **No reactive state bindings** - Explicit state management (like existing `*State` structs) is clearer

## Success Metrics

1. **One unified layout library**: Settings, Menu, Tabs all use `ui/layout.rs` and `ui/focus.rs`
2. **Delete duplicated code**: `settings/layout.rs` deleted, `ControlLayoutInfo` simplified
3. **pkg.ts UI code reduction**: 400 lines → 50 lines (88% reduction)
4. **No behavior changes**: All migrations are refactors, not functional changes

## Files to Create

| File | Purpose | Lines (est.) |
|------|---------|--------------|
| `src/view/ui/layout.rs` | `HitTest` trait, `CompositeLayout<H>`, `point_in_rect()` | ~80 |
| `src/view/ui/focus.rs` | `FocusManager<T>` | ~60 |
| `plugins/lib/controls.ts` | `ButtonControl`, `ListControl`, `FocusManager` | ~200 |
| `plugins/lib/vbuffer.ts` | `VirtualBufferBuilder` | ~100 |

**Total new code: ~440 lines** (mostly TypeScript for plugins)

## Files to Modify

| File | Change | Complexity |
|------|--------|------------|
| `src/view/settings/state.rs` | Use `FocusManager<FocusPanel>` | Low |
| `src/view/settings/render.rs` | Return `CompositeLayout<SettingsHit>`, simplify `ControlLayoutInfo` | Medium |
| `src/view/ui/menu.rs` | Add `MenuLayout`, delete `get_menu_at_position()` | Medium |
| `src/view/ui/tabs.rs` | Return `TabLayout` instead of tuple Vec | Low |
| `src/view/controls/*/mod.rs` | Add `impl HitTest for *Layout` | Low |
| `plugins/pkg.ts` | Use controls library | Medium (but simplifies) |

## Files to Delete

| File | Why |
|------|-----|
| `src/view/settings/layout.rs` | Functionality moves to `ui/layout.rs` |

## Files NOT Changed

| File | Why |
|------|-----|
| `src/view/controls/` (state/render) | Already well-designed; just add `HitTest` impls |
| `src/view/ui/scroll_panel.rs` | Already reusable |
| `src/input/handler.rs` | Input handling is orthogonal to layout |

## Open Questions

1. **Should controls implement `HitTest`?** Optional - the existing `*Layout::is_*()` methods work fine
2. **Should `FocusManager` replace `FocusPanel`?** Could be a type alias: `type FocusPanelManager = FocusManager<FocusPanel>`
3. **Plugin mouse support?** Currently pkg.ts is keyboard-only; adding mouse would need more work

## Implementation Plan: Extract First, Then Adopt

The key principle: **extract existing code into shared modules first**, then have the original code use the extraction. This validates the extraction works before anyone else adopts it.

### Step 1: Extract `point_in_rect()` to `ui/layout.rs`

**What:** Move the `contains()` helper from `settings/layout.rs:257` to a new shared module.

**Source code (settings/layout.rs:257-259):**
```rust
fn contains(&self, rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x && x < rect.x + rect.width && y >= rect.y && y < rect.y + rect.height
}
```

**Target:** Create `src/view/ui/layout.rs` with:
```rust
//! Layout utilities for hit testing
use ratatui::layout::Rect;

/// Check if a point is within a rectangle
#[inline]
pub fn point_in_rect(rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x && x < rect.x + rect.width && y >= rect.y && y < rect.y + rect.height
}
```

**Then:** Update `settings/layout.rs` to use it:
```rust
use crate::view::ui::layout::point_in_rect;

// Change: self.contains(*area, x, y)
// To:     point_in_rect(*area, x, y)
```

**Validation:** All settings hit testing continues to work identically.

---

### Step 2: Add `HitTest` trait to `ui/layout.rs`

**What:** Define a trait that layout structs can implement. This is new code, but it's a small addition to the file created in Step 1.

```rust
/// Trait for layout structs that support hit testing
pub trait HitTest {
    /// The hit result type (e.g., DropdownHit, MenuHit)
    type Hit;

    /// Test if point hits any element, returning hit info
    fn hit_test(&self, x: u16, y: u16) -> Option<Self::Hit>;

    /// Check if point is within component bounds
    fn contains(&self, x: u16, y: u16) -> bool;
}
```

**No migration yet** - this is just adding the trait definition. Existing code continues to work.

---

### Step 3: Extract `FocusManager` to `ui/focus.rs`

**What:** Generalize the focus cycling pattern from `settings/state.rs`.

**Source pattern (settings/state.rs:380-386):**
```rust
pub fn toggle_focus(&mut self) {
    self.focus_panel = match self.focus_panel {
        FocusPanel::Categories => FocusPanel::Settings,
        FocusPanel::Settings => FocusPanel::Footer,
        FocusPanel::Footer => FocusPanel::Categories,
    };
    // ... side effects follow
}
```

**Target:** Create `src/view/ui/focus.rs` with generic cycling:
```rust
//! Focus management utilities

/// Manages focus cycling through a list of elements
#[derive(Debug, Clone)]
pub struct FocusManager<T> {
    elements: Vec<T>,
    current: usize,
}

impl<T: Copy + Eq> FocusManager<T> {
    pub fn new(elements: Vec<T>) -> Self {
        Self { elements, current: 0 }
    }

    pub fn current(&self) -> Option<T> {
        self.elements.get(self.current).copied()
    }

    pub fn focus_next(&mut self) -> Option<T> {
        if self.elements.is_empty() { return None; }
        self.current = (self.current + 1) % self.elements.len();
        self.current()
    }

    pub fn focus_prev(&mut self) -> Option<T> {
        if self.elements.is_empty() { return None; }
        self.current = (self.current + self.elements.len() - 1) % self.elements.len();
        self.current()
    }

    pub fn set(&mut self, element: T) -> bool {
        if let Some(idx) = self.elements.iter().position(|&e| e == element) {
            self.current = idx;
            true
        } else {
            false
        }
    }

    pub fn is_current(&self, element: T) -> bool {
        self.current() == Some(element)
    }
}
```

**Note:** Settings keeps its side effects (`update_control_focus`, `ensure_visible`, etc.) - only the pure cycling logic is extracted.

---

### Step 4: Update `ui/mod.rs` exports

```rust
// Add to WASM-compatible modules section:
pub mod focus;
pub mod layout;

// Add to re-exports:
pub use focus::FocusManager;
pub use layout::{point_in_rect, HitTest};
```

---

### Step 5: Migrate `settings/layout.rs` to use `point_in_rect`

**What:** Replace all `self.contains(rect, x, y)` calls with `point_in_rect(rect, x, y)`.

**Changes:**
1. Add import: `use crate::view::ui::layout::point_in_rect;`
2. Delete the `contains` method from `SettingsLayout`
3. Replace ~15 calls to `self.contains(...)` with `point_in_rect(...)`

**Validation:** Run settings tests, verify mouse clicks still work.

---

### Step 6: Migrate `settings/state.rs` to use `FocusManager`

**What:** Replace manual `focus_panel` field with `FocusManager<FocusPanel>`.

**Before:**
```rust
pub struct SettingsState {
    pub focus_panel: FocusPanel,
    // ...
}

pub fn toggle_focus(&mut self) {
    let old_panel = self.focus_panel;
    self.focus_panel = match self.focus_panel {
        FocusPanel::Categories => FocusPanel::Settings,
        // ...
    };
    // side effects...
}
```

**After:**
```rust
use crate::view::ui::FocusManager;

pub struct SettingsState {
    pub focus: FocusManager<FocusPanel>,
    // ...
}

pub fn toggle_focus(&mut self) {
    let old_panel = self.focus.current().unwrap_or_default();
    self.focus.focus_next();
    // side effects remain the same...
}
```

**Note:** This requires updating all reads of `self.focus_panel` to `self.focus.current().unwrap_or(FocusPanel::Categories)`. Consider adding a helper method.

---

### Step 7: Add `MenuLayout` to `menu.rs`

**What:** Make `render_menu_bar()` return layout info instead of computing positions twice.

**Current problem (menu.rs):**
```rust
// Rendering calculates positions:
let mut x = area.x;
for menu in menus {
    let width = str_width(&menu.label) + 2;
    // render at x...
    x += width + 1;
}

// Hit testing recalculates the same positions:
pub fn get_menu_at_position(&self, menus: &[Menu], x: u16) -> Option<usize> {
    let mut current_x = 0;
    for (idx, menu) in menus.iter().enumerate() {
        let label_width = str_width(&menu.label) + 2;  // DUPLICATE!
        // ...
    }
}
```

**Solution:** Add `MenuLayout` struct, return from render:
```rust
pub struct MenuLayout {
    pub menu_areas: Vec<(usize, Rect)>,
    pub item_areas: Vec<(usize, Rect)>,
}

impl MenuLayout {
    pub fn menu_at(&self, x: u16, y: u16) -> Option<usize> {
        for (idx, area) in &self.menu_areas {
            if point_in_rect(*area, x, y) {
                return Some(*idx);
            }
        }
        None
    }
}

// render_menu_bar now returns MenuLayout
pub fn render_menu_bar(...) -> MenuLayout {
    let mut layout = MenuLayout::default();
    let mut x = area.x;
    for (idx, menu) in menus.iter().enumerate() {
        let width = str_width(&menu.label) as u16 + 2;
        let menu_area = Rect::new(x, area.y, width, 1);
        layout.menu_areas.push((idx, menu_area));
        // render...
        x += width + 1;
    }
    layout
}
```

**Then:** Delete `get_menu_at_position()` and `get_item_at_position()` - use `MenuLayout` instead.

---

### Step 8: Add `TabLayout` to `tabs.rs`

**What:** Replace `Vec<(BufferId, u16, u16, u16)>` with a proper struct.

**Current (tabs.rs):**
```rust
pub fn render_for_split(...) -> Vec<(BufferId, u16, u16, u16)>
// Returns: (buffer_id, tab_start, tab_end, close_start)
```

**After:**
```rust
pub struct TabHitArea {
    pub buffer_id: BufferId,
    pub tab_area: Rect,
    pub close_area: Rect,
}

pub struct TabLayout {
    pub tabs: Vec<TabHitArea>,
    pub scroll_left_area: Option<Rect>,
    pub scroll_right_area: Option<Rect>,
}

pub fn render_for_split(...) -> TabLayout
```

---

### Sequence Summary

| Step | Action | Type | Risk |
|------|--------|------|------|
| 1 | Create `ui/layout.rs` with `point_in_rect()` | Extract | None |
| 2 | Add `HitTest` trait | New code | None |
| 3 | Create `ui/focus.rs` with `FocusManager<T>` | Extract pattern | None |
| 4 | Update `ui/mod.rs` exports | Wire up | None |
| 5 | Migrate settings/layout.rs to use `point_in_rect` | Adopt | Low |
| 6 | Migrate settings/state.rs to use `FocusManager` | Adopt | Medium |
| 7 | Add `MenuLayout`, delete duplicate hit methods | Refactor | Medium |
| 8 | Add `TabLayout`, replace tuple Vec | Refactor | Low |

**Steps 1-4** are pure additions with zero risk.
**Steps 5-6** validate the extractions work by having Settings use them.
**Steps 7-8** are independent refactors that benefit from the shared utilities.

---

### Future Steps (After Core Migration)

Once the Rust patterns are validated:

1. **Add `impl HitTest` for control layouts** (optional, additive)
2. **Create TypeScript controls library** for plugins
3. **Migrate pkg.ts** to use the TypeScript controls
4. **Consider `CompositeLayout<H>`** if Settings/Menu/Tabs want to share more structure

---

## References

### Code to Extract From
- `src/view/settings/layout.rs:257-259` - `contains()` → `point_in_rect()`
- `src/view/settings/state.rs:380-386` - Focus cycling → `FocusManager<T>`

### Code Patterns to Follow
- `src/view/controls/dropdown/mod.rs` - Reference implementation of State/Layout/Colors pattern
- `src/view/ui/scroll_panel.rs` - Reusable scrolling abstraction (already shared)

### Documentation
- `INPUT_LAYOUT_RENDERING_SUMMARY.md` - Current architecture documentation
- `TUI Architecture Deep Dive.pdf` - Analysis of TUI framework patterns
- `TUI Frameworks vs. Web and Game Engines.pdf` - Comparison with other UI systems
