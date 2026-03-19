# Design: Ctrl+Right-Click Character Theme Info Popup

## Overview

When the user Ctrl+Right-Clicks any rendered character on screen, a popup appears showing which theme key(s) contribute to that character's appearance (foreground, background, syntax highlight category). The popup includes a clickable button to open the Theme Editor plugin and jump directly to that key.

---

## Architecture

### 1. Theme Key Resolution: Screen Position → Theme Key(s)

Every pixel on screen gets its color from a specific theme field. We need a function that, given a screen `(col, row)`, returns the **theme key path(s)** responsible for that character's styling.

**The resolution works by determining which UI region the click falls in:**

| Screen Region | How to Detect | Theme Key(s) |
|---|---|---|
| **Editor content** (syntax-highlighted text) | `split_areas` hit test → `ViewLineMapping` → byte position → `HighlightCategory` | `syntax.{keyword,string,comment,...}` for fg, `editor.bg` for bg |
| **Editor content** (unhighlighted text) | Same, but no highlight span at byte position | `editor.fg`, `editor.bg` |
| **Line numbers** | Column falls within gutter width of a split | `editor.line_number_fg`, `editor.line_number_bg` |
| **Selection** | Byte position falls within selection range | `editor.selection_bg` (overrides bg) |
| **Current line** | Row matches cursor line | `editor.current_line_bg` (overrides bg) |
| **Tab bar (active)** | `tab_layouts` hit test → active tab | `ui.tab_active_fg`, `ui.tab_active_bg` |
| **Tab bar (inactive)** | `tab_layouts` hit test → inactive tab | `ui.tab_inactive_fg`, `ui.tab_inactive_bg` |
| **Menu bar** | `menu_layout` hit test | `ui.menu_fg`, `ui.menu_bg` |
| **Status bar** | `status_bar_area` hit test | `ui.status_bar_fg`, `ui.status_bar_bg` |
| **Popup** | `popup_areas` hit test | `ui.popup_text_fg`, `ui.popup_bg` |
| **Search match** | Byte position in search match ranges | `search.bg`, `search.fg` |
| **Diagnostic underline** | Byte position in diagnostic range | `diagnostic.{error,warning,info,hint}_fg` |
| **Split separator** | `separator_areas` hit test | `ui.split_separator_fg` |
| **Scrollbar** | Scrollbar area hit test | `ui.scrollbar_track_fg` or `ui.scrollbar_thumb_fg` |
| **File explorer** | `file_explorer_area` hit test | `editor.fg`, `editor.bg` |

#### New Type: `ThemeKeyInfo`

```rust
/// Information about which theme key(s) style a specific screen position
#[derive(Debug, Clone)]
pub struct ThemeKeyInfo {
    /// The foreground theme key path (e.g., "syntax.keyword", "editor.fg")
    pub fg_key: Option<String>,
    /// The background theme key path (e.g., "editor.bg", "editor.selection_bg")
    pub bg_key: Option<String>,
    /// Human-readable description of the UI region
    pub region: String,
    /// The actual foreground color value currently applied
    pub fg_color: Option<Color>,
    /// The actual background color value currently applied
    pub bg_color: Option<Color>,
    /// For syntax highlights: the HighlightCategory name
    pub syntax_category: Option<String>,
}
```

#### New Function: `resolve_theme_key_at`

Add to `Editor` (in a new `theme_inspect.rs` module):

```rust
fn resolve_theme_key_at(&self, col: u16, row: u16) -> Option<ThemeKeyInfo> {
    let theme = &self.theme;

    // 1. Check status bar area
    if let Some((bar_row, bar_x, bar_width)) = self.cached_layout.status_bar_area {
        if row == bar_row && col >= bar_x && col < bar_x + bar_width {
            return Some(ThemeKeyInfo {
                fg_key: Some("ui.status_bar_fg".into()),
                bg_key: Some("ui.status_bar_bg".into()),
                region: "Status Bar".into(),
                fg_color: Some(theme.status_bar_fg),
                bg_color: Some(theme.status_bar_bg),
                syntax_category: None,
            });
        }
    }

    // 2. Check menu bar
    if let Some(ref menu_layout) = self.cached_layout.menu_layout {
        if point_in_rect(col, row, menu_layout.bar_area) {
            return Some(ThemeKeyInfo {
                fg_key: Some("ui.menu_fg".into()),
                bg_key: Some("ui.menu_bg".into()),
                region: "Menu Bar".into(),
                fg_color: Some(theme.menu_fg),
                bg_color: Some(theme.menu_bg),
                syntax_category: None,
            });
        }
    }

    // 3. Check tab bars (active vs inactive)
    for (split_id, tab_layout) in &self.cached_layout.tab_layouts {
        if let Some(hit) = tab_layout.hit_test(col, row) {
            let (is_active, buffer_id) = match hit { /* determine active state */ };
            let (fg_key, bg_key, fg_color, bg_color) = if is_active {
                ("ui.tab_active_fg", "ui.tab_active_bg",
                 theme.tab_active_fg, theme.tab_active_bg)
            } else {
                ("ui.tab_inactive_fg", "ui.tab_inactive_bg",
                 theme.tab_inactive_fg, theme.tab_inactive_bg)
            };
            return Some(ThemeKeyInfo { fg_key: Some(fg_key.into()), ... });
        }
    }

    // 4. Check split separators
    for &(_, _, sep_x, sep_y, sep_len) in &self.cached_layout.separator_areas {
        // ... hit test ...
    }

    // 5. Check editor content areas (the main case)
    for &(split_id, buffer_id, content_rect, ..) in &self.cached_layout.split_areas {
        if !point_in_rect(col, row, content_rect) { continue; }

        let gutter_width = /* get from buffer metadata */;
        let rel_col = col - content_rect.x;

        if rel_col < gutter_width {
            return Some(ThemeKeyInfo {
                fg_key: Some("editor.line_number_fg".into()),
                bg_key: Some("editor.line_number_bg".into()),
                region: "Line Numbers".into(),
                fg_color: Some(theme.line_number_fg),
                bg_color: Some(theme.line_number_bg),
                syntax_category: None,
            });
        }

        // Content area: resolve byte position via ViewLineMapping
        let byte_pos = screen_to_buffer_position(col, row, ...);

        // Look up highlight category at this byte position
        let category = self.get_highlight_category_at(buffer_id, byte_pos);

        match category {
            Some(cat) => {
                let (key, color) = category_to_theme_key(cat, theme);
                return Some(ThemeKeyInfo {
                    fg_key: Some(key.into()),
                    bg_key: Some("editor.bg".into()),
                    region: format!("Syntax: {}", category_display_name(cat)),
                    fg_color: Some(color),
                    bg_color: Some(theme.editor_bg),
                    syntax_category: Some(category_display_name(cat).into()),
                });
            }
            None => {
                return Some(ThemeKeyInfo {
                    fg_key: Some("editor.fg".into()),
                    bg_key: Some("editor.bg".into()),
                    region: "Editor Content".into(),
                    fg_color: Some(theme.editor_fg),
                    bg_color: Some(theme.editor_bg),
                    syntax_category: None,
                });
            }
        }
    }

    None
}
```

**Helper for category → theme key mapping:**

```rust
fn category_to_theme_key(cat: HighlightCategory, theme: &Theme) -> (&'static str, Color) {
    match cat {
        HighlightCategory::Keyword  => ("syntax.keyword",  theme.syntax_keyword),
        HighlightCategory::String   => ("syntax.string",   theme.syntax_string),
        HighlightCategory::Comment  => ("syntax.comment",  theme.syntax_comment),
        HighlightCategory::Function => ("syntax.function", theme.syntax_function),
        HighlightCategory::Type     => ("syntax.type",     theme.syntax_type),
        HighlightCategory::Variable => ("syntax.variable", theme.syntax_variable),
        HighlightCategory::Constant
        | HighlightCategory::Number
        | HighlightCategory::Attribute => ("syntax.constant", theme.syntax_constant),
        HighlightCategory::Operator
        | HighlightCategory::Property => ("syntax.operator", theme.syntax_operator),
    }
}
```

### 2. Retrieving the HighlightCategory at a Byte Position

The highlight engine currently produces `HighlightSpan` with resolved `Color` values, but for this feature we need the **category** (to know the theme key name). The internal `CachedSpan` stores the category but it's not exposed.

**Approach: Add a `category` field to `HighlightSpan`**

Currently:
```rust
pub struct HighlightSpan {
    pub range: Range<usize>,
    pub color: Color,
}
```

Change to:
```rust
pub struct HighlightSpan {
    pub range: Range<usize>,
    pub color: Color,
    pub category: Option<HighlightCategory>,  // NEW
}
```

This is populated during `resolve_spans()` in `highlighter.rs`, which already knows the `CachedSpan.category`. The `Option` accounts for spans injected by other sources (ANSI, textmate) that don't have a category.

Then add a lookup function:

```rust
impl Highlighter {
    /// Get the highlight category at a byte position (for theme inspection)
    pub fn category_at(&self, byte_pos: usize) -> Option<HighlightCategory> {
        self.cached_spans.iter().find_map(|span| {
            if span.range.contains(&byte_pos) {
                Some(span.category)
            } else {
                None
            }
        })
    }
}
```

### 3. Intercepting Ctrl+Right-Click

In `mouse_input.rs`, the current handler for `MouseEventKind::Down(MouseButton::Right)` calls `handle_right_click()` which only handles tab context menus.

**Add modifier detection:**

```rust
MouseEventKind::Down(MouseButton::Right) => {
    if mouse_event.modifiers.contains(KeyModifiers::CONTROL) {
        // Ctrl+Right-Click → theme info popup
        self.show_theme_info_popup(col, row)?;
    } else {
        // Normal right-click → existing tab context menu
        self.handle_right_click(col, row)?;
    }
    needs_render = true;
}
```

### 4. The Theme Info Popup

**New state field on `Editor`:**
```rust
pub(super) theme_info_popup: Option<ThemeInfoPopup>,
```

```rust
#[derive(Debug, Clone)]
pub struct ThemeInfoPopup {
    /// Screen position where popup appears
    pub position: (u16, u16),
    /// Resolved theme key information
    pub info: ThemeKeyInfo,
    /// Whether the "Open in Theme Editor" button is highlighted
    pub button_highlighted: bool,
}
```

**Popup visual design (rendered as a bordered box, ~34 chars wide):**

```
┌─ Theme Info ─────────────────┐
│ Region: Syntax Highlight     │
│                              │
│ Foreground: syntax.keyword   │
│   ▉ RGB(86, 156, 214)       │
│   Category: Keyword          │
│                              │
│ Background: editor.bg        │
│   ▉ RGB(30, 30, 30)         │
│                              │
│ ► Open in Theme Editor       │
└──────────────────────────────┘
```

The `▉` characters are rendered with the actual theme color applied as foreground, serving as inline color swatches.

**Sizing:** ~34 chars wide, ~12 rows tall.

**Positioning:** Use `clamp_rect_to_bounds()` (already exists in `popup.rs`) to keep within terminal. Position below-right of click, flip to above/left if near edges.

**Rendering:** Render in the main render pass, on top of everything (after popups, before nothing). Uses existing theme popup colors (`popup_bg`, `popup_border_fg`, `popup_text_fg`) for the popup itself.

```rust
fn render_theme_info_popup(frame: &mut Frame, popup: &ThemeInfoPopup, theme: &Theme) {
    let info = &popup.info;

    let mut lines = vec![];
    lines.push(Line::from(format!(" Region: {}", info.region)));
    lines.push(Line::from(""));

    if let Some(ref fg_key) = info.fg_key {
        lines.push(Line::from(format!(" Foreground: {}", fg_key)));
        if let Some(color) = info.fg_color {
            let (r, g, b) = color_to_rgb(color).unwrap_or((0, 0, 0));
            lines.push(Line::from(vec![
                Span::raw("   "),
                Span::styled("▉ ", Style::default().fg(color)),
                Span::raw(format!("RGB({}, {}, {})", r, g, b)),
            ]));
        }
        if let Some(ref cat) = info.syntax_category {
            lines.push(Line::from(format!("   Category: {}", cat)));
        }
    }

    lines.push(Line::from(""));
    if let Some(ref bg_key) = info.bg_key {
        lines.push(Line::from(format!(" Background: {}", bg_key)));
        if let Some(color) = info.bg_color {
            let (r, g, b) = color_to_rgb(color).unwrap_or((0, 0, 0));
            lines.push(Line::from(vec![
                Span::raw("   "),
                Span::styled("▉ ", Style::default().fg(color)),
                Span::raw(format!("RGB({}, {}, {})", r, g, b)),
            ]));
        }
    }

    lines.push(Line::from(""));
    let button_style = if popup.button_highlighted {
        Style::default().fg(theme.popup_selection_fg).bg(theme.popup_selection_bg)
    } else {
        Style::default().fg(theme.popup_text_fg)
    };
    lines.push(Line::from(Span::styled(" ► Open in Theme Editor ", button_style)));

    let width = 34u16;
    let height = lines.len() as u16 + 2; // +2 for border

    let rect = clamp_rect_to_bounds(
        Rect::new(popup.position.0, popup.position.1, width, height),
        frame.area(),
    );

    frame.render_widget(Clear, rect);
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.popup_border_fg))
        .title(" Theme Info ")
        .style(Style::default().bg(theme.popup_bg).fg(theme.popup_text_fg));
    let paragraph = Paragraph::new(lines).block(block);
    frame.render_widget(paragraph, rect);
}
```

**Dismissal:**
- Any click outside the popup → close it
- Escape key → close it
- Click on "Open in Theme Editor" button → open theme editor at that key, close popup
- Any other key → close it

### 5. "Open in Theme Editor" Button

When clicked, this needs to:

1. **Close the theme info popup**
2. **Store the target theme key** for the theme editor to pick up
3. **Execute the `open_theme_editor` plugin action**

**Implementation approach: Use a temporary editor variable**

In Rust, when the button is clicked:
```rust
fn open_theme_editor_at_key(&mut self, key: &str) {
    // Store jump target for the theme editor plugin to read
    self.theme_inspect_jump_key = Some(key.to_string());
    // Trigger the theme editor open command
    self.plugin_manager.execute_action_async("open_theme_editor");
}
```

The plugin API needs a way to read this. Options:
- **Best option:** Add `editor.getVariable(key)` / `editor.setVariable(key, value)` to the plugin API if it doesn't exist, or use the existing `editor.getConfig()` mechanism.
- **Simpler option:** The Rust side dispatches a custom event/hook that the plugin listens for: `editor.onCustomEvent("theme_inspect_jump", (key) => scrollToField(key))`.

In `theme_editor.ts`, modify the open function:
```typescript
globalThis.open_theme_editor = async function(): Promise<void> {
    // ... existing open logic ...

    // After opening, check if there's a jump target
    const jumpTarget = editor.getVariable("theme_inspect_jump_key");
    if (jumpTarget) {
        editor.setVariable("theme_inspect_jump_key", null);
        // Find and navigate to the field matching this key
        scrollToField(jumpTarget);
    }
};

function scrollToField(key: string): void {
    // Map theme key path to display entry index
    // e.g., "syntax.keyword" → find entry with path "syntax.keyword"
    const idx = state.displayEntries.findIndex(e =>
        !e.isSection && e.path === key
    );
    if (idx >= 0) {
        state.cursorIndex = idx;
        ensureCursorVisible();
        refreshDisplay();
    }
}
```

### 6. Mouse Interaction with the Popup

The popup needs mouse handling for:
- **Hovering over the button** → highlight it (`button_highlighted = true`)
- **Clicking the button** → trigger theme editor open
- **Clicking outside** → dismiss

In `handle_mouse_click`:
```rust
// Check theme info popup first (before other click handling)
if let Some(ref popup) = self.theme_info_popup {
    let popup_rect = /* compute rect from popup.position + dimensions */;
    if point_in_rect(col, row, popup_rect) {
        // Check if click is on the button row
        let button_row = popup.position.1 + popup_height - 2; // second-to-last row
        if row == button_row {
            let key = popup.info.fg_key.clone().or(popup.info.bg_key.clone());
            self.theme_info_popup = None;
            if let Some(key) = key {
                self.open_theme_editor_at_key(&key);
            }
            return Ok(());
        }
        // Click inside popup but not on button — ignore
        return Ok(());
    } else {
        // Click outside popup — dismiss
        self.theme_info_popup = None;
    }
}
```

For hover (in `MouseEventKind::Moved`):
```rust
if let Some(ref mut popup) = self.theme_info_popup {
    let button_row = /* ... */;
    popup.button_highlighted = row == button_row
        && col >= popup.position.0
        && col < popup.position.0 + popup_width;
}
```

### 7. File Changes Summary

| File | Change |
|---|---|
| `crates/fresh-editor/src/primitives/highlight_types.rs` | Add `category: Option<HighlightCategory>` to `HighlightSpan` |
| `crates/fresh-editor/src/primitives/highlighter.rs` | Populate `category` during span resolution; add `category_at()` |
| `crates/fresh-editor/src/primitives/highlight_engine.rs` | Update `HighlightSpan` construction to include category |
| `crates/fresh-editor/src/app/types.rs` | Add `ThemeKeyInfo`, `ThemeInfoPopup` structs |
| **NEW** `crates/fresh-editor/src/app/theme_inspect.rs` | `resolve_theme_key_at()`, `show_theme_info_popup()`, `render_theme_info_popup()`, `category_to_theme_key()` |
| `crates/fresh-editor/src/app/mod.rs` | Add `theme_info_popup: Option<ThemeInfoPopup>` field, `mod theme_inspect;` |
| `crates/fresh-editor/src/app/mouse_input.rs` | Ctrl+Right-Click detection, popup click/hover handling |
| `crates/fresh-editor/src/app/input.rs` | Escape key dismisses popup |
| `crates/fresh-editor/src/app/render.rs` | Call `render_theme_info_popup()` in the render pipeline |
| `crates/fresh-editor/plugins/theme_editor.ts` | Read jump target, add `scrollToField()` function |
| `crates/fresh-editor/tests/e2e/theme_inspect.rs` | E2E test: Ctrl+Right-Click shows popup |

### 8. Edge Cases

- **Ctrl+Right-Click on empty area past EOF**: Show `editor.bg` only
- **Ctrl+Right-Click on overlays** (search highlights, diagnostics): Show overlay theme keys as additional info in the popup
- **Ctrl+Right-Click on fold markers**: Show fold placeholder styling info
- **Multiple overlapping theme contributions**: Show all (e.g., selection bg overrides editor bg; syntax fg overrides editor fg)
- **Plugin view transforms**: When a plugin overrides colors via `submitViewTransform()`, note "overridden by plugin" in the popup
- **Terminal buffers**: Show `editor.terminal_bg`, `editor.terminal_fg`
- **Virtual buffers** (diagnostics list, grep results): Same as editor content
- **Popup already open**: Ctrl+Right-Click elsewhere repositions/updates the popup

### 9. Testing Strategy (per CONTRIBUTING.md)

1. **E2E test**: Send Ctrl+Right-Click mouse event on a known syntax-highlighted token → verify popup renders with correct theme key text
2. **E2E test**: Send Ctrl+Right-Click on status bar → verify popup shows `ui.status_bar_*` keys
3. **E2E test**: Click the "Open in Theme Editor" button → verify theme editor opens
4. **Unit test**: `resolve_theme_key_at()` with mocked layout → verify correct keys for each region
5. **Unit test**: `category_to_theme_key()` → verify all categories map correctly
