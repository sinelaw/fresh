# Cursor Rendering Bug Analysis - Issue #851

## Issue Summary

"Blinking bar" cursor style only appears to work at line endings. When the cursor
is positioned on a character (anywhere except end-of-line), the bar cursor is
visually invisible because a software `REVERSED` style creates a block-like
highlight that completely obscures the thin bar cursor.

## Root Cause

In `crates/fresh-editor/src/view/ui/split_rendering.rs`, the function
`compute_char_style()` (lines 767-789) applies `Modifier::REVERSED` to the
character under the cursor in **normal mode** (non-session mode):

```rust
// Line 781-785
} else {
    // Normal mode: apply REVERSED to all cursor positions (primary and secondary)
    // This ensures the character under the cursor is always visible
    style = style.add_modifier(Modifier::REVERSED);
}
```

This REVERSED modifier swaps FG/BG colors of the character cell, creating a
block-like visual effect. This works well for **block cursors** (the REVERSED
styling and hardware block cursor are visually consistent), but for **bar** and
**underline** cursors, the REVERSED block dominates the visual, making the thin
hardware cursor invisible.

**Session mode** (line 774-780) already handles this correctly for the primary
cursor — it skips REVERSED for the primary cursor and relies on the hardware
cursor alone. But standalone mode does not.

## Evidence from tmux capture-pane -e

### Cursor in MIDDLE of line (blinking_bar style):
```
^[[38;5;15mHello^[[7m ^[[0m^[[38;5;15m^[[48;5;0mWorld
                     ^^^^
                     REVERSED applied to cursor character
```
The `^[[7m` (SGR REVERSED) makes the space between "Hello" and "World" appear
as a white block on black background — overshadowing the blinking bar cursor.

### Cursor at END of line (blinking_bar style):
```
^[[38;5;15mHello World
```
No `^[[7m` anywhere. Only the hardware cursor (blinking bar) is visible, and it
renders correctly as a thin vertical bar.

### With cursor line highlight active (cursor at start, on 'H'):
```
^[[7m^[[38;5;15m^[[48;5;17mH^[[0m^[[38;5;15m^[[48;5;17mello
```
The cursor character 'H' gets:
- `^[[48;5;17m` = BG dark blue (cursor line highlight)
- `^[[38;5;15m` = FG white (text color)
- `^[[7m`       = REVERSED

With REVERSED active, the visual result is:
- Visual FG = dark blue (original BG)
- Visual BG = white (original FG)

This creates a WHITE BLOCK on a DARK BLUE line — completely hiding any bar
or underline cursor shape.

## Affected Cursor Styles

| Cursor Style      | Middle of Line | End of Line | Root Cause                    |
|-------------------|----------------|-------------|-------------------------------|
| blinking_block    | OK             | OK          | REVERSED matches block shape  |
| steady_block      | OK             | OK          | REVERSED matches block shape  |
| blinking_bar      | BUG            | OK          | REVERSED creates block, hides bar |
| steady_bar        | BUG            | OK          | REVERSED creates block, hides bar |
| blinking_underline| BUG            | OK          | REVERSED creates block, hides underline |
| steady_underline  | BUG            | OK          | REVERSED creates block, hides underline |

## Why End-of-Line Works

At end-of-line, the cursor position is past the last character. The rendering
code in `render_view_lines()` (around line 4709-4735) handles this case:

```rust
// line 4712-4713
let should_add_indicator = if is_active {
    software_cursor_only || !is_primary_at_end
```

For the primary cursor at end-of-line in terminal mode (`software_cursor_only`
is false), no indicator space is added. The hardware cursor alone provides the
visual — and since no REVERSED is applied to any cell, the bar cursor renders
correctly.

## Two Distinct Sub-Issues

### Sub-issue 1: REVERSED modifier on cursor character
The core problem. In `compute_char_style()`, normal mode unconditionally applies
REVERSED to the primary cursor character. For bar/underline cursor styles, this
should not be applied to the primary cursor — the hardware cursor provides
sufficient visual indication.

### Sub-issue 2: Cursor line highlight interaction
The `current_line_bg` (dark blue, color 17 in 256-color mode) further worsens
the effect. When REVERSED is applied on top of the cursor line highlight, the
color swap creates an even more prominent block that further hides thin cursors.

## Relevant Code Locations

- `crates/fresh-editor/src/view/ui/split_rendering.rs:767-789` — `compute_char_style()` cursor styling
- `crates/fresh-editor/src/view/ui/split_rendering.rs:4709-4735` — End-of-line cursor indicator
- `crates/fresh-editor/src/view/ui/split_rendering.rs:5517-5540` — Hardware cursor positioning
- `crates/fresh-editor/src/config.rs:116-212` — CursorStyle enum definition
- `crates/fresh-editor/src/main.rs:1335` — Hardware cursor style set via crossterm
- `crates/fresh-editor/src/app/mod.rs:295` — `session_mode` field
- `crates/fresh-editor/src/view/theme/types.rs:245-247` — `current_line_bg` default color

## Proposed Fix Direction (from maintainer)

The maintainer (sinelaw) suggested in the issue comments:
> "I'm going to try to have the main cursor fully hardware based and keep the
> custom rendering for the secondary cursors only."

This aligns with how **session mode** already works — it skips REVERSED for the
primary cursor. The fix would extend this behavior to normal mode for bar and
underline cursor styles, while keeping REVERSED for block cursors (where it's
needed for visibility).
