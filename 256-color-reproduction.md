# 256-Color Theme Reproduction Report

Issue: https://github.com/sinelaw/fresh/issues/1239

## Reproduction Method

1. Built fresh in debug mode: `cargo build`
2. Forced 256-color mode via `FRESH_COLOR_MODE=256`
3. Launched in tmux with `TERM=xterm-256color`
4. Tested each theme with a sample Rust file and Ctrl+P palette

## Root Cause

The `rgb_to_256()` function in `crates/fresh-editor/src/view/color_support.rs` maps
24-bit RGB colors to the nearest 256-color palette index. The 256-color palette has:
- 16 standard ANSI colors (indices 0-15)
- 216 colors in a 6x6x6 cube (indices 16-231)
- 24 grayscale shades (indices 232-255)

Many theme colors, especially subtle dark tones and carefully chosen accent colors,
map to the SAME or very similar 256-color index, destroying contrast.

## Issues Found Per Theme

### solarized-dark - WORST AFFECTED
- **Editor bg** `[0,43,54]` (teal-dark) maps to idx 16 = pure black `(0,0,0)`
- **Selection bg** `[7,54,66]` ALSO maps to idx 16 = pure black — selection is INVISIBLE
- **Line numbers** fg=idx59 `(51,51,51)` on bg=idx16 `(0,0,0)` — contrast 1.66, nearly unreadable
- **Ctrl+P selected item**: fg=idx102 `(102,102,102)` on bg=idx31 `(0,102,153)` — contrast 1.09, UNREADABLE
- **Ctrl+P shortcut keys**: fg=idx30 `(0,102,102)` on bg=idx31 `(0,102,153)` — contrast 1.09, UNREADABLE
- Solarized's precise color palette is fundamentally incompatible with the 6x6x6 cube

### nord - SEVERELY AFFECTED
- **Editor bg** `[46,52,64]` maps to idx 16 = pure black `(0,0,0)` (loses blue tint)
- **Line numbers** fg=idx23 `(0,51,51)` on bg=idx16 `(0,0,0)` — contrast 1.52, barely visible
- **Popup border**: fg=idx23 on bg=idx17 `(0,0,51)` — contrast 1.45, UNREADABLE
- **Ctrl+P selected item shortcuts**: contrast 1.88, very poor
- Nord's muted, close-together colors collapse in the 6x6x6 cube

### dracula - MODERATELY AFFECTED
- **Line numbers** fg=idx60 `(51,51,102)` on bg=idx16 `(0,0,0)` — contrast 1.81, hard to read
- **Ctrl+P selection**: purple bg looks reasonable but shortcuts are low contrast (2.16)
- **Editor bg** `[40,42,54]` maps to idx 16 = pure black (loses purple tint)

### light - MODERATELY AFFECTED
- **Ctrl+P helper text** ("file | >command | :line | #buffer"): UNREADABLE (cr=1.03)
  - fg=idx7 `(192,192,192)` on bg=idx152 `(153,204,204)` — virtually same brightness
- **Non-selected palette items**: fg=idx234 on bg=idx0 — contrast 1.23, UNREADABLE
  (light theme bg maps to black for non-highlighted items in palette)
- **Whitespace indicators**: very low contrast

### high-contrast - MILDLY AFFECTED
- **Syntax types** (Magenta on Black): contrast 2.23, readable but not great
- **Constants** (LightBlue on Black): contrast 2.44, slightly low
- High-contrast theme fares best since it uses named ANSI colors for syntax

### dark - MILDLY AFFECTED
- **Line numbers**: contrast 2.40, acceptable but reduced
- **Comments**: contrast 2.51, slightly dim
- **Help indicator** (Red on Black): contrast 1.92, poor
- Dark theme works reasonably well overall

### nostalgia - MILDLY AFFECTED
- **Tab bar**: fg=idx16 on bg=idx0 — both map to black, tab text UNREADABLE
- Otherwise okay due to use of bright, saturated colors

## Key Patterns

1. **Dark backgrounds with subtle color tints** (solarized teal, nord blue-gray, dracula purple)
   all collapse to pure black (idx 16) in the 6x6x6 cube
2. **Muted accent colors** that differ by hue but not brightness map to the same
   256-color index, destroying contrast between fg/bg
3. **Selection backgrounds** often become invisible when they map to the same index
   as the editor background
4. **The Ctrl+P palette** is the most visibly broken element across most themes,
   because it uses accent colors for the selected item background
5. **Line numbers** are problematic in themes that use subtle gray-ish colors on dark backgrounds

## Potential Solutions

1. **Improve rgb_to_256()**: Consider perceptual color distance (CIE LAB) instead of
   simple threshold-based mapping
2. **Theme-specific 256-color overrides**: Allow themes to specify alternate colors
   for 256-color mode
3. **Minimum contrast enforcement**: After mapping to 256 colors, verify fg/bg pairs
   meet a minimum contrast ratio and adjust if not
4. **Use ANSI colors in 256-color mode**: For themes like solarized that have known
   mappings to the standard 16 ANSI colors, use those instead of the 6x6x6 cube
