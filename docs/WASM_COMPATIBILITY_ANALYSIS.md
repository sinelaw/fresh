# WASM Compatibility Analysis

This document analyzes each module in fresh-editor for WASM compatibility, identifying blocking dependencies and potential solutions.

## Key Insight: Ratatui is WASM-Compatible

**Ratatui itself is WASM-compatible.** The library is a rendering abstraction that writes to a `Buffer` of `Cell`s. The platform-specific part is only the **backend** (like crossterm) that handles actual terminal I/O.

With `default-features = false`, ratatui provides:
- ✅ `Color`, `Style`, `Modifier` - styling types
- ✅ `Rect` - layout rectangles
- ✅ `Buffer`, `Cell` - terminal buffer abstraction
- ✅ `Frame` - rendering frame (backend-agnostic)
- ❌ `crossterm` backend - native terminal I/O (optional)

**Ratzilla** provides a WASM backend for ratatui that renders to browser canvas/DOM.

---

## Summary

| Layer | Total Files | WASM-Ready | Notes |
|-------|-------------|------------|-------|
| model | 14 | 14 (100%) | ✅ Complete |
| primitives | 21 | 21 (100%) | ✅ Complete - all features have WASM fallbacks |
| input | 14 | 3 (21%) | Needs crossterm abstraction |
| view | 70+ | ~51 (72%) | Needs Ratzilla backend |
| services | 38 | ~5 (13%) | Gate async/tokio |
| app | 37 | 0 (0%) | Needs full refactor |

**Key insights**:
1. **Syntect is WASM-compatible** with `fancy-regex` feature → syntax highlighting for 100+ languages
2. **All tree-sitter features have WASM fallbacks** → indentation, reference highlighting work without tree-sitter
3. **Crossterm event types are WASM-compatible** → `KeyEvent`, `MouseEvent`, etc. are pure Rust data structures

**Remaining blockers:**
1. **crossterm terminal I/O** - actual terminal read/write not available in WASM, use Ratzilla backend
2. **tokio/async** in services - gate behind runtime
3. **PTY/signals** - inherent platform limitations
4. **crate::input module** - depends on crossterm I/O for event reading

---

## Layer 1: MODEL (14 files) - 100% WASM-Ready ✅

All model files are pure Rust. Already compilable to WASM.

| File | Status | Notes |
|------|--------|-------|
| buffer.rs | ✅ Ready | Uses anyhow, regex (WASM-compatible) |
| piece_tree.rs | ✅ Ready | Core data structure |
| piece_tree_diff.rs | ✅ Ready | Diff algorithms |
| cursor.rs | ✅ Ready | Cursor state |
| marker.rs | ✅ Ready | Marker types |
| marker_tree.rs | ✅ Ready | Interval tree |
| event.rs | ✅ Ready | Streaming gated behind runtime |
| edit.rs | ✅ Ready | Edit operations |
| document_model.rs | ✅ Ready | Document abstraction |
| control_event.rs | ✅ Ready | Control events |
| line_diff.rs | ✅ Ready | Line diffing |
| composite_buffer.rs | ✅ Ready | Composite buffer |
| filesystem.rs | ✅ Ready | libc gated behind runtime |

**Status**: ✅ Complete - no changes needed.

---

## Layer 2: PRIMITIVES (21 files) - 100% WASM-Ready ✅

### All Features Have WASM-Compatible Implementations

Every primitive feature now has a pure-Rust WASM-compatible implementation:

| Feature | WASM Module | Runtime Module | Notes |
|---------|-------------|----------------|-------|
| Syntax highlighting | `textmate_engine.rs` | `highlight_engine.rs` | Syntect with fancy-regex |
| Auto-indentation | `indent_pattern.rs` | `indent.rs` | Pattern-based heuristics |
| Reference highlighting | `reference_highlight_text.rs` | `reference_highlighter.rs` | Text matching |

### WASM-Ready (All 21 files)

| File | Status | Dependencies |
|------|--------|--------------|
| display_width.rs | ✅ Ready | unicode-width |
| grapheme.rs | ✅ Ready | unicode-segmentation |
| snippet.rs | ✅ Ready | Pure Rust |
| text_property.rs | ✅ Ready | Pure Rust |
| path_utils.rs | ✅ Ready | std::path |
| line_wrapping.rs | ✅ Ready | display_width |
| line_iterator.rs | ✅ Ready | model::buffer |
| word_navigation.rs | ✅ Ready | model::buffer, grapheme |
| **ansi.rs** | ✅ Ready | ratatui::style |
| **ansi_background.rs** | ✅ Ready | ratatui::style |
| **visual_layout.rs** | ✅ Ready | ansi, display_width |
| **grammar/types.rs** | ✅ Ready | syntect with fancy-regex |
| **highlight_types.rs** | ✅ Ready | Common highlighting types |
| **textmate_engine.rs** | ✅ Ready | Syntect-only highlighting |
| **indent_pattern.rs** | ✅ Ready | Pattern-based indentation |
| **reference_highlight_text.rs** | ✅ Ready | Text-based word matching |

### Runtime-Enhanced (4 files)

These modules provide enhanced features using tree-sitter AST analysis.
WASM builds use the pure-Rust alternatives above.

| File | Enhancement | WASM Alternative |
|------|-------------|------------------|
| highlight_engine.rs | Unified engine with tree-sitter | `textmate_engine.rs` |
| highlighter.rs | Tree-sitter highlighting | `textmate_engine.rs` |
| indent.rs | AST-aware smart indentation | `indent_pattern.rs` |
| reference_highlighter.rs | Scope-aware semantic highlighting | `reference_highlight_text.rs` |

---

## Layer 3: INPUT (14 files) - 21% WASM-Ready

### Blocker: crossterm::event types

The input layer uses `crossterm::event::{KeyCode, KeyEvent, KeyModifiers}` for keyboard handling.

### WASM-Ready (3 files)

| File | Status | Notes |
|------|--------|-------|
| fuzzy.rs | ✅ Ready | Pure fuzzy matching algorithm |
| input_history.rs | ✅ Ready | Pure data structure |
| position_history.rs | ✅ Ready | Pure data structure |

### Needs Abstraction (11 files)

| File | Blocker | Solution |
|------|---------|----------|
| handler.rs | KeyCode, KeyEvent | Abstract to platform-agnostic types |
| keybindings.rs | KeyCode, KeyModifiers | Use abstract types |
| key_translator.rs | KeyCode, KeyEvent | Conversion layer |
| buffer_mode.rs | KeyCode, KeyModifiers | Use abstract types |
| composite_router.rs | KeyCode, KeyEvent | Use abstract types |
| actions.rs | Indirect | Update when handler changes |
| commands.rs | Indirect | Update when handler changes |
| command_registry.rs | Indirect | Update when handler changes |
| multi_cursor.rs | Indirect | Update when handler changes |
| action.rs | Indirect | Update when handler changes |
| vim_mode.rs | KeyCode | Use abstract types |

### Recommended Solution

Create abstract input types in `fresh-core`:

```rust
/// Platform-agnostic key code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyCode {
    Char(char),
    Enter, Esc, Backspace, Delete, Tab,
    Left, Right, Up, Down,
    Home, End, PageUp, PageDown,
    F(u8),
}

/// Platform-agnostic modifiers
#[derive(Debug, Clone, Copy, Default)]
pub struct Modifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
}

/// Platform-agnostic key event
#[derive(Debug, Clone)]
pub struct KeyEvent {
    pub code: KeyCode,
    pub modifiers: Modifiers,
}

// Conversions
#[cfg(feature = "runtime")]
impl From<crossterm::event::KeyEvent> for KeyEvent { ... }

#[cfg(feature = "wasm")]
impl From<web_sys::KeyboardEvent> for KeyEvent { ... }
```

**Effort**: 1-2 days to create abstraction and update input layer.

---

## Layer 4: VIEW (70+ files) - 70% WASM-Ready

### Key Insight

Most view files only use **ratatui types** (Color, Style, Rect, Buffer), which are WASM-compatible. The blockers are:

1. **crossterm::event** - Input handling in some view files
2. **Terminal I/O** - Actual rendering to terminal (use Ratzilla instead)

### WASM-Ready (~50 files)

Files using only ratatui types (no crossterm):

| Category | Files | Status |
|----------|-------|--------|
| Styling | color_support.rs, margin.rs, overlay.rs, stream.rs, virtual_text.rs | ✅ Ready |
| Layout | composite_view.rs, dimming.rs, split.rs, popup_mouse.rs | ✅ Ready |
| Rendering | markdown.rs, reference_highlight_overlay.rs | ✅ Ready |
| Theme | theme.rs, theme/*.rs (once filesystem abstracted) | ✅ Ready |
| UI Components | Most of view/ui/*.rs | ✅ Ready |

### Needs Gating (~16 files)

Files with crossterm input handling:

| File | Blocker | Solution |
|------|---------|----------|
| file_browser_input.rs | crossterm::event | Use abstract KeyEvent |
| popup_input.rs | crossterm::event | Use abstract KeyEvent |
| prompt_input.rs | crossterm::event | Use abstract KeyEvent |
| query_replace_input.rs | crossterm::event | Use abstract KeyEvent |

### Inherent Blockers (~4 files)

| File | Reason |
|------|--------|
| calibration_wizard.rs | Interactive terminal calibration |
| terminal_view.rs | Terminal emulator display |

### WASM Rendering Strategy

For WASM, use **Ratzilla** as the ratatui backend:

```rust
// Native (runtime feature)
#[cfg(feature = "runtime")]
use ratatui::backend::CrosstermBackend;

// WASM (wasm feature)
#[cfg(feature = "wasm")]
use ratzilla::backend::DomBackend;
```

This allows the same rendering code to work on both platforms.

---

## Layer 5: SERVICES (38 files) - Mixed

### WASM-Ready (5 files)

| File | Status | Notes |
|------|--------|-------|
| log_dirs.rs | ✅ Ready | Path utilities |
| time_source.rs | ✅ Ready | Time abstraction |
| warning_log.rs | ✅ Ready | In-memory log |
| recovery/types.rs | ✅ Ready | Data types |
| styled_html.rs | ✅ Ready | HTML generation |

### Needs Gating (28 files)

| File/Module | Blocker | Solution |
|-------------|---------|----------|
| **clipboard.rs** | arboard, crossterm OSC52 | Use browser Clipboard API |
| **fs/manager.rs** | tokio::sync | Gate behind runtime |
| **lsp/*.rs** | lsp_types, tokio | Gate (LSP not in browser) |
| **plugins/*.rs** | tokio, plugin runtime | Gate or WASM plugin runtime |
| **recovery/*.rs** | std::fs | Use FileSystem trait |
| **release_checker.rs** | ureq (HTTP) | Use fetch API |
| **telemetry.rs** | Network I/O | Gate |
| **gpm/*.rs** | GPM mouse protocol | Gate (native terminal only) |
| **tracing_setup.rs** | tracing-subscriber | Use web console |

### Inherent Blockers (5 files)

| File | Reason |
|------|--------|
| **terminal/pty.rs** | PTY requires OS process spawning |
| **terminal/term.rs** | alacritty_terminal is native-only |
| **terminal/manager.rs** | Depends on PTY |
| **signal_handler.rs** | Unix signals don't exist in browser |
| **process_limits.rs** | OS-level resource limits |

These are fundamentally incompatible with browsers and must remain runtime-only.

---

## Layer 6: APP (37 files) - Needs Refactoring

The app layer orchestrates all other layers. Key changes needed:

| Area | Change Needed |
|------|--------------|
| Input | Use abstract KeyEvent types |
| Rendering | Support both crossterm and Ratzilla backends |
| Services | Gate runtime-only services |
| Terminal | Gate PTY/terminal emulator features |

---

## Revised Refactoring Plan

### Phase 1: Input Abstraction (3-5 days)
1. Create `KeyCode`, `Modifiers`, `KeyEvent` in fresh-core
2. Add conversion traits for crossterm (runtime) and web_sys (wasm)
3. Update input layer to use abstract types
4. Update view files with input handling

### Phase 2: Backend Abstraction (1 week)
1. Add Ratzilla dependency for WASM builds
2. Create backend selection based on feature flags
3. Update app layer to use selected backend
4. Test with both backends

### Phase 3: Service Gating (3-5 days)
1. Gate LSP, plugins, terminal behind runtime
2. Abstract clipboard with trait
3. Update filesystem operations to use FileSystem trait
4. Provide WASM stubs where needed

### Phase 4: Integration & Testing (1 week)
1. Build WASM target
2. Create browser demo
3. Performance testing
4. Cross-browser compatibility

**Revised total effort**: 3-4 weeks (down from 10-12 weeks)

---

## Current Status

**Model layer**: ✅ 100% WASM-compatible
**Primitives layer**: ✅ 100% WASM-compatible

Completed:
- ✅ Model layer WASM-compatible
- ✅ FileSystem trait abstracted with NoopFileSystem for WASM
- ✅ Event streaming gated behind runtime
- ✅ Config runtime-specific functions gated
- ✅ WASM feature flag added to Cargo.toml
- ✅ Basic wasm module with WasmEditor wrapper
- ✅ **Syntect enabled for WASM** with `fancy-regex` feature (pure Rust regex)
- ✅ **Grammar module WASM-compatible** (TextMate grammar loading via syntect)
- ✅ **Theme types WASM-compatible** (view/theme/types.rs)
- ✅ **Syntax highlighting**: `textmate_engine.rs` (100+ languages)
- ✅ **Auto-indentation**: `indent_pattern.rs` (pattern-based)
- ✅ **Reference highlighting**: `reference_highlight_text.rs` (text matching)

Next steps:
1. Input abstraction (biggest remaining blocker)
2. Add Ratzilla for WASM rendering
3. Gate remaining services
