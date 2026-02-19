# Plan: Native GUI Backend for Fresh Editor

## Summary

Add an optional `gui` feature flag that launches Fresh in a native GPU-accelerated window using `winit` + `wgpu` via the `ratatui-wgpu` crate. The terminal backend remains the default. The GUI backend translates winit input events into crossterm types so zero editor internals need to change.

## Decisions (confirmed with user)

1. **Event types**: Translate winit → crossterm types directly (no custom enum)
2. **ratatui-wgpu**: Git dependency pinned to commit `cdcf5c6` (ratatui 0.30 support)
3. **Event loop**: Extract per-tick housekeeping into shared function (Option B)
4. **Initialization**: Keep `initialize_app()` terminal-only; separate `gui::run_gui()`
5. **Async init**: Use `tokio::runtime::Runtime::block_on()` for wgpu surface creation
6. **Font**: Bundle JetBrains Mono Regular via `include_bytes!` (~264KB)
7. **AppEvent**: Keep in `main.rs`

---

## Step 1: Add dependencies and `gui` feature flag

**File: `Cargo.toml` (workspace root)**
- Add `winit` and `wgpu` as workspace dependencies

**File: `crates/fresh-editor/Cargo.toml`**
- Add `gui` feature flag:
  ```toml
  gui = [
      "runtime",
      "dep:winit",
      "dep:wgpu",
      "dep:ratatui-wgpu",
  ]
  ```
- Add optional dependencies:
  ```toml
  winit = { version = "0.30", optional = true }
  wgpu = { version = "28.0", optional = true }
  ratatui-wgpu = { git = "https://github.com/Jesterhearts/ratatui-wgpu", rev = "cdcf5c6", optional = true }
  ```

## Step 2: Download and add the JetBrains Mono font file

**File: `crates/fresh-editor/fonts/JetBrainsMono-Regular.ttf`**
- Download JetBrains Mono Regular TTF from the official GitHub release
- Place in `crates/fresh-editor/fonts/`
- Add a `fonts/LICENSE-OFL.txt` with the SIL OFL 1.1 license text

## Step 3: Create the GUI module (`src/gui/mod.rs`)

**File: `crates/fresh-editor/src/gui/mod.rs`**

This module contains:

### 3a. Winit → Crossterm event translation functions

- `fn translate_key(winit::event::KeyEvent) -> Option<crossterm::event::KeyEvent>`
  - Maps `winit::keyboard::Key` → `crossterm::event::KeyCode`
  - Maps `winit::event::Modifiers` → `crossterm::event::KeyModifiers`
  - Handles named keys (Enter, Backspace, Tab, Escape, Arrow keys, Function keys, Home/End/PageUp/PageDown, Delete, Insert)
  - Handles character keys (including shifted characters)
  - Maps Super/Meta modifier to `KeyModifiers::SUPER`
  - Returns `None` for key release events (only processes press/repeat)

- `fn translate_mouse(winit::event::DeviceEvent or WindowEvent) -> Option<crossterm::event::MouseEvent>`
  - Maps mouse button presses/releases → `MouseEventKind::Down`/`Up` with `MouseButton`
  - Maps cursor moved → `MouseEventKind::Moved`
  - Maps scroll wheel → `MouseEventKind::ScrollUp`/`ScrollDown`
  - Translates pixel positions to cell positions using font metrics from the wgpu backend

### 3b. `WgpuApp` struct implementing winit's `ApplicationHandler`

```rust
struct WgpuApp {
    // Initialization state (set before event loop starts)
    config: config::Config,
    dir_context: DirectoryContext,
    file_locations: Vec<FileLocation>,
    show_file_explorer: bool,
    // ... other config/setup fields from CLI parsing

    // Runtime state (created in `resumed()`)
    state: Option<GuiState>,
}

struct GuiState {
    editor: Editor,
    terminal: Terminal<WgpuBackend<'static>>,
    window: Arc<Window>,
    needs_render: bool,
    last_render: Instant,
    workspace_enabled: bool,
}
```

**`ApplicationHandler` impl:**

- `fn resumed(&mut self, event_loop)`:
  1. Create `winit::Window` with title "Fresh" and reasonable default size
  2. Create `WgpuBackend` via `tokio::runtime::Runtime::block_on(Builder::from_font(...).build_with_target(window))`
  3. Create `Terminal::new(backend)`
  4. Create `Editor::new(...)` with config (same as terminal path)
  5. Store in `self.state = Some(GuiState { ... })`
  6. Request initial redraw

- `fn window_event(&mut self, event_loop, window_id, event)`:
  Match on `WindowEvent`:
  - `CloseRequested` → quit
  - `Resized(size)` → resize wgpu surface, update editor dimensions, request redraw
  - `KeyboardInput { event, .. }` → translate to crossterm KeyEvent, call `handle_key_event(editor, key_event)`
  - `MouseInput { state, button, .. }` → translate to crossterm MouseEvent, call `handle_mouse_event(editor, mouse_event)`
  - `MouseWheel { delta, .. }` → translate to scroll events
  - `CursorMoved { position, .. }` → track cursor position for mouse events
  - `RedrawRequested` → call `editor_tick()` then `terminal.draw(|f| editor.render(f))`

- `fn about_to_wait(&mut self, event_loop)`:
  - Call `editor_tick()` for housekeeping
  - If `needs_render`, request redraw
  - If `editor.should_quit()`, exit event loop

### 3c. `pub fn run_gui(args) -> AnyhowResult<()>`

The public entry point:
1. Parse CLI args and load config (shared with terminal path — this logic already runs before `initialize_app()`)
2. Create `WgpuApp` with config/args
3. Create `winit::event_loop::EventLoop`
4. Call `event_loop.run_app(&mut app)`

## Step 4: Extract shared per-tick logic from `run_event_loop_common`

**File: `crates/fresh-editor/src/main.rs`**

Extract the housekeeping block (lines 2737-2796 of current code) into:

```rust
/// Returns true if a render is needed after processing ticks.
/// The `clear_terminal` callback is called when a full redraw is requested.
fn editor_tick(editor: &mut Editor, workspace_enabled: bool, mut clear_terminal: impl FnMut() -> AnyhowResult<()>) -> AnyhowResult<bool> {
    let mut needs_render = false;

    if editor.process_async_messages() { needs_render = true; }
    if editor.process_pending_file_opens() { needs_render = true; }
    if editor.check_mouse_hover_timer() { needs_render = true; }
    if editor.check_semantic_highlight_timer() { needs_render = true; }
    if editor.check_completion_trigger_timer() { needs_render = true; }
    if editor.check_warning_log() { needs_render = true; }
    if editor.poll_stdin_streaming() { needs_render = true; }

    if let Err(e) = editor.auto_recovery_save_dirty_buffers() {
        tracing::debug!("Auto-recovery-save error: {}", e);
    }
    if let Err(e) = editor.auto_save_persistent_buffers() {
        tracing::debug!("Auto-save (disk) error: {}", e);
    }

    if editor.take_full_redraw_request() {
        clear_terminal()?;
        needs_render = true;
    }

    Ok(needs_render)
}
```

Then `run_event_loop_common` calls `editor_tick()` at the top of each loop iteration instead of inlining this code. The quit check + workspace save stays in the caller since the GUI path handles quit differently (via `event_loop.exit()`).

## Step 5: Wire up the `--gui` CLI flag

**File: `crates/fresh-editor/src/main.rs`**

- Add `--gui` flag to the `Cli` struct (only available when `gui` feature is enabled):
  ```rust
  #[cfg(feature = "gui")]
  #[arg(long, help = "Launch in GUI mode (native window)")]
  gui: bool,
  ```

- In `main()`, before `initialize_app()`, check if `--gui` is set:
  ```rust
  #[cfg(feature = "gui")]
  if cli.gui {
      return gui::run_gui(&cli);
  }
  ```
  This branches early, before any terminal mode setup.

## Step 6: Register the module in `lib.rs`

**File: `crates/fresh-editor/src/lib.rs`**

Add:
```rust
#[cfg(feature = "gui")]
pub mod gui;
```

## Step 7: Verify it compiles and runs

- `cargo build --features gui` — confirm compilation
- `cargo build` (default features) — confirm no regression to terminal mode
- `cargo run --features gui -- --gui` — confirm window opens and basic rendering works

---

## Files changed (summary)

| File | Change |
|---|---|
| `Cargo.toml` (workspace) | Add winit, wgpu workspace deps |
| `crates/fresh-editor/Cargo.toml` | Add `gui` feature, optional deps |
| `crates/fresh-editor/fonts/JetBrainsMono-Regular.ttf` | New: bundled font |
| `crates/fresh-editor/fonts/LICENSE-OFL.txt` | New: font license |
| `crates/fresh-editor/src/gui/mod.rs` | New: GUI backend module |
| `crates/fresh-editor/src/main.rs` | Extract `editor_tick()`, add `--gui` flag |
| `crates/fresh-editor/src/lib.rs` | Register `gui` module |

## What this does NOT change

- No changes to `Editor`, `app/input.rs`, `app/mouse_input.rs`, or any editor internals
- No changes to the `input/keybindings.rs` resolver
- No changes to `view/` rendering code
- No changes to `services/` (LSP, plugins, terminal emulation, etc.)
- Default `cargo build` is completely unaffected (gui deps are all optional)
