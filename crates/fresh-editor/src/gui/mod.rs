//! Native GUI backend for Fresh using winit + wgpu via ratatui-wgpu.
//!
//! This module provides an alternative rendering backend that displays
//! the editor in a native GPU-accelerated window instead of a terminal.
//! All winit input events are translated to crossterm types so the
//! editor internals require zero changes.

use std::num::NonZeroU32;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::{Context, Result as AnyhowResult};
use crossterm::event::{
    KeyCode, KeyEvent as CtKeyEvent, KeyEventKind, KeyEventState, KeyModifiers,
    MediaKeyCode, ModifierKeyCode,
    MouseButton as CtMouseButton, MouseEvent as CtMouseEvent, MouseEventKind,
};
use ratatui::backend::Backend;
use ratatui::Terminal;
use ratatui_wgpu::{Builder, Dimensions, Font, WgpuBackend};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{Key, NamedKey};
use winit::keyboard::KeyLocation;
use winit::window::{Window, WindowAttributes, WindowId};

use crate::app::Editor;
use crate::config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};

/// Embedded JetBrains Mono Regular font (SIL Open Font License 1.1).
const FONT_DATA: &[u8] = include_bytes!("../../fonts/JetBrainsMono-Regular.ttf");

/// Default window dimensions.
const DEFAULT_WIDTH: u32 = 1280;
const DEFAULT_HEIGHT: u32 = 800;

/// Frame duration target (60fps).
const FRAME_DURATION: Duration = Duration::from_millis(16);

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Launch the editor in GUI mode. Called from `main()` when `--gui` is passed.
pub fn run_gui(
    files: &[String],
    no_plugins: bool,
    config_path: Option<&PathBuf>,
    locale: Option<&str>,
    no_session: bool,
    log_file: Option<&PathBuf>,
) -> AnyhowResult<()> {
    // Load configuration (same layered logic as terminal path, but without
    // terminal-specific setup like raw mode / alternate screen).
    if let Some(loc) = locale {
        rust_i18n::set_locale(loc);
    }

    // Set up tracing subscriber (same as terminal path)
    let log_path = log_file
        .cloned()
        .unwrap_or_else(crate::services::log_dirs::main_log_path);
    let _tracing_handles = crate::services::tracing_setup::init_global(&log_path);
    tracing::info!("GUI mode starting");

    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();

    let loaded_config = if let Some(path) = config_path {
        config::Config::load_from_file(path)
            .with_context(|| format!("Failed to load config from {}", path.display()))?
    } else {
        config::Config::load_with_layers(&dir_context, &working_dir)
    };

    let file_locations: Vec<(PathBuf, Option<usize>, Option<usize>)> =
        files.iter().map(|f| parse_file_location(f)).collect();

    let show_file_explorer = file_locations.is_empty();

    let event_loop = EventLoop::new().context("Failed to create winit event loop")?;
    event_loop.set_control_flow(winit::event_loop::ControlFlow::Poll);

    let mut app = WgpuApp {
        config: loaded_config,
        dir_context,
        file_locations,
        show_file_explorer,
        no_plugins,
        no_session,
        working_dir: Some(working_dir),
        state: None,
    };

    event_loop
        .run_app(&mut app)
        .context("winit event loop error")?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Application state
// ---------------------------------------------------------------------------

/// Holds pre-init configuration passed from CLI.
struct WgpuApp {
    config: config::Config,
    dir_context: DirectoryContext,
    file_locations: Vec<(PathBuf, Option<usize>, Option<usize>)>,
    show_file_explorer: bool,
    no_plugins: bool,
    no_session: bool,
    working_dir: Option<PathBuf>,

    /// Runtime state — created in `resumed()`.
    state: Option<GuiState>,
}

/// Runtime state that lives for the duration of the window.
struct GuiState {
    editor: Editor,
    terminal: Terminal<WgpuBackend<'static, 'static>>,
    window: Arc<Window>,
    needs_render: bool,
    last_render: Instant,
    workspace_enabled: bool,
    /// Current cursor position in pixels (tracked across CursorMoved events).
    cursor_position: (f64, f64),
    /// Current modifier state (tracked across ModifiersChanged events).
    modifiers: KeyModifiers,
    /// Tracks which mouse button is currently held (for drag detection).
    pressed_button: Option<CtMouseButton>,
    /// Cell dimensions in pixels (width, height) for pixel→cell conversion.
    cell_size: (f64, f64),
    /// Which Alt/Option key is currently held (for macOS Left/Right distinction).
    /// On macOS, Left Alt produces composed international characters while
    /// Right Alt is used as an Alt modifier for keyboard shortcuts.
    alt_location: Option<KeyLocation>,
}

// ---------------------------------------------------------------------------
// ApplicationHandler implementation
// ---------------------------------------------------------------------------

impl ApplicationHandler for WgpuApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return; // Already initialized
        }

        match self.create_gui_state(event_loop) {
            Ok(state) => {
                state.window.request_redraw();
                self.state = Some(state);
            }
            Err(e) => {
                tracing::error!("Failed to initialize GUI: {:#}", e);
                event_loop.exit();
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        let Some(state) = self.state.as_mut() else {
            return;
        };

        match event {
            WindowEvent::CloseRequested => {
                if state.workspace_enabled {
                    if let Err(e) = state.editor.save_workspace() {
                        tracing::warn!("Failed to save workspace: {}", e);
                    }
                }
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                if size.width > 0 && size.height > 0 {
                    state.terminal.backend_mut().resize(
                        size.width,
                        size.height,
                    );
                    // Re-derive cell size from the backend after resize
                    if let Ok(ws) = state.terminal.backend_mut().window_size() {
                        let cols = ws.columns_rows.width;
                        let rows = ws.columns_rows.height;
                        state.cell_size = (
                            ws.pixels.width as f64 / cols.max(1) as f64,
                            ws.pixels.height as f64 / rows.max(1) as f64,
                        );
                        state.editor.resize(cols, rows);
                    }
                    state.needs_render = true;
                }
            }

            WindowEvent::ModifiersChanged(mods) => {
                state.modifiers = translate_modifiers(&mods.state());
            }

            WindowEvent::KeyboardInput { event, .. } => {
                // Track Alt key location (before early return for Released)
                // so we know whether Left or Right Alt is held for macOS.
                if let Key::Named(NamedKey::Alt) = &event.logical_key {
                    match event.state {
                        ElementState::Pressed => {
                            state.alt_location = Some(event.location);
                        }
                        ElementState::Released => {
                            state.alt_location = None;
                        }
                    }
                }

                if event.state == ElementState::Released {
                    return;
                }
                if let Some(key_event) =
                    translate_key_event(&event, state.modifiers, state.alt_location)
                {
                    if let Err(e) = crate::gui::handle_key(
                        &mut state.editor,
                        key_event,
                    ) {
                        tracing::error!("Key handling error: {}", e);
                    }
                    state.needs_render = true;
                }
            }

            WindowEvent::MouseInput {
                state: btn_state,
                button,
                ..
            } => {
                if let Some(ct_btn) = translate_mouse_button(button) {
                    let kind = match btn_state {
                        ElementState::Pressed => {
                            state.pressed_button = Some(ct_btn);
                            MouseEventKind::Down(ct_btn)
                        }
                        ElementState::Released => {
                            state.pressed_button = None;
                            MouseEventKind::Up(ct_btn)
                        }
                    };
                    let (col, row) = pixel_to_cell(state.cursor_position, state.cell_size);
                    let mouse_event = CtMouseEvent {
                        kind,
                        column: col,
                        row,
                        modifiers: state.modifiers,
                    };
                    match state.editor.handle_mouse(mouse_event) {
                        Ok(true) => state.needs_render = true,
                        Ok(false) => {}
                        Err(e) => tracing::error!("Mouse handling error: {}", e),
                    }
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                state.cursor_position = (position.x, position.y);
                let (col, row) = pixel_to_cell(state.cursor_position, state.cell_size);
                let kind = match state.pressed_button {
                    Some(btn) => MouseEventKind::Drag(btn),
                    None => MouseEventKind::Moved,
                };
                let mouse_event = CtMouseEvent {
                    kind,
                    column: col,
                    row,
                    modifiers: state.modifiers,
                };
                match state.editor.handle_mouse(mouse_event) {
                    Ok(true) => state.needs_render = true,
                    Ok(false) => {}
                    Err(e) => tracing::error!("Mouse handling error: {}", e),
                }
            }

            WindowEvent::MouseWheel { delta, .. } => {
                let (h_lines, v_lines) = match delta {
                    MouseScrollDelta::LineDelta(h, v) => (h as i32, v as i32),
                    MouseScrollDelta::PixelDelta(pos) => {
                        // Approximate: 1 line ≈ cell height in pixels
                        let line_h = state.cell_size.1.max(1.0);
                        ((pos.x / line_h) as i32, (pos.y / line_h) as i32)
                    }
                };
                let (col, row) = pixel_to_cell(state.cursor_position, state.cell_size);

                // Vertical scroll
                for _ in 0..v_lines.unsigned_abs() {
                    let kind = if v_lines > 0 {
                        MouseEventKind::ScrollUp
                    } else {
                        MouseEventKind::ScrollDown
                    };
                    let mouse_event = CtMouseEvent {
                        kind,
                        column: col,
                        row,
                        modifiers: state.modifiers,
                    };
                    match state.editor.handle_mouse(mouse_event) {
                        Ok(true) => state.needs_render = true,
                        Ok(false) => {}
                        Err(e) => tracing::error!("Scroll handling error: {}", e),
                    }
                }

                // Horizontal scroll
                for _ in 0..h_lines.unsigned_abs() {
                    let kind = if h_lines > 0 {
                        MouseEventKind::ScrollRight
                    } else {
                        MouseEventKind::ScrollLeft
                    };
                    let mouse_event = CtMouseEvent {
                        kind,
                        column: col,
                        row,
                        modifiers: state.modifiers,
                    };
                    match state.editor.handle_mouse(mouse_event) {
                        Ok(true) => state.needs_render = true,
                        Ok(false) => {}
                        Err(e) => tracing::error!("Scroll handling error: {}", e),
                    }
                }
            }

            WindowEvent::RedrawRequested => {
                if state.needs_render && state.last_render.elapsed() >= FRAME_DURATION {
                    if let Err(e) = state.terminal.draw(|frame| state.editor.render(frame)) {
                        tracing::error!("Render error: {}", e);
                    }
                    state.last_render = Instant::now();
                    state.needs_render = false;
                }
            }

            _ => {}
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(state) = self.state.as_mut() else {
            return;
        };

        // Run shared per-tick housekeeping.
        match crate::app::editor_tick(&mut state.editor, || Ok(())) {
            Ok(true) => state.needs_render = true,
            Ok(false) => {}
            Err(e) => tracing::error!("Tick error: {}", e),
        }

        if state.editor.should_quit() {
            if state.workspace_enabled {
                if let Err(e) = state.editor.save_workspace() {
                    tracing::warn!("Failed to save workspace: {}", e);
                }
            }
            event_loop.exit();
            return;
        }

        if state.needs_render {
            state.window.request_redraw();
        }
    }
}

// ---------------------------------------------------------------------------
// GUI state initialization
// ---------------------------------------------------------------------------

impl WgpuApp {
    fn create_gui_state(&mut self, event_loop: &ActiveEventLoop) -> AnyhowResult<GuiState> {
        let window_attrs = WindowAttributes::default()
            .with_title("Fresh")
            .with_inner_size(winit::dpi::PhysicalSize::new(DEFAULT_WIDTH, DEFAULT_HEIGHT));

        let window = Arc::new(
            event_loop
                .create_window(window_attrs)
                .context("Failed to create window")?,
        );

        let size = window.inner_size();

        // Build the wgpu backend. This is async internally (adapter/device request)
        // so we block on it using a one-shot tokio runtime.
        let rt = tokio::runtime::Runtime::new().context("Failed to create tokio runtime")?;
        let font = Font::new(FONT_DATA).context("Failed to load embedded font")?;

        let backend = rt.block_on(
            Builder::from_font(font)
                .with_width_and_height(Dimensions {
                    width: NonZeroU32::new(size.width).unwrap_or(NonZeroU32::new(1).unwrap()),
                    height: NonZeroU32::new(size.height).unwrap_or(NonZeroU32::new(1).unwrap()),
                })
                .build_with_target(window.clone()),
        )
        .context("Failed to create wgpu backend")?;

        let mut terminal = Terminal::new(backend).context("Failed to create ratatui terminal")?;

        // Query actual cell dimensions from the backend's font metrics rather
        // than guessing with hardcoded ratios.  window_size() returns both the
        // grid size (cols × rows) and the pixel size, so dividing gives the
        // exact cell size used for text layout.
        let win_size = terminal
            .backend_mut()
            .window_size()
            .context("Failed to query window size from backend")?;
        let cols = win_size.columns_rows.width;
        let rows = win_size.columns_rows.height;
        let cell_size = (
            win_size.pixels.width as f64 / cols.max(1) as f64,
            win_size.pixels.height as f64 / rows.max(1) as f64,
        );

        // For GUI, we always have true color.
        let color_capability = crate::view::color_support::ColorCapability::TrueColor;

        let filesystem: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);

        let mut editor = Editor::with_working_dir(
            self.config.clone(),
            cols,
            rows,
            self.working_dir.clone(),
            self.dir_context.clone(),
            !self.no_plugins,
            color_capability,
            filesystem,
        )
        .context("Failed to create editor instance")?;

        // ratatui-wgpu does not render a hardware cursor, so enable GUI mode
        // to ensure software cursor indicators are always visible.
        editor.set_gui_mode(true);

        let workspace_enabled = !self.no_session && self.file_locations.is_empty();

        // Open files passed on CLI
        if !self.file_locations.is_empty() {
            for (path, line, col) in &self.file_locations {
                editor.queue_file_open(path.clone(), *line, *col);
            }
        } else if self.show_file_explorer {
            editor.show_file_explorer();
        }

        // Workspace restore
        if workspace_enabled {
            match editor.try_restore_workspace() {
                Ok(true) => tracing::info!("Workspace restored"),
                Ok(false) => tracing::debug!("No previous workspace"),
                Err(e) => tracing::warn!("Failed to restore workspace: {}", e),
            }
        }

        if let Err(e) = editor.start_recovery_session() {
            tracing::warn!("Failed to start recovery session: {}", e);
        }

        Ok(GuiState {
            editor,
            terminal,
            window,
            needs_render: true,
            last_render: Instant::now(),
            workspace_enabled,
            cursor_position: (0.0, 0.0),
            modifiers: KeyModifiers::NONE,
            pressed_button: None,
            cell_size,
            alt_location: None,
        })
    }
}

// ---------------------------------------------------------------------------
// Input translation helpers (pub(crate) for e2e testing)
// ---------------------------------------------------------------------------

/// Parse a CLI file argument in `file:line:col` format.
pub fn parse_file_location(f: &str) -> (PathBuf, Option<usize>, Option<usize>) {
    let parts: Vec<&str> = f.rsplitn(3, ':').collect();
    match parts.as_slice() {
        [col, line, path] => {
            let l = line.parse().ok();
            let c = col.parse().ok();
            if l.is_some() {
                (PathBuf::from(path), l, c)
            } else {
                (PathBuf::from(f), None, None)
            }
        }
        [line, path] => {
            let l = line.parse().ok();
            if l.is_some() {
                (PathBuf::from(path), l, None)
            } else {
                (PathBuf::from(f), None, None)
            }
        }
        _ => (PathBuf::from(f), None, None),
    }
}

/// Handle a translated key event (mirrors main.rs handle_key_event).
fn handle_key(editor: &mut Editor, key_event: CtKeyEvent) -> AnyhowResult<()> {
    tracing::trace!(
        "GUI key event: code={:?}, modifiers={:?}",
        key_event.code,
        key_event.modifiers
    );

    // Event debug dialog intercepts ALL key events before normal processing,
    // mirroring the same priority logic in the terminal event loop (main.rs).
    if editor.is_event_debug_active() {
        let raw_event = crossterm::event::KeyEvent {
            code: key_event.code,
            modifiers: key_event.modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        };
        editor.handle_event_debug_input(&raw_event);
        return Ok(());
    }

    editor.handle_key(key_event.code, key_event.modifiers)?;
    Ok(())
}

/// Convert winit modifier state to crossterm KeyModifiers.
pub fn translate_modifiers(mods: &winit::keyboard::ModifiersState) -> KeyModifiers {
    let mut result = KeyModifiers::NONE;
    if mods.shift_key() {
        result |= KeyModifiers::SHIFT;
    }
    if mods.control_key() {
        result |= KeyModifiers::CONTROL;
    }
    if mods.alt_key() {
        result |= KeyModifiers::ALT;
    }
    if mods.super_key() {
        result |= KeyModifiers::SUPER;
    }
    result
}

/// Translate a winit key event to a crossterm KeyEvent.
///
/// `alt_location` tracks which Alt/Option key is held, used on macOS to
/// distinguish Left Alt (international character composition) from Right Alt
/// (keyboard shortcut modifier).
pub fn translate_key_event(
    event: &winit::event::KeyEvent,
    modifiers: KeyModifiers,
    alt_location: Option<KeyLocation>,
) -> Option<CtKeyEvent> {
    // On macOS, Left Option is used for international character input while
    // Right Option acts as an Alt modifier for keyboard shortcuts.
    let (effective_modifiers, alt_override_char) = if cfg!(target_os = "macos")
        && modifiers.contains(KeyModifiers::ALT)
    {
        match alt_location {
            Some(KeyLocation::Left) => {
                // Left Alt: strip ALT so the composed character (å, ñ, …)
                // is treated as plain text input rather than a shortcut.
                (modifiers & !KeyModifiers::ALT, None)
            }
            Some(KeyLocation::Right) => {
                // Right Alt: keep ALT for shortcuts, but undo the macOS
                // Option-composition by deriving the base character from
                // the physical key.  This way Right-Option+F produces
                // Alt+f instead of Alt+ƒ.
                let base = physical_key_to_base_char(
                    &event.physical_key,
                    modifiers.contains(KeyModifiers::SHIFT),
                );
                (modifiers, base)
            }
            _ => (modifiers, None),
        }
    } else {
        (modifiers, None)
    };

    let code = match &event.logical_key {
        Key::Named(named) => {
            translate_named_key(named, &event.location, effective_modifiers)?
        }
        Key::Character(ch) => {
            let c = alt_override_char.unwrap_or_else(|| ch.chars().next().unwrap_or('\0'));
            if c == '\0' {
                return None;
            }
            // Detect Shift+Tab → BackTab
            if c == '\t' && effective_modifiers.contains(KeyModifiers::SHIFT) {
                KeyCode::BackTab
            } else {
                KeyCode::Char(c)
            }
        }
        Key::Dead(_) | Key::Unidentified(_) => return None,
    };

    Some(CtKeyEvent {
        code,
        modifiers: effective_modifiers,
        kind: KeyEventKind::Press,
        state: KeyEventState::NONE,
    })
}

/// Map a winit physical key to its base US-layout character.
///
/// Used on macOS to undo the Option-key composition for Right Alt shortcuts.
/// Returns `None` for keys that don't have a simple character mapping (those
/// are handled as named keys and don't need this override).
fn physical_key_to_base_char(
    key: &winit::keyboard::PhysicalKey,
    shift: bool,
) -> Option<char> {
    use winit::keyboard::KeyCode as WK;
    use winit::keyboard::PhysicalKey;

    let PhysicalKey::Code(code) = key else {
        return None;
    };

    let base = match code {
        WK::KeyA => 'a',
        WK::KeyB => 'b',
        WK::KeyC => 'c',
        WK::KeyD => 'd',
        WK::KeyE => 'e',
        WK::KeyF => 'f',
        WK::KeyG => 'g',
        WK::KeyH => 'h',
        WK::KeyI => 'i',
        WK::KeyJ => 'j',
        WK::KeyK => 'k',
        WK::KeyL => 'l',
        WK::KeyM => 'm',
        WK::KeyN => 'n',
        WK::KeyO => 'o',
        WK::KeyP => 'p',
        WK::KeyQ => 'q',
        WK::KeyR => 'r',
        WK::KeyS => 's',
        WK::KeyT => 't',
        WK::KeyU => 'u',
        WK::KeyV => 'v',
        WK::KeyW => 'w',
        WK::KeyX => 'x',
        WK::KeyY => 'y',
        WK::KeyZ => 'z',
        WK::Digit0 => '0',
        WK::Digit1 => '1',
        WK::Digit2 => '2',
        WK::Digit3 => '3',
        WK::Digit4 => '4',
        WK::Digit5 => '5',
        WK::Digit6 => '6',
        WK::Digit7 => '7',
        WK::Digit8 => '8',
        WK::Digit9 => '9',
        WK::Minus => '-',
        WK::Equal => '=',
        WK::BracketLeft => '[',
        WK::BracketRight => ']',
        WK::Backslash => '\\',
        WK::Semicolon => ';',
        WK::Quote => '\'',
        WK::Comma => ',',
        WK::Period => '.',
        WK::Slash => '/',
        WK::Backquote => '`',
        _ => return None,
    };

    if shift && base.is_ascii_alphabetic() {
        Some(base.to_ascii_uppercase())
    } else {
        Some(base)
    }
}

/// Translate a winit NamedKey to a crossterm KeyCode.
pub fn translate_named_key(
    key: &NamedKey,
    location: &winit::keyboard::KeyLocation,
    modifiers: KeyModifiers,
) -> Option<KeyCode> {
    use winit::keyboard::KeyLocation;

    Some(match key {
        // Navigation
        NamedKey::ArrowUp => KeyCode::Up,
        NamedKey::ArrowDown => KeyCode::Down,
        NamedKey::ArrowLeft => KeyCode::Left,
        NamedKey::ArrowRight => KeyCode::Right,
        NamedKey::Home => KeyCode::Home,
        NamedKey::End => KeyCode::End,
        NamedKey::PageUp => KeyCode::PageUp,
        NamedKey::PageDown => KeyCode::PageDown,

        // Editing
        NamedKey::Backspace => KeyCode::Backspace,
        NamedKey::Delete => KeyCode::Delete,
        NamedKey::Insert => KeyCode::Insert,
        NamedKey::Enter => KeyCode::Enter,
        NamedKey::Tab => {
            if modifiers.contains(KeyModifiers::SHIFT) {
                KeyCode::BackTab
            } else {
                KeyCode::Tab
            }
        }
        NamedKey::Space => KeyCode::Char(' '),
        NamedKey::Escape => KeyCode::Esc,

        // Function keys
        NamedKey::F1 => KeyCode::F(1),
        NamedKey::F2 => KeyCode::F(2),
        NamedKey::F3 => KeyCode::F(3),
        NamedKey::F4 => KeyCode::F(4),
        NamedKey::F5 => KeyCode::F(5),
        NamedKey::F6 => KeyCode::F(6),
        NamedKey::F7 => KeyCode::F(7),
        NamedKey::F8 => KeyCode::F(8),
        NamedKey::F9 => KeyCode::F(9),
        NamedKey::F10 => KeyCode::F(10),
        NamedKey::F11 => KeyCode::F(11),
        NamedKey::F12 => KeyCode::F(12),
        NamedKey::F13 => KeyCode::F(13),
        NamedKey::F14 => KeyCode::F(14),
        NamedKey::F15 => KeyCode::F(15),
        NamedKey::F16 => KeyCode::F(16),
        NamedKey::F17 => KeyCode::F(17),
        NamedKey::F18 => KeyCode::F(18),
        NamedKey::F19 => KeyCode::F(19),
        NamedKey::F20 => KeyCode::F(20),
        NamedKey::F21 => KeyCode::F(21),
        NamedKey::F22 => KeyCode::F(22),
        NamedKey::F23 => KeyCode::F(23),
        NamedKey::F24 => KeyCode::F(24),
        NamedKey::F25 => KeyCode::F(25),
        NamedKey::F26 => KeyCode::F(26),
        NamedKey::F27 => KeyCode::F(27),
        NamedKey::F28 => KeyCode::F(28),
        NamedKey::F29 => KeyCode::F(29),
        NamedKey::F30 => KeyCode::F(30),
        NamedKey::F31 => KeyCode::F(31),
        NamedKey::F32 => KeyCode::F(32),
        NamedKey::F33 => KeyCode::F(33),
        NamedKey::F34 => KeyCode::F(34),
        NamedKey::F35 => KeyCode::F(35),

        // Lock keys
        NamedKey::CapsLock => KeyCode::CapsLock,
        NamedKey::NumLock => KeyCode::NumLock,
        NamedKey::ScrollLock => KeyCode::ScrollLock,

        // Misc
        NamedKey::PrintScreen => KeyCode::PrintScreen,
        NamedKey::Pause => KeyCode::Pause,
        NamedKey::ContextMenu => KeyCode::Menu,

        // Media keys
        NamedKey::MediaPlay => KeyCode::Media(MediaKeyCode::Play),
        NamedKey::MediaPause => KeyCode::Media(MediaKeyCode::Pause),
        NamedKey::MediaPlayPause => KeyCode::Media(MediaKeyCode::PlayPause),
        NamedKey::MediaStop => KeyCode::Media(MediaKeyCode::Stop),
        NamedKey::MediaTrackNext => KeyCode::Media(MediaKeyCode::TrackNext),
        NamedKey::MediaTrackPrevious => KeyCode::Media(MediaKeyCode::TrackPrevious),
        NamedKey::MediaFastForward => KeyCode::Media(MediaKeyCode::FastForward),
        NamedKey::MediaRewind => KeyCode::Media(MediaKeyCode::Rewind),
        NamedKey::MediaRecord => KeyCode::Media(MediaKeyCode::Record),
        NamedKey::AudioVolumeDown => KeyCode::Media(MediaKeyCode::LowerVolume),
        NamedKey::AudioVolumeUp => KeyCode::Media(MediaKeyCode::RaiseVolume),
        NamedKey::AudioVolumeMute => KeyCode::Media(MediaKeyCode::MuteVolume),

        // Modifier keys emitted as KeyCode::Modifier with left/right
        NamedKey::Shift => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightShift,
                _ => ModifierKeyCode::LeftShift,
            };
            KeyCode::Modifier(side)
        }
        NamedKey::Control => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightControl,
                _ => ModifierKeyCode::LeftControl,
            };
            KeyCode::Modifier(side)
        }
        NamedKey::Alt => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightAlt,
                _ => ModifierKeyCode::LeftAlt,
            };
            KeyCode::Modifier(side)
        }
        NamedKey::Super => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightSuper,
                _ => ModifierKeyCode::LeftSuper,
            };
            KeyCode::Modifier(side)
        }
        NamedKey::Hyper => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightHyper,
                _ => ModifierKeyCode::LeftHyper,
            };
            KeyCode::Modifier(side)
        }
        NamedKey::Meta => {
            let side = match location {
                KeyLocation::Right => ModifierKeyCode::RightMeta,
                _ => ModifierKeyCode::LeftMeta,
            };
            KeyCode::Modifier(side)
        }

        // All other named keys (TV remote, browser, phone, etc.) — skip
        _ => return None,
    })
}

/// Translate a winit mouse button to a crossterm mouse button.
pub fn translate_mouse_button(button: MouseButton) -> Option<CtMouseButton> {
    match button {
        MouseButton::Left => Some(CtMouseButton::Left),
        MouseButton::Right => Some(CtMouseButton::Right),
        MouseButton::Middle => Some(CtMouseButton::Middle),
        // Back, Forward, Other — no crossterm equivalent
        _ => None,
    }
}

/// Convert pixel coordinates to terminal cell coordinates.
pub fn pixel_to_cell(pixel: (f64, f64), cell_size: (f64, f64)) -> (u16, u16) {
    let col = (pixel.0 / cell_size.0.max(1.0)) as u16;
    let row = (pixel.1 / cell_size.1.max(1.0)) as u16;
    (col, row)
}

/// Convert window pixel dimensions to terminal grid dimensions (cols, rows).
pub fn cell_dimensions_to_grid(width: f64, height: f64, cell_size: (f64, f64)) -> (u16, u16) {
    let cols = (width / cell_size.0.max(1.0)) as u16;
    let rows = (height / cell_size.1.max(1.0)) as u16;
    (cols.max(1), rows.max(1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use winit::keyboard::KeyLocation;

    // -----------------------------------------------------------------------
    // translate_named_key (tests the core key mapping logic directly)
    // -----------------------------------------------------------------------

    #[test]
    fn test_named_key_navigation() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(translate_named_key(&NamedKey::ArrowUp, &loc, mods), Some(KeyCode::Up));
        assert_eq!(translate_named_key(&NamedKey::ArrowDown, &loc, mods), Some(KeyCode::Down));
        assert_eq!(translate_named_key(&NamedKey::ArrowLeft, &loc, mods), Some(KeyCode::Left));
        assert_eq!(translate_named_key(&NamedKey::ArrowRight, &loc, mods), Some(KeyCode::Right));
        assert_eq!(translate_named_key(&NamedKey::Home, &loc, mods), Some(KeyCode::Home));
        assert_eq!(translate_named_key(&NamedKey::End, &loc, mods), Some(KeyCode::End));
        assert_eq!(translate_named_key(&NamedKey::PageUp, &loc, mods), Some(KeyCode::PageUp));
        assert_eq!(translate_named_key(&NamedKey::PageDown, &loc, mods), Some(KeyCode::PageDown));
    }

    #[test]
    fn test_named_key_editing() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(translate_named_key(&NamedKey::Backspace, &loc, mods), Some(KeyCode::Backspace));
        assert_eq!(translate_named_key(&NamedKey::Delete, &loc, mods), Some(KeyCode::Delete));
        assert_eq!(translate_named_key(&NamedKey::Insert, &loc, mods), Some(KeyCode::Insert));
        assert_eq!(translate_named_key(&NamedKey::Enter, &loc, mods), Some(KeyCode::Enter));
        assert_eq!(translate_named_key(&NamedKey::Escape, &loc, mods), Some(KeyCode::Esc));
        assert_eq!(translate_named_key(&NamedKey::Space, &loc, mods), Some(KeyCode::Char(' ')));
    }

    #[test]
    fn test_tab_and_backtab() {
        let loc = KeyLocation::Standard;
        assert_eq!(
            translate_named_key(&NamedKey::Tab, &loc, KeyModifiers::NONE),
            Some(KeyCode::Tab)
        );
        assert_eq!(
            translate_named_key(&NamedKey::Tab, &loc, KeyModifiers::SHIFT),
            Some(KeyCode::BackTab)
        );
    }

    #[test]
    fn test_function_keys() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(translate_named_key(&NamedKey::F1, &loc, mods), Some(KeyCode::F(1)));
        assert_eq!(translate_named_key(&NamedKey::F5, &loc, mods), Some(KeyCode::F(5)));
        assert_eq!(translate_named_key(&NamedKey::F12, &loc, mods), Some(KeyCode::F(12)));
        assert_eq!(translate_named_key(&NamedKey::F24, &loc, mods), Some(KeyCode::F(24)));
        assert_eq!(translate_named_key(&NamedKey::F35, &loc, mods), Some(KeyCode::F(35)));
    }

    #[test]
    fn test_lock_and_misc_keys() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(translate_named_key(&NamedKey::CapsLock, &loc, mods), Some(KeyCode::CapsLock));
        assert_eq!(translate_named_key(&NamedKey::NumLock, &loc, mods), Some(KeyCode::NumLock));
        assert_eq!(translate_named_key(&NamedKey::ScrollLock, &loc, mods), Some(KeyCode::ScrollLock));
        assert_eq!(translate_named_key(&NamedKey::PrintScreen, &loc, mods), Some(KeyCode::PrintScreen));
        assert_eq!(translate_named_key(&NamedKey::Pause, &loc, mods), Some(KeyCode::Pause));
        assert_eq!(translate_named_key(&NamedKey::ContextMenu, &loc, mods), Some(KeyCode::Menu));
    }

    #[test]
    fn test_modifier_keys_left_right() {
        let mods = KeyModifiers::NONE;

        // Left side
        assert_eq!(
            translate_named_key(&NamedKey::Shift, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftShift))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Control, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftControl))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Alt, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftAlt))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Super, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftSuper))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Hyper, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftHyper))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Meta, &KeyLocation::Left, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::LeftMeta))
        );

        // Right side
        assert_eq!(
            translate_named_key(&NamedKey::Shift, &KeyLocation::Right, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::RightShift))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Control, &KeyLocation::Right, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::RightControl))
        );
        assert_eq!(
            translate_named_key(&NamedKey::Super, &KeyLocation::Right, mods),
            Some(KeyCode::Modifier(ModifierKeyCode::RightSuper))
        );
    }

    #[test]
    fn test_media_keys() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(
            translate_named_key(&NamedKey::MediaPlay, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::Play))
        );
        assert_eq!(
            translate_named_key(&NamedKey::MediaPause, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::Pause))
        );
        assert_eq!(
            translate_named_key(&NamedKey::MediaPlayPause, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::PlayPause))
        );
        assert_eq!(
            translate_named_key(&NamedKey::MediaStop, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::Stop))
        );
        assert_eq!(
            translate_named_key(&NamedKey::AudioVolumeUp, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::RaiseVolume))
        );
        assert_eq!(
            translate_named_key(&NamedKey::AudioVolumeDown, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::LowerVolume))
        );
        assert_eq!(
            translate_named_key(&NamedKey::AudioVolumeMute, &loc, mods),
            Some(KeyCode::Media(MediaKeyCode::MuteVolume))
        );
    }

    #[test]
    fn test_unknown_named_key_returns_none() {
        let loc = KeyLocation::Standard;
        let mods = KeyModifiers::NONE;
        assert_eq!(translate_named_key(&NamedKey::BrowserBack, &loc, mods), None);
        assert_eq!(translate_named_key(&NamedKey::LaunchMail, &loc, mods), None);
    }

    // -----------------------------------------------------------------------
    // translate_modifiers
    // -----------------------------------------------------------------------

    #[test]
    fn test_translate_modifiers_none() {
        let mods = winit::keyboard::ModifiersState::empty();
        assert_eq!(translate_modifiers(&mods), KeyModifiers::NONE);
    }

    #[test]
    fn test_translate_modifiers_all() {
        let mods = winit::keyboard::ModifiersState::SHIFT
            | winit::keyboard::ModifiersState::CONTROL
            | winit::keyboard::ModifiersState::ALT
            | winit::keyboard::ModifiersState::SUPER;
        let result = translate_modifiers(&mods);
        assert!(result.contains(KeyModifiers::SHIFT));
        assert!(result.contains(KeyModifiers::CONTROL));
        assert!(result.contains(KeyModifiers::ALT));
        assert!(result.contains(KeyModifiers::SUPER));
    }

    #[test]
    fn test_translate_modifiers_single() {
        assert_eq!(
            translate_modifiers(&winit::keyboard::ModifiersState::CONTROL),
            KeyModifiers::CONTROL
        );
        assert_eq!(
            translate_modifiers(&winit::keyboard::ModifiersState::ALT),
            KeyModifiers::ALT
        );
        assert_eq!(
            translate_modifiers(&winit::keyboard::ModifiersState::SUPER),
            KeyModifiers::SUPER
        );
    }

    // -----------------------------------------------------------------------
    // translate_mouse_button
    // -----------------------------------------------------------------------

    #[test]
    fn test_translate_mouse_buttons() {
        assert_eq!(translate_mouse_button(MouseButton::Left), Some(CtMouseButton::Left));
        assert_eq!(translate_mouse_button(MouseButton::Right), Some(CtMouseButton::Right));
        assert_eq!(translate_mouse_button(MouseButton::Middle), Some(CtMouseButton::Middle));
        assert_eq!(translate_mouse_button(MouseButton::Back), None);
        assert_eq!(translate_mouse_button(MouseButton::Forward), None);
        assert_eq!(translate_mouse_button(MouseButton::Other(42)), None);
    }

    // -----------------------------------------------------------------------
    // pixel_to_cell
    // -----------------------------------------------------------------------

    #[test]
    fn test_pixel_to_cell_basic() {
        let cell_size = (10.0, 20.0);
        assert_eq!(pixel_to_cell((0.0, 0.0), cell_size), (0, 0));
        assert_eq!(pixel_to_cell((10.0, 20.0), cell_size), (1, 1));
        assert_eq!(pixel_to_cell((25.0, 45.0), cell_size), (2, 2));
        assert_eq!(pixel_to_cell((99.0, 199.0), cell_size), (9, 9));
    }

    #[test]
    fn test_pixel_to_cell_zero_cell_size() {
        // Should not panic — cell_size.max(1.0) protects against division by zero
        let result = pixel_to_cell((100.0, 100.0), (0.0, 0.0));
        assert_eq!(result, (100, 100));
    }

    // -----------------------------------------------------------------------
    // cell_dimensions_to_grid
    // -----------------------------------------------------------------------

    #[test]
    fn test_cell_dimensions_to_grid() {
        let cell_size = (14.4, 28.8); // 24px font * 0.6, 24px * 1.2
        let (cols, rows) = cell_dimensions_to_grid(1280.0, 800.0, cell_size);
        assert_eq!(cols, 88); // 1280 / 14.4 = 88.88 → 88
        assert_eq!(rows, 27); // 800 / 28.8 = 27.77 → 27
    }

    #[test]
    fn test_cell_dimensions_to_grid_minimum() {
        // Very small window should still return at least 1x1
        let (cols, rows) = cell_dimensions_to_grid(1.0, 1.0, (14.4, 28.8));
        assert_eq!(cols, 1);
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_cell_dimensions_to_grid_zero_size() {
        let (cols, rows) = cell_dimensions_to_grid(0.0, 0.0, (14.4, 28.8));
        assert_eq!(cols, 1); // max(1) ensures minimum
        assert_eq!(rows, 1);
    }

    // -----------------------------------------------------------------------
    // parse_file_location
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_file_location_plain() {
        let (path, line, col) = parse_file_location("src/main.rs");
        assert_eq!(path, PathBuf::from("src/main.rs"));
        assert_eq!(line, None);
        assert_eq!(col, None);
    }

    #[test]
    fn test_parse_file_location_line_col() {
        let (path, line, col) = parse_file_location("src/main.rs:42:10");
        assert_eq!(path, PathBuf::from("src/main.rs"));
        assert_eq!(line, Some(42));
        assert_eq!(col, Some(10));
    }

    #[test]
    fn test_parse_file_location_line_only() {
        let (path, line, col) = parse_file_location("src/main.rs:42");
        assert_eq!(path, PathBuf::from("src/main.rs"));
        assert_eq!(line, Some(42));
        assert_eq!(col, None);
    }

    #[test]
    fn test_parse_file_location_non_numeric() {
        let (path, line, col) = parse_file_location("foo:bar");
        assert_eq!(path, PathBuf::from("foo:bar"));
        assert_eq!(line, None);
        assert_eq!(col, None);
    }

    // -----------------------------------------------------------------------
    // physical_key_to_base_char
    // -----------------------------------------------------------------------

    #[test]
    fn test_physical_key_to_base_char_letters() {
        use winit::keyboard::{KeyCode as WK, PhysicalKey};

        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::KeyA), false),
            Some('a')
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::KeyZ), false),
            Some('z')
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::KeyF), true),
            Some('F')
        );
    }

    #[test]
    fn test_physical_key_to_base_char_digits() {
        use winit::keyboard::{KeyCode as WK, PhysicalKey};

        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Digit0), false),
            Some('0')
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Digit9), false),
            Some('9')
        );
        // Shift doesn't change digits
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Digit5), true),
            Some('5')
        );
    }

    #[test]
    fn test_physical_key_to_base_char_punctuation() {
        use winit::keyboard::{KeyCode as WK, PhysicalKey};

        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Comma), false),
            Some(',')
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Slash), false),
            Some('/')
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Backquote), false),
            Some('`')
        );
    }

    #[test]
    fn test_physical_key_to_base_char_unknown_returns_none() {
        use winit::keyboard::{KeyCode as WK, PhysicalKey};

        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Enter), false),
            None
        );
        assert_eq!(
            physical_key_to_base_char(&PhysicalKey::Code(WK::Space), false),
            None
        );
    }
}
