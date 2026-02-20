//! GUI backend integration for Fresh.
//!
//! This module provides a thin adapter between the [`fresh_gui`] crate
//! (windowed ratatui via winit + wgpu) and the editor core.  All windowing,
//! GPU, and input-translation logic lives in `fresh-gui`; this module only
//! implements the [`fresh_gui::GuiApplication`] trait for [`Editor`].

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result as AnyhowResult};
use crossterm::event::{
    KeyEvent as CtKeyEvent, KeyEventKind, KeyEventState, MouseEvent as CtMouseEvent,
};

use crate::app::Editor;
use crate::config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};

// Re-export helpers from fresh-gui so existing code (e.g. e2e tests) can
// continue to access them via `fresh::gui::*`.
pub use fresh_gui::{
    cell_dimensions_to_grid, pixel_to_cell, translate_key_event, translate_modifiers,
    translate_mouse_button, translate_named_key, GuiApplication, GuiConfig,
};

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
    let no_session_flag = no_session;

    // Move all captured state into the closure that creates the editor app.
    fresh_gui::run(GuiConfig::default(), move |cols, rows| {
        // For GUI, we always have true color.
        let color_capability = crate::view::color_support::ColorCapability::TrueColor;
        let filesystem: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);

        let mut editor = Editor::with_working_dir(
            loaded_config,
            cols,
            rows,
            Some(working_dir),
            dir_context,
            !no_plugins,
            color_capability,
            filesystem,
        )
        .context("Failed to create editor instance")?;

        // ratatui-wgpu does not render a hardware cursor.
        editor.set_software_cursor_only(true);

        let workspace_enabled = !no_session_flag && file_locations.is_empty();

        if !file_locations.is_empty() {
            for (path, line, col) in &file_locations {
                editor.queue_file_open(path.clone(), *line, *col);
            }
        } else if show_file_explorer {
            editor.show_file_explorer();
        }

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

        Ok(EditorApp {
            editor,
            workspace_enabled,
        })
    })
}

// ---------------------------------------------------------------------------
// GuiApplication implementation for Editor
// ---------------------------------------------------------------------------

struct EditorApp {
    editor: Editor,
    workspace_enabled: bool,
}

impl GuiApplication for EditorApp {
    fn on_key(&mut self, key_event: CtKeyEvent) -> AnyhowResult<()> {
        tracing::trace!(
            "GUI key event: code={:?}, modifiers={:?}",
            key_event.code,
            key_event.modifiers
        );

        // Event debug dialog intercepts ALL key events before normal processing.
        if self.editor.is_event_debug_active() {
            let raw_event = crossterm::event::KeyEvent {
                code: key_event.code,
                modifiers: key_event.modifiers,
                kind: KeyEventKind::Press,
                state: KeyEventState::NONE,
            };
            self.editor.handle_event_debug_input(&raw_event);
            return Ok(());
        }

        self.editor
            .handle_key(key_event.code, key_event.modifiers)?;
        Ok(())
    }

    fn on_mouse(&mut self, mouse: CtMouseEvent) -> AnyhowResult<bool> {
        self.editor.handle_mouse(mouse)
    }

    fn render(&mut self, frame: &mut ratatui::Frame) {
        self.editor.render(frame);
    }

    fn tick(&mut self) -> AnyhowResult<bool> {
        crate::app::editor_tick(&mut self.editor, || Ok(()))
    }

    fn should_quit(&self) -> bool {
        self.editor.should_quit()
    }

    fn resize(&mut self, cols: u16, rows: u16) {
        self.editor.resize(cols, rows);
    }

    fn on_close(&mut self) {
        if self.workspace_enabled {
            if let Err(e) = self.editor.save_workspace() {
                tracing::warn!("Failed to save workspace: {}", e);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// File-location parsing (editor-specific utility)
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

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
}
