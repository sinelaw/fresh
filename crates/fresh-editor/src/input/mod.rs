//! Input pipeline
//!
//! This module handles the input-to-action-to-event translation.

/// Whether a key event is a keystroke the editor should act on.
///
/// A terminal that is not reporting event types only ever sends presses, so
/// this is normally trivially true. With the kitty keyboard protocol's
/// event-type reporting on (`keyboard_report_event_types`), one physical
/// keystroke instead produces a press, a repeat per auto-repeat tick while the
/// key is held, and a release. Presses and repeats are keystrokes; a release is
/// not — acting on it runs every key twice (sinelaw/fresh#2796), and ignoring
/// repeats would stop held keys from repeating.
///
/// This is the single gate for that distinction: every raw-event entry point
/// (the local event loop, the session server, the editor's own dispatch) must
/// go through it rather than comparing against `Press` directly.
pub fn is_keystroke(kind: crossterm::event::KeyEventKind) -> bool {
    use crossterm::event::KeyEventKind;
    matches!(kind, KeyEventKind::Press | KeyEventKind::Repeat)
}

pub mod actions;
pub mod buffer_mode;
pub mod command_registry;
pub mod commands;
pub mod composite_router;
pub mod fuzzy;
pub mod handler;
pub mod input_history;
pub mod key_translator;
pub mod keybindings;
mod line_move;
pub mod multi_cursor;
pub mod position_history;
pub mod quick_open;

#[cfg(test)]
pub mod tests_language_features;
