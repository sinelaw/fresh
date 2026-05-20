//! Shared key-event execution for scenario runners.
//!
//! `KeySpec` / `KeyMods` are deliberately crossterm-free data (see
//! `input_event.rs`). Every runner that routes a raw `SendKey`
//! through the production `handle_key` path needs the same three
//! steps: map the spec onto `crossterm`, call
//! `EditorTestHarness::send_key`, and wrap the error as an
//! `InputProjectionFailed`. That block lives here once instead of
//! being copy-pasted into each `dispatch_*`.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::{KeyMods, KeySpec};

/// Route a single `SendKey` event through the production key handler.
///
/// Callers that need fresh layout between keystrokes (e.g.
/// `LayoutScenario`) render after this returns; the send itself is
/// the same everywhere, so only the translation + error wrapping is
/// shared.
pub(crate) fn send_key_event(
    harness: &mut EditorTestHarness,
    code: KeySpec,
    modifiers: KeyMods,
    description: &str,
) -> Result<(), ScenarioFailure> {
    harness
        .send_key(
            key_spec_to_crossterm(code),
            key_mods_to_crossterm(modifiers),
        )
        .map_err(|e| ScenarioFailure::InputProjectionFailed {
            description: description.into(),
            reason: format!("send_key({code:?}, {modifiers:?}): {e}"),
        })
}

fn key_spec_to_crossterm(code: KeySpec) -> crossterm::event::KeyCode {
    use crossterm::event::KeyCode;
    match code {
        KeySpec::Char(c) => KeyCode::Char(c),
        KeySpec::Backspace => KeyCode::Backspace,
        KeySpec::Enter => KeyCode::Enter,
        KeySpec::Left => KeyCode::Left,
        KeySpec::Right => KeyCode::Right,
        KeySpec::Up => KeyCode::Up,
        KeySpec::Down => KeyCode::Down,
        KeySpec::Home => KeyCode::Home,
        KeySpec::End => KeyCode::End,
        KeySpec::PageUp => KeyCode::PageUp,
        KeySpec::PageDown => KeyCode::PageDown,
        KeySpec::Tab => KeyCode::Tab,
        KeySpec::BackTab => KeyCode::BackTab,
        KeySpec::Delete => KeyCode::Delete,
        KeySpec::Insert => KeyCode::Insert,
        KeySpec::Esc => KeyCode::Esc,
    }
}

fn key_mods_to_crossterm(m: KeyMods) -> crossterm::event::KeyModifiers {
    let mut out = crossterm::event::KeyModifiers::NONE;
    if m.ctrl {
        out |= crossterm::event::KeyModifiers::CONTROL;
    }
    if m.shift {
        out |= crossterm::event::KeyModifiers::SHIFT;
    }
    if m.alt {
        out |= crossterm::event::KeyModifiers::ALT;
    }
    out
}
