//! Shortcuts to intents to actions.
//!
//! ```text
//! key chord --[ Shortcuts map on the focus chain ]--> Intent   (what the user meant)
//! Intent    --[ Actions   map on the focus chain ]--> handler  (how THIS part does it)
//! ```
//!
//! The same `Intent::Cancel` resolves to different actions depending on where
//! focus is: a prompt cancels itself, a modal closes, a buffer clears its
//! selection. No central context enum and no precedence table is involved.

use crate::event::{KeyCode, KeyPress, Mods};

/// What the user meant, independent of how any particular surface does it.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Intent {
    Cancel,
    Confirm,
    Next,
    Prev,
    Up,
    Down,
    Left,
    Right,
    PageUp,
    PageDown,
    Home,
    End,
    Delete,
    /// An application-defined intent.
    Custom(&'static str),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Shortcut {
    pub key: KeyPress,
    pub intent: Intent,
}

impl Shortcut {
    pub const fn new(key: KeyPress, intent: Intent) -> Self {
        Shortcut { key, intent }
    }
}

/// The map every application starts with. A `Focusable` may override any of it
/// for its own subtree.
pub fn default_shortcuts() -> Vec<Shortcut> {
    use KeyCode::*;
    vec![
        Shortcut::new(KeyPress::new(Esc), Intent::Cancel),
        Shortcut::new(KeyPress::new(Enter), Intent::Confirm),
        Shortcut::new(KeyPress::new(Tab), Intent::Next),
        Shortcut::new(KeyPress::with(Tab, Mods::SHIFT), Intent::Prev),
        // A terminal reports Shift+Tab as `BackTab`, and most keep the Shift
        // on it; both spellings are the one intent.
        Shortcut::new(KeyPress::new(BackTab), Intent::Prev),
        Shortcut::new(KeyPress::with(BackTab, Mods::SHIFT), Intent::Prev),
        Shortcut::new(KeyPress::new(Up), Intent::Up),
        Shortcut::new(KeyPress::new(Down), Intent::Down),
        Shortcut::new(KeyPress::new(Left), Intent::Left),
        Shortcut::new(KeyPress::new(Right), Intent::Right),
        Shortcut::new(KeyPress::new(PageUp), Intent::PageUp),
        Shortcut::new(KeyPress::new(PageDown), Intent::PageDown),
        Shortcut::new(KeyPress::new(Home), Intent::Home),
        Shortcut::new(KeyPress::new(End), Intent::End),
        Shortcut::new(KeyPress::new(Delete), Intent::Delete),
    ]
}
