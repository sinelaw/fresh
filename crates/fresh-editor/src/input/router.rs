//! Editor-free decision core for key routing.
//!
//! This module holds the *decision* half of the input pipeline: pure
//! functions that take only the narrow inputs they need — the key event,
//! the [`KeybindingResolver`], and small read-only views of relevant
//! state — and return a decision value. They never see the `Editor`.
//!
//! The layering rule: `Editor` (in `app/input.rs`) is the high-level
//! imperative shell. It builds the views from live state, asks this
//! module what the key *means*, and then applies the effects the decision
//! names. Nothing in this module can mutate anything, which is what makes
//! each decision unit-testable without constructing an editor — see the
//! tests at the bottom of this file.
//!
//! The same split already exists for overlay focus (`app/overlay.rs`
//! builds a `Layer` list; pure functions decide precedence); this module
//! extends the pattern to key routing.

use crate::input::keybindings::{Action, KeyContext, KeybindingResolver};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Convert a crossterm `KeyEvent` into the `KeyEventPayload` shape
/// delivered to plugin `editor.getNextKey()` callers.
///
/// `key` matches the naming used by `defineMode` bindings:
///   - named keys are lowercase (`"escape"`, `"enter"`, `"tab"`,
///     `"space"`, `"backspace"`, arrows, `"f1"`–`"f12"`, …)
///   - printable characters are returned as-is (`"a"`, `"!"`, `" "`)
///   - unsupported / unknown keys yield an empty `key` string
pub fn key_event_to_payload(ev: &KeyEvent) -> fresh_core::api::KeyEventPayload {
    let key = match ev.code {
        KeyCode::Char(c) => c.to_string(),
        KeyCode::Esc => "escape".to_string(),
        KeyCode::Enter => "enter".to_string(),
        KeyCode::Tab => "tab".to_string(),
        KeyCode::BackTab => "backtab".to_string(),
        KeyCode::Backspace => "backspace".to_string(),
        KeyCode::Delete => "delete".to_string(),
        KeyCode::Left => "left".to_string(),
        KeyCode::Right => "right".to_string(),
        KeyCode::Up => "up".to_string(),
        KeyCode::Down => "down".to_string(),
        KeyCode::Home => "home".to_string(),
        KeyCode::End => "end".to_string(),
        KeyCode::PageUp => "pageup".to_string(),
        KeyCode::PageDown => "pagedown".to_string(),
        KeyCode::Insert => "insert".to_string(),
        KeyCode::F(n) => format!("f{}", n),
        _ => String::new(),
    };
    fresh_core::api::KeyEventPayload {
        key,
        ctrl: ev.modifiers.contains(KeyModifiers::CONTROL),
        alt: ev.modifiers.contains(KeyModifiers::ALT),
        shift: ev.modifiers.contains(KeyModifiers::SHIFT),
        meta: ev.modifiers.contains(KeyModifiers::SUPER),
    }
}

/// The chord built from what the key types on this layout, if the keymap
/// binds it. `None` when there is no distinct layout character, or when
/// nothing is bound to it and the physical chord should be used instead.
///
/// A chord is both a physical key plus modifiers (`Ctrl+Shift+7`) and the
/// character that key types (`&` on a US layout, `/` on a German one). The
/// parser reports both when they disagree — see
/// [`fresh_input_parser::KeyPress`] — because neither is right on its own.
/// **The keymap decides**: the layout reading is used only if something is
/// actually bound to it.
pub fn layout_reading(
    press: &fresh_input_parser::KeyPress,
    kb: &KeybindingResolver,
    context: KeyContext,
) -> Option<(KeyCode, KeyModifiers)> {
    let layout_char = press.layout_char?;
    // Shift is spent producing the character, so it is not part of the
    // chord built from it: the German `/` is `ctrl+/`, not `ctrl+shift+/`.
    let modifiers = press.modifiers - KeyModifiers::SHIFT;
    let code = KeyCode::Char(layout_char);
    let action = kb.resolve(&KeyEvent::new(code, modifiers), context);
    // `InsertChar` is the resolver's "nothing bound, just type it" answer.
    // Typing is the physical key's job, not this reading's.
    match action {
        Action::None | Action::InsertChar(_) => None,
        _ => Some((code, modifiers)),
    }
}

/// Keybinding precedence for a key aimed at an *unfocused* popup: the
/// user's bound `popup_cancel` (default Esc) and `popup_focus` (default
/// Alt+T) keys must still take effect even though the popup isn't
/// claiming the keyboard.
///
/// `window_context` is the active window's own key context. `popup_focus`
/// lives in the Normal/FileExplorer context defaults (not Global) so a
/// user's own binding for the same key in those contexts wins at the same
/// precedence level. If the resolution there returns anything other than
/// `PopupFocus`, it's the user's override — let the normal dispatcher
/// handle it. Don't claim `popup_cancel` from Normal because Normal's
/// default `Esc` resolves to `remove_secondary_cursors`, which would
/// shadow the popup-dismiss intent here.
///
/// Callers are responsible for the state guards (a popup is visible and
/// unfocused, no higher modal owns the keyboard) — this function only
/// decides what the key *means* under those conditions.
pub fn unfocused_popup_action(
    window_context: KeyContext,
    kb: &KeybindingResolver,
    event: &KeyEvent,
) -> Option<Action> {
    let popup_focus_match = matches!(
        kb.resolve_in_context_only(event, window_context),
        Some(Action::PopupFocus),
    );
    if popup_focus_match {
        return Some(Action::PopupFocus);
    }

    // Fall back to the Popup context for `popup_cancel`. Esc (the default
    // `popup_cancel` binding) should still dismiss an unfocused popup even
    // though the popup itself isn't claiming the keyboard — that matches
    // every other popup-dismissal affordance in the editor.
    match kb.resolve_in_context_only(event, KeyContext::Popup) {
        Some(action @ (Action::PopupCancel | Action::PopupFocus)) => Some(action),
        _ => None,
    }
}

// **`completion_popup_action` went with the walk that called it.** It asked
// the keymap for the key as it arrived, from inside a stage the shell tree is
// offered the key before, so a binding it would have found could be swallowed
// ahead of it. `Editor::popup_keys` enumerates the same two actions out of the
// `completion` section instead and declares them on the open popup's layer.

/// Read-only view of the floating widget panel state that
/// [`widget_panel_key`] decides over. Built by the Editor from live
/// state; carries exactly what the decision needs and nothing else.
pub struct WidgetPanelView {
    /// The panel is the left dock (vs a centered modal). The dock is
    /// non-modal: unhandled shortcuts blur it and fall through to the
    /// editor; a centered modal swallows them.
    pub is_left_dock: bool,
    /// The panel's currently focused widget key (previous render).
    pub focus_key: Option<String>,
    /// The focused widget is a Text input (clipboard chords belong to it).
    pub focused_widget_is_text: bool,
    /// The active window's editor mode, if any. A `defineMode` binding
    /// for a key must win over the panel's default smart-key behaviour.
    pub editor_mode: Option<String>,
}

/// What a key aimed at a floating widget panel means. The Editor executes
/// the named effect; `FallThrough` / `BlurUnconsumed` mean the key was
/// *not* consumed and continues down the normal dispatch pipeline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WidgetKeyOutcome {
    /// Fire a `widget_event` of this type at the dock's `sessions` widget.
    DockEvent(&'static str),
    /// Move panel focus to this widget key (and notify the plugin).
    FocusWidget(&'static str),
    /// Blur the panel — focus returns to the editor; the panel stays
    /// mounted. Key consumed.
    Blur,
    /// Blur the panel and let the editor handle the key (dock-style
    /// unhandled shortcut: e.g. Ctrl+P should still open the palette).
    BlurUnconsumed,
    /// Esc default: fire a `cancel` widget_event at the focused widget,
    /// then unmount the panel.
    CancelAndUnmount,
    /// Route a named smart key ("Enter", "Tab", …) through the widget
    /// command dispatcher.
    SmartKey(&'static str),
    /// Feed a printable character to the focused TextInput.
    TextChar(char),
    /// Clipboard / selection chord for the focused Text widget.
    Paste,
    Copy,
    Cut,
    SelectAll,
    /// Consumed with no effect — the modal owns the input channel.
    Swallow,
    /// The active mode explicitly binds this key — not consumed here.
    FallThrough,
}

/// Decide what a keystroke aimed at a mounted floating widget panel
/// means. Pure: reads the [`WidgetPanelView`] and the keymap, mutates
/// nothing. See the outcome variants for the effect vocabulary.
///
/// The left dock handles Enter / Esc / Space / "/" here, at the
/// floating-panel layer, *independent of editor modes*: editor modes
/// (`defineMode`) resolve against the active buffer's mode, which the
/// dock floats over — so a session whose buffer has a local mode would
/// shadow any global dock mode. Everywhere else, an explicit mode binding
/// for the key wins (`FallThrough`) — that is what `defineMode` exists
/// for. Only bindings *explicitly* set for the mode count: the resolver's
/// full `resolve()` falls back to Normal-context bindings for any mode,
/// which would falsely report Enter as bound everywhere.
pub fn widget_panel_key(
    view: &WidgetPanelView,
    kb: &KeybindingResolver,
    code: KeyCode,
    modifiers: KeyModifiers,
) -> WidgetKeyOutcome {
    use WidgetKeyOutcome::*;

    let mode_has_binding = |code: KeyCode, modifiers: KeyModifiers| {
        view.editor_mode
            .as_ref()
            .map(|mode_name| {
                let key_event = KeyEvent::new(code, modifiers);
                let mode_ctx = KeyContext::Mode(mode_name.to_string());
                kb.has_explicit_binding(&key_event, &mode_ctx)
            })
            .unwrap_or(false)
    };

    // ACKNOWLEDGED RESIDUE (recorded in the v2 review doc): this dock
    // branch hardcodes ONE plugin's widget-key conventions ("filter",
    // "sessions", "project-pick:"/"menu-pick:" prefixes, dock_menu_*
    // events) — orchestrator-specific panel semantics in host
    // dispatch. The generalization (plugin-declared key policy, or
    // these behaviors moving into kind/Component contracts) is part of
    // the app-level focus-unification arc; until then this is the one
    // deliberate plugin-shaped seam in the router.
    if view.is_left_dock {
        let on_filter = view.focus_key.as_deref() == Some("filter");
        // Any of the dock's inline dropdowns (project scope, the
        // "New Task…" create menu, or a session's "Move to…" folder
        // menu) owns the keyboard while panel focus sits on one of its
        // option rows. The plugin moves focus onto a `project-pick:` /
        // `menu-pick:` button when a menu opens; in that state ↑/↓ move
        // the cursor, Enter commits, Esc cancels — all routed to the
        // plugin as `dock_menu_*` events so they don't leak to the
        // session tree underneath.
        let on_project_menu = view
            .focus_key
            .as_deref()
            .map(|k| k.starts_with("project-pick:") || k.starts_with("menu-pick:"))
            .unwrap_or(false);
        if on_project_menu {
            match code {
                KeyCode::Up => return DockEvent("dock_menu_prev"),
                KeyCode::Down => return DockEvent("dock_menu_next"),
                // Tab/Shift+Tab navigate the menu too, so they can't
                // tab focus *out* of the open dropdown into the dock
                // toolbar behind it.
                KeyCode::Tab if modifiers.contains(KeyModifiers::SHIFT) => {
                    return DockEvent("dock_menu_prev")
                }
                KeyCode::BackTab => return DockEvent("dock_menu_prev"),
                KeyCode::Tab => return DockEvent("dock_menu_next"),
                KeyCode::Enter | KeyCode::Char(' ') => return DockEvent("dock_menu_accept"),
                KeyCode::Esc => return DockEvent("dock_menu_cancel"),
                _ => {}
            }
        }
        let sessions_focused = view
            .focus_key
            .as_deref()
            .map(|k| k == "sessions" || k.is_empty())
            .unwrap_or(true);
        match code {
            KeyCode::Esc => {
                return if on_filter {
                    // Return from the filter to the session list.
                    FocusWidget("sessions")
                } else {
                    // Leave the dock — focus the editor; dock stays visible.
                    Blur
                };
            }
            KeyCode::Enter => {
                return if on_filter {
                    FocusWidget("sessions")
                } else if sessions_focused {
                    // Enter on the session list activates the highlighted
                    // row; handled plugin-side so the discovered-vs-live
                    // decision lives next to the dialog's identical
                    // `activate` logic.
                    DockEvent("dock_activate")
                } else {
                    // A button or toggle is keyboard-focused. Run THAT
                    // control's action via the generic smart-key
                    // dispatcher instead of the list's dock_activate.
                    SmartKey("Enter")
                };
            }
            KeyCode::Char('/') if modifiers.is_empty() => return FocusWidget("filter"),
            // The standard context-menu keys open the highlighted node's
            // right-click menu. Only fire while the session tree itself is
            // focused, matching the Enter branch above.
            KeyCode::Menu if sessions_focused => return DockEvent("dock_context"),
            // F2 — the classic TUI "user menu" key. Shift+F10 (the
            // desktop-GUI convention) is unreliable in terminals.
            KeyCode::F(2) if modifiers.is_empty() && sessions_focused => {
                return DockEvent("dock_context")
            }
            // Alt+T / Alt+I / Alt+P / Alt+N: dialog OPEN_MODE chords the
            // dock can't express as an editor mode (it floats over the
            // active buffer's mode) — routed as dock widget_events.
            KeyCode::Char('t' | 'T') if modifiers.contains(KeyModifiers::ALT) => {
                return DockEvent("dock_toggle_worktrees")
            }
            KeyCode::Char('i' | 'I') if modifiers.contains(KeyModifiers::ALT) => {
                return DockEvent("dock_toggle_trivial")
            }
            KeyCode::Char('p' | 'P') if modifiers.contains(KeyModifiers::ALT) => {
                return DockEvent("dock_toggle_scope")
            }
            KeyCode::Char('n' | 'N') if modifiers.contains(KeyModifiers::ALT) => {
                return DockEvent("dock_new")
            }
            // Toggle the highlighted row's multi-select checkbox (plugin
            // owns the selection set).
            KeyCode::Char(' ') => return DockEvent("dock_space"),
            _ => {}
        }
    }

    if code == KeyCode::Esc {
        // Mode-binding precedence: a plugin's `defineMode` entry for
        // Escape wins over the default "Esc closes the modal" behaviour.
        // Lets a plugin claim Esc for a nested dismiss-the-dropdown
        // gesture before the outermost cancel fires.
        if mode_has_binding(code, modifiers) {
            return FallThrough;
        }
        return CancelAndUnmount;
    }

    let key_name: Option<&'static str> = match code {
        KeyCode::Tab => Some(if modifiers.contains(KeyModifiers::SHIFT) {
            "Shift+Tab"
        } else {
            "Tab"
        }),
        KeyCode::BackTab => Some("Shift+Tab"),
        KeyCode::Enter => Some("Enter"),
        KeyCode::Backspace => Some("Backspace"),
        KeyCode::Delete => Some("Delete"),
        KeyCode::Home => Some("Home"),
        KeyCode::End => Some("End"),
        KeyCode::Left => Some("Left"),
        KeyCode::Right => Some("Right"),
        KeyCode::Up => Some("Up"),
        KeyCode::Down => Some("Down"),
        KeyCode::PageUp => Some("PageUp"),
        KeyCode::PageDown => Some("PageDown"),
        _ => None,
    };
    if let Some(name) = key_name {
        // The orchestrator New-Session form relies on mode precedence so
        // Enter submits the form regardless of which field is focused.
        if mode_has_binding(code, modifiers) {
            return FallThrough;
        }
        return SmartKey(name);
    }

    if let KeyCode::Char(c) = code {
        // The active editor mode may have explicitly claimed this char
        // via `defineMode`. This covers *plain* chars too (not just
        // Ctrl/Alt chords): a plugin that binds a bare key like `/` gets
        // it before the text-input fast path. The trade-off is that a
        // bound bare key can't also be typed as text in that mode, which
        // is what the plugin asked for by binding it.
        if mode_has_binding(code, modifiers) {
            return FallThrough;
        }
        // Ctrl/Alt-modified chords with no mode binding: a centered
        // modal swallows them (it must not leak keys to global bindings
        // like Ctrl-P). The non-modal dock does the opposite — an
        // unhandled shortcut returns focus to the editor (blur) and
        // falls through so the editor handles it.
        if modifiers.intersects(KeyModifiers::CONTROL | KeyModifiers::ALT) {
            // Clipboard chords on a focused Text field belong to the
            // field, not the editor: Ctrl+V must paste into the field
            // (and Ctrl+A / Ctrl+C / Ctrl+X select/copy/cut its own
            // text). Resolve against the Normal context — the same
            // lookup the buffer-mounted widget routing uses.
            if view.focused_widget_is_text {
                let key_event = KeyEvent::new(code, modifiers);
                match kb.resolve(&key_event, KeyContext::Normal) {
                    Action::Paste => return Paste,
                    Action::Copy => return Copy,
                    Action::Cut => return Cut,
                    Action::SelectAll => return SelectAll,
                    _ => {}
                }
            }
            if view.is_left_dock {
                return BlurUnconsumed;
            }
            return Swallow;
        }
        let ch = if modifiers.contains(KeyModifiers::SHIFT) {
            c.to_uppercase().next().unwrap_or(c)
        } else {
            c
        };
        // Space is a special case on a focused Toggle / Button: the
        // convention is "Space activates the focused control", not
        // "insert a literal space". The smart-key dispatcher still
        // inserts " " as a char for a focused Text widget, so typing
        // spaces into text fields keeps working.
        if ch == ' ' {
            return SmartKey("Space");
        }
        return TextChar(ch);
    }

    // Any other keystroke (function keys, unhandled keycodes, …) is
    // swallowed — the modal is the exclusive owner of the input channel
    // until it unmounts.
    Swallow
}

/// Read-only view for [`should_dismiss_transient_popup`]: the state of the
/// topmost visible popup.
pub struct TransientPopupView {
    /// The popup is transient (hover, signature help) — dismissed on any
    /// key press rather than owning the keyboard.
    pub is_transient: bool,
    /// The popup currently has a text selection.
    pub has_selection: bool,
}

/// Whether a key press should dismiss the transient popup on screen.
///
/// Fires for both focused and unfocused popups: an unfocused hover popup
/// that floats over the buffer must still vanish when the user starts
/// typing — otherwise it lingers indefinitely because no key event
/// reaches it. Two exceptions:
///   - Ctrl+C while the popup has a selection (let the user copy first);
///   - the key resolves to `popup_focus` — the user is *transferring*
///     focus to the popup, and dismissing it first would close it before
///     its handler ever sees the focus action.
pub fn should_dismiss_transient_popup(
    view: &TransientPopupView,
    kb: &KeybindingResolver,
    context: KeyContext,
    event: &KeyEvent,
) -> bool {
    if !view.is_transient {
        return false;
    }
    let is_copy_key =
        event.code == KeyCode::Char('c') && event.modifiers.contains(KeyModifiers::CONTROL);
    if view.has_selection && is_copy_key {
        return false;
    }
    !matches!(kb.resolve(event, context), Action::PopupFocus)
}

/// Read-only view for [`mode_key_disposition`]: the mode-related state a
/// key is judged against while the editor buffer has focus.
pub struct ModeKeyView {
    /// The effective mode (buffer-local if present, else global) —
    /// virtual buffer modes must not be hijacked by global modes.
    pub effective_mode: Option<String>,
    /// The effective mode declares `allow_text_input` (e.g.
    /// search-replace-list): character keys are captured, other unbound
    /// keys are blocked.
    pub allows_text_input: bool,
    /// The *global* editor mode's read-only flag, when a global mode is
    /// active (e.g. vi-normal blocks all unbound keys when read-only).
    pub global_mode_read_only: Option<bool>,
    /// A widget Text input on the active buffer is focused — Shift+nav
    /// extends its selection instead of reaching the buffer.
    pub has_focused_text_widget: bool,
}

/// A selection-extending move on a focused widget text editor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidgetSelectionMove {
    WordLeft,
    WordRight,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
}

/// What a key means under the active editor mode. The shell clears the
/// window's chord state whenever the disposition is anything but
/// [`ModeKeyDisposition::ChordPending`] (clearing an empty state is a
/// no-op, so this matches the abandoned-chord semantics).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModeKeyDisposition {
    /// A mode chord completed or a mode binding resolved — run it.
    Run(Action),
    /// The key extends a pending chord — push it and wait.
    ChordPending,
    /// A text-input mode captures this character. Dispatched through
    /// the typed `dispatch_mode_text_input` lane (mode + char as
    /// structured fields); the legacy `mode_text_input@<mode>:<ch>`
    /// action-name encoding survives only for user keymaps that bound
    /// the string form, behind a deprecation warning.
    TextInput(char),
    /// Clipboard / select-all chord forwarded despite the text-input
    /// mode block — it belongs to the focused widget Text input.
    Forward(Action),
    /// Shift+nav extends the focused widget text selection. Always
    /// consumed, even when the move is a no-op at a boundary.
    WidgetSelection(WidgetSelectionMove),
    /// Consumed with no effect (unbound key in a text-input or
    /// read-only mode).
    Block,
    /// No mode claims the key — continue down the pipeline.
    FallThrough,
}

/// Decide what a key means while an editor mode is active. Pure: the
/// same chord / binding / capture precedence the mode stage of
/// `Editor::handle_key` has always applied, minus the state mutations —
/// the shell applies those based on the returned disposition.
///
/// `chord_state` is the window's pending chord prefix.
pub fn mode_key_disposition(
    view: &ModeKeyView,
    chord_state: &[(KeyCode, KeyModifiers)],
    kb: &KeybindingResolver,
    event: &KeyEvent,
) -> ModeKeyDisposition {
    use crate::input::keybindings::ChordResolution;

    if let Some(mode_name) = &view.effective_mode {
        let mode_ctx = KeyContext::Mode(mode_name.clone());
        match kb.resolve_chord(chord_state, event, mode_ctx.clone()) {
            ChordResolution::Complete(action) => return ModeKeyDisposition::Run(action),
            ChordResolution::Partial => return ModeKeyDisposition::ChordPending,
            ChordResolution::NoMatch => {}
        }
        // Mode single-key resolution (custom > keymap > plugin defaults)
        let resolved = kb.resolve(event, mode_ctx);
        if resolved != Action::None {
            return ModeKeyDisposition::Run(resolved);
        }
    }

    // Handle unbound keys for modes that want to capture input.
    //
    // Buffer-local modes with allow_text_input (e.g. search-replace-list)
    // capture character keys and block other unbound keys. Buffer-local
    // modes WITHOUT allow_text_input (e.g. diff-view) let unbound keys
    // fall through to normal keybinding handling so that Ctrl+C, arrows,
    // etc. still work.
    if view.effective_mode.is_some() && view.allows_text_input {
        if let KeyCode::Char(c) = event.code {
            let ch = if event.modifiers.contains(KeyModifiers::SHIFT) {
                c.to_uppercase().next().unwrap_or(c)
            } else {
                c
            };
            if !event
                .modifiers
                .intersects(KeyModifiers::CONTROL | KeyModifiers::ALT)
            {
                return ModeKeyDisposition::TextInput(ch);
            }
        }
        // Before blocking the key, resolve it against the Normal context
        // and forward if it's one of the clipboard / select-all actions —
        // those legitimately belong to the focused widget Text input, not
        // the underlying buffer. Other Ctrl-modified actions (e.g. Open /
        // Save / SplitVertical) stay blocked so they don't hijack a
        // focused search field.
        if let action @ (Action::Paste | Action::Copy | Action::Cut | Action::SelectAll) =
            kb.resolve(event, KeyContext::Normal)
        {
            return ModeKeyDisposition::Forward(action);
        }
        // Shift+arrow / Ctrl+Shift+arrow extend the selection on the
        // focused widget TextEdit, if any. Routed directly instead of
        // through the IPC `WidgetAction` path because selection ops are
        // host-internal — the plugin's model only cares about the
        // post-`change` value.
        if event.modifiers.contains(KeyModifiers::SHIFT) && view.has_focused_text_widget {
            let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
            let mv = match event.code {
                KeyCode::Left if ctrl => Some(WidgetSelectionMove::WordLeft),
                KeyCode::Right if ctrl => Some(WidgetSelectionMove::WordRight),
                KeyCode::Left => Some(WidgetSelectionMove::Left),
                KeyCode::Right => Some(WidgetSelectionMove::Right),
                KeyCode::Up => Some(WidgetSelectionMove::Up),
                KeyCode::Down => Some(WidgetSelectionMove::Down),
                KeyCode::Home => Some(WidgetSelectionMove::Home),
                KeyCode::End => Some(WidgetSelectionMove::End),
                _ => None,
            };
            if let Some(mv) = mv {
                return ModeKeyDisposition::WidgetSelection(mv);
            }
        }
        return ModeKeyDisposition::Block;
    }

    // Global editor modes (e.g. vi-normal) block all unbound keys when
    // read-only.
    if view.global_mode_read_only == Some(true) {
        return ModeKeyDisposition::Block;
    }
    ModeKeyDisposition::FallThrough
}

/// Chord-then-single-key resolution against one context: the last stage
/// of the pipeline. `Chord` and `Resolved` are distinct because the
/// shell treats them differently (only a single-key resolution cancels
/// in-flight LSP requests).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChordDisposition {
    /// A chord sequence completed — run its action.
    Chord(Action),
    /// The key extends a pending chord — push it and wait.
    Pending,
    /// No chord matched — single-key resolution produced this action
    /// (possibly `Action::None`).
    Resolved(Action),
}

/// Resolve a key against `context`, chords first. The shell clears the
/// chord state on anything but [`ChordDisposition::Pending`] (an
/// abandoned prefix must not poison the next key).
pub fn chord_or_key(
    chord_state: &[(KeyCode, KeyModifiers)],
    kb: &KeybindingResolver,
    event: &KeyEvent,
    context: KeyContext,
) -> ChordDisposition {
    use crate::input::keybindings::ChordResolution;
    match kb.resolve_chord(chord_state, event, context.clone()) {
        ChordResolution::Complete(action) => ChordDisposition::Chord(action),
        ChordResolution::Partial => ChordDisposition::Pending,
        ChordResolution::NoMatch => ChordDisposition::Resolved(kb.resolve(event, context)),
    }
}

/// Whether performing `action` should cancel in-flight LSP requests.
/// Stale completions must not pop up after the user has moved on — but
/// the LSP actions themselves (and no-ops) obviously keep their own
/// requests alive.
pub fn cancels_pending_lsp(action: &Action) -> bool {
    !matches!(
        action,
        Action::LspCompletion
            | Action::LspGotoDefinition
            | Action::LspReferences
            | Action::LspImplementation
            | Action::LspHover
            | Action::None
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    /// A config pinned to the `default` keymap.
    ///
    /// These tests assert *keymap semantics* (what the router does with a
    /// chord the keymap binds to Copy/SelectAll/…), not host-OS defaults:
    /// `Config::default()` selects the `macos` keymap on macOS, where e.g.
    /// Ctrl+A is Home rather than SelectAll — deliberate on that platform,
    /// but it would make these assertions test the host instead of the
    /// router. Same pinning convention as the keybinding-resolver tests.
    fn config() -> Config {
        Config {
            active_keybinding_map: "default".into(),
            ..Config::default()
        }
    }

    fn resolver() -> KeybindingResolver {
        KeybindingResolver::new(&config())
    }

    fn event(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, modifiers)
    }

    // `completion_action_requires_completion_popup` went with the function.
    // The gating it pinned — the `completion` section's bindings apply only
    // when the popup holding the keyboard is a completion list — is
    // `Editor::popup_keys`'s, which reads the *kind* to decide which sections
    // to enumerate, and `view::shell::popup::a_bound_key_runs_its_editor_action`
    // is where a bound key is shown reaching the popup.

    #[test]
    fn unfocused_popup_esc_resolves_to_cancel() {
        let kb = resolver();
        let esc = event(KeyCode::Esc, KeyModifiers::NONE);
        // Esc is popup_cancel in the Popup context; Normal's own Esc
        // (remove_secondary_cursors) must not shadow it.
        assert_eq!(
            unfocused_popup_action(KeyContext::Normal, &kb, &esc),
            Some(Action::PopupCancel)
        );
        // A key bound to neither popup_focus nor popup_cancel falls
        // through to the buffer.
        let char_a = event(KeyCode::Char('a'), KeyModifiers::NONE);
        assert_eq!(
            unfocused_popup_action(KeyContext::Normal, &kb, &char_a),
            None
        );
    }

    #[test]
    fn layout_reading_only_when_bound() {
        let kb = resolver();
        // German layout: Ctrl+Shift+7 types `/` → the layout reading is
        // ctrl+/, which the default map binds (toggle_comment), so it wins.
        let press = fresh_input_parser::KeyPress::with_layout_char(
            event(
                KeyCode::Char('7'),
                KeyModifiers::CONTROL | KeyModifiers::SHIFT,
            ),
            Some('/'),
        );
        assert_eq!(
            layout_reading(&press, &kb, KeyContext::Normal),
            Some((KeyCode::Char('/'), KeyModifiers::CONTROL))
        );
        // No distinct layout character → no layout reading.
        let plain =
            fresh_input_parser::KeyPress::new(event(KeyCode::Char('a'), KeyModifiers::NONE));
        assert_eq!(layout_reading(&plain, &kb, KeyContext::Normal), None);
    }

    #[test]
    fn dock_esc_depends_on_focus() {
        let kb = resolver();
        let dock = |focus: Option<&str>| WidgetPanelView {
            is_left_dock: true,
            focus_key: focus.map(str::to_string),
            focused_widget_is_text: false,
            editor_mode: None,
        };
        // Esc on the filter returns to the session list; elsewhere it
        // blurs the dock (which stays mounted).
        assert_eq!(
            widget_panel_key(&dock(Some("filter")), &kb, KeyCode::Esc, KeyModifiers::NONE),
            WidgetKeyOutcome::FocusWidget("sessions")
        );
        assert_eq!(
            widget_panel_key(
                &dock(Some("sessions")),
                &kb,
                KeyCode::Esc,
                KeyModifiers::NONE
            ),
            WidgetKeyOutcome::Blur
        );
        // On a centered modal, Esc cancels and unmounts.
        let modal = WidgetPanelView {
            is_left_dock: false,
            focus_key: None,
            focused_widget_is_text: false,
            editor_mode: None,
        };
        assert_eq!(
            widget_panel_key(&modal, &kb, KeyCode::Esc, KeyModifiers::NONE),
            WidgetKeyOutcome::CancelAndUnmount
        );
    }

    #[test]
    fn dock_dropdown_owns_navigation_keys() {
        let kb = resolver();
        let view = WidgetPanelView {
            is_left_dock: true,
            focus_key: Some("project-pick:2".to_string()),
            focused_widget_is_text: false,
            editor_mode: None,
        };
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Up, KeyModifiers::NONE),
            WidgetKeyOutcome::DockEvent("dock_menu_prev")
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Enter, KeyModifiers::NONE),
            WidgetKeyOutcome::DockEvent("dock_menu_accept")
        );
    }

    #[test]
    fn modal_swallows_chords_dock_blurs_through() {
        let kb = resolver();
        let mk = |is_left_dock| WidgetPanelView {
            is_left_dock,
            focus_key: None,
            focused_widget_is_text: false,
            editor_mode: None,
        };
        let ctrl_p = (KeyCode::Char('p'), KeyModifiers::CONTROL);
        // A centered modal must not leak Ctrl+P to the palette…
        assert_eq!(
            widget_panel_key(&mk(false), &kb, ctrl_p.0, ctrl_p.1),
            WidgetKeyOutcome::Swallow
        );
        // …the non-modal dock blurs and lets the editor have it.
        assert_eq!(
            widget_panel_key(&mk(true), &kb, ctrl_p.0, ctrl_p.1),
            WidgetKeyOutcome::BlurUnconsumed
        );
    }

    #[test]
    fn text_input_mode_captures_plain_chars_and_forwards_clipboard() {
        let kb = resolver();
        let view = ModeKeyView {
            effective_mode: Some("search-replace-list".to_string()),
            allows_text_input: true,
            global_mode_read_only: None,
            has_focused_text_widget: false,
        };
        // Plain characters feed the mode's text input (Shift upcases).
        assert_eq!(
            mode_key_disposition(
                &view,
                &[],
                &kb,
                &event(KeyCode::Char('a'), KeyModifiers::NONE)
            ),
            ModeKeyDisposition::TextInput('a')
        );
        assert_eq!(
            mode_key_disposition(
                &view,
                &[],
                &kb,
                &event(KeyCode::Char('a'), KeyModifiers::SHIFT)
            ),
            ModeKeyDisposition::TextInput('A')
        );
        // Clipboard chords are forwarded, not blocked…
        assert_eq!(
            mode_key_disposition(
                &view,
                &[],
                &kb,
                &event(KeyCode::Char('c'), KeyModifiers::CONTROL)
            ),
            ModeKeyDisposition::Forward(Action::Copy)
        );
        // …but other chords and unbound named keys are blocked.
        assert_eq!(
            mode_key_disposition(
                &view,
                &[],
                &kb,
                &event(KeyCode::Char('o'), KeyModifiers::CONTROL)
            ),
            ModeKeyDisposition::Block
        );
    }

    #[test]
    fn text_input_mode_routes_shift_nav_to_focused_widget() {
        let kb = resolver();
        let mk = |has_widget| ModeKeyView {
            effective_mode: Some("search-replace-list".to_string()),
            allows_text_input: true,
            global_mode_read_only: None,
            has_focused_text_widget: has_widget,
        };
        assert_eq!(
            mode_key_disposition(
                &mk(true),
                &[],
                &kb,
                &event(KeyCode::Left, KeyModifiers::SHIFT)
            ),
            ModeKeyDisposition::WidgetSelection(WidgetSelectionMove::Left)
        );
        assert_eq!(
            mode_key_disposition(
                &mk(true),
                &[],
                &kb,
                &event(KeyCode::Left, KeyModifiers::SHIFT | KeyModifiers::CONTROL)
            ),
            ModeKeyDisposition::WidgetSelection(WidgetSelectionMove::WordLeft)
        );
        // No focused widget Text → the key is simply blocked.
        assert_eq!(
            mode_key_disposition(
                &mk(false),
                &[],
                &kb,
                &event(KeyCode::Left, KeyModifiers::SHIFT)
            ),
            ModeKeyDisposition::Block
        );
    }

    #[test]
    fn read_only_global_mode_blocks_unbound_keys() {
        let kb = resolver();
        let mk = |read_only| ModeKeyView {
            effective_mode: Some("vi-normal".to_string()),
            allows_text_input: false,
            global_mode_read_only: Some(read_only),
            has_focused_text_widget: false,
        };
        // An unbound function key: read-only blocks, editable falls through.
        let f9 = event(KeyCode::F(9), KeyModifiers::NONE);
        assert_eq!(
            mode_key_disposition(&mk(true), &[], &kb, &f9),
            ModeKeyDisposition::Block
        );
        assert_eq!(
            mode_key_disposition(&mk(false), &[], &kb, &f9),
            ModeKeyDisposition::FallThrough
        );
    }

    #[test]
    fn mode_bindings_win_over_capture() {
        // A `defineMode`-style binding (config `when: "mode:NAME"`) must
        // resolve ahead of the text-input capture.
        let mut config = config();
        config.keybindings.push(crate::config::Keybinding {
            key: "enter".to_string(),
            modifiers: Vec::new(),
            keys: Vec::new(),
            action: "save".to_string(),
            args: std::collections::HashMap::new(),
            when: Some("mode:form".to_string()),
        });
        let kb = KeybindingResolver::new(&config);
        let view = ModeKeyView {
            effective_mode: Some("form".to_string()),
            allows_text_input: true,
            global_mode_read_only: None,
            has_focused_text_widget: false,
        };
        assert_eq!(
            mode_key_disposition(&view, &[], &kb, &event(KeyCode::Enter, KeyModifiers::NONE)),
            ModeKeyDisposition::Run(Action::Save)
        );
    }

    #[test]
    fn chord_or_key_walks_a_two_key_sequence() {
        let mut config = config();
        config.keybindings.push(crate::config::Keybinding {
            key: String::new(),
            modifiers: Vec::new(),
            keys: vec![
                crate::config::KeyPress {
                    key: "x".to_string(),
                    modifiers: vec!["ctrl".to_string()],
                },
                crate::config::KeyPress {
                    key: "s".to_string(),
                    modifiers: vec!["ctrl".to_string()],
                },
            ],
            action: "save".to_string(),
            args: std::collections::HashMap::new(),
            when: Some("normal".to_string()),
        });
        let kb = KeybindingResolver::new(&config);
        let ctrl_x = event(KeyCode::Char('x'), KeyModifiers::CONTROL);
        let ctrl_s = event(KeyCode::Char('s'), KeyModifiers::CONTROL);
        // First key: a pending prefix — the shell pushes it.
        assert_eq!(
            chord_or_key(&[], &kb, &ctrl_x, KeyContext::Normal),
            ChordDisposition::Pending
        );
        // Second key completes the chord.
        assert_eq!(
            chord_or_key(
                &[(KeyCode::Char('x'), KeyModifiers::CONTROL)],
                &kb,
                &ctrl_s,
                KeyContext::Normal
            ),
            ChordDisposition::Chord(Action::Save)
        );
        // No chord in flight: a plain character resolves to typing it.
        assert_eq!(
            chord_or_key(
                &[],
                &kb,
                &event(KeyCode::Char('a'), KeyModifiers::NONE),
                KeyContext::Normal
            ),
            ChordDisposition::Resolved(Action::InsertChar('a'))
        );
    }

    #[test]
    fn transient_popup_dismissal_exceptions() {
        let kb = resolver();
        let transient = |has_selection| TransientPopupView {
            is_transient: true,
            has_selection,
        };
        let char_a = event(KeyCode::Char('a'), KeyModifiers::NONE);
        let ctrl_c = event(KeyCode::Char('c'), KeyModifiers::CONTROL);
        // Any ordinary key dismisses a transient popup…
        assert!(should_dismiss_transient_popup(
            &transient(false),
            &kb,
            KeyContext::Normal,
            &char_a
        ));
        // …except Ctrl+C while it has a selection (let the user copy)…
        assert!(!should_dismiss_transient_popup(
            &transient(true),
            &kb,
            KeyContext::Normal,
            &ctrl_c
        ));
        // (without a selection Ctrl+C dismisses like any other key)
        assert!(should_dismiss_transient_popup(
            &transient(false),
            &kb,
            KeyContext::Normal,
            &ctrl_c
        ));
        // …and a non-transient popup is never dismissed this way.
        assert!(!should_dismiss_transient_popup(
            &TransientPopupView {
                is_transient: false,
                has_selection: false
            },
            &kb,
            KeyContext::Normal,
            &char_a
        ));
    }

    #[test]
    fn lsp_cancellation_spares_lsp_actions() {
        assert!(cancels_pending_lsp(&Action::Save));
        assert!(cancels_pending_lsp(&Action::InsertChar('x')));
        assert!(!cancels_pending_lsp(&Action::LspHover));
        assert!(!cancels_pending_lsp(&Action::None));
    }

    #[test]
    fn clipboard_chords_reach_a_focused_text_widget() {
        let kb = resolver();
        let view = WidgetPanelView {
            is_left_dock: false,
            focus_key: Some("path".to_string()),
            focused_widget_is_text: true,
            editor_mode: None,
        };
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Char('v'), KeyModifiers::CONTROL),
            WidgetKeyOutcome::Paste
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Char('a'), KeyModifiers::CONTROL),
            WidgetKeyOutcome::SelectAll
        );
    }
}
