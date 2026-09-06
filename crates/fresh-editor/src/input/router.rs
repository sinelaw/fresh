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
    /// The panel is non-modal — the left dock or a sidebar section: Esc
    /// blurs it and leaves it mounted, and an unbound shortcut blurs it and
    /// falls through to the editor. A centered modal cancels on Esc and
    /// swallows what it does not bind.
    pub non_modal: bool,
    /// The panel is mounted into a pane's buffer. Its widgets answer their
    /// keys like any panel's; everything they do not answer — Esc, an
    /// unbound chord, a function key — is the buffer's own route
    /// (`FallThrough`), because there is nothing to blur, cancel or
    /// swallow: the pane *is* the editor's content.
    pub pane: bool,
    /// The panel's currently focused widget key (previous render).
    pub focus_key: Option<String>,
    /// The focused widget is a Text input (clipboard chords belong to it).
    pub focused_widget_is_text: bool,
    /// The panel is a *page* (`WidgetPanelOptions::page`): a described
    /// document in a pane, over a mirror buffer whose caret is where the
    /// reader is.
    ///
    /// **A page binds no motion.** Its caret is the buffer's, so the arrows,
    /// the page keys and `Home`/`End` are the editor's own — resolved once,
    /// against the user's own keymap, by the machinery every other pane uses
    /// — and the page reads where the caret ended up (`page_follows_caret`).
    /// A page that named those keys itself re-implemented a fraction of that
    /// vocabulary and swallowed the rest: `Ctrl+PageDown` stopped switching
    /// tabs and `Ctrl+Up` stopped scrolling, because a claim is a claim
    /// whatever the modifier.
    ///
    /// A focused text field is the exception, and the reason is the same one:
    /// there the keys are the *field's*, and nothing above it wants them.
    pub page: bool,
}

/// What a key aimed at a floating widget panel means. The Editor executes
/// the named effect; `FallThrough` / `BlurUnconsumed` mean the key was
/// *not* consumed and continues down the normal dispatch pipeline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WidgetKeyOutcome {
    /// Not the panel's at all: the key goes on to the editor's own
    /// keyboard unchanged. A pane-mounted panel's answer for what its
    /// widgets decline.
    FallThrough,
    /// Blur the panel and let the editor handle the key (a non-modal
    /// panel's Esc).
    Blur,
    /// Blur the panel and let the editor handle the key (dock-style
    /// unhandled shortcut: e.g. Ctrl+P should still open the palette).
    BlurUnconsumed,
    /// Esc default: fire a `cancel` widget_event at the focused widget,
    /// then unmount the panel.
    CancelAndUnmount,
    /// Route a named smart key ("Enter", "Tab", "S-Right", "C-S-Left", …)
    /// through the widget command dispatcher. The name carries the
    /// modifiers the kinds' vocabulary distinguishes: `C-` and `S-` on the
    /// caret keys, so a field extends its selection on Shift+arrow and
    /// steps a word on Ctrl+arrow (`Text::on_key`); "Shift+Tab" keeps its
    /// historical spelling.
    SmartKey(String),
    /// Feed a printable character to the focused TextInput.
    TextChar(char),
    /// Clipboard / selection chord for the focused Text widget.
    Paste,
    Copy,
    Cut,
    SelectAll,
    /// Consumed with no effect — the modal owns the input channel.
    Swallow,
}

/// Decide what a keystroke aimed at a mounted floating widget panel
/// means. Pure: reads the [`WidgetPanelView`] and the keymap, mutates
/// nothing. See the outcome variants for the effect vocabulary.
///
/// **The panel's own chords are its keymap's, not the router's.** A key the
/// panel's plugin mode binds — the dock's `/`, Esc, Enter, its Alt chords —
/// is taken on the tree by `view::shell::panel::Keymap` before the router is
/// asked, so what arrives here is the generic vocabulary every panel shares:
/// the widget keys the kinds answer, the characters a field types, and what
/// an unbound chord does to a modal versus a non-modal panel.
pub fn widget_panel_key(
    view: &WidgetPanelView,
    kb: &KeybindingResolver,
    code: KeyCode,
    modifiers: KeyModifiers,
) -> WidgetKeyOutcome {
    use WidgetKeyOutcome::*;

    if code == KeyCode::Esc {
        // A pane's panel has nothing to leave: Esc is the buffer's.
        if view.pane {
            return FallThrough;
        }
        // A non-modal panel stays mounted: Esc leaves it.
        if view.non_modal {
            return Blur;
        }
        return CancelAndUnmount;
    }

    // **Reading, not editing a field**: a page whose caret is the buffer's,
    // with no text widget holding the keyboard. Every motion key belongs to
    // the editor there. See [`WidgetPanelView::page`].
    let reader = view.page && !view.focused_widget_is_text;

    let key_name: Option<String> = match code {
        KeyCode::Tab => Some(
            if modifiers.contains(KeyModifiers::SHIFT) {
                "Shift+Tab"
            } else {
                "Tab"
            }
            .to_string(),
        ),
        KeyCode::BackTab => Some("Shift+Tab".to_string()),
        KeyCode::Enter => Some("Enter".to_string()),
        // Ctrl deletes a word rather than a character (`Text::on_key`).
        KeyCode::Backspace | KeyCode::Delete => {
            let base = if code == KeyCode::Backspace {
                "Backspace"
            } else {
                "Delete"
            };
            let ctrl = modifiers.contains(KeyModifiers::CONTROL);
            Some(format!("{}{base}", if ctrl { "C-" } else { "" }))
        }
        KeyCode::PageUp | KeyCode::PageDown if !reader => Some(
            if code == KeyCode::PageUp {
                "PageUp"
            } else {
                "PageDown"
            }
            .to_string(),
        ),
        // The caret keys carry their modifiers: a field's selection is
        // extended by Shift and its words are stepped by Ctrl, and the
        // kinds name those `S-Left`, `C-Right`, `C-S-Left` (`Text::on_key`).
        KeyCode::Home
        | KeyCode::End
        | KeyCode::Left
        | KeyCode::Right
        | KeyCode::Up
        | KeyCode::Down
            if !reader =>
        {
            let base = match code {
                KeyCode::Home => "Home",
                KeyCode::End => "End",
                KeyCode::Left => "Left",
                KeyCode::Right => "Right",
                KeyCode::Up => "Up",
                _ => "Down",
            };
            let ctrl = modifiers.contains(KeyModifiers::CONTROL);
            let shift = modifiers.contains(KeyModifiers::SHIFT);
            Some(format!(
                "{}{}{base}",
                if ctrl { "C-" } else { "" },
                if shift { "S-" } else { "" }
            ))
        }
        _ => None,
    };
    // **A key the panel's mode binds never gets here.** The panel's keymap
    // rides on its node (`view::shell::panel::Keymap`) and takes such a key
    // on the tree's capture leg, so the router sees only what the mode left.
    if let Some(name) = key_name {
        return SmartKey(name);
    }

    if let KeyCode::Char(c) = code {
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
            if view.pane {
                return FallThrough;
            }
            if view.non_modal {
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
            return SmartKey("Space".to_string());
        }
        return TextChar(ch);
    }

    // Any other keystroke (function keys, unhandled keycodes, …) is
    // swallowed — the modal is the exclusive owner of the input channel
    // until it unmounts. A pane's panel owns no channel: the key is the
    // buffer's.
    if view.pane {
        return FallThrough;
    }
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
    /// A navigation key forwarded despite the text-input mode block: it
    /// resolves in the `Normal` context to the buffer's own motion.
    Forward(Action),
    /// Consumed with no effect (unbound key in a text-input or
    /// read-only mode).
    Block,
    /// No mode claims the key — continue down the pipeline.
    FallThrough,
}

/// The character this key event types, if it types one.
///
/// One definition for both places a mode can capture text, because two
/// hand-rolled copies of "is this typing" is how they drift. It defers
/// the modifier question to [`crate::input::keybindings::is_text_input_modifier`],
/// which is the editor's own answer and knows the case a bare
/// `!intersects(CONTROL | ALT)` gets wrong: on Windows crossterm reports
/// AltGr as Ctrl+Alt, so excluding Ctrl+Alt excludes `@ [ ] { }` on
/// German, French and Italian layouts — the characters those keyboards
/// need most.
fn typed_char(event: &KeyEvent) -> Option<char> {
    let KeyCode::Char(c) = event.code else {
        return None;
    };
    if !crate::input::keybindings::is_text_input_modifier(event.modifiers) {
        return None;
    }
    Some(if event.modifiers.contains(KeyModifiers::SHIFT) {
        c.to_uppercase().next().unwrap_or(c)
    } else {
        c
    })
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

    // **A focused text field's keys never reach here.** A panel's widgets
    // answer their keys through the widget router (`widget_panel_key`)
    // before the buffer's route is asked, and the panel's own keymap
    // (`view::shell::panel::Keymap`) lets a printable key through to a
    // focused field ahead of the mode's bindings. What arrives at this
    // stage is a key no widget took.
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
        if let Some(ch) = typed_char(event) {
            return ModeKeyDisposition::TextInput(ch);
        }
        // Navigation is not the mode's to swallow.
        //
        // A mode declaring `allow_text_input` owns the keyboard, and
        // everything it did not name was blocked here. That made every
        // such page re-declare the whole navigation set — arrows, page
        // keys, Home/End — and hand each one back to the host through a
        // plugin handler, which is a lot of plumbing to arrive at the
        // behaviour the host already implements. The welcome screen had
        // nine such handlers; before they were written, Home and End
        // simply did nothing on it.
        //
        // So navigation resolves the way it would anywhere else: to the
        // focused text widget when there is one, and to the buffer
        // otherwise. Character keys are still captured above, and every
        // other unbound key is still blocked — a focused search field
        // must not let Ctrl+O through.
        // Plain navigation only. Shift+nav with no focused widget stays
        // blocked, as it was: extending a *buffer* selection is not what
        // a page like this is for, and widening that here would be a
        // behaviour change to every `allow_text_input` mode rather than
        // a fix to the one thing that was broken.
        let plain = !event
            .modifiers
            .intersects(KeyModifiers::SHIFT | KeyModifiers::ALT);
        let nav = matches!(
            event.code,
            KeyCode::Left
                | KeyCode::Right
                | KeyCode::Up
                | KeyCode::Down
                | KeyCode::Home
                | KeyCode::End
                | KeyCode::PageUp
                | KeyCode::PageDown
        );
        if plain && nav {
            let action = kb.resolve(event, KeyContext::Normal);
            if action != Action::None {
                return ModeKeyDisposition::Forward(action);
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

    /// Esc leaves a non-modal panel mounted and blurs it; on a centered
    /// modal it cancels and unmounts. A panel's own Esc — the dock's
    /// "back to the list from the filter" — is its keymap's, on the tree,
    /// and never reaches here.
    #[test]
    fn esc_blurs_a_non_modal_panel_and_cancels_a_modal() {
        let kb = resolver();
        let view = |non_modal: bool| WidgetPanelView {
            non_modal,
            pane: false,
            focus_key: Some("sessions".to_string()),
            focused_widget_is_text: false,
            page: false,
        };
        assert_eq!(
            widget_panel_key(&view(true), &kb, KeyCode::Esc, KeyModifiers::NONE),
            WidgetKeyOutcome::Blur
        );
        assert_eq!(
            widget_panel_key(&view(false), &kb, KeyCode::Esc, KeyModifiers::NONE),
            WidgetKeyOutcome::CancelAndUnmount
        );
    }

    #[test]
    fn modal_swallows_chords_a_non_modal_panel_blurs_through() {
        let kb = resolver();
        let mk = |non_modal| WidgetPanelView {
            non_modal,
            pane: false,
            focus_key: None,
            focused_widget_is_text: false,
            page: false,
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

    /// A pane-mounted panel's widgets answer their keys like the dock's;
    /// what they decline is the buffer's own route, with nothing to blur,
    /// cancel or swallow.
    #[test]
    fn a_panes_panel_hands_what_its_widgets_decline_to_the_buffer() {
        let kb = resolver();
        let view = WidgetPanelView {
            non_modal: true,
            pane: true,
            focus_key: Some("lst".to_string()),
            focused_widget_is_text: false,
            page: false,
        };
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Down, KeyModifiers::NONE),
            WidgetKeyOutcome::SmartKey("Down".to_string())
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Right, KeyModifiers::SHIFT),
            WidgetKeyOutcome::SmartKey("S-Right".to_string()),
            "a caret key carries its modifiers to the kind"
        );
        assert_eq!(
            widget_panel_key(
                &view,
                &kb,
                KeyCode::Left,
                KeyModifiers::CONTROL | KeyModifiers::SHIFT
            ),
            WidgetKeyOutcome::SmartKey("C-S-Left".to_string())
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Esc, KeyModifiers::NONE),
            WidgetKeyOutcome::FallThrough
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::Char('p'), KeyModifiers::CONTROL),
            WidgetKeyOutcome::FallThrough
        );
        assert_eq!(
            widget_panel_key(&view, &kb, KeyCode::F(2), KeyModifiers::NONE),
            WidgetKeyOutcome::FallThrough
        );
    }

    #[test]
    fn text_input_mode_captures_plain_chars_and_blocks_chords() {
        let kb = resolver();
        let view = ModeKeyView {
            effective_mode: Some("search-replace-list".to_string()),
            allows_text_input: true,
            global_mode_read_only: None,
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
        // Chords and unbound named keys are blocked: a focused field's
        // clipboard chords never reach the mode (the widget router takes
        // them), and a page with no field must not let Ctrl+O through.
        assert_eq!(
            mode_key_disposition(
                &view,
                &[],
                &kb,
                &event(KeyCode::Char('c'), KeyModifiers::CONTROL)
            ),
            ModeKeyDisposition::Block
        );
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
    fn read_only_global_mode_blocks_unbound_keys() {
        let kb = resolver();
        let mk = |read_only| ModeKeyView {
            effective_mode: Some("vi-normal".to_string()),
            allows_text_input: false,
            global_mode_read_only: Some(read_only),
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
            non_modal: false,
            pane: false,
            focus_key: Some("path".to_string()),
            focused_widget_is_text: true,
            page: false,
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
