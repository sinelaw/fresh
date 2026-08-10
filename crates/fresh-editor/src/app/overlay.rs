//! Unified overlay **layer** model (P2).
//!
//! The editor presents a stack of overlays on top of the editor content:
//! the event-debug dialog, full-screen modals (settings, keybinding editor,
//! calibration wizard, workspace-trust prompt), the menu, the prompt,
//! popups, the centered widget modal, and the left dock. Each one used to
//! have its own focus-precedence, terminal-blocking and mouse-capture
//! logic scattered across `input.rs`, `input_dispatch.rs`, `mouse_input.rs`
//! and `render.rs` — and the conditional ladders went out of sync (the
//! mouse handler's modal precedence didn't match the keyboard handler's,
//! `dispatch_terminal_input`'s `in_modal` predicate over-listed the same
//! fields, the unfocused-popup guard re-listed Settings/Menu/Prompt).
//!
//! This module makes the stack a first-class ordered list. Every callsite
//! that asks "which overlay is in charge?" — keyboard focus
//! (`get_key_context`), the unfocused-popup modal guard
//! (`resolve_unfocused_popup_action`), the terminal-input gate
//! (`dispatch_terminal_input`) and the mouse early-capture ladder
//! (`handle_mouse`) — reads from the *same* `Editor::overlay_layers()`
//! list, so the precedence rules live in one place. The `impl Editor`
//! block at the bottom of this file is that source of truth: it builds
//! the layer stack from live editor state and answers the shared focus
//! queries (`popups_capture_keys`, `get_key_context`, …) every input
//! path consults.

use super::Editor;
use crate::input::keybindings::KeyContext;

/// Identifies a concrete overlay. The ordering of `overlay_layers`
/// (top-first), not this enum's declaration order, defines precedence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LayerKind {
    /// The event-debug dialog (`active_window().event_debug`) — a
    /// full-screen modal with its own input dispatcher.
    EventDebug,
    Settings,
    /// The keybinding editor (`keybinding_editor`) — a full-screen modal
    /// with its own input dispatcher; transparent to `KeyContext`-driven
    /// keybinding resolution.
    KeybindingEditor,
    /// The calibration wizard (`calibration_wizard`) — same as above.
    CalibrationWizard,
    /// The workspace-trust prompt: a global popup whose top resolver is
    /// `PopupResolver::WorkspaceTrust`, painted in the modal z-band and
    /// dispatched by a bespoke mouse/key handler. Distinct from `Popup`
    /// so its dedicated dispatchers can be located top-down by kind.
    WorkspaceTrust,
    Menu,
    Prompt,
    Popup,
    /// The tab bar's "+" new-tab popup (`active_window().new_tab_menu`). A
    /// modal chrome menu with a custom key dispatcher
    /// (`handle_context_menu_key`), so it's transparent to `KeyContext`
    /// resolution but still blocks PTY routing while open.
    NewTabMenu,
    /// The tab right-click context menu (`active_window().tab_context_menu`),
    /// same treatment as `NewTabMenu`.
    TabContextMenu,
    /// The file-explorer right-click context menu
    /// (`active_window().file_explorer_context_menu`), same treatment as
    /// `NewTabMenu` / `TabContextMenu`: a modal chrome menu with a custom key
    /// dispatcher, transparent to `KeyContext` resolution but blocking PTY
    /// routing while open.
    FileExplorerContextMenu,
    /// The close-split confirmation popup (`active_window().close_split_menu`),
    /// same treatment as the other native context menus: a modal chrome menu
    /// with a custom key dispatcher, transparent to `KeyContext` resolution but
    /// blocking PTY routing while open.
    CloseSplitMenu,
    /// The centered widget modal (`floating_widget_panel`).
    FloatingModal,
    /// The editor-global left dock (`dock`).
    Dock,
    /// The editor content / window splits — the bottom layer.
    Editor,
}

/// One entry in the overlay stack: a present overlay (or the always-present
/// editor base), with the per-layer flags the dispatchers need.
#[derive(Debug, Clone)]
pub(crate) struct Layer {
    pub kind: LayerKind,
    /// Whether this layer currently owns the keyboard. Modal layers set
    /// this whenever present; focusable layers (dock, popup) only while
    /// focused/capturing; the editor base always sets it so a top-down
    /// walk always terminates.
    pub owns_keyboard: bool,
    /// The keybinding context to resolve against when this layer is the
    /// keyboard owner. `None` for layers whose keys are intercepted by a
    /// custom dispatcher (event-debug, calibration wizard, keybinding
    /// editor) and never reach `KeyContext`-driven resolution — they are
    /// transparent to `resolve_focus_context`, which keeps walking below
    /// them.
    pub key_context: Option<KeyContext>,
    /// Whether this layer, while present, blocks routing of keys to the
    /// PTY child of a terminal buffer underneath. A blurred dock leaves
    /// the terminal usable; a merely-visible popup does not (it covers
    /// the active buffer and the user's keystrokes belong to the popup).
    pub blocks_terminal_input: bool,
}

/// Resolve the keyboard-owning `KeyContext` from an ordered (top-first)
/// layer list: the first owning layer that has a `KeyContext` wins.
/// Layers without a `KeyContext` (custom-dispatch modals) are skipped —
/// their input dispatcher has already intercepted keys upstream, so they
/// are transparent to `KeyContext`-driven resolution.
///
/// The editor base layer always owns and has a `KeyContext`, so this
/// never returns `None` for a well-formed stack.
pub(crate) fn resolve_focus_context(layers: &[Layer]) -> Option<KeyContext> {
    layers
        .iter()
        .find(|l| l.owns_keyboard && l.key_context.is_some())
        .and_then(|l| l.key_context.clone())
}

/// True iff any layer in the stack currently blocks routing to the PTY
/// child of a terminal buffer underneath.
pub(crate) fn any_layer_blocks_terminal_input(layers: &[Layer]) -> bool {
    layers.iter().any(|l| l.blocks_terminal_input)
}

/// True iff a layer ranked *above* the popup layer currently owns the
/// keyboard. Used by the unfocused-popup key interception: any owning
/// layer above `Popup` is exactly Settings / Menu / Prompt (the only
/// layers ranked higher), and while one of those is up the popup must
/// not intercept keys. Callers guarantee a `Popup` layer is present, so
/// the `take_while` stops before the editor base layer.
pub(crate) fn popup_blocked_by_higher_modal(layers: &[Layer]) -> bool {
    layers
        .iter()
        .take_while(|l| l.kind != LayerKind::Popup)
        .any(|l| l.owns_keyboard)
}

impl Editor {
    /// Whether editor-pane popups (LSP completion, hover, signature help,
    /// global plugin popups, …) should intercept keyboard input.
    ///
    /// Returns `false` when:
    ///   - the user has focus on the file explorer pane (popups belong
    ///     to the editor pane, and the explorer must own its own
    ///     keystrokes), or
    ///   - the topmost visible popup is unfocused (LSP popups appear
    ///     unfocused so they don't silently swallow the next keystroke;
    ///     the user grabs focus explicitly with `popup_focus`,
    ///     default `Alt+T`).
    ///
    /// Buffer-switch handlers (e.g. `open_file_preview`) clear stale
    /// popups so a popup tied to the previous preview doesn't follow the
    /// user across buffers.
    ///
    /// Single source of truth for both `get_key_context` (binding resolution)
    /// and `dispatch_modal_input` (handler routing) so the two cannot drift.
    pub(crate) fn popups_capture_keys(&self) -> bool {
        use crate::input::keybindings::KeyContext;
        use crate::view::popup::PopupResolver;
        // The workspace-trust prompt is an editor-wide modal shown at startup:
        // it must own the keyboard regardless of which pane is focused.
        // Opening a *directory* focuses the file-explorer pane, which would
        // otherwise short-circuit below and leave the (rendered) prompt
        // un-interactable.
        let trust_prompt_up = self
            .global_popups
            .top()
            .is_some_and(|p| p.focused && matches!(p.resolver, PopupResolver::WorkspaceTrust));
        if trust_prompt_up {
            return true;
        }
        if matches!(self.active_window().key_context, KeyContext::FileExplorer) {
            return false;
        }
        self.topmost_popup_focused()
    }

    /// Whether the topmost visible popup (global stack first, then the
    /// active buffer's stack) has been marked focused. Returns `false`
    /// when no popup is visible — the caller is responsible for
    /// short-circuiting that case.
    pub(crate) fn topmost_popup_focused(&self) -> bool {
        if let Some(popup) = self.global_popups.top() {
            return popup.focused;
        }
        if let Some(popup) = self.active_state().popups.top() {
            return popup.focused;
        }
        // No popup → no capture. Returning `false` here is safe because
        // every caller gates on visibility before reaching this path.
        false
    }

    /// Build the editor's overlay stack, ordered top-first (highest
    /// keyboard-focus precedence first), ending with the always-present
    /// editor base layer.
    ///
    /// This is the single source of truth for overlay precedence: focus
    /// resolution (`get_key_context`), the unfocused-popup modal guard
    /// (`resolve_unfocused_popup_action`), the terminal-input gate
    /// (`dispatch_terminal_input`), and the mouse early-capture ladder
    /// (`handle_mouse`) all read from this list rather than keeping their
    /// own conditional ladders.
    pub(crate) fn overlay_layers(&self) -> Vec<crate::app::overlay::Layer> {
        use crate::input::keybindings::KeyContext;

        let mut layers = Vec::new();

        // Event-debug dialog intercepts every key event ahead of every
        // other path (see `handle_key_event`), so it sits at the top of
        // the stack. Its dispatcher is custom (no `KeyContext`).
        if self.active_window().is_event_debug_active() {
            layers.push(Layer {
                kind: LayerKind::EventDebug,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // Full-screen modals own the keyboard whenever they are present.
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            layers.push(Layer {
                kind: LayerKind::Settings,
                owns_keyboard: true,
                key_context: Some(KeyContext::Settings),
                blocks_terminal_input: true,
            });
        }
        // Keybinding editor and calibration wizard install their own
        // input dispatchers (see `input_dispatch.rs`), so they are
        // transparent to `KeyContext`-driven keybinding resolution
        // (`key_context: None`) — but they fully own the keyboard while
        // present and block PTY routing.
        if self.keybinding_editor.is_some() {
            layers.push(Layer {
                kind: LayerKind::KeybindingEditor,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        if self.calibration_wizard.is_some() {
            layers.push(Layer {
                kind: LayerKind::CalibrationWizard,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The workspace-trust prompt is a `global_popups` entry with its
        // own modal z-band, key handler and mouse handler. When it's the
        // top of the global stack it takes the place of the generic
        // `Popup` layer so the dedicated handlers can be reached by
        // top-down kind dispatch (`handle_mouse`, `input_dispatch`).
        let trust_on_top = self.global_popups.top().is_some_and(|p| {
            matches!(
                p.resolver,
                crate::view::popup::PopupResolver::WorkspaceTrust
            )
        });
        if trust_on_top {
            layers.push(Layer {
                kind: LayerKind::WorkspaceTrust,
                owns_keyboard: self.popups_capture_keys(),
                key_context: Some(KeyContext::Popup),
                blocks_terminal_input: true,
            });
        }
        if self.menu_state.active_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::Menu,
                owns_keyboard: true,
                key_context: Some(KeyContext::Menu),
                blocks_terminal_input: true,
            });
        }
        if self.is_prompting() {
            // Find/replace prompts resolve in the narrower `SearchPrompt`
            // context, which owns the match-mode toggles and otherwise falls
            // through to `Prompt`. Every other prompt stays in `Prompt`, so
            // the toggle keys (Alt+W etc.) never fire outside an actual search.
            let key_context = if self.active_prompt_has_search_options() {
                KeyContext::SearchPrompt
            } else {
                KeyContext::Prompt
            };
            layers.push(Layer {
                kind: LayerKind::Prompt,
                owns_keyboard: true,
                key_context: Some(key_context),
                blocks_terminal_input: true,
            });
        }
        // A non-trust popup is *present* whenever visible, but only *owns*
        // the keyboard while capturing (`popups_capture_keys`); a
        // merely-visible unfocused popup falls through. Either way a
        // visible popup blocks PTY routing — it covers the active buffer.
        if !trust_on_top
            && (self.global_popups.is_visible() || self.active_state().popups.is_visible())
        {
            layers.push(Layer {
                kind: LayerKind::Popup,
                owns_keyboard: self.popups_capture_keys(),
                key_context: Some(KeyContext::Popup),
                blocks_terminal_input: true,
            });
        }
        // The tab-bar popups (the "+" new-tab menu and the tab right-click
        // context menu) are modal chrome: while one is open it owns the
        // keyboard via a custom dispatcher (`handle_context_menu_key`, run
        // from `handle_key` ahead of `KeyContext` resolution), so they expose
        // no `KeyContext` here.
        // Like any covering overlay they block PTY routing — otherwise keys
        // would leak into an active terminal buffer underneath instead of
        // driving the menu. Ranked below `Popup` so the unfocused-popup
        // `take_while` guard above is unaffected.
        if self.active_window().new_tab_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::NewTabMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        if self.active_window().tab_context_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::TabContextMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The file-explorer right-click context menu gets the same treatment
        // as the tab-bar menus: a custom key dispatcher owns the keyboard
        // while it's open (transparent to `KeyContext`), and it blocks PTY
        // routing so keys drive the menu rather than leaking into a terminal
        // buffer underneath.
        if self.active_window().file_explorer_context_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::FileExplorerContextMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The close-split confirmation popup gets the same treatment as the
        // other native context menus: a custom key dispatcher owns the keyboard
        // while it's open and it blocks PTY routing.
        if self.active_window().close_split_menu.is_some() {
            layers.push(Layer {
                kind: LayerKind::CloseSplitMenu,
                owns_keyboard: true,
                key_context: None,
                blocks_terminal_input: true,
            });
        }
        // The centered widget modal (picker / new-session form / plugin
        // overlay) owns the keyboard when focused. It resolves as `Normal`
        // regardless of the underlying buffer's (possibly stale) context so
        // mode-keybinding lookups still fire for the panel's own chords.
        // It blocks PTY routing whenever present — the modal sits on top
        // of (and obscures) the active terminal buffer.
        if let Some(f) = self.floating_widget_panel.as_ref() {
            layers.push(Layer {
                kind: LayerKind::FloatingModal,
                owns_keyboard: f.focused,
                key_context: Some(KeyContext::Normal),
                blocks_terminal_input: true,
            });
        }
        // The editor-global dock owns the keyboard only while focused; a
        // blurred dock stays visible but lets the buffer underneath keep
        // the keyboard *and* receive PTY routing (the dock lives beside
        // the chrome, not over it).
        if let Some(d) = self.dock.as_ref() {
            layers.push(Layer {
                kind: LayerKind::Dock,
                owns_keyboard: d.focused,
                key_context: Some(KeyContext::Dock),
                blocks_terminal_input: d.focused,
            });
        }
        // The editor content is the keyboard owner of last resort.
        let base_context = if self
            .active_window()
            .is_composite_buffer(self.active_buffer())
        {
            KeyContext::CompositeBuffer
        } else {
            self.active_window().key_context.clone()
        };
        layers.push(Layer {
            kind: LayerKind::Editor,
            owns_keyboard: true,
            key_context: Some(base_context),
            blocks_terminal_input: false,
        });

        layers
    }

    /// True iff any overlay layer is currently blocking key routing to a
    /// terminal buffer's PTY child. The single source of truth for the
    /// "is anything modal up?" question.
    pub(crate) fn presents_blocking_overlay(&self) -> bool {
        crate::app::overlay::any_layer_blocks_terminal_input(&self.overlay_layers())
    }

    /// Determine the current keybinding context based on UI state.
    ///
    /// Returns the `KeyContext` of the topmost overlay layer that owns the
    /// keyboard (see [`Editor::overlay_layers`]).
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        crate::app::overlay::resolve_focus_context(&self.overlay_layers())
            .expect("editor base layer always owns the keyboard")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layer(kind: LayerKind, owns: bool, ctx: Option<KeyContext>, blocks: bool) -> Layer {
        Layer {
            kind,
            owns_keyboard: owns,
            key_context: ctx,
            blocks_terminal_input: blocks,
        }
    }

    fn base() -> Layer {
        layer(LayerKind::Editor, true, Some(KeyContext::Normal), false)
    }

    #[test]
    fn topmost_owning_layer_wins() {
        let layers = [
            layer(
                LayerKind::Settings,
                false,
                Some(KeyContext::Settings),
                false,
            ),
            layer(LayerKind::Popup, true, Some(KeyContext::Popup), true),
            layer(LayerKind::Dock, true, Some(KeyContext::Dock), true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Popup));
    }

    #[test]
    fn falls_through_unfocused_layers_to_base() {
        let layers = [
            layer(
                LayerKind::FloatingModal,
                false,
                Some(KeyContext::Normal),
                true,
            ),
            layer(LayerKind::Dock, false, Some(KeyContext::Dock), false),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
    }

    #[test]
    fn base_layer_terminates_the_walk() {
        let layers = [base()];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
        assert!(!any_layer_blocks_terminal_input(&layers));
    }

    /// Custom-dispatch modals own the keyboard but expose no
    /// `KeyContext`. `resolve_focus_context` must walk past them and
    /// return the base context — matching the historical behavior when
    /// `get_key_context` happened to be queried while one of those
    /// modals was up.
    #[test]
    fn keycontext_walk_is_transparent_to_custom_dispatch_modals() {
        let layers = [
            layer(LayerKind::CalibrationWizard, true, None, true),
            base(),
        ];
        assert_eq!(resolve_focus_context(&layers), Some(KeyContext::Normal));
        assert!(any_layer_blocks_terminal_input(&layers));
    }

    /// A merely-visible (unfocused) popup blocks PTY routing — it
    /// covers the active buffer. A blurred dock does not block; a
    /// focused dock does.
    #[test]
    fn terminal_blocking_differs_from_keyboard_ownership() {
        let popup_visible_not_capturing = [
            layer(LayerKind::Popup, false, Some(KeyContext::Popup), true),
            base(),
        ];
        assert_eq!(
            resolve_focus_context(&popup_visible_not_capturing),
            Some(KeyContext::Normal),
        );
        assert!(any_layer_blocks_terminal_input(
            &popup_visible_not_capturing
        ));

        let blurred_dock = [
            layer(LayerKind::Dock, false, Some(KeyContext::Dock), false),
            base(),
        ];
        assert!(!any_layer_blocks_terminal_input(&blurred_dock));
    }
}
