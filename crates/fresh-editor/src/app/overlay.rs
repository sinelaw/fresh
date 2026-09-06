//! What is layered over the editor's content, read off the shell tree.
//!
//! The editor presents overlays on top of its content — full-screen modals
//! (settings, keybinding editor, calibration wizard, workspace-trust prompt),
//! the menu, the prompt, popups, the centered widget modal, the left dock and
//! the sidebar's sections — and four questions about them used to be
//! answered by a ranked stack of `Layer` declarations that every surface
//! kept in step with the tree by hand: which keyboard vocabulary applies,
//! whether a terminal's PTY may take raw input, whether the editor's own
//! content holds the keyboard, and whether a modal surface covers the
//! content. Every one of them is a property of the tree — where focus is,
//! and which layers are up with which modality — so they are read from it,
//! and the stack, its ranks and its kinds are gone.
//!
//! **Read over a tree as current as the facts.** A key or a pointer event may
//! change any of the state the description reads, so `handle_key` and
//! `handle_mouse` lay the tree out from the facts before routing and mark
//! the description stale after; `get_key_context` lays it out again if a
//! fact was applied since (`Editor::lay_out_shell_if_stale`).

use super::Editor;

impl Editor {
    /// True while the workspace-trust prompt is the TOP of the global
    /// popup stack — the state in which its dedicated mouse/key
    /// handlers (and its dedicated overlay layer) take over from the
    /// generic popup treatment.
    pub(crate) fn workspace_trust_on_top(&self) -> bool {
        self.global_popups.top().is_some_and(|p| {
            matches!(
                p.resolver,
                crate::view::popup::PopupResolver::WorkspaceTrust
            )
        })
    }

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
    /// The one place this is decided: the frame declares the popup's
    /// keyboard seam from it (`view::shell::popup::keyboard`), which is what
    /// `get_key_context` then reads, and `dispatch_popup_keys` routes by it.
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

    /// Whether the tree's focus sits inside any layer — a modal, the menu,
    /// the prompt, a popup holding the keyboard, a focused dock, panel or
    /// sidebar section. When it does not, the editor's own content has the
    /// keyboard.
    /// Whether focus sits inside a layer that is an *overlay* — something
    /// layered over the content. A pane-mounted panel's keyboard layer is
    /// not one (`panel::is_base_layer`): it confines the ring to the panel
    /// in the pane, and the pane is the content.
    fn focus_in_a_layer(&self) -> bool {
        self.shell_ui.as_ref().is_some_and(|ui| {
            ui.layers_holding_focus().into_iter().any(|l| {
                !ui.key_of(l)
                    .is_some_and(|k| crate::view::shell::panel::is_base_layer(&k))
            })
        })
    }

    /// Whether a surface in the tree carries `key` — the presence question
    /// for a layer that is up whether or not it holds focus.
    fn shell_has(&self, key: &fresh_ui::Key) -> bool {
        self.shell_ui
            .as_ref()
            .is_some_and(|ui| ui.find_by_key(key).is_some())
    }

    /// True iff an overlay is blocking key routing to a terminal buffer's
    /// PTY child: something over the content holds the keyboard, or a popup
    /// or the centered widget panel is up over the content. A blurred dock
    /// leaves the dived-into terminal usable; a merely-visible popup does
    /// not (it covers the active buffer and the keystrokes belong to it).
    pub(crate) fn presents_blocking_overlay(&self) -> bool {
        self.focus_in_a_layer()
            || self.shell_has(&crate::view::shell::popup::popup_key(0))
            || self.shell_has(&crate::view::shell::panel::key())
    }

    /// True iff the editor pane itself owns the keyboard — nothing above
    /// it (menu, prompt, modal, context menu, dock, floating panel, a
    /// key-capturing popup) has claimed it.
    ///
    /// Asked by the bracketed-paste routing: a paste belongs to whatever
    /// owns the keyboard, and a panel mounted *into a buffer* only owns it
    /// when nothing is layered over that buffer.
    pub(crate) fn editor_base_owns_keyboard(&self) -> bool {
        !self.focus_in_a_layer()
    }

    /// True iff a modal overlay — a prompt's card, the menu, a full-screen
    /// modal (settings, keybinding editor, calibration wizard, workspace
    /// trust), a native context menu, or the centered widget modal —
    /// currently covers the editor content: a layer is up that swallows keys
    /// or blocks the pointer (`Ui::modal_up`). The dock, a sidebar section
    /// and the prompt's own row confine focus without covering anything,
    /// and are not modal.
    ///
    /// Used to suppress mouse-hover LSP requests while a modal overlay is
    /// up: positions under the pointer map to the buffer *behind* the
    /// overlay, so a hover there would query — and render a popup for —
    /// content the user cannot see (sinelaw/fresh#2912).
    pub(crate) fn modal_overlay_active(&self) -> bool {
        self.shell_ui.as_ref().is_some_and(|ui| ui.modal_up())
    }

    /// The keybinding context the next key resolves against: the vocabulary
    /// of whichever surface holds the keyboard.
    ///
    /// Read off the tree's focus chain, from the focused element outward,
    /// through `view::shell::frame::key_context_of`; a chain that names no
    /// surface is the editor's own content, whose context is the active
    /// window's (or `CompositeBuffer` for a composite buffer). The tree is
    /// laid out from the facts first, so a decision made earlier in this
    /// same key — a dismissal, a focus move — is what the answer reflects.
    pub fn get_key_context(&mut self) -> crate::input::keybindings::KeyContext {
        self.lay_out_shell_if_stale();
        if let Some(ui) = self.shell_ui.as_ref() {
            if let Some(f) = ui.focused() {
                for e in ui.path_to(f).into_iter().rev() {
                    let Some(k) = ui.key_of(e) else { continue };
                    if let Some(c) = crate::view::shell::frame::key_context_of(&k) {
                        return c;
                    }
                }
            }
        }
        self.base_key_context()
    }

    /// The editor content's own context.
    fn base_key_context(&self) -> crate::input::keybindings::KeyContext {
        use crate::input::keybindings::KeyContext;
        if self
            .active_window()
            .is_composite_buffer(self.active_buffer())
        {
            KeyContext::CompositeBuffer
        } else {
            self.active_window().key_context.clone()
        }
    }
}
