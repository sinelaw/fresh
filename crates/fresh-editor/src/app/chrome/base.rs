//! The base surface: the tab-strip right-click fallback and the
//! wheel's drop floor (scroll surfaces each own their boxes in the
//! Splits / FileExplorer components; what falls to the base has
//! nothing scrollable under the pointer).

use crate::app::types::TabContextMenu;
use crate::view::ui::tabs::TabHit;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct Base;

impl ChromeComponent for Base {
    fn collect(&self, _ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:base", 0);
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press != PointerPress::Right {
            return Ok(Disposition::Pass);
        }
        // Right-click on a tab raises its context menu; anywhere else
        // on the base surface clears it. Context menus only make sense
        // for buffer tabs; groups are plugin-managed.
        let tab_hit =
            ed.active_layout().tab_layouts.iter().find_map(
                |(split_id, tab_layout)| match tab_layout.hit_test(ev.col, ev.row) {
                    Some(TabHit::TabName(target) | TabHit::CloseButton(target)) => {
                        target.as_buffer().map(|bid| (*split_id, bid))
                    }
                    _ => None,
                },
            );
        if let Some((split_id, buffer_id)) = tab_hit {
            ed.active_window_mut().tab_context_menu =
                Some(TabContextMenu::new(buffer_id, split_id, ev.col, ev.row + 1));
        } else {
            ed.active_window_mut().tab_context_menu = None;
        }
        Ok(Disposition::Consumed)
    }

    fn on_wheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        // The wheel's floor: chrome that owns no scrollable content —
        // the menu bar, the status bar, separators, empty frame —
        // DROPS the wheel rather than handing it to the focused pane
        // (sinelaw/fresh#2969). The scrollable surfaces (splits, tab
        // strips, the explorer) each claim their own boxes above; what
        // reaches the base has nothing under the pointer to move.
        Ok(super::Disposition::Consumed)
    }

    fn on_hwheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        // Same drop ruling as the vertical wheel.
        Ok(super::Disposition::Consumed)
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        use crate::input::keybindings::KeyContext;
        // The editor content is the keyboard owner of last resort.
        let base_context = if ed.active_window().is_composite_buffer(ed.active_buffer()) {
            KeyContext::CompositeBuffer
        } else {
            ed.active_window().key_context.clone()
        };
        out.push((
            super::layer_rank::EDITOR_BASE,
            Layer {
                kind: LayerKind::Editor,
                owns_keyboard: true,
                key_context: Some(base_context),
                blocks_terminal_input: false,
            },
        ));
    }

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        // The keyboard owner of last resort ALWAYS answers — the walk
        // terminates here (`handle_key` relies on it).
        Some(
            ed.dispatch_base_key(event.code, event.modifiers)
                .map(|_| crate::input::handler::InputResult::Consumed),
        )
    }
}

/// Behavior owned by this component: the key pipeline's tail (moved
/// verbatim from `handle_key` — the editor content's own keyboard
/// handling, reached when every layer above has declined).
impl Editor {
    /// Mode bindings, composite-buffer routing, then chord/keybinding
    /// resolution against the current context. The context is derived
    /// fresh from the layer stack here: a rung above may have
    /// dismissed a popup (completion returning `Ignored` closes it via
    /// deferred actions), and the old pipeline's post-modal recalc is
    /// exactly this.
    pub(super) fn dispatch_base_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::input::router;
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let context = self.get_key_context();

        // Only check buffer mode keybindings when the editor buffer has focus.
        // FileExplorer, Menu, Prompt, Popup contexts should not trigger mode bindings
        // (e.g. markdown-source's Enter handler should not fire while the explorer is focused).
        //
        // CompositeBuffer is included so a composite buffer's plugin-defined
        // mode (e.g. the review-diff `diff-view` mode) can bind keys the core
        // composite handling leaves free — like Enter / Alt+O to open the file
        // under the cursor. Keys the mode does not bind fall through unchanged
        // to the composite router and the CompositeBuffer keymap below, so
        // built-in hunk navigation (n/p/]/[) and close (q) are unaffected.
        let should_check_mode_bindings = matches!(
            context,
            crate::input::keybindings::KeyContext::Normal
                | crate::input::keybindings::KeyContext::CompositeBuffer
        );

        if should_check_mode_bindings {
            if let Some(result) = self.dispatch_mode_bindings(&key_event, code, modifiers) {
                return result;
            }
        }

        // --- Composite buffer input routing ---
        // If the active buffer is a composite buffer (side-by-side diff),
        // route remaining composite-specific keys (scroll, pane switch, close)
        // through CompositeInputRouter before falling through to regular
        // keybinding resolution. Hunk navigation (n/p/]/[) is handled by the
        // Action system via CompositeBuffer context bindings.
        {
            let active_buf = self.active_buffer();
            let active_split = self.effective_active_split();
            if self.active_window().is_composite_buffer(active_buf) {
                if let Some(handled) =
                    self.try_route_composite_key(active_split, active_buf, &key_event)
                {
                    return handled;
                }
            }
        }

        // Resolve the key against the current context, chords first —
        // the decision is [`router::chord_or_key`]. An abandoned chord
        // prefix is cleared so it can't poison the next key.
        let disposition = {
            let keybindings = self.keybindings.read().unwrap();
            router::chord_or_key(
                &self.active_window().chord_state,
                &keybindings,
                &key_event,
                context.clone(),
            )
        };
        match disposition {
            router::ChordDisposition::Chord(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.active_window_mut().chord_state.clear();
                self.handle_action(action)
            }
            router::ChordDisposition::Pending => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.active_window_mut().chord_state.push((code, modifiers));
                Ok(())
            }
            router::ChordDisposition::Resolved(action) => {
                self.active_window_mut().chord_state.clear();
                tracing::trace!("Context: {:?} -> Action: {:?}", context, action);
                // Cancel pending LSP requests on user actions (except LSP
                // actions themselves) so stale completions don't show up
                // after the user has moved on.
                if router::cancels_pending_lsp(&action) {
                    self.active_window_mut().cancel_pending_lsp_requests();
                }
                // Keys the file browser ignores (its Alt+letter toggles) resolve
                // here in the Prompt context; the resulting prompt/file-browser
                // actions belong to the browser's state machine, not the generic
                // handler.
                if self.is_file_open_active() && self.handle_file_open_action(&action) {
                    return Ok(());
                }
                // All remaining actions delegate to handle_action.
                self.handle_action(action)
            }
        }
    }
}
