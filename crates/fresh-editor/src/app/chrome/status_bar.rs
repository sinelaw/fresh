//! The status bar row.

use crate::app::types::HoverTarget;
use crate::input::keybindings::Action;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct StatusBar;

impl ChromeComponent for StatusBar {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(area) = ed.status_bar_area_now() {
            let mut b = LayoutBox::plain(
                "chrome:status_bar",
                area.y as u32,
                area.x as u32,
                area.width as u32,
                area.height as u32,
            );
            b.z = 40;
            t.push(b);
        }
    }

    fn hover(&self, ed: &mut Editor, _bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        // One generic hit-test over every clickable segment, on geometry
        // derived from live state (encoding, LSP, remote, ...).
        let (area, layout) = ed.status_bar_layout_now()?;
        if row != area.y {
            return None;
        }
        for (id, indicator_row, start, end) in &layout.clickable {
            if row == *indicator_row && col >= *start && col < *end {
                return Some(HoverTarget::StatusBarClickable(*id));
            }
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        if ev.press != PointerPress::Left {
            return Ok(Disposition::Pass);
        }
        if let Some(r) = ed.handle_click_status_bar(ev.col, ev.row) {
            r?;
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Map a click on a status-bar segment to its editor `Action`. This is the
    /// single id→action table for the generic click rail; adding a clickable
    /// element means adding one arm here (plus listing it in
    /// `StatusBarRenderer::clickable_for_kind`).
    ///
    /// Most segments dismiss any open menu-style popup first (the #1941
    /// follow-up: otherwise a stale popup overlaps the new prompt). The LSP,
    /// remote, and read-only menus are the exceptions — each owns a toggle
    /// (a second click closes it), so dismissing first would defeat the toggle;
    /// they clear other popups themselves after their toggle check.
    fn dispatch_status_bar_click(
        &mut self,
        id: crate::view::ui::status_bar::StatusBarClickable,
    ) -> AnyhowResult<()> {
        use crate::view::ui::status_bar::StatusBarClickable as C;
        match id {
            C::LineEnding => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetLineEnding)
            }
            C::Encoding => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetEncoding)
            }
            C::Language => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetLanguage)
            }
            // Owns its own toggle (second click closes the popup).
            C::Lsp => self.handle_action(Action::ShowLspStatus),
            // Owns its own toggle; clears other popups itself after the check.
            C::RemoteIndicator => self.handle_action(Action::ShowRemoteIndicatorMenu),
            C::WorkspaceTrust => {
                // Opens the (cancellable) workspace-trust prompt.
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::WorkspaceTrustPrompt)
            }
            C::Warnings => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::ShowWarnings)
            }
            C::Messages => self.handle_action(Action::ShowStatusLog),
            // Owns its own toggle (second click closes the read-only menu).
            C::ReadOnly => self.handle_action(Action::ShowReadOnlyMenu),
            C::Update => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::UpdateFresh)
            }
            C::RestartTerminal => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::RestartTerminal)
            }
        }
    }

    pub(super) fn handle_click_status_bar(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let (area, layout) = self.status_bar_layout_now()?;
        if row != area.y {
            return None;
        }
        // Generic click rail: one hit-test over every clickable segment,
        // on geometry derived from live state. The id→Action mapping (and
        // each element's popup-dismiss nuance) lives in
        // `dispatch_status_bar_click`.
        for (id, r, s, e) in layout.clickable {
            if row == r && col >= s && col < e {
                return Some(self.dispatch_status_bar_click(id));
            }
        }
        // Plugin-registered tokens. On a hit, fire
        // `status_bar_token_clicked` so the registering plugin can react.
        // We split the registry key (`"<plugin>:<token>"`) on the first
        // colon — that's how `register_status_bar_element` builds it.
        for (key, (r, s, e)) in layout.plugin_token_areas {
            if row == r && col >= s && col < e {
                let (plugin_name, token_name) = match key.split_once(':') {
                    Some((p, t)) => (p.to_string(), t.to_string()),
                    None => (String::new(), key.clone()),
                };
                self.dismiss_menu_popups_for_prompt();
                self.plugin_manager.read().unwrap().run_hook(
                    "status_bar_token_clicked",
                    crate::services::plugins::hooks::HookArgs::StatusBarTokenClicked {
                        plugin_name,
                        token_name,
                    },
                );
                return Some(Ok(()));
            }
        }
        None
    }
}
