//! The status bar row.

use crate::input::keybindings::Action;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct StatusBar;

impl ChromeComponent for StatusBar {
    /// **Nothing of the bar's input.** Every element is a keyed node in the
    /// shell's tree and answers its own press and hover — built-in indicators
    /// and plugin tokens alike. What is left is the box that claims the row,
    /// so a press on a *gap* between elements still lands on the bar rather
    /// than falling through to whatever is beneath it.
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
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Map a click on a status-bar segment to its editor `Action`. This is the
    /// single id→action table for the generic click rail. The roster is
    /// enforced by exhaustiveness at BOTH ends, not by this comment: a new
    /// `StatusBarClickable` variant fails to compile here (no wildcard arm),
    /// and a new `ElementKind` fails to compile in
    /// `StatusBarRenderer::clickable_for_kind` (whose non-clickable kinds are
    /// an explicit list, also wildcard-free).
    ///
    /// Most segments dismiss any open menu-style popup first (the #1941
    /// follow-up: otherwise a stale popup overlaps the new prompt). The LSP,
    /// remote, and read-only menus are the exceptions — each owns a toggle
    /// (a second click closes it), so dismissing first would defeat the toggle;
    /// they clear other popups themselves after their toggle check.
    pub(crate) fn dispatch_status_bar_click(
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

    /// Fire a plugin's `status_bar_token_clicked` hook.
    ///
    /// The registry key is `"<plugin>:<token>"` — how
    /// `register_status_bar_element` builds it — so it splits on the first
    /// colon. Reached from the tree now: the token is a keyed element that
    /// answers its own press, rather than a rectangle a click rail searched
    /// for after missing every built-in indicator.
    pub(crate) fn fire_status_bar_token_click(&mut self, key: &str) {
        let (plugin_name, token_name) = match key.split_once(':') {
            Some((p, t)) => (p.to_string(), t.to_string()),
            None => (String::new(), key.to_string()),
        };
        self.dismiss_menu_popups_for_prompt();
        self.plugin_manager.read().unwrap().run_hook(
            "status_bar_token_clicked",
            crate::services::plugins::hooks::HookArgs::StatusBarTokenClicked {
                plugin_name,
                token_name,
            },
        );
    }
}
