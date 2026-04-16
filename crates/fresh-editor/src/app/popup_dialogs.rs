//! Popup-dialog orchestrators on `Editor`.
//!
//! These build and show various popups as buffer-level events:
//! warnings popup, LSP status popup (with refresh hook), file-message
//! popup, and a small text-properties query helper. The biggest of
//! these — build_and_show_lsp_status_popup — is ~315 lines of popup
//! construction that has nothing to do with buffer management proper;
//! it just needed access to the buffer to dispatch the ShowPopup event.

use rust_i18n::t;

use crate::app::warning_domains::WarningDomain;

use super::Editor;

impl Editor {
    /// Show warnings by opening the warning log file directly
    ///
    /// If there are no warnings, shows a brief status message.
    /// Otherwise, opens the warning log file for the user to view.
    pub fn show_warnings_popup(&mut self) {
        if !self.warning_domains.has_any_warnings() {
            self.status_message = Some(t!("warnings.none").to_string());
            return;
        }

        // Open the warning log file directly
        self.open_warning_log();
    }

    /// Show LSP status popup with details about servers active for the current buffer.
    /// Lists each server with its status and provides actions: restart, stop, view log.
    pub fn show_lsp_status_popup(&mut self) {
        // Toggle behavior: if the LSP popup is already showing, close it
        // instead of rebuilding and re-showing it.  This lets clicking the
        // status-bar LSP indicator a second time dismiss the popup, matching
        // the common affordance for status-bar menus.
        if self.pending_lsp_status_popup.is_some() {
            self.hide_popup();
            self.pending_lsp_status_popup = None;
            return;
        }

        let has_error = self.warning_domains.lsp.level() == crate::app::WarningLevel::Error;
        let language = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());

        // Compute the set of configured servers whose binaries are not
        // resolvable — plugins and the popup itself both need this to
        // decide between "offer to start" and "offer install help".
        let missing_servers: Vec<String> = self
            .config
            .lsp
            .get(&language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| c.enabled && !c.command.is_empty())
                    .filter(|c| !crate::services::lsp::command_exists(&c.command))
                    .map(|c| c.command.clone())
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self.is_lsp_language_user_dismissed(&language);

        // Fire the LspStatusClicked hook for plugins
        self.plugin_manager.run_hook(
            "lsp_status_clicked",
            crate::services::plugins::hooks::HookArgs::LspStatusClicked {
                language: language.clone(),
                has_error,
                missing_servers,
                user_dismissed,
            },
        );

        self.build_and_show_lsp_status_popup(&language);
    }

    /// Rebuild the LSP-status popup in place if it's currently open.
    ///
    /// Used when an async event (progress update, server state change) might
    /// change the popup's contents — notably while rust-analyzer is indexing
    /// and emits `$/progress` every few hundred ms.  Without this, the popup
    /// would freeze on the snapshot taken at open time while the status-bar
    /// spinner keeps moving, making them look disconnected.
    pub fn refresh_lsp_status_popup_if_open(&mut self) {
        if self.pending_lsp_status_popup.is_none() {
            return;
        }
        let language = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());
        // Replace contents: hide then rebuild.  hide_popup() clears
        // pending_lsp_status_popup via handle_popup_cancel pathways, but
        // here we're calling it directly without routing through a cancel,
        // so stash and restore the marker so the rebuild sees "already
        // open" and doesn't fall through the toggle branch.
        let was_pending = self.pending_lsp_status_popup.take();
        self.hide_popup();
        drop(was_pending);
        self.build_and_show_lsp_status_popup(&language);
    }

    fn build_and_show_lsp_status_popup(&mut self, language: &str) {
        use crate::services::async_bridge::LspServerStatus;

        // Build a unified list of all configured servers for this language,
        // merged with their runtime status (if running).
        let running_statuses: std::collections::HashMap<String, LspServerStatus> = self
            .lsp_server_statuses
            .iter()
            .filter(|((lang, _), _)| lang == language)
            .map(|((_, name), status)| (name.clone(), *status))
            .collect();

        let configured_servers: Vec<String> = self
            .config
            .lsp
            .get(language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| !c.command.is_empty())
                    .map(|c| c.display_name())
                    .collect()
            })
            .unwrap_or_default();

        // Per-server binary availability map (display_name → bool).
        // `command_exists` is cached, so repeated popup opens or a
        // refresh-while-open are cheap.  We look up by display name
        // because `all_servers` below is built from display names;
        // LspServerConfig::display_name() falls back to the command
        // basename when no explicit `name` is set.
        let missing_by_server: std::collections::HashMap<String, bool> = self
            .config
            .lsp
            .get(language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| !c.command.is_empty())
                    .map(|c| {
                        (
                            c.display_name(),
                            !crate::services::lsp::command_exists(&c.command),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self.is_lsp_language_user_dismissed(language);

        if configured_servers.is_empty() && running_statuses.is_empty() {
            self.status_message = Some(t!("lsp.no_server_active").to_string());
            return;
        }

        // Merge: start with configured servers, then add any running servers
        // not in the config (shouldn't happen, but be safe).
        let mut all_servers: Vec<String> = configured_servers;
        for name in running_statuses.keys() {
            if !all_servers.contains(name) {
                all_servers.push(name.clone());
            }
        }
        all_servers.sort();

        // Build the popup's items as view-level `PopupListItem`s directly.
        // We bypass the `PopupListItemData` event type here because we need
        // the `disabled` field (for "View Log" when no log exists), which
        // is a view-only concern and plumbing it through the event boundary
        // would require touching ~40 existing literals across the test
        // suite.
        let mut items: Vec<crate::view::popup::PopupListItem> = Vec::new();
        let mut action_keys: Vec<(String, String)> = Vec::new();

        /// Truncate `s` to at most `max_cells` display cells, appending an
        /// ellipsis if truncation happened (the ellipsis is included in the
        /// budget, so the result is ≤ `max_cells` wide regardless of input).
        fn truncate(s: &str, max_cells: usize) -> String {
            use unicode_width::UnicodeWidthChar;
            let w = unicode_width::UnicodeWidthStr::width(s);
            if w <= max_cells {
                return s.to_string();
            }
            let budget = max_cells.saturating_sub(1);
            let mut used = 0;
            let mut out = String::new();
            for ch in s.chars() {
                let cw = ch.width().unwrap_or(0);
                if used + cw > budget {
                    break;
                }
                used += cw;
                out.push(ch);
            }
            out.push('…');
            out
        }
        const PROGRESS_FIELD_MAX: usize = 14;
        const POPUP_WIDTH_MAX: u16 = 50;

        for name in &all_servers {
            let status = running_statuses.get(name).copied();
            let is_active = status
                .map(|s| !matches!(s, LspServerStatus::Shutdown))
                .unwrap_or(false);
            // A server is "missing" only when it's NOT currently running
            // (an absolute-path binary could have been removed mid-session,
            // but the live server is still talking to us).
            let binary_missing =
                !is_active && missing_by_server.get(name).copied().unwrap_or(false);

            // Header: server name + status (data = None → not clickable,
            // not underlined).  Swap the "not running" label for a more
            // actionable "binary not found" when we can see up-front that
            // a start attempt would fail — this is the user-visible half
            // of the pre-click probe.
            let (icon, label) = match status {
                Some(LspServerStatus::Running) => ("●", "ready"),
                Some(LspServerStatus::Error) => ("✗", "error"),
                Some(LspServerStatus::Starting) => ("◌", "starting"),
                Some(LspServerStatus::Initializing) => ("◌", "initializing"),
                Some(LspServerStatus::Shutdown) | None => {
                    if binary_missing {
                        ("○", "binary not in PATH")
                    } else {
                        ("○", "not running")
                    }
                }
            };
            items.push(crate::view::popup::PopupListItem::new(format!(
                "{} {} ({})",
                icon, name, label
            )));

            // Progress row immediately UNDER the server's name row, if
            // there's an active `$/progress` notification for this
            // language.  Indented to match the action rows below, and the
            // title + message fields are individually truncated so a
            // runaway progress path can't stretch the popup.  The popup
            // width is pinned in advance (see below) so the row's content
            // changing never reshapes the popup.
            if let Some(info) = self
                .lsp_progress
                .values()
                .find(|info| info.language == language)
            {
                let mut line = format!("    ⏳ {}", truncate(&info.title, PROGRESS_FIELD_MAX));
                if let Some(ref msg) = info.message {
                    line.push_str(&format!(" · {}", truncate(msg, PROGRESS_FIELD_MAX)));
                }
                if let Some(pct) = info.percentage {
                    line.push_str(&format!(" ({}%)", pct));
                }
                items.push(crate::view::popup::PopupListItem::new(line));
            }

            if is_active {
                // Restart
                let restart_key = format!("restart:{}/{}", language, name);
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Restart {}", name))
                        .with_data(restart_key.clone()),
                );
                action_keys.push((restart_key, format!("Restart {}", name)));

                // Stop
                let stop_key = format!("stop:{}/{}", language, name);
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Stop {}", name))
                        .with_data(stop_key.clone()),
                );
                action_keys.push((stop_key, format!("Stop {}", name)));
            } else if binary_missing {
                // Show a disabled advisory row instead of an actionable
                // "Start" — clicking Start here would spawn, fail, and
                // noise up the status area.  The per-language
                // Install/Dismiss actions are added once at the end of
                // the popup, below.
                items.push(
                    crate::view::popup::PopupListItem::new(format!(
                        "    Install {} to enable",
                        name
                    ))
                    .disabled(),
                );
            } else {
                // Start
                let start_key = format!("start:{}", language);
                if !action_keys.iter().any(|(k, _)| k == &start_key) {
                    items.push(
                        crate::view::popup::PopupListItem::new(format!("    Start {}", name))
                            .with_data(start_key.clone()),
                    );
                    action_keys.push((start_key, format!("Start {}", name)));
                }
            }
        }

        // Dismiss / Enable row — shown whenever the language has at
        // least one configured server.  Gives the user a surface to
        // mute the pill (dim style) and, later, to restore it.  We
        // reuse `all_servers.is_empty()` as the "nothing here" signal
        // since languages with zero configured-or-running servers
        // already bailed out above.
        if user_dismissed {
            let enable_key = format!("enable:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!(
                    "    Enable LSP pill for {}",
                    language
                ))
                .with_data(enable_key.clone()),
            );
            action_keys.push((enable_key, format!("Enable LSP for {}", language)));
        } else {
            let dismiss_key = format!("dismiss:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!(
                    "    Disable LSP pill for {}",
                    language
                ))
                .with_data(dismiss_key.clone()),
            );
            action_keys.push((dismiss_key, format!("Disable LSP for {}", language)));
        }

        // View log action (always, at the end) — grayed out and
        // non-actionable when no log file exists yet for this language
        // (e.g. the server was never started, or has been rotated away).
        let log_path = crate::services::log_dirs::lsp_log_path(language);
        let log_exists = log_path.exists();
        let log_key = format!("log:{}", language);
        let mut log_item = crate::view::popup::PopupListItem::new("    View Log".to_string());
        if log_exists {
            log_item = log_item.with_data(log_key.clone());
            action_keys.push((log_key, "View Log".to_string()));
        } else {
            log_item = log_item.disabled();
        }
        items.push(log_item);

        // Store action keys for handling confirmation
        self.pending_lsp_status_popup = Some(action_keys);

        // Pin the popup width up-front, using the *worst-case* widths for
        // any row that varies at runtime (the progress line).  This keeps
        // the popup from jittering when progress messages come and go or
        // change length — the whole point of the spinner + live-refresh
        // pair is that the UI should look stable while the LSP churns.
        //
        //   worst-case progress line =
        //     "    ⏳ " (4-space indent + ⏳ (2 cells) + space = 7 cells)
        //     + PROGRESS_FIELD_MAX   (title)
        //     + " · "                (3 cells)
        //     + PROGRESS_FIELD_MAX   (message)
        //     + " (100%)"            (7 cells)
        //   = 7 + 14 + 3 + 14 + 7 = 45 cells
        const PROGRESS_LINE_MAX: usize = 7 + PROGRESS_FIELD_MAX + 3 + PROGRESS_FIELD_MAX + 7;
        let max_static_item_width = items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(20);
        let popup_width =
            (max_static_item_width.max(PROGRESS_LINE_MAX) as u16 + 4).clamp(30, POPUP_WIDTH_MAX);

        // Pre-select the first actionable item (skip header items with no
        // data and disabled items like a non-existent View Log).
        let first_actionable = items
            .iter()
            .position(|i| i.data.is_some() && !i.disabled)
            .unwrap_or(0);

        // Left-align the popup's column with the LSP indicator on the
        // status bar, if we know where it was drawn in the last frame.
        // Falls back to the previous BottomRight anchor when the LSP
        // segment isn't visible (e.g. first render).
        let position = self
            .cached_layout
            .status_bar_lsp_area
            .map(
                |(_, col_start, _)| crate::view::popup::PopupPosition::AboveStatusBarAt {
                    x: col_start,
                },
            )
            .unwrap_or(crate::view::popup::PopupPosition::BottomRight);

        use crate::view::popup::{Popup, PopupContent, PopupKind};
        use ratatui::style::Style;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some(format!("LSP Servers ({})", language)),
            description: None,
            transient: false,
            content: PopupContent::List {
                items,
                selected: first_actionable,
            },
            position,
            width: popup_width,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(self.theme.popup_border_fg),
            background_style: Style::default().bg(self.theme.popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
        };

        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.popups.show(popup);
        }
    }

    /// Show a transient hover popup with the given message text, positioned below the cursor.
    /// Used for file-open messages (e.g. `file.txt:10@"Look at this"`).
    pub fn show_file_message_popup(&mut self, message: &str) {
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        // Build markdown: message text + blank line + italic hint
        let md = format!("{}\n\n*esc to dismiss*", message);
        // Size popup width to content: longest line + border padding, clamped to reasonable bounds
        let content_width = message.lines().map(|l| l.len()).max().unwrap_or(0) as u16;
        let hint_width = 16u16; // "*esc to dismiss*"
        let popup_width = (content_width.max(hint_width) + 4).clamp(20, 60);

        let mut popup = Popup::markdown(&md, &self.theme, Some(&self.grammar_registry));
        popup.transient = false;
        popup.position = PopupPosition::BelowCursor;
        popup.width = popup_width;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.popups.show(popup);
        }
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::primitives::text_property::TextProperty>> {
        let state = self.buffers.get(&self.active_buffer())?;
        let cursor_pos = self.active_cursors().primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }
}
