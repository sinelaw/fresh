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

/// True when `popup` is the LSP status popup (as built by
/// `build_and_show_lsp_status_popup`). Used by the auto-prompt
/// drain to find and clean up orphan prompts on non-active
/// buffers without affecting unrelated popups (completion, hover,
/// etc.) that might be on top.
fn is_lsp_status_popup(popup: &crate::view::popup::Popup) -> bool {
    matches!(popup.resolver, crate::view::popup::PopupResolver::LspStatus)
}

impl Editor {
    /// Show warnings by opening the warning log file directly
    ///
    /// If there are no warnings, shows a brief status message.
    /// Otherwise, opens the warning log file for the user to view.
    pub fn show_warnings_popup(&mut self) {
        if !self.active_window_mut().warning_domains.has_any_warnings() {
            self.active_window_mut().status_message = Some(t!("warnings.none").to_string());
            return;
        }

        // Open the warning log file directly
        self.open_warning_log();
    }

    /// Show LSP status popup with details about servers active for the current buffer.
    /// Lists each server with its status and provides actions: restart, stop, view log.
    ///
    /// User-initiated (status-bar click, `lsp_status` action). The popup
    /// grabs focus on show because the user explicitly asked for it,
    /// matching the historical click-to-pick-action affordance.
    pub fn show_lsp_status_popup(&mut self) {
        // Toggle behavior: if the LSP popup is already showing, close it
        // instead of rebuilding and re-showing it.  This lets clicking the
        // status-bar LSP indicator a second time dismiss the popup, matching
        // the common affordance for status-bar menus.
        if self
            .active_state()
            .popups
            .top()
            .is_some_and(is_lsp_status_popup)
        {
            self.hide_popup();
            return;
        }

        let has_error =
            self.active_window_mut().warning_domains.lsp.level() == crate::app::WarningLevel::Error;
        let language = self
            .buffers()
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());

        // Compute the set of configured servers whose binaries are not
        // resolvable — plugins and the popup itself both need this to
        // decide between "offer to start" and "offer install help".
        // Probe missing binaries through the active authority. When the
        // LspManager isn't wired (tests or very early boot), fall
        // back to the synchronous host-side `which` probe — same path
        // `command_exists_via_authority` would take after the
        // long-running spawner bootstrap completes.
        let missing_servers: Vec<String> = self
            .config
            .lsp
            .get(&language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| c.enabled && !c.command.is_empty())
                    .filter(|c| match self.lsp() {
                        Some(mgr) => !mgr.command_exists_via_authority(&c.command),
                        None => !crate::services::lsp::command_exists(&c.command),
                    })
                    .map(|c| c.command.clone())
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self
            .active_window()
            .is_lsp_language_user_dismissed(&language);

        // Fire the LspStatusClicked hook for plugins. A plugin's
        // handler may itself push a popup (e.g. the embedded
        // rust-lsp.ts plugin shows install instructions when its
        // `rustLspError` is set).
        self.plugin_manager.read().unwrap().run_hook(
            "lsp_status_clicked",
            crate::services::plugins::hooks::HookArgs::LspStatusClicked {
                language: language.clone(),
                has_error,
                missing_servers,
                user_dismissed,
            },
        );

        // If something is already on the popup stack at this point
        // — either pushed by the hook above (the common case: a
        // plugin's `editor.showActionPopup` in response to
        // `lsp_status_clicked`) or already showing when the user
        // clicked the indicator — don't stack the built-in LSP
        // Servers popup on top. The hook's popup is the more
        // contextual answer to the click; layering two popups for
        // one gesture is the user-reported "I had several kinds of
        // popups" bug.
        if self.active_state().popups.top().is_some() {
            return;
        }

        self.build_and_show_lsp_status_popup(&language, true);
    }

    /// Rebuild the LSP-status popup in place if it's currently open.
    ///
    /// Used when an async event (progress update, server state change) might
    /// change the popup's contents — notably while rust-analyzer is indexing
    /// and emits `$/progress` every few hundred ms.  Without this, the popup
    /// would freeze on the snapshot taken at open time while the status-bar
    /// spinner keeps moving, making them look disconnected.
    pub fn refresh_lsp_status_popup_if_open(&mut self) {
        // Only rebuild if the active buffer's top popup IS an LSP
        // status popup — otherwise we'd spuriously build one on top of
        // unrelated state.
        if !self
            .active_state()
            .popups
            .top()
            .is_some_and(is_lsp_status_popup)
        {
            return;
        }
        let language = self
            .buffers()
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());
        // Replace contents: hide then rebuild. Refresh is triggered by
        // async progress updates while the popup is already on screen,
        // so we keep its existing focused state — flipping it back to
        // unfocused on every progress tick would yank focus away from
        // a user mid-interaction.
        let was_focused = self
            .active_state()
            .popups
            .top()
            .map(|p| p.focused)
            .unwrap_or(true);
        self.hide_popup();
        self.build_and_show_lsp_status_popup(&language, was_focused);
    }

    fn build_and_show_lsp_status_popup(&mut self, language: &str, focused: bool) {
        use crate::services::async_bridge::LspServerStatus;

        // Build a unified list of all configured servers for this language,
        // merged with their runtime status (if running).
        let running_statuses: std::collections::HashMap<String, LspServerStatus> = self
            .active_window()
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
                        let missing = match self.lsp() {
                            Some(mgr) => !mgr.command_exists_via_authority(&c.command),
                            None => !crate::services::lsp::command_exists(&c.command),
                        };
                        (c.display_name(), missing)
                    })
                    .collect()
            })
            .unwrap_or_default();
        // Per-server auto_start flag map (display_name → auto_start).
        // Used to decide whether to offer an "Enable auto-start for X"
        // row alongside the "Start X" action — relevant only when the
        // server is enabled but dormant and the user hasn't opted into
        // auto-start yet.
        let auto_start_by_server: std::collections::HashMap<String, bool> = self
            .config
            .lsp
            .get(language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| !c.command.is_empty())
                    .map(|c| (c.display_name(), c.auto_start))
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self
            .active_window()
            .is_lsp_language_user_dismissed(language);

        if configured_servers.is_empty() && running_statuses.is_empty() {
            self.active_window_mut().status_message = Some(t!("lsp.no_server_active").to_string());
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
            // of the pre-click probe. The `binary_missing` signal comes
            // from the authority-routed `command_exists` (L-3c), so the
            // "not installed" copy says where it actually isn't: in the
            // container for container authorities, on the host
            // otherwise.
            let authority_is_container = self.authority().display_label.starts_with("Container:");
            let missing_label = if authority_is_container {
                "not installed in container"
            } else {
                "binary not in PATH"
            };
            let (icon, label) = match status {
                Some(LspServerStatus::Running) => ("●", "ready"),
                Some(LspServerStatus::Error) => ("✗", "error"),
                Some(LspServerStatus::Starting) => ("◌", "starting"),
                Some(LspServerStatus::Initializing) => ("◌", "initializing"),
                Some(LspServerStatus::Shutdown) | None => {
                    if binary_missing {
                        ("○", missing_label)
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
                .active_window()
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
                // noise up the status area. Copy shifts with the
                // authority so the user is pointed at the right
                // install surface: `devcontainer.json`'s
                // `postCreateCommand` for containers, the host's
                // package manager otherwise.
                let advisory = if authority_is_container {
                    format!("    Install {name} in container (postCreateCommand)")
                } else {
                    format!("    Install {name} to enable")
                };
                items.push(crate::view::popup::PopupListItem::new(advisory).disabled());
            } else {
                // Two sibling rows for a dormant server, in the
                // order the user most likely wants:
                //
                //   "Start <name> (always)" — persist auto_start=true
                //                              AND start the server now.
                //                              Listed first because
                //                              persistent-start is the
                //                              common case, so pre-
                //                              selecting it lets the
                //                              user press Enter and
                //                              move on.
                //   "Start <name> once"     — start for this session,
                //                              config stays auto_start=false.
                //
                // The "once" suffix is only needed (vs. just "Start")
                // when the "(always)" sibling is also present — i.e.
                // when auto_start is currently false. Otherwise there
                // is nothing to disambiguate it from.
                let is_manual = !auto_start_by_server.get(name).copied().unwrap_or(true);

                // "(always)" row — first, so it's the default.
                if is_manual {
                    let autostart_key = format!("autostart:{}/{}", language, name);
                    items.push(
                        crate::view::popup::PopupListItem::new(format!(
                            "    Start {} (always)",
                            name
                        ))
                        .with_data(autostart_key.clone()),
                    );
                    action_keys.push((autostart_key, format!("Start {} (always)", name)));
                }

                // "once" / plain Start row.
                let start_label = if is_manual {
                    format!("    Start {} once", name)
                } else {
                    format!("    Start {}", name)
                };
                let start_action_label = if is_manual {
                    format!("Start {} once", name)
                } else {
                    format!("Start {}", name)
                };
                let start_key = format!("start:{}", language);
                if !action_keys.iter().any(|(k, _)| k == &start_key) {
                    items.push(
                        crate::view::popup::PopupListItem::new(start_label)
                            .with_data(start_key.clone()),
                    );
                    action_keys.push((start_key, start_action_label));
                }
            }
        }

        // Disable / Enable row — shown whenever the language has at
        // least one configured server. The label flips on either the
        // session-level dismiss flag OR the persisted `enabled = false`
        // half: both mean "the language is currently muted from the
        // user's POV", and showing "Disable" while the config already
        // has every server disabled would leave the user with no
        // surface to undo it. Picking the row writes through to the
        // matching half of the state in `handle_lsp_status_action`
        // (`dismiss:` flips both, `enable:` flips both) so the two
        // signals stay in sync after every round-trip.
        let any_enabled = self
            .config
            .lsp
            .get(language)
            .is_some_and(|cfg| cfg.as_slice().iter().any(|c| c.enabled));
        let muted = user_dismissed || !any_enabled;
        if muted {
            let enable_key = format!("enable:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!("    Enable LSP for {}", language))
                    .with_data(enable_key.clone()),
            );
            action_keys.push((enable_key, format!("Enable LSP for {}", language)));
        } else {
            let dismiss_key = format!("dismiss:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!("    Disable LSP for {}", language))
                    .with_data(dismiss_key.clone()),
            );
            action_keys.push((dismiss_key, format!("Disable LSP for {}", language)));
        }

        // View log action — grayed out and non-actionable when no
        // log file exists yet for this language (e.g. the server was
        // never started, or has been rotated away).
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

        // Plugin-contributed rows — injected between View Log and
        // Dismiss as an extra "Plugin actions" section. This is the
        // merge half of "Option B" (#1941 follow-up): instead of
        // plugins pushing their own separate popup via
        // `editor.showActionPopup` (which stacked on top of this one
        // and confused the user), they install rows here via
        // `PluginCommand::SetLspMenuContributions` and the editor
        // routes the eventual selection back via
        // `action_popup_result` with `popup_id = "lsp_status"` and
        // `action_id = "{plugin_id}|{item_id}"`.
        //
        // Sorted by (plugin_id, item index) for stable ordering so
        // the popup doesn't shuffle rows between renders. A single
        // header row labels the section when there's at least one
        // contributed item, so the user can tell the rows below come
        // from a plugin (vs. built-in actions like Stop/Restart).
        let mut contributed: Vec<(&String, &Vec<crate::app::LspMenuItem>)> = self
            .active_window()
            .lsp_menu_contributions
            .iter()
            .filter_map(|((lang, plugin_id), items)| {
                if lang == language && !items.is_empty() {
                    Some((plugin_id, items))
                } else {
                    None
                }
            })
            .collect();
        contributed.sort_by(|a, b| a.0.cmp(b.0));
        if !contributed.is_empty() {
            // Section header — non-actionable, mimics the language
            // header at the top (no data → not clickable).
            items.push(crate::view::popup::PopupListItem::new(
                "  ─ Plugin actions ─".to_string(),
            ));
            for (plugin_id, plugin_items) in contributed {
                for it in plugin_items {
                    let key = format!("plugin:{}|{}", plugin_id, it.id);
                    items.push(
                        crate::view::popup::PopupListItem::new(format!("    {}", it.label))
                            .with_data(key.clone()),
                    );
                    action_keys.push((key, it.label.clone()));
                }
            }
        }

        // Trailing Dismiss row — gives users an on-screen way out of
        // the popup without having to know that Esc works. The key
        // label is looked up from the keybinding resolver so a
        // rebound PopupCancel stays visible in the row label
        // ("Dismiss (Q)", etc.). Falls back to "Esc" as the usual
        // default if the resolver has no binding at all (unusual,
        // but we don't want an empty parenthetical).
        let cancel_binding = self
            .keybindings
            .read()
            .ok()
            .and_then(|kb| {
                kb.get_keybinding_for_action(
                    &crate::input::keybindings::Action::PopupCancel,
                    crate::input::keybindings::KeyContext::Popup,
                )
            })
            .unwrap_or_else(|| "Esc".to_string());
        let cancel_key = "cancel_popup".to_string();
        items.push(
            crate::view::popup::PopupListItem::new(format!("    Dismiss ({})", cancel_binding))
                .with_data(cancel_key.clone()),
        );
        action_keys.push((cancel_key, format!("Dismiss ({})", cancel_binding)));
        // `action_keys` is no longer kept on the editor — each list
        // item already carries its action key in its `data` field, and
        // the `LspStatus` resolver on the popup tells confirm how to
        // interpret that data. The local binding is retained only to
        // keep the existing construction logic unchanged; it falls out
        // of scope with the rest.
        let _ = action_keys;

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
        // segment isn't visible (e.g. first render). `status_row` comes
        // from the same cached layout so the popup hugs the status bar
        // even in prompt-auto-hide mode.
        let position = self
            .active_chrome()
            .status_bar_lsp_area
            .map(
                |(status_row, col_start, _)| crate::view::popup::PopupPosition::AboveStatusBarAt {
                    x: col_start,
                    status_row,
                },
            )
            .unwrap_or(crate::view::popup::PopupPosition::BottomRight);

        use crate::view::popup::{Popup, PopupContent, PopupKind, PopupResolver};
        use ratatui::style::Style;

        let focus_hint = if !focused {
            self.popup_focus_key_hint()
        } else {
            None
        };
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
            border_style: Style::default().fg(self.theme.read().unwrap().popup_border_fg),
            background_style: Style::default().bg(self.theme.read().unwrap().popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            // This is the LSP status popup — mark it so confirm/cancel
            // routes through handle_lsp_status_action regardless of what
            // other popups are on screen.
            resolver: PopupResolver::LspStatus,
            focused,
            focus_key_hint: focus_hint,
        };

        let buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.popups.show(popup);
        }
    }

    /// Show the Remote Indicator context menu popup.
    ///
    /// The menu is context-aware based on the current authority state:
    /// - **Local:** offers "Attach to Dev Container" (when a devcontainer
    ///   config is detectable) and "Open Dev Container Config".
    /// - **Connected (container):** offers "Reopen Locally" (detach),
    ///   "Rebuild Container", and "Show Container Info".
    /// - **Connected (SSH):** offers "Disconnect Remote" and "Show Info".
    /// - **Disconnected:** offers "Reconnect" (best-effort) and "Go Local".
    ///
    /// Clicking the `{remote}` status-bar element a second time toggles
    /// the popup closed, matching the LSP-indicator affordance.
    ///
    /// # Design note
    ///
    /// Plugin-owned actions (attach, rebuild) are dispatched via
    /// `Action::PluginAction` so core code never names the devcontainer
    /// plugin directly. If the plugin isn't loaded the action becomes a
    /// no-op with a status message, which is the same fallback every
    /// other plugin-command invocation site uses.
    pub fn show_remote_indicator_popup(&mut self) {
        use crate::view::popup::{Popup, PopupContent, PopupKind, PopupListItem, PopupResolver};
        use ratatui::style::Style;

        if self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| matches!(p.resolver, PopupResolver::RemoteIndicator))
        {
            self.hide_popup();
            return;
        }

        let connection = self.connection_display_string();
        let is_disconnected = connection
            .as_deref()
            .is_some_and(|c| c.contains("(Disconnected)"));
        let is_container = connection
            .as_deref()
            .is_some_and(|c| c.starts_with("Container:"));
        let is_ssh = connection.is_some() && !is_container;

        let devcontainer_config_path = self.find_devcontainer_config();

        let mut items: Vec<PopupListItem> = Vec::new();
        let mut title: String = String::new();

        // Plugin-supplied override (Connecting / FailedAttach) takes
        // precedence over the authority-derived branches. A Connecting
        // indicator shouldn't render the "Reopen in Container" menu
        // of the underlying derived state — an attach is in flight;
        // the user needs Show Logs / Cancel / (after B-3b) Retry.
        //
        // Local / Connected / Disconnected overrides are treated as
        // labelling shortcuts, not menu-shape changes — they fall
        // through to the derived branches below.
        use crate::view::ui::status_bar::RemoteIndicatorOverride;
        let override_handled = matches!(
            self.remote_indicator_override,
            Some(RemoteIndicatorOverride::Connecting { .. })
                | Some(RemoteIndicatorOverride::FailedAttach { .. })
        );
        if let Some(over) = self.remote_indicator_override.clone() {
            match over {
                RemoteIndicatorOverride::Connecting { label } => {
                    let suffix = label
                        .filter(|s| !s.is_empty())
                        .map(|s| format!(" — {}", s))
                        .unwrap_or_default();
                    title = format!("Remote: Connecting{}", suffix);
                    items.push(
                        PopupListItem::new("    Cancel Startup".to_string())
                            .with_data("plugin:devcontainer_cancel_attach".to_string()),
                    );
                    items.push(
                        PopupListItem::new("    Show Logs".to_string())
                            .with_data("plugin:devcontainer_show_build_logs".to_string()),
                    );
                }
                RemoteIndicatorOverride::FailedAttach { error } => {
                    let suffix = error
                        .filter(|s| !s.is_empty())
                        .map(|s| format!(" — {}", s))
                        .unwrap_or_default();
                    title = format!("Remote: Attach failed{}", suffix);
                    items.push(
                        PopupListItem::new("    Retry".to_string())
                            .with_data("plugin:devcontainer_retry_attach".to_string()),
                    );
                    items.push(
                        PopupListItem::new("    Reopen Locally".to_string())
                            .with_data("clear_override".to_string()),
                    );
                    items.push(
                        PopupListItem::new("    Show Build Logs".to_string())
                            .with_data("plugin:devcontainer_show_build_logs".to_string()),
                    );
                }
                _ => {
                    // Fall through to the derived branches.
                }
            }
        }

        if !override_handled {
            match (connection.as_deref(), is_disconnected) {
                // Connected authority (container or SSH), not disconnected.
                (Some(label), false) => {
                    title = format!("Remote: {}", label);
                    if is_container {
                        items.push(
                            PopupListItem::new("    Reopen Locally".to_string())
                                .with_data("detach".to_string()),
                        );
                        items.push(
                            PopupListItem::new("    Rebuild Container".to_string())
                                .with_data("plugin:devcontainer_rebuild".to_string()),
                        );
                        items.push(
                            PopupListItem::new("    Show Container Logs".to_string())
                                .with_data("plugin:devcontainer_show_logs".to_string()),
                        );
                        items.push(
                            PopupListItem::new("    Show Container Info".to_string())
                                .with_data("plugin:devcontainer_show_info".to_string()),
                        );
                        // The build log file from the most recent
                        // `devcontainer up` survives the post-attach
                        // restart (path stashed in plugin global state,
                        // file lives under the workspace's
                        // `.fresh-cache/`). Surfacing it here means
                        // users can revisit "what did the build
                        // actually do" any time after attach without
                        // hunting through the file tree.
                        items.push(
                            PopupListItem::new("    Show Build Logs".to_string())
                                .with_data("plugin:devcontainer_show_build_logs".to_string()),
                        );
                    } else if is_ssh {
                        items.push(
                            PopupListItem::new("    Disconnect Remote".to_string())
                                .with_data("detach".to_string()),
                        );
                    }
                }
                // Disconnected — warn and offer fallbacks.
                (Some(_), true) => {
                    title = "Remote: Disconnected".to_string();
                    items.push(
                        PopupListItem::new("    Go Local".to_string())
                            .with_data("detach".to_string()),
                    );
                }
                // Local authority.
                (None, _) => {
                    title = "Remote: Local".to_string();
                    if devcontainer_config_path.is_some() {
                        items.push(
                            PopupListItem::new("    Reopen in Container".to_string())
                                .with_data("plugin:devcontainer_attach".to_string()),
                        );
                        items.push(
                            PopupListItem::new("    Open Dev Container Config".to_string())
                                .with_data("plugin:devcontainer_open_config".to_string()),
                        );
                    } else {
                        // No .devcontainer present — offer the scaffold
                        // so users can bootstrap a config in one click
                        // without dropping to a shell. The scaffold
                        // command is plugin-owned and registered
                        // unconditionally at plugin load, so this row is
                        // always actionable.
                        items.push(
                            PopupListItem::new("    Create Dev Container Config".to_string())
                                .with_data("plugin:devcontainer_scaffold_config".to_string()),
                        );
                    }
                }
            }
        } // end: if !override_handled

        // Dismiss row — mirrors the LSP popup's terminal Dismiss row so
        // users have an on-screen way out of the popup.
        let cancel_binding = self
            .keybindings
            .read()
            .ok()
            .and_then(|kb| {
                kb.get_keybinding_for_action(
                    &crate::input::keybindings::Action::PopupCancel,
                    crate::input::keybindings::KeyContext::Popup,
                )
            })
            .unwrap_or_else(|| "Esc".to_string());
        items.push(
            PopupListItem::new(format!("    Dismiss ({})", cancel_binding))
                .with_data("cancel_popup".to_string()),
        );

        let first_actionable = items
            .iter()
            .position(|i| i.data.is_some() && !i.disabled)
            .unwrap_or(0);

        // Anchor the popup to the remote-indicator's left edge if it's
        // visible in the last frame; otherwise fall back to the bottom-
        // right corner so the popup still appears. `status_row` comes
        // from the same cached layout so the popup hugs the status bar
        // even in prompt-auto-hide mode.
        let position = self
            .active_chrome()
            .status_bar_remote_area
            .map(
                |(status_row, col_start, _)| crate::view::popup::PopupPosition::AboveStatusBarAt {
                    x: col_start,
                    status_row,
                },
            )
            .unwrap_or(crate::view::popup::PopupPosition::BottomRight);

        let popup_width = (items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(24)
            + 4) as u16;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some(title),
            description: None,
            transient: false,
            content: PopupContent::List {
                items,
                selected: first_actionable,
            },
            position,
            width: popup_width.clamp(28, 50),
            max_height: 10,
            bordered: true,
            border_style: Style::default().fg(self.theme.read().unwrap().popup_border_fg),
            background_style: Style::default().bg(self.theme.read().unwrap().popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::RemoteIndicator,
            // Explicitly invoked from the status-bar `{remote}` element,
            // so this popup wants the keyboard immediately.
            focused: true,
            focus_key_hint: None,
        };

        let buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.popups.show(popup);
        }
    }

    /// Dispatch the action selected from the Remote Indicator popup.
    ///
    /// - `"detach"` — `clear_authority()` (falls back to local).
    /// - `"clear_override"` — drop the Remote Indicator override
    ///   without changing the authority. Used by the FailedAttach
    ///   "Reopen Locally" row: nothing to detach (no authority was
    ///   ever installed), but the FailedAttach indicator should
    ///   clear.
    /// - `"plugin:<name>"` — forwards to `Action::PluginAction(name)`.
    /// - `"cancel_popup"` — no-op; the popup framework already
    ///   closed the popup when the row was confirmed.
    /// - anything else — logged and ignored.
    pub fn handle_remote_indicator_action(&mut self, action_key: &str) {
        if action_key == "detach" {
            self.remote_indicator_override = None;
            self.clear_authority();
            return;
        }
        if action_key == "clear_override" {
            self.remote_indicator_override = None;
            return;
        }
        if action_key == "cancel_popup" {
            return;
        }
        if let Some(plugin_action) = action_key.strip_prefix("plugin:") {
            // `handle_action` wires this through the plugin manager; if
            // the plugin isn't loaded it surfaces a status message, which
            // is the correct no-op behavior for every plugin-command
            // invocation site in the codebase. We still want to log an
            // unexpected dispatch error — plugin misbehavior shouldn't
            // leave the user staring at a silently-failed Retry click.
            if let Err(e) = self.handle_action(crate::input::keybindings::Action::PluginAction(
                plugin_action.to_string(),
            )) {
                tracing::warn!(
                    "remote indicator popup: dispatching '{}' failed: {}",
                    plugin_action,
                    e
                );
            }
            return;
        }
        tracing::warn!(
            "handle_remote_indicator_action: unknown action key '{}'",
            action_key
        );
    }

    /// Show the trust prompt if this workspace is undecided and contains
    /// content whose execution trust matters (env files, project manifests,
    /// `.sln`/`.csproj`, …). No-op once a decision is recorded or when there's
    /// nothing to gate. Called from every editor-startup path (in-process run
    /// and the session server) so the prompt fires regardless of launch mode.
    pub fn maybe_prompt_workspace_trust(&mut self) {
        let store = crate::services::workspace_trust::TrustStore::for_project_dir(
            &self.dir_context.project_state_dir(&self.working_dir),
        );
        if store.is_decided() {
            return; // already decided for this project
        }
        if !crate::services::workspace_trust::workspace_has_executable_content(&self.working_dir) {
            return; // nothing whose trust matters here
        }
        self.show_workspace_trust_popup(false);
    }

    /// Show the workspace-trust prompt: a centered list asking how this
    /// project's tooling should be treated. Surfaced on opening an
    /// untrusted project that contains executable content (env files,
    /// `.csproj`/`.sln`, …). The default-focused choice is the safe
    /// "Restricted" — dismissing with Escape leaves the project undecided
    /// (and re-asks next open), while selecting any row records the
    /// decision so the prompt stops appearing.
    pub fn show_workspace_trust_popup(&mut self, cancellable: bool) {
        use crate::view::popup::{Popup, PopupContent, PopupKind, PopupResolver};
        use ratatui::style::Style;

        self.workspace_trust_prompt_cancellable = cancellable;
        self.workspace_trust_markers =
            crate::services::workspace_trust::executable_content_markers(&self.working_dir);

        // Don't stack a second copy if one is already up. The prompt lives on
        // the editor-level (global) stack so it renders regardless of which
        // buffer is active — opening a directory makes the file-explorer /
        // dashboard the active buffer, which would orphan a buffer-scoped
        // popup and leave it unrendered.
        if self
            .global_popups
            .top()
            .is_some_and(|p| matches!(p.resolver, PopupResolver::WorkspaceTrust))
        {
            return;
        }

        // Seed the radio selection from the project's current level so a
        // command-palette invocation shows the active choice; at startup
        // (undecided) this is the safe Restricted default.
        let selected = match self.authority.workspace_trust.level() {
            crate::services::workspace_trust::TrustLevel::Trusted => 0,
            crate::services::workspace_trust::TrustLevel::Restricted => 1,
            crate::services::workspace_trust::TrustLevel::Blocked => 2,
        };

        let items = vec![
            crate::view::popup::PopupListItem::new("Trust this folder".to_string())
                .with_detail("Allow project tooling (LSP, env managers, tasks) to run".to_string())
                .with_data("trusted".to_string()),
            crate::view::popup::PopupListItem::new("Keep restricted (default)".to_string())
                .with_detail("Don't run repo-controlled code; system tools still run".to_string())
                .with_data("restricted".to_string()),
            crate::view::popup::PopupListItem::new("Block all execution".to_string())
                .with_detail("No processes run at all in this workspace".to_string())
                .with_data("blocked".to_string()),
        ];

        let popup_width = (items
            .iter()
            .map(|i| {
                let detail_w = i
                    .detail
                    .as_deref()
                    .map(unicode_width::UnicodeWidthStr::width)
                    .unwrap_or(0);
                unicode_width::UnicodeWidthStr::width(i.text.as_str()).max(detail_w)
            })
            .max()
            .unwrap_or(40)
            + 4) as u16;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some("This project can run code on your machine. Trust it?".to_string()),
            description: None,
            transient: false,
            content: PopupContent::List { items, selected },
            position: crate::view::popup::PopupPosition::Centered,
            width: popup_width.clamp(40, 70),
            max_height: 10,
            bordered: true,
            border_style: Style::default().fg(self.theme.read().unwrap().popup_border_fg),
            background_style: Style::default().bg(self.theme.read().unwrap().popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::WorkspaceTrust,
            focused: true,
            focus_key_hint: None,
        };

        self.global_popups.show(popup);
    }

    /// Dispatch the choice selected from the workspace-trust prompt.
    /// `"trusted"` / `"restricted"` / `"blocked"` set the level (persisted,
    /// and the editor restarts so the new policy applies to already-running
    /// tooling). Anything else is logged and ignored.
    pub fn handle_workspace_trust_action(&mut self, action_key: &str) {
        use crate::services::workspace_trust::TrustLevel;
        let level = match action_key {
            "trusted" => TrustLevel::Trusted,
            "restricted" => TrustLevel::Restricted,
            "blocked" => TrustLevel::Blocked,
            other => {
                tracing::warn!("handle_workspace_trust_action: unknown action key '{other}'");
                return;
            }
        };
        self.set_workspace_trust_level(level);
    }

    /// Keyboard handling for the workspace-trust modal. Returns `Some(Consumed)`
    /// for every key (the modal swallows everything): arrows and the mnemonics
    /// `T`/`K`/`B` move the radio selection (two-step — they don't confirm),
    /// `Enter`/`O` confirm the current selection, the user's global quit key
    /// quits the editor, and `Esc` is inert.
    pub(crate) fn handle_workspace_trust_key(
        &mut self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::handler::InputResult> {
        use crate::input::handler::InputResult;
        use crate::input::keybindings::{Action, KeyContext};
        use crossterm::event::KeyCode;

        let cancellable = self.workspace_trust_prompt_cancellable;

        // The mandatory open-time gate (not cancellable) binds its secondary
        // action to the user's global quit key (default Ctrl+Q) and quits the
        // editor. A voluntarily-opened prompt (cancellable) does not — Escape
        // cancels it instead.
        if !cancellable {
            let resolved = self
                .keybindings
                .read()
                .ok()
                .map(|kb| kb.resolve(event, KeyContext::Normal));
            if matches!(resolved, Some(Action::Quit) | Some(Action::ForceQuit)) {
                self.hide_popup();
                self.should_quit = true;
                return Some(InputResult::Consumed);
            }
        }

        match event.code {
            KeyCode::Up => self.move_workspace_trust_selection(-1),
            KeyCode::Down => self.move_workspace_trust_selection(1),
            KeyCode::Char('t') | KeyCode::Char('T') => self.set_workspace_trust_selection(0),
            KeyCode::Char('k') | KeyCode::Char('K') => self.set_workspace_trust_selection(1),
            KeyCode::Char('b') | KeyCode::Char('B') => self.set_workspace_trust_selection(2),
            KeyCode::Enter | KeyCode::Char('o') | KeyCode::Char('O') => {
                self.confirm_workspace_trust(self.current_workspace_trust_selection());
            }
            // Escape cancels a voluntarily-opened prompt; on the mandatory gate
            // it (and every other key) is inert but still consumed (modal).
            KeyCode::Esc if cancellable => self.hide_popup(),
            _ => {}
        }
        Some(InputResult::Consumed)
    }

    /// Set the radio selection to an absolute index (0=Trust, 1=Restricted,
    /// 2=Block) without confirming.
    fn set_workspace_trust_selection(&mut self, index: usize) {
        if let Some(popup) = self.global_popups.top_mut() {
            if let crate::view::popup::PopupContent::List { selected, .. } = &mut popup.content {
                *selected = index.min(2);
            }
        }
    }

    /// The currently-highlighted radio index (0=Trust, 1=Restricted, 2=Block).
    pub(crate) fn current_workspace_trust_selection(&self) -> usize {
        self.global_popups
            .top()
            .and_then(|p| match &p.content {
                crate::view::popup::PopupContent::List { selected, .. } => Some(*selected),
                _ => None,
            })
            .unwrap_or(1)
    }

    /// Move the radio selection by `delta`, wrapping across the three options.
    fn move_workspace_trust_selection(&mut self, delta: i32) {
        if let Some(popup) = self.global_popups.top_mut() {
            if let crate::view::popup::PopupContent::List { selected, .. } = &mut popup.content {
                *selected = (((*selected as i32) + delta).rem_euclid(3)) as usize;
            }
        }
    }

    /// Record the trust decision for radio `index` and dismiss the modal.
    pub(crate) fn confirm_workspace_trust(&mut self, index: usize) {
        let key = match index {
            0 => "trusted",
            2 => "blocked",
            _ => "restricted",
        };
        self.hide_popup();
        self.handle_workspace_trust_action(key);
    }

    /// Probe for a `devcontainer.json` under the current working
    /// directory. Mirrors the first two priorities of the devcontainer
    /// plugin's `findConfig()` so the Remote Indicator menu can decide
    /// whether to offer "Reopen in Container" without actually having to
    /// call into the plugin.
    ///
    /// Routes through `authority.filesystem` per `CONTRIBUTING.md`
    /// guideline 4, so an SSH-rooted workspace probes the remote host
    /// rather than the local one.
    fn find_devcontainer_config(&self) -> Option<std::path::PathBuf> {
        let cwd = self.working_dir();
        let fs = self.authority.filesystem.as_ref();
        let primary = cwd.join(".devcontainer").join("devcontainer.json");
        if fs.exists(&primary) {
            return Some(primary);
        }
        let secondary = cwd.join(".devcontainer.json");
        if fs.exists(&secondary) {
            return Some(secondary);
        }
        None
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

        let mut popup = Popup::markdown(
            &md,
            &*self.theme.read().unwrap(),
            Some(&self.grammar_registry),
        );
        popup.transient = false;
        popup.position = PopupPosition::BelowCursor;
        popup.width = popup_width;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.read().unwrap().popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.read().unwrap().popup_bg);

        let buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.popups.show(popup);
        }
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::primitives::text_property::TextProperty>> {
        let state = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&self.active_buffer())?;
        let cursor_pos = self.active_cursors().primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }
}
