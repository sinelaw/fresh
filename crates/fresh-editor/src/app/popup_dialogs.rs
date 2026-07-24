//! Popup-dialog orchestrators on `Editor`.
//!
//! These build and show various popups as buffer-level events:
//! warnings popup, LSP status popup (with refresh hook), file-message
//! popup, and a small text-properties query helper. The LSP status popup
//! is the largest; it is split into `collect_lsp_status_servers` (gather
//! state), `push_lsp_server_rows` / `push_lsp_footer_rows` (build the list),
//! and `present_lsp_status_popup` (pin width + show), orchestrated by
//! `build_and_show_lsp_status_popup`.

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

/// Hard-wrap `text` to `width` display columns, breaking words that are longer
/// than `width` (e.g. a long, space-less config-file path) so they never
/// overflow a popup's border. Whitespace-separated where possible.
fn hard_wrap(text: &str, width: usize) -> Vec<String> {
    use unicode_width::UnicodeWidthChar;

    if width == 0 {
        return vec![text.to_string()];
    }
    let ch_width = |c: char| UnicodeWidthChar::width(c).unwrap_or(1);

    let mut lines = Vec::new();
    let mut cur = String::new();
    let mut cur_w = 0usize;

    let push_word = |word: &str, lines: &mut Vec<String>, cur: &mut String, cur_w: &mut usize| {
        for c in word.chars() {
            let w = ch_width(c);
            if *cur_w + w > width && !cur.is_empty() {
                lines.push(std::mem::take(cur));
                *cur_w = 0;
            }
            cur.push(c);
            *cur_w += w;
        }
    };

    for word in text.split(' ') {
        let word_w: usize = word.chars().map(ch_width).sum();
        if cur.is_empty() {
            push_word(word, &mut lines, &mut cur, &mut cur_w);
        } else if cur_w + 1 + word_w <= width {
            cur.push(' ');
            cur_w += 1;
            cur.push_str(word);
            cur_w += word_w;
        } else {
            lines.push(std::mem::take(&mut cur));
            cur_w = 0;
            push_word(word, &mut lines, &mut cur, &mut cur_w);
        }
    }
    if !cur.is_empty() {
        lines.push(cur);
    }
    if lines.is_empty() {
        lines.push(String::new());
    }
    lines
}

/// Max display cells for each variable field (title / message) of the LSP
/// progress line. Used to pin the popup width so it doesn't jitter as live
/// progress messages come and go.
const LSP_PROGRESS_FIELD_MAX: usize = 14;
/// Hard cap on the LSP-status popup width.
const LSP_POPUP_WIDTH_MAX: u16 = 50;
/// Worst-case width of the runtime-varying progress line, used when pinning
/// the popup width:
///   "    ⏳ " (4-space indent + ⏳ (2 cells) + space = 7 cells)
///   + field (title) + " · " (3) + field (message) + " (100%)" (7)
const LSP_PROGRESS_LINE_MAX: usize = 7 + LSP_PROGRESS_FIELD_MAX + 3 + LSP_PROGRESS_FIELD_MAX + 7;

/// Truncate `s` to at most `max_cells` display cells, appending an ellipsis
/// if truncation happened (the ellipsis is included in the budget, so the
/// result is ≤ `max_cells` wide regardless of input).
fn truncate_to_cells(s: &str, max_cells: usize) -> String {
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

/// A language's configured + running LSP servers, gathered once up-front so
/// row-building doesn't have to re-query `self` for every server. Built by
/// [`Editor::collect_lsp_status_servers`].
struct LspStatusServers {
    /// All server display-names for the language (configured ∪ running), sorted.
    names: Vec<String>,
    /// display-name → live runtime status, for servers that are running.
    running: std::collections::HashMap<String, crate::services::async_bridge::LspServerStatus>,
    /// display-name → binary-missing. Only meaningful when not running.
    missing: std::collections::HashMap<String, bool>,
    /// display-name → configured `auto_start`.
    auto_start: std::collections::HashMap<String, bool>,
    /// The user dismissed this language for the session.
    user_dismissed: bool,
    /// At least one configured server has `enabled = true`.
    any_enabled: bool,
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

    /// Build and show the LSP status popup for `language`. Orchestrates three
    /// cohesive steps: gather the configured/running servers, build the list
    /// rows, then pin + present the popup.
    fn build_and_show_lsp_status_popup(&mut self, language: &str, focused: bool) {
        let servers = self.collect_lsp_status_servers(language);
        if servers.names.is_empty() {
            self.active_window_mut().status_message = Some(t!("lsp.no_server_active").to_string());
            return;
        }

        // Build the popup's items as view-level `PopupListItem`s directly. We
        // bypass the `PopupListItemData` event type because we need the
        // `disabled` field (a view-only concern). Each item carries its own
        // action key in `data`, and the `LspStatus` resolver tells confirm how
        // to interpret it, so no separate action-key table is needed.
        let mut items: Vec<crate::view::popup::PopupListItem> = Vec::new();
        self.push_lsp_server_rows(language, &servers, &mut items);
        self.push_lsp_footer_rows(language, &servers, &mut items);
        self.present_lsp_status_popup(language, items, focused);
    }

    /// Gather the configured + running LSP servers for `language`, merged with
    /// their runtime status, binary availability, and auto-start config.
    fn collect_lsp_status_servers(&self, language: &str) -> LspStatusServers {
        use crate::services::async_bridge::LspServerStatus;

        let running: std::collections::HashMap<String, LspServerStatus> = self
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

        // Per-server binary availability map (display_name → missing).
        // `command_exists` is cached, so repeated popup opens or a
        // refresh-while-open are cheap. We look up by display name because
        // `names` below is built from display names; `display_name()` falls
        // back to the command basename when no explicit `name` is set.
        let missing: std::collections::HashMap<String, bool> = self
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

        // Per-server auto_start flag map — used to decide whether to offer an
        // "Start X (always)" row alongside the plain "Start X".
        let auto_start: std::collections::HashMap<String, bool> = self
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

        let any_enabled = self
            .config
            .lsp
            .get(language)
            .is_some_and(|cfg| cfg.as_slice().iter().any(|c| c.enabled));

        // Merge: start with configured servers, then add any running servers
        // not in the config (shouldn't happen, but be safe).
        let mut names = configured_servers;
        for name in running.keys() {
            if !names.contains(name) {
                names.push(name.clone());
            }
        }
        names.sort();

        LspStatusServers {
            names,
            running,
            missing,
            auto_start,
            user_dismissed,
            any_enabled,
        }
    }

    /// Push one block of rows per server — a status header, an optional live
    /// progress line, and the per-server action rows (restart/stop, install
    /// advisory, or start) — into `items`.
    fn push_lsp_server_rows(
        &self,
        language: &str,
        servers: &LspStatusServers,
        items: &mut Vec<crate::view::popup::PopupListItem>,
    ) {
        use crate::services::async_bridge::LspServerStatus;

        // The "not installed" copy says where it actually isn't: in the
        // container for container authorities, on the host otherwise.
        let authority_is_container = self.authority().display_label.starts_with("Container:");
        let missing_label = if authority_is_container {
            "not installed in container"
        } else {
            "binary not in PATH"
        };

        for name in &servers.names {
            let status = servers.running.get(name).copied();
            let is_active = status
                .map(|s| !matches!(s, LspServerStatus::Shutdown))
                .unwrap_or(false);
            // A server is "missing" only when it's NOT currently running (an
            // absolute-path binary could have been removed mid-session, but
            // the live server is still talking to us).
            let binary_missing = !is_active && servers.missing.get(name).copied().unwrap_or(false);

            // Header: server name + status (no data → not clickable). Swap the
            // "not running" label for an actionable "binary not found" when a
            // start attempt would clearly fail.
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
                "{icon} {name} ({label})"
            )));

            // Progress row immediately UNDER the server's name row, if there's
            // an active `$/progress` notification for this language. Fields are
            // individually truncated so a runaway progress path can't stretch
            // the popup (the width is pinned in advance).
            if let Some(info) = self
                .active_window()
                .lsp_progress
                .values()
                .find(|info| info.language == language)
            {
                let mut line = format!(
                    "    ⏳ {}",
                    truncate_to_cells(&info.title, LSP_PROGRESS_FIELD_MAX)
                );
                if let Some(ref msg) = info.message {
                    line.push_str(&format!(
                        " · {}",
                        truncate_to_cells(msg, LSP_PROGRESS_FIELD_MAX)
                    ));
                }
                if let Some(pct) = info.percentage {
                    line.push_str(&format!(" ({pct}%)"));
                }
                items.push(crate::view::popup::PopupListItem::new(line));
            }

            if is_active {
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Restart {name}"))
                        .with_data(format!("restart:{language}/{name}")),
                );
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Stop {name}"))
                        .with_data(format!("stop:{language}/{name}")),
                );
            } else if binary_missing {
                // A disabled advisory row instead of an actionable "Start" —
                // clicking Start here would spawn, fail, and noise up the
                // status area. Copy shifts with the authority so the user is
                // pointed at the right install surface.
                let advisory = if authority_is_container {
                    format!("    Install {name} in container (postCreateCommand)")
                } else {
                    format!("    Install {name} to enable")
                };
                items.push(crate::view::popup::PopupListItem::new(advisory).disabled());
            } else {
                // Two sibling rows for a dormant server, ordered by what the
                // user most likely wants:
                //   "Start <name> (always)" — persist auto_start=true AND start
                //                              now. Listed first (the default)
                //                              so Enter does the common thing.
                //   "Start <name> once"     — start for this session only.
                // The "once" suffix is only needed when the "(always)" sibling
                // is present (i.e. auto_start is currently false).
                let is_manual = !servers.auto_start.get(name).copied().unwrap_or(true);

                if is_manual {
                    items.push(
                        crate::view::popup::PopupListItem::new(format!(
                            "    Start {name} (always)"
                        ))
                        .with_data(format!("autostart:{language}/{name}")),
                    );
                }

                let start_label = if is_manual {
                    format!("    Start {name} once")
                } else {
                    format!("    Start {name}")
                };
                // All dormant servers for a language share the same `start:`
                // key; only emit the row once.
                let start_key = format!("start:{language}");
                if !items
                    .iter()
                    .any(|i| i.data.as_deref() == Some(start_key.as_str()))
                {
                    items.push(
                        crate::view::popup::PopupListItem::new(start_label).with_data(start_key),
                    );
                }
            }
        }
    }

    /// Push the language-level footer rows — enable/disable, view log, plugin
    /// contributions, and the trailing dismiss row — into `items`.
    fn push_lsp_footer_rows(
        &self,
        language: &str,
        servers: &LspStatusServers,
        items: &mut Vec<crate::view::popup::PopupListItem>,
    ) {
        // Disable / Enable row. The label flips on either the session-level
        // dismiss flag OR a fully-`enabled = false` config: both mean "the
        // language is currently muted", and showing "Disable" while every
        // server is already disabled would leave no surface to undo it.
        let muted = servers.user_dismissed || !servers.any_enabled;
        if muted {
            items.push(
                crate::view::popup::PopupListItem::new(format!("    Enable LSP for {language}"))
                    .with_data(format!("enable:{language}")),
            );
        } else {
            items.push(
                crate::view::popup::PopupListItem::new(format!("    Disable LSP for {language}"))
                    .with_data(format!("dismiss:{language}")),
            );
        }

        // View log action — grayed out and non-actionable when no log file
        // exists yet for this language.
        let log_path = crate::services::log_dirs::lsp_log_path(language);
        let mut log_item = crate::view::popup::PopupListItem::new("    View Log".to_string());
        if log_path.exists() {
            log_item = log_item.with_data(format!("log:{language}"));
        } else {
            log_item = log_item.disabled();
        }
        items.push(log_item);

        // Plugin-contributed rows — injected as an extra "Plugin actions"
        // section. Sorted by plugin_id for stable ordering; a single header
        // labels the section so the user can tell these rows come from a
        // plugin (vs. built-in actions like Stop/Restart).
        let mut contributed: Vec<(&String, &Vec<crate::app::LspMenuItem>)> = self
            .active_window()
            .lsp_menu_contributions
            .iter()
            .filter_map(|((lang, plugin_id), plugin_items)| {
                if lang == language && !plugin_items.is_empty() {
                    Some((plugin_id, plugin_items))
                } else {
                    None
                }
            })
            .collect();
        contributed.sort_by(|a, b| a.0.cmp(b.0));
        if !contributed.is_empty() {
            items.push(crate::view::popup::PopupListItem::new(
                "  ─ Plugin actions ─".to_string(),
            ));
            for (plugin_id, plugin_items) in contributed {
                for it in plugin_items {
                    items.push(
                        crate::view::popup::PopupListItem::new(format!("    {}", it.label))
                            .with_data(format!("plugin:{}|{}", plugin_id, it.id)),
                    );
                }
            }
        }

        // Trailing Dismiss row — an on-screen way out for users who don't know
        // Esc works. The key label comes from the keybinding resolver so a
        // rebound PopupCancel stays visible ("Dismiss (Q)", etc.), falling back
        // to "Esc".
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
            crate::view::popup::PopupListItem::new(format!("    Dismiss ({cancel_binding})"))
                .with_data("cancel_popup".to_string()),
        );
    }

    /// Pin the popup width (using worst-case widths so it doesn't jitter),
    /// choose the anchor + initial selection, and show the assembled `items`
    /// as the LSP-status list popup on the active buffer.
    fn present_lsp_status_popup(
        &mut self,
        language: &str,
        items: Vec<crate::view::popup::PopupListItem>,
        focused: bool,
    ) {
        use crate::view::popup::{Popup, PopupContent, PopupKind, PopupResolver};
        use ratatui::style::Style;

        let max_static_item_width = items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(20);
        let popup_width = (max_static_item_width.max(LSP_PROGRESS_LINE_MAX) as u16 + 4)
            .clamp(30, LSP_POPUP_WIDTH_MAX);

        // Pre-select the first actionable item (skip header items with no data
        // and disabled items like a non-existent View Log).
        let first_actionable = items
            .iter()
            .position(|i| i.data.is_some() && !i.disabled)
            .unwrap_or(0);

        // Left-align the popup's column with the LSP indicator on the status
        // bar, if we know where it was drawn in the last frame. Falls back to
        // the BottomRight anchor when the LSP segment isn't visible.
        let position = self
            .active_chrome()
            .status_bar
            .clickable_area(crate::view::ui::status_bar::StatusBarClickable::Lsp)
            .map(
                |(status_row, col_start, _)| crate::view::popup::PopupPosition::AboveStatusBarAt {
                    x: col_start,
                    status_row,
                },
            )
            .unwrap_or(crate::view::popup::PopupPosition::BottomRight);

        let focus_hint = if !focused {
            self.popup_focus_key_hint()
        } else {
            None
        };
        let popup = Popup {
            kind: PopupKind::List,
            title: Some(format!("LSP Servers ({language})")),
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
            // Mark this as the LSP status popup so confirm/cancel routes through
            // handle_lsp_status_action regardless of what else is on screen.
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
    /// - **Disconnected:** offers "Reconnect" (best-effort) for a remote-agent
    ///   window; nothing else (the reconnect also runs automatically).
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
        // Not a toggle-close: clear any *other* menu popup (a different
        // status-bar picker left open) before building this one, so the
        // remote menu never renders over a stale popup (#1941). Done here,
        // after the toggle check, rather than in the click handler — doing it
        // there would close our own popup and defeat the toggle.
        self.dismiss_menu_popups_for_prompt();

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

        // Core-driven FailedAttach: a remote (SSH) workspace whose reconnect
        // failed (recorded on the window, not via the plugin override). Offer a
        // single Retry that re-runs the core reconnect — independent of the
        // devcontainer-specific override path above. There is deliberately no
        // "Reopen Locally" here: the SSH flow keeps its remote authority (the
        // error clears on a successful reconnect, which also runs automatically
        // in the background), so a local fallback would only strand the open
        // remote buffers on a mismatched filesystem.
        let core_failed_attach = !override_handled
            && self
                .active_window()
                .remote_reconnect_error
                .as_deref()
                .is_some();
        if core_failed_attach {
            let err = self
                .active_window()
                .remote_reconnect_error
                .clone()
                .unwrap_or_default();
            title = if err.is_empty() {
                "Remote: Reconnect failed".to_string()
            } else {
                format!("Remote: Reconnect failed — {err}")
            };
            items.push(
                PopupListItem::new("    Retry".to_string())
                    .with_data("retry_reconnect".to_string()),
            );
        }

        if !override_handled && !core_failed_attach {
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
                // Disconnected — offer a reconnect and nothing else. The old
                // "Go Local" (`detach`) escape hatch was removed: it silently
                // swapped the window's remote authority for the local one,
                // stranding the open remote buffers on a mismatched filesystem —
                // surprising behaviour with no obvious way back. Recovery from a
                // dropped link is a reconnect, which also runs automatically in
                // the background.
                (Some(_), true) => {
                    title = "Remote: Disconnected".to_string();
                    // Offer Reconnect for a live remote-agent (SSH/kube) window:
                    // its backend can be rebuilt from the stored
                    // `RemoteAgentSpec`, re-pointing this window's authority and
                    // respawning its dead terminal over the new link. Container
                    // (`Plugin`) windows reconnect through their owning plugin
                    // (`devcontainer up`), not here, so they don't get this row.
                    let is_remote_agent = matches!(
                        self.active_window().authority_spec,
                        crate::services::authority::SessionAuthoritySpec::RemoteAgent(_)
                    );
                    if is_remote_agent {
                        items.push(
                            PopupListItem::new("    Reconnect".to_string())
                                .with_data("reconnect".to_string()),
                        );
                    }
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
            .status_bar
            .clickable_area(crate::view::ui::status_bar::StatusBarClickable::RemoteIndicator)
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

    /// Show the read-only indicator menu, anchored to the status bar's
    /// `{read_only}` segment. Offers to enable editing (which dispatches
    /// `Action::ToggleReadOnly`). Toggles closed on a second click, mirroring
    /// the LSP / remote menus.
    pub fn show_read_only_popup(&mut self) {
        use crate::view::popup::{
            Popup, PopupContent, PopupKind, PopupListItem, PopupPosition, PopupResolver,
        };
        use ratatui::style::Style;

        // Second click on the indicator closes the menu instead of rebuilding.
        if self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| matches!(p.resolver, PopupResolver::ReadOnly))
        {
            self.hide_popup();
            return;
        }
        // Not a toggle-close: clear any other menu popup left open so this one
        // never renders over a stale popup (#1941).
        self.dismiss_menu_popups_for_prompt();

        let items = vec![
            PopupListItem::new(format!("    {}", t!("read_only.menu.enable_editing")))
                .with_data("toggle_read_only".to_string()),
            PopupListItem::new(format!("    {}", t!("read_only.menu.cancel")))
                .with_data("cancel".to_string()),
        ];

        let position = self
            .active_chrome()
            .status_bar
            .clickable_area(crate::view::ui::status_bar::StatusBarClickable::ReadOnly)
            .map(
                |(status_row, col_start, _)| PopupPosition::AboveStatusBarAt {
                    x: col_start,
                    status_row,
                },
            )
            .unwrap_or(PopupPosition::BottomRight);

        let popup_width = (items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(24)
            + 4) as u16;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some(t!("read_only.menu.title").to_string()),
            description: None,
            transient: false,
            content: PopupContent::List { items, selected: 0 },
            position,
            width: popup_width.clamp(28, 50),
            max_height: 10,
            bordered: true,
            border_style: Style::default().fg(self.theme.read().unwrap().popup_border_fg),
            background_style: Style::default().bg(self.theme.read().unwrap().popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::ReadOnly,
            // Explicitly invoked from the status-bar `{read_only}` element, so
            // this popup wants the keyboard immediately.
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

    /// Dispatch the action selected from the read-only indicator menu.
    /// `"toggle_read_only"` flips the buffer's read-only state (enabling
    /// editing); `"cancel"` is a no-op (the popup already closed).
    pub fn handle_read_only_menu_action(&mut self, action_key: &str) {
        match action_key {
            "toggle_read_only" => {
                if let Err(e) =
                    self.handle_action(crate::input::keybindings::Action::ToggleReadOnly)
                {
                    tracing::warn!("read-only menu: toggling read-only failed: {}", e);
                }
            }
            "cancel" => {}
            other => {
                tracing::warn!(
                    "handle_read_only_menu_action: unknown action key '{}'",
                    other
                );
            }
        }
    }

    /// Show the update-available menu, anchored to the status bar's `{update}`
    /// segment. Offers to run the update — which opens a **local** terminal
    /// buffer running `fresh --cmd update --yes` (so package-manager / sudo
    /// prompts work and the binary that gets updated is the one actually
    /// running) — or to dismiss. Toggles closed on a second click, mirroring the
    /// LSP / remote / read-only menus.
    pub fn show_update_popup(&mut self, version: &str) {
        use crate::view::popup::{
            Popup, PopupContent, PopupKind, PopupListItem, PopupPosition, PopupResolver,
        };
        use ratatui::style::Style;

        // Second click on the indicator closes the menu instead of rebuilding.
        if self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| matches!(p.resolver, PopupResolver::Update))
        {
            self.hide_popup();
            return;
        }
        // Clear any *other* status-bar menu left open before building this one
        // (done after the toggle check so it never closes our own popup).
        self.dismiss_menu_popups_for_prompt();

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

        let items: Vec<PopupListItem> = vec![
            PopupListItem::new(format!(
                "    {}",
                t!("update.popup_update", version = version)
            ))
            .with_data("update".to_string()),
            PopupListItem::new(format!("    Dismiss ({})", cancel_binding))
                .with_data("cancel_popup".to_string()),
        ];

        let position = self
            .active_chrome()
            .status_bar
            .clickable_area(crate::view::ui::status_bar::StatusBarClickable::Update)
            .map(
                |(status_row, col_start, _)| PopupPosition::AboveStatusBarAt {
                    x: col_start,
                    status_row,
                },
            )
            .unwrap_or(PopupPosition::BottomRight);

        let popup_width = (items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(24)
            + 4) as u16;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some(t!("update.available_title").to_string()),
            description: None,
            transient: false,
            content: PopupContent::List { items, selected: 0 },
            position,
            width: popup_width.clamp(28, 50),
            max_height: 10,
            bordered: true,
            border_style: Style::default().fg(self.theme.read().unwrap().popup_border_fg),
            background_style: Style::default().bg(self.theme.read().unwrap().popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::Update,
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

    /// Handle a click/confirm on the update-available menu. "update" launches
    /// the local update terminal; any other key just dismisses (already done by
    /// the caller).
    pub fn handle_update_menu_action(&mut self, action_key: &str) {
        if action_key == "update" {
            self.start_self_update();
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
        if action_key == "reconnect" || action_key == "retry_reconnect" {
            // Reconnect the active window's remote backend on explicit request:
            // the Disconnected popup's "Reconnect" row and the FailedAttach
            // "Retry" row both land here. `force_reconnect_remote_session`
            // clears any recorded error up front (so the indicator flips to
            // "Connecting"), forces the connect even for a *live* window whose
            // stale keepalive is still parked, and is a no-op for a non-remote
            // workspace. The reconnect path is plugins-gated (remote sessions
            // are created through the orchestrator plugin), so this is a no-op
            // in a plugins-less build.
            #[cfg(feature = "plugins")]
            self.force_reconnect_remote_session(self.active_window);
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
    ///
    /// `cancellable` picks the modal's secondary button. The mandatory
    /// open-time gate (`false`) offers **Quit** — there is nothing else to fall
    /// back to before a decision is made. Prompts raised by *activating another
    /// workspace* in an already-running editor (opening/switching an
    /// Orchestrator session, changing the project) pass `true` so the secondary
    /// is **Cancel**: dismissing it simply leaves the just-activated workspace
    /// Restricted rather than tearing down the whole editor — the other open
    /// sessions must survive.
    pub fn maybe_prompt_workspace_trust(&mut self, cancellable: bool) {
        // Phase 1 of the trust+env+devcontainer UX plan (see
        // `docs/internal/trust-env-devcontainer-ux-plan.md`): when the
        // workspace is undecided AND has executable content, the core trust
        // modal is the *single* trust prompt for every kind of marker —
        // env-shell (`.envrc`/`mise.toml`/`.tool-versions`), project
        // manifests, devcontainer config, .NET solution/project files. It is
        // shown with concrete framing: the popup names the *specific* markers
        // that triggered it (Cargo.toml, build.rs, .envrc, App.sln…) rather
        // than the abstract "this project can run code on your machine." The
        // workspace starts Restricted while waiting for the user to choose.
        //
        // Previously env-shell folders were carved out here so the
        // env-manager plugin could surface its own combined "Trust this
        // folder and activate?" popup — a *second* trust UI for the same
        // decision, which is exactly the duplication users hit. Now the
        // plugin no longer asks the trust question: it activates the env as a
        // *consequence* of trust, driven by the `trust_changed` hook this
        // editor fires when the level changes (see env-manager.ts). One
        // decision, one prompt, one place it is recorded.
        //
        // A decision the user explicitly recorded is always honored — this
        // branch only fires for undecided projects.
        let store = crate::services::workspace_trust::TrustStore::for_project_dir(
            &self.dir_context.project_state_dir(self.working_dir()),
        );
        if store.is_decided() {
            return; // respect a decision the user already recorded
        }

        let markers =
            crate::services::workspace_trust::executable_content_markers(self.working_dir());

        if markers.is_empty() {
            // Nothing executable to gate (plain text/docs). Trust silently so
            // the restricted chip doesn't appear and the user isn't blocked on
            // a question with no real downside. Persist it — same decision we'd
            // record if the user had explicitly confirmed.
            self.authority()
                .workspace_trust
                .set_level(crate::services::workspace_trust::TrustLevel::Trusted);
            return;
        }

        // All executable content — including a *bare* `.venv`/`venv` — goes
        // through the trust decision. A virtualenv is a module-namespace
        // boundary, NOT a security boundary: activating it runs the repo's
        // interpreter, which auto-executes any `.pth`/`sitecustomize.py` shipped
        // inside the venv (a documented malware-drop vector), so its mere
        // presence must not silently grant trust. "Path-only" still governs how
        // it *activates* — silently, with no second prompt, once the workspace
        // is trusted (see env-manager) — it just no longer exempts the folder
        // from the trust decision itself.

        // Seed Restricted *in memory only* —
        // `set_level_transient` does not write to disk. The on-disk store
        // stays undecided until the user picks a concrete option in the
        // modal. That preserves the contract: cancelling (quit) leaves the
        // project undecided so the prompt fires again next time, while any
        // deliberate choice (the modal's three radios) writes the decision
        // through via `set_level`.
        self.authority()
            .workspace_trust
            .set_level_transient(crate::services::workspace_trust::TrustLevel::Restricted);

        // Secondary button follows the caller: Quit for the mandatory
        // open-time gate, Cancel for a workspace-activation prompt (where any
        // concrete option resolves it and Esc just leaves it Restricted).
        self.show_workspace_trust_popup(cancellable);
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
        self.workspace_trust_scroll = 0;
        self.workspace_trust_markers =
            crate::services::workspace_trust::executable_content_markers(self.working_dir());

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
        let selected = match self.authority().workspace_trust.level() {
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
    /// `"trusted"` / `"restricted"` / `"blocked"` set the level (persisted);
    /// the new policy applies live to the next authority-routed spawn, scoped
    /// to this session's window — no editor restart. Anything else is logged
    /// and ignored.
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
        let fs = self.authority().filesystem.as_ref();
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
            &self.theme.read().unwrap(),
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

    /// Show a prominent, centered modal popup reporting that a settings save
    /// failed.
    ///
    /// Used when the config file on disk can't be parsed: the save is aborted
    /// and the file is left untouched, but the user must be told loudly. A
    /// status-bar line is far too easy to miss for "your change didn't take
    /// effect", so this raises a focused, centered popup (red border) that the
    /// user dismisses with Esc — and on dismissal we open the offending config
    /// file (for `layer`) so they can fix the syntax error right away.
    ///
    /// The body is hard-wrapped here (long config paths have no spaces to break
    /// on) and rendered as plain text so a long file name wraps inside the
    /// border instead of being clipped.
    pub fn show_settings_save_error_popup(
        &mut self,
        layer: crate::config_io::ConfigLayer,
        error: &str,
    ) {
        use crate::view::popup::{Popup, PopupPosition, PopupResolver};
        use ratatui::style::Style;

        const WIDTH: u16 = 64;
        // Border (2) + a little inner padding/scrollbar headroom (2).
        let wrap_width = (WIDTH as usize).saturating_sub(4);

        let detail = t!("settings.failed_to_save", error = error).to_string();
        let unchanged = t!("settings.save_failed_unchanged").to_string();
        let open_hint = t!("settings.save_failed_open_hint").to_string();
        let title = t!("settings.save_failed_title").to_string();

        // One blank line between paragraphs; each paragraph hard-wrapped so a
        // long, space-less path breaks rather than overflowing the border.
        let mut lines: Vec<String> = Vec::new();
        for (i, para) in [detail.as_str(), unchanged.as_str(), open_hint.as_str()]
            .iter()
            .enumerate()
        {
            if i > 0 {
                lines.push(String::new());
            }
            lines.extend(hard_wrap(para, wrap_width));
        }

        let popup = {
            let theme = self.theme.read().unwrap();
            let mut p = Popup::text(lines, &theme)
                .with_title(title)
                .with_focused(true);
            p.transient = false;
            p.position = PopupPosition::Centered;
            p.width = WIDTH;
            p.max_height = 14;
            // Red border to read as an error, not a neutral info popup.
            p.border_style = Style::default().fg(theme.diagnostic_error_fg);
            p.background_style = Style::default().bg(theme.popup_bg);
            p.resolver = PopupResolver::SettingsSaveError { layer };
            p
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
