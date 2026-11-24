//! The editor's state as data, for a client that draws its own UI
//! (`docs/internal/web-ui.md`, *Everything else, as data plus change
//! events*).
//!
//! Each function answers one topic as JSON. A host diffs a topic against what
//! it last sent a client and pushes it when it changed; nothing here holds
//! client state or knows how anything is shown.

use super::Editor;
use crate::model::event::{BufferId, LeafId};
use crate::view::split::SplitNode;
use fresh_i18n::t;
use serde_json::{json, Value};

impl Editor {
    /// The active window's split tree: containers with their direction and
    /// ratio, leaves with the buffer they show and their tabs.
    pub fn engine_layout(&self) -> Value {
        let win = self.active_window();
        let Some((mgr, states)) = win.buffers.splits() else {
            return Value::Null;
        };
        fn node(
            n: &SplitNode,
            states: &std::collections::HashMap<
                crate::model::event::LeafId,
                crate::view::split::SplitViewState,
            >,
        ) -> Value {
            match n {
                SplitNode::Leaf {
                    buffer_id,
                    split_id,
                    role,
                } => {
                    let tabs: Vec<usize> = states
                        .get(split_id)
                        .map(|s| s.buffer_tab_ids().map(|b| b.0).collect())
                        .unwrap_or_default();
                    json!({
                        "leaf": split_id.0 .0,
                        "buffer": buffer_id.0,
                        "tabs": tabs,
                        "role": role.as_ref().map(|r| format!("{r:?}")),
                    })
                }
                SplitNode::Grouped {
                    split_id,
                    name,
                    layout,
                    ..
                } => json!({
                    "group": split_id.0 .0,
                    "name": name,
                    "layout": node(layout, states),
                }),
                SplitNode::Split {
                    direction,
                    first,
                    second,
                    ratio,
                    split_id,
                    ..
                } => json!({
                    "id": split_id.0 .0,
                    // Horizontal stacks its children top and bottom.
                    "stack": matches!(direction, crate::model::event::SplitDirection::Horizontal),
                    "ratio": ratio,
                    "a": node(first, states),
                    "b": node(second, states),
                }),
            }
        }
        json!({
            // Leaf ids are numbered per window: the window says which
            // workspace's panes these are.
            "window": self.active_window_id().0,
            "active": mgr.active_split().0 .0,
            "root": node(mgr.root(), states),
        })
    }

    /// The rows of a right-click menu the web is opening: the terminal's
    /// own menu for that place — a tab's (the editor's items, then the
    /// plugins'), the tab bar's "+" — or, for an explorer entry, the
    /// plugins' items (the web's explorer has its own for files).
    pub fn engine_menu_rows(&self, at: &WebMenu) -> Vec<Value> {
        self.web_menu_rows(at)
            .into_iter()
            .map(|r| json!({ "label": r.label, "id": r.id, "destructive": r.destructive, "disabled": r.disabled }))
            .collect()
    }

    /// The rows of the menu at `at`, each with its stable id.
    fn web_menu_rows(&self, at: &WebMenu) -> Vec<crate::app::types::ContextMenuRow> {
        match at {
            WebMenu::Tab { leaf, buffer } => self.tab_menu_for(*leaf, *buffer, 0, 0).rows(),
            WebMenu::NewTab { .. } => crate::app::types::NewTabMenuItem::all()
                .iter()
                .map(|i| crate::app::types::ContextMenuRow {
                    label: i.label(),
                    id: i.id().to_string(),
                    destructive: i.destructive(),
                    disabled: false,
                })
                .collect(),
            WebMenu::Explorer { path, dir } => {
                let plugin = self.explorer_plugin_items(entry_kind(*dir), path.clone());
                plugin
                    .items
                    .iter()
                    .enumerate()
                    .map(|(i, e)| crate::app::types::ContextMenuRow {
                        label: e.label.clone(),
                        id: format!("plugin:{i}:{}", e.handler),
                        destructive: false,
                        disabled: !e.enabled,
                    })
                    .collect()
            }
        }
    }

    /// Choose the row `id` of that menu, through the path the terminal's
    /// choice takes: the menu is built, its row highlighted and activated
    /// at once (a disabled row does nothing), so the terminal never shows it.
    /// By id, not place: if the menu changed since the client was given its
    /// rows (a plugin's item came or went), a row that is gone is refused
    /// rather than another one run.
    pub fn engine_menu_choose(&mut self, at: &WebMenu, id: &str) -> Result<(), String> {
        use crate::app::types::ContextMenuKind;
        let index = self
            .web_menu_rows(at)
            .iter()
            .position(|r| r.id == id)
            .ok_or_else(|| format!("that menu item is gone ({id})"))?;
        let kind = match at {
            WebMenu::Tab { leaf, buffer } => {
                let mut menu = self.tab_menu_for(*leaf, *buffer, 0, 0);
                if index >= menu.menu.item_count {
                    return Err(format!("no row {index}"));
                }
                menu.menu.highlighted = index;
                self.active_window_mut().tab_context_menu = Some(menu);
                ContextMenuKind::Tab
            }
            WebMenu::NewTab { leaf } => {
                let mut menu = crate::app::types::NewTabMenu::new(*leaf, 0, 0);
                if index >= menu.menu.item_count {
                    return Err(format!("no row {index}"));
                }
                menu.menu.highlighted = index;
                self.active_window_mut().new_tab_menu = Some(menu);
                ContextMenuKind::NewTab
            }
            WebMenu::Explorer { path, dir } => {
                let plugin = self.explorer_plugin_items(entry_kind(*dir), path.clone());
                let entry = plugin
                    .items
                    .get(index)
                    .ok_or_else(|| format!("no row {index}"))?;
                if entry.enabled {
                    let handler = entry.handler.clone();
                    self.run_plugin_menu_handler(&handler, &plugin.target);
                }
                return Ok(());
            }
        };
        let r = self
            .activate_highlighted_context_menu(kind)
            .map_err(|e| e.to_string());
        // A disabled row leaves the terminal's menu open; the web's is closed.
        self.active_window_mut().close_context_menus();
        r
    }

    /// Every open buffer: its name, path, and state.
    pub fn engine_buffers(&self) -> Value {
        let win = self.active_window();
        let mut ids: Vec<BufferId> = win.buffers.iter().map(|(id, _)| *id).collect();
        ids.sort_by_key(|b| b.0);
        let list: Vec<Value> = ids
            .into_iter()
            .filter_map(|id| {
                let state = win.buffers.get(&id)?;
                let meta = win.buffer_metadata.get(&id);
                let path = meta.and_then(|m| match &m.kind {
                    crate::app::types::BufferKind::File { path, .. } => {
                        Some(path.to_string_lossy().into_owned())
                    }
                    _ => None,
                });
                Some(json!({
                    "id": id.0,
                    "name": self.get_buffer_display_name(id),
                    "path": path,
                    "modified": state.buffer.is_modified(),
                    "readOnly": meta.map(|m| m.read_only).unwrap_or(false),
                    "hidden": meta.map(|m| m.hidden_from_tabs).unwrap_or(false),
                    "language": state.language,
                    "terminal": win.is_terminal_buffer(id),
                }))
            })
            .collect();
        Value::Array(list)
    }
}

impl Editor {
    /// What the terminal's status bar reports, as facts: the active buffer's
    /// cursor, language, encoding and line ending, its diagnostics, the LSP
    /// state, the messages, and the plugins' status items.
    pub fn engine_status(&self) -> Value {
        let buffer = self.active_buffer();
        let split = self.effective_active_split();
        let win = self.active_window();
        let mut out = serde_json::Map::new();
        if let Some(state) = win.buffers.get(&buffer) {
            let cursors = win
                .buffers
                .splits()
                .and_then(|(_, vs)| vs.get(&split))
                .map(|v| v.buffer_tab_state().cursors.clone());
            if let Some(cursors) = cursors {
                let pos = cursors.primary().position;
                let line = win.primary_cursor_line(split, buffer);
                let line_start = state.buffer.line_start_offset(line).unwrap_or(0);
                let col = state.buffer.slice_bytes(line_start..pos.max(line_start));
                let col = String::from_utf8_lossy(&col).chars().count();
                out.insert("line".into(), json!(line + 1));
                out.insert("col".into(), json!(col + 1));
                out.insert("cursors".into(), json!(cursors.count()));
                out.insert("lines".into(), json!(state.buffer.line_count()));
            }
            out.insert("language".into(), json!(state.language));
            out.insert(
                "languageName".into(),
                json!(crate::primitives::grammar::short_display_name(
                    &state.display_name
                )),
            );
            out.insert(
                "encoding".into(),
                json!(state.buffer.encoding().display_name()),
            );
            out.insert(
                "lineEnding".into(),
                json!(state.buffer.line_ending().display_name()),
            );
            let ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
            let (mut e, mut w, mut i) = (0, 0, 0);
            for o in state.overlays.in_namespace(&ns) {
                match o.priority {
                    100 => e += 1,
                    50 => w += 1,
                    _ => i += 1,
                }
            }
            out.insert(
                "diagnostics".into(),
                json!({ "errors": e, "warnings": w, "info": i }),
            );
        }
        out.insert("buffer".into(), json!(buffer.0));
        out.insert(
            "readOnly".into(),
            json!(win
                .buffer_metadata
                .get(&buffer)
                .is_some_and(|m| m.read_only)),
        );
        let meta = win.buffer_metadata.get(&buffer);
        let (lsp, lsp_state) = crate::app::lsp_status::compose_lsp_status(
            &win.buffers
                .get(&buffer)
                .map(|s| s.language.clone())
                .unwrap_or_default(),
            meta.filter(|m| !m.lsp_enabled)
                .and_then(|m| m.lsp_disabled_reason.as_deref()),
            &win.lsp_progress,
            &win.lsp_server_statuses,
            &self.config.lsp,
            &win.user_dismissed_lsp_languages,
            self.config.lsp_enabled,
        );
        out.insert("lsp".into(), json!(lsp));
        out.insert("lspState".into(), json!(format!("{lsp_state:?}")));
        // The key that opens the palette (the terminal status bar's hint).
        out.insert(
            "paletteKey".into(),
            json!(self.keybindings.read().unwrap().get_keybinding_for_action(
                &crate::input::keybindings::Action::QuickOpen,
                crate::input::keybindings::KeyContext::Global,
            )),
        );
        out.insert("message".into(), json!(win.status_message));
        out.insert("pluginMessage".into(), json!(win.plugin_status_message));
        out.insert("remote".into(), json!(self.connection_display_string()));
        out.insert("session".into(), json!(self.session_display_name()));
        out.insert(
            "trust".into(),
            json!(format!("{:?}", self.authority().workspace_trust.level())),
        );
        let mut items: Vec<(String, String)> = self
            .get_status_bar_element_values(buffer)
            .into_iter()
            .collect();
        items.sort();
        out.insert(
            "items".into(),
            Value::Array(
                items
                    .into_iter()
                    .map(|(k, v)| json!({ "key": k, "text": v }))
                    .collect(),
            ),
        );
        Value::Object(out)
    }

    /// Whether the keyboard-calibration wizard is up. It calibrates a
    /// terminal's key encoding, which a browser does not have.
    pub fn engine_calibration_open(&self) -> bool {
        self.calibration_wizard.is_some()
    }

    /// Close the calibration wizard a web request opened, saying why.
    pub fn engine_refuse_calibration(&mut self) {
        self.calibration_wizard = None;
        self.set_status_message(t!("calibration.tui_only").to_string());
    }

    /// Every command, with its keybinding.
    pub fn engine_commands(&self) -> Value {
        let commands = self.command_registry().read().unwrap().get_all();
        let kb = self.keybindings.read().unwrap();
        let context = self.active_window().key_context.clone();
        Value::Array(
            commands
                .iter()
                .filter(|c| !matches!(c.action, crate::input::keybindings::Action::CalibrateInput))
                .map(|c| {
                    json!({
                        "name": c.name,
                        "label": c.get_localized_name(),
                        "description": c.get_localized_description(),
                        "keys": kb.get_keybinding_for_action(&c.action, context.clone()),
                    })
                })
                .collect(),
        )
    }

    /// Run the command named `name`, as the palette would.
    pub fn engine_run_command(&mut self, name: &str) -> anyhow::Result<()> {
        let action = self
            .command_registry()
            .read()
            .unwrap()
            .get_all()
            .into_iter()
            .find(|c| c.name == name)
            .map(|c| c.action)
            .ok_or_else(|| anyhow::anyhow!("no command {name:?}"))?;
        self.handle_action(action)
    }

    /// The menu bar's menus, with each item's command, keys and state.
    pub fn engine_menus(&self) -> Value {
        fn item(ed: &Editor, i: &fresh_core::menu::MenuItem) -> Value {
            use fresh_core::menu::MenuItem::*;
            let ctx = &ed.menu_state().context;
            match i {
                Separator { .. } => json!({ "separator": true }),
                Action {
                    label,
                    action,
                    args,
                    checkbox,
                    ..
                } => json!({
                    "label": label,
                    "action": action,
                    "args": args,
                    "keys": ed.accelerator_for(action),
                    "enabled": crate::view::ui::menu::is_menu_item_enabled(i, ctx),
                    "checked": checkbox
                        .as_ref()
                        .map(|_| crate::view::ui::menu::is_checkbox_checked(checkbox, ctx)),
                }),
                Submenu { label, items } => json!({
                    "label": label,
                    "items": items.iter().map(|i| item(ed, i)).collect::<Vec<_>>(),
                }),
                DynamicSubmenu { label, .. } => json!({ "label": label, "items": [] }),
                Label { info } => json!({ "info": info }),
            }
        }
        // The Alt+letter the keymap binds to opening each menu, so the page
        // opens a menu only on a key the keymap gives it: a rebinding, or
        // `menu_bar_mnemonics` off (the resolver then drops the mnemonics),
        // leaves the key to the core.
        let mnemonics: Vec<(String, char)> = self
            .keybindings
            .read()
            .map(|kb| {
                ('a'..='z')
                    .filter_map(|c| {
                        let key = crossterm::event::KeyEvent::new(
                            crossterm::event::KeyCode::Char(c),
                            crossterm::event::KeyModifiers::ALT,
                        );
                        match kb.resolve(&key, crate::input::keybindings::KeyContext::Normal) {
                            crate::input::keybindings::Action::MenuOpen(name) => Some((name, c)),
                            _ => None,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();
        Value::Array(
            self.all_menus_expanded()
                .iter()
                .map(|m| {
                    let mnemonic = mnemonics
                        .iter()
                        .find(|(name, _)| crate::config::MenuExt::match_id(m).eq_ignore_ascii_case(name))
                        .map(|(_, c)| c.to_ascii_uppercase().to_string());
                    json!({
                        "label": m.label,
                        "mnemonic": mnemonic,
                        "items": m.items.iter()
                        .filter(|i| !matches!(i, fresh_core::menu::MenuItem::Action { action, .. } if action == "calibrate_input"))
                        .map(|i| item(self, i)).collect::<Vec<_>>(),
                    })
                })
                .collect(),
        )
    }

    /// Run a menu item's action.
    pub fn engine_run_menu_action(
        &mut self,
        name: &str,
        args: &std::collections::HashMap<String, Value>,
    ) -> anyhow::Result<()> {
        let action = crate::input::keybindings::Action::from_str(name, args)
            .unwrap_or_else(|| crate::input::keybindings::Action::PluginAction(name.to_string()));
        self.handle_action(action)
    }

    /// The question the editor is asking, if any: a prompt (with its
    /// suggestions) or a confirmation (with its choices).
    pub fn engine_ask(&self) -> Value {
        if self.workspace_trust_on_top() {
            let cancellable = self.workspace_trust_prompt_cancellable;
            return json!({
                "kind": "trust",
                "title": fresh_i18n::t!("trust.dialog.security_warning"),
                "path": self.working_dir().display().to_string(),
                "triggers": self.workspace_trust_markers,
                "options": crate::view::workspace_trust_dialog::options()
                    .into_iter()
                    .map(|o| json!({ "label": o.label, "description": o.description }))
                    .collect::<Vec<_>>(),
                "selected": self.current_workspace_trust_selection(),
                "okLabel": fresh_i18n::t!("trust.dialog.btn_ok"),
                "declineLabel": if cancellable {
                    fresh_i18n::t!("trust.dialog.btn_cancel")
                } else {
                    fresh_i18n::t!("trust.dialog.btn_quit")
                },
            });
        }
        let Some(p) = self.active_window().prompt.as_ref() else {
            return Value::Null;
        };
        if let Some(c) = p.confirm.as_ref() {
            return json!({
                "kind": "confirm",
                "title": c.title,
                "body": c.body,
                "detail": c.detail,
                "choices": c.choices.iter().map(|ch| json!({
                    "label": ch.label,
                    "destructive": ch.tone == crate::view::confirm::Tone::Destructive,
                })).collect::<Vec<_>>(),
                "selected": c.selected,
                "escape": c.escape,
            });
        }
        let suggestions: Vec<Value> = p
            .suggestions
            .iter()
            .take(500)
            .map(|s| {
                json!({
                    "text": s.text,
                    "description": s.description,
                    "keys": s.keybinding,
                    "disabled": s.disabled,
                })
            })
            .collect();
        // A prompt that browses the filesystem (Open File, Save As, Switch
        // Project, …) carries the directory it is showing.
        let browser = self.active_window().file_open_state.as_ref().map(|f| {
            let selected = f.selected_index();
            json!({
                "dir": f.current_dir.to_string_lossy(),
                "entries": f.entries.iter().filter(|e| e.matches_filter).map(|e| {
                    let m = e.fs_entry.metadata.as_ref();
                    json!({
                        "name": e.fs_entry.name,
                        "dir": e.fs_entry.is_dir(),
                        "size": m.map(|m| m.size),
                        "modified": m.and_then(|m| m.modified)
                            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                            .map(|d| d.as_secs()),
                    })
                }).collect::<Vec<_>>(),
                "indices": f.entries.iter().enumerate()
                    .filter(|(_, e)| e.matches_filter).map(|(i, _)| i).collect::<Vec<_>>(),
                "selected": selected,
                "loading": f.loading,
                "error": f.error,
                "showHidden": f.show_hidden,
                "shortcuts": f.shortcuts.iter().map(|s| json!({
                    "label": s.label, "path": s.path.to_string_lossy(), "description": s.description,
                })).collect::<Vec<_>>(),
            })
        });
        json!({
            "kind": "prompt",
            "browser": browser,
            "type": format!("{:?}", p.prompt_type).split(['(', ' ', '{']).next().unwrap_or(""),
            "message": p.message,
            "query": p.input_str(),
            "suggestions": suggestions,
            "total": p.suggestions.len(),
            "selected": p.selected_suggestion(),
        })
    }

    /// Answer the prompt: set its text, pick a suggestion, accept or cancel.
    pub fn engine_prompt_set(&mut self, text: &str) {
        if let Some(p) = self.prompt_mut() {
            p.set_input(text.to_string());
        }
        self.update_prompt_suggestions();
    }

    /// Pick the entry at `index` of the file browser's listing (an index
    /// into its entries, as `browser.indices` gives them).
    pub fn engine_browser_select(&mut self, index: usize) {
        if let Some(f) = self.active_window_mut().file_open_state.as_mut() {
            f.active_section = crate::app::file_open::FileOpenSection::Files;
            f.select_index(Some(index));
        }
    }

    /// Show `dir` in the file browser.
    pub fn engine_browser_cd(&mut self, dir: std::path::PathBuf) {
        if let Some(p) = self.prompt_mut() {
            p.clear();
        }
        self.load_file_open_directory(dir);
    }

    pub fn engine_prompt_select(&mut self, index: Option<usize>) {
        if let Some(p) = self.prompt_mut() {
            p.select_suggestion(index);
        }
    }

    /// The open popups — completion, hover, code actions, plugin lists — as
    /// content. The client places them (at its caret, say).
    pub fn engine_popups(&self) -> Value {
        use crate::view::popup::{PopupContent, PopupKind};
        let popup = |p: &crate::view::popup::Popup, global: bool| {
            let kind = match p.kind {
                PopupKind::Completion => "completion",
                PopupKind::Hover => "hover",
                PopupKind::Action => "action",
                PopupKind::List => "list",
                PopupKind::Text => "text",
            };
            let content = match &p.content {
                PopupContent::List { items, selected } => json!({
                    "items": items.iter().map(|i| json!({
                        "text": i.text,
                        "detail": i.detail,
                        "icon": i.icon,
                        "disabled": i.disabled,
                    })).collect::<Vec<_>>(),
                    "selected": selected,
                }),
                PopupContent::Text(lines) => json!({
                    "lines": lines.iter().map(|l| json!([{ "t": l }])).collect::<Vec<_>>(),
                }),
                PopupContent::Markdown(styled) => json!({
                    "lines": styled.iter().map(|l| Value::Array(l.spans.iter().map(|s| {
                        let mut o = serde_json::Map::new();
                        o.insert("t".into(), json!(s.text));
                        if let Some(c) = s.style.fg.and_then(css_color) {
                            o.insert("fg".into(), json!(c));
                        }
                        if let Some(c) = s.style.bg.and_then(css_color) {
                            o.insert("bg".into(), json!(c));
                        }
                        let m = s.style.add_modifier;
                        if m.contains(ratatui::style::Modifier::BOLD) { o.insert("b".into(), json!(true)); }
                        if m.contains(ratatui::style::Modifier::ITALIC) { o.insert("i".into(), json!(true)); }
                        if m.contains(ratatui::style::Modifier::UNDERLINED) { o.insert("u".into(), json!(true)); }
                        Value::Object(o)
                    }).collect())).collect::<Vec<_>>(),
                }),
            };
            json!({
                "kind": kind,
                "title": p.title,
                "description": p.description,
                "global": global,
                "content": content,
                "at": popup_at(&p.position),
            })
        };
        let mut out: Vec<Value> = self
            .active_state()
            .popups
            .all()
            .iter()
            .map(|p| popup(p, false))
            .collect();
        // The trust prompt is a popup the terminal draws as its own modal;
        // it is asked through `engine_ask`.
        if let Some(p) = self
            .global_popups
            .top()
            .filter(|_| !self.workspace_trust_on_top())
        {
            out.push(popup(p, true));
        }
        Value::Array(out)
    }

    /// Pick the `index`th item of the top popup's list.
    pub fn engine_popup_select(&mut self, index: usize) {
        if let Some(p) = self.global_popups.top_mut() {
            p.select_index(index);
            return;
        }
        let buffer = self.active_buffer();
        if let Some(state) = self.active_window_mut().buffers.get_mut(&buffer) {
            if let Some(p) = state.popups.top_mut() {
                p.select_index(index);
            }
        }
    }

    /// The plugin panels mounted now: where the editor put each (the dock,
    /// a floating panel, a sidebar section, a pane), its widget spec and the
    /// state of its stateful widgets. Where a client shows them is its own.
    pub fn engine_panels(&self) -> Value {
        use crate::app::PanelSlot;
        let interior_json = |slot: String, i: &crate::view::shell::panel::Interior| {
            json!({
                "slot": slot,
                "spec": self.styled_spec(&i.spec),
                "instances": i.states.iter()
                    .filter_map(|(k, st)| Some((k.clone(), instance_json(st)?)))
                    .collect::<serde_json::Map<_, _>>(),
                "focusKey": i.focus_key,
                "keyboard": i.keyboard,
            })
        };
        let mut out = Vec::new();
        let mut slots = vec![(PanelSlot::Dock, "dock".to_string())];
        slots.push((PanelSlot::Floating, "floating".to_string()));
        for (i, s) in self.sidebar_sections.iter().enumerate() {
            if s.panel.is_some() {
                slots.push((PanelSlot::Sidebar(i), format!("sidebar:{i}")));
            }
        }
        for (slot, name) in slots {
            let Some(interior) = self.panel_interior(slot) else {
                continue;
            };
            let mut v = interior_json(name, &interior);
            if matches!(slot, PanelSlot::Floating) {
                if let Some(d) = self.panel_description() {
                    v["title"] = json!(d.title);
                    v["closable"] = json!(d.closable);
                    // Anchored at the cell that opened it (a plugin's menu),
                    // rather than centred.
                    v["anchored"] = json!(matches!(
                        d.spot,
                        crate::view::shell::panel::Spot::Anchored { .. }
                    ));
                }
            }
            out.push(v);
        }
        let mut panes: Vec<_> = self.pane_interiors().into_iter().collect();
        panes.sort_by_key(|(leaf, _)| leaf.0 .0);
        for (leaf, interior) in panes {
            out.push(interior_json(format!("pane:{}", leaf.0 .0), &interior));
        }
        Value::Array(out)
    }

    /// The settings: the schema, and the effective values.
    /// The live lists a settings schema names with `x-enum-from` (the
    /// `$themes` a theme setting chooses from: built-ins and user themes, as
    /// Select Theme lists them), as `{source: [{name, value}]}`.
    pub fn engine_setting_sources(&self) -> Value {
        let themes: Vec<Value> = self
            .theme_registry
            .settings_theme_options()
            .into_iter()
            .map(|(name, value)| json!({ "name": name, "value": value }))
            .collect();
        json!({ "$themes": themes })
    }

    pub fn engine_settings(&self) -> Value {
        json!({
            "values": serde_json::to_value(&self.config).unwrap_or(Value::Null),
        })
    }

    /// Change settings, as the settings screen saves them: `changes` maps a
    /// JSON pointer (`/editor/tab_size`) to its new value, written to the
    /// user's config.
    pub fn engine_set_settings(&mut self, changes: &serde_json::Map<String, Value>) {
        let was_open = self.settings_state.as_ref().is_some_and(|s| s.visible);
        if !was_open {
            self.open_settings();
        }
        if let Some(state) = self.settings_state.as_mut() {
            for (k, v) in changes {
                state.pending_changes.insert(k.clone(), v.clone());
            }
        }
        self.save_settings();
        if !was_open {
            self.close_settings(false);
        }
    }

    /// A directory's entries, through the window's filesystem (local or
    /// remote): directories first, then files, each by name.
    pub fn engine_list_dir(&self, path: &std::path::Path) -> anyhow::Result<Value> {
        let fs = &self.authority().filesystem;
        let host = crate::model::host_path::HostPathBuf::from_path_buf(path.to_path_buf());
        let mut entries = fs.read_dir(host.as_host_path())?;
        entries.sort_by(|a, b| b.is_dir().cmp(&a.is_dir()).then(a.name.cmp(&b.name)));
        Ok(Value::Array(
            entries
                .iter()
                .map(|e| json!({ "name": e.name, "dir": e.is_dir() }))
                .collect(),
        ))
    }

    /// Drop `buffer`'s tab, dragged from `source`, onto `target`: into its
    /// tab bar (`zone` "bar", before tab `index` or at the end), its middle
    /// ("center"), or one of its sides, which splits it ("left", "right",
    /// "top", "bottom") — the terminal's tab drag, as one command.
    pub fn engine_tab_drop(
        &mut self,
        buffer: BufferId,
        source: crate::model::event::LeafId,
        target: crate::model::event::LeafId,
        zone: &str,
        index: Option<usize>,
    ) -> anyhow::Result<()> {
        use crate::app::types::TabDropZone as Z;
        let zone = match zone {
            "bar" => Z::TabBar(target, index),
            "center" => Z::SplitCenter(target),
            "left" => Z::SplitLeft(target),
            "right" => Z::SplitRight(target),
            "top" => Z::SplitTop(target),
            "bottom" => Z::SplitBottom(target),
            other => anyhow::bail!("no drop zone {other:?}"),
        };
        self.execute_tab_drop(buffer, source, zone);
        Ok(())
    }

    /// A new, unsaved buffer named `name` holding `text`: a file a browser
    /// was handed without a path (dropped from the user's computer), which
    /// Save As puts somewhere.
    pub fn engine_new_buffer_with(&mut self, name: &str, text: &str) -> BufferId {
        let id = self.new_buffer();
        if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&id) {
            meta.display_name = name.to_string();
        }
        // Into the new buffer itself, not through paste: an open prompt
        // would take a paste, and the text is the buffer's to begin with
        // (unsaved; there is nothing to undo back to).
        if let Some(state) = self.buffers_mut().get_mut(&id) {
            state.buffer.insert(0, text);
            state.buffer.set_modified(!text.is_empty());
        }
        id
    }

    /// Set the split `id`'s ratio (the first child's share, 0..1).
    pub fn engine_set_split_ratio(&mut self, id: usize, ratio: f32) -> anyhow::Result<()> {
        let container = crate::model::event::ContainerId(fresh_core::SplitId(id));
        let set = self
            .active_window_mut()
            .buffers
            .splits_mut()
            .is_some_and(|(mgr, _)| mgr.set_ratio(container, ratio.clamp(0.05, 0.95)));
        if !set {
            anyhow::bail!("no split {id}");
        }
        self.relayout();
        Ok(())
    }

    /// Diagnostics across every open buffer (errors, warnings), and how many
    /// files are open.
    pub fn engine_totals(&self) -> (usize, usize, usize) {
        let ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
        let win = self.active_window();
        let (mut errors, mut warnings, mut files) = (0, 0, 0);
        for (id, state) in win.buffers.iter() {
            for o in state.overlays.in_namespace(&ns) {
                match o.priority {
                    100 => errors += 1,
                    50 => warnings += 1,
                    _ => {}
                }
            }
            if matches!(
                win.buffer_metadata.get(id).map(|m| &m.kind),
                Some(crate::app::types::BufferKind::File { .. })
            ) {
                files += 1;
            }
        }
        (errors, warnings, files)
    }

    /// The project root the client's explorer starts at.
    pub fn engine_root(&self) -> Value {
        json!(self.working_dir().to_string_lossy())
    }
}

impl Editor {
    /// A spec as JSON, with every style's colours also resolved to CSS
    /// (`fgCss`/`bgCss` beside `fg`/`bg`). A plugin colours text by theme
    /// key — `ui.help_key_fg` for a key hint, a syntax colour for a logo —
    /// and only the host knows what those keys are in the theme in use.
    pub(crate) fn styled_spec(&self, spec: &fresh_core::api::WidgetSpec) -> Value {
        let mut v = serde_json::to_value(spec).unwrap_or(Value::Null);
        let theme = self.theme();
        resolve_styles(&mut v, &theme);
        v
    }
}

/// Walk `v`, adding `fgCss`/`bgCss` to every style object whose `fg`/`bg` is
/// a theme key or an `[r, g, b]` triple.
fn resolve_styles(v: &mut Value, theme: &crate::view::theme::Theme) {
    match v {
        Value::Object(o) => {
            for name in ["style", "hoverStyle"] {
                if let Some(Value::Object(st)) = o.get_mut(name) {
                    for (from, to) in [("fg", "fgCss"), ("bg", "bgCss")] {
                        let css = match st.get(from) {
                            Some(Value::String(key)) => {
                                theme.resolve_theme_key(key).and_then(css_color)
                            }
                            Some(Value::Array(rgb)) if rgb.len() == 3 => {
                                let c = |i: usize| rgb[i].as_u64().unwrap_or(0).min(255);
                                Some(format!("#{:02x}{:02x}{:02x}", c(0), c(1), c(2)))
                            }
                            _ => None,
                        };
                        if let Some(css) = css {
                            st.insert(to.to_string(), Value::String(css));
                        }
                    }
                }
            }
            for (_, child) in o.iter_mut() {
                resolve_styles(child, theme);
            }
        }
        Value::Array(a) => a.iter_mut().for_each(|c| resolve_styles(c, theme)),
        _ => {}
    }
}

/// A widget's host-owned state, in the names the page's widget renderer
/// reads (`widgetEl`, js/65-widgets.js).
pub(crate) fn instance_json(st: &crate::widgets::WidgetInstanceState) -> Option<Value> {
    use crate::widgets::WidgetInstanceState as S;
    Some(match st {
        S::None => return None,
        S::List { selected_index } => json!({ "selectedIndex": selected_index }),
        S::Text {
            editor,
            completions,
            completion_selected_index,
            completion_navigated,
            ..
        } => {
            // The caret as a byte offset into the joined value, which is how
            // the page places it (and how a click reports it back).
            let before: usize = editor
                .lines
                .iter()
                .take(editor.cursor_row)
                .map(|l| l.len() + 1)
                .sum();
            json!({
                "textValue": editor.value(),
                "cursorByte": before + editor.cursor_col,
                "completions": completions.iter().map(|c| c.value.clone()).collect::<Vec<_>>(),
                "completionSelected": completion_selected_index,
                "completionNavigated": completion_navigated,
            })
        }
        S::Tree {
            selected_index,
            expanded_keys,
        } => json!({
            "selectedIndex": selected_index,
            "expandedKeys": expanded_keys.iter().collect::<Vec<_>>(),
        }),
        S::Number { value, .. } => json!({ "numberValue": value }),
        S::Dropdown {
            selected_index,
            open,
            highlight,
        } => json!({
            "selectedIndex": selected_index,
            "dropdownOpen": open,
            "highlight": highlight,
        }),
        S::Radio { selected_index } => json!({ "selectedIndex": selected_index }),
        S::DualList {
            included,
            active_included,
            available_cursor,
            included_cursor,
        } => json!({
            "included": included,
            "activeIncluded": active_included,
            "availableCursor": available_cursor,
            "includedCursor": included_cursor,
        }),
    })
}

pub(crate) fn css_color(c: ratatui::style::Color) -> Option<String> {
    use ratatui::style::Color;
    // The xterm 256-colour cube and ramp, so an indexed colour a theme names
    // reads the same in a browser as in a terminal.
    fn indexed(i: u8) -> (u8, u8, u8) {
        const BASE: [(u8, u8, u8); 16] = [
            (0, 0, 0),
            (205, 0, 0),
            (0, 205, 0),
            (205, 205, 0),
            (0, 0, 238),
            (205, 0, 205),
            (0, 205, 205),
            (229, 229, 229),
            (127, 127, 127),
            (255, 0, 0),
            (0, 255, 0),
            (255, 255, 0),
            (92, 92, 255),
            (255, 0, 255),
            (0, 255, 255),
            (255, 255, 255),
        ];
        match i {
            0..=15 => BASE[i as usize],
            16..=231 => {
                let i = i - 16;
                let step = |v: u8| if v == 0 { 0 } else { 55 + v * 40 };
                (step(i / 36), step((i / 6) % 6), step(i % 6))
            }
            232..=255 => {
                let v = 8 + (i - 232) * 10;
                (v, v, v)
            }
        }
    }
    let (r, g, b) = match c {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Indexed(i) => indexed(i),
        Color::Black => indexed(0),
        Color::Red => indexed(1),
        Color::Green => indexed(2),
        Color::Yellow => indexed(3),
        Color::Blue => indexed(4),
        Color::Magenta => indexed(5),
        Color::Cyan => indexed(6),
        Color::Gray => indexed(7),
        Color::DarkGray => indexed(8),
        Color::LightRed => indexed(9),
        Color::LightGreen => indexed(10),
        Color::LightYellow => indexed(11),
        Color::LightBlue => indexed(12),
        Color::LightMagenta => indexed(13),
        Color::LightCyan => indexed(14),
        Color::White => indexed(15),
        Color::Reset => return None,
    };
    Some(format!("#{r:02x}{g:02x}{b:02x}"))
}

/// Where a popup goes, for a client: at the caret (`cursor`, `below`,
/// `above`), `centered`, `bottomRight`, or above a status bar item
/// (`status:lsp`, `status:remote`, `status:readOnly`, `status:update`, …).
fn popup_at(p: &crate::view::popup::PopupPosition) -> Value {
    use crate::view::popup::PopupPosition as P;
    use crate::view::ui::status_bar::StatusBarClickable as C;
    json!(match p {
        P::AtCursor => "cursor".to_string(),
        P::BelowCursor => "below".to_string(),
        P::AboveCursor => "above".to_string(),
        P::Fixed { .. } | P::Centered | P::CenteredOverlay { .. } => "centered".to_string(),
        P::BottomRight => "bottomRight".to_string(),
        P::AboveStatusBarAt(c) => format!(
            "status:{}",
            match c {
                C::Lsp => "lsp",
                C::RemoteIndicator => "remote",
                C::ReadOnly => "readOnly",
                C::Update => "update",
                C::Encoding => "encoding",
                C::LineEnding => "lineEnding",
                C::Language => "language",
                C::Palette => "palette",
                C::Warnings => "warnings",
                C::Messages => "messages",
                C::WorkspaceTrust => "trust",
                C::RestartTerminal => "restart",
                C::SearchNext | C::SearchPrevious => "search",
            }
        ),
    })
}

/// A right-click menu the web opens: where.
pub enum WebMenu {
    /// A tab: `buffer`'s, in the pane `leaf`.
    Tab { leaf: LeafId, buffer: BufferId },
    /// The "+" of the pane `leaf`'s tab bar.
    NewTab { leaf: LeafId },
    /// An explorer entry, by its path; `dir` when it is a folder.
    Explorer { path: String, dir: bool },
}

fn entry_kind(dir: bool) -> fresh_core::api::FileExplorerEntryKind {
    if dir {
        fresh_core::api::FileExplorerEntryKind::Directory
    } else {
        fresh_core::api::FileExplorerEntryKind::File
    }
}
