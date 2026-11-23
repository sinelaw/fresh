//! The core's API for a web client that is its own application
//! (`docs/internal/web-ui.md`).
//!
//! One WebSocket per client carries JSON both ways:
//!
//! - **Requests** `{"id": n, "op": "...", ...}`, answered by a reply
//!   `{"re": n, "ok": true, ...}` or `{"re": n, "error": "..."}`. A request
//!   without an `id` gets no reply.
//! - **Events** `{"ev": "<topic>", ...}`, pushed when a topic the client sees
//!   changed: each open view's lines, the split layout, the open buffers.
//!
//! The client owns all of its UI. What it gets here is the editor: buffers
//! through views laid out at its own size, editing as commands, and the rest
//! as data. No client state lives in the editor; a client's views (its
//! size and scroll of each pane) live in its [`ApiClient`].

use std::collections::{BTreeMap, HashMap};

use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::style::{Color, Modifier};
use serde_json::{json, Map, Value};

use crate::app::client_pane::{ClientPane, ClientPaneState, PaneRender};
use crate::app::Editor;
use crate::model::event::{BufferId, LeafId, SplitId};

/// One connected client's side of the API: its views and what it was last
/// sent of each topic.
#[derive(Default)]
pub struct ApiClient {
    views: BTreeMap<u64, ClientPane>,
    next_view: u64,
    /// Fingerprint of each topic as last sent.
    sent: HashMap<String, u64>,
    /// Replies to send with the next push.
    replies: Vec<Value>,
    /// This client's views of panes in workspaces switched away from, which
    /// it closed: its scroll and cursors there, for when it opens the pane
    /// again. Dropped when the window or the pane is gone.
    away: HashMap<(fresh_core::WindowId, LeafId), ClientPane>,
    /// The editor's clipboard as last seen; a change is sent so the browser
    /// can put it on the system clipboard. Seeded on the first push, so what
    /// was copied before the client came is not replayed.
    clipboard: Option<u64>,
    /// The client's keybinding screen, while it is open: the editor's
    /// keybinding model, edited by `keys.*` and saved by `keys.save`.
    keys: Option<crate::app::keybinding_editor::KeybindingEditor>,
    /// Each view's rows as last sent, by fingerprint: a view's event carries
    /// only the rows that changed since.
    rows: HashMap<u64, Vec<u64>>,
}

type Reply = Result<Map<String, Value>, String>;

fn u(v: &Value, k: &str) -> Option<u64> {
    v.get(k).and_then(Value::as_u64)
}

fn flag(v: &Value, k: &str) -> bool {
    v.get(k).and_then(Value::as_bool).unwrap_or(false)
}

fn ok() -> Reply {
    Ok(Map::new())
}

impl ApiClient {
    /// Apply one message from the client. Its reply, if it asked for one, is
    /// sent with the next [`Self::push`].
    pub fn handle(&mut self, editor: &mut Editor, msg: &Value) {
        let op = msg.get("op").and_then(Value::as_str).unwrap_or("");
        let calibrating = editor.engine_calibration_open();
        let result = match self
            .active_view(editor)
            .filter(|_| acts_on_cursors(op, msg))
        {
            // A command that does not name a view acts where this client is:
            // its active view's cursors, not the terminal's or another
            // client's (a palette's Go to Line, a completion accepted, a
            // menu's Select All).
            Some(id) => {
                let mut pane = self.views.remove(&id).expect("an active view");
                let r = editor
                    .in_client_pane(&mut pane, |ed| self.dispatch(ed, op, msg))
                    .unwrap_or_else(|| Err("the view's pane is gone".into()));
                self.views.insert(id, pane);
                r
            }
            None => self.dispatch(editor, op, msg),
        };
        // The wizard calibrates a terminal's key encoding; a browser reports
        // keys directly, and the wizard would be a modal it cannot see.
        if !calibrating && editor.engine_calibration_open() {
            editor.engine_refuse_calibration();
        }
        let Some(id) = msg.get("id").cloned() else {
            if let Err(e) = result {
                tracing::warn!("web api {op}: {e}");
            }
            return;
        };
        self.replies.push(match result {
            Ok(mut m) => {
                m.insert("re".into(), id);
                m.insert("ok".into(), Value::Bool(true));
                Value::Object(m)
            }
            Err(e) => json!({ "re": id, "error": e }),
        });
    }

    /// This client's view of the editor's active pane, if it has one.
    fn active_view(&self, editor: &Editor) -> Option<u64> {
        let active = editor
            .active_window()
            .buffers
            .splits()
            .map(|(m, _)| m.active_split())?;
        let window = editor.active_window_id();
        self.views
            .iter()
            .find(|(_, p)| p.window == window && p.leaf == active)
            .map(|(&id, _)| id)
    }

    fn view(&mut self, msg: &Value) -> Result<&mut ClientPane, String> {
        let id = u(msg, "view").ok_or("missing view")?;
        self.views
            .get_mut(&id)
            .ok_or_else(|| format!("no view {id}"))
    }

    /// Run `f` as an edit in the message's view when it names one, else in
    /// whatever has the editor's focus.
    fn in_view(&mut self, editor: &mut Editor, msg: &Value, f: impl FnOnce(&mut Editor)) -> Reply {
        if msg.get("view").is_some() {
            let pane = self.view(msg)?;
            editor
                .in_client_pane(pane, f)
                .ok_or("the view's pane is gone")?;
        } else {
            f(editor);
        }
        ok()
    }

    fn dispatch(&mut self, editor: &mut Editor, op: &str, msg: &Value) -> Reply {
        match op {
            "view.open" => {
                let leaf = LeafId(SplitId(u(msg, "leaf").ok_or("missing leaf")? as usize));
                let w = u(msg, "width").unwrap_or(80) as u16;
                let h = u(msg, "height").unwrap_or(24) as u16;
                editor
                    .active_window()
                    .pane_buffer(leaf)
                    .ok_or("no such pane")?;
                self.next_view += 1;
                let id = self.next_view;
                // A pane this client viewed before its workspace was switched
                // away from comes back with the client's scroll and cursors.
                let window = editor.active_window_id();
                let pane = match self.away.remove(&(window, leaf)) {
                    Some(mut pane) => {
                        pane.resize(w, h);
                        pane
                    }
                    None => ClientPane::new(window, leaf, w, h),
                };
                self.views.insert(id, pane);
                let mut m = Map::new();
                m.insert("view".into(), json!(id));
                Ok(m)
            }
            "view.resize" => {
                let pane = self.view(msg)?;
                let w = u(msg, "width").unwrap_or(pane.width as u64) as u16;
                let h = u(msg, "height").unwrap_or(pane.height as u64) as u16;
                pane.resize(w, h);
                ok()
            }
            "view.close" => {
                let id = u(msg, "view").ok_or("missing view")?;
                // Closed because its workspace was switched away from: kept
                // for when the client opens that pane again.
                if let Some(pane) = self.views.remove(&id) {
                    if editor.client_pane_state(&pane) == ClientPaneState::Away {
                        self.away.insert((pane.window, pane.leaf), pane);
                    }
                }
                self.sent.remove(&format!("view:{id}"));
                self.rows.remove(&id);
                ok()
            }
            "view.scroll" => {
                let lines = msg.get("lines").and_then(Value::as_i64).unwrap_or(0) as i32;
                let pane = self.view(msg)?;
                editor.scroll_client_pane(pane, lines);
                ok()
            }
            "view.scrollto" => {
                let fraction = msg
                    .get("fraction")
                    .and_then(Value::as_f64)
                    .ok_or("missing fraction")?;
                let pane = self.view(msg)?;
                editor.scroll_client_pane_to(pane, fraction);
                ok()
            }
            "view.press" => {
                let (col, row) = (
                    u(msg, "col").unwrap_or(0) as u16,
                    u(msg, "row").unwrap_or(0) as u16,
                );
                let clicks = u(msg, "clicks").unwrap_or(1) as u8;
                let mods = modifiers(msg);
                let pane = self.view(msg)?;
                editor.press_client_pane(pane, col, row, mods, clicks);
                ok()
            }
            "view.drag" => {
                let (col, row) = (
                    u(msg, "col").unwrap_or(0) as u16,
                    u(msg, "row").unwrap_or(0) as u16,
                );
                let pane = self.view(msg)?;
                editor.drag_client_pane(pane, col, row);
                ok()
            }
            "key" => self.in_view(editor, msg, |ed| super::apply_key(ed, msg)),
            "action" => {
                let name = msg
                    .get("name")
                    .and_then(Value::as_str)
                    .ok_or("missing name")?;
                let args: HashMap<String, Value> = msg
                    .get("args")
                    .and_then(Value::as_object)
                    .map(|m| m.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
                    .unwrap_or_default();
                let action = crate::input::keybindings::Action::from_str(name, &args)
                    .ok_or_else(|| format!("no action {name:?}"))?;
                let mut err = None;
                self.in_view(editor, msg, |ed| {
                    if let Err(e) = ed.handle_action(action) {
                        err = Some(e.to_string());
                    }
                })?;
                err.map_or_else(ok, Err)
            }
            "paste" => self.in_view(editor, msg, |ed| super::apply_paste(ed, msg)),
            "split.focus" => {
                let leaf = LeafId(SplitId(u(msg, "leaf").ok_or("missing leaf")? as usize));
                editor.focus_pane(leaf);
                ok()
            }
            "tab.show" => {
                let leaf = LeafId(SplitId(u(msg, "leaf").ok_or("missing leaf")? as usize));
                let buffer = BufferId(u(msg, "buffer").ok_or("missing buffer")? as usize);
                editor.show_in_pane(leaf, buffer);
                ok()
            }
            "tab.drop" => {
                let leaf = |k: &str| u(msg, k).map(|i| LeafId(SplitId(i as usize)));
                editor
                    .engine_tab_drop(
                        BufferId(u(msg, "buffer").ok_or("missing buffer")? as usize),
                        leaf("leaf").ok_or("missing leaf")?,
                        leaf("target").ok_or("missing target")?,
                        msg.get("zone").and_then(Value::as_str).unwrap_or("center"),
                        u(msg, "index").map(|i| i as usize),
                    )
                    .map_err(|e| e.to_string())?;
                ok()
            }
            "buffer.new" => {
                let name = msg
                    .get("name")
                    .and_then(Value::as_str)
                    .unwrap_or("untitled");
                let text = msg.get("text").and_then(Value::as_str).unwrap_or("");
                let id = editor.engine_new_buffer_with(name, text);
                let mut m = Map::new();
                m.insert("buffer".into(), json!(id.0));
                Ok(m)
            }
            "split.ratio" => {
                let id = u(msg, "split").ok_or("missing split")? as usize;
                let ratio = msg
                    .get("ratio")
                    .and_then(Value::as_f64)
                    .ok_or("missing ratio")? as f32;
                editor
                    .engine_set_split_ratio(id, ratio)
                    .map_err(|e| e.to_string())?;
                ok()
            }
            "tab.close" => {
                let leaf = LeafId(SplitId(u(msg, "leaf").ok_or("missing leaf")? as usize));
                let buffer = BufferId(u(msg, "buffer").ok_or("missing buffer")? as usize);
                // As the terminal's tab ×: unsaved work is asked about first.
                editor.close_tab_in_split(buffer, leaf);
                ok()
            }
            // A right-click menu's rows, and choosing one: the terminal's
            // menus for the place (see `WebMenu`).
            "menu.rows" => {
                let at = web_menu(msg)?;
                let mut m = Map::new();
                m.insert("rows".into(), Value::Array(editor.engine_menu_rows(&at)));
                Ok(m)
            }
            "menu.choose" => {
                let at = web_menu(msg)?;
                let index = u(msg, "index").ok_or("missing index")? as usize;
                editor.engine_menu_choose(&at, index)?;
                ok()
            }
            "file.open" => {
                let path = msg
                    .get("path")
                    .and_then(Value::as_str)
                    .ok_or("missing path")?;
                let path = std::path::PathBuf::from(path);
                if !path.is_absolute() {
                    return Err("path must be absolute".into());
                }
                editor.queue_file_open(path, None, None, None, None, None, None);
                ok()
            }
            "commands.list" => {
                let mut m = Map::new();
                m.insert("commands".into(), editor.engine_commands());
                Ok(m)
            }
            "command.run" => {
                let name = msg
                    .get("name")
                    .and_then(Value::as_str)
                    .ok_or("missing name")?;
                let mut err = None;
                self.in_view(editor, msg, |ed| {
                    err = ed.engine_run_command(name).err().map(|e| e.to_string());
                })?;
                err.map_or_else(ok, Err)
            }
            "menu.run" => {
                let name = msg
                    .get("action")
                    .and_then(Value::as_str)
                    .ok_or("missing action")?;
                let args: HashMap<String, Value> = msg
                    .get("args")
                    .and_then(Value::as_object)
                    .map(|m| m.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
                    .unwrap_or_default();
                let mut err = None;
                self.in_view(editor, msg, |ed| {
                    err = ed
                        .engine_run_menu_action(name, &args)
                        .err()
                        .map(|e| e.to_string());
                })?;
                err.map_or_else(ok, Err)
            }
            "prompt.set" => {
                let text = msg.get("text").and_then(Value::as_str).unwrap_or("");
                editor.engine_prompt_set(text);
                ok()
            }
            "prompt.select" => {
                editor.engine_prompt_select(u(msg, "index").map(|i| i as usize));
                ok()
            }
            "browser.select" => {
                editor.engine_browser_select(u(msg, "index").ok_or("missing index")? as usize);
                ok()
            }
            "browser.cd" => {
                let path = msg
                    .get("path")
                    .and_then(Value::as_str)
                    .ok_or("missing path")?;
                editor.engine_browser_cd(std::path::PathBuf::from(path));
                ok()
            }
            "browser.hidden" => {
                editor.file_open_toggle_hidden();
                ok()
            }
            "prompt.confirm" => act(editor, crate::input::keybindings::Action::PromptConfirm),
            "prompt.cancel" => act(editor, crate::input::keybindings::Action::PromptCancel),
            "confirm.choose" => {
                editor.confirm_dialog_choose(u(msg, "index").ok_or("missing index")? as usize);
                ok()
            }
            "trust.select" => {
                editor.set_workspace_trust_selection(u(msg, "index").ok_or("missing index")? as usize);
                ok()
            }
            "trust.confirm" => {
                let i = editor.current_workspace_trust_selection();
                editor.confirm_workspace_trust(i);
                ok()
            }
            "trust.decline" => {
                editor.decline_workspace_trust();
                ok()
            }
            "popup.select" => {
                editor.engine_popup_select(u(msg, "index").ok_or("missing index")? as usize);
                ok()
            }
            "popup.confirm" => act(editor, crate::input::keybindings::Action::PopupConfirm),
            "popup.cancel" => act(editor, crate::input::keybindings::Action::PopupCancel),
            "panel.event" => {
                use crate::view::shell::widgets::Slot;
                let slot = msg
                    .get("slot")
                    .and_then(Value::as_str)
                    .ok_or("missing slot")?;
                let slot = match slot.split_once(':') {
                    None if slot == "dock" => Slot::Dock,
                    None if slot == "floating" => Slot::Floating,
                    Some(("sidebar", i)) => Slot::Sidebar(i.parse().map_err(|_| "bad slot")?),
                    Some(("pane", i)) => {
                        Slot::Pane(LeafId(SplitId(i.parse().map_err(|_| "bad slot")?)))
                    }
                    _ => return Err(format!("bad slot {slot:?}")),
                };
                let str_of = |k: &str| msg.get(k).and_then(Value::as_str).unwrap_or("").to_string();
                editor.press_native_widget(
                    slot,
                    crate::app::native_panels::NativeWidgetPress {
                        widget_key: str_of("widgetKey"),
                        event_type: str_of("eventType"),
                        payload: msg.get("payload").cloned().unwrap_or_else(|| json!({})),
                        text_cursor: u(msg, "textCursor").map(|b| b as usize),
                        clicks: u(msg, "clicks").unwrap_or(1).min(3) as u8,
                        at: (
                            u(msg, "col").unwrap_or(0) as u16,
                            u(msg, "row").unwrap_or(0) as u16,
                        ),
                    },
                );
                ok()
            }
            "settings.schema" => {
                let mut m = Map::new();
                let schema: Value =
                    serde_json::from_str(include_str!("../../plugins/config-schema.json"))
                        .map_err(|e| e.to_string())?;
                m.insert("schema".into(), schema);
                m.insert("sources".into(), editor.engine_setting_sources());
                Ok(m)
            }
            "settings.set" => {
                let changes = msg
                    .get("changes")
                    .and_then(Value::as_object)
                    .ok_or("missing changes")?;
                editor.engine_set_settings(changes);
                ok()
            }
            "dir.list" => {
                let path = msg
                    .get("path")
                    .and_then(Value::as_str)
                    .ok_or("missing path")?;
                let mut m = Map::new();
                m.insert(
                    "entries".into(),
                    editor
                        .engine_list_dir(std::path::Path::new(path))
                        .map_err(|e| e.to_string())?,
                );
                Ok(m)
            }
            "file.rename" => {
                let from = msg
                    .get("from")
                    .and_then(Value::as_str)
                    .ok_or("missing from")?;
                let to = msg.get("to").and_then(Value::as_str).ok_or("missing to")?;
                editor
                    .engine_rename(std::path::Path::new(from), std::path::Path::new(to))
                    .map_err(|e| e.to_string())?;
                ok()
            }
            "file.create" => {
                let path = msg
                    .get("path")
                    .and_then(Value::as_str)
                    .ok_or("missing path")?;
                editor
                    .engine_create(std::path::Path::new(path), flag(msg, "dir"))
                    .map_err(|e| e.to_string())?;
                ok()
            }
            "file.delete" => {
                let path = msg
                    .get("path")
                    .and_then(Value::as_str)
                    .ok_or("missing path")?;
                editor
                    .engine_delete(std::path::Path::new(path))
                    .map_err(|e| e.to_string())?;
                ok()
            }
            "keys.open" => {
                self.keys = Some(editor.keybinding_model());
                ok()
            }
            "keys.set" => {
                let keys = self
                    .keys
                    .as_mut()
                    .ok_or("the keybinding screen is not open")?;
                let (code, mods) =
                    super::key_of(msg.get("key").ok_or("missing key")?).ok_or("not a key")?;
                let action = msg
                    .get("action")
                    .and_then(Value::as_str)
                    .ok_or("missing action")?;
                let context = msg
                    .get("context")
                    .and_then(Value::as_str)
                    .unwrap_or("normal");
                match u(msg, "index") {
                    Some(i) => {
                        if !keys.select_binding(i as usize) {
                            return Err(format!("no binding {i}"));
                        }
                        keys.open_edit_dialog();
                    }
                    None => keys.open_add_dialog(),
                }
                let d = keys.edit_dialog.as_mut().ok_or("cannot edit this row")?;
                d.key_code = Some(code);
                d.modifiers = mods;
                d.chord_keys.clear();
                d.key_display = crate::input::keybindings::format_keybinding(&code, &mods);
                d.action_text = action.to_string();
                d.context = context.to_string();
                match keys.apply_edit_dialog() {
                    Some(err) => {
                        keys.edit_dialog = None;
                        Err(err)
                    }
                    None => ok(),
                }
            }
            "keys.delete" | "keys.disable" => {
                let keys = self
                    .keys
                    .as_mut()
                    .ok_or("the keybinding screen is not open")?;
                let i = u(msg, "index").ok_or("missing index")? as usize;
                if !keys.select_binding(i) {
                    return Err(format!("no binding {i}"));
                }
                use crate::app::keybinding_editor::DeleteResult as D;
                match if op == "keys.delete" {
                    keys.delete_selected()
                } else {
                    keys.disable_selected()
                } {
                    D::CannotDelete | D::NothingSelected => {
                        Err("this binding cannot be changed".into())
                    }
                    _ => ok(),
                }
            }
            "keys.conflicts" => {
                let keys = self
                    .keys
                    .as_ref()
                    .ok_or("the keybinding screen is not open")?;
                let (code, mods) =
                    super::key_of(msg.get("key").ok_or("missing key")?).ok_or("not a key")?;
                let context = msg
                    .get("context")
                    .and_then(Value::as_str)
                    .unwrap_or("normal");
                let mut m = Map::new();
                m.insert(
                    "conflicts".into(),
                    json!(keys.find_conflicts(code, mods, context)),
                );
                m.insert(
                    "display".into(),
                    json!(crate::input::keybindings::format_keybinding(&code, &mods)),
                );
                Ok(m)
            }
            "keys.save" => {
                let keys = self
                    .keys
                    .take()
                    .ok_or("the keybinding screen is not open")?;
                editor.save_keybinding_editor_changes(&keys);
                ok()
            }
            "keys.close" => {
                self.keys = None;
                ok()
            }
            "skin.theme" => {
                super::skins::apply_skin_theme(editor, msg)?;
                ok()
            }
            "quit" => {
                super::apply_quit(editor);
                ok()
            }
            _ => Err(format!("unknown op {op:?}")),
        }
    }

    /// Everything to send the client now: the replies queued since the last
    /// push, then each topic that changed since it was last sent.
    pub fn push(&mut self, editor: &mut Editor) -> Vec<Value> {
        let mut out = std::mem::take(&mut self.replies);
        fn topic(sent: &mut HashMap<String, u64>, out: &mut Vec<Value>, key: String, ev: Value) {
            let fp = super::value_fingerprint(&ev);
            if sent.get(&key) != Some(&fp) {
                sent.insert(key, fp);
                out.push(ev);
            }
        }
        // The build first: a client from another build reloads before it
        // reads anything else.
        topic(
            &mut self.sent,
            &mut out,
            "build".into(),
            json!({ "ev": "build", "build": super::PAGE_BUILD }),
        );
        // The project root: the active window's, so it changes when the
        // Orchestrator switches workspaces, and the explorer follows.
        topic(
            &mut self.sent,
            &mut out,
            "root".into(),
            json!({ "ev": "root", "root": editor.engine_root() }),
        );
        topic(
            &mut self.sent,
            &mut out,
            "layout".into(),
            json!({ "ev": "layout", "layout": editor.engine_layout() }),
        );
        topic(
            &mut self.sent,
            &mut out,
            "buffers".into(),
            json!({ "ev": "buffers", "buffers": editor.engine_buffers() }),
        );
        let clip = editor.clipboard_text();
        let h = {
            use std::hash::{Hash, Hasher};
            let mut h = std::collections::hash_map::DefaultHasher::new();
            clip.hash(&mut h);
            h.finish()
        };
        if self.clipboard.is_some_and(|last| last != h) {
            out.push(json!({ "ev": "clipboard", "text": clip }));
        }
        self.clipboard = Some(h);
        // The status is this client's: its active view's cursor.
        let status = match self.active_view(editor) {
            Some(id) => {
                let mut pane = self.views.remove(&id).expect("an active view");
                let s = editor
                    .with_client_pane(&mut pane, |ed| ed.engine_status())
                    .unwrap_or_else(|| editor.engine_status());
                self.views.insert(id, pane);
                s
            }
            None => editor.engine_status(),
        };
        for (name, value) in [
            ("status", status),
            ("menus", editor.engine_menus()),
            ("ask", editor.engine_ask()),
            ("popups", editor.engine_popups()),
            ("panels", editor.engine_panels()),
            ("settings", editor.engine_settings()),
            ("theme", theme(editor)),
            ("skinValues", super::skins::skin_values(editor)),
        ] {
            let mut ev = Map::new();
            ev.insert("ev".into(), json!(name));
            ev.insert(name.into(), value);
            topic(&mut self.sent, &mut out, name.into(), Value::Object(ev));
        }
        let keys = self.keys.as_ref().map(keybindings_json);
        topic(
            &mut self.sent,
            &mut out,
            "keybindings".into(),
            json!({ "ev": "keybindings", "keybindings": keys }),
        );
        let mut gone = Vec::new();
        // Kept until the client opens the pane again, whether its workspace
        // is still away or active again; dropped once the pane is gone.
        self.away
            .retain(|_, pane| editor.client_pane_state(pane) != ClientPaneState::Gone);
        for (&id, pane) in self.views.iter_mut() {
            match editor.client_pane_state(pane) {
                ClientPaneState::Shown => {}
                // Not drawn while its workspace is away; the client closes it
                // once it has the new layout.
                ClientPaneState::Away => continue,
                ClientPaneState::Gone => {
                    gone.push(id);
                    continue;
                }
            }
            match editor.render_client_pane(pane) {
                Some(r) => {
                    let sent_rows = self.rows.entry(id).or_default();
                    if let Some(ev) = view_event(id, &r, &mut self.sent, sent_rows) {
                        out.push(ev);
                    }
                }
                None => gone.push(id),
            }
        }
        for id in gone {
            self.views.remove(&id);
            self.sent.remove(&format!("view:{id}"));
            self.rows.remove(&id);
            out.push(json!({ "ev": "view.closed", "view": id }));
        }
        out
    }
}

/// The keybinding model as the client's screen shows it: every binding with
/// its index (what `keys.*` name it by), and what an edit may use.
fn keybindings_json(k: &crate::app::keybinding_editor::KeybindingEditor) -> Value {
    use crate::app::keybinding_editor::BindingSource as S;
    let mut contexts = crate::app::keybinding_editor::EditBindingState::base_context_options();
    contexts.extend(k.mode_contexts.iter().cloned());
    json!({
        "keymap": k.active_keymap,
        "changed": k.has_changes,
        "contexts": contexts,
        "actions": k.available_actions,
        "bindings": k.bindings.iter().enumerate().map(|(i, b)| json!({
            "index": i,
            "keys": b.key_display,
            "action": b.action,
            "label": b.action_display,
            "context": b.context,
            "source": match b.source {
                S::Keymap => "keymap",
                S::Custom => "custom",
                S::Plugin => "plugin",
                S::Unbound => "unbound",
            },
        })).collect::<Vec<_>>(),
    })
}

/// Whether `op` acts on the cursors where the client is: every editing
/// command that does not name a view itself. Views, the keybinding screen,
/// settings and files do not.
/// Where a `menu.*` request's menu is: `{at: "tab", leaf, buffer}`,
/// `{at: "newTab", leaf}` or `{at: "explorer", path, dir}`.
fn web_menu(msg: &Value) -> Result<crate::app::WebMenu, String> {
    use crate::app::WebMenu;
    let leaf = || {
        u(msg, "leaf")
            .map(|l| LeafId(SplitId(l as usize)))
            .ok_or("missing leaf")
    };
    Ok(match msg.get("at").and_then(Value::as_str) {
        Some("tab") => WebMenu::Tab {
            leaf: leaf()?,
            buffer: BufferId(u(msg, "buffer").ok_or("missing buffer")? as usize),
        },
        Some("newTab") => WebMenu::NewTab { leaf: leaf()? },
        Some("explorer") => WebMenu::Explorer {
            path: msg
                .get("path")
                .and_then(Value::as_str)
                .ok_or("missing path")?
                .to_string(),
            dir: msg.get("dir").and_then(Value::as_bool).unwrap_or(false),
        },
        _ => return Err("at must be tab, newTab or explorer".into()),
    })
}

fn acts_on_cursors(op: &str, msg: &Value) -> bool {
    msg.get("view").is_none()
        && !op.starts_with("view.")
        && !op.starts_with("keys.")
        && !op.starts_with("settings.")
        && !matches!(
            op,
            "commands.list" | "dir.list" | "file.rename" | "file.create" | "file.delete" | "quit"
        )
}

fn act(editor: &mut Editor, action: crate::input::keybindings::Action) -> Reply {
    editor.handle_action(action).map_err(|e| e.to_string())?;
    ok()
}

fn modifiers(msg: &Value) -> KeyModifiers {
    let mut m = KeyModifiers::empty();
    if flag(msg, "shift") {
        m |= KeyModifiers::SHIFT;
    }
    if flag(msg, "ctrl") {
        m |= KeyModifiers::CONTROL;
    }
    if flag(msg, "alt") {
        m |= KeyModifiers::ALT;
    }
    m
}

/// The colours a client themes itself from: the editor theme's named
/// colours, as CSS.
fn theme(editor: &Editor) -> Value {
    let t = editor.theme();
    let c = |c: Color| super::color_css(c);
    json!({
        "name": t.name,
        "bg": c(t.editor_bg),
        "fg": c(t.editor_fg),
        "accent": c(t.cursor),
        "muted": c(t.line_number_fg),
        "selectionBg": c(t.selection_bg),
        "menuBg": c(t.menu_bg),
        "menuFg": c(t.menu_fg),
        "menuHighlightBg": c(t.menu_highlight_bg),
        "menuHighlightFg": c(t.menu_highlight_fg),
        "popupBg": c(t.popup_bg),
        "popupFg": c(t.popup_text_fg),
        "border": c(t.popup_border_fg),
        "statusBg": c(t.status_bar_bg),
        "statusFg": c(t.status_bar_fg),
        "tabActiveBg": c(t.tab_active_bg),
        "tabInactiveBg": c(t.tab_inactive_bg),
        "tabActiveFg": c(t.tab_active_fg),
        "tabInactiveFg": c(t.tab_inactive_fg),
        "error": c(t.diagnostic_error_fg),
        "warning": c(t.diagnostic_warning_fg),
        "scrollbarThumb": c(t.scrollbar_thumb_fg),
    })
}

/// A view's event, or `None` when nothing in it changed: its facts (size,
/// caret, where its window is) and only the visual rows that changed since
/// the client was last sent them, as `[row, runs]` pairs; `count` is how many
/// rows the view has, so the client drops any past it. A row is runs of cells
/// that share a style, each with the column it starts at.
fn view_event(
    id: u64,
    r: &PaneRender,
    sent: &mut HashMap<String, u64>,
    sent_rows: &mut Vec<u64>,
) -> Option<Value> {
    let count = r.cells.area.height as usize;
    let drawn = drawn_rows(&r.cells);
    let mut rows = Vec::new();
    let mut prints = Vec::with_capacity(count);
    for (y, cells) in drawn.iter().enumerate() {
        let row = runs(cells, r.cells.area.width);
        let fp = super::value_fingerprint(&row);
        if sent_rows.get(y) != Some(&fp) {
            rows.push(json!([y, row]));
        }
        prints.push(fp);
    }
    *sent_rows = prints;
    let facts = json!({
        "buffer": r.buffer.0,
        "width": r.cells.area.width,
        "count": count,
        "gutter": r.gutter,
        "caret": r.caret.map(|(c, row)| json!([c, row])),
        "top": r.top_byte,
        "len": r.len,
        "topLine": r.top_line,
        "selection": r.selection,
        "lines": r.lines,
    });
    let key = format!("view:{id}");
    let fp = super::value_fingerprint(&facts);
    if rows.is_empty() && sent.get(&key) == Some(&fp) {
        return None;
    }
    sent.insert(key, fp);
    let mut ev = facts;
    ev["ev"] = json!("view");
    ev["view"] = json!(id);
    ev["rows"] = Value::Array(rows);
    Some(ev)
}

/// Each row's cells as a terminal backend draws them: every cell it writes,
/// with its column, in column order. A cell it does not write is covered by
/// the cell before it (the second column of a wide character). This is
/// `Buffer::diff`, the contract every terminal output goes through, taken
/// against a buffer no cell can equal — so the columns a cell covers are the
/// backend's to say, not this code's.
fn drawn_rows(buf: &Buffer) -> Vec<Vec<(u16, &ratatui::buffer::Cell)>> {
    let area = buf.area;
    let mut never = ratatui::buffer::Cell::default();
    never.set_symbol("\u{0}");
    let blank = Buffer::filled(area, never);
    let mut rows: Vec<Vec<(u16, &ratatui::buffer::Cell)>> = vec![Vec::new(); area.height as usize];
    for (x, y, cell) in blank.diff(buf) {
        rows[(y - area.y) as usize].push((x - area.x, cell));
    }
    rows
}

/// A row as runs of cells that share a style. A run is `{x, t, …style}`:
/// `t` holds one entry per column from `x` — the cell's grapheme where a
/// cell is drawn, `""` for each column it covers — so a run is exactly as
/// many columns wide as `t` is long, and the client draws each grapheme at
/// its own column.
fn runs(cells: &[(u16, &ratatui::buffer::Cell)], width: u16) -> Value {
    let mut out: Vec<Value> = Vec::new();
    let mut cur: Option<(u16, Vec<String>, (Color, Color, Modifier))> = None;
    for (i, &(x, cell)) in cells.iter().enumerate() {
        let end = cells.get(i + 1).map_or(width, |&(next, _)| next);
        let style = (cell.fg, cell.bg, cell.modifier);
        if cur.as_ref().is_none_or(|(_, _, s)| *s != style) {
            if let Some(done) = cur.take() {
                out.push(span(done));
            }
            cur = Some((x, Vec::new(), style));
        }
        if let Some((_, t, _)) = cur.as_mut() {
            t.push(cell.symbol().to_string());
            t.extend((x + 1..end).map(|_| String::new()));
        }
    }
    if let Some(done) = cur.take() {
        out.push(span(done));
    }
    Value::Array(out)
}

fn span((x, text, (fg, bg, m)): (u16, Vec<String>, (Color, Color, Modifier))) -> Value {
    let mut o = Map::new();
    o.insert("x".into(), json!(x));
    o.insert("t".into(), json!(text));
    if let Some(c) = super::color_css(fg) {
        o.insert("fg".into(), json!(c));
    }
    if let Some(c) = super::color_css(bg) {
        o.insert("bg".into(), json!(c));
    }
    for (bit, k) in [
        (Modifier::BOLD, "b"),
        (Modifier::ITALIC, "i"),
        (Modifier::UNDERLINED, "u"),
        (Modifier::REVERSED, "r"),
        (Modifier::DIM, "d"),
        (Modifier::CROSSED_OUT, "s"),
    ] {
        if m.contains(bit) {
            o.insert(k.into(), Value::Bool(true));
        }
    }
    Value::Object(o)
}

#[cfg(test)]
mod tests {
    use super::ApiClient;
    use serde_json::{json, Value};

    /// An editor on one file, its config in a throwaway directory so what a
    /// test saves never reaches the user's.
    fn editor_with(text: &str) -> (tempfile::TempDir, crate::app::Editor) {
        let dir = tempfile::tempdir().expect("temp dir");
        let file = dir.path().join("a.txt");
        std::fs::write(&file, text).expect("write");
        let dirs = crate::config_io::DirectoryContext::for_testing(&dir.path().join(".cfg"));
        let mut cfg = crate::config::Config::default();
        cfg.editor.animations = false;
        let mut ed = crate::app::Editor::with_working_dir(
            cfg,
            100,
            30,
            Some(dir.path().to_path_buf()),
            dirs,
            false,
            crate::view::color_support::ColorCapability::TrueColor,
            std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
        )
        .expect("build editor");
        ed.open_file(&file).expect("open");
        (dir, ed)
    }

    /// Send `msg` with an id; its reply, and everything else that push sent.
    fn call_all(c: &mut ApiClient, ed: &mut crate::app::Editor, msg: Value) -> (Value, Vec<Value>) {
        let mut m = msg;
        m["id"] = json!(1);
        c.handle(ed, &m);
        let out = c.push(ed);
        let reply = out
            .iter()
            .find(|v| v.get("re") == Some(&json!(1)))
            .cloned()
            .expect("a reply");
        (reply, out)
    }

    fn call(c: &mut ApiClient, ed: &mut crate::app::Editor, msg: Value) -> Value {
        call_all(c, ed, msg).0
    }

    fn event(out: &[Value], ev: &str) -> Option<Value> {
        out.iter().rev().find(|v| v["ev"] == ev).cloned()
    }

    /// The rows a view event carries, by row; a row it did not carry (it
    /// had not changed) is empty.
    fn text_of(view: &Value) -> Vec<String> {
        let mut out = vec![String::new(); view["count"].as_u64().unwrap() as usize];
        for r in view["rows"].as_array().unwrap() {
            out[r[0].as_u64().unwrap() as usize] = r[1]
                .as_array()
                .unwrap()
                .iter()
                .flat_map(|s| s["t"].as_array().unwrap().iter())
                .map(|g| g.as_str().unwrap())
                .collect::<String>();
        }
        out
    }

    fn open_view(c: &mut ApiClient, ed: &mut crate::app::Editor, w: u64, h: u64) -> (u64, Value) {
        let leaf = ed.engine_layout()["active"].clone();
        let (r, out) = call_all(
            c,
            ed,
            json!({"op": "view.open", "leaf": leaf, "width": w, "height": h}),
        );
        let id = r["view"].as_u64().expect("view id");
        let view = out
            .into_iter()
            .rev()
            .find(|v| v["ev"] == "view" && v["view"] == id)
            .expect("the view's lines");
        (id, view)
    }

    #[test]
    fn a_view_is_laid_out_at_the_clients_width() {
        let long = "word ".repeat(30);
        let (_d, mut ed) = editor_with(&format!("{long}\nsecond\n"));
        let mut c = ApiClient::default();
        let (_, view) = open_view(&mut c, &mut ed, 40, 10);
        assert_eq!(view["width"], 40);
        let rows = text_of(&view);
        let second = rows
            .iter()
            .position(|r| r.contains("second"))
            .expect("second line shown");
        assert!(
            second >= 3,
            "the long line wraps over several rows at 40 columns: {rows:?}"
        );
        assert!(rows.iter().all(|r| r.chars().count() <= 40));
    }

    #[test]
    fn keys_in_a_view_edit_the_buffer() {
        let (_d, mut ed) = editor_with("hello\n");
        let mut c = ApiClient::default();
        let (id, _) = open_view(&mut c, &mut ed, 40, 6);
        for k in ["a", "b", "c"] {
            c.handle(&mut ed, &json!({"op": "key", "view": id, "key": k}));
        }
        let out = c.push(&mut ed);
        let view = event(&out, "view").expect("lines changed");
        assert!(
            text_of(&view)[0].contains("abchello"),
            "{:?}",
            text_of(&view)
        );
        let buffers = event(&out, "buffers").expect("buffer state changed");
        assert!(buffers["buffers"]
            .as_array()
            .unwrap()
            .iter()
            .any(|b| b["modified"] == true));
    }

    #[test]
    fn a_keystroke_sends_only_the_rows_it_changed() {
        let text: String = (1..=30).map(|i| format!("line {i}\n")).collect();
        let (_d, mut ed) = editor_with(&text);
        let mut c = ApiClient::default();
        let (id, first) = open_view(&mut c, &mut ed, 40, 10);
        assert_eq!(
            first["rows"].as_array().unwrap().len(),
            10,
            "the first event has every row"
        );
        c.handle(&mut ed, &json!({"op": "key", "view": id, "key": "x"}));
        let ev = event(&c.push(&mut ed), "view").expect("the edit is sent");
        let rows: Vec<u64> = ev["rows"]
            .as_array()
            .unwrap()
            .iter()
            .map(|r| r[0].as_u64().unwrap())
            .collect();
        assert!(rows.contains(&0), "the edited row: {rows:?}");
        assert!(
            rows.len() <= 2,
            "only the edited row (and the one the caret left): {rows:?}"
        );
        assert!(
            event(&c.push(&mut ed), "view").is_none(),
            "nothing changed, nothing sent"
        );
    }

    #[test]
    fn each_client_types_at_its_own_cursor() {
        let (_d, mut ed) = editor_with("one\ntwo\nthree\n");
        let (mut a, mut b) = (ApiClient::default(), ApiClient::default());
        let (va, _) = open_view(&mut a, &mut ed, 40, 6);
        let (vb, first) = open_view(&mut b, &mut ed, 40, 6);
        let g = first["gutter"].as_u64().unwrap();
        // B puts its caret at the start of "three"; A's stays on "one".
        b.handle(
            &mut ed,
            &json!({"op": "view.press", "view": vb, "col": g, "row": 2}),
        );
        a.handle(&mut ed, &json!({"op": "key", "view": va, "key": "A"}));
        b.handle(&mut ed, &json!({"op": "key", "view": vb, "key": "B"}));
        // A line typed above B's caret by A moves B's caret along with it.
        a.handle(&mut ed, &json!({"op": "key", "view": va, "key": "Enter"}));
        b.handle(&mut ed, &json!({"op": "key", "view": vb, "key": "C"}));
        let text = ed.active_state().buffer.to_string().unwrap();
        assert_eq!(text, "A\none\ntwo\nBCthree\n");
    }

    #[test]
    fn a_command_without_a_view_acts_at_the_clients_cursor() {
        let (_d, mut ed) = editor_with("one\ntwo\nthree\n");
        let (mut a, mut b) = (ApiClient::default(), ApiClient::default());
        let (_va, _) = open_view(&mut a, &mut ed, 40, 6);
        let (vb, first) = open_view(&mut b, &mut ed, 40, 6);
        let g = first["gutter"].as_u64().unwrap();
        b.handle(
            &mut ed,
            &json!({"op": "view.press", "view": vb, "col": g, "row": 1}),
        );
        // A menu command with no view acts where B is.
        b.handle(
            &mut ed,
            &json!({"op": "menu.run", "action": "move_line_end"}),
        );
        b.handle(&mut ed, &json!({"op": "paste", "text": "!"}));
        let text = ed.active_state().buffer.to_string().unwrap();
        assert_eq!(text, "one\ntwo!\nthree\n");
        let status = event(&b.push(&mut ed), "status").expect("B's status")["status"].clone();
        assert_eq!(status["line"], 2, "B's status shows B's cursor");
    }

    #[test]
    fn each_client_scrolls_its_own_view() {
        let text: String = (1..=200).map(|i| format!("line {i}\n")).collect();
        let (_d, mut ed) = editor_with(&text);
        let (mut a, mut b) = (ApiClient::default(), ApiClient::default());
        let (va, _) = open_view(&mut a, &mut ed, 40, 10);
        let (_, before) = open_view(&mut b, &mut ed, 40, 10);
        a.handle(
            &mut ed,
            &json!({"op": "view.scroll", "view": va, "lines": 50}),
        );
        let scrolled = event(&a.push(&mut ed), "view").expect("a's view moved");
        assert!(
            text_of(&scrolled)[0].contains("line 51"),
            "{:?}",
            text_of(&scrolled)
        );
        let out = b.push(&mut ed);
        let after = event(&out, "view").unwrap_or(before);
        assert!(
            text_of(&after)[0].contains("line 1"),
            "b's view did not move"
        );
    }

    #[test]
    fn a_prompt_is_asked_and_answered() {
        let (_d, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        c.push(&mut ed);
        c.handle(&mut ed, &json!({"op": "action", "name": "command_palette"}));
        let ask = event(&c.push(&mut ed), "ask").expect("the palette asks");
        assert_eq!(ask["ask"]["kind"], "prompt");
        c.handle(
            &mut ed,
            &json!({"op": "prompt.set", "text": ">split vertical"}),
        );
        let ask = event(&c.push(&mut ed), "ask").expect("suggestions follow the text");
        assert!(ask["ask"]["suggestions"][0]["text"]
            .as_str()
            .unwrap()
            .contains("Split"));
        c.handle(&mut ed, &json!({"op": "prompt.confirm"}));
        let out = c.push(&mut ed);
        assert_eq!(event(&out, "ask").unwrap()["ask"], Value::Null);
        let layout = event(&out, "layout").expect("the split shows in the layout");
        assert!(layout["layout"]["root"]["a"].is_object(), "{layout}");
    }

    #[test]
    fn quit_stops_a_clean_editor_and_asks_about_unsaved_work() {
        let (_d, mut clean) = editor_with("unchanged\n");
        let mut c = ApiClient::default();
        call(&mut c, &mut clean, json!({"op": "quit"}));
        assert!(clean.should_quit(), "a clean editor quits at once");
        drop(clean);

        let (_d2, mut dirty) = editor_with("unchanged\n");
        let mut c = ApiClient::default();
        let (id, _) = open_view(&mut c, &mut dirty, 40, 5);
        c.handle(&mut dirty, &json!({"op": "key", "view": id, "key": "x"}));
        call(&mut c, &mut dirty, json!({"op": "quit"}));
        assert!(!dirty.should_quit(), "unsaved work must be asked about");
        assert_eq!(dirty.engine_ask()["kind"], "confirm");
    }

    #[test]
    fn the_trust_prompt_is_asked_and_a_selection_is_not_consent() {
        let (_d, mut ed) = editor_with("");
        let mut c = ApiClient::default();
        ed.show_workspace_trust_popup(true);
        let ask = event(&c.push(&mut ed), "ask").expect("asked")["ask"].clone();
        assert_eq!(ask["kind"], "trust");
        assert_eq!(ask["options"].as_array().unwrap().len(), 3);
        c.handle(&mut ed, &json!({"op": "trust.select", "index": 2}));
        let ask = event(&c.push(&mut ed), "ask").expect("selection moved")["ask"].clone();
        assert_eq!(
            (ask["kind"].clone(), ask["selected"].clone()),
            (json!("trust"), json!(2))
        );
        assert!(event(&c.push(&mut ed), "popups").is_none_or(|p| p["popups"] == json!([])));
        c.handle(&mut ed, &json!({"op": "trust.decline"}));
        assert_eq!(
            event(&c.push(&mut ed), "ask").expect("answered")["ask"],
            Value::Null
        );
        assert!(!ed.should_quit(), "a cancellable prompt only closes");
    }

    #[test]
    fn a_binding_added_on_the_shortcuts_screen_works_once_saved() {
        let (_d, mut ed) = editor_with("hello\n");
        let mut c = ApiClient::default();
        call(&mut c, &mut ed, json!({"op": "keys.open"}));
        let key = json!({"key": "F7", "code": "F7"});
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "keys.set", "key": key, "action": "move_line_end", "context": "normal"}),
        );
        assert_eq!(r["ok"], true, "{r}");
        let out = c.push(&mut ed);
        let kb = event(&out, "keybindings").map(|e| e["keybindings"].clone());
        if let Some(kb) = kb {
            assert_eq!(kb["changed"], true);
        }
        let (_, out) = call_all(&mut c, &mut ed, json!({"op": "keys.save"}));
        assert_eq!(
            event(&out, "keybindings").unwrap()["keybindings"],
            Value::Null
        );
        let saved = std::fs::read_to_string(ed.dir_context().config_path()).expect("saved");
        assert!(saved.contains("move_line_end"), "{saved}");
        let (id, _) = open_view(&mut c, &mut ed, 40, 5);
        c.handle(
            &mut ed,
            &json!({"op": "key", "view": id, "key": "F7", "code": "F7"}),
        );
        c.handle(&mut ed, &json!({"op": "key", "view": id, "key": "!"}));
        let view = event(&c.push(&mut ed), "view").expect("edited");
        assert!(
            text_of(&view)[0].contains("hello!"),
            "F7 went to the line end: {:?}",
            text_of(&view)
        );
    }

    #[test]
    fn open_file_asks_with_the_directory_it_browses() {
        let (d, mut ed) = editor_with("x\n");
        std::fs::write(d.path().join("other.txt"), "y").unwrap();
        let mut c = ApiClient::default();
        c.push(&mut ed);
        c.handle(&mut ed, &json!({"op": "action", "name": "open"}));
        // The listing loads asynchronously.
        let mut browser = Value::Null;
        for _ in 0..200 {
            crate::app::editor_tick(&mut ed, || Ok(())).ok();
            if let Some(a) = event(&c.push(&mut ed), "ask") {
                browser = a["ask"]["browser"].clone();
            }
            if browser["entries"]
                .as_array()
                .is_some_and(|e| e.iter().any(|e| e["name"] == "other.txt"))
            {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        let entries = browser["entries"].as_array().expect("a listing");
        let i = entries
            .iter()
            .position(|e| e["name"] == "other.txt")
            .expect("other.txt listed");
        let index = browser["indices"][i].clone();
        c.handle(&mut ed, &json!({"op": "browser.select", "index": index}));
        c.handle(&mut ed, &json!({"op": "key", "key": "Enter"}));
        for _ in 0..200 {
            crate::app::editor_tick(&mut ed, || Ok(())).ok();
            if ed
                .engine_buffers()
                .as_array()
                .unwrap()
                .iter()
                .any(|b| b["name"].as_str().unwrap_or("").ends_with("other.txt"))
            {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        panic!("other.txt did not open");
    }

    #[cfg(unix)]
    #[test]
    fn a_terminal_in_a_view_is_sized_to_it_and_drawn_in_it() {
        let (_d, mut ed) = editor_with("");
        let mut c = ApiClient::default();
        c.handle(&mut ed, &json!({"op": "action", "name": "open_terminal"}));
        let (id, _) = open_view(&mut c, &mut ed, 50, 8);
        for ch in "echo $COLUMNS-x-$LINES".chars() {
            c.handle(
                &mut ed,
                &json!({"op": "key", "view": id, "key": ch.to_string()}),
            );
        }
        c.handle(&mut ed, &json!({"op": "key", "view": id, "key": "Enter"}));
        let mut last = Vec::new();
        for _ in 0..500 {
            crate::app::editor_tick(&mut ed, || Ok(())).ok();
            if let Some(v) = event(&c.push(&mut ed), "view") {
                last = text_of(&v);
            }
            if last.iter().any(|l| l.trim_end() == "50-x-8") {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        panic!("the shell did not report the view's size: {last:?}");
    }

    #[test]
    fn a_tab_dropped_on_an_edge_splits_and_the_split_takes_a_ratio() {
        let (d, mut ed) = editor_with("one\n");
        std::fs::write(d.path().join("b.txt"), "two\n").unwrap();
        ed.open_file(&d.path().join("b.txt")).unwrap();
        let mut c = ApiClient::default();
        let layout = ed.engine_layout();
        let leaf = layout["active"].clone();
        let buffer = layout["root"]["buffer"].clone();
        call(
            &mut c,
            &mut ed,
            json!({"op": "tab.drop", "buffer": buffer, "leaf": leaf, "target": leaf, "zone": "right"}),
        );
        let root = ed.engine_layout()["root"].clone();
        assert_eq!(root["stack"], false, "split side by side: {root}");
        let split = root["id"].clone();
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "split.ratio", "split": split, "ratio": 0.3}),
        );
        assert_eq!(r["ok"], true, "{r}");
        let ratio = ed.engine_layout()["root"]["ratio"].as_f64().unwrap();
        assert!((ratio - 0.3).abs() < 1e-6, "{ratio}");
    }

    #[test]
    fn the_scrollbar_moves_a_view_by_line() {
        let text: String = (0..1000).map(|i| format!("line {i}\n")).collect();
        let (_d, mut ed) = editor_with(&text);
        let mut c = ApiClient::default();
        let (id, first) = open_view(&mut c, &mut ed, 40, 10);
        assert_eq!(first["lines"], 1001);
        assert_eq!(first["topLine"], 0);
        c.handle(
            &mut ed,
            &json!({"op": "view.scrollto", "view": id, "fraction": 0.5}),
        );
        let ev = event(&c.push(&mut ed), "view").expect("moved");
        assert!(
            (499..=501).contains(&ev["topLine"].as_u64().unwrap()),
            "{}",
            ev["topLine"]
        );
        assert!(text_of(&ev)[0].contains("line 50"), "{:?}", text_of(&ev));
    }

    #[test]
    fn a_dropped_file_becomes_a_named_unsaved_buffer() {
        let (_d, mut ed) = editor_with("");
        let mut c = ApiClient::default();
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "buffer.new", "name": "notes.md", "text": "# hi\n"}),
        );
        let id = r["buffer"].as_u64().unwrap();
        let b = ed
            .engine_buffers()
            .as_array()
            .unwrap()
            .iter()
            .find(|b| b["id"] == id)
            .cloned()
            .expect("listed");
        assert_eq!(b["name"], "notes.md");
        assert_eq!(b["modified"], true);
        assert_eq!(ed.active_state().buffer.to_string().unwrap(), "# hi\n");
    }

    /// A row is the cells a terminal draws, one entry per column: a
    /// grapheme where one is drawn, `""` for a column the one before covers.
    /// So every grapheme's column is the core's, whatever its width.
    #[test]
    fn a_row_has_one_entry_per_column() {
        let (_d, mut ed) = editor_with("日本x👍🏽é│\n");
        let mut c = ApiClient::default();
        let (_, view) = open_view(&mut c, &mut ed, 40, 3);
        let g = view["gutter"].as_u64().unwrap() as usize;
        let mut columns: Vec<String> = Vec::new();
        for run in view["rows"][0][1].as_array().unwrap() {
            let x = run["x"].as_u64().unwrap() as usize;
            assert_eq!(x, columns.len(), "runs tile the row");
            columns.extend(
                run["t"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|t| t.as_str().unwrap().to_string()),
            );
        }
        assert_eq!(columns.len(), 40, "every column is accounted for");
        assert_eq!(
            &columns[g..g + 9],
            ["日", "", "本", "", "x", "👍🏽", "", "é", "│"]
                .map(String::from)
                .as_slice()
        );
    }

    #[test]
    fn calibration_is_not_opened_for_a_browser() {
        let (_d, mut ed) = editor_with("");
        let mut c = ApiClient::default();
        let menus = ed.engine_menus().to_string();
        assert!(!menus.contains("calibrate_input"));
        c.handle(&mut ed, &json!({"op": "action", "name": "calibrate_input"}));
        assert!(!ed.engine_calibration_open());
    }

    #[test]
    fn an_unknown_op_is_an_error_reply() {
        let (_d, mut ed) = editor_with("");
        let mut c = ApiClient::default();
        let r = call(&mut c, &mut ed, json!({"op": "nope"}));
        assert!(r["error"].as_str().unwrap().contains("nope"));
    }

    #[test]
    fn a_skins_editor_theme_is_saved_and_applied() {
        let (dir, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        c.push(&mut ed);
        let theme = json!({"name": "probe-theme", "extends": "builtin://light",
            "editor": {"bg": [1, 2, 3]}});
        let (r, out) = call_all(
            &mut c,
            &mut ed,
            json!({"op": "skin.theme", "skin": "probe", "theme": theme}),
        );
        assert_eq!(r["ok"], json!(true), "{r}");
        let saved = dir.path().join(".cfg/config/themes/web-skins/probe.json");
        assert!(
            saved.is_file(),
            "saved where the editor's theme list finds it"
        );
        let ev = out
            .iter()
            .find(|v| v["ev"] == "theme")
            .expect("the new theme is sent");
        assert_eq!(ev["theme"]["bg"], json!("#010203"));
        // A theme by name, as `@editor-theme builtin://dark` asks.
        let (r, out) = call_all(
            &mut c,
            &mut ed,
            json!({"op": "skin.theme", "name": "builtin://dark"}),
        );
        assert_eq!(r["ok"], json!(true), "{r}");
        assert!(out.iter().any(|v| v["ev"] == "theme"));
        // Not a skin id, or not a theme: refused, nothing written.
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "skin.theme", "skin": "../x", "theme": {}}),
        );
        assert!(r["error"].is_string(), "{r}");
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "skin.theme", "skin": "bad", "theme": {"editor": 7}}),
        );
        assert!(r["error"].as_str().unwrap().contains("invalid"), "{r}");
        assert!(!dir
            .path()
            .join(".cfg/config/themes/web-skins/bad.json")
            .exists());
    }

    #[test]
    fn skin_values_carry_plugin_values_and_the_hosts_own() {
        let (_dir, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        ed.set_skin_value("probe:level".into(), "42".into());
        let out = c.push(&mut ed);
        let ev = out
            .iter()
            .find(|v| v["ev"] == "skinValues")
            .expect("skinValues");
        let vals = &ev["skinValues"];
        assert_eq!(vals["probe:level"], json!("42"));
        assert_eq!(vals["skin_data:buffers"], json!("1"));
        assert!(
            vals["skin_data:time"]
                .as_str()
                .is_some_and(|t| t.len() == 5),
            "{vals}"
        );
        // A removed value is gone from the next push.
        ed.set_skin_value("probe:level".into(), String::new());
        let out = c.push(&mut ed);
        let ev = out
            .iter()
            .find(|v| v["ev"] == "skinValues")
            .expect("changed, so sent");
        assert!(ev["skinValues"].get("probe:level").is_none());
    }

    #[test]
    fn a_client_hears_the_pages_build_first() {
        let (_dir, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        let out = c.push(&mut ed);
        assert_eq!(
            out[0],
            json!({ "ev": "build", "build": super::super::PAGE_BUILD })
        );
        assert_eq!(super::super::PAGE_BUILD.len(), 16, "a 64-bit hash, in hex");
        assert!(
            !c.push(&mut ed).iter().any(|v| v["ev"] == "build"),
            "once per connection"
        );
    }

    #[test]
    fn the_root_follows_the_active_workspace() {
        let (dir, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        let roots = |out: &[Value]| -> Vec<String> {
            out.iter()
                .filter(|v| v["ev"] == "root")
                .map(|v| v["root"].as_str().unwrap().to_string())
                .collect()
        };
        let first = roots(&c.push(&mut ed));
        assert_eq!(first, vec![ed.working_dir().to_string_lossy().to_string()]);
        // The Orchestrator switches to another workspace: the client hears
        // the new root, once.
        let other = dir.path().join("other");
        std::fs::create_dir(&other).expect("mkdir");
        let other = other.canonicalize().expect("canonical");
        let w = ed.create_window_at(other.clone(), "other".into());
        ed.set_active_window(w);
        assert_eq!(
            roots(&c.push(&mut ed)),
            vec![other.to_string_lossy().to_string()]
        );
        assert!(roots(&c.push(&mut ed)).is_empty(), "not resent unchanged");
    }

    #[test]
    fn a_clients_place_in_a_workspace_survives_switching_away_and_back() {
        let text: String = (1..=50).map(|i| format!("line {i}\n")).collect();
        let (dir, mut ed) = editor_with(&text);
        let home = ed.active_window_id();
        let mut c = ApiClient::default();
        c.push(&mut ed);
        let (view, _) = open_view(&mut c, &mut ed, 60, 10);
        let leaf = ed.engine_layout()["active"].clone();
        for _ in 0..5 {
            c.handle(
                &mut ed,
                &json!({"op": "key", "view": view, "key": "ArrowDown"}),
            );
        }
        let line = |c: &mut ApiClient, ed: &mut crate::app::Editor| {
            event(&c.push(ed), "status").map(|s| s["status"]["line"].clone())
        };
        assert_eq!(line(&mut c, &mut ed), Some(json!(6)));
        // The Orchestrator switches to another workspace: the layout names
        // it, the old view is not drawn, and the client closes it.
        let other = dir.path().join("other");
        std::fs::create_dir(&other).expect("mkdir");
        let w = ed.create_window_at(other.canonicalize().unwrap(), "other".into());
        ed.set_active_window(w);
        let out = c.push(&mut ed);
        let layout = event(&out, "layout").expect("the other workspace's layout");
        assert_eq!(layout["layout"]["window"], json!(w.0));
        assert!(
            !out.iter().any(|v| v["ev"] == "view" && v["view"] == view),
            "not drawn while away"
        );
        c.handle(&mut ed, &json!({"op": "view.close", "view": view}));
        // Back again: opening that pane finds the client's cursor where it was.
        ed.set_active_window(home);
        c.push(&mut ed);
        let (r, out) = call_all(
            &mut c,
            &mut ed,
            json!({"op": "view.open", "leaf": leaf, "width": 60, "height": 10}),
        );
        assert!(r["view"].is_u64(), "{r}");
        let status = event(&out, "status").expect("the status, from the client's view");
        assert_eq!(status["status"]["line"], 6, "the cursor kept its line");
    }

    #[test]
    fn a_tabs_menu_is_the_terminals_and_runs_there() {
        let (_dir, mut ed) = editor_with("x\n");
        let mut c = ApiClient::default();
        let out = c.push(&mut ed);
        let layout = &out.iter().find(|v| v["ev"] == "layout").expect("layout")["layout"];
        let leaf = layout["active"].as_u64().unwrap();
        let buffer = layout["root"]["buffer"].as_u64().unwrap();
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "menu.rows", "at": "tab", "leaf": leaf, "buffer": buffer}),
        );
        let rows = r["rows"].as_array().expect("rows");
        let ids: Vec<&str> = rows.iter().map(|r| r["id"].as_str().unwrap()).collect();
        assert!(
            ids.contains(&"closeOthers") && ids.contains(&"copyFullPath"),
            "{ids:?}"
        );
        // Choosing "Close" closes the tab, as the terminal's menu does.
        let close = ids.iter().position(|i| *i == "close").unwrap();
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "menu.choose", "at": "tab", "leaf": leaf, "buffer": buffer, "index": close}),
        );
        assert_eq!(r["ok"], json!(true), "{r}");
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "menu.rows", "at": "newTab", "leaf": leaf}),
        );
        assert_eq!(r["rows"].as_array().unwrap().len(), 2);
        let r = call(
            &mut c,
            &mut ed,
            json!({"op": "menu.choose", "at": "tab", "leaf": leaf, "buffer": buffer, "index": 999}),
        );
        assert!(r["error"].is_string());
    }
}
