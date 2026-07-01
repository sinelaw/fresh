//! Local HTTP bridge that hosts the **real** editor for the web UI (no mocks).
//!
//! The frontend renders the real editor by tapping the **actual render
//! pipeline**: we run `Editor::render` once into an in-memory cell buffer, then
//! read the geometry the pipeline already aggregated for the frame
//! (`WindowLayoutCache` + `ChromeLayout`) and slice the rendered cells. Nothing
//! about layout, highlighting, tabs, scrollbars, or split borders is
//! re-implemented — we only re-target the final drawing:
//!
//!   - **buffer interiors** (text inside each split pane) are emitted as the
//!     real, syntax-highlighted **cells** the pipeline drew in each pane's
//!     `content_rect`;
//!   - **chrome** (menu bar, status bar, tabs, scrollbars, split borders, the
//!     file-explorer pane) is emitted as **semantic regions** (role + rect, with
//!     thumb/orientation as needed) so the frontend draws real UI elements.
//!
//! Routes (single-threaded — the editor is not `Send`, one client):
//!   - `GET /`        → serves `web-ui/index.html`
//!   - `GET /favicon.ico` → 204
//!   - `GET /state`   → `{ w, h, grid, regions }` from the real render
//!   - `POST /key`    → runs the real `Editor::handle_key`, returns `/state`
//!   - `POST /resize` → `{cols, rows}` → `Editor::resize`, returns `/state`

use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::backend::TestBackend;
use ratatui::buffer::Buffer;
use ratatui::layout::{Position, Rect};
use ratatui::style::{Color, Modifier};
use ratatui::Terminal;
use serde_json::{json, Value};

use crate::app::Editor;
use crate::config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};

/// Default terminal size the bridge boots / resets to (cols, rows). One source
/// so `run()` and the `/reset` route can't drift apart.
const DEFAULT_SIZE: (u16, u16) = (140, 44);

/// Construct a fresh editor exactly as the web bridge does: real plugin runtime
/// enabled, init.ts loaded, chrome drawn as a semantic model (not cells). Shared
/// by `run()`, the `/reset` route (scenario isolation) and the parity test
/// runner so all three drive an identical editor.
pub fn build_editor(cols: u16, rows: u16, files: &[PathBuf]) -> Result<Editor> {
    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();
    let cfg = config::Config::load_with_layers(&dir_context, &working_dir);
    let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);

    let mut editor = Editor::with_working_dir(
        cfg,
        cols,
        rows,
        Some(working_dir),
        dir_context,
        true, // plugins_enabled: load the real plugin runtime (git, orchestrator,
        // env manager, …) so the web UI is as full-featured as the TUI.
        crate::view::color_support::ColorCapability::TrueColor,
        fs,
    )?;
    // Mirror the TUI boot: load the user's init.ts and fire the plugins-loaded
    // lifecycle hook. Plugin loads run on the plugin thread and arrive via the
    // AsyncBridge, which `editor_tick` (run on every poll) drains — so by the
    // time the UI settles the plugin menus/commands are present.
    editor.load_init_script_async(true);
    editor.fire_plugins_loaded_hook();

    // We render chrome (menu, dropdown, command palette) as native HTML from the
    // semantic model, so tell the pipeline to compute chrome *layout* but not draw
    // it into the cells — the cell buffer carries pane interiors only, with no
    // chrome to hide. See docs/internal/UNIFIED_SCENE_DESIGN.md (Phase 1).
    editor.suppress_chrome_cells = true;
    for f in files {
        if let Err(e) = editor.open_file(f) {
            eprintln!("open_file {f:?} failed: {e}");
        }
    }
    Ok(editor)
}

/// Apply one parity-scenario step to the editor: a key, a mouse event at a cell,
/// an action by name, a literal string to type, or a tick. Shared by the web
/// `/step` route and the Rust parity runner so both drive identical input.
pub fn apply_step(editor: &mut Editor, step: &Value) {
    if let Some(s) = step.get("type").and_then(|t| t.as_str()) {
        for ch in s.chars() {
            apply_key(editor, &json!({ "key": ch.to_string() }));
        }
    } else if step.get("key").is_some() {
        apply_key(editor, step);
    } else if step.get("kind").is_some() {
        apply_mouse(editor, step);
    } else if let Some(name) = step.get("action").and_then(|a| a.as_str()) {
        if let Some(act) =
            crate::input::keybindings::Action::from_str(name, &std::collections::HashMap::new())
        {
            if let Err(e) = editor.handle_action(act) {
                eprintln!("[webui] action error: {e}");
            }
        }
    }
    // Drain async work / step animations. The `bool` (needs-render) is moot —
    // the bridge re-renders the scene on every request — but surface a real
    // tick error rather than swallowing it.
    if let Err(e) = crate::app::editor_tick(editor, || Ok(())) {
        eprintln!("[webui] editor_tick error: {e}");
    }
}

/// Build the semantic scene (the same model the web frontend renders). Public so
/// the parity runner can assert on the identical scene the browser sees.
pub fn scene_value(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    scene_json(editor, cols, rows)
}

/// Render the SAME editor the way the TUI would — chrome drawn into cells — and
/// return the joined cell text. Used by the parity test to assert the web's
/// semantic scene and the terminal's cell rendering agree for one editor state
/// (single source of truth). Temporarily clears `suppress_chrome_cells`, then
/// restores it so the caller's web mode is unaffected.
pub fn render_tui_cells(editor: &mut Editor, cols: u16, rows: u16) -> String {
    let prev = editor.suppress_chrome_cells;
    editor.suppress_chrome_cells = false;
    let (buf, _) = render_to_buffer(editor, cols, rows);
    editor.suppress_chrome_cells = prev;
    let mut out = String::new();
    for y in 0..buf.area.height {
        for x in 0..buf.area.width {
            out.push_str(buf.cell((x, y)).map(|c| c.symbol()).unwrap_or(" "));
        }
        out.push('\n');
    }
    out
}

pub fn run(addr: &str, files: &[PathBuf]) -> Result<()> {
    let (mut cols, mut rows) = DEFAULT_SIZE;
    let mut editor = build_editor(cols, rows, files)?;

    let listener = TcpListener::bind(addr)?;
    eprintln!("fresh web bridge on http://{addr}  (real render pipeline, no mocks)");
    let html_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../web-ui/index.html");

    for stream in listener.incoming() {
        let mut stream = match stream {
            Ok(s) => s,
            Err(_) => continue,
        };
        if let Err(e) = handle_conn(
            &mut stream,
            &mut editor,
            html_path,
            &mut cols,
            &mut rows,
            files,
        ) {
            eprintln!("conn error: {e}");
        }
    }
    Ok(())
}

fn handle_conn(
    stream: &mut TcpStream,
    editor: &mut Editor,
    html_path: &str,
    cols: &mut u16,
    rows: &mut u16,
    files: &[PathBuf],
) -> Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request_line = String::new();
    if reader.read_line(&mut request_line)? == 0 {
        return Ok(());
    }
    let mut it = request_line.split_whitespace();
    let method = it.next().unwrap_or("");
    let path = it.next().unwrap_or("/");

    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line)? == 0 || line == "\r\n" || line == "\n" {
            break;
        }
        if let Some(v) = line.to_ascii_lowercase().strip_prefix("content-length:") {
            content_length = v.trim().parse().unwrap_or(0);
        }
    }
    let mut body = vec![0u8; content_length];
    if content_length > 0 {
        reader.read_exact(&mut body)?;
    }

    match (method, path) {
        ("GET", "/") => {
            let html = std::fs::read_to_string(html_path)
                .unwrap_or_else(|_| "<h1>web-ui/index.html not found</h1>".into());
            respond(
                stream,
                "200 OK",
                "text/html; charset=utf-8",
                html.as_bytes(),
            )
        }
        ("GET", "/favicon.ico") => respond(stream, "204 No Content", "image/x-icon", b""),
        ("GET", "/state") => {
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/key") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            apply_key(editor, &v);
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/mouse") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            apply_mouse(editor, &v);
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/action") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            if let Some(name) = v.get("action").and_then(|a| a.as_str()) {
                let args: std::collections::HashMap<String, Value> = v
                    .get("args")
                    .and_then(|a| a.as_object())
                    .map(|m| m.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
                    .unwrap_or_default();
                if let Some(act) = crate::input::keybindings::Action::from_str(name, &args) {
                    if let Err(e) = editor.handle_action(act) {
                        eprintln!("[webui] action error: {e}");
                    }
                }
            }
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/widget") => {
            // Native plugin-widget interaction. For the overlay prompt toolbar,
            // a Toggle/Button click forwards the widget `key`; the editor flips
            // the toggle in-spec and fires the plugin's `widget_event` — the
            // exact path a TUI toolbar click takes.
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            match v.get("surface").and_then(|s| s.as_str()) {
                Some("toolbar") => {
                    if let Some(key) = v.get("key").and_then(|k| k.as_str()) {
                        editor.toggle_overlay_toolbar_widget(key);
                    }
                }
                Some("panel") => {
                    // Floating/dock widget: deliver the clicked hit by index,
                    // running the same path as a TUI cell click.
                    let plugin = v.get("plugin").and_then(|p| p.as_str()).unwrap_or("");
                    let panel_id = v.get("panelId").and_then(|p| p.as_u64()).unwrap_or(0);
                    if let Some(idx) = v.get("hitIndex").and_then(|i| i.as_u64()) {
                        editor.deliver_widget_hit_by_index(plugin, panel_id, idx as usize);
                    }
                }
                _ => {}
            }
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/settings") => {
            // Native Settings interaction: the frontend sends the `SettingsHit`
            // it rendered (kind + indices); we run the SAME dispatch a TUI cell
            // click would (`dispatch_settings_hit`).
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            let a = v.get("a").and_then(|x| x.as_u64()).unwrap_or(0) as usize;
            let bb = v.get("b").and_then(|x| x.as_u64()).unwrap_or(0) as usize;
            let dbl = v.get("double").and_then(|x| x.as_bool()).unwrap_or(false);
            use crate::view::settings::SettingsHit as H;
            let kind = v.get("kind").and_then(|k| k.as_str()).unwrap_or("");
            // Entry (add/edit) sub-dialog interactions take a separate semantic
            // path — the dialog is its own stacked state, not a main-panel item.
            if kind == "entryItem" {
                editor.entry_dialog_select_item(a);
                let s = tick_scene(editor, *cols, *rows).to_string();
                return respond(stream, "200 OK", "application/json", s.as_bytes());
            }
            if kind == "entryButton" {
                let btn = v.get("button").and_then(|x| x.as_str()).unwrap_or("cancel");
                editor.entry_dialog_activate_button(btn);
                let s = tick_scene(editor, *cols, *rows).to_string();
                return respond(stream, "200 OK", "application/json", s.as_bytes());
            }
            let hit = match kind {
                "category" => Some(H::Category(a)),
                "categoryDisclosure" => Some(H::CategoryDisclosure(a)),
                "categorySection" => Some(H::CategorySection(a, bb)),
                "item" => Some(H::Item(a)),
                "controlToggle" => Some(H::ControlToggle(a)),
                "controlDropdown" => Some(H::ControlDropdown(a)),
                "controlDropdownOption" => Some(H::ControlDropdownOption(a, bb)),
                "controlDecrement" => Some(H::ControlDecrement(a)),
                "controlIncrement" => Some(H::ControlIncrement(a)),
                "controlText" => Some(H::ControlText(a)),
                "controlMapRow" => Some(H::ControlMapRow(a, bb)),
                "controlMapAddNew" => Some(H::ControlMapAddNew(a)),
                "controlTextListRow" => Some(H::ControlTextListRow(a, bb)),
                "controlDualListAvailable" => Some(H::ControlDualListAvailable(a, bb)),
                "controlDualListIncluded" => Some(H::ControlDualListIncluded(a, bb)),
                "controlDualListAdd" => Some(H::ControlDualListAdd(a)),
                "controlDualListRemove" => Some(H::ControlDualListRemove(a)),
                "controlDualListMoveUp" => Some(H::ControlDualListMoveUp(a)),
                "controlDualListMoveDown" => Some(H::ControlDualListMoveDown(a)),
                "controlInherit" => Some(H::ControlInherit(a)),
                "searchResult" => Some(H::SearchResult(a)),
                "save" => Some(H::SaveButton),
                "cancel" => Some(H::CancelButton),
                "reset" => Some(H::ResetButton),
                "layer" => Some(H::LayerButton),
                "edit" => Some(H::EditButton),
                "clearCategory" => Some(H::ClearCategoryButton),
                _ => None,
            };
            if let Some(hit) = hit {
                editor.dispatch_settings_hit(hit, 0, dbl);
            }
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/kbedit") => {
            // Native keybinding-editor click: select the display row the frontend
            // rendered (same as a TUI row click). Other interactions are keyboard.
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            if let Some(a) = v.get("a").and_then(|x| x.as_u64()) {
                editor.kbedit_select_display_row(a as usize);
            }
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/resize") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            if let Some(c) = v.get("cols").and_then(|x| x.as_u64()) {
                *cols = (c as u16).clamp(20, 400);
            }
            if let Some(r) = v.get("rows").and_then(|x| x.as_u64()) {
                *rows = (r as u16).clamp(8, 200);
            }
            editor.resize(*cols, *rows);
            let s = tick_scene(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        // Parity-harness routes: apply one scenario step, and reset to a fresh
        // editor so each scenario runs in isolation (mirrors the Rust runner,
        // which builds a fresh editor per scenario).
        ("POST", "/step") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            apply_step(editor, &v);
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/reset") => {
            (*cols, *rows) = DEFAULT_SIZE;
            match build_editor(*cols, *rows, files) {
                Ok(e) => *editor = e,
                Err(err) => eprintln!("reset failed: {err}"),
            }
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        _ => respond(stream, "404 Not Found", "text/plain", b"not found"),
    }
}

fn respond(stream: &mut TcpStream, status: &str, ctype: &str, body: &[u8]) -> Result<()> {
    // No CORS header: the frontend is served from this same origin, so it needs
    // none, and `Access-Control-Allow-Origin: *` would let any site the user
    // visits read `/state` (live buffer contents) cross-origin. Same-origin
    // policy is the protection here.
    let header = format!(
        "HTTP/1.1 {status}\r\nContent-Type: {ctype}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
    );
    stream.write_all(header.as_bytes())?;
    stream.write_all(body)?;
    stream.flush()?;
    Ok(())
}

/// Run the real render pipeline into an in-memory cell buffer, returning the
/// rendered cells and the real hardware-cursor cell the pipeline set (if any).
fn render_to_buffer(editor: &mut Editor, cols: u16, rows: u16) -> (Buffer, Option<(u16, u16)>) {
    use ratatui::backend::Backend;
    let backend = TestBackend::new(cols, rows);
    let mut terminal = Terminal::new(backend).expect("terminal");
    terminal.draw(|frame| editor.render(frame)).expect("draw");
    let buf = terminal.backend().buffer().clone();
    let cursor = terminal
        .backend_mut()
        .get_cursor_position()
        .ok()
        .map(|p| (p.x, p.y));
    (buf, cursor)
}

fn rect_json(r: Rect) -> Value {
    json!({ "x": r.x, "y": r.y, "w": r.width, "h": r.height })
}

/// Slice the rendered cells inside `r` into rows of styled runs.
fn cells_json(buf: &Buffer, r: Rect) -> Value {
    let mut rows = Vec::with_capacity(r.height as usize);
    for y in r.y..r.y.saturating_add(r.height) {
        let mut runs: Vec<Value> = Vec::new();
        let mut cur_text = String::new();
        let mut cur_fg: Option<String> = None;
        let mut cur_bg: Option<String> = None;
        let mut cur_mods = Modifier::empty();
        let flush = |runs: &mut Vec<Value>,
                     text: &mut String,
                     fg: &Option<String>,
                     bg: &Option<String>,
                     m: Modifier| {
            if !text.is_empty() {
                runs.push(json!({
                    "t": text,
                    "fg": fg, "bg": bg,
                    "b": m.contains(Modifier::BOLD),
                    "i": m.contains(Modifier::ITALIC),
                    "u": m.contains(Modifier::UNDERLINED),
                    "r": m.contains(Modifier::REVERSED),
                }));
                text.clear();
            }
        };
        for x in r.x..r.x.saturating_add(r.width) {
            let Some(cell) = buf.cell(Position::new(x, y)) else {
                continue;
            };
            let fg = color_css(cell.fg);
            let bg = color_css(cell.bg);
            let m = cell.modifier;
            if !cur_text.is_empty() && (fg != cur_fg || bg != cur_bg || m != cur_mods) {
                flush(&mut runs, &mut cur_text, &cur_fg, &cur_bg, cur_mods);
            }
            cur_fg = fg;
            cur_bg = bg;
            cur_mods = m;
            cur_text.push_str(cell.symbol());
        }
        flush(&mut runs, &mut cur_text, &cur_fg, &cur_bg, cur_mods);
        rows.push(Value::Array(runs));
    }
    Value::Array(rows)
}

/// Advance the editor one "tick" (drain async LSP/plugin/file messages, fire
/// timers, step animations) exactly as the TUI event loop does, then build the
/// scene. This is what lets the browser frontend get fresh frames by polling
/// rather than only in response to its own input.
fn tick_scene(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    // Needs-render bool is moot (we render unconditionally below); don't swallow
    // a real tick error.
    if let Err(e) = crate::app::editor_tick(editor, || Ok(())) {
        eprintln!("[webui] editor_tick error: {e}");
    }
    scene_json(editor, cols, rows)
}

fn scene_json(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    let (buf, cursor) = render_to_buffer(editor, cols, rows);
    let w = buf.area.width;
    let h = buf.area.height;

    // Semantic popups (completion / hover / action / list / text) — derived once
    // in the core (`Editor::popups_view`) and rendered as native UI, not cells.
    let popups = serde_json::to_value(editor.popups_view()).unwrap_or_else(|_| json!([]));

    // Semantic menu model — derived once in the core (`Editor::menu_view`) and
    // shared with the TUI renderer; the bridge only serializes it. See
    // crates/fresh-editor/src/view/scene.rs.
    let menu_view = serde_json::to_value(editor.menu_view()).unwrap_or_else(|_| json!({}));
    let get = |k: &str| menu_view.get(k).cloned().unwrap_or(Value::Null);
    let menus = get("menus");
    let menu_open = get("menuOpen");
    let menu_highlight = get("menuHighlight");
    let submenu_path = get("submenuPath");
    let dropdown = get("dropdown");

    // --- per-window geometry from the pipeline's layout cache ---
    let layout = editor.active_layout();
    let content = layout.editor_content_area.unwrap_or(Rect::new(0, 0, w, h));
    // The menu bar spans the FULL width at row 0 — exactly as the TUI draws it,
    // *above* any left dock (the dock/file-explorer carve the rows below). Using
    // `content.x` here would shift the whole menu right when a left dock opens.
    // Per-menu title x still comes from the editor's MenuLayout cell positions
    // (so titles + their dropdowns align); only the container is full-width.
    let menubar_rect = (content.y > 0).then(|| Rect::new(0, 0, w, content.y));

    let panes: Vec<Value> = layout
        .split_areas
        .iter()
        .map(
            |(leaf, bufid, content_rect, scrollbar_rect, thumb_s, thumb_e)| {
                // Tabs are derived once in the core (`Editor::tab_bar_view`).
                let tb = editor.tab_bar_view(*leaf);
                // Emit the line-number gutter as its own cell block, separate
                // from the buffer text, sliced at the renderer's real gutter
                // width — so the frontend keeps the gutter out of the text flow
                // (future native selection covers only code). `cells` is the
                // buffer text after the gutter; `gutter` is the line-number
                // column. When line numbers are off, gutterWidth is 0 and
                // `cells` is the whole pane.
                let gw = editor
                    .leaf_gutter_width(*leaf, *bufid)
                    .min(content_rect.width);
                let gutter_rect =
                    Rect::new(content_rect.x, content_rect.y, gw, content_rect.height);
                let text_rect = Rect::new(
                    content_rect.x + gw,
                    content_rect.y,
                    content_rect.width - gw,
                    content_rect.height,
                );
                json!({
                    "leaf": leaf.0 .0,
                    "buffer": bufid.0,
                    "content": rect_json(*content_rect),
                    "gutterWidth": gw,
                    "gutter": if gw > 0 { cells_json(&buf, gutter_rect) } else { Value::Null },
                    "cells": cells_json(&buf, text_rect),
                    "tabBar": serde_json::to_value(tb.bar).unwrap_or(Value::Null),
                    "tabs": serde_json::to_value(tb.tabs).unwrap_or_else(|_| json!([])),
                    "vscroll": rect_json(*scrollbar_rect),
                    "thumbStart": thumb_s,
                    "thumbEnd": thumb_e,
                })
            },
        )
        .collect();

    let separators: Vec<Value> = layout
        .separator_areas
        .iter()
        .map(|(_id, dir, x, y, len)| {
            json!({
                "vertical": matches!(dir, crate::model::event::SplitDirection::Vertical),
                "x": x, "y": y, "len": len,
            })
        })
        .collect();

    // Semantic file explorer (sidebar tree) — derived once in the core
    // (`Editor::file_explorer_view`) and rendered as native UI, not cells.
    let file_explorer = serde_json::to_value(editor.file_explorer_view()).unwrap_or(Value::Null);

    // Semantic status bar and command palette are derived once in the core
    // (`Editor::status_view` / `Editor::palette_view`); the bridge only
    // serializes them. See crates/fresh-editor/src/view/scene.rs.
    let statusbar = serde_json::to_value(editor.status_view()).unwrap_or(Value::Null);
    let mut palette = serde_json::to_value(editor.palette_view()).unwrap_or(Value::Null);
    // The overlay preview pane is real rendered buffer cells (a phantom-leaf
    // pipeline render into its content rect), not chrome — so slice them from
    // the buffer just like a pane interior and attach them to the palette. The
    // frontend draws these cells inside its native preview frame.
    if let Some(pv) = palette.get("previewRect").cloned() {
        let u = |k: &str| pv.get(k).and_then(|x| x.as_u64()).unwrap_or(0) as u16;
        let pr = Rect::new(u("x"), u("y"), u("w"), u("h"));
        if pr.width > 0 && pr.height > 0 {
            let cells = cells_json(&buf, pr);
            if let Some(obj) = palette.as_object_mut() {
                obj.insert("previewCells".to_string(), cells);
            }
        }
    }
    let trust_dialog = serde_json::to_value(editor.trust_dialog_view()).unwrap_or(Value::Null);
    // Plugin-mounted floating / dock widget panels (e.g. the orchestrator dock),
    // rendered natively from their WidgetSpec.
    let widgets = serde_json::to_value(editor.widgets_view()).unwrap_or(Value::Null);
    // Active right-click / new-tab context menu, rendered natively.
    let context_menu = serde_json::to_value(editor.context_menu_view()).unwrap_or(Value::Null);
    // Auxiliary modals (keybinding editor / event-debug / theme-info popup).
    let aux_modal = serde_json::to_value(editor.aux_modals_view()).unwrap_or(Value::Null);
    // Full keybinding editor modal (header/search/filters, table, edit dialog…).
    let keybinding_editor =
        serde_json::to_value(editor.keybinding_editor_view()).unwrap_or(Value::Null);
    // Full Settings modal (category tree, items, search, entry dialog).
    let settings = serde_json::to_value(editor.settings_view()).unwrap_or(Value::Null);

    // Theme-accurate chrome palette: the active editor `Theme` resolves every
    // UI color the TUI draws with. The frontend's CSS variables (--bg, --fg,
    // --accent, …) are seeded from these so the native HTML chrome matches the
    // terminal instead of a fixed dark palette. Color→CSS is the web renderer's
    // job (the TUI uses `Color` directly), so it lives here in the bridge.
    let theme = {
        let t = editor.theme.read().unwrap();
        json!({
            "name": t.name,
            "bg": color_css(t.editor_bg),
            "fg": color_css(t.editor_fg),
            "accent": color_css(t.cursor),
            "muted": color_css(t.line_number_fg),
            "selectionBg": color_css(t.selection_bg),
            "menuBg": color_css(t.menu_bg),
            "menuFg": color_css(t.menu_fg),
            "menuHi": color_css(t.menu_highlight_bg),
            "popupBg": color_css(t.popup_bg),
            "popupFg": color_css(t.popup_text_fg),
            "border": color_css(t.popup_border_fg),
            "statusBg": color_css(t.status_bar_bg),
            "statusFg": color_css(t.status_bar_fg),
            "tabActiveBg": color_css(t.tab_active_bg),
        })
    };

    let regions = json!({
        "menubar": menubar_rect.map(rect_json),
        "menus": menus,
        "menuOpen": menu_open,
        "menuHighlight": menu_highlight,
        "submenuPath": submenu_path,
        "dropdown": dropdown,
        "statusbar": statusbar,
        "fileExplorer": file_explorer,
        "panes": panes,
        "separators": separators,
        "popups": popups,
        "palette": palette,
        "trustDialog": trust_dialog,
        "widgets": widgets,
        "contextMenu": context_menu,
        "auxModal": aux_modal,
        "keybindingEditor": keybinding_editor,
        "settings": settings,
        "cursor": cursor.map(|(x, y)| json!({ "x": x, "y": y })),
        // Pacing hint for the frontend's poll loop: when something is animating /
        // an LSP spinner is live / a timer is pending, poll fast; otherwise idle
        // slowly (just to pick up async LSP/file events).
        "poll": json!({
            "active": editor.active_window().animations.is_active()
                || editor.active_window().has_active_lsp_progress()
                || editor.next_periodic_redraw_deadline().is_some(),
        }),
    });

    json!({ "w": w, "h": h, "regions": regions, "theme": theme })
}

/// Map a browser key to a crossterm key and run the real input path.
fn apply_key(editor: &mut Editor, v: &Value) {
    let key = v.get("key").and_then(|k| k.as_str()).unwrap_or("");
    let ctrl = v.get("ctrl").and_then(|b| b.as_bool()).unwrap_or(false);
    let alt = v.get("alt").and_then(|b| b.as_bool()).unwrap_or(false);
    let meta = v.get("meta").and_then(|b| b.as_bool()).unwrap_or(false);
    let shift = v.get("shift").and_then(|b| b.as_bool()).unwrap_or(false);

    let code = match key {
        "Enter" => KeyCode::Enter,
        "Backspace" => KeyCode::Backspace,
        "Delete" => KeyCode::Delete,
        "Tab" => KeyCode::Tab,
        "Escape" => KeyCode::Esc,
        "ArrowUp" => KeyCode::Up,
        "ArrowDown" => KeyCode::Down,
        "ArrowLeft" => KeyCode::Left,
        "ArrowRight" => KeyCode::Right,
        "Home" => KeyCode::Home,
        "End" => KeyCode::End,
        "PageUp" => KeyCode::PageUp,
        "PageDown" => KeyCode::PageDown,
        s if s.chars().count() == 1 => KeyCode::Char(s.chars().next().unwrap()),
        _ => return,
    };
    let mut mods = KeyModifiers::empty();
    if ctrl {
        mods |= KeyModifiers::CONTROL;
    }
    if alt {
        mods |= KeyModifiers::ALT;
    }
    if meta {
        mods |= KeyModifiers::SUPER;
    }
    if shift && !matches!(code, KeyCode::Char(_)) {
        mods |= KeyModifiers::SHIFT;
    }
    if let Err(e) = editor.handle_key(code, mods) {
        eprintln!("handle_key error: {e}");
    }
}

/// Forward a browser mouse/wheel event to the real `Editor::handle_mouse` at
/// cell coordinates; the editor does all hit-testing (panes, tabs, scrollbars,
/// separators), exactly as the terminal/GUI frontends do.
fn apply_mouse(editor: &mut Editor, v: &Value) {
    let col = v.get("col").and_then(|x| x.as_u64()).unwrap_or(0) as u16;
    let row = v.get("row").and_then(|x| x.as_u64()).unwrap_or(0) as u16;
    let n = v
        .get("n")
        .and_then(|x| x.as_u64())
        .unwrap_or(1)
        .clamp(1, 10);
    let button = match v.get("button").and_then(|b| b.as_str()) {
        Some("right") => MouseButton::Right,
        Some("middle") => MouseButton::Middle,
        _ => MouseButton::Left,
    };
    let kind = match v.get("kind").and_then(|k| k.as_str()).unwrap_or("") {
        "down" => MouseEventKind::Down(button),
        "up" => MouseEventKind::Up(button),
        "drag" => MouseEventKind::Drag(button),
        "moved" => MouseEventKind::Moved,
        "scrollup" => MouseEventKind::ScrollUp,
        "scrolldown" => MouseEventKind::ScrollDown,
        "scrollleft" => MouseEventKind::ScrollLeft,
        "scrollright" => MouseEventKind::ScrollRight,
        _ => return,
    };
    let mut mods = KeyModifiers::empty();
    if v.get("ctrl").and_then(|b| b.as_bool()).unwrap_or(false) {
        mods |= KeyModifiers::CONTROL;
    }
    if v.get("alt").and_then(|b| b.as_bool()).unwrap_or(false) {
        mods |= KeyModifiers::ALT;
    }
    if v.get("shift").and_then(|b| b.as_bool()).unwrap_or(false) {
        mods |= KeyModifiers::SHIFT;
    }
    for _ in 0..n {
        let ev = MouseEvent {
            kind,
            column: col,
            row,
            modifiers: mods,
        };
        if let Err(e) = editor.handle_mouse(ev) {
            eprintln!("handle_mouse error: {e}");
            break;
        }
    }
}

/// The 16 ANSI colors (indices 0-15), shared by `color_css` (named-color arm)
/// and `indexed_css` so the same logical color resolves to one hex no matter
/// whether it arrives as a named `Color` or a `Color::Indexed`.
const ANSI16: [(u8, u8, u8); 16] = [
    (0, 0, 0),          // 0 black
    (0xcd, 0x31, 0x31), // 1 red
    (0x0d, 0xbc, 0x79), // 2 green
    (0xe5, 0xe5, 0x10), // 3 yellow
    (0x24, 0x72, 0xc8), // 4 blue
    (0xbc, 0x3f, 0xbc), // 5 magenta
    (0x11, 0xa8, 0xcd), // 6 cyan
    (0xe5, 0xe5, 0xe5), // 7 white / gray
    (0x66, 0x66, 0x66), // 8 bright black / dark gray
    (0xf1, 0x4c, 0x4c), // 9 bright red
    (0x23, 0xd1, 0x8b), // 10 bright green
    (0xf5, 0xf5, 0x43), // 11 bright yellow
    (0x3b, 0x8e, 0xea), // 12 bright blue
    (0xd6, 0x70, 0xd6), // 13 bright magenta
    (0x29, 0xb8, 0xdb), // 14 bright cyan
    (0xff, 0xff, 0xff), // 15 bright white
];

fn hex(r: u8, g: u8, b: u8) -> String {
    format!("#{r:02x}{g:02x}{b:02x}")
}

/// ratatui Color → CSS hex (or None for the terminal default). Named colors map
/// to their ANSI index in `ANSI16` so they agree with `Color::Indexed`.
fn color_css(c: Color) -> Option<String> {
    let ansi = |i: usize| {
        let (r, g, b) = ANSI16[i];
        hex(r, g, b)
    };
    Some(match c {
        Color::Reset => return None,
        Color::Rgb(r, g, b) => hex(r, g, b),
        Color::Black => ansi(0),
        Color::Red => ansi(1),
        Color::Green => ansi(2),
        Color::Yellow => ansi(3),
        Color::Blue => ansi(4),
        Color::Magenta => ansi(5),
        Color::Cyan => ansi(6),
        Color::Gray => ansi(7),
        Color::DarkGray => ansi(8),
        Color::LightRed => ansi(9),
        Color::LightGreen => ansi(10),
        Color::LightYellow => ansi(11),
        Color::LightBlue => ansi(12),
        Color::LightMagenta => ansi(13),
        Color::LightCyan => ansi(14),
        Color::White => ansi(15),
        Color::Indexed(i) => return Some(indexed_css(i)),
    })
}

/// xterm-256 palette → hex.
fn indexed_css(i: u8) -> String {
    let (r, g, b) = if i < 16 {
        ANSI16[i as usize]
    } else if i < 232 {
        let n = i - 16;
        let levels = [0u8, 95, 135, 175, 215, 255];
        (
            levels[(n / 36) as usize],
            levels[((n / 6) % 6) as usize],
            levels[(n % 6) as usize],
        )
    } else {
        let v = 8 + (i - 232) * 10;
        (v, v, v)
    };
    hex(r, g, b)
}
