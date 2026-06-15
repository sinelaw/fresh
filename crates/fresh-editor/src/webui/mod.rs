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

pub fn run(addr: &str, files: &[PathBuf]) -> Result<()> {
    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();
    let cfg = config::Config::load_with_layers(&dir_context, &working_dir);
    let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);

    let (mut cols, mut rows) = (140u16, 44u16);
    let mut editor = Editor::with_working_dir(
        cfg,
        cols,
        rows,
        Some(working_dir),
        dir_context,
        false,
        crate::view::color_support::ColorCapability::TrueColor,
        fs,
    )?;
    for f in files {
        if let Err(e) = editor.open_file(f) {
            eprintln!("open_file {f:?} failed: {e}");
        }
    }

    let listener = TcpListener::bind(addr)?;
    eprintln!("fresh web bridge on http://{addr}  (real render pipeline, no mocks)");
    let html_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../web-ui/index.html");

    for stream in listener.incoming() {
        let mut stream = match stream {
            Ok(s) => s,
            Err(_) => continue,
        };
        if let Err(e) = handle_conn(&mut stream, &mut editor, html_path, &mut cols, &mut rows) {
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
            respond(stream, "200 OK", "text/html; charset=utf-8", html.as_bytes())
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
                    editor.dispatch_action_for_tests(act);
                }
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
        _ => respond(stream, "404 Not Found", "text/plain", b"not found"),
    }
}

fn respond(stream: &mut TcpStream, status: &str, ctype: &str, body: &[u8]) -> Result<()> {
    let header = format!(
        "HTTP/1.1 {status}\r\nContent-Type: {ctype}\r\nContent-Length: {}\r\nAccess-Control-Allow-Origin: *\r\nConnection: close\r\n\r\n",
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

/// Union of a set of rects (None if empty).
fn union_rect<'a>(rects: impl Iterator<Item = &'a Rect>) -> Option<Rect> {
    let mut acc: Option<Rect> = None;
    for r in rects {
        acc = Some(match acc {
            None => *r,
            Some(a) => {
                let x0 = a.x.min(r.x);
                let y0 = a.y.min(r.y);
                let x1 = (a.x + a.width).max(r.x + r.width);
                let y1 = (a.y + a.height).max(r.y + r.height);
                Rect::new(x0, y0, x1 - x0, y1 - y0)
            }
        });
    }
    acc
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
        let mut flush = |runs: &mut Vec<Value>, text: &mut String, fg: &Option<String>, bg: &Option<String>, m: Modifier| {
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
            let Some(cell) = buf.cell(Position::new(x, y)) else { continue };
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

/// Plain text of cells `[x0, x1)` on row `y` of the rendered buffer (no styling).
/// Used to lift the *text* of a chrome segment (a status-bar indicator) out of
/// the pipeline's render so the frontend can show it as a native UI label rather
/// than a cell grid.
fn text_in_row(buf: &Buffer, y: u16, x0: u16, x1: u16) -> String {
    let mut s = String::new();
    for x in x0..x1 {
        if let Some(cell) = buf.cell(Position::new(x, y)) {
            s.push_str(cell.symbol());
        }
    }
    s
}

/// Build the scene: the real cell grid + semantic chrome regions, all from the
/// pipeline's own per-frame layout caches.
/// Convert a single `MenuItem` into the semantic JSON the frontend renders as
/// native HTML (no cells). Enabled/checked state and accelerators come straight
/// from the editor so the browser menu mirrors the TUI/GUI menus exactly.
fn menu_item_json(editor: &Editor, item: &fresh_core::menu::MenuItem) -> Value {
    use fresh_core::menu::MenuItem::*;
    match item {
        Separator { .. } => json!({ "kind": "sep" }),
        Action {
            label,
            action,
            args,
            when,
            checkbox,
        } => json!({
            "kind": "action",
            "label": label,
            "action": action,
            "args": args,
            "accel": editor.accelerator_for(action),
            "enabled": when
                .as_ref()
                .map(|w| editor.menu_state().context.get(w))
                .unwrap_or(true),
            "checked": checkbox
                .as_ref()
                .map(|c| editor.menu_state().context.get(c)),
        }),
        Submenu { label, items } => json!({
            "kind": "submenu",
            "label": label,
            "items": items.iter().map(|i| menu_item_json(editor, i)).collect::<Vec<_>>(),
        }),
        DynamicSubmenu { label, .. } => json!({
            "kind": "submenu",
            "label": label,
            "items": [],
        }),
        Label { info } => json!({ "kind": "label", "label": info }),
    }
}

/// Short tag for a prompt type, so the frontend can label the palette/picker.
fn prompt_type_tag(t: &crate::view::prompt::PromptType) -> &'static str {
    use crate::view::prompt::PromptType::*;
    match t {
        QuickOpen => "quickopen",
        LiveGrep => "livegrep",
        Search | ReplaceSearch | QueryReplaceSearch => "search",
        OpenFile | OpenFileWithEncoding { .. } => "openfile",
        SaveFileAs => "saveas",
        GotoLine | GotoByteOffset => "goto",
        _ => "input",
    }
}

/// Advance the editor one "tick" (drain async LSP/plugin/file messages, fire
/// timers, step animations) exactly as the TUI event loop does, then build the
/// scene. This is what lets the browser frontend get fresh frames by polling
/// rather than only in response to its own input.
fn tick_scene(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    let _ = crate::app::editor_tick(editor, || Ok(()));
    scene_json(editor, cols, rows)
}

fn scene_json(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    let (buf, cursor) = render_to_buffer(editor, cols, rows);
    let w = buf.area.width;
    let h = buf.area.height;

    // Overlays (misc popups/dialogs) — the pipeline already drew them into the
    // cells and recorded their rects on ChromeLayout; we emit rect + cells so the
    // frontend draws each as a floating UI element.
    // NOTE: neither the menu dropdown NOR the command palette / picker are
    // included here — both are emitted as *semantic* models below and rendered
    // as native UI, not cells.
    let chrome = editor.active_chrome();
    let mut overlay_rects: Vec<Rect> = Vec::new();
    for p in &chrome.popup_areas {
        overlay_rects.push(p.1);
    }
    for g in &chrome.global_popup_areas {
        overlay_rects.push(g.1);
    }
    let overlays: Vec<Value> = overlay_rects
        .iter()
        .filter(|r| r.width > 0 && r.height > 0)
        .map(|r| json!({ "rect": rect_json(*r), "cells": cells_json(&buf, *r) }))
        .collect();

    // Semantic menu model (Option 2: the editor stays the source of truth for
    // which menu is open / highlighted; the frontend renders native HTML).
    let menu_areas: std::collections::HashMap<usize, Rect> = chrome
        .menu_layout
        .as_ref()
        .map(|m| m.menu_areas.iter().cloned().collect())
        .unwrap_or_default();
    let ms = editor.menu_state();
    let menu_open = ms.active_menu;
    let menu_highlight = ms.highlighted_item;
    let submenu_path = ms.submenu_path.clone();
    let menus: Vec<Value> = editor
        .expanded_menu_definitions()
        .iter()
        .enumerate()
        .map(|(i, m)| {
            json!({
                "label": m.label,
                "x": menu_areas.get(&i).map(|r| r.x),
                "w": menu_areas.get(&i).map(|r| r.width),
                "items": m.items.iter().map(|it| menu_item_json(editor, it)).collect::<Vec<_>>(),
            })
        })
        .collect();
    // Cell geometry of the *currently open* dropdown (and any expanded submenu),
    // straight from the pipeline's MenuLayout. The frontend positions native HTML
    // rows at these exact rects and forwards clicks/hovers back through
    // `handle_mouse`, so the editor stays the single source of truth for which
    // item is highlighted / selected / closed.
    let dropdown = chrome.menu_layout.as_ref().and_then(|ml| {
        if ml.item_areas.is_empty() {
            return None;
        }
        let items: Vec<Value> = ml
            .item_areas
            .iter()
            .map(|(idx, r)| json!({ "index": idx, "rect": rect_json(*r) }))
            .collect();
        let submenus: Vec<Value> = ml
            .submenu_areas
            .iter()
            .map(|(depth, idx, r)| json!({ "depth": depth, "index": idx, "rect": rect_json(*r) }))
            .collect();
        Some(json!({
            "rect": union_rect(ml.item_areas.iter().map(|(_, r)| r)).map(rect_json),
            "items": items,
            "submenus": submenus,
        }))
    });

    // --- per-window geometry from the pipeline's layout cache ---
    let active_buffer = editor.active_buffer();
    let layout = editor.active_layout();
    let content = layout.editor_content_area.unwrap_or(Rect::new(0, 0, w, h));
    let menubar_rect = (content.y > 0).then(|| Rect::new(0, 0, w, content.y));

    let panes: Vec<Value> = layout
        .split_areas
        .iter()
        .map(|(leaf, bufid, content_rect, scrollbar_rect, thumb_s, thumb_e)| {
            let tl = layout.tab_layouts.get(leaf);
            let tabs: Vec<Value> = tl
                .map(|t| {
                    t.tabs
                        .iter()
                        .map(|tab| {
                            let bid = tab.target.as_buffer();
                            json!({
                                "bufferId": bid.map(|b| b.0),
                                "label": bid.and_then(|b| editor.buffer_display_name(b))
                                    .unwrap_or_else(|| "untitled".into()),
                                "active": bid == Some(active_buffer),
                                "modified": bid.map(|b| editor.buffer_is_modified(b)).unwrap_or(false),
                                "rect": rect_json(tab.tab_area),
                                "closeRect": rect_json(tab.close_area),
                            })
                        })
                        .collect()
                })
                .unwrap_or_default();
            json!({
                "leaf": leaf.0 .0,
                "buffer": bufid.0,
                "content": rect_json(*content_rect),
                "cells": cells_json(&buf, *content_rect),
                "tabBar": tl.map(|t| rect_json(t.bar_area)),
                "tabs": tabs,
                "vscroll": rect_json(*scrollbar_rect),
                "thumbStart": thumb_s,
                "thumbEnd": thumb_e,
            })
        })
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

    let file_explorer = layout
        .file_explorer_area
        .map(|r| json!({ "rect": rect_json(r), "cells": cells_json(&buf, r) }));

    // Semantic status bar. The pipeline records *where* each indicator sits
    // (col ranges on ChromeLayout) and what it means; we lift each segment's
    // text out of the rendered row and emit a labeled model so the frontend can
    // draw native pills (left message + right indicators), not a cell grid.
    let statusbar = chrome.status_bar_area.map(|(sy, sx, sw)| {
        let bar_end = sx.saturating_add(sw);
        let mid = sx.saturating_add(sw / 2);
        // All indicators the pipeline placed, in column order.
        let mut ind: Vec<(&str, (u16, u16, u16), Option<String>)> = Vec::new();
        let mut push = |name: &'static str, area: Option<(u16, u16, u16)>| {
            if let Some(a) = area {
                ind.push((name, a, None));
            }
        };
        push("lsp", chrome.status_bar_lsp_area);
        push("warning", chrome.status_bar_warning_area);
        push("language", chrome.status_bar_language_area);
        push("encoding", chrome.status_bar_encoding_area);
        push("lineEnding", chrome.status_bar_line_ending_area);
        push("remote", chrome.status_bar_remote_area);
        push("trust", chrome.status_bar_trust_area);
        push("message", chrome.status_bar_message_area);
        for (key, a) in &chrome.status_bar_plugin_token_areas {
            ind.push(("plugin", *a, Some(key.clone())));
        }
        ind.sort_by_key(|(_, (_, start, _), _)| *start);

        // Tile the whole bar: labeled indicators + the untracked text runs
        // between them (file name / Ln,Col live there). Whitespace-only gaps are
        // dropped so the left/right indicator groups separate cleanly.
        let mut segments: Vec<Value> = Vec::new();
        let mut emit_gap = |segs: &mut Vec<Value>, from: u16, to: u16| {
            if to > from {
                let t = text_in_row(&buf, sy, from, to);
                if !t.trim().is_empty() {
                    segs.push(json!({
                        "name": "text", "key": Value::Null, "text": t.trim().to_string(),
                        "x": from, "w": to - from, "side": if from < mid {"left"} else {"right"},
                    }));
                }
            }
        };
        let mut cur = sx;
        for (name, (row, start, end), key) in &ind {
            emit_gap(&mut segments, cur, *start);
            segments.push(json!({
                "name": name, "key": key,
                "text": text_in_row(&buf, *row, *start, *end).trim().to_string(),
                "x": start, "w": end.saturating_sub(*start),
                "side": if *start < mid {"left"} else {"right"},
            }));
            cur = (*end).max(cur);
        }
        emit_gap(&mut segments, cur, bar_end);

        json!({
            "rect": rect_json(Rect::new(sx, sy, sw, 1)),
            "segments": segments,
        })
    });

    // Semantic command palette / picker. The pipeline records the popup
    // geometry; the editor's prompt holds the query + filtered suggestions +
    // selection. We emit a semantic model rendered as native HTML (no cells) and
    // route clicks back through handle_mouse at the pipeline's list cell rect so
    // selection/confirm stay authoritative. Only emit it for prompts that show a
    // picker list (or a floating overlay) — bottom-row text prompts are skipped.
    let sugg_outer = chrome.suggestions_outer_area;
    let sugg_area = chrome.suggestions_area;
    let prompt_results = chrome.prompt_results_area;
    let palette = editor.active_window().prompt.as_ref().and_then(|p| {
        if p.suggestions.is_empty() && !p.overlay {
            return None;
        }
        let suggestions: Vec<Value> = p
            .suggestions
            .iter()
            .map(|s| {
                json!({
                    "text": s.text,
                    "description": s.description,
                    "keybinding": s.keybinding,
                    "disabled": s.disabled,
                })
            })
            .collect();
        let title: String = p.title.iter().map(|t| t.text.as_str()).collect();
        let list_rect = sugg_area.map(|(r, _, _, _)| r).or(prompt_results);
        let (scroll_start, visible, total) = sugg_area
            .map(|(_, s, v, t)| (s, v, t))
            .unwrap_or((p.scroll_offset, p.suggestions.len(), p.suggestions.len()));
        Some(json!({
            "query": p.input,
            "message": p.message,
            "promptType": prompt_type_tag(&p.prompt_type),
            "overlay": p.overlay,
            "title": title,
            "status": p.status,
            "selected": p.selected_suggestion,
            "scrollStart": scroll_start,
            "visibleCount": visible,
            "total": total,
            "outerRect": sugg_outer.map(rect_json),
            "listRect": list_rect.map(rect_json),
            "suggestions": suggestions,
        }))
    });

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
        "overlays": overlays,
        "palette": palette,
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

    json!({ "w": w, "h": h, "regions": regions })
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
    let n = v.get("n").and_then(|x| x.as_u64()).unwrap_or(1).clamp(1, 10);
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

/// ratatui Color → CSS hex (or None for the terminal default).
fn color_css(c: Color) -> Option<String> {
    let hex = |r: u8, g: u8, b: u8| format!("#{r:02x}{g:02x}{b:02x}");
    Some(match c {
        Color::Reset => return None,
        Color::Rgb(r, g, b) => hex(r, g, b),
        Color::Black => hex(0, 0, 0),
        Color::Red => hex(0xcd, 0x31, 0x31),
        Color::Green => hex(0x0d, 0xbc, 0x79),
        Color::Yellow => hex(0xe5, 0xe5, 0x10),
        Color::Blue => hex(0x24, 0x72, 0xc8),
        Color::Magenta => hex(0xbc, 0x3f, 0xbc),
        Color::Cyan => hex(0x11, 0xa8, 0xcd),
        Color::Gray => hex(0xcc, 0xcc, 0xcc),
        Color::DarkGray => hex(0x66, 0x66, 0x66),
        Color::LightRed => hex(0xf1, 0x4c, 0x4c),
        Color::LightGreen => hex(0x23, 0xd1, 0x8b),
        Color::LightYellow => hex(0xf5, 0xf5, 0x43),
        Color::LightBlue => hex(0x3b, 0x8e, 0xea),
        Color::LightMagenta => hex(0xd6, 0x70, 0xd6),
        Color::LightCyan => hex(0x29, 0xb8, 0xdb),
        Color::White => hex(0xe5, 0xe5, 0xe5),
        Color::Indexed(i) => return Some(indexed_css(i)),
    })
}

/// xterm-256 palette → hex.
fn indexed_css(i: u8) -> String {
    let basic = [
        (0, 0, 0), (0xcd, 0x31, 0x31), (0x0d, 0xbc, 0x79), (0xe5, 0xe5, 0x10),
        (0x24, 0x72, 0xc8), (0xbc, 0x3f, 0xbc), (0x11, 0xa8, 0xcd), (0xe5, 0xe5, 0xe5),
        (0x66, 0x66, 0x66), (0xf1, 0x4c, 0x4c), (0x23, 0xd1, 0x8b), (0xf5, 0xf5, 0x43),
        (0x3b, 0x8e, 0xea), (0xd6, 0x70, 0xd6), (0x29, 0xb8, 0xdb), (0xff, 0xff, 0xff),
    ];
    let (r, g, b) = if i < 16 {
        basic[i as usize]
    } else if i < 232 {
        let n = i - 16;
        let levels = [0u8, 95, 135, 175, 215, 255];
        (levels[(n / 36) as usize], levels[((n / 6) % 6) as usize], levels[(n % 6) as usize])
    } else {
        let v = 8 + (i - 232) * 10;
        (v, v, v)
    };
    format!("#{r:02x}{g:02x}{b:02x}")
}
