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
use crossterm::event::{KeyCode, KeyModifiers};
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
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/key") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            apply_key(editor, &v);
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/action") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            if let Some(name) = v.get("action").and_then(|a| a.as_str()) {
                if let Some(act) =
                    crate::input::keybindings::Action::from_str(name, &std::collections::HashMap::new())
                {
                    editor.dispatch_action_for_tests(act);
                }
            }
            let s = scene_json(editor, *cols, *rows).to_string();
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
            let s = scene_json(editor, *cols, *rows).to_string();
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

/// Run the real render pipeline into an in-memory cell buffer.
fn render_to_buffer(editor: &mut Editor, cols: u16, rows: u16) -> Buffer {
    let backend = TestBackend::new(cols, rows);
    let mut terminal = Terminal::new(backend).expect("terminal");
    terminal
        .draw(|frame| editor.render(frame))
        .expect("draw");
    terminal.backend().buffer().clone()
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

/// Build the scene: the real cell grid + semantic chrome regions, all from the
/// pipeline's own per-frame layout caches.
fn scene_json(editor: &mut Editor, cols: u16, rows: u16) -> Value {
    let buf = render_to_buffer(editor, cols, rows);
    let w = buf.area.width;
    let h = buf.area.height;

    // --- semantic regions from the pipeline's layout caches ---
    let layout = editor.active_layout();
    let content = layout.editor_content_area.unwrap_or(Rect::new(0, 0, w, h));

    // menu bar = the band above the editor content; status bar = the row below it.
    let menubar = if content.y > 0 {
        Some(Rect::new(0, 0, w, content.y))
    } else {
        None
    };
    let status_y = content.y.saturating_add(content.height);
    let statusbar = if status_y < h {
        Some(Rect::new(0, status_y, w, 1))
    } else {
        None
    };

    let panes: Vec<Value> = layout
        .split_areas
        .iter()
        .map(|(leaf, bufid, content_rect, scrollbar_rect, thumb_s, thumb_e)| {
            let tab_bar = layout.tab_layouts.get(leaf).map(|t| t.bar_area);
            json!({
                "leaf": leaf.0 .0,
                "buffer": bufid.0,
                "content": rect_json(*content_rect),
                "cells": cells_json(&buf, *content_rect),
                "tabBar": tab_bar.map(rect_json),
                "tabCells": tab_bar.map(|r| cells_json(&buf, r)),
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

    let file_explorer = layout.file_explorer_area.map(|r| {
        json!({ "rect": rect_json(r), "cells": cells_json(&buf, r) })
    });

    let regions = json!({
        "menubar": menubar.map(|r| json!({ "rect": rect_json(r), "cells": cells_json(&buf, r) })),
        "statusbar": statusbar.map(|r| json!({ "rect": rect_json(r), "cells": cells_json(&buf, r) })),
        "fileExplorer": file_explorer,
        "panes": panes,
        "separators": separators,
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
