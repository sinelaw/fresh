//! Local HTTP bridge that hosts the **real** editor for the web UI (no mocks).
//!
//! This is the end-to-end wire from `web-ui/index.html` to the actual
//! [`Editor`]: the browser is the frontend, a real `Editor` is the backend,
//! talking over localhost. It is the same `Backend` seam the UI already has —
//! just an HTTP transport instead of Tauri's `invoke`/event (so it can be
//! driven headlessly). Architecture: Direction A in
//! `docs/internal/NON_TERMINAL_UI_RESEARCH.md`.
//!
//! Routes (single-threaded, one connection at a time — the editor is not
//! `Send`, and there is exactly one client):
//!   - `GET  /`        → serves `web-ui/index.html`
//!   - `GET  /state`   → real `{ chrome, active, buffers }` JSON
//!   - `POST /key`     → maps a browser key to a crossterm `KeyEvent` and runs
//!                       the real `Editor::handle_key`, then returns `/state`
//!
//! Partial wire (intentionally): keyboard input drives real editing; chrome
//! events (tab/divider/palette clicks) and a live caret are wired in later.
//! Everything rendered is genuine editor state.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use crossterm::event::{KeyCode, KeyModifiers};
use serde_json::{json, Value};

use crate::app::Editor;
use crate::config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};

const MAX_LINES: usize = 1000;

/// Run the bridge until the process is killed. `files` are opened in the real
/// editor on startup.
pub fn run(addr: &str, files: &[PathBuf]) -> Result<()> {
    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();
    let cfg = config::Config::load_with_layers(&dir_context, &working_dir);
    let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);

    let mut editor = Editor::with_working_dir(
        cfg,
        120,
        40,
        Some(working_dir),
        dir_context,
        false, // no plugins for the bridge demo
        crate::view::color_support::ColorCapability::TrueColor,
        fs,
    )?;

    let mut names: Vec<(usize, String)> = Vec::new();
    for f in files {
        match editor.open_file(f) {
            Ok(id) => names.push((
                id.0,
                f.file_name()
                    .map(|n| n.to_string_lossy().into_owned())
                    .unwrap_or_else(|| format!("buffer#{}", id.0)),
            )),
            Err(e) => eprintln!("open_file {f:?} failed: {e}"),
        }
    }

    let listener = TcpListener::bind(addr)?;
    eprintln!("fresh web bridge listening on http://{addr}  (real editor, no mocks)");

    let html_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../web-ui/index.html");

    for stream in listener.incoming() {
        let mut stream = match stream {
            Ok(s) => s,
            Err(_) => continue,
        };
        if let Err(e) = handle_conn(&mut stream, &mut editor, &names, html_path) {
            eprintln!("conn error: {e}");
        }
    }
    Ok(())
}

fn handle_conn(
    stream: &mut TcpStream,
    editor: &mut Editor,
    names: &[(usize, String)],
    html_path: &str,
) -> Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request_line = String::new();
    if reader.read_line(&mut request_line)? == 0 {
        return Ok(());
    }
    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or("");
    let path = parts.next().unwrap_or("/");

    // Drain headers, capture Content-Length.
    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line)? == 0 {
            break;
        }
        if line == "\r\n" || line == "\n" {
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
                .unwrap_or_else(|_| "<h1>web-ui/index.html not found</h1>".to_string());
            respond(stream, "200 OK", "text/html; charset=utf-8", html.as_bytes())
        }
        ("GET", "/favicon.ico") => respond(stream, "204 No Content", "image/x-icon", b""),
        ("GET", "/state") => {
            let s = state_json(editor, names).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())
        }
        ("POST", "/key") => {
            let v: Value = serde_json::from_slice(&body).unwrap_or(json!({}));
            apply_key(editor, &v);
            let s = state_json(editor, names).to_string();
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

/// Build the real editor state for the frontend: the chrome snapshot plus the
/// actual text of every buffer reachable from the split tree.
fn state_json(editor: &Editor, names: &[(usize, String)]) -> Value {
    let chrome = serde_json::to_value(editor.chrome_snapshot()).unwrap_or(json!(null));
    let active = editor.active_buffer().0;

    let mut buffers = serde_json::Map::new();
    for id in editor.buffers().ids() {
        if let Some(st) = editor.buffers().get(&id) {
            let text = st.buffer.to_string().unwrap_or_default();
            let lines: Vec<&str> = text.split('\n').take(MAX_LINES).collect();
            let name = names
                .iter()
                .find(|(bid, _)| *bid == id.0)
                .map(|(_, n)| n.clone())
                .unwrap_or_else(|| format!("untitled#{}", id.0));
            buffers.insert(
                id.0.to_string(),
                json!({ "name": name, "lines": lines, "lineCount": text.matches('\n').count() + 1 }),
            );
        }
    }

    json!({ "chrome": chrome, "active": active, "buffers": Value::Object(buffers) })
}

/// Map a browser key descriptor to a crossterm key and run the real input path.
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
        _ => return, // ignore unmapped keys
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
    // Only attach SHIFT for non-printable keys; printable chars already carry case.
    if shift && !matches!(code, KeyCode::Char(_)) {
        mods |= KeyModifiers::SHIFT;
    }

    if let Err(e) = editor.handle_key(code, mods) {
        eprintln!("handle_key error: {e}");
    }
}
