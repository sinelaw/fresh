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
//! Transport (single-threaded — the editor is not `Send`, everything stays on
//! this one thread):
//!
//! **WebSocket push (`GET /ws`)** is the browser's live channel
//! (docs/internal/web-ui.md §3.1). Hand-rolled RFC 6455, matching the
//! hand-rolled HTTP below. On connect the client receives
//! `{"type":"hello","seq":0,"scene":<full scene>}`; afterwards the server
//! pushes `{"type":"frame","seq":N,"changed":{<path>:<value>,...}}` frames
//! ONLY when something changed, where each path replaces its value wholesale:
//! `"w"`, `"h"`, `"theme"`, `"clipboard"`, `"regions.<key>"` — except panes,
//! which diff one level deeper as `"regions.panes.<index>"` plus
//! `"regions.panes.len"` when the pane count changes (panes carry the bulk of
//! the bytes; typing resends only the changed pane). Client→server input is
//! JSON text frames, tagged `{"type":"key"|"mouse"|"action"|"widget"|
//! "settings"|"kbedit"|"paste"|"resize"}` — each carrying the same fields as
//! the HTTP POST bodies below.
//!
//! Session model: MANY WebSocket clients, ALL mirroring the one single-threaded
//! editor (shared-view). Every `/ws` upgrade JOINS the client set — a second
//! tab, another device, or the page you load after a server restart all connect
//! and stay live; none is bounced (the old first-come-`409` model locked out
//! the newcomer, so a stale/half-open background tab could hold the single slot
//! until a server restart — the exact "can't load the page" failure this
//! replaced). The scene is built once per tick and pushed to each client as its
//! own region diff; input is accepted from all of them and applied to the one
//! editor (for one user across tabs/devices the interleaving is the point;
//! independent per-client cursors/viewports are the deeper §3.7 work, still
//! PLANNED). Because there is one editor and one grid but clients have
//! different window sizes, the render is fit to the element-wise MIN of every
//! client's viewport (`effective_size`) so it fits all of them — bigger windows
//! letterbox. A liveness heartbeat keeps this honest: the server pings every
//! `WS_PING_INTERVAL` and reaps a client gone silent past `WS_LIVENESS_TIMEOUT`
//! (browsers auto-pong, so a live-but-idle client stays), so a half-open peer —
//! a tab killed without a clean close, a slept laptop — can't linger and, in
//! particular, can't pin the shared grid to a dead small window's size. The
//! `Origin` header, when
//! present, must have the same host as the request's `Host` header (i.e. the
//! request is same-origin) AND that host must be a loopback/LAN literal or our
//! bind address, or the upgrade is rejected with `403 Forbidden`. The
//! same-origin half blocks a plain cross-origin page; the Host-allowlist half
//! blocks DNS rebinding, where an attacker-controlled name resolves to us so
//! `Origin` and `Host` agree (see `origin_host_matches` / `host_is_allowed`).
//! Comparing against `Host` (rather than the bind address) is what lets a
//! wildcard bind like `--web 0.0.0.0:8137` work when reached as
//! `127.0.0.1:8137`; non-browser tools send no Origin and are accepted.
//!
//! **HTTP routes** all keep working (full-scene responses); curl, the Playwright
//! harness and the parity routes depend on them. A mutation made over HTTP
//! reaches a connected WebSocket client as a pushed diff on the next tick.
//! State-mutating `POST`s are gated by the SAME same-origin/Host check as the
//! `/ws` upgrade (in `serve_request`), so a cross-origin browser page can't
//! drive the editor over HTTP — but non-browser callers (curl, the parity
//! harness, the Playwright request API) send no `Origin` and pass:
//!   - `GET /`        → serves the page assembled from `web-ui/` (see `INDEX_HTML`)
//!   - `GET /favicon.ico` → 204
//!   - `GET /state`   → `{ w, h, regions, theme, clipboard }` from the real render
//!   - `POST /key`    → runs the real `Editor::handle_key`, returns `/state`
//!   - `POST /paste`  → `{text}` → the editor's bracketed-paste path, returns `/state`
//!   - `POST /resize` → `{cols, rows}` → `Editor::resize`, returns `/state`
//!   - `POST /mouse` `/action` `/widget` `/settings` `/kbedit` → same pattern
//!   - `POST /step` `/reset` → parity-harness routes (no clipboard attach)

use std::collections::HashMap;
use std::io::{ErrorKind, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Result;
use crossterm::event::{
    Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};
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

/// The web-UI frontend served at `GET /`, embedded at compile time from the
/// page assembled by `build.rs` out of the split sources under `web-ui/`
/// (`shell.html` + `css/*.css` + `js/*.js`, concatenated in filename order).
/// The assembled page is the *only* source — there is no on-disk fallback,
/// so `fresh --web` (and the example bridge) is fully self-contained and
/// behaves identically wherever the binary runs. Editing the frontend
/// therefore requires a rebuild.
const INDEX_HTML: &str = include_str!(concat!(env!("OUT_DIR"), "/webui-index.html"));

/// Cap on the clipboard text exposed in the scene (`ClipboardSync`). Anything
/// larger is truncated at a char boundary — a copy that big is better served
/// by a future dedicated fetch than by riding along on every scene response.
const CLIPBOARD_TEXT_CAP: usize = 1 << 20; // 1 MiB

/// Outbound OS-clipboard mirror (docs/internal/web-ui.md §3.5).
///
/// The editor's copy actions run server-side and land in its internal
/// clipboard (plus OSC 52/arboard for the TUI, which can't reach the
/// browser's clipboard). The bridge exposes that text in the scene as
/// `"clipboard": {"seq": N, "text": "..."}` where `seq` increments whenever
/// the copied text changed since the last scene build; the frontend writes
/// the text to `navigator.clipboard` when it sees a new `seq` (inside the
/// user-activation window of the very keypress/click that did the copy).
///
/// State lives here in the bridge — a hash of the last-seen text — so no new
/// core-editor state is needed; the core only gained the read-only
/// `Editor::clipboard_text()` accessor.
struct ClipboardSync {
    seq: u64,
    last_hash: u64,
}

impl ClipboardSync {
    /// Seed from the editor's current clipboard so the first scene after boot
    /// doesn't replay pre-existing content into the browser's clipboard.
    fn new(editor: &Editor) -> Self {
        Self {
            seq: 0,
            last_hash: Self::hash(editor.clipboard_text()),
        }
    }

    fn hash(text: &str) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut h = std::collections::hash_map::DefaultHasher::new();
        text.hash(&mut h);
        h.finish()
    }

    /// Attach the clipboard object to a scene, bumping `seq` if the editor's
    /// clipboard text changed. The text is never logged.
    fn attach(&mut self, editor: &Editor, scene: &mut Value) {
        let text = editor.clipboard_text();
        let h = Self::hash(text);
        if h != self.last_hash {
            self.last_hash = h;
            self.seq += 1;
        }
        let mut end = text.len().min(CLIPBOARD_TEXT_CAP);
        while !text.is_char_boundary(end) {
            end -= 1;
        }
        scene["clipboard"] = json!({ "seq": self.seq, "text": &text[..end] });
    }
}

/// Construct a fresh editor exactly as the web bridge does: real plugin runtime
/// enabled, init.ts loaded, chrome drawn as a semantic model (not cells). Shared
/// by `run()`, the `/reset` route (scenario isolation) and the parity test
/// runner so all three drive an identical editor.
pub fn build_editor(cols: u16, rows: u16, files: &[PathBuf]) -> Result<Editor> {
    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();
    let mut cfg = config::Config::load_with_layers(&dir_context, &working_dir);
    // Cell-level (TUI) animations would stream as bursts of frame diffs over
    // the bridge; the web frontend implements its own CSS-level motion
    // instead, so they are forced off regardless of the user's config.
    cfg.editor.animations = false;
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
    // chrome to hide. See docs/internal/web-ui.md.
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

/// Tick cadence: rebuild/diff/push the scene at ~25 fps while the editor
/// reports activity (animations / LSP progress / pending timers — the same
/// hint the scene exposes as `regions.poll.active`), and at a relaxed idle
/// cadence otherwise so async events still land within a quarter second with
/// zero input — client or no client.
const TICK_ACTIVE: Duration = Duration::from_millis(40);
const TICK_IDLE: Duration = Duration::from_millis(250);
/// Event-loop sleep. Small while a WS client is connected so its input is
/// picked up within a few ms (input latency is the point of the push
/// transport; the nonblocking accept/read per iteration are near-free, so
/// idle CPU stays ~0% — measured, see docs/internal/web-ui.md §3.1). Longer
/// when nothing is connected.
const SLEEP_CONNECTED: Duration = Duration::from_millis(3);
const SLEEP_IDLE: Duration = Duration::from_millis(25);
/// How long a connection may take to finish sending its request head + body
/// before we drop it (browsers open speculative sockets that never send).
const HTTP_READ_DEADLINE: Duration = Duration::from_secs(10);
/// WebSocket liveness heartbeat. We ping the client every `WS_PING_INTERVAL`;
/// a live browser auto-pongs (any inbound frame refreshes the session's
/// last-heard-from clock), so a client that goes silent past
/// `WS_LIVENESS_TIMEOUT` (a few missed pings) is a half-open peer — a tab
/// killed without a clean close, a slept laptop, a dropped network — and is
/// reaped. Without this a dead connection is invisible (no clean FIN ever
/// arrives): it would linger in the client set forever and, worse, keep
/// constraining the shared grid to its (now dead) window's size via
/// `effective_size`, shrinking every live client until a server restart.
const WS_PING_INTERVAL: Duration = Duration::from_secs(15);
const WS_LIVENESS_TIMEOUT: Duration = Duration::from_secs(45);
/// Cap on a buffered HTTP request (head + body). Pastes are the biggest
/// legitimate payload; anything larger is a runaway client.
const HTTP_REQUEST_CAP: usize = 8 << 20;

pub fn run(addr: &str, files: &[PathBuf]) -> Result<()> {
    let (mut cols, mut rows) = DEFAULT_SIZE;
    let mut editor = build_editor(cols, rows, files)?;
    let mut clip = ClipboardSync::new(&editor);

    // Bind the in-process control socket so a `fresh` run inside an embedded
    // terminal can forward opens *and* drive the command channel
    // (`ListCommands` / `RunCommand`) back to this editor — same as the TUI
    // path (main.rs). Web mode previously skipped this, so an agent workspace
    // got a `FRESH_CMD_TOKEN` with no socket to reach. Best-effort: on failure
    // the editor still runs, nested launches just open inline.
    if let Err(e) = crate::server::local_control::start() {
        eprintln!("[webui] local control socket unavailable: {e}");
    }

    let listener = TcpListener::bind(addr)?;
    listener.set_nonblocking(true)?;
    // Host part of the bind address — only the fallback for the WS-upgrade
    // Origin check when a request carries no Host header (the check prefers
    // the request's Host header; see `origin_host_matches`).
    let bind_host = addr.rsplit_once(':').map(|(h, _)| h).unwrap_or(addr);
    eprintln!(
        "fresh web bridge on http://{addr}  (real render pipeline, no mocks; WS push on /ws)"
    );

    // In-flight HTTP requests whose head/body hasn't fully arrived yet (reads
    // are nonblocking so a slow client can never stall the editor loop).
    let mut pending: Vec<PendingConn> = Vec::new();
    // Every connected WebSocket client. All view the SAME single-threaded
    // editor (shared-view mirroring, see the module docs): input is accepted
    // from all, the scene is built once per tick and pushed to each, and the
    // rendered grid is fit to the SMALLEST client's viewport so it fits every
    // window (bigger windows letterbox). Each client keeps its own diff cache.
    let mut ws: Vec<WsSession> = Vec::new();
    let mut next_tick = Instant::now();

    loop {
        // 0) Drain nested-forward command requests (file/dir opens, and the
        //    `fresh --cmd` command channel) before anything else, exactly like
        //    the TUI loop (main.rs). `pump` is a cheap no-op until `start()`
        //    has bound the socket and never blocks. Its result is folded into
        //    `had_input` below so the queued work is applied and pushed in this
        //    same iteration rather than waiting for the idle tick.
        let control_changed = crate::server::local_control::pump(&mut editor);

        // 1) Drain input from EVERY client FIRST (this also detects clients that
        //    closed, e.g. a browser reload). `resize` messages update the
        //    sending client's own wanted grid size (the effective size is the
        //    element-wise min across clients, recomputed in step 4b) and are NOT
        //    applied to the editor directly — every other message joins one
        //    shared input batch applied to the single editor. Read/parse errors
        //    drop just that client; the editor and the loop live on, and its
        //    reconnect gets a fresh hello. A dropped client may have been the
        //    size constraint, so recompute the effective size afterwards.
        let mut inputs: Vec<Value> = Vec::new();
        let mut resize_dirty = false;
        {
            let mut i = 0;
            while i < ws.len() {
                match ws[i].drain_messages() {
                    Ok(msgs) => {
                        for m in &msgs {
                            let v: Value =
                                serde_json::from_str(m).unwrap_or_else(|_| json!({}));
                            if v.get("type").and_then(|t| t.as_str()) == Some("resize") {
                                if ws[i].note_resize(&v) {
                                    resize_dirty = true;
                                }
                            } else {
                                inputs.push(v);
                            }
                        }
                        i += 1;
                    }
                    Err(e) => {
                        eprintln!("[webui] ws client disconnected: {e}");
                        ws.remove(i);
                        resize_dirty = true;
                    }
                }
            }
        }

        // 2) Accept new connections into the pending pool (nonblocking).
        loop {
            match listener.accept() {
                Ok((stream, _)) => {
                    // The pump relies on nonblocking reads; a socket we can't
                    // configure is dropped rather than risked stalling the
                    // editor loop. nodelay is folded in — it never fails on a
                    // live socket, and a dead one belongs on the floor anyway.
                    if stream.set_nonblocking(true).is_err() || stream.set_nodelay(true).is_err() {
                        continue;
                    }
                    pending.push(PendingConn {
                        stream,
                        buf: Vec::new(),
                        since: Instant::now(),
                    });
                }
                Err(e) if e.kind() == ErrorKind::WouldBlock => break,
                Err(e) => {
                    eprintln!("accept error: {e}");
                    break;
                }
            }
        }

        // 3) Pump pending connections; serve the complete ones. A `/ws`
        //    upgrade JOINS the client set (or gets 403); anything else runs
        //    through the HTTP routes, blocking only for its short localhost
        //    response write. An HTTP route that mutated the editor (input
        //    routes, /step, /reset) counts as input so connected WS clients
        //    get the resulting diff pushed this pass.
        let mut http_mutated = false;
        let mut i = 0;
        while i < pending.len() {
            match pump_pending(&mut pending[i]) {
                Pump::NeedMore if pending[i].since.elapsed() <= HTTP_READ_DEADLINE => i += 1,
                Pump::NeedMore | Pump::Closed => {
                    pending.remove(i);
                }
                Pump::Ready(req) => {
                    let conn = pending.remove(i);
                    match serve_request(
                        conn.stream,
                        &req,
                        &mut editor,
                        &mut cols,
                        &mut rows,
                        files,
                        &mut clip,
                        bind_host,
                    ) {
                        Ok(Served::WsClient(mut session)) => {
                            // A new client mirrors the same editor. Seed its
                            // wanted size from the current effective grid so it
                            // doesn't momentarily shrink everyone to the default
                            // before its own `resize` lands; its real viewport
                            // arrives in the hello handler's resize a tick later.
                            session.want = Some((cols, rows));
                            ws.push(session);
                        }
                        Ok(Served::Http { mutated }) => http_mutated |= mutated,
                        Err(e) => eprintln!("conn error: {e}"),
                    }
                }
            }
        }

        // 4) Apply the whole (non-resize) input batch in order via the same
        //    dispatch the HTTP routes use — but do NOT render per message.
        //    Input from several mirrored clients interleaves here; for one user
        //    across tabs/devices that's the point, and the editor applies each
        //    message atomically regardless of origin.
        let mut applied_input = false;
        for v in &inputs {
            let kind = v.get("type").and_then(|t| t.as_str()).unwrap_or("");
            if apply_message(&mut editor, kind, v, &mut cols, &mut rows) {
                applied_input = true;
            } else {
                eprintln!("[webui] ignoring unknown ws message type {kind:?}");
            }
        }

        // 4b) Recompute the effective grid size — the element-wise min of every
        //     client's wanted size, so the render fits the smallest viewport —
        //     whenever a client resized, joined, or left. Resizing the editor
        //     counts as input so the new fit is pushed to everyone this pass.
        let mut resized_editor = false;
        if resize_dirty {
            if let Some((ec, er)) = effective_size(&ws) {
                if (ec, er) != (cols, rows) {
                    cols = ec;
                    rows = er;
                    editor.resize(cols, rows);
                    resized_editor = true;
                }
            }
        }

        // 5) One tick per loop pass at most: immediately after input, else on
        //    the tick deadline. The editor keeps ticking at the idle cadence
        //    even with no client connected (async LSP/plugin/file events must
        //    not stall); with no clients we skip the scene build entirely —
        //    the diff caches belong to the connected sessions, and a reconnect
        //    starts over with a fresh hello anyway.
        let now = Instant::now();
        // `control_changed` (a `fresh --cmd` request applied above) forces the
        // tick + scene push this iteration so command effects show immediately.
        let had_input = applied_input || http_mutated || control_changed || resized_editor;
        if had_input || now >= next_tick {
            let needs_render = tick_only(&mut editor);
            let active_hint = poll_active(&editor);
            // Build the scene ONCE and push a per-client diff to each — but only
            // when something can have changed: input was applied (over WS or an
            // HTTP route), the tick reported needs-render (the TUI's own redraw
            // signal), or time-driven UI is in flight (animations / LSP spinner
            // — `poll_active`, refreshed on the fast cadence below). An
            // unchanged scene diffs to nothing anyway; skipping the build keeps
            // connected-but-idle sessions near zero CPU. Each client's diff is
            // against ITS OWN last-sent cache (they connected at different
            // times), so a shared scene still yields correct per-client frames.
            if !ws.is_empty() && (had_input || needs_render || active_hint) {
                let scene = build_scene(&mut editor, cols, rows, &mut clip);
                let mut i = 0;
                while i < ws.len() {
                    if let Err(e) = ws[i].push_diff(&scene) {
                        eprintln!("[webui] ws push failed, dropping client: {e}");
                        ws.remove(i);
                    } else {
                        i += 1;
                    }
                }
            }
            let interval = if active_hint { TICK_ACTIVE } else { TICK_IDLE };
            next_tick = Instant::now() + interval;
        }

        // 5b) Heartbeat + drain buffered outbound bytes for EACH client — a
        //     periodic ping (and the liveness check that reaps a silent
        //     half-open peer, so a dead small window can't pin everyone's grid
        //     to its size), leftover frame bytes for a slow-but-alive peer, and
        //     pongs queued while draining input. `flush` is nonblocking; a peer
        //     whose backlog blows past the cap is dropped here rather than
        //     allowed to stall the loop. Reaping a client can free the size
        //     constraint, so recompute the effective size next pass by flagging
        //     it (handled via the drop path in step 1 on the following tick).
        {
            let before = ws.len();
            let mut i = 0;
            while i < ws.len() {
                if let Err(e) = ws[i].heartbeat().and_then(|()| ws[i].flush()) {
                    eprintln!("[webui] ws dropped: {e}");
                    ws.remove(i);
                } else {
                    i += 1;
                }
            }
            // A reap here shrank the constraint set; refit next pass.
            if ws.len() != before {
                if let Some((ec, er)) = effective_size(&ws) {
                    if (ec, er) != (cols, rows) {
                        cols = ec;
                        rows = er;
                        editor.resize(cols, rows);
                    }
                }
            }
        }

        // 6) Pace the loop without a busy spin. Stay snappy while any WS client
        //    is connected (input latency) or an HTTP request is mid-assembly
        //    (its bytes usually land one iteration after the accept); idle
        //    slowly otherwise — CPU stays near zero either way.
        std::thread::sleep(if !ws.is_empty() || !pending.is_empty() {
            SLEEP_CONNECTED
        } else {
            SLEEP_IDLE
        });
    }
}

/// The grid size the shared editor should render at: the element-wise MIN of
/// every client's wanted (cols, rows), so the render fits the SMALLEST viewport
/// and every client can see the whole grid (bigger windows letterbox). Clients
/// that haven't reported a size yet (`want == None`) don't constrain it.
/// Returns `None` when no client has reported — the caller then leaves the
/// current size untouched (there's nothing to fit to).
fn effective_size(clients: &[WsSession]) -> Option<(u16, u16)> {
    let mut acc: Option<(u16, u16)> = None;
    for c in clients {
        if let Some((wc, wr)) = c.want {
            acc = Some(match acc {
                Some((ac, ar)) => (ac.min(wc), ar.min(wr)),
                None => (wc, wr),
            });
        }
    }
    acc
}

// ---------------------------------------------------------------------------
// HTTP request assembly (nonblocking) + routing
// ---------------------------------------------------------------------------

/// A parsed HTTP request (head + full body). Header names are lowercased.
struct HttpRequest {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

impl HttpRequest {
    fn header(&self, name: &str) -> Option<&str> {
        self.headers
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.as_str())
    }
}

/// A connection whose request bytes are still arriving.
struct PendingConn {
    stream: TcpStream,
    buf: Vec<u8>,
    since: Instant,
}

enum Pump {
    NeedMore,
    Closed,
    Ready(HttpRequest),
}

/// Read whatever is available (nonblocking) and try to parse a complete
/// request out of the buffer.
fn pump_pending(conn: &mut PendingConn) -> Pump {
    let mut tmp = [0u8; 16384];
    loop {
        match conn.stream.read(&mut tmp) {
            Ok(0) => return Pump::Closed,
            Ok(n) => {
                conn.buf.extend_from_slice(&tmp[..n]);
                if conn.buf.len() > HTTP_REQUEST_CAP {
                    return Pump::Closed;
                }
            }
            Err(e) if e.kind() == ErrorKind::WouldBlock => break,
            Err(e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(_) => return Pump::Closed,
        }
    }
    match try_parse_request(&conn.buf) {
        Some(req) => Pump::Ready(req),
        None => Pump::NeedMore,
    }
}

/// Parse one HTTP request if `buf` holds the complete head and body.
fn try_parse_request(buf: &[u8]) -> Option<HttpRequest> {
    let head_end = buf.windows(4).position(|w| w == b"\r\n\r\n")? + 4;
    let head = std::str::from_utf8(&buf[..head_end]).ok()?;
    let mut lines = head.split("\r\n");
    let mut it = lines.next()?.split_whitespace();
    let method = it.next()?.to_string();
    let path = it.next().unwrap_or("/").to_string();
    let mut headers = Vec::new();
    let mut content_length = 0usize;
    for line in lines {
        if let Some((name, val)) = line.split_once(':') {
            let name = name.trim().to_ascii_lowercase();
            let val = val.trim().to_string();
            if name == "content-length" {
                content_length = val.parse().unwrap_or(0);
            }
            headers.push((name, val));
        }
    }
    // A declared body larger than the request cap is a runaway (or hostile)
    // client — reject it before any arithmetic. This guard is also what keeps
    // the `checked_add` below from being the only thing between us and a panic:
    // a `Content-Length: 18446744073709551615` header would otherwise make
    // `head_end + content_length` wrap (release) or overflow-panic (debug), and
    // the wrapped value slipped past the length check and sliced an inverted
    // range — one unauthenticated request that killed the single editor thread.
    if content_length > HTTP_REQUEST_CAP {
        return None;
    }
    let end = head_end.checked_add(content_length)?;
    if buf.len() < end {
        return None; // body still arriving (or never will — dropped on deadline)
    }
    let body = buf[head_end..end].to_vec();
    Some(HttpRequest {
        method,
        path,
        headers,
        body,
    })
}

/// Outcome of serving one complete request.
enum Served {
    /// The request was a successful `/ws` upgrade — this is the new client.
    WsClient(WsSession),
    /// A plain HTTP exchange; `mutated` = the route may have changed editor
    /// state (input routes, /step, /reset), so a connected WS client should
    /// get a diff pushed without waiting for the tick deadline.
    Http { mutated: bool },
}

/// Serve one complete request: a WS upgrade joins the client set; everything else
/// goes through the HTTP routes with `Connection: close`. State-mutating POSTs
/// are gated by the same-origin/Host guard so a foreign page can't drive the
/// editor over HTTP (see below).
#[allow(clippy::too_many_arguments)]
fn serve_request(
    mut stream: TcpStream,
    req: &HttpRequest,
    editor: &mut Editor,
    cols: &mut u16,
    rows: &mut u16,
    files: &[PathBuf],
    clip: &mut ClipboardSync,
    bind_host: &str,
) -> Result<Served> {
    // Serve the response blocking for simplicity — but bound the write. `GET /`
    // returns the whole embedded page (~200 KB), so a client that stops reading
    // (or advertises a tiny/zero window) would otherwise wedge `write_all` on
    // this one editor thread forever. A write deadline drops such a peer instead
    // (the WS handshake's own small write is well under it, and `/ws` then
    // switches back to nonblocking). The read side is already bounded by the
    // pending-pool `HTTP_READ_DEADLINE`.
    stream.set_nonblocking(false)?;
    stream.set_write_timeout(Some(HTTP_READ_DEADLINE))?;
    let wants_ws = req.method == "GET"
        && req.path == "/ws"
        && req
            .header("upgrade")
            .is_some_and(|u| u.to_ascii_lowercase().contains("websocket"));
    if wants_ws {
        return match upgrade_ws(stream, req, editor, *cols, *rows, clip, bind_host)? {
            Some(session) => Ok(Served::WsClient(session)),
            None => Ok(Served::Http { mutated: false }),
        };
    }
    // CSRF / DNS-rebinding guard for state-mutating requests. `upgrade_ws` checks
    // the `/ws` upgrade; the POST routes mutate the editor too, so a cross-origin
    // browser page (which sends its own `Origin`) must be rejected here as well —
    // otherwise the same-origin guard is trivially bypassed by choosing HTTP over
    // WS. It fires only when an `Origin` is present and mismatched/untrusted, so
    // non-browser callers (curl, the Playwright request API, the parity harness)
    // send no `Origin` and pass — the routes stay scriptable. GET stays open: the
    // same-origin policy already stops a foreign page from *reading* a response.
    if req.method == "POST" {
        if let Some(origin) = req.header("origin") {
            if !origin_host_matches(origin, req.header("host"), bind_host) {
                respond(
                    &mut stream,
                    "403 Forbidden",
                    "text/plain",
                    b"origin not allowed",
                )?;
                return Ok(Served::Http { mutated: false });
            }
        }
    }
    let mutated = handle_http(&mut stream, req, editor, cols, rows, files, clip)?;
    Ok(Served::Http { mutated })
}

/// The HTTP routes (full-scene responses). The mutating `POST` routes are
/// reachable only after `serve_request`'s same-origin guard, so a cross-origin
/// page can't drive them; non-browser callers (curl, the Playwright request
/// API, the parity harness `/step` `/reset`) send no `Origin` and pass. Returns
/// whether the route may have mutated editor state.
#[allow(clippy::too_many_arguments)]
fn handle_http(
    stream: &mut TcpStream,
    req: &HttpRequest,
    editor: &mut Editor,
    cols: &mut u16,
    rows: &mut u16,
    files: &[PathBuf],
    clip: &mut ClipboardSync,
) -> Result<bool> {
    let body_json = || serde_json::from_slice::<Value>(&req.body).unwrap_or_else(|_| json!({}));
    match (req.method.as_str(), req.path.as_str()) {
        ("GET", "/") => {
            respond(
                stream,
                "200 OK",
                "text/html; charset=utf-8",
                INDEX_HTML.as_bytes(),
            )?;
            Ok(false)
        }
        ("GET", "/favicon.ico") => {
            respond(stream, "204 No Content", "image/x-icon", b"")?;
            Ok(false)
        }
        ("GET", "/state") => {
            let s = tick_scene(editor, *cols, *rows, clip).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())?;
            Ok(false)
        }
        // Input routes: the route name IS the message kind, and `apply_message`
        // is the same dispatch the WebSocket transport uses — the two transports
        // cannot drift. Each returns the full post-tick scene, exactly as
        // before; a connected WS client gets the mutation pushed as a diff in
        // the same loop pass (the `true` below counts as input).
        (
            "POST",
            p @ ("/key" | "/paste" | "/mouse" | "/action" | "/widget" | "/settings" | "/kbedit"
            | "/resize"),
        ) => {
            apply_message(editor, &p[1..], &body_json(), cols, rows);
            let s = tick_scene(editor, *cols, *rows, clip).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())?;
            Ok(true)
        }
        // Parity-harness routes: apply one scenario step, and reset to a fresh
        // editor so each scenario runs in isolation (mirrors the Rust runner,
        // which builds a fresh editor per scenario).
        ("POST", "/step") => {
            let v = body_json();
            apply_step(editor, &v);
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())?;
            Ok(true)
        }
        ("POST", "/reset") => {
            (*cols, *rows) = DEFAULT_SIZE;
            match build_editor(*cols, *rows, files) {
                Ok(e) => *editor = e,
                Err(err) => eprintln!("reset failed: {err}"),
            }
            let s = scene_json(editor, *cols, *rows).to_string();
            respond(stream, "200 OK", "application/json", s.as_bytes())?;
            Ok(true)
        }
        _ => {
            respond(stream, "404 Not Found", "text/plain", b"not found")?;
            Ok(false)
        }
    }
}

// ---------------------------------------------------------------------------
// Shared input dispatch (HTTP routes + WebSocket messages)
// ---------------------------------------------------------------------------

/// Apply one input message by kind — the single dispatch behind both the HTTP
/// POST routes (kind = route name) and the WS `{"type": kind, ...}` messages.
/// Returns false for unknown kinds. Does NOT render: callers decide when a
/// scene is built (per HTTP request, or once per WS input batch).
fn apply_message(
    editor: &mut Editor,
    kind: &str,
    v: &Value,
    cols: &mut u16,
    rows: &mut u16,
) -> bool {
    match kind {
        "key" => apply_key(editor, v),
        "mouse" => apply_mouse(editor, v),
        "action" => apply_action(editor, v),
        "paste" => apply_paste(editor, v),
        "widget" => apply_widget(editor, v),
        "settings" => apply_settings(editor, v),
        "kbedit" => apply_kbedit(editor, v),
        "resize" => apply_resize(editor, v, cols, rows),
        _ => return false,
    }
    true
}

/// Run a named editor action (with optional args) through the real dispatch.
fn apply_action(editor: &mut Editor, v: &Value) {
    if let Some(name) = v.get("action").and_then(|a| a.as_str()) {
        let args: HashMap<String, Value> = v
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
}

/// Inbound OS clipboard (docs/internal/web-ui.md §3.5/§4). The frontend's
/// document `paste` listener sends the clipboard text in ONE message — long
/// pastes never loop through per-char key events. Delivery reuses the exact
/// path a terminal bracketed paste takes (`Ev::Paste` in app/lifecycle.rs): a
/// focused floating panel / dock text field first, then the buffer / prompt /
/// terminal paste in `paste_text`.
fn apply_paste(editor: &mut Editor, v: &Value) {
    if let Some(text) = v.get("text").and_then(|t| t.as_str()) {
        if !editor.paste_bracketed_into_focused_panel(text) {
            editor.paste_text(text.to_string());
        }
    }
}

/// Native plugin-widget interaction. For the overlay prompt toolbar, a
/// Toggle/Button click forwards the widget `key`; the editor flips the toggle
/// in-spec and fires the plugin's `widget_event` — the exact path a TUI
/// toolbar click takes. Floating/dock widgets deliver the clicked hit by
/// index, running the same path as a TUI cell click.
fn apply_widget(editor: &mut Editor, v: &Value) {
    match v.get("surface").and_then(|s| s.as_str()) {
        Some("toolbar") => {
            if let Some(key) = v.get("key").and_then(|k| k.as_str()) {
                editor.toggle_overlay_toolbar_widget(key);
            }
        }
        Some("panel") => {
            let plugin = v.get("plugin").and_then(|p| p.as_str()).unwrap_or("");
            let panel_id = v.get("panelId").and_then(|p| p.as_u64()).unwrap_or(0);
            // Caret placement from a native text input: the browser
            // positioned its caret on click and reports the byte offset;
            // the host TextEdit (source of truth) follows. Not a widget
            // *event* — no plugin hook fires, exactly like a TUI click
            // that only moves the caret.
            if let Some(byte) = v.get("textCursor").and_then(|b| b.as_u64()) {
                let widget_key = v.get("widgetKey").and_then(|k| k.as_str()).unwrap_or("");
                editor.set_widget_text_cursor(plugin, panel_id, widget_key, byte as usize);
                return;
            }
            let hit_index = v
                .get("hitIndex")
                .and_then(|i| i.as_u64())
                .map(|i| i as usize);
            // Preferred shape: the hit's IDENTITY (widgetKey + eventType +
            // payload) with the raw index as tiebreaker — robust against the
            // hits list being regenerated (or windowed to the TUI viewport)
            // between the pushed frame and the click. The bare-index shape
            // stays for compat (curl, older clients).
            if let Some(event_type) = v.get("eventType").and_then(|e| e.as_str()) {
                let widget_key = v.get("widgetKey").and_then(|k| k.as_str()).unwrap_or("");
                let payload = v.get("payload").cloned().unwrap_or_else(|| json!({}));
                editor.deliver_widget_hit_semantic(
                    plugin, panel_id, widget_key, event_type, &payload, hit_index,
                );
            } else if let Some(idx) = hit_index {
                editor.deliver_widget_hit_by_index(plugin, panel_id, idx);
            }
        }
        _ => {}
    }
}

/// Native Settings interaction: the frontend sends the `SettingsHit` it
/// rendered (kind + indices); we run the SAME dispatch a TUI cell click would
/// (`dispatch_settings_hit`). Entry (add/edit) sub-dialog interactions take a
/// separate semantic path — the dialog is its own stacked state, not a
/// main-panel item.
fn apply_settings(editor: &mut Editor, v: &Value) {
    let a = v.get("a").and_then(|x| x.as_u64()).unwrap_or(0) as usize;
    let bb = v.get("b").and_then(|x| x.as_u64()).unwrap_or(0) as usize;
    let dbl = v.get("double").and_then(|x| x.as_bool()).unwrap_or(false);
    use crate::view::settings::SettingsHit as H;
    let kind = v.get("kind").and_then(|k| k.as_str()).unwrap_or("");
    if kind == "entryItem" {
        editor.entry_dialog_select_item(a);
        return;
    }
    if kind == "entryButton" {
        let btn = v.get("button").and_then(|x| x.as_str()).unwrap_or("cancel");
        editor.entry_dialog_activate_button(btn);
        return;
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
}

/// Native keybinding-editor click: select the display row the frontend
/// rendered (same as a TUI row click). Other interactions are keyboard.
fn apply_kbedit(editor: &mut Editor, v: &Value) {
    if let Some(a) = v.get("a").and_then(|x| x.as_u64()) {
        editor.kbedit_select_display_row(a as usize);
    }
}

/// `{cols, rows}` → the real `Editor::resize`, updating the bridge's size.
fn apply_resize(editor: &mut Editor, v: &Value, cols: &mut u16, rows: &mut u16) {
    if let Some(c) = v.get("cols").and_then(|x| x.as_u64()) {
        *cols = (c as u16).clamp(20, 400);
    }
    if let Some(r) = v.get("rows").and_then(|x| x.as_u64()) {
        *rows = (r as u16).clamp(8, 200);
    }
    editor.resize(*cols, *rows);
}

// ---------------------------------------------------------------------------
// WebSocket: hand-rolled RFC 6455 (handshake, frame codec, session + diffs)
// ---------------------------------------------------------------------------

/// Sec-WebSocket-Accept = base64(SHA1(key + RFC 6455 GUID)).
fn ws_accept_key(key: &str) -> String {
    use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
    use sha1::{Digest, Sha1};
    let mut h = Sha1::new();
    h.update(key.as_bytes());
    h.update(b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
    BASE64.encode(h.finalize())
}

/// The bare host of a `host[:port]` or `scheme://host[:port]/path` string,
/// unwrapping an IPv6 bracket (`[::1]:8080` → `::1`).
fn host_only(s: &str) -> &str {
    let rest = s.split("://").nth(1).unwrap_or(s);
    let hostport = rest.split('/').next().unwrap_or(rest);
    if let Some(h) = hostport.strip_prefix('[') {
        h.split(']').next().unwrap_or(h)
    } else {
        hostport
            .rsplit_once(':')
            .map(|(h, _)| h)
            .unwrap_or(hostport)
    }
}

/// A `Host` value we'll accept for a WebSocket upgrade. Matching `Origin`
/// against `Host` alone is not enough: DNS rebinding makes a name the attacker
/// controls (`attacker.com`, its record flipped to `127.0.0.1` after first
/// load) resolve to us, so the browser sends `Host: attacker.com` *and*
/// `Origin: http://attacker.com` — the two agree and the same-origin check
/// passes. Requiring the host to be a literal IP, `localhost`, or the exact
/// address we bound to closes that hole: a rebound *name* is refused, while
/// every legitimate localhost/LAN reach (an IP, or `localhost`, including the
/// wildcard-bind case) still passes. A deployment that needs a real hostname is
/// the §3.7 story (real auth), not this dev-bridge guard.
fn host_is_allowed(host: &str, bind_host: &str) -> bool {
    host.eq_ignore_ascii_case("localhost")
        || host.parse::<std::net::IpAddr>().is_ok()
        || host.eq_ignore_ascii_case(bind_host)
}

/// True when the WebSocket upgrade is same-origin AND reached under a Host we
/// trust: the `Origin` header's host matches the host the browser connected to
/// (the `Host` header), and that host is a loopback/LAN literal or our bind
/// address (see `host_is_allowed` — the DNS-rebinding guard). A page on another
/// origin sends its own `Origin` while connecting to our `Host`, so the two
/// disagree and it's rejected.
///
/// Comparing against `Host` (not the bind address) is what makes wildcard
/// binds work: with `--web 0.0.0.0:8137` the browser reaches the server as
/// `127.0.0.1:8137` (or a LAN IP), and its `Origin` host matches that `Host`,
/// even though neither equals the literal bind host `0.0.0.0`. When no `Host`
/// header is present we fall back to the bind host. Non-browser tools send no
/// `Origin` and never reach this check.
fn origin_host_matches(origin: &str, host_header: Option<&str>, bind_host: &str) -> bool {
    let bind = host_only(bind_host);
    let target = host_only(host_header.unwrap_or(bind_host));
    host_is_allowed(target, bind) && host_only(origin).eq_ignore_ascii_case(target)
}

/// Handle a `/ws` upgrade request: enforce the Origin policy (403), then
/// handshake (101), switch the socket to nonblocking, and send the full-scene
/// hello. The returned session is added to the mirrored client set by the
/// CALLER (`run`); the upgrade itself never rejects a well-formed same-origin
/// request — every client joins (shared-view mirroring, see the module docs).
#[allow(clippy::too_many_arguments)]
fn upgrade_ws(
    mut stream: TcpStream,
    req: &HttpRequest,
    editor: &mut Editor,
    cols: u16,
    rows: u16,
    clip: &mut ClipboardSync,
    bind_host: &str,
) -> Result<Option<WsSession>> {
    if let Some(origin) = req.header("origin") {
        if !origin_host_matches(origin, req.header("host"), bind_host) {
            respond(
                &mut stream,
                "403 Forbidden",
                "text/plain",
                b"origin not allowed",
            )?;
            return Ok(None);
        }
    }
    let Some(key) = req.header("sec-websocket-key") else {
        respond(
            &mut stream,
            "400 Bad Request",
            "text/plain",
            b"missing Sec-WebSocket-Key",
        )?;
        return Ok(None);
    };
    let resp = format!(
        "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: {}\r\n\r\n",
        ws_accept_key(key)
    );
    stream.write_all(resp.as_bytes())?;
    stream.flush()?;
    stream.set_nonblocking(true)?;
    let mut session = WsSession::new(stream);
    let scene = tick_scene(editor, cols, rows, clip);
    session.send_hello(&scene)?;
    Ok(Some(session))
}

/// One parsed WebSocket frame.
struct WsFrame {
    fin: bool,
    opcode: u8,
    payload: Vec<u8>,
}

/// Cap on a single inbound frame / fragmented message (a paste is the biggest
/// legitimate payload).
const WS_PAYLOAD_CAP: usize = 16 << 20;

/// Parse one frame from the front of `buf`, returning it and the bytes
/// consumed. `None` = incomplete (wait for more bytes); `Err` = malformed.
fn ws_parse_frame(buf: &[u8]) -> Result<Option<(WsFrame, usize)>> {
    if buf.len() < 2 {
        return Ok(None);
    }
    let fin = buf[0] & 0x80 != 0;
    let opcode = buf[0] & 0x0F;
    let masked = buf[1] & 0x80 != 0;
    let (mut len, mut off) = ((buf[1] & 0x7F) as u64, 2usize);
    if len == 126 {
        if buf.len() < 4 {
            return Ok(None);
        }
        len = u16::from_be_bytes([buf[2], buf[3]]) as u64;
        off = 4;
    } else if len == 127 {
        if buf.len() < 10 {
            return Ok(None);
        }
        len = u64::from_be_bytes(buf[2..10].try_into().unwrap());
        off = 10;
    }
    if len > WS_PAYLOAD_CAP as u64 {
        anyhow::bail!("ws frame too large ({len} bytes)");
    }
    let mask = if masked {
        if buf.len() < off + 4 {
            return Ok(None);
        }
        let k = [buf[off], buf[off + 1], buf[off + 2], buf[off + 3]];
        off += 4;
        Some(k)
    } else {
        None
    };
    let len = len as usize;
    if buf.len() < off + len {
        return Ok(None);
    }
    let mut payload = buf[off..off + len].to_vec();
    if let Some(k) = mask {
        // Client→server frames are always masked; unmask in place.
        for (i, b) in payload.iter_mut().enumerate() {
            *b ^= k[i % 4];
        }
    }
    Ok(Some((
        WsFrame {
            fin,
            opcode,
            payload,
        },
        off + len,
    )))
}

/// Encode one server→client frame (unmasked, 7/16/64-bit lengths — scenes are
/// ~41 KB, so 16-bit is the common case).
fn ws_encode(opcode: u8, payload: &[u8]) -> Vec<u8> {
    let mut f = Vec::with_capacity(payload.len() + 10);
    f.push(0x80 | (opcode & 0x0F));
    match payload.len() {
        n if n < 126 => f.push(n as u8),
        n if n <= 0xFFFF => {
            f.push(126);
            f.extend_from_slice(&(n as u16).to_be_bytes());
        }
        n => {
            f.push(127);
            f.extend_from_slice(&(n as u64).to_be_bytes());
        }
    }
    f.extend_from_slice(payload);
    f
}

/// Cap on a session's buffered-but-unsent outbound bytes. A peer that stops
/// draining its socket makes `outbuf` grow (frames are queued, never blocked
/// on); past this the client is behind beyond recovery and is dropped rather
/// than buffered without bound. Sized well above a single max frame so a brief
/// stall is absorbed, not a real one.
const WS_OUTBUF_CAP: usize = 8 << 20; // 8 MiB

/// THE WebSocket client, plus the diff cache backing the region-diff protocol
/// (see the module docs). The cache holds an 8-byte fingerprint of every unit
/// last sent — top-level scalars, each `regions.<key>` except panes, and each
/// pane by index (see `value_fingerprint`) — so change detection costs one
/// streaming hash per unit and no persistent per-frame allocation, rather than
/// keeping a full serialized copy (panes are tens of KB each). It is written
/// ONLY by this session's hello/push path — HTTP routes build their own full
/// scenes independently and never touch it.
struct WsSession {
    stream: TcpStream,
    /// Inbound bytes not yet parsed into complete frames.
    inbuf: Vec<u8>,
    /// Encoded outbound frames the socket hasn't accepted yet. Writes are
    /// always nonblocking (`flush`); a slow peer's backlog lands here instead
    /// of stalling the single editor loop.
    outbuf: Vec<u8>,
    /// Accumulated payload of an in-flight fragmented message.
    frag: Vec<u8>,
    frag_text: bool,
    seq: u64,
    top: HashMap<&'static str, u64>,
    regions: HashMap<String, u64>,
    panes: Vec<u64>,
    /// Liveness clock: the last time ANY inbound bytes arrived (input, a pong,
    /// a ping — all count). Refreshed in `drain_messages`; read by `heartbeat`
    /// to reap a peer that has gone silent past `WS_LIVENESS_TIMEOUT`.
    last_recv: Instant,
    /// When we last sent a heartbeat ping — throttles them to `WS_PING_INTERVAL`.
    last_ping: Instant,
    /// This client's wanted grid size (cols, rows), from its `resize` messages.
    /// The bridge fits the editor to the element-wise MIN across all clients so
    /// the render fits every viewport (`effective_size`); `None` until the
    /// client first reports (seeded to the current effective size on join so it
    /// doesn't transiently shrink the grid before its real viewport arrives).
    want: Option<(u16, u16)>,
}

/// Fingerprint a JSON value for change detection by streaming its serialization
/// through a hasher — no `String` is allocated, and the cache stores 8 bytes
/// instead of a full serialized copy. Serialization is deterministic here
/// (scene objects are built key-by-key), so an unchanged unit hashes the same.
/// A 64-bit SipHash collision (a changed unit that hashes identically, so its
/// frame is skipped) is astronomically unlikely and would self-correct on the
/// next real change or reconnect — an acceptable trade for a pure render cache.
fn value_fingerprint(v: &Value) -> u64 {
    use std::hash::Hasher;
    struct HashWriter(std::collections::hash_map::DefaultHasher);
    impl std::io::Write for HashWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.write(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut hw = HashWriter(std::collections::hash_map::DefaultHasher::new());
    // Writing to a hasher never fails, and Value serialization is infallible —
    // `drop` the (always-Ok) result without a `let _` (denied for must-use).
    drop(serde_json::to_writer(&mut hw, v));
    hw.0.finish()
}

/// Top-level scene keys diffed as single units ("regions" is handled per key).
const TOP_KEYS: [&str; 5] = ["w", "h", "theme", "clipboard", "windowId"];

impl WsSession {
    fn new(stream: TcpStream) -> Self {
        Self {
            stream,
            inbuf: Vec::new(),
            outbuf: Vec::new(),
            frag: Vec::new(),
            frag_text: false,
            seq: 0,
            top: HashMap::new(),
            regions: HashMap::new(),
            panes: Vec::new(),
            last_recv: Instant::now(),
            last_ping: Instant::now(),
            want: None,
        }
    }

    /// Record this client's wanted grid size from a `resize` message, applying
    /// the same clamps as the HTTP `/resize` route (`apply_resize`). Returns
    /// true if the wanted size changed — the caller then recomputes the
    /// effective (min-across-clients) size and resizes the shared editor.
    fn note_resize(&mut self, v: &Value) -> bool {
        let c = v
            .get("cols")
            .and_then(|x| x.as_u64())
            .map(|c| (c as u16).clamp(20, 400));
        let r = v
            .get("rows")
            .and_then(|x| x.as_u64())
            .map(|r| (r as u16).clamp(8, 200));
        // Keep whichever dimension the message carried; a resize always sends
        // both, but tolerate a partial one by falling back to the last value.
        let cur = self.want.unwrap_or(DEFAULT_SIZE);
        let next = (c.unwrap_or(cur.0), r.unwrap_or(cur.1));
        if self.want != Some(next) {
            self.want = Some(next);
            true
        } else {
            false
        }
    }

    /// Heartbeat, called once per loop pass. Reap the peer if it has gone
    /// silent past the liveness deadline (a half-open connection never sends a
    /// clean close, so this is the only thing that frees its slot), and
    /// otherwise ping on the interval so a live-but-idle browser keeps
    /// auto-ponging and stays counted as alive. The ping is enqueued, not
    /// written — the caller's `flush` sends it, so this never blocks.
    fn heartbeat(&mut self) -> std::io::Result<()> {
        let now = Instant::now();
        if now.duration_since(self.last_recv) > WS_LIVENESS_TIMEOUT {
            return Err(std::io::Error::new(
                ErrorKind::TimedOut,
                "ws peer silent past liveness deadline (half-open); reaping",
            ));
        }
        if now.duration_since(self.last_ping) >= WS_PING_INTERVAL {
            self.last_ping = now;
            self.enqueue(0x9, b""); // ping (empty payload)
        }
        Ok(())
    }

    /// `{"type":"hello","seq":0,"scene":<full scene>}` — sent once per
    /// connection; seeds the diff cache and restarts `seq`.
    fn send_hello(&mut self, scene: &Value) -> std::io::Result<()> {
        self.seq = 0;
        self.diff(scene); // seed the cache; the hello carries the full scene
        let msg = json!({ "type": "hello", "seq": 0, "scene": scene }).to_string();
        self.enqueue(0x1, msg.as_bytes());
        self.flush()
    }

    /// Diff `scene` against the last-sent one and push a
    /// `{"type":"frame","seq":N,"changed":{...}}` if anything changed.
    fn push_diff(&mut self, scene: &Value) -> std::io::Result<()> {
        let changed = self.diff(scene);
        if changed.is_empty() {
            return self.flush(); // still drain any backlog from earlier frames
        }
        self.seq += 1;
        let msg = json!({ "type": "frame", "seq": self.seq, "changed": Value::Object(changed) })
            .to_string();
        self.enqueue(0x1, msg.as_bytes());
        self.flush()
    }

    /// Compute the changed-paths map and update the cache. A changed value
    /// replaces the previous one wholesale (null is a legal value — regions
    /// are frequently null).
    fn diff(&mut self, scene: &Value) -> serde_json::Map<String, Value> {
        let mut changed = serde_json::Map::new();
        for k in TOP_KEYS {
            let v = scene.get(k).cloned().unwrap_or(Value::Null);
            let h = value_fingerprint(&v);
            if self.top.get(k) != Some(&h) {
                self.top.insert(k, h);
                changed.insert(k.to_string(), v);
            }
        }
        let empty = serde_json::Map::new();
        let regions = scene
            .get("regions")
            .and_then(|r| r.as_object())
            .unwrap_or(&empty);
        for (k, v) in regions {
            if k == "panes" {
                continue; // diffed one level deeper below
            }
            let h = value_fingerprint(v);
            if self.regions.get(k) != Some(&h) {
                self.regions.insert(k.clone(), h);
                changed.insert(format!("regions.{k}"), v.clone());
            }
        }
        // Panes carry the bulk of the bytes — diff per pane so typing resends
        // only the changed one, plus a `len` entry when the count changes.
        let panes: &[Value] = regions
            .get("panes")
            .and_then(|p| p.as_array())
            .map(|v| v.as_slice())
            .unwrap_or(&[]);
        let new_hashes: Vec<u64> = panes.iter().map(value_fingerprint).collect();
        for (i, (p, h)) in panes.iter().zip(&new_hashes).enumerate() {
            if self.panes.get(i) != Some(h) {
                changed.insert(format!("regions.panes.{i}"), p.clone());
            }
        }
        if new_hashes.len() != self.panes.len() {
            changed.insert("regions.panes.len".to_string(), json!(new_hashes.len()));
        }
        self.panes = new_hashes;
        changed
    }

    /// Queue one encoded frame for sending. Never touches the socket — `flush`
    /// does, nonblocking — so enqueuing can't stall the editor loop.
    fn enqueue(&mut self, opcode: u8, payload: &[u8]) {
        self.outbuf.extend_from_slice(&ws_encode(opcode, payload));
    }

    /// Write as much of `outbuf` as the kernel will take right now (nonblocking)
    /// and keep the rest for the next loop pass. This is the only place the
    /// socket is written, and it never blocks: a slow/stalled peer just leaves
    /// bytes queued. A backlog past `WS_OUTBUF_CAP` means the peer isn't
    /// draining at all — error out so the caller drops the client instead of
    /// buffering unboundedly.
    fn flush(&mut self) -> std::io::Result<()> {
        if self.outbuf.len() > WS_OUTBUF_CAP {
            return Err(std::io::Error::new(
                ErrorKind::Other,
                "ws outbound backlog exceeded cap; peer not draining",
            ));
        }
        let mut sent = 0;
        while sent < self.outbuf.len() {
            match self.stream.write(&self.outbuf[sent..]) {
                Ok(0) => return Err(std::io::Error::from(ErrorKind::WriteZero)),
                Ok(n) => sent += n,
                Err(e) if e.kind() == ErrorKind::WouldBlock => break,
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            }
        }
        if sent > 0 {
            self.outbuf.drain(..sent);
        }
        Ok(())
    }

    /// Drain ALL pending inbound frames (nonblocking; `WouldBlock` ends the
    /// drain) into complete text messages — the input batch. Control frames
    /// are handled here (ping→pong, pong ignored, close→echo + `Err` so the
    /// caller drops the client); continuation frames are accumulated
    /// defensively.
    fn drain_messages(&mut self) -> std::io::Result<Vec<String>> {
        let mut tmp = [0u8; 16384];
        loop {
            match self.stream.read(&mut tmp) {
                Ok(0) => return Err(std::io::Error::from(ErrorKind::ConnectionAborted)),
                Ok(n) => {
                    // Any inbound bytes — input, a pong, a ping — prove the peer
                    // is alive; refresh the liveness clock `heartbeat` reads.
                    self.last_recv = Instant::now();
                    self.inbuf.extend_from_slice(&tmp[..n]);
                    if self.inbuf.len() > WS_PAYLOAD_CAP * 2 {
                        return Err(std::io::Error::from(ErrorKind::InvalidData));
                    }
                }
                Err(e) if e.kind() == ErrorKind::WouldBlock => break,
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            }
        }
        let mut out = Vec::new();
        loop {
            let (frame, used) = match ws_parse_frame(&self.inbuf) {
                Ok(Some(x)) => x,
                Ok(None) => break,
                Err(e) => return Err(std::io::Error::new(ErrorKind::InvalidData, e.to_string())),
            };
            self.inbuf.drain(..used);
            match frame.opcode {
                0x1 | 0x2 => {
                    if frame.fin {
                        if frame.opcode == 0x1 {
                            out.push(String::from_utf8_lossy(&frame.payload).into_owned());
                        } // binary frames are not part of the protocol; ignore
                    } else {
                        self.frag = frame.payload;
                        self.frag_text = frame.opcode == 0x1;
                    }
                }
                0x0 => {
                    self.frag.extend_from_slice(&frame.payload);
                    if self.frag.len() > WS_PAYLOAD_CAP {
                        return Err(std::io::Error::from(ErrorKind::InvalidData));
                    }
                    if frame.fin {
                        if self.frag_text {
                            out.push(String::from_utf8_lossy(&self.frag).into_owned());
                        }
                        self.frag = Vec::new();
                    }
                }
                0x8 => {
                    // Close: echo it (best effort — the peer may already be
                    // gone, and we're dropping the client either way) and
                    // report the disconnect.
                    self.enqueue(0x8, &frame.payload);
                    drop(self.flush());
                    return Err(std::io::Error::new(
                        ErrorKind::ConnectionAborted,
                        "client sent close",
                    ));
                }
                0x9 => self.enqueue(0xA, &frame.payload), // ping → queue a pong
                0xA => {}                                 // pong: ignore
                _ => {}                                   // unknown opcode: ignore
            }
        }
        Ok(out)
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
/// timers, step animations) exactly as the TUI event loop does, without
/// building a scene. Returns the tick's needs-render signal — the same bool
/// the TUI uses to decide whether to redraw (true after an error too, so a
/// failed tick can't wedge the display).
fn tick_only(editor: &mut Editor) -> bool {
    match crate::app::editor_tick(editor, || Ok(())) {
        Ok(needs_render) => needs_render,
        Err(e) => {
            eprintln!("[webui] editor_tick error: {e}");
            true
        }
    }
}

/// The scene's `regions.poll.active` hint, computed straight from the editor:
/// true while something is animating / an LSP spinner is live / a timer is
/// pending. Drives the server's tick cadence (and is still exposed in the
/// scene for the frontend/tests).
fn poll_active(editor: &Editor) -> bool {
    editor.active_window().animations.is_active()
        || editor.active_window().has_active_lsp_progress()
        || editor.next_periodic_redraw_deadline().is_some()
}

/// Build the browser-facing scene (no tick): `scene_json` plus the
/// outbound-clipboard mirror (`ClipboardSync`). The parity harness
/// (`scene_value`) uses `scene_json` directly and carries no clipboard. The
/// clipboard `seq` bumps on actual text changes (hash compare), so it cannot
/// double-increment no matter which path — `GET /state` or WS push — builds
/// the scene.
fn build_scene(editor: &mut Editor, cols: u16, rows: u16, clip: &mut ClipboardSync) -> Value {
    let mut scene = scene_json(editor, cols, rows);
    clip.attach(editor, &mut scene);
    scene
}

/// Tick, then build the browser-facing scene. Called per HTTP request (as
/// always) and for the WS hello; the WS push loop ticks and builds separately
/// so an idle tick doesn't pay for a render.
fn tick_scene(editor: &mut Editor, cols: u16, rows: u16, clip: &mut ClipboardSync) -> Value {
    tick_only(editor);
    build_scene(editor, cols, rows, clip)
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
    // The OpenFile / SaveFileAs / SwitchProject browser is painted as a cell
    // overlay above the prompt row (FileBrowserRenderer). Its band can span
    // chrome the frontend renders natively — with the file explorer open, the
    // name column lands in explorer cells that pane slices never carry — so
    // ship the whole popup band as cells; the frontend draws it as one block
    // (clicks route through the pixel→cell path into the existing
    // file-browser hit-test).
    if let Some(fb) = editor.active_window().file_browser_layout.as_ref() {
        let r = fb.popup_area;
        if r.width > 0 && r.height > 0 {
            if let Some(obj) = palette.as_object_mut() {
                obj.insert("browserRect".to_string(), rect_json(r));
                obj.insert("browserCells".to_string(), cells_json(&buf, r));
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
        // Pacing hint: when something is animating / an LSP spinner is live /
        // a timer is pending, the server ticks fast (TICK_ACTIVE), otherwise
        // idles (TICK_IDLE, just to pick up async LSP/file events). Kept in
        // the scene so tests (and any polling client) can observe it.
        "poll": json!({ "active": poll_active(editor) }),
    });

    // The active window id lets the frontend tell a WORKSPACE SWITCH (dock
    // live-switch: everything changes at once) from an in-place layout change
    // (explorer toggle) — the former gets a hard cut, not layout motion.
    let window_id = editor.active_window.0;
    json!({ "w": w, "h": h, "windowId": window_id, "regions": regions, "theme": theme })
}

/// Map a browser key to a crossterm key and run the real input path.
fn apply_key(editor: &mut Editor, v: &Value) {
    let key = v.get("key").and_then(|k| k.as_str()).unwrap_or("");
    let ctrl = v.get("ctrl").and_then(|b| b.as_bool()).unwrap_or(false);
    let alt = v.get("alt").and_then(|b| b.as_bool()).unwrap_or(false);
    let meta = v.get("meta").and_then(|b| b.as_bool()).unwrap_or(false);
    let shift = v.get("shift").and_then(|b| b.as_bool()).unwrap_or(false);

    let mut code = match key {
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
    // SHIFT handling mirrors what crossterm delivers to the TUI. Keep it on
    // named keys (Tab, arrows, …) and on ASCII *letters*: the browser sends the
    // uppercase form for a shifted letter, and `normalize_key` reconciles the
    // redundant SHIFT — which is exactly what makes Ctrl+Shift+<letter> bindings
    // fire (dropping SHIFT there left `Ctrl+Shift+N` resolving as plain
    // `Ctrl+N`). Do NOT add it for a shifted *symbol* like '!': the shift is
    // already baked into the character, so a bare symbol is what the terminal
    // sends (a spurious SHIFT would diverge and could break symbol input).
    let shift_is_meaningful = match code {
        KeyCode::Char(c) => c.is_ascii_alphabetic(),
        _ => true,
    };
    if shift && shift_is_meaningful {
        mods |= KeyModifiers::SHIFT;
    }
    // A browser reports Shift+Tab as `{key:"Tab", shift:true}`, but crossterm
    // delivers real-terminal Shift+Tab as `KeyCode::BackTab` (carrying SHIFT),
    // and the editor's reverse-navigation paths (Settings dialog focus, popup
    // `select_prev`, `SwitchPane(Prev)`) match on `BackTab`. Fold so the web
    // event is identical to the terminal one — without it, Shift+Tab moved the
    // wrong way (or was a no-op) on the web.
    if code == KeyCode::Tab && mods.contains(KeyModifiers::SHIFT) {
        code = KeyCode::BackTab;
    }
    // Wave-animation dismissal parity with the TUI event loops (main.rs and
    // the daemon server loop): ANY key press dismisses the interactive wave
    // and is CONSUMED — it only stops the show, it doesn't also act on the
    // editor. `KeyEvent::new` sets kind=Press, which the dismissal requires.
    if editor.maybe_dismiss_wave_animation(&Event::Key(KeyEvent::new(code, mods))) {
        return;
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
    // Wave-animation dismissal parity with the TUI event loops: ANY mouse
    // activity (move, click, wheel) dismisses the interactive wave and is
    // CONSUMED. One representative event is enough — the whole batch below
    // carries the same kind/cell.
    if editor.maybe_dismiss_wave_animation(&Event::Mouse(MouseEvent {
        kind,
        column: col,
        row,
        modifiers: mods,
    })) {
        return;
    }
    // Explicit multi-click count from the browser (`event.detail`). The editor
    // detects double/triple clicks itself (`detect_multi_click`) by comparing
    // wall-clock spacing and the exact cell of consecutive Downs — reliable in
    // a terminal, but jitter-sensitive across the HTTP hop and stricter than
    // the browser's own few-pixel slop (two clicks the browser counts as a
    // double can straddle a cell boundary). When the browser already counted,
    // prime the editor's OWN click-tracking state so its detection resolves
    // the same count deterministically: `detect_multi_click` sees a
    // just-now previous click at this cell and bumps `click_count` to the
    // browser's count (2 = double, ≥3 = triple; the editor's word/line
    // selection then runs through its normal, unmodified path).
    let count = v.get("count").and_then(|x| x.as_u64()).unwrap_or(1);
    if count >= 2 && matches!(kind, MouseEventKind::Down(MouseButton::Left)) {
        let w = editor.active_window_mut();
        w.previous_click_time = Some(std::time::Instant::now());
        w.previous_click_position = Some((col, row));
        w.click_count = (count - 1).min(2) as u8;
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

#[cfg(test)]
mod tests {
    use super::{host_only, origin_host_matches, try_parse_request, HTTP_REQUEST_CAP};

    #[test]
    fn absurd_content_length_is_rejected_not_panicked() {
        // Regression: a `Content-Length` near usize::MAX used to make
        // `head_end + content_length` wrap (release) / overflow-panic (debug),
        // and the wrapped value sliced an inverted range — one unauthenticated
        // request that killed the single editor thread. It must now parse to
        // "incomplete" (None), never panic.
        let raw = b"POST /key HTTP/1.1\r\nContent-Length: 18446744073709551615\r\n\r\n";
        assert!(try_parse_request(raw).is_none());
        // A declared body just over the request cap is likewise refused up front.
        let over = format!(
            "POST /key HTTP/1.1\r\nContent-Length: {}\r\n\r\n",
            HTTP_REQUEST_CAP + 1
        );
        assert!(try_parse_request(over.as_bytes()).is_none());
    }

    #[test]
    fn well_formed_request_with_body_still_parses() {
        // A request whose declared body has fully arrived parses (the fix must
        // reject only absurd/oversize lengths, not normal ones).
        let raw = b"POST /action HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello";
        let req = try_parse_request(raw).expect("complete request parses");
        assert_eq!(req.method, "POST");
        assert_eq!(req.path, "/action");
        assert_eq!(req.body, b"hello");
        // A request whose body hasn't fully arrived yet is "incomplete", not an error.
        let partial = b"POST /action HTTP/1.1\r\nContent-Length: 5\r\n\r\nhel";
        assert!(try_parse_request(partial).is_none());
    }

    #[test]
    fn host_only_strips_scheme_port_and_brackets() {
        assert_eq!(host_only("http://127.0.0.1:8137/ws"), "127.0.0.1");
        assert_eq!(host_only("127.0.0.1:8137"), "127.0.0.1");
        assert_eq!(host_only("localhost"), "localhost");
        assert_eq!(host_only("http://[::1]:8137"), "::1");
        assert_eq!(host_only("[::1]:8137"), "::1");
        assert_eq!(host_only("0.0.0.0"), "0.0.0.0");
    }

    #[test]
    fn same_origin_upgrade_is_allowed_for_wildcard_bind() {
        // The regression: `--web 0.0.0.0:8137` reached via 127.0.0.1. The
        // browser's Origin host matches the Host it connected to, so the
        // upgrade must be allowed even though neither equals the bind host.
        assert!(origin_host_matches(
            "http://127.0.0.1:8137",
            Some("127.0.0.1:8137"),
            "0.0.0.0",
        ));
        // Same for a LAN IP served by the same wildcard bind.
        assert!(origin_host_matches(
            "http://192.168.1.5:8137",
            Some("192.168.1.5:8137"),
            "0.0.0.0",
        ));
        // ...and for localhost / IPv6 loopback.
        assert!(origin_host_matches(
            "http://localhost:8137",
            Some("localhost:8137"),
            "0.0.0.0",
        ));
        assert!(origin_host_matches(
            "http://[::1]:8137",
            Some("[::1]:8137"),
            "::",
        ));
    }

    #[test]
    fn cross_origin_upgrade_is_rejected() {
        // A page on another origin connects to our Host but carries its own
        // Origin — the mismatch is the guard that must still fire.
        assert!(!origin_host_matches(
            "http://evil.example.com",
            Some("127.0.0.1:8137"),
            "0.0.0.0",
        ));
        assert!(!origin_host_matches(
            "https://attacker.test:443",
            Some("127.0.0.1:8137"),
            "127.0.0.1",
        ));
    }

    #[test]
    fn dns_rebinding_upgrade_is_rejected() {
        // Rebinding: the attacker's page is served from `attacker.com`, whose
        // DNS record is flipped to 127.0.0.1 after first load. The browser then
        // connects to us sending Host AND Origin both `attacker.com`, so the
        // Origin-equals-Host check passes — but the Host is an untrusted name,
        // not a loopback/LAN literal, so the allowlist must reject it.
        assert!(!origin_host_matches(
            "http://attacker.com",
            Some("attacker.com:8137"),
            "0.0.0.0",
        ));
        assert!(!origin_host_matches(
            "http://attacker.com:8137",
            Some("attacker.com:8137"),
            "127.0.0.1",
        ));
        // A bare IP / localhost Host is still fine (the legitimate reach).
        assert!(origin_host_matches(
            "http://127.0.0.1:8137",
            Some("127.0.0.1:8137"),
            "0.0.0.0",
        ));
    }

    #[test]
    fn falls_back_to_bind_host_without_a_host_header() {
        assert!(origin_host_matches(
            "http://127.0.0.1:8137",
            None,
            "127.0.0.1",
        ));
        assert!(!origin_host_matches(
            "http://127.0.0.1:8137",
            None,
            "10.0.0.1",
        ));
    }
}
