//! The web host: serves the web client and the core's API to it
//! (`docs/internal/web-ui.md`).
//!
//! The web client is an application of its own (`web-ui/`, assembled into
//! one page by `build.rs`). It talks to the core over one WebSocket, `/api`
//! ([`api`]): requests and replies, and events when something it sees
//! changed. Nothing here renders for the browser; the core lays out the text
//! of each view the client opens and sends it as lines, and everything else
//! as data.
//!
//! Two hosts, one [`WebBridge`] (single-threaded either way — the editor is
//! not `Send`, so everything stays on the thread that owns it):
//!
//!   - [`run`] builds its own editor and serves only browsers: the
//!     `webui_server` example, and the only place `POST /reset` may rebuild
//!     the editor.
//!   - `fresh --web` runs the session daemon (`crate::server::EditorServer`)
//!     with the bridge inside it, so browsers and `fresh -a` terminals are
//!     two transports onto one editor.
//!
//! The editor still lays out its own frame at its own size every pass
//! (headless here): plugin render hooks and mounted panels run in it, and the
//! terminals attached to a daemon are drawn from it.
//!
//! Transport is hand-rolled HTTP and RFC 6455, nonblocking throughout: a
//! slow or dead peer is reaped by the heartbeat or dropped past its outbound
//! cap, never allowed to stall the editor loop. Same-origin and `Host`
//! checks guard the upgrade and the mutating routes.

pub mod api;
mod skins;

use std::io::{ErrorKind, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Result;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use fresh_input_parser::Event;
use ratatui::backend::TestBackend;
use ratatui::style::Color;
use ratatui::Terminal;
use serde_json::Value;

use crate::app::Editor;
use crate::config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};

/// The editor's own size when nothing else sets it: the grid its frame is
/// laid out at (terminals attached to a daemon size it there).
pub const DEFAULT_SIZE: (u16, u16) = (140, 44);

/// The web-UI frontend served at `GET /`, embedded at compile time from the
/// page assembled by `build.rs` out of the split sources under `web-ui/`
/// (`shell.html` + `css/*.css` + `js/*.js`, concatenated in filename order).
/// The assembled page is the *only* source — there is no on-disk fallback,
/// so `fresh --web` (and the example bridge) is fully self-contained and
/// behaves identically wherever the binary runs. Editing the frontend
/// therefore requires a rebuild.
const INDEX_HTML: &str = include_str!(concat!(env!("OUT_DIR"), "/webui-index.html"));
/// The page's build, stamped into it by build.rs (its `fresh-build` meta) and
/// sent to every client as the `build` event: a page from another build
/// reloads itself.
pub(crate) const PAGE_BUILD: &str = include_str!(concat!(env!("OUT_DIR"), "/webui-build.txt"));
/// Must match `web-ui/css/10-base.css` `@font-face` and the `shell.html`
/// preload. Versioned, so it can be cached forever.
const NERD_FONT_PATH: &str = "/fonts/symbols-nerd-font-mono-3.5.1.woff2";
/// Symbols Nerd Font Mono, for the cells' private-use glyphs (file icons,
/// theme glyphs). See `fonts/LICENSE-SymbolsNerdFont.md`.
const NERD_FONT: &[u8] = include_bytes!("../../fonts/SymbolsNerdFontMono-Regular.woff2");

/// Construct a fresh editor exactly as the web bridge does: real plugin runtime
/// enabled, init.ts loaded, chrome drawn as a semantic model (not cells). Shared
/// by `run()`, the `/reset` route (scenario isolation) and the parity test
/// runner so all three drive an identical editor.
/// Build the editor the bridge serves.
///
/// `fire_ready` is the startup lifecycle's second hook. A SERVED session wants
/// it — it is where the Orchestrator opens its dock and the welcome screen
/// opens itself. The harness entry points (`POST /reset`, the scene-parity
/// runner) want it OFF: both exist to hand a test a known editor, and the
/// hook's effects arrive asynchronously from the plugin thread, so firing it
/// would let a dock or a welcome tab land on the buffer a tick or two after
/// the reset returned — exactly the race the reset exists to remove.
pub fn build_editor(cols: u16, rows: u16, files: &[PathBuf], fire_ready: bool) -> Result<Editor> {
    let dir_context = DirectoryContext::from_system()?;
    let working_dir = std::env::current_dir().unwrap_or_default();
    let mut cfg = config::Config::load_with_layers(&dir_context, &working_dir);
    // Cell-level animations are the terminal's; the web client has its own.
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

    for f in files {
        if let Err(e) = editor.open_file(f) {
            eprintln!("open_file {f:?} failed: {e}");
        }
    }
    // The second startup lifecycle hook, in the order main.rs and the daemon
    // fire them: `plugins_loaded` once the registry and init.ts are in, then
    // `ready` once the startup files are open. `ready` is the "we have
    // finished starting up" signal — it is where the Orchestrator opens its
    // dock and the welcome screen opens itself — so a bridge that fired only
    // the first served a session with neither, while a directly-launched
    // editor and `fresh -a` had both. Opening the files first is what lets a
    // plugin branching on "is a real file open?" see them rather than race
    // them.
    if fire_ready {
        editor.process_pending_file_opens();
        editor.fire_ready_hook();
    }
    Ok(editor)
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
/// idle CPU stays ~0% — measured, see docs/internal/web-ui.md). Longer
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
/// constraining the editor's home size to its (now dead) window's via
/// `wanted_size` until a server restart.
const WS_PING_INTERVAL: Duration = Duration::from_secs(15);
const WS_LIVENESS_TIMEOUT: Duration = Duration::from_secs(45);
/// Cap on a buffered HTTP request (head + body). Pastes are the biggest
/// legitimate payload; anything larger is a runaway client.
const HTTP_REQUEST_CAP: usize = 8 << 20;

/// What one [`WebBridge::poll`] pass did.
#[derive(Default)]
pub struct BridgePoll {
    /// Browser input reached the editor, so the host should tick and push
    /// this pass instead of waiting for a tick deadline.
    pub mutated: bool,
}

/// The browser-facing half of the web host, decoupled from who owns the
/// editor ([`run`], or the session daemon).
pub struct WebBridge {
    listener: TcpListener,
    /// Host part of the bind address — the fallback for the upgrade's Origin
    /// check when a request carries no Host header.
    bind_host: String,
    /// In-flight HTTP requests whose head/body hasn't fully arrived yet.
    pending: Vec<PendingConn>,
    /// Every connected client. All share the same editor; each has its own
    /// views and its own record of what it was sent ([`api::ApiClient`]).
    ws: Vec<WsSession>,
    /// Files the `/reset` route re-opens in the rebuilt editor.
    files: Vec<PathBuf>,
    /// When clients were last pushed to: what changes with no editor
    /// activity at all (a skin's machine figures, the clock) is pushed on
    /// this slow cadence (`due`).
    last_push: Instant,
    /// Whether `POST /reset` may replace the editor wholesale: only for the
    /// standalone host, which owns its editor.
    allow_reset: bool,
}

impl WebBridge {
    /// Bind the HTTP/WebSocket listener.
    pub fn bind(addr: &str, files: &[PathBuf], allow_reset: bool) -> Result<Self> {
        let listener = TcpListener::bind(addr)?;
        listener.set_nonblocking(true)?;
        let bind_host = addr
            .rsplit_once(':')
            .map(|(h, _)| h)
            .unwrap_or(addr)
            .to_string();
        Ok(Self {
            listener,
            bind_host,
            pending: Vec::new(),
            ws: Vec::new(),
            files: files.to_vec(),
            allow_reset,
            last_push: Instant::now(),
        })
    }

    /// True while a browser is connected or a request is mid-assembly. The
    /// host paces its loop by this, and the daemon keeps its idle timeout
    /// from reaping a session someone is using from a browser.
    pub fn is_busy(&self) -> bool {
        !self.ws.is_empty() || !self.pending.is_empty()
    }

    /// Accept, read and apply everything the browsers sent this pass: input
    /// from every connected client first (which also notices clients that
    /// left), then new connections, then the requests that completed.
    pub fn poll(&mut self, editor: &mut Editor) -> BridgePoll {
        let mut out = BridgePoll::default();
        let mut i = 0;
        while i < self.ws.len() {
            match self.ws[i].drain_messages() {
                Ok(msgs) => {
                    for m in &msgs {
                        match serde_json::from_str::<Value>(m) {
                            Ok(v) => {
                                self.ws[i].api.handle(editor, &v);
                                out.mutated = true;
                            }
                            Err(e) => eprintln!("[webui] unreadable message: {e}"),
                        }
                    }
                    i += 1;
                }
                Err(e) => {
                    eprintln!("[webui] ws client disconnected: {e}");
                    self.ws.remove(i);
                }
            }
        }

        loop {
            match self.listener.accept() {
                Ok((stream, _)) => {
                    // The pump relies on nonblocking reads; a socket we can't
                    // configure is dropped rather than risk stalling the loop.
                    if stream.set_nonblocking(true).is_err() || stream.set_nodelay(true).is_err() {
                        continue;
                    }
                    self.pending.push(PendingConn {
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

        let mut i = 0;
        while i < self.pending.len() {
            match pump_pending(&mut self.pending[i]) {
                Pump::NeedMore if self.pending[i].since.elapsed() <= HTTP_READ_DEADLINE => i += 1,
                Pump::NeedMore | Pump::Closed => {
                    self.pending.remove(i);
                }
                Pump::Ready(req) => {
                    let conn = self.pending.remove(i);
                    match serve_request(
                        conn.stream,
                        &req,
                        editor,
                        &self.files,
                        &self.bind_host,
                        self.allow_reset,
                    ) {
                        Ok(Served::Client(session)) => self.ws.push(*session),
                        Ok(Served::Http { mutated }) => out.mutated |= mutated,
                        Err(e) => eprintln!("conn error: {e}"),
                    }
                }
            }
        }
        out
    }

    /// Whether a push is due with nothing having changed in the editor: a
    /// client is connected and none has been sent anything for a while.
    pub fn due(&self) -> bool {
        const EVERY: Duration = Duration::from_secs(2);
        !self.ws.is_empty() && self.last_push.elapsed() >= EVERY
    }

    /// Lay out the editor's own frame, then push each client what changed
    /// for it. Does not tick — the host owns that. A client whose push fails
    /// is dropped.
    pub fn push(&mut self, editor: &mut Editor, cols: u16, rows: u16) {
        if self.ws.is_empty() {
            self.last_push = Instant::now();
            return;
        }
        render_to_buffer(editor, cols, rows);
        self.push_rendered(editor);
    }

    /// [`Self::push`] for a host that has just drawn the editor's own frame
    /// itself (the session daemon draws it for its terminals): the frame's
    /// side effects — plugin render hooks, mounted panels — are current, so
    /// drawing it a second time here would only cost the time.
    pub fn push_rendered(&mut self, editor: &mut Editor) {
        self.last_push = Instant::now();
        let mut i = 0;
        while i < self.ws.len() {
            let session = &mut self.ws[i];
            for ev in session.api.push(editor) {
                session.enqueue(0x1, ev.to_string().as_bytes());
            }
            if let Err(e) = session.flush() {
                eprintln!("[webui] ws push failed, dropping client: {e}");
                self.ws.remove(i);
            } else {
                i += 1;
            }
        }
    }

    /// Sleep up to `timeout`, but wake as soon as a browser sends something,
    /// a connection arrives, a request's bytes come in, or a backlog can be
    /// written: a host loop waits here instead of sleeping, so input is read
    /// the moment it lands rather than at the next poll: poll(2) on Unix,
    /// WSAPoll on Windows.
    pub fn wait(&self, timeout: Duration) {
        #[cfg(unix)]
        {
            use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
            use std::os::fd::AsFd;
            let mut fds = Vec::with_capacity(1 + self.ws.len() + self.pending.len());
            fds.push(PollFd::new(self.listener.as_fd(), PollFlags::POLLIN));
            for s in &self.ws {
                let mut flags = PollFlags::POLLIN;
                if !s.outbuf.is_empty() {
                    flags |= PollFlags::POLLOUT;
                }
                fds.push(PollFd::new(s.stream.as_fd(), flags));
            }
            for c in &self.pending {
                fds.push(PollFd::new(c.stream.as_fd(), PollFlags::POLLIN));
            }
            let t = PollTimeout::try_from(timeout).unwrap_or(PollTimeout::MAX);
            // An interrupted or failed poll only ends the wait early.
            if let Err(e) = poll(&mut fds, t) {
                tracing::trace!("[webui] wait ended early: {e}");
            }
        }
        #[cfg(windows)]
        {
            use std::os::windows::io::AsRawSocket;
            use windows_sys::Win32::Networking::WinSock::{
                WSAPoll, POLLRDNORM, POLLWRNORM, WSAPOLLFD,
            };
            let fd = |socket: u64, events: i16| WSAPOLLFD {
                fd: socket as usize,
                events,
                revents: 0,
            };
            let mut fds = Vec::with_capacity(1 + self.ws.len() + self.pending.len());
            fds.push(fd(self.listener.as_raw_socket(), POLLRDNORM));
            for s in &self.ws {
                let mut events = POLLRDNORM;
                if !s.outbuf.is_empty() {
                    events |= POLLWRNORM;
                }
                fds.push(fd(s.stream.as_raw_socket(), events));
            }
            for c in &self.pending {
                fds.push(fd(c.stream.as_raw_socket(), POLLRDNORM));
            }
            let ms = timeout.as_millis().min(i32::MAX as u128) as i32;
            // SAFETY: `fds` is a live, initialised array of `fds.len()`
            // entries for the duration of the call; WSAPoll only writes their
            // `revents`. An error or interruption only ends the wait early.
            let r = unsafe { WSAPoll(fds.as_mut_ptr(), fds.len() as u32, ms) };
            if r < 0 {
                tracing::trace!("[webui] wait ended early: WSAPoll error");
            }
        }
        #[cfg(not(any(unix, windows)))]
        std::thread::sleep(timeout);
    }

    /// Heartbeat and drain buffered outbound bytes for each client; a peer
    /// gone silent past the liveness deadline, or whose backlog blows past
    /// the cap, is dropped.
    pub fn maintain(&mut self) {
        let mut i = 0;
        while i < self.ws.len() {
            if let Err(e) = self.ws[i].heartbeat().and_then(|()| self.ws[i].flush()) {
                eprintln!("[webui] ws dropped: {e}");
                self.ws.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

/// Run the standalone host: build an editor, serve it to browsers, forever.
/// `fresh --web` does not come here — it runs the session daemon, which hosts
/// the same [`WebBridge`] so terminals can attach to the same editor.
pub fn run(addr: &str, files: &[PathBuf]) -> Result<()> {
    let (cols, rows) = DEFAULT_SIZE;
    let mut editor = build_editor(cols, rows, files, true)?;

    // The in-process control socket, so a `fresh` run inside an embedded
    // terminal can forward opens and drive the command channel back to this
    // editor — as the TUI does.
    if let Err(e) = crate::server::local_control::start() {
        eprintln!("[webui] local control socket unavailable: {e}");
    }

    let mut bridge = WebBridge::bind(addr, files, true)?;
    eprintln!("fresh web on http://{addr}");

    let mut next_tick = Instant::now();
    loop {
        let control_changed = crate::server::local_control::pump(&mut editor);
        let poll = bridge.poll(&mut editor);
        let now = Instant::now();
        let had_input = poll.mutated || control_changed;
        if had_input || now >= next_tick {
            let needs_render = tick_only(&mut editor);
            let active = poll_active(&editor);
            if had_input || needs_render || active || bridge.due() {
                bridge.push(&mut editor, cols, rows);
            }
            next_tick = Instant::now() + if active { TICK_ACTIVE } else { TICK_IDLE };
        }
        bridge.maintain();
        // Woken by a browser's input, or at the next tick.
        let until_tick = next_tick.saturating_duration_since(Instant::now());
        bridge.wait(until_tick.min(if bridge.is_busy() {
            SLEEP_CONNECTED
        } else {
            SLEEP_IDLE
        }));
    }
}

// ---------------------------------------------------------------------------
// HTTP request assembly (nonblocking) + routing
// ---------------------------------------------------------------------------

/// A parsed HTTP request. Its body has been read off the connection, and no
/// route needs it. Header names are lowercased.
struct HttpRequest {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
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
    Some(HttpRequest {
        method,
        path,
        headers,
    })
}

/// What a completed request turned into.
enum Served {
    /// A WebSocket upgrade to `/api`: a new client.
    Client(Box<WsSession>),
    /// An HTTP route, and whether it changed the editor.
    Http { mutated: bool },
}

fn serve_request(
    mut stream: TcpStream,
    req: &HttpRequest,
    editor: &mut Editor,
    files: &[PathBuf],
    bind_host: &str,
    allow_reset: bool,
) -> Result<Served> {
    // Responses are written blocking, but bounded: a client that stops
    // reading is dropped at the deadline rather than wedging the loop.
    stream.set_nonblocking(false)?;
    stream.set_write_timeout(Some(HTTP_READ_DEADLINE))?;
    let path = req.path.split('?').next().unwrap_or_default();
    let upgrade = req.method == "GET"
        && req
            .header("upgrade")
            .is_some_and(|u| u.to_ascii_lowercase().contains("websocket"));
    if upgrade && path == "/api" {
        return Ok(match ws_handshake(stream, req, bind_host)? {
            Some(stream) => Served::Client(Box::new(WsSession::new(stream))),
            None => Served::Http { mutated: false },
        });
    }
    // CSRF / DNS-rebinding guard for the mutating route: a cross-origin page
    // sends its own Origin and is refused. Non-browser callers send none.
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
    let mutated = match (req.method.as_str(), path) {
        ("GET", "/") => {
            // Never kept: the page must be the one this server was built with.
            respond_with(
                &mut stream,
                "200 OK",
                "text/html; charset=utf-8",
                "Cache-Control: no-store\r\n",
                INDEX_HTML.as_bytes(),
            )?;
            false
        }
        ("GET", NERD_FONT_PATH) => {
            // 1.2 MB: written from its own thread so the editor loop never
            // waits on a slow link. The cloned handle keeps the write deadline.
            let mut font_stream = stream.try_clone()?;
            std::thread::Builder::new()
                .name("webui-font".into())
                .spawn(move || {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = respond_with(
                        &mut font_stream,
                        "200 OK",
                        "font/woff2",
                        "Cache-Control: public, max-age=31536000, immutable\r\n",
                        NERD_FONT,
                    );
                })?;
            false
        }
        // The skins folder (the web client's own theming): its index, and
        // its files read-only.
        ("GET", "/skins/index.json") => {
            let dir = skins::skins_dir(&editor.dir_context().config_dir);
            let body = skins::index_json(&dir);
            respond_with(
                &mut stream,
                "200 OK",
                "application/json",
                "Cache-Control: no-store\r\n",
                body.as_bytes(),
            )?;
            false
        }
        ("GET", p) if p.starts_with("/skins/") => {
            // A skin's images and fonts can be large: read and written from
            // their own thread, as the font is, so the editor loop never
            // waits on the disk or a slow link.
            let dir = skins::skins_dir(&editor.dir_context().config_dir);
            let rel = p["/skins/".len()..].to_string();
            let mut file_stream = stream.try_clone()?;
            std::thread::Builder::new()
                .name("webui-skin-file".into())
                .spawn(move || {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = match skins::read_file(&dir, &rel) {
                        Some((ctype, bytes)) => respond_with(
                            &mut file_stream,
                            "200 OK",
                            ctype,
                            "Cache-Control: no-store\r\n",
                            &bytes,
                        ),
                        None => respond(
                            &mut file_stream,
                            "404 Not Found",
                            "text/plain",
                            b"not found",
                        ),
                    };
                })?;
            false
        }
        ("GET", "/favicon.ico") => {
            respond(&mut stream, "204 No Content", "text/plain", b"")?;
            false
        }
        // A fresh editor, for a harness that wants each case to start clean.
        // Refused when the editor is not ours to throw away (the daemon's
        // carries the recovery session and attached terminals).
        ("POST", "/reset") if !allow_reset => {
            respond(
                &mut stream,
                "409 Conflict",
                "text/plain",
                b"/reset is unavailable in a shared session",
            )?;
            false
        }
        ("POST", "/reset") => {
            let (cols, rows) = DEFAULT_SIZE;
            // The old editor's files are let go first, or the new one would
            // find them open in another editor and get them read-only.
            editor.release_file_locks();
            match build_editor(cols, rows, files, false) {
                Ok(e) => *editor = e,
                Err(err) => eprintln!("reset failed: {err}"),
            }
            respond(&mut stream, "200 OK", "text/plain", b"ok")?;
            true
        }
        _ => {
            respond(&mut stream, "404 Not Found", "text/plain", b"not found")?;
            false
        }
    };
    Ok(Served::Http { mutated })
}

fn respond(stream: &mut TcpStream, status: &str, ctype: &str, body: &[u8]) -> Result<()> {
    respond_with(stream, status, ctype, "", body)
}

/// `respond`, with `extra` (whole `Name: value\r\n` lines) added to the headers.
fn respond_with(
    stream: &mut TcpStream,
    status: &str,
    ctype: &str,
    extra: &str,
    body: &[u8],
) -> Result<()> {
    // No CORS header: the frontend is served from this same origin, so it needs
    // none, and `Access-Control-Allow-Origin: *` would let any site the user
    // visits read `/state` (live buffer contents) cross-origin. Same-origin
    // policy is the protection here.
    let header = format!(
        "HTTP/1.1 {status}\r\nContent-Type: {ctype}\r\nContent-Length: {}\r\n{extra}Connection: close\r\n\r\n",
        body.len()
    );
    stream.write_all(header.as_bytes())?;
    stream.write_all(body)?;
    stream.flush()?;
    Ok(())
}

// ---------------------------------------------------------------------------
// WebSocket: security, handshake, framing
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
/// real auth (out of scope, web-ui.md), not this dev-bridge guard.
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

/// The WebSocket upgrade: same-origin check, then the handshake. `None` when
/// the request was refused (the refusal has been written).
fn ws_handshake(
    mut stream: TcpStream,
    req: &HttpRequest,
    bind_host: &str,
) -> Result<Option<TcpStream>> {
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
    Ok(Some(stream))
}

/// One parsed WebSocket frame.
struct WsFrame {
    fin: bool,
    opcode: u8,
    payload: Vec<u8>,
}

/// Cap on a single inbound frame / fragmented message (a paste is the biggest
/// legitimate payload).
pub(crate) const WS_PAYLOAD_CAP: usize = 16 << 20;

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

/// One connected client: its socket's buffers and liveness, and its side of
/// the API.
struct WsSession {
    stream: TcpStream,
    /// Inbound bytes not yet parsed into complete frames.
    inbuf: Vec<u8>,
    /// Encoded outbound frames the socket hasn't accepted yet. Writes are
    /// nonblocking (`flush`); a slow peer's backlog lands here.
    outbuf: Vec<u8>,
    /// Accumulated payload of an in-flight fragmented message.
    frag: Vec<u8>,
    frag_text: bool,
    /// The last time any inbound bytes arrived; read by `heartbeat`.
    last_recv: Instant,
    /// When we last sent a heartbeat ping.
    last_ping: Instant,
    /// The peer has left; reported on the next drain, after what it sent
    /// before leaving was applied.
    gone: Option<std::io::Error>,
    api: api::ApiClient,
}

impl WsSession {
    fn new(stream: TcpStream) -> Self {
        Self {
            stream,
            inbuf: Vec::new(),
            outbuf: Vec::new(),
            frag: Vec::new(),
            frag_text: false,
            last_recv: Instant::now(),
            last_ping: Instant::now(),
            gone: None,
            api: api::ApiClient::default(),
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
            return Err(std::io::Error::other(
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
        if let Some(why) = self.gone.take() {
            return Err(why);
        }
        let mut tmp = [0u8; 16384];
        // The peer left (end of stream, or a close frame). What it sent
        // before leaving is still applied: a client may send a command and
        // close at once (a host quitting the editor does), and dropping the
        // command with the connection would lose it.
        let mut gone: Option<std::io::Error> = None;
        loop {
            match self.stream.read(&mut tmp) {
                Ok(0) => {
                    gone = Some(std::io::Error::from(ErrorKind::ConnectionAborted));
                    break;
                }
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
                    // report the disconnect once what came before it is in.
                    self.enqueue(0x8, &frame.payload);
                    drop(self.flush());
                    gone = Some(std::io::Error::new(
                        ErrorKind::ConnectionAborted,
                        "client sent close",
                    ));
                    break;
                }
                0x9 => self.enqueue(0xA, &frame.payload), // ping → queue a pong
                0xA => {}                                 // pong: ignore
                _ => {}                                   // unknown opcode: ignore
            }
        }
        match gone {
            Some(why) if out.is_empty() => Err(why),
            Some(why) => {
                self.gone = Some(why);
                Ok(out)
            }
            None => Ok(out),
        }
    }
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

// ---------------------------------------------------------------------------
// The editor side: its frame, its tick, and input it shares with the terminal
// ---------------------------------------------------------------------------

/// Lay out and draw the editor's own frame at its own size, headless: the
/// plugin render hooks and mounted panels run in it, whoever is watching.
fn render_to_buffer(editor: &mut Editor, cols: u16, rows: u16) {
    let mut terminal = Terminal::new(TestBackend::new(cols, rows)).expect("terminal");
    if let Err(e) = terminal.draw(|frame| editor.render(frame)) {
        eprintln!("[webui] render failed: {e}");
    }
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

/// The digit a browser's physical `code` names — `"Digit1"` → `'1'`. Every
/// other code (letters, punctuation, named keys) is left to the reported
/// character, which already matches what a terminal sends.
fn digit_row_char(code: &str) -> Option<char> {
    let rest = code.strip_prefix("Digit")?;
    let mut chars = rest.chars();
    let d = chars.next()?;
    (chars.next().is_none() && d.is_ascii_digit()).then_some(d)
}

/// Map a browser key to a crossterm key and run the real input path.
pub(crate) fn apply_key(editor: &mut Editor, v: &Value) {
    let Some((code, mods)) = key_of(v) else {
        return;
    };
    // Wave-animation dismissal parity with the TUI event loops (main.rs and
    // the daemon server loop): ANY key press dismisses the interactive wave
    // and is CONSUMED — it only stops the show, it doesn't also act on the
    // editor. `KeyEvent::new` sets kind=Press, which the dismissal requires.
    if editor.maybe_dismiss_wave_animation(&Event::key(KeyEvent::new(code, mods))) {
        return;
    }
    if let Err(e) = editor.handle_key(code, mods) {
        eprintln!("handle_key error: {e}");
    }
}

/// A browser key event (`{key, code, ctrl, alt, shift, meta}`) as the key
/// and modifiers a terminal reports for the same press. `None` for a key with
/// no terminal equivalent.
pub(crate) fn key_of(v: &Value) -> Option<(KeyCode, KeyModifiers)> {
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
        s if s.len() >= 2 && s.starts_with('F') && s[1..].parse::<u8>().is_ok() => {
            KeyCode::F(s[1..].parse().ok()?)
        }
        "Insert" => KeyCode::Insert,
        _ => return None,
    };
    // A browser reports the SHIFTED character a key produces: Ctrl+Shift+1
    // arrives as `{key:"!", code:"Digit1"}`. Keybindings — and the kitty-
    // protocol events a terminal delivers — are written against the UNSHIFTED
    // key plus a SHIFT modifier (`set_bookmark` is Ctrl+Shift+0..9), so
    // forwarding '!' left all ten bookmark slots unreachable from a browser.
    // With CONTROL held the character is not text being typed, so the physical
    // `code` is the better witness, and it is what the terminal reports. Plain
    // typing is untouched (no CONTROL), and ALT is excluded so AltGr layouts
    // keep producing their own characters.
    let digit_row = match ctrl && !alt {
        true => v
            .get("code")
            .and_then(|c| c.as_str())
            .and_then(digit_row_char),
        false => None,
    };
    if let Some(d) = digit_row {
        code = KeyCode::Char(d);
    }
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
        // A digit recovered from the physical key above is the UNSHIFTED
        // character, so its SHIFT is real information (Ctrl+Shift+1 →
        // `set_bookmark`), unlike the '!' the browser reported.
        KeyCode::Char(_) if digit_row.is_some() => true,
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
    Some((code, mods))
}

/// Inbound OS clipboard (docs/internal/web-ui.md). The frontend's
/// document `paste` listener sends the clipboard text in ONE message — long
/// pastes never loop through per-char key events. Delivery reuses the exact
/// path a terminal bracketed paste takes (`Ev::Paste` in app/lifecycle.rs): a
/// focused floating panel / dock text field first, then the buffer / prompt /
/// terminal paste in `paste_text`.
pub(crate) fn apply_paste(editor: &mut Editor, v: &Value) {
    if let Some(text) = v.get("text").and_then(|t| t.as_str()) {
        if !editor.paste_bracketed_into_focused_panel(text) {
            editor.paste_text(text.to_string());
        }
    }
}

/// A request from the page's host to close it — a desktop window's close
/// button, say. Unlike the `quit` action, which in a session daemon asks
/// whether to detach the client or stop the session, closing the window that
/// IS the session's only front end means stopping it: what remains to ask
/// about is the unsaved work hot exit cannot keep (and the clean-quit
/// confirmation, for whoever opted into it).
fn apply_quit(editor: &mut Editor) {
    let confirm_clean = editor.config().editor.confirm_quit;
    editor.quit_with_prompts(confirm_clean);
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
    use super::{
        digit_row_char, host_only, origin_host_matches, try_parse_request, HTTP_REQUEST_CAP,
    };

    #[test]
    fn the_page_carries_its_build() {
        let meta = format!(
            r#"<meta name="fresh-build" content="{}">"#,
            super::PAGE_BUILD
        );
        assert!(
            super::INDEX_HTML.contains(&meta),
            "the page is stamped with its build"
        );
        assert!(!super::INDEX_HTML.contains("@BUILD@"));
    }

    /// The CSS `@font-face` and the preload name `NERD_FONT_PATH`, which serves WOFF2.
    #[test]
    fn nerd_font_is_served_where_the_page_loads_it() {
        assert_eq!(&super::NERD_FONT[..4], b"wOF2", "the bundled font is WOFF2");
        let rel = super::NERD_FONT_PATH.trim_start_matches('/');
        assert_eq!(
            super::INDEX_HTML.matches(rel).count(),
            2,
            "the @font-face src and the preload link both load {rel}"
        );
    }

    #[test]
    fn digit_row_is_recovered_from_the_physical_key() {
        // Regression: a browser reports Ctrl+Shift+1 as `{key:"!",
        // code:"Digit1"}`, and the ten `set_bookmark` bindings are written
        // against the unshifted digit plus SHIFT, so the bridge reads the
        // digit off the physical key.
        assert_eq!(digit_row_char("Digit1"), Some('1'));
        assert_eq!(digit_row_char("Digit0"), Some('0'));
        assert_eq!(digit_row_char("Digit9"), Some('9'));
        // Everything else keeps the character the browser reported.
        assert_eq!(digit_row_char("KeyA"), None);
        assert_eq!(digit_row_char("Numpad1"), None);
        assert_eq!(digit_row_char("Digit"), None);
        assert_eq!(digit_row_char("Digit12"), None);
        assert_eq!(digit_row_char("Digitx"), None);
        assert_eq!(digit_row_char(""), None);
    }

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
