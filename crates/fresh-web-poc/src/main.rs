//! `fresh-poc`: Fresh split into a data server and a thick web client.
//!
//! The server is responsible for data only: workspaces on hosts, their files
//! and directories (with open documents served through a windowed API over
//! the editor's lazy piece tree, so huge files work), the ordered edit log of
//! each document, terminals, processes, and one connection per language
//! server. One server holds many workspaces (`src/orchestrator.rs`), each
//! with its own host, trust, documents, terminals and servers
//! (`src/session.rs`). Everything about the experience — layout, editing,
//! undo, plugins, and every marker and decoration — lives in the client
//! (`web/`).
//!
//! Usage: `fresh-poc [--port N] [DIR]`, then open the printed URL.

mod agents;
mod authority;
#[macro_use]
mod error;
mod docs;
mod hub;
mod languages;
mod lsp;
mod lsp_shapes;
mod orchestrator;
mod ot;
mod process;
mod session;
mod skins;
mod terminal;
mod themes;
mod workspace;

use anyhow::{anyhow, Result};
use authority::HostSpec;
use orchestrator::{Open, Orchestrator};
use serde_json::{json, Value};
use std::path::PathBuf;
use std::sync::Arc;

mod assets {
    include!(concat!(env!("OUT_DIR"), "/assets.rs"));
    include!(concat!(env!("OUT_DIR"), "/highlight_wasm.rs"));
}

fn content_type(path: &str) -> &'static str {
    match path.rsplit('.').next() {
        Some("html") => "text/html; charset=utf-8",
        Some("js") => "text/javascript; charset=utf-8",
        Some("css") => "text/css; charset=utf-8",
        Some("json") => "application/json",
        Some("svg") => "image/svg+xml",
        Some("png") => "image/png",
        Some("jpg" | "jpeg") => "image/jpeg",
        Some("gif") => "image/gif",
        Some("webp") => "image/webp",
        Some("woff2") => "font/woff2",
        Some("woff") => "font/woff",
        Some("ttf") => "font/ttf",
        Some("otf") => "font/otf",
        _ => "application/octet-stream",
    }
}

/// One of the web client's own files: embedded in the binary, or read from
/// `FRESH_POC_WEB` while developing the client. These are this program's
/// files, on this machine, not the workspace's, so they are the one read that
/// does not go through the authority.
fn asset(path: &str) -> Option<Vec<u8>> {
    if let Some(dir) = std::env::var_os("FRESH_POC_WEB") {
        let p = PathBuf::from(dir).join(path);
        if !p.components().any(|c| c.as_os_str() == "..") {
            return std::fs::read(p).ok();
        }
    }
    assets::ASSETS
        .iter()
        .find(|(p, _)| *p == path)
        .map(|(_, b)| b.to_vec())
}

/// The names directly inside one of the client's own folders ("skins").
fn asset_children(dir: &str) -> Vec<String> {
    if let Some(root) = std::env::var_os("FRESH_POC_WEB") {
        let mut names: Vec<String> = std::fs::read_dir(PathBuf::from(root).join(dir))
            .map(|r| {
                r.flatten()
                    .map(|e| e.file_name().to_string_lossy().into_owned())
                    .collect()
            })
            .unwrap_or_default();
        names.sort();
        return names;
    }
    let prefix = format!("{dir}/");
    let mut names: Vec<String> = assets::ASSETS
        .iter()
        .filter_map(|(p, _)| {
            p.strip_prefix(&prefix)?
                .split('/')
                .next()
                .map(str::to_string)
        })
        .collect();
    names.sort();
    names.dedup();
    names
}

fn main() -> Result<()> {
    let mut port = 8090u16;
    let mut trusted = true;
    let mut host = HostSpec::Local;
    let mut hosts = 0;
    let mut slow = false;
    let mut root: Option<String> = None;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--port" => {
                port = args
                    .next()
                    .and_then(|p| p.parse().ok())
                    .ok_or_else(|| anyhow!("--port N"))?
            }
            "--restricted" => trusted = false,
            "--ssh" => {
                let target = args
                    .next()
                    .ok_or_else(|| anyhow!("--ssh [user@]host[:port]"))?;
                host = HostSpec::Ssh {
                    target,
                    identity: None,
                    options: Vec::new(),
                };
                hosts += 1;
            }
            "--container" => {
                let id = args
                    .next()
                    .ok_or_else(|| anyhow!("--container NAME_OR_ID"))?;
                host = HostSpec::Container { id };
                hosts += 1;
            }
            "--agent" => {
                host = HostSpec::Agent;
                hosts += 1;
            }
            "--slow-network" => slow = true,
            "-h" | "--help" => {
                println!(
                    "usage: fresh-poc [--port N] [--restricted] [--ssh [user@]host[:port] | --container NAME | --agent] [--slow-network] [DIR]\n\n\
                     DIR is the first workspace; the web client opens more.\n\n\
                     --ssh            the workspace is on another machine (files through the editor's remote agent)\n\
                     --container      processes, terminals and language servers run in a running container\n\
                     \x20                (docker exec); DIR must be bind-mounted at the same path in it\n\
                     --agent          the remote agent on this machine, without ssh (the remote code path, locally)\n\
                     --slow-network   add network-like delays to every filesystem call\n\
                     --restricted     start untrusted: no processes, terminals or language servers until trusted"
                );
                return Ok(());
            }
            _ => root = Some(a),
        }
    }
    if hosts > 1 {
        return Err(anyhow!("--ssh, --container and --agent exclude each other"));
    }
    // The first workspace opens as every other does.
    let given = root.unwrap_or_else(|| ".".into());
    // The registry kept last time first, then the command line's folder
    // (which is one of them when it was open then).
    let orch = Arc::new(Orchestrator::new(
        slow,
        Some(orchestrator::Store::local(port)?),
    ));
    orch.restore()?;
    let (first, _) = orch
        .open(&Open::new(host, &given, trusted))
        .map_err(|e| anyhow!("{given}: {e}"))?;
    println!(
        "fresh-poc serving {} ({}) on http://127.0.0.1:{port}/",
        first.ws.root.display(),
        first.ws.authority.name()
    );
    drop(first);
    tokio::runtime::Runtime::new()?.block_on(serve(orch, port))
}

/// Requests from clients: `POST /api` (one JSON operation, run on the
/// blocking pool, since it may wait on a lock, a host or a language server),
/// `GET /events` (the client's Server-Sent Events stream), skins' files,
/// the client's syntax highlighter and its grammars (`/hl/`), and the
/// client's own files.
async fn serve(orch: Arc<Orchestrator>, port: u16) -> Result<()> {
    use axum::routing::{get, post};
    let router = axum::Router::new()
        .route("/api", post(api))
        .route("/events", get(events))
        .route("/skins/{id}/{*path}", get(builtin_skin_file))
        .route("/workspaces/{ws}/skins/{id}/{*path}", get(skin_file))
        .route("/hl/highlight.wasm", get(highlight_wasm))
        .route("/hl/syntaxes.packdump", get(syntaxes_packdump))
        .fallback(get(static_file))
        .with_state(orch.clone());
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", port)).await?;
    tokio::select! {
        r = axum::serve(listener, router) => r?,
        r = stop_signal() => r?,
    }
    // Everything started on the hosts ends with the server.
    tokio::task::spawn_blocking(move || orch.shutdown()).await?;
    Ok(())
}

/// Interrupt or termination.
async fn stop_signal() -> Result<()> {
    let mut term = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())?;
    tokio::select! {
        r = tokio::signal::ctrl_c() => r?,
        _ = term.recv() => {}
    }
    Ok(())
}

type State = axum::extract::State<Arc<Orchestrator>>;

async fn api(
    axum::extract::State(orch): State,
    body: axum::body::Bytes,
) -> axum::response::Response {
    use axum::response::IntoResponse;
    let reply = tokio::task::spawn_blocking(move || {
        match request(&body).and_then(|(op, client, msg)| match client {
            None if op == "hello" => Ok(orch.hello(&msg)),
            None => Err(error::err(
                error::Code::BadRequest,
                format!("{op}: no client (say hello first)"),
            )),
            Some(client) => orch.dispatch(client, &op, &msg),
        }) {
            Ok(v) => json!({ "ok": v }),
            Err(e) => json!({ "error": error::to_json(&e) }),
        }
    })
    .await
    .unwrap_or_else(|e| json!({ "error": {"code": "failed", "message": e.to_string()} }));
    (
        [(axum::http::header::CACHE_CONTROL, "no-store")],
        axum::Json(reply),
    )
        .into_response()
}

/// A request's body: a JSON object with its `op`, and (but for `hello`) the
/// `client` making it (none: absent or null). Anything else is a bad
/// request.
fn request(body: &[u8]) -> anyhow::Result<(String, Option<u64>, Value)> {
    let bad = |what: String| error::err(error::Code::BadRequest, what);
    let msg: Value = serde_json::from_slice(body).map_err(|e| bad(format!("not JSON: {e}")))?;
    let op = msg
        .get("op")
        .and_then(Value::as_str)
        .ok_or_else(|| bad("no op".into()))?
        .to_string();
    let client = match msg.get("client") {
        None | Some(Value::Null) => None,
        Some(c) => Some(
            c.as_u64()
                .ok_or_else(|| bad(format!("{op}: client is not a number")))?,
        ),
    };
    Ok((op, client, msg))
}

#[derive(serde::Deserialize)]
struct EventsQuery {
    client: u64,
}

/// Forgets the client when its event stream goes away.
struct ClientGuard(Arc<Orchestrator>, u64);
impl Drop for ClientGuard {
    fn drop(&mut self) {
        self.0.drop_client(self.1);
    }
}

async fn events(
    axum::extract::State(orch): State,
    axum::extract::Query(q): axum::extract::Query<EventsQuery>,
) -> axum::response::Response {
    use axum::response::sse::{Event, KeepAlive, Sse};
    use axum::response::IntoResponse;
    let Some(rx) = orch.hub.take_stream(q.client) else {
        return (axum::http::StatusCode::NOT_FOUND, "no such client").into_response();
    };
    let guard = ClientGuard(orch, q.client);
    let stream = futures_util::stream::unfold((rx, guard), |(mut rx, guard)| async move {
        let msg = rx.recv().await?;
        Some((
            Ok::<_, std::convert::Infallible>(Event::default().data(msg)),
            (rx, guard),
        ))
    });
    Sse::new(stream)
        .keep_alive(KeepAlive::default())
        .into_response()
}

fn file_response(path: &str, found: Option<Vec<u8>>) -> axum::response::Response {
    use axum::response::IntoResponse;
    match found {
        Some(b) => (
            [
                (axum::http::header::CONTENT_TYPE, content_type(path)),
                (axum::http::header::CACHE_CONTROL, "no-store"),
            ],
            b,
        )
            .into_response(),
        None => (axum::http::StatusCode::NOT_FOUND, "not found").into_response(),
    }
}

async fn static_file(uri: axum::http::Uri) -> axum::response::Response {
    let p = uri.path().trim_start_matches('/');
    let p = if p.is_empty() { "index.html" } else { p };
    file_response(p, asset(p))
}

/// The client's syntax highlighter, built for the browser by `build.rs`.
async fn highlight_wasm() -> axum::response::Response {
    built_file("application/wasm", assets::HIGHLIGHT_WASM)
}

/// The grammars the highlighter loads (`hl_init`): the editor's own syntax
/// set, as the packdump it starts from.
async fn syntaxes_packdump() -> axum::response::Response {
    built_file(
        "application/octet-stream",
        fresh_editor_core::primitives::grammar::default_packdump(),
    )
}

fn built_file(content_type: &'static str, bytes: &'static [u8]) -> axum::response::Response {
    use axum::response::IntoResponse;
    (
        [
            (axum::http::header::CONTENT_TYPE, content_type),
            (axum::http::header::CACHE_CONTROL, "no-store"),
        ],
        bytes,
    )
        .into_response()
}

/// A built-in skin's file.
async fn builtin_skin_file(
    axum::extract::Path((id, path)): axum::extract::Path<(String, String)>,
) -> axum::response::Response {
    let found = skins::file(None, &id, &path);
    file_response(&path, found)
}

/// A skin's file as a workspace sees it: a user skin's is read through the
/// workspace's authority (`src/skins.rs`).
async fn skin_file(
    axum::extract::State(orch): State,
    axum::extract::Path((ws, id, path)): axum::extract::Path<(u64, String, String)>,
) -> axum::response::Response {
    let p = path.clone();
    let found = tokio::task::spawn_blocking(move || {
        let s = orch.session(ws)?;
        skins::file(Some(&s.ws), &id, &p)
    })
    .await
    .ok()
    .flatten();
    file_response(&path, found)
}
