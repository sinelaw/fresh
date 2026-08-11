//! Protocol definitions for client-server communication
//!
//! The protocol uses two channels:
//! - **Data channel**: Raw bytes, no framing (stdin→server, server→stdout)
//! - **Control channel**: JSON messages for out-of-band communication

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Protocol version - must match between client and server
///
/// v2: added `ClientControl::OpenWindow` (open a directory as a new
/// orchestrator window), used by the nested-terminal forwarding path.
/// v3: added the agent channel — an optional `cmd_token` on `ClientHello` (the
/// per-workspace capability token, read from `$FRESH_CMD_TOKEN`, that
/// authorizes a launched agent to drive the editor).
/// v4: the channel carries *scripts* rather than command ids —
/// `ClientControl::RunScript` / `ServerControl::ScriptResult` replace the
/// command-id verbs (`ListCommands` / `RunCommand` / `CommandList`).
pub const PROTOCOL_VERSION: u32 = 4;

/// Terminal size in columns and rows
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TermSize {
    pub cols: u16,
    pub rows: u16,
}

impl TermSize {
    pub fn new(cols: u16, rows: u16) -> Self {
        Self { cols, rows }
    }
}

/// Client hello message sent during handshake
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientHello {
    /// Protocol version
    pub protocol_version: u32,
    /// Client binary version (e.g., "0.15.0")
    pub client_version: String,
    /// Initial terminal size
    pub term_size: TermSize,
    /// Environment variables relevant for rendering
    /// Keys: TERM, COLORTERM, LANG, LC_ALL
    pub env: HashMap<String, Option<String>>,
    /// Per-workspace capability token (from `$FRESH_CMD_TOKEN`), presented so
    /// the server can authorize `RunScript` against this workspace's grant.
    /// `None` for clients that carry no token (a plain attach, an older
    /// client) — script evaluation is then refused.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cmd_token: Option<String>,
}

impl ClientHello {
    /// Create a new ClientHello with current environment
    pub fn new(term_size: TermSize) -> Self {
        let mut env = HashMap::new();

        // Collect terminal-relevant environment variables
        for key in &["TERM", "COLORTERM", "LANG", "LC_ALL"] {
            env.insert(key.to_string(), std::env::var(key).ok());
        }

        Self {
            protocol_version: PROTOCOL_VERSION,
            client_version: env!("CARGO_PKG_VERSION").to_string(),
            term_size,
            env,
            // Populated from `$FRESH_CMD_TOKEN` when present so a client that
            // was spawned inside a Fresh workspace can drive it.
            cmd_token: std::env::var("FRESH_CMD_TOKEN")
                .ok()
                .filter(|t| !t.is_empty()),
        }
    }

    /// Get the TERM value
    pub fn term(&self) -> Option<&str> {
        self.env.get("TERM").and_then(|v| v.as_deref())
    }

    /// Check if truecolor is supported
    pub fn supports_truecolor(&self) -> bool {
        self.env
            .get("COLORTERM")
            .and_then(|v| v.as_deref())
            .map(|v| v == "truecolor" || v == "24bit")
            .unwrap_or(false)
    }
}

/// Server hello message sent in response to ClientHello
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerHello {
    /// Protocol version
    pub protocol_version: u32,
    /// Server binary version
    pub server_version: String,
    /// Daemon identifier (encoded working directory)
    pub session_id: String,
}

impl ServerHello {
    pub fn new(session_id: String) -> Self {
        Self {
            protocol_version: PROTOCOL_VERSION,
            server_version: env!("CARGO_PKG_VERSION").to_string(),
            session_id,
        }
    }
}

/// Version mismatch error response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionMismatch {
    pub server_version: String,
    pub client_version: String,
    /// Suggested action: "restart_server", "upgrade_client"
    pub action: String,
    pub message: String,
}

/// Control messages from client to server
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ClientControl {
    /// Initial handshake
    Hello(ClientHello),
    /// Terminal was resized
    Resize { cols: u16, rows: u16 },
    /// Keepalive ping
    Ping,
    /// Request to detach (keep server running)
    Detach,
    /// Request to quit (shutdown server if last client)
    Quit,
    /// Request to open files in the editor
    OpenFiles {
        files: Vec<FileRequest>,
        #[serde(default)]
        wait: bool,
    },
    /// Request to open a directory as a new orchestrator workspace (a `Window`).
    ///
    /// Unlike `OpenFiles` (which opens buffers in the current window),
    /// this pops a brand-new window rooted at `path` and focuses it.
    /// Used when a `fresh <dir>` is invoked from inside Fresh's own
    /// embedded terminal: the directory becomes a new workspace (a `Window`)
    /// instead of launching a second editor in the terminal.
    OpenWindow { path: String },
    /// Evaluate a plugin script in this editor, on the workspace the caller's
    /// token is bound to (the target window is derived from the token, never
    /// passed in). The server answers with `ServerControl::ScriptResult`
    /// carrying whatever the script's last expression evaluated to. Refused
    /// unless the presented token was granted script access.
    ///
    /// The source is TypeScript (plain JS is a subset), evaluated through the
    /// same pipeline as `init.ts` and "Load Plugin from Buffer": one QuickJS
    /// context, the full `editor` API, `await` available at the top level.
    RunScript {
        source: String,
        /// Load the script under this plugin name instead of a generated
        /// per-request one, replacing any previous load of the same name.
        ///
        /// A script is never unloaded (that would tear down the terminals and
        /// workspaces it was run to create), so an unnamed script that mounts
        /// a panel or subscribes to an event stacks another copy every time it
        /// is submitted. A name makes install idempotent, which is what a
        /// persistent watcher needs. `None` keeps the generated name.
        #[serde(default)]
        as_name: Option<String>,
    },
}

/// A file to open with optional line/column position, range, and hover message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileRequest {
    pub path: String,
    pub line: Option<usize>,
    pub column: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_line: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_column: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

/// Control messages from server to client
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ServerControl {
    /// Handshake response
    Hello(ServerHello),
    /// Version mismatch error
    VersionMismatch(VersionMismatch),
    /// Keepalive pong
    Pong,
    /// Set terminal title
    SetTitle { title: String },
    /// Ring the bell
    Bell,
    /// Server is shutting down
    Quit { reason: String },
    /// Error message
    Error { message: String },
    /// Signal that a --wait operation has completed
    WaitComplete,
    /// Set the system clipboard on the client side
    /// The client should use the specified methods to copy the text
    SetClipboard {
        text: String,
        /// Whether to use OSC 52 escape sequences
        use_osc52: bool,
        /// Whether to use native system clipboard (arboard)
        use_system_clipboard: bool,
    },
    /// Tell this client to suspend itself (SIGTSTP on Unix) and resume on `fg`.
    ///
    /// Dispatched when the user triggers `Action::SuspendProcess` in session
    /// mode: only the client should drop back to the shell — the daemon
    /// keeps running so the editor state is preserved and picked up cleanly
    /// when the client resumes.
    SuspendClient,
    /// Answer to `RunScript`: whether the script ran to completion, the reason
    /// it didn't (refused, failed to compile, threw), and its result value
    /// JSON-encoded for the client to print.
    ScriptResult {
        ok: bool,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        error: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        output: Option<String>,
    },
}

/// Wrapper for control channel messages (used for JSON serialization)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ControlMessage {
    Client(ClientControl),
    Server(ServerControl),
}

/// Read a JSON control message from a reader
pub fn read_control_message<R: std::io::BufRead>(reader: &mut R) -> std::io::Result<String> {
    let mut line = String::new();
    reader.read_line(&mut line)?;
    Ok(line)
}

/// Write a JSON control message to a writer
pub fn write_control_message<W: std::io::Write>(
    writer: &mut W,
    msg: &impl Serialize,
) -> std::io::Result<()> {
    let json = serde_json::to_string(msg).map_err(|e| std::io::Error::other(e.to_string()))?;
    writeln!(writer, "{}", json)?;
    writer.flush()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_client_hello_captures_protocol_version() {
        let hello = ClientHello::new(TermSize::new(80, 24));
        assert_eq!(hello.protocol_version, PROTOCOL_VERSION);
    }

    #[test]
    fn test_client_hello_roundtrip() {
        let hello = ClientHello::new(TermSize::new(120, 40));
        let json = serde_json::to_string(&hello).unwrap();
        let parsed: ClientHello = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.term_size.cols, 120);
        assert_eq!(parsed.term_size.rows, 40);
    }

    #[test]
    fn test_open_window_roundtrip() {
        let msg = ClientControl::OpenWindow {
            path: "/home/user/project".to_string(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        // serde(rename_all = "snake_case") should tag it "open_window"
        assert!(json.contains("\"type\":\"open_window\""));
        match serde_json::from_str::<ClientControl>(&json).unwrap() {
            ClientControl::OpenWindow { path } => assert_eq!(path, "/home/user/project"),
            other => panic!("expected OpenWindow, got {:?}", other),
        }
    }

    #[test]
    fn test_control_messages_use_snake_case_tags() {
        let resize = ClientControl::Resize {
            cols: 100,
            rows: 50,
        };
        let json = serde_json::to_string(&resize).unwrap();
        // serde(rename_all = "snake_case") should produce "resize"
        assert!(json.contains("\"type\":\"resize\""));
    }

    #[test]
    fn test_server_hello_includes_session_id() {
        let hello = ServerHello::new("my-session".to_string());
        assert_eq!(hello.session_id, "my-session");
        assert_eq!(hello.protocol_version, PROTOCOL_VERSION);
    }

    #[test]
    fn test_version_mismatch_roundtrip() {
        let mismatch = VersionMismatch {
            server_version: "1.0.0".to_string(),
            client_version: "2.0.0".to_string(),
            action: "upgrade_server".to_string(),
            message: "Version mismatch".to_string(),
        };
        let msg = ServerControl::VersionMismatch(mismatch);
        let json = serde_json::to_string(&msg).unwrap();
        let parsed: ServerControl = serde_json::from_str(&json).unwrap();

        match parsed {
            ServerControl::VersionMismatch(m) => {
                assert_eq!(m.server_version, "1.0.0");
                assert_eq!(m.client_version, "2.0.0");
            }
            _ => panic!("Expected VersionMismatch"),
        }
    }

    #[test]
    fn test_truecolor_detection() {
        let mut hello = ClientHello::new(TermSize::new(80, 24));

        // No COLORTERM
        hello.env.remove("COLORTERM");
        assert!(!hello.supports_truecolor());

        // truecolor
        hello
            .env
            .insert("COLORTERM".to_string(), Some("truecolor".to_string()));
        assert!(hello.supports_truecolor());

        // 24bit
        hello
            .env
            .insert("COLORTERM".to_string(), Some("24bit".to_string()));
        assert!(hello.supports_truecolor());
    }

    #[test]
    fn test_all_client_control_variants_serialize() {
        let variants: Vec<ClientControl> = vec![
            ClientControl::Hello(ClientHello::new(TermSize::new(80, 24))),
            ClientControl::Resize {
                cols: 100,
                rows: 50,
            },
            ClientControl::Ping,
            ClientControl::Detach,
            ClientControl::Quit,
            ClientControl::OpenFiles {
                files: vec![FileRequest {
                    path: "/test/file.txt".to_string(),
                    line: Some(10),
                    column: Some(5),
                    end_line: None,
                    end_column: None,
                    message: None,
                }],
                wait: false,
            },
            ClientControl::RunScript {
                source: "return editor.activeWindow();".to_string(),
            },
        ];

        for variant in variants {
            let json = serde_json::to_string(&variant).unwrap();
            let _: ClientControl = serde_json::from_str(&json).unwrap();
        }
    }

    #[test]
    fn test_all_server_control_variants_serialize() {
        let variants: Vec<ServerControl> = vec![
            ServerControl::Hello(ServerHello::new("test".to_string())),
            ServerControl::Pong,
            ServerControl::SetTitle {
                title: "Test".to_string(),
            },
            ServerControl::Bell,
            ServerControl::Quit {
                reason: "test".to_string(),
            },
            ServerControl::Error {
                message: "error".to_string(),
            },
            ServerControl::WaitComplete,
            ServerControl::SetClipboard {
                text: "hello".to_string(),
                use_osc52: true,
                use_system_clipboard: true,
            },
            ServerControl::SuspendClient,
            ServerControl::ScriptResult {
                ok: true,
                error: None,
                output: None,
            },
        ];

        for variant in variants {
            let json = serde_json::to_string(&variant).unwrap();
            let _: ServerControl = serde_json::from_str(&json).unwrap();
        }
    }

    #[test]
    fn test_run_script_roundtrip() {
        // Newlines and quotes are the common case for a script, not an edge
        // case — the wire form has to carry a whole source file intact.
        let source = "const w = editor.activeWindow();\nreturn { \"w\": w };\n";
        let msg = ClientControl::RunScript {
            source: source.to_string(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("\"type\":\"run_script\""));
        match serde_json::from_str::<ClientControl>(&json).unwrap() {
            ClientControl::RunScript { source: got } => assert_eq!(got, source),
            other => panic!("expected RunScript, got {:?}", other),
        }
    }

    #[test]
    fn test_hello_cmd_token_optional() {
        // A hello serialized without a token must parse back with `None`.
        let json = r#"{"protocol_version":3,"client_version":"x","term_size":{"cols":80,"rows":24},"env":{}}"#;
        let parsed: ClientHello = serde_json::from_str(json).unwrap();
        assert_eq!(parsed.cmd_token, None);
    }
}
