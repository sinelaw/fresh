//! Tests for remote module
//!
//! These tests verify the protocol, parsing, and basic functionality
//! without requiring an actual SSH connection.

use super::connection::ConnectionParams;
use super::protocol::*;
use super::AGENT_SOURCE;

#[test]
fn test_protocol_version() {
    assert_eq!(PROTOCOL_VERSION, 1);
}

#[test]
fn test_request_serialization() {
    let req = AgentRequest::new(42, "read", serde_json::json!({"path": "/test.txt"}));
    let line = req.to_json_line();

    assert!(line.ends_with('\n'));
    assert!(line.contains("\"id\":42"));
    assert!(line.contains("\"m\":\"read\""));
    assert!(line.contains("\"/test.txt\""));
}

#[test]
fn test_response_parsing_ready() {
    let json = r#"{"id":0,"ok":true,"v":1}"#;
    let resp: AgentResponse = serde_json::from_str(json).unwrap();

    assert!(resp.is_ready());
    assert_eq!(resp.version, Some(1));
    assert!(!resp.is_final());
    assert!(!resp.is_data());
}

#[test]
fn test_response_parsing_data() {
    let json = r#"{"id":1,"d":{"data":"SGVsbG8="}}"#;
    let resp: AgentResponse = serde_json::from_str(json).unwrap();

    assert!(resp.is_data());
    assert!(!resp.is_final());

    let data = resp.data.unwrap();
    let b64 = data.get("data").unwrap().as_str().unwrap();
    let decoded = decode_base64(b64).unwrap();
    assert_eq!(decoded, b"Hello");
}

#[test]
fn test_response_parsing_result() {
    let json = r#"{"id":1,"r":{"size":100}}"#;
    let resp: AgentResponse = serde_json::from_str(json).unwrap();

    assert!(resp.is_final());
    assert!(resp.result.is_some());
    assert!(resp.error.is_none());

    let result = resp.result.unwrap();
    assert_eq!(result.get("size").unwrap().as_u64().unwrap(), 100);
}

#[test]
fn test_response_parsing_error() {
    let json = r#"{"id":1,"e":"file not found"}"#;
    let resp: AgentResponse = serde_json::from_str(json).unwrap();

    assert!(resp.is_final());
    assert!(resp.result.is_none());
    assert_eq!(resp.error, Some("file not found".to_string()));
}

#[test]
fn test_base64_roundtrip() {
    let data = b"Hello, World! \x00\x01\x02\xff";
    let encoded = encode_base64(data);
    let decoded = decode_base64(&encoded).unwrap();
    assert_eq!(data.as_slice(), decoded.as_slice());
}

#[test]
fn test_base64_empty() {
    let encoded = encode_base64(b"");
    let decoded = decode_base64(&encoded).unwrap();
    assert!(decoded.is_empty());
}

#[test]
fn test_read_params() {
    let params = read_params("/path/to/file", None, None);
    assert_eq!(params["path"], "/path/to/file");
    assert!(params.get("off").is_none());
    assert!(params.get("len").is_none());

    let params = read_params("/path/to/file", Some(100), Some(50));
    assert_eq!(params["path"], "/path/to/file");
    assert_eq!(params["off"], 100);
    assert_eq!(params["len"], 50);
}

#[test]
fn test_write_params() {
    let params = write_params("/path/to/file", b"Hello");
    assert_eq!(params["path"], "/path/to/file");
    assert_eq!(params["data"], encode_base64(b"Hello"));
}

#[test]
fn test_stat_params() {
    let params = stat_params("/path/to/file", true);
    assert_eq!(params["path"], "/path/to/file");
    assert_eq!(params["link"], true);

    let params = stat_params("/path/to/file", false);
    assert_eq!(params["link"], false);
}

#[test]
fn test_exec_params() {
    let params = exec_params(
        "rg",
        &["pattern".to_string(), ".".to_string()],
        Some("/home"),
    );
    assert_eq!(params["cmd"], "rg");
    assert_eq!(params["args"], serde_json::json!(["pattern", "."]));
    assert_eq!(params["cwd"], "/home");

    let params = exec_params("ls", &[], None);
    assert_eq!(params["cmd"], "ls");
    assert!(params.get("cwd").is_none());
}

#[test]
fn test_cancel_params() {
    let params = cancel_params(42);
    assert_eq!(params["id"], 42);
}

#[test]
fn test_remote_dir_entry_parsing() {
    let json = r#"{
        "name": "test.rs",
        "path": "/home/user/test.rs",
        "dir": false,
        "file": true,
        "link": false,
        "link_dir": false,
        "size": 1234,
        "mtime": 1700000000,
        "mode": 33188
    }"#;

    let entry: RemoteDirEntry = serde_json::from_str(json).unwrap();
    assert_eq!(entry.name, "test.rs");
    assert_eq!(entry.path, "/home/user/test.rs");
    assert!(!entry.dir);
    assert!(entry.file);
    assert!(!entry.link);
    assert_eq!(entry.size, 1234);
    assert_eq!(entry.mode, 33188); // 0o100644
}

#[test]
fn test_remote_metadata_parsing() {
    let json = r#"{
        "size": 5678,
        "mtime": 1700000000,
        "mode": 33188,
        "uid": 1000,
        "gid": 1000,
        "dir": false,
        "file": true,
        "link": false
    }"#;

    let meta: RemoteMetadata = serde_json::from_str(json).unwrap();
    assert_eq!(meta.size, 5678);
    assert_eq!(meta.mtime, 1700000000);
    assert_eq!(meta.mode, 33188);
    assert_eq!(meta.uid, 1000);
    assert!(!meta.dir);
    assert!(meta.file);
}

#[test]
fn test_connection_params_parse() {
    // Basic user@host
    let params = ConnectionParams::parse("alice@server.com").unwrap();
    assert_eq!(params.user.as_deref(), Some("alice"));
    assert_eq!(params.host, "server.com");
    assert_eq!(params.port, None);

    // With port
    let params = ConnectionParams::parse("bob@example.org:2222").unwrap();
    assert_eq!(params.user.as_deref(), Some("bob"));
    assert_eq!(params.host, "example.org");
    assert_eq!(params.port, Some(2222));

    // User optional: a bare host parses with user = None.
    let params = ConnectionParams::parse("noatsign").unwrap();
    assert_eq!(params.user, None);
    assert_eq!(params.host, "noatsign");

    // Invalid cases (empty user / empty host / empty string).
    assert!(ConnectionParams::parse("@nouser").is_none());
    assert!(ConnectionParams::parse("nohost@").is_none());
    assert!(ConnectionParams::parse("").is_none());
}

#[test]
fn test_connection_params_to_string() {
    let params = ConnectionParams {
        user: Some("alice".to_string()),
        host: "server.com".to_string(),
        port: None,
        identity_file: None,
        extra_args: Vec::new(),
    };
    assert_eq!(params.to_string(), "alice@server.com");

    let params = ConnectionParams {
        user: Some("bob".to_string()),
        host: "example.org".to_string(),
        port: Some(2222),
        identity_file: None,
        extra_args: Vec::new(),
    };
    assert_eq!(params.to_string(), "bob@example.org:2222");
}

// Test that ExecResult parses correctly
#[test]
fn test_exec_result_parsing() {
    let json = r#"{"code": 0}"#;
    let result: ExecResult = serde_json::from_str(json).unwrap();
    assert_eq!(result.code, 0);

    let json = r#"{"code": -1}"#;
    let result: ExecResult = serde_json::from_str(json).unwrap();
    assert_eq!(result.code, -1);
}

// Test ExecOutput parsing
#[test]
fn test_exec_output_parsing() {
    let json = r#"{"out": "SGVsbG8="}"#;
    let output: ExecOutput = serde_json::from_str(json).unwrap();
    assert_eq!(output.out, Some("SGVsbG8=".to_string()));
    assert_eq!(output.err, None);

    let json = r#"{"err": "RXJyb3I="}"#;
    let output: ExecOutput = serde_json::from_str(json).unwrap();
    assert_eq!(output.out, None);
    assert_eq!(output.err, Some("RXJyb3I=".to_string()));

    let json = r#"{"out": "T3V0", "err": "RXJy"}"#;
    let output: ExecOutput = serde_json::from_str(json).unwrap();
    assert!(output.out.is_some());
    assert!(output.err.is_some());
}

// ============================================================================
// Integration tests - spawn Python agent locally
// ============================================================================

/// Helper to spawn agent and communicate with it
#[cfg(test)]
mod agent_integration {
    use super::*;
    use std::io::{BufRead, BufReader, Write};
    use std::process::{Command, Stdio};

    /// Spawn the Python agent and return stdin/stdout handles
    fn spawn_agent() -> Option<(
        std::process::ChildStdin,
        BufReader<std::process::ChildStdout>,
    )> {
        let mut child = Command::new("python3")
            .arg("-u")
            .arg("-c")
            .arg(AGENT_SOURCE)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .ok()?;

        let stdin = child.stdin.take()?;
        let stdout = BufReader::new(child.stdout.take()?);

        Some((stdin, stdout))
    }

    /// Send a request and read the response
    fn send_request(
        stdin: &mut std::process::ChildStdin,
        stdout: &mut BufReader<std::process::ChildStdout>,
        method: &str,
        params: serde_json::Value,
    ) -> Option<AgentResponse> {
        let req = AgentRequest::new(1, method, params);
        stdin.write_all(req.to_json_line().as_bytes()).ok()?;
        stdin.flush().ok()?;

        let mut line = String::new();
        stdout.read_line(&mut line).ok()?;
        serde_json::from_str(&line).ok()
    }

    #[test]
    fn test_agent_ready_message() {
        let Some((_, mut stdout)) = spawn_agent() else {
            eprintln!("Skipping test: Python3 not available");
            return;
        };

        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();
        let resp: AgentResponse = serde_json::from_str(&line).unwrap();

        assert!(resp.is_ready());
        assert_eq!(resp.version, Some(1));
    }

    #[test]
    fn test_agent_stat_command() {
        let Some((mut stdin, mut stdout)) = spawn_agent() else {
            eprintln!("Skipping test: Python3 not available");
            return;
        };

        // Read ready message
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();

        // Stat a known path (root directory always exists)
        let resp = send_request(
            &mut stdin,
            &mut stdout,
            "stat",
            serde_json::json!({
                "path": "/"
            }),
        );

        assert!(resp.is_some());
        let resp = resp.unwrap();
        assert!(resp.is_final());
        assert!(resp.result.is_some());

        let result = resp.result.unwrap();
        assert!(result.get("dir").and_then(|v| v.as_bool()).unwrap_or(false));
    }

    #[test]
    fn test_agent_exists_command() {
        let Some((mut stdin, mut stdout)) = spawn_agent() else {
            eprintln!("Skipping test: Python3 not available");
            return;
        };

        // Read ready message
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();

        // Check a path that exists
        let resp = send_request(
            &mut stdin,
            &mut stdout,
            "exists",
            serde_json::json!({
                "path": "/"
            }),
        );
        assert!(resp.is_some());
        let result = resp.unwrap().result.unwrap();
        assert!(result
            .get("exists")
            .and_then(|v| v.as_bool())
            .unwrap_or(false));

        // Check a path that doesn't exist
        let resp = send_request(
            &mut stdin,
            &mut stdout,
            "exists",
            serde_json::json!({
                "path": "/nonexistent/path/12345"
            }),
        );
        assert!(resp.is_some());
        let result = resp.unwrap().result.unwrap();
        assert!(!result
            .get("exists")
            .and_then(|v| v.as_bool())
            .unwrap_or(true));
    }

    #[test]
    fn test_agent_realpath_command() {
        let Some((mut stdin, mut stdout)) = spawn_agent() else {
            eprintln!("Skipping test: Python3 not available");
            return;
        };

        // Read ready message
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();

        // Canonicalize home directory
        let resp = send_request(
            &mut stdin,
            &mut stdout,
            "realpath",
            serde_json::json!({
                "path": "~"
            }),
        );
        assert!(resp.is_some());
        let result = resp.unwrap().result.unwrap();
        let path = result.get("path").and_then(|v| v.as_str()).unwrap();
        assert!(
            std::path::Path::new(path).is_absolute(),
            "realpath should return absolute path, got: {}",
            path
        );
        assert!(!path.contains('~'));
    }

    #[test]
    fn test_agent_unknown_method() {
        let Some((mut stdin, mut stdout)) = spawn_agent() else {
            eprintln!("Skipping test: Python3 not available");
            return;
        };

        // Read ready message
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();

        // Send unknown method
        let resp = send_request(
            &mut stdin,
            &mut stdout,
            "unknown_method",
            serde_json::json!({}),
        );
        assert!(resp.is_some());
        let resp = resp.unwrap();
        assert!(resp.error.is_some());
        assert!(resp.error.unwrap().contains("unknown method"));
    }
}
