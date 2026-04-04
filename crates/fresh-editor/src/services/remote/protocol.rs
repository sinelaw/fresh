//! Agent protocol types
//!
//! JSON-based protocol for communication with the remote agent.
//! All binary data is base64 encoded.

use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use serde::{Deserialize, Serialize};

/// Protocol version
pub const PROTOCOL_VERSION: u32 = 1;

/// Request sent to the agent
#[derive(Debug, Clone, Serialize)]
pub struct AgentRequest {
    pub id: u64,
    #[serde(rename = "m")]
    pub method: String,
    #[serde(rename = "p")]
    pub params: serde_json::Value,
}

impl AgentRequest {
    pub fn new(id: u64, method: impl Into<String>, params: serde_json::Value) -> Self {
        Self {
            id,
            method: method.into(),
            params,
        }
    }

    pub fn to_json_line(&self) -> String {
        serde_json::to_string(self).unwrap() + "\n"
    }
}

/// Response from the agent - can be one of three types
#[derive(Debug, Clone, Deserialize)]
pub struct AgentResponse {
    pub id: u64,
    /// Streaming data (intermediate)
    #[serde(rename = "d")]
    pub data: Option<serde_json::Value>,
    /// Final result (success)
    #[serde(rename = "r")]
    pub result: Option<serde_json::Value>,
    /// Error message (failure)
    #[serde(rename = "e")]
    pub error: Option<String>,
    /// Ready message fields
    pub ok: Option<bool>,
    #[serde(rename = "v")]
    pub version: Option<u32>,
}

impl AgentResponse {
    /// Check if this is the ready message
    pub fn is_ready(&self) -> bool {
        self.ok == Some(true)
    }

    /// Check if this is a streaming data message
    pub fn is_data(&self) -> bool {
        self.data.is_some()
    }

    /// Check if this is a final message (result or error)
    pub fn is_final(&self) -> bool {
        self.result.is_some() || self.error.is_some()
    }
}

/// Directory entry returned by `ls` command
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct RemoteDirEntry {
    pub name: String,
    pub path: String,
    #[serde(default)]
    pub dir: bool,
    #[serde(default)]
    pub file: bool,
    #[serde(default)]
    pub link: bool,
    #[serde(default)]
    pub link_dir: bool,
    #[serde(default)]
    pub size: u64,
    #[serde(default)]
    pub mtime: i64,
    #[serde(default)]
    pub mode: u32,
}

/// File metadata returned by `stat` command
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct RemoteMetadata {
    pub size: u64,
    pub mtime: i64,
    pub mode: u32,
    #[serde(default)]
    pub uid: u32,
    #[serde(default)]
    pub gid: u32,
    #[serde(default)]
    pub dir: bool,
    #[serde(default)]
    pub file: bool,
    #[serde(default)]
    pub link: bool,
}

/// Process execution result
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct ExecResult {
    pub code: i32,
}

/// Streaming output from exec
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct ExecOutput {
    #[serde(default)]
    pub out: Option<String>,
    #[serde(default)]
    pub err: Option<String>,
}

/// Helper to encode bytes to base64
pub fn encode_base64(data: &[u8]) -> String {
    BASE64.encode(data)
}

/// Helper to decode base64 to bytes
pub fn decode_base64(s: &str) -> Result<Vec<u8>, base64::DecodeError> {
    BASE64.decode(s)
}

/// Build params for read request
pub fn read_params(path: &str, offset: Option<u64>, len: Option<usize>) -> serde_json::Value {
    let mut params = serde_json::json!({"path": path});
    if let Some(off) = offset {
        params["off"] = serde_json::json!(off);
    }
    if let Some(l) = len {
        params["len"] = serde_json::json!(l);
    }
    params
}

/// Build params for count_lf request (count newlines in a file range)
pub fn count_lf_params(path: &str, offset: u64, len: usize) -> serde_json::Value {
    serde_json::json!({"path": path, "off": offset, "len": len})
}

/// Build params for write request
pub fn write_params(path: &str, data: &[u8]) -> serde_json::Value {
    serde_json::json!({
        "path": path,
        "data": encode_base64(data)
    })
}

/// Build params for sudo_write request (write file as root)
pub fn sudo_write_params(
    path: &str,
    data: &[u8],
    mode: u32,
    uid: u32,
    gid: u32,
) -> serde_json::Value {
    serde_json::json!({
        "path": path,
        "data": encode_base64(data),
        "mode": mode,
        "uid": uid,
        "gid": gid
    })
}

/// Build params for stat request
pub fn stat_params(path: &str, follow_symlinks: bool) -> serde_json::Value {
    serde_json::json!({
        "path": path,
        "link": follow_symlinks
    })
}

/// Build params for ls request
pub fn ls_params(path: &str) -> serde_json::Value {
    serde_json::json!({"path": path})
}

/// Build params for an exec request sent to the remote agent.
pub fn exec_params(cmd: &str, args: &[String], cwd: Option<&str>) -> serde_json::Value {
    let mut params = serde_json::json!({
        "cmd": cmd,
        "args": args
    });
    if let Some(dir) = cwd {
        params["cwd"] = serde_json::json!(dir);
    }
    params
}

/// Build params for cancel request
pub fn cancel_params(request_id: u64) -> serde_json::Value {
    serde_json::json!({"id": request_id})
}

/// Build params for append request
pub fn append_params(path: &str, data: &[u8]) -> serde_json::Value {
    serde_json::json!({
        "path": path,
        "data": encode_base64(data)
    })
}

/// Build params for truncate request
pub fn truncate_params(path: &str, len: u64) -> serde_json::Value {
    serde_json::json!({
        "path": path,
        "len": len
    })
}

/// A single operation in a patch recipe
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum PatchOp {
    /// Copy a range from the original file
    Copy { copy: CopyRange },
    /// Insert new content
    Insert { insert: InsertData },
}

/// Range to copy from original file
#[derive(Debug, Clone, Serialize)]
pub struct CopyRange {
    pub off: u64,
    pub len: u64,
}

/// Data to insert
#[derive(Debug, Clone, Serialize)]
pub struct InsertData {
    pub data: String, // base64 encoded
}

impl PatchOp {
    /// Create a copy operation
    pub fn copy(offset: u64, len: u64) -> Self {
        PatchOp::Copy {
            copy: CopyRange { off: offset, len },
        }
    }

    /// Create an insert operation
    pub fn insert(data: &[u8]) -> Self {
        PatchOp::Insert {
            insert: InsertData {
                data: encode_base64(data),
            },
        }
    }
}

/// Build params for patch request
pub fn patch_params(src: &str, dst: Option<&str>, ops: &[PatchOp]) -> serde_json::Value {
    let mut params = serde_json::json!({
        "src": src,
        "ops": ops
    });
    if let Some(d) = dst {
        params["dst"] = serde_json::json!(d);
    }
    params
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request_serialization() {
        let req = AgentRequest::new(1, "read", serde_json::json!({"path": "/test.txt"}));
        let json = req.to_json_line();
        assert!(json.contains("\"id\":1"));
        assert!(json.contains("\"m\":\"read\""));
        assert!(json.contains("\"p\":{\"path\":\"/test.txt\"}"));
    }

    #[test]
    fn test_response_parsing() {
        let ready = r#"{"id":0,"ok":true,"v":1}"#;
        let resp: AgentResponse = serde_json::from_str(ready).unwrap();
        assert!(resp.is_ready());
        assert_eq!(resp.version, Some(1));

        let data = r#"{"id":1,"d":{"data":"SGVsbG8="}}"#;
        let resp: AgentResponse = serde_json::from_str(data).unwrap();
        assert!(resp.is_data());
        assert!(!resp.is_final());

        let result = r#"{"id":1,"r":{"size":5}}"#;
        let resp: AgentResponse = serde_json::from_str(result).unwrap();
        assert!(resp.is_final());
        assert!(resp.result.is_some());

        let error = r#"{"id":1,"e":"not found"}"#;
        let resp: AgentResponse = serde_json::from_str(error).unwrap();
        assert!(resp.is_final());
        assert_eq!(resp.error, Some("not found".to_string()));
    }

    #[test]
    fn test_base64_roundtrip() {
        let data = b"Hello, World!";
        let encoded = encode_base64(data);
        let decoded = decode_base64(&encoded).unwrap();
        assert_eq!(data.as_slice(), decoded.as_slice());
    }

    #[test]
    fn test_patch_op_copy_serialization() {
        let op = PatchOp::copy(100, 500);
        let json = serde_json::to_string(&op).unwrap();
        assert!(json.contains("\"copy\""));
        assert!(json.contains("\"off\":100"));
        assert!(json.contains("\"len\":500"));
        // Should NOT contain "insert"
        assert!(!json.contains("\"insert\""));
    }

    #[test]
    fn test_patch_op_insert_serialization() {
        let op = PatchOp::insert(b"hello");
        let json = serde_json::to_string(&op).unwrap();
        assert!(json.contains("\"insert\""));
        assert!(json.contains("\"data\":\"aGVsbG8=\"")); // base64 of "hello"
                                                         // Should NOT contain "copy"
        assert!(!json.contains("\"copy\""));
    }

    #[test]
    fn test_patch_params() {
        let ops = vec![
            PatchOp::copy(0, 100),
            PatchOp::insert(b"new content"),
            PatchOp::copy(200, 300),
        ];

        // Same src and dst
        let params = patch_params("/path/to/file", None, &ops);
        assert_eq!(params["src"], "/path/to/file");
        assert!(params.get("dst").is_none() || params["dst"].is_null());
        assert!(params["ops"].is_array());
        assert_eq!(params["ops"].as_array().unwrap().len(), 3);

        // Different dst
        let params = patch_params("/src/file", Some("/dst/file"), &ops);
        assert_eq!(params["src"], "/src/file");
        assert_eq!(params["dst"], "/dst/file");
    }
}
