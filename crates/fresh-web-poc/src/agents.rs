//! Agent sessions run outside the editor, found on a host for the
//! orchestrator's Import Sessions: the editor's agent-sessions scanners for
//! Claude Code and Codex (`crates/fresh-editor/plugins/agent_sessions_*.ts`),
//! read through the host's filesystem.
//!
//! - Claude Code: `~/.claude/projects/<encoded cwd>/<session id>.jsonl`; the
//!   records carry `cwd`, `gitBranch` and `sessionId`. Sub-agent transcripts
//!   (one level deeper) are left out: they cannot be resumed on their own.
//! - Codex: `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl`; the first record
//!   is `{"type": "session_meta", "payload": {"id", "cwd"}}`.
//!
//! Only the head of each transcript is read. A session's title is its first
//! message from the user. Newest first; at most [`MAX`].

use crate::workspace::{host, Fs};
use fresh_editor_core::model::filesystem::{EntryType, WalkOptions};
use serde_json::{json, Value};
use std::path::{Path, PathBuf};
use std::sync::atomic::AtomicBool;
use std::time::UNIX_EPOCH;

/// How many sessions are listed.
pub const MAX: usize = 200;
/// How much of a transcript is read.
const HEAD: usize = 64 * 1024;

/// A transcript found: its path, when it was last written, and which agent.
struct Found {
    path: PathBuf,
    rel: String,
    mtime: u64,
    size: u64,
    agent: &'static str,
}

fn walk(fs: &Fs, root: &Path, depth: usize, agent: &'static str, out: &mut Vec<Found>) {
    let cancel = AtomicBool::new(false);
    let opts = WalkOptions {
        include_hidden: true,
        max_depth: depth,
        max_entries: 20_000,
        ..WalkOptions::default()
    };
    let _ = fs.walk(host(root).as_host_path(), &opts, &cancel, &mut |e| {
        if matches!(e.entry_type, EntryType::File) && e.rel.ends_with(".jsonl") {
            out.push(Found {
                path: e.path.to_host_path_buf().into(),
                rel: e.rel.to_string(),
                mtime: e
                    .metadata
                    .modified
                    .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
                    .map(|d| d.as_millis() as u64)
                    .unwrap_or(0),
                size: e.metadata.size,
                agent,
            });
        }
        true
    });
}

/// The JSON records in the head of a transcript (a cut last line is left
/// out).
fn records(fs: &Fs, path: &Path, size: u64) -> Vec<Value> {
    let len = HEAD.min(size as usize);
    let Ok(bytes) = fs.read_range(host(path).as_host_path(), 0, len) else {
        return Vec::new();
    };
    String::from_utf8_lossy(&bytes)
        .lines()
        .filter_map(|l| serde_json::from_str(l).ok())
        .collect()
}

/// The first string at `key` in a record or its `payload`.
fn pick(records: &[Value], key: &str) -> Option<String> {
    records.iter().find_map(|r| {
        r.get(key)
            .or_else(|| r.get("payload").and_then(|p| p.get(key)))
            .and_then(Value::as_str)
            .filter(|s| !s.is_empty())
            .map(str::to_string)
    })
}

/// The user's first message: Claude Code's `{"type": "user", "message":
/// {"content"}}` (a string or text parts), Codex's `user_message` event.
fn first_prompt(records: &[Value]) -> Option<String> {
    let text = records.iter().find_map(|r| {
        let content = if r["type"] == "user" {
            &r["message"]["content"]
        } else if r["payload"]["type"] == "user_message" {
            &r["payload"]["message"]
        } else {
            return None;
        };
        let t = match content {
            Value::String(s) => s.clone(),
            Value::Array(parts) => parts
                .iter()
                .filter_map(|p| p["text"].as_str())
                .collect::<Vec<_>>()
                .join(" "),
            _ => return None,
        };
        // Command and environment wrappers are not what the user asked.
        let t = t.trim().to_string();
        (!t.is_empty() && !t.starts_with('<')).then_some(t)
    })?;
    let line = text.split_whitespace().collect::<Vec<_>>().join(" ");
    Some(if line.chars().count() > 120 {
        format!("{}…", line.chars().take(119).collect::<String>())
    } else {
        line
    })
}

/// Every Claude Code and Codex session under `home` on the host.
pub fn scan(fs: &Fs, home: &Path) -> Vec<Value> {
    let mut found = Vec::new();
    // `<bucket>/<id>.jsonl` is depth 2; a sub-agent's is deeper.
    walk(fs, &home.join(".claude/projects"), 2, "claude", &mut found);
    walk(fs, &home.join(".codex/sessions"), 4, "codex", &mut found);
    found.retain(|f| f.agent != "claude" || f.rel.matches('/').count() == 1);
    found.sort_by(|a, b| b.mtime.cmp(&a.mtime));
    found
        .into_iter()
        .take(MAX)
        .filter_map(|f| {
            let recs = records(fs, &f.path, f.size);
            let stem = f
                .path
                .file_stem()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();
            let id = match f.agent {
                "claude" => pick(&recs, "sessionId").unwrap_or(stem),
                _ => pick(&recs, "id")?,
            };
            let cwd = pick(&recs, "cwd")?;
            Some(json!({
                "agent": f.agent, "id": id, "cwd": cwd, "branch": pick(&recs, "gitBranch"),
                "title": first_prompt(&recs), "mtime": f.mtime, "path": f.path.to_string_lossy(),
                // How to pick it up again in a terminal in `cwd`.
                "resume": match f.agent {
                    "claude" => format!("claude --resume {id}"),
                    _ => format!("codex resume {id}"),
                },
            }))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn claude_and_codex_sessions_are_found() {
        let home = tempfile::tempdir().unwrap();
        let h = home.path();
        let claude = h.join(".claude/projects/-w-app");
        std::fs::create_dir_all(claude.join("abc/subagents")).unwrap();
        std::fs::write(
            claude.join("abc.jsonl"),
            concat!(
                "{\"type\":\"queue-operation\",\"sessionId\":\"abc\"}\n",
                "{\"type\":\"user\",\"cwd\":\"/w/app\",\"gitBranch\":\"main\",\"message\":{\"content\":\"<command-name>x</command-name>\"}}\n",
                "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Fix the   login bug\"}]}}\n",
            ),
        )
        .unwrap();
        std::fs::write(
            claude.join("abc/subagents/s1.jsonl"),
            "{\"cwd\":\"/w/app\"}\n",
        )
        .unwrap();
        let codex = h.join(".codex/sessions/2026/11/24");
        std::fs::create_dir_all(&codex).unwrap();
        std::fs::write(
            codex.join("rollout-1-xyz.jsonl"),
            concat!(
                "{\"type\":\"session_meta\",\"payload\":{\"id\":\"xyz\",\"cwd\":\"/w/api\"}}\n",
                "{\"type\":\"event_msg\",\"payload\":{\"type\":\"user_message\",\"message\":\"add tests\"}}\n",
            ),
        )
        .unwrap();
        let fs: Fs = Arc::new(fresh_editor_core::model::filesystem::StdFileSystem);
        let mut all = scan(&fs, h);
        all.sort_by(|a, b| a["agent"].as_str().cmp(&b["agent"].as_str()));
        assert_eq!(all.len(), 2, "{all:?}");
        let (c, x) = (&all[0], &all[1]);
        assert_eq!(
            (
                c["id"].clone(),
                c["cwd"].clone(),
                c["branch"].clone(),
                c["title"].clone()
            ),
            (
                json!("abc"),
                json!("/w/app"),
                json!("main"),
                json!("Fix the login bug")
            )
        );
        assert_eq!(c["resume"], "claude --resume abc");
        assert_eq!(
            (
                x["id"].clone(),
                x["cwd"].clone(),
                x["title"].clone(),
                x["resume"].clone()
            ),
            (
                json!("xyz"),
                json!("/w/api"),
                json!("add tests"),
                json!("codex resume xyz")
            )
        );
    }
}
