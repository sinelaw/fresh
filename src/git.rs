//! Git integration - async operations for git commands

use crate::async_bridge::{AsyncMessage, GitGrepMatch};
use std::process::Stdio;
use std::sync::mpsc;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

/// Check if the current directory is inside a git repository
pub async fn is_git_repo() -> bool {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--is-inside-work-tree")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .await;

    matches!(output, Ok(status) if status.success())
}

/// Execute git grep and send results back through the bridge
///
/// Args:
/// - query: The search query
/// - sender: Channel to send results back to main loop
pub async fn git_grep(query: String, sender: mpsc::Sender<AsyncMessage>) {
    // Don't run empty queries
    if query.trim().is_empty() {
        let _ = sender.send(AsyncMessage::GitGrepResults {
            query: query.clone(),
            results: vec![],
        });
        return;
    }

    // Run git grep with line numbers and column numbers
    // -n = show line numbers
    // --column = show column numbers
    // -I = ignore binary files
    // --heading = group by file (but we parse it line by line)
    let mut child = match Command::new("git")
        .arg("grep")
        .arg("-n")
        .arg("--column")
        .arg("-I")
        .arg("--")
        .arg(&query)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
    {
        Ok(child) => child,
        Err(_) => {
            // Git command failed (probably not a git repo or git not installed)
            let _ = sender.send(AsyncMessage::GitGrepResults {
                query: query.clone(),
                results: vec![],
            });
            return;
        }
    };

    let stdout = child.stdout.take().unwrap();
    let reader = BufReader::new(stdout);
    let mut lines = reader.lines();

    let mut results = Vec::new();

    // Parse git grep output
    // Format: file:line:column:content
    while let Ok(Some(line)) = lines.next_line().await {
        if let Some(match_result) = parse_git_grep_line(&line) {
            results.push(match_result);

            // Limit results to prevent overwhelming the UI
            if results.len() >= 100 {
                break;
            }
        }
    }

    // Wait for command to complete
    let _ = child.wait().await;

    // Send results back to main loop
    let _ = sender.send(AsyncMessage::GitGrepResults {
        query: query.clone(),
        results,
    });
}

/// Parse a line from git grep output
/// Format: file:line:column:content
fn parse_git_grep_line(line: &str) -> Option<GitGrepMatch> {
    let mut parts = line.splitn(4, ':');

    let file = parts.next()?.to_string();
    let line_str = parts.next()?;
    let column_str = parts.next()?;
    let content = parts.next()?.to_string();

    let line_num = line_str.parse::<usize>().ok()?;
    let column = column_str.parse::<usize>().ok()?;

    Some(GitGrepMatch {
        file,
        line: line_num,
        column,
        content: content.trim().to_string(),
    })
}

/// Execute git ls-files and filter by query, sending results back through the bridge
///
/// Args:
/// - query: The search query to filter files
/// - sender: Channel to send results back to main loop
pub async fn git_ls_files(query: String, sender: mpsc::Sender<AsyncMessage>) {
    // Run git ls-files
    let output = match Command::new("git")
        .arg("ls-files")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .output()
        .await
    {
        Ok(output) => output,
        Err(_) => {
            // Git command failed
            let _ = sender.send(AsyncMessage::GitLsFilesResults {
                query: query.clone(),
                files: vec![],
            });
            return;
        }
    };

    if !output.status.success() {
        let _ = sender.send(AsyncMessage::GitLsFilesResults {
            query: query.clone(),
            files: vec![],
        });
        return;
    }

    // Parse output and filter by query
    let all_files = String::from_utf8_lossy(&output.stdout);
    let query_lower = query.to_lowercase();

    let mut filtered_files: Vec<String> = all_files
        .lines()
        .filter(|file| {
            if query.trim().is_empty() {
                return true;
            }
            // Fuzzy match: all characters of query must appear in order
            let file_lower = file.to_lowercase();
            let mut query_chars = query_lower.chars();
            let mut current_char = query_chars.next();

            for file_char in file_lower.chars() {
                if let Some(qc) = current_char {
                    if qc == file_char {
                        current_char = query_chars.next();
                    }
                } else {
                    break;
                }
            }

            current_char.is_none() // All query characters matched
        })
        .take(100) // Limit results
        .map(|s| s.to_string())
        .collect();

    // Sort by relevance: prefer matches at the end of the path (filename)
    filtered_files.sort_by_key(|file| {
        let filename = file.rsplit('/').next().unwrap_or(file);
        let filename_lower = filename.to_lowercase();

        // Score: lower is better
        // Prioritize files where query appears in filename
        if query_lower.is_empty() {
            0
        } else if filename_lower.contains(&query_lower) {
            0
        } else {
            1
        }
    });

    // Send results back to main loop
    let _ = sender.send(AsyncMessage::GitLsFilesResults {
        query,
        files: filtered_files,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_git_grep_line() {
        let line = "src/main.rs:10:5:fn main() {";
        let result = parse_git_grep_line(line).unwrap();

        assert_eq!(result.file, "src/main.rs");
        assert_eq!(result.line, 10);
        assert_eq!(result.column, 5);
        assert_eq!(result.content, "fn main() {");
    }

    #[test]
    fn test_parse_git_grep_line_with_colons_in_content() {
        let line = "config.json:5:10:  \"port\": 8080,";
        let result = parse_git_grep_line(line).unwrap();

        assert_eq!(result.file, "config.json");
        assert_eq!(result.line, 5);
        assert_eq!(result.column, 10);
        assert_eq!(result.content, "\"port\": 8080,");
    }
}
