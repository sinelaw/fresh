//! Helpers for the sidebar e2e tests: what the rendered column says.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use std::fs;
use std::path::Path;

/// The Markdown plugins the outline needs, into `<project>/plugins`.
pub fn install_markdown_plugins(project: &Path) {
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_toc");
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin(&plugins_dir, "markdown_source");
    copy_plugin_lib(&plugins_dir);
}

pub fn screen_has(h: &EditorTestHarness, needle: &str) -> bool {
    h.screen_to_string().contains(needle)
}

/// The explorer holds the keyboard: its header shows no focus hint.
pub fn explorer_focused(h: &EditorTestHarness) -> bool {
    h.screen_to_string()
        .lines()
        .any(|l| l.contains("File Explorer") && !l.contains("(Ctrl+E)"))
}

/// Something other than the explorer holds the keyboard.
pub fn explorer_hinted(h: &EditorTestHarness) -> bool {
    screen_has(h, "File Explorer (Ctrl+E)")
}

/// The rows of the `Contents` section: everything between its header and
/// the next section border, trimmed of the selection mark, non-empty.
pub fn contents_rows(h: &EditorTestHarness) -> Vec<String> {
    let s = h.screen_to_string();
    let mut rows = Vec::new();
    let mut inside = false;
    for line in s.lines() {
        if line.contains("Contents") && (line.contains('▼') || line.contains('▶')) {
            inside = true;
            continue;
        }
        if inside {
            if line.starts_with('└') || line.starts_with('├') {
                break;
            }
            let text = line
                .trim_start_matches('│')
                .split('│')
                .next()
                .unwrap_or("")
                .trim()
                .trim_start_matches('▌')
                .trim()
                .to_string();
            if !text.is_empty() {
                rows.push(text);
            }
        }
    }
    rows
}

/// The `Contents` row that carries the selection mark, trimmed to its text.
pub fn selected_contents_row(h: &EditorTestHarness) -> Option<String> {
    let s = h.screen_to_string();
    let mut inside = false;
    for line in s.lines() {
        if line.contains("Contents") && line.contains('▼') {
            inside = true;
            continue;
        }
        if inside {
            if line.starts_with('└') || line.starts_with('├') {
                break;
            }
            if let Some(i) = line.find('▌') {
                let rest = &line[i + '▌'.len_utf8()..];
                return Some(rest.split('│').next().unwrap_or("").trim().to_string());
            }
        }
    }
    None
}

/// `wait_until`, naming what was waited for and showing the screen on
/// timeout.
pub fn wait_for(h: &mut EditorTestHarness, what: &str, pred: impl Fn(&EditorTestHarness) -> bool) {
    h.wait_until(pred)
        .unwrap_or_else(|e| panic!("{what}: {e}\n{}", h.screen_to_string()));
}
