//! Built-in Quick Open Providers
//!
//! This module contains the standard providers:
//! - FileProvider: Find files in the project (default, no prefix)
//! - CommandProvider: Command palette (prefix: ">")
//! - BufferProvider: Switch between open buffers (prefix: "#")
//! - GotoLineProvider: Go to a specific line (prefix: ":")

use super::{QuickOpenContext, QuickOpenProvider, QuickOpenResult};
use crate::input::commands::Suggestion;
use crate::input::fuzzy::fuzzy_match;
use rust_i18n::t;

// ============================================================================
// Command Provider (prefix: ">")
// ============================================================================

/// Provider for the command palette
pub struct CommandProvider {
    /// Reference to the command registry for filtering
    command_registry:
        std::sync::Arc<std::sync::RwLock<crate::input::command_registry::CommandRegistry>>,
    /// Keybinding resolver for showing shortcuts
    keybinding_resolver:
        std::sync::Arc<std::sync::RwLock<crate::input::keybindings::KeybindingResolver>>,
}

impl CommandProvider {
    pub fn new(
        command_registry: std::sync::Arc<
            std::sync::RwLock<crate::input::command_registry::CommandRegistry>,
        >,
        keybinding_resolver: std::sync::Arc<
            std::sync::RwLock<crate::input::keybindings::KeybindingResolver>,
        >,
    ) -> Self {
        Self {
            command_registry,
            keybinding_resolver,
        }
    }
}

impl QuickOpenProvider for CommandProvider {
    fn prefix(&self) -> &str {
        ">"
    }

    fn name(&self) -> &str {
        "Commands"
    }

    fn hint(&self) -> &str {
        ">  Commands"
    }

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        let registry = self.command_registry.read().unwrap();
        let keybindings = self.keybinding_resolver.read().unwrap();

        registry.filter(
            query,
            context.key_context,
            &keybindings,
            context.has_selection,
            &context.custom_contexts,
            context.buffer_mode.as_deref(),
        )
    }

    fn on_select(
        &self,
        selected_index: Option<usize>,
        query: &str,
        context: &QuickOpenContext,
    ) -> QuickOpenResult {
        let registry = self.command_registry.read().unwrap();
        let keybindings = self.keybinding_resolver.read().unwrap();

        let suggestions = registry.filter(
            query,
            context.key_context,
            &keybindings,
            context.has_selection,
            &context.custom_contexts,
            context.buffer_mode.as_deref(),
        );

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if suggestion.disabled {
                    return QuickOpenResult::Error(t!("status.command_not_available").to_string());
                }

                // Find the command by name
                let commands = registry.get_all();
                if let Some(cmd) = commands
                    .iter()
                    .find(|c| c.get_localized_name() == suggestion.text)
                {
                    // Record usage for frecency
                    drop(keybindings);
                    drop(registry);
                    if let Ok(mut reg) = self.command_registry.write() {
                        reg.record_usage(&cmd.name);
                    }
                    return QuickOpenResult::ExecuteAction(cmd.action.clone());
                }
            }
        }

        QuickOpenResult::None
    }
}

// ============================================================================
// Buffer Provider (prefix: "#")
// ============================================================================

/// Provider for switching between open buffers
pub struct BufferProvider;

impl BufferProvider {
    pub fn new() -> Self {
        Self
    }
}

impl Default for BufferProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl QuickOpenProvider for BufferProvider {
    fn prefix(&self) -> &str {
        "#"
    }

    fn name(&self) -> &str {
        "Buffers"
    }

    fn hint(&self) -> &str {
        "#  Buffers"
    }

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        let mut suggestions: Vec<(Suggestion, i32, usize)> = context
            .open_buffers
            .iter()
            .filter_map(|buf| {
                if buf.path.is_empty() {
                    return None; // Skip unnamed buffers
                }

                let display_name = if buf.modified {
                    format!("{} [+]", buf.name)
                } else {
                    buf.name.clone()
                };

                let match_result = if query.is_empty() {
                    crate::input::fuzzy::FuzzyMatch {
                        matched: true,
                        score: 0,
                        match_positions: vec![],
                    }
                } else {
                    fuzzy_match(query, &buf.name)
                };

                if match_result.matched {
                    Some((
                        Suggestion {
                            text: display_name,
                            description: Some(buf.path.clone()),
                            value: Some(buf.id.to_string()),
                            disabled: false,
                            keybinding: None,
                            source: None,
                        },
                        match_result.score,
                        buf.id,
                    ))
                } else {
                    None
                }
            })
            .collect();

        // Sort by score (higher is better), then by ID (lower = older = higher priority when tied)
        suggestions.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.2.cmp(&b.2)));

        suggestions.into_iter().map(|(s, _, _)| s).collect()
    }

    fn on_select(
        &self,
        selected_index: Option<usize>,
        query: &str,
        context: &QuickOpenContext,
    ) -> QuickOpenResult {
        let suggestions = self.suggestions(query, context);

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if let Some(value) = &suggestion.value {
                    if let Ok(buffer_id) = value.parse::<usize>() {
                        return QuickOpenResult::ShowBuffer(buffer_id);
                    }
                }
            }
        }

        QuickOpenResult::None
    }

    fn preview(
        &self,
        selected_index: usize,
        context: &QuickOpenContext,
    ) -> Option<(String, Option<usize>)> {
        let suggestions = self.suggestions("", context);
        suggestions
            .get(selected_index)
            .and_then(|s| s.description.clone().map(|path| (path, None)))
    }
}

// ============================================================================
// Go to Line Provider (prefix: ":")
// ============================================================================

/// Provider for jumping to a specific line number
pub struct GotoLineProvider;

impl GotoLineProvider {
    pub fn new() -> Self {
        Self
    }
}

impl Default for GotoLineProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl QuickOpenProvider for GotoLineProvider {
    fn prefix(&self) -> &str {
        ":"
    }

    fn name(&self) -> &str {
        "Go to Line"
    }

    fn hint(&self) -> &str {
        ":  Go to Line"
    }

    fn suggestions(&self, query: &str, _context: &QuickOpenContext) -> Vec<Suggestion> {
        if query.is_empty() {
            return vec![Suggestion {
                text: t!("quick_open.goto_line_hint").to_string(),
                description: Some(t!("quick_open.goto_line_desc").to_string()),
                value: None,
                disabled: true,
                keybinding: None,
                source: None,
            }];
        }

        if let Ok(line_num) = query.parse::<usize>() {
            if line_num > 0 {
                return vec![Suggestion {
                    text: t!("quick_open.goto_line", line = line_num.to_string()).to_string(),
                    description: Some(t!("quick_open.press_enter").to_string()),
                    value: Some(line_num.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }];
            }
        }

        // Invalid input
        vec![Suggestion {
            text: t!("quick_open.invalid_line").to_string(),
            description: Some(query.to_string()),
            value: None,
            disabled: true,
            keybinding: None,
            source: None,
        }]
    }

    fn on_select(
        &self,
        selected_index: Option<usize>,
        query: &str,
        _context: &QuickOpenContext,
    ) -> QuickOpenResult {
        // Try to parse from the suggestion value first, then from query
        if selected_index.is_some() {
            if let Ok(line_num) = query.parse::<usize>() {
                if line_num > 0 {
                    return QuickOpenResult::GotoLine(line_num);
                }
            }
        }

        QuickOpenResult::None
    }
}

// ============================================================================
// File Provider (default, no prefix)
// ============================================================================

/// Provider for finding files in the project
///
/// This is the default provider (empty prefix) that provides file suggestions
/// using git ls-files, fd, find, or directory traversal.
pub struct FileProvider {
    /// Cached file list (populated lazily)
    file_cache: std::sync::Arc<std::sync::RwLock<Option<Vec<FileEntry>>>>,
    /// Frecency data for ranking
    frecency: std::sync::Arc<std::sync::RwLock<std::collections::HashMap<String, FrecencyData>>>,
}

#[derive(Clone)]
struct FileEntry {
    relative_path: String,
    frecency_score: f64,
}

#[derive(Clone)]
struct FrecencyData {
    access_count: u32,
    last_access: std::time::Instant,
}

impl FileProvider {
    pub fn new() -> Self {
        Self {
            file_cache: std::sync::Arc::new(std::sync::RwLock::new(None)),
            frecency: std::sync::Arc::new(std::sync::RwLock::new(std::collections::HashMap::new())),
        }
    }

    /// Clear the file cache (e.g., after file system changes)
    pub fn clear_cache(&self) {
        if let Ok(mut cache) = self.file_cache.write() {
            *cache = None;
        }
    }

    /// Record file access for frecency ranking
    pub fn record_access(&self, path: &str) {
        if let Ok(mut frecency) = self.frecency.write() {
            let entry = frecency.entry(path.to_string()).or_insert(FrecencyData {
                access_count: 0,
                last_access: std::time::Instant::now(),
            });
            entry.access_count += 1;
            entry.last_access = std::time::Instant::now();
        }
    }

    fn get_frecency_score(&self, path: &str) -> f64 {
        if let Ok(frecency) = self.frecency.read() {
            if let Some(data) = frecency.get(path) {
                let hours_since_access = data.last_access.elapsed().as_secs_f64() / 3600.0;

                // Mozilla-style frecency weighting
                let recency_weight = if hours_since_access < 4.0 {
                    100.0
                } else if hours_since_access < 24.0 {
                    70.0
                } else if hours_since_access < 24.0 * 7.0 {
                    50.0
                } else if hours_since_access < 24.0 * 30.0 {
                    30.0
                } else if hours_since_access < 24.0 * 90.0 {
                    10.0
                } else {
                    1.0
                };

                return data.access_count as f64 * recency_weight;
            }
        }
        0.0
    }

    /// Load files from the project directory
    fn load_files(&self, cwd: &str) -> Vec<FileEntry> {
        // Check cache first
        if let Ok(cache) = self.file_cache.read() {
            if let Some(files) = cache.as_ref() {
                return files.clone();
            }
        }

        // Try different file discovery methods
        let files = self
            .try_git_files(cwd)
            .or_else(|| self.try_fd_files(cwd))
            .or_else(|| self.try_find_files(cwd))
            .unwrap_or_else(Vec::new);

        // Add frecency scores
        let files: Vec<FileEntry> = files
            .into_iter()
            .map(|path| FileEntry {
                frecency_score: self.get_frecency_score(&path),
                relative_path: path,
            })
            .collect();

        // Update cache
        if let Ok(mut cache) = self.file_cache.write() {
            *cache = Some(files.clone());
        }

        files
    }

    fn try_git_files(&self, cwd: &str) -> Option<Vec<String>> {
        let output = std::process::Command::new("git")
            .args(["ls-files", "--cached", "--others", "--exclude-standard"])
            .current_dir(cwd)
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let files: Vec<String> = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| !line.is_empty() && !line.starts_with(".git/"))
            .map(|s| s.to_string())
            .collect();

        Some(files)
    }

    fn try_fd_files(&self, cwd: &str) -> Option<Vec<String>> {
        let output = std::process::Command::new("fd")
            .args([
                "--type",
                "f",
                "--hidden",
                "--exclude",
                ".git",
                "--max-results",
                "50000",
            ])
            .current_dir(cwd)
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let files: Vec<String> = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| !line.is_empty())
            .map(|s| s.to_string())
            .collect();

        Some(files)
    }

    fn try_find_files(&self, cwd: &str) -> Option<Vec<String>> {
        let output = std::process::Command::new("find")
            .args([
                ".",
                "-type",
                "f",
                "-not",
                "-path",
                "*/.git/*",
                "-not",
                "-path",
                "*/node_modules/*",
                "-not",
                "-path",
                "*/target/*",
                "-not",
                "-path",
                "*/__pycache__/*",
            ])
            .current_dir(cwd)
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let files: Vec<String> = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| !line.is_empty())
            .map(|s| s.trim_start_matches("./").to_string())
            .take(50000)
            .collect();

        Some(files)
    }
}

impl Default for FileProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl QuickOpenProvider for FileProvider {
    fn prefix(&self) -> &str {
        ""
    }

    fn name(&self) -> &str {
        "Files"
    }

    fn hint(&self) -> &str {
        "Files"
    }

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        let files = self.load_files(&context.cwd);

        if files.is_empty() {
            return vec![Suggestion {
                text: t!("quick_open.no_files").to_string(),
                description: None,
                value: None,
                disabled: true,
                keybinding: None,
                source: None,
            }];
        }

        let max_results = 100;

        let mut scored_files: Vec<(FileEntry, i32)> = if query.is_empty() {
            // Sort by frecency when no query
            let mut files = files;
            files.sort_by(|a, b| {
                b.frecency_score
                    .partial_cmp(&a.frecency_score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            files
                .into_iter()
                .take(max_results)
                .map(|f| (f, 0))
                .collect()
        } else {
            // Filter and score by fuzzy match
            files
                .into_iter()
                .filter_map(|file| {
                    let match_result = fuzzy_match(query, &file.relative_path);
                    if match_result.matched {
                        // Boost score by frecency (normalized)
                        let frecency_boost = (file.frecency_score / 100.0).min(20.0) as i32;
                        Some((file, match_result.score + frecency_boost))
                    } else {
                        None
                    }
                })
                .collect()
        };

        // Sort by score
        scored_files.sort_by(|a, b| b.1.cmp(&a.1));
        scored_files.truncate(max_results);

        scored_files
            .into_iter()
            .map(|(file, _)| Suggestion {
                text: file.relative_path.clone(),
                description: None,
                value: Some(file.relative_path),
                disabled: false,
                keybinding: None,
                source: None,
            })
            .collect()
    }

    fn on_select(
        &self,
        selected_index: Option<usize>,
        query: &str,
        context: &QuickOpenContext,
    ) -> QuickOpenResult {
        let suggestions = self.suggestions(query, context);

        if let Some(idx) = selected_index {
            if let Some(suggestion) = suggestions.get(idx) {
                if let Some(path) = &suggestion.value {
                    // Record access for frecency
                    self.record_access(path);

                    return QuickOpenResult::OpenFile {
                        path: path.clone(),
                        line: None,
                        column: None,
                    };
                }
            }
        }

        QuickOpenResult::None
    }

    fn preview(
        &self,
        selected_index: usize,
        context: &QuickOpenContext,
    ) -> Option<(String, Option<usize>)> {
        let suggestions = self.suggestions("", context);
        suggestions
            .get(selected_index)
            .and_then(|s| s.value.clone().map(|path| (path, None)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::quick_open::BufferInfo;

    fn make_test_context() -> QuickOpenContext {
        QuickOpenContext {
            cwd: "/tmp".to_string(),
            open_buffers: vec![
                BufferInfo {
                    id: 1,
                    path: "/tmp/main.rs".to_string(),
                    name: "main.rs".to_string(),
                    modified: false,
                },
                BufferInfo {
                    id: 2,
                    path: "/tmp/lib.rs".to_string(),
                    name: "lib.rs".to_string(),
                    modified: true,
                },
            ],
            active_buffer_id: 1,
            active_buffer_path: Some("/tmp/main.rs".to_string()),
            has_selection: false,
            key_context: crate::input::keybindings::KeyContext::Normal,
            custom_contexts: std::collections::HashSet::new(),
            buffer_mode: None,
        }
    }

    #[test]
    fn test_buffer_provider_suggestions() {
        let provider = BufferProvider::new();
        let context = make_test_context();

        let suggestions = provider.suggestions("", &context);
        assert_eq!(suggestions.len(), 2);

        // Modified buffer should show [+]
        let lib_suggestion = suggestions
            .iter()
            .find(|s| s.text.contains("lib.rs"))
            .unwrap();
        assert!(lib_suggestion.text.contains("[+]"));
    }

    #[test]
    fn test_buffer_provider_filter() {
        let provider = BufferProvider::new();
        let context = make_test_context();

        let suggestions = provider.suggestions("main", &context);
        assert_eq!(suggestions.len(), 1);
        assert!(suggestions[0].text.contains("main.rs"));
    }

    #[test]
    fn test_goto_line_provider() {
        let provider = GotoLineProvider::new();
        let context = make_test_context();

        // Valid line number
        let suggestions = provider.suggestions("42", &context);
        assert_eq!(suggestions.len(), 1);
        assert!(!suggestions[0].disabled);

        // Empty query shows hint
        let suggestions = provider.suggestions("", &context);
        assert_eq!(suggestions.len(), 1);
        assert!(suggestions[0].disabled);

        // Invalid input
        let suggestions = provider.suggestions("abc", &context);
        assert_eq!(suggestions.len(), 1);
        assert!(suggestions[0].disabled);
    }

    #[test]
    fn test_goto_line_on_select() {
        let provider = GotoLineProvider::new();
        let context = make_test_context();

        let result = provider.on_select(Some(0), "42", &context);
        match result {
            QuickOpenResult::GotoLine(line) => assert_eq!(line, 42),
            _ => panic!("Expected GotoLine result"),
        }
    }
}
