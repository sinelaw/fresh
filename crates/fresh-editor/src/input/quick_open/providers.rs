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

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        let registry = self.command_registry.read().unwrap();
        let keybindings = self.keybinding_resolver.read().unwrap();

        registry.filter(
            query,
            context.key_context.clone(),
            &keybindings,
            context.has_selection,
            &context.custom_contexts,
            context.buffer_mode.as_deref(),
            context.has_lsp_config,
        )
    }

    fn on_select(
        &self,
        suggestion: Option<&Suggestion>,
        _query: &str,
        _context: &QuickOpenContext,
    ) -> QuickOpenResult {
        let suggestion = match suggestion {
            Some(s) if !s.disabled => s,
            Some(_) => {
                return QuickOpenResult::Error(t!("status.command_not_available").to_string())
            }
            None => return QuickOpenResult::None,
        };

        let registry = self.command_registry.read().unwrap();
        let cmd = registry
            .get_all()
            .into_iter()
            .find(|c| c.get_localized_name() == suggestion.text);

        let Some(cmd) = cmd else {
            return QuickOpenResult::None;
        };

        let action = cmd.action.clone();
        let name = cmd.name.clone();
        drop(registry);

        if let Ok(mut reg) = self.command_registry.write() {
            reg.record_usage(&name);
        }
        QuickOpenResult::ExecuteAction(action)
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

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        let mut scored: Vec<(Suggestion, i32, usize)> = context
            .open_buffers
            .iter()
            .filter(|buf| !buf.path.is_empty())
            .filter_map(|buf| {
                let m = if query.is_empty() {
                    crate::input::fuzzy::FuzzyMatch {
                        matched: true,
                        score: 0,
                        match_positions: vec![],
                    }
                } else {
                    fuzzy_match(query, &buf.name)
                };
                if !m.matched {
                    return None;
                }

                let display_name = if buf.modified {
                    format!("{} [+]", buf.name)
                } else {
                    buf.name.clone()
                };

                let suggestion = Suggestion::new(display_name)
                    .with_description(buf.path.clone())
                    .with_value(buf.id.to_string());
                Some((suggestion, m.score, buf.id))
            })
            .collect();

        // Sort by score (higher is better), then by ID (lower = older = higher priority when tied)
        scored.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.2.cmp(&b.2)));
        scored.into_iter().map(|(s, _, _)| s).collect()
    }

    fn on_select(
        &self,
        suggestion: Option<&Suggestion>,
        _query: &str,
        _context: &QuickOpenContext,
    ) -> QuickOpenResult {
        suggestion
            .and_then(|s| s.value.as_deref())
            .and_then(|v| v.parse::<usize>().ok())
            .map(QuickOpenResult::ShowBuffer)
            .unwrap_or(QuickOpenResult::None)
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

    fn suggestions(&self, query: &str, _context: &QuickOpenContext) -> Vec<Suggestion> {
        if query.is_empty() {
            return vec![
                Suggestion::disabled(t!("quick_open.goto_line_hint").to_string())
                    .with_description(t!("quick_open.goto_line_desc").to_string()),
            ];
        }

        match query.parse::<usize>() {
            Ok(n) if n > 0 => {
                vec![
                    Suggestion::new(t!("quick_open.goto_line", line = n.to_string()).to_string())
                        .with_description(t!("quick_open.press_enter").to_string())
                        .with_value(n.to_string()),
                ]
            }
            _ => vec![
                Suggestion::disabled(t!("quick_open.invalid_line").to_string())
                    .with_description(query.to_string()),
            ],
        }
    }

    fn on_select(
        &self,
        suggestion: Option<&Suggestion>,
        _query: &str,
        _context: &QuickOpenContext,
    ) -> QuickOpenResult {
        suggestion
            .and_then(|s| s.value.as_deref())
            .and_then(|v| v.parse::<usize>().ok())
            .filter(|&n| n > 0)
            .map(QuickOpenResult::GotoLine)
            .unwrap_or(QuickOpenResult::None)
    }
}

// ============================================================================
// File Provider (default, no prefix)
// ============================================================================

/// Directory names to skip during file walking (shared with plugin_commands.rs pattern).
const IGNORED_DIRS: &[&str] = &[
    ".git",
    "node_modules",
    "target",
    "__pycache__",
    ".hg",
    ".svn",
    ".DS_Store",
];

const MAX_FILES: usize = 50_000;

/// Provider for finding files in the project
///
/// Uses `git ls-files` via [`ProcessSpawner`] as the fast path (respects
/// `.gitignore`, works on remote hosts), then falls back to recursive
/// directory walking via the [`FileSystem`] trait (works on all platforms
/// including Windows, and on remote filesystems).
pub struct FileProvider {
    /// Cached file list (populated lazily)
    file_cache: std::sync::Arc<std::sync::RwLock<Option<Vec<FileEntry>>>>,
    /// Frecency data for ranking
    frecency: std::sync::Arc<std::sync::RwLock<std::collections::HashMap<String, FrecencyData>>>,
    /// Filesystem abstraction (local or remote)
    filesystem: std::sync::Arc<dyn crate::model::filesystem::FileSystem + Send + Sync>,
    /// Process spawner for running git (local or remote)
    process_spawner: std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
    /// Tokio runtime handle for blocking on async ProcessSpawner calls
    runtime_handle: Option<tokio::runtime::Handle>,
}

// Manual Clone: all fields are Arc/Option<Handle> which are Clone
impl Clone for FileProvider {
    fn clone(&self) -> Self {
        Self {
            file_cache: self.file_cache.clone(),
            frecency: self.frecency.clone(),
            filesystem: self.filesystem.clone(),
            process_spawner: self.process_spawner.clone(),
            runtime_handle: self.runtime_handle.clone(),
        }
    }
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
    pub fn new(
        filesystem: std::sync::Arc<dyn crate::model::filesystem::FileSystem + Send + Sync>,
        process_spawner: std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
        runtime_handle: Option<tokio::runtime::Handle>,
    ) -> Self {
        Self {
            file_cache: std::sync::Arc::new(std::sync::RwLock::new(None)),
            frecency: std::sync::Arc::new(std::sync::RwLock::new(std::collections::HashMap::new())),
            filesystem,
            process_spawner,
            runtime_handle,
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

        // Fast path: git ls-files via ProcessSpawner (works locally and remotely)
        // Fallback: recursive walk via FileSystem trait (works on all platforms)
        let files = self
            .try_git_files(cwd)
            .or_else(|| self.try_walk_dir(cwd))
            .unwrap_or_default();

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

    /// Try listing files via `git ls-files` using the ProcessSpawner.
    ///
    /// This is the fast path: it respects `.gitignore`, is fast on large repos,
    /// and works on remote hosts via RemoteProcessSpawner.
    fn try_git_files(&self, cwd: &str) -> Option<Vec<String>> {
        let handle = self.runtime_handle.as_ref()?;
        let result = handle
            .block_on(self.process_spawner.spawn(
                "git".to_string(),
                vec![
                    "ls-files".to_string(),
                    "--cached".to_string(),
                    "--others".to_string(),
                    "--exclude-standard".to_string(),
                ],
                Some(cwd.to_string()),
            ))
            .ok()?;

        if result.exit_code != 0 {
            return None;
        }

        let files: Vec<String> = result
            .stdout
            .lines()
            .filter(|line| !line.is_empty() && !line.starts_with(".git/"))
            .map(|s| s.to_string())
            .collect();

        Some(files)
    }

    /// Recursive directory walk via the FileSystem trait.
    ///
    /// This is the universal fallback that works on all platforms (including
    /// Windows where git/fd/find may not be available) and on remote
    /// filesystems via RemoteFileSystem.
    fn try_walk_dir(&self, cwd: &str) -> Option<Vec<String>> {
        use std::path::Path;

        let base = Path::new(cwd);
        let mut files = Vec::new();
        let mut stack = vec![base.to_path_buf()];

        while let Some(dir) = stack.pop() {
            let entries = match self.filesystem.read_dir(&dir) {
                Ok(entries) => entries,
                Err(_) => continue,
            };

            for entry in entries {
                // Skip hidden files/dirs (dot-prefixed)
                if entry.name.starts_with('.') {
                    continue;
                }

                match entry.entry_type {
                    crate::model::filesystem::EntryType::File => {
                        if let Ok(rel) = entry.path.strip_prefix(base) {
                            // Normalize to forward slashes for consistent display
                            let rel_str = rel.to_string_lossy().replace('\\', "/");
                            files.push(rel_str);
                            if files.len() >= MAX_FILES {
                                return Some(files);
                            }
                        }
                    }
                    crate::model::filesystem::EntryType::Directory => {
                        if !IGNORED_DIRS.contains(&entry.name.as_str()) {
                            stack.push(entry.path);
                        }
                    }
                    _ => {} // skip symlinks etc.
                }
            }
        }

        if files.is_empty() {
            None
        } else {
            Some(files)
        }
    }
}

impl QuickOpenProvider for FileProvider {
    fn prefix(&self) -> &str {
        ""
    }

    fn suggestions(&self, query: &str, context: &QuickOpenContext) -> Vec<Suggestion> {
        // Strip :line:col suffix so fuzzy matching works when the user appends a jump target
        let (path_part, _, _) = super::parse_path_line_col(query);
        let search_query = if path_part.is_empty() {
            query
        } else {
            &path_part
        };

        let files = self.load_files(&context.cwd);
        if files.is_empty() {
            return vec![Suggestion::disabled(t!("quick_open.no_files").to_string())];
        }

        let max_results = 100;
        let mut scored: Vec<(FileEntry, i32)> = if search_query.is_empty() {
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
            files
                .into_iter()
                .filter_map(|file| {
                    let m = fuzzy_match(search_query, &file.relative_path);
                    if !m.matched {
                        return None;
                    }
                    let frecency_boost = (file.frecency_score / 100.0).min(20.0) as i32;
                    Some((file, m.score + frecency_boost))
                })
                .collect()
        };

        scored.sort_by(|a, b| b.1.cmp(&a.1));
        scored.truncate(max_results);

        scored
            .into_iter()
            .map(|(file, _)| {
                Suggestion::new(file.relative_path.clone()).with_value(file.relative_path)
            })
            .collect()
    }

    fn on_select(
        &self,
        suggestion: Option<&Suggestion>,
        query: &str,
        _context: &QuickOpenContext,
    ) -> QuickOpenResult {
        let (path_part, line, column) = super::parse_path_line_col(query);

        // Use the selected suggestion's path if available
        if let Some(path) = suggestion.and_then(|s| s.value.as_deref()) {
            self.record_access(path);
            return QuickOpenResult::OpenFile {
                path: path.to_string(),
                line,
                column,
            };
        }

        // Fallback: direct path input with :line:col
        if line.is_some() && !path_part.is_empty() {
            self.record_access(&path_part);
            return QuickOpenResult::OpenFile {
                path: path_part,
                line,
                column,
            };
        }

        QuickOpenResult::None
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
            has_lsp_config: true,
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

        let suggestions = provider.suggestions("42", &context);
        let result = provider.on_select(suggestions.first(), "42", &context);
        match result {
            QuickOpenResult::GotoLine(line) => assert_eq!(line, 42),
            _ => panic!("Expected GotoLine result"),
        }
    }
}
