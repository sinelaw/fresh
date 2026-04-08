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

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

/// A single file entry in the Quick Open file list.
#[derive(Clone, Debug)]
pub struct FileEntry {
    relative_path: String,
    frecency_score: f64,
}

#[derive(Clone)]
struct FrecencyData {
    access_count: u32,
    last_access: std::time::Instant,
}

/// Shared state between the FileProvider and its background loading task.
///
/// Wrapped in a single `Arc<Mutex<>>` to keep the FileProvider struct flat.
struct FileCache {
    /// The cached file list, or `None` if not yet loaded.
    files: Option<std::sync::Arc<Vec<FileEntry>>>,
    /// Whether a background load is in progress.
    loading: bool,
}

/// Provider for finding files in the project.
///
/// Uses `git ls-files` via [`ProcessSpawner`] as the fast path (respects
/// `.gitignore`, works on remote hosts), then falls back to recursive
/// directory walking via the [`FileSystem`] trait.
///
/// File enumeration runs on a background thread to avoid blocking the UI.
/// When the cache is empty, `suggestions()` returns a "Loading…" placeholder
/// and kicks off a background task.  When the task finishes it sends an
/// `AsyncMessage::QuickOpenFilesLoaded` which the editor handles by calling
/// `set_cache()` and refreshing the prompt.
#[derive(Clone)]
pub struct FileProvider {
    cache: std::sync::Arc<std::sync::Mutex<FileCache>>,
    frecency: std::sync::Arc<std::sync::RwLock<std::collections::HashMap<String, FrecencyData>>>,
    filesystem: std::sync::Arc<dyn crate::model::filesystem::FileSystem + Send + Sync>,
    process_spawner: std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
    runtime_handle: Option<tokio::runtime::Handle>,
    async_sender: Option<std::sync::mpsc::Sender<crate::services::async_bridge::AsyncMessage>>,
    /// Cancel flag shared with the background walk task.
    cancel: std::sync::Arc<std::sync::atomic::AtomicBool>,
}

impl FileProvider {
    pub fn new(
        filesystem: std::sync::Arc<dyn crate::model::filesystem::FileSystem + Send + Sync>,
        process_spawner: std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
        runtime_handle: Option<tokio::runtime::Handle>,
        async_sender: Option<std::sync::mpsc::Sender<crate::services::async_bridge::AsyncMessage>>,
    ) -> Self {
        Self {
            cache: std::sync::Arc::new(std::sync::Mutex::new(FileCache {
                files: None,
                loading: false,
            })),
            frecency: std::sync::Arc::new(std::sync::RwLock::new(std::collections::HashMap::new())),
            filesystem,
            process_spawner,
            runtime_handle,
            async_sender,
            cancel: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        }
    }

    /// Clear the file cache (e.g., after file system changes).
    pub fn clear_cache(&self) {
        self.cancel
            .store(true, std::sync::atomic::Ordering::Relaxed);
        if let Ok(mut c) = self.cache.lock() {
            c.files = None;
            c.loading = false;
        }
    }

    /// Cancel any in-progress background file load.
    /// Called when the user closes Quick Open so we don't keep walking.
    pub fn cancel_loading(&self) {
        self.cancel
            .store(true, std::sync::atomic::Ordering::Relaxed);
        if let Ok(mut c) = self.cache.lock() {
            c.loading = false;
        }
    }

    /// Update the file cache with results from a background load.
    pub fn set_cache(&self, files: std::sync::Arc<Vec<FileEntry>>) {
        if let Ok(mut c) = self.cache.lock() {
            c.files = Some(files);
            c.loading = false;
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
        self.frecency
            .read()
            .ok()
            .and_then(|m| m.get(path).map(frecency_score))
            .unwrap_or(0.0)
    }

    /// Get the cached file list, or `None` if not yet loaded.
    ///
    /// If no cache exists and no load is in progress, spawns a background
    /// task that will populate the cache and notify the UI via
    /// `AsyncMessage::QuickOpenFilesLoaded`.
    fn get_or_start_loading(&self, cwd: &str) -> Option<std::sync::Arc<Vec<FileEntry>>> {
        let mut cache = self.cache.lock().ok()?;

        if let Some(files) = &cache.files {
            return Some(std::sync::Arc::clone(files));
        }

        if cache.loading {
            return None; // already loading
        }

        // No cache, not loading — kick off background load
        let (sender, handle) = match (&self.async_sender, &self.runtime_handle) {
            (Some(s), Some(h)) => (s.clone(), h.clone()),
            _ => {
                // No async support — fall back to synchronous load
                drop(cache);
                return self.load_files_sync(cwd);
            }
        };

        cache.loading = true;
        // Reset cancel flag for this new load
        self.cancel
            .store(false, std::sync::atomic::Ordering::Relaxed);
        let cancel = std::sync::Arc::clone(&self.cancel);
        let frecency = std::sync::Arc::clone(&self.frecency);
        let filesystem = std::sync::Arc::clone(&self.filesystem);
        let process_spawner = std::sync::Arc::clone(&self.process_spawner);
        let cwd = cwd.to_string();

        handle.spawn_blocking(move || {
            let files = try_git_files_blocking(&process_spawner, &cwd)
                .or_else(|| try_walk_dir_blocking(&*filesystem, &cwd, &cancel))
                .unwrap_or_default();

            let frecency_map = frecency.read().ok();
            let entries: Vec<FileEntry> = files
                .into_iter()
                .map(|path| {
                    let score = frecency_map
                        .as_ref()
                        .and_then(|m| m.get(&path))
                        .map(frecency_score)
                        .unwrap_or(0.0);
                    FileEntry {
                        relative_path: path,
                        frecency_score: score,
                    }
                })
                .collect();

            // Receiver may be dropped if the editor is shutting down — that's fine.
            drop(sender.send(
                crate::services::async_bridge::AsyncMessage::QuickOpenFilesLoaded(
                    std::sync::Arc::new(entries),
                ),
            ));
        });

        None
    }

    /// Synchronous fallback when no tokio runtime is available (e.g., tests).
    fn load_files_sync(&self, cwd: &str) -> Option<std::sync::Arc<Vec<FileEntry>>> {
        let files = self
            .try_git_files(cwd)
            .or_else(|| self.try_walk_dir(cwd))
            .unwrap_or_default();

        let entries: Vec<FileEntry> = files
            .into_iter()
            .map(|path| FileEntry {
                frecency_score: self.get_frecency_score(&path),
                relative_path: path,
            })
            .collect();

        let files = std::sync::Arc::new(entries);
        self.set_cache(std::sync::Arc::clone(&files));
        Some(files)
    }

    /// Synchronous `try_git_files` — used by the sync fallback path.
    fn try_git_files(&self, cwd: &str) -> Option<Vec<String>> {
        let handle = self.runtime_handle.as_ref()?;
        try_git_files_with_handle(&self.process_spawner, cwd, handle)
    }

    /// Synchronous `try_walk_dir` — used by the sync fallback path.
    fn try_walk_dir(&self, cwd: &str) -> Option<Vec<String>> {
        let cancel = std::sync::atomic::AtomicBool::new(false);
        try_walk_dir_blocking(&*self.filesystem, cwd, &cancel)
    }
}

// ---------------------------------------------------------------------------
// Free functions used by both the sync path and the background task
// ---------------------------------------------------------------------------

/// List files via `git ls-files` using a `ProcessSpawner` (blocking).
///
/// Called from `spawn_blocking` so we can't hold a tokio runtime handle —
/// `ProcessSpawner::spawn` is async, so we use `tokio::runtime::Handle::block_on`
/// from *inside* the blocking thread.
fn try_git_files_blocking(
    spawner: &std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
    cwd: &str,
) -> Option<Vec<String>> {
    // Inside spawn_blocking we can use Handle::current() since the runtime is alive.
    let handle = tokio::runtime::Handle::try_current().ok()?;
    try_git_files_with_handle(spawner, cwd, &handle)
}

fn try_git_files_with_handle(
    spawner: &std::sync::Arc<dyn crate::services::remote::ProcessSpawner>,
    cwd: &str,
    handle: &tokio::runtime::Handle,
) -> Option<Vec<String>> {
    let result = handle
        .block_on(spawner.spawn(
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

/// Walk the directory tree via `FileSystem::walk_files` (blocking).
fn try_walk_dir_blocking(
    fs: &dyn crate::model::filesystem::FileSystem,
    cwd: &str,
    cancel: &std::sync::atomic::AtomicBool,
) -> Option<Vec<String>> {
    use std::path::Path;

    let base = Path::new(cwd);
    let mut files = Vec::new();

    // Errors (e.g., root doesn't exist) are treated as "no files found".
    drop(
        fs.walk_files(base, IGNORED_DIRS, cancel, &mut |_path, rel| {
            files.push(rel.to_string());
            files.len() < MAX_FILES
        }),
    );

    if files.is_empty() {
        None
    } else {
        Some(files)
    }
}

/// Compute frecency score for a single entry.
fn frecency_score(data: &FrecencyData) -> f64 {
    let hours_since_access = data.last_access.elapsed().as_secs_f64() / 3600.0;
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
    data.access_count as f64 * recency_weight
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

        // Show a clear error when the remote connection is lost
        if !self.filesystem.is_remote_connected() {
            return vec![Suggestion::disabled(
                "Remote connection lost — cannot list files".to_string(),
            )];
        }

        // Get cached files or kick off background load
        let files = match self.get_or_start_loading(&context.cwd) {
            Some(f) => f,
            None => {
                // Background load in progress — show placeholder
                return vec![Suggestion::disabled("Loading files…".to_string())];
            }
        };

        if files.is_empty() {
            return vec![Suggestion::disabled(t!("quick_open.no_files").to_string())];
        }

        let max_results = 100;

        // Work with references to avoid cloning the whole Arc<Vec>
        let mut scored: Vec<(&FileEntry, i32)> = if search_query.is_empty() {
            let mut entries: Vec<_> = files.iter().map(|f| (f, 0i32)).collect();
            entries.sort_by(|a, b| {
                b.0.frecency_score
                    .partial_cmp(&a.0.frecency_score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            entries.truncate(max_results);
            entries
        } else {
            files
                .iter()
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
                Suggestion::new(file.relative_path.clone()).with_value(file.relative_path.clone())
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::quick_open::BufferInfo;

    fn make_test_context(cwd: &str) -> QuickOpenContext {
        QuickOpenContext {
            cwd: cwd.to_string(),
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
        let context = make_test_context("/tmp");

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
        let context = make_test_context("/tmp");

        let suggestions = provider.suggestions("main", &context);
        assert_eq!(suggestions.len(), 1);
        assert!(suggestions[0].text.contains("main.rs"));
    }

    #[test]
    fn test_goto_line_provider() {
        let provider = GotoLineProvider::new();
        let context = make_test_context("/tmp");

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
        let context = make_test_context("/tmp");

        let suggestions = provider.suggestions("42", &context);
        let result = provider.on_select(suggestions.first(), "42", &context);
        match result {
            QuickOpenResult::GotoLine(line) => assert_eq!(line, 42),
            _ => panic!("Expected GotoLine result"),
        }
    }

    // ====================================================================
    // FileProvider tests
    // ====================================================================

    /// A ProcessSpawner that always fails — forces FileProvider to use the
    /// FileSystem walk fallback, which is exactly the code path that was
    /// broken on Windows and remote filesystems.
    struct FailingSpawner;

    #[async_trait::async_trait]
    impl crate::services::remote::ProcessSpawner for FailingSpawner {
        async fn spawn(
            &self,
            _command: String,
            _args: Vec<String>,
            _cwd: Option<String>,
        ) -> Result<crate::services::remote::SpawnResult, crate::services::remote::SpawnError>
        {
            Err(crate::services::remote::SpawnError::Process(
                "no git in test".to_string(),
            ))
        }
    }

    /// Create a FileProvider backed by StdFileSystem and a FailingSpawner
    /// (no runtime handle, so try_git_files is skipped entirely).
    fn make_file_provider() -> FileProvider {
        FileProvider::new(
            std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
            std::sync::Arc::new(FailingSpawner),
            None, // no runtime → git ls-files path is skipped, sync fallback used
            None, // no async sender → sync fallback used
        )
    }

    #[test]
    fn test_file_provider_discovers_files_via_walk() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        // Create a small project structure
        std::fs::write(base.join("main.rs"), b"fn main() {}").unwrap();
        std::fs::write(base.join("lib.rs"), b"pub mod foo;").unwrap();
        std::fs::create_dir(base.join("src")).unwrap();
        std::fs::write(base.join("src").join("foo.rs"), b"// foo").unwrap();

        let provider = make_file_provider();
        let context = make_test_context(&base.display().to_string());
        let suggestions = provider.suggestions("", &context);

        // Should find all 3 files
        assert_eq!(suggestions.len(), 3);
        let paths: Vec<&str> = suggestions
            .iter()
            .filter_map(|s| s.value.as_deref())
            .collect();
        assert!(paths.contains(&"main.rs"));
        assert!(paths.contains(&"lib.rs"));
        assert!(paths.contains(&"src/foo.rs"));
    }

    #[test]
    fn test_file_provider_skips_ignored_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        std::fs::write(base.join("app.rs"), b"").unwrap();
        // These directories should be skipped
        std::fs::create_dir(base.join("node_modules")).unwrap();
        std::fs::write(base.join("node_modules").join("pkg.js"), b"").unwrap();
        std::fs::create_dir(base.join("target")).unwrap();
        std::fs::write(base.join("target").join("debug.o"), b"").unwrap();

        let provider = make_file_provider();
        let context = make_test_context(&base.display().to_string());
        let suggestions = provider.suggestions("", &context);

        assert_eq!(suggestions.len(), 1);
        assert_eq!(suggestions[0].value.as_deref(), Some("app.rs"));
    }

    #[test]
    fn test_file_provider_skips_hidden_files() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        std::fs::write(base.join("visible.txt"), b"").unwrap();
        std::fs::write(base.join(".hidden"), b"").unwrap();
        std::fs::create_dir(base.join(".git")).unwrap();
        std::fs::write(base.join(".git").join("config"), b"").unwrap();

        let provider = make_file_provider();
        let context = make_test_context(&base.display().to_string());
        let suggestions = provider.suggestions("", &context);

        assert_eq!(suggestions.len(), 1);
        assert_eq!(suggestions[0].value.as_deref(), Some("visible.txt"));
    }

    #[test]
    fn test_file_provider_fuzzy_filter() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path();

        std::fs::write(base.join("main.rs"), b"").unwrap();
        std::fs::write(base.join("lib.rs"), b"").unwrap();
        std::fs::write(base.join("README.md"), b"").unwrap();

        let provider = make_file_provider();
        let context = make_test_context(&base.display().to_string());
        let suggestions = provider.suggestions("main", &context);

        assert_eq!(suggestions.len(), 1);
        assert_eq!(suggestions[0].value.as_deref(), Some("main.rs"));
    }

    #[test]
    fn test_file_provider_empty_dir() {
        let dir = tempfile::tempdir().unwrap();

        let provider = make_file_provider();
        let context = make_test_context(&dir.path().display().to_string());
        let suggestions = provider.suggestions("", &context);

        // Should show "no files" disabled suggestion
        assert_eq!(suggestions.len(), 1);
        assert!(suggestions[0].disabled);
    }
}
