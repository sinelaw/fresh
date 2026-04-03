//! Unified Workspace Engine
//!
//! Single data structure that indexes all workspace files and serves every
//! LSP operation. No fallback paths, no probabilistic filters, no separate
//! hot/warm/cold paths.
//!
//! ## How it works
//!
//! 1. Every file is tokenized and its definition symbols stored in a symbol table
//! 2. A reverse index maps symbol names → (file, location) for O(1) definition lookup
//! 3. References are found by word-boundary text search across indexed file contents
//! 4. Fuzzy matching uses precomputed deletion neighborhoods
//! 5. All data is exact — no false positives, no false negatives for definitions

use std::path::{Path, PathBuf};

use dashmap::DashMap;

use tower_lsp::lsp_types::Url;

use crate::fuzzy::deletion_neighborhood::DeletionIndex;
use crate::parsing::symbols::Symbol;
use crate::parsing::tokenizer::{self, LangFamily};

/// A symbol definition with its file location.
#[derive(Debug, Clone)]
pub struct SymbolLocation {
    pub file: PathBuf,
    pub symbol: Symbol,
}

/// A reference (usage) of a name found via text search.
#[derive(Debug, Clone)]
pub struct Reference {
    pub file: PathBuf,
    pub line: usize,
    /// Column as a character offset (Unicode-aware).
    pub col: usize,
    pub len: usize,
}

/// Per-file state stored in the workspace.
struct FileEntry {
    source: String,
    symbols: Vec<Symbol>,
}

/// Unified workspace index. One engine, one path, all operations.
pub struct Workspace {
    /// Per-file parsed state. Source text + extracted symbols.
    files: DashMap<PathBuf, FileEntry>,

    /// Reverse index: symbol name → list of (file, symbol) defining it.
    /// This is the primary lookup structure for go-to-definition.
    definitions: DashMap<String, Vec<SymbolLocation>>,

    /// Fuzzy index for typo-tolerant workspace symbol search and completion.
    fuzzy: std::sync::RwLock<DeletionIndex>,
}

impl Workspace {
    pub fn new() -> Self {
        Self {
            files: DashMap::new(),
            definitions: DashMap::new(),
            fuzzy: std::sync::RwLock::new(DeletionIndex::new()),
        }
    }

    // ── Indexing ─────────────────────────────────────────────────────────

    /// Index a file: tokenize, extract symbols, update all indices.
    pub fn index_file(&self, path: PathBuf, source: String) {
        let lang = path
            .extension()
            .and_then(|e| e.to_str())
            .and_then(LangFamily::from_extension);

        let tokens = lang
            .map(|l| tokenizer::scan(&source, l))
            .unwrap_or_default();

        let mut symbols = Symbol::from_tokens(&tokens);

        // Enrich symbols with doc comments and signatures from the source text
        if let Some(l) = lang {
            Symbol::enrich_from_source(&mut symbols, &source, l);
        }

        // Remove old definitions for this file before inserting new ones
        self.remove_definitions_for_file(&path);

        // Insert into reverse definition index
        for sym in &symbols {
            let loc = SymbolLocation {
                file: path.clone(),
                symbol: sym.clone(),
            };
            self.definitions
                .entry(sym.name.clone())
                .or_default()
                .push(loc);
        }

        // Update fuzzy index
        if let Ok(mut fuzzy) = self.fuzzy.write() {
            for sym in &symbols {
                fuzzy.insert(&sym.name);
            }
        }

        self.files.insert(path, FileEntry { source, symbols });
    }

    /// Scan a directory tree and index all files with supported extensions.
    ///
    /// This is the workspace-level equivalent of dependency indexing: walk the
    /// project root and index every source file so cross-file resolution works
    /// from startup. Safe to call from a background thread — DashMap provides
    /// concurrent read access while scanning is in progress.
    ///
    /// Files already in the index (e.g., from a prior `did_open`) are skipped
    /// unless `force` is true.
    pub fn scan_directory(&self, root: &Path) -> ScanStats {
        let mut stats = ScanStats::default();
        self.scan_dir_recursive(root, &mut stats, 0);
        tracing::info!(
            "Workspace scan complete: {} files indexed, {} skipped, {} errors",
            stats.indexed,
            stats.skipped,
            stats.errors
        );
        stats
    }

    /// Maximum directory depth for workspace scanning.
    const MAX_SCAN_DEPTH: usize = 20;

    fn scan_dir_recursive(&self, dir: &Path, stats: &mut ScanStats, depth: usize) {
        if depth > Self::MAX_SCAN_DEPTH {
            return;
        }

        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                // Skip directories that shouldn't be scanned
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if should_skip_dir(name) {
                        continue;
                    }
                }
                self.scan_dir_recursive(&path, stats, depth + 1);
            } else if path.is_file() {
                // Only index files with extensions the tokenizer supports
                let has_lang = path
                    .extension()
                    .and_then(|e| e.to_str())
                    .and_then(LangFamily::from_extension)
                    .is_some();

                if !has_lang {
                    continue;
                }

                // Skip files already opened by the editor (they have fresher content)
                if self.files.contains_key(&path) {
                    stats.skipped += 1;
                    continue;
                }

                match std::fs::read_to_string(&path) {
                    Ok(source) => {
                        self.index_file(path, source);
                        stats.indexed += 1;
                    }
                    Err(_) => {
                        stats.errors += 1;
                    }
                }
            }
        }
    }

    /// Re-index a file after edits (same as index_file, just a clearer name).
    pub fn update_file(&self, path: PathBuf, source: String) {
        self.index_file(path, source);
    }

    /// Remove a file from all indices.
    pub fn remove_file(&self, path: &Path) {
        self.remove_definitions_for_file(path);
        self.files.remove(path);
    }

    /// Remove all definition entries for a given file from the reverse index.
    fn remove_definitions_for_file(&self, path: &Path) {
        // We need to iterate all definition entries and remove those pointing to this file.
        // This is O(total definitions) in the worst case, but file updates are infrequent.
        let mut empty_keys = Vec::new();
        for mut entry in self.definitions.iter_mut() {
            entry.value_mut().retain(|loc| loc.file != *path);
            if entry.value().is_empty() {
                empty_keys.push(entry.key().clone());
            }
        }
        for key in empty_keys {
            self.definitions.remove(&key);
        }
    }

    // ── Queries ─────────────────────────────────────────────────────────

    /// Find all definitions of a symbol name. O(1) hash lookup.
    pub fn find_definitions(&self, name: &str) -> Vec<SymbolLocation> {
        self.definitions
            .get(name)
            .map(|v| v.value().clone())
            .unwrap_or_default()
    }

    /// Find all references (usages) of a symbol name across all indexed files.
    ///
    /// This does a word-boundary text search on every indexed file's source.
    /// It's the only operation that scans file contents, and it's exhaustive —
    /// no probabilistic filtering, no missed files.
    pub fn find_references(&self, name: &str) -> Vec<Reference> {
        let mut results = Vec::new();

        for entry in self.files.iter() {
            let path = entry.key();
            let file = entry.value();

            find_word_occurrences(name, &file.source, path, &mut results);
        }

        results
    }

    /// Get all symbols defined in a specific file.
    pub fn file_symbols(&self, path: &Path) -> Vec<Symbol> {
        self.files
            .get(path)
            .map(|e| e.symbols.clone())
            .unwrap_or_default()
    }

    /// Get the source text for a file.
    pub fn file_source(&self, path: &Path) -> Option<String> {
        self.files.get(path).map(|e| e.source.clone())
    }

    /// Get the source text for a file by LSP URI.
    pub fn file_source_from_uri(&self, uri: &Url) -> Option<String> {
        let path = uri.to_file_path().ok()?;
        self.file_source(&path)
    }

    /// Search for symbols by name, with fuzzy/typo tolerance.
    /// Returns (symbol_name, locations) pairs.
    pub fn search_symbols(&self, query: &str) -> Vec<SymbolLocation> {
        let names = if let Ok(fuzzy) = self.fuzzy.read() {
            fuzzy
                .resolve(query)
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        } else {
            return Vec::new();
        };

        let mut results = Vec::new();
        for name in names {
            if let Some(locs) = self.definitions.get(&name) {
                results.extend(locs.value().iter().cloned());
            }
        }
        results
    }

    /// Get completion candidates matching a partial name.
    pub fn completions(&self, prefix: &str) -> Vec<SymbolLocation> {
        // Fuzzy resolve includes exact prefix matches via deletion neighborhoods
        self.search_symbols(prefix)
    }

    /// Get hover information for a symbol: signature + doc comment.
    /// Returns (signature, doc_comment) if found.
    pub fn hover_info(&self, name: &str) -> Option<(Option<String>, Option<String>)> {
        let defs = self.find_definitions(name);
        let loc = defs.first()?;
        Some((
            loc.symbol.signature.clone(),
            loc.symbol.doc_comment.clone(),
        ))
    }

    /// Find the function symbol being called at a given position.
    ///
    /// Scans backwards from the cursor position to find the function name
    /// before the opening parenthesis, then returns the symbol's signature
    /// and the active parameter index.
    pub fn signature_help_at(
        &self,
        source: &str,
        line: usize,
        col: usize,
    ) -> Option<(SymbolLocation, usize)> {
        let lines: Vec<&str> = source.lines().collect();
        let current_line = lines.get(line)?;
        let chars: Vec<char> = current_line.chars().collect();

        // Count commas and find the function name by scanning backwards
        let mut comma_count = 0usize;
        let mut paren_depth = 0i32;
        let mut scan_col = col.min(chars.len());

        // First scan backwards on the current line to find matching '('
        while scan_col > 0 {
            scan_col -= 1;
            match chars[scan_col] {
                ')' => paren_depth += 1,
                '(' => {
                    if paren_depth == 0 {
                        // Found the opening paren — now find the function name
                        let mut name_end = scan_col;
                        while name_end > 0 && chars[name_end - 1] == ' ' {
                            name_end -= 1;
                        }
                        let mut name_start = name_end;
                        while name_start > 0
                            && (chars[name_start - 1] == '_'
                                || chars[name_start - 1].is_alphanumeric())
                        {
                            name_start -= 1;
                        }
                        if name_start < name_end {
                            let func_name: String =
                                chars[name_start..name_end].iter().collect();
                            let defs = self.find_definitions(&func_name);
                            if let Some(loc) = defs.into_iter().next() {
                                return Some((loc, comma_count));
                            }
                        }
                        return None;
                    }
                    paren_depth -= 1;
                }
                ',' if paren_depth == 0 => comma_count += 1,
                _ => {}
            }
        }

        None
    }

    /// Number of indexed files.
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Total number of definitions across all files.
    pub fn definition_count(&self) -> usize {
        self.definitions.iter().map(|e| e.value().len()).sum()
    }

    /// Total number of unique symbol names.
    pub fn unique_symbol_count(&self) -> usize {
        self.definitions.len()
    }
}

impl Default for Workspace {
    fn default() -> Self {
        Self::new()
    }
}

// ── Workspace scanning ─────────────────────────────────────────────────

/// Statistics from a workspace directory scan.
#[derive(Debug, Default)]
pub struct ScanStats {
    pub indexed: usize,
    pub skipped: usize,
    pub errors: usize,
}

/// Directories to skip during workspace scanning.
fn should_skip_dir(name: &str) -> bool {
    matches!(
        name,
        "target"
            | "node_modules"
            | ".git"
            | ".hg"
            | ".svn"
            | "__pycache__"
            | ".venv"
            | "venv"
            | ".env"
            | "env"
            | "build"
            | "dist"
            | ".next"
            | ".nuxt"
            | "vendor"
            | ".idea"
            | ".vscode"
    ) || name.starts_with('.')
}

// ── Word-boundary text search ───────────────────────────────────────────

/// Find all occurrences of `word` in `source` that are at word boundaries.
/// Appends results to `out`. Unicode-aware.
fn find_word_occurrences(word: &str, source: &str, file: &Path, out: &mut Vec<Reference>) {
    if word.is_empty() {
        return;
    }

    let mut line = 0usize;
    let mut search_from = 0usize;

    while let Some(byte_pos) = source[search_from..].find(word) {
        let abs_pos = search_from + byte_pos;

        // Count newlines between last position and this match
        for &b in source[search_from..abs_pos].as_bytes() {
            if b == b'\n' {
                line += 1;
            }
        }
        // Find the start of the current line
        let line_start_byte = source[..abs_pos].rfind('\n').map(|p| p + 1).unwrap_or(0);

        let end_pos = abs_pos + word.len();

        // Check word boundaries
        let start_ok = abs_pos == 0 || !is_ident_char_at(source, abs_pos - 1);
        let end_ok = end_pos >= source.len() || !is_ident_char_at(source, end_pos);

        if start_ok && end_ok {
            let col = source[line_start_byte..abs_pos].chars().count();
            out.push(Reference {
                file: file.to_path_buf(),
                line,
                col,
                len: word.chars().count(),
            });
        }

        search_from = abs_pos + word.len().max(1);
    }
}

/// Check if the character at byte position `pos` in `source` is an identifier char.
fn is_ident_char_at(source: &str, pos: usize) -> bool {
    // Get the char that starts at or contains this byte position
    if pos >= source.len() {
        return false;
    }
    // Find the char boundary at or before pos
    let s = &source[pos..];
    if let Some(ch) = s.chars().next() {
        ch == '_' || ch.is_alphanumeric()
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn index_and_find_definitions() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn main() {}\nfn process_data() {}".to_string(),
        );
        ws.index_file(
            PathBuf::from("/src/lib.rs"),
            "fn helper() {}\nfn process_data() {}".to_string(),
        );

        let defs = ws.find_definitions("process_data");
        assert_eq!(defs.len(), 2);
        assert!(defs.iter().any(|d| d.file == PathBuf::from("/src/main.rs")));
        assert!(defs.iter().any(|d| d.file == PathBuf::from("/src/lib.rs")));

        let defs = ws.find_definitions("main");
        assert_eq!(defs.len(), 1);
    }

    #[test]
    fn find_references_across_files() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn main() { process_data(); }".to_string(),
        );
        ws.index_file(
            PathBuf::from("/src/lib.rs"),
            "fn process_data() {}\nfn other() { process_data(); }".to_string(),
        );

        let refs = ws.find_references("process_data");
        assert_eq!(refs.len(), 3); // 1 in main.rs, 2 in lib.rs
    }

    #[test]
    fn references_respect_word_boundaries() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/test.rs"),
            "fn process() {}\nfn process_data() {}\nlet preprocessed = 1;".to_string(),
        );

        let refs = ws.find_references("process");
        // Should match "process" in fn process() but NOT "process_data" or "preprocessed"
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].line, 0);
    }

    #[test]
    fn update_file_replaces_old_symbols() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn old_function() {}".to_string(),
        );
        assert_eq!(ws.find_definitions("old_function").len(), 1);

        ws.update_file(
            PathBuf::from("/src/main.rs"),
            "fn new_function() {}".to_string(),
        );
        assert_eq!(ws.find_definitions("old_function").len(), 0);
        assert_eq!(ws.find_definitions("new_function").len(), 1);
    }

    #[test]
    fn remove_file_clears_all_data() {
        let ws = Workspace::new();
        ws.index_file(PathBuf::from("/src/main.rs"), "fn foo() {}".to_string());
        assert_eq!(ws.file_count(), 1);
        assert_eq!(ws.find_definitions("foo").len(), 1);

        ws.remove_file(Path::new("/src/main.rs"));
        assert_eq!(ws.file_count(), 0);
        assert_eq!(ws.find_definitions("foo").len(), 0);
    }

    #[test]
    fn file_symbols_returns_all_symbols() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/lib.rs"),
            "fn a() {}\nstruct B {}\nenum C {}".to_string(),
        );

        let syms = ws.file_symbols(Path::new("/src/lib.rs"));
        assert_eq!(syms.len(), 3);
        let names: Vec<&str> = syms.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"a"));
        assert!(names.contains(&"B"));
        assert!(names.contains(&"C"));
    }

    #[test]
    fn fuzzy_search_finds_typos() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn process_data() {}".to_string(),
        );

        // Transposition typo
        let results = ws.search_symbols("process_dtaa");
        assert!(
            results.iter().any(|r| r.symbol.name == "process_data"),
            "Should find process_data via fuzzy: {results:?}"
        );
    }

    #[test]
    fn unicode_definitions_and_references() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn über_config() {}\nfn test() { über_config(); }".to_string(),
        );

        let defs = ws.find_definitions("über_config");
        assert_eq!(defs.len(), 1);

        let refs = ws.find_references("über_config");
        assert_eq!(refs.len(), 2); // definition + usage
    }

    #[test]
    fn word_boundary_unicode() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/test.py"),
            "def données(): pass\ndef données_extra(): pass".to_string(),
        );

        let refs = ws.find_references("données");
        // Should match "données" but not "données_extra"
        assert_eq!(refs.len(), 1);
    }

    #[test]
    fn reference_column_is_char_offset() {
        let ws = Workspace::new();
        // "fn x() { café(); }" — café starts at char 9
        ws.index_file(PathBuf::from("/test.rs"), "fn café() {}".to_string());

        let refs = ws.find_references("café");
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].col, 3); // "fn " = 3 chars
    }

    #[test]
    fn cross_language_workspace() {
        let ws = Workspace::new();
        ws.index_file(PathBuf::from("/src/main.rs"), "fn process() {}".to_string());
        ws.index_file(
            PathBuf::from("/src/app.py"),
            "def process():\n    pass".to_string(),
        );
        ws.index_file(
            PathBuf::from("/src/app.js"),
            "function process() {}".to_string(),
        );

        let defs = ws.find_definitions("process");
        assert_eq!(defs.len(), 3);
    }

    #[test]
    fn completions_return_results() {
        let ws = Workspace::new();
        ws.index_file(
            PathBuf::from("/src/main.rs"),
            "fn process_data() {}\nfn process_request() {}".to_string(),
        );

        let results = ws.completions("process_dat");
        assert!(
            results.iter().any(|r| r.symbol.name == "process_data"),
            "Completions should include process_data"
        );
    }

    /// Integration test: index the quicklsp crate's own source files.
    /// Runs in CI without any external repo downloads.
    #[test]
    fn index_own_source() {
        let ws = Workspace::new();
        let src_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");

        let mut file_count = 0;
        index_dir_recursive(&ws, &src_dir, &mut file_count);

        assert!(
            file_count > 5,
            "Should index at least 5 source files, got {file_count}"
        );
        assert!(
            ws.definition_count() > 20,
            "Should find >20 definitions in own source, got {}",
            ws.definition_count()
        );
        assert!(
            ws.unique_symbol_count() > 10,
            "Should find >10 unique symbols, got {}",
            ws.unique_symbol_count()
        );

        // Should find our own types
        let defs = ws.find_definitions("Workspace");
        assert!(!defs.is_empty(), "Should find Workspace definition");

        let defs = ws.find_definitions("QuickLspServer");
        assert!(!defs.is_empty(), "Should find QuickLspServer definition");

        // References should find usages across files
        let refs = ws.find_references("Workspace");
        assert!(
            refs.len() >= 2,
            "Should find >=2 references to Workspace, got {}",
            refs.len()
        );

        // Fuzzy search should work
        let results = ws.search_symbols("Workspce"); // typo
        assert!(
            results.iter().any(|r| r.symbol.name == "Workspace"),
            "Fuzzy search should resolve typo 'Workspce' to 'Workspace'"
        );
    }

    fn index_dir_recursive(ws: &Workspace, dir: &Path, count: &mut usize) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                index_dir_recursive(ws, &path, count);
            } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                if ext == "rs" {
                    if let Ok(source) = std::fs::read_to_string(&path) {
                        ws.index_file(path, source);
                        *count += 1;
                    }
                }
            }
        }
    }

    // ── Scan + didOpen ordering integration tests ──────────────────────

    /// Helper: create a temp directory with two Rust source files.
    fn make_scan_fixture() -> (tempfile::TempDir, PathBuf, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let a = dir.path().join("a.rs");
        let b = dir.path().join("b.rs");
        std::fs::write(&a, "/// Doc for alpha.\nfn alpha() {}").unwrap();
        std::fs::write(&b, "/// Doc for beta.\nfn beta() { alpha(); }").unwrap();
        (dir, a, b)
    }

    #[test]
    fn scan_then_did_open_replaces_with_editor_version() {
        let (dir, a, _b) = make_scan_fixture();
        let ws = Workspace::new();

        // Phase 1: scan indexes both files from disk
        ws.scan_directory(dir.path());
        assert_eq!(ws.find_definitions("alpha").len(), 1);
        assert_eq!(ws.find_definitions("beta").len(), 1);

        // Phase 2: editor opens a.rs with different content (renamed function)
        ws.index_file(a, "fn alpha_v2() {}".to_string());

        // Editor version wins: alpha is gone, alpha_v2 is present
        assert_eq!(ws.find_definitions("alpha").len(), 0);
        assert_eq!(ws.find_definitions("alpha_v2").len(), 1);
        // b.rs still has beta from the scan
        assert_eq!(ws.find_definitions("beta").len(), 1);
    }

    #[test]
    fn did_open_then_scan_preserves_editor_version() {
        let (dir, a, _b) = make_scan_fixture();
        let ws = Workspace::new();

        // Phase 1: editor opens a.rs with modified content
        ws.index_file(a, "fn alpha_edited() {}".to_string());
        assert_eq!(ws.find_definitions("alpha_edited").len(), 1);

        // Phase 2: scan runs — should SKIP a.rs (already in index)
        let stats = ws.scan_directory(dir.path());
        assert_eq!(stats.skipped, 1); // a.rs skipped
        assert_eq!(stats.indexed, 1); // b.rs indexed

        // Editor version preserved
        assert_eq!(ws.find_definitions("alpha_edited").len(), 1);
        assert_eq!(ws.find_definitions("alpha").len(), 0);
        // b.rs picked up from scan
        assert_eq!(ws.find_definitions("beta").len(), 1);
    }

    #[test]
    fn scan_then_did_change_replaces_scanned_version() {
        let (dir, a, _b) = make_scan_fixture();
        let ws = Workspace::new();

        // Phase 1: scan indexes from disk
        ws.scan_directory(dir.path());
        assert_eq!(ws.find_definitions("alpha").len(), 1);
        let info = ws.hover_info("alpha");
        assert!(info.is_some());

        // Phase 2: editor sends didChange (update_file) with new content
        ws.update_file(a, "fn alpha_changed() {}".to_string());

        // Changed version wins
        assert_eq!(ws.find_definitions("alpha").len(), 0);
        assert_eq!(ws.find_definitions("alpha_changed").len(), 1);
    }

    #[test]
    fn scan_only_enables_cross_file_resolution() {
        let (dir, _a, _b) = make_scan_fixture();
        let ws = Workspace::new();

        // No files opened via didOpen — scan is the only source
        ws.scan_directory(dir.path());

        // Both files indexed from scan
        assert_eq!(ws.find_definitions("alpha").len(), 1);
        assert_eq!(ws.find_definitions("beta").len(), 1);

        // Hover works on scanned symbols
        let (sig, doc) = ws.hover_info("alpha").unwrap();
        assert!(sig.unwrap().contains("alpha"));
        assert!(doc.unwrap().contains("Doc for alpha"));

        // Cross-file: beta references alpha
        let refs = ws.find_references("alpha");
        assert!(refs.len() >= 2, "alpha should appear in both files");
    }

    #[test]
    fn scan_skips_excluded_directories() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("src");
        let target = dir.path().join("target");
        let node_modules = dir.path().join("node_modules");
        let git = dir.path().join(".git");

        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(&target).unwrap();
        std::fs::create_dir_all(&node_modules).unwrap();
        std::fs::create_dir_all(&git).unwrap();

        std::fs::write(src.join("lib.rs"), "fn real() {}").unwrap();
        std::fs::write(target.join("gen.rs"), "fn generated() {}").unwrap();
        std::fs::write(node_modules.join("dep.js"), "function dep() {}").unwrap();
        std::fs::write(git.join("hook.rs"), "fn hook() {}").unwrap();

        let ws = Workspace::new();
        ws.scan_directory(dir.path());

        assert_eq!(ws.find_definitions("real").len(), 1);
        assert_eq!(ws.find_definitions("generated").len(), 0);
        assert_eq!(ws.find_definitions("dep").len(), 0);
        assert_eq!(ws.find_definitions("hook").len(), 0);
    }
}
