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

        let symbols = Symbol::from_tokens(&tokens);

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
}
