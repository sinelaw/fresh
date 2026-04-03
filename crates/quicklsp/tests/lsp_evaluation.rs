//! QuickLSP Feature Evaluation Test
//!
//! Indexes fixture files and exercises every LSP feature, printing full results
//! for manual assessment. Run with:
//!
//!   cargo test -p quicklsp --test lsp_evaluation -- --nocapture

use std::path::{Path, PathBuf};

use quicklsp::deps::DependencyIndex;
use quicklsp::parsing::symbols;
use quicklsp::workspace::Workspace;

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
}

fn setup_workspace() -> Workspace {
    let ws = Workspace::new();
    let dir = fixtures_dir();

    for entry in std::fs::read_dir(&dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_file() {
            let source = std::fs::read_to_string(&path).unwrap();
            ws.index_file(path, source);
        }
    }

    ws
}

fn print_separator(title: &str) {
    println!();
    println!("{}", "=".repeat(72));
    println!("  {title}");
    println!("{}", "=".repeat(72));
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn evaluate_document_symbols() {
    let ws = setup_workspace();
    let dir = fixtures_dir();

    print_separator("1. DOCUMENT SYMBOLS — list all definitions per file");

    for lang in &["sample_rust.rs", "sample_python.py", "sample_typescript.ts"] {
        let path = dir.join(lang);
        let symbols = ws.file_symbols(&path);
        println!("\n  File: {lang}");
        println!("  Symbols found: {}", symbols.len());
        println!(
            "  {:<30} {:<12} {:<6} {:<6} {}",
            "Name", "Kind", "Line", "Col", "Keyword"
        );
        println!(
            "  {:-<30} {:-<12} {:-<6} {:-<6} {:-<10}",
            "", "", "", "", ""
        );
        for s in &symbols {
            println!(
                "  {:<30} {:<12?} {:<6} {:<6} {}",
                s.name, s.kind, s.line, s.col, s.def_keyword
            );
        }
    }
}

#[test]
fn evaluate_go_to_definition() {
    let ws = setup_workspace();

    print_separator("2. GO TO DEFINITION — look up where symbols are defined");

    let queries = &[
        // Rust symbols
        "Config",
        "Server",
        "Handler",
        "Request",
        "Response",
        "Status",
        "create_config",
        "process_request",
        "validate_request",
        "MAX_RETRIES",
        "DEFAULT_TIMEOUT",
        "StatusCode",
        "HandlerResult",
        // Cross-file: same name in multiple languages
        "Server",
        "Config",
        // Unicode
        "données_utilisateur",
        "Über",
        // Module
        "utils",
        // Nested
        "outer",
        "inner",
        // Static/const
        "FINAL_STATUS",
        "GLOBAL_COUNTER",
        // Nonexistent
        "does_not_exist",
        "FooBarBaz",
    ];

    println!("\n  {:<30} {:<6} {}", "Symbol", "Defs", "Locations");
    println!("  {:-<30} {:-<6} {:-<50}", "", "", "");

    for name in queries {
        let defs = ws.find_definitions(name);
        if defs.is_empty() {
            println!("  {:<30} {:<6} (none)", name, 0);
        } else {
            for (i, d) in defs.iter().enumerate() {
                let file = d.file.file_name().unwrap().to_str().unwrap();
                let loc = format!(
                    "{}:{}:{} ({:?})",
                    file,
                    d.symbol.line + 1,
                    d.symbol.col + 1,
                    d.symbol.kind
                );
                if i == 0 {
                    println!("  {:<30} {:<6} {}", name, defs.len(), loc);
                } else {
                    println!("  {:<30} {:<6} {}", "", "", loc);
                }
            }
        }
    }
}

#[test]
fn evaluate_find_references() {
    let ws = setup_workspace();

    print_separator("3. FIND REFERENCES — all usages of a symbol across all files");

    let queries = &[
        "Config",          // used in multiple files and many places
        "MAX_RETRIES",     // constant used in bodies
        "process_request", // function called in multiple places
        "Handler",         // trait/class across languages
        "port",            // field name, very common
        "request",         // parameter name
        "Server",          // class/struct across languages
        "validate_input",  // Python only
        "StatusCode",      // type alias
        "does_not_exist",  // should find nothing
    ];

    for name in queries {
        let refs = ws.find_references(name);
        println!("\n  '{}' — {} references", name, refs.len());
        for r in &refs {
            let file = r.file.file_name().unwrap().to_str().unwrap();
            // Read the actual line from the source to show context
            let source = ws.file_source(&r.file).unwrap_or_default();
            let line_text = source.lines().nth(r.line).unwrap_or("(line not found)");
            let trimmed = line_text.trim();
            let preview = if trimmed.len() > 70 {
                format!("{}...", &trimmed[..67])
            } else {
                trimmed.to_string()
            };
            println!("    {}:{}:{} | {}", file, r.line + 1, r.col + 1, preview);
        }
    }
}

#[test]
fn evaluate_workspace_symbol_search() {
    let ws = setup_workspace();

    print_separator("4. WORKSPACE SYMBOL SEARCH — exact and fuzzy queries");

    let queries = &[
        // Exact matches
        ("Config", "exact match"),
        ("Server", "exact, multi-file"),
        ("process_request", "exact function"),
        // Fuzzy / typo matches
        ("Confg", "missing 'i'"),
        ("Srevr", "missing 'e' twice"),
        ("procses_request", "transposition"),
        ("create_confg", "missing 'i' in compound"),
        ("Hnadler", "transposition"),
        ("MAX_RETIRES", "transposition in constant"),
        // Partial / prefix-like
        ("validate", "prefix-ish"),
        // Nonexistent
        ("zzzzzzz", "total garbage"),
    ];

    for (query, description) in queries {
        let results = ws.search_symbols(query);
        println!("\n  query: '{}' ({})", query, description);
        if results.is_empty() {
            println!("    (no results)");
        } else {
            for r in results.iter().take(5) {
                let file = r.file.file_name().unwrap().to_str().unwrap();
                println!(
                    "    -> {} ({:?}) at {}:{}:{}",
                    r.symbol.name,
                    r.symbol.kind,
                    file,
                    r.symbol.line + 1,
                    r.symbol.col + 1,
                );
            }
            if results.len() > 5 {
                println!("    ... and {} more", results.len() - 5);
            }
        }
    }
}

#[test]
fn evaluate_completions() {
    let ws = setup_workspace();

    print_separator("5. COMPLETIONS — prefix-based symbol suggestions");

    let prefixes = &[
        "Conf", // should suggest Config
        "proc", // should suggest process_request
        "Hand", // should suggest Handler, HandlerResult
        "val",  // should suggest validate_request, validate_input, validate_port
        "Serv", // should suggest Server
        "crea", // should suggest create_config, createConfig
        "MAX",  // should suggest MAX_RETRIES
        "Stat", // should suggest Status, StatusCode
        "donn", // should suggest données_utilisateur (Unicode)
        "xyz",  // should suggest nothing
    ];

    for prefix in prefixes {
        let results = ws.completions(prefix);
        let mut seen = std::collections::HashSet::new();
        let unique: Vec<_> = results
            .iter()
            .filter(|r| seen.insert(r.symbol.name.clone()))
            .take(8)
            .collect();
        let names: Vec<&str> = unique.iter().map(|r| r.symbol.name.as_str()).collect();
        println!("  '{}' -> [{}]", prefix, names.join(", "));
    }
}

#[test]
fn evaluate_cross_language() {
    let ws = setup_workspace();

    print_separator("6. CROSS-LANGUAGE — same symbol names across Rust, Python, TS");

    let shared_names = &[
        "Config", "Server", "Handler", "Request", "Response", "Status",
    ];

    for name in shared_names {
        let defs = ws.find_definitions(name);
        let files: Vec<&str> = defs
            .iter()
            .map(|d| d.file.extension().unwrap().to_str().unwrap())
            .collect();
        println!("  {:<20} defined in: {:?}", name, files);
    }
}

#[test]
fn evaluate_unicode() {
    let ws = setup_workspace();

    print_separator("7. UNICODE — identifiers with non-ASCII characters");

    // Definition lookup
    let unicode_names = &["données_utilisateur", "Über"];
    for name in unicode_names {
        let defs = ws.find_definitions(name);
        if defs.is_empty() {
            println!("  {:<30} MISSING (not found)", name);
        } else {
            for d in &defs {
                println!(
                    "  {:<30} found at line {} col {} ({:?})",
                    name,
                    d.symbol.line + 1,
                    d.symbol.col + 1,
                    d.symbol.kind,
                );
            }
        }
    }

    // References
    println!();
    for name in unicode_names {
        let refs = ws.find_references(name);
        println!("  '{}' — {} references", name, refs.len());
        for r in &refs {
            println!("    line {} col {}", r.line + 1, r.col + 1);
        }
    }

    // Fuzzy search for typos in Unicode names
    println!();
    let typo_queries = &[
        ("donnés_utilisateur", "missing 'e'"),
        ("Übr", "missing 'e'"),
    ];
    for (query, desc) in typo_queries {
        let results = ws.search_symbols(query);
        let names: Vec<&str> = results.iter().map(|r| r.symbol.name.as_str()).collect();
        println!("  fuzzy '{}' ({}) -> {:?}", query, desc, names);
    }
}

#[test]
fn evaluate_file_update() {
    let ws = setup_workspace();

    print_separator("8. FILE UPDATE — re-index after edit, old symbols removed");

    let path = fixtures_dir().join("sample_rust.rs");

    // Before
    let before = ws.find_definitions("Config");
    let before_count = before.iter().filter(|d| d.file == path).count();
    println!(
        "  Before update: 'Config' definitions in sample_rust.rs: {}",
        before_count
    );
    println!(
        "  Before update: 'NewStruct' definitions: {}",
        ws.find_definitions("NewStruct").len()
    );

    // Simulate editing the file: replace Config with NewStruct
    ws.update_file(
        path.clone(),
        "struct NewStruct { x: u32 }\nfn new_function() {}".to_string(),
    );

    let after_config = ws
        .find_definitions("Config")
        .iter()
        .filter(|d| d.file == path)
        .count();
    let after_new = ws.find_definitions("NewStruct").len();
    let after_fn = ws.find_definitions("new_function").len();
    println!(
        "  After update:  'Config' definitions in sample_rust.rs: {}",
        after_config
    );
    println!("  After update:  'NewStruct' definitions: {}", after_new);
    println!("  After update:  'new_function' definitions: {}", after_fn);

    // Config should still exist in the other fixture files
    let other_configs = ws
        .find_definitions("Config")
        .iter()
        .filter(|d| d.file != path)
        .count();
    println!("  Config still in other files: {}", other_configs);
}

#[test]
fn evaluate_hover() {
    let ws = setup_workspace();

    print_separator("9. HOVER — doc comments and signatures on symbol hover");

    let queries = &[
        // Rust symbols with doc comments
        "Config",
        "process_request",
        "validate_request",
        "Server",
        "Handler",
        "MAX_RETRIES",
        "create_config",
        "Status",
        // Python symbols
        "process_request",
        "validate_input",
        // TypeScript symbols
        "processRequest",
        "createConfig",
        // Unicode
        "données_utilisateur",
        // No docs expected
        "outer",
        "inner",
        // Nonexistent
        "does_not_exist",
    ];

    for name in queries {
        let info = ws.hover_info(name);
        println!("\n  hover('{}'):", name);
        match info {
            Some((sig, doc)) => {
                if let Some(s) = &sig {
                    println!("    signature: {}", s);
                }
                if let Some(d) = &doc {
                    for line in d.lines() {
                        println!("    doc: {}", line);
                    }
                }
                if sig.is_none() && doc.is_none() {
                    println!("    (definition found, no docs or signature)");
                }
            }
            None => println!("    (not found)"),
        }
    }
}

#[test]
fn evaluate_signature_help() {
    let ws = setup_workspace();

    print_separator("10. SIGNATURE HELP — parameter hints inside function calls");

    // Simulate cursor positions inside function calls
    // Using the Rust fixture source
    let dir = fixtures_dir();
    let rust_path = dir.join("sample_rust.rs");
    let rust_source = ws.file_source(&rust_path).unwrap();

    // Find lines with function calls to test signature help
    println!("\n  Testing signature help on Rust fixture:");
    for (line_num, line_text) in rust_source.lines().enumerate() {
        if line_text.contains("process_request(") && !line_text.trim().starts_with("fn ")
            && !line_text.trim().starts_with("///")
        {
            // Simulate cursor just after the opening paren
            let col = line_text.find("process_request(").unwrap() + "process_request(".len();
            if let Some((loc, active_param)) =
                ws.signature_help_at(&rust_source, line_num, col)
            {
                println!("    line {}: {}", line_num + 1, line_text.trim());
                println!("    signature: {:?}", loc.symbol.signature);
                println!("    active_param: {}", active_param);
                if let Some(ref sig) = loc.symbol.signature {
                    let params = symbols::extract_parameters(sig);
                    println!("    parameters: {:?}", params);
                }
            }
        }
    }

    // Test parameter extraction separately
    println!("\n  Parameter extraction tests:");
    let test_sigs = &[
        "fn process_request(config: &Config, request: &Request) -> Response",
        "def process_request(config, request)",
        "function processRequest(config: Config, request: Request): Response",
        "fn create_config() -> Config",
        "fn validate_request(request: &Request) -> HandlerResult",
    ];
    for sig in test_sigs {
        let params = symbols::extract_parameters(sig);
        println!("    {} -> {:?}", sig, params);
    }
}

#[test]
fn evaluate_doc_extraction_detail() {
    let ws = setup_workspace();
    let dir = fixtures_dir();

    print_separator("11. DOC EXTRACTION DETAIL — per-file symbol docs and signatures");

    for lang in &["sample_rust.rs", "sample_python.py", "sample_typescript.ts"] {
        let path = dir.join(lang);
        let symbols = ws.file_symbols(&path);
        println!("\n  File: {}", lang);
        println!(
            "  {:<25} {:<10} {:<50} {}",
            "Name", "Kind", "Signature", "Doc (first line)"
        );
        println!(
            "  {:-<25} {:-<10} {:-<50} {:-<30}",
            "", "", "", ""
        );
        for s in &symbols {
            let sig = s
                .signature
                .as_deref()
                .unwrap_or("(none)")
                .chars()
                .take(48)
                .collect::<String>();
            let doc_first = s
                .doc_comment
                .as_ref()
                .and_then(|d| d.lines().next())
                .unwrap_or("(none)");
            let doc_preview: String = doc_first.chars().take(28).collect();
            println!(
                "  {:<25} {:<10?} {:<50} {}",
                s.name, s.kind, sig, doc_preview
            );
        }
    }
}

#[test]
fn evaluate_dependency_indexing() {
    use quicklsp::deps;

    print_separator("12. DEPENDENCY INDEXING — manifest-driven, incremental");

    let dep_index = DependencyIndex::new();

    // Detect and resolve from repo root
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("should find repo root");

    println!("  Repo root: {}", repo_root.display());

    // Parse Cargo.lock to show the manifest parsing works
    let cargo_deps = deps::cargo::parse_lock_file(repo_root);
    println!("  Cargo.lock: {} transitive dependencies", cargo_deps.len());
    for (name, version) in cargo_deps.iter().take(5) {
        println!("    {} v{}", name, version);
    }
    if cargo_deps.len() > 5 {
        println!("    ... and {} more", cargo_deps.len() - 5);
    }

    // Resolve and index only the first few packages for the test
    let resolved = deps::cargo::resolve_package_dirs(repo_root, &cargo_deps);
    println!("  Resolved {} packages on disk", resolved.len());

    // Index just a few to verify the mechanism (don't block the test)
    let to_index: Vec<_> = resolved.into_iter().take(5).collect();
    println!("\n  Indexing {} packages for test...", to_index.len());
    for pkg in &to_index {
        println!(
            "    {:?}: {}",
            pkg.ecosystem,
            pkg.path.file_name().unwrap_or_default().to_string_lossy()
        );
    }

    // Feed them into the dep index
    dep_index.enqueue_packages(to_index);
    dep_index.index_pending();

    println!(
        "  Result: {} packages, {} files, {} definitions",
        dep_index.package_count(),
        dep_index.file_count(),
        dep_index.definition_count()
    );

    // Try hover lookups on dep symbols
    let dep_symbols = &["DashMap", "LanguageServer", "DeletionIndex", "Token"];
    println!("\n  Dependency hover lookups:");
    for name in dep_symbols {
        let info = dep_index.hover_info(name);
        match info {
            Some((sig, doc)) => {
                let sig_preview = sig
                    .as_deref()
                    .map(|s| s.chars().take(60).collect::<String>())
                    .unwrap_or_else(|| "(no sig)".into());
                let doc_preview = doc
                    .as_ref()
                    .and_then(|d| d.lines().next())
                    .map(|s| s.chars().take(40).collect::<String>())
                    .unwrap_or_else(|| "(no doc)".into());
                println!("    {} -> sig: {} | doc: {}", name, sig_preview, doc_preview);
            }
            None => println!("    {} -> (not found in indexed subset)", name),
        }
    }
}

#[test]
fn evaluate_summary_stats() {
    let ws = setup_workspace();

    print_separator("13. SUMMARY STATISTICS");

    println!("  Files indexed:      {}", ws.file_count());
    println!("  Total definitions:  {}", ws.definition_count());
    println!("  Unique symbols:     {}", ws.unique_symbol_count());

    // Per-file breakdown
    let dir = fixtures_dir();
    for lang in &["sample_rust.rs", "sample_python.py", "sample_typescript.ts"] {
        let path = dir.join(lang);
        let syms = ws.file_symbols(&path);
        println!("  {}: {} symbols", lang, syms.len());
    }
}
