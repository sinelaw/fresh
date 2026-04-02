//! QuickLSP Feature Evaluation Test
//!
//! Indexes fixture files and exercises every LSP feature, printing full results
//! for manual assessment. Run with:
//!
//!   cargo test -p quicklsp --test lsp_evaluation -- --nocapture

use std::path::{Path, PathBuf};

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
fn evaluate_summary_stats() {
    let ws = setup_workspace();

    print_separator("9. SUMMARY STATISTICS");

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
