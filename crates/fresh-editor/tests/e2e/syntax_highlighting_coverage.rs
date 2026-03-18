//! E2E tests for syntax highlighting coverage across all supported file types.
//!
//! These tests verify that each fixture file in tests/fixtures/syntax_highlighting/
//! loads with the correct syntax detection and produces colored output (where supported).
//!
//! Each test opens a fixture file, renders, and checks:
//! 1. The highlighter is active (syntax was detected)
//! 2. The content area has non-default foreground colors (highlighting is applied)

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use ratatui::style::Color;
use std::path::PathBuf;

/// Get the path to a syntax highlighting fixture file.
fn fixture_path(filename: &str) -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .join("tests/fixtures/syntax_highlighting")
        .join(filename)
}

/// Default foreground color for the high-contrast theme (White = Color::Indexed(15))
const _DEFAULT_FG: Color = Color::Indexed(15);
/// Line number color
const _LINE_NUM_FG: Color = Color::Indexed(244);

/// Helper: open a fixture file and check syntax highlighting.
///
/// Returns (has_highlighter, number_of_distinct_highlight_colors)
/// where highlight colors are foreground colors in the content area that differ
/// from the default text color and line number color.
fn check_highlighting(filename: &str) -> (bool, usize) {
    let path = fixture_path(filename);
    assert!(path.exists(), "Fixture file not found: {}", path.display());

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();

    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let has_hl = harness.has_highlighter();

    // Collect distinct foreground colors from content area (rows 2-20, cols 8-100)
    // Row 0 = menu bar, Row 1 = tab bar, content starts at row 2
    // Cols 0-7 are typically line numbers/gutter
    let mut colors = std::collections::HashSet::new();
    for y in 2..20 {
        for x in 8..100 {
            if let Some(style) = harness.get_cell_style(x, y) {
                if let Some(fg) = style.fg {
                    // Exclude default text, line numbers, empty line tilde, and black
                    match fg {
                        Color::Indexed(15) => {} // default white text
                        Color::Indexed(244) => {} // line numbers
                        Color::Indexed(237) => {} // tilde empty lines
                        Color::Indexed(0) => {}   // black
                        Color::Indexed(236) => {} // dark gray UI
                        Color::Reset => {}
                        _ => {
                            colors.insert(format!("{:?}", fg));
                        }
                    }
                }
            }
        }
    }

    (has_hl, colors.len())
}

// ============================================================
// Tests for languages with WORKING syntax highlighting
// ============================================================

macro_rules! test_highlighting_works {
    ($test_name:ident, $filename:expr, $min_colors:expr) => {
        #[test]
        fn $test_name() {
            let (has_hl, color_count) = check_highlighting($filename);
            assert!(
                has_hl,
                "{}: expected highlighter to be active",
                $filename
            );
            assert!(
                color_count >= $min_colors,
                "{}: expected at least {} highlight colors, got {}",
                $filename,
                $min_colors,
                color_count
            );
        }
    };
}

// --- Tree-sitter highlighted languages ---
test_highlighting_works!(test_highlight_rust, "hello.rs", 2);
test_highlighting_works!(test_highlight_python, "hello.py", 2);
test_highlighting_works!(test_highlight_javascript, "hello.js", 2);
test_highlighting_works!(test_highlight_typescript, "hello.ts", 2);
test_highlighting_works!(test_highlight_tsx, "hello.tsx", 2);
test_highlighting_works!(test_highlight_go, "hello.go", 2);
test_highlighting_works!(test_highlight_c, "hello.c", 2);
test_highlighting_works!(test_highlight_cpp, "hello.cpp", 2);
test_highlighting_works!(test_highlight_json, "hello.json", 2);
test_highlighting_works!(test_highlight_java, "hello.java", 2);
test_highlighting_works!(test_highlight_csharp, "hello.cs", 2);
test_highlighting_works!(test_highlight_php, "hello.php", 2);
test_highlighting_works!(test_highlight_ruby, "hello.rb", 2);
test_highlighting_works!(test_highlight_lua, "hello.lua", 2);
test_highlighting_works!(test_highlight_bash, "hello.sh", 2);
test_highlighting_works!(test_highlight_html, "hello.html", 2);
test_highlighting_works!(test_highlight_css, "hello.css", 2);

// --- Syntect (TextMate) highlighted languages ---
test_highlighting_works!(test_highlight_cc, "hello.cc", 2);
test_highlighting_works!(test_highlight_hpp, "hello.hpp", 2);
test_highlighting_works!(test_highlight_h, "hello.h", 2);
test_highlighting_works!(test_highlight_jsx, "hello.jsx", 2);
test_highlighting_works!(test_highlight_mjs, "hello.mjs", 2);
test_highlighting_works!(test_highlight_python_stub, "hello.pyi", 2);
test_highlighting_works!(test_highlight_scala, "hello.scala", 2);
test_highlighting_works!(test_highlight_haskell, "hello.hs", 2);
test_highlighting_works!(test_highlight_clojure, "hello.clj", 2);
test_highlighting_works!(test_highlight_erlang, "hello.erl", 2);
test_highlighting_works!(test_highlight_ocaml, "hello.ml", 2);
test_highlighting_works!(test_highlight_perl, "hello.pl", 2);
test_highlighting_works!(test_highlight_lisp, "hello.lisp", 2);
test_highlighting_works!(test_highlight_d, "hello.d", 2);
test_highlighting_works!(test_highlight_r, "hello.r", 2);
test_highlighting_works!(test_highlight_sql, "hello.sql", 2);
test_highlighting_works!(test_highlight_latex, "hello.tex", 2);
test_highlighting_works!(test_highlight_markdown, "hello.md", 2);
test_highlighting_works!(test_highlight_yaml, "hello.yaml", 2);
test_highlighting_works!(test_highlight_xml, "hello.xml", 2);
test_highlighting_works!(test_highlight_batch, "hello.bat", 2);

// --- Embedded grammar languages ---
test_highlighting_works!(test_highlight_toml, "hello.toml", 2);
test_highlighting_works!(test_highlight_zig, "hello.zig", 2);
test_highlighting_works!(test_highlight_odin, "hello.odin", 2);
test_highlighting_works!(test_highlight_typst, "hello.typ", 2);
test_highlighting_works!(test_highlight_gitconfig, "hello.gitconfig", 2);

// --- Alternate filenames/extensions that should work ---
test_highlighting_works!(test_highlight_bash_ext, "hello.bash", 2);
test_highlighting_works!(test_highlight_zsh, "hello.zsh", 2);
test_highlighting_works!(test_highlight_makefile, "Makefile", 2);
test_highlighting_works!(test_highlight_gnumakefile, "GNUmakefile", 2);
test_highlighting_works!(test_highlight_mk, "hello.mk", 2);
test_highlighting_works!(test_highlight_gemfile, "Gemfile", 2);
test_highlighting_works!(test_highlight_rakefile, "Rakefile", 2);
test_highlighting_works!(test_highlight_vagrantfile, "Vagrantfile", 2);
test_highlighting_works!(test_highlight_docker_compose, "docker-compose.yml", 2);

// ============================================================
// Tests for files where syntax is detected but highlighting is
// partial or absent (known issues documented in the review)
// ============================================================

macro_rules! test_syntax_detected {
    ($test_name:ident, $filename:expr) => {
        #[test]
        fn $test_name() {
            let (has_hl, _color_count) = check_highlighting($filename);
            assert!(
                has_hl,
                "{}: expected highlighter to be active (syntax detected)",
                $filename
            );
        }
    };
}

// These files have syntax detected but may not produce highlight colors
// due to scope mapping gaps or tree-sitter fallback issues.
test_syntax_detected!(test_detect_gitignore, "hello.gitignore");
test_syntax_detected!(test_detect_diff, "hello.diff");

// ============================================================
// Tests for files that currently have NO highlighting
// These serve as regression markers - when support is added,
// upgrade them to test_highlighting_works!
// ============================================================

macro_rules! test_no_highlighting_yet {
    ($test_name:ident, $filename:expr) => {
        #[test]
        fn $test_name() {
            let (has_hl, color_count) = check_highlighting($filename);
            // Document current state: no highlighting.
            // When support is added, this test should be changed to
            // test_highlighting_works! and assert has_hl + colors.
            if has_hl && color_count >= 2 {
                // Support was added! This is good - update test to test_highlighting_works!
                eprintln!(
                    "NOTE: {} now has highlighting ({} colors) - upgrade this test!",
                    $filename, color_count
                );
            }
            // For now we just ensure the file opens without crashing
        }
    };
}

// --- Languages not in syntect defaults (no grammar) ---
test_no_highlighting_yet!(test_nohl_kotlin, "hello.kt");
test_no_highlighting_yet!(test_nohl_swift, "hello.swift");
test_no_highlighting_yet!(test_nohl_dart, "hello.dart");
test_no_highlighting_yet!(test_nohl_elixir, "hello.ex");
test_no_highlighting_yet!(test_nohl_fsharp, "hello.fs");
test_no_highlighting_yet!(test_nohl_nix, "hello.nix");
test_no_highlighting_yet!(test_nohl_powershell, "hello.ps1");
test_no_highlighting_yet!(test_nohl_scss, "hello.scss");
test_no_highlighting_yet!(test_nohl_less, "hello.less");
test_no_highlighting_yet!(test_nohl_ini, "hello.ini");
test_no_highlighting_yet!(test_nohl_julia, "hello.jl");
test_no_highlighting_yet!(test_nohl_nim, "hello.nim");
test_no_highlighting_yet!(test_nohl_gleam, "hello.gleam");
test_no_highlighting_yet!(test_nohl_v, "hello.v");
test_no_highlighting_yet!(test_nohl_solidity, "hello.sol");
test_no_highlighting_yet!(test_nohl_cmake, "CMakeLists.txt");

// --- DevOps/Infrastructure files ---
test_no_highlighting_yet!(test_nohl_dockerfile, "Dockerfile");
test_no_highlighting_yet!(test_nohl_dockerfile_dev, "Dockerfile.dev");
test_no_highlighting_yet!(test_nohl_containerfile, "Containerfile");
test_no_highlighting_yet!(test_nohl_terraform, "hello.tf");
test_no_highlighting_yet!(test_nohl_protobuf, "hello.proto");
test_no_highlighting_yet!(test_nohl_graphql, "hello.graphql");
test_no_highlighting_yet!(test_nohl_jenkinsfile, "Jenkinsfile");
test_no_highlighting_yet!(test_nohl_earthfile, "Earthfile");
test_no_highlighting_yet!(test_nohl_tiltfile, "Tiltfile");
test_no_highlighting_yet!(test_nohl_justfile, "justfile");
test_no_highlighting_yet!(test_nohl_bazel, "BUILD.bazel");
test_no_highlighting_yet!(test_nohl_gomod, "go.mod");
test_no_highlighting_yet!(test_nohl_dotenv, "hello.env");
test_no_highlighting_yet!(test_nohl_editorconfig, "hello.editorconfig");

// --- Requested languages (from GitHub issues) ---
test_no_highlighting_yet!(test_nohl_kdl, "hello.kdl");
test_no_highlighting_yet!(test_nohl_nushell, "hello.nu");

// --- Web framework languages ---
test_no_highlighting_yet!(test_nohl_vue, "hello.vue");
test_no_highlighting_yet!(test_nohl_svelte, "hello.svelte");
test_no_highlighting_yet!(test_nohl_astro, "hello.astro");

// --- Extension mapping gaps ---
test_no_highlighting_yet!(test_nohl_cjs, "hello.cjs");
test_no_highlighting_yet!(test_nohl_mts, "hello.mts");
