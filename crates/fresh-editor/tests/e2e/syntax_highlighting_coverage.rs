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
                        Color::Indexed(15) => {}  // default white text
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
            assert!(has_hl, "{}: expected highlighter to be active", $filename);
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

// These files have syntax detected with partial highlighting
test_syntax_detected!(test_detect_gitignore, "hello.gitignore");
test_highlighting_works!(test_highlight_diff, "hello.diff", 2);

// --- New embedded grammars (added for languages not in syntect defaults) ---
test_highlighting_works!(test_highlight_kotlin, "hello.kt", 2);
test_highlighting_works!(test_highlight_swift, "hello.swift", 2);
test_highlighting_works!(test_highlight_dart, "hello.dart", 2);
test_highlighting_works!(test_highlight_elixir, "hello.ex", 2);
test_highlighting_works!(test_highlight_fsharp, "hello.fs", 2);
test_highlighting_works!(test_highlight_nix, "hello.nix", 2);
test_highlighting_works!(test_highlight_powershell, "hello.ps1", 2);
test_highlighting_works!(test_highlight_scss, "hello.scss", 2);
test_highlighting_works!(test_highlight_less, "hello.less", 2);
test_highlighting_works!(test_highlight_ini, "hello.ini", 2);
test_highlighting_works!(test_highlight_julia, "hello.jl", 2);
test_highlighting_works!(test_highlight_nim, "hello.nim", 2);
test_highlighting_works!(test_highlight_gleam, "hello.gleam", 2);
test_highlighting_works!(test_highlight_v, "hello.v", 2);
test_highlighting_works!(test_highlight_solidity, "hello.sol", 2);
test_highlighting_works!(test_highlight_cmake, "CMakeLists.txt", 2);

// --- DevOps/Infrastructure files ---
test_highlighting_works!(test_highlight_dockerfile, "Dockerfile", 2);
test_highlighting_works!(test_highlight_dockerfile_dev, "Dockerfile.dev", 2);
test_highlighting_works!(test_highlight_containerfile, "Containerfile", 2);
test_highlighting_works!(test_highlight_terraform, "hello.tf", 2);
test_highlighting_works!(test_highlight_protobuf, "hello.proto", 2);
test_highlighting_works!(test_highlight_graphql, "hello.graphql", 2);
test_highlighting_works!(test_highlight_earthfile, "Earthfile", 2);
test_highlighting_works!(test_highlight_tiltfile, "Tiltfile", 2);
test_highlighting_works!(test_highlight_justfile, "justfile", 2);
test_highlighting_works!(test_highlight_bazel, "BUILD.bazel", 2);
test_highlighting_works!(test_highlight_gomod, "go.mod", 2);
test_highlighting_works!(test_highlight_dotenv, "hello.env", 2);
test_highlighting_works!(test_highlight_editorconfig, "hello.editorconfig", 2);

// --- Requested languages (from GitHub issues) ---
test_highlighting_works!(test_highlight_kdl, "hello.kdl", 2);
test_highlighting_works!(test_highlight_nushell, "hello.nu", 2);

// --- Web framework languages ---
test_highlighting_works!(test_highlight_vue, "hello.vue", 2);
test_highlighting_works!(test_highlight_svelte, "hello.svelte", 2);
test_highlighting_works!(test_highlight_astro, "hello.astro", 2);

test_highlighting_works!(test_highlight_hyprlang, "hyprland.conf", 2);

// --- HDL languages (Verilog / SystemVerilog / VHDL) ---
test_highlighting_works!(test_highlight_verilog, "hello.vh", 2);
test_highlighting_works!(test_highlight_systemverilog, "hello.sv", 2);
test_highlighting_works!(test_highlight_vhdl, "hello.vhd", 2);

// --- Extension mappings (fixed) ---
test_highlighting_works!(test_highlight_cjs, "hello.cjs", 2);
test_highlighting_works!(test_highlight_mts, "hello.mts", 2);
test_highlighting_works!(test_highlight_jenkinsfile, "Jenkinsfile", 2);
