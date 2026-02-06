use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Test auto-close quotes in Rust (should happen)
#[test]
fn test_auto_close_quotes_rust() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();

    // Verify language detection
    let language = &harness.editor().active_state().language;
    assert_eq!(language, "rust", "Language should be rust");

    // Type a quote
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    // Should have two quotes: ""
    harness.assert_buffer_content("\"\"");

    // Cursor should be at 1 (between the quotes)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 1);
}

/// Test auto-close quotes in Text (should NOT happen)
#[test]
fn test_no_auto_close_quotes_text() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();

    // Type a quote
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    // Should have only one quote: "
    harness.assert_buffer_content("\"");

    // Cursor should be at 1 (after the quote)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 1);
}

/// Test word movement with punctuation
#[test]
fn test_word_movement_e2e() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "foo.bar").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();

    // Cursor at 0. Ctrl+Right.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();

    // Should stop at '.' (pos 3)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 3);

    // Ctrl+Right again.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();

    // Should stop at 'bar' (pos 4)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 4);
}

/// Test that C++20 module file extensions (.cppm and .ixx) are detected as C++
/// Issue #955: C++20 module file extensions should be treated as C++
#[test]
fn test_cpp20_module_file_extensions() {
    let temp_dir = TempDir::new().unwrap();

    // Test .cppm extension
    let cppm_file = temp_dir.path().join("module.cppm");
    std::fs::write(&cppm_file, "export module hello;").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&cppm_file).unwrap();

    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "cpp",
        ".cppm files should be detected as C++ (issue #955)"
    );

    // Test .ixx extension
    let ixx_file = temp_dir.path().join("module.ixx");
    std::fs::write(&ixx_file, "export module hello;").unwrap();

    harness.open_file(&ixx_file).unwrap();

    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "cpp",
        ".ixx files should be detected as C++ (issue #955)"
    );
}

/// Test that Typst (.typ) files are detected as typst language
/// Issue #944: Add Typst language support
#[test]
fn test_typst_file_extension() {
    let temp_dir = TempDir::new().unwrap();

    let typ_file = temp_dir.path().join("document.typ");
    std::fs::write(&typ_file, "#set text(size: 12pt)\n= Hello World\n").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&typ_file).unwrap();

    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "typst",
        ".typ files should be detected as Typst (issue #944)"
    );
}

/// Test that default LSP configurations exist for common languages
/// Issue #946: Add more default LSP configurations
#[test]
fn test_default_lsp_configs_exist() {
    let config = Config::default();

    // Languages that should have LSP configs
    let expected_languages = vec![
        ("rust", "rust-analyzer"),
        ("python", "pylsp"),
        ("javascript", "typescript-language-server"),
        ("typescript", "typescript-language-server"),
        ("html", "vscode-html-language-server"),
        ("css", "vscode-css-language-server"),
        ("c", "clangd"),
        ("cpp", "clangd"),
        ("go", "gopls"),
        ("json", "vscode-json-language-server"),
        ("csharp", "csharp-ls"),
        ("java", "jdtls"),
        ("bash", "bash-language-server"),
        ("lua", "lua-language-server"),
        ("ruby", "solargraph"),
        ("php", "phpactor"),
        ("yaml", "yaml-language-server"),
        ("toml", "taplo"),
        ("typst", "tinymist"),
    ];

    for (language, expected_command) in expected_languages {
        let lsp_config = config.lsp.get(language);
        assert!(
            lsp_config.is_some(),
            "Missing default LSP config for language: {}",
            language
        );
        let lsp_config = lsp_config.unwrap();
        assert_eq!(
            lsp_config.command, expected_command,
            "Wrong LSP command for {}: expected {}, got {}",
            language, expected_command, lsp_config.command
        );
    }
}

/// Test Ctrl+D multicursor
#[test]
fn test_ctrl_d_multicursor_e2e() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "foo foo foo").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();

    // Select first "foo" (0..3)
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 3);
    assert_eq!(cursor.anchor, Some(0));

    // Press Ctrl+D (mapped to 'd' with control usually, relying on keybindings)
    // Assuming default keybindings map Ctrl+D to AddCursorNextMatch
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify 2 cursors
    let count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(count, 2);

    // Check positions: second cursor should be at 7 (4..7)
    let cursors: Vec<_> = harness
        .editor()
        .active_state()
        .cursors
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    assert!(cursors.contains(&7));
}
