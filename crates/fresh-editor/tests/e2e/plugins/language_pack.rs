//! E2E tests for language pack loading API
//!
//! Tests the plugin API for registering grammars, language configs, and LSP servers.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use std::fs;

/// Test that registerGrammar API works and applies syntax highlighting
#[test]
fn test_register_grammar_api() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(project_root.clone()).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    // Create a grammars directory with a simple test grammar
    let grammars_dir = plugins_dir.join("grammars");
    fs::create_dir(&grammars_dir).unwrap();

    // Create a minimal Sublime syntax grammar for a test language
    let test_grammar = r#"%YAML 1.2
---
name: TestLang
scope: source.testlang
file_extensions: [tl]

contexts:
  main:
    - match: \b(fn|let|if|else)\b
      scope: keyword.control.testlang
    - match: //.*$
      scope: comment.line.testlang
    - match: '"[^"]*"'
      scope: string.quoted.double.testlang
"#;
    let grammar_path = grammars_dir.join("testlang.sublime-syntax");
    fs::write(&grammar_path, test_grammar).unwrap();

    // Create a plugin that registers the grammar
    let test_plugin = format!(
        r###"
const editor = getEditor();

// Register the test grammar
const grammarPath = "{}";
const result = editor.registerGrammar("testlang", grammarPath, ["tl"]);
editor.debug(`registerGrammar result: ${{result}}`);

// Reload to apply
editor.reloadGrammars();

editor.setStatus("Test language registered!");
"###,
        grammar_path.to_string_lossy().replace('\\', "\\\\")
    );

    let test_plugin_path = plugins_dir.join("test_language.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a test file with the .tl extension
    let test_file = project_root.join("test.tl");
    fs::write(&test_file, "fn main() {\n    let x = \"hello\";\n}\n").unwrap();

    // Create harness with the project directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open the test file
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Grammar rebuild happens asynchronously in a background thread after
    // reloadGrammars() fires.  Wait for the async GrammarRegistryBuilt
    // message to be processed, which re-detects syntax for open buffers.
    harness
        .wait_until(|h| h.editor().active_state().language == "testlang")
        .unwrap();

    let language = &harness.editor().active_state().language;
    assert_eq!(
        language, "testlang",
        "Language should be detected as 'testlang', got '{}'",
        language
    );
}

/// Test that registerLanguageConfig API works
#[test]
fn test_register_language_config_api() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(project_root.clone()).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    // Create a plugin that registers language config
    let test_plugin = r####"
const editor = getEditor();

// Register language configuration for a test language
const result = editor.registerLanguageConfig("testlang2", {
    commentPrefix: "//",
    blockCommentStart: "/*",
    blockCommentEnd: "*/",
    tabSize: 2,
    useTabs: false,
    autoIndent: true,
    formatter: null,
});

editor.debug(`registerLanguageConfig result: ${result}`);
editor.setStatus(result ? "Config registered!" : "Config registration failed!");
"####;

    let test_plugin_path = plugins_dir.join("test_config.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create harness
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.render().unwrap();

    // Check status message indicates success
    harness.assert_screen_contains("Config registered!");
}

/// Test grammar with extensions properly detects file type
#[test]
fn test_grammar_extension_detection() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(project_root.clone()).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    let grammars_dir = plugins_dir.join("grammars");
    fs::create_dir(&grammars_dir).unwrap();

    // Minimal grammar in Sublime syntax format
    let test_grammar = r#"%YAML 1.2
---
name: CustomScript
scope: source.customscript
file_extensions: [cscript, cs2]

contexts:
  main:
    - match: \bprint\b
      scope: keyword.other
"#;
    let grammar_path = grammars_dir.join("customscript.sublime-syntax");
    fs::write(&grammar_path, test_grammar).unwrap();

    let test_plugin = format!(
        r###"
const editor = getEditor();
editor.registerGrammar("customscript", "{}", ["cscript", "cs2"]);
editor.reloadGrammars();
"###,
        grammar_path.to_string_lossy().replace('\\', "\\\\")
    );

    let test_plugin_path = plugins_dir.join("test_ext.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create test files with both extensions
    let test_file1 = project_root.join("script.cscript");
    let test_file2 = project_root.join("script.cs2");
    fs::write(&test_file1, "print hello").unwrap();
    fs::write(&test_file2, "print world").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Test first extension - wait for async grammar rebuild to complete
    harness.open_file(&test_file1).unwrap();
    harness.render().unwrap();

    // Grammar rebuild happens asynchronously in a background thread after
    // reloadGrammars() fires.  Wait for the async GrammarRegistryBuilt
    // message to be processed, which re-detects syntax for open buffers.
    harness
        .wait_until(|h| h.editor().active_state().language == "customscript")
        .unwrap();
    let lang1 = harness.editor().active_state().language.clone();

    // Test second extension
    harness.open_file(&test_file2).unwrap();
    harness
        .wait_until(|h| h.editor().active_state().language == "customscript")
        .unwrap();
    let lang2 = harness.editor().active_state().language.clone();

    assert_eq!(
        lang1, "customscript",
        "First extension should detect customscript"
    );
    assert_eq!(
        lang2, "customscript",
        "Second extension should detect customscript"
    );
}
