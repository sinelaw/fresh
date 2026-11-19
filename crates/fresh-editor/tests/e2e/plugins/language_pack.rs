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

(async function() {{
  // Register the test grammar
  const grammarPath = "{}";
  const result = editor.registerGrammar("testlang", grammarPath, ["tl"]);
  editor.debug(`registerGrammar result: ${{result}}`);

  // Reload to apply (async — waits for background grammar build)
  await editor.reloadGrammars();

  editor.setStatus("Test language registered!");
}})();
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

    // Create harness — 120×24: with `{remote}` on the default
    // status bar the plugin-emitted "Config registered!" message
    // is truncated at 80 cols.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
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
(async function() {{
  editor.registerGrammar("customscript", "{}", ["cscript", "cs2"]);
  await editor.reloadGrammars();
}})();
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

/// #3790: a live-installed language pack whose grammar `name:` differs from
/// the package name (the `fresh --cmd init language` default: package
/// `my-language`, grammar `My Language`) must highlight right away, as it
/// does after a restart.
#[test]
fn test_register_grammar_named_differently_from_package_highlights() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    let grammars_dir = plugins_dir.join("grammars");
    fs::create_dir_all(&grammars_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    let test_grammar = r#"%YAML 1.2
---
name: My Language
scope: source.my-language
file_extensions: [mylang]

contexts:
  main:
    - match: \b(fn|let)\b
      scope: keyword.control.my-language
"#;
    let grammar_path = grammars_dir.join("my-language.sublime-syntax");
    fs::write(&grammar_path, test_grammar).unwrap();

    // What pkg.ts's loadLanguagePack does: everything keyed by the package
    // name.
    let test_plugin = format!(
        r###"
const editor = getEditor();
(async function() {{
  editor.registerGrammar("my-language", "{}", ["mylang"]);
  editor.registerLanguageConfig("my-language", {{
    commentPrefix: "#", blockCommentStart: null, blockCommentEnd: null,
    useTabs: null, tabSize: null, autoIndent: null, showWhitespaceTabs: null,
    formatter: null,
  }});
  await editor.reloadGrammars();
}})();
"###,
        grammar_path.to_string_lossy().replace('\\', "\\\\")
    );
    fs::write(plugins_dir.join("test_my_language.ts"), test_plugin).unwrap();

    let test_file = project_root.join("test.mylang");
    fs::write(&test_file, "fn main\nlet x\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            h.editor().active_state().language == "my-language"
                && h.editor().active_state().highlighter.has_highlighting()
        })
        .unwrap();
}

/// Open `file`, wait for it to be detected as `language`, then check that its
/// tab size is `tab_size` and that Enter on its `# one` line continues the
/// comment (the language's edit rule).
fn assert_language_settings_apply(
    harness: &mut EditorTestHarness,
    language: &str,
    tab_size: usize,
    when: &str,
) {
    use crossterm::event::{KeyCode, KeyModifiers};
    assert!(
        harness.config().languages.contains_key(language),
        "{when}: the config has the language"
    );
    assert_eq!(
        harness.editor().active_state().buffer_settings.tab_size,
        tab_size,
        "{when}: the open buffer keeps the language's tab size"
    );
    let before = harness.get_buffer_content().unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert!(
        harness
            .get_buffer_content()
            .unwrap()
            .starts_with("# one\n# \n"),
        "{when}: the language's edit rule applies: {:?}",
        harness.get_buffer_content()
    );
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), before);
}

/// A config reload re-resolves the open buffers' language settings, so the
/// languages no config file holds must survive it: an installed language
/// pack's (scanned at startup) and one a plugin registers with
/// `registerLanguageConfig`. They were dropped, and their buffers fell back
/// to the global settings and lost their edit rules.
#[test]
fn test_config_reload_keeps_pack_and_registered_languages() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp_dir.path());
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    let grammars_dir = plugins_dir.join("grammars");
    fs::create_dir_all(&grammars_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    let grammar = |name: &str| {
        format!(
            "%YAML 1.2\n---\nname: {name}\nscope: source.{name}\nfile_extensions: [{name}]\n\ncontexts:\n  main:\n    - match: '#.*$'\n      scope: comment.line.{name}\n"
        )
    };
    let rule = r#"{ "on": "newline", "before": "^(\\s*# ?)\\S", "insert": "\\n$1" }"#;

    // An installed language pack, as the package manager leaves it.
    let pack_dir = dir_context
        .config_dir
        .join("languages")
        .join("packages")
        .join("tpack");
    fs::create_dir_all(pack_dir.join("grammars")).unwrap();
    fs::write(
        pack_dir.join("grammars").join("tpack.sublime-syntax"),
        grammar("tpack"),
    )
    .unwrap();
    fs::write(
        pack_dir.join("package.json"),
        format!(
            r##"{{
    "name": "tpack",
    "version": "1.0.0",
    "description": "test pack",
    "type": "language",
    "fresh": {{
        "grammar": {{ "file": "grammars/tpack.sublime-syntax", "extensions": ["tpack"] }},
        "language": {{ "commentPrefix": "#", "tabSize": 3, "editRules": [{rule}] }}
    }}
}}"##
        ),
    )
    .unwrap();

    // A language a plugin registers.
    let grammar_path = grammars_dir.join("treg.sublime-syntax");
    fs::write(&grammar_path, grammar("treg")).unwrap();
    fs::write(
        plugins_dir.join("test_treg.ts"),
        format!(
            r###"
const editor = getEditor();
(async function() {{
  editor.registerGrammar("treg", "{}", ["treg"]);
  editor.registerLanguageConfig("treg", {{
    commentPrefix: "#", blockCommentStart: null, blockCommentEnd: null,
    useTabs: null, tabSize: 5, autoIndent: null, showWhitespaceTabs: null,
    formatter: null,
    editRules: [{rule}],
  }});
  await editor.reloadGrammars();
}})();
"###,
            grammar_path.to_string_lossy().replace('\\', "\\\\")
        ),
    )
    .unwrap();

    let pack_file = project_root.join("a.tpack");
    let reg_file = project_root.join("b.treg");
    fs::write(&pack_file, "# one\n").unwrap();
    fs::write(&reg_file, "# one\n").unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        100,
        24,
        Default::default(),
        project_root.clone(),
        dir_context,
    )
    .unwrap();
    harness
        .wait_until(|h| h.config().languages.contains_key("treg"))
        .unwrap();

    harness.open_file(&pack_file).unwrap();
    harness
        .wait_until(|h| h.editor().active_state().language == "tpack")
        .unwrap();
    assert_language_settings_apply(&mut harness, "tpack", 3, "pack, before the reload");
    harness.open_file(&reg_file).unwrap();
    harness
        .wait_until(|h| h.editor().active_state().language == "treg")
        .unwrap();
    assert_language_settings_apply(&mut harness, "treg", 5, "registered, before the reload");

    harness.editor_mut().reload_config();

    assert_language_settings_apply(&mut harness, "treg", 5, "registered, after the reload");
    harness.open_file(&pack_file).unwrap();
    assert_eq!(harness.editor().active_state().language, "tpack");
    assert_language_settings_apply(&mut harness, "tpack", 3, "pack, after the reload");
}
