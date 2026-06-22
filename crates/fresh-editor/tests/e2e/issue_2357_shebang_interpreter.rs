//! Reproduction for issue #2357: recognize file type based on shebang.
//!
//! Reported by @shemgp: opening a file whose first line is a shebang should
//! pick up syntax highlighting from the interpreter, not just from the
//! extension. The #1598 fix covered interpreters syntect recognises via a
//! `first_line_match` regex (sh/bash, python, ruby, …), but many languages
//! Fresh ships grammars for — fish, Lua, PowerShell, Tcl, … — define no such
//! regex, so an extensionless `#!/usr/bin/fish` script still fell through to
//! plain text.
//!
//! Opening such a file should now detect the language from the shebang.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use std::fs;
use tempfile::TempDir;

fn detect_language_from_shebang(filename: &str, contents: &str) -> String {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();
    let test_file = working_dir.join(filename);
    fs::write(&test_file, contents).unwrap();

    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_full_grammar_registry()
            .with_working_dir(working_dir.clone()),
    )
    .unwrap();

    harness.open_file(&test_file).unwrap();
    harness.editor().active_state().language.clone()
}

#[test]
fn test_extensionless_fish_shebang_is_detected() {
    // `fish` has no syntect first-line regex; only the interpreter table
    // resolves it. Before the fix this returned "text".
    let language = detect_language_from_shebang(
        "deploy",
        "#!/usr/bin/fish\n\nset -x MY_VAR hello\necho $MY_VAR\n",
    );
    assert_eq!(
        language, "fish",
        "extensionless file with `#!/usr/bin/fish` shebang should be detected as fish, got {language:?}",
    );
}

#[test]
fn test_extensionless_lua_env_shebang_is_detected() {
    let language = detect_language_from_shebang("run", "#!/usr/bin/env lua\n\nprint('hello')\n");
    assert_eq!(
        language, "lua",
        "extensionless file with `#!/usr/bin/env lua` shebang should be detected as lua, got {language:?}",
    );
}
