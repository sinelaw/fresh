//! Reproduction for issue #1598: no syntax highlighting if file extension is
//! missing with version 0.2.24.
//!
//! Reported by @mabod: a file named `test` (no extension) with a shebang
//! `#!/usr/bin/zsh` at the top is not detected as a shell script in
//! 0.2.24, even though it worked in 0.2.23.
//!
//! Opening such a file should detect a shell language (bash/zsh/shell) from
//! the shebang, not fall back to plain `text`.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_extensionless_file_with_zsh_shebang_is_detected_as_shell() {
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();

    // Exactly the file from the issue: no extension, `#!/usr/bin/zsh` shebang.
    let test_file = working_dir.join("test");
    fs::write(
        &test_file,
        "#!/usr/bin/zsh\n\nexport MY_ZSH=\"$0\"\nexport MY_ZSH_VERSION=2.8\n",
    )
    .unwrap();

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

    let language = harness.editor().active_state().language.clone();
    let display_name = harness.editor().active_state().display_name.clone();

    eprintln!(
        "Detected language={:?} display_name={:?}",
        language, display_name
    );

    assert_ne!(
        language, "text",
        "extensionless file with `#!/usr/bin/zsh` shebang should be detected \
         as a shell language via shebang, not fall back to plain text \
         (display_name was {:?})",
        display_name
    );
}
