//! E2E: the status bar names a shell script's grammar "Bash", not syntect's
//! "Bourne Again Shell (bash)" (issue #2727).
//!
//! The long name pushed other status-bar elements out on narrow terminals and
//! is unlike every other language label. Only the status-bar label changes;
//! the grammar keeps its syntect name everywhere it is looked up by.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

fn create_harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

#[test]
fn test_shell_script_status_bar_shows_short_bash_name() {
    for (name, content) in [
        ("script.sh", "echo hi\n"),
        // A `#!/bin/sh` script with no extension, as in the report.
        ("runme", "#!/bin/sh\necho hi\n"),
    ] {
        let mut harness = create_harness();
        let dir = harness.project_dir().unwrap().to_path_buf();
        let path = dir.join(name);
        std::fs::write(&path, content).unwrap();
        harness.open_file(&path).unwrap();
        harness.render().unwrap();

        let status = harness.get_status_bar();
        assert!(
            status.contains(" Bash "),
            "{name}: the status bar should label the grammar `Bash`; got:\n{status}"
        );
        assert!(
            !status.contains("Bourne Again Shell"),
            "{name}: the status bar should not show syntect's long grammar name; got:\n{status}"
        );
    }
}
