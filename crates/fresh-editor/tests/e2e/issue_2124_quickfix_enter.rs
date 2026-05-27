//! Regression test for issue #2124: pressing Enter on a line in the
//! `*Quickfix*` dock buffer must open the referenced file at its
//! `line:col`, instead of showing "Editing disabled in this buffer".
//!
//! The test drives the real flow — Live Grep → export to Quickfix
//! (Alt+M) → Enter on a match — and asserts only on rendered output and
//! the resulting active buffer (CONTRIBUTING §2). It fails on the
//! pre-fix build (Enter trips the read-only editing guard) and passes
//! once the `quickfix-list` mode binds Enter to `quickfix_goto`.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

#[test]
fn quickfix_enter_opens_match_location() {
    let git_check = std::process::Command::new("git").arg("--version").output();
    if git_check.is_err() || !git_check.as_ref().unwrap().status.success() {
        eprintln!("Skipping test: `git` is not installed or not in PATH");
        return;
    }

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().canonicalize().unwrap().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let run_git = |args: &[&str]| {
        let out = std::process::Command::new("git")
            .arg("-c")
            .arg("commit.gpgsign=false")
            .args(args)
            .current_dir(&project_root)
            .env("GIT_AUTHOR_NAME", "t")
            .env("GIT_AUTHOR_EMAIL", "t@t")
            .env("GIT_COMMITTER_NAME", "t")
            .env("GIT_COMMITTER_EMAIL", "t@t")
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&out.stderr)
        );
    };
    run_git(&["init", "--quiet", "-b", "main"]);

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    let unique = "QUICKFIX_TARGET_5c1d";
    let target_path = project_root.join("target.rs");
    fs::write(&target_path, format!("// {unique}\nfn target() {{}}\n")).unwrap();
    run_git(&["add", "target.rs"]);
    run_git(&["commit", "--quiet", "-m", "seed"]);

    // Start in a different file so the active buffer is clearly not the
    // target until we navigate into it.
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "start\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        140,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Open Live Grep and search for the unique token.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(unique).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("target.rs"))
        .unwrap();

    // Export the results to the Quickfix dock (Alt+M).
    harness
        .send_key(KeyCode::Char('m'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Quickfix") && s.contains("target.rs")
        })
        .unwrap();

    // The dock is focused after export. Step onto the first match line
    // (line 0 is the "Quickfix: …" header) and press Enter.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .map(|c| c.contains(unique) && !c.contains("Quickfix:"))
                .unwrap_or(false)
        })
        .unwrap();

    // The read-only editing guard must never fire on the navigation key.
    harness.assert_screen_not_contains("Editing disabled");

    // The active buffer is now the target file, not the Quickfix list.
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains(unique) && !content.contains("Quickfix:"),
        "Enter on a Quickfix match must open target.rs; active buffer was:\n{content}"
    );
}
