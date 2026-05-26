use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::commands::Suggestion;
use fresh::input::keybindings::Action;
use fresh::services::live_grep_state::{GrepMatch, LiveGrepLastState};
use fresh::view::prompt::PromptType;
use std::fs;

/// End-to-end coverage of the git-grep provider path: in a real git
/// working tree the registry should select git-grep (priority 0),
/// shell out, parse the column-aware output, and surface results in
/// the floating overlay's title and result list.
#[test]
fn test_live_grep_git_grep_flow_finds_match_in_repo() {
    let git_check = std::process::Command::new("git").arg("--version").output();
    if git_check.is_err() || !git_check.as_ref().unwrap().status.success() {
        eprintln!("Skipping test: `git` is not installed or not in PATH");
        return;
    }

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().canonicalize().unwrap().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Initialise a real git repo so `git rev-parse --is-inside-work-tree`
    // succeeds and `isAvailable` returns true for git-grep.
    let run_git = |args: &[&str]| {
        let out = std::process::Command::new("git")
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

    let unique = "GIT_GREP_TOKEN_8a2e";
    let target_path = project_root.join("target.rs");
    fs::write(&target_path, format!("// {unique}\nfn target() {{}}\n")).unwrap();
    // git-grep only reports tracked files, so add + commit.
    run_git(&["add", "target.rs"]);
    run_git(&["commit", "--quiet", "-m", "seed"]);

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

    // Open palette → invoke "Live Grep".
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

    // Two assertions in the rendered overlay:
    //   1. the result list contains target.rs (proves git-grep ran
    //      and parseGrepOutput parsed its `path:line:col:content`
    //      output).
    //   2. the overlay title carries the active provider name
    //      (proves git-grep — not a fallback — was selected).
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("target.rs") && s.contains("git-grep")
        })
        .unwrap();

    // No "⚠" — the error sentinel pushed by the Finder catch when
    // a provider throws. Its absence is the post-fix invariant.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains('⚠'),
        "overlay must not show an error indicator on a successful search; got:\n{}",
        screen
    );

    // Pressing Enter should open the matched file at the unique line.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(unique))
        .unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains(unique),
        "buffer must contain the matched marker after Enter; got: {content}"
    );
}

/// The Buffers scope must find matches in *open* buffers, including
/// unmodified ones, when it is the only file-backed scope active. The
/// buffer scan is otherwise restricted to modified buffers (the disk grep
/// covers saved ones) — but with Files off, that restriction left a
/// buffers-only search finding nothing for content plainly in an open
/// buffer. This drives the real plugin path (no subprocess: the buffer
/// scan is pure JS over `listBuffers`/`getBufferText`).
#[test]
fn test_live_grep_buffers_scope_finds_unmodified_open_buffer() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().canonicalize().unwrap().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    let token = "BUF_SCOPE_TOKEN_7c1d";
    let target = project_root.join("notes.txt");
    fs::write(&target, format!("alpha\n{token} here\nomega\n")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        140,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    // Open the file but make no edits — it's an *unmodified* buffer.
    harness.open_file(&target).unwrap();
    harness.render().unwrap();

    // Open palette → invoke "Live Grep".
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep (Find").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search in:"))
        .unwrap();

    // Narrow to Buffers only: turn off Files (Alt+L) and Terminals (Alt+T),
    // leaving the default-on Buffers scope as the sole file-backed source.
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::ALT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    harness.type_text(token).unwrap();

    // The unmodified open buffer must surface in the result list. Assert on
    // the `path:line` tail (`notes.txt:2`) — the result label is
    // left-truncated when the (temp-dir) path is long, so the leading
    // `[buf]` source badge can scroll off-screen; the trailing
    // `<file>:<line>` stays visible and only ever appears in a result row.
    harness
        .wait_until(|h| h.screen_to_string().contains("notes.txt:2"))
        .unwrap();
}

/// Test Live Grep plugin - basic search and preview functionality
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_basic_search() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create test files with searchable content
    let file1_content = "fn main() {\n    println!(\"Hello, world!\");\n}\n";
    let file2_content = "fn helper() {\n    println!(\"Helper function\");\n}\n";
    let file3_content = "// This file contains UNIQUE_MARKER for testing\nlet x = 42;\n";

    fs::write(project_root.join("main.rs"), file1_content).unwrap();
    fs::write(project_root.join("helper.rs"), file2_content).unwrap();
    fs::write(project_root.join("test.rs"), file3_content).unwrap();

    // Create a file to open initially
    let fixture = TestFixture::new("initial.txt", "Initial file content\n").unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the initial file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Open command palette and find Live Grep
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep to appear in palette (plugin loaded)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Live Grep") || s.contains("Find in Files")
        })
        .unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now we should be in the live grep prompt
    // Type a search query
    harness.type_text("UNIQUE_MARKER").unwrap();

    // Wait for search results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("test.rs") || s.contains("UNIQUE_MARKER")
        })
        .unwrap();

    // Press Escape to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify we're back to normal state
    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("Initial file content")
            || final_screen.contains("Live grep cancelled"),
        "Should return to normal state after ESC. Got:\n{}",
        final_screen
    );
}

/// Test Live Grep - selecting a result opens the file
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_select_result() {
    // Create harness with temp project directory
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with unique content
    let target_content = "// TARGET_FILE\nfn target_function() {\n    let result = 123;\n}\n";
    fs::write(project_root.join("target.rs"), target_content).unwrap();

    // Create initial file in project dir
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "Starting point\n").unwrap();

    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for the target
    harness.type_text("TARGET_FILE").unwrap();

    // Wait for results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("target.rs") || s.contains("TARGET_FILE")
        })
        .unwrap();

    // Press Enter to select the result
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for target file to open
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("TARGET_FILE") || s.contains("target_function")
        })
        .unwrap();
}

/// Test Live Grep - preview split appears and closes on ESC
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_live_grep_preview_split() {
    // Create harness with temp project directory
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with content to search
    let search_content = "PREVIEW_TEST_CONTENT\nLine 2\nLine 3\nLine 4\nLine 5\n";
    fs::write(project_root.join("preview_test.txt"), search_content).unwrap();

    // Create initial file in project dir
    let main_file = project_root.join("main.txt");
    fs::write(&main_file, "Main file\n").unwrap();

    harness.open_file(&main_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for content
    harness.type_text("PREVIEW_TEST").unwrap();

    // Wait for preview split to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("*Preview*") || s.contains("PREVIEW_TEST_CONTENT")
        })
        .unwrap();

    // Press ESC to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Wait for preview split to close
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Preview*"))
        .unwrap();
}

/// Test Live Grep - input is preserved when navigating results
#[test]
fn test_live_grep_input_preserved() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create multiple files with matching content
    for i in 1..=5 {
        let content = format!("MULTI_MATCH line in file {}\n", i);
        fs::write(project_root.join(format!("file{}.txt", i)), content).unwrap();
    }

    // Create initial file
    let fixture = TestFixture::new("start.txt", "Start\n").unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("MULTI_MATCH").unwrap();

    // Wait for results to appear
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("file1.txt") || s.contains("MULTI_MATCH")
        })
        .unwrap();

    // Navigate down through results
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen_after_nav = harness.screen_to_string();
    println!("Screen after navigation:\n{}", screen_after_nav);

    // The prompt should still show "MULTI_MATCH" (input preserved)
    // This verifies our fix that plugin prompts don't overwrite input on navigation
    assert!(
        screen_after_nav.contains("MULTI_MATCH"),
        "Search input should be preserved when navigating results. Got:\n{}",
        screen_after_nav
    );

    // Clean up
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test Live Grep searches in the working directory, not the process current directory
///
/// This test verifies that when the editor's working directory is set to a path
/// different from the process's current directory, Live Grep searches in the
/// working directory (where the user's project is) rather than where fresh was launched.
#[test]
fn test_live_grep_uses_working_dir() {
    // Check if ripgrep is available (required by live_grep plugin)
    let rg_check = std::process::Command::new("rg").arg("--version").output();

    if rg_check.is_err() || !rg_check.as_ref().unwrap().status.success() {
        eprintln!("Skipping test: ripgrep (rg) is not installed or not in PATH");
        eprintln!("Live Grep plugin requires ripgrep to function");
        return;
    }

    // Create a temporary project directory - this will be our working_dir
    // It is intentionally different from std::env::current_dir()
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the live_grep plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // Create a test file with a unique marker that only exists in our temp project
    // This marker should NOT exist in the fresh repo's actual directory
    let unique_marker = "WORKDIR_TEST_UNIQUE_7f3a9b2c";
    let test_content = format!(
        "// This file contains {}\n// It should be found by live grep\nlet x = 42;\n",
        unique_marker
    );
    fs::write(project_root.join("workdir_test.rs"), test_content).unwrap();

    // Create initial file in project dir
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "Starting point for workdir test\n").unwrap();

    // Create harness with working_dir set to project_root
    // This is the key: working_dir != current_dir()
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Start Live Grep via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Grep").unwrap();

    // Wait for Live Grep command to appear (plugin loaded)
    harness
        .wait_until(|h| h.screen_to_string().contains("Live Grep"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for our unique marker
    harness.type_text(unique_marker).unwrap();

    // Wait for results - should find our file in the working directory
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("workdir_test.rs")
        })
        .unwrap();

    // Verify the result is from our working directory
    harness.assert_screen_contains("workdir_test.rs");

    // Press Enter to open the file at the match location
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the file to open - look for the unique marker in the screen
    // (it will appear in the editor content area once the file is loaded)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(unique_marker)
        })
        .unwrap();

    // Verify the buffer content is from our working directory
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains(unique_marker),
        "Buffer should contain the unique marker from working_dir. Got: {}",
        content
    );

    // Verify we're on line 1 (where the marker is)
    // The status bar format is "Ln X, Col Y" (1-indexed)
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Ln 1"),
        "Cursor should be on line 1 (the match line). Status bar: {}",
        status_bar
    );
}

/// Regression test for issue #1796 (capture side): cancelling the
/// Live Grep prompt with no input and no streamed results must NOT
/// populate the Resume cache. Pre-fix, `cancel_prompt` stored
/// `Some(LiveGrepLastState { cached_results: Some(vec![]), .. })`,
/// which combined with the Resume gate's `cached_results.is_some()`
/// check caused Resume to open an empty static popup.
#[test]
fn test_resume_live_grep_capture_skips_empty_dismissal() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .editor_mut()
        .start_prompt("Live grep: ".to_string(), PromptType::LiveGrep);
    // Press Esc immediately — no input typed, no results seeded.
    harness.editor_mut().cancel_prompt();

    assert!(
        harness.editor().live_grep_last_state_for_tests().is_none(),
        "Cancelling Live Grep with empty input must not populate the Resume cache; \
         pre-fix this stored Some(LiveGrepLastState {{ cached_results: Some(vec![]), .. }}) \
         which made Resume open an empty popup."
    );
}

/// Regression test for the bug surfaced after the initial fix shipped:
/// pressing Enter on a Live Grep result jumps to the file but loses
/// the Resume cache, so Alt+r returns the user to a fresh-empty popup
/// instead of their match list. Pre-fix `confirm_prompt` had no
/// caching for Live Grep prompts; only `cancel_prompt` did. Post-fix
/// the confirm path mirrors the cancel path's gates.
#[test]
fn test_resume_live_grep_capture_on_confirm_with_results() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .editor_mut()
        .start_prompt("Live grep: ".to_string(), PromptType::LiveGrep);
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.input = "needle".to_string();
        prompt.cursor_pos = prompt.input.len();
        let mut s = Suggestion::new("src/foo.rs:42".to_string());
        s.description = Some("fn needle() {}".to_string());
        s.value = Some("src/foo.rs:42:1".to_string());
        prompt.suggestions = vec![s];
        prompt.selected_suggestion = Some(0);
    }
    let _ = harness.editor_mut().confirm_prompt();

    let cached = harness
        .editor()
        .live_grep_last_state_for_tests()
        .expect("Confirming Live Grep on a real result must populate the Resume cache");
    assert_eq!(cached.query, "needle");
    assert_eq!(cached.selected_index, Some(0));
    let results = cached
        .cached_results
        .as_ref()
        .expect("cached_results must be Some after confirm");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].file, "src/foo.rs");
    assert_eq!(results[0].line, 42);
}

/// Regression test for issue #1796 (replay side): even if a degenerate
/// `Some(empty Vec)` cache is somehow present, `ResumeLiveGrep` must
/// fall through to the fresh-start path rather than seeding an empty
/// `PromptType::LiveGrep` overlay. Defends against any future code
/// path that writes such a state, independent of the capture-side gate.
#[test]
fn test_resume_live_grep_replay_skips_empty_cache() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .editor_mut()
        .set_live_grep_last_state_for_tests(Some(LiveGrepLastState {
            query: String::new(),
            selected_index: None,
            cached_results: Some(Vec::<GrepMatch>::new()),
            cached_at: None,
            last_results_snapshot_id: None,
        }));
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::ResumeLiveGrep);

    // Plugins aren't loaded in this minimal harness, so the fresh-start
    // path can't create a plugin prompt. Pre-fix the replay branch would
    // still seed a PromptType::LiveGrep overlay from the empty cache —
    // post-fix the gate rejects empty results so no prompt opens.
    let prompt_input = harness.editor().prompt_input();
    assert!(
        prompt_input.is_none(),
        "Resume with an empty cached_results must fall through to the fresh-start \
         path, not seed a PromptType::LiveGrep overlay from the empty cache. \
         Got prompt_input = {:?}",
        prompt_input
    );
}

/// Resume runs the *same* flow as a fresh Live Grep (via the plugin's
/// `resume_live_grep`), seeded with the last query — it no longer builds a
/// bespoke core overlay from the Rust-side cache. In this minimal harness no
/// plugins are loaded, so Resume opens no prompt even when a cache exists;
/// that absence is the regression guard against the old bespoke path.
#[test]
fn test_resume_live_grep_routes_through_plugin_not_core_cache() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness
        .editor_mut()
        .set_live_grep_last_state_for_tests(Some(LiveGrepLastState {
            query: "cached_query".to_string(),
            selected_index: Some(0),
            cached_results: Some(vec![GrepMatch {
                file: "src/foo.rs".to_string(),
                line: 42,
                column: 1,
                content: "fn cached_query() {}".to_string(),
            }]),
            cached_at: None,
            last_results_snapshot_id: None,
        }));
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::ResumeLiveGrep);
    assert_eq!(
        harness.editor().prompt_input(),
        None,
        "Resume must route through the plugin flow, not seed a bespoke core \
         overlay from the Rust cache (no plugins loaded here, so no prompt opens)"
    );
}

/// Regression test: opening the Utility Dock when a side-by-side
/// (vertical) split already exists must place the dock as a sibling
/// of the *root*, so it spans the full width below both panes — not
/// nested under whichever pane was focused.
///
/// Pre-fix the dock-creation site used `split_active_positioned`,
/// which split the active leaf and produced a tree like
/// `Vertical(left, Horizontal(right, dock))` — visually the dock
/// appeared only under the right pane. Post-fix, `split_root_positioned`
/// produces `Horizontal(Vertical(left, right), dock)` so the dock
/// spans the full editor width.
#[test]
fn test_open_terminal_in_dock_spans_full_width_with_existing_vsplit() {
    use fresh::view::split::SplitNode;
    use fresh_core::SplitDirection;

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    // Set up: vertical (side-by-side) split.
    harness.editor_mut().split_pane_vertical();
    {
        let sm = harness.editor().split_manager_for_tests();
        assert!(
            matches!(
                sm.root(),
                SplitNode::Split {
                    direction: SplitDirection::Vertical,
                    ..
                }
            ),
            "precondition: root must be a Vertical Split, got {:?}",
            sm.root()
        );
        assert_eq!(sm.root().count_leaves(), 2);
    }

    // Act: open the dock. Action::OpenTerminalInDock spawns a
    // terminal after creating the dock leaf; the terminal spawn is
    // best-effort in the harness but the split-tree mutation runs
    // synchronously before it, which is what we care about here.
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);

    // Assert: root is now a Horizontal Split whose first child is
    // the original Vertical split (the two side-by-side editor
    // panes) and whose second child is the dock leaf.
    let sm = harness.editor().split_manager_for_tests();
    match sm.root() {
        SplitNode::Split {
            direction: SplitDirection::Horizontal,
            first,
            ..
        } => {
            assert!(
                matches!(
                    first.as_ref(),
                    SplitNode::Split {
                        direction: SplitDirection::Vertical,
                        ..
                    }
                ),
                "first child of root must be the original Vertical split — pre-fix \
                 the dock got nested under the active pane, leaving the root as the \
                 original Vertical and the dock as a child of one of its leaves. Got {:?}",
                first
            );
        }
        other => panic!(
            "root must be a Horizontal Split after dock creation, got {:?}",
            other
        ),
    }
    assert_eq!(
        sm.root().count_leaves(),
        3,
        "expected 3 leaves (left, right, dock); got tree: {:?}",
        sm.root()
    );
}

/// Regression test: a freshly-created Utility Dock must contain only
/// the buffer that triggered its creation (the terminal). Pre-fix
/// the dock was seeded with the user's previously-active buffer as a
/// placeholder, and `open_terminal()` then added the terminal as a
/// second tab — leaving a phantom tab for whatever the user had been
/// editing alongside the terminal in the dock.
#[test]
fn test_open_terminal_in_dock_has_only_terminal_tab() {
    use fresh::view::split::SplitRole;

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    let initial_buffer = harness.editor().active_buffer();

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::OpenTerminalInDock);

    let sm = harness.editor().split_manager_for_tests();
    let dock_leaf = sm
        .find_leaf_by_role(SplitRole::UtilityDock)
        .expect("dock leaf must be created by OpenTerminalInDock");
    let view_state = harness
        .editor()
        .split_view_state_for_tests(dock_leaf)
        .expect("dock leaf must have a SplitViewState");
    let tabs: Vec<_> = view_state.buffer_tab_ids_vec();

    assert_eq!(
        tabs.len(),
        1,
        "fresh dock must contain exactly one tab (the terminal); got {:?}",
        tabs
    );
    assert_ne!(
        tabs[0], initial_buffer,
        "the single tab must NOT be the user's previously-active buffer — \
         pre-fix the dock was seeded with that buffer as a placeholder."
    );
    // The lone tab is the terminal: the leaf's buffer_id and the
    // editor's active_buffer should both point at it (open_terminal
    // sets terminal mode and makes the terminal active).
    let leaf_buffer = sm.get_buffer_id(dock_leaf.into()).expect("leaf has buffer");
    assert_eq!(tabs[0], leaf_buffer);
}

// ===================================================================
// Floating-overlay preview / input bug reproductions
//
// These exercise the centred Live Grep overlay (issue #1796): the
// filter input box and the right-hand preview pane.
//
// They deliberately do NOT drive the async grep backend (ripgrep / git
// grep): the live search path is debounced + spawns a subprocess and is
// known to time out intermittently under the test harness (see the
// `#[ignore = "flaky test"]` tags on the search-driven tests above).
// Instead they open the overlay and seed the result list directly — the
// same pattern the Resume tests use — then drive real keyboard events
// and assert only on rendered screen output. This keeps the preview /
// input behaviour under test deterministic and decoupled from grep.
// ===================================================================

/// Open the editor in a fresh temp project containing `files`, then
/// launch the Live Grep floating overlay (issue #1796). The overlay is
/// wide enough (200 cols) that the right-hand preview pane is shown.
/// Returns the harness plus the `TempDir` guard — keep it alive.
fn open_live_grep_overlay(
    files: &[(&str, &str)],
    config: fresh::config::Config,
) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    for (name, content) in files {
        fs::write(project_root.join(name), content).unwrap();
    }
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "start\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(200, 44, config, project_root).unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    harness
        .editor_mut()
        .start_prompt("Live grep: ".to_string(), PromptType::LiveGrep);
    // The `floatingOverlay` flag is what triggers the preview-pane layout
    // and `prepare_overlay_preview`; the plugin sets it at runtime, so we
    // set it directly here.
    harness.editor_mut().prompt_mut().unwrap().overlay = true;
    harness.render().unwrap();

    (harness, temp_dir)
}

/// Seed the overlay's result list with `labels` (each a parseable
/// `path:line` string, resolved against the working dir) and select
/// `selected`. Mirrors the streamed-results shape the Finder produces.
fn seed_overlay_results(harness: &mut EditorTestHarness, labels: &[&str], selected: Option<usize>) {
    let prompt = harness.editor_mut().prompt_mut().unwrap();
    prompt.suggestions = labels
        .iter()
        .map(|l| Suggestion::new(l.to_string()))
        .collect();
    prompt.selected_suggestion = selected;
}

/// Issue #1: undo/redo must operate on the filter input box, not the
/// underlying buffer. Pre-fix, Ctrl+Z/Ctrl+Y returned `Ignored` from the
/// prompt input handler and fell through to the global buffer
/// undo/redo, so the typed query was never reverted/restored.
#[test]
fn test_live_grep_input_undo_redo() {
    let (mut harness, _tmp) = open_live_grep_overlay(&[], Default::default());

    harness.type_text("ZQXJV").unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Live grep: ZQXJV"),
        "input box should show the typed query; got:\n{}",
        harness.screen_to_string()
    );

    // Undo must revert the typed input.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        !harness.screen_to_string().contains("ZQXJV"),
        "Ctrl+Z must undo the input edit; pre-fix it fell through to the \
         buffer's undo and left the query untouched. Screen:\n{}",
        harness.screen_to_string()
    );

    // Redo must restore it.
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("ZQXJV"),
        "Ctrl+Y must redo the input edit. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Issue #2: when the query stops matching anything (result list goes
/// empty), the preview pane must clear too. Pre-fix,
/// `prepare_overlay_preview` early-returned without dropping the stale
/// `overlay_preview_state`, so the previous match's file content kept
/// rendering in the preview.
#[test]
fn test_live_grep_preview_clears_when_no_matches() {
    // PREVIEWMARKERAAA sits on a context line, so it only ever appears in
    // the preview pane — the result-list label is the parseable
    // `path:line`, never the file's contents.
    let files = &[("aaa.txt", "PREVIEWMARKERAAA\nmatch line here\n")];
    let (mut harness, _tmp) = open_live_grep_overlay(files, Default::default());

    seed_overlay_results(&mut harness, &["aaa.txt:2"], Some(0));
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("PREVIEWMARKERAAA"),
        "preview should show the selected file's context; got:\n{}",
        harness.screen_to_string()
    );

    // Query now matches nothing: the result list clears.
    seed_overlay_results(&mut harness, &[], None);
    harness.render().unwrap();
    assert!(
        !harness.screen_to_string().contains("PREVIEWMARKERAAA"),
        "preview must clear when there are no results; pre-fix the stale \
         match content kept rendering. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Issue #3: navigating between results in *different* files must update
/// the preview to the newly-selected file. Pre-fix, the buffer switch
/// updated the preview view-state but not `OverlayPreviewState.buffer_id`,
/// so the renderer drew the previous file's text at the new file's scroll
/// offset (wrong content, or blank past EOF).
#[test]
fn test_live_grep_preview_follows_selection_across_files() {
    // Each file has a unique context marker on line 1; the marker only
    // appears in the preview pane.
    let files = &[
        ("aaa.txt", "PREVIEWMARKERAAA\nmatch in aaa\n"),
        ("bbb.txt", "PREVIEWMARKERBBB\nmatch in bbb\n"),
    ];
    let (mut harness, _tmp) = open_live_grep_overlay(files, Default::default());

    seed_overlay_results(&mut harness, &["aaa.txt:2", "bbb.txt:2"], Some(0));
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("PREVIEWMARKERAAA") && !screen.contains("PREVIEWMARKERBBB"),
        "preview should start on aaa.txt; got:\n{screen}"
    );

    // Arrow-key down to the second result (bbb.txt).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("PREVIEWMARKERBBB"),
        "preview must follow the selection to bbb.txt; pre-fix it rendered \
         aaa.txt's buffer at bbb.txt's scroll offset (wrong content or \
         blank). Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Issue #4: the preview must wrap long lines so the full match context
/// is visible, rather than horizontally scrolling and pushing earlier
/// parts of the line off-screen. A marker near the middle of a very long
/// line and the token near its end must both be visible at once. The
/// global `line_wrap` is disabled so the bug is exercised: the preview
/// must wrap regardless, since it has no horizontal scroll affordance.
#[test]
fn test_live_grep_preview_wraps_long_lines() {
    // One very long line: padding, MIDMARKERWORD (~col 70), more padding,
    // ENDMATCHWORD (~col 185). Both markers are far past the preview's
    // visible width, so only wrapping can show them together.
    let long_line = format!(
        "{} MIDMARKERWORD {} ENDMATCHWORD\n",
        "x".repeat(70),
        "y".repeat(100)
    );
    let files = &[("long.txt", long_line.as_str())];
    let mut config = fresh::config::Config::default();
    config.editor.line_wrap = false;
    let (mut harness, _tmp) = open_live_grep_overlay(files, config);

    seed_overlay_results(&mut harness, &["long.txt:1"], Some(0));
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("MIDMARKERWORD") && screen.contains("ENDMATCHWORD"),
        "preview must wrap the long match line so both the mid-line marker \
         and the end token are visible; without wrapping the start scrolls \
         off. Screen:\n{screen}"
    );
}

/// Issue #5: non-file scopes (terminals, diagnostics, buffers) carry a
/// source badge in the result-list label — e.g. `[term] …` — which makes
/// the label unparseable as a `path:line:col`. The authoritative location
/// lives in the suggestion's `value`. The overlay preview must use `value`,
/// not the user-facing label; pre-fix it parsed the badge-prefixed label,
/// resolved a bogus path, and rendered an empty preview for terminal-only
/// results.
#[test]
fn test_live_grep_preview_uses_value_not_badged_label() {
    // The real backing file the preview should load. Its context marker
    // only ever appears in the preview pane.
    let files = &[("scrollback.txt", "TERMPREVIEWMARKER\nmatch line here\n")];
    let (mut harness, _tmp) = open_live_grep_overlay(files, Default::default());

    // Mirror a terminal-scope suggestion: the label is badge-prefixed (so
    // parsing it as a path fails / points nowhere), while `value` holds the
    // authoritative relative path:line:col.
    {
        let prompt = harness.editor_mut().prompt_mut().unwrap();
        prompt.suggestions = vec![Suggestion::new("[term] not-a-real-file.txt:1".to_string())
            .with_value("scrollback.txt:2:1".to_string())];
        prompt.selected_suggestion = Some(0);
    }
    harness.render().unwrap();

    assert!(
        harness.screen_to_string().contains("TERMPREVIEWMARKER"),
        "preview must load the file named by the suggestion's `value`, not \
         the badge-prefixed label; pre-fix it parsed `[term] …` into a bogus \
         path and rendered an empty preview. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The floating-overlay search prompt must be **mouse-modal**: clicks on its
/// chrome (input row, toolbar, separator, empty body) — and right-clicks and
/// double-clicks — must never reach the buffer behind it and move its cursor.
/// Regression guard for the §12 full-width header band (which sits over the
/// buffer) and the toolbar controls.
#[test]
fn test_live_grep_overlay_is_mouse_modal() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // A tall, multi-line file so a leaked click would visibly move the cursor.
    let file = project_root.join("buf.txt");
    let body = (0..40)
        .map(|i| format!("line {i} with some words here"))
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(&file, body).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // Buffer cursor starts at offset 0.
    let before = harness.cursor_position();
    assert_eq!(before, 0);

    // Open the Live Grep overlay via the command palette.
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
    assert!(
        harness.screen_to_string().contains("Live grep:"),
        "Live Grep overlay should be open; screen:\n{}",
        harness.screen_to_string()
    );

    // Overlay is 80%×80% centred in 100×30 → x∈[10,90), y∈[3,27). Click on the
    // input row (y=3), the toolbar row (y=4), the separator (y=5) and the body
    // (y=15) — all over the buffer behind the overlay.
    let click = |h: &mut EditorTestHarness, col: u16, row: u16| {
        h.mouse_click(col, row).unwrap();
    };
    for (col, row) in [(50u16, 3u16), (50, 4), (50, 5), (40, 15)] {
        click(&mut harness, col, row);
        assert_eq!(
            harness.cursor_position(),
            before,
            "left-click at ({col},{row}) leaked to the buffer and moved its cursor"
        );
    }

    // Right-click (would otherwise open a context menu / theme popup).
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Right),
            column: 50,
            row: 4,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    assert_eq!(
        harness.cursor_position(),
        before,
        "right-click leaked to the buffer"
    );

    // Double-click (two rapid left presses at the same spot).
    for _ in 0..2 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 40,
                row: 15,
                modifiers: KeyModifiers::empty(),
            })
            .unwrap();
    }
    assert_eq!(
        harness.cursor_position(),
        before,
        "double-click leaked to the buffer"
    );

    // The overlay stays up through all of it (no stray dismissal).
    assert!(
        harness.screen_to_string().contains("Live grep:"),
        "overlay should still be open after the clicks"
    );
}
