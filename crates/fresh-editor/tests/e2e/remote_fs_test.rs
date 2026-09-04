use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
use std::sync::Arc;
use std::time::Duration;

/// Effectively-unbounded per-request timeout for the test harness.
///
/// `AgentChannel` defaults to 10s, which is right for a real interactive
/// session and wrong for a test: these run as threads of one binary alongside
/// ~4700 others, each `create_test_filesystem` spins up its own multi-threaded
/// Tokio runtime and a `python3` agent process, and a loaded machine can take
/// longer than that to turn round even the tiny `info` request while still
/// making forward progress. A blown deadline is not a failed assertion — it
/// surfaces as `home_dir()` handing back `None` and the `unwrap` below
/// panicking, which reads like the anchoring regression the test is here to
/// catch. Removing the deadline removes the ambiguity (and the time
/// dependency CONTRIBUTING.md §Testing 3 rules out); `remote_filesystem_tests`
/// does the same for the same reason.
const TEST_HARNESS_REQUEST_TIMEOUT: Duration = Duration::from_secs(60 * 60);

fn create_test_filesystem() -> Option<(RemoteFileSystem, tempfile::TempDir, tokio::runtime::Runtime)>
{
    let temp_dir = tempfile::tempdir().ok()?;
    let rt = tokio::runtime::Runtime::new().ok()?;

    let channel = rt.block_on(spawn_local_agent()).ok()?;
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
    let fs = RemoteFileSystem::new(channel, "test@localhost".to_string());

    Some((fs, temp_dir, rt))
}

/// Test that the file explorer root is anchored at the provided working directory,
/// not the remote home directory. Reproduces the bug where `fresh user@host:/some/path`
/// would show the home directory instead of `/some/path` in the file explorer.
#[test]
fn test_remote_file_explorer_anchored_at_working_dir() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };
    let fs_arc: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> = Arc::new(fs);

    // Create a subdirectory to use as the working dir (simulating user@host:/path/to/project)
    //
    // In the test's own temp dir, not under `home_dir()`. The local agent's
    // home *is* the real `$HOME` of whatever machine runs the suite, so the
    // fixture used to be built at a fixed `~/my_test_project` shared by every
    // run and every concurrent test — no isolation at all, against
    // CONTRIBUTING.md Testing §4, and leaving debris in the user's home
    // directory besides. The tempdir was already being created; it was just
    // bound to `_temp_dir` and never used.
    //
    // What the test needs from `home_dir()` is only that the working dir is
    // *not* it, which any path outside home satisfies.
    let home_dir = fs_arc.home_dir().unwrap();
    let project_dir = temp_dir.path().join("my_test_project");
    std::fs::create_dir_all(&project_dir).unwrap();
    std::fs::write(project_dir.join("hello.txt"), "hello world").unwrap();
    std::fs::create_dir_all(project_dir.join("src")).unwrap();
    std::fs::write(project_dir.join("src/main.rs"), "fn main() {}").unwrap();

    let mut harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_working_dir(project_dir.clone())
            .with_filesystem(fs_arc),
    )
    .unwrap();

    // Open file explorer with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();

    // The file explorer root should show "my_test_project" (the working dir name)
    // as the first directory entry, NOT the home directory name.
    let home_dir_name = home_dir.file_name().unwrap().to_string_lossy().to_string();

    // Wait for the async tree build to land — the panel chrome (remote
    // "[hostname]" title) appears immediately with a "Loading…" body, so
    // the title is not the ready signal; a rendered directory entry is.
    // Either name resolves the wait: the assert below decides which one
    // (project = fixed, home = the regression) actually anchored the root.
    //
    // Restricted to explorer *tree* rows. `home_dir_name` is a bare path
    // component — "runner" on a GitHub runner, "root" in a container — and
    // matching it anywhere on screen made the wait resolve on unrelated text
    // (a status-bar path, or the substring inside "project_root"). The assert
    // below then picked that same unrelated line and failed on it.
    let is_tree_row = |line: &str| {
        line.contains('│') || line.contains('>') || line.contains('▼') || line.contains('└')
    };
    let names_a_dir =
        |line: &str| line.contains("my_test_project") || line.contains(&home_dir_name);
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|l| is_tree_row(l) && names_a_dir(l))
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("File explorer screen:\n{screen}");

    // Find the first line in the explorer that contains a directory name.
    // The root node appears first; if the bug is present it will be the
    // home dir (e.g. "root") instead of "my_test_project".
    let first_dir_line = screen.lines().find(|l| is_tree_row(l) && names_a_dir(l));

    let first_dir_line = first_dir_line.expect("File explorer should show directory entries");
    assert!(
        first_dir_line.contains("my_test_project"),
        "First directory in file explorer should be 'my_test_project' (the working dir), \
         but got: {:?}. The explorer is rooted at the home directory '{}' instead.",
        first_dir_line,
        home_dir_name,
    );
}

#[test]
fn test_remote_fs_large_file_edits() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };
    let fs_arc = Arc::new(fs);

    let file_path = temp_dir.path().join("remote_large.txt");

    // Create 100 lines, ~10KB (enough for 500 byte threshold)
    let mut content = String::new();
    let mut expected_lines = Vec::new();
    for i in 0..100 {
        let line = format!("Line {:03}: original content\n", i);
        content.push_str(&line);
        expected_lines.push(line);
    }
    // Write using std::fs (shared temp dir)
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    large_file_threshold_bytes: 500, // Force large file mode
                    auto_indent: false,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(fs_arc),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // 1. Edit Beginning (Line 0)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("START_EDIT ").unwrap();
    expected_lines[0] = format!("START_EDIT {}", expected_lines[0]);

    // 2. Edit Middle (Line 50)
    for _ in 0..50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("MIDDLE_EDIT ").unwrap();
    expected_lines[50] = format!("MIDDLE_EDIT {}", expected_lines[50]);

    // 3. Edit End (Line 99)
    for _ in 0..49 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("END_EDIT ").unwrap();
    expected_lines[99] = format!("END_EDIT {}", expected_lines[99]);

    // Save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify
    let saved_content = std::fs::read_to_string(&file_path).unwrap();
    let saved_lines: Vec<&str> = saved_content.lines().collect();

    assert_eq!(
        saved_lines.len(),
        expected_lines.len(),
        "Line count mismatch"
    );

    for (i, (got, want)) in saved_lines.iter().zip(expected_lines.iter()).enumerate() {
        let want_trimmed = want.trim_end_matches('\n');
        assert_eq!(
            *got, want_trimmed,
            "Line {} mismatch:\n  got:      {:?}\n  expected: {:?}",
            i, got, want_trimmed
        );
    }
}

#[test]
fn test_remote_huge_file_mid_and_start_insert() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };
    let fs_arc = Arc::new(fs);

    let file_path = temp_dir.path().join("remote_huge_mid_start.txt");

    // Create 1,000,000 lines
    let mut content = String::new();
    let mut expected_lines = Vec::new();
    for i in 0..1_000_000 {
        let line = format!("Line {:05}: original content\n", i);
        content.push_str(&line);
        expected_lines.push(line);
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(fs_arc),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Each original line is 29 bytes: "Line XXXXX: original content\n"
    let line_len = 29;
    // Track extra bytes inserted before each line to compute byte offsets
    let mut extra_bytes_before_line = 0usize;
    let mut last_edit_line = None;

    let _iterations = 3;
    for target_line in vec![5000, 3] {
        // Compute byte offset for the target line, accounting for prior edits
        let extra = if last_edit_line.map_or(false, |l: usize| l < target_line) {
            extra_bytes_before_line
        } else {
            0
        };
        let target_byte = target_line * line_len + extra;

        // Navigate via Ctrl+G → dismiss scan prompt → byte offset
        harness
            .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
            .unwrap();
        // Dismiss the scan confirmation prompt — opens byte offset prompt
        let _ = harness.type_text("n");
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        let _ = harness.type_text(&format!("{}B", target_byte));
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Go to start of line
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        let edit_text = format!("ITER_{}_", target_line);
        harness.type_text(&edit_text).unwrap();
        expected_lines[target_line] = format!("{}{}", edit_text, expected_lines[target_line]);

        // Track cumulative extra bytes for byte offset calculation
        extra_bytes_before_line += edit_text.len();
        last_edit_line = Some(target_line);

        // Save
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();

        // Verify
        let saved_content = std::fs::read_to_string(&file_path).unwrap();
        let saved_lines: Vec<&str> = saved_content.lines().collect();

        assert_eq!(
            saved_lines.len(),
            expected_lines.len(),
            "Line count mismatch at iter {}",
            target_line
        );

        for (i, (got, want)) in saved_lines.iter().zip(expected_lines.iter()).enumerate() {
            let want_trimmed = want.trim_end_matches('\n');
            assert_eq!(
                *got, want_trimmed,
                "Line {} mismatch at iter {}:\n  got:      {:?}\n  expected: {:?}",
                i, target_line, got, want_trimmed
            );
        }
    }
}
/// Test edits at beginning, middle, and end of a large file using the e2e harness
#[test]
fn test_remote_large_file_edits_beginning_middle_end() {
    use std::fs;
    use tempfile::TempDir;

    let Some((fs, _remote_temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };
    let fs_arc = Arc::new(fs);

    let local_temp_dir = TempDir::new().unwrap();
    let file_path = local_temp_dir.path().join("large_edit_test.txt");

    // Create 100 lines, ~10KB (enough for 500 byte threshold)
    let mut content = String::new();
    let mut expected_lines = Vec::new();
    let lines = 1_000_000;
    let line_len = format!("Line {:04}  original content\n", 1).len();
    for i in 0..lines {
        let line = format!("Line {:04}  original content\n", i);
        content.push_str(&line);
        expected_lines.push(line);
    }
    fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(fresh::config::Config {
                editor: fresh::config::EditorConfig {
                    estimated_line_length: line_len,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(fs_arc),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Edit lines
    let steps = 7;
    for i in 0..steps {
        let target_line = (steps - 1 - i) * (lines / steps);
        let target_byte = target_line * line_len;
        println!("{}", harness.screen_to_string());
        harness
            .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
            .unwrap();
        // Dismiss the scan confirmation prompt — opens byte offset prompt
        let _ = harness.type_text("n");
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        println!("target byte: {}", target_byte);
        let _ = harness.type_text(&format!("{}B", target_byte).to_string());
        println!("{}", harness.screen_to_string());
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        harness.type_text("MIDDLE_EDIT ").unwrap();
        let edited_screen = harness.screen_to_string();
        println!("{}", edited_screen);
        // find exactly which line was modified and update the equivalent line in expected_lines
        for screen_line in edited_screen.lines() {
            if let Some(match_index) = screen_line.find("MIDDLE_EDIT Line ") {
                let line_num_str: Vec<&str> = screen_line
                    [(match_index + "MIDDLE_EDIT Line ".len())..]
                    .split_whitespace()
                    .collect();
                println!("match: {}", line_num_str[0]);
                let line_num = line_num_str[0].parse::<usize>().unwrap();
                expected_lines[line_num] = format!("MIDDLE_EDIT {}", expected_lines[line_num]);
                println!("expected: {}", expected_lines[line_num]);
            }
        }
    }

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("END_EDIT").unwrap();
    expected_lines.push(format!("END_EDIT"));

    // Save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify
    let saved_content = fs::read_to_string(&file_path).unwrap();
    let saved_lines: Vec<&str> = saved_content.lines().collect();

    // Note: lines() strips newlines, so we need to compare carefully
    assert_eq!(
        saved_lines.len(),
        expected_lines.len(),
        "Line count mismatch"
    );

    for (i, (got, want)) in saved_lines.iter().zip(expected_lines.iter()).enumerate() {
        let want_trimmed = want.trim_end_matches('\n');
        assert_eq!(
            *got, want_trimmed,
            "Line {} mismatch:\n  got:      {:?}\n  expected: {:?}",
            i, got, want_trimmed
        );
    }
}
