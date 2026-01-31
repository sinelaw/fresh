use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
use std::sync::Arc;

fn create_test_filesystem() -> Option<(RemoteFileSystem, tempfile::TempDir, tokio::runtime::Runtime)>
{
    let temp_dir = tempfile::tempdir().ok()?;
    let rt = tokio::runtime::Runtime::new().ok()?;

    let channel = rt.block_on(spawn_local_agent()).ok()?;
    let fs = RemoteFileSystem::new(channel, "test@localhost".to_string());

    Some((fs, temp_dir, rt))
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

    // Create 10,000 lines
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

    let _iterations = 3;
    for target_line in vec![5000, 3] {
        // 1. Edit Middle (Line 5000)
        // Reset to start
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .unwrap();
        // Navigate
        harness
            .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, target_line)
            .unwrap();

        // Edit line target_line
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        let edit_text = format!("ITER_{}_", target_line);
        harness.type_text(&edit_text).unwrap();
        expected_lines[target_line] = format!("{}{}", edit_text, expected_lines[target_line]);

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
        let target = (steps - 1 - i) * (lines / steps);
        println!("{}", harness.screen_to_string());
        harness
            .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
            .unwrap();
        println!("target line: {}", target);
        let _ = harness.type_text(&format!("{}", target).to_string());
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
