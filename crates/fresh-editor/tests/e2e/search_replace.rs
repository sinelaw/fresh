//! E2E tests for the Search & Replace plugin (multi-file project-wide search/replace)

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Set up a project directory with the search_replace plugin.
fn setup_search_replace_project() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    (temp_dir, project_root)
}

/// Create test files in the project directory.
fn create_test_files(project_root: &std::path::Path) {
    fs::write(
        project_root.join("alpha.txt"),
        "hello world\nfoo bar\nhello again\n",
    )
    .unwrap();
    fs::write(
        project_root.join("beta.txt"),
        "hello from beta\nno match here\n",
    )
    .unwrap();
    fs::write(
        project_root.join("gamma.txt"),
        "nothing relevant\njust filler\n",
    )
    .unwrap();
}

/// Open command palette, find "Search and Replace in Project", execute it.
fn open_search_replace_via_palette(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Search and Replace").unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Search and Replace") || s.contains("Search & Replace")
        })
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Complete the inline edit flow: panel opens → type search → Enter → type replace → Enter → search runs.
/// In the new UX, characters are typed directly into the panel fields (no prompts).
fn enter_search_and_replace(harness: &mut EditorTestHarness, search: &str, replace: &str) {
    // Panel opens with focus on search field — wait for it to render
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Type the search term directly (characters go into the inline field)
    harness.type_text(search).unwrap();
    harness.render().unwrap();

    // Press Enter to move to replace field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type the replacement
    harness.type_text(replace).unwrap();
    harness.render().unwrap();

    // Press Enter to confirm and run search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Plugin loads and the command appears in the palette.
#[test]
fn test_search_replace_plugin_loads() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Search and Replace").unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Search and Replace"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Search flow shows a results panel with correct matches.
#[test]
fn test_search_replace_shows_results_panel() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("gamma.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "goodbye");

    // Wait for results panel to render with both file groups (streaming results arrive per-file)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("[v]") && s.contains("alpha.txt") && s.contains("beta.txt")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    // gamma.txt has no "hello" — should not appear in the matches section.
    // Note: gamma.txt may appear in the tab bar since it's the opened file.
    assert!(
        !screen.contains("gamma.txt ("),
        "gamma.txt should not appear in match results. Screen:\n{}",
        screen
    );
}

/// Space toggles item selection; deselected items are shown with [ ].
#[test]
fn test_search_replace_toggle_selection() {
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(
        project_root.join("only.txt"),
        "apple orange\napple banana\n",
    )
    .unwrap();

    let start_file = project_root.join("only.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "apple", "pear");

    // Wait for results panel with checkboxes AND for focus to stabilize on
    // the matches panel.  After rerunSearch() completes, a .then() callback
    // sets focusPanel="matches" and re-renders.  wait_until_stable ensures
    // that extra render cycle has settled before we send navigation keys.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("[v]") && s.contains("only.txt")
        })
        .unwrap();

    // Focus is now on matches panel at index 0 (first file node).
    // Navigate down to the first match row (child of the file node).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Toggle the match with Space
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();

    // Wait for the deselected checkbox to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("[ ]"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[ ]") && screen.contains("[v]"),
        "Should have one deselected and one selected item. Screen:\n{}",
        screen
    );
}

/// Escape closes the panel without performing any replacements.
#[test]
fn test_search_replace_escape_closes_panel() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "NOPE");

    harness
        .wait_until(|h| h.screen_to_string().contains("Search/Replace"))
        .unwrap();

    // Close with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Wait for the panel split to disappear (tab bar no longer shows *Search/Replace*)
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // File should be unchanged
    let alpha = fs::read_to_string(project_root.join("alpha.txt")).unwrap();
    assert!(
        alpha.contains("hello"),
        "alpha.txt should be unchanged after Escape. Got:\n{}",
        alpha
    );
}

/// Searching for a pattern with no matches shows the "No matches" message.
#[test]
fn test_search_replace_no_matches() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "ZZZZNOTFOUND", "whatever");

    harness
        .wait_until(|h| h.screen_to_string().contains("No matches"))
        .unwrap();
}

/// Cancelling at the search field (before typing) closes the empty panel.
#[test]
fn test_search_replace_cancel_at_search_field() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);

    // Panel opens with search field focused
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Cancel — should close the empty panel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();
}

/// Escape when panel has content keeps panel open (need explicit close).
/// Actually Escape always closes the panel in the current design.
#[test]
fn test_search_replace_escape_always_closes() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);

    // Type search term
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Escape should close the panel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();
}

/// Execute replacement — files should be modified on disk via Alt+Enter.
#[test]
fn test_search_replace_executes_replacement() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("gamma.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "goodbye");

    // Wait for search results to be populated (match stats appear in the panel)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Press Alt+Enter to execute Replace All
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();

    // Wait for the status message confirming replacement
    harness
        .wait_until(|h| h.screen_to_string().contains("Replaced"))
        .unwrap();

    // Verify files were modified on disk
    let alpha = fs::read_to_string(project_root.join("alpha.txt")).unwrap();
    assert!(
        alpha.contains("goodbye") && !alpha.contains("hello"),
        "alpha.txt should have 'hello' replaced with 'goodbye'. Got:\n{}",
        alpha
    );

    let beta = fs::read_to_string(project_root.join("beta.txt")).unwrap();
    assert!(
        beta.contains("goodbye") && !beta.contains("hello"),
        "beta.txt should have 'hello' replaced. Got:\n{}",
        beta
    );

    let gamma = fs::read_to_string(project_root.join("gamma.txt")).unwrap();
    assert_eq!(gamma, "nothing relevant\njust filler\n");
}

/// Replacing with an empty string deletes the matched text.
#[test]
fn test_search_replace_delete_pattern() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(project_root.join("target.txt"), "remove_me stays\n").unwrap();

    let start_file = project_root.join("target.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);

    // Panel opens with search field
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("remove_me").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Empty replacement — just press Enter to confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for search results to be populated (match stats appear in the panel)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Alt+Enter to execute Replace All
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Replaced"))
        .unwrap();

    let content = fs::read_to_string(project_root.join("target.txt")).unwrap();
    assert_eq!(
        content, " stays\n",
        "remove_me should be deleted. Got: {:?}",
        content
    );
}

/// Multiple matches on the same line — all occurrences on the line get replaced.
#[test]
fn test_search_replace_multiple_matches_same_line() {
    init_tracing_from_env();

    // Install a SIGUSR1 handler that prints a backtrace for debugging hangs.
    // In CI you can send `kill -USR1 <pid>` to get a snapshot of where the test is stuck.
    #[cfg(unix)]
    {
        unsafe {
            libc::signal(libc::SIGUSR1, _sigusr1_handler as libc::sighandler_t);
        }
    }

    let start = std::time::Instant::now();
    let elapsed = || format!("{:.1}s", start.elapsed().as_secs_f64());

    eprintln!("[DEBUG {}] test_search_replace_multiple_matches_same_line: starting", elapsed());

    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(project_root.join("multi.txt"), "aa bb aa cc aa\nno match\n").unwrap();
    eprintln!("[DEBUG {}] project set up at {:?}", elapsed(), project_root);

    let start_file = project_root.join("multi.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] file opened and initial render done", elapsed());
    eprintln!("[DEBUG {}] screen after open:\n{}", elapsed(), harness.screen_to_string());

    // --- Open command palette ---
    eprintln!("[DEBUG {}] opening command palette (Ctrl+P)", elapsed());
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    eprintln!("[DEBUG {}] command palette prompt is active", elapsed());

    harness.type_text("Search and Replace").unwrap();
    eprintln!("[DEBUG {}] typed 'Search and Replace' into palette", elapsed());

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Search and Replace") || s.contains("Search & Replace")
        })
        .unwrap();
    eprintln!("[DEBUG {}] palette shows Search and Replace option", elapsed());
    eprintln!("[DEBUG {}] screen:\n{}", elapsed(), harness.screen_to_string());

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] pressed Enter on palette item", elapsed());

    // --- Enter search and replace terms ---
    eprintln!("[DEBUG {}] waiting for Search: field", elapsed());
    {
        let mut wait_iters = 0u64;
        harness
            .wait_until(|h| {
                wait_iters += 1;
                if wait_iters % 20 == 0 {
                    eprintln!(
                        "[DEBUG wait_until Search:] iteration {}, screen:\n{}",
                        wait_iters,
                        h.screen_to_string()
                    );
                }
                h.screen_to_string().contains("Search:")
            })
            .unwrap();
    }
    eprintln!("[DEBUG {}] Search: field visible", elapsed());

    harness.type_text("aa").unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] typed search term 'aa'", elapsed());

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] pressed Enter to move to replace field", elapsed());

    harness.type_text("ZZ").unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] typed replace term 'ZZ'", elapsed());

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] pressed Enter to confirm and run search", elapsed());
    eprintln!("[DEBUG {}] screen after search submitted:\n{}", elapsed(), harness.screen_to_string());

    // Wait for search results to be populated (match stats appear in the panel)
    eprintln!("[DEBUG {}] waiting for search results (matches + [v])", elapsed());
    {
        let mut wait_iters = 0u64;
        harness
            .wait_until(|h| {
                wait_iters += 1;
                if wait_iters % 20 == 0 {
                    eprintln!(
                        "[DEBUG wait_until matches] iteration {}, screen:\n{}",
                        wait_iters,
                        h.screen_to_string()
                    );
                }
                let s = h.screen_to_string();
                s.contains("matches") && s.contains("[v]")
            })
            .unwrap();
    }
    eprintln!("[DEBUG {}] search results populated", elapsed());
    eprintln!("[DEBUG {}] screen:\n{}", elapsed(), harness.screen_to_string());

    // Alt+Enter to execute Replace All
    eprintln!("[DEBUG {}] pressing Alt+Enter to Replace All", elapsed());
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();
    eprintln!("[DEBUG {}] Alt+Enter sent", elapsed());

    eprintln!("[DEBUG {}] waiting for 'Replaced' confirmation", elapsed());
    {
        let mut wait_iters = 0u64;
        harness
            .wait_until(|h| {
                wait_iters += 1;
                if wait_iters % 20 == 0 {
                    eprintln!(
                        "[DEBUG wait_until Replaced] iteration {}, screen:\n{}",
                        wait_iters,
                        h.screen_to_string()
                    );
                }
                h.screen_to_string().contains("Replaced")
            })
            .unwrap();
    }
    eprintln!("[DEBUG {}] replacement confirmed", elapsed());

    let content = fs::read_to_string(project_root.join("multi.txt")).unwrap();
    eprintln!("[DEBUG {}] multi.txt content: {:?}", elapsed(), content);
    assert!(
        content.contains("ZZ bb ZZ cc ZZ"),
        "All occurrences on the line should be replaced. Got:\n{}",
        content
    );
    assert!(
        !content.contains("aa"),
        "No 'aa' should remain. Got:\n{}",
        content
    );
    eprintln!("[DEBUG {}] test PASSED", elapsed());
}

/// SIGUSR1 handler — prints a message to stderr so CI logs show that the signal was received.
/// This is intentionally minimal (async-signal-safe) — just writes a fixed string.
#[cfg(unix)]
extern "C" fn _sigusr1_handler(_sig: libc::c_int) {
    // write(2, ...) is async-signal-safe
    let msg = b"\n[SIGUSR1] Signal received - test is likely stuck in a wait_until loop. Check preceding DEBUG output for last step.\n";
    unsafe {
        libc::write(2, msg.as_ptr() as *const libc::c_void, msg.len());
    }
}
