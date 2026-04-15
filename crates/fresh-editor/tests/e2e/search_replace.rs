//! E2E tests for the Search & Replace plugin (multi-file project-wide search/replace)

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
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

/// Trigger Replace All (Alt+Enter), accept the confirmation prompt, and wait
/// for the "Replaced" status. Used by every test that exercises a successful
/// replacement — the confirmation prompt was added to guard against the
/// accidental-replace-you-can't-undo case described in bug #1.
fn confirm_replace_all(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replaced"))
        .unwrap();
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

    // Wait for search results to be populated AND for the panel focus to
    // stabilize before sending Alt+Enter.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Press Alt+Enter to execute Replace All (confirms via prompt).
    confirm_replace_all(&mut harness);

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

    // Wait for search results to be populated AND for the panel focus to
    // stabilize before sending Alt+Enter.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Alt+Enter to execute Replace All (confirms via prompt).
    confirm_replace_all(&mut harness);

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

    let start = std::time::Instant::now();
    let elapsed = || format!("{:.1}s", start.elapsed().as_secs_f64());

    eprintln!(
        "[DEBUG {}] test_search_replace_multiple_matches_same_line: starting",
        elapsed()
    );

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
    eprintln!(
        "[DEBUG {}] screen after open:\n{}",
        elapsed(),
        harness.screen_to_string()
    );

    // --- Open command palette ---
    eprintln!("[DEBUG {}] opening command palette (Ctrl+P)", elapsed());
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    eprintln!("[DEBUG {}] command palette prompt is active", elapsed());

    harness.type_text("Search and Replace").unwrap();
    eprintln!(
        "[DEBUG {}] typed 'Search and Replace' into palette",
        elapsed()
    );

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Search and Replace") || s.contains("Search & Replace")
        })
        .unwrap();
    eprintln!(
        "[DEBUG {}] palette shows Search and Replace option",
        elapsed()
    );
    eprintln!(
        "[DEBUG {}] screen:\n{}",
        elapsed(),
        harness.screen_to_string()
    );

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
    eprintln!(
        "[DEBUG {}] pressed Enter to move to replace field",
        elapsed()
    );

    harness.type_text("ZZ").unwrap();
    harness.render().unwrap();
    eprintln!("[DEBUG {}] typed replace term 'ZZ'", elapsed());

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    eprintln!(
        "[DEBUG {}] pressed Enter to confirm and run search",
        elapsed()
    );
    eprintln!(
        "[DEBUG {}] screen after search submitted:\n{}",
        elapsed(),
        harness.screen_to_string()
    );

    // Wait for search results to be populated AND for the panel focus to
    // stabilize.  After rerunSearch() completes, a .then() callback sets
    // focusPanel="matches" and re-renders.  wait_until_stable ensures that
    // extra render cycle has settled before we send Alt+Enter.
    eprintln!(
        "[DEBUG {}] waiting for search results (matches + [v]) and stability",
        elapsed()
    );
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();
    eprintln!("[DEBUG {}] search results populated and stable", elapsed());
    eprintln!(
        "[DEBUG {}] screen:\n{}",
        elapsed(),
        harness.screen_to_string()
    );

    // Alt+Enter to execute Replace All (confirms via prompt).
    eprintln!("[DEBUG {}] pressing Alt+Enter to Replace All", elapsed());
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    eprintln!(
        "[DEBUG {}] Alt+Enter sent and confirmation accepted",
        elapsed()
    );

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

/// Bug 5 (upstream): the search/replace split must not persist across an
/// editor restart.  The `*Search/Replace*` panel is a transient virtual
/// buffer — the workspace serializer previously remembered the split
/// shape and, since the virtual buffer itself can't be rebuilt from
/// disk, the restored split silently showed "some random file" (usually
/// whichever file was active in the main pane).
///
/// Verify by restarting the harness with a shared `DirectoryContext` and
/// asserting that the only visible file content after restore appears
/// exactly once — i.e. exactly one split, no orphan duplicate.
#[test]
fn test_search_replace_split_not_restored_across_restart() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project_root");
    std::fs::create_dir(&project_dir).unwrap();

    let plugins_dir = project_dir.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    // Use a distinctive content so we can count how many splits show it.
    let file = project_dir.join("persist.txt");
    fs::write(
        &file,
        "UNIQUEMARKERPERSIST alpha\nUNIQUEMARKERPERSIST beta\n",
    )
    .unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open file, open the Search/Replace panel, then shutdown.
    {
        let mut harness = EditorTestHarness::create(
            160,
            40,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.open_file(&file).unwrap();
        harness.render().unwrap();

        open_search_replace_via_palette(&mut harness);
        harness
            .wait_until(|h| h.screen_to_string().contains("Search:"))
            .unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore.  The *Search/Replace* virtual buffer is gone, so
    // if the split were restored it would end up showing the main file as
    // a duplicate.  Assert we have exactly one split for persist.txt.
    {
        let mut harness = EditorTestHarness::create(
            160,
            40,
            HarnessOptions::new()
                .with_config(Config::default())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should have restored");
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        // The search/replace panel must not come back.
        assert!(
            !screen.contains("*Search/Replace*"),
            "*Search/Replace* panel should not be restored after restart.\n\
             Screen:\n{}",
            screen
        );
        // The file content should appear exactly once — not duplicated as
        // an orphan "random file" split left behind by the stale layout.
        let marker_occurrences = screen.matches("UNIQUEMARKERPERSIST alpha").count();
        assert_eq!(
            marker_occurrences, 1,
            "persist.txt content should appear once (single split), not \
             duplicated by a leftover split from the replaced panel.\n\
             Screen:\n{}",
            screen
        );
    }
}

/// Regression: closing the *Search/Replace* panel via the `Close Buffer`
/// command after a project-wide replace used to leave a stray split behind
/// showing one of the auto-opened hidden buffers (b.txt, c.txt) instead of
/// closing the panel's split entirely.  The replace opens each modified
/// file via `open_file_no_focus`, which unconditionally attaches the new
/// buffer as a tab to the preferred split — leaving phantom tabs behind.
/// Close-Buffer would then fall through to a hidden file tab instead of
/// closing the split.
#[test]
fn test_search_replace_close_buffer_after_replace_closes_split() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "XYZ");
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();
    confirm_replace_all(&mut harness);

    // Invoke Close Buffer via the command palette while focus is still on
    // the *Search/Replace* buffer.  This must close the whole panel split —
    // not swap the split to a hidden buffer that was auto-opened by the
    // replace.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Close Buffer").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Close the current buffer"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // After Close Buffer: only the original alpha.txt tab should remain.
    // No *Search/Replace* tab, no duplicate alpha.txt in a leftover split,
    // no beta.txt / gamma.txt tabs from the auto-opened hidden buffers.
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();
    let screen = harness.screen_to_string();
    let alpha_tab_count = screen.matches("alpha.txt ×").count();
    assert_eq!(
        alpha_tab_count, 1,
        "alpha.txt should appear as exactly one tab after closing the panel.\n\
         Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("beta.txt ×"),
        "beta.txt (auto-opened hidden buffer) must not end up as a tab.\n\
         Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("gamma.txt ×"),
        "gamma.txt must not end up as a tab.\nScreen:\n{}",
        screen
    );
}

/// Bug 3 (upstream): opening the search/replace panel used to create the
/// virtual buffer in the *current* split's tab bar AND in a new split,
/// leaving `*Search/Replace*` visible twice on screen.  Assert it appears
/// exactly once.
#[test]
fn test_search_replace_panel_not_duplicated_in_tabs() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 40, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    let screen = harness.screen_to_string();
    // Count only tab-bar occurrences (label followed by the close × or
    // end-of-tab spacing).  Tabs render as `*Search/Replace* ×`; the
    // bottom status bar shows `*Search/Replace* [RO]` which we ignore.
    let tab_occurrences = screen.matches("*Search/Replace* ×").count();
    assert_eq!(
        tab_occurrences, 1,
        "The *Search/Replace* buffer should have exactly one tab on screen \
         (in its own split), not duplicated as a tab in the source split.\n\
         Screen:\n{}",
        screen
    );
}

/// Bug 1 (upstream) companion: pressing Alt+Enter opens a confirmation
/// prompt explaining that the replace is not restore-safe.  Cancelling the
/// prompt must leave the file unchanged.
#[test]
fn test_search_replace_confirmation_prompt_cancel_leaves_files_untouched() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    let original = "hello world\nhello again\n";
    fs::write(project_root.join("cancel.txt"), original).unwrap();

    let start_file = project_root.join("cancel.txt");
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
    enter_search_and_replace(&mut harness, "hello", "XYZ");

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Trigger Replace All — expect the confirmation prompt to open.
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();
    harness.wait_for_prompt().unwrap();

    // Prompt text should warn about the undo caveat.
    let prompt_screen = harness.screen_to_string();
    assert!(
        prompt_screen.contains("Undo only covers"),
        "Confirmation prompt should warn about undo scope.  Screen:\n{}",
        prompt_screen
    );

    // Cancel the prompt with Escape.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replacement cancelled"))
        .unwrap();

    // File must be unchanged on disk.
    let after = fs::read_to_string(project_root.join("cancel.txt")).unwrap();
    assert_eq!(
        after, original,
        "File must be unchanged after cancelling the confirmation prompt."
    );
}

/// Bug 1 (upstream) — full manual reproduction: after the replace saves
/// the file, the auto-revert poller sees a fresh mtime and, if it runs
/// before the user presses Undo, calls `revert_buffer_by_id` which
/// resets the event log.  Undo then finds nothing to revert.
///
/// We trigger this path deterministically by invoking `handle_file_changed`
/// explicitly — the production equivalent of the polling tick firing
/// after the save.  Without the mtime refresh in `handle_replace_in_buffer`
/// this wipes the BulkEdit and the assertion below fails.
#[test]
fn test_search_replace_undo_survives_auto_revert_poll() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    let file = project_root.join("auto_revert.txt");
    fs::write(&file, "hello one\nhello two\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "XYZ");
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();
    confirm_replace_all(&mut harness);

    // Simulate the auto-revert poller tick firing after our save —
    // equivalent to the `file_mod_times` mismatch that production sees
    // when `save_to_file` is followed by a polling cycle.
    harness
        .editor_mut()
        .handle_file_changed(file.to_str().unwrap());
    harness.render().unwrap();

    // Close the panel.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("XYZ one"))
        .unwrap();

    // Undo via the command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Undo").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Undo the last edit"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("hello one") && s.contains("hello two") && !s.contains("XYZ one")
        })
        .unwrap();
}

/// Bug 1 (upstream) variant: with multiple files already open before the
/// replace, the `Undo` command must still revert the currently-focused
/// buffer after a project-wide replace.
#[test]
fn test_search_replace_is_undoable_with_multiple_open_buffers() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(project_root.join("multi_a.txt"), "hello A1\nhello A2\n").unwrap();
    fs::write(
        project_root.join("multi_b.txt"),
        "hello B1\nhello B2\nhello B3\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open both files; multi_b.txt is active at the end of this setup.
    harness
        .open_file(&project_root.join("multi_a.txt"))
        .unwrap();
    harness
        .open_file(&project_root.join("multi_b.txt"))
        .unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    enter_search_and_replace(&mut harness, "hello", "XYZ");
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();
    confirm_replace_all(&mut harness);

    // Close the panel; focus returns to multi_b.txt (the previously active
    // file buffer in the source split).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // Sanity: multi_b.txt buffer shows the replaced text.
    harness
        .wait_until(|h| h.screen_to_string().contains("XYZ B1"))
        .unwrap();

    // Invoke Undo via the command palette.  Active buffer at this point is
    // multi_b.txt — its event log must carry the BulkEdit so Undo reverts.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Undo").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Undo the last edit"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // multi_b.txt buffer reverts; multi_a.txt was touched by the replace
    // but user hasn't focused it yet — its event log should still have the
    // BulkEdit so focusing it and undoing would revert it too.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("hello B1")
                && s.contains("hello B2")
                && s.contains("hello B3")
                && !s.contains("XYZ B1")
        })
        .unwrap();
}

/// Bug 1 (upstream): after a project-wide replace, the `Undo` command must
/// actually revert the in-memory buffer for the currently-focused file.
/// Previously replaceInFile bypassed the event log, so Ctrl+Z / the Undo
/// command was a no-op and users couldn't recover from a mistaken replace.
#[test]
fn test_search_replace_is_undoable_via_command_palette() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(
        project_root.join("undo.txt"),
        "hello world\nhello there\nfinal hello line\n",
    )
    .unwrap();

    let start_file = project_root.join("undo.txt");
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
    enter_search_and_replace(&mut harness, "hello", "XYZ");

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    confirm_replace_all(&mut harness);

    // Close the panel so focus returns to undo.txt.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // Sanity: the focused buffer shows the replaced text.
    harness
        .wait_until(|h| h.screen_to_string().contains("XYZ world"))
        .unwrap();

    // Invoke `Undo` via the command palette (avoids any terminal Ctrl+Z/SUSP
    // ambiguity and tests the command, not the keybinding).
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Undo").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Undo the last edit"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Buffer must revert to the pre-replace content.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("hello world")
                && s.contains("hello there")
                && s.contains("final hello line")
                && !s.contains("XYZ world")
        })
        .unwrap();
}

/// Bug 2 (upstream): after a successful replace, the match tree must be
/// refreshed.  It previously kept displaying the pre-replacement matches
/// (e.g. still showed "hello" rows after replacing hello→XYZ), hiding what
/// actually changed and feeding the repeat-replace corruption (bug 4).
#[test]
fn test_search_replace_refreshes_match_list_after_replace() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(
        project_root.join("refresh.txt"),
        "hello world\nhello again\nhello there\n",
    )
    .unwrap();

    let start_file = project_root.join("refresh.txt");
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
    enter_search_and_replace(&mut harness, "hello", "XYZ");

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // Pre-condition: the match tree shows the pre-replacement context.
    let before = harness.screen_to_string();
    assert!(
        before.contains("hello world"),
        "Expected pre-replacement match context on screen. Got:\n{}",
        before
    );

    // Execute replacement.
    confirm_replace_all(&mut harness);

    // After the replacement settles, the match tree must no longer display
    // stale "hello" rows — the file has no "hello" left, so the list should
    // either be empty ("No matches") or show fresh post-replace state.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            // Tree has refreshed: stale match-row context is gone.
            !s.contains("hello world") && !s.contains("hello again") && !s.contains("hello there")
        })
        .unwrap();

    let after = harness.screen_to_string();
    assert!(
        !after.contains("hello world"),
        "Match list must not display stale 'hello world' row after replacement.\n{}",
        after
    );
    assert!(
        !after.contains("hello again"),
        "Match list must not display stale 'hello again' row after replacement.\n{}",
        after
    );
}

/// Bug 4 (upstream): pressing Alt+Enter a second time should not re-apply
/// the replacement using stale byte offsets from the pre-replacement search,
/// which would corrupt the file (e.g. "XYZ world" → "hhXYZrld").
///
/// After a successful replace, the panel must refresh its match list before
/// honoring another Alt+Enter.  A second Alt+Enter must leave the file
/// byte-for-byte identical to the state after the first Alt+Enter.
#[test]
fn test_search_replace_second_alt_enter_does_not_corrupt_files() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    // Use a replacement shorter than the pattern to make byte-offset drift
    // observable: "hello" → "XYZ" shrinks the file by 2 bytes per match, so
    // replaying the original offsets would clobber innocent bytes.
    fs::write(
        project_root.join("corrupt.txt"),
        "hello world\nhello there\nthis is a hello test\nfinal hello line\n",
    )
    .unwrap();

    let start_file = project_root.join("corrupt.txt");
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
    enter_search_and_replace(&mut harness, "hello", "XYZ");

    // Wait for search results and focus to stabilize.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("[v]")
        })
        .unwrap();

    // First Alt+Enter — replace all (confirms via prompt).
    confirm_replace_all(&mut harness);

    let after_first = fs::read_to_string(project_root.join("corrupt.txt")).unwrap();
    assert_eq!(
        after_first, "XYZ world\nXYZ there\nthis is a XYZ test\nfinal XYZ line\n",
        "First Alt+Enter should produce clean replacements. Got:\n{}",
        after_first
    );

    // Let the panel settle (rerunSearchQuiet should have cleared the list
    // since "hello" no longer exists in the file).
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("Replacing"))
        .unwrap();

    // Second Alt+Enter — with a correctly-refreshed match list there are no
    // remaining matches, so this must be a no-op on disk.
    harness.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();

    // Give any async replace work a chance to finish.  We can't key off
    // "Replaced" here because a correct implementation will NOT produce that
    // status a second time — so wait for the screen to stabilize instead.
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    let after_second = fs::read_to_string(project_root.join("corrupt.txt")).unwrap();
    assert_eq!(
        after_second, after_first,
        "Second Alt+Enter must not modify the file further (no stale offsets). \
         After first: {:?}\nAfter second: {:?}",
        after_first, after_second
    );
}
