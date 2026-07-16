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

/// Open the project-wide Search & Replace panel. Uses the Alt+A
/// keybinding (§10) rather than the command palette, because the
/// palette now offers two near-identical entries ("Search and Replace
/// in Project" vs "Search and Replace in Current File") and any prefix
/// filter that matches one matches both — Enter selects whichever the
/// palette highlights first, which is non-deterministic.
fn open_search_replace_via_palette(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
}

/// Complete the inline edit flow: panel opens → type search → Tab to
/// replace field → type replace. Search runs continuously via the
/// debounced typing path; no explicit "submit" needed.
///
/// Important: do NOT press Enter in the search field. The widget
/// runtime treats Enter on a single-line Text widget as "picker-style
/// activate", which fires the adjacent Tree's activate event — the
/// plugin's handler then opens the first match's file and the focus
/// leaves the panel.
fn enter_search_and_replace(harness: &mut EditorTestHarness, search: &str, replace: &str) {
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    harness.type_text(search).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.type_text(replace).unwrap();
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

/// Regression (issue #2664): the panel must keep exactly one focused
/// element. Pressing Down while a toolbar toggle is focused has to move
/// focus *into* the results tree — so the toggle loses its focus ring and
/// the next key acts on the highlighted match — instead of "peeking" the
/// tree selection while the toggle keeps its focus ring (two focused
/// elements at once).
///
/// Observed on the rendered cell background of the `Case` toggle: it
/// gains the focus highlight when tabbed to, and must revert to its
/// unfocused background once Down moves focus into the results. Without
/// the fix the toggle keeps focus, its background never reverts, and this
/// hangs — the intended "fails without the change" reproducer.
#[test]
fn test_search_replace_arrow_from_toolbar_focuses_results() {
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

    // Type the pattern with focus on the search field (no Tab to replace).
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("hello").unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:1") && s.contains("beta.txt")
        })
        .unwrap();

    // Sample the `Case` toggle's background. The focus marker preserves
    // control width, so the "Case" text stays at a fixed cell across
    // focus changes — a stable probe point. It is unfocused now (focus
    // is on the search field).
    let (case_col, case_row) = harness
        .find_text_on_screen("Case")
        .expect("the Case toggle should be visible");
    let case_bg =
        move |h: &EditorTestHarness| h.get_cell_style(case_col, case_row).and_then(|s| s.bg);
    let unfocused_bg = case_bg(&harness);

    // Tab onto the Case toggle: Search → Replace → Files → All Files → Case, then
    // wait for it to actually gain the focus highlight (Tab is handled on
    // the async plugin thread).
    for _ in 0..4 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    }
    harness.wait_until(|h| case_bg(h) != unfocused_bg).unwrap();

    // Down must move focus *into* the results tree, so the Case toggle
    // loses its focus highlight and reverts to its unfocused background.
    // Without the fix, Down only peeks the tree while the toggle keeps
    // focus, so this never reverts and the wait hangs.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.wait_until(|h| case_bg(h) == unfocused_bg).unwrap();
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

/// "No matches found" — the placeholder is a lie if the search
/// hasn't actually completed. See §17 of
/// `docs/internal/search-replace-scope-replan-on-widgets.md`.
#[test]
fn test_search_replace_no_premature_no_matches_label() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Type a pattern but do NOT confirm — no search has been
    // kicked off yet.
    harness.type_text("ZZZNOTFOUND").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("No matches"),
        "Should not show 'No matches' before a search has actually run. \
         Got:\n{}",
        screen
    );
}

/// §1: opening via the "Search and Replace in Current File" command
/// restricts the results to the active buffer. The scope row reads
/// "Only in: alpha.txt", the Replace All button label includes the
/// filename, and a pattern that matches in multiple files shows
/// matches from alpha.txt only.
#[test]
fn test_search_replace_scope_current_file_only() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // Open via palette → "Search and Replace in Current File".
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Search and Replace in Current File")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Search and Replace in Current File")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // The scope row + restricted Replace All label are visible *before*
    // any pattern is typed (they're rendered from PanelState.allFiles +
    // sourceBufferRelPath, both set by openPanel).
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Only in: alpha.txt"),
        "Scope row should read 'Only in: alpha.txt'. Got:\n{}",
        screen
    );
    assert!(
        screen.contains("Replace All in alpha.txt"),
        "Replace-All button should be filename-scoped. Got:\n{}",
        screen
    );
    assert!(
        screen.contains("[ ] All Files"),
        "All Files toggle should be unchecked. Got:\n{}",
        screen
    );

    // Type a pattern that matches in both alpha.txt and beta.txt.
    // Filter must drop the beta.txt match.
    harness.type_text("hello").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("matches"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("alpha.txt"),
        "alpha.txt should appear in restricted results. Got:\n{}",
        screen
    );
    assert!(
        !screen.contains("beta.txt"),
        "beta.txt must NOT appear when scope is restricted to alpha.txt. \
         Got:\n{}",
        screen
    );
}

/// §1 follow-up: closing the panel must restore focus to the source
/// split. Otherwise reopening via "Search and Replace in Current File"
/// sees a stale (utility-dock) active buffer and the scope row
/// degrades to "Only in: (unsaved buffer)" — the user's filename is
/// gone, and post-filter rejects every match.
#[test]
fn test_search_replace_scope_after_close_reopen() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    // First trip: open project-wide, then close with Escape.
    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // Second trip: open via the current-file command and verify scope
    // still names alpha.txt (not "(unsaved buffer)").
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Search and Replace in Current File")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Search and Replace in Current File")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Only in:"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Only in: alpha.txt"),
        "After Escape-close of the prior panel, the current-file path \
         must still read 'Only in: alpha.txt' (not 'unsaved buffer'). \
         Got:\n{}",
        screen
    );
    assert!(
        !screen.contains("(unsaved buffer)"),
        "Source-split focus must have been restored on close. Got:\n{}",
        screen
    );
}

/// §1 regression: "Search and Replace in Current File" must work when the
/// active buffer is an unnamed/unsaved buffer (no path on disk). The host
/// can't reach such a buffer via the project file-walk, so it searches the
/// buffer's in-memory content directly (addressed by buffer id). Before the
/// fix this showed "No matches found" even when the buffer clearly contained
/// the pattern, and Replace All was a no-op.
#[test]
fn test_search_replace_current_file_unnamed_buffer() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    // Disk files also contain "hello" — they must NOT leak into a search
    // scoped to the unnamed buffer.
    create_test_files(&project_root);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 30, Default::default(), project_root)
            .unwrap();

    // Create a fresh unnamed buffer and put three "hello" occurrences in it.
    harness.new_buffer().unwrap();
    harness.type_text("hello world hello there hello").unwrap();
    harness.render().unwrap();

    // Open via palette → "Search and Replace in Current File".
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Search and Replace in Current File")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Search and Replace in Current File")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Search + replace term. The scope is the unnamed buffer.
    enter_search_and_replace(&mut harness, "hello", "goodbye");

    // The three in-buffer matches must be found (this is the regression:
    // it used to read "No matches found").
    harness
        .wait_until(|h| h.screen_to_string().contains("3 matches"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("No matches"),
        "Unnamed-buffer search must find the in-memory matches, not report \
         'No matches'. Got:\n{}",
        screen
    );
    // Disk files that also contain "hello" must stay out of the scoped results.
    assert!(
        !screen.contains("alpha.txt") && !screen.contains("beta.txt"),
        "Disk files must not leak into a search scoped to the unnamed buffer. \
         Got:\n{}",
        screen
    );

    // Replace all three, then close the panel and confirm the buffer's
    // visible content was actually rewritten in memory.
    confirm_replace_all(&mut harness);
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("goodbye world goodbye there goodbye")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("hello world hello there hello"),
        "Original text should be gone after replace. Got:\n{}",
        screen
    );
}

/// Issue #2112: "Search and Replace in Current File" must work when the active
/// buffer is a file located *outside* the project workspace root (e.g. opened
/// from /tmp). The project file-walk is rooted at the working directory, so an
/// out-of-root file is never reached — before the fix this reported "No matches
/// found" even though the file clearly contained the pattern. The host must
/// search the source buffer's file directly when it lies outside the walk root.
#[test]
fn test_search_replace_current_file_outside_workspace() {
    init_tracing_from_env();
    let (temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    // A file that is a sibling of the working dir, i.e. outside the project
    // walk root. The walker rooted at `project_root` can never reach it.
    let outside_file = temp_dir.path().join("outside.txt");
    fs::write(&outside_file, "Line 1: testing the search\nplain line\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&outside_file).unwrap();
    harness.render().unwrap();

    // Open via palette → "Search and Replace in Current File".
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Search and Replace in Current File")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Search and Replace in Current File")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // The scope row must name the out-of-root file. It can't be made relative
    // to the workspace root, so it shows the absolute path.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Only in:") && screen.contains("outside.txt"),
        "Scope row should name the out-of-root file. Got:\n{}",
        screen
    );

    // Search for text that exists in the out-of-root file.
    harness.type_text("testing").unwrap();

    // Wait for the search to settle — either the match-count header or the
    // "No matches found" status appears once the backend reports `done`.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Matches (1 in 1 files)") || s.contains("No matches found")
        })
        .unwrap();

    // The match in the out-of-root file must be found (this is the
    // regression: it used to read "No matches found").
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Matches (1 in 1 files)"),
        "Out-of-root current-file search must find the on-disk match, not \
         report 'No matches'. Got:\n{}",
        screen
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

/// Regression: once a zero-match search settles, the match-list *body*
/// must update from the transient "Searching…" placeholder to "No
/// matches" — not stay stuck on "Searching…". The small count label
/// next to the input fields flipped correctly even with the bug, so a
/// plain `contains("No matches")` check passes either way; the body was
/// the stuck part. Assert no "Searching" text remains after the screen
/// stabilizes.
#[test]
fn test_search_replace_no_matches_clears_searching() {
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
        .wait_until_stable(|h| h.screen_to_string().contains("No matches"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Searching"),
        "match-list body must not stay on 'Searching…' after a zero-match \
         search settles. Got:\n{}",
        screen
    );
}

/// The keyboard-hint row sits directly above the "Matches" separator, so
/// the match list is the last thing in the panel buffer. Asserts on the
/// relative line order of the rendered hint row, the separator, and the
/// empty-state body.
#[test]
fn test_search_replace_help_row_above_matches() {
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    // No search typed yet: separator shows "Matches" and the body shows
    // the "Type a search pattern above" empty state.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("Matches") && s.contains("Type a search pattern")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let help_idx = lines
        .iter()
        .position(|l| l.contains("include/exclude"))
        .unwrap_or_else(|| panic!("help hint row not rendered. Got:\n{}", screen));
    let sep_idx = lines
        .iter()
        .position(|l| l.contains("Matches") && l.contains('─'))
        .unwrap_or_else(|| panic!("Matches separator not rendered. Got:\n{}", screen));
    let body_idx = lines
        .iter()
        .position(|l| l.contains("Type a search pattern"))
        .unwrap_or_else(|| panic!("empty-state body not rendered. Got:\n{}", screen));

    assert!(
        help_idx < sep_idx,
        "help hint row (line {}) must render above the Matches separator \
         (line {}). Got:\n{}",
        help_idx,
        sep_idx,
        screen
    );
    assert!(
        sep_idx < body_idx,
        "Matches separator (line {}) and its list (body line {}) must be \
         the last section in the panel. Got:\n{}",
        sep_idx,
        body_idx,
        screen
    );
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

/// Regression: after a project-wide replace + undo, the buffer now
/// differs from disk (undo only touches in-memory state), so the tab
/// must show the modified indicator (`*`).  Previously the event log's
/// `saved_at_index` was left at its pre-replace value, and undo moved
/// `current_index` back to match, making `update_modified_from_event_log`
/// clear the modified flag even though disk still had the XYZ content.
#[test]
fn test_search_replace_undo_marks_buffer_as_modified() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(project_root.join("dirty.txt"), "hello one\nhello two\n").unwrap();

    let start_file = project_root.join("dirty.txt");
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

    // Close the panel; focus returns to dirty.txt.  Right after the
    // replace the tab should NOT be dirty — buffer matches disk.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("XYZ one"))
        .unwrap();
    assert!(
        !harness.screen_to_string().contains("dirty.txt*"),
        "Right after replace, dirty.txt buffer should match disk (no `*`).\n\
         Screen:\n{}",
        harness.screen_to_string()
    );

    // Undo.
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

    // Buffer reverted.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("hello one") && s.contains("hello two") && !s.contains("XYZ one")
        })
        .unwrap();

    // Disk still has XYZ (undo didn't touch disk).
    let on_disk = fs::read_to_string(project_root.join("dirty.txt")).unwrap();
    assert_eq!(
        on_disk, "XYZ one\nXYZ two\n",
        "Undo must not modify disk — it only reverts the in-memory buffer."
    );

    // Tab must show the modified marker because buffer != disk.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("dirty.txt*") || screen.contains("dirty.txt [+]"),
        "After undo, dirty.txt should be marked modified (buffer != disk).\n\
         Screen:\n{}",
        screen
    );
}

/// Regression: closing the *Search/Replace* panel via the tab × button
/// (mouse click) used to leave a stray split behind showing a duplicate
/// of the original buffer, while the `Close Buffer` command closed the
/// split cleanly.  Both close paths should behave the same.
#[test]
fn test_search_replace_tab_x_button_closes_whole_split() {
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
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Find the panel's buffer id and split id, then invoke the same code
    // path the mouse × handler uses.
    let (panel_buffer, panel_split) = {
        let editor = harness.editor();
        let split_id = editor.effective_active_split();
        let buffer_id = editor.active_buffer();
        (buffer_id, split_id)
    };
    harness
        .editor_mut()
        .close_tab_in_split(panel_buffer, panel_split);
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("*Search/Replace*"),
        "Panel should be gone after × close.\nScreen:\n{}",
        screen
    );
    let alpha_tab_count = screen.matches("alpha.txt ×").count();
    assert_eq!(
        alpha_tab_count, 1,
        "alpha.txt should appear as exactly one tab after × close — no \
         leftover split with a duplicate alpha.txt pane.\nScreen:\n{}",
        screen
    );
}

/// Regression: opening the panel, closing it via Escape, then immediately
/// reopening it used to fail silently — the plugin state held a stale
/// reference to the now-dead virtual buffer and `updatePanelContent` noop'd.
/// After the fix (a `buffer_closed` hook resets plugin state), reopen
/// creates a fresh panel.
#[test]
fn test_search_replace_reopen_after_close_works() {
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

    // Open panel once.
    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Close via Escape (plugin's own close path).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // Reopen — panel must be visible again.
    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    assert!(
        harness.screen_to_string().contains("*Search/Replace*"),
        "Panel should reopen after having been closed."
    );
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

// ============================================================================
// Stage 1 unification: clipboard + selection ops on focused widget Text inputs
// ============================================================================
//
// These tests cover the user-visible payoff of the
// Settings + widget-framework unification: pasting, selecting,
// copying, and cutting against a widget `Text` input goes through
// the same `TextEdit` primitive the Settings UI uses, so the
// keyboard shortcuts every other text input supports now work
// inside the panel too. We observe behavior only through rendered
// output, per the `CONTRIBUTING.md` E2E rules.
//
// All tests focus the **Replace** field (Tab once from the
// default search-focused state). The Search and Replace fields
// are *the same widget kind* — both `WidgetSpec::Text` mounted in
// the same panel with the same focus / keymap / clipboard
// routing — so they exercise identical Stage 1 code paths.
// We use Replace because typing into Search triggers the
// plugin's debounced async search worker (150 ms wall-clock
// `editor.delay`), which raced with subsequent keystrokes on
// slow CI runners and caused timeouts: `wait_until_stable`
// polls every 50 ms and the screen *appears* stable during
// the 150 ms debounce window, so stability passes before the
// search fires. Replace has none of that async tail, so the
// tests are deterministic regardless of runner speed.

/// Helper: open search_replace, Tab to the Replace field, and
/// return a harness where the user's next keystroke lands in
/// that field. Verified by typing a sentinel char and observing
/// it appear in the Replace brackets (not Search).
fn open_panel_with_focus_on_replace(
    project_root: &std::path::Path,
) -> (EditorTestHarness, std::path::PathBuf) {
    let start_file = project_root.join("alpha.txt");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.to_path_buf(),
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Tab from Search → Replace.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    // The screen rendering doesn't show focus via plain ASCII
    // (it's a background-color overlay that `screen_to_string`
    // strips), so verify focus moved by typing a sentinel that
    // lands in the Replace field, then clean it up. The Replace
    // field renders as `Replace: [<value> ...]`.
    harness.type_text("Z").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [Z"))
        .expect("Tab should have moved focus to the Replace field");
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [ "))
        .expect("Backspace should have cleared the sentinel");

    (harness, start_file)
}

/// Ctrl+V into an empty widget Text field inserts the clipboard
/// contents at the cursor — same as typing them.
#[test]
fn test_widget_text_paste_inserts_clipboard() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    harness
        .editor_mut()
        .set_clipboard_for_test("hello".to_string());
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [hello"))
        .unwrap();
}

/// Pasting text that contains a newline into a single-line
/// widget Text field flattens the newline to a space rather than
/// silently concatenating tokens. Validates the behavior the
/// Stage 1 plan calls out explicitly.
#[test]
fn test_widget_text_paste_with_newline_flattened_to_space() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    harness
        .editor_mut()
        .set_clipboard_for_test("foo\nbar".to_string());
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Newline must have become a single space — no
            // "foobar" (concatenation) and no actual line break
            // inside the field.
            s.contains("Replace: [foo bar") && !s.contains("Replace: [foobar")
        })
        .unwrap();
}

/// Ctrl+A selects the whole widget Text value; typing then
/// replaces it. This is the "GUI text input" baseline most users
/// expect.
#[test]
fn test_widget_text_select_all_then_type_replaces_value() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    harness.type_text("abc").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [abc"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    // Typing now replaces the (fully-selected) value.
    harness.type_text("xyz").unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Must show the new value; the old "abc" prefix must
            // not be hanging around.
            s.contains("Replace: [xyz")
                && !s.contains("Replace: [abcxyz")
                && !s.contains("Replace: [abc")
        })
        .unwrap();
}

/// Shift+Right extends the selection char-by-char. Ctrl+C copies
/// the selection; navigating to the end of the value and Ctrl+V
/// duplicates the substring. Exercises three Stage 1 paths in
/// one flow: selection extension, host-side widget copy, and
/// host-side widget paste.
#[test]
fn test_widget_text_shift_right_copy_paste_duplicates_substring() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    harness.type_text("abc").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [abc"))
        .unwrap();

    // Home → cursor at start. Shift+Right twice → selection
    // covers "ab".
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    // Ctrl+C copies "ab".
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    // End → cursor past "abc". Ctrl+V pastes "ab" → "abcab".
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [abcab"))
        .unwrap();
}

/// Result file groups are presented in natural (human) path order once
/// a search finishes, not in the random file-arrival order the parallel
/// project walk produces (#2435). `item2.txt` must sort before
/// `item10.txt` — plain lexicographic order would place `item10.txt`
/// second, so this also guards against an accidental string sort.
#[test]
fn test_search_replace_natural_sort_order() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    // The searched files (each holds one match). Created in an order
    // that is neither natural nor lexicographic so the rendered order
    // can only come from an explicit sort.
    for name in ["item10.txt", "item1.txt", "item2.txt"] {
        fs::write(project_root.join(name), "needle here\n").unwrap();
    }
    // A start buffer with no match, so the searched names appear only in
    // the matches section (not in the tab bar, which would offset the
    // first-occurrence lookups below).
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "nothing to find\n").unwrap();

    // A tall window so the results split has room for all six tree rows
    // (3 file rows + 3 match rows) without scrolling.
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 60, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness.type_text("needle").unwrap();
    harness.render().unwrap();

    // The natural sort is applied once the search finishes and the tree
    // is re-emitted. Wait for the terminal state: all three file rows
    // present, in natural order (item1 < item2 < item10). Without the
    // sort the final order is the random file-arrival order, so this
    // condition never settles and the test fails.
    let row_of =
        |s: &str, needle: &str| -> Option<usize> { s.lines().position(|l| l.contains(needle)) };
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            match (
                row_of(&s, "item1.txt"),
                row_of(&s, "item2.txt"),
                row_of(&s, "item10.txt"),
            ) {
                (Some(r1), Some(r2), Some(r10)) => r1 < r2 && r2 < r10,
                _ => false,
            }
        })
        .unwrap();
}

/// Issue #2619: the Files field accepts comma-separated basename and
/// workspace-relative path globs, and filters before the project search runs.
#[test]
fn test_search_replace_file_glob() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    fs::create_dir_all(project_root.join("src/nested")).unwrap();
    fs::create_dir_all(project_root.join("tests")).unwrap();
    fs::write(project_root.join("root.rs"), "glob needle\n").unwrap();
    fs::write(project_root.join("src/main.rs"), "glob needle\n").unwrap();
    fs::write(project_root.join("src/nested/lib.rs"), "glob needle\n").unwrap();
    fs::write(project_root.join("tests/test.rs"), "glob needle\n").unwrap();
    fs::write(project_root.join("src/skip.txt"), "glob needle\n").unwrap();
    let start_file = project_root.join("start.txt");
    fs::write(&start_file, "nothing here\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 60, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness.type_text("glob needle").unwrap();
    // Gate on the rendered field value before waiting on results: if any
    // keystroke were lost or misrouted, the stuck-wait screen dump would
    // then show a wrong field value at THIS wait — a pinpointed diagnosis
    // — instead of a silent, unexplained hang on the results predicate.
    harness
        .wait_until(|h| h.screen_to_string().contains("Search: [glob needle"))
        .unwrap();

    // First observe the completed unfiltered result set. This makes the
    // later disappearance assertions a real state transition rather than a
    // predicate that can pass vacuously while results are still streaming.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("(5 matches / 5 files)")
                && s.contains("root.rs (1/1)")
                && s.contains("src/main.rs (1/1)")
                && s.contains("src/nested/lib.rs (1/1)")
                && s.contains("tests/test.rs (1/1)")
                && s.contains("src/skip.txt (1/1)")
        })
        .unwrap();

    // Search → Replace. Confirm the asynchronous Tab was processed by
    // observing a sentinel in the rendered Replace field, then remove it.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.type_text("Z").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [Z"))
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [ "))
        .unwrap();

    // Replace → Files. The rendered field value is the semantic gate that
    // proves focus arrived before the glob was typed.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.type_text("src/*.rs, root.rs").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Files: [src/*.rs, root.rs"))
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("(2 matches / 2 files)")
                && s.contains("root.rs (1/1)")
                && s.contains("src/main.rs (1/1)")
                && !s.contains("src/nested/lib.rs")
                && !s.contains("tests/test.rs")
                && !s.contains("src/skip.txt")
        })
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("src/nested/lib.rs"),
        "single-star path glob must not cross directories. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("tests/test.rs") && !screen.contains("src/skip.txt"),
        "non-matching paths must be filtered from search results. Screen:\n{}",
        screen
    );
}

/// Shift+Right twice selects two chars; Backspace deletes the
/// whole selection, not just one char. Regression test for the
/// pre-unification behaviour where the widget framework had no
/// selection so Backspace always removed one char.
#[test]
fn test_widget_text_shift_right_then_backspace_deletes_selection() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    harness.type_text("abc").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [abc"))
        .unwrap();

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    // Backspace on a 2-char selection should remove both, leaving "c".
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Field must read `Replace: [c` (with whatever padding
            // the renderer adds) and the original `[abc` must be
            // gone.
            s.contains("Replace: [c")
                && !s.contains("Replace: [abc")
                && !s.contains("Replace: [ab ")
                && !s.contains("Replace: [ab]")
        })
        .unwrap();
}

/// Issue #2434: a single keystroke walks through the result set from the
/// editor. Ctrl+Alt+Right opens the next match (focus moves to the source
/// split so it can be edited) and reports "Match n/total"; Ctrl+Alt+Left
/// steps back. The first press is sent while the panel is focused (mode
/// binding); later presses land in the editor (normal-context keymap
/// binding), so this exercises both wiring paths.
#[test]
fn test_search_replace_next_match_navigation() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);

    // "hello" appears twice in alpha.txt and once in beta.txt. The total
    // denominator is left unchecked below because the plugin sources
    // copied into the temp project can contribute extra matches; the
    // assertions only depend on the ordinal moving 1 → 2 → 1.
    let start_file = project_root.join("gamma.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("hello").unwrap();

    // Wait until the search has settled with the test files in the
    // result set.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("matches") && s.contains("alpha.txt") && s.contains("beta.txt")
        })
        .unwrap();

    // First match (from the panel via the mode binding).
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Match 1/"))
        .unwrap();

    // Next match (now from the editor via the normal-context keymap binding).
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Match 2/"))
        .unwrap();

    // Previous match steps back.
    harness
        .send_key(KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Match 1/"))
        .unwrap();
}

/// Issue #2434 follow-up: clicking a match row moves the panel selection
/// AND opens that match in the source split (matching the "click a result
/// to jump to it" expectation). The panel opens from gamma.txt, which has
/// no "hello", so a "hello world" line appearing in the top (source) pane
/// proves the click opened alpha.txt there.
#[test]
fn test_search_replace_click_opens_match() {
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
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("hello").unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:1") && s.contains("beta.txt")
        })
        .unwrap();

    // Sanity: the source pane (top rows) shows gamma.txt, not any match.
    let before = harness.screen_to_string();
    let top_before: String = before.lines().take(6).collect::<Vec<_>>().join("\n");
    assert!(
        !top_before.contains("hello world"),
        "source pane should show gamma.txt before the click. Top:\n{}",
        top_before
    );

    // Click the "alpha.txt:1" match row, inside its text (past the checkbox).
    let row = before
        .lines()
        .position(|l| l.contains("alpha.txt:1"))
        .expect("alpha.txt:1 match row must be visible") as u16;
    harness.mouse_click(20, row).unwrap();

    // The source pane now shows alpha.txt line 1 ("hello world").
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.lines().take(6).any(|l| l.contains("hello world"))
        })
        .unwrap();
}

/// Issue #2434: pressing the next-match key must move the *visible*
/// selection in the panel, not just the editor cursor. Regression guard
/// for the host `SetSelectedIndex` mutation ignoring the Tree instance
/// state (it used to write a List state, so the highlight never moved).
/// Asserts on the rendered background, not on model state.
#[test]
fn test_search_replace_nav_highlights_selection() {
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
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("hello").unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:1") && s.contains("beta.txt")
        })
        .unwrap();

    // Screen rows of the match leaf rows (they render "…:<line> - hello…").
    let screen = harness.screen_to_string();
    let match_rows: Vec<u16> = screen
        .lines()
        .enumerate()
        .filter(|(_, l)| l.contains(" - hello"))
        .map(|(i, _)| i as u16)
        .collect();
    assert!(
        match_rows.len() >= 2,
        "expected multiple match rows. Screen:\n{}",
        screen
    );

    let bg_at = |h: &EditorTestHarness, row: u16| h.get_cell_style(20, row).and_then(|s| s.bg);
    let before: Vec<_> = match_rows.iter().map(|&r| bg_at(&harness, r)).collect();

    // Nav to the first match — its row must gain a selection background.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Match 1/"))
        .unwrap();

    let after: Vec<_> = match_rows.iter().map(|&r| bg_at(&harness, r)).collect();
    assert!(
        before != after,
        "a match row's background must change to show the moved selection. \
         rows={:?} before={:?} after={:?}",
        match_rows,
        before,
        after
    );
}

/// Clicking inside a Search & Replace text field moves the caret to
/// the clicked column, exactly like every GUI text input — the next
/// typed character lands where you clicked, not at the end of the
/// value (#2573: "the cursor doesn't attach to the nearest text
/// position in the search and replace panel").
///
/// Driven on the Replace field because typing into Search kicks off
/// the debounced async search worker; the Replace field exercises the
/// identical widget kind + click path with no async tail, so the test
/// is deterministic. The `test_search_replace_click_positions_cursor_in_search_field`
/// case below covers the literally-reported Search field.
#[test]
fn test_widget_text_click_positions_cursor() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();
    create_test_files(&project_root);
    let (mut harness, _) = open_panel_with_focus_on_replace(&project_root);

    // Type a value whose characters are all distinct so the click
    // target is unambiguous. Cursor is now at the end (after "Z").
    harness.type_text("abcXYZ").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Replace: [abcXYZ"))
        .unwrap();

    // Locate the value on screen and click on the "X" cell (index 3),
    // so the caret should land *before* the "X".
    let (vcol, vrow) = harness
        .find_text_on_screen("abcXYZ")
        .expect("the typed value should be visible in the field");
    harness.mouse_click(vcol + 3, vrow).unwrap();

    // Type a sentinel; with the caret repositioned it splits the value.
    harness.type_text("!").unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Fixed: caret moved to the click → "abc!XYZ".
            // Bug: caret stayed at end → "abcXYZ!".
            s.contains("Replace: [abc!XYZ") && !s.contains("Replace: [abcXYZ!")
        })
        .unwrap();
}

/// The literally-reported case (#2573): clicking in the **Search**
/// field repositions the caret there. Same mechanism as the Replace
/// test above; kept separate so the reported field is covered directly.
#[test]
fn test_search_replace_click_positions_cursor_in_search_field() {
    init_tracing_from_env();
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
    // A pattern that matches nothing in the fixtures, so the value
    // appears only in the Search field (not in any results row).
    harness.type_text("abcXYZ").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search: [abcXYZ"))
        .unwrap();

    let (vcol, vrow) = harness
        .find_text_on_screen("abcXYZ")
        .expect("the typed pattern should be visible in the Search field");
    // Click on the "X" cell (index 3) → caret before "X".
    harness.mouse_click(vcol + 3, vrow).unwrap();

    harness.type_text("!").unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Search: [abc!XYZ") && !s.contains("Search: [abcXYZ!")
        })
        .unwrap();
}

/// True when the active buffer's cursor sits exactly on `needle`. Match
/// positions are resolved from live interval markers and opened via an
/// async read (`getBufferText`), so tests wait on the *landing* rather than
/// asserting immediately after the "Match n/m" status appears.
fn cursor_sits_on(h: &EditorTestHarness, needle: &str) -> bool {
    match h.get_buffer_content() {
        Some(content) => content
            .get(h.cursor_position()..)
            .map_or(false, |tail| tail.starts_with(needle)),
        None => false,
    }
}

/// Issue #2583: match positions must track buffer edits.
///
/// The "step through matches and edit each" workflow (#2434) means the user
/// edits the buffer between steps. Inserting a line above a later match used
/// to leave every subsequent `Ctrl+Alt+→` / Enter-open landing on the stale
/// pre-edit line. With interval-marker anchoring the cursor lands on the
/// match's current position.
#[test]
fn test_search_replace_positions_track_edits() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    // A distinctive needle keeps the copied plugin sources out of the
    // result set, so the two matches below are the whole set (1 → 2).
    fs::write(
        project_root.join("alpha.txt"),
        "first line plain\nZZNEEDLE one here\nmiddle text\nanother ZZNEEDLE two\ntail line\n",
    )
    .unwrap();

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("ZZNEEDLE").unwrap();
    // The match *rows* ("alpha.txt:2 …") only render once the search has run,
    // unlike "alpha.txt" (the tab) / "ZZNEEDLE" (the field), which are on
    // screen the instant the panel opens.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:2") && s.contains("alpha.txt:4")
        })
        .unwrap();

    // Step to the first match; wait for the async open to focus the source
    // split and land the cursor on it.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE one here"))
        .unwrap();

    // Insert a brand-new line *above* the second match. "another ZZNEEDLE
    // two" moves from line 4 down to line 5 (keeping its column).
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("inserted row").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step to the second match — it must land on the shifted "ZZNEEDLE two",
    // not the stale line 4 ("middle text").
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE two"))
        .unwrap();
}

/// Issue #2583, column variant: inserting characters *before* a match on its
/// own line shifts the match's column, so stepping back to it lands on the
/// match text rather than the freshly typed prefix.
#[test]
fn test_search_replace_positions_track_column_edits() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(
        project_root.join("alpha.txt"),
        "ZZNEEDLE alpha\nZZNEEDLE beta\n",
    )
    .unwrap();

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("ZZNEEDLE").unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:1") && s.contains("alpha.txt:2")
        })
        .unwrap();

    // Step to the first match (line 1, col 1) and wait for the landing.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE alpha"))
        .unwrap();

    // Type "xx" before the match on its own line: it now starts at col 3.
    harness.type_text("xx").unwrap();
    harness.render().unwrap();

    // Step away to the second match and back to the first. The return step
    // must honour the shifted column and land on "ZZNEEDLE", after the "xx".
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE beta"))
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| {
            let pos = h.cursor_position();
            cursor_sits_on(h, "ZZNEEDLE alpha")
                && pos >= 2
                && h.get_buffer_content()
                    .and_then(|c| c.get(pos - 2..pos).map(|s| s == "xx"))
                    .unwrap_or(false)
        })
        .unwrap();
}

/// Issue #2583, the case the event-hook approach could not cover: **bulk**
/// edits — multi-cursor typing, paste-over, type-over-selection,
/// query-replace, LSP code-action rewrites — apply as a single `BulkEdit`
/// that never fires after_insert / after_delete. The editor still shifts
/// interval markers on that path, so an anchored match tracks it.
///
/// Here two cursors press Enter at once (definitively the bulk path),
/// inserting two lines above the matches. The old byte-math approach was
/// blind to this edit; marker anchoring is not.
#[test]
fn test_search_replace_positions_track_bulk_edits() {
    init_tracing_from_env();
    let (_temp_dir, project_root) = setup_search_replace_project();

    fs::write(
        project_root.join("alpha.txt"),
        "aaaa\nbbbb\nZZNEEDLE one\nZZNEEDLE two\n",
    )
    .unwrap();

    let start_file = project_root.join("alpha.txt");
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    open_search_replace_via_palette(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
    harness.type_text("ZZNEEDLE").unwrap();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("alpha.txt:3") && s.contains("alpha.txt:4")
        })
        .unwrap();

    // Step to the first match and wait for the landing (focus in source).
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE one"))
        .unwrap();

    // Put a cursor at the start of line 1 and a second at the start of line 2,
    // then press Enter at both at once — a multi-cursor bulk edit that adds two
    // lines above the matches. "ZZNEEDLE two" moves from line 4 down to line 6.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.editor_mut().add_cursor_below();
    harness.render().unwrap();
    assert_eq!(
        harness.editor().active_cursors().iter().count(),
        2,
        "test setup: expected two cursors for the bulk edit"
    );
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step to the second match: marker anchoring lands it on the shifted
    // "ZZNEEDLE two". (With the after_insert/after_delete approach the bulk
    // edit is invisible, so this stayed on the stale line 4.)
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();
    harness
        .wait_until(|h| cursor_sits_on(h, "ZZNEEDLE two"))
        .unwrap();
}
