//! E2E tests reproducing bugs from the combined UX report
//! (REVIEW_DIFF_COMBINED_UX_REPORT.md).
//!
//! Each test is named after the bug it reproduces and is expected to
//! **fail** (or demonstrate the broken behavior) until the underlying
//! bug is fixed.  Once fixed, the test becomes the regression guard.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Copy audit_mode plugin and its library into the test repo.
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Wait until a review panel has *settled* — the toolbar is up **and** the
/// diff stream behind it has been filled.
///
/// The toolbar alone is not that gate. Both bootstraps (`start_review_diff`
/// and `bootstrapRangeReview`) set the "Generating Review Diff Stream..."
/// status before their first `git` call and leave it up until
/// `openReviewPanels` finishes; the toolbar is painted by the
/// `createBufferGroup` near the *start* of `openReviewPanels`, while the
/// stream's body is written by the `updateMagitDisplay` that follows and the
/// status is only replaced by the `updateReviewStatus` after that. So a wait
/// for "next hunk" on its own resolves on a frame whose diff area is still
/// blank — observed as `test_range_review_lays_out_every_file` reading an
/// empty stream with "Generating Review Diff Stream..." still on the status
/// line.
///
/// Requiring both conditions cannot resolve early either: the status is set
/// before the panels exist, so "toolbar present" and "not generating" are
/// first true together only once the panels have been filled.
fn wait_for_review_ready(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", screen);
            }
            screen.contains("next hunk") && !screen.contains("Generating Review")
        })
        .unwrap();
}

/// Open Review Diff via command palette and wait for it to load.
/// Returns the initial screen string.
fn open_review_diff(harness: &mut EditorTestHarness) -> String {
    // `run_palette_command` waits for the row to be listed before confirming:
    // this command comes from a plugin, so typing the name and pressing Enter
    // could fire before it was registered (Quick Open re-filters on input
    // change only, so a late registration is never picked up on its own).
    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();

    wait_for_review_ready(harness);

    harness.screen_to_string()
}

/// Create a standard repo with one committed file and one unstaged modification.
/// Returns (repo, path_to_modified_file).
fn repo_with_one_modification() -> (GitTestRepo, std::path::PathBuf) {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs = repo.path.join("src/main.rs");
    fs::write(&main_rs, "fn main() {\n    println!(\"CHANGED\");\n}\n").unwrap();
    (repo, main_rs)
}

/// Create a repo with multiple modified files (for multi-hunk / navigation tests).
fn repo_with_multi_hunk_file() -> (GitTestRepo, std::path::PathBuf) {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify main.rs to have multiple hunks (changes in two separate regions)
    let main_rs = repo.path.join("src/main.rs");
    let content = r#"fn main() {
    println!("Hello, HUNK_ONE!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("HUNK_TWO server started");
    println!("Extra line added");
}
"#;
    fs::write(&main_rs, content).unwrap();
    (repo, main_rs)
}

/// Switch on the git settings that rewrite `git diff`'s output. Each is
/// something a real user's config may carry, and each on its own is enough to
/// stop Review Diff's parser producing usable hunks, or to misnumber them: an
/// external driver prints its own format, a textconv filter diffs converted
/// text, forced colour wraps every line in escapes, the prefix settings strip
/// the `a/`/`b/` paths the parser matches on, and `suppressBlankEmpty` takes
/// the marker off an empty context line.
///
/// `core.pager` is deliberately not configured: git only starts a pager when
/// stdout is a tty, and `editor.spawnProcess` reads through a pipe, so a pager
/// shim could never run and would protect nothing.
#[cfg(unix)]
fn configure_hostile_diff_output(repo: &GitTestRepo) {
    use std::os::unix::fs::PermissionsExt;

    // Everything the fixture installs lives under `.git/`, which git never
    // reports as untracked. In the working tree these files would show up as
    // untracked hunks inside the very panel the assertions read, competing for
    // the rows the marker has to be visible on.
    let shim = |name: &str, body: &str| -> String {
        let path = repo.create_file(name, body);
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755))
            .expect("make diff shim executable");
        path.to_string_lossy().into_owned()
    };

    // A `difft`-style driver: side-by-side text where a patch is expected.
    let external = shim(
        ".git/difft",
        "#!/bin/sh\nprintf '%s\\n' 'src/main.rs --- Rust' '1 fn main() | 1 fn main()'\n",
    );

    // The textconv filter *keeps* the difference and prepends two header
    // lines, so an unsuppressed filter yields a perfectly parseable patch
    // whose hunk line numbers are all off by two. A filter that erased the
    // change instead would make git emit an empty diff, and the test would
    // then fail for "no hunks" without ever exercising the line-number
    // corruption `--no-textconv` exists to prevent -- the corruption that
    // feeds `buildHunkPatch` and then `git apply`.
    let textconv = shim(
        ".git/textconv.sh",
        "#!/bin/sh\nprintf 'HDR1\\nHDR2\\n'; cat \"$1\"\n",
    );

    // `.git/info/attributes` binds the driver exactly as a tracked
    // `.gitattributes` would, without adding a file to the working tree.
    repo.create_file(".git/info/attributes", "*.rs diff=fresh-test\n");

    set_git_config(
        repo,
        &[
            ("diff.external", external.as_str()),
            ("diff.fresh-test.textconv", textconv.as_str()),
            ("color.diff", "always"),
            ("diff.noprefix", "true"),
            ("diff.mnemonicPrefix", "true"),
            // Drops the leading space from an empty context line. The row's
            // first byte is what the host's diff gutter counts it by, so an
            // unsuppressed empty line belongs to neither side and every
            // number below it in the hunk comes out one short.
            ("diff.suppressBlankEmpty", "true"),
        ],
    );
}

/// Write each `(key, value)` into the repo's local git config.
#[cfg(unix)]
fn set_git_config(repo: &GitTestRepo, pairs: &[(&str, &str)]) {
    use crate::common::git_test_helper::git_command;

    for (key, value) in pairs {
        let output = git_command(&repo.path)
            .args(["config", "--local", key, value])
            .output()
            .expect("run git config");
        assert!(
            output.status.success(),
            "git config {key} failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

/// The new-file line number the review panel prints in its gutter for the row
/// carrying `marker`.
///
/// A row reads `<old> <new> │ +<content>`, drawn to the right of the
/// file-list divider, so this scans back from the diff prefix rather than
/// assuming a column: the rightmost `+` before the marker is the diff
/// prefix, and the last number in front of it is the new-file number.
/// Added lines leave the old-number field blank, which is why the last
/// number is unambiguous.
#[cfg(unix)]
fn marker_new_line_number(screen: &str, marker: &str) -> Option<u32> {
    let row = screen.lines().find(|l| l.contains(marker))?;
    let (before_marker, _) = row.split_once(marker)?;
    let (gutter, _) = before_marker.rsplit_once('+')?;
    // The gutter is the host's: `OLD NEW │`, with the old column blank on
    // an added row. Take its last number, past the separator.
    gutter
        .split_whitespace()
        .filter_map(|tok| tok.parse::<u32>().ok())
        .next_back()
}

/// Stash the working tree so `Review Diff: Stash` has something to show. Reverts the
/// tree as a side effect, which is what makes the stash the only diff source.
#[cfg(unix)]
fn git_stash(repo: &GitTestRepo) {
    use crate::common::git_test_helper::git_command;

    let output = git_command(&repo.path)
        .args(["stash", "push", "-m", "review-stash-fixture"])
        .output()
        .expect("run git stash");
    assert!(
        output.status.success(),
        "git stash failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

// ---------------------------------------------------------------------------
// BUG-1: CompositeInputRouter dead code — side-by-side vim keys broken
// ---------------------------------------------------------------------------

/// BUG-1: In the side-by-side diff view, pressing `j` should scroll down
/// but instead produces "Editing disabled in this buffer" because the
/// CompositeInputRouter is never wired into the key dispatch pipeline.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug1_side_by_side_vim_keys_produce_editing_disabled() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Wait for the side-by-side view or the loading message to finish
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press `j` — this should scroll down, not show "Editing disabled"
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 screen after j:\n{}", screen);

    assert!(
        !screen.contains("Editing disabled"),
        "BUG-1: `j` in side-by-side diff should scroll, not show \
         'Editing disabled'. CompositeInputRouter is not wired into \
         key dispatch. Screen:\n{}",
        screen
    );
}

/// BUG-1 (cont): `Escape` should close the side-by-side view, but it
/// does nothing because the CompositeInputRouter is dead code.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug1_side_by_side_escape_does_not_close() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press Escape — should close the side-by-side view
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 escape screen:\n{}", screen);

    // The side-by-side view should be closed — no more "OLD (HEAD)" or "*Diff:" tab
    assert!(
        !screen.contains("OLD (HEAD)"),
        "BUG-1: Escape in side-by-side diff should close the composite view. \
         CompositeInputRouter is dead code. Screen:\n{}",
        screen
    );
}

/// BUG-1 (cont): `Tab` should switch panes in side-by-side view.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug1_side_by_side_tab_does_not_switch_pane() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Press Tab — should switch pane, not show "Editing disabled"
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-1 tab screen:\n{}", screen);

    assert!(
        !screen.contains("Editing disabled"),
        "BUG-1: Tab in side-by-side diff should switch pane, not show \
         'Editing disabled'. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-2: Terminal resize destroys Review Diff layout
// ---------------------------------------------------------------------------

/// BUG-2: Resizing the terminal while in Review Diff mode causes the
/// toolbar, header, separator, and content to disappear.  The layout does
/// not recover even after resizing back.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug2_resize_destroys_review_diff_layout() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let screen_before = open_review_diff(&mut harness);

    // Sanity: we have the full layout
    assert!(screen_before.contains("GIT STATUS"), "pre-check");
    assert!(screen_before.contains("DIFF"), "pre-check");

    // Resize down
    harness.resize(80, 24).unwrap();

    // Process async plugin commands from the resize handler
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") || s.contains("DIFF")
        })
        .unwrap();

    // Resize back to original
    harness.resize(120, 40).unwrap();

    // Wait for the layout to be rebuilt after resize-back
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") && s.contains("DIFF")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("BUG-2 screen after resize cycle:\n{}", screen_after);

    // The GIT STATUS header and DIFF panel should still be visible
    assert!(
        screen_after.contains("GIT STATUS"),
        "BUG-2: GIT STATUS header missing after resize cycle. The review \
         diff layout was destroyed. Screen:\n{}",
        screen_after
    );
    assert!(
        screen_after.contains("DIFF"),
        "BUG-2: DIFF panel missing after resize cycle. Screen:\n{}",
        screen_after
    );
}

// ---------------------------------------------------------------------------
// BUG-3: File Explorer steals focus from Review Diff on launch
// ---------------------------------------------------------------------------

/// BUG-3: When File Explorer is open, opening Review Diff does not transfer
/// focus.  `j` is captured by the File Explorer instead of navigating the
/// review diff file list.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug3_file_explorer_steals_review_diff_keys() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer with Ctrl+E
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Now open Review Diff while File Explorer is focused
    let screen = open_review_diff(&mut harness);

    // Verify review diff loaded
    assert!(screen.contains("GIT STATUS"), "Review diff should load");

    // Press j — should navigate the review diff file list,
    // NOT trigger the File Explorer quick-search
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after_j = harness.screen_to_string();
    println!("BUG-3 screen after j:\n{}", screen_after_j);

    // The File Explorer title should NOT change to "/j" (quick-search mode)
    assert!(
        !screen_after_j.contains("/j"),
        "BUG-3: File Explorer intercepted `j` key instead of review diff. \
         Review Diff should auto-focus when opened. Screen:\n{}",
        screen_after_j
    );
}

// ---------------------------------------------------------------------------
// BUG-4: Hunk navigation (n/p) non-functional in diff panel
// ---------------------------------------------------------------------------

/// BUG-4: Pressing `n` in the diff panel should jump to the next hunk
/// header, but the cursor stays at its current position.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug4_hunk_navigation_n_does_not_move_cursor() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Switch focus to the diff panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Record the screen before pressing n
    let _screen_before_n = harness.screen_to_string();

    // Press Home to go to line 1 of the diff
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Helper to extract the line number from the status bar ("Ln X, Col Y")
    fn extract_ln(screen: &str) -> Option<usize> {
        screen.lines().find_map(|l| {
            if let Some(idx) = l.find("Ln ") {
                let rest = &l[idx + 3..];
                let num_str: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                num_str.parse().ok()
            } else {
                None
            }
        })
    }

    let screen_at_home = harness.screen_to_string();
    let ln_before = extract_ln(&screen_at_home).expect("status bar should show Ln");

    // Press n to jump to next hunk — uses setBufferCursor (O(1)) to move
    // the cursor directly to the hunk header byte offset.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();

    // Give async commands time to process
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }

    let screen_after_n = harness.screen_to_string();
    let ln_after = extract_ln(&screen_after_n).expect("status bar should show Ln after n");

    println!("BUG-4 before n: Ln {}", ln_before);
    println!("BUG-4 after n:  Ln {}", ln_after);

    // The cursor line should have jumped forward to the next hunk header
    // (the multi-hunk file has two separate @@ regions, so Ln should
    // increase by more than 1).
    assert!(
        ln_after > ln_before,
        "BUG-4: Pressing `n` should move cursor forward to next hunk. \
         Ln before={}, Ln after={}. Screen:\n{}",
        ln_before,
        ln_after,
        screen_after_n
    );

    // Press n again to reach the second hunk, then p to go back
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen_at_second = harness.screen_to_string();
    let ln_second =
        extract_ln(&screen_at_second).expect("status bar should show Ln at second hunk");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen_after_p = harness.screen_to_string();
    let ln_after_p = extract_ln(&screen_after_p).expect("status bar should show Ln after p");
    assert!(
        ln_after_p < ln_second,
        "BUG-4: Pressing `p` should move cursor back to previous hunk. \
         Ln at_second={}, Ln after_p={}",
        ln_second,
        ln_after_p
    );
}

/// Verify that the status bar reads cursor position from the inner panel leaf
/// (via effective_active_split) rather than the outer split. After the fix,
/// the status bar correctly shows "*diff*" as the buffer name and the cursor
/// line number from the diff panel's view state.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_set_buffer_cursor_updates_status_bar_for_panel_buffer() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Switch focus to the diff panel with Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The status bar should show "*diff*" (the inner panel buffer name),
    // NOT the outer split's buffer name.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("*diff*"),
        "Status bar should display the inner panel buffer name '*diff*' \
         when a buffer group panel is focused. Screen:\n{}",
        screen
    );

    // Pressing 'n' should jump to the next hunk and update the status bar
    // line number — this verifies both the cursor movement and the status
    // bar reading from effective_active_split().
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    fn extract_ln(screen: &str) -> Option<usize> {
        screen.lines().find_map(|l| {
            if let Some(idx) = l.find("Ln ") {
                let rest = &l[idx + 3..];
                let num_str: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                num_str.parse().ok()
            } else {
                None
            }
        })
    }

    let screen_at_home = harness.screen_to_string();
    let ln_at_home = extract_ln(&screen_at_home).expect("status bar should show Ln");

    // Press n twice to navigate through hunks (first n → first hunk,
    // second n → second hunk)
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        for _ in 0..10 {
            harness.tick_and_render().unwrap();
        }
    }

    let screen_after = harness.screen_to_string();
    let ln_after = extract_ln(&screen_after).expect("status bar should show Ln");

    println!(
        "Panel cursor test: Ln at_home={}, Ln after_2n={}",
        ln_at_home, ln_after
    );
    assert!(
        ln_after > ln_at_home,
        "Hunk navigation should move the cursor forward. \
         Ln at_home={}, Ln after_2n={}",
        ln_at_home,
        ln_after
    );
}

// ---------------------------------------------------------------------------
// BUG-5: Side-by-side drill-down fails for deleted files
// ---------------------------------------------------------------------------

/// BUG-5: Pressing Enter on a deleted file shows "Loading side-by-side
/// diff..." indefinitely because readFile returns null for deleted files.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug5_deleted_file_drill_down_hangs() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Delete a tracked file to create a "D" status entry
    let utils_path = repo.path.join("src/utils.rs");
    fs::remove_file(&utils_path).expect("delete file");

    // Also modify a file so there's something to open the editor with
    let main_rs = repo.path.join("src/main.rs");
    fs::write(&main_rs, "fn main() { /* modified */ }\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("modified"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Navigate to find the deleted file (utils.rs with D status)
    // Navigate down through the file list to find it
    let mut found_deleted = false;
    for _ in 0..15 {
        let s = harness.screen_to_string();
        if s.contains("utils.rs") {
            found_deleted = true;
            break;
        }
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    if !found_deleted {
        // If utils.rs isn't visible, skip — the git status may not list it
        println!("BUG-5: Deleted file not found in review diff list, skipping");
        return;
    }

    // Navigate to the deleted file entry
    for _ in 0..15 {
        let s = harness.screen_to_string();
        // Check if the diff panel shows utils.rs
        if s.contains("DIFF FOR") && s.contains("utils.rs") {
            break;
        }
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Press Enter to drill down into side-by-side view for the deleted file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Give it a moment for the async operation
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    println!("BUG-5 deleted file drill-down:\n{}", screen);

    // The view should either show the diff or show an error — but NOT be
    // stuck on "Loading side-by-side diff..." forever
    assert!(
        !screen.contains("Loading side-by-side diff"),
        "BUG-5: Drill-down on deleted file is stuck on 'Loading side-by-side \
         diff...' — readFile returns null for deleted files. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-6: Comments added from files panel never display inline
// ---------------------------------------------------------------------------

/// BUG-6: Comments added while the files panel is focused are stored as
/// hunk-level comments with no line info, so they never render inline
/// in the diff view.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug6_comment_from_files_panel_not_visible_in_diff() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // With files panel focused (default), press c to add a comment
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();

    // Wait for comment prompt
    harness.wait_until(|h| h.editor().is_prompting()).unwrap();

    // Type a distinctive comment
    harness.type_text("BUG6_TEST_COMMENT").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Now switch to diff panel to look for the comment inline
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("BUG-6 screen after comment:\n{}", screen);

    // The comment should be visible somewhere in the diff panel
    assert!(
        screen.contains("BUG6_TEST_COMMENT"),
        "BUG-6: Comment added from files panel should be visible inline in \
         the diff view, but it is not rendered. Comments with no line info \
         are skipped by pushLineComments(). Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// BUG-7: Escape does not exit File Explorer focus
// ---------------------------------------------------------------------------

/// BUG-7: When the File Explorer has focus, pressing Escape should
/// transfer focus back to the editor/review diff, but it has no effect.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug7_escape_does_not_exit_file_explorer_focus() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Verify File Explorer is focused: pressing Down should move its cursor
    let _screen_before = harness.screen_to_string();

    // Press Escape to try to leave File Explorer focus
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now press `j` — if Escape worked, this should go to the editor.
    // If Escape did NOT work, the File Explorer still has focus and `j`
    // triggers its quick-search.
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("BUG-7 screen after Escape + j:\n{}", screen_after);

    // If File Explorer still has focus, its title changes to "/j"
    assert!(
        !screen_after.contains("/j"),
        "BUG-7: Escape should exit File Explorer focus, but it did not. \
         `j` was still captured by the File Explorer quick-search. Screen:\n{}",
        screen_after
    );
}

// ---------------------------------------------------------------------------
// BUG-9: Down arrow doesn't scroll viewport in side-by-side view
// ---------------------------------------------------------------------------

/// BUG-9: In the side-by-side diff view, pressing Down updates the status
/// bar line number but the viewport doesn't scroll to follow the cursor.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug9_side_by_side_down_arrow_no_viewport_scroll() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a file with many lines so we can scroll past the viewport
    let main_rs = repo.path.join("src/main.rs");
    let mut content = String::from("fn main() {\n");
    for i in 0..60 {
        content.push_str(&format!("    println!(\"Line {}\"); // LONG_FILE\n", i));
    }
    content.push_str("}\n");
    fs::write(&main_rs, &content).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        30, // Small viewport to force scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("LONG_FILE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side view
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen_initial = harness.screen_to_string();

    // Press Down many times to move past the visible viewport
    for _ in 0..25 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after_down = harness.screen_to_string();
    println!("BUG-9 initial:\n{}", screen_initial);
    println!("BUG-9 after 25 Downs:\n{}", screen_after_down);

    // The viewport content should have changed (scrolled) after moving
    // the cursor 25 lines down. If the viewport didn't scroll, the visible
    // lines will be identical to the initial state.
    // Compare the content area (skip status bar which will differ)
    let initial_content: Vec<&str> = screen_initial.lines().take(20).collect();
    let after_content: Vec<&str> = screen_after_down.lines().take(20).collect();

    assert_ne!(
        initial_content, after_content,
        "BUG-9: After pressing Down 25 times in side-by-side view, the \
         viewport should have scrolled to follow the cursor, but the \
         displayed content is unchanged. Screen:\n{}",
        screen_after_down
    );
}

// ---------------------------------------------------------------------------
// BUG-10: Toolbar "Export" label truncated
// ---------------------------------------------------------------------------

/// BUG-10: With the File Explorer sidebar open (narrower viewport), the
/// toolbar's `e Export` hint is truncated.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_bug10_toolbar_export_label_truncated() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100, // Narrow enough to trigger truncation with explorer open
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open File Explorer to narrow the review diff viewport
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Switch focus away from File Explorer so review diff keys work
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Re-open File Explorer
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    let screen = harness.screen_to_string();
    println!("BUG-10 screen:\n{}", screen);

    // Check if "Export" appears in the toolbar area
    // The toolbar shows hints like "s Stage  u Unstage  d Discard ... e Export"
    let has_full_export_label = screen.contains("e Export") || screen.contains("Export");

    // This is a cosmetic bug — we just note if the label is truncated
    assert!(
        has_full_export_label,
        "BUG-10: The toolbar 'Export' label is truncated when the File Explorer \
         narrows the viewport. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// Additional UX tests
// ---------------------------------------------------------------------------

/// After opening an embedded terminal, opening Review Diff should still work:
/// cursor movement and Tab panel switching should function correctly.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_review_diff_works_after_terminal_opened() {
    init_tracing_from_env();

    // Check PTY availability — skip test if not available
    if portable_pty::native_pty_system()
        .openpty(portable_pty::PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping test: PTY not available");
        return;
    }

    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    // Open an embedded terminal via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Open Terminal").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Give terminal time to start
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }

    // Now open Review Diff
    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("GIT STATUS"),
        "Review Diff should open successfully after terminal. Screen:\n{}",
        screen
    );

    // Tab should switch focus between file list and diff panels
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.tick_and_render().unwrap();
    }
    let screen_after_tab = harness.screen_to_string();
    // After Tab, either the files or diff panel should be focused.
    // Verify we're still in Review Diff (not switched to terminal tab).
    assert!(
        screen_after_tab.contains("Review Diff") || screen_after_tab.contains("GIT STATUS"),
        "Tab in Review Diff mode should toggle panels, not switch tabs. Screen:\n{}",
        screen_after_tab
    );

    // Down arrow should work for navigation (file list selection)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Verify the screen didn't produce an error
    let screen_after_move = harness.screen_to_string();
    assert!(
        !screen_after_move.contains("TypeError") && !screen_after_move.contains("Error:"),
        "Cursor movement should not produce errors after terminal was opened. Screen:\n{}",
        screen_after_move
    );
}

/// When the file list in Review Diff has more files than the visible height,
/// moving down/up beyond the view should auto-scroll.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_review_diff_file_list_auto_scrolls() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create many modified files to exceed the viewport
    for i in 0..20 {
        let path = repo.path.join(format!("src/mod_{}.rs", i));
        fs::write(&path, format!("fn func_{}() {{}}\n", i)).unwrap();
    }

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20, // Short viewport to force scrolling
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    let first_file = repo.path.join("src/mod_0.rs");
    harness.open_file(&first_file).unwrap();
    harness.render().unwrap();

    let _screen = open_review_diff(&mut harness);

    // Record the initial file list content
    let screen_initial = harness.screen_to_string();
    println!("File list scroll test: initial screen (20 files, 20 rows)");

    // Move down many times to go past the visible area in the files panel
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let screen_after = harness.screen_to_string();
    println!("File list scroll test: after 15x Down");

    // The visible file list content should have changed (scrolled)
    // Compare a few content lines — if auto-scroll works, we should see
    // different file names than initially.
    let initial_lines: Vec<&str> = screen_initial.lines().take(15).collect();
    let after_lines: Vec<&str> = screen_after.lines().take(15).collect();

    assert_ne!(
        initial_lines, after_lines,
        "File list should auto-scroll when moving cursor past visible area. \
         The visible content didn't change after 15 Down presses."
    );
}

// ---------------------------------------------------------------------------
// Toolbar rendering after refresh ('r')
// ---------------------------------------------------------------------------

/// Helper: assert that the toolbar contains the expected key hints.
/// The toolbar is the line containing "Stage" (always present in review mode).
fn assert_toolbar_rendered(screen: &str, context: &str) {
    assert!(
        screen.contains("s Stage"),
        "{context}: toolbar should contain 's Stage'. Screen:\n{screen}"
    );
    assert!(
        screen.contains("q Close"),
        "{context}: toolbar should contain 'q Close'. Screen:\n{screen}"
    );
}

/// Pressing 'r' (refresh) in Review Diff with modified files should
/// re-render the toolbar, file list, and diff panels correctly.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_refresh_preserves_toolbar_with_modifications() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);
    assert_toolbar_rendered(&screen, "Before refresh (modified files)");
    assert!(
        screen.contains("src/main.rs"),
        "File list should show the modified file before refresh"
    );

    // Press 'r' to refresh
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") && s.contains("DIFF")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    assert_toolbar_rendered(&screen_after, "After refresh (modified files)");
    assert!(
        screen_after.contains("src/main.rs"),
        "File list should still show the modified file after refresh"
    );
}

/// Open Review Diff when the working tree is clean (no modifications),
/// then modify a file externally and press 'r' — the toolbar must render
/// correctly both before and after refresh.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_refresh_toolbar_empty_then_modified() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Working tree is clean — no modifications
    let main_rs = repo.path.join("src/main.rs");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);
    assert_toolbar_rendered(&screen, "Clean working tree");

    // Now modify a file externally (outside the editor)
    fs::write(&main_rs, "fn main() {\n    println!(\"MODIFIED\");\n}\n").unwrap();

    // Press 'r' to refresh — should pick up the new modification
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    // Wait for all three panels (toolbar + file list + diff) to be populated.
    // The toolbar contains "s Stage"; the file list contains the modified file.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("src/main.rs") && s.contains("s Stage")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    assert_toolbar_rendered(&screen_after, "After refresh (new modification)");
    assert!(
        screen_after.contains("src/main.rs"),
        "File list should show newly modified file after refresh. Screen:\n{}",
        screen_after
    );
}

/// Open Review Diff with a staged file, press 'r' — toolbar and file list
/// should render correctly for staged content.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_refresh_toolbar_with_staged_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify and stage a file
    let main_rs = repo.path.join("src/main.rs");
    fs::write(
        &main_rs,
        "fn main() {\n    println!(\"STAGED_CHANGE\");\n}\n",
    )
    .unwrap();
    repo.git_add(&["src/main.rs"]);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);
    assert_toolbar_rendered(&screen, "Before refresh (staged file)");
    // Staged files should appear in the file list
    assert!(
        screen.contains("src/main.rs"),
        "File list should show staged file. Screen:\n{}",
        screen
    );

    // Press 'r' to refresh
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") && s.contains("DIFF")
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    assert_toolbar_rendered(&screen_after, "After refresh (staged file)");
    assert!(
        screen_after.contains("src/main.rs"),
        "File list should still show staged file after refresh. Screen:\n{}",
        screen_after
    );
}

// ---------------------------------------------------------------------------
// ISSUE #6: n / p are inert when the files pane has focus
// ---------------------------------------------------------------------------

/// On entry, focus sits on the files pane. Pressing `n` there should
/// still advance hunks — today the plugin branches on `focusPanel ===
/// 'diff'` and does nothing otherwise, so the key feels broken until
/// the user finds Tab.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue6_n_from_files_pane_advances_hunks() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _ = open_review_diff(&mut harness);
    // Do NOT press Tab — focus stays on the files pane.

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // The status bar's Hunk-index indicator (issue #3 fix) moves when a
    // hunk is current — that's our observable that `n` did something.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.to_lowercase().contains("hunk 1 of")
        })
        .unwrap();

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.to_lowercase().contains("hunk 2 of")
        })
        .unwrap();
}

// ---------------------------------------------------------------------------
// ISSUE #7: n / p do not cross file boundaries
// ---------------------------------------------------------------------------

/// Pressing `n` from the last hunk of file A should advance to the first
/// hunk of file B (in display order). Today it clamps at end of file A,
/// so a user reviewing multiple files has to `Tab` and `j` between files
/// to make progress.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue7_next_hunk_crosses_file_boundaries() {
    init_tracing_from_env();

    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Two files, one hunk each.
    let main_rs = repo.path.join("src/main.rs");
    fs::write(
        &main_rs,
        "fn main() {\n    println!(\"FILE_A_CHANGE\");\n}\n",
    )
    .unwrap();
    let lib_rs = repo.path.join("src/lib.rs");
    fs::write(
        &lib_rs,
        "// library\npub fn helper() {\n    // FILE_B_CHANGE\n}\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("FILE_A_CHANGE"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // The file list is sorted alphabetically, so lib.rs (FILE_B_CHANGE)
    // sits at index 0 and main.rs (FILE_A_CHANGE) at index 1. The diff
    // panel starts on lib.rs.
    let initial = harness.screen_to_string();
    assert!(
        initial.contains("FILE_B_CHANGE"),
        "pre-check: diff should start on lib.rs. Screen:\n{}",
        initial
    );

    // Tab into the diff pane. First `n` lands on lib.rs's only hunk.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness.tick_and_render().unwrap();
    }
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..5 {
        harness.tick_and_render().unwrap();
    }
    // Second `n`: must cross into main.rs's first hunk.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("FILE_A_CHANGE") && screen.contains("DIFF FOR src/main.rs"),
        "ISSUE-7: second `n` should have crossed from lib.rs to main.rs. \
         Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// ISSUE #8: n / p hints invisible in the files-pane toolbar
// ---------------------------------------------------------------------------

/// On entry to Review Diff, focus starts on the files pane. The toolbar
/// at that moment does not advertise the `n` / `p` hunk-navigation keys,
/// so a user who never presses Tab never discovers they exist. The hints
/// should be visible in both toolbars.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue8_n_and_p_hints_visible_on_files_pane_toolbar() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let screen = open_review_diff(&mut harness);
    // Focus is on the files pane at this point — no Tab has been pressed.
    // The toolbar must still advertise hunk navigation keys.
    assert!(
        screen.contains("Next") && screen.contains("Prev"),
        "ISSUE-8: files-pane toolbar must advertise the `n Next` / \
         `p Prev` hunk-navigation hints. Screen:\n{}",
        screen
    );
}

// ---------------------------------------------------------------------------
// ISSUE #1: Terminal resize leaves chrome hidden after shrink + grow
// ---------------------------------------------------------------------------

/// After shrinking the terminal to a small size and growing it back, the
/// menu bar, tab row, and toolbar should all be visible again. Checking
/// only GIT STATUS/DIFF (as test_bug2 does) is not enough — the bug that
/// the usability review flagged is that the *chrome* stays hidden.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue1_resize_cycle_restores_all_chrome() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let before = open_review_diff(&mut harness);

    // Pre-check: full chrome is present before resize.
    assert!(before.contains(" File "), "pre-check: menu visible");
    assert!(
        before.contains("*Review Diff*"),
        "pre-check: tab row visible"
    );
    // Group 1 fits comfortably; use "Stage" / "Unstage" as the toolbar
    // witness (later groups may degrade to key-only at narrow widths).
    assert!(
        before.contains("Stage") && before.contains("Unstage"),
        "pre-check: toolbar visible"
    );

    // Shrink.
    harness.resize(80, 24).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") || s.contains("DIFF")
        })
        .unwrap();

    // Grow back.
    harness.resize(160, 45).unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("GIT STATUS") && s.contains("DIFF")
        })
        .unwrap();

    // Every piece of chrome that was visible pre-resize must be visible again.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(" File ")
                && s.contains("*Review Diff*")
                && s.contains("Stage")
                && s.contains("Unstage")
        })
        .unwrap();
}

// ---------------------------------------------------------------------------
// ISSUE #2: Side-by-side n/p leave the status-bar Ln/Col stale
// ---------------------------------------------------------------------------

/// In the side-by-side diff view, pressing `n` (next hunk) jumps the
/// viewport and the composite cursor but does not update the editor's
/// primary cursor — so the status bar keeps reading "Ln 1" even after we
/// have navigated tens of lines down. Arrow keys sync correctly, so
/// hunk navigation should too.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue2_side_by_side_next_hunk_updates_status_bar() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Drill down into side-by-side.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            (s.contains("OLD (HEAD)") || s.contains("*Diff:"))
                && !s.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Pre-check: the status bar starts at Ln 1.
    let initial = harness.screen_to_string();
    assert!(
        initial.contains("Ln 1, Col 1"),
        "ISSUE-2 pre-check: status bar should read 'Ln 1, Col 1' on \
         entry to side-by-side. Screen:\n{}",
        initial
    );

    // Press `n` twice — the cursor should jump past line 1 into a later
    // hunk, so the status bar Ln indicator must update.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();

    // Semantic wait: the status bar should report a line other than 1.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.lines().any(|line| {
                if let Some(idx) = line.find("Ln ") {
                    let rest = &line[idx + 3..];
                    let num: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
                    if let Ok(n) = num.parse::<u32>() {
                        return n > 1;
                    }
                }
                false
            })
        })
        .unwrap();
}

// ---------------------------------------------------------------------------
// ISSUE #3: No "Hunk N of M" indicator
// ---------------------------------------------------------------------------

/// After opening Review Diff with multiple hunks and navigating through them,
/// the status bar should show a current-hunk index (e.g. "Hunk 1 of N"), not
/// just the total hunk count.
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue3_status_bar_shows_current_hunk_index() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_multi_hunk_file();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        45,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_ONE"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // Tab to the diff panel so `n` / `p` jump between hunks.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Jump to the first hunk.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            let l = s.to_lowercase();
            l.contains("hunk 1 of") || l.contains("hunk 1/")
        })
        .unwrap();

    // Advance to the second hunk.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            let l = s.to_lowercase();
            l.contains("hunk 2 of") || l.contains("hunk 2/")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("ISSUE-3 final screen:\n{}", screen);
}

// ---------------------------------------------------------------------------
// ISSUE #4: Empty state is ambiguous (not a git repo vs clean repo)
// ---------------------------------------------------------------------------

/// Open Review Diff in a non-git directory and in a clean git repo.
/// The two screens must not be byte-identical — the user needs to know why
/// there is no content (not a repository vs. no changes to review).
#[test]
#[ignore = "needs port to unified-stream layout"]
fn test_issue4_empty_state_distinguishes_not_git_from_clean_repo() {
    init_tracing_from_env();

    // Scenario A: plain (non-git) temp dir with the audit_mode plugin staged.
    let nongit = tempfile::TempDir::new().unwrap();
    let plugins_dir_a = nongit.path().join("plugins");
    fs::create_dir_all(&plugins_dir_a).unwrap();
    copy_plugin(&plugins_dir_a, "audit_mode");
    copy_plugin_lib(&plugins_dir_a);

    let mut harness_a = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        nongit.path().to_path_buf(),
    )
    .unwrap();
    harness_a.render().unwrap();

    let screen_nongit = open_review_diff(&mut harness_a);

    // Scenario B: clean git repo (committed, no working-tree changes).
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let mut harness_b = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness_b.render().unwrap();

    let screen_clean = open_review_diff(&mut harness_b);

    println!("ISSUE-4 non-git screen:\n{}", screen_nongit);
    println!("ISSUE-4 clean-repo screen:\n{}", screen_clean);

    // The two user-visible screens must differ — otherwise the user cannot
    // tell "not a git repo" from "clean repo, nothing to review".
    assert_ne!(
        screen_nongit, screen_clean,
        "ISSUE-4: non-git and clean-repo Review Diff screens are \
         byte-identical. Users cannot distinguish 'no repo' from \
         'no changes'. Non-git screen:\n{}\nClean-repo screen:\n{}",
        screen_nongit, screen_clean,
    );

    // Each screen should carry a readable affordance explaining the state.
    assert!(
        screen_nongit.to_lowercase().contains("not")
            && screen_nongit.to_lowercase().contains("git"),
        "ISSUE-4: non-git screen should mention it is not a git \
         repository. Screen:\n{}",
        screen_nongit
    );
    assert!(
        screen_clean.to_lowercase().contains("no changes")
            || screen_clean.to_lowercase().contains("no change"),
        "ISSUE-4: clean-repo screen should say there are no changes. \
         Screen:\n{}",
        screen_clean
    );
}

// ---------------------------------------------------------------------------
// ISSUE #2036: `r` refresh feels unreliable (slow async refresh shows stale
// numbers until it lands; range mode silently ignores working-tree edits).
// ---------------------------------------------------------------------------

/// Pressing `r` in working-tree mode must produce immediate visible
/// feedback. Before #2036 the handler kicked off the async `git status` +
/// `git diff` chain without updating any user-visible surface, so users
/// stared at stale `+N / -M` totals for the duration of the refresh and
/// concluded the keystroke had been dropped. The fix sets a status
/// message synchronously on the keypress so the user knows the refresh is
/// in flight even before the new diff lands.
#[test]
fn test_issue2036_refresh_shows_immediate_feedback() {
    init_tracing_from_env();
    let (repo, main_rs) = repo_with_one_modification();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Externally append more lines so the on-disk totals diverge from
    // what Review Diff currently shows. The refresh must pick this up.
    let mut new_content = fs::read_to_string(&main_rs).unwrap();
    new_content.push_str("// extra line one\n// extra line two\n// extra line three\n");
    fs::write(&main_rs, &new_content).unwrap();

    // Press `r` once. The very next render must already carry the
    // refresh-in-flight marker — that is the user's only signal that the
    // keystroke landed before the async git calls complete.
    //
    // Straight through `Editor::handle_key`, not `send_key`: the harness's
    // key path drains async work to a fixed point before it returns, and
    // for this plugin that means the whole `git status` + `git diff` chain.
    // The refresh would then be over — final status on the status bar,
    // new lines in the diff — before the first frame this test can look
    // at, and the wait below would sit forever on a message that had
    // already been overwritten. That is how this test timed out on CI: the
    // 10s screen dumps showed the refreshed diff and `Review Diff: 1
    // hunks` while the wait was still looking for "refreshing".
    harness
        .editor_mut()
        .handle_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();

    // Watch the frames from the keypress on, and stop as soon as either
    // the feedback shows or the refreshed diff lands without it — never
    // keep waiting for a state that can no longer arrive. "extra line
    // three" is one of the lines appended above, so it is on screen only
    // once the refresh has picked the file change up.
    let mut saw_feedback = false;
    harness
        .wait_until(|h| {
            let s = h.screen_to_string().to_lowercase();
            saw_feedback |= s.contains("refreshing");
            saw_feedback || s.contains("extra line three")
        })
        .unwrap();
    assert!(
        saw_feedback,
        "`r` must acknowledge the keypress before the async refresh lands; \
         the refreshed diff appeared with no refresh-in-flight status ever \
         rendered"
    );

    // The refresh should ultimately complete and the post-refresh status
    // summary (the existing "Review Diff: N hunks" message) should land.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string().to_lowercase();
            // Either the indexed form ("hunk X of N") or the bare summary;
            // both indicate `updateReviewStatus` has run.
            (s.contains("review diff:") || s.contains("hunk ")) && !s.contains("refreshing")
        })
        .unwrap();
}

/// In `range` mode the refresh is intentionally a no-op for working-tree
/// changes: the diff is always between two refs, so unstaged edits never
/// show up. Before #2036 there was no surface explaining this, so a user
/// who modified a file and then hit `r` saw nothing change and assumed
/// the refresh was broken. The status message now says so explicitly the
/// moment `r` is pressed.
#[test]
fn test_issue2036_range_refresh_explains_working_tree_excluded() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("first commit");

    // A second commit gives us a non-trivial HEAD~..HEAD range to review.
    let main_rs = repo.path.join("src/main.rs");
    fs::write(
        &main_rs,
        "fn main() {\n    println!(\"second commit content\");\n}\n",
    )
    .unwrap();
    repo.git_add_all();
    repo.git_commit("second commit");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();

    // Open Review Diff: Range against HEAD~..HEAD.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff: Range").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Range picker prompt is open with "HEAD" prefilled — clear it and
    // type our range.
    harness.wait_for_prompt().unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("HEAD~..HEAD").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Range mode opens with the range label in the toolbar header.
            s.contains("HEAD~..HEAD") && !s.contains("Generating Review")
        })
        .unwrap();

    // Modify the file in the working tree — exactly the case the issue
    // calls out as confusing.
    fs::write(
        &main_rs,
        "fn main() {\n    println!(\"working tree edit\");\n}\n",
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();

    // The status bar must explain why `r` looks like a no-op: range
    // refreshes don't include the working tree. Match a partial phrase
    // because the status bar trims long messages with an ellipsis at
    // narrower widths.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string().to_lowercase();
            s.contains("working tree not")
        })
        .unwrap();
}

/// Issue #2117: discarding a hunk whose change adds an *unterminated* final
/// line (no trailing newline) failed with "patch does not apply". The
/// reconstructed patch dropped git's "\ No newline at end of file" marker, so
/// `git apply --reverse` refused it even though the equivalent `git diff |
/// git apply --reverse` succeeds. With the marker preserved the discard
/// succeeds and the working tree is restored.
#[test]
fn test_issue2117_discard_hunk_with_no_trailing_newline() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Commit a file that ends with a newline.
    let original = "alpha\nbeta\ngamma\n";
    let notes = repo.create_file("notes.txt", original);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Working-tree change: append a final line WITHOUT a trailing newline,
    // which git renders with a "\ No newline at end of file" marker.
    fs::write(&notes, "alpha\nbeta\ngamma\nNO_NEWLINE_LINE").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&notes).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("NO_NEWLINE_LINE"))
        .unwrap();

    open_review_diff(&mut harness);

    // Focus the diff panel and land the cursor on the hunk (not the file
    // header) so `d` performs a *hunk*-level discard — the path that builds
    // and reverse-applies a patch.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // Wait for the hunk to be on screen rather than pumping a fixed number of
    // ticks: `tick_and_render` doesn't sleep, so `for _ in 0..10` elapses in
    // microseconds and gates nothing. `d` on an unloaded panel discards
    // nothing (or the wrong thing).
    harness
        .wait_until(|h| h.screen_to_string().contains("NO_NEWLINE_LINE"))
        .unwrap();

    // `d` opens the confirmation prompt; Enter accepts the default
    // ("Discard hunk").
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // The discard must actually revert the working tree on disk: the
    // unterminated added line is gone and the committed lines are restored.
    // Compare line-ending-agnostically — whether git writes the restored file
    // back as LF or CRLF depends on the user's core.autocrlf, which the test
    // leaves at its platform default; that's git's choice, not the feature's.
    //
    // Wait for the write instead of reading the file straight after a fixed
    // `for _ in 0..20 { tick_and_render() }` pump. The discard shells out to
    // git on the plugin thread; twenty sleepless ticks are microseconds, so
    // the old form read the file back before the patch had been applied and
    // failed within a couple of seconds — the Windows failure on this test.
    //
    // The wait also terminates on a rendered patch error, so a genuine
    // regression (the bug this test covers) still fails loudly and quickly
    // instead of hanging until nextest's timeout.
    let restored = |h: &EditorTestHarness| {
        let _ = h;
        fs::read_to_string(&notes)
            .map(|s| s.replace("\r\n", "\n") == original)
            .unwrap_or(false)
    };
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            restored(h) || screen.contains("Patch failed") || screen.contains("does not apply")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Patch failed") && !screen.contains("does not apply"),
        "Issue #2117: discarding a hunk that adds an unterminated final line \
         must not fail with a patch error. Screen:\n{}",
        screen
    );

    let after = fs::read_to_string(&notes).unwrap();
    assert_eq!(
        after.replace("\r\n", "\n"),
        original,
        "Issue #2117: discarding the hunk should restore the committed \
         content (removing the unterminated added line). Got: {after:?}"
    );
}

// ---------------------------------------------------------------------------
// Oversized changesets
// ---------------------------------------------------------------------------

/// Open Review Diff: Range on `HEAD~..HEAD` and wait for the stream to render.
fn open_review_range_head(harness: &mut EditorTestHarness) {
    // Through `run_palette_command`, which waits for the row to be listed as
    // a *result* before confirming. "Review Diff: Range" is registered by the
    // audit_mode plugin and Quick Open re-filters on input change only, so
    // typing the name and pressing Enter blind fires on whichever row was
    // selected when the last keystroke landed — running some other command,
    // after which none of the waits below can resolve.
    harness.run_palette_command("Review Diff: Range").unwrap();

    // Wait for the *range picker*, not merely for "a prompt". The palette is
    // itself a prompt, so `wait_for_prompt` is satisfied by the one we just
    // confirmed — a wait that gates nothing (CONTRIBUTING.md Code §16) and
    // that leaves the keystrokes below editing the palette's query instead
    // of the picker's prefilled "HEAD". The label is the picker's own
    // (`prompt.review_range`), so it can only be on screen once it is open.
    harness
        .wait_for_screen_contains("Review (range A..B or commit SHA)")
        .unwrap();

    // The picker opens prefilled with "HEAD" — clear it and type ours.
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("HEAD~..HEAD").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    wait_for_review_ready(harness);
}

/// Review Diff: Range parses Git's patch output, so it has to pin the output format
/// instead of mistaking a user's reformatted (or empty) diff for no changes.
#[test]
#[cfg(unix)]
fn test_range_review_ignores_external_diff_and_format_settings() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    repo.create_file("src/main.rs", "fn main() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file(
        "src/main.rs",
        "fn main() {\n    // RANGE_EXTERNAL_DIFF_MARKER\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("second commit");
    configure_hostile_diff_output(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    open_review_range_head(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("RANGE_EXTERNAL_DIFF_MARKER"),
        "Review Diff: Range should render Git's native patch even when diff.external, \
         a textconv filter, colour and prefix settings are configured. Screen:\n{screen}"
    );
    // The marker is the second line of the new file. Under the textconv filter
    // git would diff two header lines' worth of extra content and report it as
    // line 4, which parses cleanly and renders the marker just the same -- the
    // line number is the only thing that gives the corruption away.
    assert_eq!(
        marker_new_line_number(&screen, "RANGE_EXTERNAL_DIFF_MARKER"),
        Some(2),
        "Review Diff: Range should number the marker by the real file, not by \
         textconv output. Screen:\n{screen}"
    );
}

/// The same for the working-tree review, which fetches its hunks through a
/// different set of `git diff` invocations (staged / unstaged / untracked) and
/// so needs the format pinned independently of the range path.
#[test]
#[cfg(unix)]
fn test_review_diff_ignores_external_diff_and_format_settings() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    repo.create_file("src/main.rs", "fn main() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    // Leave the change in the working tree: this is the unstaged path.
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    // WORKTREE_EXTERNAL_DIFF_MARKER\n}\n",
    );
    configure_hostile_diff_output(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);

    assert!(
        screen.contains("WORKTREE_EXTERNAL_DIFF_MARKER"),
        "Review Diff should render Git's native patch for the working tree even \
         when diff.external, a textconv filter, colour and prefix settings are \
         configured. Screen:\n{screen}"
    );
    // See the range test: with textconv left on, the patch still parses and the
    // marker still renders, but every line number is shifted by the filter's
    // two header lines -- and those numbers are what `buildHunkPatch` hands to
    // `git apply` when staging or discarding the hunk.
    assert_eq!(
        marker_new_line_number(&screen, "WORKTREE_EXTERNAL_DIFF_MARKER"),
        Some(2),
        "Review Diff should number the marker by the real file, not by textconv \
         output. Screen:\n{screen}"
    );
}

/// The stash review is the only caller that reaches `git` through the
/// `range.command` override, so it is the only test that exercises splicing the
/// format flags into a multi-word sub-command (`stash show`). It is also where
/// the choice of `-c diff.srcPrefix=` over `--src-prefix=` is load-bearing:
/// `git stash show` mangles the latter, which would strip the `a/`/`b/` paths
/// the diff parser matches on.
#[test]
#[cfg(unix)]
fn test_stash_review_ignores_external_diff_and_format_settings() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    repo.create_file("src/main.rs", "fn main() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file(
        "src/main.rs",
        "fn main() {\n    // STASH_EXTERNAL_DIFF_MARKER\n}\n",
    );
    git_stash(&repo);
    configure_hostile_diff_output(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Review Diff: Stash").unwrap();
    // The ref prompt opens prefilled with `stash@{0}`, which is the entry we
    // just pushed, so confirm it as-is. Waiting for *a* prompt would not gate
    // on it: the palette this command was picked from is itself a prompt and
    // is still up when the Enter above is dispatched, so `wait_for_prompt`
    // resolves on the palette and the Enter below can land on it instead of
    // the ref picker (CONTRIBUTING.md Code §16). The picker's own label
    // (`prompt.review_stash`) can only be on screen once it is open.
    harness
        .wait_for_screen_contains("Review stash (e.g. stash@{0})")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    wait_for_review_ready(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("STASH_EXTERNAL_DIFF_MARKER"),
        "Review Diff: Stash should render Git's native patch even when diff.external, \
         colour and prefix settings are configured. Screen:\n{screen}"
    );
    assert_eq!(
        marker_new_line_number(&screen, "STASH_EXTERNAL_DIFF_MARKER"),
        Some(2),
        "Review Diff: Stash should number the marker by the real file. Screen:\n{screen}"
    );
}

/// `diff.suppressBlankEmpty=true` makes git print an empty context line as
/// `""` instead of `" "`. Review Diff classifies rows by their first byte, so
/// such a row used to vanish from the hunk and every row below it was numbered
/// one too low — the numbers `buildHunkPatch` then hands to `git apply`.
#[test]
#[cfg(unix)]
fn test_review_diff_keeps_blank_context_lines_under_suppress_blank_empty() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // The blank line is inside the hunk's leading context, above the change.
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    let a = 1;\n\n    let b = 2;\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file(
        "src/main.rs",
        "fn main() {\n    let a = 1;\n\n    let b = 2;\n    // BLANK_CONTEXT_MARKER\n}\n",
    );
    set_git_config(&repo, &[("diff.suppressBlankEmpty", "true")]);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);

    assert!(
        screen.contains("BLANK_CONTEXT_MARKER"),
        "Review Diff should render the hunk. Screen:\n{screen}"
    );
    // The marker is line 5 of the new file; with the blank context row
    // dropped it would be counted as line 4.
    assert_eq!(
        marker_new_line_number(&screen, "BLANK_CONTEXT_MARKER"),
        Some(5),
        "Review Diff should keep the empty context line above the marker and \
         number the marker by the real file. Screen:\n{screen}"
    );
}

/// With `core.quotePath=true` (git's default) a non-ASCII path is quoted and
/// octal-escaped in every diff header: `diff --git "a/src/a\303\261adido.rs"
/// ...`. Review Diff matches the unquoted `a/`/`b/` form, so the file used to
/// list with no hunks at all.
#[test]
#[cfg(unix)]
fn test_review_diff_parses_non_ascii_paths_under_quotepath() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    repo.create_file("src/añadido.rs", "fn main() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file(
        "src/añadido.rs",
        "fn main() {\n    // QUOTEPATH_MARKER\n}\n",
    );
    // Pinned explicitly so the test keeps its meaning if git's default moves.
    set_git_config(&repo, &[("core.quotePath", "true")]);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);

    assert!(
        screen.contains("QUOTEPATH_MARKER"),
        "Review Diff should render the hunk of a non-ASCII path even though \
         git quotes it in the diff headers. Screen:\n{screen}"
    );
    assert_eq!(
        marker_new_line_number(&screen, "QUOTEPATH_MARKER"),
        Some(2),
        "Review Diff should number the marker by the real file. Screen:\n{screen}"
    );
}

/// Side-by-Side Diff resolves the file's repo-relative path through
/// `git ls-files`, whose output is quoted under `core.quotePath` just like the
/// diff headers. A quoted path matches nothing afterwards, so the file was
/// treated as untracked and the view never opened.
#[test]
#[cfg(unix)]
fn test_side_by_side_diff_parses_non_ascii_paths_under_quotepath() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    let path = repo.create_file("src/añadido.rs", "fn main() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file(
        "src/añadido.rs",
        "fn main() {\n    // SIDE_BY_SIDE_QUOTEPATH_MARKER\n}\n",
    );
    set_git_config(&repo, &[("core.quotePath", "true")]);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Side-by-Side Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast: a failed path lookup reports the file as unchanged
            // and never opens the view, which would otherwise be a hang.
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{screen}");
            }
            screen.contains("OLD (HEAD)") && screen.contains("NEW (Working)")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("SIDE_BY_SIDE_QUOTEPATH_MARKER"),
        "Side-by-Side Diff should show the working-tree change of a non-ASCII \
         path. Screen:\n{screen}"
    );
}

/// A big range (the reported case: `HEAD~100..HEAD`) used to open on a
/// list of file headers and *no diff at all*. Above a line threshold the
/// stream rendered exactly one file, and the one it picked came from the
/// sidebar's dir-grouped order rather than the diff's, so it was rarely
/// the file at the top of the stream the cursor was parked on.
///
/// The threshold is gone — it was compensating for per-frame work in the
/// host that grew with the buffer's decorations, which is fixed — so what
/// has to hold now is simply that every file's hunks are in the stream:
/// the first file's on screen at the top, the last file's reachable by
/// walking hunks, and no file rendered as a bare header.
#[test]
fn test_range_review_lays_out_every_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Two files whose diff order (`src/a/deep.rs` then `src/zeta.rs`, sorted
    // by path) is *not* the sidebar's order, which groups by directory and
    // puts plain `src/` above `src/a/`. The stream is emitted in diff order,
    // so the cursor's first screen is the first file of the diff.
    repo.create_file("src/a/deep.rs", "fn deep() {}\n");
    repo.create_file("src/zeta.rs", "fn zeta() {}\n");
    repo.git_add_all();
    repo.git_commit("first commit");

    repo.create_file("src/a/deep.rs", "fn deep() {\n    // DEEP_MARKER\n}\n");
    repo.create_file("src/zeta.rs", "fn zeta() {\n    // ZETA_MARKER\n}\n");
    repo.git_add_all();
    repo.git_commit("second commit");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    open_review_range_head(&mut harness);

    // This is the load-bearing assertion, and it is specific: the sidebar
    // groups by directory and sorts `src/` above `src/a/`, so its first
    // file is `src/zeta.rs`, while the diff — which is what the stream
    // emits and where the cursor starts — begins with `src/a/deep.rs`.
    // Anything that renders only the sidebar's first file fails here.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("DEEP_MARKER"),
        "the stream must open on the first file of the *diff*, not a wall \
         of headers. Screen:\n{screen}"
    );

    // `n` walks hunks across file boundaries, into the rest of the diff.
    let mut crossed = false;
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        for _ in 0..5 {
            harness.tick_and_render().unwrap();
        }
        if harness.screen_to_string().contains("ZETA_MARKER") {
            crossed = true;
            break;
        }
    }
    let screen = harness.screen_to_string();
    assert!(
        crossed,
        "`n` should cross into the next file of the diff. Screen:\n{screen}"
    );
}

/// Clicking the sticky header jumps to the pinned file's first hunk. It
/// used to find that row by counting hunks and skipping collapsed files —
/// but collapse is a conceal, so a collapsed file's hunk headers are still
/// rows in the stream, and the count drifted by exactly the hunks it
/// skipped. With a collapsed file above, the click landed inside *that*
/// file instead of the pinned one.
#[test]
fn test_sticky_header_jump_survives_a_collapsed_file_above() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // `a_first.rs` sorts before `b_second.rs` in both the diff's order and
    // the sidebar's, so the collapsed file is reliably the one above.
    repo.create_file("src/a_first.rs", "fn one() {}\nfn two() {}\n");
    repo.create_file("src/b_second.rs", "fn three() {}\n");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    fs::write(
        repo.path.join("src/a_first.rs"),
        "fn one() {\n    // FIRST_CHANGE\n}\nfn two() {\n    // ALSO_FIRST\n}\n",
    )
    .unwrap();
    fs::write(
        repo.path.join("src/b_second.rs"),
        "fn three() {\n    // SECOND_CHANGE\n}\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);

    let row_containing = |h: &EditorTestHarness, needle: &str, below: u16| -> Option<u16> {
        (below..40).find(|r| h.screen_row_text(*r).contains(needle))
    };

    // The stream starts with the section header; the sticky header is the
    // row above it, past the separator the layout draws between them.
    let section_row = row_containing(&harness, "UNSTAGED", 0).expect("section header on screen");
    let sticky_row = section_row - 2;

    // Collapse the first file: step onto its header (the row after the
    // section header) and press Enter.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("FIRST_CHANGE"))
        .unwrap();

    // Put the cursor in the second file so the sticky header pins it.
    harness
        .send_key(KeyCode::Char('.'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("SECOND_CHANGE"))
        .unwrap();

    // The status bar's "Hunk N of M" is the plugin's own numbering over the
    // rows it rendered, so it says where the jump landed without mapping
    // screen rows to buffer lines — a mapping a collapsed file breaks.
    // src/b_second.rs is the last file, so its first hunk is the last hunk.
    harness.mouse_click(2, sticky_row).unwrap();
    harness
        .wait_until(|h| hunk_index(&h.screen_to_string()).is_some())
        .unwrap();

    let screen = harness.screen_to_string();
    let (current, count) = hunk_index(&screen).expect("status bar reports a hunk index");
    assert!(
        count >= 2,
        "test needs at least two hunks to tell the files apart, got {count}. \
         Screen:\n{screen}"
    );
    assert_eq!(
        current, count,
        "clicking the sticky header pinned to src/b_second.rs should land on \
         its hunk — the last one — but landed on hunk {current} of {count}, \
         inside the collapsed file above. Screen:\n{screen}"
    );
}

/// Parse `Hunk N of M` out of the review status line.
fn hunk_index(screen: &str) -> Option<(usize, usize)> {
    let idx = screen.find("Hunk ")?;
    let rest = &screen[idx + 5..];
    let (current, rest) = rest.split_once(" of ")?;
    let count: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
    Some((current.trim().parse().ok()?, count.parse().ok()?))
}

/// Every file of an ordinary review is laid out too — the same contract,
/// on the path most reviews take.
#[test]
fn test_normal_review_lays_out_every_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    fs::write(
        repo.path.join("src/main.rs"),
        "fn main() {\n    println!(\"MAIN_MARKER\");\n}\n",
    )
    .unwrap();
    fs::write(
        repo.path.join("src/utils.rs"),
        "pub fn format_output(msg: &str) -> String {\n    // UTILS_MARKER\n    msg.to_string()\n}\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("MAIN_MARKER") && screen.contains("UTILS_MARKER"),
        "both changed files' hunks belong in the stream. Screen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// FILES sidebar: it owns its scroll window
// ---------------------------------------------------------------------------

/// A repo with `count` modified `.rs` files — enough of them that the
/// FILES sidebar's tree cannot show them all at once.
fn repo_with_many_modified_files(count: usize) -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    for i in 0..count {
        fs::write(
            repo.path.join(format!("src/mod_{i:03}.rs")),
            format!("pub fn f_{i}() {{}}\n"),
        )
        .unwrap();
    }
    repo.git_add_all();
    repo.git_commit("Initial commit");
    for i in 0..count {
        fs::write(
            repo.path.join(format!("src/mod_{i:03}.rs")),
            format!("pub fn f_{i}() {{ /* CHANGED */ }}\n"),
        )
        .unwrap();
    }
    repo
}

/// The sidebar's file rows, read out of the left-hand column band the
/// FILES panel occupies (the diff stream names files too, so the rest of
/// the row has to be cut away before counting).
fn sidebar_file_rows(screen: &str, sidebar_cols: usize) -> Vec<String> {
    screen
        .lines()
        .map(|l| l.chars().take(sidebar_cols).collect::<String>())
        .filter(|l| l.contains("mod_"))
        .collect()
}

/// Show the FILES sidebar (the `F` toggle) and settle the frame.
fn show_files_panel(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('F'), KeyModifiers::SHIFT)
        .unwrap();
    harness.tick_and_render().unwrap();
    harness.tick_and_render().unwrap();
}

/// Growing the terminal must grow the FILES tree with it. The row budget
/// is the host's — the panel's live height minus its header and filter
/// row — so the rows the resize added fill with files instead of staying
/// blank while files are still unshown.
#[test]
fn test_files_panel_fills_its_height_after_a_resize() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(60);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);

    let before = sidebar_file_rows(&harness.screen_to_string(), 20).len();
    assert!(
        before > 0,
        "the sidebar should list files to begin with. Screen:\n{}",
        harness.screen_to_string()
    );

    harness.resize(120, 55).unwrap();
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    let after = sidebar_file_rows(&screen, 20).len();
    assert!(
        after >= before + 20,
        "25 rows of new panel height should carry ~25 more files, not stay \
         blank: {before} rows before the resize, {after} after. Screen:\n{screen}"
    );
}

/// Wheeling past the end of the file tree must stop there. The panel
/// buffer underneath is pinned (`scrollable: false`), so the wheel can't
/// fall through to it and carry the panel's own header off the top —
/// which left a blank row at the bottom that nothing could scroll back.
#[test]
fn test_files_panel_wheel_past_the_end_keeps_the_header() {
    use crossterm::event::{MouseEvent, MouseEventKind};

    init_tracing_from_env();
    let repo = repo_with_many_modified_files(60);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);

    assert!(
        harness.screen_to_string().contains("FILES"),
        "the sidebar starts with its header on screen. Screen:\n{}",
        harness.screen_to_string()
    );

    // Well past the bottom of a 60-file tree, so the last events arrive
    // with the tree already at its bound.
    for _ in 0..60 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                column: 5,
                row: 10,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("FILES"),
        "the FILES header must survive a wheel that runs off the end of the \
         tree — it scrolled off when the wheel fell through to the panel \
         buffer. Screen:\n{screen}"
    );
}

/// The sidebar's scrollbar is the tree's, and it is grabbable: a press on
/// the track jumps the tree to that position. The bar is painted inside
/// the panel (the panel reserves the columns for it) because the panel's
/// buffer is pinned and so reserves no scrollbar column of its own.
#[test]
fn test_files_panel_scrollbar_press_scrolls_the_tree() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(60);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);

    let screen = harness.screen_to_string();
    // The panel's right edge: the column of the divider between the
    // sidebar and the diff, on a row the sidebar is drawing files into.
    let divider_col = screen
        .lines()
        .find(|l| l.chars().take(40).collect::<String>().contains("mod_"))
        .and_then(|l| l.chars().position(|c| c == '\u{2502}'))
        .expect("the sidebar is drawn next to the diff") as u16;
    let track_col = divider_col - 1;

    let before = sidebar_file_rows(&screen, 20);
    assert!(!before.is_empty());

    // Near the bottom of the track: the tree should jump toward the end
    // of the list.
    harness.mouse_click(track_col, 25).unwrap();
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    let after = sidebar_file_rows(&screen, 20);
    assert_ne!(
        before, after,
        "pressing the sidebar's scrollbar should scroll its tree. \
         Screen:\n{screen}"
    );
}

/// A hunk carrying an empty context line is still numbered by counting
/// that line.
///
/// `diff.suppressBlankEmpty` prints an empty context line as `` rather
/// than `` ``, and a row's first byte is exactly what the host's diff
/// gutter classifies it by: with the marker gone the row counts as
/// neither side, and every number below it in the hunk is one short of
/// the line the plugin stages and anchors comments to. The two would
/// then disagree, and the reader would stage a line other than the one
/// the gutter names.
#[test]
#[cfg(unix)]
fn test_review_diff_numbers_rows_past_an_empty_context_line() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // The blank line is line 2, inside the hunk and above the change, so
    // a gutter that skips it misnumbers the marker.
    repo.create_file(
        "src/main.rs",
        "fn main() {\n\n    let x = 1;\n    let y = 2;\n    let z = 3;\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("first commit");
    repo.create_file(
        "src/main.rs",
        "fn main() {\n\n    let x = 1;\n    let y = 2;\n    let z = 4; // BLANK_CONTEXT_MARKER\n}\n",
    );
    configure_hostile_diff_output(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("BLANK_CONTEXT_MARKER"),
        "the changed line should be on screen. Screen:\n{screen}"
    );
    assert_eq!(
        marker_new_line_number(&screen, "BLANK_CONTEXT_MARKER"),
        Some(5),
        "the marker is line 5 of the working file; a gutter that does not \
         count the empty context line above it numbers this row 4, and the \
         reader then stages the line above the one the gutter names. \
         Screen:\n{screen}"
    );
}

/// The FILES sidebar lays its rows out to the width the host gave the
/// panel, not to a guess.
///
/// The panel's geometry was never reported — a pane arriving on screen was
/// not treated as a change — so the plugin elided rows to a ratio of
/// whatever viewport it had recorded, and nothing corrected it until a
/// divider drag. A narrow terminal makes the difference visible: the guess
/// has a 12-column floor, and rows built to 12 columns in a 10-column
/// panel are clipped, `…` and status letter included.
#[test]
fn test_files_panel_rows_fit_a_narrow_panel() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(4);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        40,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);

    // Read off the divider rather than assumed: the share of a 40-column
    // screen is the host's to decide.
    let sidebar_cols = |h: &EditorTestHarness| -> usize {
        h.screen_to_string()
            .lines()
            .filter_map(|l| l.find('\u{2502}'))
            .min()
            .unwrap_or(0)
    };
    let settled = |h: &EditorTestHarness| {
        let rows = sidebar_file_rows(&h.screen_to_string(), sidebar_cols(h));
        !rows.is_empty() && rows.iter().all(|r| r.trim_end().ends_with(" M"))
    };
    // The repaint is coalesced behind a short timer, so the frame the
    // toggle painted is not the one to assert on.
    for _ in 0..60 {
        if settled(&harness) {
            break;
        }
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }

    let cols = sidebar_cols(&harness);
    let screen = harness.screen_to_string();
    let rows = sidebar_file_rows(&screen, cols);
    assert!(
        !rows.is_empty(),
        "the sidebar should be listing files. Screen:\n{screen}"
    );
    for row in &rows {
        assert!(
            row.trim_end().ends_with(" M"),
            "every row keeps its status letter inside the panel — this one \
             was built too wide and clipped: {row:?}. Screen:\n{screen}"
        );
    }
}

/// The sidebar row carrying the tree's selection band, found by the
/// background a widget tree paints behind its selected node
/// (`ui.popup_selection_bg`) within the panel's own columns.
fn selected_sidebar_row(harness: &EditorTestHarness) -> Option<String> {
    let selection_bg = harness.editor().theme().popup_selection_bg;
    let screen = harness.screen_to_string();
    let divider = screen
        .lines()
        .filter_map(|line| line.find('\u{2502}'))
        .min()? as u16;
    let area = harness.buffer().area;
    (0..area.height).find_map(|y| {
        let washed = (0..divider)
            .filter(|&x| {
                harness
                    .get_cell_style(x, y)
                    .is_some_and(|style| style.bg == Some(selection_bg))
            })
            .count();
        if washed < 3 {
            return None;
        }
        screen
            .lines()
            .nth(y as usize)
            .map(|line| line.chars().take(divider as usize).collect())
    })
}

/// Clicking a file in the sidebar moves the highlight to it — including
/// from the reading posture, with the keys on the diff.
///
/// The click's write recorded a `List` state for a `Tree`, which the tree
/// ignores in favour of the spec's seed, so the highlight only landed
/// right when a repaint followed the click — as it does in side-by-side,
/// where picking a file rebuilds the panel, and does not in the unified
/// stream.
#[test]
fn test_clicking_a_file_moves_the_sidebar_highlight() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(5);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);
    harness.tick_and_render().unwrap();

    // The keys on the diff: reading, not navigating the sidebar.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    let row = screen
        .lines()
        .enumerate()
        .filter(|(_, l)| {
            l.chars()
                .take(20)
                .collect::<String>()
                .contains("mod_004.rs")
        })
        .map(|(y, _)| y as u16)
        .next()
        .unwrap_or_else(|| panic!("the sidebar lists mod_004.rs. Screen:\n{screen}"));

    harness.mouse_click(5, row).unwrap();
    for _ in 0..20 {
        if selected_sidebar_row(&harness).is_some_and(|r| r.contains("mod_004.rs")) {
            break;
        }
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }

    let selected = selected_sidebar_row(&harness);
    assert!(
        selected
            .as_deref()
            .is_some_and(|r| r.contains("mod_004.rs")),
        "clicking mod_004.rs should move the sidebar highlight onto it, \
         not leave it on {selected:?}. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The highlight follows the *file*, not a row number. The host stores an
/// absolute index into a node list the sidebar rebuilds whenever the
/// changeset moves, so without re-pinning from the key the band slides
/// onto the neighbouring file — and stays there, the current file never
/// having changed.
#[test]
fn test_sidebar_highlight_survives_a_rebuild_above_it() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(5);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    let row = screen
        .lines()
        .enumerate()
        .filter(|(_, l)| {
            l.chars()
                .take(20)
                .collect::<String>()
                .contains("mod_004.rs")
        })
        .map(|(y, _)| y as u16)
        .next()
        .unwrap_or_else(|| panic!("the sidebar lists mod_004.rs. Screen:\n{screen}"));
    harness.mouse_click(5, row).unwrap();
    harness
        .wait_until(|h| selected_sidebar_row(h).is_some_and(|r| r.contains("mod_004.rs")))
        .unwrap();

    // Sorts above every `mod_*`, and the review's watch poll picks it up:
    // the node list grows a row above the selection.
    fs::write(repo.path.join("src/lib.rs"), "pub struct Config {}\n").unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|l| l.chars().take(20).collect::<String>().contains("lib.rs"))
        })
        .unwrap();
    harness.tick_and_render().unwrap();

    let selected = selected_sidebar_row(&harness);
    assert!(
        selected
            .as_deref()
            .is_some_and(|r| r.contains("mod_004.rs")),
        "a file appearing above the selection must not slide the highlight \
         onto its neighbour — it is on {selected:?}. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The band says "the file you are reading", so it has to survive the tree
/// losing keyboard focus. The host clears a blurred tree's selection, so
/// clicking the panel's filter field takes the marker away unless the
/// plugin puts it back.
#[test]
fn test_sidebar_highlight_survives_focusing_the_filter() {
    init_tracing_from_env();
    let repo = repo_with_many_modified_files(5);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    show_files_panel(&mut harness);
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    let file_row = screen
        .lines()
        .enumerate()
        .filter(|(_, l)| {
            l.chars()
                .take(20)
                .collect::<String>()
                .contains("mod_004.rs")
        })
        .map(|(y, _)| y as u16)
        .next()
        .unwrap_or_else(|| panic!("the sidebar lists mod_004.rs. Screen:\n{screen}"));
    harness.mouse_click(5, file_row).unwrap();
    harness
        .wait_until(|h| selected_sidebar_row(h).is_some_and(|r| r.contains("mod_004.rs")))
        .unwrap();

    let filter_row = screen
        .lines()
        .position(|l| l.contains("Filter files"))
        .unwrap_or_else(|| panic!("the sidebar has a filter field. Screen:\n{screen}"))
        as u16;
    harness.mouse_click(5, filter_row).unwrap();
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }

    let selected = selected_sidebar_row(&harness);
    assert!(
        selected
            .as_deref()
            .is_some_and(|r| r.contains("mod_004.rs")),
        "focusing the filter field must not take the \"you are here\" band \
         away — it is on {selected:?}. Screen:\n{}",
        harness.screen_to_string()
    );
}
