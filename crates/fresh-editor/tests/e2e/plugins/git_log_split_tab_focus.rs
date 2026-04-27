//! Regression test: clicking a buffer-group tab (e.g. *Git Log*) in a split
//! pane must activate the group in THAT pane, not in whichever pane happens
//! to be focused.
//!
//! Reproduction:
//!   1. open a file
//!   2. open Git Log via command palette (single split with two tabs:
//!      hello.txt and *Git Log*)
//!   3. click the hello.txt tab so the file buffer is the active tab again
//!   4. run "split horizontal" — this creates a new split BELOW the original
//!      and the new (bottom) split becomes the active one
//!   5. click the *Git Log* tab in the TOP pane's tab bar
//!
//! Expected: the Git Log view renders in the TOP pane where its tab lives.
//! Bug: the Git Log view renders in the BOTTOM pane because the tab-click
//! handler routed the activation through `active_split` instead of the
//! clicked split.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::time::Duration;

fn advance_past_double_click(harness: &mut EditorTestHarness) {
    let dc = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc);
}

/// Find the column where the given text starts in a specific row, counting
/// chars (not bytes) so wide box-drawing glyphs don't throw off the index.
fn col_of_text_in_row(harness: &EditorTestHarness, row: u16, needle: &str) -> u16 {
    let row_text = harness.screen_row_text(row);
    let needle: Vec<char> = needle.chars().collect();
    let chars: Vec<char> = row_text.chars().collect();
    chars
        .windows(needle.len())
        .position(|w| w == needle.as_slice())
        .unwrap_or_else(|| panic!("{:?} not in row {row}: {row_text:?}", needle)) as u16
}

#[test]
fn clicking_group_tab_activates_group_in_the_clicked_split() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_log_plugin();

    // Deliberately not calling `repo.change_to_repo_dir()` — it mutates
    // process-global cwd, which is not safe under parallel test execution
    // (CONTRIBUTING §4). The git_log plugin passes `editor.getCwd()` to
    // `spawnProcess`, which resolves to the editor's `working_dir` set
    // below — process cwd is not needed.
    let width = 120u16;
    let height = 40u16;
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open a file so we have a concrete, non-scratch tab to anchor on.
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();

    // Trigger Git Log via the command palette. This creates a buffer group
    // and adds a *Git Log* tab to the current split.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane") && s.contains("Initial commit")
        })
        .unwrap();

    // Step 3: click the main.rs tab so the file buffer becomes the active
    // tab again (clearing the active group). Tabs are on row 1.
    const TAB_BAR_ROW: u16 = 1;
    let file_tab_col = col_of_text_in_row(&harness, TAB_BAR_ROW, "main.rs");
    advance_past_double_click(&mut harness);
    harness.mouse_click(file_tab_col, TAB_BAR_ROW).unwrap();
    // Git Log toolbar ("switch pane" hint) is gone because the file tab is
    // now active in the single split.
    harness
        .wait_until(|h| !h.screen_to_string().contains("switch pane"))
        .unwrap();

    // Step 4: split horizontally via the command palette. This creates a
    // new split BELOW the original, and the new (bottom) split becomes
    // the active one. The top split still has both tabs.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("split horiz").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the split to be in place by observing rendered buffer
    // *content* in the lower half of the screen, rather than the transient
    // "Split pane horizontally" core status message: any plugin
    // `editor.setStatus(...)` call (the active git_log group emits several
    // around layout/focus changes) clears `App::status_message` and wins, so
    // the core message can disappear before wait_until samples it.
    //
    // We can't key on a specific buffer's source string (e.g. "Hello,
    // world!" from main.rs) because the active buffer at the moment split-
    // horiz fires is non-deterministic — we've observed both the *log*
    // group buffer and main.rs landing in the new bottom pane on different
    // platforms. What is stable: whatever buffer the new pane gets, it is
    // rendered as an editable view with a line-number gutter (the `│`
    // separator after each line number). Before the split, the lower half
    // of the screen is just `~` empty-line markers (main.rs is 13 lines
    // and the pane is 38+ rows tall), so a gutter `│` never appears below
    // the half-screen mark. The split places a fresh buffer view below the
    // horizontal separator, so a `│` appearing in the lower half is the
    // monotonic post-split signal.
    let lower_half_start = (height / 2) as usize;
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .enumerate()
                .any(|(row, line)| row >= lower_half_start && line.contains('│'))
        })
        .unwrap();

    // Sanity check: the *Git Log* tab label still lives on the top tab bar
    // (row 1). It must be there for the next click to target the TOP pane.
    let git_log_tab_col = col_of_text_in_row(&harness, TAB_BAR_ROW, "Git Log");

    // Step 5: click the *Git Log* tab in the TOP pane's tab bar.
    advance_past_double_click(&mut harness);
    harness.mouse_click(git_log_tab_col, TAB_BAR_ROW).unwrap();
    // Let the group activate and re-render.
    harness
        .wait_until(|h| h.screen_to_string().contains("switch pane"))
        .unwrap();

    // The Git Log's sticky toolbar ("switch pane" hint) must appear in the
    // TOP half of the screen, NOT the BOTTOM half. Without the fix, the
    // activation lands on the newly-created (active) bottom split, so the
    // toolbar shows up below the horizontal separator instead of above it.
    let screen = harness.screen_to_string();
    let toolbar_rows: Vec<usize> = screen
        .lines()
        .enumerate()
        .filter(|(_, line)| line.contains("switch pane"))
        .map(|(row, _)| row)
        .collect();

    let top_half_end = (height / 2) as usize;
    assert!(
        !toolbar_rows.is_empty(),
        "git log toolbar not rendered at all; screen:\n{screen}"
    );
    assert!(
        toolbar_rows.iter().all(|row| *row < top_half_end),
        "git log activated in the wrong split: 'switch pane' toolbar \
         appeared on rows {toolbar_rows:?}, but the clicked *Git Log* tab \
         lives in the TOP pane (rows < {top_half_end}). Screen:\n{screen}"
    );
}
