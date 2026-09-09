//! Paging over a collapsed file in the Review Diff unified stream
//! (issue #3029).
//!
//! A collapsed file draws one header row, so a page of scrolling has to
//! step over its whole body for free. When the page motion spends its
//! budget on the hidden rows instead, the view stops moving — and the
//! cursor walks off into content nobody can see — for as many presses as
//! the collapsed file has hidden rows.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

fn open_review_diff(harness: &mut EditorTestHarness) {
    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("next hunk") && !screen.contains("Generating Review")
        })
        .unwrap();
}

/// A repo whose working tree rewrites every line of every file, so file
/// `f` contributes roughly `2 * lines[f]` rows to the unified stream.
fn repo_with_rewritten_files(lines: &[usize]) -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    for (f, count) in lines.iter().enumerate() {
        let body: String = (0..*count)
            .map(|l| format!("fn original_{f:02}_{l}() {{ let x = {l}; }}\n"))
            .collect();
        repo.create_file(&format!("src/file{f:02}.rs"), &body);
    }
    repo.git_add_all();
    repo.git_commit("baseline");
    for (f, count) in lines.iter().enumerate() {
        let body: String = (0..*count)
            .map(|l| format!("fn changed_{f:02}_{l}() {{ let y = {l}; }}\n"))
            .collect();
        repo.create_file(&format!("src/file{f:02}.rs"), &body);
    }
    repo
}

fn press(harness: &mut EditorTestHarness, code: KeyCode) {
    harness.send_key(code, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Paging towards a collapsed file has to walk *over* it, not through it.
/// `src/file02.rs` here is forty times longer than its neighbours, so a
/// fold-blind page motion parks the view at its header for dozens of
/// presses while the cursor crawls through rows the renderer never draws.
#[test]
fn test_paging_steps_over_a_collapsed_file() {
    init_tracing_from_env();
    // file00 / file01 lead in, file02 is the big body to collapse, and
    // file03 is the landing zone just past it.
    let repo = repo_with_rewritten_files(&[20, 20, 800, 20]);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    open_review_diff(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("original_00_0"))
        .unwrap();

    // `.` walks file to file; Enter on a file header is its disclosure
    // toggle. Collapse file02, then come back to the top of the stream.
    for _ in 0..2 {
        press(&mut harness, KeyCode::Char('.'));
    }
    press(&mut harness, KeyCode::Enter);
    harness
        .wait_until(|h| h.screen_to_string().contains("\u{25b8} src/file02.rs"))
        .unwrap();
    for _ in 0..2 {
        press(&mut harness, KeyCode::Char(','));
    }
    harness
        .wait_until(|h| h.screen_to_string().contains("original_00_0"))
        .unwrap();

    // file00 and file01 draw ~40 diff rows each and the collapsed file02
    // draws one, so file03's content is well under 150 rendered rows away
    // — a handful of pages on a 40-row terminal. The cap is deliberately
    // loose; the fold-blind motion needs ~60 presses to cross file02's
    // 1600 hidden rows.
    const CAP: usize = 30;
    let mut down = 0;
    while !harness.screen_to_string().contains("_03_") {
        assert!(
            down < CAP,
            "PageDown never reached src/file03.rs past the collapsed \
             src/file02.rs — {CAP} presses were spent inside its hidden \
             body. Screen:\n{}",
            harness.screen_to_string()
        );
        press(&mut harness, KeyCode::PageDown);
        down += 1;
    }

    // And back: PageUp has to step over the same fold in one press too.
    let mut up = 0;
    while !harness.screen_to_string().contains("original_00_0") {
        assert!(
            up < CAP,
            "PageUp never got back above the collapsed src/file02.rs — \
             {CAP} presses were spent inside its hidden body. Screen:\n{}",
            harness.screen_to_string()
        );
        press(&mut harness, KeyCode::PageUp);
        up += 1;
    }
}
