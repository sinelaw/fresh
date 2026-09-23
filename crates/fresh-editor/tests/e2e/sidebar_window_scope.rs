//! Sidebar sections are scoped to what the user is looking at
//! (sinelaw/fresh#3326, D′ / F).
//!
//! * **D′** — with a different Markdown file in each window, each window
//!   shows its own outline, and activating a row never opens a file in the
//!   other window — before the fix it opened a *second, independent buffer*
//!   of the other window's file in this one.
//! * **F** — a section restored from a workspace whose plugin never mounts
//!   it again is a placeholder only until plugins have loaded; after that it
//!   is gone.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crate::common::sidebar::{contents_rows, install_markdown_plugins, screen_has, wait_for};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::Path;

const DOC_A: &str = "# Alpha Doc\n\n## First\n\ntext\n\n## Second\n\nmore\n";
const DOC_B: &str = "# Beta Doc\n\n## Uno\n\ntext\n\n## Dos\n\nmore\n";

fn launch(project: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_working_dir(project.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

/// Show the sidebar column if it is hidden, then hand the keyboard back to
/// the editor.
fn show_sidebar(h: &mut EditorTestHarness) {
    if !screen_has(h, "File Explorer") {
        h.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
            .unwrap();
        h.wait_for_file_explorer().unwrap();
        h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
}

fn has_row(h: &EditorTestHarness, row: &str) -> bool {
    contents_rows(h).iter().any(|r| r == row)
}

/// **D′.** Each window shows its own outline, and a row activated in window B
/// jumps inside B's own document — it never opens A's file in B.
#[test]
fn each_window_shows_its_own_outline_and_a_row_never_opens_the_other_windows_file() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.md"), DOC_A).unwrap();
    let other = temp_dir.path().join("other");
    fs::create_dir(&other).unwrap();
    fs::write(other.join("b.md"), DOC_B).unwrap();
    install_markdown_plugins(&project);
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let mut h = launch(&project, &dir_context);

    h.open_file(&project.join("a.md")).unwrap();
    show_sidebar(&mut h);
    wait_for(&mut h, "A's outline", |h| has_row(h, "Alpha Doc"));

    let b = h
        .editor_mut()
        .create_window_at(other.clone(), "other".to_string());
    h.editor_mut().set_active_window(b);
    h.open_file(&other.join("b.md")).unwrap();
    show_sidebar(&mut h);
    wait_for(&mut h, "B's outline", |h| has_row(h, "Beta Doc"));
    assert!(
        !has_row(&h, "Alpha Doc"),
        "window B lists window A's headings\n{}",
        h.screen_to_string()
    );

    // Activate a row: Focus Next Sidebar Section twice (explorer, then the
    // section), Down onto "Uno", Enter. The activation goes to the plugin
    // and comes back as a jump to "## Uno" (line 3).
    h.run_palette_command("Focus Next Sidebar Section").unwrap();
    h.run_palette_command("Focus Next Sidebar Section").unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    wait_for(&mut h, "the jump to Uno", |h| screen_has(h, "Ln 3,"));
    let tabs = h
        .screen_to_string()
        .lines()
        .find(|l| l.contains("b.md"))
        .unwrap_or_default()
        .to_string();
    assert!(
        !tabs.contains("a.md"),
        "a row in B's Contents opened A's file in B: {tabs:?}\n{}",
        h.screen_to_string()
    );

    // Back in A, A's own outline, untouched.
    h.editor_mut().set_active_window(fresh_core::WindowId(1));
    wait_for(&mut h, "A's outline after the return", |h| {
        has_row(h, "Alpha Doc")
    });
    assert!(!has_row(&h, "Beta Doc"));
}

/// **F.** A restored section whose plugin never mounts it again is gone once
/// plugins have loaded — no "Panel unavailable" placeholder for the life of
/// the workspace.
#[test]
fn a_restored_section_nobody_mounts_does_not_outlive_plugin_load() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.md"), DOC_A).unwrap();
    install_markdown_plugins(&project);
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    {
        let mut h = launch(&project, &dir_context);
        h.open_file(&project.join("a.md")).unwrap();
        show_sidebar(&mut h);
        wait_for(&mut h, "the outline", |h| screen_has(h, "Alpha Doc"));
        h.shutdown(true).unwrap();
    }

    // Relaunch with the plugin gone: nothing will mount the section again.
    // The harness mirrors the project's plugins into the shared config dir
    // and the mirror only ever adds, so the copy there goes too.
    fs::remove_file(project.join("plugins").join("markdown_toc.ts")).unwrap();
    let mirrored = dir_context
        .config_dir
        .join("plugins")
        .join("markdown_toc.ts");
    if mirrored.exists() {
        fs::remove_file(&mirrored).unwrap();
    }
    let mut h = launch(&project, &dir_context);
    let restored = h
        .editor_mut()
        .restore_active_window_on_launch(false)
        .unwrap();
    assert!(restored, "the saved workspace restores");
    h.wait_for_file_explorer().unwrap();
    // Give plugin load and a few frames time to settle.
    for _ in 0..10 {
        h.render().unwrap();
    }
    assert!(
        !screen_has(&h, "Panel unavailable"),
        "a placeholder outlived plugin load\n{}",
        h.screen_to_string()
    );
    assert!(
        !screen_has(&h, "Contents"),
        "the dead section is still in the column\n{}",
        h.screen_to_string()
    );
}
