//! Sidebar sections are scoped to what the user is looking at
//! (sinelaw/fresh#3326, D / D′ / F; `docs/internal/plugin-window-scope-plan.md`).
//!
//! Written before the fix, to pin the defects:
//!
//! * **D** — a Markdown outline mounted while window A was active must not
//!   show in window B, whose active buffer is not Markdown; it comes back on
//!   the return to A.
//! * **D′** — with a different Markdown file in each window, each window
//!   shows its own outline, and activating a row never opens a file in the
//!   other window — before the fix it opened a *second, independent buffer*
//!   of the other window's file in this one.
//! * **F** — a section restored from a workspace whose plugin never mounts
//!   it again is a placeholder only until plugins have loaded; after that it
//!   is gone.
//!
//! Written before the fix and `#[ignore]`d until Phase 3 landed; they run
//! against the fix now.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};

const DOC_A: &str = "# Alpha Doc\n\n## First\n\ntext\n\n## Second\n\nmore\n";
const DOC_B: &str = "# Beta Doc\n\n## Uno\n\ntext\n\n## Dos\n\nmore\n";

fn install_markdown_plugins(project: &Path) {
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_toc");
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin(&plugins_dir, "markdown_source");
    copy_plugin_lib(&plugins_dir);
}

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
    if !h.screen_to_string().contains("File Explorer") {
        h.send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
            .unwrap();
        h.wait_for_file_explorer().unwrap();
        h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
}

fn screen_has(h: &EditorTestHarness, needle: &str) -> bool {
    h.screen_to_string().contains(needle)
}

/// The rows of the `Contents` section: everything between its header and
/// the column's bottom border, trimmed, non-empty.
fn contents_rows(h: &EditorTestHarness) -> Vec<String> {
    let s = h.screen_to_string();
    let mut rows = Vec::new();
    let mut inside = false;
    for line in s.lines() {
        if line.contains("Contents") && (line.contains("▼") || line.contains("▶")) {
            inside = true;
            continue;
        }
        if inside {
            if line.starts_with('└') || line.starts_with('├') {
                break;
            }
            let cell = line.trim_start_matches('│');
            let text = cell
                .split('│')
                .next()
                .unwrap_or("")
                .trim()
                .trim_start_matches('▌')
                .trim()
                .to_string();
            if !text.is_empty() {
                rows.push(text);
            }
        }
    }
    rows
}

fn run_palette_command(h: &mut EditorTestHarness, command: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.wait_until(|h| h.screen_to_string().contains(command))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

/// A project with `a.md`, a second root with `b.md`, the plugins installed.
fn two_roots() -> (tempfile::TempDir, PathBuf, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.md"), DOC_A).unwrap();
    fs::write(project.join("plain.txt"), "hello\n").unwrap();
    let other = temp_dir.path().join("other");
    fs::create_dir(&other).unwrap();
    fs::write(other.join("b.md"), DOC_B).unwrap();
    install_markdown_plugins(&project);
    (temp_dir, project, other)
}

/// **D.** The outline follows the window: window B, whose active buffer is
/// not Markdown, shows no `Contents`; back in A it is there again.
#[test]
fn an_outline_mounted_in_one_window_does_not_show_in_another() {
    init_tracing_from_env();
    let (temp_dir, project, other) = two_roots();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let mut h = launch(&project, &dir_context);

    h.open_file(&project.join("a.md")).unwrap();
    show_sidebar(&mut h);
    h.wait_until(|h| screen_has(h, "Contents") && screen_has(h, "Alpha Doc"))
        .unwrap();

    // Window B: a plain-text buffer, sidebar shown.
    let b = h
        .editor_mut()
        .create_window_at(other.clone(), "other".to_string());
    h.editor_mut().set_active_window(b);
    h.wait_until(|h| !screen_has(h, "Alpha Doc") || !screen_has(h, "a.md"))
        .unwrap();
    h.open_file(&other.join("b.md")).unwrap();
    h.open_file(&project.join("plain.txt")).unwrap();
    show_sidebar(&mut h);
    h.wait_for_file_explorer().unwrap();
    h.render().unwrap();
    h.render().unwrap();
    assert!(
        !screen_has(&h, "Alpha Doc"),
        "window B shows window A's outline\n{}",
        h.screen_to_string()
    );

    // Back to A: the outline is there again.
    h.editor_mut().set_active_window(fresh_core::WindowId(1));
    h.wait_until(|h| screen_has(h, "Alpha Doc"))
        .unwrap_or_else(|e| {
            panic!(
                "A's outline after the return: {e}\n{}",
                h.screen_to_string()
            )
        });
}

/// **D′.** Each window shows its own outline, and a row activated in window B
/// jumps inside B's own document — it never opens A's file in B.
#[test]
fn each_window_shows_its_own_outline_and_a_row_never_opens_the_other_windows_file() {
    init_tracing_from_env();
    let (temp_dir, project, other) = two_roots();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let mut h = launch(&project, &dir_context);

    h.open_file(&project.join("a.md")).unwrap();
    show_sidebar(&mut h);
    h.wait_until(|h| screen_has(h, "Alpha Doc")).unwrap();

    let b = h
        .editor_mut()
        .create_window_at(other.clone(), "other".to_string());
    h.editor_mut().set_active_window(b);
    h.open_file(&other.join("b.md")).unwrap();
    show_sidebar(&mut h);
    h.wait_until(|h| contents_rows(h).iter().any(|r| r == "Beta Doc"))
        .unwrap_or_else(|e| panic!("B's outline: {e}\n{}", h.screen_to_string()));
    assert!(
        !contents_rows(&h).iter().any(|r| r == "Alpha Doc"),
        "window B lists window A's headings\n{}",
        h.screen_to_string()
    );

    // Activate a row: Focus Next Sidebar Section twice (explorer, then the
    // section), Down onto "Uno", Enter.
    run_palette_command(&mut h, "Focus Next Sidebar Section");
    run_palette_command(&mut h, "Focus Next Sidebar Section");
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The activation goes to the plugin and comes back as a jump: wait for
    // the cursor to land on "## Uno" (line 3) so the check below is not
    // made before anything could have opened.
    h.wait_until(|h| h.screen_to_string().contains("Ln 3,"))
        .unwrap_or_else(|e| panic!("the jump to Uno: {e}\n{}", h.screen_to_string()));
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
    h.wait_until(|h| contents_rows(h).iter().any(|r| r == "Alpha Doc"))
        .unwrap_or_else(|e| {
            panic!(
                "A's outline after the return: {e}\n{}",
                h.screen_to_string()
            )
        });
    assert!(!contents_rows(&h).iter().any(|r| r == "Beta Doc"));
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
        h.wait_until(|h| screen_has(h, "Alpha Doc")).unwrap();
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
