//! A plugin panel as a section of the sidebar column
//! (`docs/internal/sidebar-sections-design.md`).
//!
//! Drives only keys and the mouse and asserts on rendered output, per
//! CONTRIBUTING.md §2. One test, four claims in order, because each state
//! is the next one's precondition:
//!
//! * `mountSidebarSection` puts a header row under the explorer that is
//!   the *shared* border row of §3.4 — `├ ▼ Outline ────×─┤`, no `└┘` above
//!   it — with the panel's rows below;
//! * dragging that header moves the divider, and the rows go with it;
//! * a click on the header collapses the section to its header row and
//!   the explorer takes the rows it gave up;
//! * the section survives a restart: restored before its plugin mounts
//!   it, it is a collapsed header over a placeholder, and the plugin's
//!   next mount adopts it in place.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::Path;

const PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_sidebar_section.ts"
));

fn install_plugin(project: &Path) {
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_sidebar_section.ts"), PLUGIN).unwrap();
}

fn launch(project: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
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

/// The screen row whose text contains `needle`.
fn row_of(h: &EditorTestHarness, needle: &str) -> Option<u16> {
    h.screen_to_string()
        .lines()
        .position(|l| l.contains(needle))
        .map(|r| r as u16)
}

fn line(h: &EditorTestHarness, row: u16) -> String {
    h.screen_to_string()
        .lines()
        .nth(row as usize)
        .unwrap_or_default()
        .to_string()
}

/// The row of the column's bottom border.
fn bottom_border_row(h: &EditorTestHarness) -> u16 {
    row_of(h, "└").expect("the sidebar's bottom border")
}

#[test]
fn a_plugin_section_mounts_drags_collapses_and_survives_a_restore() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    fs::write(project.join("a.txt"), "hello\n").unwrap();
    install_plugin(&project);
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // A column the drag can move the divider in: an inside cell of the
    // header row, clear of the chevron and the `×`.
    let x = 6u16;

    {
        let mut h = launch(&project, &dir_context);
        // A bare directory shows the explorer by default.
        h.editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        h.wait_until(|h| h.screen_to_string().contains("File Explorer"))
            .unwrap();

        run_palette_command(&mut h, "SidebarTest: Mount");
        h.wait_until(|h| h.screen_to_string().contains("alpha"))
            .unwrap();
        h.render().unwrap();

        // **The header is the shared border row.** It sits under the
        // explorer's rows, opens with `├` and closes with `×─┤`, the
        // explorer's title has grown its chevron, and the panel's first
        // row is directly beneath it.
        let explorer = row_of(&h, "▼ File Explorer").expect("the explorer's header, with chevron");
        let header = row_of(&h, "▼ Outline").expect("the section's header");
        assert!(header > explorer, "the section is under the explorer");
        let hl = line(&h, header);
        assert!(
            hl.starts_with('├') && hl.contains("×─┤"),
            "the header is the shared border row: {hl:?}"
        );
        assert!(
            !line(&h, header - 1).starts_with('└'),
            "no bottom border of the explorer above it: {:?}",
            line(&h, header - 1)
        );
        assert_eq!(
            row_of(&h, "alpha"),
            Some(header + 1),
            "the panel's first row"
        );
        assert!(
            row_of(&h, "a.txt").is_some_and(|r| r < header),
            "the tree is above the section"
        );
        assert_eq!(
            bottom_border_row(&h),
            header + 1 + 4,
            "four requested rows, then the column's bottom border\n{}",
            h.screen_to_string()
        );

        // **Drag the divider up three rows.** The explorer gives up three
        // rows and the section takes them: the header and every row of
        // the panel move up together.
        h.mouse_drag(x, header, x, header - 3).unwrap();
        h.render().unwrap();
        let moved = row_of(&h, "▼ Outline").expect("the header after the drag");
        assert_eq!(
            moved,
            header - 3,
            "the divider tracks the pointer\n{}",
            h.screen_to_string()
        );
        assert_eq!(row_of(&h, "alpha"), Some(moved + 1));
        assert_eq!(
            bottom_border_row(&h),
            header + 1 + 4,
            "the column did not change height"
        );

        // **A click on the header collapses the section.** Its body is
        // gone, its header is the row above the bottom border, and the
        // explorer took the rest of the column.
        h.mouse_click(x, moved).unwrap();
        h.render().unwrap();
        let collapsed = row_of(&h, "▶ Outline").expect("a collapsed header");
        assert!(row_of(&h, "alpha").is_none(), "the body is gone");
        assert_eq!(
            collapsed + 1,
            bottom_border_row(&h),
            "one row, directly above the bottom border\n{}",
            h.screen_to_string()
        );
        assert!(collapsed > moved, "the explorer grew into the freed rows");
        assert!(
            line(&h, collapsed - 1).starts_with('│'),
            "the explorer's walls reach down to the header: {:?}",
            line(&h, collapsed - 1)
        );

        h.shutdown(true).unwrap();
    }

    {
        // **Restored before the plugin has mounted it**, the section is a
        // collapsed header over a placeholder — it keeps its place.
        let mut h = launch(&project, &dir_context);
        let restored = h
            .editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        assert!(restored, "the saved workspace restores");
        h.wait_until(|h| h.screen_to_string().contains("▶ Outline"))
            .unwrap();
        assert!(row_of(&h, "alpha").is_none(), "nothing mounted yet");

        // The plugin's mount adopts the restored section in place: open it
        // and the panel's rows are back under the same header.
        run_palette_command(&mut h, "SidebarTest: Mount");
        h.wait_until(|h| h.screen_to_string().contains("▶ Outline"))
            .unwrap();
        let collapsed = row_of(&h, "▶ Outline").expect("still collapsed, as saved");
        h.mouse_click(x, collapsed).unwrap();
        h.wait_until(|h| h.screen_to_string().contains("alpha"))
            .unwrap();
        let header = row_of(&h, "▼ Outline").expect("re-opened");
        assert_eq!(row_of(&h, "alpha"), Some(header + 1));
        assert!(
            row_of(&h, "Panel unavailable").is_none(),
            "the placeholder gave way to the panel\n{}",
            h.screen_to_string()
        );
    }
}
