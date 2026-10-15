//! E2E tests for markdown_compose's view modes: Source, Rendered, and the
//! Source/Compose dual mode (the source beside its composed view, scrolling
//! together).
//!
//! Every assertion reads the screen (CONTRIBUTING.md Testing §2). The fixture
//! has markers that tell the two views apart:
//!
//! - `PARA` is on screen once per pane showing the top of the document, so
//!   counting it counts panes;
//! - `**boldword**` keeps its asterisks only in source view;
//! - the table gets a drawn frame (`┌`) only in rendered view;
//! - `LONGSTART … LONGEND` is one line wider than any pane, so `LONGEND` is on
//!   screen only where the line wraps, and `LONGSTART`'s column shows whether
//!   the pane draws its text at the left edge or centred on a page.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

const WIDTH: u16 = 200;
const HEIGHT: u16 = 40;

fn fixture() -> String {
    let mut md = String::from("# View Modes\n\n");
    md.push_str("Intro with **boldword** here. PARA\n\n");
    md.push_str("| Name | Qty |\n|------|-----|\n| pear | 2 |\n\n");
    md.push_str(&format!("LONGSTART {}LONGEND\n\n", "word ".repeat(50)));
    for i in 1..=150 {
        md.push_str(&format!("line {i:03} **b**\n\n"));
    }
    md
}

fn setup() -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().join("project");
    let plugins_dir = root.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md = root.join("doc.md");
    std::fs::write(&md, fixture()).unwrap();

    let mut config = Config::default();
    config.editor.line_wrap = true;
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(WIDTH, HEIGHT, config, root).unwrap();
    harness.open_file(&md).unwrap();
    harness.wait_for_screen_contains("PARA").unwrap();
    (harness, temp)
}

/// Run a command from the palette. Each test then waits for the screen it
/// expects (CONTRIBUTING.md Testing §3): the plugin redraws over several
/// frames, so no fixed number of frames or a quiet screen says it is done.
/// A screen that never comes fails the wait.
fn run(h: &mut EditorTestHarness, command: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

const SOURCE: &str = "Markdown: Source Mode";
const RENDERED: &str = "Markdown: Rendered Mode";
const DUAL: &str = "Markdown: Source/Compose Dual Mode";
const CYCLE: &str = "Markdown: Cycle View Mode";
const TOGGLE_DUAL: &str = "Markdown: Toggle Source/Compose Dual Mode";

fn count(h: &EditorTestHarness, text: &str) -> usize {
    h.screen_to_string().matches(text).count()
}

/// The screen's rows cut into the left and right halves, leaving out a few
/// columns around the middle where a two-pane layout draws its divider.
fn halves(h: &EditorTestHarness) -> (String, String) {
    let mid = WIDTH as usize / 2;
    let (mut left, mut right) = (String::new(), String::new());
    for row in 0..HEIGHT {
        let chars: Vec<char> = h.screen_row_text(row).chars().collect();
        left.extend(chars.iter().take(mid - 3));
        left.push('\n');
        right.extend(chars.iter().skip(mid + 3));
        right.push('\n');
    }
    (left, right)
}

/// The column `text` starts at in the left half of the screen.
fn left_column_of(h: &EditorTestHarness, text: &str) -> Option<usize> {
    let (left, _) = halves(h);
    left.lines()
        .find_map(|row| row.find(text).map(|byte| row[..byte].chars().count()))
}

/// `panes` panes, every one of them showing raw source.
fn source_panes(h: &EditorTestHarness, panes: usize) -> bool {
    let screen = h.screen_to_string();
    count(h, "PARA") == panes
        && screen.matches("**boldword**").count() == panes
        && !screen.contains('┌')
}

/// One pane, rendered.
fn rendered(h: &EditorTestHarness) -> bool {
    let screen = h.screen_to_string();
    count(h, "PARA") == 1 && !screen.contains("**boldword**") && screen.contains('┌')
}

/// Dual mode: raw source with its line numbers on the left, the rendered preview
/// with no gutter on the right.
fn dual(h: &EditorTestHarness) -> bool {
    if count(h, "PARA") != 2 {
        return false;
    }
    let (left, right) = halves(h);
    // What a pane draws left of `Intro` (line 3).
    let gutter = |pane: &str| -> Option<String> {
        let row = pane.lines().find(|row| row.contains("Intro"))?;
        Some(row[..row.find("Intro")?].trim().to_string())
    };
    left.contains("**boldword**")
        && left.contains("|------|")
        && !left.contains('┌')
        && right.contains("boldword")
        && !right.contains("**boldword**")
        && right.contains('┌')
        && gutter(&left).is_some_and(|g| g.contains('3'))
        && gutter(&right).is_some_and(|g| g.is_empty())
}

/// Cycle goes Source → Rendered → dual mode → Source, in the one pane it started in.
#[test]
fn cycle_goes_through_rendered_and_dual_back_to_source() {
    let (mut h, _tmp) = setup();
    h.wait_until(|h| source_panes(h, 1)).unwrap();

    run(&mut h, CYCLE);
    h.wait_until(rendered).unwrap();

    run(&mut h, CYCLE);
    h.wait_until(dual).unwrap();

    run(&mut h, CYCLE);
    h.wait_until(|h| source_panes(h, 1)).unwrap();
}

/// Toggle Dual Mode opens the preview beside the source, in a tab of its own
/// next to the file's, and closes it again.
#[test]
fn toggle_dual_opens_and_closes_the_preview() {
    let (mut h, _tmp) = setup();

    run(&mut h, TOGGLE_DUAL);
    h.wait_until(|h| {
        let tabs = h.screen_row_text(1);
        dual(h) && tabs.contains("doc.md (Source/Compose)") && tabs.matches("doc.md").count() == 2
    })
    .unwrap();

    run(&mut h, TOGGLE_DUAL);
    h.wait_until(|h| source_panes(h, 1)).unwrap();
}

/// Dual mode adds one pane, and leaving it closes only that one: panes the user
/// opened on the file stay open.
#[test]
fn dual_leaves_the_users_own_panes_open() {
    let (mut h, _tmp) = setup();
    run(&mut h, "Split Vertical");
    h.wait_until(|h| source_panes(h, 2)).unwrap();

    // Dual mode beside the user's two panes: three panes, one of them rendered.
    run(&mut h, DUAL);
    h.wait_until(|h| count(h, "PARA") == 3 && h.screen_to_string().contains('┌'))
        .unwrap();

    run(&mut h, SOURCE);
    h.wait_until(|h| source_panes(h, 2)).unwrap();
}

/// Dual mode leaves the source pane's line wrap as the user set it, during dual mode and
/// after it.
#[test]
fn dual_keeps_the_source_panes_line_wrap() {
    let (mut h, _tmp) = setup();
    h.wait_until(|h| source_panes(h, 1) && h.screen_to_string().contains("LONGEND"))
        .unwrap();

    run(&mut h, DUAL);
    h.wait_until(|h| dual(h) && halves(h).0.contains("LONGEND"))
        .unwrap();

    run(&mut h, SOURCE);
    h.wait_until(|h| source_panes(h, 1) && h.screen_to_string().contains("LONGEND"))
        .unwrap();
}

/// The source pane in dual mode, and the pane left after it, draw text from the
/// left edge, not centred on a page, even when dual mode started from Rendered,
/// which had centred that pane.
#[test]
fn source_pane_is_full_width_in_and_after_dual_mode() {
    let (mut h, _tmp) = setup();
    h.wait_until(|h| source_panes(h, 1)).unwrap();
    let edge = left_column_of(&h, "LONGSTART").expect("LONGSTART on screen");

    run(&mut h, RENDERED);
    h.wait_until(rendered).unwrap();

    run(&mut h, DUAL);
    h.wait_until(|h| dual(h) && left_column_of(h, "LONGSTART") == Some(edge))
        .unwrap();

    run(&mut h, SOURCE);
    h.wait_until(|h| source_panes(h, 1) && left_column_of(h, "LONGSTART") == Some(edge))
        .unwrap();
}

/// Scrolling the source pane in dual mode scrolls the preview with it: the line at
/// the top of the source pane is on screen in the preview too, rendered (no
/// `**` left in it).
#[test]
fn dual_preview_scrolls_with_the_source() {
    let (mut h, _tmp) = setup();
    run(&mut h, DUAL);
    h.wait_until(dual).unwrap();

    for _ in 0..4 {
        h.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| {
        let (left, right) = halves(h);
        let first_line = left.lines().find_map(|row| {
            let at = row.find("line ")?;
            row.get(at..at + 8)
        });
        count(h, "PARA") == 0
            && first_line.is_some_and(|line| right.contains(line))
            && !right.contains("**")
    })
    .unwrap();
}

/// Closing the dual view's tab by hand ends dual mode: the file is back in its pane, and the
/// next Toggle Dual Mode opens dual mode again rather than trying to leave it.
#[test]
fn closing_the_dual_tab_ends_dual_mode() {
    let (mut h, _tmp) = setup();
    run(&mut h, DUAL);
    h.wait_until(dual).unwrap();

    run(&mut h, "Close Tab");
    h.wait_until(|h| source_panes(h, 1)).unwrap();

    run(&mut h, TOGGLE_DUAL);
    h.wait_until(dual).unwrap();
}

/// the dual view's source panel edits the file, and the preview shows the edit.
#[test]
fn typing_in_the_dual_source_panel_updates_the_preview() {
    let (mut h, _tmp) = setup();
    run(&mut h, DUAL);
    h.wait_until(dual).unwrap();

    h.type_text("EDITED ").unwrap();
    h.wait_until(|h| {
        let (left, right) = halves(h);
        left.contains("EDITED # View Modes") && right.contains("EDITED")
    })
    .unwrap();
}

/// A mode command acts on the pane in focus, even when another pane has the
/// same file in dual mode: Rendered in one pane leaves the other's dual mode open.
#[test]
fn mode_commands_leave_another_panes_dual_mode_alone() {
    let (mut h, _tmp) = setup();
    run(&mut h, "Split Vertical");
    h.wait_until(|h| source_panes(h, 2)).unwrap();

    // Dual mode in the new (right) pane, then back to the left one.
    run(&mut h, DUAL);
    h.wait_until(|h| {
        count(h, "PARA") == 3 && h.screen_row_text(1).contains("doc.md (Source/Compose)")
    })
    .unwrap();
    run(&mut h, "Previous Split");

    run(&mut h, RENDERED);
    h.wait_until(|h| {
        let (left, _) = halves(h);
        count(h, "PARA") == 3
            && !left.contains("**boldword**")
            && h.screen_row_text(1).contains("doc.md (Source/Compose)")
    })
    .unwrap();
}

/// Composing the dual view's source panel by hand lays that panel out as a
/// page: its own width, centred. The page width used to land on the pane
/// holding the dual view instead, leaving the panel edge to edge.
#[test]
fn compose_in_a_dual_panel_lays_out_that_panel() {
    let (mut h, _tmp) = setup();
    h.wait_until(|h| source_panes(h, 1)).unwrap();
    let edge = left_column_of(&h, "LONGSTART").expect("LONGSTART on screen");

    run(&mut h, DUAL);
    h.wait_until(dual).unwrap();
    run(&mut h, "Markdown: Toggle Compose");
    // The left panel is about 100 columns and the page 80: centred, the text
    // starts well in from the panel's edge.
    h.wait_until(|h| {
        !halves(h).0.contains("**boldword**")
            && left_column_of(h, "LONGSTART").is_some_and(|c| c >= 5)
    })
    .unwrap();

    run(&mut h, SOURCE);
    h.wait_until(|h| source_panes(h, 1) && left_column_of(h, "LONGSTART") == Some(edge))
        .unwrap();
}
