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

// ---------------------------------------------------------------------------
// Scroll sync between the dual view's panes
// ---------------------------------------------------------------------------
//
// A short document with the blocks the preview draws taller than the source
// (a spaced list, a framed table), in a small terminal, as reported: the
// preview drifted away from the source below the top line, moved unevenly,
// stepped backwards while the source went forwards, and ignored the wheel over
// the pane without the focus.

const SYNC_WIDTH: u16 = 80;
const SYNC_HEIGHT: u16 = 30;

fn trip_doc() -> String {
    let mut md = String::from("# Trip Plan\n\nIntro paragraph about the trip.\n\n");
    md.push_str("## Packing\n\n- socks\n- shirts\n- charger\n- passport\n\n");
    md.push_str("## Budget\n\n| Item  | Cost |\n|-------|------|\n");
    md.push_str("| Food  | 100  |\n| Hotel | 300  |\n| Train | 80   |\n\n");
    md.push_str("## Day one\n\nArrive in the morning.\n\n```sh\necho hello\nls -la\n```\n\n");
    md.push_str("> Remember the tickets.\n> And the umbrella.\n\n");
    for (day, plan) in [
        ("two", "Walk around town."),
        ("three", "Museum visit."),
        ("four", "Hike up the hill."),
        ("five", "Head home."),
    ] {
        md.push_str(&format!("## Day {day}\n\n{plan}\n\n"));
    }
    md.push_str("## Notes\n\nThe end.\n");
    md
}

/// The trip document in dual mode, source pane focused, at the top.
fn dual_setup() -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().join("project");
    let plugins_dir = root.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md = root.join("trip.md");
    std::fs::write(&md, trip_doc()).unwrap();
    let mut h = EditorTestHarness::with_config_and_working_dir(
        SYNC_WIDTH,
        SYNC_HEIGHT,
        Config::default(),
        root,
    )
    .unwrap();
    h.open_file(&md).unwrap();
    h.wait_for_screen_contains("Trip Plan").unwrap();
    run(&mut h, DUAL);
    h.wait_until(|h| {
        let rows = pane_rows(h);
        rows.first().is_some_and(|r| r.source_line == Some(1))
            && rows.iter().any(|r| r.preview.contains('┌'))
    })
    .unwrap();
    (h, temp)
}

/// One screen row of the dual view: the source pane's line number and text,
/// and the preview's text.
struct PaneRow {
    source_line: Option<usize>,
    source: String,
    preview: String,
}

/// The dual view's content rows, split at the divider between the panes: the
/// column holding `│` on every row of the source pane's gutter's right.
fn pane_rows(h: &EditorTestHarness) -> Vec<PaneRow> {
    // Padded back to the full width: the screen's rows come trimmed.
    let rows: Vec<Vec<char>> = (0..SYNC_HEIGHT)
        .map(|r| {
            let mut row: Vec<char> = h.screen_row_text(r).chars().collect();
            row.resize(SYNC_WIDTH as usize, ' ');
            row
        })
        .collect();
    // Content rows carry the gutter: a line number (or, past the end of the
    // file, spaces), then `│`, in the first few columns.
    let gutter = |row: &[char]| -> Option<usize> {
        let at = row.iter().take(8).position(|c| *c == '│')?;
        row[..at]
            .iter()
            .all(|c| c.is_ascii_digit() || *c == ' ')
            .then_some(at)
    };
    let content: Vec<&Vec<char>> = rows.iter().filter(|r| gutter(r).is_some()).collect();
    let Some(divider) = (SYNC_WIDTH as usize / 3..SYNC_WIDTH as usize * 2 / 3)
        .find(|&c| !content.is_empty() && content.iter().all(|r| r.get(c) == Some(&'│')))
    else {
        return Vec::new();
    };
    content
        .iter()
        .map(|row| {
            let g = gutter(row).unwrap();
            let number: String = row[..g].iter().collect();
            PaneRow {
                source_line: number.trim().parse().ok(),
                // Up to the source pane's scrollbar column, just left of the divider.
                source: row[g + 1..divider - 1].iter().collect(),
                preview: row[divider + 1..row.len() - 1].iter().collect(),
            }
        })
        .collect()
}

/// A row's words, with the markup the two panes draw differently left out:
/// `- socks` and `• socks`, `| Food | 100 |` and `│ Food │ 100 │`.
fn words(text: &str) -> String {
    text.chars()
        .filter(|c| c.is_alphanumeric() || *c == ' ')
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Some line shows in both panes on the same row, give or take one — the two
/// are lined up there. One row of slack: a quote's lines joined into one
/// wrapped paragraph in the preview take a row less than they count for.
fn lined_up(rows: &[PaneRow]) -> bool {
    rows.iter().enumerate().any(|(i, r)| {
        let text = words(&r.source);
        !text.is_empty()
            && (i.saturating_sub(1)..=i + 1)
                .filter_map(|j| rows.get(j))
                .any(|other| words(&other.preview) == text)
    })
}

/// The line at the top of the source pane.
fn source_top(rows: &[PaneRow]) -> Option<usize> {
    rows.first().and_then(|r| r.source_line)
}

/// The screen row of each heading the preview shows, by heading.
fn preview_headings(rows: &[PaneRow]) -> Vec<(String, usize)> {
    const HEADINGS: [&str; 8] = [
        "Packing",
        "Budget",
        "Day one",
        "Day two",
        "Day three",
        "Day four",
        "Day five",
        "Notes",
    ];
    HEADINGS
        .iter()
        .filter_map(|t| {
            let row = rows
                .iter()
                .position(|r| r.preview.trim() == *t || r.preview.trim() == format!("## {t}"))?;
            Some((t.to_string(), row))
        })
        .collect()
}

/// Whether the preview moved its content up — scrolled down — from `before`
/// to `after`, as the headings both show say. `None` when they share none.
fn preview_moved(
    before: &[(String, usize)],
    after: &[(String, usize)],
) -> Option<std::cmp::Ordering> {
    before.iter().find_map(|(t, b)| {
        let a = after.iter().find(|(u, _)| u == t)?.1;
        Some(b.cmp(&a))
    })
}

/// Scrolling the source pane down a line at a time: the source moves one line
/// per press, the preview follows it down — never back up — and the two stay
/// lined up on some row all the way, ending together at the end of the document.
///
/// Before the fix the preview drifted 4–10 rows from the source (no row lined
/// up), the source itself stalled on rows only the preview draws (its scroll
/// counted the preview's table borders and list spacers), and once stepped
/// back a line while Ctrl+Down was pressed.
#[test]
fn dual_preview_follows_the_source_scrolled_line_by_line() {
    let (mut h, _tmp) = dual_setup();
    let mut headings = preview_headings(&pane_rows(&h));
    // The source scrolls until its last page: 51 rows in a 26-row window.
    for line in 2..=26 {
        h.send_key(KeyCode::Down, KeyModifiers::CONTROL).unwrap();
        h.wait_until(|h| {
            let rows = pane_rows(h);
            source_top(&rows) == Some(line) && lined_up(&rows)
        })
        .unwrap();
        let now = preview_headings(&pane_rows(&h));
        assert_ne!(
            preview_moved(&headings, &now),
            Some(std::cmp::Ordering::Less),
            "the preview scrolled back up while the source went down to line {line}"
        );
        headings = now;
    }
    h.wait_until(|h| {
        let rows = pane_rows(h);
        rows.iter().any(|r| r.source.contains("The end."))
            && rows.iter().any(|r| r.preview.contains("The end."))
    })
    .unwrap();

    // And back up, to the top of both.
    for line in (1..=25).rev() {
        h.send_key(KeyCode::Up, KeyModifiers::CONTROL).unwrap();
        h.wait_until(|h| {
            let rows = pane_rows(h);
            source_top(&rows) == Some(line) && lined_up(&rows)
        })
        .unwrap();
        let now = preview_headings(&pane_rows(&h));
        assert_ne!(
            preview_moved(&headings, &now),
            Some(std::cmp::Ordering::Greater),
            "the preview scrolled down while the source went up to line {line}"
        );
        headings = now;
    }
    h.wait_until(|h| {
        let rows = pane_rows(h);
        rows.first()
            .is_some_and(|r| r.preview.contains("Trip Plan"))
    })
    .unwrap();
}

/// Scrolling from the preview pane: the preview moves a row per press, the
/// source follows it — never back — lined up with it, to the end of both.
#[test]
fn dual_source_follows_the_preview_scrolled_line_by_line() {
    let (mut h, _tmp) = dual_setup();
    // Focus the preview: a click on its "Intro" row.
    let intro = pane_rows(&h)
        .iter()
        .position(|r| r.preview.contains("Intro paragraph"))
        .expect("intro on screen") as u16;
    h.mouse_click(SYNC_WIDTH * 3 / 4, 2 + intro).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Ln 3"))
        .unwrap();

    let mut source = source_top(&pane_rows(&h)).unwrap();
    let mut headings = preview_headings(&pane_rows(&h));
    let mut steps = 0;
    // Until the source shows the end too: the preview, taller, shows its last
    // line a few rows before its last page, which is where the source's is.
    while !pane_rows(&h).iter().any(|r| r.source.contains("The end.")) {
        steps += 1;
        assert!(
            steps <= 40,
            "the preview never reached the end of the document"
        );
        let before = headings.clone();
        h.send_key(KeyCode::Down, KeyModifiers::CONTROL).unwrap();
        h.wait_until(|h| {
            let rows = pane_rows(h);
            let now = preview_headings(&rows);
            // The preview moved, or its headings scrolled out of view.
            preview_moved(&before, &now) != Some(std::cmp::Ordering::Equal) && lined_up(&rows)
        })
        .unwrap();
        let rows = pane_rows(&h);
        let now_source = source_top(&rows).unwrap();
        assert!(
            now_source >= source,
            "the source went back from line {source} to {now_source} while the preview went down"
        );
        source = now_source;
        headings = preview_headings(&rows);
    }
    assert!(
        pane_rows(&h).iter().any(|r| r.preview.contains("The end.")),
        "the source reached the end before the preview"
    );
}

/// The wheel scrolls the pane under the pointer, focused or not, and the other
/// pane follows. Before the fix, the wheel over the pane without the focus did
/// nothing: the sync, run from the focused pane, put it straight back.
#[test]
fn dual_wheel_over_either_pane_scrolls_both() {
    let (mut h, _tmp) = dual_setup();
    let middle = SYNC_HEIGHT / 2;

    // The source has the focus; the wheel over the preview.
    for _ in 0..3 {
        h.mouse_scroll_down(SYNC_WIDTH * 3 / 4, middle).unwrap();
    }
    h.wait_until(|h| {
        let rows = pane_rows(h);
        source_top(&rows).is_some_and(|l| l > 1)
            && !rows.iter().any(|r| r.preview.contains("Trip Plan"))
            && lined_up(&rows)
    })
    .unwrap();

    // Focus the preview; the wheel over the source, back to the top.
    h.mouse_click(SYNC_WIDTH * 3 / 4, middle).unwrap();
    for _ in 0..6 {
        h.mouse_scroll_up(SYNC_WIDTH / 4, middle).unwrap();
    }
    h.wait_until(|h| {
        let rows = pane_rows(h);
        source_top(&rows) == Some(1)
            && rows
                .first()
                .is_some_and(|r| r.preview.contains("Trip Plan"))
    })
    .unwrap();

    // And down again over the source, which the preview follows.
    for _ in 0..3 {
        h.mouse_scroll_down(SYNC_WIDTH / 4, middle).unwrap();
    }
    h.wait_until(|h| {
        let rows = pane_rows(h);
        source_top(&rows).is_some_and(|l| l > 1)
            && !rows.iter().any(|r| r.preview.contains("Trip Plan"))
            && lined_up(&rows)
    })
    .unwrap();
}

/// Typing in the preview pane edits the file: the source pane shows it.
#[test]
fn typing_in_the_dual_preview_edits_the_source() {
    let (mut h, _tmp) = dual_setup();
    let intro = pane_rows(&h)
        .iter()
        .position(|r| r.preview.contains("Intro paragraph"))
        .expect("intro on screen") as u16;
    let row = h.screen_row_text(2 + intro);
    let column = row[..row.rfind("Intro").unwrap()].chars().count() as u16;
    h.mouse_click(column, 2 + intro).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Ln 3"))
        .unwrap();
    h.type_text("EDITED ").unwrap();
    h.wait_until(|h| {
        let rows = pane_rows(h);
        rows.iter().any(|r| r.source.contains("EDITED Intro"))
            && rows.iter().any(|r| r.preview.contains("EDITED Intro"))
    })
    .unwrap();
}
