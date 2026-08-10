//! Golden render snapshot of the TUI menu bar + dropdowns. This is the safety
//! net for the Phase-3 unification (making `MenuRenderer` derive its content
//! from the shared `Editor::menu_view()` projection): the TUI menu rendering
//! must stay byte-identical across that refactor.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The menu bar row (0) plus the dropdown box rows (from the first `┌` to the
/// last `└`), right-trimmed. Captures exactly the menu rendering while ignoring
/// buffer / status-bar content.
fn menu_region(h: &EditorTestHarness) -> Vec<String> {
    let height = h.terminal_height() as u16;
    let mut rows = vec![h.get_row_text(0).trim_end().to_string()];
    let (mut top, mut bot) = (None, None);
    for r in 0..height {
        let t = h.get_row_text(r);
        if t.contains('┌') && top.is_none() {
            top = Some(r);
        }
        if t.contains('└') {
            bot = Some(r);
        }
    }
    if let (Some(t), Some(b)) = (top, bot) {
        for r in t..=b {
            rows.push(h.get_row_text(r).trim_end().to_string());
        }
    }
    rows
}

const FILE_MENU_GOLDEN: &[&str] = &[
    " File   Edit   View   Selection   Go   LSP   Help",
    "┌──────────────────────────────────────┐",
    "│ New File                       Ctrl+N│",
    "│ Open File...                   Ctrl+O│",
    "│ ─────────────────────────────────────│",
    "│ Save                           Ctrl+S│",
    "│ Save As...                           │",
    "│ Save All                             │",
    "│ Revert                               │",
    "│ Reload with Encoding...              │",
    "│ ─────────────────────────────────────│",
    "│ Close Buffer                         │",
    "│ ─────────────────────────────────────│",
    "│ Switch Project...                    │",
    "│ ─────────────────────────────────────│",
    "│ Detach                               │",
    "│ Quit                           Ctrl+Q│",
    "└──────────────────────────────────────┘",
];

#[test]
fn menu_render_golden_file_menu() {
    let mut h = EditorTestHarness::with_temp_project_no_plugins(100, 40).unwrap();
    h.send_key(KeyCode::Char('f'), KeyModifiers::ALT).unwrap();
    // Opening the menu can take an extra tick to paint the dropdown box, so a
    // single render() may capture the bare menu bar (no box) and flake. Wait
    // semantically for the dropdown to be fully painted (its bottom border
    // present) and for the render to settle before snapshotting.
    h.wait_until_stable(|h| {
        let rows = menu_region(h);
        rows.last().is_some_and(|r| r.contains('└'))
    })
    .unwrap();
    let rows = menu_region(&h);
    if rows != FILE_MENU_GOLDEN {
        eprintln!("---- ACTUAL FILE MENU ----");
        for r in &rows {
            eprintln!("{r}");
        }
        panic!("File menu rendering changed — see actual above vs FILE_MENU_GOLDEN");
    }
}

/// View menu: opened by navigating File -> Right -> Right. Exercises the
/// checkmark and submenu-arrow rendering paths. We assert on stable substrings
/// (checkmark glyph + the Terminal submenu) rather than an exact box, since the
/// View menu's content (themes/locales) varies by environment.
#[test]
fn menu_render_golden_view_menu() {
    let mut h = EditorTestHarness::with_temp_project_no_plugins(100, 40).unwrap();
    h.send_key(KeyCode::Char('f'), KeyModifiers::ALT).unwrap();
    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    // Opening + navigating menus can take an extra tick to repaint the dropdown,
    // so a single render() may capture a half-painted View menu and flake. Wait
    // semantically for the View dropdown content to render (and settle) first.
    h.wait_until_stable(|h| {
        let rows = menu_region(h);
        rows.iter().any(|r| r.contains("Line Numbers"))
            && rows.last().is_some_and(|r| r.contains('└'))
    })
    .unwrap();
    let rows = menu_region(&h);
    let joined = rows.join("\n");
    eprintln!("---- VIEW MENU REGION ----\n{joined}\n---- END ----");
    assert!(
        rows.iter().any(|r| r.contains("Line Numbers")),
        "View menu should list 'Line Numbers'"
    );
    assert!(
        rows.iter().any(|r| r.contains("Terminal")),
        "View menu should list the 'Terminal' submenu"
    );
    // Checkbox items render ☑ (checked) / ☐ (unchecked); at least one View item
    // is checked by default (line numbers / wrap / vertical scrollbar).
    assert!(
        joined.contains('☑'),
        "View menu should show a checked (☑) item"
    );
    assert!(
        joined.contains('☐'),
        "View menu should show an unchecked (☐) item"
    );
    // Submenus render a trailing arrow.
    assert!(
        rows.iter()
            .any(|r| r.contains("Terminal") && r.contains('>')),
        "Terminal submenu should render a '>' arrow"
    );
}
