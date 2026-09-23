//! E2E tests for the settings tree-view (left panel) ↔ body-panel
//! synchronization. Covers, in both directions for every flow:
//!
//!   1. Tree keyboard nav scrolls the body to the matching section.
//!   2. Clicking a tree section row jumps the body to that section.
//!   3a. Body keyboard scroll updates the tree section highlight.
//!   3b. Body mouse-wheel scroll updates the tree section highlight.
//!   4. At every assertion point exactly ONE row in the left panel is
//!      visually highlighted (single-cursor invariant) — checked via the
//!      cell background color, not just the `>` glyph, so the test
//!      catches a regression where two rows get a highlight bg.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Editor sections (alphabetical), each described by:
/// 0: a prefix that matches the section name as it appears in the tree
///    (which truncates to the 14 cols available for section text in the
///    24-col left panel — e.g. "Bracket Matching" renders as
///    "Bracket Matchi");
/// 1: a unique item name in each section, used to verify "the body is
///    currently showing section X" by looking for X's first item.
const EDITOR_SECTIONS: &[(&str, &str)] = &[
    ("Bracket Matchi", "Highlight Matching Brackets"),
    ("Completion", "Completion Popup Auto Show"),
    ("Diagnostics", "Diagnostics Inline Text"),
    ("Display", "Animations"),
    ("Editing", "Auto Close"),
    ("Keyboard", "Keyboard Disambiguate"),
    ("LSP", "Enable Inlay Hints"),
    ("Mouse", "Double Click Time Ms"),
    ("Performance", "Large File"),
    ("Recovery", "Buffer Cleanup"),
    ("Startup", "Auto Detect Project"),
    ("Status Bar", "Show Prompt Line"),
    ("Whitespace", "Show Trailing Whitespace"),
];

/// Open Settings, navigate to the Editor category, and expand it so
/// section rows are visible in the tree.
fn open_editor_expanded(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    // Navigate from default General → Clipboard → Editor (2 Downs).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Right expands the focused expandable category (Editor has many sections).
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Bounding x range of the left-panel tree on a `width`-col terminal.
/// Modal is 90% of width, centered, with 1-col borders on each side; the
/// categories panel takes 24 cols of the inner area.
fn tree_bounds(width: u16) -> (u16, u16) {
    let modal_width = (width * 90 / 100).min(160);
    let modal_x = width.saturating_sub(modal_width) / 2;
    let tree_x = modal_x + 1; // skip modal left border
    (tree_x, tree_x + 24)
}

/// Bounding y range of the modal interior. Used to clip probes so that
/// the menu bar / status bar / outside-modal cells don't pollute bg
/// counts when looking for highlighted tree rows.
fn modal_y_range(height: u16) -> (u16, u16) {
    let modal_height = height * 90 / 100;
    let modal_y = height.saturating_sub(modal_height) / 2;
    (modal_y + 1, modal_y + modal_height - 1) // skip top + bottom borders
}

/// Find the screen row that holds the given section name *inside the
/// left-panel tree* (ignores other places the name might appear in the
/// body). Returns the (col, row) of the section name's first character.
fn find_tree_row(harness: &EditorTestHarness, name: &str, term_width: u16) -> Option<(u16, u16)> {
    let (tree_x_start, tree_x_end) = tree_bounds(term_width);
    let screen = harness.screen_to_string();
    for (row_idx, line) in screen.lines().enumerate() {
        if let Some(col) = line.find(name) {
            let col = col as u16;
            if col >= tree_x_start && col < tree_x_end {
                return Some((col, row_idx as u16));
            }
        }
    }
    None
}

/// Sample the bg color of every cell at the tree probe column and
/// return the most common one — that's the popup_bg baseline. Using
/// the *mode* avoids ambiguity around what counts as "the panel
/// background" when probing cells: at most one row can have the
/// highlight bg, so the mode of all sampled cells is necessarily the
/// non-highlight popup_bg.
fn tree_bg_mode(harness: &EditorTestHarness, term_width: u16) -> Option<ratatui::style::Color> {
    use std::collections::HashMap;
    let (tree_x_start, _) = tree_bounds(term_width);
    let probe_col = tree_x_start + 5;
    let buffer = harness.buffer();
    let (y_start, y_end) = modal_y_range(buffer.area.height);
    let mut counts: HashMap<ratatui::style::Color, usize> = HashMap::new();
    for y in y_start..y_end {
        if let Some(style) = harness.get_cell_style(probe_col, y) {
            if let Some(bg) = style.bg {
                *counts.entry(bg).or_insert(0) += 1;
            }
        }
    }
    counts.into_iter().max_by_key(|&(_, c)| c).map(|(bg, _)| bg)
}

/// y values of left-panel tree rows whose background differs from the
/// most-common bg — i.e. rows the user sees as visually highlighted.
fn highlighted_tree_rows(harness: &EditorTestHarness, term_width: u16) -> Vec<u16> {
    let Some(baseline) = tree_bg_mode(harness, term_width) else {
        return Vec::new();
    };
    let (tree_x_start, _) = tree_bounds(term_width);
    let probe_col = tree_x_start + 5;
    let buffer = harness.buffer();
    let (y_start, y_end) = modal_y_range(buffer.area.height);
    let mut rows = Vec::new();
    for y in y_start..y_end {
        if let Some(style) = harness.get_cell_style(probe_col, y) {
            if style.bg.is_some() && style.bg != Some(baseline) {
                rows.push(y);
            }
        }
    }
    rows
}

/// The single-highlight invariant: exactly one row has a non-baseline
/// bg color in the left panel. Test #4.
fn count_highlighted_tree_rows(harness: &EditorTestHarness, term_width: u16) -> usize {
    highlighted_tree_rows(harness, term_width).len()
}

/// True when the body panel is currently anchored on `section` — i.e.
/// either the section's header or its first item is on screen, AND
/// none of the items from the *next* section are visible above the
/// midline (which would mean we're past it).
///
/// This is more forgiving than a strict "first item visible" check:
/// when the user jumps to a section the scroll snaps to its top, but
/// depending on viewport height the very first item may sit at the
/// last row and get clipped to its top border. Looking for the
/// section header text (rendered in the body just above the first
/// card) catches that case.
fn body_shows_section(harness: &EditorTestHarness, section: &(&str, &str)) -> bool {
    let header = section.0;
    let item = section.1;
    let screen = harness.screen_to_string();
    screen.contains(item) || screen.contains(header)
}

/// 1. Tree keyboard Up/Down on expanded sections scrolls the body.
#[test]
fn tree_keyboard_nav_scrolls_body_both_directions() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click on a middle section (Display) so the cursor lands there.
    let middle = &EDITOR_SECTIONS[3]; // Display
    let (col, row) = find_tree_row(&harness, middle.0, 120)
        .unwrap_or_else(|| panic!("could not find '{}' in tree", middle.0));
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, middle),
        "after click, body should show '{}'. Screen:\n{}",
        middle.1,
        harness.screen_to_string()
    );

    // Down on the tree → body should show the NEXT section (Editing).
    // Section clicks keep focus in the tree, so keyboard navigation can
    // continue from the clicked section without an extra focus hop.
    let next = &EDITOR_SECTIONS[4];
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, next),
        "after tree Down, body should show next section '{}'. Screen:\n{}",
        next.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "after tree Down, exactly one tree row should be highlighted"
    );

    // Up on the tree → body should return to Display.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, middle),
        "after tree Up, body should return to '{}'. Screen:\n{}",
        middle.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "after tree Up, exactly one tree row should be highlighted"
    );

    // One more Up → previous section (Diagnostics).
    let prev = &EDITOR_SECTIONS[2];
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, prev),
        "after another tree Up, body should show '{}'. Screen:\n{}",
        prev.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "after another tree Up, exactly one tree row should be highlighted"
    );
}

/// 2. Clicking a tree section row jumps the body to that section, in
///    both directions (lower section after upper, then back upward).
#[test]
fn click_tree_section_jumps_body_both_directions() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click a section near the top first (Bracket Matching).
    let upper = &EDITOR_SECTIONS[0];
    let (col, row) = find_tree_row(&harness, upper.0, 120).expect("upper section not in tree");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, upper),
        "click upper section: body should show '{}'. Screen:\n{}",
        upper.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after clicking upper section"
    );

    // Click a section near the bottom of the visible list (LSP — index 6).
    // This walks downward in the tree.
    let lower = &EDITOR_SECTIONS[6];
    let (col, row) = find_tree_row(&harness, lower.0, 120).expect("lower section not in tree");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, lower),
        "click lower section: body should show '{}'. Screen:\n{}",
        lower.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after clicking lower section"
    );

    // Click an upper section again — covers the upward direction.
    let (col, row) = find_tree_row(&harness, upper.0, 120)
        .expect("upper section not in tree (after second click)");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();
    assert!(
        body_shows_section(&harness, upper),
        "click upper section (after lower): body should show '{}'. Screen:\n{}",
        upper.1,
        harness.screen_to_string()
    );
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after clicking back upward"
    );
}

#[test]
fn click_tree_section_keeps_left_tree_focus_marker() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    let section = &EDITOR_SECTIONS[6]; // LSP
    let (col, row) = find_tree_row(&harness, section.0, 120).expect("LSP section row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    let (tree_x_start, _) = tree_bounds(120);
    assert_eq!(
        harness.get_cell(tree_x_start, row).as_deref(),
        Some(">"),
        "mouse-clicking a section row should keep keyboard focus in the \
         left tree, matching top-level category clicks. Screen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        body_shows_section(&harness, section),
        "section click should still jump the body to the clicked section. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// 3a. Body keyboard scroll moves the left-panel section highlight in
///     both directions.
#[test]
fn body_keyboard_scroll_updates_tree_highlight_both_directions() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Land in the middle (Display).
    let start = &EDITOR_SECTIONS[3];
    let (col, row) = find_tree_row(&harness, start.0, 120).expect("Display row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    // Section clicks keep focus in the tree; move to the body before
    // testing body keyboard scroll.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press PageDown several times in the body — content scrolls forward.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Whatever later section is now showing in the body, the tree
    // should highlight a section AFTER Display (further down). Verify
    // the highlight has moved off Display and onto a row whose
    // alphabetical index is ≥ Display's index.
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after body PageDown"
    );
    let later = highlighted_section_name(&harness, 120);
    let later_idx = later
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .unwrap_or_else(|| panic!("expected highlighted section, got {later:?}"));
    assert!(
        later_idx >= 3,
        "after PageDown, highlight should be at Display or later, got {later:?}"
    );

    // PageUp back — highlight should move BACK toward Display (or before).
    for _ in 0..6 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after body PageUp"
    );
    let earlier = highlighted_section_name(&harness, 120);
    let earlier_idx = earlier
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .unwrap_or_else(|| panic!("expected highlighted section, got {earlier:?}"));
    assert!(
        earlier_idx <= 3,
        "after PageUp, highlight should be at Display or earlier, got {earlier:?}"
    );
    assert!(
        earlier_idx < later_idx,
        "PageUp should move highlight BACK from earlier PageDown position"
    );
}

/// 3b. Body mouse-wheel scroll moves the left-panel section highlight
///     in both directions.
#[test]
fn body_wheel_scroll_updates_tree_highlight_both_directions() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    let start = &EDITOR_SECTIONS[3];
    let (col, row) = find_tree_row(&harness, start.0, 120).expect("Display row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    // Find a column inside the body to scroll on.
    let body_col: u16 = 100;
    let body_row: u16 = 20;

    for _ in 0..15 {
        harness.mouse_scroll_down(body_col, body_row).unwrap();
    }
    harness.render().unwrap();
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after body wheel-down"
    );
    let later = highlighted_section_name(&harness, 120);
    let later_idx = later
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .unwrap_or_else(|| panic!("expected highlighted section, got {later:?}"));
    assert!(
        later_idx >= 3,
        "after wheel-down, highlight should be at Display or later, got {later:?}"
    );

    for _ in 0..30 {
        harness.mouse_scroll_up(body_col, body_row).unwrap();
    }
    harness.render().unwrap();
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after body wheel-up"
    );
    let earlier = highlighted_section_name(&harness, 120);
    let earlier_idx = earlier
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .unwrap_or_else(|| panic!("expected highlighted section, got {earlier:?}"));
    assert!(
        earlier_idx <= later_idx,
        "wheel-up should not move highlight forward"
    );
}

/// Returns the section name on the row whose left-panel bg differs from
/// the popup baseline — i.e. the row the user sees as highlighted.
/// Returns `None` if no row is highlighted (which itself fails the
/// per-test single-highlight assertion separately).
fn highlighted_section_name(harness: &EditorTestHarness, term_width: u16) -> Option<String> {
    let (tree_x_start, tree_x_end) = tree_bounds(term_width);
    let screen = harness.screen_to_string();
    for y in highlighted_tree_rows(harness, term_width) {
        let line = screen.lines().nth(y as usize)?;
        for (sec_name, _) in EDITOR_SECTIONS {
            if let Some(col) = line.find(sec_name) {
                let col = col as u16;
                if col >= tree_x_start && col < tree_x_end {
                    return Some((*sec_name).to_string());
                }
            }
        }
    }
    None
}

/// 4b. Single-highlight invariant under mouse hover. The previous
/// test #4 only fires the cursor-selection path; this one specifically
/// reproduces the visual bug where a hovered row + a selected row both
/// got non-baseline backgrounds (with two DIFFERENT colors:
/// `menu_hover_bg` for the hover, `menu_highlight_bg` for the cursor).
/// At any one time the user should see exactly one highlight.
#[test]
fn tree_single_highlight_under_hover_and_selection() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click an upper section to make it the cursor row.
    let cursor_section = &EDITOR_SECTIONS[1]; // Completion
    let (cursor_col, cursor_row) =
        find_tree_row(&harness, cursor_section.0, 120).expect("cursor section row");
    harness.mouse_click(cursor_col, cursor_row).unwrap();
    harness.render().unwrap();

    // Now hover the mouse over a DIFFERENT tree row (a section further
    // down in the same expanded category).
    let hover_section = &EDITOR_SECTIONS[5]; // Keyboard
    let (hover_col, hover_row) =
        find_tree_row(&harness, hover_section.0, 120).expect("hover section row");
    harness.mouse_move(hover_col, hover_row).unwrap();
    harness.render().unwrap();

    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant under hover-vs-cursor: exactly ONE \
         tree row should have a non-baseline bg, but found multiple. \
         Screen:\n{}",
        harness.screen_to_string()
    );

    // And the same in reverse: cursor on a lower section, hover on a
    // higher one.
    let cursor_section = &EDITOR_SECTIONS[5]; // Keyboard
    let (col, row) = find_tree_row(&harness, cursor_section.0, 120).expect("Keyboard row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    let hover_section = &EDITOR_SECTIONS[1]; // Completion
    let (hcol, hrow) = find_tree_row(&harness, hover_section.0, 120).expect("Completion row");
    harness.mouse_move(hcol, hrow).unwrap();
    harness.render().unwrap();

    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant (reverse): exactly ONE tree row \
         should have a non-baseline bg. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// 4c. Single-highlight invariant after click + move-away + wheel-scroll.
/// User-reported repro: click an item in the tree, move the mouse off
/// the tree (no hover bg in the tree anymore), then mouse-wheel-scroll
/// the body. The body's current section changes; the tree should
/// re-highlight ONE row (the new section), not retain the old selection
/// alongside the new one.
#[test]
fn tree_single_highlight_after_click_then_wheel_scroll() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click a tree section row.
    let clicked = &EDITOR_SECTIONS[1]; // Completion
    let (col, row) = find_tree_row(&harness, clicked.0, 120).expect("Completion row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    // Move the mouse away from the tree, into a "neutral" body cell.
    // Use a column past the divider (>= 32) and a row inside the body
    // that's just empty space rather than a card edge.
    harness.mouse_move(110, 35).unwrap();
    harness.render().unwrap();

    // Wheel-scroll the body downward to advance current_section.
    let body_col: u16 = 100;
    let body_row: u16 = 20;
    for _ in 0..15 {
        harness.mouse_scroll_down(body_col, body_row).unwrap();
    }
    harness.render().unwrap();

    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "after click + move-away + wheel-scroll: exactly ONE tree row \
         should be highlighted. Screen:\n{}",
        harness.screen_to_string()
    );

    // And the same in reverse — scroll back up.
    for _ in 0..30 {
        harness.mouse_scroll_up(body_col, body_row).unwrap();
    }
    harness.render().unwrap();

    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "after wheel-up: exactly ONE tree row should be highlighted. \
         Screen:\n{}",
        harness.screen_to_string()
    );
}

/// User-reported bug: clicking a middle section, then mouse-wheel-UP
/// doesn't sync the tree highlight back to an earlier section. (Wheel-
/// down works; only wheel-up was broken.) Reproducer for the regression.
#[test]
fn wheel_up_after_click_syncs_tree_highlight_to_earlier_section() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click a middle section so the tree cursor is in the middle.
    let middle = &EDITOR_SECTIONS[3]; // Display
    let (col, row) = find_tree_row(&harness, middle.0, 120).expect("Display row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    let middle_name = highlighted_section_name(&harness, 120);
    assert_eq!(
        middle_name.as_deref(),
        Some(middle.0),
        "after click, tree should highlight '{}', got {:?}",
        middle.0,
        middle_name
    );

    // Now wheel-UP on the body. The body scrolls back toward earlier
    // sections, so the tree highlight should shift to one of them.
    let body_col: u16 = 100;
    let body_row: u16 = 20;
    for _ in 0..30 {
        harness.mouse_scroll_up(body_col, body_row).unwrap();
    }
    harness.render().unwrap();

    let after_up = highlighted_section_name(&harness, 120);
    let after_up_idx = after_up
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n));
    assert!(
        matches!(after_up_idx, Some(idx) if idx < 3),
        "after wheel-UP from middle (Display), tree highlight should \
         move to a section BEFORE Display; got {:?}. Screen:\n{}",
        after_up,
        harness.screen_to_string()
    );
}

// ─── Keyboard-only navigation tests (per user's bug report) ──────────────────

/// Helper: send Tab repeatedly until focus lands back on Categories.
/// Mouse clicks can move focus to Settings; the tree-cursor tests want
/// to verify keyboard behavior with focus on Categories.
fn focus_categories(harness: &mut EditorTestHarness) {
    // Send BackTab once — settings → categories (one step in the focus
    // cycle). For other starting panels this also lands eventually
    // because the cycle has only 3 elements.
    for _ in 0..4 {
        let screen = harness.screen_to_string();
        // Heuristic: when categories is focused, the cursor row in the
        // tree shows '>'. We stop when we see a `│>` substring (tree
        // panel left edge plus marker).
        if screen.contains("│>") {
            return;
        }
        harness
            .send_key(KeyCode::BackTab, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
}

/// Bug A: Right key MUST NOT switch focus to the Settings panel. After
/// the user previously locked in "only Tab switches panels", pressing
/// Right on a category should ONLY expand it; pressing Right on a
/// non-expandable category or an already-expanded one should be a no-op
/// — the tree cursor stays put and the body panel doesn't take focus.
#[test]
fn right_key_never_moves_focus_to_settings() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_settings().unwrap();

    // Right on General (no sections / non-expandable) — must stay in tree.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("│>"),
        "Right on a non-expandable category must keep focus in the \
         tree (the `>` cursor marker should still be visible). Screen:\n{}",
        screen
    );

    // Walk to Editor (expandable) and press Right twice — first expands,
    // second should be a no-op (already expanded), neither should move
    // focus.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("│>") && screen.contains("▼"),
        "Right on expandable category: must expand AND keep focus in tree. \
         Screen:\n{}",
        screen
    );

    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("│>"),
        "Right on already-expanded category: focus must stay in tree. \
         Screen:\n{}",
        screen
    );
}

/// Bug B: pressing Right to expand a category must NOT visually jump
/// the cursor to the first section row. The cursor stays on the
/// CATEGORY row; sections appear *below* it. A subsequent Down then
/// walks into the sections.
#[test]
fn right_to_expand_does_not_jump_cursor_to_first_section() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_settings().unwrap();

    // Walk to Editor and expand it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cursor must still be on the Editor row, not on Bracket Matchi.
    // Look for `>` immediately followed (after some spaces) by the
    // chevron and 'Editor' — that's the category row indicator.
    let screen = harness.screen_to_string();
    let editor_row_has_cursor = screen
        .lines()
        .any(|l| l.contains(">▼") && l.contains("Editor"));
    assert!(
        editor_row_has_cursor,
        "After Right to expand, cursor should remain on the 'Editor' \
         category row. Screen:\n{}",
        screen
    );

    // And the section rows in the tree should NOT be marked as the cursor.
    let any_section_has_cursor = screen.lines().any(|l| {
        l.contains(">      ") // 6 spaces of indent before section name
            && (l.contains("Bracket Matchi") || l.contains("Completion"))
    });
    assert!(
        !any_section_has_cursor,
        "After Right to expand, no section row should be marked with `>`. \
         Screen:\n{}",
        screen
    );
}

/// Bug C: After Right to expand, pressing Down must walk the cursor
/// into the first section row. (Pre-fix, the cursor was stuck on the
/// category because tree_cursor_index derived from scroll state.)
#[test]
fn down_after_expand_walks_into_first_section() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_settings().unwrap();

    // General → Clipboard → Editor.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Expand Editor.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    // Step into the first section.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let on_first_section = screen.lines().any(|l| l.contains(">      Bracket Matchi"));
    assert!(
        on_first_section,
        "After Down past an expanded category, cursor must land on the \
         first section ('Bracket Matchi'). Screen:\n{}",
        screen
    );
    // And exactly one row is visually highlighted.
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant"
    );
}

/// Bug D: With a category expanded, repeated Down walks through every
/// section row in order, then to the next category. Up walks back the
/// other way. (Pre-fix, the cursor was stuck because the scroll-derived
/// `current_section_index` clamped the indicator.)
#[test]
fn down_then_up_walks_every_visible_tree_row() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_settings().unwrap();

    // General → Clipboard → Editor → expand.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    // Editor (cursor) → Bracket Matchi → Completion → Diagnostics.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        harness
            .screen_to_string()
            .lines()
            .any(|l| l.contains(">      Diagnostics")),
        "After Editor + 3xDown, cursor should be on Diagnostics. Screen:\n{}",
        harness.screen_to_string()
    );

    // Up x2 → Bracket Matchi.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        harness
            .screen_to_string()
            .lines()
            .any(|l| l.contains(">      Bracket Matchi")),
        "After 2xUp, cursor should be back on Bracket Matchi. Screen:\n{}",
        harness.screen_to_string()
    );

    // One more Up → Editor (the category row).
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        harness
            .screen_to_string()
            .lines()
            .any(|l| l.contains(">▼") && l.contains("Editor")),
        "Up from first section should land on the parent category. Screen:\n{}",
        harness.screen_to_string()
    );

    // Up again → Clipboard (previous category). Accept any whitespace
    // between the `>` cursor marker and the category name (the chevron
    // column for non-expandable categories is rendered as a plain
    // space, and the icon adds a variable-width glyph).
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let on_clipboard = screen.lines().any(|l| {
        if let Some(arrow_idx) = l.find('>') {
            if let Some(label_idx) = l.find("Clipboard") {
                return label_idx > arrow_idx && (label_idx - arrow_idx) < 12;
            }
        }
        false
    });
    assert!(
        on_clipboard,
        "Up from category should land on previous category (Clipboard). \
         Screen:\n{}",
        screen
    );
}

/// Bug E (user-reported addition): click a middle section in the tree
/// under a category with >3 sections, then mouse-wheel scroll UP in
/// the body. The left-panel highlight must auto-sync to an EARLIER
/// section.
#[test]
fn click_middle_section_then_wheel_up_syncs_to_earlier_section() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click on a section deep into the list (Editor has > 3 sections).
    let middle = &EDITOR_SECTIONS[5]; // Keyboard
    let (col, row) = find_tree_row(&harness, middle.0, 120).expect("Keyboard row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();
    assert_eq!(
        highlighted_section_name(&harness, 120).as_deref(),
        Some(middle.0),
        "after click, tree should highlight Keyboard"
    );

    // Mouse-wheel UP on the body panel (col past the divider).
    let body_col: u16 = 100;
    let body_row: u16 = 20;
    for _ in 0..40 {
        harness.mouse_scroll_up(body_col, body_row).unwrap();
    }
    harness.render().unwrap();

    // The highlight must have moved to an EARLIER section than Keyboard.
    let after = highlighted_section_name(&harness, 120);
    let after_idx = after
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n));
    assert!(
        matches!(after_idx, Some(idx) if idx < 5),
        "after wheel-up from Keyboard (idx 5), highlight should be on \
         a section BEFORE it; got {:?}. Screen:\n{}",
        after,
        harness.screen_to_string()
    );

    // Single-highlight invariant still holds.
    assert_eq!(
        count_highlighted_tree_rows(&harness, 120),
        1,
        "single-highlight invariant after wheel-up sync"
    );
}

/// Resuming keyboard nav after body scroll: scroll the body, Tab back
/// to the tree, press Up — the cursor should walk relative to where
/// the body is now (i.e. starting from the synced section), not where
/// the user last clicked.
#[test]
fn keyboard_up_after_body_scroll_starts_from_synced_section() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_editor_expanded(&mut harness);

    // Click an early section, then wheel-down so the body is in a later
    // section; the tree cursor follows.
    let start = &EDITOR_SECTIONS[1]; // Completion
    let (col, row) = find_tree_row(&harness, start.0, 120).expect("Completion row");
    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    let body_col: u16 = 100;
    let body_row: u16 = 20;
    for _ in 0..30 {
        harness.mouse_scroll_down(body_col, body_row).unwrap();
    }
    harness.render().unwrap();
    let synced = highlighted_section_name(&harness, 120);
    let synced_idx = synced
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .expect("synced section");
    assert!(
        synced_idx > 1,
        "after wheel-down, highlight should be past Completion"
    );

    // BackTab into the categories panel, press Up: cursor should now
    // step from the synced section (not from Completion).
    focus_categories(&mut harness);
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let after = highlighted_section_name(&harness, 120);
    let after_idx = after
        .as_deref()
        .and_then(|n| EDITOR_SECTIONS.iter().position(|s| s.0 == n))
        .expect("highlighted after Up");
    assert_eq!(
        after_idx,
        synced_idx - 1,
        "Up should step from the synced section ({}) to {} (one earlier), got {}",
        synced_idx,
        synced_idx - 1,
        after_idx
    );
}
