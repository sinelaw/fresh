//! E2E: block-level markdown elements are rendered, not shown as source, in
//! compose/preview mode (issue #2967, items 2-4).
//!
//! Emphasis, links and inline code were already prettified in compose mode,
//! but block-level syntax was left literal: `# Heading` kept its hashes,
//! `> quote` kept its angle bracket, `---` rendered as three dashes, and list
//! bullets stayed as `-`/`*`. These tests drive the real plugin and assert on
//! rendered output only.
//!
//! Note on the cursor: markup on the cursor's own line is deliberately
//! revealed so the source stays editable. Every fixture here therefore keeps
//! the cursor on line 1 (a plain paragraph) and puts the elements under test
//! further down.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Modifier;

/// Open `content` as markdown in a harness carrying the real
/// `markdown_compose` plugin, and enable compose mode via the command palette.
///
/// Returns the harness and the temp dir (kept alive for the test's duration).
fn composed(content: &str, width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("blocks.md");
    std::fs::write(&md_path, content).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("blocks.md");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // `**` disappearing is the established "the conceal pass has run" signal.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    (harness, temp_dir)
}

/// The screen line containing `needle`, panicking with the screen if absent.
fn line_with(harness: &EditorTestHarness, needle: &str) -> String {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
        .to_string()
}

/// Character column of `glyph` on a screen line.
///
/// Chars, not bytes: `str::find` returns a byte offset, so a line carrying a
/// multi-byte glyph before the one being measured (the gutter's `▾` fold arrow
/// is three bytes) reports a column several past the truth, and comparing two
/// such offsets across lines compares different units.
fn glyph_column(line: &str, glyph: char) -> usize {
    line.chars()
        .position(|c| c == glyph)
        .unwrap_or_else(|| panic!("no '{glyph}' on line {line:?}"))
}

/// Whether a screen row is visually empty in the content area.
///
/// The scrollbar track is drawn in the last column, so an otherwise blank row
/// can still carry a `▌` marker glyph — `trim().is_empty()` is not enough.
fn is_blank_row(line: &str) -> bool {
    line.chars().all(|c| c == ' ' || c == '▌')
}

// ---------------------------------------------------------------------------
// Headings (issue item 2, the highest-value one)
// ---------------------------------------------------------------------------

const HEADINGS_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

# Alpha Heading

Body under alpha.

## Beta Heading

Body under beta.

### Gamma Heading

Body under gamma.
";

/// ATX `#` markers are concealed, leaving just the heading text.
#[test]
fn test_headings_conceal_hash_markers() {
    let (harness, _tmp) = composed(HEADINGS_MD, 100, 30);

    for heading in ["Alpha Heading", "Beta Heading", "Gamma Heading"] {
        let line = line_with(&harness, heading);
        assert!(
            !line.contains('#'),
            "heading '{heading}' should render without its `#` marker, got: {line:?}\n\
             Screen:\n{}",
            harness.screen_to_string(),
        );
    }
}

/// Heading text is styled by level: bold, and each of the first three levels
/// gets its own colour. Before this change heading lines carried no overlay at
/// all, so they rendered in the plain body foreground.
#[test]
fn test_headings_are_styled_by_level() {
    let (harness, _tmp) = composed(HEADINGS_MD, 100, 30);

    let probe = |text: &str| {
        let (col, row) = harness
            .find_text_on_screen(text)
            .unwrap_or_else(|| panic!("'{text}' not on screen"));
        harness
            .get_cell_style(col, row)
            .unwrap_or_else(|| panic!("no style at '{text}'"))
    };

    let alpha = probe("Alpha Heading");
    let beta = probe("Beta Heading");
    let gamma = probe("Gamma Heading");
    let body = probe("Body under alpha");

    for (name, style) in [("alpha", alpha), ("beta", beta), ("gamma", gamma)] {
        assert!(
            style.add_modifier.contains(Modifier::BOLD),
            "{name} heading should render bold",
        );
    }

    assert_ne!(
        alpha.fg, body.fg,
        "heading colour must differ from body text colour",
    );
    assert_ne!(alpha.fg, beta.fg, "h1 and h2 must be coloured differently");
    assert_ne!(beta.fg, gamma.fg, "h2 and h3 must be coloured differently");
}

// ---------------------------------------------------------------------------
// Thematic breaks (issue item 2: "dividers")
// ---------------------------------------------------------------------------

const DIVIDER_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

Above the first rule.

---

Between the rules.

***

Below the second rule.
";

/// `---` and `***` render as one horizontal rule spanning the measure, not as
/// literal dashes/asterisks.
#[test]
fn test_dividers_render_as_horizontal_rules() {
    let (harness, _tmp) = composed(DIVIDER_MD, 100, 30);
    let screen = harness.screen_to_string();

    let rules: Vec<&str> = screen
        .lines()
        .filter(|l| l.trim_start().starts_with('─'))
        .collect();
    assert_eq!(
        rules.len(),
        2,
        "both `---` and `***` should render as a box-drawing rule.\nScreen:\n{screen}",
    );

    for rule in &rules {
        let dashes = rule.chars().filter(|c| *c == '─').count();
        assert!(
            dashes > 20,
            "a thematic break should span the measure, not the three source \
             characters; got {dashes} rule characters in {rule:?}",
        );
    }

    assert!(
        !screen.contains("---"),
        "literal `---` must not remain on screen.\nScreen:\n{screen}",
    );
    assert!(
        !screen.contains("***"),
        "literal `***` must not remain on screen.\nScreen:\n{screen}",
    );
}

/// A rule must not swallow the blank line after it. The whole-line conceal has
/// to stop before the newline; when it covered the terminator the rule and the
/// following line rendered as one row, pulling the rest of the document up.
#[test]
fn test_divider_does_not_swallow_following_line() {
    let (harness, _tmp) = composed(DIVIDER_MD, 100, 30);
    let screen = harness.screen_to_string();

    let rule_row = screen
        .lines()
        .position(|l| l.trim_start().starts_with('─'))
        .expect("no rule on screen");
    let next = screen.lines().nth(rule_row + 1).unwrap_or("");
    assert!(
        is_blank_row(next),
        "the blank source line after `---` must still occupy its own row; \
         found {next:?} directly under the rule.\nScreen:\n{screen}",
    );
}

// ---------------------------------------------------------------------------
// Block quotes (issue item 2: "quote blocks")
// ---------------------------------------------------------------------------

const QUOTE_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

> First quoted line.
> Second quoted line.

Between the quotes.

> Outer quoted line.
> > Inner quoted line.

Trailing paragraph.
";

/// `>` markers are replaced by a left bar, and consecutive quote lines each
/// get one so the block reads as a single bordered aside.
#[test]
fn test_block_quotes_render_a_left_bar() {
    let (harness, _tmp) = composed(QUOTE_MD, 100, 30);
    let screen = harness.screen_to_string();

    for quoted in ["First quoted line.", "Second quoted line."] {
        let line = line_with(&harness, quoted);
        assert!(
            !line.contains('>'),
            "quote marker should be concealed on {line:?}",
        );
        assert!(
            line.contains('▌'),
            "quote line should render a left bar, got {line:?}",
        );
    }

    assert!(
        !screen.contains("> "),
        "no literal `> ` markers should remain.\nScreen:\n{screen}",
    );
}

/// Quoted text is styled as an aside rather than as body copy. The default
/// theme paints `syntax.comment` the same colour as body text, so this asserts
/// the italic the overlay also carries.
#[test]
fn test_quoted_text_is_styled() {
    let (harness, _tmp) = composed(QUOTE_MD, 100, 30);

    let (col, row) = harness
        .find_text_on_screen("First quoted line.")
        .expect("quoted text not on screen");
    let style = harness
        .get_cell_style(col, row)
        .expect("no style on quoted text");
    assert!(
        style.add_modifier.contains(Modifier::ITALIC),
        "quoted text should render italic",
    );
}

/// Nesting depth stays visible: `> >` renders two bars, not one.
#[test]
fn test_nested_quote_renders_two_bars() {
    let (harness, _tmp) = composed(QUOTE_MD, 100, 30);

    let outer = line_with(&harness, "Outer quoted line.");
    let inner = line_with(&harness, "Inner quoted line.");
    assert_eq!(
        outer.chars().filter(|c| *c == '▌').count(),
        1,
        "a single-depth quote should draw one bar, got {outer:?}",
    );
    assert_eq!(
        inner.chars().filter(|c| *c == '▌').count(),
        2,
        "a doubly-nested quote should draw two bars, got {inner:?}",
    );
}

// ---------------------------------------------------------------------------
// Wrapped block quotes
// ---------------------------------------------------------------------------

/// One quote line, no newlines in it, long enough that compose mode has to
/// soft-wrap it several times. `ALPHAWORD` / `OMEGAWORD` bracket the text so
/// the first and last visual rows can be found without matching each other.
const WRAPPED_QUOTE_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

> ALPHAWORD begins a single quoted line with no newlines in it that is deliberately long \
enough that compose mode has to soft-wrap it across several visual rows, which is exactly \
where the bar used to stop being drawn, and it runs on for a good while yet so the wrap \
happens at any measure before it finally reaches OMEGAWORD.

Trailing paragraph.
";

/// Rows of the wrapped quote, top to bottom: from the one holding `ALPHAWORD`
/// through the one holding `OMEGAWORD`. Panics unless the quote actually
/// wrapped — a fixture that fits on one row would make these tests pass
/// vacuously.
fn wrapped_quote_rows(harness: &EditorTestHarness) -> Vec<String> {
    let screen = harness.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let find = |needle: &str| {
        rows.iter()
            .position(|l| l.contains(needle))
            .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
    };
    let first = find("ALPHAWORD");
    let last = find("OMEGAWORD");
    assert!(
        last > first,
        "the quote must soft-wrap for this test to mean anything, but ALPHAWORD and \
         OMEGAWORD landed on the same row.\nScreen:\n{screen}",
    );
    rows[first..=last].iter().map(|l| l.to_string()).collect()
}

/// Every visual row of a soft-wrapped quote carries the bar, so the block
/// reads as one bordered aside. Only the first row used to have one: the bar
/// comes from concealing the `>` marker, which exists on the source row alone,
/// and the continuation rows were indented but bare.
#[test]
fn test_wrapped_quote_keeps_its_bar_on_every_row() {
    let (harness, _tmp) = composed(WRAPPED_QUOTE_MD, 100, 30);
    let rows = wrapped_quote_rows(&harness);

    let bar_column = glyph_column(&rows[0], '▌');
    for row in &rows {
        assert_eq!(
            glyph_column(row, '▌'),
            bar_column,
            "every row of a wrapped quote should carry the bar in the same column, \
             but {row:?} does not match the first row's column {bar_column}.\n\
             Rows:\n{rows:#?}",
        );
    }
}

/// The bar sits in the continuation indent rather than in front of it, so the
/// quoted text keeps one column on every row. A prefix added *on top of* the
/// indent would push the continuation rows one cell right of the first.
#[test]
fn test_wrapped_quote_text_stays_in_one_column() {
    let (harness, _tmp) = composed(WRAPPED_QUOTE_MD, 100, 30);
    let rows = wrapped_quote_rows(&harness);

    let text_column = |row: &str| {
        let bar = glyph_column(row, '▌');
        row.chars()
            .enumerate()
            .skip(bar + 1)
            .find(|(_, c)| !c.is_whitespace())
            .map(|(i, _)| i)
            .unwrap_or_else(|| panic!("no text after the bar on {row:?}"))
    };

    let first = text_column(&rows[0]);
    for row in &rows {
        assert_eq!(
            text_column(row),
            first,
            "quoted text should start in the same column on every wrapped row, \
             but {row:?} starts at {} not {first}.\nRows:\n{rows:#?}",
            text_column(row),
        );
    }
}

/// Putting the cursor on the quote reveals the `>` on its source row, as it
/// does for all markup — but the continuation rows keep their bar. Their
/// leading columns are entirely virtual: there is no source markup there to
/// reveal, and dropping the bar would break the block's edge exactly when the
/// user is working inside it.
#[test]
fn test_wrapped_quote_keeps_its_bar_while_the_cursor_is_on_it() {
    let (mut harness, _tmp) = composed(WRAPPED_QUOTE_MD, 100, 30);

    // Line 3 of the fixture is the quote.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("3").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("> ALPHAWORD"))
        .unwrap();
    harness.wait_for_async_quiescence(8).unwrap();

    let rows = wrapped_quote_rows(&harness);
    assert!(
        rows[0].contains("> ALPHAWORD"),
        "the cursor's own row should reveal its `>` for editing, got {:?}",
        rows[0],
    );
    for row in &rows[1..] {
        assert!(
            row.contains('▌'),
            "a continuation row should keep its bar while the cursor is on the \
             quote, got {row:?}.\nRows:\n{rows:#?}",
        );
    }
}

// ---------------------------------------------------------------------------
// Lists (issue item "lists")
// ---------------------------------------------------------------------------

const LIST_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

- first item
- second item
  - nested item
  - another nested item
- third item

1. ordered one
2. ordered two
";

/// `-` bullets render as a bullet glyph.
#[test]
fn test_list_bullets_render_as_bullet_glyph() {
    let (harness, _tmp) = composed(LIST_MD, 100, 30);

    for item in ["first item", "second item", "third item", "nested item"] {
        let line = line_with(&harness, item);
        assert!(
            line.contains('•'),
            "list item '{item}' should render a bullet glyph, got {line:?}",
        );
        assert!(
            !line.contains('-'),
            "list item '{item}' should not keep its `-` marker, got {line:?}",
        );
    }
}

/// Ordered items keep their numbering — only unordered bullets are replaced.
#[test]
fn test_ordered_list_markers_are_preserved() {
    let (harness, _tmp) = composed(LIST_MD, 100, 30);

    let one = line_with(&harness, "ordered one");
    assert!(
        one.contains("1."),
        "an ordered item must keep its number, got {one:?}",
    );
    assert!(
        !one.contains('•'),
        "an ordered item must not gain a bullet glyph, got {one:?}",
    );
}

/// Consecutive items are separated by a blank row, so a list reads as
/// discrete entries rather than a solid block of text.
#[test]
fn test_consecutive_list_items_are_vertically_spaced() {
    let (harness, _tmp) = composed(LIST_MD, 100, 30);
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    let row_of = |needle: &str| {
        lines
            .iter()
            .position(|l| l.contains(needle))
            .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
    };

    let first = row_of("first item");
    let second = row_of("second item");
    assert_eq!(
        second - first,
        2,
        "consecutive list items should have a blank row between them; \
         'first item' at row {first}, 'second item' at row {second}.\n\
         Screen:\n{screen}",
    );
    assert!(
        is_blank_row(lines[first + 1]),
        "the row between two list items should be blank, got {:?}",
        lines[first + 1],
    );
}

/// A nested item is indented deeper than its two-space source indent, so
/// nesting depth is readable without counting spaces.
#[test]
fn test_nested_list_items_get_a_deeper_indent() {
    let (harness, _tmp) = composed(LIST_MD, 100, 30);

    let indent_of = |needle: &str| glyph_column(&line_with(&harness, needle), '•');

    let top = indent_of("second item");
    let nested = indent_of("nested item");
    assert!(
        nested > top + 2,
        "a nested item's bullet should sit deeper than its two-space source \
         indent would put it: top-level bullet at column {top}, nested at \
         {nested}",
    );
}

/// Item spacing must survive editing inside the list.
///
/// Regression: the spacer was originally placed above an item whose
/// *predecessor* was also an item. That needs two lines, but an edit-sized
/// `lines_changed` batch carries only the lines the edit touched — so the
/// spacer got cleared (this line is in the batch) and could not be re-derived
/// (its neighbour isn't). Editing inside a list silently dropped its spacing
/// and never recovered it, because `lines_changed` is edge-triggered and no
/// later batch revisits those ranges. Deciding from the item's own line alone
/// makes clear-and-rebuild a complete decision.
#[test]
fn test_list_spacing_survives_an_edit_inside_the_list() {
    let (mut harness, _tmp) = composed(LIST_MD, 100, 30);

    // Put the cursor at the end of "first item" (line 3 of LIST_MD) and open a
    // new item below it.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("3").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("- inserted item").unwrap();

    // Move off the edited line so its markup is no longer cursor-revealed.
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 3)
        .unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("inserted item"))
        .unwrap();
    harness.wait_for_async_quiescence(8).unwrap();

    let screen = harness.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let row_of = |needle: &str| {
        rows.iter()
            .position(|l| l.contains(needle))
            .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
    };

    let first = row_of("first item");
    let inserted = row_of("inserted item");
    assert_eq!(
        inserted - first,
        2,
        "the newly typed item should be separated from the one above it; \
         'first item' at row {first}, 'inserted item' at row {inserted}.\n\
         Screen:\n{screen}",
    );
    assert!(
        is_blank_row(rows[first + 1]),
        "expected a blank spacer row between the items after the edit, got {:?}",
        rows[first + 1],
    );
}

/// Turning compose off must take the inter-item spacer rows with it. They are
/// virtual lines, so unlike conceals they do not disappear on their own when
/// the mode ends — the namespace has to be cleared explicitly, exactly as the
/// table border frame does.
#[test]
fn test_list_spacers_are_removed_when_compose_is_disabled() {
    let (mut harness, _tmp) = composed(LIST_MD, 100, 30);

    // Sanity: spacing is present while composing.
    let composed_screen = harness.screen_to_string();
    let composed_rows: Vec<&str> = composed_screen.lines().collect();
    let first = composed_rows
        .iter()
        .position(|l| l.contains("first item"))
        .expect("list not rendered");
    assert!(
        is_blank_row(composed_rows[first + 1]),
        "expected a spacer row while composing",
    );

    // Toggle compose back off.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Source markers coming back is the "compose is off" signal.
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("**"))
        .unwrap();
    harness.wait_for_async_quiescence(8).unwrap();

    let screen = harness.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let first = rows
        .iter()
        .position(|l| l.contains("first item"))
        .expect("list not on screen after disabling compose");
    let second = rows
        .iter()
        .position(|l| l.contains("second item"))
        .expect("second item not on screen after disabling compose");
    assert_eq!(
        second - first,
        1,
        "spacer rows must not survive compose being turned off; 'first item' \
         at row {first}, 'second item' at row {second}.\nScreen:\n{screen}",
    );
}

/// A `#` that isn't a heading (no space, so CommonMark reads it as text) must
/// be left alone — otherwise `#hashtag` would silently lose its marker.
#[test]
fn test_non_heading_hash_is_left_alone() {
    let md = "\
Opening paragraph with **emphasis** on line 1.

Tagged as #hashtag in the body.
";
    let (harness, _tmp) = composed(md, 100, 30);

    let line = line_with(&harness, "hashtag");
    assert!(
        line.contains("#hashtag"),
        "a bare `#hashtag` is not a heading and must keep its `#`, got: {line:?}",
    );
}
