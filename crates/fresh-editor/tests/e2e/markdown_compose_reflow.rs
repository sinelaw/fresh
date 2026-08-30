//! E2E: compose/preview mode flows a block's source lines into one paragraph.
//!
//! Markdown reads a run of consecutive non-blank lines as ONE paragraph (or
//! one list item, or one quote) — the newlines between them are word
//! separators. Compose mode used to render each source line as its own row, so
//! a hard-wrapped document (the ordinary way markdown is written) came out
//! ragged, with a list item's continuation lines starting back at the item's
//! own column instead of hanging under its text.
//!
//! These drive the real plugin and assert only on rendered rows.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// Open `content` as markdown with the real `markdown_compose` plugin and turn
/// compose mode on from the command palette.
fn composed(content: &str, width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("flow.md");
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
    harness.assert_screen_contains("flow.md");

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

    (harness, temp_dir)
}

/// The screen row index containing `needle`, panicking with the screen if absent.
fn row_with(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
}

/// The screen line containing `needle`.
fn line_with(harness: &EditorTestHarness, needle: &str) -> String {
    let screen = harness.screen_to_string();
    let row = row_with(harness, needle);
    screen.lines().nth(row).unwrap().to_string()
}

/// Character column `needle` starts at within `line`.
fn column_in(line: &str, needle: &str) -> usize {
    let byte = line
        .find(needle)
        .unwrap_or_else(|| panic!("'{needle}' not on row {line:?}"));
    line[..byte].chars().count()
}

/// Character column `needle` starts at on the row it appears on.
fn column_of(harness: &EditorTestHarness, needle: &str) -> usize {
    column_in(&line_with(harness, needle), needle)
}

/// Wait until the plugin has decorated the buffer: `**` disappearing is the
/// established "the conceal pass has run" signal in this suite.
fn settle(harness: &mut EditorTestHarness) {
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
}

// ---------------------------------------------------------------------------
// Paragraphs
// ---------------------------------------------------------------------------

const PARAGRAPH_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

A hard-wrapped paragraph whose first source line
stops early,
and whose third source line finishes the thought.

Tail.
";

/// The headline case: three source lines become one flowed paragraph, so the
/// words that followed a source break move up onto the row above.
#[test]
fn a_hard_wrapped_paragraph_flows_into_one_block() {
    let (mut harness, _tmp) = composed(PARAGRAPH_MD, 100, 20);
    settle(&mut harness);

    let line = line_with(&harness, "stops early");
    assert!(
        line.contains("A hard-wrapped paragraph whose first source line stops early,"),
        "the paragraph's source lines should flow into one row, got {line:?}",
    );
}

/// Flowing a paragraph must not swallow the blank line that ends it: the
/// paragraph after it still starts on its own row.
#[test]
fn a_blank_line_still_ends_the_flowed_paragraph() {
    let (mut harness, _tmp) = composed(PARAGRAPH_MD, 100, 20);
    settle(&mut harness);

    assert!(
        !line_with(&harness, "Tail.").contains("finishes the thought"),
        "the blank line before `Tail.` must still break the block.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// A hard break — two trailing spaces — is the one line ending markdown does
/// NOT read as a space, so the rows either side of it must stay apart.
#[test]
fn a_hard_break_is_not_flowed_away() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

Before the hard break  
after the hard break.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    assert!(
        !line_with(&harness, "Before the hard break").contains("after the hard break"),
        "a two-space hard break must still end its row.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// A heading is a block of its own: the paragraph written directly under it
/// must not be flowed into the heading's row.
#[test]
fn a_heading_does_not_flow_into_the_paragraph_below_it() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

## A heading
Body written directly under the heading.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    assert!(
        !line_with(&harness, "A heading").contains("Body written directly"),
        "a heading is single-line and must not flow.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

// ---------------------------------------------------------------------------
// List items
// ---------------------------------------------------------------------------

const LIST_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = \"abort\"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).

2. Second item.
";

/// An item's continuation lines join it, and the rows it wraps onto hang under
/// the item's text rather than restarting at the marker's column.
#[test]
fn a_list_item_flows_and_its_wrapped_rows_hang_under_its_text() {
    let (mut harness, _tmp) = composed(LIST_MD, 90, 20);
    settle(&mut harness);

    let first = line_with(&harness, "min-size profile");
    assert!(
        first.contains("inherits"),
        "the item's continuation line should flow onto the row above, got {first:?}",
    );

    // "min-size" is the item's first word, so its column is where the item's
    // text starts; every wrapped row of the same item has to line up with it.
    let text_column = column_in(&first, "min-size profile");
    let marker_column = column_in(&first, "1.");
    assert!(
        text_column > marker_column,
        "the item's text should sit right of its `1.` marker on {first:?}",
    );
    let screen = harness.screen_to_string();
    let wrapped = screen
        .lines()
        .nth(row_with(&harness, "min-size profile") + 1)
        .expect("the item should wrap onto a second row");
    assert_eq!(
        wrapped.len() - wrapped.trim_start().len(),
        text_column,
        "a wrapped row of the item should hang under its text, \
         got {wrapped:?}.\nScreen:\n{screen}",
    );
}

/// The next `1.`/`2.` marker opens its own item: two items never flow together.
#[test]
fn a_new_marker_starts_a_new_item() {
    let (mut harness, _tmp) = composed(LIST_MD, 90, 20);
    settle(&mut harness);

    assert!(
        !line_with(&harness, "Second item.").contains("debuginfo"),
        "a new list marker must start a new block.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

// ---------------------------------------------------------------------------
// Block quotes
// ---------------------------------------------------------------------------

/// A quote's lines flow together, and the `>` run of each continuation is
/// swallowed with the newline rather than drawing a second bar mid-row.
#[test]
fn a_quote_flows_and_keeps_one_bar_per_row() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

> A quoted block that is
> split across two source lines.

Tail.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    let line = line_with(&harness, "A quoted block");
    assert!(
        line.contains("split across two source lines."),
        "the quote's source lines should flow into one row, got {line:?}",
    );
    assert_eq!(
        line.chars().filter(|c| *c == '▌').count(),
        1,
        "the flowed row should draw one bar, not one per source line, got {line:?}",
    );
}

// ---------------------------------------------------------------------------
// Code blocks
// ---------------------------------------------------------------------------

/// Code is never prose: a fenced block's lines keep their own rows however
/// many of them there are in a row without a blank line.
#[test]
fn fenced_code_lines_are_not_flowed() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

```rust
let a = 1;
let b = 2;
let c = 3;
```

Tail.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    let line = line_with(&harness, "let a = 1;");
    assert!(
        !line.contains("let b = 2;"),
        "code lines must keep their own rows, got {line:?}",
    );
}

/// Raw HTML is not prose either. A `<div>` on its own line and the lines under
/// it are markup the reader has to see laid out as written — flowing them puts
/// the opening tag and the text it wraps on one row.
#[test]
fn an_html_block_is_not_flowed() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

<div class=\"note\">
  <img src=\"./showcase.gif\" alt=\"demo\" />
</div>

<!-- A comment that runs
     onto a second line -->

Tail.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !line_with(&harness, "<div class=").contains("<img"),
        "an HTML block's lines must keep their own rows.\nScreen:\n{screen}",
    );
    assert!(
        !line_with(&harness, "<!-- A comment").contains("onto a second line"),
        "an HTML comment's lines must keep their own rows.\nScreen:\n{screen}",
    );
}

/// A paragraph that merely *starts* with an inline tag is still prose, and
/// still flows — the HTML rule keys on the whole line being markup.
#[test]
fn a_paragraph_opening_with_an_inline_tag_still_flows() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

<em>Emphasised</em> opening words and then
the rest of the sentence.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    assert!(
        line_with(&harness, "opening words").contains("the rest of the sentence."),
        "a paragraph beginning with an inline tag should still flow.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// An indented code block is code too, and a run that opens with four columns
/// of indent is one. A list item's continuation lines are indented as well —
/// past four columns for a wide marker — which is why this keys on the run's
/// first line and not on any indented line.
#[test]
fn an_indented_code_block_is_not_flowed() {
    const MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

    let indented = 1;
    let code = 2;

12. A wide marker whose continuation is indented
    four columns and still belongs to the item.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !line_with(&harness, "let indented = 1;").contains("let code = 2;"),
        "an indented code block's lines must keep their own rows.\nScreen:\n{screen}",
    );
    assert!(
        line_with(&harness, "A wide marker").contains("four columns"),
        "a wide marker's continuation is not an indented code block.\nScreen:\n{screen}",
    );
}

/// Front matter is metadata, not a paragraph: `title:` and `outline:` are
/// separate fields and flowing them onto one row makes the block unreadable.
#[test]
fn yaml_front_matter_is_not_flowed() {
    const MD: &str = "\
---
title: \"Split View\"
outline: false
---

# Doc

Body paragraph written over
two source lines.
";
    let (mut harness, _tmp) = composed(MD, 100, 20);
    settle(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !line_with(&harness, "title:").contains("outline:"),
        "front-matter fields must keep their own rows.\nScreen:\n{screen}",
    );
    // The body below it still flows, so this is about the front matter and not
    // about reflow having stopped altogether.
    assert!(
        line_with(&harness, "Body paragraph").contains("two source lines."),
        "the body below the front matter should still flow.\nScreen:\n{screen}",
    );
}

// ---------------------------------------------------------------------------
// Batches
// ---------------------------------------------------------------------------

/// A `lines_changed` batch carries only lines the editor has not offered
/// before, so a paragraph revealed a line at a time by scrolling would arrive
/// split across batches — and the plugin, seeing a line without its
/// predecessor, would leave it unflowed. The editor closes the batch over the
/// whole run for a composing buffer, so scrolling to a paragraph renders it the
/// same as opening on it.
#[test]
fn a_paragraph_scrolled_into_view_flows_like_any_other() {
    let mut md = String::from("Opening paragraph with **emphasis** for line 1.\n\n");
    for p in 0..12 {
        md.push_str(&format!("Paragraph {p} opens here,\n"));
        md.push_str("continues on a second source line,\n");
        md.push_str("and closes on a third.\n\n");
    }

    let (mut harness, _tmp) = composed(&md, 90, 14);
    settle(&mut harness);

    // One row at a time, so each new line arrives in a batch of its own —
    // the case a run-closed batch exists for.
    for _ in 0..24 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.wait_until_stable(|_| true).unwrap();
    }
    harness.wait_for_async_quiescence(4).unwrap();

    let screen = harness.screen_to_string();
    let unflowed: Vec<&str> = screen
        .lines()
        .filter(|l| l.trim() == "and closes on a third.")
        .collect();
    assert!(
        unflowed.is_empty(),
        "every visible paragraph should be flowed after scrolling; \
         these rows are still on their own.\nScreen:\n{screen}",
    );
}

/// Reflow makes a source line worth less than a screen row, so a window sized
/// in source lines stops above the viewport's bottom. The editor grows both the
/// token build and the plugin's batch by the line breaks the conceals swallow;
/// without that the frame ends in EOF tildes with the document still going.
#[test]
fn a_reflowed_viewport_is_filled_to_its_last_row() {
    let mut md = String::from("Opening paragraph with **emphasis** for line 1.\n\n");
    for p in 0..12 {
        md.push_str(&format!("Paragraph {p} opens here,\n"));
        md.push_str("continues on a second source line,\n");
        md.push_str("and closes on a third.\n\n");
    }

    let (mut harness, _tmp) = composed(&md, 90, 20);
    settle(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains('~'),
        "the viewport should be filled with document, not end-of-file \
         tildes.\nScreen:\n{screen}",
    );
}

// ---------------------------------------------------------------------------
// The caret
// ---------------------------------------------------------------------------

/// Walk the caret over every position of a document and report the ones where
/// the editor drew no caret at all.
///
/// `render_observing_cursor` is the production signal: it parks the terminal
/// caret at the origin and renders, so `None` means the editor never called
/// `Frame::set_cursor_position` — which is exactly what it does when no
/// rendered cell carries the caret's byte. (A caret legitimately at (0, 0)
/// would read the same, which cannot happen here: a composed buffer's text
/// starts past the gutter and the page margin.) Returns the buffer offsets the
/// caret vanished at, with the step index that reached them.
fn walk_and_collect_hidden(
    harness: &mut EditorTestHarness,
    key: KeyCode,
    steps: usize,
) -> Vec<(usize, usize)> {
    let mut hidden = Vec::new();
    let mut last_position = usize::MAX;
    for step in 0..steps {
        harness.send_key(key, KeyModifiers::NONE).unwrap();
        let drawn = harness.render_observing_cursor().unwrap();
        let position = harness.cursor_position();
        if position == last_position {
            break; // ran into the end of the buffer
        }
        last_position = position;
        if drawn.is_none() {
            hidden.push((step, position));
        }
    }
    hidden
}

/// The caret must have a cell to sit on at every position of a composed
/// document — arrowing through one should never make it vanish.
///
/// It is a property rather than a case because the ways compose mode can lose
/// it are open-ended: every conceal, soft break and reflow join moves the
/// mapping from a byte to the cell that draws it, and the caret disappears
/// wherever a byte ends up claimed by no cell at all. Walking the whole
/// document forwards and back covers each of those boundaries in both
/// directions, which is where the asymmetries show up.
#[test]
fn the_caret_is_never_lost_while_arrowing_through_a_composed_document() {
    // Deliberately varied: every construct that adds a conceal or a break, and
    // paragraphs long enough to wrap so the caret crosses a wrap point both at
    // a join and at an ordinary space.
    const MD: &str = "\
# Heading with `code` and **bold**

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = \"abort\"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).

2. Second item that also spans several source lines in the original file and
   is long enough that it has to wrap at the page measure more than once when
   the whole item is flowed together.

A plain paragraph written with hard-wrapped source lines that flow into one
block and then wrap, so the caret meets both a join and an ordinary space at
a row boundary.

> A quoted block that is split across
> two source lines.

```rust
fn answer() -> u32 { 42 }
```

| col | val |
|-----|-----|
| a   | 1   |

Tail paragraph.
";

    let (mut harness, _tmp) = composed(MD, 90, 40);
    settle(&mut harness);

    let total = MD.len();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render_observing_cursor().unwrap();

    let forward = walk_and_collect_hidden(&mut harness, KeyCode::Right, total + 8);
    assert!(
        forward.is_empty(),
        "arrowing right lost the caret at {} position(s) — (step, byte offset): {:?}\nScreen:\n{}",
        forward.len(),
        forward,
        harness.screen_to_string(),
    );

    let backward = walk_and_collect_hidden(&mut harness, KeyCode::Left, total + 8);
    assert!(
        backward.is_empty(),
        "arrowing left lost the caret at {} position(s) — (step, byte offset): {:?}\nScreen:\n{}",
        backward.len(),
        backward,
        harness.screen_to_string(),
    );
}

/// The step past `End` must keep the caret drawn.
///
/// `End` on a reflowed row still stops one cell short of the row's edge — it
/// lands ON the last character rather than after it, because the row's end is
/// derived from its last drawn source byte and a row that spans a concealed
/// line ending leaves the wrap's own byte to the row below. That is a separate
/// defect in the shared soft-break machinery, not covered here; what this
/// pins is that the step which used to lose the caret no longer does.
#[test]
fn the_step_past_end_keeps_the_caret_on_a_reflowed_row() {
    const MD: &str = "\
Cursor home line.

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = \"abort\"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).
";
    let (mut harness, _tmp) = composed(MD, 90, 20);
    settle(&mut harness);

    // Onto the block, then to the end of the row the caret sits on.
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render_observing_cursor().unwrap();
    let at_end = harness.cursor_position();

    // One more step right must leave the row, not creep along it: if `End`
    // stopped short, there is still text between it and the row's edge.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    let drawn = harness.render_observing_cursor().unwrap();
    assert!(
        drawn.is_some(),
        "the caret vanished one step past `End` (byte {at_end}).\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// `End` must not take the caret off the row it was on.
///
/// The caret's byte and the cell that draws it are two different things, and
/// `End` is where they come apart: a row reports its end as the last source
/// byte it drew, and the separator a wrap broke on may be drawn by the row
/// BELOW. Move the caret to that byte and it is drawn at the start of the next
/// row — the caret visibly jumps a line, whatever the buffer offset says.
///
/// So this asserts the drawn position, not the offset. An earlier version
/// asserted where typing landed in the buffer, which a caret sitting on the
/// wrong row satisfies perfectly, and so missed exactly that regression.
///
/// The three fixtures are the three ways a row can end, which behave
/// differently and were all worth separating: a reflowed row (compose joins
/// two source lines, and the wrap falls in the line below), a row the plugin's
/// soft break alone wrapped (source line shorter than the window), and one the
/// editor's own wrap broke as well (source line longer than the window). The
/// middle one is the case a single long-line fixture silently fails to cover.
///
/// Known and NOT asserted here: on a reflowed row `End` stops ON the row's last
/// character rather than after it. That one is real but smaller, and every fix
/// tried for it so far moved one of the other two rows instead.
#[test]
fn end_keeps_the_caret_on_its_own_row() {
    const REFLOWED: &str = "\
Cursor home line.

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = \"abort\"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).
";
    // 98 columns of source in a 120-column window: too short for the editor's
    // own wrap, so the compose measure's soft break is the only thing that
    // breaks the row.
    const PLUGIN_WRAP: &str = "\
Cursor home line.

Alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi \
omicron pi rho sigma end.
";
    // Longer than the window, so the editor wraps it too.
    const EDITOR_WRAP: &str = "\
Cursor home line.

Alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi \
omicronpi rho sigma tau upsilon phi chi psi omega end and more words here to \
run past the window width by a comfortable margin.
";

    for (label, md, width, marker) in [
        ("reflowed", REFLOWED, 90u16, "unwinding tables) and"),
        ("plugin wrap", PLUGIN_WRAP, 120, "mu nu xi"),
        ("editor wrap", EDITOR_WRAP, 80, "mu nu xi"),
    ] {
        let (mut harness, _tmp) = composed(md, width, 20);
        settle(&mut harness);

        let target_row = row_with(&harness, marker) as u16;
        for _ in 0..40 {
            if harness.screen_cursor_position().1 == target_row {
                break;
            }
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
            harness.render().unwrap();
        }
        let before = harness
            .render_observing_cursor()
            .unwrap()
            .unwrap_or_else(|| panic!("{label}: no caret drawn before `End`"));
        assert_eq!(
            before.1, target_row,
            "{label}: could not put the caret on the row holding {marker:?}",
        );

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        let after = harness
            .render_observing_cursor()
            .unwrap()
            .unwrap_or_else(|| panic!("{label}: `End` left no caret drawn"));

        assert_eq!(
            after.1,
            before.1,
            "{label}: `End` moved the caret off its row, from {before:?} to \
             {after:?}.\nScreen:\n{}",
            harness.screen_to_string(),
        );
        // And it reaches the row's end: the cell just past its last glyph. A
        // soft break consumes the space it fell on, so that cell is carried by
        // no token — the row owns the position anyway, and `End` is what puts
        // the caret there. Stopping a column earlier leaves it ON the last
        // character, where what you type next goes in ahead of that character.
        let row_text = harness
            .screen_to_string()
            .lines()
            .nth(after.1 as usize)
            .unwrap_or_default()
            .to_string();
        let last_glyph_col = row_text.trim_end().chars().count();
        // A row whose wrap consumed a separator ends one past its last glyph:
        // that cell IS the separator. A row split mid-run has no separator to
        // stand on — the byte past its last glyph is the first character of the
        // row below, which that row draws — so there the row's end is its last
        // glyph, and stepping past it would take the caret off the row.
        let expected = if label == "no-whitespace run" {
            last_glyph_col - 1
        } else {
            last_glyph_col
        };
        assert_eq!(
            after.0 as usize, expected,
            "{label}: `End` should sit at column {expected} but is at {}.\n\
             Row: {row_text:?}",
            after.0,
        );
    }
}

/// `Home` returns to the row it was on, at its start — the counterpart to
/// `End`, and the one that goes wrong when a row and the layout disagree about
/// which bytes belong to it. A reflowed row spans a concealed line ending, so
/// the byte `End` leaves the caret on is past every byte any cell draws; if the
/// row below claims it, `Home` from there walks to that row's start instead and
/// the caret jumps backwards into the middle of the line it was on.
#[test]
fn home_returns_to_the_start_of_the_row_end_left() {
    const MD: &str = "\
Cursor home line.

1. **`min-size` profile** (`[profile.min-size]` in the root `Cargo.toml`)
   inherits `release` and additionally sets `panic = \"abort\"` (drops unwinding
   tables) and `strip = true` (strips symbols + debuginfo).
";
    let (mut harness, _tmp) = composed(MD, 90, 20);
    settle(&mut harness);

    let target_row = row_with(&harness, "unwinding tables) and") as u16;
    for _ in 0..40 {
        if harness.screen_cursor_position().1 == target_row {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let start = harness.render_observing_cursor().unwrap().expect("caret");

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render_observing_cursor().unwrap().expect("caret");
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    let home = harness.render_observing_cursor().unwrap().expect("caret");

    assert_eq!(
        home.1,
        target_row,
        "`Home` after `End` left the row, to {home:?}.\nScreen:\n{}",
        harness.screen_to_string(),
    );
    assert!(
        home.0 <= start.0,
        "`Home` after `End` should return to the row's start, not land right of \
         where the caret began ({start:?}); got {home:?}",
    );
}
