use crate::common::harness::EditorTestHarness;

/// Within one rendered table every frame line — borders and content rows — must
/// have the same right edge (the table is a rectangle). Returns the sorted set
/// of distinct right-edge columns of the contiguous run of box-drawing lines, so
/// a caller can assert it has exactly one element. A mismatch means rows were
/// laid out at different column widths (the partial-batch width wobble).
#[cfg(feature = "plugins")]
fn table_frame_right_edges(screen: &str) -> Vec<usize> {
    const BOX: &[char] = &['┌', '┬', '┐', '├', '┼', '┤', '└', '┴', '┘', '│', '─'];
    let right_edge = |line: &str| -> Option<usize> {
        line.chars()
            .enumerate()
            .filter(|(_, c)| BOX.contains(c))
            .map(|(i, _)| i)
            .last()
    };
    // Take the first contiguous run of table lines (skip any stray box glyphs
    // elsewhere on screen).
    let mut edges = std::collections::BTreeSet::new();
    let mut in_table = false;
    for line in screen.lines() {
        match right_edge(line) {
            Some(col) => {
                in_table = true;
                edges.insert(col);
            }
            None => {
                if in_table {
                    break;
                }
            }
        }
    }
    edges.into_iter().collect()
}

/// Regression test: inserting a blank line *above* a table in compose mode must
/// not corrupt the table's borders.
///
/// In compose/preview mode the `markdown_compose` plugin draws a box-drawing
/// frame around tables: a `┌─┬─┐` top border above the header, `├─┼─┤`
/// separators between rows, and a `└─┴─┘` bottom border.  Those borders are
/// virtual lines anchored to the table rows, so when text is inserted above the
/// table they should simply ride downward unchanged.
///
/// The bug class: the plugin keys its table bookkeeping (border namespaces, the
/// cached column-width map, and the first/last-row classification) by *line
/// number*.  Inserting lines above the table renumbers every row, so that state
/// goes stale — the border pass eventually believes the header has a table row
/// above it and stops drawing the `┌─┬─┐` top border (it renders the bare header
/// with no frame, or a `├─┼─┤` separator instead).
///
/// The corruption is cumulative: the table renders correctly for the first few
/// inserts and only breaks once enough stale per-line state has piled up, so a
/// single Enter is not a sufficient reproducer.  This test hammers Enter at the
/// top of the file and asserts — on rendered output only — that the table is
/// still a single well-formed frame with its `┌` top border above the header.
#[test]
fn test_table_border_survives_insert_above() {
    use crate::common::harness::{copy_plugin, copy_plugin_lib};
    use crate::common::tracing::init_tracing_from_env;
    use crossterm::event::{KeyCode, KeyModifiers};

    init_tracing_from_env();

    // A document with a table a few lines down so a top-of-file insert keeps it
    // comfortably inside the viewport.
    let md_content = "\
# Table Border Regression

Intro paragraph one.

| Task   | Owner | Status |
|--------|-------|--------|
| Build  | Alice | Done   |
| Deploy | Dave  | Done   |
| Docs   | Frank | Done   |

Tail paragraph one.
Tail paragraph two.
";

    // -- Project with the markdown_compose plugin -------------------------
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("table.md");
    std::fs::write(&md_path, md_content).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 40, Default::default(), project_root)
            .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("table.md");

    // Enable compose mode via the command palette.
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

    // Wait until the table frame is rendered (top border drawn) and stable.
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();

    // Sanity: a well-formed top border exists before we edit.
    assert!(
        harness.screen_to_string().contains('┌'),
        "compose mode should draw a ┌ top border before editing.\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // -- Insert blank lines at the very top of the file ------------------
    // Move to the start of the buffer, then press Enter several times,
    // letting the view settle after each (mirrors a user hammering Enter).
    // A single insert was not enough to expose the deeper failure: the table
    // renders correctly for the first few inserts and only loses its top border
    // once the plugin's per-line bookkeeping has accumulated enough stale state
    // (around the 5th insert with this layout), so we insert well past that.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    for _ in 0..8 {
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        let mut prev = String::new();
        harness
            .wait_until_stable(|h| {
                let s = h.screen_to_string();
                let stable = s == prev;
                prev = s;
                stable
            })
            .unwrap();
    }

    // Let the edit storm's async work drain. The table's borders/conceals are
    // produced asynchronously by the plugin thread: each edit fires
    // `lines_changed` fire-and-forget, the plugin processes it against the
    // shared state snapshot, and the resulting `addVirtualLine`/`addConceal`
    // commands are drained a tick later.
    harness.wait_for_async_quiescence(8).unwrap();

    // Force one consistent full redraw, then settle again. Under nextest's
    // heavy parallel load the plugin thread can fall behind *during* the edit
    // storm and process a `lines_changed` batch against a state snapshot the
    // editor has already advanced past — anchoring a border a line off. Because
    // `lines_changed` is edge-triggered (only byte ranges not yet seen), that
    // stale frame would otherwise stick until the rows are next touched. A
    // benign cursor move clears `seen_byte_ranges` (see `handle_refresh_lines`)
    // so every visible line re-fires `lines_changed`; now that input has
    // stopped, the snapshot matches the buffer and the table redraws correctly.
    // This asserts the table *converges* — the guarantee an async decoration
    // plugin can actually make — rather than that every mid-storm frame is
    // already perfect. The move (Down then Up) stays in the blank region above
    // the heading, so it never reveals table-cell markup.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(8).unwrap();

    // -- The regression check --------------------------------------------
    // The table must still be a single, well-formed frame: a `┌─┬─┐` top
    // border directly above the header row, and exactly one frame on screen
    // (no missing / duplicated / orphaned borders).  We assert on the
    // *rendered* screen only.
    let after = harness.screen_to_string();
    let rows: Vec<&str> = after.lines().collect();

    let header_idx = rows
        .iter()
        .position(|r| r.contains("Task") && r.contains("Owner") && r.contains("Status"))
        .unwrap_or_else(|| panic!("table header row not found on screen.\nScreen:\n{}", after));
    assert!(
        header_idx > 0,
        "header row is at the very top with no room for a border.\nScreen:\n{}",
        after,
    );

    // The line directly above the header must be the table's top border.
    let above_header = rows[header_idx - 1];
    assert!(
        above_header.contains('┌'),
        "table top border (┌─┬─┐) is missing directly above the header after \
         inserting lines above the table.\nLine above header: {:?}\nScreen:\n{}",
        above_header,
        after,
    );

    // Exactly one frame: no missing, duplicated, or orphaned corners.
    assert_eq!(
        after.matches('┌').count(),
        1,
        "expected exactly one table top-left corner (┌).\nScreen:\n{}",
        after,
    );
    assert_eq!(
        after.matches('└').count(),
        1,
        "expected exactly one table bottom-left corner (└).\nScreen:\n{}",
        after,
    );
}

/// A rendered table frame must strictly alternate border / content lines, with
/// no two border lines adjacent and no blank line inside the frame.
///
/// In compose mode a well-formed table renders as:
///
/// ```text
///   ┌─┬─┐   border (top)
///   │ … │   content (header)
///   ├─┼─┤   border (source `|---|` separator)
///   │ … │   content (row)
///   ├─┼─┤   border (inter-row separator)
///   │ … │   content (row)
///   └─┴─┘   border (bottom)
/// ```
///
/// i.e. `B C B C … C B`. The corruption this guards against is *doubled*
/// inter-row separators (`├─┼─┤` with no `│` content row between them) and a
/// spurious blank line just under the top border — neither of which the
/// corner-counting check above catches (it still sees exactly one `┌`/`└`).
///
/// `border`  = a horizontal box-drawing line (contains `─`).
/// `content` = a cell line (contains `│`, no `─`).
/// Anything else inside the frame (e.g. a blank line) is a violation.
///
/// Only built with the `plugins` feature — without it `markdown_compose` does
/// not run and there is no table frame to check (and the test below that uses
/// this helper relies on a plugins-gated editor hook).
#[cfg(feature = "plugins")]
fn assert_table_frame_well_formed(screen: &str) {
    let lines: Vec<&str> = screen.lines().collect();
    let top = lines
        .iter()
        .position(|l| l.contains('┌'))
        .unwrap_or_else(|| panic!("no table top border (┌) on screen.\nScreen:\n{}", screen));
    let bottom = lines
        .iter()
        .rposition(|l| l.contains('└'))
        .unwrap_or_else(|| panic!("no table bottom border (└) on screen.\nScreen:\n{}", screen));
    assert!(
        bottom > top,
        "bottom border above top border?\nScreen:\n{}",
        screen
    );

    #[derive(PartialEq, Debug)]
    enum Kind {
        Border,
        Content,
        Other,
    }
    let classify = |l: &str| -> Kind {
        if l.contains('─') {
            Kind::Border
        } else if l.contains('│') {
            Kind::Content
        } else {
            Kind::Other
        }
    };

    // First and last line of the frame must be borders.
    assert_eq!(
        classify(lines[top]),
        Kind::Border,
        "frame must start with a border line.\nScreen:\n{}",
        screen
    );
    assert_eq!(
        classify(lines[bottom]),
        Kind::Border,
        "frame must end with a border line.\nScreen:\n{}",
        screen
    );

    // Strict alternation between the two borders: B C B C … C B.
    let mut expect_border = true;
    for (i, line) in lines[top..=bottom].iter().enumerate() {
        let kind = classify(line);
        assert_ne!(
            kind,
            Kind::Other,
            "unexpected non-table line inside the frame at frame-row {} ({:?}) — a blank \
             line or stray text broke the table frame.\nScreen:\n{}",
            i,
            line,
            screen
        );
        let want = if expect_border {
            Kind::Border
        } else {
            Kind::Content
        };
        assert_eq!(
            kind, want,
            "table frame is not strictly alternating border/content at frame-row {} ({:?}); \
             this is the doubled-separator / displaced-row corruption.\nScreen:\n{}",
            i, line, screen
        );
        expect_border = !expect_border;
    }
}

/// Regression test for the table-border *doubled-separator* corruption.
///
/// Distinct from `test_table_border_survives_insert_above`: that test hammers
/// Enter with the view settling after every keystroke (so the plugin thread
/// stays in lock-step) and only checks the `┌`/`└` corner counts — it does not
/// catch *doubled inter-row separators* (`├─┼─┤` with no `│` row between them),
/// which is what this guards against.
///
/// ## History and what this now guards
///
/// The original bug: tables were tracked as core interval markers with a stored
/// row array. `lines_changed` is fired *fire-and-forget* to the plugin thread,
/// which read the marker off a shared snapshot the editor mutates concurrently,
/// so a batch for edit *N* could be processed after the marker was shifted for
/// edit *N+1*. The marker then sat one byte off the event positions, the plugin
/// merged the event rows into the offset stored rows, and baked a *duplicate*
/// set of rows into the payload — doubled separators that survived convergence.
///
/// Tables no longer hold any marker or stored byte positions: borders are
/// emitted per line, anchored to auto-shifting virtual-line markers, and rebuilt
/// from the live `lines_changed` event each frame (Alternative 1 in
/// docs/internal/MARKDOWN_COMPOSE_TABLE_POSITION_OWNERSHIP.md). So the historical
/// desync is now *structurally impossible*. This test nudges a plugin marker one
/// byte ahead of the buffer via `shift_plugin_markers_for_edit` and forces a
/// redraw — exactly the state that used to corrupt — and asserts the frame stays
/// strictly well-formed: a forward guard that the table no longer depends on any
/// marker coordinate. (The per-line clear's tolerance of an offset border anchor
/// — the new model's own failure mode — is unit-tested in
/// `view::virtual_text::tests::test_clear_lines_in_range_tolerates_offset_anchor`.)
///
/// Gated on `plugins`: drives the plugins-only `markdown_compose` pipeline and
/// uses the plugins-gated `shift_plugin_markers_for_edit`.
#[cfg(feature = "plugins")]
#[test]
fn test_table_border_no_doubled_separators_on_marker_event_desync() {
    use crate::common::harness::{copy_plugin, copy_plugin_lib};
    use crate::common::tracing::init_tracing_from_env;
    use crossterm::event::{KeyCode, KeyModifiers};

    init_tracing_from_env();

    // Heading directly above the table (one blank line) — the reproduction
    // layout. Short cells so no cell wraps (keeps a correct frame strictly
    // border/content alternating).
    let md_content = "\
# Files

| Name     | Description  |
|----------|--------------|
| main.rs  | entry point  |
| lib.rs   | library root |
| state.rs | editor state |
| input.rs | key handling |

Tail paragraph.
";

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("files.md");
    std::fs::write(&md_path, md_content).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 40, Default::default(), project_root)
            .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("files.md");

    // Enable compose mode via the command palette.
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

    // Wait until the table frame is rendered and stable, and verify it starts
    // out well-formed (so the assertion below catches a *regression*, not a
    // pre-existing break).
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();
    assert_table_frame_well_formed(&harness.screen_to_string());

    // -- Inject the cross-thread offset deterministically ----------------
    // The marker now matches the buffer. Shift it one byte forward *without*
    // editing the buffer — exactly the state a lagging plugin observes when it
    // processes a `lines_changed` for older positions after the editor has
    // already shifted the marker. `pos = 0` is above the table, so both the
    // marker's start and end move forward by one.
    let buf = harness.editor().active_buffer();
    harness
        .editor_mut()
        .shift_plugin_markers_for_edit(buf, 0, 0, 1);

    // Force `lines_changed` to re-fire for every visible line against the live
    // (unchanged) buffer positions: a benign cursor move clears
    // `seen_byte_ranges`. The plugin now sees event rows one byte off its
    // marker — the trigger.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(8).unwrap();

    let after = harness.screen_to_string();
    // Still exactly one frame…
    assert_eq!(
        after.matches('┌').count(),
        1,
        "expected exactly one table top-left corner (┌).\nScreen:\n{}",
        after,
    );
    // …and that frame must be strictly well-formed: no doubled `├─┼─┤`
    // separators, no blank line under the top border.
    assert_table_frame_well_formed(&after);
}

/// Regression test: a table's column widths must stay uniform across all its
/// rows even when a *partial* `lines_changed` batch (a mouse-wheel scroll that
/// reveals new rows without a cursor-move refresh) re-measures only some rows.
///
/// The per-line border model computes column widths from the rows present in
/// each batch. When the table content fits the compose width, columns are
/// content-sized, so a batch that does NOT include the table's widest row lays
/// its rows out narrower than a batch that does — and since a mouse-wheel scroll
/// fires `lines_changed` only for newly-revealed rows (no cursor move to refresh
/// the whole viewport), rows measured in different batches end up at different
/// widths on screen at the same time: mismatched right edges, the corruption
/// reproduced interactively with a continuous edit storm.
///
/// Deterministic because it needs no async lag — just a partial batch. Asserts
/// every visible table frame line shares one right edge.
///
/// Fixed by the grow-only column-width memo (a widths-only marker that
/// accumulates across batches) plus the grow-refresh that re-renders
/// already-visible rows when a wider row scrolls in.
#[cfg(feature = "plugins")]
#[test]
fn test_table_columns_uniform_width_under_partial_scroll() {
    use crate::common::harness::{copy_plugin, copy_plugin_lib};
    use crate::common::tracing::init_tracing_from_env;
    use crossterm::event::{KeyCode, KeyModifiers};

    init_tracing_from_env();

    // A table taller than the viewport whose widest first-column cell sits near
    // the bottom, so the top rows render narrow and revealing the wide row later
    // (via scroll) would widen only the rows in that batch. Content fits the
    // width, so columns are content-sized (mismatches show as different right
    // edges, not just a moved interior junction).
    let mut md = String::from("# Wide Table\n\n| Key | Value |\n|-----|-------|\n");
    for i in 1..=24 {
        if i == 20 {
            md.push_str("| this-is-a-very-long-key-column-cell-here | v |\n");
        } else {
            md.push_str(&format!("| k{:02} | v |\n", i));
        }
    }
    md.push_str("\nTail.\n");

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("wide.md");
    std::fs::write(&md_path, &md).unwrap();

    // Short viewport so the table can't all fit — scrolling reveals new rows in
    // partial batches.
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 18, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("wide.md");

    // Enable compose.
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
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();

    // Mouse-wheel scroll down a step at a time (no cursor move → no
    // whole-viewport refresh). Rows measured in an earlier batch keep their
    // width as they ride; rows revealed later are measured in their own batch.
    // After each step assert the whole visible frame still has one right edge —
    // a wobble shows the moment a row measured with the wide "Value" header is
    // on screen next to one measured without it.
    for step in 0..10 {
        harness.mouse_scroll_down(50, 9).unwrap();
        harness.wait_for_async_quiescence(4).unwrap();

        let after = harness.screen_to_string();
        let edges = table_frame_right_edges(&after);
        assert_eq!(
            edges.len(),
            1,
            "after scroll step {}, table frame lines have mismatched right edges \
             (column widths wobbled across a partial scroll batch): distinct \
             right-edge columns = {:?}.\nScreen:\n{}",
            step,
            edges,
            after,
        );
    }
}

/// Deterministic guard for the edit-storm convergence fix (`event_apply.rs`).
///
/// A structure-changing edit — a newline inserted or deleted — must clear the
/// buffer's `seen_byte_ranges` so that *every* visible line re-fires
/// `lines_changed` on the next render, the same whole-viewport convergence an
/// inter-line cursor move already triggers.
///
/// Why it matters: an Insert/Delete storm emits **no** `MoveCursor` events, so
/// before this fix nothing cleared `seen_byte_ranges` during the storm. Only
/// the edited line was invalidated; every other row merely shifted and stayed
/// "seen", so it never re-fired. A per-line decoration plugin
/// (`markdown_compose`'s table borders / conceals) then left stale decorations
/// on the un-re-fired rows — scattered across the buffer versions they were
/// last emitted at — and they persisted until the user happened to make an
/// explicit cursor move. The async corruption itself can't be reproduced in the
/// synchronous test harness (it needs the plugin thread to lag behind a real
/// edit storm), so this asserts the editor-side mechanism that fixes it.
///
/// An intra-line edit (no line-count change) must stay on the cheap path: only
/// ranges containing the edit point drop; far ranges survive (shifted).
#[test]
fn structure_changing_edit_clears_seen_byte_ranges() {
    use fresh::model::event::Event;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let buf = harness.editor().active_buffer();
    let cursor_id = harness.editor().active_cursors().primary_id();

    // Seed three "already processed" ranges, well clear of byte 0.
    let seed = |h: &mut EditorTestHarness| {
        let seen = h
            .editor_mut()
            .active_window_mut()
            .seen_byte_ranges
            .entry(buf)
            .or_default();
        seen.clear();
        seen.insert((0, 1));
        seen.insert((50, 60));
        seen.insert((100, 110));
    };
    let seen_is_empty = |h: &EditorTestHarness| {
        h.editor()
            .active_window()
            .seen_byte_ranges
            .get(&buf)
            .map(|s| s.is_empty())
            .unwrap_or(true)
    };

    // Intra-line edit (no newline): cheap path — far ranges survive.
    seed(&mut harness);
    harness
        .apply_event(Event::Insert {
            position: 0,
            text: "x".to_string(),
            cursor_id,
        })
        .unwrap();
    assert!(
        !seen_is_empty(&harness),
        "an intra-line edit must NOT wipe all seen ranges (cheap path preserved)"
    );

    // Structure-changing insert (newline): full clear so every line re-fires.
    // Buffer is now "x"; inserting "\n" at 0 yields "\nx".
    seed(&mut harness);
    harness
        .apply_event(Event::Insert {
            position: 0,
            text: "\n".to_string(),
            cursor_id,
        })
        .unwrap();
    assert!(
        seen_is_empty(&harness),
        "a newline insert must clear all seen ranges so every visible line re-fires"
    );

    // Structure-changing delete (newline): converges the same way.
    // Buffer is "\nx"; deleting [0,1) removes the leading newline.
    seed(&mut harness);
    harness
        .apply_event(Event::Delete {
            range: 0..1,
            deleted_text: "\n".to_string(),
            cursor_id,
        })
        .unwrap();
    assert!(
        seen_is_empty(&harness),
        "a newline delete must also clear all seen ranges"
    );
}



/// Regression test: when a table's LAST row wraps onto multiple visual lines,
/// the `└─┴─┘` bottom border must render *below the whole wrapped row*, not
/// after its first visual line.
///
/// The border is emitted as a virtual line *below* the last row's anchor byte.
/// On a soft-wrapped host line the renderer places that "below" line after the
/// first visual row of the wrap instead of after the last, so the row's
/// continuation spills out underneath the already-closed frame:
///
/// ```text
///   │ Productivity │ command palette, ... git │   (first visual row)
///   └──────────────┴──────────────────────────┘   (bottom border — too early)
///   │              │ log, diagnostics panel   │   (continuation, OUTSIDE the frame)
/// ```
///
/// A well-formed frame ends with the bottom border: no table content line may
/// appear after it.
#[cfg(feature = "plugins")]
#[test]
#[ignore = "reproduces an unfixed bug: the bottom border renders after the \
            first visual row of a wrapped last row, so the continuation spills \
            below the closed frame. Run with --ignored."]
fn test_table_bottom_border_below_wrapped_last_row() {
    use crate::common::harness::{copy_plugin, copy_plugin_lib};
    use crossterm::event::{KeyCode, KeyModifiers};

    // The last data row's Features cell is long enough to wrap at this width.
    let md = "\
# Doc

## Feature Overview

| Category | Features |
|----------|----------|
| **Editing** | undo/redo, multi-cursor |
| **Productivity** | command palette, menu bar, keyboard macros, git log, diagnostics panel, and more |

## Installation
";
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("wide.md");
    std::fs::write(&md_path, md).unwrap();

    // Narrow viewport so the last row's Features cell wraps.
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(70, 40, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

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
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    let bottom_idx = lines
        .iter()
        .rposition(|l| l.contains('└'))
        .unwrap_or_else(|| panic!("no bottom border (└) on screen.\nScreen:\n{}", screen));

    // No table content row (a `│` cell line) may appear after the bottom border:
    // the frame must fully enclose the wrapped last row.
    for (i, line) in lines.iter().enumerate().skip(bottom_idx + 1) {
        assert!(
            !line.contains('│'),
            "table content spilled below the bottom border at row {} ({:?}): the \
             `└─┘` border was placed before the last row finished wrapping.\nScreen:\n{}",
            i,
            line,
            screen,
        );
    }
}
