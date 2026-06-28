use crate::common::harness::EditorTestHarness;

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
/// ## What actually goes wrong
///
/// In compose mode the table plugin runs on a separate thread and reads table
/// markers from a shared state snapshot. The editor shifts those markers as the
/// buffer is edited, but `lines_changed` is fired *fire-and-forget*: its line
/// byte-positions are captured at render time, while the plugin reads the marker
/// coordinates *later*, off the live snapshot. Under load the plugin processes a
/// `lines_changed` for edit *N* only after the editor has already shifted the
/// marker for edit *N+1* — so the marker sits one byte off the event's row
/// positions. `updateTableBlocks` then unions the event rows into the marker's
/// (offset) stored rows, stretches the marker (`min` start / `max` end), and
/// bakes a *duplicate* set of row positions into the marker payload, so the
/// border pass draws two separators per row. The damage is persistent — it
/// survives a convergence redraw.
///
/// ## Why this test injects the offset directly
///
/// The corruption requires that one-byte marker/event disagreement, which in
/// production is a cross-thread timing race. The deterministic test harness
/// serializes the plugin pipeline (it drains async work to quiescence after each
/// keypress), so the race cannot be provoked through the normal key API. Instead
/// we reproduce the race's *exact consequence* deterministically: set up a
/// stable, correctly-rendered table, then nudge the plugin marker one byte ahead
/// of the live buffer via `shift_plugin_markers_for_edit` *without editing the
/// buffer*, and force a `lines_changed` redraw. That is precisely the state a
/// lagging plugin thread observes. A correct plugin (event positions are the
/// source of truth) rebuilds a clean frame; the buggy marker-trusting merge
/// produces — and then preserves — doubled separators.
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
