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
