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
/// The bug: the plugin's `cursor_moved` handler called `editor.refreshLines()`,
/// which cleared the buffer-wide "seen lines" set.  Pressing Enter at the top of
/// the document moves the cursor, so the whole viewport — including the
/// untouched table — was re-decorated.  The border pass then ran against
/// freshly renumbered lines using stale per-line state and reclassified the
/// table's *first* row as having a row above it, replacing the `┌─┬─┐` top
/// border with a `├─┼─┤` separator.
///
/// Observed only via rendered output: after a single Enter at the start of the
/// file, the table must still have a `┌` top-border corner on screen.
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

    // -- Insert a blank line at the very top of the file ------------------
    // Move to the start of the buffer, then press Enter once.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Let the view settle (the corrupted state is itself stable, so this
    // returns either way — the assertion below is what distinguishes them).
    let mut prev = String::new();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            let stable = s == prev;
            prev = s;
            stable
        })
        .unwrap();

    // -- The regression check --------------------------------------------
    // With the bug, the top border `┌─┬─┐` is replaced by a separator
    // `├─┼─┤`, so no `┌` remains on screen.
    let after = harness.screen_to_string();
    assert!(
        after.contains('┌'),
        "table top border (┌) disappeared after inserting a line above the \
         table — it was reclassified as a `├` separator.\nScreen:\n{}",
        after,
    );
}
