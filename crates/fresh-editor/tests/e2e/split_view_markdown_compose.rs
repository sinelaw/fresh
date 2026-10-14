#[cfg(feature = "plugins")]
use crate::common::harness::HarnessOptions;
/// E2E tests for split view with markdown compose mode.
///
/// Tests the use case: same markdown document shown in two vertical splits,
/// left panel in source mode (plain), right panel in compose mode.
///
/// Key requirements tested:
/// 1. Compose mode only applies to the right panel (conceals, soft breaks)
/// 2. Line numbers visible in source panel, hidden in compose panel
/// 3. Scroll synchronization between panels
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// Build a markdown document with various formatting elements
fn build_test_markdown() -> String {
    let mut md = String::from("# Split View Test\n\n");
    md.push_str("## Introduction\n\n");
    md.push_str("This is a **bold text** and *italic text* with a [link](https://example.com) in the introduction paragraph.\n\n");
    for i in 0..30 {
        md.push_str(&format!(
            "Paragraph {}: Here is **bold** and *italic* text with a [link](https://example.com/p{}) to test compose mode rendering.\n\n",
            i, i
        ));
    }
    md.push_str("## Conclusion\n\n");
    md.push_str("Final paragraph with **bold** and *italic* text.\n");
    md
}

/// Set up a harness with the markdown_compose plugin loaded and a test file open.
fn setup_split_compose_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("test.md");
    std::fs::write(&md_path, build_test_markdown()).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .unwrap();

    // Open the file
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("test.md");

    (harness, temp_dir)
}

/// Helper: create a vertical split on the same buffer
fn create_vertical_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("split vert").unwrap();
    harness.wait_for_screen_contains("Split pane vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();
}

/// Helper: enable compose mode via command palette
fn enable_compose_mode(harness: &mut EditorTestHarness) {
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

    // Wait for compose mode to fully activate (conceals applied).
    // Only check the RIGHT half of the screen (the compose panel) for concealed
    // markers. The left panel stays in source mode and will always show ** markers.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            // Find the separator column (│)
            let sep_col = s
                .lines()
                .nth(2)
                .and_then(|l| l.char_indices().find(|(_, c)| *c == '│').map(|(i, _)| i));
            let Some(sep) = sep_col else {
                return false;
            };
            // Count ** markers only in the right half (compose panel)
            let right_bold_lines = s
                .lines()
                .skip(2)
                .filter(|l| {
                    if l.len() > sep + 1 {
                        l[sep + 1..].contains("**")
                    } else {
                        false
                    }
                })
                .count();
            right_bold_lines <= 2 // At most cursor line may show markers
        })
        .unwrap();
}

/// Helper: enable scroll sync via command palette
fn enable_scroll_sync(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Scroll Sync").unwrap();
    harness
        .wait_for_screen_contains("Toggle Scroll Sync")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

/// Helper: switch to next split via command palette
fn switch_to_next_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("next split").unwrap();
    harness.wait_for_screen_contains("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

/// Run a command-palette command by typing a query and accepting the first hit.
#[cfg(feature = "plugins")]
fn run_palette_command(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(query).unwrap();
    harness.wait_for_screen_contains(query).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

/// Regression: disabling compose/preview mode in one split must put THAT split
/// back into source mode (raw markdown), even when another split on the same
/// buffer is still in compose mode.
///
/// Compose decorations — table cell conceals (`|` → `│`, padding), soft breaks,
/// and the `┌─┬─┐` border virtual lines — live on the *buffer*, not the split,
/// and `markdown_compose` re-emits them on every `lines_changed` as long as
/// `isComposingInAnySplit` is true. `view_mode`, however, is *per split*
/// (`handle_set_view_mode` writes only the active split's buffer state). So when
/// one split is toggled back to source while a sibling split keeps composing,
/// the still-composing split re-adds the buffer-global decorations and the
/// source split renders them too: it shows the boxed/concealed table instead of
/// the raw `| Name | Role | City |` source.
///
/// Reproduced interactively in tmux: the source split keeps its line-number
/// gutter (proving it *is* in source mode) yet still draws the framed table.
///
/// The invariant asserted here, on rendered output only: with one split in
/// compose and one in source, the screen must contain BOTH the compose frame
/// (`┌`, from the composing split) AND at least one raw ASCII `|` (from the
/// source split's table). A compose table conceals every `|` to `│`, so a screen
/// with zero ASCII `|` means the source split is wrongly rendering the table as
/// composed — the bug.
#[cfg(feature = "plugins")]
#[test]
fn test_disable_compose_in_one_split_restores_source_in_that_split() {
    init_tracing_from_env();

    // Small table near the top so both splits show it from the first line.
    let md = "\
# Title

Some text before the table.

| Name | Role | City |
| --- | --- | --- |
| Alice | Engineer | London |
| Bob | Designer | Paris |
| Carol | Manager | Berlin |

Text after the table.
";
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("table.md");
    std::fs::write(&md_path, md).unwrap();

    // Wide terminal so a 50/50 vertical split leaves each pane wide enough to
    // show the whole table row.
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 40, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("table.md");

    // 1. Enable compose on the single view. The table renders framed and every
    //    raw '|' is concealed.
    run_palette_command(&mut harness, "Toggle Compose");
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();
    assert!(
        !harness.screen_to_string().contains('|'),
        "compose mode should conceal every raw '|' before splitting.\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // 2. Split vertically. The new (right) split becomes active and defaults to
    //    SOURCE mode (`SplitViewState::with_buffer`); the left split keeps
    //    composing.
    run_palette_command(&mut harness, "Split Vertical");
    harness.wait_for_async_quiescence(6).unwrap();

    // 3. Toggle compose ON then OFF in the active (right) split: the right split
    //    starts in source, so the first toggle composes it (both splits now
    //    compose) and the second runs the disable path — clearing the buffer's
    //    compose decorations — while the left split is still composing and
    //    immediately re-adds them.
    run_palette_command(&mut harness, "Toggle Compose"); // right -> compose
    harness.wait_for_async_quiescence(6).unwrap();
    run_palette_command(&mut harness, "Toggle Compose"); // right -> source
    harness.wait_for_async_quiescence(10).unwrap();

    // -- The regression check -------------------------------------------------
    let screen = harness.screen_to_string();

    // The left split is still composing, so its frame must be present.
    assert!(
        screen.contains('┌'),
        "the still-composing split should keep its table frame.\nScreen:\n{}",
        screen,
    );

    // The right split is in source mode (it owns the active line-number gutter),
    // so the raw markdown table — with ASCII '|' separators — must be visible.
    // If the whole screen has no ASCII '|', the source split is wrongly drawing
    // the composed (concealed) table: the bug.
    assert!(
        screen.contains('|'),
        "disabling compose in one split left no raw '|' on screen: the source-mode \
         split is still rendering the composed/concealed table because the sibling \
         split is composing and the buffer-global decorations leak across splits.\n\
         Screen:\n{}",
        screen,
    );
}

/// Test that compose mode only affects the active split, not both panels.
///
/// Setup: vertical split of same markdown file
/// - Right panel (initially active after split): enable compose mode
/// - Left panel: should remain in source mode
///
/// Expected: Left panel shows raw markdown markers (**bold**, *italic*, [link](...))
///           Right panel conceals markers (shows formatted text)
// TODO: fix wait conditions in enable_compose_mode — they timeout because the
// separator lookup / byte slicing doesn't match actual rendered output.
#[test]
#[ignore]
fn test_split_view_compose_only_in_one_panel() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split - new (right) split becomes active
    create_vertical_split(&mut harness);

    // Enable compose mode in the right panel
    enable_compose_mode(&mut harness);

    let screen = harness.screen_to_string();

    // The screen has a vertical split. Find the separator column.
    // In a 160-wide terminal with 50/50 split, separator is around col 80.
    let first_content_line = screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // Left half (source mode) should show raw markdown markers
        let left_half: String = screen
            .lines()
            .skip(2)
            .take(30)
            .map(|l| if l.len() > sep { &l[..sep] } else { l })
            .collect::<Vec<_>>()
            .join("\n");

        // Right half (compose mode) should have concealed markers
        let right_half: String = screen
            .lines()
            .skip(2)
            .take(30)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        // Source mode (left) should show ** markers
        let left_bold_count = left_half.matches("**").count();
        // Compose mode (right) should conceal ** markers
        let right_bold_count = right_half.matches("**").count();

        assert!(
            left_bold_count > right_bold_count,
            "Source panel (left) should show more ** markers than compose panel (right).\n\
             Left ** count: {}, Right ** count: {}\n\
             Left panel:\n{}\n\nRight panel:\n{}",
            left_bold_count,
            right_bold_count,
            left_half,
            right_half,
        );
    }
}

/// Test that line numbers appear in source panel but not in compose panel.
///
/// When compose mode is enabled, line numbers should be hidden in that split
/// but remain visible in the source-mode split.
// TODO: fix wait conditions in enable_compose_mode — see test_split_view_compose_only_in_one_panel
#[test]
#[ignore]
fn test_split_view_line_numbers_per_split() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split
    create_vertical_split(&mut harness);

    // Enable compose mode in right panel
    enable_compose_mode(&mut harness);

    let screen = harness.screen_to_string();

    // Find separator
    let first_content_line = screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // Left half should have line numbers (e.g., "  1 │")
        let left_half: String = screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep { &l[..sep] } else { l })
            .collect::<Vec<_>>()
            .join("\n");

        // Right half should NOT have line numbers in compose mode
        let right_half: String = screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        // Source mode (left) should have line number patterns like "  1 │" or " 10 │"
        let left_has_line_numbers = left_half.lines().any(|l| {
            let trimmed = l.trim_start();
            trimmed.starts_with("1 ") || trimmed.starts_with("2 ") || trimmed.starts_with("3 ")
        });

        assert!(
            left_has_line_numbers,
            "Source panel (left) should show line numbers.\nLeft panel:\n{}",
            left_half,
        );

        // Compose mode (right) should NOT start lines with line numbers
        // In compose mode, lines start with the actual content (possibly indented)
        let right_has_line_numbers = right_half
            .lines()
            .filter(|l| !l.trim().is_empty())
            .any(|l| {
                let trimmed = l.trim_start();
                // Line numbers look like "  1 │" at the start
                trimmed.len() > 4
                    && trimmed
                        .chars()
                        .take(4)
                        .all(|c| c.is_ascii_digit() || c == ' ')
                    && trimmed.chars().nth(4) == Some('│')
            });

        assert!(
            !right_has_line_numbers,
            "Compose panel (right) should NOT show line numbers.\nRight panel:\n{}",
            right_half,
        );
    }
}

/// Test that scrolling in one split moves the view in the other split.
///
/// Both panels show the same buffer, so scrolling down in one panel
/// should move the other panel to show the same source lines.
// TODO: fix wait conditions in enable_compose_mode — see test_split_view_compose_only_in_one_panel
#[test]
#[ignore]
fn test_split_view_scroll_sync() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split
    create_vertical_split(&mut harness);

    // Enable compose mode in right panel
    enable_compose_mode(&mut harness);

    // Enable scroll sync (off by default)
    enable_scroll_sync(&mut harness);

    // Switch to left (source) panel
    switch_to_next_split(&mut harness);

    // Capture the "before scroll" state
    let before_screen = harness.screen_to_string();

    // Scroll down significantly in the left panel
    for _ in 0..25 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    harness.render().unwrap();

    let after_screen = harness.screen_to_string();

    // The screen should have changed (scrolled)
    assert_ne!(
        before_screen, after_screen,
        "Screen should change after scrolling down 25 lines"
    );

    // Find separator
    let first_content_line = after_screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // The right panel should also have scrolled
        let right_after: String = after_screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        let right_before: String = before_screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        assert_ne!(
            right_before, right_after,
            "Right panel (compose) should also scroll when left panel (source) scrolls.\n\
             Right panel before:\n{}\n\nRight panel after:\n{}",
            right_before, right_after,
        );
    }
}

/// The rows of `pane`, clipped to its own columns — so an assertion about one
/// pane cannot be satisfied (or broken) by what its neighbour drew.
#[cfg(feature = "plugins")]
fn pane_rows(harness: &EditorTestHarness, pane: fresh::model::event::LeafId) -> Vec<String> {
    let rect = harness
        .editor()
        .pane_content_rect(pane)
        .expect("the shell laid the pane out");
    harness
        .screen_to_string()
        .lines()
        .skip(rect.y as usize)
        .take(rect.height as usize)
        .map(|line| {
            line.chars()
                .skip(rect.x as usize)
                .take(rect.width as usize)
                .collect()
        })
        .collect()
}

/// A project with the `markdown_compose` plugin and one markdown file, opened.
///
/// `full_grammar` is what the code-frame assertions need: compose frames a
/// fenced block from the region classification the *Markdown grammar* makes, so
/// with the default empty registry there is no classification and — correctly —
/// no frame to assert about (see `markdown_compose_code_frame`).
#[cfg(feature = "plugins")]
fn compose_split_harness(md: &str, full_grammar: bool) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("doc.md");
    std::fs::write(&md_path, md).unwrap();

    let mut options = HarnessOptions::new()
        .with_working_dir(project_root.clone())
        .without_empty_plugins_dir();
    if full_grammar {
        options = options.with_full_grammar_registry();
    }
    let mut harness = EditorTestHarness::create(160, 40, options).unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    (harness, temp_dir)
}

/// Regression: a fenced code block must render as plain numbered lines in a
/// Source-mode split, even while a sibling split composes the same buffer.
///
/// Compose frames a code block with a `┌──┐` / `└──┘` border (conceals, already
/// gated on the split's mode) *and* with `│` side rails on every body line. The
/// rails are inline virtual text, which lives on the buffer and had no such
/// gate: a source split drew them around the raw code. Worse, the rails are
/// spliced in before wrapping, so each body line came out wide enough to fold —
/// and a folded row carries no line number, so the block's lines lost their
/// gutter entries too. That is exactly what the bug report described: "the ```
/// code blocks are missing the line number in the gutter and they have | |
/// borders inserted for them".
///
/// Asserted on rendered output only, and only on the source split's own
/// columns: its code line keeps its gutter number, and nothing past the gutter
/// is a rail.
#[cfg(feature = "plugins")]
#[test]
fn test_code_block_renders_plain_in_source_split_while_sibling_composes() {
    let md = "\
# Title

Some text before the code.

```rust
fn main() {
    println!(\"hello\");
}
```

Text after the code.
";
    let (mut harness, _tmp) = compose_split_harness(md, true);

    // Split first, then compose the LEFT split — so the right one, which stays
    // in source, is the split the assertions look at.
    run_palette_command(&mut harness, "Split Vertical");
    harness.wait_for_async_quiescence(6).unwrap();
    let source_pane = harness.editor().active_window().get_active_split();

    run_palette_command(&mut harness, "Previous Split");
    harness.render().unwrap();
    run_palette_command(&mut harness, "Toggle Compose");
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();

    // The composing split proves the frame is being emitted at all — without
    // this the test would pass on a buffer that simply has no decorations.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('┌'),
        "the composing split should frame the code block.\nScreen:\n{screen}",
    );

    let row = pane_rows(&harness, source_pane)
        .into_iter()
        .find(|r| r.contains("println!"))
        .unwrap_or_else(|| panic!("the source split never showed the code.\nScreen:\n{screen}"));
    let (gutter, text) = row
        .split_once('│')
        .unwrap_or_else(|| panic!("the source split drew no gutter: {row:?}.\nScreen:\n{screen}"));
    assert!(
        gutter.contains('7'),
        "the source split's code line lost its gutter line number: {row:?}.\n\
         Screen:\n{screen}",
    );
    assert!(
        !text.contains('│'),
        "the source split drew compose's code rails around the raw code: \
         {row:?}.\nScreen:\n{screen}",
    );
}

/// Regression: composing one split must not turn line wrap on in a sibling
/// split showing the same buffer in source mode.
///
/// `enableMarkdownCompose` turns native wrap on so a long unbreakable line
/// still folds inside the page. It used to ask for that with no split id, which
/// the editor reads as "every split showing this buffer" — and the hook runs on
/// `buffer_activated`, so merely moving focus onto the composing split rewrote
/// the source split's wrap setting, undoing the user's own "Toggle Line Wrap
/// (Current Buffer)".
#[cfg(feature = "plugins")]
#[test]
fn test_composing_one_split_leaves_a_sibling_splits_line_wrap_alone() {
    // One line far wider than either pane, so wrap on / off is visible as a
    // different number of rows carrying its text.
    let long = "word ".repeat(60);
    // `## Section` is the compose signal below — deliberately not line 1, which
    // is where the caret sits: compose reveals the markup on the caret's own
    // row, so a heading there stays raw forever and waiting on it never ends.
    let (mut harness, _tmp) =
        compose_split_harness(&format!("# Title\n\n{long}\n\n## Section\n"), false);

    // Rows of `pane` carrying the long line: 1 with wrap off, several with it on.
    fn long_line_rows(harness: &EditorTestHarness, pane: fresh::model::event::LeafId) -> usize {
        pane_rows(harness, pane)
            .iter()
            .filter(|r| r.contains("word"))
            .count()
    }

    run_palette_command(&mut harness, "Split Vertical");
    harness.wait_for_async_quiescence(6).unwrap();
    let source_pane = harness.editor().active_window().get_active_split();

    // The new right split is active. Get its wrap *off*, whichever way
    // `editor.line_wrap` happens to default — asserting the toggle's direction
    // instead made this depend on the harness's config, and a render taken
    // before the toggle had settled read the old layout and let the setup pass
    // while leaving wrap on.
    if long_line_rows(&harness, source_pane) != 1 {
        run_palette_command(&mut harness, "Toggle Line Wrap (Current");
    }
    harness
        .wait_until_stable(|h| long_line_rows(h, source_pane) == 1)
        .unwrap();

    // Compose the LEFT split, then come back. Both moves fire
    // `buffer_activated`, which is where the wrap request lives.
    run_palette_command(&mut harness, "Previous Split");
    harness.render().unwrap();
    let compose_pane = harness.editor().active_window().get_active_split();
    assert_ne!(
        compose_pane, source_pane,
        "the test needs focus on the other split before composing"
    );
    run_palette_command(&mut harness, "Toggle Compose");
    // Wait for the sibling to be *actually* composing rather than for a timer:
    // compose conceals a heading's `##` markers, so a `Section` heading that
    // still reads `## Section` is a pane that is not composing yet. The scenario
    // does not exist until then, and a run where the toggle never landed would
    // otherwise report on an invariant it never exercised.
    harness
        .wait_until_stable(|h| {
            pane_rows(h, compose_pane)
                .iter()
                .any(|r| r.contains("Section") && !r.contains("## Section"))
        })
        .unwrap();
    run_palette_command(&mut harness, "Next Split");
    harness.wait_for_async_quiescence(6).unwrap();

    let rows_now = long_line_rows(&harness, source_pane);
    assert_eq!(
        rows_now,
        1,
        "composing the sibling split turned line wrap back on in the source \
         split: its long line now occupies {rows_now} rows instead of 1.\
         \nScreen:\n{}",
        harness.screen_to_string(),
    );
}

#[cfg(feature = "plugins")]
fn visible_panes(harness: &EditorTestHarness) -> Vec<fresh::model::event::LeafId> {
    let mut leaves: Vec<_> = harness
        .editor()
        .active_window()
        .split_manager()
        .visible_leaves()
        .into_iter()
        .map(|(l, _)| l)
        .collect();
    leaves.sort_by_key(|&l| {
        harness
            .editor()
            .pane_content_rect(l)
            .map(|r| r.x)
            .unwrap_or(0)
    });
    leaves
}

/// Test Joplin-style 3 markdown viewing modes:
/// Mode 1 (Source): Raw unrendered text editor.
/// Mode 2 (Rendered): Solely rendered Markdown preview.
/// Mode 3 (Duo): Half unrendered source, half rendered preview side-by-side.
/// Also tests cycling (Source -> Rendered -> Duo -> Source) and toggling duo mode.
#[cfg(feature = "plugins")]
#[test]
fn test_joplin_markdown_three_modes_cycle_and_toggle() {
    let md = "# Joplin Mode Test\n\nHere is **bold text** and *italic text* in a paragraph.\n";
    let (mut harness, _tmp) = compose_split_harness(md, false);

    // Initial state: Mode 1 (Source)
    // Single split, shows raw markdown symbols like **bold text**
    assert_eq!(
        visible_panes(&harness).len(),
        1,
        "initially should have 1 split in Source mode"
    );
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("**bold text**"))
        .unwrap();

    // 1. Cycle to Mode 2: Rendered Mode
    run_palette_command(&mut harness, "Markdown: Cycle View Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    // In Rendered Mode: still 1 split, but compose mode hides raw **bold text** markers
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            visible_panes(h).len() == 1 && !s.contains("**bold text**") && s.contains("bold text")
        })
        .unwrap();

    // 2. Cycle to Mode 3: Duo Mode
    run_palette_command(&mut harness, "Markdown: Cycle View Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    // In Duo Mode: 2 splits side by side.
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    // Left pane is Source mode (shows **bold text**), right pane is Rendered mode (conceals **)
    harness
        .wait_until_stable(|h| {
            let left_text = pane_rows(h, left_pane).join(" ");
            let right_text = pane_rows(h, right_pane).join(" ");
            left_text.contains("**bold text**")
                && right_text.contains("bold text")
                && !right_text.contains("**bold text**")
        })
        .unwrap();

    // Both panes in Duo mode must have line wrap disabled
    let left_wrap = harness
        .editor()
        .active_window()
        .split_view_states()
        .get(&left_pane)
        .unwrap()
        .buffer_tab_state()
        .viewport
        .line_wrap_enabled;
    let right_wrap = harness
        .editor()
        .active_window()
        .split_view_states()
        .get(&right_pane)
        .unwrap()
        .buffer_tab_state()
        .viewport
        .line_wrap_enabled;
    assert!(
        !left_wrap,
        "Left pane in Duo mode should have line wrap disabled"
    );
    assert!(
        !right_wrap,
        "Right pane in Duo mode should have line wrap disabled"
    );

    // 3. Cycle to Mode 1: back to Source Mode
    run_palette_command(&mut harness, "Markdown: Cycle View Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| {
            visible_panes(h).len() == 1 && h.screen_to_string().contains("**bold text**")
        })
        .unwrap();

    // 4. Test explicit Toggle Duo command:
    // From Source mode -> toggles into Duo mode
    run_palette_command(&mut harness, "Markdown: Toggle Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    // Calling Toggle Duo again -> returns to Source mode (single split)
    run_palette_command(&mut harness, "Markdown: Toggle Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| {
            visible_panes(h).len() == 1 && h.screen_to_string().contains("**bold text**")
        })
        .unwrap();
}

/// Test that in Duo mode, the horizontal scrollbar synchronizes both views simultaneously:
/// scrolling or dragging the horizontal scrollbar on one view moves both views in lockstep.
#[cfg(feature = "plugins")]
#[test]
fn test_joplin_duo_mode_horizontal_scrollbar_sync() {
    use crossterm::event::{MouseEvent, MouseEventKind};

    // A long line that exceeds split width so horizontal scrolling occurs
    let long_segment = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let wide_line = format!(
        "# Wide Header\n\nSTART_MARKER {} {} {} END_MARKER\n",
        long_segment.repeat(5),
        long_segment.repeat(5),
        long_segment.repeat(5)
    );

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("wide.md");
    std::fs::write(&md_path, &wide_line).unwrap();

    let mut config = fresh::config::Config::default();
    config.editor.show_horizontal_scrollbar = true;

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 30, config, project_root).unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Switch to Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    let get_left_cols = |h: &EditorTestHarness| -> (usize, usize) {
        let win = h.editor().active_window();
        let vs_map = win.split_view_states();
        let l_col = vs_map
            .get(&left_pane)
            .unwrap()
            .buffer_tab_state()
            .viewport
            .left_column;
        let r_col = vs_map
            .get(&right_pane)
            .unwrap()
            .buffer_tab_state()
            .viewport
            .left_column;
        (l_col, r_col)
    };

    // Both splits start with left_column == 0
    let (l0, r0) = get_left_cols(&harness);
    assert_eq!(l0, 0, "Initial left split left_column should be 0");
    assert_eq!(r0, 0, "Initial right split left_column should be 0");

    // Both panes must have visible START_MARKER before scrolling
    let left_rows_before = pane_rows(&harness, left_pane).join(" ");
    let right_rows_before = pane_rows(&harness, right_pane).join(" ");
    assert!(
        left_rows_before.contains("START_MARKER"),
        "Left pane should show start marker"
    );
    assert!(
        right_rows_before.contains("START_MARKER"),
        "Right pane should show start marker"
    );

    // 1. Test mouse wheel horizontal scrolling on LEFT pane (ScrollRight)
    let left_rect = harness.editor().pane_content_rect(left_pane).unwrap();
    for _ in 0..10 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::ScrollRight,
                column: left_rect.x + 5,
                row: left_rect.y + 2,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
        harness.render().unwrap();
    }

    let (l1, r1) = get_left_cols(&harness);
    assert!(
        l1 > 0,
        "Left pane should have scrolled horizontally (l1 = {l1})"
    );
    assert_eq!(
        l1, r1,
        "Right pane should synchronize left_column with Left pane (l1={l1}, r1={r1})"
    );

    // 2. Test mouse wheel horizontal scrolling on RIGHT pane (ScrollRight)
    let right_rect = harness.editor().pane_content_rect(right_pane).unwrap();
    for _ in 0..10 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::ScrollRight,
                column: right_rect.x + 5,
                row: right_rect.y + 2,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
        harness.render().unwrap();
    }

    let (l2, r2) = get_left_cols(&harness);
    assert!(
        l2 > l1,
        "Left pane should have scrolled further (l2 = {l2})"
    );
    assert_eq!(
        l2, r2,
        "Both panes should synchronize when right pane is scrolled (l2={l2}, r2={r2})"
    );

    // Verify rendered screen changes on both sides:
    // START_MARKER has scrolled out of view on both left and right panes
    let left_rows_scrolled = pane_rows(&harness, left_pane).join(" ");
    let right_rows_scrolled = pane_rows(&harness, right_pane).join(" ");
    assert!(
        !left_rows_scrolled.contains("START_MARKER"),
        "START_MARKER should have scrolled off-screen on left pane"
    );
    assert!(
        !right_rows_scrolled.contains("START_MARKER"),
        "START_MARKER should have scrolled off-screen on right pane"
    );

    // 3. Test horizontal scrollbar track click / drag
    let left_bar = harness
        .editor()
        .pane_hscroll_rect(left_pane)
        .expect("left horizontal scrollbar rect must be present");
    let start_x = left_bar.x + 1;
    let end_x = left_bar.x + left_bar.width.min(20);
    let bar_y = left_bar.y;

    harness.mouse_drag(start_x, bar_y, end_x, bar_y).unwrap();
    harness.render().unwrap();

    let (l3, r3) = get_left_cols(&harness);
    assert_eq!(
        l3, r3,
        "Dragging horizontal scrollbar on left split should keep both splits in lockstep (l3={l3}, r3={r3})"
    );
    assert!(
        l3 != l2,
        "Dragging horizontal scrollbar on left split should have changed scroll position (l3={l3}, l2={l2})"
    );

    let right_bar = harness
        .editor()
        .pane_hscroll_rect(right_pane)
        .expect("right horizontal scrollbar rect must be present");
    let start_x = right_bar.x + 1;
    let end_x = right_bar.x + right_bar.width.min(15);
    let bar_y = right_bar.y;

    harness.mouse_drag(start_x, bar_y, end_x, bar_y).unwrap();
    harness.render().unwrap();

    let (l4, r4) = get_left_cols(&harness);
    assert_eq!(
        l4, r4,
        "Dragging horizontal scrollbar on right split should keep both splits in lockstep (l4={l4}, r4={r4})"
    );
    assert!(
        l4 != l3,
        "Dragging horizontal scrollbar on right split should have changed scroll position (l4={l4}, l3={l3})"
    );
}

/// Test entering Duo mode when 3 splits are open on the same buffer:
/// it should close extraneous splits so exactly 2 splits remain configured in Duo mode
/// (left in source mode, right in compose mode), without creating an extra dangling split.
#[cfg(feature = "plugins")]
#[test]
fn test_joplin_duo_mode_from_three_splits() {
    let md = "# Three Splits Test\n\n**bold content** here\n";
    let (mut harness, _tmp) = compose_split_harness(md, false);

    // Create 2 vertical splits so we have 3 splits total
    run_palette_command(&mut harness, "Split Vertical");
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();
    run_palette_command(&mut harness, "Split Vertical");
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 3)
        .unwrap();

    // Now invoke Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    // Must have exactly 2 splits (the 3rd split was closed)
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    harness
        .wait_until_stable(|h| {
            let left_text = pane_rows(h, left_pane).join(" ");
            let right_text = pane_rows(h, right_pane).join(" ");
            left_text.contains("**bold content**")
                && right_text.contains("bold content")
                && !right_text.contains("**bold content**")
        })
        .unwrap();
}

/// Test transition from Rendered Mode to Duo Mode:
/// verifies that the preview pane preserves its conceals and decorations
/// without them being wiped out when the left pane is placed into source mode.
#[cfg(feature = "plugins")]
#[test]
fn test_joplin_rendered_to_duo_mode_preserves_conceals() {
    let md = "# Heading 1\n\nLine with **bold text** and [link](https://fresh.dev)\n";
    let (mut harness, _tmp) = compose_split_harness(md, false);

    // Switch to Rendered Mode (single split in compose mode)
    run_palette_command(&mut harness, "Markdown: Rendered Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            visible_panes(h).len() == 1 && !s.contains("**bold text**") && s.contains("bold text")
        })
        .unwrap();

    // Now switch directly to Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    // Left pane has raw source (**bold text**), right pane has rendered preview (concealed **)
    harness
        .wait_until_stable(|h| {
            let left_text = pane_rows(h, left_pane).join(" ");
            let right_text = pane_rows(h, right_pane).join(" ");
            left_text.contains("**bold text**")
                && right_text.contains("bold text")
                && !right_text.contains("**bold text**")
        })
        .unwrap();

    // Now switch back to Rendered Mode: should collapse to 1 split in compose mode
    run_palette_command(&mut harness, "Markdown: Rendered Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            visible_panes(h).len() == 1 && !s.contains("**bold text**") && s.contains("bold text")
        })
        .unwrap();
}

/// Test closing a split in Duo mode and cycling modes:
/// When one split of Duo mode is closed manually, cycling the mode should
/// cleanly proceed without crashing or hanging due to leaked scroll sync groups.
#[cfg(feature = "plugins")]
#[test]
fn test_joplin_duo_mode_split_closure_and_cycle() {
    let md = "# Split Close Test\n\nSome **formatted** text here.\n";
    let (mut harness, _tmp) = compose_split_harness(md, false);

    // Enter Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    // Focus the right pane and close it
    run_palette_command(&mut harness, "Next Split");
    harness.render().unwrap();
    run_palette_command(&mut harness, "Close current split pane");
    harness
        .wait_until_stable(|h| visible_panes(h).len() == 1)
        .unwrap();

    // Cycle view mode: since surviving pane is source, should cycle to Rendered Mode
    run_palette_command(&mut harness, "Markdown: Cycle View Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            visible_panes(h).len() == 1 && !s.contains("**formatted**") && s.contains("formatted")
        })
        .unwrap();
}

/// Test that markdown tables render properly in Duo mode:
/// The left pane displays raw source with pipes and delimiters,
/// while the right pane renders table borders (┌, ├, │, └) and conceals syntax.
#[cfg(feature = "plugins")]
#[test]
fn test_duo_mode_table_renders_properly() {
    let md = "\
### 🎯 Desglose de \"Hard Negatives\" y Brechas Detectadas

| Subárea | Precisión | \"No Idea\" | Latencia Promedio | Diagnóstico de Conceptos Fallados |
|---|---|---|---|---|
| **2.3 Redes** | **0.0%** (0/3) | 2 | 98.8 s | Direccionamiento IPv6 |
| **2.2 Compiladores** | **0.0%** (0/4) | 1 | 88.4 s | Análisis léxico |
";
    let (mut harness, _tmp) = compose_split_harness(md, false);

    // Enter Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    harness
        .wait_until_stable(|h| {
            let left_text = pane_rows(h, left_pane).join(" ");
            let right_text = pane_rows(h, right_pane).join(" ");

            // Left pane has raw source table syntax
            let left_ok = left_text.contains("|---|") && left_text.contains("**2.3 Redes**");
            // Right pane has rendered borders and conceals raw delimiters
            let right_ok = right_text.contains('┌')
                && right_text.contains('└')
                && !right_text.contains("|---|")
                && right_text.contains("2.3 Redes");
            left_ok && right_ok
        })
        .unwrap();
}

#[cfg(feature = "plugins")]
#[test]
fn test_duo_mode_full_demo_md_table_renders() {
    let md = r#"# 📊 Diagnóstico Psicométrico & Prompts Quirúrgicos para Gemini Web (EGEL Plus CENEVAL)

**Fecha de Evaluación del Diagnóstico:** 13 de octubre de 2026  
**Fecha del Examen Oficial:** 24 de octubre de 2026 (⏱️ **10 días restantes**)  
**Sustentante:** Estudiante de Ingeniería en Computación  

---

## 🔍 1. Radiografía de Resultados (Época 1: 13 de Octubre)

En la última simulación de 50 reactivos representativos, obtuviste **25 aciertos de 50 (50.0% de precisión global)** con una latencia promedio de **131.8 segundos por reactivo** y **10 reactivos marcados con "No tengo idea" (20% de incertidumbre epistémica)**.

```
                    PRECISIÓN GLOBAL: 50.0% (25 / 50)
 ┌──────────────────────────────────────────────────────────────┐
 │ Área 5: Transversal (Comprensión y Redacción)   100.0% [3/3] │ 🟢 DOMINIO
 │ Área 4: IA, Minería y Cómputo Distribuido        55.6% [5/9] │ 🟡 EN RIESGO
 │ Área 3: Ing. Software y Bases de Datos           53.8% [7/13]│ 🟡 EN RIESGO
 │ Área 1: Algoritmos, Estructuras y Matemáticas    53.3% [8/15]│ 🟡 EN RIESGO
 │ Área 2: Sistemas, Compiladores y Redes           20.0% [2/10]│ 🔴 ALERTA ROJA
 └──────────────────────────────────────────────────────────────┘
```

> [!CAUTION]
> **Alerta Crítica en Área 2 (Sistemas de Cómputo e Infraestructura):**
> Obtuviste solo un **20.0% de precisión**. En particular, **Compiladores (2.2)** y **Redes (2.3)** tuvieron **0% de aciertos**. En CENEVAL, reprobar un área disciplinar compromete el dictamen de *Sobresaliente* o *Satisfactorio*. Los siguientes 10 días deben priorizar este cuello de botella.

---

### 🎯 Desglose de "Hard Negatives" y Brechas Detectadas

| Subárea | Precisión | "No Idea" | Latencia Promedio | Diagnóstico de Conceptos Fallados |
|---|---|---|---|---|
| **2.3 Redes** | **0.0%** (0/3) | 2 | 98.8 s | Direccionamiento IPv6 (reglas de compresión `::`), Enrutamiento OSPF (métrica de costo y Área 0 troncal), DNS recursivo vs iterativo. |
| **2.2 Compiladores** | **0.0%** (0/4) | 1 | 88.4 s | Análisis léxico (token, lexema, patrón), SDD (atributos sintetizados vs heredados), Tabla de símbolos jerárquica por ámbitos, Enlace dinámico (`.so`/PIC/PLT/GOT). |
| **4.2 Minería de Datos** | **33.3%** (1/3) | 1 | 183.6 s | Reglas de asociación (Soporte, Confianza, Lift), Método del Codo (WCSS en K-Means). Latencia muy alta (3 min/ítem). |
| **1.2 Estructuras de Datos** | **33.3%** (1/3) | 1 | 126.8 s | B+ Tree vs Hash Index (búsquedas por rango vs igualdad), Colas circulares con aritmética modular (`(rear+1)%N`). |
| **3.4 Seguridad** | **33.3%** (1/3) | 1 | 62.0 s | Modelos de control de acceso (DAC vs MAC vs RBAC), Cabeceras de seguridad HTTP (`Content-Security-Policy` contra XSS). |
| **1.1 Algoritmos** | **50.0%** (2/4) | 1 | 143.8 s | Búsqueda de patrones (KMP y tabla LPS), Factor de carga en Tablas Hash ($\alpha = n/m$, $O(1+\alpha)$ promedio vs $O(n)$ peor caso). |
| **1.4 Lógica Computacional** | **50.0%** (2/4) | 1 | 104.9 s | Implicantes Primos Esenciales (EPIs en Karnaugh/Quine-McCluskey), Función lógica de Multiplexores (MUX 4:1). |
| **3.3 Bases de Datos** | **50.0%** (2/4) | 1 | 142.1 s | Filtrado con agregados (`WHERE` vs `HAVING`), Bases de datos NoSQL Clave-Valor (Redis para alta concurrencia de sesiones). |
| **3.2 Lenguajes de Prog.** | **66.7%** (2/3) | 0 | 118.4 s | Modificador `static` en C/C++: duración de almacenamiento estática en data segment vs ámbito léxico local. |
| **4.3 Cómputo Distribuido** | **66.7%** (2/3) | 1 | 138.4 s | Modelo de fallas de Cristian/Hadzilacos (Falla bizantina/arbitraria). |
"#;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("doc.md");
    std::fs::write(&md_path, &md).unwrap();

    let options = HarnessOptions::new()
        .with_working_dir(project_root.clone())
        .without_empty_plugins_dir()
        .with_full_grammar_registry();

    // 120 width like user's terminal (each split ~60 cols)
    let mut harness = EditorTestHarness::create(120, 30, options).unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Enter Duo Mode
    run_palette_command(&mut harness, "Markdown: Duo Mode");
    harness.wait_for_async_quiescence(6).unwrap();

    harness
        .wait_until_stable(|h| visible_panes(h).len() == 2)
        .unwrap();

    // Scroll down to the table (table is at line 32)
    for _ in 0..35 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.wait_for_async_quiescence(6).unwrap();

    let leaves = visible_panes(&harness);
    let left_pane = leaves[0];
    let right_pane = leaves[1];

    let left_text = pane_rows(&harness, left_pane).join("\n");
    let right_text = pane_rows(&harness, right_pane).join("\n");

    eprintln!("LEFT PANE:\n{left_text}");
    eprintln!("RIGHT PANE:\n{right_text}");

    // Right pane should have rendered table with borders
    assert!(
        right_text.contains('┌'),
        "Right pane must have top border ┌:\n{right_text}"
    );
    assert!(
        right_text.contains('└'),
        "Right pane must have bottom border └:\n{right_text}"
    );
    assert!(
        !right_text.contains("|---|"),
        "Right pane delimiter row must be concealed:\n{right_text}"
    );
}
