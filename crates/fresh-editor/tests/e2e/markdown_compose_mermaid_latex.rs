//! E2E tests for Mermaid diagrams and LaTeX math in compose mode.
//!
//! Compose does not draw either itself: it finds them and draws what the
//! command its `renderers` map names for the block's language prints. These
//! tests stand stub scripts in for the tools, so what the screen shows is
//! known exactly — `M<formula>` for math, `DIAGRAM<first line>` for a
//! diagram — and check that compose finds every delimiter, frames the tool's
//! rows in place of the source, shows the source again while the cursor is in
//! the block, leaves the source alone where there is no tool (or it fails),
//! and draws only in compose panes.
//!
//! The stubs are shell scripts, so the tests that need one run on Unix. Each
//! check looks for text only the drawing produces and waits for the screen it
//! expects (CONTRIBUTING.md Testing §2, §3).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};

const WIDTH: u16 = 140;

/// The plugin's settings: the `renderers` map given, empty by default, so a
/// machine with `termaid` or `utftex` installed behaves like one without.
fn compose_config(renderers: serde_json::Value) -> Config {
    let mut config = Config::default();
    config.plugins.insert(
        "markdown_compose".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "renderers": renderers }),
        },
    );
    config
}

fn harness_with_renderers(
    md: &str,
    renderers: serde_json::Value,
) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("test.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::create(
        WIDTH,
        35,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry()
            .with_config(compose_config(renderers)),
    )
    .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.wait_for_screen_contains("test.md").unwrap();
    (harness, temp_dir)
}

fn run(h: &mut EditorTestHarness, command: &str) {
    // Enter only once the palette lists the command: its list follows the
    // typing a step behind, and Enter before then runs whatever was first.
    h.run_palette_command(command).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

fn screen_has(h: &EditorTestHarness, text: &str) -> bool {
    h.screen_to_string().contains(text)
}

/// With no tool, math and diagrams are shown as written: the inline formula
/// with its dollars, display math as its line, a Mermaid fence as code.
#[test]
fn without_a_tool_math_and_diagrams_show_their_source() {
    let md = "# Title\n\nSee $x^2$ here.\n\n$$E = mc^2$$\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
    let (mut h, _tmp) = harness_with_renderers(
        md,
        serde_json::json!({
            "math": { "command": "fresh-test-no-such-renderer", "args": ["{source}"] },
            "mermaid": { "command": "fresh-test-no-such-renderer", "args": ["{file}"] },
        }),
    );
    run(&mut h, "Markdown: Toggle Compose");
    h.wait_until(|h| {
        screen_has(h, "See $x^2$ here.")
            && screen_has(h, "$$E = mc^2$$")
            && screen_has(h, "graph TD")
            && screen_has(h, "A --> B")
            && !screen_has(h, "math ·")
    })
    .unwrap();
}

#[cfg(unix)]
mod with_stub_tools {
    use super::*;

    /// A stand-in renderer: a script written into `dir`.
    fn stub(dir: &std::path::Path, name: &str, body: &str) -> String {
        use std::os::unix::fs::PermissionsExt;
        let path = dir.join(name);
        std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        path.to_string_lossy().into_owned()
    }

    /// The default table with both stubs in it, at the arguments the
    /// defaults pass: math `--line-width {width} -- {source}` (also for
    /// `latex`), Mermaid `{file} --width {width}`.
    /// Math prints `M<formula>` (a formula over several lines on one row),
    /// with a row above and below when it holds a `\frac`, as a 2-D layout
    /// would. Mermaid prints its first line and a row count, and fails on a
    /// `gantt`, as a tool does on a diagram type it does not know.
    fn stubs(dir: &std::path::Path) -> serde_json::Value {
        let math = stub(
            dir,
            "stub-math",
            r#"for a; do last=$a; done
f=$(printf '%s' "$last" | tr '\n' ' ')
case "$f" in *'\frac'*) printf 'FRAC TOP\nM<%s>\nFRAC BOTTOM\n' "$f" ;; *) printf 'M<%s>\n' "$f" ;; esac"#,
        );
        let mermaid = stub(
            dir,
            "stub-mermaid",
            r#"grep -q gantt "$1" && exit 3
printf 'DIAGRAM<%s>\nDIAGRAM %s lines\n' "$(head -n 1 "$1")" "$(wc -l < "$1" | tr -d ' ')""#,
        );
        serde_json::json!({
            "math": { "command": math, "args": ["--line-width", "{width}", "--", "{source}"] },
            "latex": { "command": math, "args": ["--line-width", "{width}", "--", "{source}"] },
            "mermaid": { "command": mermaid, "args": ["{file}", "--width", "{width}"] },
        })
    }

    fn compose_with_stubs(md: &str) -> (EditorTestHarness, tempfile::TempDir, tempfile::TempDir) {
        let tools = tempfile::TempDir::new().unwrap();
        let (mut h, tmp) = harness_with_renderers(md, stubs(tools.path()));
        run(&mut h, "Markdown: Toggle Compose");
        (h, tmp, tools)
    }

    /// Inline math in every delimiter — `$…$`, `\(…\)`, GitLab's $`…`$ —
    /// is replaced by the tool's drawing when that is one row. A formula the
    /// tool draws on several rows stays as written.
    #[test]
    fn inline_math_is_drawn_by_the_tool_when_it_is_one_row() {
        let md = "# Title\n\nA $x^2$, b \\(\\pi r\\), c $`\\beta_1`$, d $\\frac{a}{b}$.\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "A M<x^2>, b M<\\pi r>, c M<\\beta_1>, d $\\frac{a}{b}$."))
            .unwrap();
    }

    /// Prices are not math, so the tool never sees them.
    #[test]
    fn prices_stay_text() {
        let md =
            "# Title\n\nPlans cost $10 (basic) and $20 (pro), or $5-$10. Also $x^2$.\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            screen_has(h, "M<x^2>") && screen_has(h, "$10 (basic) and $20 (pro), or $5-$10.")
        })
        .unwrap();
    }

    /// `\[citation\]` followed by text is markdown's escaped bracket.
    #[test]
    fn an_escaped_bracket_in_prose_is_not_math() {
        let md = "# Title\n\n\\[citation needed\\] and $\\alpha$.\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "M<\\alpha>.") && !screen_has(h, "math ·"))
            .unwrap();
    }

    /// Display math on one line, `$$…$$` and `\[…\]`, is drawn in a frame
    /// named after the tool, in as many rows as the tool prints.
    #[test]
    fn one_line_display_math_is_drawn_in_a_frame() {
        let md = "# Title\n\n$$E = mc^2$$\n\n\\[ \\frac{\\rho}{\\varepsilon_0} \\]\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            screen_has(h, "math · stub-math")
                && screen_has(h, "M<E = mc^2>")
                && screen_has(h, "FRAC TOP")
                && screen_has(h, "M<\\frac{\\rho}{\\varepsilon_0}>")
                && screen_has(h, "FRAC BOTTOM")
                && !screen_has(h, "$$")
                && !screen_has(h, "\\[")
        })
        .unwrap();
    }

    /// A `$$` or `\[` block over several lines is one formula for the tool,
    /// including a formula written on the opening line.
    #[test]
    fn multiline_display_math_is_one_formula() {
        let md = "# Title\n\n$$ a =\n\\frac{1}{2}\n$$\n\n\\[\n\\alpha^2\n\\]\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            screen_has(h, "M<a = \\frac{1}{2}>")
                && screen_has(h, "FRAC BOTTOM")
                && screen_has(h, "M<\\alpha^2>")
                && !screen_has(h, "$$")
                && !screen_has(h, "\\[")
        })
        .unwrap();
    }

    /// A ```latex fence is drawn by its entry in the map.
    #[test]
    fn a_fenced_latex_block_is_drawn_in_a_latex_frame() {
        let md = "# Title\n\nIntro.\n\n```latex\n\\alpha + \\beta\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            screen_has(h, "latex · stub-math")
                && screen_has(h, "M<\\alpha + \\beta>")
                && !screen_has(h, "```")
        })
        .unwrap();
    }

    /// `$$` in an ordinary code block is code.
    #[test]
    fn display_math_delimiters_in_a_code_block_are_code() {
        let md = "# Title\n\n```markdown\n$$\nE = mc^2\n$$\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "E = mc^2") && !screen_has(h, "M<"))
            .unwrap();
    }

    /// A `$$` that is never closed ends at the next blank line; the prose
    /// after it stays prose.
    #[test]
    fn an_unclosed_display_math_block_does_not_swallow_the_document() {
        let md = "# Title\n\n$$\nx^2\n\nSome *prose* here.\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "Some prose here.") && !screen_has(h, "math ·"))
            .unwrap();
    }

    /// The cursor moving past a one-line `$$…$$` leaves it drawn with its
    /// whole frame. (At the very start of the next line it still counts as on
    /// the line, as for every other conceal.)
    #[test]
    fn the_cursor_after_a_one_line_block_leaves_it_drawn() {
        let md = "# Title\n\n$$E = mc^2$$\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "M<E = mc^2>")).unwrap();
        for _ in 0..3 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| {
            screen_has(h, "M<E = mc^2>")
                && screen_has(h, "── math · stub-math ──")
                && screen_has(h, "└──")
        })
        .unwrap();
    }

    /// A Mermaid block is drawn from the tool's output, in a frame named
    /// after it, in place of its source.
    #[test]
    fn a_mermaid_block_is_drawn_by_the_tool() {
        let md = "# Title\n\nIntro.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            screen_has(h, "mermaid · stub-mermaid")
                && screen_has(h, "DIAGRAM<graph TD>")
                && !screen_has(h, "A --> B")
        })
        .unwrap();
    }

    /// A diagram the tool cannot draw stays a code block.
    #[test]
    fn a_diagram_the_tool_fails_on_stays_code() {
        let md = "# Title\n\n```mermaid\ngantt\n    title Plan\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "title Plan") && !screen_has(h, "DIAGRAM"))
            .unwrap();
    }

    /// The cursor in a diagram's block shows its source for editing; leaving
    /// the block draws the diagram again.
    #[test]
    fn the_cursor_in_a_diagram_shows_its_source() {
        let md = "# Title\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "DIAGRAM<graph TD>") && !screen_has(h, "A --> B"))
            .unwrap();

        for _ in 0..3 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.wait_until(|h| screen_has(h, "```mermaid") && screen_has(h, "A --> B"))
            .unwrap();

        h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
        h.wait_until(|h| screen_has(h, "DIAGRAM<graph TD>") && !screen_has(h, "A --> B"))
            .unwrap();
    }

    /// In the Source/Compose dual mode the drawings are in the composed
    /// panel only: the source panel shows the Markdown as written.
    #[test]
    fn dual_mode_draws_in_the_composed_panel_only() {
        let md = "# Doc\n\nSee $x^2$ here.\n\n$$\\frac{a}{b}$$\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let tools = tempfile::TempDir::new().unwrap();
        let (mut h, _tmp) = harness_with_renderers(md, stubs(tools.path()));
        run(&mut h, "Markdown: Source/Compose Dual Mode");

        let mid = WIDTH as usize / 2;
        h.wait_until(|h| {
            let (mut left, mut right) = (String::new(), String::new());
            for row in 0..35 {
                let chars: Vec<char> = h.screen_row_text(row).chars().collect();
                left.extend(chars.iter().take(mid - 3));
                left.push('\n');
                right.extend(chars.iter().skip(mid + 3));
                right.push('\n');
            }
            left.contains("$x^2$")
                && left.contains("```mermaid")
                && !left.contains("DIAGRAM")
                // A drawing's extra rows are virtual lines: compose panes only.
                && !left.contains("FRAC")
                && right.contains("FRAC BOTTOM")
                && right.contains("M<x^2>")
                && right.contains("DIAGRAM<graph TD>")
                && !right.contains("```mermaid")
        })
        .unwrap();
    }

    /// Any language can have a renderer: a ```dot block drawn by the command
    /// named for `dot`, with `{lang}` and the file named after it.
    #[test]
    fn any_language_can_have_a_renderer() {
        let tools = tempfile::TempDir::new().unwrap();
        let dot = stub(
            tools.path(),
            "stub-dot",
            r#"case "$2" in *.dot) printf 'GRAPH<%s> from %s\n' "$(head -n 1 "$2")" "$1" ;; *) exit 1 ;; esac"#,
        );
        let md = "# Title\n\n```dot\ndigraph { a -> b }\n```\n\nTail.\n";
        let (mut h, _tmp) = harness_with_renderers(
            md,
            serde_json::json!({ "dot": { "command": dot, "args": ["{lang}", "{file}"] } }),
        );
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| {
            screen_has(h, "dot · stub-dot")
                && screen_has(h, "GRAPH<digraph { a -> b }> from dot")
                && !screen_has(h, "```dot")
        })
        .unwrap();
    }

    /// An entry with an empty command turns a language off: the block is code.
    #[test]
    fn an_entry_with_no_command_turns_a_language_off() {
        let tools = tempfile::TempDir::new().unwrap();
        let mut table = stubs(tools.path());
        table["mermaid"] = serde_json::json!({ "command": "", "args": [] });
        let md = "# Title\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nSee $x^2$.\n";
        let (mut h, _tmp) = harness_with_renderers(md, table);
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| {
            screen_has(h, "M<x^2>") && screen_has(h, "A --> B") && !screen_has(h, "DIAGRAM")
        })
        .unwrap();
    }

    /// `{source}` is passed as written: `$` sequences in it are not patterns,
    /// and a formula starting with `-` comes after `--`, so it is not read as
    /// an option.
    #[test]
    fn source_text_is_passed_as_written() {
        let md = "# Title\n\n$$x $& y$$\n\n$$-x$$\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "M<x $& y>") && screen_has(h, "M<-x>"))
            .unwrap();
    }

    /// A blank line inside a block has no text to put a row of the drawing
    /// over: the row is drawn at its line break instead, and a source line
    /// past the drawing's last row is an empty, framed row.
    #[test]
    fn a_blank_line_in_a_block_gets_its_row_of_the_drawing() {
        let md = "# Title\n\n```mermaid\ngraph TD\n\n    A --> B\n    B --> C\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| {
            let s = h.screen_to_string();
            let rows: Vec<&str> = s.lines().collect();
            let second = rows.iter().position(|r| r.contains("DIAGRAM 3 lines"));
            // The two rows are on consecutive screen rows, and the extra
            // source line under them is a framed row, not bare text.
            matches!(second, Some(i) if rows[i - 1].contains("DIAGRAM<graph TD>")
                && rows[i + 1].trim_end().ends_with('│')
                && !rows[i + 1].contains("B --> C"))
        })
        .unwrap();
    }

    /// The cursor on the line after a drawn block, at its start, is outside
    /// the block: the drawing stays.
    #[test]
    fn the_cursor_just_after_a_block_leaves_it_drawn() {
        let md = "# Title\n\n```mermaid\ngraph TD\n    A --> B\n```\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "DIAGRAM<graph TD>"))
            .unwrap();
        // Down to the start of "Tail.", right under the closing fence.
        for _ in 0..6 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| screen_has(h, "DIAGRAM<graph TD>") && !screen_has(h, "A --> B"))
            .unwrap();
    }

    /// Text typed above a drawn block moves it; moving the cursor into the
    /// block afterwards still shows its source.
    #[test]
    fn a_block_moved_by_an_edit_above_still_shows_its_source_at_the_cursor() {
        let md = "# Title\n\nIntro.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let (mut h, _tmp, _tools) = compose_with_stubs(md);
        h.wait_until(|h| screen_has(h, "DIAGRAM<graph TD>"))
            .unwrap();
        for _ in 0..2 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        // Longer than the whole block, so its old range is behind the cursor.
        h.type_text(&" more".repeat(12)).unwrap();
        h.wait_until(|h| screen_has(h, "more more")).unwrap();
        h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        for _ in 0..3 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.wait_until(|h| screen_has(h, "A --> B") && screen_has(h, "```mermaid"))
            .unwrap();
    }

    /// In dual mode, moving the cursor through a block in the source pane
    /// leaves the compose pane's drawing whole: only a composed pane's own
    /// cursor shows a block's source.
    #[test]
    fn the_source_panes_cursor_does_not_change_the_compose_pane() {
        let tools = tempfile::TempDir::new().unwrap();
        let tall = stub(
            tools.path(),
            "stub-tall",
            r#"printf 'ROW 1\nROW 2\nROW 3\nROW 4\nROW 5\nROW 6\n'"#,
        );
        let md = "# Doc\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let (mut h, _tmp) = harness_with_renderers(
            md,
            serde_json::json!({ "mermaid": { "command": tall, "args": ["{file}"] } }),
        );
        run(&mut h, "Markdown: Source/Compose Dual Mode");
        h.wait_until(|h| screen_has(h, "ROW 6")).unwrap();
        // Into the block, in the source pane (the one in focus).
        for _ in 0..3 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.wait_until(|h| {
            screen_has(h, "ROW 1") && screen_has(h, "ROW 6") && screen_has(h, "A --> B")
        })
        .unwrap();
    }

    /// A renderer that could not be run is tried again when the buffer next
    /// enters preview: installing it, then turning compose off and on again,
    /// draws the block without restarting the editor.
    #[test]
    fn a_renderer_installed_later_is_used_when_preview_is_entered_again() {
        let tools = tempfile::TempDir::new().unwrap();
        let later = tools.path().join("installed-later");
        let md = "# Doc\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
        let (mut h, _tmp) = harness_with_renderers(
            md,
            serde_json::json!({ "mermaid": { "command": later.to_string_lossy(), "args": ["{file}"] } }),
        );
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| screen_has(h, "A --> B") && screen_has(h, "not found"))
            .unwrap();
        // "Install" it.
        stub(tools.path(), "installed-later", r#"printf 'NOW DRAWN\n'"#);
        run(&mut h, "Markdown: Toggle Compose");
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| screen_has(h, "NOW DRAWN")).unwrap();
    }

    /// A tool that prints colour: its escape codes are not drawn and take no
    /// columns — every row of the frame keeps its right rail in line with
    /// the top corner — and the colours they name are drawn: on a row in
    /// place of a source line, and on one past the block's last line (a
    /// virtual line). Other escape codes (a window title here) are dropped.
    #[test]
    fn a_tools_colours_are_drawn_and_its_escape_codes_take_no_columns() {
        use ratatui::style::{Color, Modifier};
        let tools = tempfile::TempDir::new().unwrap();
        let colour = stub(
            tools.path(),
            "stub-colour",
            r#"printf '\033[31mRED\033[0m plain\n'
printf '\033]0;a title\007\033[1;38;2;0;200;0mGREEN\033[22;39m after\n'
printf '\033[44mBLUE\033[49m\n'"#,
        );
        let md = "# Doc\n\n```dot\na\nb\n```\n\nTail.\n";
        let (mut h, _tmp) = harness_with_renderers(
            md,
            serde_json::json!({ "dot": { "command": colour, "args": ["{file}"] } }),
        );
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| {
            screen_has(h, "RED plain") && screen_has(h, "GREEN after") && screen_has(h, "BLUE")
        })
        .unwrap();

        let screen = h.screen_to_string();
        assert!(
            !screen.contains("[31m") && !screen.contains('\u{1b}') && !screen.contains("a title"),
            "escape codes drawn as text:\n{screen}"
        );
        // The right rail of every drawn row sits under the frame's corner.
        let (_, top) = h.find_text_on_screen("dot · stub-colour").unwrap();
        let corner = |row: &str| row.chars().position(|c| c == '┐');
        let rail = |row: &str| {
            row.chars()
                .collect::<Vec<_>>()
                .iter()
                .rposition(|&c| c == '│')
        };
        let rows: Vec<&str> = screen.lines().collect();
        let corner_col = corner(rows[top as usize]).expect("frame top");
        for text in ["RED plain", "GREEN after", "BLUE"] {
            let row = rows.iter().find(|r| r.contains(text)).unwrap();
            assert_eq!(
                rail(row),
                Some(corner_col),
                "rail out of line on {row:?}\n{screen}"
            );
        }

        let (x, y) = h.find_text_on_screen("RED plain").unwrap();
        assert_eq!(h.get_cell_style(x, y).unwrap().fg, Some(Color::Red));
        assert_ne!(
            h.get_cell_style(x + 4, y).unwrap().fg,
            Some(Color::Red),
            "colour reset"
        );

        let (x, y) = h.find_text_on_screen("GREEN after").unwrap();
        let green = h.get_cell_style(x, y).unwrap();
        assert_eq!(green.fg, Some(Color::Rgb(0, 200, 0)));
        assert!(green.add_modifier.contains(Modifier::BOLD));
        let after = h.get_cell_style(x + 6, y).unwrap();
        assert_ne!(after.fg, Some(Color::Rgb(0, 200, 0)));
        assert!(!after.add_modifier.contains(Modifier::BOLD));

        // The third row is past the block's two lines: a virtual line.
        let (x, y) = h.find_text_on_screen("BLUE").unwrap();
        assert_eq!(h.get_cell_style(x, y).unwrap().bg, Some(Color::Blue));
    }

    /// In dual mode the wheel over the preview scrolls it a row a notch,
    /// through a drawing as through text, and the frames that follow leave it
    /// there. The drawing's rows, framed to the narrow preview, used to count
    /// as two rows each in the preview's row index (wrapped at the source
    /// pane's gutter-narrowed width) while the preview drew one, so a notch
    /// through them moved nothing on screen. And a preview scrolled between
    /// two of the source's positions was pulled back to one of them by the
    /// second frame after the notch, when the focused source pane drove the
    /// sync again though neither pane had moved.
    #[test]
    fn the_wheel_scrolls_a_dual_preview_a_row_a_notch_through_a_drawing() {
        let tools = tempfile::TempDir::new().unwrap();
        let tall = stub(
            tools.path(),
            "stub-tall",
            r#"for i in 1 2 3 4 5 6 7 8; do printf 'ROW %s\n' $i; done"#,
        );
        let mut md = String::from("# Doc\n\nBefore.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\n");
        for i in 0..60 {
            md.push_str(&format!("After {i}\n\n"));
        }
        let (mut h, _tmp) = harness_with_renderers(
            &md,
            serde_json::json!({ "mermaid": { "command": tall, "args": ["{file}"] } }),
        );
        h.editor_mut().config_mut().editor.mouse_wheel_scroll_lines = 1;
        run(&mut h, "Markdown: Source/Compose Dual Mode");
        h.wait_until(|h| screen_has(h, "ROW 8")).unwrap();

        // The preview is the right half; its first text row is screen row 2.
        let half = (WIDTH / 2) as usize;
        let preview_top = |h: &EditorTestHarness| -> String {
            let screen = h.screen_to_string();
            let row: String = screen.lines().nth(2).unwrap().chars().skip(half).collect();
            row.replace(['▌', '│'], " ").trim().to_string()
        };
        let mut tops = Vec::new();
        for _ in 0..16 {
            h.mouse_scroll_down(WIDTH - 20, 15).unwrap();
            // Frames with nothing new to draw, as a redraw or an idle tick
            // brings: they must not move either pane.
            for _ in 0..3 {
                h.render().unwrap();
            }
            tops.push(preview_top(&h));
        }
        let drawing: Vec<&str> = tops
            .iter()
            .map(String::as_str)
            .filter(|t| t.starts_with("ROW "))
            .collect();
        assert_eq!(
            drawing,
            (1..=8).map(|i| format!("ROW {i}")).collect::<Vec<_>>(),
            "each notch should bring the next row of the drawing to the top: {tops:?}"
        );
    }

    /// In dual mode, a drawing that arrives inside the window — the tool
    /// answering after the panes were synced — lines the preview up with the
    /// source again on the frames that follow, as the source's scroll would
    /// have. Once leaving the pair alone on a frame where neither pane moved,
    /// the sync kept the preview where the old layout put it: a wheel notch and
    /// a notch back then moved it somewhere else, not back to where it stood.
    #[test]
    fn a_drawing_arriving_in_a_dual_view_lines_the_panes_up_again() {
        let tools = tempfile::TempDir::new().unwrap();
        let slow = stub(
            tools.path(),
            "stub-slow",
            r#"sleep 1; for i in $(seq 1 14); do printf 'ROW %s\n' $i; done"#,
        );
        let mut md = String::from("# Doc\n\n");
        for i in 0..40 {
            md.push_str(&format!("Pre {i}\n\n"));
        }
        md.push_str("```mermaid\ngraph TD\n    A --> B\n```\n\n");
        for i in 0..40 {
            md.push_str(&format!("After {i}\n\n"));
        }
        let (mut h, _tmp) = harness_with_renderers(
            &md,
            serde_json::json!({ "mermaid": { "command": slow, "args": ["{file}"] } }),
        );
        run(&mut h, "Markdown: Source/Compose Dual Mode");
        h.wait_until(|h| screen_has(h, "Pre 0")).unwrap();

        // Wheel the source until the block's fence is in its upper part.
        let half = (WIDTH / 2) as usize;
        let left_row_of = |h: &EditorTestHarness, text: &str| -> Option<usize> {
            h.screen_to_string()
                .lines()
                .position(|l| l.chars().take(half).collect::<String>().contains(text))
        };
        for _ in 0..40 {
            if left_row_of(&h, "```mermaid").is_some_and(|r| (4..12).contains(&r)) {
                break;
            }
            h.mouse_scroll_down(10, 15).unwrap();
        }
        assert!(left_row_of(&h, "```mermaid").is_some_and(|r| (4..12).contains(&r)));
        // The drawing arrives a moment later, taller than the source block.
        h.wait_until(|h| screen_has(h, "ROW 14")).unwrap();
        for _ in 0..3 {
            h.render().unwrap();
        }
        let before = h.screen_to_string();

        h.mouse_scroll_down(10, 15).unwrap();
        h.mouse_scroll_up(10, 15).unwrap();
        for _ in 0..3 {
            h.render().unwrap();
        }
        assert_eq!(
            h.screen_to_string(),
            before,
            "a notch down and back up should leave both panes where they were"
        );
    }

    /// A compose pane wide enough to keep a desk margin draws its gutter in
    /// the margin, not across the text: the wheel steps a row a notch through
    /// a drawing there as well, the row index counting the page's own width.
    #[test]
    fn the_wheel_scrolls_a_wide_compose_pane_a_row_a_notch_through_a_drawing() {
        let tools = tempfile::TempDir::new().unwrap();
        let tall = stub(
            tools.path(),
            "stub-tall",
            r#"for i in 1 2 3 4 5 6 7 8; do printf 'ROW %s\n' $i; done"#,
        );
        let mut md = String::from("# Doc\n\nBefore.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\n");
        for i in 0..60 {
            md.push_str(&format!("After {i}\n\n"));
        }
        let (mut h, _tmp) = harness_with_renderers(
            &md,
            serde_json::json!({ "mermaid": { "command": tall, "args": ["{file}"] } }),
        );
        h.editor_mut().config_mut().editor.mouse_wheel_scroll_lines = 1;
        run(&mut h, "Markdown: Toggle Compose");
        h.wait_until(|h| screen_has(h, "ROW 8")).unwrap();
        let top = |h: &EditorTestHarness| -> String {
            let screen = h.screen_to_string();
            let row = screen.lines().nth(2).unwrap().to_string();
            row.replace(['▌', '│'], " ").trim().to_string()
        };
        let mut tops = Vec::new();
        for _ in 0..16 {
            h.mouse_scroll_down(WIDTH / 2, 15).unwrap();
            for _ in 0..3 {
                h.render().unwrap();
            }
            tops.push(top(&h));
        }
        let drawing: Vec<&str> = tops
            .iter()
            .map(String::as_str)
            .filter(|t| t.starts_with("ROW "))
            .collect();
        assert_eq!(
            drawing,
            (1..=8).map(|i| format!("ROW {i}")).collect::<Vec<_>>(),
            "each notch should bring the next row of the drawing to the top: {tops:?}"
        );
    }

    /// In dual mode the wheel scrolls the pane under the pointer: over the
    /// source, a line a notch (the preview following it), whichever pane has
    /// the keyboard.
    #[test]
    fn the_wheel_over_a_dual_views_source_scrolls_the_source() {
        let tools = tempfile::TempDir::new().unwrap();
        let tall = stub(
            tools.path(),
            "stub-tall",
            r#"for i in 1 2 3 4 5 6 7 8; do printf 'ROW %s\n' $i; done"#,
        );
        let mut md = String::from("# Doc\n\nBefore.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\n");
        for i in 0..60 {
            md.push_str(&format!("After {i}\n\n"));
        }
        let (mut h, _tmp) = harness_with_renderers(
            &md,
            serde_json::json!({ "mermaid": { "command": tall, "args": ["{file}"] } }),
        );
        h.editor_mut().config_mut().editor.mouse_wheel_scroll_lines = 1;
        run(&mut h, "Markdown: Source/Compose Dual Mode");
        h.wait_until(|h| screen_has(h, "ROW 8")).unwrap();
        // The source's top line, read off its gutter.
        let half = (WIDTH / 2) as usize;
        let source_top = |h: &EditorTestHarness| -> usize {
            let screen = h.screen_to_string();
            let row: String = screen.lines().nth(2).unwrap().chars().take(half).collect();
            // The number before the gutter's `│`; a fold arrow may sit by it.
            let gutter = row.split('│').next().unwrap();
            gutter
                .chars()
                .filter(char::is_ascii_digit)
                .collect::<String>()
                .parse()
                .unwrap_or_else(|_| panic!("no line number in {row:?}"))
        };
        let start = source_top(&h);
        for notch in 1..=20 {
            h.mouse_scroll_down(10, 15).unwrap();
            for _ in 0..3 {
                h.render().unwrap();
            }
            assert_eq!(
                source_top(&h),
                start + notch,
                "notch {notch} over the source"
            );
        }
        for notch in 1..=20 {
            h.mouse_scroll_up(10, 15).unwrap();
            for _ in 0..3 {
                h.render().unwrap();
            }
            assert_eq!(
                source_top(&h),
                start + 20 - notch,
                "notch {notch} up over the source"
            );
        }
    }

    /// After the wheel scrolled the preview, the source still follows its
    /// cursor: arrowing down past the bottom of the source scrolls it, and the
    /// preview with it. The preview leads the sync while it is the pane the
    /// user last scrolled, but a cursor the source must keep on screen moves
    /// the source, and the source leads again.
    #[test]
    fn the_source_follows_its_cursor_after_the_wheel_scrolled_the_preview() {
        let tools = tempfile::TempDir::new().unwrap();
        let tall = stub(
            tools.path(),
            "stub-tall",
            r#"for i in 1 2 3 4 5 6 7 8; do printf 'ROW %s\n' $i; done"#,
        );
        let mut md = String::from("# Doc\n\nBefore.\n\n```mermaid\ngraph TD\n    A --> B\n```\n\n");
        for i in 0..60 {
            md.push_str(&format!("After {i}\n\n"));
        }
        let (mut h, _tmp) = harness_with_renderers(
            &md,
            serde_json::json!({ "mermaid": { "command": tall, "args": ["{file}"] } }),
        );
        h.editor_mut().config_mut().editor.mouse_wheel_scroll_lines = 1;
        run(&mut h, "Markdown: Source/Compose Dual Mode");
        h.wait_until(|h| screen_has(h, "ROW 8")).unwrap();
        for _ in 0..5 {
            h.mouse_scroll_down(WIDTH - 20, 15).unwrap();
        }
        // Down through the source, the pane with the keyboard, well past
        // the bottom of its window.
        for _ in 0..80 {
            h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        }
        h.render().unwrap();
        // The cursor is on line 81, between `After 35` and `After 36`; the
        // source shows it.
        let half = (WIDTH / 2) as usize;
        let screen = h.screen_to_string();
        let source_shows = screen.lines().any(|l| {
            l.chars()
                .take(half)
                .collect::<String>()
                .contains("After 36")
        });
        assert!(
            source_shows,
            "the source should show its cursor's line:\n{screen}"
        );
    }
}
