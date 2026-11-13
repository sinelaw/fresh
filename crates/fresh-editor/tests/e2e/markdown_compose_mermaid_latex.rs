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
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
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
    /// defaults pass: math `--line-width {width} {source}` (also for
    /// `latex`), Mermaid `{file} --width {width}`.
    /// Math prints `M<formula>` (a formula over several lines on one row),
    /// with a row above and below when it holds a `\frac`, as a 2-D layout
    /// would. Mermaid prints its first line and a row count, and fails on a
    /// `gantt`, as a tool does on a diagram type it does not know.
    fn stubs(dir: &std::path::Path) -> serde_json::Value {
        let math = stub(
            dir,
            "stub-math",
            r#"f=$(printf '%s' "$3" | tr '\n' ' ')
case "$f" in *'\frac'*) printf 'FRAC TOP\nM<%s>\nFRAC BOTTOM\n' "$f" ;; *) printf 'M<%s>\n' "$f" ;; esac"#,
        );
        let mermaid = stub(
            dir,
            "stub-mermaid",
            r#"grep -q gantt "$1" && exit 3
printf 'DIAGRAM<%s>\nDIAGRAM %s lines\n' "$(head -n 1 "$1")" "$(wc -l < "$1" | tr -d ' ')""#,
        );
        serde_json::json!({
            "math": { "command": math, "args": ["--line-width", "{width}", "{source}"] },
            "latex": { "command": math, "args": ["--line-width", "{width}", "{source}"] },
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
}
