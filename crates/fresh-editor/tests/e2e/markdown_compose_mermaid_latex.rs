//! E2E tests for Mermaid diagrams and LaTeX math in compose mode.
//!
//! What the renderers draw is unit-tested (`plugins/tests/latex.test.ts`,
//! `plugins/tests/mermaid.test.ts`); these check that compose puts it on
//! screen: math inline and in display blocks, diagrams in place of their
//! fenced source, the source back while the cursor is in the block, and only
//! in compose panes — the source panel of the Source/Compose dual mode shows
//! the Markdown as written.
//!
//! Each check looks for text only the rendering produces (`α`, `²`, a box
//! around a node's label), never words the source itself contains, and waits
//! for the screen it expects (CONTRIBUTING.md Testing §2, §3).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};

const WIDTH: u16 = 140;

/// The plugin's settings: external renderers off unless a test names one,
/// so a machine with `termaid` or `utftex` installed draws what these
/// tests expect from the built-in renderers.
fn compose_config(settings: serde_json::Value) -> Config {
    let mut defaults = serde_json::json!({ "mermaidCommand": "", "mathCommand": "" });
    if let (Some(d), Some(s)) = (defaults.as_object_mut(), settings.as_object()) {
        for (k, v) in s {
            d.insert(k.clone(), v.clone());
        }
    }
    let mut config = Config::default();
    config.plugins.insert(
        "markdown_compose".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: defaults,
        },
    );
    config
}

fn harness_with(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    harness_with_settings(md, serde_json::json!({}))
}

fn harness_with_settings(
    md: &str,
    settings: serde_json::Value,
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
            .with_config(compose_config(settings)),
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

/// `md` open in compose mode, the cursor on the first line (a heading, so
/// no block below it is revealed for editing).
fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    let (mut harness, temp) = harness_with(md);
    run(&mut harness, "Markdown: Toggle Compose");
    (harness, temp)
}

fn screen_has(h: &EditorTestHarness, text: &str) -> bool {
    h.screen_to_string().contains(text)
}

#[test]
fn inline_math_is_drawn_with_unicode_symbols() {
    let md = "# Title\n\nInline math: $x^2$ and $\\alpha + \\beta = \\gamma$.\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| {
        screen_has(h, "x²") && screen_has(h, "α + β = γ") && !screen_has(h, "\\alpha")
    })
    .unwrap();
}

#[test]
fn display_math_is_drawn_in_a_math_frame() {
    let md = "# Title\n\nIntro.\n\n$$E = mc^2$$\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Math") && screen_has(h, "E = mc²") && !screen_has(h, "$$"))
        .unwrap();
}

#[test]
fn multiline_display_math_is_drawn_in_a_math_frame() {
    let md = "# Title\n\nIntro.\n\n$$\n\\frac{a}{b} + \\sqrt{x}\n$$\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Math") && screen_has(h, "a/b + √(x)") && !screen_has(h, "$$"))
        .unwrap();
}

/// MathJax and KaTeX's own delimiters, `\\(…\\)` inline and `\\[…\\]` for
/// display, and GitLab's inline $`…`$, are drawn like `$…$` and `$$…$$`.
#[test]
fn mathjax_and_gitlab_delimiters_are_drawn() {
    let md = "# Title\n\nParen: \\(\\pi r^2\\) here. GitLab: $`\\beta_1`$ here.\n\n\\[ \\frac{\\rho}{\\varepsilon_0} \\]\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| {
        screen_has(h, "Paren: πr² here.")
            && screen_has(h, "GitLab: β₁ here.")
            && screen_has(h, "ρ/ε₀")
            && screen_has(h, "Math")
            && !screen_has(h, "\\(")
            && !screen_has(h, "\\[")
            && !screen_has(h, "$`")
    })
    .unwrap();
}

/// A `\\[` alone on its line opens display math that runs to the `\\]`; its
/// lines are drawn in a math frame, not re-flowed into one paragraph.
#[test]
fn multiline_bracket_display_math_is_drawn_in_a_math_frame() {
    let md = "# Title\n\nIntro.\n\n\\[\n\\alpha^2\n\\]\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Math") && screen_has(h, "α²") && !screen_has(h, "\\["))
        .unwrap();
}

/// `\\[` followed by text is markdown's escaped bracket, not math.
#[test]
fn an_escaped_bracket_in_prose_is_not_math() {
    let md = "# Title\n\n\\[citation needed\\] and $\\alpha$.\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "α.") && !screen_has(h, "Math"))
        .unwrap();
}

/// Prices are not math: "$10 (basic) and $" is not a formula.
#[test]
fn prices_stay_text() {
    let md = "# Title\n\nPlans cost $10 (basic) and $20 (pro), or $5-$10. Also $x^2$.\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "x²") && screen_has(h, "$10 (basic) and $20 (pro), or $5-$10."))
        .unwrap();
}

/// `$$` in an ordinary code block is code: the block keeps its rails and
/// its text.
#[test]
fn display_math_delimiters_in_a_code_block_are_code() {
    let md = "# Title\n\n```markdown\n$$\nE = mc^2\n$$\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "E = mc^2") && !screen_has(h, "Math") && !screen_has(h, "mc²"))
        .unwrap();
}

/// A formula on the line that opens a block is drawn, not hidden behind
/// the frame.
#[test]
fn a_formula_on_the_opening_line_is_drawn() {
    let md = "# Title\n\nIntro.\n\n$$ E = mc^2\n+ \\alpha\n$$\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Math") && screen_has(h, "E = mc²") && screen_has(h, "+ α"))
        .unwrap();
    // `+ α` is a formula's line, not a list item: no spacer row after it.
    let screen = h.screen_to_string();
    let rows: Vec<&str> = screen.lines().collect();
    let at = rows.iter().position(|r| r.contains("+ α")).unwrap();
    assert!(
        rows[at + 1].contains('└'),
        "frame bottom right below the last row:\n{screen}"
    );
}

/// A `$$` that is never closed ends at the next blank line, and the prose
/// after it stays prose.
#[test]
fn an_unclosed_display_math_block_does_not_swallow_the_document() {
    let md = "# Title\n\n$$\nx^2\n\nSome *prose* here with \\alpha.\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Some prose here with \\alpha.") && !screen_has(h, "Math"))
        .unwrap();
}

/// The cursor moving past a one-line `$$…$$` leaves it drawn with its whole
/// frame. (At the very start of the next line it still counts as on the
/// line, as for every other conceal.)
#[test]
fn the_cursor_after_a_one_line_block_leaves_it_drawn() {
    let md = "# Title\n\n$$E = mc^2$$\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "E = mc²")).unwrap();
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        screen_has(h, "E = mc²") && screen_has(h, "── Math ──") && screen_has(h, "└──")
    })
    .unwrap();
}

#[test]
fn a_fenced_latex_block_is_drawn_in_a_latex_frame() {
    let md = "# Title\n\nIntro.\n\n```latex\n\\alpha + \\beta = \\gamma\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "LaTeX") && screen_has(h, "α + β = γ") && !screen_has(h, "```"))
        .unwrap();
}

#[test]
fn a_mermaid_flowchart_is_drawn_in_place_of_its_source() {
    let md = "# Title\n\nIntro.\n\n```mermaid\ngraph TD\n    A[Start Node] --> B(End Node)\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| {
        screen_has(h, "Mermaid Flowchart")
            && screen_has(h, "│ Start Node │")
            && screen_has(h, "│ End Node │")
            && !screen_has(h, "graph TD")
    })
    .unwrap();
}

/// `A -->|label| B` labels the link: the label is drawn by its arrow, and
/// is not taken for part of the node after it.
#[test]
fn a_mermaid_edge_label_is_drawn_by_its_arrow() {
    let md = "# Title\n\nIntro.\n\n```mermaid\nflowchart TD\n    A{Ready?} -->|Yes| B[Ship it]\n    A -->|No| C[Wait]\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| {
        screen_has(h, "▼ Yes")
            && screen_has(h, "▼ No")
            && screen_has(h, "│ Ship it │")
            && !screen_has(h, "|Yes|")
    })
    .unwrap();
}

#[test]
fn a_mermaid_sequence_diagram_is_drawn_in_place_of_its_source() {
    let md = "# Title\n\nIntro.\n\n```mermaid\nsequenceDiagram\n    Alice->>Bob: Hello World\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| {
        screen_has(h, "Mermaid Sequence")
            && screen_has(h, "│ Alice │")
            && screen_has(h, " Hello World ")
            && !screen_has(h, "->>")
    })
    .unwrap();
}

/// The cursor in a diagram's block shows its source for editing; leaving
/// the block draws the diagram again.
#[test]
fn the_cursor_in_a_diagram_shows_its_source() {
    let md = "# Title\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
    let (mut h, _tmp) = compose_harness(md);
    h.wait_until(|h| screen_has(h, "Mermaid Flowchart") && !screen_has(h, "graph TD"))
        .unwrap();

    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| screen_has(h, "```mermaid") && screen_has(h, "graph TD"))
        .unwrap();

    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| screen_has(h, "Mermaid Flowchart") && !screen_has(h, "graph TD"))
        .unwrap();
}

/// In the Source/Compose dual mode the diagram and the math are drawn in the
/// composed panel only: the source panel shows the Markdown as written.
#[test]
fn dual_mode_draws_diagrams_and_math_in_the_composed_panel_only() {
    let md =
        "# Doc\n\nSee $x^2$ here.\n\n```mermaid\ngraph TD\n    A[Start] --> B(End)\n```\n\nTail.\n";
    let (mut h, _tmp) = harness_with(md);
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
            && !left.contains("Mermaid Flowchart")
            && right.contains("x²")
            && right.contains("Mermaid Flowchart")
            && !right.contains("```mermaid")
    })
    .unwrap();
}

/// A stand-in renderer: a script that prints a few rows naming what it was
/// given, so the test can tell its drawing from the built-in one.
#[cfg(unix)]
fn stub_renderer(dir: &std::path::Path, name: &str, body: &str) -> String {
    use std::os::unix::fs::PermissionsExt;
    let path = dir.join(name);
    std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    path.to_string_lossy().into_owned()
}

/// With a renderer configured, display math and Mermaid are drawn from its
/// output (in as many rows as it prints), the frame names it, and inline math
/// keeps the built-in renderer.
#[cfg(unix)]
#[test]
fn a_configured_external_renderer_draws_display_math_and_diagrams() {
    let tools = tempfile::TempDir::new().unwrap();
    let math = stub_renderer(
        tools.path(),
        "stub-math",
        r#"printf 'TOP ROW\nMATH[%s] w=%s\nLAST ROW\n' "$3" "$2""#,
    );
    let mermaid = stub_renderer(
        tools.path(),
        "stub-mermaid",
        r#"printf 'DIAGRAM ROW 1\nDIAGRAM %s lines\nDIAGRAM ROW 3\n' "$(wc -l < "$2" | tr -d ' ')""#,
    );
    let md = "# Title\n\nInline $x^2$ here.\n\n$$E = mc^2$$\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
    let (mut h, _tmp) = harness_with_settings(
        md,
        serde_json::json!({ "mathCommand": math, "mermaidCommand": mermaid }),
    );
    run(&mut h, "Markdown: Toggle Compose");
    h.wait_until(|h| {
        screen_has(h, "MATH[E = mc^2]")
            && screen_has(h, "TOP ROW")
            && screen_has(h, "LAST ROW")
            && screen_has(h, "Math · stub-math")
            && screen_has(h, "DIAGRAM ROW 3")
            && screen_has(h, "Mermaid · stub-mermaid")
            && screen_has(h, "Inline x² here.")
    })
    .unwrap();
}

/// A renderer that is not installed leaves the built-in drawing in place.
#[test]
fn a_missing_external_renderer_falls_back_to_the_built_in_one() {
    let md =
        "# Title\n\n$$E = mc^2$$\n\n```mermaid\ngraph TD\n    A[Start Node] --> B\n```\n\nTail.\n";
    let (mut h, _tmp) = harness_with_settings(
        md,
        serde_json::json!({
            "mathCommand": "fresh-test-no-such-renderer",
            "mermaidCommand": "fresh-test-no-such-renderer",
        }),
    );
    run(&mut h, "Markdown: Toggle Compose");
    h.wait_until(|h| {
        screen_has(h, "E = mc²")
            && screen_has(h, "│ Start Node │")
            && screen_has(h, "── Math ──")
            && screen_has(h, "Mermaid Flowchart")
    })
    .unwrap();
}
