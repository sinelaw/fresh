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

const WIDTH: u16 = 140;

fn harness_with(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
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
            .with_full_grammar_registry(),
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
