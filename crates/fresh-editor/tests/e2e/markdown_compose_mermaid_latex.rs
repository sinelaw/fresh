//! End-to-end tests for Mermaid diagrams and LaTeX math equations in Markdown Compose mode.
//!
//! Tests:
//! 1. Inline LaTeX equations ($x = 1$, $\alpha + \beta = \gamma$)
//! 2. Single-line display math ($$E = mc^2$$)
//! 3. Multiline display math ($$\n...\n$$)
//! 4. Fenced math and latex blocks (```math and ```latex)
//! 5. Mermaid flowchart diagrams (```mermaid\ngraph TD\n...)
//! 6. Mermaid sequence diagrams (```mermaid\nsequenceDiagram\n...)
//! 7. Mermaid pie charts (```mermaid\npie title...\n...)
//! 8. Cursor reveal for diagrams and math blocks

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
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
        100,
        35,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
    )
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

    // Park cursor at top
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    (harness, temp_dir)
}

#[test]
fn inline_latex_math_renders() {
    let md = "# Title\n\nInline math: $x = 1$ and $\\alpha + \\beta = \\gamma$.\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("x = 1") && screen.contains("α + β = γ")
        })
        .expect("inline math should render as unicode");

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("\\alpha"),
        "raw latex macro \\alpha should be concealed"
    );
}

#[test]
fn single_line_display_math_renders() {
    let md = "# Title\n\nIntro.\n\n$$E = mc^2$$\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Math") && screen.contains("E = mc²")
        })
        .expect("single line $$ math should render with Math frame and superscripts");
}

#[test]
fn multiline_display_math_renders() {
    let md = "# Title\n\nIntro.\n\n$$\n\\frac{a}{b} + \\sqrt{x}\n$$\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Math") && (screen.contains("a / b") || screen.contains("√(x)"))
        })
        .expect("multiline $$ math should render fractions and square roots");
}

#[test]
fn fenced_latex_block_renders() {
    let md = "# Title\n\nIntro.\n\n```latex\n\\alpha + \\beta = \\gamma\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("LaTeX") && screen.contains("α + β = γ")
        })
        .expect("fenced latex block should render with LaTeX frame");
}

#[test]
fn mermaid_flowchart_renders() {
    let md = "# Title\n\nIntro.\n\n```mermaid\ngraph TD\n    A[Start Node] --> B(End Node)\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Mermaid")
                && screen.contains("Start Node")
                && screen.contains("End Node")
        })
        .expect("mermaid flowchart should render with nodes and frame");
}

#[test]
fn mermaid_sequence_diagram_renders() {
    let md = "# Title\n\nIntro.\n\n```mermaid\nsequenceDiagram\n    Alice->>Bob: Hello World\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Mermaid Sequence")
                && screen.contains("Alice")
                && screen.contains("Bob")
                && screen.contains("Hello World")
        })
        .expect("mermaid sequence diagram should render participants and message");
}

#[test]
fn mermaid_pie_chart_renders() {
    let md = "# Title\n\nIntro.\n\n```mermaid\npie title Fruit Share\n    \"Apples\" : 60\n    \"Bananas\" : 40\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Mermaid")
                && screen.contains("Fruit Share")
                && screen.contains("Apples")
                && screen.contains("Bananas")
        })
        .expect("mermaid pie chart should render title, keys, and meters");
}

#[test]
fn cursor_reveals_mermaid_source() {
    let md = "# Title\n\n```mermaid\ngraph TD\n    A --> B\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    // Initially framed
    harness
        .wait_until(|h| h.screen_to_string().contains("Mermaid"))
        .expect("initially framed");

    // Move cursor down into the mermaid block
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    // Cursor is inside the block: raw mermaid source should reveal
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("```mermaid") || screen.contains("graph TD")
        })
        .expect("cursor in mermaid block should reveal raw source markdown");

    // Move cursor back to top
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    // When cursor leaves, it should re-render as diagram
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Mermaid") && !screen.contains("```mermaid")
        })
        .expect("cursor leaving should re-frame diagram");
}

#[test]
fn duo_mode_renders_mermaid_and_math() {
    let md = "# Doc\n\n$x = 1$\n\n```mermaid\ngraph TD\n    A --> B\n```\n";
    let (mut harness, _tmp) = compose_harness(md);

    // Switch to Duo mode via palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Markdown: Duo Mode").unwrap();
    harness
        .wait_for_screen_contains("Markdown: Duo Mode")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.wait_for_async_quiescence(6).unwrap();

    // Verify screen shows both splits: one with raw source and one with Mermaid / x = 1
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Left split (source) has ```mermaid, Right split (compose) has Mermaid frame
            screen.contains("```mermaid") && screen.contains("Mermaid") && screen.contains("x = 1")
        })
        .expect(
            "duo mode should show raw source in left split and rendered diagram in right split",
        );
}

#[test]
fn nested_latex_fractions_and_roots_render() {
    let md = "# Title\n\nIntro.\n\n$$\n\\frac{\\sqrt{a}}{\\sqrt{b + \\sqrt{c}}}\n$$\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("√(a)/(√(b + √(c)))")
        })
        .expect("nested fractions and square roots should render correctly");
}

#[test]
fn complex_mermaid_branching_and_cycles_render() {
    let md = "# Title\n\n```mermaid\ngraph TD\n    A[Start Node] --> B(Choice A)\n    A --> C(Choice B)\n    B --> A\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Start Node")
                && screen.contains("Choice A")
                && screen.contains("Choice B")
                && screen.contains("↺")
        })
        .expect("branching flowchart with cycle should render nodes and loop indicator");
}

#[test]
fn mermaid_sequence_diagram_aliases_and_notes() {
    let md = "# Title\n\n```mermaid\nsequenceDiagram\n    participant A as Alice\n    participant B as Bob\n    A->>B: Ping\n    note over A, B: Working\n    B-->>A: Pong\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Alice")
                && screen.contains("Bob")
                && screen.contains("Ping")
                && screen.contains("Pong")
                && screen.contains("Working")
        })
        .expect("sequence diagram should resolve participant aliases and render notes");

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("B-"),
        "participant B should not be corrupted by arrow dashes"
    );
}

#[test]
fn mermaid_pie_chart_title_on_next_line() {
    let md = "# Title\n\n```mermaid\npie\n    title Browser Share\n    \"Chrome\" : 65\n    \"Firefox\" : 20\n```\n\nTail.\n";
    let (mut harness, _tmp) = compose_harness(md);

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Browser Share")
                && screen.contains("Chrome")
                && screen.contains("Firefox")
        })
        .expect("pie chart should parse title on next line");
}
