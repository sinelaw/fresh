//! E2E: `.h` headers highlight with the grammar that detection actually
//! resolved (issue #3009).
//!
//! `.h` is mapped to C by the default `[languages]` table, but the LSP-side
//! detection already promotes a header to `cpp` when the surrounding tree
//! smells like C++ (a sibling C++ source, an ancestor `compile_commands.json`).
//! That promotion used to rename the buffer's language id only: the grammar,
//! the status-bar label and every colour on screen still came from the C
//! extension table. These tests open the *same header bytes* in two trees and
//! assert on rendered output only.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use ratatui::style::Color;

/// Same bytes in both trees. `namespace` / `class` / `virtual` are C++
/// keywords and plain identifiers in C; `counter` is a plain identifier in
/// both, so it is the per-buffer reference for "unhighlighted".
const HEADER: &str = "\
namespace ui {
class Widget {
public:
    virtual void draw();
};
}
int counter;
";

fn create_harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// Foreground colour of the first cell of `text` on screen.
fn fg_at(harness: &EditorTestHarness, text: &str) -> Color {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("'{text}' not found on screen"));
    harness
        .get_cell_style(col, row)
        .and_then(|s| s.fg)
        .unwrap_or_else(|| panic!("no fg style at '{text}' ({col},{row})"))
}

/// A header next to a C++ source is C++: the status bar says so and the C++
/// keywords are painted with the keyword colour rather than plain foreground.
#[test]
fn test_h_header_beside_cpp_source_highlights_as_cpp() {
    let mut harness = create_harness();
    let dir = harness.project_dir().unwrap().join("cpp_tree");
    std::fs::create_dir_all(&dir).unwrap();
    let header = dir.join("widget.h");
    std::fs::write(&header, HEADER).unwrap();
    // The decisive C++ signal — a sibling translation unit.
    std::fs::write(dir.join("widget.cpp"), "#include \"widget.h\"\n").unwrap();

    harness.open_file(&header).unwrap();
    harness.render().unwrap();

    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C++"),
        "status bar should report C++ for a header in a C++ tree. Got: {status_bar}"
    );

    let plain = fg_at(&harness, "counter");
    assert_ne!(
        fg_at(&harness, "namespace"),
        plain,
        "`namespace` must render as a keyword, not plain foreground"
    );
    assert_ne!(
        fg_at(&harness, "virtual"),
        plain,
        "`virtual` must render as a keyword, not plain foreground"
    );
}

/// The same bytes in a plain C project keep the C grammar: `namespace` and
/// `virtual` are ordinary identifiers there and must render unhighlighted.
#[test]
fn test_h_header_in_pure_c_project_stays_c() {
    let mut harness = create_harness();
    let dir = harness.project_dir().unwrap().join("c_tree");
    std::fs::create_dir_all(&dir).unwrap();
    let header = dir.join("widget.h");
    std::fs::write(&header, HEADER).unwrap();
    // Only C siblings — no C++ signal anywhere.
    std::fs::write(dir.join("widget.c"), "#include \"widget.h\"\n").unwrap();

    harness.open_file(&header).unwrap();
    harness.render().unwrap();

    let status_bar = harness.get_status_bar();
    assert!(
        !status_bar.contains("C++"),
        "status bar must not report C++ for a header in a pure C tree. Got: {status_bar}"
    );

    let plain = fg_at(&harness, "counter");
    assert_eq!(
        fg_at(&harness, "namespace"),
        plain,
        "`namespace` is an ordinary identifier in C and must not be highlighted"
    );
    assert_eq!(
        fg_at(&harness, "virtual"),
        plain,
        "`virtual` is an ordinary identifier in C and must not be highlighted"
    );
}
