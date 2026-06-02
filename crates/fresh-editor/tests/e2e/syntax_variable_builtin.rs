use crate::common::harness::{EditorTestHarness, HarnessOptions};

#[test]
fn test_js_variable_builtin_highlight() {
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    let js_file = project_dir.join("test.js");
    std::fs::write(
        &js_file,
        "class A {\n    constructor() {\n        this.foo = 1;\n    }\n}\n",
    )
    .unwrap();
    harness.open_file(&js_file).unwrap();
    harness.render().unwrap();

    let expected_color = harness.editor().theme().syntax_variable_builtin;
    let mut found = false;
    for y in 0..10 {
        for x in 0..120 {
            if let Some(style) = harness.get_cell_style(x, y) {
                let c = harness.get_cell(x, y);
                if style.fg == Some(expected_color) {
                    if c == Some("t".to_string()) {
                        found = true;
                    }
                }
            }
        }
    }

    assert!(
        found,
        "Did not find expected variable_builtin color for 'this'"
    );
}
