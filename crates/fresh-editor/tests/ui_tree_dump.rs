//! `Action::DumpUiTree` opens the retained shell tree in a read-only buffer.
mod common;
use common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;

#[test]
fn dump_ui_tree_opens_the_tree_in_a_read_only_buffer() {
    let mut h = EditorTestHarness::new(100, 30).unwrap();
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join("hello.rs");
    std::fs::write(&p, "fn main() {}\n").unwrap();
    h.open_file(&p).unwrap();

    h.editor_mut().dispatch_action_for_tests(Action::DumpUiTree);
    h.render().unwrap();

    let text = h.get_buffer_content().unwrap();
    for needle in [
        "\"key\": \"chrome_column\"",
        "\"key\": \"pane_content:0\"",
        "\"type\": \"TextRun\"",
        "\"rect\": {\"x\": 0, \"y\": 0, \"w\": 100, \"h\": 30}",
        "\"text\": \" File  \"",
    ] {
        assert!(text.contains(needle), "missing {needle:?} in:\n{text}");
    }
    // A program is the point of the JSON shape, so parse it like one would.
    let root: serde_json::Value = serde_json::from_str(&text).expect("valid JSON");
    assert_eq!(root["rect"]["w"], 100);
    assert!(root["children"].is_array());
    let screen = h.screen_to_string();
    assert!(screen.contains("*ui-tree*"), "tab missing:\n{screen}");
    assert!(
        screen.contains("[RO]"),
        "read-only marker missing:\n{screen}"
    );
}
