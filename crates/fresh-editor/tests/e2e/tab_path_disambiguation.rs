//! E2E: tabs for same-named files identify themselves by path (issue #2851).
//!
//! Opening several files that share a name — `mod.rs`, `index.ts`, `main.rs` —
//! used to produce tabs labelled `mod.rs 1` / `mod.rs 2`, which says nothing
//! about which file each tab holds. Each colliding tab now carries the shortest
//! trailing path fragment that tells it apart from the others.

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use std::path::MAIN_SEPARATOR_STR;
use tempfile::TempDir;

/// Join `parts` with the platform separator, matching how the tab bar renders
/// a disambiguated label.
fn joined(parts: &[&str]) -> String {
    parts.join(MAIN_SEPARATOR_STR)
}

/// Open a harness rooted at a temp project directory.
fn harness_in(dir: &TempDir) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_working_dir(dir.path().to_path_buf())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

#[test]
fn same_named_tabs_show_their_distinguishing_directory() {
    let temp = TempDir::new().unwrap();
    let model = temp.path().join("model");
    let view = temp.path().join("view");
    std::fs::create_dir_all(&model).unwrap();
    std::fs::create_dir_all(&view).unwrap();
    let model_mod = model.join("mod.rs");
    let view_mod = view.join("mod.rs");
    std::fs::write(&model_mod, "// model\n").unwrap();
    std::fs::write(&view_mod, "// view\n").unwrap();

    let mut harness = harness_in(&temp);
    harness.open_file(&model_mod).unwrap();
    harness.open_file(&view_mod).unwrap();
    harness.render().unwrap();

    let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
    assert!(
        tab_bar.contains(&joined(&["model", "mod.rs"])),
        "the model tab should name its directory.\nTab bar: {tab_bar}"
    );
    assert!(
        tab_bar.contains(&joined(&["view", "mod.rs"])),
        "the view tab should name its directory.\nTab bar: {tab_bar}"
    );
    assert!(
        !tab_bar.contains("mod.rs 1") && !tab_bar.contains("mod.rs 2"),
        "same-named tabs should be told apart by path, not by a number.\nTab bar: {tab_bar}"
    );
}

#[test]
fn a_uniquely_named_tab_keeps_its_bare_file_name() {
    let temp = TempDir::new().unwrap();
    let alpha = temp.path().join("alpha");
    let beta = temp.path().join("beta");
    std::fs::create_dir_all(&alpha).unwrap();
    std::fs::create_dir_all(&beta).unwrap();
    let alpha_mod = alpha.join("mod.rs");
    let beta_mod = beta.join("mod.rs");
    let solo = alpha.join("unique_name.rs");
    std::fs::write(&alpha_mod, "// a\n").unwrap();
    std::fs::write(&beta_mod, "// b\n").unwrap();
    std::fs::write(&solo, "// solo\n").unwrap();

    let mut harness = harness_in(&temp);
    harness.open_file(&alpha_mod).unwrap();
    harness.open_file(&beta_mod).unwrap();
    harness.open_file(&solo).unwrap();
    harness.render().unwrap();

    let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
    assert!(
        tab_bar.contains(&joined(&["alpha", "mod.rs"])),
        "colliding tab should carry its directory.\nTab bar: {tab_bar}"
    );
    assert!(
        tab_bar.contains(&joined(&["beta", "mod.rs"])),
        "colliding tab should carry its directory.\nTab bar: {tab_bar}"
    );
    assert!(
        !tab_bar.contains(&joined(&["alpha", "unique_name.rs"])),
        "a tab whose name is already unique should not grow a path.\nTab bar: {tab_bar}"
    );
    assert!(
        tab_bar.contains("unique_name.rs"),
        "the uniquely named tab should still be there.\nTab bar: {tab_bar}"
    );
}

#[test]
fn disambiguation_grows_only_until_the_paths_differ() {
    // Both files live in `.../src/main.rs`, so one directory is not enough —
    // the labels must reach up to `a` / `b`, and stop there.
    let temp = TempDir::new().unwrap();
    let a_src = temp.path().join("a").join("src");
    let b_src = temp.path().join("b").join("src");
    std::fs::create_dir_all(&a_src).unwrap();
    std::fs::create_dir_all(&b_src).unwrap();
    let a_main = a_src.join("main.rs");
    let b_main = b_src.join("main.rs");
    std::fs::write(&a_main, "// a\n").unwrap();
    std::fs::write(&b_main, "// b\n").unwrap();

    let mut harness = harness_in(&temp);
    harness.open_file(&a_main).unwrap();
    harness.open_file(&b_main).unwrap();
    harness.render().unwrap();

    let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
    assert!(
        tab_bar.contains(&joined(&["a", "src", "main.rs"])),
        "one shared directory is not enough to disambiguate.\nTab bar: {tab_bar}"
    );
    assert!(
        tab_bar.contains(&joined(&["b", "src", "main.rs"])),
        "one shared directory is not enough to disambiguate.\nTab bar: {tab_bar}"
    );
    // The temp directory above `a` / `b` is shared too, so it must not appear:
    // the label stops at the first component that differs.
    let temp_name = temp
        .path()
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    assert!(
        !tab_bar.contains(&joined(&[&temp_name, "a", "src", "main.rs"])),
        "the label should stop at the first differing component.\nTab bar: {tab_bar}"
    );
}
