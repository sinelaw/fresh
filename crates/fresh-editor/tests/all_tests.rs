//! Every `fresh-editor` integration test, in one binary.
//!
//! Each test root used to be its own `tests/*.rs`, and cargo builds one
//! binary per root -- each statically linking the whole editor and its
//! dependency graph. Compiling and linking those separately dominated the
//! cost of `cargo test`, so they are pulled in here as modules instead.
//! The files stay where they are; `#[path]` keeps `include_str!` and the
//! insta snapshot paths pointing at the same places as before.
//!
//! The root list is generated: `build.rs` scans `tests/` and writes the `mod`
//! declarations, so adding a test file needs no edit here. `autotests = false`
//! in Cargo.toml is what makes that necessary -- with cargo's own discovery
//! off, a root nothing names compiles into nothing, silently.

mod common;
mod e2e;
mod semantic;

// The generated `mod` declarations, plus `GENERATED_ROOTS` naming them.
include!(concat!(env!("OUT_DIR"), "/test_roots.rs"));

/// The generated root list must still match what is on disk.
///
/// Generation removes the need to maintain a list, but not the failure mode
/// underneath it: if the build script does not re-run, a new root is absent
/// from `OUT_DIR` and its tests do not run -- silently, exactly as when the
/// list was written by hand. `build.rs` declares `tests/` as a rerun trigger,
/// so this should be unreachable; it is here because "should" is doing the
/// work, and the cost of being wrong is coverage that disappears without a
/// symptom.
#[test]
fn generated_root_list_matches_the_directory() {
    // Declared in Cargo.toml as their own targets instead (feature-gated).
    // Kept in step with the same list in build.rs.
    const SEPARATE_TARGETS: &[&str] = &["scene_parity"];

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let mut on_disk: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            if !entry.path().is_file() {
                return None;
            }
            let name = entry.file_name().to_string_lossy().into_owned();
            let stem = name.strip_suffix(".rs")?.to_string();
            if stem == "all_tests" || SEPARATE_TARGETS.contains(&stem.as_str()) {
                return None;
            }
            Some(stem)
        })
        .collect();
    on_disk.sort();

    let generated: Vec<String> = GENERATED_ROOTS.iter().map(|s| s.to_string()).collect();

    let missing: Vec<&String> = on_disk.iter().filter(|r| !generated.contains(r)).collect();
    let stale: Vec<&String> = generated.iter().filter(|r| !on_disk.contains(r)).collect();

    assert!(
        missing.is_empty() && stale.is_empty(),
        "the generated test-root list is out of step with tests/.\n\
         on disk but not compiled in (their tests are NOT running): {missing:?}\n\
         compiled in but no longer on disk: {stale:?}\n\
         `touch crates/fresh-editor/build.rs` and rebuild; if that fixes it, the \
         rerun trigger in generate_test_roots() is not firing."
    );
}
