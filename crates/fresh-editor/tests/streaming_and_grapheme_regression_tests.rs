//! Regression tests for two fixes that came out of the wrapped-line
//! navigation performance review:
//!
//! * `extend_streaming` mutates buffer content (appends bytes) but did
//!   not bump `buffer.version()`, so version-keyed layout caches
//!   (visual-row index, line-wrap cache) could remain reachable with
//!   pre-append state.
//!
//! * `prev_grapheme_boundary` stepped one cluster too far when given a
//!   position INSIDE a multi-byte code point whose cluster is a single
//!   code point (accented latin, CJK): the snapped char boundary IS the
//!   containing cluster's start, and stepping `prev_boundary` from it
//!   overshoots.

mod common;

use common::harness::EditorTestHarness;

/// `extend_streaming` appends content and must bump `buffer.version()`
/// like every other mutation path — layout caches key on the version,
/// so an unchanged version can serve pre-append layout.
#[test]
fn extend_streaming_bumps_buffer_version() {
    use std::io::Write;

    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join("stream.txt");
    std::fs::write(&path, "hello\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let before = harness.editor().active_state().buffer.version();

    // The stream grows on disk; the editor is told to extend.
    let mut f = std::fs::OpenOptions::new()
        .append(true)
        .open(&path)
        .unwrap();
    f.write_all(b"appended\n").unwrap();
    f.flush().unwrap();
    let new_size = std::fs::metadata(&path).unwrap().len() as usize;
    harness
        .editor_mut()
        .active_state_mut()
        .buffer
        .extend_streaming(&path, new_size);

    let after = harness.editor().active_state().buffer.version();
    assert_ne!(
        before,
        after,
        "extend_streaming appended {} bytes but buffer.version() did not change — \
         version-keyed layout caches will serve pre-append content",
        new_size - 6,
    );
}

/// For a position INSIDE a multi-byte code point,
/// `prev_grapheme_boundary` must return the start of the CONTAINING
/// cluster, not the boundary one cluster earlier.
#[test]
fn prev_grapheme_boundary_mid_code_point_returns_containing_cluster_start() {
    use fresh::primitives::grapheme::prev_grapheme_boundary;

    // "aé": 'a' at 0, 'é' occupies bytes 1..3.  Position 2 is inside
    // 'é'; its containing cluster starts at 1.
    assert_eq!(prev_grapheme_boundary("aé", 2), 1);

    // Sanity: a position on a char boundary INSIDE a multi-code-point
    // cluster still resolves to the cluster start...
    assert_eq!(prev_grapheme_boundary("ที่", 3), 0);
    // ...and a position on a cluster boundary steps to the previous one.
    assert_eq!(prev_grapheme_boundary("aé", 1), 0);
    assert_eq!(prev_grapheme_boundary("aé", 3), 1);
}
