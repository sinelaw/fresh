//! Opening a large file must not do work proportional to the file — issues
//! #3142 and #1806, which share that cause.
//!
//! * #3142: a large *binary* file was read whole into memory and line-indexed
//!   before the open returned. A 2.1 GB zip froze the editor for ~13 s and
//!   peaked at ~2.5 GB resident; the reporter's machine OOM'd instead.
//! * #1806: a large *single-line* file paid the wrap machine over the whole
//!   line to answer viewport questions bounded by the screen. A 53 MB
//!   single-line JSON took ~19 s to first render against 0.24 s for the
//!   byte-identical multi-line control.
//!
//! Both are pinned on **bytes resident after the open** rather than on the
//! clock. That is the property in both bugs — work proportional to the file —
//! and unlike a wall-clock or RSS bound it means the same thing on a loaded CI
//! runner as on an idle laptop. Timings are printed as diagnostics only.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use std::io::Write;
use std::time::Instant;

const W: u16 = 120;
const H: u16 = 40;

fn wrapping_config(large_file_threshold_bytes: u64) -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.large_file_threshold_bytes = large_file_threshold_bytes;
    config
}

/// Bytes of the opened file the editor is holding in memory.
fn resident_bytes(harness: &EditorTestHarness) -> usize {
    harness.editor().active_state().buffer.resident_bytes()
}

/// `[0,1,2,...]` on a single line, plus the byte-identical control with every
/// comma turned into a newline. Same bytes, same length, different shape —
/// which is exactly what makes the comparison below meaningful.
fn single_line_and_control(values: usize) -> (String, String) {
    let mut single = String::from("[");
    for i in 0..values {
        if i > 0 {
            single.push(',');
        }
        single.push_str(&i.to_string());
    }
    single.push(']');
    let control = single.replace(',', "\n");
    (single, control)
}

/// Issue #1806. Opening a file that is one enormous line pulls in about as
/// little of it as opening its byte-identical multi-line twin does.
///
/// Before the per-line row-count cap, deciding where the viewport could sit
/// wrapped the whole logical line — which meant reading all of it, chunk by
/// chunk, into memory. The multi-line control is the same bytes in a shape
/// that never triggered that, so the two resident figures being alike is the
/// statement that the single-line shape no longer costs extra.
#[test]
fn single_line_open_reads_no_more_than_multi_line() {
    // ~19 MB each. Large enough that reading the whole line is unmistakable
    // against the bound below, small enough to write in well under a second.
    let (single, control) = single_line_and_control(2_600_000);
    let file_bytes = single.len();
    let dir = tempfile::tempdir().unwrap();

    let single_path = dir.path().join("one_line.json");
    std::fs::File::create(&single_path)
        .unwrap()
        .write_all(single.as_bytes())
        .unwrap();
    let control_path = dir.path().join("many_lines.txt");
    std::fs::File::create(&control_path)
        .unwrap()
        .write_all(control.as_bytes())
        .unwrap();
    assert_eq!(control.len(), file_bytes, "the control must be identical");

    // Threshold well under both files, so this is the large-file (lazy) path.
    let mut open = |path: &std::path::Path| {
        let mut harness =
            EditorTestHarness::with_config(W, H, wrapping_config(1024 * 1024)).unwrap();
        let started = Instant::now();
        harness.open_file(path).unwrap();
        let elapsed = started.elapsed();
        assert!(!harness.screen_to_string().is_empty());
        (resident_bytes(&harness), elapsed)
    };

    let (control_resident, control_time) = open(&control_path);
    let (single_resident, single_time) = open(&single_path);

    eprintln!(
        "single-line: {single_resident} bytes resident in {single_time:?}; \
         multi-line control: {control_resident} bytes in {control_time:?} \
         (file is {file_bytes} bytes)"
    );

    // Ten times the control, and still an order of magnitude under the file:
    // loose enough to absorb the odd extra chunk the renderer touches, tight
    // enough that reading the line to wrap it cannot pass.
    let budget = (control_resident * 10).max(1024 * 1024);
    assert!(
        single_resident < budget,
        "opening a {file_bytes}-byte single-line file left {single_resident} bytes \
         resident against a {control_resident}-byte control (budget {budget}) — \
         per-line work is unbounded again"
    );
}

/// Issue #3142. A large binary file opens without being read into memory at
/// all: the file becomes one unread piece, and nothing indexes its lines.
#[test]
fn large_binary_open_does_not_read_the_file() {
    // 64 MB of binary. The old path read every byte and built a line-start
    // index over them before `open_file` returned.
    const FILE_MB: usize = 64;

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("big.bin");
    {
        let mut file = std::fs::File::create(&path).unwrap();
        // A 1 MB block with NUL bytes in it — binary by any detector, and
        // sprinkled with newlines so the old line index would have been large.
        let mut block = vec![0u8; 1024 * 1024];
        for (i, b) in block.iter_mut().enumerate() {
            *b = match i % 256 {
                0 => 0x00,
                10 => b'\n',
                n => n as u8,
            };
        }
        for _ in 0..FILE_MB {
            file.write_all(&block).unwrap();
        }
    }

    let mut harness = EditorTestHarness::with_config(W, H, wrapping_config(1024 * 1024)).unwrap();
    let started = Instant::now();
    harness.open_file(&path).unwrap();
    let elapsed = started.elapsed();

    let resident = resident_bytes(&harness);
    eprintln!("opened {FILE_MB} MB binary in {elapsed:?}, {resident} bytes resident");

    let buffer = &harness.editor().active_state().buffer;
    assert!(buffer.is_binary(), "NUL bytes must trip binary detection");
    assert_eq!(
        buffer.total_bytes(),
        FILE_MB * 1024 * 1024,
        "the whole file is still addressable"
    );
    assert_eq!(
        buffer.line_count(),
        None,
        "no line index is built over the file at open"
    );
    // The first render legitimately pulls the chunks it draws; everything
    // beyond that would be the file being read for its own sake.
    assert!(
        resident < 4 * 1024 * 1024,
        "opening a {FILE_MB} MB binary left {resident} bytes resident — it is being read whole"
    );

    // And it is still the binary buffer the user expects.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[BIN]"),
        "expected a binary buffer, got:\n{screen}"
    );
}

/// The bytes are still there — laziness moved the read, it did not drop it.
/// A binary buffer is served from the file as the viewport asks for it.
#[test]
fn large_binary_content_still_renders() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("marked.bin");
    let mut data = b"FRESHMARK\x00\x01\x02".to_vec();
    data.resize(4 * 1024 * 1024, 0x41); // 'A' padding
    std::fs::File::create(&path)
        .unwrap()
        .write_all(&data)
        .unwrap();

    let mut harness = EditorTestHarness::with_config(W, H, wrapping_config(1024 * 1024)).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("FRESHMARK"),
        "the first bytes of the file should be on screen:\n{screen}"
    );
}
