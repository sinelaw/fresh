//! Opening a large file must not block the editor for work proportional to
//! the file — issue #3142, where a large *binary* file was read whole into
//! memory and line-indexed before the open returned. A 2.1 GB zip froze the
//! editor for ~13 s and peaked at ~2.5 GB resident; the reporter's machine
//! OOM'd instead.
//!
//! Pinned here with generous bounds: the fixed path is bounded by the
//! *viewport*, so it finishes in a fraction of a second at any file size, and
//! the margins below are tens of times what it needs.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use std::io::Write;
use std::time::{Duration, Instant};

const W: u16 = 120;
const H: u16 = 40;

fn wrapping_config(large_file_threshold_bytes: u64) -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.large_file_threshold_bytes = large_file_threshold_bytes;
    config
}

#[cfg(target_os = "linux")]
fn current_rss_kb() -> u64 {
    let status = std::fs::read_to_string("/proc/self/status").unwrap_or_default();
    for line in status.lines() {
        if let Some(val) = line.strip_prefix("VmRSS:") {
            return val
                .trim()
                .trim_end_matches("kB")
                .trim_end_matches("KB")
                .trim()
                .parse()
                .unwrap_or(0);
        }
    }
    0
}

#[cfg(not(target_os = "linux"))]
fn current_rss_kb() -> u64 {
    0
}

/// Issue #3142. A large binary file opens without being read into memory:
/// resident memory must not track the file's size, and the open must return
/// promptly rather than after a full read plus line-index scan.
#[test]
fn large_binary_open_does_not_read_the_file() {
    if current_rss_kb() == 0 {
        eprintln!("Skipping: cannot measure RSS on this platform");
        return;
    }

    // 64 MB of binary. The old path would have added ~64 MB of buffer plus a
    // line-start index; the bound below is a fraction of that, so it fails on
    // any variant that slurps.
    const FILE_MB: u64 = 64;
    const MAX_GROWTH_MB: u64 = 16;

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
    // Draw once so the harness's own steady-state allocations are in the
    // baseline and only the open is measured.
    harness.render().unwrap();
    let baseline = current_rss_kb();

    let started = Instant::now();
    harness.open_file(&path).unwrap();
    let elapsed = started.elapsed();
    let growth_mb = current_rss_kb().saturating_sub(baseline) / 1024;

    eprintln!("opened {FILE_MB} MB binary in {elapsed:?}, RSS +{growth_mb} MB");

    assert!(
        growth_mb < MAX_GROWTH_MB,
        "opening a {FILE_MB} MB binary grew RSS by {growth_mb} MB — it is being read whole"
    );
    assert!(
        elapsed < Duration::from_secs(5),
        "opening a {FILE_MB} MB binary took {elapsed:?}"
    );

    // And it is still the binary buffer the user expects: read-only, with the
    // large-file byte gutter rather than line numbers.
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
