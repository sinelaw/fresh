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
//! clock: that is the property in both bugs, and unlike a wall-clock or RSS
//! bound it means the same on a loaded CI runner as on an idle laptop.

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

/// `[0,1,2,...]` on one line, plus the byte-identical control with every comma
/// turned into a newline. Same bytes, different shape — which is what makes the
/// comparison meaningful.
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
/// Placing the viewport used to wrap the whole logical line, reading all of it
/// in. The control is the same bytes in a shape that never did, so the two
/// resident figures being alike is the statement that shape costs nothing.
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
    let open = |path: &std::path::Path| {
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

    // Loose enough for the odd extra chunk the renderer touches, tight enough
    // that reading the line to wrap it cannot pass.
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
    // The first render legitimately pulls the chunks it draws; more than that
    // would be the file being read for its own sake.
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

/// Issue #1806, the other half: `Down` on a file that is one enormous line did
/// nothing at all — the view stayed on the first screenful however long the key
/// was held, so the rest of the file was unreachable by keyboard.
///
/// A lazily-loaded buffer is the case that broke: no wrap index, so the
/// byte-oriented pass alone places the viewport.
///
/// Asserts on rendered output (CONTRIBUTING §2): the file is one line of
/// ascending markers, so the topmost marker says where the viewport is.
#[test]
fn arrow_down_scrolls_a_lazily_loaded_single_line_file() {
    use crossterm::event::{KeyCode, KeyModifiers};

    // ~320 KB on one line. Every marker is 8 bytes, so the marker at the top
    // of the screen is the viewport position, in a form the screen can show.
    let mut content = String::new();
    for i in 0..40_000u32 {
        content.push_str(&format!("M{i:06} "));
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("one_line.txt");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    // Threshold far under the file, so this is the lazy path with no index.
    let mut harness = EditorTestHarness::with_config(W, H, wrapping_config(1024)).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let first_screen = harness.screen_to_string();
    assert!(
        first_screen.contains("Byte "),
        "the status bar should report a byte offset, i.e. this is the \
         large-file path the bug lives on:\n{first_screen}"
    );
    let (top_before, bottom_before) =
        marker_range(&first_screen).expect("the first screenful should show markers");

    // Enough that the new top is below the first screenful's bottom, so a
    // single row of drift cannot pass for scrolling.
    for _ in 0..100 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let scrolled = harness.screen_to_string();
    let (top_after, _) = marker_range(&scrolled).expect("markers should still be on screen");
    assert!(
        top_after > bottom_before,
        "after 100 Down presses the view should have scrolled past its first \
         screenful (top marker M{top_before:06}..M{bottom_before:06} before), but the \
         top of the screen is M{top_after:06} — the arrow key is not scrolling:\n{scrolled}"
    );
}

/// Lowest and highest `M`-prefixed six-digit marker on screen. The markers
/// ascend along the line, so this is which part of the file is displayed.
fn marker_range(screen: &str) -> Option<(u32, u32)> {
    let mut lo: Option<u32> = None;
    let mut hi: Option<u32> = None;
    for line in screen.lines() {
        let bytes = line.as_bytes();
        for (i, _) in line.match_indices('M') {
            let digits = &bytes[i + 1..];
            if digits.len() >= 6 && digits[..6].iter().all(|b| b.is_ascii_digit()) {
                if let Ok(v) = std::str::from_utf8(&digits[..6]).ok()?.parse::<u32>() {
                    lo = Some(lo.map_or(v, |x| x.min(v)));
                    hi = Some(hi.map_or(v, |x| x.max(v)));
                }
            }
        }
    }
    lo.zip(hi)
}

/// Issue #1806, deeper in: walking down one enormous line has to keep working
/// tens of thousands of characters into it.
///
/// The original wedge: `build_base_tokens` forced a row break every
/// `MAX_SAFE_LINE_WIDTH` characters counted from wherever its read began, while
/// the viewport wrapped the same text as one run, so the two drifted a row per
/// 10,000 characters and `Down` died about 40,000 in. That break is gone —
/// `WrapRule::Chop` owns the bound — and this holds the behaviour it cost.
///
/// A wide terminal keeps the press count small: the old wedge was at a
/// character count, not a row count.
#[test]
fn arrow_down_walks_deep_into_one_enormous_line() {
    use crossterm::event::{KeyCode, KeyModifiers};

    const WIDE: u16 = 1000;
    const SHORT: u16 = 12;
    /// Comfortably past the fourth forced break at 40,000 characters.
    const PAST_THE_WEDGE_BYTES: u32 = 60_000;

    let mut content = String::new();
    for i in 0..20_000u32 {
        content.push_str(&format!("M{i:06} "));
    }
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("one_line.txt");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    let mut harness = EditorTestHarness::with_config(WIDE, SHORT, wrapping_config(1024)).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // ~990 bytes a row here, so this walks well past 60,000 bytes into the
    // line — and each press has to land on a row that was actually drawn.
    for _ in 0..90 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let (top_after, _) = marker_range(&screen).expect("markers should be on screen");
    assert!(
        top_after * 8 > PAST_THE_WEDGE_BYTES,
        "after 90 Down presses the top of the screen should be past byte \
         {PAST_THE_WEDGE_BYTES} of the line, but it is M{top_after:06} (byte {}) — the walk \
         is stuck at the renderer's forced break:\n{screen}",
        top_after * 8
    );
}

/// Issue #1806, the last wall: paging down one enormous line left the cursor
/// behind at the 100,000-byte read boundary while the view scrolled on past
/// it.
///
/// The lookup landing the cursor on the new top row read one `MAX_LINE_BYTES`
/// piece and clamped the row index into it, pinning the cursor at that piece's
/// last row — and a cursor off the drawn window cannot be moved at all.
///
/// Asserts on rendered output (CONTRIBUTING §2): the status bar reports the
/// cursor's byte offset in large-file mode.
#[test]
fn paging_down_one_long_line_carries_the_cursor_past_the_read_boundary() {
    use crossterm::event::{KeyCode, KeyModifiers};

    /// The boundary the cursor used to stop at: `MAX_LINE_BYTES`.
    const READ_PIECE_BYTES: usize = 100_000;

    // ~320 KB on one line — several read pieces.
    let mut content = String::new();
    for i in 0..40_000u32 {
        content.push_str(&format!("M{i:06} "));
    }
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("one_line.txt");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    let mut harness = EditorTestHarness::with_config(W, H, wrapping_config(1024)).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Enough pages to be well past the first read piece.
    for _ in 0..40 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let cursor_byte = status_bar_byte(&screen)
        .expect("large-file mode reports the cursor's byte offset in the status bar");
    assert!(
        cursor_byte > READ_PIECE_BYTES,
        "after 40 PageDown presses the cursor should be well past byte \
         {READ_PIECE_BYTES} of the line, but the status bar reads Byte {cursor_byte} — it is \
         pinned to the end of the first read piece while the view scrolled \
         on:\n{screen}"
    );
}

/// The cursor offset the status bar reports in large-file mode (`Byte N`).
fn status_bar_byte(screen: &str) -> Option<usize> {
    let at = screen.find("Byte ")?;
    screen[at + "Byte ".len()..]
        .split_whitespace()
        .next()?
        .parse()
        .ok()
}

/// Issue #1806, reported against this branch: holding `Down` eventually stopped
/// advancing and cycled between a few positions around byte 99,999 — one short
/// of the `MAX_LINE_BYTES` boundary the reader splits the line on.
///
/// Sized to just cross that boundary; a wide terminal reaches it in fewer
/// presses. Every press must advance the cursor: a walk down a line visits each
/// row once, so revisiting one is the cycle.
#[test]
fn arrow_down_never_revisits_a_row_across_the_read_boundary() {
    use crossterm::event::{KeyCode, KeyModifiers};

    const WIDE: u16 = 1000;
    const SHORT: u16 = 12;
    /// `LineIterator` splits an over-long line here.
    const READ_PIECE_BYTES: usize = 100_000;

    // `[0,1,2,...,39999]` — `big50.json`'s shape, ~229 KB on one line: several
    // read pieces, and comfortably more rows than the walk below asks for, so
    // the end of the file is never what stops it.
    let mut content = String::from("[");
    for i in 0..40_000u32 {
        if i > 0 {
            content.push(',');
        }
        content.push_str(&i.to_string());
    }
    content.push(']');
    assert!(
        content.len() > READ_PIECE_BYTES + 1000,
        "must cross the boundary"
    );

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("big.json");
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    let mut harness = EditorTestHarness::with_config(WIDE, SHORT, wrapping_config(1024)).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    let mut seen: Vec<usize> = Vec::new();
    let mut previous = 0usize;
    for press in 1..=140 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        let at = status_bar_byte(&screen).expect("the status bar reports the cursor's byte");
        assert!(
            at > previous,
            "press {press} moved the cursor from byte {previous} to {at} — it is not \
             advancing. The last presses went {:?}, and the read-piece boundary is at \
             {READ_PIECE_BYTES}:\n{screen}",
            &seen[seen.len().saturating_sub(6)..]
        );
        previous = at;
        seen.push(at);
    }

    assert!(
        previous > READ_PIECE_BYTES,
        "140 presses should have carried the cursor past byte {READ_PIECE_BYTES}, \
         got {previous} — the walk is too slow to have tested the boundary at all"
    );
}
