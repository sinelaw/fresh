//! Editing and moving in a file that is one enormous line must cost the
//! screen, not the file.
//!
//! #3162 made *opening* such a file bounded. What it left behind is everything
//! that happens next: a frame, a keystroke and an arrow press each re-asked
//! questions whose answers scale with the line — "where does this line start",
//! "where does it end", "how many lines are below this one" — and answered them
//! by reading the line. On a 50 MB single-line JSON that is megabytes of
//! reading per keypress, which is what "editing is still incredibly slow" was.
//!
//! Pinned on **bytes read from the buffer**, not on the clock, so these mean
//! the same on a loaded CI runner as on an idle laptop (CONTRIBUTING §3). Each
//! bound is stated against the byte-identical multi-line control: same bytes,
//! different shape, so a gap between the two is the shape costing something.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh_editor_core::counters::work;
use std::io::Write;

const W: u16 = 120;
const H: u16 = 40;

fn config(line_wrap: bool) -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = line_wrap;
    // Far under both files, so this is the lazily-loaded large-file path.
    config.editor.large_file_threshold_bytes = 1024 * 1024;
    config
}

/// `[0,1,2,...]` on one line — no whitespace anywhere, the shape of a minified
/// JSON array — plus the byte-identical control with every comma turned into a
/// newline.
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

/// ~19 MB each: large enough that reading a line is unmistakable against the
/// bounds below, small enough to write in well under a second.
const VALUES: usize = 2_600_000;

fn write_pair(dir: &std::path::Path) -> (std::path::PathBuf, std::path::PathBuf, usize) {
    let (single, control) = single_line_and_control(VALUES);
    let bytes = single.len();
    assert_eq!(control.len(), bytes, "the control must be byte-identical");

    let single_path = dir.join("one_line.json");
    std::fs::File::create(&single_path)
        .unwrap()
        .write_all(single.as_bytes())
        .unwrap();
    let control_path = dir.join("many_lines.txt");
    std::fs::File::create(&control_path)
        .unwrap()
        .write_all(control.as_bytes())
        .unwrap();
    (single_path, control_path, bytes)
}

/// Bytes the buffer handed out while `f` ran, on this thread.
fn bytes_read(harness: &mut EditorTestHarness, f: impl FnOnce(&mut EditorTestHarness)) -> u64 {
    work::reset();
    f(harness);
    work::buffer_bytes_read()
}

fn opened(path: &std::path::Path, line_wrap: bool) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config(W, H, config(line_wrap)).unwrap();
    harness.open_file(path).unwrap();
    // Two frames: the first settles the viewport, the second is steady state.
    harness.render().unwrap();
    harness.render().unwrap();
    harness
}

/// A steady-state frame reads a screenful, whatever shape the file is in.
#[test]
fn a_frame_on_a_single_line_file_reads_no_more_than_on_the_control() {
    for line_wrap in [false, true] {
        let dir = tempfile::tempdir().unwrap();
        let (single_path, control_path, file_bytes) = write_pair(dir.path());

        let mut control = opened(&control_path, line_wrap);
        let control_read = bytes_read(&mut control, |h| h.render().unwrap());

        let mut single = opened(&single_path, line_wrap);
        let single_read = bytes_read(&mut single, |h| h.render().unwrap());

        eprintln!(
            "[wrap={line_wrap}] frame: single-line {single_read} bytes, \
             control {control_read} bytes (file is {file_bytes})"
        );
        let budget = (control_read * 20).max(1024 * 1024);
        assert!(
            single_read < budget,
            "a frame on a {file_bytes}-byte single-line file read {single_read} bytes \
             against {control_read} on the byte-identical multi-line control \
             (budget {budget}) — per-line work is back"
        );
    }
}

/// So does a keystroke. This is the reported bug: the file opens, and then
/// every character typed costs the line.
#[test]
fn typing_a_character_reads_no_more_than_on_the_control() {
    for line_wrap in [false, true] {
        let dir = tempfile::tempdir().unwrap();
        let (single_path, control_path, file_bytes) = write_pair(dir.path());

        let mut control = opened(&control_path, line_wrap);
        let control_read = bytes_read(&mut control, |h| h.type_text("x").unwrap());

        let mut single = opened(&single_path, line_wrap);
        let single_read = bytes_read(&mut single, |h| h.type_text("x").unwrap());

        eprintln!(
            "[wrap={line_wrap}] keystroke: single-line {single_read} bytes, \
             control {control_read} bytes (file is {file_bytes})"
        );
        let budget = (control_read * 20).max(1024 * 1024);
        assert!(
            single_read < budget,
            "typing one character into a {file_bytes}-byte single-line file read \
             {single_read} bytes against {control_read} on the byte-identical \
             multi-line control (budget {budget})"
        );
    }
}

/// And an arrow press, which additionally must not walk to the line's start to
/// work out which column it is in.
#[test]
fn arrow_keys_read_no_more_than_on_the_control() {
    for line_wrap in [false, true] {
        let dir = tempfile::tempdir().unwrap();
        let (single_path, control_path, file_bytes) = write_pair(dir.path());

        let press = |h: &mut EditorTestHarness| {
            for _ in 0..10 {
                h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
            }
            h.render().unwrap();
        };

        let mut control = opened(&control_path, line_wrap);
        let control_read = bytes_read(&mut control, press);

        let mut single = opened(&single_path, line_wrap);
        let single_read = bytes_read(&mut single, press);

        eprintln!(
            "[wrap={line_wrap}] ten Downs: single-line {single_read} bytes, \
             control {control_read} bytes (file is {file_bytes})"
        );
        let budget = (control_read * 20).max(4 * 1024 * 1024);
        assert!(
            single_read < budget,
            "ten Down presses in a {file_bytes}-byte single-line file read \
             {single_read} bytes against {control_read} on the byte-identical \
             multi-line control (budget {budget})"
        );
    }
}

/// A frame draws what it can show and segments no more than that.
///
/// With soft wrap off a row used to be chopped at 10,000 columns — a constant
/// with no relation to the pane — so a 40-row frame put ~400,000 characters
/// through Unicode segmentation and width measurement to draw ~4,800.
#[test]
fn a_frame_segments_what_it_draws_and_not_the_chop_width() {
    let dir = tempfile::tempdir().unwrap();
    let (single_path, _, _) = write_pair(dir.path());

    let mut harness = opened(&single_path, false);
    work::reset();
    harness.render().unwrap();
    let measured = work::text_bytes_measured();

    // A generous multiple of what the pane can hold (120 columns × 40 rows),
    // and far under the old row-chop cost.
    let budget = (W as u64) * (H as u64) * 4;
    assert!(
        measured < budget,
        "a frame measured {measured} bytes' width for a {W}×{H} pane (budget \
         {budget}) — rows are being laid out at the safety chop again"
    );
}

/// With soft wrap off, a logical line is one visual row.
///
/// So a file with no line breaks in it draws exactly one row of text and
/// nothing below, and `Down` has nowhere to go. Reading such a file vertically
/// is what soft wrap is for; inventing rows by chopping the line at a fixed
/// column made the pane's contents depend on a constant, and left the viewport
/// and the renderer disagreeing about how many rows there were.
///
/// Asserts on rendered output (CONTRIBUTING §2).
#[test]
fn soft_wrap_off_draws_one_row_for_a_file_with_no_line_breaks() {
    let dir = tempfile::tempdir().unwrap();
    let (single_path, _, _) = write_pair(dir.path());

    let mut harness = opened(&single_path, false);
    let screen = harness.screen_to_string();

    // The first row of the file is drawn...
    assert!(
        screen.contains("[0,1,2,3,4"),
        "the file's first row should be on screen:\n{screen}"
    );
    // ...and the rows below it are empty-buffer filler, not more of the line.
    let filler = screen
        .lines()
        .filter(|l| l.trim_start().starts_with('~'))
        .count();
    assert!(
        filler > H as usize / 2,
        "a file of one logical line should draw one row and filler below it, \
         got {filler} filler rows:\n{screen}"
    );

    // Down has nowhere to go: the cursor stays where it is.
    let byte_before = status_bar_byte(&screen);
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let after = harness.screen_to_string();
    assert_eq!(
        status_bar_byte(&after),
        byte_before,
        "with soft wrap off there is no row below the only row, so Down must \
         not move the cursor:\n{after}"
    );
}

/// With soft wrap on, the same file is readable: rows are pane-width and the
/// cursor walks down them.
#[test]
fn soft_wrap_on_walks_down_the_same_file() {
    let dir = tempfile::tempdir().unwrap();
    let (single_path, _, _) = write_pair(dir.path());

    let mut harness = opened(&single_path, true);
    let before = status_bar_byte(&harness.screen_to_string());

    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let after = status_bar_byte(&screen);

    assert!(
        after > before,
        "ten Down presses with soft wrap on should walk into the line \
         (byte {before:?} -> {after:?}):\n{screen}"
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

/// A line wider than the pane costs its own right-hand end, not the line below
/// it.
///
/// With soft wrap off a logical line is one row, and a row stops at the
/// columns the pane can show. Cutting it there leaves the rest of the line to
/// be stepped over — but only when there is a rest: the reader hands back whole
/// lines when they are short enough, terminator included, and stepping again
/// from there walks past the *next* line and drops it from the frame. On a
/// large file whose lines are merely wider than the window — a log, a CSV,
/// pretty-printed JSON — that is every second line missing from the screen.
///
/// Asserts on rendered output (CONTRIBUTING §2).
#[test]
fn every_wide_line_is_drawn_not_every_other_one() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wide_lines.txt");

    // Lines several times the pane's width, in a file big enough to take the
    // lazily-loaded large-file path.
    let width = W as usize * 4;
    let mut content = String::new();
    let mut line = 0usize;
    while content.len() < 12 * 1024 * 1024 {
        let tag = format!("L{line:06}");
        content.push_str(&tag);
        content.extend(std::iter::repeat_n('.', width - tag.len()));
        content.push('\n');
        line += 1;
    }
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    let mut harness = opened(&path, false);
    let screen = harness.screen_to_string();

    // Every line from the top of the file, in order, one per row.
    let drawn: Vec<usize> = screen
        .lines()
        .filter_map(|row| {
            let i = row.find('L')?;
            row.get(i + 1..i + 7)?.parse::<usize>().ok()
        })
        .collect();

    assert!(
        drawn.len() >= 10,
        "expected the pane to be full of lines, got {drawn:?}:\n{screen}"
    );
    let expected: Vec<usize> = (0..drawn.len()).collect();
    assert_eq!(
        drawn, expected,
        "lines wider than the pane are being skipped: the frame drew {drawn:?} \
         where the file's lines run 0, 1, 2, …\n{screen}"
    );
}

/// A file of long lines is still a file of lines: the arrow keys move between
/// them.
///
/// Every line-boundary search here is bounded, which is what keeps a keypress
/// on a 19 MB single line from scanning the file. A bound is a claim, though —
/// "past here there is no line" — and on a file whose lines are merely long,
/// the claim is false and the motion stops working: nothing above, nothing
/// below, and the cursor stuck on whichever line it landed on.
///
/// End-to-end coverage of the ground between the two files the rest of this
/// module uses — one endless line, or lines shorter than a screen — on the path
/// the arrow keys actually take, which is the rendered view's own rows. The
/// byte-based motions underneath are pinned separately, in
/// `input::actions::tests::vertical_motion_finds_the_line_above_and_below_on_long_lines`.
///
/// Asserts on rendered output (CONTRIBUTING §2).
#[test]
fn arrows_move_between_lines_that_are_merely_long() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("long_lines.txt");

    // Well past the 64 KB that the line-start search used to stop at, and well
    // inside what a line-structured file can ask for.
    let width = 200 * 1024;
    let mut content = String::new();
    for line in 0..64 {
        let tag = format!("L{line:06}");
        content.push_str(&tag);
        content.extend(std::iter::repeat_n('.', width - tag.len()));
        content.push('\n');
    }
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    for line_wrap in [false, true] {
        let mut harness = opened(&path, line_wrap);
        let start = status_bar_byte(&harness.screen_to_string());

        // Down onto the next line, then back up onto the first.
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let down = status_bar_byte(&harness.screen_to_string());
        assert!(
            down > start,
            "[wrap={line_wrap}] Down did not move off the first line \
             (byte {start:?} -> {down:?}); a {width}-byte line is long, not endless"
        );

        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let up = status_bar_byte(&harness.screen_to_string());
        assert_eq!(
            up, start,
            "[wrap={line_wrap}] Up did not come back (byte {down:?} -> {up:?})"
        );
    }
}

/// A viewport over long lines scrolls where it is pointed.
///
/// With soft wrap off a row is a line, so the scroll clamp asks "are there a
/// screenful of line starts below this position" and backs the viewport up when
/// there are not. The walk that answers is bounded — it has to be, on a file
/// whose next line break is megabytes away — but a budget for the walk as a
/// whole rather than per row it is counting runs out partway down a pane full
/// of ten-kilobyte lines. The clamp then concludes the rows are not there and
/// hauls the viewport back up every time it is moved, and the file will not
/// scroll.
///
/// Asserts on rendered output (CONTRIBUTING §2).
#[test]
fn a_pane_of_long_lines_scrolls() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("ten_k_lines.txt");

    // Lines long enough that a screenful of them is more than a flat
    // quarter-megabyte budget, in a file big enough for the large-file path.
    let width = 10 * 1024;
    let mut content = String::new();
    let mut line = 0usize;
    while content.len() < 12 * 1024 * 1024 {
        let tag = format!("L{line:06}");
        content.push_str(&tag);
        content.extend(std::iter::repeat_n('.', width - tag.len()));
        content.push('\n');
        line += 1;
    }
    std::fs::File::create(&path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    let mut harness = opened(&path, false);
    let top_of = |screen: &str| -> Option<usize> {
        screen.lines().find_map(|row| {
            let i = row.find('L')?;
            row.get(i + 1..i + 7)?.parse::<usize>().ok()
        })
    };

    let first = top_of(&harness.screen_to_string());
    assert_eq!(first, Some(0), "the file opens at its first line");

    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let after = top_of(&screen);

    assert!(
        after.is_some_and(|l| l >= H as usize),
        "three PageDowns over {width}-byte lines left the pane at line {after:?}; \
         a viewport that cannot count its own rows refuses to scroll:\n{screen}"
    );
}

/// `End` goes to the end of the line, shows it, and `Home` comes back.
///
/// Three separate faults meet on one keypress, and each shows up as a number
/// that is a constant rather than a position in the document:
///
/// * `End` lands on the reader's per-piece read cap — a budget, not the line's
///   end — so the cursor stops 100,000 bytes into a line that is megabytes
///   long.
/// * The row is built as the line's first *n* columns rather than as the
///   columns the pane is scrolled to, so once the view scrolls past that
///   prefix the row is drawn from text the build never produced: an empty row,
///   and no cell to put the caret in. The line and the cursor both vanish.
/// * `Home` then lands on the backward line-start search's floor — how far it
///   looked, not where the line starts — so it takes two presses to get back,
///   and the first one lands in the middle of the line.
///
/// The file has to be this big. The row a pane draws with wrap off is a
/// *window* into its line, and a file whose far end sits within a screenful of
/// its start exercises none of that — it draws either way. Past the large-file
/// threshold, with an end megabytes from the start, it is the window or
/// nothing.
///
/// Asserts on rendered output (CONTRIBUTING §2). The cursor's byte is read
/// from the editor rather than the status bar, because reaching the end of the
/// line necessarily walks the rest of it — and the piece tree records what that
/// walk crosses, so the file ends up line-indexed and the status bar switches
/// from a byte offset to a line and column. That is the honest report of what
/// the editor now knows; it just is not a fixed string to scrape.
#[test]
fn end_reaches_the_end_of_a_long_line_and_home_comes_back() {
    let dir = tempfile::tempdir().unwrap();
    // The module's own ~19 MB fixture, and it has to be that big.
    //
    // The row a pane draws is built as a *prefix* of the line — every column
    // from its start to the right-hand edge of the view — so how far right the
    // view can be scrolled and still have something to draw is bounded by how
    // much of one line the build will read. A file whose far end sits inside
    // that reach exercises none of this: it draws, and the test passes without
    // touching the case that fails. Past the reach the row is built short, the
    // view is scrolled beyond what it contains, and the pane goes blank.
    //
    // It also has to be past the large-file threshold, which is 10 MB by
    // default — below it a wrap-off row is chopped at `MAX_SAFE_LINE_WIDTH`
    // and the same blank appears from a different direction.
    let (path, _, bytes) = write_pair(dir.path());
    assert!(
        bytes > 10 * 1024 * 1024,
        "{bytes} is inside the build's reach, so this would pass without \
         testing anything"
    );

    let mut harness = opened(&path, false);
    assert_eq!(harness.cursor_position(), 0, "the file opens at its start");

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();

    assert_eq!(
        harness.cursor_position(),
        bytes,
        "End must reach the end of the line, not the reader's read cap:\n{screen}"
    );
    assert!(
        screen.contains(']'),
        "End scrolled the view to the end of the line but the row was built \
         from the line's start, so nothing is drawn:\n{screen}"
    );

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();

    assert_eq!(
        harness.cursor_position(),
        0,
        "one Home must return to the start of the line, not to how far the \
         search looked:\n{screen}"
    );
    assert!(
        screen.contains("[0,1,2"),
        "the start of the line should be back on screen:\n{screen}"
    );
}

#[test]
fn stepping_left_from_the_end_of_a_long_line_moves_the_caret_not_the_view() {
    let dir = tempfile::tempdir().unwrap();
    let (path, _, bytes) = write_pair(dir.path());

    let mut harness = opened(&path, false);
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        bytes,
        "End reaches the line's end"
    );

    let settled = harness.left_column();
    assert!(
        settled > 0,
        "End should have scrolled the view along the line"
    );

    // Ten steps, all of them well inside a 120-column pane: the caret has room
    // to walk left without the view needing to move at all.
    //
    // The view is clamped so it never scrolls into the empty space past a
    // line's end, and that clamp needs the line's length. Measuring the line by
    // reading it stops at the reader's cap, so the length came back as the
    // cursor's own column — which makes the clamp say the cursor *is* the last
    // visible column. Every Left then dragged the window left with it and the
    // caret stayed pinned to the right-hand edge, which is not stepping left,
    // it is scrolling.
    for step in 1..=10 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        assert_eq!(
            harness.cursor_position(),
            bytes - step,
            "Left must step the caret back one byte"
        );
        assert_eq!(
            harness.left_column(),
            settled,
            "the view must hold still while the caret has room to move in it \
             (step {step}):\n{}",
            harness.screen_to_string()
        );
    }
}

/// Typing costs the same wherever the caret sits on a line.
///
/// It did not. An edit at the far end of a 19 MB line read the line twice —
/// 38 MB per keystroke, 2.3 s in a debug build — while the identical edit at
/// byte 0 of the same file read a few hundred kilobytes. Two unrelated callers,
/// each asking for the text between the line's start and the caret:
///
/// * `line_start_and_blank_prefix` walked backwards to find the line start,
///   which on a file that is one line is the whole file.
/// * `collect_lsp_changes` converted the edit's byte offsets into LSP's UTF-16
///   positions, which needs the line prefix counted — and the send path then
///   threw the result away, because the buffer has no language server.
///
/// Stated as a ratio against the same edit near the line's start rather than as
/// an absolute figure: the claim is that the caret's column does not enter into
/// what a keystroke costs, and that holds whatever the fixture's size.
#[test]
fn an_edit_costs_the_same_at_either_end_of_a_long_line() {
    let dir = tempfile::tempdir().unwrap();
    let (single_path, _, file_bytes) = write_pair(dir.path());
    let mut harness = opened(&single_path, false);

    let at_start = bytes_read(&mut harness, |h| {
        h.send_key(KeyCode::Char('X'), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    });
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        file_bytes,
        "the caret should be at the line's far end"
    );

    let at_end = bytes_read(&mut harness, |h| {
        h.send_key(KeyCode::Char('X'), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    });

    eprintln!(
        "edit at byte 0: {at_start} bytes, at byte {file_bytes}: {at_end} bytes \
         (file is {file_bytes})"
    );
    let budget = (at_start * 8).max(4 * 1024 * 1024);
    assert!(
        at_end < budget,
        "typing at byte {file_bytes} of a {file_bytes}-byte line read {at_end} \
         bytes against {at_start} for the same keystroke at byte 0 (budget \
         {budget}) — the caret's column is back in the cost of a keystroke"
    );
}
