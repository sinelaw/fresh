//! End-to-end: compose mode frames fenced code blocks the way it frames
//! tables — the ``` delimiters concealed into a `┌─ lang ─┐` / `└────┘` box,
//! the body left to the embedded-language highlighter.
//!
//! The property worth testing is not that a frame appears. It is that the
//! frame is decided from the *editor's* region classification rather than from
//! the lines a `lines_changed` batch happens to contain, so:
//!
//!   * a closing fence scrolled into view pages below its opener still closes
//!     the box (the batch-local failure — a frame that comes and goes with
//!     scroll position);
//!   * a fence with no info string is framed correctly at both ends, which no
//!     rule reading a line's own text can do (a bare ``` opens or closes
//!     depending on every fence above it);
//!   * an edit that silently changes which lines are inside a block re-frames
//!     the lines it did not touch.
//!
//! All assertions are on rendered output only.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// Open a markdown document with the real `markdown_compose` plugin loaded and
/// compose mode enabled through the command palette.
///
/// Unlike the other compose suites this one needs the **full grammar
/// registry**: the frame is drawn from the region classification the Markdown
/// grammar produces, so with the default empty registry there is no Markdown
/// syntax, no embedded-region host, and — correctly — no framing at all.
fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("code.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
    )
    .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    (harness, temp_dir)
}

/// Rows carrying a code-frame top / bottom border. Table frames are excluded:
/// their corner rows always carry a `┬`/`┴` column junction, a code frame never
/// does.
fn frame_rows(screen: &str, corner: char) -> Vec<usize> {
    screen
        .lines()
        .enumerate()
        .filter(|(_, l)| l.contains(corner) && !l.contains('┬') && !l.contains('┴'))
        .map(|(i, _)| i)
        .collect()
}

fn tops(screen: &str) -> Vec<usize> {
    frame_rows(screen, '┌')
}

fn bottoms(screen: &str) -> Vec<usize> {
    frame_rows(screen, '└')
}

/// Park the cursor somewhere with no markup, so nothing on screen is being
/// revealed for editing. Compose deliberately reveals the markup on the
/// cursor's own line (and the line above it when the cursor is at column 0),
/// which would otherwise read as a missing frame.
fn park_cursor_at_top(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
}

/// The headline case: a fenced block renders as a box, and the ``` delimiters
/// are gone. The border carries no language tag — the code inside is already
/// highlighted with that language's grammar, so the tag said nothing the block
/// did not.
#[test]
fn fenced_block_renders_as_a_frame() {
    let (mut harness, _tmp) =
        compose_harness("# Doc\n\nIntro.\n\n```rust\nfn answer() -> u32 { 42 }\n```\n\nTail.\n");

    harness
        .wait_until(|h| h.screen_to_string().contains('┌'))
        .expect("compose mode should frame the fenced block");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    let top_row = screen
        .lines()
        .find(|l| l.contains('┌'))
        .expect("top border on screen");
    assert!(
        !top_row.chars().any(char::is_alphanumeric),
        "the border carries no text at all — the fence's info string is not \
         repeated in it; top row was {top_row:?}"
    );
    assert_eq!(tops(&screen).len(), 1, "one top border.\nScreen:\n{screen}");
    assert_eq!(
        bottoms(&screen).len(),
        1,
        "one bottom border.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("```"),
        "the delimiters are concealed by the frame.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("fn answer() -> u32 { 42 }"),
        "the body is left untouched so its highlighting shows through.\n\
         Screen:\n{screen}"
    );
}

/// The box is closed on all four sides: the body line carries a `│` rail at
/// each edge, in the same columns as the corners of the borders above and
/// below it. Getting the width right is the whole difficulty — a rail computed
/// against a different budget than its border is off on every line of the
/// block, and one computed against the full measure wraps onto its own row.
#[test]
fn body_rails_line_up_with_the_border_corners() {
    let (mut harness, _tmp) =
        compose_harness("# Doc\n\n```rust\nfn answer() -> u32 { 42 }\n```\n\nTail.\n");

    harness
        .wait_until(|h| h.screen_to_string().contains('└'))
        .expect("compose mode should frame the fenced block");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    let edges = |needle: char| -> Option<(usize, usize)> {
        let line = screen.lines().find(|l| l.contains(needle))?;
        let cols: Vec<usize> = line
            .chars()
            .enumerate()
            .filter(|(_, c)| "┌┐└┘│─".contains(*c))
            .map(|(i, _)| i)
            .collect();
        Some((*cols.first()?, *cols.last()?))
    };

    let top = edges('┌').expect("top border on screen");
    let bottom = edges('└').expect("bottom border on screen");
    let body = screen
        .lines()
        .find(|l| l.contains("fn answer"))
        .expect("body line on screen");
    let rails: Vec<usize> = body
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '│')
        .map(|(i, _)| i)
        .collect();

    assert_eq!(
        rails.len(),
        2,
        "the body line should carry exactly two rails, one per edge; body \
         line was {body:?}"
    );
    assert_eq!(
        (rails[0], rails[1]),
        top,
        "the rails must sit in the same columns as the top border's corners.\n\
         Screen:\n{screen}"
    );
    assert_eq!(
        top, bottom,
        "top and bottom borders must span the same columns.\nScreen:\n{screen}"
    );
}

/// Columns of the first and last box-drawing glyph on the row containing
/// `needle`, or on the row that *is* `needle` when it names a border.
fn frame_edges(screen: &str, row: &str) -> Option<(usize, usize)> {
    let line = screen.lines().find(|l| l.contains(row))?;
    let cols: Vec<usize> = line
        .chars()
        .enumerate()
        .filter(|(_, c)| "┌┐└┘│─".contains(*c))
        .map(|(i, _)| i)
        .collect();
    Some((*cols.first()?, *cols.last()?))
}

/// Every row of a block sits in the same columns — including the two the frame
/// cannot draw the ordinary way.
///
/// A *blank* line has no character to anchor a rail to, so its rails hang off
/// the newline cell; that is the one cell the renderer used to pad on both
/// sides, which left blank rows visibly pinched one column inside their own
/// frame. A line too wide for the frame is broken here rather than left to the
/// renderer, which would fold it to column zero — outside the frame, with no
/// rails on the continuation.
#[test]
fn blank_and_wrapped_rows_keep_the_frame_columns() {
    let long = "    let v = compute(alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota);";
    let md = format!("# Doc\n\n```rust\nfn f() {{\n\n{long}\n}}\n```\n\nTail.\n");
    let (mut harness, _tmp) = compose_harness(&md);

    harness
        .wait_until(|h| h.screen_to_string().contains('└'))
        .expect("the block should frame");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    let top = frame_edges(&screen, "┌").expect("top border on screen");

    // The wrapped line's tail lands on a row of its own; both of its rows, and
    // the blank row, must match the border's columns.
    assert!(
        screen.contains("iota"),
        "the whole long line should still be on screen.\nScreen:\n{screen}"
    );
    let rows: Vec<&str> = screen
        .lines()
        .skip_while(|l| !l.contains('┌'))
        .take_while(|l| !l.contains('└'))
        .filter(|l| l.contains('│'))
        .collect();
    assert!(
        rows.len() >= 5,
        "expected the brace, blank, both rows of the wrapped line and the \
         closing brace; saw {rows:?}"
    );
    for row in &rows {
        let cols: Vec<usize> = row
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '│')
            .map(|(i, _)| i)
            .collect();
        assert_eq!(
            (cols.first().copied(), cols.last().copied()),
            (Some(top.0), Some(top.1)),
            "every body row must be railed in the border's own columns; \
             row {row:?} was not.\nScreen:\n{screen}"
        );
    }
}

/// A fence with no info string is framed at both ends.
///
/// This is the case a plugin cannot decide for itself: a bare ``` is an opener
/// or a closer depending on every fence above it, and a `lines_changed` batch
/// carries no such context. Guessing from the line's own text inverts the box
/// on exactly this document.
#[test]
fn bare_fence_is_framed_at_both_ends() {
    let (mut harness, _tmp) = compose_harness("# Doc\n\n```\nplain preformatted\n```\n\nTail.\n");

    harness
        .wait_until(|h| h.screen_to_string().contains('└'))
        .expect("a bare fence should still close its frame");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    let (top, bottom) = (tops(&screen), bottoms(&screen));
    assert_eq!(top.len(), 1, "one top border.\nScreen:\n{screen}");
    assert_eq!(bottom.len(), 1, "one bottom border.\nScreen:\n{screen}");
    assert!(
        top[0] < bottom[0],
        "the box must open above and close below its content, not the other \
         way round — top at row {}, bottom at row {}.\nScreen:\n{screen}",
        top[0],
        bottom[0],
    );
}

/// The case a batch-local implementation gets wrong, and the reason the
/// classification comes from the editor: scrolled deep into a block taller than
/// the viewport, the closing fence still closes the box even though its opening
/// fence has been off screen — and out of every `lines_changed` batch — for
/// pages.
#[test]
fn closing_fence_frames_after_scrolling_past_the_opener() {
    let mut md = String::from("# Doc\n\nIntro.\n\n```rust\n");
    for i in 0..120 {
        md.push_str(&format!("    let v{i} = compute({i});\n"));
    }
    md.push_str("```\n\nTail paragraph.\n");

    let (mut harness, _tmp) = compose_harness(&md);
    harness
        .wait_until(|h| h.screen_to_string().contains('┌'))
        .expect("the opening fence should frame");

    // Scroll until the tail paragraph — and therefore the closing fence just
    // above it — is on screen.
    for _ in 0..12 {
        if harness.screen_to_string().contains("Tail paragraph.") {
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.wait_for_async_quiescence(4).unwrap();
    }
    harness
        .wait_until(|h| h.screen_to_string().contains("Tail paragraph."))
        .expect("should have scrolled to the end of the block");
    harness.wait_for_async_quiescence(6).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("```"),
        "the closing delimiter must still be concealed into a border after \
         scrolling past its opener — a batch-local rule leaves it literal \
         here.\nScreen:\n{screen}"
    );
    assert_eq!(
        bottoms(&screen).len(),
        1,
        "exactly one bottom border, below the block.\nScreen:\n{screen}"
    );
    assert!(
        tops(&screen).is_empty(),
        "the opening fence is off screen, so no top border is drawn.\n\
         Screen:\n{screen}"
    );
}

/// A run with no space in it — a bare URL, the shape every install snippet
/// has — is cut mid-word rather than left to overflow.
///
/// This is the case a break-only-at-spaces rule cannot serve: there is no
/// space to break at, so the row ran past the frame, lost its right rail, and
/// was folded by the renderer to column zero with no rails at all on the
/// remainder.
#[test]
fn an_unbreakable_run_is_cut_to_fit_the_frame() {
    let url = "https://example.com/an/extremely/long/unbreakable/path/with/no/spaces/at/all/that/cannot/be/wrapped/anywhere";
    let md = format!("# Doc\n\n```text\n{url}\n```\n\nTail.\n");
    let (mut harness, _tmp) = compose_harness(&md);

    harness
        .wait_until(|h| h.screen_to_string().contains('└'))
        .expect("the block should frame");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    let top = frame_edges(&screen, "┌").expect("top border on screen");
    let rows: Vec<&str> = screen
        .lines()
        .skip_while(|l| !l.contains('┌'))
        .take_while(|l| !l.contains('└'))
        .filter(|l| l.contains('│'))
        .collect();

    assert!(
        rows.len() >= 2,
        "the url is wider than the frame, so it must occupy several rows; \
         saw {rows:?}"
    );
    for row in &rows {
        let cols: Vec<usize> = row
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '│')
            .map(|(i, _)| i)
            .collect();
        assert_eq!(
            (cols.first().copied(), cols.last().copied()),
            (Some(top.0), Some(top.1)),
            "every row of the cut url must be railed in the border's own \
             columns; row {row:?} was not.\nScreen:\n{screen}"
        );
    }

    // And no character of it was dropped in the cutting.
    let joined: String = rows
        .iter()
        .flat_map(|r| r.chars().filter(|c| !c.is_whitespace() && *c != '│'))
        .collect();
    assert_eq!(
        joined, url,
        "the url must be split across rows without losing or duplicating \
         anything.\nScreen:\n{screen}"
    );
}

/// Markdown syntax inside a code block is code, not markdown. Before the region
/// classification was available the per-line pass had no way to know, so a `#`
/// comment became a heading, a `-` became a bullet, and a line of pipes was
/// drawn as a table — inside a shell block.
#[test]
fn markdown_syntax_inside_a_block_is_left_alone() {
    let (mut harness, _tmp) = compose_harness(
        "# Doc\n\n```sh\n# not a heading\n- not a bullet\n| not | a table |\n\
         *not emphasis*\n```\n\nTail.\n",
    );

    harness
        .wait_until(|h| h.screen_to_string().contains('└'))
        .expect("the shell block should frame");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    for literal in [
        "# not a heading",
        "- not a bullet",
        "| not | a table |",
        "*not emphasis*",
    ] {
        assert!(
            screen.contains(literal),
            "{literal:?} is code and must render verbatim.\nScreen:\n{screen}"
        );
    }
    assert!(
        !screen.contains('┼'),
        "no table frame may be drawn from pipes inside a code block.\n\
         Screen:\n{screen}"
    );
    assert!(
        !screen.contains('•'),
        "no bullet glyph inside a code block.\nScreen:\n{screen}"
    );
}

/// An intra-line edit to a fence delimiter changes which lines are inside the
/// block *below* the edit — lines the edit never touched and which the editor
/// therefore does not re-offer on their own. Appending text to a closing fence
/// stops it closing (CommonMark forbids an info string on a closer), so
/// everything after it becomes block body and must lose its framing.
///
/// Without the plugin's post-edit re-fire those lines keep the frame they were
/// last rendered with, disagreeing with the highlighting, which does converge.
#[test]
fn breaking_a_closing_fence_reframes_the_lines_below_it() {
    let (mut harness, _tmp) =
        compose_harness("# Doc\n\n```js\nconst a = 1;\n```\n\n```sh\necho hi\n```\n\nTail.\n");

    harness
        .wait_until(|h| tops(&h.screen_to_string()).len() == 2)
        .expect("both blocks should frame");

    // Land on the closing fence of the first block (line index 4) and append a
    // word, so it is no longer a valid closer. No line count change, so nothing
    // forces a whole-viewport refresh on the editor's side.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" x").unwrap();
    park_cursor_at_top(&mut harness);

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            tops(&s).len() == 1 && bottoms(&s).len() == 1
        })
        .expect(
            "with the first block never closed, the second block's delimiters \
             are body lines: exactly one frame should remain, opened by the js \
             fence and closed by the file's last fence",
        );

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("```sh"),
        "the sh fence is inside the js block now, so it renders literally as \
         code rather than opening a frame of its own.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("``` x"),
        "and so does the delimiter that stopped closing.\nScreen:\n{screen}"
    );
}

/// A prose line of exactly `bytes` bytes, newline included.
///
/// The exact sizing is the point: the highlighter drops a parse checkpoint
/// every `CHECKPOINT_INTERVAL` (256) bytes, at the first line start past that
/// many bytes, and the test below needs one to land on a fence delimiter.
fn sized_line(prefix: &str, bytes: usize) -> String {
    let mut line = prefix.to_string();
    while line.len() < bytes - 1 {
        line.push_str(" filler");
    }
    line.truncate(bytes - 1);
    line.push('\n');
    line
}

/// Opening a file part-way down and scrolling up must not frame prose as code.
///
/// A viewport that starts mid-document reads the region structure by resuming
/// from the nearest parse checkpoint above it rather than from the top of the
/// file. A checkpoint stores the state *before* its own position; one that
/// landed on a fence delimiter used to store the state from *after* that line,
/// so resuming there re-parsed a closing ``` as an opening one and every prose
/// line below the block reported as fence body — framed, rails and all, with
/// its inline markup left literal. It cleared only once the view scrolled above
/// the poisoned checkpoint, which is exactly the "scroll up far enough and it
/// fixes itself" shape of the report.
#[test]
fn prose_below_a_block_is_not_framed_when_the_view_starts_mid_document() {
    // 256 bytes of prose, then a 256-byte block: the second checkpoint lands
    // on the block's closing delimiter.
    let mut md = String::new();
    for i in 0..4 {
        md.push_str(&sized_line(&format!("Intro line {i}"), 64));
    }
    assert_eq!(md.len(), 256, "preamble must fill exactly one interval");
    md.push_str("```bash\n");
    for _ in 0..8 {
        md.push_str(&format!("echo {}\n", "x".repeat(25)));
    }
    assert_eq!(
        md.len(),
        512,
        "closing fence must start on the next interval"
    );
    md.push_str("```\n\n");
    for i in 0..40 {
        md.push_str(&sized_line(&format!("Prose {i} with a `code` span"), 64));
        md.push('\n');
    }
    // Markup-free last line, so parking the cursor at the end of the buffer
    // never reveals the raw markup of a line the assertions read.
    md.push_str("TAIL MARKER\n");

    let (mut harness, _tmp) = compose_harness(&md);
    harness
        .wait_until(|h| h.screen_to_string().contains('┌'))
        .expect("the block should frame");

    // Jump to the end: the prose below the block is now first drawn — and so
    // first classified — with the viewport well past that checkpoint, the way
    // it is when a file is opened at a line in the middle.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(6).unwrap();

    // Walk back up a notch at a time, checking every intermediate view. The
    // document's first line coming into view is the signal that the sweep has
    // covered every position the viewport can start from.
    let mut reached_top = false;
    for _ in 0..120 {
        let screen = harness.screen_to_string();
        for row in screen.lines().filter(|l| l.contains("Prose ")) {
            assert!(
                !row.contains('│') && !row.contains('┌') && !row.contains('└'),
                "prose below the code block must not be framed as code; row \
                 was {row:?}.\nScreen:\n{screen}"
            );
            assert!(
                !row.contains('`'),
                "prose below the code block keeps its inline markup concealed \
                 — literal backticks mean it was read as code; row was \
                 {row:?}.\nScreen:\n{screen}"
            );
        }
        reached_top = screen.contains("Intro line 0");
        if reached_top {
            break;
        }
        harness.mouse_scroll_up(10, 10).unwrap();
        harness.wait_for_async_quiescence(4).unwrap();
    }

    assert!(
        reached_top,
        "the sweep must reach the top of the document, so the window where the \
         stale checkpoint was the nearest anchor is covered"
    );
    let screen = harness.screen_to_string();
    assert_eq!(
        tops(&screen).len(),
        1,
        "back at the top, the one real block still frames.\nScreen:\n{screen}"
    );
}
