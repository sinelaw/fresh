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

/// The headline case: a fenced block renders as a box labelled with its
/// language, and the ``` delimiters are gone.
#[test]
fn fenced_block_renders_as_a_labelled_frame() {
    let (mut harness, _tmp) =
        compose_harness("# Doc\n\nIntro.\n\n```rust\nfn answer() -> u32 { 42 }\n```\n\nTail.\n");

    harness
        .wait_until(|h| h.screen_to_string().contains('┌'))
        .expect("compose mode should frame the fenced block");
    park_cursor_at_top(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("─ rust ─"),
        "the frame should carry the fence's info string.\nScreen:\n{screen}"
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
        screen.contains("─ js ─"),
        "the surviving frame is the js block's.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("─ sh ─"),
        "the sh fence is inside the js block now, so it is not a frame.\n\
         Screen:\n{screen}"
    );
}
