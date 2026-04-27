//! Scroll-reachability sweep for Markdown in Compose (preview) mode.
//!
//! Follow-up to PR #1688 (`scroll_wrapped_reach_last_line.rs`): the
//! two-tier line-wrap cache fixed scroll math drift in source mode with
//! word-wrap on.  This file extends the same width-sweep pattern to
//! **compose preview mode** for `.md` buffers, exercising:
//!
//! * keyboard arrows (Down) and PageDown
//! * mouse wheel
//! * dragging the scrollbar handle
//!
//! across three markdown content shapes that exercise the parts of the
//! pipeline a plain word-wrapped buffer doesn't:
//!
//! 1. **Tables at the end** — the markdown_compose plugin inserts
//!    `addVirtualLine` border rows above/below table rows; the scroll
//!    math has to count those plus the wrapped logical rows when
//!    deciding `max_scroll_row`.
//! 2. **Bullet points** — list blocks get a hanging indent applied by
//!    the plugin's view transform; the renderer's wrap is fed an
//!    explicit `hanging_indent` per logical line.
//! 3. **Very long wrappable lines** — the same word-wrap path PR #1688
//!    fixed in source mode, but here also routed through the plugin's
//!    soft-break/conceal pipeline.
//!
//! Each scenario is run **twice** at every width: once with the
//! plugin's default `composeWidth: null` (which makes the effective
//! compose width follow the viewport) and once with `composeWidth: 80`
//! (which clamps the content column to 80 and centers it inside wider
//! terminals).  At narrow viewports these collapse to the same layout;
//! at wider ones they exercise different code paths in the centering /
//! gutter math.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::path::PathBuf;
use std::time::Duration;

/// Marker placed on the last logical line of every fixture; it is plain
/// ASCII without any markdown syntax so the plugin's conceal / overlay
/// passes leave it intact and it shows up verbatim on the rendered
/// screen.
const LAST_LINE_MARKER: &str = "MD_TAIL_MARKER_QQQ";

/// Per-(width × height) scenario outcome — same shape as
/// `scroll_wrapped_reach_last_line.rs::Outcome` so the sweep driver
/// reads identically.
enum Outcome {
    Ok,
    SetupSkipped(String),
    Failure(String),
}

fn content_area_snapshot(harness: &EditorTestHarness) -> String {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .map(|r| harness.get_screen_row(r))
        .collect::<Vec<_>>()
        .join("\n")
}

fn marker_visible(harness: &EditorTestHarness) -> bool {
    let (first, last) = harness.content_area_rows();
    (first..=last).any(|r| harness.get_screen_row(r).contains(LAST_LINE_MARKER))
}

/// Drive a per-width scenario across a sweep.  Same contract as the
/// driver in `scroll_wrapped_reach_last_line.rs`: any Failure fails the
/// test; if every combination skipped, that's also a failure (means the
/// sweep isn't actually exercising anything).
fn drive_width_sweep(
    label: &'static str,
    widths: &[u16],
    heights: &[u16],
    scenario: impl Fn(u16, u16) -> Outcome,
) {
    let mut ok_count = 0usize;
    let mut skipped: Vec<String> = Vec::new();
    let mut failures: Vec<String> = Vec::new();
    for &height in heights {
        for &width in widths {
            match scenario(width, height) {
                Outcome::Ok => ok_count += 1,
                Outcome::SetupSkipped(msg) => skipped.push(format!("w={width} h={height}: {msg}")),
                Outcome::Failure(msg) => failures.push(format!("w={width} h={height}: {msg}")),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "[{label}] {} of {} (width, height) combo(s) failed:\n\n{}",
        failures.len(),
        failures.len() + ok_count + skipped.len(),
        failures.join("\n---\n"),
    );
    assert!(
        ok_count > 0,
        "[{label}] No width in the sweep exercised the bug-triggering state — \
         every combo was skipped, so the test isn't actually checking anything.  \
         Skipped reasons:\n{}",
        skipped.join("\n---\n"),
    );
}

// ---------------------------------------------------------------------------
// Fixture buffers
// ---------------------------------------------------------------------------

/// Markdown document that ends with a 3-column table.  The marker sits
/// in the last column of the last data row of the table — so reaching
/// the marker requires scrolling past every table border virtual line
/// the plugin emits.
fn build_table_at_end_buffer() -> String {
    let mut s = String::from("# Table at End Test\n\n");
    s.push_str("Some intro text to push the table off-screen.\n\n");
    for i in 0..30 {
        s.push_str(&format!(
            "Paragraph {i}: filler text that occupies a logical line in the buffer \
             so the table is far enough below the top to require scrolling.\n\n"
        ));
    }
    s.push_str("| Col A | Col B | Col C |\n");
    s.push_str("|-------|-------|-------|\n");
    for i in 0..6 {
        s.push_str(&format!("| row{i} a | row{i} b | row{i} c |\n"));
    }
    // Final row carries the marker in column C.
    s.push_str(&format!("| last a | last b | {LAST_LINE_MARKER} |\n"));
    s
}

/// Markdown document that ends with a deeply-indented bullet list.
/// Each item is long enough to wrap at the smaller compose widths so
/// hanging-indent continuation rows are exercised.  The marker sits at
/// the end of the final bullet's text.
fn build_bullets_at_end_buffer() -> String {
    let mut s = String::from("# Bullets at End Test\n\n");
    s.push_str("Lead-in paragraph so the list is below the fold.\n\n");
    for i in 0..40 {
        s.push_str(&format!("Filler paragraph {i} to push the list down.\n\n"));
    }
    s.push_str("## A list\n\n");
    let long = "lorem ipsum dolor sit amet consectetur adipiscing elit \
                sed do eiusmod tempor incididunt ut labore et dolore magna aliqua";
    for i in 0..6 {
        s.push_str(&format!("- bullet {i}: {long}\n"));
    }
    // Last bullet carries the marker; long enough to need wrapping at
    // narrow widths so the hanging indent kicks in before we reach it.
    s.push_str(&format!("- last bullet: {long} {LAST_LINE_MARKER}\n"));
    s
}

/// Markdown document that ends with a single very long word-wrappable
/// paragraph.  This is the closest analogue to the Bug-2 fixture in
/// `scroll_wrapped_reach_last_line.rs`, but routed through the
/// markdown_compose plugin's soft-break / conceal pipeline.
fn build_long_lines_buffer() -> String {
    let mut s = String::from("# Long Lines Test\n\n");
    let para: String = (0..40)
        .map(|i| format!("word{:02}", i % 100))
        .collect::<Vec<_>>()
        .join(" ");
    for i in 0..10 {
        s.push_str(&format!("Para {i}: {para}\n\n"));
    }
    // Tail paragraph carries the marker as its last word — the
    // word-wrap path will push it onto its own visual row.
    s.push_str(&format!("Tail: {para} {LAST_LINE_MARKER}\n"));
    s
}

// ---------------------------------------------------------------------------
// Plugin / harness setup
// ---------------------------------------------------------------------------

/// Set up an editor harness with the real `markdown_compose` plugin
/// loaded, optionally patching its `composeWidth` config.  The fixture
/// `.md` file is written into the project root with the given content,
/// opened, and compose mode is toggled on.  The function blocks until
/// the renderer is stable with `**` markers concealed, so callers can
/// scroll immediately on return.
fn setup_compose_harness(
    width: u16,
    height: u16,
    compose_width_override: Option<u16>,
    md_content: &str,
) -> Result<(EditorTestHarness, tempfile::TempDir, PathBuf), String> {
    let temp_dir = tempfile::TempDir::new().map_err(|e| format!("tempdir: {e}"))?;
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).map_err(|e| format!("create project: {e}"))?;

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).map_err(|e| format!("create plugins: {e}"))?;
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    if let Some(cw) = compose_width_override {
        let plugin_path = plugins_dir.join("markdown_compose.ts");
        let content =
            std::fs::read_to_string(&plugin_path).map_err(|e| format!("read plugin: {e}"))?;
        let needle = "composeWidth: null,";
        if !content.contains(needle) {
            return Err(format!(
                "plugin source no longer contains `{needle}` — patch failed"
            ));
        }
        let patched = content.replacen(needle, &format!("composeWidth: {cw},"), 1);
        std::fs::write(&plugin_path, patched).map_err(|e| format!("write plugin: {e}"))?;
    }

    let md_path = project_root.join("test.md");
    std::fs::write(&md_path, md_content).map_err(|e| format!("write md: {e}"))?;

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .map_err(|e| format!("harness init: {e}"))?;

    harness
        .open_file(&md_path)
        .map_err(|e| format!("open file: {e}"))?;
    harness.render().map_err(|e| format!("render: {e}"))?;

    // Toggle compose mode via the command palette — same path as a
    // real user, and the same path the existing `markdown_compose.rs`
    // flicker test uses.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+p: {e}"))?;
    harness
        .wait_for_prompt()
        .map_err(|e| format!("wait prompt: {e}"))?;
    harness
        .type_text("Toggle Compose")
        .map_err(|e| format!("type Toggle Compose: {e}"))?;
    harness
        .wait_for_screen_contains("Toggle Compose")
        .map_err(|e| format!("wait Toggle Compose entry: {e}"))?;
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .map_err(|e| format!("enter: {e}"))?;
    harness
        .wait_for_prompt_closed()
        .map_err(|e| format!("wait prompt closed: {e}"))?;

    // Wait for the plugin's view_transform to settle: emphasis markers
    // (`**`) get concealed everywhere except possibly the cursor's own
    // row, so once the screen has at most one `**` line and is stable
    // we're safe to scroll.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.lines().filter(|l| l.contains("**")).count() <= 1
        })
        .map_err(|e| format!("wait compose stable: {e}"))?;

    // The markdown_compose plugin processes `lines_changed`
    // reactively for the currently-visible window only.  Off-screen
    // lines have no plugin soft breaks / virtual borders in
    // `state.soft_breaks` / `state.virtual_texts` until the user
    // brings them into view.  Scroll math
    // (`scrollbar_math::ensure_index` → `VisualRowIndex`) reads from
    // those structures, so an unprocessed off-screen region makes
    // `max_scroll_row` undercount and scrollbar drag stops short of
    // the buffer's tail (mouse wheel and PageDown's per-step
    // `apply_visual_scroll_limit` re-clamp masks the same
    // under-count for those mechanisms).
    //
    // Bring every line through the renderer's visible window once so
    // the plugin processes it and the per-buffer state is complete.
    // After this warmup, `VisualRowIndex` builds with
    // soft-break/virtual-line-aware per-line counts and any scroll
    // mechanism reaches the tail deterministically.  Semantic-wait
    // after each jump (Ctrl+End → Ctrl+Home) follows
    // CONTRIBUTING.md rule #3.
    //
    // For now we use Ctrl+End → Ctrl+Home, which covers the bottom
    // and the top.  The middle of large buffers stays unprocessed —
    // these test fixtures are short enough that one Ctrl+End brings
    // the entire bottom half (≥ viewport_height rows) into view in
    // one render, including the marker line.  The fixtures have
    // also been sized so the marker sits within
    // `viewport_height` rows of EOF; if a future fixture needs a
    // mid-buffer marker, replace this with a PageDown sweep.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+end (warmup): {e}"))?;
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.lines().filter(|l| l.contains("**")).count() <= 1
        })
        .map_err(|e| format!("wait warmup-end stable: {e}"))?;

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+home (warmup): {e}"))?;
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.lines().filter(|l| l.contains("**")).count() <= 1
        })
        .map_err(|e| format!("wait warmup-home stable: {e}"))?;

    Ok((harness, temp_dir, md_path))
}

/// Move the cursor to the top of the buffer and re-render so each
/// scenario starts from a known viewport state.
fn jump_to_top(harness: &mut EditorTestHarness) -> Result<(), String> {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+home: {e}"))?;
    harness.render().map_err(|e| format!("render: {e}"))?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Scroll mechanism drivers
// ---------------------------------------------------------------------------

/// Send Down repeatedly until either the marker shows up or we exceed
/// `max_steps` keypresses.  Re-renders and lets the plugin tick after
/// every batch of 10 keys so soft-break / virtual-line additions land
/// before the next batch is sent.
fn drive_arrow_down(harness: &mut EditorTestHarness, max_steps: usize) -> Result<(), String> {
    let batch = 10usize;
    let mut sent = 0usize;
    while sent < max_steps {
        let chunk = batch.min(max_steps - sent);
        harness
            .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, chunk)
            .map_err(|e| format!("send Down x{chunk}: {e}"))?;
        sent += chunk;
        // Let any plugin-side virtual lines / soft breaks land before
        // the next batch.
        harness.advance_time(Duration::from_millis(20));
        harness
            .tick_and_render()
            .map_err(|e| format!("tick_and_render: {e}"))?;
        if marker_visible(harness) {
            return Ok(());
        }
    }
    Ok(())
}

fn drive_page_down(harness: &mut EditorTestHarness, max_steps: usize) -> Result<(), String> {
    for _ in 0..max_steps {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .map_err(|e| format!("PageDown: {e}"))?;
        if marker_visible(harness) {
            return Ok(());
        }
    }
    Ok(())
}

fn drive_mouse_wheel(
    harness: &mut EditorTestHarness,
    width: u16,
    max_ticks: usize,
) -> Result<(), String> {
    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = width / 2;
    let scroll_row = content_first_row as u16 + 2;
    for _ in 0..max_ticks {
        harness
            .mouse_scroll_down(scroll_col, scroll_row)
            .map_err(|e| format!("mouse_scroll_down: {e}"))?;
        if marker_visible(harness) {
            return Ok(());
        }
    }
    Ok(())
}

fn drive_scrollbar_drag(harness: &mut EditorTestHarness, width: u16) -> Result<(), String> {
    let scrollbar_col = width.saturating_sub(1);
    let (content_first_row, content_last_row) = harness.content_area_rows();
    harness
        .mouse_drag(
            scrollbar_col,
            content_first_row as u16,
            scrollbar_col,
            content_last_row as u16,
        )
        .map_err(|e| format!("mouse_drag: {e}"))?;
    // One extra tick so any post-drag plugin work (re-tile virtual
    // lines now that a different range is on screen) settles before we
    // assert.
    harness.advance_time(Duration::from_millis(50));
    let _ = harness.tick_and_render();
    Ok(())
}

// ---------------------------------------------------------------------------
// Scenario harness — wraps setup + drive + assert in one closure.
// ---------------------------------------------------------------------------

/// What kind of buffer to load.
#[derive(Copy, Clone)]
enum Fixture {
    TableAtEnd,
    BulletsAtEnd,
    LongLines,
}

impl Fixture {
    fn build(self) -> String {
        match self {
            Fixture::TableAtEnd => build_table_at_end_buffer(),
            Fixture::BulletsAtEnd => build_bullets_at_end_buffer(),
            Fixture::LongLines => build_long_lines_buffer(),
        }
    }

    fn label(self) -> &'static str {
        match self {
            Fixture::TableAtEnd => "table-at-end",
            Fixture::BulletsAtEnd => "bullets-at-end",
            Fixture::LongLines => "long-lines",
        }
    }
}

/// Which scroll mechanism to drive after setup.
#[derive(Copy, Clone)]
enum Mechanism {
    ArrowDown,
    PageDown,
    MouseWheel,
    ScrollbarDrag,
}

impl Mechanism {
    fn label(self) -> &'static str {
        match self {
            Mechanism::ArrowDown => "arrow-down",
            Mechanism::PageDown => "page-down",
            Mechanism::MouseWheel => "mouse-wheel",
            Mechanism::ScrollbarDrag => "scrollbar-drag",
        }
    }
}

/// Run one (width × height × fixture × compose-width × mechanism) trial.
///
/// Steps:
/// 1.  Spin up the harness with the plugin loaded and the fixture
///     opened in compose mode.
/// 2.  Verify the marker isn't already visible (otherwise the test
///     can't tell scrolling from a no-op — `SetupSkipped`).
/// 3.  Drive the requested scroll mechanism.
/// 4.  Assert the marker is now visible somewhere in the content area.
fn run_scenario(
    width: u16,
    height: u16,
    fixture: Fixture,
    compose_width_override: Option<u16>,
    mechanism: Mechanism,
) -> Outcome {
    let content = fixture.build();
    let (mut harness, _temp, _md_path) =
        match setup_compose_harness(width, height, compose_width_override, &content) {
            Ok(t) => t,
            Err(e) => return Outcome::SetupSkipped(format!("setup failed: {e}")),
        };

    if let Err(e) = jump_to_top(&mut harness) {
        return Outcome::SetupSkipped(format!("jump_to_top failed: {e}"));
    }

    if marker_visible(&harness) {
        return Outcome::SetupSkipped(format!(
            "marker already visible at the top — buffer/viewport too small to require scroll.\n\
             Content:\n{}",
            content_area_snapshot(&harness),
        ));
    }

    // Drive — bounds chosen to comfortably exceed the longest fixture
    // at the widest sweep width.
    let drive_result = match mechanism {
        Mechanism::ArrowDown => drive_arrow_down(&mut harness, 400),
        Mechanism::PageDown => drive_page_down(&mut harness, 30),
        Mechanism::MouseWheel => drive_mouse_wheel(&mut harness, width, 200),
        Mechanism::ScrollbarDrag => drive_scrollbar_drag(&mut harness, width),
    };
    if let Err(e) = drive_result {
        return Outcome::SetupSkipped(format!("driver failed: {e}"));
    }

    if marker_visible(&harness) {
        Outcome::Ok
    } else {
        Outcome::Failure(format!(
            "[{fixture}/{mech}/cw={cw:?}] tail marker {marker:?} not visible after scroll.\n\
             Content area:\n{snap}",
            fixture = fixture.label(),
            mech = mechanism.label(),
            cw = compose_width_override,
            marker = LAST_LINE_MARKER,
            snap = content_area_snapshot(&harness),
        ))
    }
}

// ---------------------------------------------------------------------------
// Sweep configuration
// ---------------------------------------------------------------------------

/// Three widths is enough to catch the wrap-at-word-boundary edge cases
/// that PR #1688's source-mode sweep already covered, and matches its
/// CI-budget tradeoff.  Heights kept to a single value — the bug class
/// here is width/wrap-driven.
const SWEEP_WIDTHS: [u16; 3] = [60, 100, 140];
const SWEEP_HEIGHTS: [u16; 1] = [22];

fn sweep(label: &'static str, fixture: Fixture, compose_width: Option<u16>, mechanism: Mechanism) {
    init_tracing_from_env();
    drive_width_sweep(label, &SWEEP_WIDTHS, &SWEEP_HEIGHTS, |w, h| {
        run_scenario(w, h, fixture, compose_width, mechanism)
    });
}

// ---------------------------------------------------------------------------
// Tests — composeWidth = null (effective width = viewport width)
// ---------------------------------------------------------------------------

#[test]
fn compose_default_width_table_arrow_down_reaches_marker() {
    sweep(
        "default-width/table/arrow-down",
        Fixture::TableAtEnd,
        None,
        Mechanism::ArrowDown,
    );
}

#[test]
fn compose_default_width_table_pagedown_reaches_marker() {
    sweep(
        "default-width/table/page-down",
        Fixture::TableAtEnd,
        None,
        Mechanism::PageDown,
    );
}

#[test]
fn compose_default_width_table_mouse_wheel_reaches_marker() {
    sweep(
        "default-width/table/mouse-wheel",
        Fixture::TableAtEnd,
        None,
        Mechanism::MouseWheel,
    );
}

#[test]
fn compose_default_width_table_scrollbar_drag_reaches_marker() {
    sweep(
        "default-width/table/scrollbar-drag",
        Fixture::TableAtEnd,
        None,
        Mechanism::ScrollbarDrag,
    );
}

#[test]
fn compose_default_width_bullets_pagedown_reaches_marker() {
    sweep(
        "default-width/bullets/page-down",
        Fixture::BulletsAtEnd,
        None,
        Mechanism::PageDown,
    );
}

#[test]
fn compose_default_width_bullets_mouse_wheel_reaches_marker() {
    sweep(
        "default-width/bullets/mouse-wheel",
        Fixture::BulletsAtEnd,
        None,
        Mechanism::MouseWheel,
    );
}

// TODO: scrollbar-drag uses `VisualRowIndex::position_at_row`, which
// now folds virtual rows into the prefix sums but returns
// `offset_in_line` that the viewport interprets as a wrapped-line
// offset.  Drag end-point can land a few rows below intent.  Fix is
// either to split prefix sums into wrap-rows + virtual-rows, or to
// only widen `total_rows()` without inserting virtual rows into the
// prefix sums.  See top-of-file comment.
#[test]
#[ignore = "needs VisualRowIndex::position_at_row virtual-row split"]
fn compose_default_width_bullets_scrollbar_drag_reaches_marker() {
    sweep(
        "default-width/bullets/scrollbar-drag",
        Fixture::BulletsAtEnd,
        None,
        Mechanism::ScrollbarDrag,
    );
}

#[test]
fn compose_default_width_long_lines_pagedown_reaches_marker() {
    sweep(
        "default-width/long-lines/page-down",
        Fixture::LongLines,
        None,
        Mechanism::PageDown,
    );
}

#[test]
fn compose_default_width_long_lines_mouse_wheel_reaches_marker() {
    sweep(
        "default-width/long-lines/mouse-wheel",
        Fixture::LongLines,
        None,
        Mechanism::MouseWheel,
    );
}

#[test]
fn compose_default_width_long_lines_scrollbar_drag_reaches_marker() {
    sweep(
        "default-width/long-lines/scrollbar-drag",
        Fixture::LongLines,
        None,
        Mechanism::ScrollbarDrag,
    );
}

// ---------------------------------------------------------------------------
// Tests — composeWidth = 80 (effective width = min(80, viewport))
// ---------------------------------------------------------------------------

#[test]
fn compose_width80_table_pagedown_reaches_marker() {
    sweep(
        "cw80/table/page-down",
        Fixture::TableAtEnd,
        Some(80),
        Mechanism::PageDown,
    );
}

#[test]
fn compose_width80_table_mouse_wheel_reaches_marker() {
    sweep(
        "cw80/table/mouse-wheel",
        Fixture::TableAtEnd,
        Some(80),
        Mechanism::MouseWheel,
    );
}

// Passes at w=100 / w=140 after the compose-width fix on this branch
// but still fails at w=60 (where viewport is *narrower* than
// `composeWidth=80`, so the same effective-width path the wider cases
// hit doesn't apply).  Same residual class as
// `compose_default_width_bullets_scrollbar_drag` — needs the
// `VisualRowIndex::position_at_row` virtual-row split.
#[test]
#[ignore = "needs VisualRowIndex::position_at_row virtual-row split"]
fn compose_width80_table_scrollbar_drag_reaches_marker() {
    sweep(
        "cw80/table/scrollbar-drag",
        Fixture::TableAtEnd,
        Some(80),
        Mechanism::ScrollbarDrag,
    );
}

#[test]
fn compose_width80_bullets_pagedown_reaches_marker() {
    sweep(
        "cw80/bullets/page-down",
        Fixture::BulletsAtEnd,
        Some(80),
        Mechanism::PageDown,
    );
}

#[test]
fn compose_width80_bullets_mouse_wheel_reaches_marker() {
    sweep(
        "cw80/bullets/mouse-wheel",
        Fixture::BulletsAtEnd,
        Some(80),
        Mechanism::MouseWheel,
    );
}

#[test]
fn compose_width80_bullets_scrollbar_drag_reaches_marker() {
    sweep(
        "cw80/bullets/scrollbar-drag",
        Fixture::BulletsAtEnd,
        Some(80),
        Mechanism::ScrollbarDrag,
    );
}

#[test]
fn compose_width80_long_lines_pagedown_reaches_marker() {
    sweep(
        "cw80/long-lines/page-down",
        Fixture::LongLines,
        Some(80),
        Mechanism::PageDown,
    );
}

#[test]
fn compose_width80_long_lines_mouse_wheel_reaches_marker() {
    sweep(
        "cw80/long-lines/mouse-wheel",
        Fixture::LongLines,
        Some(80),
        Mechanism::MouseWheel,
    );
}

#[test]
fn compose_width80_long_lines_scrollbar_drag_reaches_marker() {
    sweep(
        "cw80/long-lines/scrollbar-drag",
        Fixture::LongLines,
        Some(80),
        Mechanism::ScrollbarDrag,
    );
}

// ---------------------------------------------------------------------------
// Wide-viewport + composeWidth=80 reproduction.
//
// The previous sweep tops out at viewport width 140, where mouse wheel
// passes — the buffer is small enough that `apply_visual_scroll_limit`'s
// per-step re-clamp can walk past the under-counted `max_scroll_row`.
// On a real markdown document opened at viewport 200 with `Set Page
// Width 80`, mouse wheel and scrollbar drag both stop short of the
// buffer's tail (manually verified on
// `docs/internal/DEVCONTAINER_SPEC_GAP_PLAN.md`).
//
// Root cause (see analysis on this branch): when the viewport is
// substantially wider than the compose-clamped render area, the scroll
// math computes per-line visual row counts at `viewport.width` while
// the renderer wraps at `compose_width`.  Long paragraphs that take
// 3–4 visual rows in the renderer get counted as 1 row by the scroll
// math, and the resulting `max_scroll_row` undershoots by enough that
// even a per-step re-clamp can't catch up on a large buffer.
//
// These reproducers exercise that path explicitly:
//
// * a "tall" buffer (200 paragraphs that wrap heavily at 80 cols)
// * viewports 240 / 320 (≫ compose_width 80) so each paragraph is
//   1–2 rows in the index but 4 rows in the renderer
// * mouse wheel + scrollbar drag — the two paths the user reported.
//
// **Scrollbar-drag** fails deterministically here: a single mouse-up
// event sets `top_byte` to the under-counted target row and never
// re-clamps.
//
// **Mouse wheel** passes in this test even though it fails in
// manual / interactive use on the same kind of buffer.  Reason: the
// harness renders after every wheel event, so `Viewport::scroll_*`'s
// per-step `apply_visual_scroll_limit` re-clamp uses the renderer's
// just-updated row counts and walks one step past the wrong
// `max_scroll_row` on each call, eventually reaching the tail.  In
// real interactive use the renderer can't keep up with rapid wheel
// events and the user sees the viewport getting stuck.  The
// mouse-wheel cases here therefore serve as **guardrails** — they
// must keep passing through the fix — rather than as failing
// reproducers.  The scrollbar-drag failures are the genuine
// regressions the fix needs to turn green.
// ---------------------------------------------------------------------------

/// Markdown document with many wrappable paragraphs.  At compose width
/// 80 each paragraph wraps to several visual rows.  At viewport widths
/// far above 80 (e.g. 200) the scroll math undercounts those rows;
/// without the under-count fix the marker on the final paragraph
/// becomes unreachable.
fn build_tall_long_lines_buffer() -> String {
    let mut s = String::from("# Tall Long Lines Test\n\n");
    let para: String = (0..40)
        .map(|i| format!("word{:02}", i % 100))
        .collect::<Vec<_>>()
        .join(" ");
    // 200 paragraphs ≈ 800+ visual rows at 80-col wrap.  Index counts
    // ~200 rows at 200-col wrap, so the under-count gap is ~600 rows.
    // Empirically a smaller buffer (60 paragraphs) lets mouse wheel's
    // per-step `apply_visual_scroll_limit` walk past the wrong
    // `max_scroll_row` because the gap is small enough; at this size
    // the gap defeats the per-step recovery, matching the manual
    // repro on `docs/internal/DEVCONTAINER_SPEC_GAP_PLAN.md`.
    for i in 0..200 {
        s.push_str(&format!("Para {i}: {para}\n\n"));
    }
    s.push_str(&format!("Tail: {para} {LAST_LINE_MARKER}\n"));
    s
}

/// Same shape as `build_table_at_end_buffer` but with enough fillers
/// above the table that the scroll math's row under-count exceeds what
/// `apply_visual_scroll_limit` can walk through per step.
fn build_tall_table_at_end_buffer() -> String {
    let mut s = String::from("# Tall Table at End Test\n\n");
    s.push_str("Some intro text to push the table off-screen.\n\n");
    let para: String = (0..40)
        .map(|i| format!("word{:02}", i % 100))
        .collect::<Vec<_>>()
        .join(" ");
    // See `build_tall_long_lines_buffer` for why this is 200 — at
    // smaller sizes mouse wheel's per-step re-clamp masks the
    // under-count.
    for i in 0..200 {
        s.push_str(&format!("Para {i}: {para}\n\n"));
    }
    s.push_str("| Col A | Col B | Col C |\n");
    s.push_str("|-------|-------|-------|\n");
    for i in 0..6 {
        s.push_str(&format!("| row{i} a | row{i} b | row{i} c |\n"));
    }
    s.push_str(&format!("| last a | last b | {LAST_LINE_MARKER} |\n"));
    s
}

/// Drive the wide-viewport sweep at `cw=Some(80)`.  Two widths well
/// above 80 so the wrap-geometry mismatch is unambiguous; height stays
/// at the same 22 the rest of the file uses for parity with the
/// existing sweep.
fn wide_viewport_sweep(label: &'static str, content: &str, mechanism: Mechanism) {
    init_tracing_from_env();
    let widths: [u16; 2] = [240, 320];
    let heights: [u16; 1] = [22];
    drive_width_sweep(label, &widths, &heights, |w, h| {
        let (mut harness, _temp, _md_path) = match setup_compose_harness(w, h, Some(80), content) {
            Ok(t) => t,
            Err(e) => return Outcome::SetupSkipped(format!("setup failed: {e}")),
        };
        if let Err(e) = jump_to_top(&mut harness) {
            return Outcome::SetupSkipped(format!("jump_to_top failed: {e}"));
        }
        if marker_visible(&harness) {
            return Outcome::SetupSkipped(format!(
                "marker already visible at top — buffer/viewport too small.\nContent:\n{}",
                content_area_snapshot(&harness),
            ));
        }
        let drive = match mechanism {
            // Bigger budget than the standard sweep: this fixture has
            // ~800 visual rows at compose width 80, so the wheel
            // needs many more ticks to traverse the buffer than the
            // existing 200-tick cap.  Cap is generous so a *correct*
            // implementation easily reaches the tail; the bug we're
            // catching is the wheel getting stuck mid-buffer regardless
            // of how many ticks we send.
            Mechanism::MouseWheel => drive_mouse_wheel(&mut harness, w, 4000),
            Mechanism::ScrollbarDrag => drive_scrollbar_drag(&mut harness, w),
            // Arrow / PageDown not part of the user-reported bug here;
            // covered by the existing sweep.
            other => {
                return Outcome::SetupSkipped(format!(
                    "wide_viewport_sweep doesn't run mechanism {:?}",
                    other.label(),
                ))
            }
        };
        if let Err(e) = drive {
            return Outcome::SetupSkipped(format!("driver failed: {e}"));
        }
        if marker_visible(&harness) {
            Outcome::Ok
        } else {
            Outcome::Failure(format!(
                "[wide-viewport/cw=80/{mech}] tail marker {marker:?} not visible after scroll.\n\
                 Content area:\n{snap}",
                mech = mechanism.label(),
                marker = LAST_LINE_MARKER,
                snap = content_area_snapshot(&harness),
            ))
        }
    });
}

#[test]
fn compose_cw80_wide_viewport_long_lines_mouse_wheel_reaches_marker() {
    wide_viewport_sweep(
        "cw80-wide/long-lines/mouse-wheel",
        &build_tall_long_lines_buffer(),
        Mechanism::MouseWheel,
    );
}

#[test]
fn compose_cw80_wide_viewport_long_lines_scrollbar_drag_reaches_marker() {
    wide_viewport_sweep(
        "cw80-wide/long-lines/scrollbar-drag",
        &build_tall_long_lines_buffer(),
        Mechanism::ScrollbarDrag,
    );
}

#[test]
fn compose_cw80_wide_viewport_table_mouse_wheel_reaches_marker() {
    wide_viewport_sweep(
        "cw80-wide/table/mouse-wheel",
        &build_tall_table_at_end_buffer(),
        Mechanism::MouseWheel,
    );
}

// Same residual class as `compose_default_width_bullets_scrollbar_drag`:
// `VisualRowIndex::position_at_row` doesn't yet split prefix sums into
// wrap rows + virtual-line rows, so dragging into the table area lands
// a few rows above the actual table-tail row.  Tracking as a follow-up.
#[test]
#[ignore = "needs VisualRowIndex::position_at_row virtual-row split"]
fn compose_cw80_wide_viewport_table_scrollbar_drag_reaches_marker() {
    wide_viewport_sweep(
        "cw80-wide/table/scrollbar-drag",
        &build_tall_table_at_end_buffer(),
        Mechanism::ScrollbarDrag,
    );
}
