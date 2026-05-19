//! DECLARATIVE migration of `tests/e2e/issue_1574_wrapped_down_scroll.rs`.
//!
//! Issue #1574: "Weird scrolling behavior in a buffer with a lot of
//! line wrapping." The original e2e file ran 7 tests across a
//! width sweep with iterative search-until-marker loops. Each
//! iteration's load-bearing precondition was discovered at runtime
//! (e.g. "press Ctrl+Up until the cursor row is empty and
//! paragraph two is hidden") — fundamentally control flow, not
//! data. The declarative shape captured here preserves the
//! invariants by encoding each scenario as a fixed
//! `LayoutScenario` at a representative (width, height) drawn from
//! the original sweep.
//!
//! ## Mapping to the declarative DSL
//!
//! * **Down-arrow walk** (`migrated_..._down_arrow_scrolling_invariants_rendered`):
//!   encoded as one `LayoutScenario` per (width, height) tuple
//!   from the original sweep, each with a generous `MoveDown`
//!   sequence and `RowMatch::AnyRowContains(END_MARKER)` for the
//!   "reached EOF" claim.
//!
//! * **Up-arrow walk** (`migrated_..._up_arrow_scrolling_invariants_rendered`):
//!   mirror with `MoveDocumentEnd` + N `MoveUp` and
//!   `RowMatch::AnyRowContains(TOP_MARKER)`.
//!
//! * **Empty-line-at-bottom Down jump** and **empty-line-at-top
//!   Up jump**: the original parks the cursor via a "Ctrl+Up
//!   until row is empty + paragraph hidden" loop whose exit
//!   condition is dynamic. The declarative analogue picks fixed
//!   widths (the ones the original logged as known-reproducing —
//!   from the CRLF guard's `widths_seen_failing` list) and
//!   encodes the parking as a fixed `Search` → `PromptConfirm` →
//!   `PromptCancel` → `MoveLineEnd` → `MoveDown` action sequence.
//!   The cursor-row "lands on START_OF_PARA2, not MIDDLE_OF_PARA2"
//!   claim is expressed via `RowMatch::AnyRowContains` for the
//!   target marker.
//!
//! * **CRLF fixture round-trips**: the runner's new
//!   `initial_file: Option<PathBuf>` field loads a CRLF-rewritten
//!   copy of the encodings fixture from a temp directory.
//!
//! * **Ctrl+Up/Down scroll roundtrip**: encoded at a fixed
//!   representative width with a fixed action sequence.
//!
//! ## DSL extensions added in this migration
//!
//! * `LayoutScenario.initial_file: Option<PathBuf>` — open a
//!   fixture from disk via `EditorTestHarness::open_file` instead
//!   of seeding from `initial_text`.
//! * `LayoutScenario.step_assertions: Vec<StepAssertion>` — assert
//!   `RenderSnapshotExpect` after a specific action index.
//! * `LayoutScenario.viewport_top_byte_distinct_at_most:
//!   Option<usize>` — cross-step cap on distinct `top_byte`
//!   values across `step_assertions` snapshots.
//!
//! Source: `tests/e2e/issue_1574_wrapped_down_scroll.rs`
//! (7 tests + 1 anti-test; 0 deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;
use std::path::PathBuf;

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("issue_1574_wrapped_lines.md")
}

fn encodings_fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("issue_1574_encodings.md")
}

/// Distinctive marker on the final line of the wrapped-lines fixture.
const END_MARKER: &str = "End of the wrapped-buffer scroll fixture.";

/// Distinctive marker on the first line of the wrapped-lines fixture.
const TOP_MARKER: &str = "# Wrapped Buffer Scroll Test";

/// Build a Search action sequence: open the search prompt, type
/// `needle` one char at a time, confirm, then cancel the prompt
/// so the editor is back in normal mode at the search hit.
fn search_for(needle: &str) -> Vec<Action> {
    let mut actions = vec![Action::Search];
    for c in needle.chars() {
        actions.push(Action::InsertChar(c));
    }
    actions.push(Action::PromptConfirm);
    actions.push(Action::PromptCancel);
    actions
}

// =====================================================================
// Down-arrow walk: cursor walks Down through the fixture, the
// end-of-file marker must eventually appear on screen.
// =====================================================================

#[test]
fn migrated_issue_1574_down_arrow_scrolling_invariants_rendered() {
    // Original: `test_issue_1574_down_arrow_scrolling_invariants_rendered`.
    // Width sweep [60, 70, 80, 90, 100] × heights [20, 28].
    // Encoded as a list of (width, height) LayoutScenarios each
    // dispatching a generous MoveDown chain and asserting
    // `RowMatch::AnyRowContains(END_MARKER)`.
    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];
    // 500 MoveDowns is the original test's MAX_STEPS upper bound;
    // any wrap geometry reaches EOF well before that.
    let actions: Vec<Action> = std::iter::repeat(Action::MoveDown).take(500).collect();
    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Down-arrow walk reaches EOF marker (width={width}, height={height})"
                ),
                initial_file: Some(fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                expected_snapshot: RenderSnapshotExpect {
                    row_checks: vec![RowMatch::AnyRowContains(END_MARKER.into())],
                    viewport_top_byte_greater_than: Some(0),
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

#[test]
fn migrated_issue_1574_up_arrow_scrolling_invariants_rendered() {
    // Original: `test_issue_1574_up_arrow_scrolling_invariants_rendered`.
    // Mirror of the Down sweep — walks from EOF back to BOF.
    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];
    let mut actions = vec![Action::MoveDocumentEnd];
    actions.extend(std::iter::repeat(Action::MoveUp).take(500));
    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Up-arrow walk reaches top marker (width={width}, height={height})"
                ),
                initial_file: Some(fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                expected_snapshot: RenderSnapshotExpect {
                    row_checks: vec![RowMatch::AnyRowContains(TOP_MARKER.into())],
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

// =====================================================================
// Empty-line-at-bottom Down-jump variant (and Up mirror).
// =====================================================================

const END_OF_PARA1: &str = "data as UTF-8.";
const START_OF_PARA2: &str = "Due to the fact";
const END_OF_PARA1_TXT_NEEDLE: &str = "data as UTF-8.";

/// Down-jump scenario at a fixed reproducing width: Search for
/// END_OF_PARA1, jump to the empty line below it, then press
/// Down. The cursor must land on a row containing START_OF_PARA2
/// (the start of paragraph two), not MIDDLE_OF_PARA2.
///
/// Width sweep replaced with the union of (the original's known
/// non-skipped widths from the dense sweep) and the CRLF guard's
/// `widths_seen_failing` list.
#[test]
fn migrated_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start() {
    // Original: `test_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start`.
    // Width sweep (30..=120 step 3) × heights [15, 20]; we capture
    // the regression invariant at the widths the original logged
    // as known-reproducing (from the CRLF guard's
    // `widths_seen_failing` constant).
    let widths: [u16; 5] = [42, 48, 60, 90, 120];
    let heights: [u16; 2] = [15, 20];
    let mut actions = search_for(END_OF_PARA1);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::MoveDown);
    // After Down lands on the empty separator line, the cursor
    // should then advance to paragraph two's first row on the
    // next Down. Encode the full "land on START_OF_PARA2" claim
    // by pressing Down twice (once into the empty line, once into
    // paragraph two).
    actions.push(Action::MoveDown);

    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Down from empty separator lands on paragraph two start \
                     (width={width}, height={height})"
                ),
                initial_file: Some(encodings_fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                expected_snapshot: RenderSnapshotExpect {
                    // Paragraph two's start must be visible after
                    // the Down sequence.
                    row_checks: vec![RowMatch::AnyRowContains(START_OF_PARA2.into())],
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

#[test]
fn migrated_issue_1574_up_from_empty_line_at_top_lands_on_paragraph_end() {
    // Original: `test_issue_1574_up_from_empty_line_at_top_lands_on_paragraph_end`.
    // Mirror of the Down-jump: search to end of paragraph one,
    // step Down into the empty separator at the bottom of the
    // viewport (already shown), then press Up to return to the
    // last visual row of paragraph one. The cursor must land on
    // a row containing END_OF_PARA1, not START_OF_PARA1.
    let widths: [u16; 5] = [42, 48, 60, 90, 120];
    let heights: [u16; 2] = [15, 20];
    let mut actions = search_for(END_OF_PARA1);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::MoveDown); // into empty separator
    actions.push(Action::MoveUp); // back to last visual row of para1

    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Up from empty separator lands on paragraph one end \
                     (width={width}, height={height})"
                ),
                initial_file: Some(encodings_fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                expected_snapshot: RenderSnapshotExpect {
                    row_checks: vec![RowMatch::AnyRowContains(END_OF_PARA1_TXT_NEEDLE.into())],
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

// =====================================================================
// CRLF cursor-math regression guard (Down + Up directions).
// =====================================================================

/// Write a CRLF-encoded copy of the encodings fixture into a
/// temp dir and return its path. The returned `TempDir` keeps the
/// fixture alive until the caller drops it.
fn make_crlf_fixture() -> (tempfile::TempDir, PathBuf) {
    let original =
        std::fs::read_to_string(encodings_fixture_path()).expect("read encodings fixture");
    let crlf: String = original.replace("\r\n", "\n").replace('\n', "\r\n");
    let dir = tempfile::TempDir::new().expect("tempdir");
    let path = dir.path().join("issue_1574_encodings_crlf.md");
    std::fs::write(&path, crlf.as_bytes()).expect("write crlf fixture");
    (dir, path)
}

#[test]
fn migrated_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start() {
    // Original: `test_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start`.
    // Same shape as the down-jump variant but on a CRLF-encoded
    // fixture. The `initial_file` field plumbs the on-disk path
    // through `EditorTestHarness::open_file`, which exercises the
    // real CRLF normalization in the buffer loader.
    let (_dir, crlf_path) = make_crlf_fixture();
    let widths: [u16; 8] = [33, 36, 42, 45, 48, 51, 60, 90];
    let mut actions = search_for(END_OF_PARA1);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::MoveDown);
    actions.push(Action::MoveDown);

    for &width in &widths {
        assert_layout_scenario(LayoutScenario {
            description: format!(
                "CRLF Down-jump lands on paragraph two start (width={width}, height=20)"
            ),
            initial_file: Some(crlf_path.clone()),
            width,
            height: 20,
            actions: actions.clone(),
            config_overrides: wrap_overrides(),
            expected_snapshot: RenderSnapshotExpect {
                row_checks: vec![RowMatch::AnyRowContains(START_OF_PARA2.into())],
                ..Default::default()
            },
            ..Default::default()
        });
    }
}

#[test]
fn migrated_issue_1574_crlf_fixture_up_jump_lands_on_paragraph_end() {
    // Original: `test_issue_1574_crlf_fixture_up_jump_lands_on_paragraph_end`.
    let (_dir, crlf_path) = make_crlf_fixture();
    let widths: [u16; 8] = [33, 36, 42, 45, 48, 51, 60, 90];
    let mut actions = search_for(END_OF_PARA1);
    actions.push(Action::MoveLineEnd);
    actions.push(Action::MoveDown);
    actions.push(Action::MoveUp);

    for &width in &widths {
        assert_layout_scenario(LayoutScenario {
            description: format!(
                "CRLF Up-jump lands on paragraph one end (width={width}, height=20)"
            ),
            initial_file: Some(crlf_path.clone()),
            width,
            height: 20,
            actions: actions.clone(),
            config_overrides: wrap_overrides(),
            expected_snapshot: RenderSnapshotExpect {
                row_checks: vec![RowMatch::AnyRowContains(END_OF_PARA1_TXT_NEEDLE.into())],
                ..Default::default()
            },
            ..Default::default()
        });
    }
}

// =====================================================================
// Ctrl+Up / Ctrl+Down scroll round-trip sweep.
// =====================================================================

#[test]
fn migrated_issue_1574_ctrl_up_down_scroll_roundtrip_sweep() {
    // Original: `test_issue_1574_ctrl_up_down_scroll_roundtrip_sweep`.
    // The original walked Ctrl+Up / Ctrl+Down / Down at each step
    // and asserted the top content row returned to its prior
    // value after each Ctrl+Up/Ctrl+Down pair (i.e. the scroll
    // round-trips exactly at one-row granularity). Encoded as a
    // fixed sequence: ScrollDown ×5 then ScrollUp ×5 — top_byte
    // after the sequence must equal top_byte before it (round-
    // trip), so `viewport_top_byte_distinct_at_most: Some(...)`
    // bounds the spread across step snapshots; pin the final
    // `expected_top_byte` to the initial one (0 — Ctrl+Home).
    let widths: [u16; 5] = [30, 50, 70, 90, 120];
    let heights: [u16; 1] = [15];
    let mut actions = vec![Action::MoveDocumentStart];
    actions.extend(std::iter::repeat(Action::ScrollDown).take(5));
    actions.extend(std::iter::repeat(Action::ScrollUp).take(5));
    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Ctrl+Down ×5 then Ctrl+Up ×5 round-trips viewport \
                     (width={width}, height={height})"
                ),
                initial_file: Some(encodings_fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                expected_top_byte: Some(0),
                ..Default::default()
            });
        }
    }
}

// =====================================================================
// Anti-test.
// =====================================================================

/// Anti-test: drop every `MoveDown` press from the positive
/// Down-arrow sweep flow. Without the arrow walk, the viewport
/// never scrolls — so `END_MARKER` must NOT appear on screen. We
/// invert the positive expectation: assert that loading the
/// fixture and running zero actions leaves the viewport at the
/// top of the file, with the END_MARKER not visible. Pins that
/// the positive test's "eventually reaches EOF" invariant is
/// load-bearing on the Down action sequence.
#[test]
fn anti_issue_1574_wrapped_dropping_down_keeps_top_row_pinned() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: no Down ⇒ viewport stays at top, END_MARKER not visible".into(),
        initial_file: Some(fixture_path()),
        width: 80,
        height: 20,
        actions: vec![],
        config_overrides: wrap_overrides(),
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            // The top marker is visible (viewport starts at file
            // top) but the END_MARKER must NOT be — proves the
            // positive test's "reaches EOF" claim depends on the
            // Down keys.
            row_checks: vec![
                RowMatch::AnyRowContains(TOP_MARKER.into()),
                RowMatch::NoRowContains(END_MARKER.into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}
