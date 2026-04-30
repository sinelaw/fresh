//! Class A theorems: pure buffer + caret state.
//!
//! Use this for tests where the only observable that matters is buffer
//! text and cursor positions / selections — case conversion, indent,
//! sort lines, smart-home (text-only), text mutations, etc.
//!
//! Tests using this module **must not** import anything else from
//! `fresh::*` besides `fresh::test_api`. The
//! `scripts/check-semantic-test-isolation.sh` lint enforces it.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::{Action, Caret};

/// Expected state of one cursor.
///
/// Wrapper around the test-API `Caret` so test code can spell
/// expectations as `CursorExpect::at(5)` and `CursorExpect::range(0, 5)`
/// instead of building a `Caret` with `Some` / `None`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CursorExpect {
    pub position: usize,
    pub anchor: Option<usize>,
}

impl CursorExpect {
    pub fn at(position: usize) -> Self {
        Self {
            position,
            anchor: None,
        }
    }
    pub fn range(anchor: usize, position: usize) -> Self {
        Self {
            position,
            anchor: Some(anchor),
        }
    }
}

impl PartialEq<Caret> for CursorExpect {
    fn eq(&self, other: &Caret) -> bool {
        self.position == other.position && self.anchor == other.anchor
    }
}

/// A declarative buffer-state test.
///
/// `initial_text` seeds the buffer; the cursor starts at byte 0
/// (where `load_buffer_from_text` leaves it). To begin elsewhere,
/// prepend a navigation action like `Action::MoveDocumentEnd`.
pub struct BufferTheorem {
    /// Human-readable claim — appears in assertion failures.
    pub description: &'static str,
    /// Buffer contents at t=0. Cursor is at byte 0.
    pub initial_text: &'static str,
    /// Action sequence applied left-to-right.
    pub actions: Vec<Action>,
    /// Buffer text at t=∞.
    pub expected_text: &'static str,
    /// Primary cursor at t=∞.
    pub expected_primary: CursorExpect,
    /// Secondary cursors at t=∞, in ascending byte-position order.
    /// The runner always sees the primary first; this list covers the
    /// remaining cursors. Empty for single-cursor theorems.
    pub expected_extra_cursors: Vec<CursorExpect>,
    /// Optional: text reachable through the union of selections at t=∞.
    /// `None` skips the assertion. `Some("")` asserts no selection.
    pub expected_selection_text: Option<&'static str>,
}

impl Default for BufferTheorem {
    fn default() -> Self {
        Self {
            description: "<unnamed theorem>",
            initial_text: "",
            actions: Vec::new(),
            expected_text: "",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: Vec::new(),
            expected_selection_text: None,
        }
    }
}

/// Apply `action` `n` times. Useful for lifting `for _ in 0..n
/// { send_key(...) }` into a single declarative repetition.
pub fn repeat(action: Action, n: usize) -> impl Iterator<Item = Action> {
    std::iter::repeat_n(action, n)
}

/// Evaluate a `BufferTheorem` against a headless `Editor`.
///
/// The runner never calls `harness.render()`. If a theorem fails because
/// it depends on layout state (e.g. viewport scroll), it is in the wrong
/// domain — use `LayoutTheorem` (Phase 3) or keep the test imperative.
pub fn assert_buffer_theorem(t: BufferTheorem) {
    // 80×24 is the default; layout dimensions are irrelevant because
    // the renderer never runs. We use `with_temp_project` so the test
    // gets an isolated working directory (per CONTRIBUTING.md §3.4).
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(t.initial_text)
        .expect("load_buffer_from_text failed");

    let api = harness.api_mut();
    api.dispatch_seq(&t.actions);

    // ── Assert buffer text ──────────────────────────────────────────
    let actual_text = api.buffer_text();
    assert_eq!(
        actual_text, t.expected_text,
        "[{}] buffer text mismatch",
        t.description
    );

    // ── Assert cursors ──────────────────────────────────────────────
    let primary = api.primary_caret();
    assert!(
        t.expected_primary == primary,
        "[{}] primary cursor mismatch:\n   expected = {:?}\n   actual   = {:?}",
        t.description,
        t.expected_primary,
        primary,
    );

    let all_carets = api.carets();
    assert_eq!(
        all_carets.len(),
        1 + t.expected_extra_cursors.len(),
        "[{}] cursor count mismatch (got {} cursors, expected {})",
        t.description,
        all_carets.len(),
        1 + t.expected_extra_cursors.len(),
    );

    // `carets()` is sorted ascending by position; the primary may be at
    // any sorted index, so we filter it out and compare the remainder
    // against the expected secondaries (also sorted ascending).
    let mut secondaries: Vec<Caret> = all_carets
        .into_iter()
        .filter(|c| !(c.position == primary.position && c.anchor == primary.anchor))
        .collect();
    secondaries.sort_by_key(|c| c.position);

    let mut expected_secondaries = t.expected_extra_cursors.clone();
    expected_secondaries.sort_by_key(|c| c.position);

    for (got, want) in secondaries.iter().zip(expected_secondaries.iter()) {
        assert!(
            *want == *got,
            "[{}] secondary cursor mismatch:\n   expected = {:?}\n   actual   = {:?}",
            t.description,
            want,
            got,
        );
    }

    // ── Assert selection text (optional) ────────────────────────────
    if let Some(expected) = t.expected_selection_text {
        let actual = api.selection_text();
        assert_eq!(
            actual, expected,
            "[{}] selection text mismatch",
            t.description
        );
    }
}
