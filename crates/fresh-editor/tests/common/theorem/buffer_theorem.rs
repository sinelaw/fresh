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
use crate::common::theorem::failure::TheoremFailure;
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
/// Returns `Ok(())` on success or `Err(TheoremFailure)` on the first
/// failed assertion. Never panics. Designed for external drivers
/// (fuzzers, generators, proof-search loops) that need to call the
/// runner in a tight loop and inspect typed failures.
///
/// Test authors usually want `assert_buffer_theorem` instead — see
/// below.
///
/// The runner never calls `harness.render()`. If a theorem fails because
/// it depends on layout state (e.g. viewport scroll), it is in the wrong
/// domain — use `LayoutTheorem` or keep the test imperative.
pub fn check_buffer_theorem(t: BufferTheorem) -> Result<(), TheoremFailure> {
    // 80×24 is the default; layout dimensions are irrelevant because
    // the renderer never runs. We use `with_temp_project` so the test
    // gets an isolated working directory (per CONTRIBUTING.md §3.4).
    //
    // Harness construction failures are infrastructure-level (no
    // disk, no temp dir) and are not theorem failures — they bubble
    // up as panics from the helper. An external driver running this
    // in a tight loop should already trust its environment.
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(t.initial_text)
        .expect("load_buffer_from_text failed");

    let api = harness.api_mut();
    api.dispatch_seq(&t.actions);

    // ── Assert buffer text ──────────────────────────────────────────
    let actual_text = api.buffer_text();
    if actual_text != t.expected_text {
        return Err(TheoremFailure::BufferTextMismatch {
            description: t.description.to_string(),
            expected: t.expected_text.to_string(),
            actual: actual_text,
        });
    }

    // ── Assert cursors ──────────────────────────────────────────────
    let primary = api.primary_caret();
    if t.expected_primary != primary {
        return Err(TheoremFailure::PrimaryCursorMismatch {
            description: t.description.to_string(),
            expected: t.expected_primary,
            actual: primary,
        });
    }

    let all_carets = api.carets();
    let expected_count = 1 + t.expected_extra_cursors.len();
    if all_carets.len() != expected_count {
        return Err(TheoremFailure::CursorCountMismatch {
            description: t.description.to_string(),
            expected: expected_count,
            actual: all_carets.len(),
        });
    }

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

    for (i, (got, want)) in secondaries
        .iter()
        .zip(expected_secondaries.iter())
        .enumerate()
    {
        if want != got {
            return Err(TheoremFailure::SecondaryCursorMismatch {
                description: t.description.to_string(),
                index: i,
                expected: *want,
                actual: *got,
            });
        }
    }

    // ── Assert selection text (optional) ────────────────────────────
    if let Some(expected) = t.expected_selection_text {
        let actual = api.selection_text();
        if actual != expected {
            return Err(TheoremFailure::SelectionTextMismatch {
                description: t.description.to_string(),
                expected: expected.to_string(),
                actual,
            });
        }
    }

    Ok(())
}

/// Panicking wrapper around [`check_buffer_theorem`] for test authors.
///
/// The panic message is `Display` of the underlying `TheoremFailure`,
/// which exactly mirrors the legacy `assert_eq!` / `assert!` text — so
/// `#[should_panic(expected = "…")]` meta-tests continue to work.
pub fn assert_buffer_theorem(t: BufferTheorem) {
    if let Err(f) = check_buffer_theorem(t) {
        panic!("{f}");
    }
}
