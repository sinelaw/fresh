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
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

/// Wrap-sensitive theorems need a tighter terminal to actually
/// trigger wrapping. Use this struct when the default 80×24 won't
/// produce the layout you need. The default runner
/// `assert_buffer_theorem` always uses 80×24; for custom dimensions
/// use [`assert_buffer_theorem_with_terminal`].
#[derive(Debug, Clone, Copy)]
pub struct TerminalSize {
    pub width: u16,
    pub height: u16,
}

impl Default for TerminalSize {
    fn default() -> Self {
        Self {
            width: 80,
            height: 24,
        }
    }
}

/// Behavior flags that materially change the response to a typed
/// character — auto-close brackets, auto-indent on newline, and
/// auto-surround a selection. The default `EditorTestHarness`
/// turns all three OFF "for simpler testing" (so basic inserts are
/// 1-character changes), which is wrong when the *theorem under
/// test* is the auto-pair behavior itself. Pass explicit
/// `BehaviorFlags::production()` (or a custom subset) via
/// [`assert_buffer_theorem_with_behavior`] to re-enable production
/// semantics for that one test.
///
/// Default mirrors the harness default (all off) so existing
/// theorems are unaffected.
#[derive(Debug, Clone, Copy)]
pub struct BehaviorFlags {
    pub auto_close: bool,
    pub auto_indent: bool,
    pub auto_surround: bool,
}

impl Default for BehaviorFlags {
    fn default() -> Self {
        Self {
            auto_close: false,
            auto_indent: false,
            auto_surround: false,
        }
    }
}

impl BehaviorFlags {
    /// Match the production defaults (`Config::default()`): every
    /// auto-* feature on. Use for migrations of e2e tests that
    /// configured the harness with `harness_with_auto_indent()` or
    /// equivalent.
    pub fn production() -> Self {
        Self {
            auto_close: true,
            auto_indent: true,
            auto_surround: true,
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
    check_buffer_theorem_with_terminal(t, TerminalSize::default())
}

/// Same as [`check_buffer_theorem`] but with custom terminal
/// dimensions. Use this for wrap-sensitive theorems where the
/// default 80×24 doesn't produce the layout you need (e.g.,
/// SmartHome over a soft-wrapped continuation line).
pub fn check_buffer_theorem_with_terminal(
    t: BufferTheorem,
    term: TerminalSize,
) -> Result<(), TheoremFailure> {
    check_buffer_theorem_full(t, term, BehaviorFlags::default())
}

/// Same as [`check_buffer_theorem`] but with explicit auto-* behavior
/// flags. Use this when the theorem-under-test is auto-close /
/// auto-indent / auto-surround.
pub fn check_buffer_theorem_with_behavior(
    t: BufferTheorem,
    behavior: BehaviorFlags,
) -> Result<(), TheoremFailure> {
    check_buffer_theorem_full(t, TerminalSize::default(), behavior)
}

fn check_buffer_theorem_full(
    t: BufferTheorem,
    term: TerminalSize,
    behavior: BehaviorFlags,
) -> Result<(), TheoremFailure> {
    // We use a temp-project harness so the test gets an isolated
    // working directory (per CONTRIBUTING.md §3.4).
    //
    // Harness construction failures are infrastructure-level (no
    // disk, no temp dir) and are not theorem failures — they bubble
    // up as panics from the helper. An external driver running this
    // in a tight loop should already trust its environment.
    let mut harness = if behavior_is_default(behavior) {
        // Default path: behavior flags off (harness default); preserve
        // the exact construction `with_temp_project` uses so existing
        // theorems behave identically.
        EditorTestHarness::with_temp_project(term.width, term.height)
            .expect("EditorTestHarness::with_temp_project failed")
    } else {
        // Need an explicit Config so the harness's "config not
        // provided" branch doesn't reset auto_close/auto_indent.
        let mut config = fresh::config::Config::default();
        config.editor.auto_close = behavior.auto_close;
        config.editor.auto_indent = behavior.auto_indent;
        config.editor.auto_surround = behavior.auto_surround;
        EditorTestHarness::with_temp_project_and_config(term.width, term.height, config)
            .expect("EditorTestHarness::with_temp_project_and_config failed")
    };
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

/// Panicking wrapper around [`check_buffer_theorem_with_terminal`].
pub fn assert_buffer_theorem_with_terminal(t: BufferTheorem, term: TerminalSize) {
    if let Err(f) = check_buffer_theorem_with_terminal(t, term) {
        panic!("{f}");
    }
}

/// Panicking wrapper around [`check_buffer_theorem_with_behavior`].
pub fn assert_buffer_theorem_with_behavior(t: BufferTheorem, behavior: BehaviorFlags) {
    if let Err(f) = check_buffer_theorem_with_behavior(t, behavior) {
        panic!("{f}");
    }
}

fn behavior_is_default(b: BehaviorFlags) -> bool {
    !b.auto_close && !b.auto_indent && !b.auto_surround
}
