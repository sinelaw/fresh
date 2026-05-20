//! `BufferScenario` — pure buffer + caret state.
//!
//! A scenario is a value: `(initial state, action sequence, expected
//! final state)`. The runner instantiates a headless editor, applies
//! the actions through `EditorTestApi`, and asserts on the resulting
//! state. No `terminal.draw`, no `crossterm::KeyCode`, no screen
//! scraping.
//!
//! Tests using this module **must not** import anything else from
//! `fresh::*` besides `fresh::test_api`. The
//! `scripts/check-semantic-test-isolation.sh` lint enforces it.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::key_dispatch::send_key_event;
use fresh::test_api::{Action, Caret};

/// Expected state of one cursor.
///
/// Wrapper around the test-API `Caret` so test code can spell
/// expectations as `CursorExpect::at(5)` and `CursorExpect::range(0, 5)`
/// instead of building a `Caret` with `Some` / `None`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
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

/// Wrap-sensitive scenarios need a tighter terminal to actually
/// trigger wrapping. The default 80×24 is fine for most theorems;
/// SmartHome over a soft-wrapped continuation line, for example,
/// needs a tighter width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
/// turns all three OFF "for simpler testing"; scenarios whose
/// subject *is* the auto-* behavior set [`BehaviorFlags::production`]
/// in the scenario value.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct BehaviorFlags {
    pub auto_close: bool,
    pub auto_indent: bool,
    pub auto_surround: bool,
}

impl BehaviorFlags {
    /// Match the production defaults (`Config::default()`): every
    /// auto-* feature on. Use for scenarios migrated from e2e tests
    /// that configured the harness with `harness_with_auto_indent()`
    /// or equivalent.
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

/// A declarative buffer-state scenario.
///
/// Construct with struct-update syntax and `..Default::default()` to
/// avoid spelling the unset fields:
///
/// ```ignore
/// BufferScenario {
///     description: "ToUpperCase uppercases the selection".into(),
///     initial_text: "hello".into(),
///     actions: vec![Action::SelectAll, Action::ToUpperCase],
///     expected_text: "HELLO".into(),
///     expected_primary: CursorExpect::at(5),
///     ..Default::default()
/// }
/// ```
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct BufferScenario {
    /// Human-readable claim — appears in assertion failures.
    pub description: String,
    /// Buffer contents at t=0. Cursor starts at byte 0.
    pub initial_text: String,
    /// Auto-close / auto-indent / auto-surround flags. Defaults off.
    #[serde(default)]
    pub behavior: BehaviorFlags,
    /// Optional fixture filename. The extension drives language
    /// detection — `Some("x.rs")` triggers Rust comment prefix and
    /// auto-pair semantics for quote chars; `Some("x.py")` triggers
    /// Python; `None` uses the default `test_buffer.txt`.
    #[serde(default)]
    pub language: Option<String>,
    /// Terminal dimensions for wrap-sensitive scenarios.
    #[serde(default)]
    pub terminal: TerminalSize,
    /// Action sequence applied left-to-right.
    #[serde(default)]
    pub actions: Vec<Action>,
    /// Optional input-event sequence, applied **after** `actions`.
    ///
    /// `actions` is the right level for almost every buffer
    /// scenario — pure semantic operations expressed as `Action`s.
    /// Some claims, though, must be observed through the production
    /// key handler (`Editor::handle_key`) rather than the action
    /// layer: `Shift+Backspace` (issue #1588) only reaches
    /// `DeleteBackward` because `normalize_key` strips the
    /// redundant SHIFT before lookup. Dispatching
    /// `Action::DeleteBackward` directly bypasses that path and
    /// can't catch the regression.
    ///
    /// `events` lets the scenario remain pure data while still
    /// routing through the same `handle_key` path — the runner
    /// translates each `InputEvent::SendKey { code, modifiers }`
    /// into `harness.send_key(...)` and each
    /// `InputEvent::Action(a)` into `api.dispatch(a)`. Mouse and
    /// prompt variants are rejected (use `InputScenario` /
    /// `ModalScenario` for those subjects).
    #[serde(default)]
    pub events: Vec<InputEvent>,
    /// Buffer text at t=∞.
    pub expected_text: String,
    /// Primary cursor at t=∞.
    ///
    /// Always asserted unless `skip_cursor_check` is true. The
    /// opt-out exists for scenarios whose final cursor position
    /// is an implementation detail of a multi-cursor or
    /// macro-replay collapse step (e.g. the multi-cursor macro
    /// stack-overflow regression test) — the load-bearing claim
    /// there is on the buffer text, not the surviving cursor's
    /// byte position.
    pub expected_primary: CursorExpect,
    /// When true, suppress both the primary-cursor and
    /// cursor-count assertions. Defaults to false (cursor
    /// checked, single-cursor enforced).
    #[serde(default)]
    pub skip_cursor_check: bool,
    /// Secondary cursors at t=∞, in ascending byte-position order.
    /// The runner always sees the primary first; this list covers the
    /// remaining cursors. Empty for single-cursor scenarios.
    #[serde(default)]
    pub expected_extra_cursors: Vec<CursorExpect>,
    /// Optional: text reachable through the union of selections at
    /// t=∞. `None` skips the assertion. `Some("")` asserts no
    /// selection.
    #[serde(default)]
    pub expected_selection_text: Option<String>,
}

const DEFAULT_FILENAME: &str = "test_buffer.txt";

/// Evaluate a `BufferScenario` against a headless editor.
///
/// Returns `Ok(())` on success or `Err(ScenarioFailure)` on the first
/// failed assertion. Never panics on a content mismatch. Designed for
/// external drivers (proptest, shadow-model differential, corpus
/// replay) that need to call the runner in a tight loop and inspect
/// typed failures.
///
/// Scenario authors usually want [`assert_buffer_scenario`] instead.
///
/// The runner never calls `harness.render()`. If a scenario fails
/// because it depends on layout state (e.g. viewport scroll), it is
/// in the wrong domain — use `LayoutScenario` instead.
pub fn check_buffer_scenario(s: BufferScenario) -> Result<(), ScenarioFailure> {
    let mut timer =
        crate::common::timing::Timer::start(format!("buffer_scenario: {}", s.description));
    let term = s.terminal;
    // BufferScenario observes only buffer text + caret state, dispatched
    // through core `Action`s exposed by `fresh::test_api`. Plugins can't
    // reach that observable surface, so we skip plugin loading to save
    // ~440 ms per test. See `EditorTestHarness::with_temp_project_no_plugins`
    // for the broader caveat.
    let mut harness = if behavior_is_default(s.behavior) {
        EditorTestHarness::with_temp_project_no_plugins(term.width, term.height)
            .expect("EditorTestHarness::with_temp_project_no_plugins failed")
    } else {
        let mut config = fresh::config::Config::default();
        config.editor.auto_close = s.behavior.auto_close;
        config.editor.auto_indent = s.behavior.auto_indent;
        config.editor.auto_surround = s.behavior.auto_surround;
        EditorTestHarness::with_temp_project_and_config_no_plugins(term.width, term.height, config)
            .expect("EditorTestHarness::with_temp_project_and_config_no_plugins failed")
    };
    timer.phase("harness_create");
    // Force the per-harness clipboard into internal-only mode so a
    // Copy in this scenario can't leak into a parallel test's Paste
    // through the OS clipboard (arboard/X11/Wayland) and vice versa.
    // Without this, a flake surfaces: e.g. test A copies " world",
    // test B copies "universe", test B's Paste sees A's " world"
    // because they share the process-global SYSTEM_CLIPBOARD.
    harness.editor_mut().set_clipboard_for_test(String::new());
    let filename = s.language.as_deref().unwrap_or(DEFAULT_FILENAME);
    let _fixture = harness
        .load_buffer_from_text_named(filename, &s.initial_text)
        .expect("load_buffer_from_text_named failed");
    timer.phase("load_buffer");

    harness.api_mut().dispatch_seq(&s.actions);
    // `events` runs after `actions` so a scenario can pre-load
    // state through actions, then exercise the production key
    // handler via `SendKey` for the bit under test.
    if !s.events.is_empty() {
        // Mouse / cell-projection variants depend on viewport
        // size and gutter width being reconciled, which happens
        // during render. Rendering here is a no-op for pure
        // `SendKey` / `Action` events.
        harness.render().expect("render before events failed");
        for ev in &s.events {
            dispatch_buffer_event(&mut harness, ev, &s.description)?;
        }
    }
    timer.phase("dispatch_actions");
    let result = assert_buffer_expectations(&mut harness, &s);
    timer.phase("assertions");
    drop(harness);
    timer.phase("harness_drop");
    timer.finish();
    result
}

/// Assert a `BufferScenario`'s expectations against the *live* state of
/// `harness` (already positioned by the scenario's setup + actions, or
/// by an active reset + actions in the combination driver). Factored
/// out of [`check_buffer_scenario`] so the combination meta-test can
/// run a scenario's own full assertions against a shared harness.
pub(crate) fn assert_buffer_expectations(
    harness: &mut EditorTestHarness,
    s: &BufferScenario,
) -> Result<(), ScenarioFailure> {
    let api = harness.api_mut();

    // ── Assert buffer text ──────────────────────────────────────────
    let actual_text = api.buffer_text();
    if actual_text != s.expected_text {
        return Err(ScenarioFailure::BufferTextMismatch {
            description: s.description.clone(),
            expected: s.expected_text.clone(),
            actual: actual_text,
        });
    }

    // ── Assert cursors ──────────────────────────────────────────────
    let primary = api.primary_caret();
    if !s.skip_cursor_check {
        if s.expected_primary != primary {
            return Err(ScenarioFailure::PrimaryCursorMismatch {
                description: s.description.clone(),
                expected: s.expected_primary,
                actual: primary,
            });
        }

        let all_carets = api.carets();
        let expected_count = 1 + s.expected_extra_cursors.len();
        if all_carets.len() != expected_count {
            return Err(ScenarioFailure::CursorCountMismatch {
                description: s.description.clone(),
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

        let mut expected_secondaries = s.expected_extra_cursors.clone();
        expected_secondaries.sort_by_key(|c| c.position);

        for (i, (got, want)) in secondaries
            .iter()
            .zip(expected_secondaries.iter())
            .enumerate()
        {
            if want != got {
                return Err(ScenarioFailure::SecondaryCursorMismatch {
                    description: s.description.clone(),
                    index: i,
                    expected: *want,
                    actual: *got,
                });
            }
        }
    }

    // ── Assert selection text (optional) ────────────────────────────
    if let Some(expected) = &s.expected_selection_text {
        let actual = api.selection_text();
        if &actual != expected {
            return Err(ScenarioFailure::SelectionTextMismatch {
                description: s.description.clone(),
                expected: expected.clone(),
                actual,
            });
        }
    }

    Ok(())
}

/// Panicking wrapper around [`check_buffer_scenario`] for test
/// authors. The panic message is `Display` of the underlying
/// `ScenarioFailure`, which exactly mirrors the legacy `assert_eq!` /
/// `assert!` text — so `#[should_panic(expected = "…")]` meta-tests
/// continue to work.
pub fn assert_buffer_scenario(s: BufferScenario) {
    crate::common::scenario::minimize::report(&s);
    if let Err(f) = check_buffer_scenario(s) {
        panic!("{f}");
    }
}

fn behavior_is_default(b: BehaviorFlags) -> bool {
    !b.auto_close && !b.auto_indent && !b.auto_surround
}

/// Translate a single `InputEvent` for the buffer-scenario runner.
///
/// The runner accepts `Action` (dispatched through `EditorTestApi`)
/// and `SendKey` (routed through the production `handle_key` path
/// via `EditorTestHarness::send_key`). Other variants are
/// out-of-domain — those belong to `InputScenario` (mouse / IME)
/// or `ModalScenario` (prompt flow). The runner rejects them
/// rather than silently dropping them, so a mis-typed scenario
/// fails loudly.
fn dispatch_buffer_event(
    harness: &mut EditorTestHarness,
    ev: &InputEvent,
    description: &str,
) -> Result<(), ScenarioFailure> {
    match ev {
        InputEvent::Action(a) => {
            harness.api_mut().dispatch(a.clone());
            Ok(())
        }
        InputEvent::SendKey { code, modifiers } => {
            send_key_event(harness, *code, *modifiers, description)
        }
        InputEvent::Mouse(crate::common::scenario::context::MouseEvent::Click {
            row,
            col,
            button: crate::common::scenario::context::MouseButton::Left,
        }) => {
            // Click sequencing — repeated clicks at the same
            // position within the editor's click-detection window
            // are promoted to double-click / triple-click semantics
            // by `Editor::handle_mouse`. We don't have to model the
            // promotion here; we just send each click through.
            let consumed = harness.api_mut().dispatch_mouse_click(*col, *row);
            if !consumed {
                return Err(ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!(
                        "Editor did not consume Mouse::Click({col},{row}) — likely outside the buffer area"
                    ),
                });
            }
            Ok(())
        }
        other => Err(ScenarioFailure::InputProjectionFailed {
            description: description.into(),
            reason: format!(
                "BufferScenario does not handle {other:?} — use InputScenario \
                 (mouse/IME) or ModalScenario (prompt) for that subject"
            ),
        }),
    }
}
