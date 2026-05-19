//! `LayoutScenario` — layout-dependent observables.
//!
//! Layout state (viewport scroll, hardware cursor screen position,
//! gutter width, visible byte range) is reconciled by the render
//! pipeline, not by action dispatch alone. `LayoutScenario` runs a
//! single render pass at the end of the action sequence so layout
//! state settles before assertion. Scenarios still avoid `for {
//! send_key; render; }` style imperative transcripts.
//!
//! Two assertion shapes are supported:
//! - `expected_top_byte`: legacy single-field shortcut, kept for
//!   the already-landed scenarios.
//! - `expected_snapshot`: a [`RenderSnapshotExpect`] with optional
//!   per-field constraints; unset fields wildcard-match.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::{MouseButton as CtxMouseButton, MouseEvent as CtxMouseEvent};
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::Observable;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect};
use fresh::test_api::{Action, EditorTestApi};

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct LayoutScenario {
    pub description: String,
    pub initial_text: String,
    /// Optional path to a fixture file to open via the editor's
    /// real file-open pipeline. When `Some(_)`, `initial_text` is
    /// ignored and the file at this path is loaded into the active
    /// buffer (used for tests whose load-bearing precondition is
    /// loading an on-disk fixture, e.g. CRLF round-trips).
    #[serde(default)]
    pub initial_file: Option<std::path::PathBuf>,
    pub width: u16,
    pub height: u16,
    pub actions: Vec<Action>,
    /// Optional input events (mouse, IME, etc.) dispatched after
    /// `actions` and before the final assertion render. Each event
    /// is translated to the editor's real input path (e.g. a
    /// `MouseEvent::Wheel { dy < 0 }` becomes a real
    /// `crossterm::MouseEventKind::ScrollDown` routed through
    /// `Editor::handle_mouse`). Use this for scenarios whose
    /// load-bearing precondition is a mouse interaction — scrollbar
    /// drags, wheel scrolls, clicks at specific cells — that have no
    /// direct `Action` projection.
    #[serde(default)]
    pub events: Vec<InputEvent>,
    /// Optional editor config. None ⇒ default config. Use for
    /// scenarios where `line_wrap` / `show_horizontal_scrollbar`
    /// etc. are load-bearing.
    #[serde(default, skip_serializing, skip_deserializing)]
    pub config: Option<fresh::config::Config>,
    /// Declarative editor-config overrides. Each `Some(_)` field
    /// is applied on top of `Config::default()` before the
    /// harness is built. Use this from semantic tests that
    /// can't import `fresh::config::Config` directly (the lint
    /// forbids the import outside harness-direct files). If
    /// both `config` and `config_overrides` are set, `config`
    /// wins (the explicit full struct path).
    #[serde(default)]
    pub config_overrides: ScenarioConfigOverrides,
    /// Single-field shortcut: assert just the viewport's top byte.
    /// Kept because most landed scenarios only care about scroll.
    #[serde(default)]
    pub expected_top_byte: Option<usize>,
    /// Multi-field expectation. Combine with or replace
    /// `expected_top_byte`.
    #[serde(default)]
    pub expected_snapshot: RenderSnapshotExpect,
    /// Per-step expectations for multi-step / cross-state claims.
    /// Each entry `{ after_action_index, expect }` is asserted after
    /// dispatching `actions[0..=after_action_index]` and rendering.
    /// Enables declarative encoding of invariants like "before X,
    /// top=A; after X, top=B" or "top_byte changes at most once
    /// across these N moves" (express as N expectations each pinning
    /// to one of two top values via
    /// `viewport_top_byte_in_set`).
    #[serde(default)]
    pub step_assertions: Vec<StepAssertion>,
    /// Cross-step invariant: across the snapshots taken at every
    /// `step_assertions` entry (in their original order), the
    /// number of distinct `viewport_top_byte` values observed must
    /// be `<= max`. Used to encode "viewport scrolled at most N
    /// times over this action sequence" — the load-bearing claim
    /// of issue #1147's viewport-stability tests. Only step
    /// snapshots count; the initial and final snapshots do not, so
    /// the caller controls exactly which points are observed.
    #[serde(default)]
    pub viewport_top_byte_distinct_at_most: Option<usize>,
    /// One-shot "redraw-screen" flag assertion: when `Some(want)`,
    /// the runner checks
    /// `EditorTestApi::take_full_redraw_request_for_tests()` against
    /// `want` after final actions/events have settled. Used for
    /// migrated `Action::RedrawScreen` (issue #1070) — the only
    /// observable for that action is the one-shot flag the event
    /// loop polls each frame.
    #[serde(default)]
    pub expected_full_redraw_requested: Option<bool>,
}

/// One per-step expectation. `after_action_index` is 0-based into
/// `actions`; the runner dispatches `actions[0..=after_action_index]`,
/// renders, then checks `expect` against the resulting snapshot.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct StepAssertion {
    pub after_action_index: usize,
    pub expect: RenderSnapshotExpect,
}

/// Declarative subset of `fresh::config::EditorConfig` flags that
/// scenario-mode tests need to set without importing
/// `fresh::config::Config` directly. Each `Some(_)` overrides the
/// corresponding field on `Config::default()`. New flags can be
/// added here as scenarios require them; production-internal
/// fields stay out of the test surface.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ScenarioConfigOverrides {
    #[serde(default)]
    pub line_wrap: Option<bool>,
    #[serde(default)]
    pub wrap_indent: Option<bool>,
    #[serde(default)]
    pub show_horizontal_scrollbar: Option<bool>,
    #[serde(default)]
    pub show_vertical_scrollbar: Option<bool>,
}

impl ScenarioConfigOverrides {
    /// True when at least one override is set.
    pub fn is_some(&self) -> bool {
        self.line_wrap.is_some()
            || self.wrap_indent.is_some()
            || self.show_horizontal_scrollbar.is_some()
            || self.show_vertical_scrollbar.is_some()
    }

    /// Apply this struct's overrides on top of a default Config.
    pub fn into_config(self) -> fresh::config::Config {
        let mut config = fresh::config::Config::default();
        if let Some(v) = self.line_wrap {
            config.editor.line_wrap = v;
        }
        if let Some(v) = self.wrap_indent {
            config.editor.wrap_indent = v;
        }
        if let Some(v) = self.show_horizontal_scrollbar {
            config.editor.show_horizontal_scrollbar = v;
        }
        if let Some(v) = self.show_vertical_scrollbar {
            config.editor.show_vertical_scrollbar = v;
        }
        config
    }
}

pub fn check_layout_scenario(s: LayoutScenario) -> Result<(), ScenarioFailure> {
    let width = if s.width == 0 { 80 } else { s.width };
    let height = if s.height == 0 { 24 } else { s.height };

    let effective_config: Option<fresh::config::Config> = match s.config.clone() {
        Some(cfg) => Some(cfg),
        None if s.config_overrides.is_some() => Some(s.config_overrides.clone().into_config()),
        None => None,
    };
    let mut harness = match effective_config {
        Some(cfg) => EditorTestHarness::with_config(width, height, cfg)
            .expect("EditorTestHarness::with_config failed"),
        None => EditorTestHarness::with_temp_project(width, height)
            .expect("EditorTestHarness::with_temp_project failed"),
    };
    if let Some(path) = &s.initial_file {
        harness.open_file(path).expect("open_file failed");
    } else {
        let _fixture = harness
            .load_buffer_from_text(&s.initial_text)
            .expect("load_buffer_from_text failed");
    }

    harness.render().expect("initial render failed");

    // Determine whether per-row text inspection is needed anywhere
    // in the scenario (final expectation or any step expectation).
    let needs_rows = !s.expected_snapshot.row_checks.is_empty()
        || s.step_assertions
            .iter()
            .any(|sa| !sa.expect.row_checks.is_empty());

    let extract_snapshot = |h: &mut EditorTestHarness| -> RenderSnapshot {
        if needs_rows {
            RenderSnapshot::extract_with_rendered_rows(h)
        } else {
            RenderSnapshot::extract(h)
        }
    };

    // Per-step assertions: dispatch up to and including
    // `after_action_index`, render, and check `expect`. Steps are
    // applied in their original order; after the last step we
    // continue dispatching any remaining actions for the final
    // assertion. Action index is checkpointed across steps so we
    // never re-dispatch.
    let mut dispatched_up_to: usize = 0; // exclusive upper bound
    let mut step_assertions = s.step_assertions.clone();
    step_assertions.sort_by_key(|sa| sa.after_action_index);

    let mut top_byte_observations: Vec<usize> = Vec::new();

    for step in &step_assertions {
        let want_inclusive = step.after_action_index + 1;
        assert!(
            want_inclusive <= s.actions.len(),
            "step_assertion.after_action_index {} is out of range (actions.len() = {})",
            step.after_action_index,
            s.actions.len()
        );
        if want_inclusive > dispatched_up_to {
            let slice = &s.actions[dispatched_up_to..want_inclusive];
            {
                let api: &mut dyn EditorTestApi = harness.api_mut();
                api.dispatch_seq(slice);
            }
            harness.render().expect("render between step assertions failed");
            dispatched_up_to = want_inclusive;
        }
        let snapshot = extract_snapshot(&mut harness);
        top_byte_observations.push(snapshot.viewport.top_byte);
        if let Some((field, expected, actual)) = step.expect.check_against(&snapshot) {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: format!(
                    "{} [step after_action_index={}]",
                    s.description, step.after_action_index
                ),
                field: field.to_string(),
                expected,
                actual,
            });
        }
    }

    // Dispatch the remaining actions (if any) for the final assertion.
    if dispatched_up_to < s.actions.len() {
        let remaining = &s.actions[dispatched_up_to..];
        let api: &mut dyn EditorTestApi = harness.api_mut();
        api.dispatch_seq(remaining);
    }

    // Dispatch declarative input events (mouse, etc.) after the
    // Action sequence. Each event is translated to the editor's
    // real input path.
    for ev in &s.events {
        dispatch_layout_event(&mut harness, ev, &s.description)?;
    }

    harness.render().expect("final render failed");

    if let Some(want) = s.expected_top_byte {
        let actual = harness.api_mut().viewport_top_byte();
        if actual != want {
            return Err(ScenarioFailure::ViewportTopByteMismatch {
                description: s.description,
                expected: want,
                actual,
            });
        }
    }

    let snapshot = extract_snapshot(&mut harness);
    // Note: only step-assertion snapshots feed into the
    // `viewport_top_byte_distinct_at_most` count, not the final.

    if let Some(max_distinct) = s.viewport_top_byte_distinct_at_most {
        let mut sorted = top_byte_observations.clone();
        sorted.sort();
        sorted.dedup();
        if sorted.len() > max_distinct {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description.clone(),
                field: "viewport_top_byte_distinct_at_most".into(),
                expected: format!("<= {max_distinct} distinct value(s)"),
                actual: format!(
                    "{} distinct value(s): {:?}",
                    sorted.len(),
                    sorted
                ),
            });
        }
    }

    if let Some((field, expected, actual)) = s.expected_snapshot.check_against(&snapshot) {
        return Err(ScenarioFailure::SnapshotFieldMismatch {
            description: s.description,
            field: field.to_string(),
            expected,
            actual,
        });
    }

    if let Some(want) = s.expected_full_redraw_requested {
        let actual = harness.api_mut().take_full_redraw_request_for_tests();
        if actual != want {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description,
                field: "full_redraw_requested".into(),
                expected: want.to_string(),
                actual: actual.to_string(),
            });
        }
    }
    Ok(())
}

/// Translate a high-level `InputEvent` into the editor's input
/// path. Only the variants actually exercised by `LayoutScenario`
/// today are wired; other variants return an
/// `InputProjectionFailed` failure so a typo in test data fails
/// loudly rather than silently no-oping.
fn dispatch_layout_event(
    harness: &mut EditorTestHarness,
    ev: &InputEvent,
    description: &str,
) -> Result<(), ScenarioFailure> {
    use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
    match ev {
        InputEvent::Action(a) => {
            harness.api_mut().dispatch(a.clone());
            harness.render().expect("render after Action event failed");
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Click { row, col, button }) => {
            let xbutton = match button {
                CtxMouseButton::Left => MouseButton::Left,
                CtxMouseButton::Right => MouseButton::Right,
                CtxMouseButton::Middle => MouseButton::Middle,
            };
            let down = MouseEvent {
                kind: MouseEventKind::Down(xbutton),
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(down)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("mouse Down: {e}"),
                })?;
            let up = MouseEvent {
                kind: MouseEventKind::Up(xbutton),
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(up)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("mouse Up: {e}"),
                })?;
            harness.render().expect("render after click failed");
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Drag {
            from_row,
            from_col,
            to_row,
            to_col,
            button,
        }) => {
            let xbutton = match button {
                CtxMouseButton::Left => MouseButton::Left,
                CtxMouseButton::Right => MouseButton::Right,
                CtxMouseButton::Middle => MouseButton::Middle,
            };
            let down = MouseEvent {
                kind: MouseEventKind::Down(xbutton),
                column: *from_col,
                row: *from_row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(down)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("drag Down: {e}"),
                })?;
            // Interpolate intermediate drag positions, matching
            // EditorTestHarness::mouse_drag's semantics so test
            // behavior stays equivalent.
            let steps = ((*to_row as i32 - *from_row as i32).abs())
                .max((*to_col as i32 - *from_col as i32).abs())
                .max(1);
            for i in 1..=steps {
                let t = i as f32 / steps as f32;
                let col = *from_col as f32 + (*to_col as f32 - *from_col as f32) * t;
                let row = *from_row as f32 + (*to_row as f32 - *from_row as f32) * t;
                let drag = MouseEvent {
                    kind: MouseEventKind::Drag(xbutton),
                    column: col as u16,
                    row: row as u16,
                    modifiers: KeyModifiers::empty(),
                };
                harness.send_mouse(drag).map_err(|e| {
                    ScenarioFailure::InputProjectionFailed {
                        description: description.into(),
                        reason: format!("drag step: {e}"),
                    }
                })?;
            }
            let up = MouseEvent {
                kind: MouseEventKind::Up(xbutton),
                column: *to_col,
                row: *to_row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(up)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("drag Up: {e}"),
                })?;
            harness.render().expect("render after drag failed");
            Ok(())
        }
        InputEvent::Mouse(CtxMouseEvent::Wheel { row, col, dy }) => {
            // Negative dy = scroll down (content moves up); positive
            // dy = scroll up. Matches the convention in
            // EditorTestHarness::mouse_scroll_down/up where each
            // call advances one wheel notch.
            let kind = if *dy < 0 {
                MouseEventKind::ScrollDown
            } else {
                MouseEventKind::ScrollUp
            };
            let event = MouseEvent {
                kind,
                column: *col,
                row: *row,
                modifiers: KeyModifiers::empty(),
            };
            harness
                .send_mouse(event)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("wheel: {e}"),
                })?;
            harness.render().expect("render after wheel failed");
            Ok(())
        }
        other => Err(ScenarioFailure::InputProjectionFailed {
            description: description.into(),
            reason: format!("LayoutScenario does not handle {other:?} — extend the runner if a scenario needs it"),
        }),
    }
}

pub fn assert_layout_scenario(s: LayoutScenario) {
    if let Err(f) = check_layout_scenario(s) {
        panic!("{f}");
    }
}
