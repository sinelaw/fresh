//! `MarkerRoundtripScenario` — declarative migration of
//! `tests/e2e/undo_redo_marker_roundtrip.rs`.
//!
//! Margin markers (line indicators) track a byte position that must
//! survive Undo/Redo through every edit op. The original e2e tests
//! reach into `editor.active_state_mut().margins` directly to seed a
//! marker and to read its position back; that surface is internal,
//! has no `Action` projection, and pre-migration semantic tests
//! mirror the e2e style by importing `EditorTestHarness` and the
//! private types directly.
//!
//! This scenario type lifts the whole flow into data:
//!
//! 1. Seed N markers at named byte offsets (the `initial_markers`
//!    list).
//! 2. Run a setup action sequence to position the cursor.
//! 3. Walk a list of `MarkerStep`s. Each step is either an action
//!    (`Op`), an `Undo`, a `Redo`, or an inline assertion
//!    (`AssertText`, `AssertMarkers`).
//!
//! Cursor setup, edit ops, and Undo/Redo all go through
//! `EditorTestApi::dispatch`; marker seeding and readback go through
//! `EditorTestApi::seed_marker` / `marker_positions`. No
//! `EditorTestHarness::editor_mut()` calls in the test files.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::failure::ScenarioFailure;
use fresh::test_api::Action;

/// A marker to seed at scenario start.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MarkerSeed {
    pub byte_offset: usize,
    /// Glyph + namespace string. The scenario asserts positions by
    /// this symbol; pick a unique value per marker family.
    pub symbol: String,
    /// Color name (`"red"`, `"green"`, etc.). Falls back to
    /// `Color::Red` on unknown strings — the color is irrelevant for
    /// position-invariant claims.
    #[serde(default = "default_color")]
    pub color: String,
}

fn default_color() -> String {
    "red".into()
}

impl MarkerSeed {
    pub fn red(byte_offset: usize, symbol: &str) -> Self {
        Self {
            byte_offset,
            symbol: symbol.into(),
            color: "red".into(),
        }
    }
}

/// One step in the roundtrip walk. `Op` dispatches an action,
/// `Undo`/`Redo` invoke the corresponding actions, and `AssertText`/
/// `AssertMarkers` pin observables at that point.
///
/// `SaveMarkers` / `AssertMarkersMatchSaved` record marker positions
/// at one step and assert they re-appear at a later step. This lets
/// scenarios pin the Undo/Redo roundtrip claim ("Redo restores the
/// post-op marker position, whatever it happens to be") without
/// hard-coding the internal post-op value — exactly what the e2e
/// `verify_roundtrip` helper did when it captured `post_op_marker`
/// in a local before checking it across Undo+Redo.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", content = "data", rename_all = "snake_case")]
pub enum MarkerStep {
    Op(Action),
    Undo,
    Redo,
    AssertText(String),
    AssertMarkers {
        symbol: String,
        positions: Vec<usize>,
    },
    /// Capture marker positions for `symbol` into the slot named
    /// `slot`. A later `AssertMarkersMatchSaved` reads the same slot.
    SaveMarkers {
        symbol: String,
        slot: String,
    },
    /// Assert marker positions for `symbol` equal the previously
    /// captured `slot`. Slot must have been saved earlier in the
    /// step list, else the step fails with an explicit error.
    AssertMarkersMatchSaved {
        symbol: String,
        slot: String,
    },
    /// Capture buffer text into `slot`.
    SaveText {
        slot: String,
    },
    /// Assert buffer text equals the previously saved `slot`.
    AssertTextMatchSaved {
        slot: String,
    },
}

/// A fully declarative marker-roundtrip scenario.
///
/// Each test becomes a `MarkerRoundtripScenario { ... }` literal:
/// seed markers, position the cursor with `cursor_setup`, then walk
/// `steps` (which interleave ops with assertions).
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct MarkerRoundtripScenario {
    pub description: String,
    pub initial_text: String,
    pub initial_markers: Vec<MarkerSeed>,
    /// Cursor placement actions, dispatched after the buffer is
    /// loaded but before any markers are seeded. Pure positioning —
    /// no edits.
    #[serde(default)]
    pub cursor_setup: Vec<Action>,
    /// Whether to disable auto-indent and auto-close while running the
    /// scenario. Defaults to `true` because all e2e marker-roundtrip
    /// tests turn them off — they interfere with single-edit-op
    /// invariants.
    #[serde(default = "default_true")]
    pub disable_auto_features: bool,
    pub steps: Vec<MarkerStep>,
}

fn default_true() -> bool {
    true
}

pub fn check_marker_roundtrip_scenario(s: MarkerRoundtripScenario) -> Result<(), ScenarioFailure> {
    let mut harness = if s.disable_auto_features {
        // Re-create the harness config the e2e tests use: auto_indent
        // and auto_close off so single-edit ops are unaffected by
        // bracket-pair / indent insertions.
        let mut config = fresh::config::Config::default();
        config.editor.auto_indent = false;
        config.editor.auto_close = false;
        EditorTestHarness::with_temp_project_and_config(80, 24, config)
            .expect("EditorTestHarness::with_temp_project_and_config failed")
    } else {
        EditorTestHarness::with_temp_project(80, 24)
            .expect("EditorTestHarness::with_temp_project failed")
    };

    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");

    // Position the cursor first; only then seed markers, so seed
    // byte-offsets are computed against the post-setup buffer (same
    // shape the e2e tests use).
    {
        let api = harness.api_mut();
        api.dispatch_seq(&s.cursor_setup);
    }
    {
        let api = harness.api_mut();
        for seed in &s.initial_markers {
            api.seed_marker(seed.byte_offset, &seed.symbol, &seed.color);
        }
    }

    // Walk the step list.
    let mut saved_markers: std::collections::HashMap<String, Vec<usize>> =
        std::collections::HashMap::new();
    let mut saved_text: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();

    for (i, step) in s.steps.iter().enumerate() {
        match step {
            MarkerStep::Op(action) => {
                harness.api_mut().dispatch(action.clone());
            }
            MarkerStep::Undo => {
                harness.api_mut().dispatch(Action::Undo);
            }
            MarkerStep::Redo => {
                harness.api_mut().dispatch(Action::Redo);
            }
            MarkerStep::AssertText(expected) => {
                let actual = harness.api_mut().buffer_text();
                if &actual != expected {
                    return Err(ScenarioFailure::BufferTextMismatch {
                        description: format!("{}: step {i}", s.description),
                        expected: expected.clone(),
                        actual,
                    });
                }
            }
            MarkerStep::AssertMarkers { symbol, positions } => {
                let actual = harness.api_mut().marker_positions(symbol);
                if &actual != positions {
                    return Err(ScenarioFailure::WorkspaceStateMismatch {
                        description: format!("{}: step {i}", s.description),
                        field: format!("marker_positions({symbol:?})"),
                        expected: format!("{positions:?}"),
                        actual: format!("{actual:?}"),
                    });
                }
            }
            MarkerStep::SaveMarkers { symbol, slot } => {
                let positions = harness.api_mut().marker_positions(symbol);
                saved_markers.insert(slot.clone(), positions);
            }
            MarkerStep::AssertMarkersMatchSaved { symbol, slot } => {
                let expected = saved_markers.get(slot).ok_or_else(|| {
                    ScenarioFailure::WorkspaceStateMismatch {
                        description: format!("{}: step {i}", s.description),
                        field: format!("saved_markers[{slot:?}]"),
                        expected: "previously saved value".into(),
                        actual: "slot was never saved".into(),
                    }
                })?;
                let actual = harness.api_mut().marker_positions(symbol);
                if &actual != expected {
                    return Err(ScenarioFailure::WorkspaceStateMismatch {
                        description: format!("{}: step {i}", s.description),
                        field: format!("marker_positions({symbol:?}) matches slot {slot:?}"),
                        expected: format!("{expected:?}"),
                        actual: format!("{actual:?}"),
                    });
                }
            }
            MarkerStep::SaveText { slot } => {
                let text = harness.api_mut().buffer_text();
                saved_text.insert(slot.clone(), text);
            }
            MarkerStep::AssertTextMatchSaved { slot } => {
                let expected =
                    saved_text
                        .get(slot)
                        .ok_or_else(|| ScenarioFailure::BufferTextMismatch {
                            description: format!("{}: step {i}", s.description),
                            expected: format!("previously saved slot {slot:?}"),
                            actual: "slot was never saved".into(),
                        })?;
                let actual = harness.api_mut().buffer_text();
                if &actual != expected {
                    return Err(ScenarioFailure::BufferTextMismatch {
                        description: format!(
                            "{}: step {i} text matches slot {slot:?}",
                            s.description
                        ),
                        expected: expected.clone(),
                        actual,
                    });
                }
            }
        }
    }
    Ok(())
}

pub fn assert_marker_roundtrip_scenario(s: MarkerRoundtripScenario) {
    if let Err(f) = check_marker_roundtrip_scenario(s) {
        panic!("{f}");
    }
}
