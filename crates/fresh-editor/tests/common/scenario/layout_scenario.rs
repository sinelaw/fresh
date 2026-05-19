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
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::observable::Observable;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect};
use fresh::test_api::{Action, EditorTestApi};

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct LayoutScenario {
    pub description: String,
    pub initial_text: String,
    pub width: u16,
    pub height: u16,
    pub actions: Vec<Action>,
    /// Optional editor config. None ⇒ default config. Use for
    /// scenarios where `line_wrap` / `show_horizontal_scrollbar`
    /// etc. are load-bearing.
    #[serde(default, skip_serializing, skip_deserializing)]
    pub config: Option<fresh::config::Config>,
    /// Single-field shortcut: assert just the viewport's top byte.
    /// Kept because most landed scenarios only care about scroll.
    #[serde(default)]
    pub expected_top_byte: Option<usize>,
    /// Multi-field expectation. Combine with or replace
    /// `expected_top_byte`.
    #[serde(default)]
    pub expected_snapshot: RenderSnapshotExpect,
}

pub fn check_layout_scenario(s: LayoutScenario) -> Result<(), ScenarioFailure> {
    let width = if s.width == 0 { 80 } else { s.width };
    let height = if s.height == 0 { 24 } else { s.height };

    let mut harness = match s.config.clone() {
        Some(cfg) => EditorTestHarness::with_config(width, height, cfg)
            .expect("EditorTestHarness::with_config failed"),
        None => EditorTestHarness::with_temp_project(width, height)
            .expect("EditorTestHarness::with_temp_project failed"),
    };
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");

    harness.render().expect("initial render failed");

    {
        let api: &mut dyn EditorTestApi = harness.api_mut();
        api.dispatch_seq(&s.actions);
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

    // If the scenario asks for per-row text checks, use the
    // slower extract_with_rendered_rows path; otherwise the
    // cheap extract is sufficient.
    let needs_rows = !s.expected_snapshot.row_checks.is_empty();
    let snapshot = if needs_rows {
        RenderSnapshot::extract_with_rendered_rows(&mut harness)
    } else {
        RenderSnapshot::extract(&mut harness)
    };
    if let Some((field, expected, actual)) = s.expected_snapshot.check_against(&snapshot) {
        return Err(ScenarioFailure::SnapshotFieldMismatch {
            description: s.description,
            field: field.to_string(),
            expected,
            actual,
        });
    }
    Ok(())
}

pub fn assert_layout_scenario(s: LayoutScenario) {
    if let Err(f) = check_layout_scenario(s) {
        panic!("{f}");
    }
}
