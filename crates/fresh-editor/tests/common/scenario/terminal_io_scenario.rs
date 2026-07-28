//! `TerminalIoScenario` — ANSI bytes via vt100 round-trip.
//!
//! Asserts on the [`RoundTripGrid`] produced by piping the editor's
//! emitted ANSI through the harness's existing `render_real()` →
//! `rio-vt` parser → typed grid pipeline. Catches escape-emission
//! bugs (redundant SGR resets, incorrect cursor positioning,
//! incremental-redraw correctness regressions) without committing
//! to specific byte sequences.
//!
//! Phase 8 ships fully real — the harness already had the vt100
//! infrastructure; this scenario type just gives it a typed
//! observable contract that scenarios can target.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::{Observable, RoundTripGrid};
use fresh::test_api::EditorTestApi;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TerminalIoScenario {
    pub description: String,
    pub initial_text: String,
    pub width: u16,
    pub height: u16,
    pub events: Vec<InputEvent>,
    /// What we expect the post-render vt100 grid to look like. Use
    /// [`GridExpect`] for partial matches; the equality compare
    /// here is the strict full-grid form.
    pub expected: RoundTripGrid,
}

/// Partial expectation: only the fields set on the expectation
/// are asserted. Useful when a test cares about a single row,
/// column, or cursor position rather than the full grid.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct GridExpect {
    #[serde(default)]
    pub row_at: Option<(u16, String)>,
    #[serde(default)]
    pub cell_at: Option<(u16, u16, String)>,
    #[serde(default)]
    pub hardware_cursor: Option<(u16, u16)>,
}

impl GridExpect {
    pub fn check_against(&self, grid: &RoundTripGrid) -> Option<(&'static str, String, String)> {
        if let Some((row, want)) = &self.row_at {
            let got = grid.rows.get(*row as usize).cloned().unwrap_or_default();
            if got.trim_end() != want.trim_end() {
                return Some(("row_at", want.clone(), got));
            }
        }
        if let Some((row, col, want)) = &self.cell_at {
            let got = grid
                .rows
                .get(*row as usize)
                .and_then(|r| r.chars().nth(*col as usize).map(|c| c.to_string()))
                .unwrap_or_default();
            if &got != want {
                return Some(("cell_at", want.clone(), got));
            }
        }
        if let Some(want) = self.hardware_cursor {
            if Some(want) != grid.hardware_cursor {
                return Some((
                    "hardware_cursor",
                    format!("{want:?}"),
                    format!("{:?}", grid.hardware_cursor),
                ));
            }
        }
        None
    }
}

pub fn check_terminal_io_scenario(s: TerminalIoScenario) -> Result<(), ScenarioFailure> {
    let width = if s.width == 0 { 80 } else { s.width };
    let height = if s.height == 0 { 24 } else { s.height };

    let mut harness = EditorTestHarness::with_temp_project(width, height)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");

    {
        let api: &mut dyn EditorTestApi = harness.api_mut();
        for ev in &s.events {
            match ev {
                InputEvent::Action(a) => api.dispatch(a.clone()),
                other => {
                    return Err(ScenarioFailure::InputProjectionFailed {
                        description: s.description,
                        reason: format!(
                            "TerminalIoScenario does not route {other:?}; wrong scenario type"
                        ),
                    });
                }
            }
        }
    }

    let grid = RoundTripGrid::extract(&mut harness);
    if grid != s.expected {
        return Err(ScenarioFailure::SnapshotFieldMismatch {
            description: s.description,
            field: "round_trip_grid".into(),
            expected: format!("{:?}", s.expected),
            actual: format!("{grid:?}"),
        });
    }
    Ok(())
}

/// Lighter assertion shape — only checks the fields named on
/// `expect`, ignoring everything else in the grid.
pub fn check_terminal_io_scenario_partial(
    s: TerminalIoScenario,
    expect: GridExpect,
) -> Result<(), ScenarioFailure> {
    let width = if s.width == 0 { 80 } else { s.width };
    let height = if s.height == 0 { 24 } else { s.height };

    let mut harness = EditorTestHarness::with_temp_project(width, height)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");

    {
        let api: &mut dyn EditorTestApi = harness.api_mut();
        for ev in &s.events {
            if let InputEvent::Action(a) = ev {
                api.dispatch(a.clone());
            }
        }
    }

    let grid = RoundTripGrid::extract(&mut harness);
    if let Some((field, expected, actual)) = expect.check_against(&grid) {
        return Err(ScenarioFailure::SnapshotFieldMismatch {
            description: s.description,
            field: field.into(),
            expected,
            actual,
        });
    }
    Ok(())
}

pub fn assert_terminal_io_scenario(s: TerminalIoScenario) {
    if let Err(f) = check_terminal_io_scenario(s) {
        panic!("{f}");
    }
}

pub fn assert_terminal_io_scenario_partial(s: TerminalIoScenario, expect: GridExpect) {
    if let Err(f) = check_terminal_io_scenario_partial(s, expect) {
        panic!("{f}");
    }
}
