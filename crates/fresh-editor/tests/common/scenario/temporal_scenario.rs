//! `TemporalScenario` — timed sequences of frames.
//!
//! Drives the editor through a sequence of `InputEvent`s
//! interleaved with `AdvanceClock(Duration)` ticks. After each
//! tick, captures a `RenderSnapshot`. The scenario asserts on the
//! resulting `Vec<RenderSnapshot>`.
//!
//! Phase 10 is fully real: the editor already reads time through
//! the existing `services::time_source::TimeSource` trait, and
//! the harness already swaps in a `TestTimeSource` whose
//! `advance(d)` is wired here. Animations *do* progress when the
//! mock clock advances.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::MockClock;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::Observable;
use crate::common::scenario::render_snapshot::RenderSnapshot;
use std::time::Duration;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TemporalScenario {
    pub description: String,
    pub initial_text: String,
    /// Initial mock clock. None ⇒ epoch 0.
    #[serde(default)]
    pub clock: Option<MockClock>,
    /// Mixed sequence: editor actions interleaved with
    /// `AdvanceClock(Duration)` events. Each `AdvanceClock` yields
    /// one snapshot in the result.
    pub events: Vec<InputEvent>,
    pub expected_frames: Vec<RenderSnapshot>,
}

pub fn check_temporal_scenario(s: TemporalScenario) -> Result<(), ScenarioFailure> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");
    harness.render().expect("initial render failed");

    let mut frames = Vec::new();
    let mut elapsed = Duration::ZERO;

    // Apply the scenario's initial clock seed by pre-advancing the
    // logical time source. The harness's TestTimeSource starts at
    // logical zero; this lets a scenario simulate "the editor has
    // been running for N ms" before the first event.
    if let Some(seed) = s.clock {
        if seed.epoch_ms > 0 {
            let pre = Duration::from_millis(seed.epoch_ms);
            harness.advance_time(pre);
            elapsed += pre;
        }
    }

    for ev in &s.events {
        match ev {
            InputEvent::Action(a) => {
                harness.api_mut().dispatch(a.clone());
            }
            InputEvent::AdvanceClock(d) => {
                elapsed += *d;
                // Advance the harness's `TestTimeSource` (already
                // wired into the editor through the existing
                // `services::time_source::TimeSource` trait).
                // Editor animations / debounce / auto-save logic
                // consult that source, so they advance in lockstep.
                harness.advance_time(*d);
                harness.render().expect("frame render failed");
                frames.push(RenderSnapshot::extract(&mut harness));
            }
            other => {
                return Err(ScenarioFailure::InputProjectionFailed {
                    description: s.description,
                    reason: format!(
                        "TemporalScenario does not route {other:?}; wrong scenario type"
                    ),
                });
            }
        }
    }

    if frames.len() != s.expected_frames.len() {
        return Err(ScenarioFailure::InputProjectionFailed {
            description: s.description,
            reason: format!(
                "frame count mismatch: expected {}, got {} (after {elapsed:?})",
                s.expected_frames.len(),
                frames.len()
            ),
        });
    }
    for (i, (want, got)) in s.expected_frames.iter().zip(frames.iter()).enumerate() {
        if want != got {
            return Err(ScenarioFailure::SnapshotFieldMismatch {
                description: s.description,
                field: format!("frame[{i}]"),
                expected: format!("{want:?}"),
                actual: format!("{got:?}"),
            });
        }
    }
    Ok(())
}

pub fn assert_temporal_scenario(s: TemporalScenario) {
    if let Err(f) = check_temporal_scenario(s) {
        panic!("{f}");
    }
}
