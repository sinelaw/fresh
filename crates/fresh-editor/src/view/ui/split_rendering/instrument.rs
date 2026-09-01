//! Frame-cost instruments for the text pipeline.
//!
//! Three counters, kept per thread so that tests running in parallel each
//! see only their own frames:
//!
//! - `pane_placements`: [`super::orchestration::reconcile::place_pane`] ran —
//!   a pane's viewport, margins and wrap index were settled for a frame;
//! - `buffer_layouts`: `compute_buffer_layout` ran — a pane was formatted;
//! - `view_data_builds`: `build_view_data` ran — a pane's rows were built.
//!
//! The invariant the retained-mode migration rests on is that these three
//! advance in lockstep: **a visible text pane is placed once, formatted
//! once, and has its rows built once per frame.** The test harness asserts
//! it around every `Editor::render`; `tests/e2e/frame_once_per_pane.rs`
//! asserts the count against the number of panes on screen.
//!
//! Always compiled — three relaxed increments per pane per frame cost
//! nothing measurable — and read only through `test_api::frame_counters`.

use std::cell::Cell;

/// A snapshot of the counters. Differences between two snapshots taken
/// around a frame are that frame's cost.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FrameCounters {
    pub pane_placements: u64,
    pub buffer_layouts: u64,
    pub view_data_builds: u64,
}

impl FrameCounters {
    /// What happened between `earlier` and `self`.
    pub fn since(self, earlier: FrameCounters) -> FrameCounters {
        FrameCounters {
            pane_placements: self.pane_placements - earlier.pane_placements,
            buffer_layouts: self.buffer_layouts - earlier.buffer_layouts,
            view_data_builds: self.view_data_builds - earlier.view_data_builds,
        }
    }
}

thread_local! {
    static COUNTERS: Cell<FrameCounters> = const { Cell::new(FrameCounters {
        pane_placements: 0,
        buffer_layouts: 0,
        view_data_builds: 0,
    }) };
}

/// The counters as they stand on this thread.
pub fn snapshot() -> FrameCounters {
    COUNTERS.with(|c| c.get())
}

pub(crate) fn count_pane_placement() {
    COUNTERS.with(|c| {
        let mut v = c.get();
        v.pane_placements += 1;
        c.set(v);
    });
}

pub(crate) fn count_buffer_layout() {
    COUNTERS.with(|c| {
        let mut v = c.get();
        v.buffer_layouts += 1;
        c.set(v);
    });
}

pub(crate) fn count_view_data_build() {
    COUNTERS.with(|c| {
        let mut v = c.get();
        v.view_data_builds += 1;
        c.set(v);
    });
}
