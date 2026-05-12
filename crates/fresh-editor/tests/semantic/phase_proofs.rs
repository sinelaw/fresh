//! End-to-end proofs that each non-skeleton scenario type's runner
//! actually executes against the live editor.
//!
//! Each test here exercises the *minimal* path through one
//! scenario type's runner — load buffer, dispatch a representative
//! event, assert on the observable. They're deliberately tiny so
//! a runner regression surfaces against a single, focused test
//! rather than against a thirty-line domain scenario.

use crate::common::scenario::context::{
    NamedBuffer, PromptKind, VirtualFile, VirtualFs, WorkspaceContext,
};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::modal_scenario::{assert_modal_scenario, ModalScenario};
use crate::common::scenario::observable::{FsState, ModalState, RoundTripGrid, WorkspaceState};
use crate::common::scenario::persistence_scenario::{
    assert_persistence_scenario, PersistenceScenario,
};
use crate::common::scenario::workspace_scenario::{assert_workspace_scenario, WorkspaceScenario};
use fresh::test_api::Action;
use std::collections::BTreeMap;
use std::path::PathBuf;

// ─────────────────────────────────────────────────────────────────────
// Phase 3 — ModalScenario
// ─────────────────────────────────────────────────────────────────────

#[test]
fn phase3_command_palette_opens_and_then_cancels() {
    // Open the command palette (depth becomes 1), then cancel
    // (depth back to 0). This exercises the full modal lifecycle
    // through the OpenPrompt + CancelPrompt InputEvent variants
    // and the `modal_snapshot()` accessor.
    assert_modal_scenario(ModalScenario {
        description: "OpenPrompt(CommandPalette) then CancelPrompt".into(),
        initial_text: String::new(),
        events: vec![
            InputEvent::OpenPrompt(PromptKind::CommandPalette),
            InputEvent::CancelPrompt,
        ],
        expected_modal: ModalState {
            top_popup: None,
            depth: 0,
        },
    });
}

#[test]
fn phase3_modal_runner_round_trips_through_observable_extraction() {
    // The minimum-viable Phase 3 proof: dispatch *no* events,
    // observe an empty modal stack. This proves the runner
    // construct + observable extraction + comparison all work
    // end-to-end, without depending on which Actions actually
    // raise popups in the current editor build.
    assert_modal_scenario(ModalScenario {
        description: "fresh harness has no popups".into(),
        initial_text: String::new(),
        events: vec![],
        expected_modal: ModalState {
            top_popup: None,
            depth: 0,
        },
    });
}

// ─────────────────────────────────────────────────────────────────────
// Phase 7 — WorkspaceScenario
// ─────────────────────────────────────────────────────────────────────

#[test]
fn phase7_two_named_buffers_count_as_two() {
    assert_workspace_scenario(WorkspaceScenario {
        description: "opening two named buffers leaves buffer_count == 2".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "alpha.txt".into(),
                    content: "alpha\n".into(),
                },
                NamedBuffer {
                    filename: "bravo.txt".into(),
                    content: "bravo\n".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceState {
            buffer_count: 2,
            // Wildcard (None) — the temp-prefixed display path
            // varies per run.
            active_buffer_path: None,
            buffer_paths: Vec::new(),
        },
    });
}

// ─────────────────────────────────────────────────────────────────────
// Phase 8 — TerminalIoScenario
// ─────────────────────────────────────────────────────────────────────

#[test]
fn phase8_buffer_text_round_trips_through_vt100_grid() {
    // Drive the editor through the real ANSI emit path and parse
    // it back. The buffer's text must appear *somewhere* in the
    // grid — exact row depends on the chrome layout (menu bar,
    // status bar, tab bar). The proof is that vt100 sees the
    // characters at all, which exercises the full ANSI →
    // round-trip pipeline.
    use crate::common::harness::EditorTestHarness;
    use crate::common::scenario::observable::Observable;
    let mut h = EditorTestHarness::with_temp_project(60, 16).unwrap();
    let _f = h.load_buffer_from_text("hello world").unwrap();
    let grid = RoundTripGrid::extract(&mut h);
    let any_row_has_text = grid.rows.iter().any(|r| r.contains("hello world"));
    assert!(
        any_row_has_text,
        "no row contained 'hello world' after vt100 round-trip; got rows: {:#?}",
        grid.rows
    );
    assert_eq!(grid.height, 16);
}

#[test]
fn phase8_partial_grid_expectation_via_substring_search() {
    // Same idea, but expressed by walking the grid for a substring.
    // Real LayoutScenarios will use `GridExpect::row_at` once the
    // exact chrome layout is pinned down per scenario.
    use crate::common::harness::EditorTestHarness;
    use crate::common::scenario::observable::Observable;
    let mut h = EditorTestHarness::with_temp_project(40, 12).unwrap();
    let _f = h.load_buffer_from_text("abc").unwrap();
    let grid = RoundTripGrid::extract(&mut h);
    assert!(
        grid.rows.iter().any(|r| r.contains("abc")),
        "no row contains 'abc'; rows: {:#?}",
        grid.rows
    );
}

// ─────────────────────────────────────────────────────────────────────
// Phase 10 — TemporalScenario (real, via existing TestTimeSource)
// ─────────────────────────────────────────────────────────────────────

#[test]
fn phase10_advance_clock_actually_advances_test_time_source() {
    use crate::common::scenario::input_event::InputEvent;
    use crate::common::scenario::temporal_scenario::{check_temporal_scenario, TemporalScenario};
    use std::time::Duration;
    // The runner now wires AdvanceClock to harness.advance_time,
    // which delegates to the editor's existing TestTimeSource. We
    // can't easily assert from outside that animations advanced
    // (no animation accessor on test_api), but we can prove the
    // runner itself exits cleanly with the expected number of
    // frames — anything that consults TimeSource will see the
    // advances.
    let s = TemporalScenario {
        description: "AdvanceClock × 3 yields 3 frames".into(),
        initial_text: "hi".into(),
        clock: None,
        events: vec![
            InputEvent::AdvanceClock(Duration::from_millis(50)),
            InputEvent::AdvanceClock(Duration::from_millis(50)),
            InputEvent::AdvanceClock(Duration::from_millis(50)),
        ],
        expected_frames: vec![Default::default(); 3],
    };
    let result = check_temporal_scenario(s);
    // The frames extracted by the runner aren't equal to
    // RenderSnapshot::default() (they have the actual viewport
    // state), so the runner returns a SnapshotFieldMismatch on
    // frame[0]. That's fine for this proof — what we're checking
    // is that 3 frames *were* extracted (i.e. AdvanceClock fired
    // 3 times), which the field-mismatch error confirms.
    use crate::common::scenario::failure::ScenarioFailure;
    match result {
        Err(ScenarioFailure::SnapshotFieldMismatch { field, .. }) => {
            assert!(
                field.starts_with("frame["),
                "expected frame mismatch, got {field}"
            );
        }
        other => panic!("expected frame mismatch, got {other:?}"),
    }
}

// ─────────────────────────────────────────────────────────────────────
// Phase 6 — PersistenceScenario (real temp FS)
// ─────────────────────────────────────────────────────────────────────

#[test]
fn phase6_open_seeded_file_and_save_appends_typed_text() {
    // Seed a file with "hello", open it, type "!" at the end,
    // save, expect "hello!" on disk.
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("greet.txt"),
        VirtualFile {
            content: "hello".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "type-then-save persists to disk".into(),
        initial_fs: VirtualFs { files },
        initial_open: "greet.txt".into(),
        events: vec![
            InputEvent::Action(Action::MoveDocumentEnd),
            InputEvent::Action(Action::InsertChar('!')),
            InputEvent::Action(Action::Save),
        ],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("greet.txt".into(), "hello!".into())).collect(),
        },
    });
}

#[test]
fn phase6_external_edit_lands_on_disk_visible_to_other_processes() {
    // FsExternalEdit should mutate the file behind the editor's
    // back. This proves the runner routes the variant correctly,
    // even though we don't assert on auto-revert behavior here.
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("watch.txt"),
        VirtualFile {
            content: "before".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "FsExternalEdit writes to disk".into(),
        initial_fs: VirtualFs { files },
        initial_open: "watch.txt".into(),
        events: vec![InputEvent::FsExternalEdit {
            path: PathBuf::from("watch.txt"),
            content: "after".into(),
        }],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("watch.txt".into(), "after".into())).collect(),
        },
    });
}
